# Code Examples
## 2023-09-01 Rewrite archivedon

```
#use input

#pub {
	#fun run(
		inputPath: String
		outputPath: String
		fetchOutbox: String
		defaultMaxPages: Word
		pageItemsCount: Word
	): Future(Result((); Error)) = {
		#let input = input.load(inputPath).await.try
		#let env = Env(
			client = reqwest.Client.new()
			output = Output.load(Path.new(outputPath)).await.try
			templates = Teampltes.create().try
			defaultMaxPages = defaultMaxPages
			staticBaseUrl = Url.parse(input.staticBaseUrl)
			fetchOutbox = fetchOutbox
			pageItemsCount = pageItemsCount
		)

		#let predefUrls = savePredefs(env).await.try

		env.output.saveTopPage(env.templates.renderTopHtml(TopHtmlParams(
			title = #mch input.title #in {
				Some(title) #> title
				None #> "Archived ActivityPub Server"
			}
			description = #mch input.description #in {
				Some(description) #> description
				None #> "A hub of archived ActivityPub servers."
			}
		)).try).await.try

		#for account #in {
			account #> fetch_account(env; predefUrls; account).await.try
		}

		Ok(())
	}
}

fetchAccount: [^logger](
	env: Env(logger)
	predefUrls: PredefUrls
	account: String
) -> Result((); Error) = {
	#let accountStripped = account.stripPrefix("@").unwrapOr(account)
	#let account = #mch accountStripped.splitOne("@") #in {
		None #> return Err("Illegal account: #{account#}".into())
		Some((username; domain)) #> Account.new(username; domain; env.staticBaseUrl).try
	}

	#let subject = "acct:#{account.ident#}"

	#let accountActorUrl = webfinger.fetchApAccountActorUrl(
		env.client;
		account.domain;
		subject;
	).await.try
	#let accountActor = activitypub.fetchActor(
		env.client;
		accountActorUrl;
	).await.try

	#if {
		&(
			!accountActor.mastodonExtItems.suspended.isSomeAnd({x #> x});
			accountActor.activityStreamsExtItems.movedTo.isNone();
		) #> println("Warning: account=#{account.ident#} is not suspended")
	}

	saveWebfingerResource(env.output; subject; account.actor_url; account.profile_url).await.try

	#let outboxUrlOpt = #case {
		env.fetchOutbox #> #mch accountActor.actorItems #in {
			None #> None
			Some(actorItems) #> Some(
				fetchOutboxCollectionRef(
					env;
					account;
					apModel.ObjectOrLink.Link(apModel.Link.from(
						actorItems.outbox.asStr()
					));
				).await.try
			)
		}
		_ #> None
	}

	saveProfileResource(
		env.output;
		account;
		accountActor;
		env.templates;
	).await.try

	#let originalAccountIdOpt = accountActor.id.clone()
	#let originalAccountLinkOpt = accountActor.objectItems.url.clone()

	saveActorResource(
		env.output;
		account;
		accountActor;
		predefUrls;
		outboxUrlOpt;
	).await.try

	#if {
		#let Some(actorId) = originalAccountIdOpt #> #mch FullUrl.parse(actorId) #in {
			Ok(actorIdUrl) #> {
				saveRedirectMap(
					env;
					actorIdUrl.domain();
					actorIdUrl.path();
					("application/activity+json");
					account.actorUrl;
				)
			}
			Err(err) #> {
				println("Warning: ID of actor is illegal: id=#{actorId#}, err=#{err#}")
			}
		}
	}

	#if #let Some(link) = originalAccountIdOpt #> {
		#if #let Some(oldUrl) = link.asFullUrl() #> {
			#let mediaType = #case {
				link.mediaType.isEmpty() #> {
					Vec.from("*/*")
				}
				_ #> {
					link.mediaType.clone()
				}
			}
			saveRedirectMap(
				env;
				oldUrl.domain();
				oldUrl.path();
				mediaType;
				account.profileUrl;
			).await.try
		}
	}

	Ok(())
}
```

## 2023-09-01 List Impl

```
#dat List(e: Type) =
	| Nil
	| Cons(head: e; rest: List(e))

#let L = List

#imp self[elem]: L(elem) #> {
	#fun map[after](f: elem -> after): L(after) = {
		#mch self #in {
			Nil #> Nil
			Cons(head; rest) => Cons(f(head); rest.map(f))	
		}
	}

	#fun reverse(): L(elem) = {
		#fun go(acc; rest) = {
			#mch rest #in {
				Nil #> acc
				Cons(head; rest) #> go(Cons(head; acc); rest)
			}
		}

		go(Nil; self)
	}
}
```
## 2023-09-05 Hetero List
```
#dat List(e: Type) =
	| Nil
	| Cons(head: e; rest: List(e))

#dat HList(types: List(Type)) #by {
	HNil: HList(Nil)
	HCons[^type: Type; ^types: List(Type)](head: type; rest: HNil(types)): HList(Cons(type; types))
}
```
