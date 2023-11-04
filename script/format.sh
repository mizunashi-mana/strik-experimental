#!/usr/bin/env bash

[ "$TRACE" = "true" ] && set -x
set -euo pipefail

function format_project {
    local project_path="$1"
    local files=()

    while IFS='' read -r file_path; do
        files+=("$file_path")
    done < <(find "$project_path" -name '*.hs')

    if [ "${#files[@]}" -ne 0 ]; then
        (
            cd "$project_path" &&
            stylish-haskell -i "${files[@]}"
        )
    fi
}

PROJECT_DIR=${PROJECT_DIR:-"$(cd "$(dirname "$(dirname "${BASH_SOURCE[0]}")")" && pwd)"}

find "${PROJECT_DIR}" -name '*.cabal' | while IFS='' read -r cabal_path; do
    project_path="$(dirname "$cabal_path")"
    echo "Target: $cabal_path"

    format_project "$project_path"
done
