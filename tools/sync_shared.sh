#!/usr/bin/env bash
#
# Copy the shared modules into each app's R/ folder.
#
# Every app folder has to be self-contained, because that is the unit
# shinyapps.io / Posit Connect deploys. So shared/ is the source of truth and
# each app gets a byte-identical copy in its own R/, which Shiny auto-sources
# at startup.
#
#   tools/sync_shared.sh          copy shared/ -> <app>/R/
#   tools/sync_shared.sh --check  fail if any copy has drifted (for CI)
#
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
SHARED="$ROOT/shared"

# app folder : files it needs
NETWORK_FILES=(ui_kit.R ai_insights.R keyword_network.R network_app.R)
FORECAST_FILES=(ui_kit.R ai_insights.R)

CHECK_ONLY=0
[[ "${1:-}" == "--check" ]] && CHECK_ONLY=1

status=0

sync_app() {
  local app="$1"; shift
  local files=("$@")
  mkdir -p "$ROOT/$app/R"
  for file in "${files[@]}"; do
    local src="$SHARED/$file"
    local dst="$ROOT/$app/R/$file"
    if [[ $CHECK_ONLY -eq 1 ]]; then
      if ! cmp -s "$src" "$dst"; then
        echo "drift: $app/R/$file differs from shared/$file"
        status=1
      fi
    else
      cp "$src" "$dst"
      echo "synced: $app/R/$file"
    fi
  done

  # Drop copies of shared modules this app no longer needs. Files the app
  # owns itself (anything with no counterpart in shared/) are left alone.
  if [[ $CHECK_ONLY -eq 0 ]]; then
    for existing in "$ROOT/$app/R"/*.R; do
      [[ -e "$existing" ]] || continue
      local name
      name="$(basename "$existing")"
      [[ -e "$SHARED/$name" ]] || continue
      local keep=0
      for file in "${files[@]}"; do
        [[ "$name" == "$file" ]] && keep=1
      done
      if [[ $keep -eq 0 ]]; then
        rm "$existing"
        echo "removed stale: $app/R/$name"
      fi
    done
  fi
}

sync_app network          "${NETWORK_FILES[@]}"
sync_app AmazonNetwork    "${NETWORK_FILES[@]}"
sync_app WikiNetwork      "${NETWORK_FILES[@]}"
sync_app Forecasting-trends "${FORECAST_FILES[@]}"

if [[ $CHECK_ONLY -eq 1 && $status -eq 0 ]]; then
  echo "all app copies are in sync with shared/"
fi

exit $status
