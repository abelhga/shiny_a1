# shiny_a1

Four R Shiny apps for exploring what people search for, and where it is heading.

| App | What it does |
| --- | --- |
| [`Forecasting-trends/`](Forecasting-trends) | Google Trends interest over time, forecast with Prophet, with backtested accuracy and adaptive anomaly detection |
| [`network/`](network) | Co-occurrence network of Google Suggest autocompletions |
| [`AmazonNetwork/`](AmazonNetwork) | The same, from Amazon's search autocomplete, per marketplace |
| [`WikiNetwork/`](WikiNetwork) | The same, from Wikipedia article search |

Every app has an optional **AI insights** tab that asks an OpenAI model to write
a plain-language read-out of whatever is on screen, with an optional free-text
focus box ("which cluster should I target first?") to steer the commentary.

## Running an app

```r
# once
Rscript install_dependencies.R

# then, from the repository root
shiny::runApp("network")
shiny::runApp("Forecasting-trends")
```

Or open any `app.R` in RStudio and click **Run App**.

Needs R 4.1+, `shiny` 1.7+ and `bslib` 0.5+. Prophet takes a while to install
the first time because of `rstan`; nothing else does.

## Enabling the AI insights panel

```bash
cp .Renviron.example .Renviron   # then edit it, and restart R
```

```
OPENAI_API_KEY=sk-...
```

The apps use the **Responses API** with the current GPT-5.6 family, selectable
per request in the panel itself:

| Model id | Use it for |
| --- | --- |
| `gpt-5.6-luna` | default — fastest and cheapest, fine for these summaries |
| `gpt-5.6-terra` | balanced |
| `gpt-5.6-sol` | the most careful reading of a messy network |

Reasoning effort is exposed too and defaults to `low`, because these are short
summarisation prompts where latency is what you feel. `OPENAI_MODEL` overrides
the default model and `OPENAI_BASE_URL` points the apps at an Azure or
compatible endpoint. No key is ever read from anywhere but the environment, and
`.Renviron` is git-ignored.

Without a key the panel explains what is missing and everything else in the app
keeps working.

## Layout

```
shared/              source of truth for code used by more than one app
  ui_kit.R           Bootstrap 5 theme (light/dark), CSS, KPI tiles, plotly theming
  ai_insights.R      OpenAI Responses API client + the Shiny module
  keyword_network.R  tokenising, co-occurrence graphs, scoring, drawing, harvesting
  network_app.R      the whole UI/server for a keyword-network app
<app>/R/             byte-identical copies of what that app needs, auto-sourced by Shiny
tools/sync_shared.sh copies shared/ into each app; --check reports drift
tests/               logic checks that need no network access and no API key
code/                the original dissertation R Markdown the forecasting app grew out of
```

Each app folder has to stand alone, because that is the unit shinyapps.io and
Posit Connect deploy. So `shared/` is edited, then copied:

```bash
tools/sync_shared.sh          # shared/ -> <app>/R/
tools/sync_shared.sh --check  # fail if a copy has drifted
```

The three network apps are now thin: each one supplies a `fetcher(query, scope)`
and a config list, and `shared/network_app.R` builds the rest.

## Tests

```bash
Rscript tests/run_all.R
```

Covers tokenising, graph construction, suggestion harvesting (budgets, caching,
failure handling), Google Trends series cleaning, granularity inference and
anomaly scoring. No network calls, no API key.

## What each app does

### Forecasting-trends

Compares up to five terms, then forecasts whichever one you pick.

- **Interest over time** — all terms, with a range slider.
- **Forecast** — Prophet's prediction and 80% interval, with the forecast start
  marked. Trend shape (linear, logistic or flat), seasonality mode, trend
  flexibility and public holidays are all adjustable. The logistic option
  saturates near 100, because the Trends index cannot leave its 0–100 scale —
  a rising linear forecast happily would.
- **Held-out scoring** — the model is refit without the most recent stretch of
  data and scored on it (MAE, MAPE, RMSE, interval coverage), so the forecast
  comes with an error bar rather than an invitation to trust it.
- **Seasonality** — Prophet's component decomposition.
- **Anomalies** — three ways to decide what counts as unusual (against a local
  window, against the whole series, or against the model's own fit), with a
  sensitivity threshold in robust-z units. The scoring uses median/MAD rather
  than mean/sd, because a spike large enough to matter inflates an sd score
  enough to hide inside it.
- **Related queries** — optionally, the top and rising searches Google
  associates with each term, with a CSV download. Off by default because it is
  a second, heavier request against the same rate limit.
- **Data** — the full table, plus CSV downloads.

Google Trends values are relative (0–100 within the comparison you asked for),
never absolute search volumes. It also compares at most five terms per
request — the app now says so when it has to trim your list instead of
trimming it silently.

### The keyword network apps

Type a seed keyword, and the app walks the source's autocomplete outward —
alphabetically (`seed a`, `seed b`, …, up to depth 4), by vector (feed each
suggestion back in as a new seed), or **by questions & prepositions**
(`how seed`, `seed for`, `seed vs`, …, in the language you are querying —
the quickest way to surface intent rather than vocabulary). Words that appear
in the same suggestion get an edge; the weight is how many suggestions they
share.

- A **request budget** with a live estimate, so a depth-3 alphabetical crawl
  (703 requests) cannot be started by accident. Progress is reported per
  request and results are cached for the session (a sidebar link clears the
  cache when you want fresh data).
- **Adaptive defaults** — after harvesting, the app picks the physics solver,
  node count, edge-weight floor and label size that suit the graph it actually
  got.
- **Live controls** — physics, fit-to-view and re-layout talk to the running
  widget through a proxy instead of rebuilding it. Node count, link threshold,
  sizing metric and cluster filter re-draw without re-fetching anything.
- **Louvain clusters**, colour-blind-safe palette, per-node tooltips with
  frequency, degree, weighted strength and betweenness.
- **Terms / Charts / Suggestions** tabs with sortable tables, bar charts and
  CSV downloads, plus a **GraphML export** of the full scored graph for Gephi
  or Cytoscape.
- Light and dark themes, switchable without a reload — charts included.

Stop words follow the language you are querying in — including, for Amazon, the
marketplace's own language rather than always English.

## Notes and limits

- Google Suggest results are shaped by the location Google sees you from. The
  language selector does not change that; a VPN does.
- All three sources are undocumented autocomplete endpoints. They rate limit,
  and they can change shape without notice. Failed requests are counted and
  skipped rather than aborting the crawl, so a partial network still draws.
- Wikipedia asks for a descriptive `User-Agent`; the app sends one.

## Credits

Originally by Abel Hernández García — <hi@abelhga.com> · <https://www.abelhga.com>.
The forecasting work started from
[Christopher Yee's Google Trends walkthrough](https://www.christopheryee.org/blog/mining-google-trends-data-with-r-featuring-gtrendsr/).
