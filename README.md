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

Two ceilings bound what the panel can spend, because on a public host the key
is one shared env var and anyone can press the button: `AI_MAX_CALLS` per
browser session (default 5; a reload starts a new session, so this is
friction, not a wall) and `AI_MAX_CALLS_PER_DAY` per process (default 200).
The real wall is the monthly limit you set on the key itself at OpenAI.

## Where these run

Three places, one codebase. What separates them is configuration, not code:
every ceiling below is an environment variable with a safe default, so an
instance that sets nothing is the public one.

| | Where | For whom | Ceilings |
|---|---|---|---|
| Showcase | [abelhga.com/tools](https://www.abelhga.com/tools) — rewritten to run in a browser tab | anyone, no sign-up | the browser's |
| Free mirror | Posit Connect Cloud | anyone | defaults: 500 requests a crawl, 5 AI read-outs a session |
| **Private** | **Railway, behind a password** | **the owner** | raised by env vars |

| Variable | Default | What it caps |
|---|---|---|
| `MAX_REQUEST_BUDGET` | 500 | requests per crawl, clamped server-side |
| `AI_MAX_CALLS` | 5 | AI read-outs per browser session |
| `AI_MAX_CALLS_PER_DAY` | 200 | AI read-outs per process per day |
| `APP_PASSWORD` | unset | unset means no login at all |
| `GATE_ENABLED` | unset | `1` turns on the funnel below |
| `OWNER_KEY` | unset | `?owner=<key>` once sets a cookie that bypasses every ceiling |
| `OWNER_REQUEST_BUDGET` | 5000 | the owner's crawl ceiling |

Raising `MAX_REQUEST_BUDGET` buys a longer crawl, not an unlimited one: Google
and Amazon rate limit a datacenter IP well before a few hundred requests. For a
genuinely big crawl, run it locally on a residential connection.

## The funnel

The public instance is the landing page of a LinkedIn post, so it meters
visitors instead of asking for a password (`shared/gate.R`):

1. **One free go** — the first visit to any app.
2. **Two more for an email** — on the second visit a modal asks for an address
   (in Spanish or English, after the browser's language). The address goes to
   Supabase with what they were analysing, which is what qualifies the contact.
3. **Then "write to me on LinkedIn"**, with a `mailto:` whose subject carries
   the terms they were looking at.

A "visit" is a browser session; a reload or a dropped websocket within 30
minutes is the same visit. State lives in first-party cookies for a year. It
is a funnel, not a fortress: clearing cookies starts over, and that is fine.
The owner opens `?owner=<OWNER_KEY>` once and is never metered again.

Around it, four things that make the first click from the post land well:

- **The analysis lives in the URL** (`?kw=a,b&geo=MX&time=today+5-y` on the
  forecast, `?q=seed&scope=es&method=by_questions` on the networks). Opening a
  link runs it, a forced reload reproduces it, and the *Share* button copies
  it (native share sheet on phones). `session$allowReconnect(TRUE)` plus the
  timeouts in `docker/shiny-server.conf` cover the short drops.
- **A cache for Google Trends** (memory, then disk, six hours) so the terms
  everybody asks for after the post never reach Google's rate limit twice.
- **A featured analysis** (`FEATURED_KW`, `FEATURED_GEO`, `FEATURED_TIME`)
  warmed when the forecasting process starts, linked from the landing page.
- **Events** (`visit`, `email_asked`, `email_left`, `contact_shown`,
  `contact_click`, `share`) in `web_events`, so the funnel can be measured
  with one query.

| Variable | What it is |
|---|---|
| `SUPABASE_URL`, `SUPABASE_ANON_KEY` | where `web_leads` and `web_events` live; the key can only insert (RLS + revoked reads) |
| `LINKEDIN_URL` | the contact button (default: the owner's profile) |
| `PUBLIC_SITE_URL` | the site the apps belong to, for the copy and the Open Graph image |
| `FEATURED_KW`, `FEATURED_GEO`, `FEATURED_TIME` | the featured analysis |
| `GATE_CACHE_DIR` | the disk cache (default under `tempdir()`) |

```sql
-- how is the funnel doing?
select event, count(*) from web_events where created_at > now() - interval '7 days' group by 1;
select email, app, context, created_at from web_leads order by created_at desc;
```

## Publishing on Posit Connect Cloud

Each app folder carries a `manifest.json` (from `rsconnect::writeManifest()`,
R 4.3.3, every package pinned to CRAN), which is what git-backed publishing
needs to build the app straight from this repository. On
[Posit Connect Cloud](https://connect.posit.cloud):

1. **Publish → Shiny (R)**, connect GitHub, pick this repository and the
   `main` branch.
2. Application directory `Forecasting-trends`, primary file `app.R`. Repeat
   as separate content for `network` and `AmazonNetwork` (and `WikiNetwork`
   if you want it). Enable republish-on-push if offered.
3. Environment variables, per app: `OPENAI_API_KEY` (as a secret), and
   optionally `OPENAI_MODEL`, `AI_MAX_CALLS`, `AI_MAX_CALLS_PER_DAY`.
4. Paste the resulting URLs into `src/lib/r-apps.js` in
   [`my-website`](https://github.com/abelhga/my-website) so abelhga.com
   links each browser tool to its full app.

What to expect on the free plan (as of September 2026: 20 active hours a
month, 2 CPUs, 4 GB): the forecasting app takes a while to build the first
time (rstan/prophet), apps sleep between visits and the first visitor after a
sleep waits 30–60 s. Because the apps are public, the request budget is
capped server-side at 500 per crawl and there is no "clear cache" link —
the suggestion cache is shared by every session in the process and wiping it
would wipe it for everyone.

## Running the container (Railway, or anywhere)

The `Dockerfile` builds all four apps into one image: Shiny Server serves them
under one port, nginx sits in front for the port and the password.

```bash
docker build -t shiny-a1 .
docker run --rm -p 8080:8080 -e PORT=8080 shiny-a1                 # open
docker run --rm -p 8080:8080 -e PORT=8080 \
  -e APP_USER=abel -e APP_PASSWORD=… shiny-a1                      # private
```

```
/             a static landing page
/forecasting  Forecasting-trends
/network      network
/amazon       AmazonNetwork
/wiki         WikiNetwork
```

Two things the open edition of Shiny Server cannot do on its own, and how
`docker/entrypoint.sh` does them:

- **The port.** Railway injects `$PORT` at runtime; `listen` in
  `shiny-server.conf` is a fixed number in a file. nginx listens on `$PORT`
  and proxies to 3838 inside. Its config also carries the websocket `Upgrade`
  headers — without those, every app loads and then shows "Disconnected from
  the server".
- **The password.** Authentication is a Connect feature, not an open-source
  one. With `APP_USER` and `APP_PASSWORD` set, nginx asks for them; the hash is
  generated at each start, so the password lives in the platform's variables
  and never in this repository. Leave `APP_PASSWORD` unset and the site is
  open — same image, public deployment.

On Railway: new project → deploy from this repository → set the variables
above → generate a domain. The build takes a few minutes because the base
image pulls its R packages as precompiled binaries from Posit Package Manager
(with plain CRAN, prophet would drag rstan through a C++ compile instead).
Prophet is memory-hungry once loaded; if the forecasting app dies on open,
give the service more RAM.

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
failure handling), Google Trends series cleaning, granularity inference,
anomaly scoring and the AI panel's request shape and spend ceilings. No
network calls, no API key.

## What each app does

### Forecasting-trends

Compares up to five terms and fits one Prophet model per term. The series
picker forecasts them all at once (the default when several came back) or any
single one.

- **Interest over time** — all terms, with a range slider.
- **Forecast** — Prophet's prediction and 80% interval, with the forecast start
  marked. With "All terms" selected, every term gets its own observed line,
  dotted forecast and interval in its own colour, and the KPI tiles switch to
  one forecast-change tile per term. Trend shape (linear, logistic or flat),
  seasonality mode, trend flexibility and public holidays are all adjustable.
  The logistic option saturates near 100, because the Trends index cannot
  leave its 0–100 scale — a rising linear forecast happily would.
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
- **Data** — the full table, plus CSV downloads. The forecast CSV always
  carries every term, whatever is on screen.
- **AI insights** — the briefing sent to the model covers every fetched term
  (history, trend, per-term forecast, flagged dates, rankings by level and by
  forecast change); an "Insights about" picker narrows it to one term when you
  want the read-out separately.

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
  request and results are cached in the process (shared by every session, so
  a term someone else just looked up costs nothing). The budget tops out at
  500 per crawl; a full depth-3 alphabetical crawl is a local run.
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
