# OQ-81 recon — readings as wave-upstreams: exposure census (2026-06-10)

**Scope:** read-only recon for OQ-81 ("do kernel readings make sound wave-upstreams for
supplementary axes?"). No code changed. Probe + raw output in this directory
(`probe_reading_edges.py`, `probe_output.txt`). Worktree `oq81-investigation`, base `1b7dfadb`.

## Question taken up

OQ-81's stated resolution path is: *"read generated supplementary-axis stories whose upstream is
a reading — does the reading's claimed_type visibly distort the supplementary axis's authoring?"*
This recon establishes whether such stories exist anywhere, and where the reading-edge population
actually lives.

## Findings

### F1 — No story in any corpus was ever generated with a reading's claimed_type injected.

The proposed read-the-stories resolution has an **empty evidence base**. Per generation path:

- **Pre-merge c-orchestrator** dropped recognized kernel readings entirely (OQ-79 mech-1), so a
  reading was never in `generated_by_id` and `upstream_context()` injected nothing for it.
- **gkc `--scope`** (the path that generated the kernel_v1-era corpus, incl. `kernel_run_01/02`)
  is wave-free: `flatten_manifests` drops `downstream_of` on ordinary axes silently — declared in
  its own docstring (`agent/generate_kernel_corpus.py:229–236` "KNOWN GAP") and OQ-82.
- **Post-merge unified backend** has run live only on flat topics and the Zionism P3 manifest;
  the Zionism phase-0 manifests carry zero `downstream_of` edges (probe output). Live corpus has
  7 kernel-reading stories, none a supplementary axis with a reading upstream.
- **P4** (commit `ed2ec212`) was a deterministic mechanism witness with canned fake responses —
  it proves the wave fires, it produced no real authored story.

So resolution requires a **deliberate generation experiment** (same supplementary axis generated
with vs. without the reading upstream context — an A/B in the style of
`audits/2026-05-20_ab_test/`), or a design ruling that moots the experiment (see F3).

### F2 — The reading-edge population is archive-format; the CURRENT decompose format emits
### kernel-CONCEPT-typed edges instead, and those are silently inert (Pattern-5-shaped).

- **Archived kernel manifests** (`outputs/kernel_manifests/`, gitignored, main checkout): 263
  manifests with `axes[]`; **106 carry reading-typed edges, 184 reading-edge axes** (the OQ's
  "176/166" finding at a slightly different filter; positive control: the census fires on the
  OQ-cited `dutch_flood_control_culture` ← `husk_reading` and `vatican_ii_composite` ←
  `continuity/rupture_reading` examples).
- **Phase-0 manifests** (current primed-SCOPE format, 22 manifests, 2026-06-06): **zero**
  reading-typed edges. Kernel-manifest supplementary axes instead name the *kernel concept*
  (`software_source_status_kernel`, `literacy_acquisition_kernel`, …) in `downstream_of` —
  **21 such deps, and every one is dangling**: no generation_sequence entry matches, so no story
  is ever generated under that id. In `generate_from_manifests` the wave filter requires
  `u in run_ids` to gate, and `upstream_context()` requires `generated_by_id[u]` to inject —
  both miss, so the declared dependency silently contributes nothing and no warning fires.
  The supplementary author declares dependence on the kernel and receives no context; absence
  passes as success (Build-Discipline Pattern 5 shape, at the manifest→wave seam).
- Caveat at altitude: "zero reading-edges in the current format" is a 22-manifest empirical
  observation, not a schema guarantee — nothing forbids SCOPE from emitting a reading-typed edge,
  so the OQ-81 injection path remains reachable.

### F3 — The current format already encodes the OQ's candidate fix.

OQ-81's fix-space says "a supplementary axis depends on the kernel, not on one reading's
verdict; … inject the kernel substrate instead of a reading's claimed_type." The current SCOPE
output *already names the kernel, not a reading*, in `downstream_of` — the model is declaring
exactly the dependency the fix proposes; the backend just has no handler for it (F2). The two
findings reduce to one wiring decision.

## Decision left to the operator (human-ruled; not self-resolved)

Three options, one choice:

- **(a) Suppress reading-typed upstreams** at seed-build (skip deps that are reading_ids):
  closes OQ-81 fail-closed; archive-format manifests re-run safely; kernel-concept edges stay
  inert (F2 gap remains, could be filed separately).
- **(b) Inject kernel substrate for kernel-typed deps** (e.g. the manifest's kernel summary /
  readings list as upstream context) and suppress reading-typed deps: resolves OQ-81 *and* the
  F2 dangling-edge gap in one wiring; requires a small generation witness (the context block is
  a new prompt input).
- **(c) Run the A/B experiment first** (reading-claimed_type vs. no-context vs. kernel-substrate
  arms on the same supplementary axis) and rule from the stories: the only path that answers the
  *appropriateness* question empirically rather than by design argument; costs a small batch run.

(b) matches both the OQ's own design logic and what the current decompose format already emits;
(c) is the only one that produces a witness for the quality question as posed. Cheapest
falsifier: (c) at 1 axis × 3 arms is a handful of batch calls.

## What this does NOT establish

- Nothing here witnesses whether injected reading context *distorts* authoring — that is the
  open quality question and needs generated arms (F1).
- The 106/184 archive census is over `outputs/kernel_manifests/` as found on the main checkout
  2026-06-10; that store is gitignored and the count is not durable beyond this snapshot
  (raw output pasted in `probe_output.txt`).
