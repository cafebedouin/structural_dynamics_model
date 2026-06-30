# OQ-23 HOLD deliverables (operator ruled option 3, 2026-06-29)

The operator ruled **HOLD** on the RED census: the census proved the *symptom*
(contamination flows on every populated leg) but cannot say *whose bug it is*
(network vs template vs neither), and `forecloses` leaking the same way shows
this is one relation-agnostic design question spanning OQ-23/OQ-24. Three
deliverables were required before re-ruling. Findings below.

---

## D1 — The generation template's rationale for the sibling `affects_constraint` edge

**The sibling `affects_constraint` edges are authored deliberately, by the LLM
correctly following the DP-001 ε-invariance instruction — not an accident, not a
hallucination.**

Every story carries this instruction as a header comment (emitted by
`python/generate_constraint_pl.py:528-531`, principle in
`epsilon_invariance_principle.md`):

> "Each constraint story must have a single, stable base extractiveness (ε). If
> changing the observable used to evaluate this constraint would change ε, you
> are looking at two distinct constraints. Write separate .pl files for each,
> **link them with affects_constraint/2**, and document the relationship…"

Sibling readings of a kernel have **structurally distinct ε by design** (OQ-26;
the kernel generator builds them that way). So by DP-001 they ARE "distinct
constraints" and the instruction says to link them via `affects_constraint/2`.
The leaking story says so in its own words —
`prolog/testsets/press_reformation_causation__strategic_deployment.pl:398`:

> "The three readings have structurally distinct epsilon values … the
> ε-invariance principle requires separate stories per reading, **linked via
> affects_constraints**."

`jewish_sovereignty_palestine__cultural_zionist_reading.pl:269-272` links all
four siblings the same way.

**Consequence — `affects_constraint` is OVERLOADED across (at least) three
distinct authoring intents:**
1. **DP-001 ε-split linkage** — "these are ε-distinct readings of one thing"
   (the sibling edges; documentation of relatedness).
2. **UKE dependency / argument graph** — "this constraint feeds into that one"
   (`uke_write_v2.1.md:155`, `stage0.md:153`: upstream→downstream, directional).
3. **Generic network edge** — `network.affects_constraints` in the JSON.

The FPN reads ALL of them as a single thing: a contamination conduit. The
sibling edges were authored under intent (1) — documentation that two readings
are ε-distinct-but-related — and the FPN traverses them under "low-purity
neighbor contaminates high-purity neighbor." **That is the collision.** Neither
"the template is wrong" nor "the network is wrong" is clean: the template
follows a real instruction; the network applies a real model; the relation
*carrying both* records no provenance bit saying which intent authored each edge
(build_discipline: "carry the provenance bit with the value").

**Redundancy that sharpens the ruling:** sibling readings ALSO carry typed
`cs_reading_relation(UID, Sibling, coexists_with|forecloses|influences)` edges —
authored by the kernel generator's Rule 4 (`agent/generate_kernel_corpus.py:452-466`),
which is the *typed* successor mechanism. So sibling relatedness is **doubly
encoded**: once typed (cs_reading_relation, inert in the FPN) and once untyped
(affects_constraint, contamination-bearing). The DP-001 `affects_constraint`
linkage plausibly **predates** the typed cs_reading_relation mechanism — making
the sibling affects_constraint edge redundant relatedness-encoding whose only
*additional* effect is to leak into the mechanical consumers (see D3).

---

## D3 — Blast radius of option-1's filter

`affects_constraint` has **7 reader sites** (besides `scenario_manager.pl:44`'s
retractall cleanup):

| site | what it reads the edge AS |
|---|---|
| `drl_purity_network.pl:102,105` | contamination conduit (the FPN — the leak) |
| `drl_composition.pl:125` `detect_extraction_dominance` | composite→component ("a Rope corrupted by an embedded Snare") |
| `signature_detection.pl:270` `has_viable_alternatives` | incoming edge → C has viable alternatives |
| `drl_counterfactual.pl:64,101` `dependency_chain` | declared dependency for counterfactual impact |
| `inferred_coupling_protocol.pl:213` | structural coupling inference |
| `constraint_bridge.pl:61` | bridge (RecID→Target) |
| `uke_dr_bridge.pl:13` | UKE bridge |

**Two filter sitings, very different radii:**
- **FPN-local** (add the filter inside `constraint_neighbors_existing/2`): narrow
  — only contamination changes. BUT it leaves the SAME sibling-edge conflation
  live in the other 6 readers. E.g. `detect_extraction_dominance` still treats a
  sibling snare as an "embedded component" of its sibling; `has_viable_alternatives`
  still counts a sibling edge; `dependency_chain` still reads siblings as
  dependencies. So an FPN-local filter fixes the one leak the canary happened to
  measure and armors over 6 parallel latent conflations — exactly the
  "armoring the wrong layer" risk the hold named, generalized across consumers.
- **Source-level** (don't author the sibling `affects_constraint` at all — or
  retract same-kernel typed-sibling edges at load): wide — touches all 7. This is
  the "disambiguate the overloaded relation" fix D1 points to, and because the
  typed `cs_reading_relation` already encodes the sibling relatedness, removing
  the redundant affects_constraint copy loses no authored information.

**This means the real question is not "filter coexists_with in the FPN."** It is:
*should ε-distinct sibling readings be linked via the contamination-bearing
`affects_constraint` at all, given (a) they already carry typed
cs_reading_relation edges and (b) that linkage silently feeds 6 non-FPN
consumers?* OQ-23 and OQ-24 are two instances (coexists_with, forecloses) of this
one design question.

---

## D2 — Why eligible pairs do NOT leak (positive control on the mechanism model)

**The mechanism model is COMPLETE: every non-leaking eligible pair is explained by
exactly the two predicted reasons; ZERO unexplained on both legs.** Categorizer:
`probe_nonleak.pl` (reasons: `not_coupled` = no affects_constraint edge;
`donor_zero_strength` = coupled but the lower-purity donor's dr_type has
contamination strength 0 — mountain/unknown).

| leg | eligible | leaked | non-leaking-eligible | not_coupled | donor_zero_strength | **unexplained** |
|---|---|---|---|---|---|---|
| flash | 461 | 310 | 151 | 100 | 51 (mostly `unknown`, some `mountain`) | **0** |
| kernel_v1 | 676 | 645 | 31 | 14 | 17 (`unknown`) | **0** |

So I can predict exactly which eligible pairs don't leak: those with no
`affects_constraint` edge, or whose lower-purity sibling classifies to a
non-contaminating type. No residual mechanism mystery — the leak is fully
characterized as "eligible ∧ coupled ∧ contaminating-donor."

**Bonus finding (and it sharpens the hold): the exact leak count is
cache-order-sensitive — non-deterministic across runs.** The census process
reported flash leaked=361 / kernel_v1 leaked=662; the categorizer process (same
code, same corpus) reported flash leaked=310 / kernel_v1 leaked=645. A direct
in-process probe (`probe_disc.pl`) found **0 leak-but-ineligible pairs** —
i.e. WITHIN any single run `leak ⟹ eligible` holds exactly and the census is
self-consistent. The cross-run drift is `purity_score`/`dr_type` reading the
Boltzmann memo cache in traversal-dependent order (the OQ-112 / "Boltzmann memo
caches read stale unless cleared" class, CLAUDE.md). **The qualitative RED is
robust** (every run: hundreds of eligible leaks on flash/kernel_v1, 2 on
testsets, 178 on haiku); the *precise* numerator is soft to ±~15%. This is an
independent reason not to land a filter tuned to an exact count — and a small
OQ-112 instance worth its own note.

---

## Synthesis for the re-ruling

The hold's question — *whose bug is it?* — resolves not to network-vs-template but
to: **`affects_constraint` is an overloaded relation, and the sibling edges are
redundant with the typed `cs_reading_relation` that already encodes the same
relatedness.** The decision space the operator now has, with evidence:

- **Option 1 (FPN-local filter):** narrow, but leaves 6 other consumers reading
  the same sibling edges (D3) — armors one leak, not the overload.
- **Option 1′ (source-level: stop authoring sibling `affects_constraint`, or strip
  same-kernel typed-sibling edges at load):** addresses all 7 consumers; loses no
  information because `cs_reading_relation` already carries sibling relatedness;
  also resolves the OQ-24 forecloses leak. This is the "disambiguate the overload"
  fix D1+D3 converge on. Wide blast radius — needs the old-vs-new pipeline diff
  across all consumers (composition/signature/counterfactual), not just the FPN.
- **Option 2 (accept dual-channel):** retire "zero by definition"; siblings
  genuinely contaminate. Cheapest, but then `detect_extraction_dominance` et al.
  treating siblings as components/dependencies is also "accepted," which may not
  be intended.
- **A real fourth question D1 surfaced:** is the DP-001 instruction ("link ε-distinct
  constraints via affects_constraint/2") STALE now that typed `cs_reading_relation`
  exists? If so the fix is upstream in the generation template / DP-001 doc, and the
  engine stays honest. This is the template-side option the hold asked to expose.

---

## INVESTIGATION (operator ruled option 4: witness "redundant" before ruling 1′)

Reversible load-time strip of same-kernel typed-sibling `affects_constraint`
edges; per-consumer old-vs-new diff. Probe: `probe_strip.pl`; logs
`strip_probe_testsets.log`, `strip_probe_kernel_v1.log`.

### The "redundant" claim is FALSE — stripping changes 4 of 5 reachable consumers

| consumer | testsets (3 pairs) | kernel_v1 (859 endpoints) | reads sibling edge as |
|---|---|---|---|
| FPN effective_purity | CHANGED −2/+2 | CHANGED −522/+522 | contamination conduit (the leak) |
| composition `detect_extraction_dominance` | CHANGED −3 | CHANGED **−737** | composite→component ("embedded snare") |
| counterfactual `dependency_chain` | CHANGED −6 | CHANGED **−1516** | ordered dependency |
| inferred_coupling baseline | CHANGED −6 | CHANGED −1516 | coupling edge (reads it directly) |
| signature `has_viable_alternatives` | NO DIFF | NO DIFF | (unaffected — gated on intent_viable_alternative) |
| constraint_bridge / uke_dr_bridge | n/a | n/a | recommendation-source-gated; sibling sources are constraints → structurally unreachable (0 reachable) |

Concrete conflations (not just the FPN): on kernel_v1, composition flags **737**
sibling pairs as "a Rope corrupted by an embedded Snare"
(e.g. `abrahamic_covenant__ishmael_covenant_reading` carrying
`embedded_snare(…isaac_covenant_reading, 0.58)`) — a sibling reading read as an
embedded component. Counterfactual treats all **1516** sibling edges as dependency
chains. **These are the SAME sibling-as-X conflation as the FPN leak, surfacing in
3 more consumers.** So `cs_reading_relation` does NOT carry what these consumers
read from `affects_constraint`: they read directed graph STRUCTURE
(component-embedding, dependency-ordering, contamination), not a relatedness label.

### The discriminant is corpus-dependent (imprecise on the live leg)

- **kernel_v1:** 1516 same-kernel edges, **all 1516 typed-sibling** (0 untyped).
  Clean: `same-kernel + typed-sibling` == `same-kernel`.
- **testsets:** 64 same-kernel edges, only **6 typed-sibling**; the other **58 are
  same-kernel UNTYPED** — and all 58 connect two readings (both `cs_story_uid`),
  i.e. they ARE sibling pairs whose `cs_reading_relation` does not resolve (OQ-58
  dangling: 146 source-has-relation-but-name-mismatched). For those 58 the
  `affects_constraint` edge is the SOLE structural encoding — `cs_reading_relation`
  covers only 6/64 same-kernel sibling pairs on the live leg.

So a `same-kernel + typed-sibling` strip is clean on the archive but **misses
58/64 sibling edges on testsets** (under-strips). A `same-kernel` strip (here
== sibling, since 0 same-kernel non-sibling edges found) is complete but, for the
58, removes the only sibling-relatedness record present.

### Verdict for the re-ruling

The witness came back **not clean** (the operator's flip condition). "Redundant /
no info loss" is falsified: stripping changes FPN + composition + counterfactual +
coupling. The new subjects are `detect_extraction_dominance` (737 sibling
embedded-snare flags) and `dependency_chain` (1516 sibling "dependencies") — both
reading sibling edges as structure `cs_reading_relation` does not carry.

This reframes the question one more layer: **should sibling readings carry
`affects_constraint` at all for the mechanical consumers?** All four diffs are
arguably the same error (a parallel sibling reading is not a contamination source,
not an embedded component, not a dependency) — which would make the fix "siblings
don't get `affects_constraint`; sibling relatedness lives in `cs_reading_relation`
(and OQ-58 coverage must be completed so that's not lossy)." But whether each
consumer's change is a CORRECTION (the conflation was wrong) or a LOSS (the
consumer legitimately needs the edge) is a per-consumer design call — the operator's
to make. What is now witnessed: it is NOT a localized FPN issue, and it is NOT
free of information change.

---

## REACHABILITY WITNESS (operator ruled option 2: per-consumer, first step = does each read SHIP)

The "redundant" falsification disqualified uniform fixes (option 1) — the 4
consumers read 4 *different* structures, and only the FPN read is witnessed-wrong;
the others were asserted wrong by analogy. Per-row discriminator: **does each
consumer's sibling-edge read reach a shipped product/verdict, or die internally?**
Traced callers + output paths (no engine change).

| consumer | reads sibling edge as | reaches a shipped product? | row ruling |
|---|---|---|---|
| FPN `effective_purity` | contamination | **YES** — `json_report.pl:321-323,1200` writes `contamination_network.effective_purity` into `pipeline_output.json`; also giant_comp, fpn_report, metric_drift, abductive | **ships + wrong → FIX (consumer-local)** |
| composition `detect_extraction_dominance` | composite→component | **NO** — zero callers anywhere (.pl/.py) | **inert-wrong → log, no engine change** |
| counterfactual `dependency_chain` | ordered dependency | **NO** — only caller is `simulate_cut`, which has no live caller (one comment) | **inert-wrong → log** |
| coupling `inferred_coupling` baseline | coupling edge | **YES** — `inferred_coupling_protocol.pl:410` ships `BaseEdgeCount` (incl. sibling explicit edges) to `coupling_protocol.md` | **ships → wrong by the module's OWN OQ-84 logic** (it excludes same-kernel *shared-agent* as "not a coupling signal" but not same-kernel *explicit*) → fix candidate |
| signature `has_viable_alternatives` | (no diff) | — | drop (inert) |

So of the 4 diffing consumers, **2 ship (FPN, coupling) and 2 are inert
(composition, counterfactual)**. The inert two are wrong-but-harmless (computed,
never consumed) — log, don't engine-change (build_discipline: unwired ≠ worthless,
but also unshipped ≠ urgent). OQ-58 is **decoupled**: completing typed-edge
coverage makes `cs_reading_relation` a complete *label*, but these consumers read
graph *structure* the label never carries — so OQ-58 is not on this critical path.

### Fix scope refinement (flag before landing)

The FPN row's fix site `constraint_neighbors_existing/2` is shared by **5
contamination-topology consumers** (`drl_fpn`, `network_dynamics`×3,
`giant_component_analysis`, `json_report`). So filtering same-kernel sibling
`affects_constraint` edges there is *contamination-topology-local*, not FPN-only:
it also changes giant-component connectivity. Coherent (all read
`constraint_neighbors` as the contamination graph) but an output-changing engine
change with a giant_comp ripple — wants the old-vs-new pipeline diff across legs
and a manual go before landing.

**Discriminant for the fix:** use `same-kernel` (not `same-kernel + typed-sibling`).
Empirically every same-kernel `affects_constraint` edge connects two readings (0
non-sibling found: testsets 64/64, kernel_v1 1516/1516), so `same-kernel` == sibling
here; it is complete (catches the 58 testsets edges the typed discriminant misses),
OQ-58-independent, and mirrors the existing line-105 shared-agent intra-kernel guard
exactly.

### Per-row forward plan
- **FPN:** consumer-local fix in `constraint_neighbors_existing/2` (add the
  same-kernel guard to the explicit-edge findalls, mirroring line 105). Output-changing
  → old-vs-new pipeline diff + canary-census→0 witness, manual go. Covers OQ-24 (forecloses
  rides the same FPN read).
- **coupling:** parallel candidate — extend the OQ-84 intra-kernel guard in
  `compute_baseline_edges` to explicit edges. Separate consumer-local fix.
- **composition, counterfactual:** log as inert-wrong (sibling read is wrong but
  unshipped); revisit only if a consumer is ever wired.
- **OQ-58:** flag as its own coverage item; NOT a blocker here.

---

## GIANT-COMPONENT RIPPLE WITNESS (operator ruled option 2: read the ripple before picking the site)

The FPN-fix site `constraint_neighbors_existing/2` is shared by giant_comp. Reversible
strip + giant_comp connectivity diff, with a positive control that the strip reaches the
topology layer (raw `affects_constraint` count must drop by the stripped count). Logs:
`giant_ripple_testsets.log`, `giant_ripple_kernel_v1.log`.

| leg | strip count | raw affects (pos. control) | components old→new | giant size old→new |
|---|---|---|---|---|
| testsets (107) | 64 | 228→164 (−64 ✓) | 66 → **87** | 12 → 9 |
| kernel_v1 (1106) | 1516 | 3585→2069 (−1516 ✓) | 276 → **789** | 334 → **70** |

**Positive control passed on both legs** (raw count dropped by exactly the strip count → the
strip reached the topology layer; a zero-change would have been real, not a missed plumbing).

**RIPPLE IS LARGE, especially on kernel_v1: the giant component collapses 334→70 and components
triple 276→789.** Same-kernel sibling `affects_constraint` edges are heavily load-bearing for
giant_comp connectivity. So the shared-site fix is NOT a quiet FPN-only change — it would
transform giant_comp's headline finding ("the constraint network forms a giant component of N").

### Correction or loss? (the giant_comp row's ruling)

- **Correction reading:** same-kernel siblings aren't a cross-kernel coupling signal — the engine
  ALREADY says so in two places (`drl_purity_network.pl:105` shared-agent intra-kernel guard;
  `inferred_coupling_protocol` OQ-84 guard). By that precedent the TRUE cross-kernel giant is ~70,
  and 334 was inflated by within-kernel sibling clustering. Extending the guard to explicit edges
  removes an existing inconsistency (shared-agent guarded, explicit not).
- **Loss reading:** giant_comp legitimately counts authored `affects_constraint` (incl. sibling)
  as network structure, and 334 is the intended answer; the strip would erase real topology.

The OQ-84 precedent leans **correction**, but (a) extending an OQ-84-shared-agent guard to the
explicit-edge channel is a design choice the evidence does not settle (the engine currently does
NOT guard explicit edges), and (b) the magnitude (334→70) makes this a consequential
reinterpretation of a shipped headline metric — not a side effect to land silently under an FPN
fix. So per the option-2 branching this is **significant ripple → operator rules correction-vs-loss**:
- correction → option 1 (shared site) lands; giant_comp 334→70 logged as a second intended correction.
- loss → option 4 (narrower contamination-only site, leaving giant_comp topology untouched) required.
