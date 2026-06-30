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
`scratchpad/nonleak.pl` (reasons: `not_coupled` = no affects_constraint edge;
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
in-process probe (`scratchpad/disc.pl`) found **0 leak-but-ineligible pairs** —
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
