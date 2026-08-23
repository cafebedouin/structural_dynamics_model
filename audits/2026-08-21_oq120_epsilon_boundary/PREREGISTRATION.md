# PREREGISTRATION — OQ-120 Phase 0, ε transition map

**Frozen:** 2026-08-23, before any Step B code was written or run.
**OQ:** OQ-120 — Rope/snare ε boundary under feeding.
**Plan:** `~/.claude/plans/switched-to-plan-mode-gentle-mochi.md`, Step C.
**Scope:** Phase 0 only — local, zero API spend. No generation arm.

This file freezes the three definitions and the three branches **verbatim with their numbers**,
before the sweep runs. Its md5 is recorded in `audit_log.md` above the first result line.

---

## Which type the pair is read on

**`FT`, the signature-resolved type.** OQ-120 asks whether *"the engine's seat-constituted type
moves across that boundary"*, and the seat-constituted type is what `dr_type/3` returns after
`integrate_signature_with_modal/3` — i.e. FT, not the raw `classify_from_metrics` MT. MT is
recorded at every point regardless, and **MT-invariant / FT-only** transitions are reported as
their own named category: that is exactly the shape of the already-witnessed
`authority_vacuum_incommensurability` flip, where the whole delta lived in the signature layer,
and it must not be silently absorbed into the general count.

## The three numbers

Over the union of every leg enumerated at execution time, at 0.01 rail resolution:

- **N_eps** — (story × seat) cells with ≥1 located transition whose `deciding_gates` contains
  `snare_epsilon_floor` or `rope_epsilon_ceiling`, and whose **FT pair has at least one endpoint
  in {rope, snare, tangled_rope, naturalized}**.
  *Widened deliberately.* An earlier draft admitted only the five explicit pairs {rope,snare},
  {rope,tangled_rope}, {rope,naturalized}, {tangled_rope,snare}, {naturalized,snare} — which
  **excludes the one ε-boundary flip already witnessed on disk**
  (`authority_vacuum_incommensurability`, piton→rope at the institutional seat, ε 0.44→0.46).
  Under the narrow filter the sweep could relocate a documented effect at exactly 0.45 and score
  it `G1a — found nothing`. A gate that can score a known positive as a null is not a gate.
  **Transitions with an `unknown` endpoint are counted but reported separately** — `unknown` is
  the cascade's terminal fallback, so moving into it is a loss of classification rather than a
  type change, and pooling the two would inflate N_eps with non-events.
- **N_reach** — of N_eps, those whose transition ε lies within [min,max] of *authored* ε over the
  claimed-rope-or-snare population **of that leg's own model stratum**.
  *Per-stratum, never pooled.* OQ-78 ruling 5 forbids pooling ε-keyed statistics across Author
  strata (`ISSUES.md:3591`), and OQ-347 measures why it would be meaningless here: cross-FAMILY ε
  exact agreement is **≈3%** (Claude `.x8` vs Flash `.x5` rails). A pooled [min,max] would union
  three different authoring idioms into a range that describes none of them. Computed once per
  model stratum and reported separately; thinking-on legs are their own stratum (their ε moves
  ≥0.10 on 17–25% of redraws vs 4–9% thinking-off, so they are a different authoring regime, not
  more of the same one).
- **N_rail** — of N_reach, those crossed by a single 0.01 rail step (a feeding effect must move a
  whole rail step to be reportable at all).

## The three branches

**All three are conditional on C1 having fired and C2 having declined** — if the controls did not
discriminate, the gate is uninterpretable and the correct output is that, not a branch.

- **G0 — CLOSES OQ-120 on the structural finding, no spend** (OR-1 **RULED**, operator
  2026-08-21). `N_rail ≥ 10` but every qualifying transition attributes to
  `rope_epsilon_ceiling`, `snare_chi_floor`, or `coalition_fired`, and **none** to
  `snare_epsilon_floor`. Then the rope/snare ε split is not free — it is dominated by χ and by the
  coalition step at 0.46, which moves d rather than the gate.
- **G2 — a spend-go becomes worth requesting.** `N_rail ≥ 10` **and** ≥2 distinct **MODELS**
  contribute **and** ≥1 located `snare_epsilon_floor` transition **whose observed FT pair is
  exactly {rope, snare}**. Below 10 a two-arm design has no power at the only permitted
  resolution; a single model makes it a corpus artifact.
  **The pair, not the gate bit, is the criterion.** `:591`'s claim is not *"this gate is
  reachable"*; it is *"this gate separates **rope** from **snare**."* Those are different
  propositions, and gate-reachability does not entail pair-correctness. If `snare_epsilon_floor`
  fires and the observed pair is {tangled_rope, snare}, the gate is reachable **and the label is
  still wrong**.
  **Corroboration is counted in DISTINCT MODELS, not legs.** Same-model redraw pairs
  (`flash2/flash3`, `sonnet2/sonnet3`, `haiku2/haiku3`, `stealth2/stealth3`, `kimi/kimi2`) **are
  the floor itself** (OQ-347: seat-vector churn with ε pinned — flash 22% / sonnet 38% / haiku
  65%), so two same-model legs agreeing is inside the noise the floor describes. Per OQ-347's
  operative line the result is read against **the noisier of the two models' floors**.
  **`archives/datasets/kernel_v1` does not count toward the ≥2** — pre-reset, different generation
  regime, retrospective breadth only; a `snare_epsilon_floor` transition appearing *only* there is
  a regime-bound observation, never live corroboration.
  **Prefer the situation-fixed core if it exists by execution time** (OQ-347 step 4). If the core
  is not yet built, say so rather than substituting.
- **G1 — re-specify, no spend.** Anything else — which includes two materially different
  situations that must not be narrated together: **G1a** `N_rail < 10` (found nothing, or found
  too little to power anything) and **G1b** a `snare_epsilon_floor`-attributed transition exists
  but on a single leg. Record which. Neither licenses a label commit.

**[G1-D] — G1's effect on Step D. CANONICAL.** Under G1, Step D lands **no commit**. Its
documentation action — write `threshold_boundary_relabel.md` with the G1a/G1b subtype — **still
runs**. Nothing else in Step D executes. The label question is then left **open**; this plan does
not schedule its return, and any later relabel is a fresh decision requiring its own evidence.

**The relabel is licensed by G0 or G2 only** — an underpowered or single-leg result is *"we don't
know."*

**Expected outcome is G0 — and that expectation has no standing against these frozen numbers.**
The branch is decided from the gate-bit deltas and this file, never from the expectation.

---

## Controls (run first, fail-closed, printed loud)

Every skip prints `SKIPPED <reason>`, is counted in the summary, and is echoed into
`gate_readout.md`. A skipped control is never a passed one.

| | control | must | proves |
|---|---|---|---|
| C1 | positive plant: a coalition-**ineligible** story (victims < 3, so d cannot jump) with χ ≫ 0.66 and supp ≥ 0.60, overlaid `directionality_override(C, analytical, 1.0)` | show a transition at exactly 0.46 with `snare_epsilon_floor` in the delta set | the probe can *see* an ε-floor crossing |
| C2 | the same story and grid, with `snare_epsilon_floor` overlaid to 0.90 | C1's 0.46 transition must **vanish** | the probe reads the threshold, not the rail — the only arm that declines on the thing under test |
| C3 | S4 shadow guard (`carbon_tax_2026`), verbatim | the guard must **fail** on a shadowed perturbation | the guard is called and can decline |
| C4 | `copyright_constitutional_mandate__judicial_ambiguity_reading` (haiku leg, supp 0.28) | at all 55 rail points ε ∈ [0.46, 1.00], report `snare_suppression_floor` blocking and `snare_epsilon_floor` satisfied-but-not-deciding — fires 0×, declines 55× | the new attributor is itself two-sided, on naturally-arising data |

C2 is non-negotiable. It uses `retractall`+`assertz`+`clear_all_caches`, not
`probe_harness:with_overlay/3` — `snapshot/2` collects only `clause(M:T,true)`, so a config rule
would be silently skipped (OQ-302/326).

C1 carrier selection is a procedure, not a hope: from the per-leg classifications, select stories
with `suppression ≥ 0.60` and fewer than 3 victims, ranked by max seat χ descending; take the top
candidate whose authored ε admits χ ≥ 0.66 at some rail point under the analytical override. If
**no** carrier exists on any leg, C1 falls back to a synthetic overlay constraint asserted for the
probe only, and the writeup states that C1 ran synthetic — which downgrades it from
naturally-arising to authored-decoy grade, reported at that altitude. C1 is **never** silently
skipped.

## Grid

The 0.01 rail (101 points — the only reportable resolution, OQ-78 ruling 1) ∪ bracket triples
{T−δ, T, T+δ} at δ=1e-4 for T ∈ {0.10, 0.25, 0.30, 0.35, 0.45, 0.46, 0.60, 0.66} ∪ adaptive
bisection to 1e-4 wherever adjacent points differ in (MT, FT) at any seat. The χ-side thresholds
are included: their ε-crossings cannot be pre-placed, because f·σ is itself ε-dependent. Restore
after **every** probe point.

## Emission

**A gate-bit delta vector, not a scalar "deciding gate."** Per probe point per seat: `eps, d, f_d,
scope_mod, chi, supp, coalition_fired` plus the independently-evaluated gate bits; per located
transition, `deciding_gates` = the set of bits that changed. Load-bearing: crossing 0.46 flips
`snare_epsilon_floor` f→t, `coalition_fired` f→t, **and** `snare_chi_floor` t→f at once, and a
scalar attributor would have to pick one — which is a narrative, not a measurement.

## Non-authoring-facing

OQ-78 ruling 3 (`ISSUES.md:3585`) binds on this phase's OUTPUT, not only on generation. The
transition map contains exact ε values at which types flip — precisely the numeric disclosure
ruling 3 forbids reaching an authoring stage. It must never feed a prompt, a seed file, or
`epsilon_bin`.
