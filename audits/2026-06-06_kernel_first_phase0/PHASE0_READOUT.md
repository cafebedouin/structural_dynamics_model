# Phase 0 (Step 0a) readout — kernel-first auto-routing: self-classifier accuracy

Plan: `~/.claude/plans/cc-audit-brief-golden-pebble.md` (Phase 0, Step 0a).
Probe: `python/audits/kernel_first_phase0.py`. Raw: `outputs/kernel_first_phase0/*.manifest.json`,
`outcome_rows.json`, `outcome_table.md`, `hard_flat_precheck_grounding.txt`, `run.log`.
Model: claude-sonnet-4-5-20250929. All SCOPE calls `research_context=""` (priming isolated from
grounding). Date: 2026-06-06.

## Question

Does the EXISTING primed self-classifier (gkc `_scope_user_prompt`'s `is_contested_kernel`,
refusal exit intact) already separate kernel from flat — i.e. is the cheap fix ("route through the
primed path c-orchestrator bypasses, trust the verdict, confirm with eyeball") viable, or does the
self-classifier confabulate `true` on a real hard-flat (→ 0b: grounding leg / force arm needed)?

## Result table (raw counts; reps capture temp-0.2 stochasticity)

| topic | is_kernel | reps agree? | readings (per rep) | how it decided |
|---|---|---|---|---|
| magnifica_wholedoc (full 1223-line doc) | **True** | 3/3 | 4,3,4 | distinct readings, real constituencies |
| magnifica_brief (compressed main-idea+variants) | **True** | 3/3 | 3,3,3 | distinct readings, real constituencies |
| zionism (neutral statement) — positive control | **True** | 2/2 | 5,5 | distinct readings, real constituencies |
| easy_flat ("drive on the right") — easy negative | **False** | 2/2 | 0 | reasoned `omega_not_a_kernel` |
| hard_flat (ISO-8601 vs US date) — pre-checked means-only | **False** | 3/3 | 0 | reasoned `omega_not_a_kernel` |

Stochastically stable **across reps on these five topics**: zero verdict flips. Caveat (do not
over-update): reps measure *consistency*, not *correctness*, and 13 reps over 5 topics is a small
sample — the absence of a single flip is weak evidence of stability *as a property* of the primed
classifier. Claim scoped to the topics tested; NOT generalized to "the primed classifier is stable."
(K1's coin-flippiness was the *unprimed/in-passing* gate — a different mechanism.)

## Eyeball (real vs confabulated — the call the mechanical label cannot make)

- **magnifica** is a REAL kernel. Whole-doc found {magisterial-imago-Dei, technocratic-optimization,
  democratic-pluralist, market-libertarian}; brief found {imago-Dei, autonomy-rights, posthuman}.
  Authority groundings are accurate (Magisterial tradition / utilitarian-SV / Rawlsian / Austrian-
  Lockean / secular bioethics / transhumanist-longtermist). Axiom contradictions are genuine and
  correctly identify mutual exclusivity. The whole document produced a RICHER set than the brief —
  **size did not dilute the kernel once primed.**
- **Zionism** is a REAL kernel: the five canonical contested readings, each correctly sourced.
- **easy_flat / hard_flat** refused with REASONED `omega_not_a_kernel`, not low-salience miss.
  hard_flat omega: "both formats encode identical information with identical precision. There is no
  shared commitment being read differently." It even flagged its own scope boundary
  (`omega_yyyy_mm_dd_privilege`: a "true standards dispute would include non-Gregorian calendars;
  their absence suggests…"). The confabulation boundary was genuinely tested and HELD.

## Hard-flat label validation (means-only pre-check)

Grounding (`hard_flat_precheck_grounding.txt`) confirms ISO-8601-vs-US is a **genuine, substantive,
real-constituency** dispute (NASA, W3C, US military actors; medical-record / cross-border-contract
stakes; "clash between linguistic/cultural convention and technical/logical optimization") — NOT a
trivial easy-flat. Within the topic as scoped (numeric format, both Gregorian), **no foundational-
commitment strand surfaced in the grounding** (readout = absence-of-finding in this search, not
absence). Label holds: a valid hard-flat (real heat, means-only).

## Verdict: CHEAP BRANCH — and it survived scrutiny

The self-classifier separated all four classes correctly: real kernels → `true` with genuine
distinct readings; easy AND substantive-hard flats → reasoned `false`. **Priming alone flips
magnifica** (3/3, whole-doc and brief) — the magnifica failure was the *unprimed* c-orchestrator
path never asking the kernel question, NOT dilution. **0b is NOT triggered** — no confabulation on
the hard-flat, so the grounding leg / force arm are not required to make routing work.

Implication (per plan): the fix is the small build — route topics through the primed SCOPE path
c-orchestrator bypasses; trust the `is_contested_kernel` verdict as the router; keep
`coherence_eyeball` mandatory on kernel-routed topics as the load-bearing last-resort backstop; the
safe failure direction (confabulated kernel → spurious construction *pair*, never silent flat-loss)
covers a missed one.

## Honest limits (scrutinize, don't celebrate)

- **Hard-flat n = 1 topic.** One clean negative witnesses that the self-classifier held on *this*
  hard-flat, not on all. "I didn't find a confabulation" ≠ "it doesn't confabulate." Cheapest
  strengthening before full architectural commitment: 1–2 MORE hard-flats — especially a **heated
  policy-means dispute** in the corpus's political domain (where the shared axiom is real but less
  obvious than ISO-8601's, so confabulation is more tempting) and a deliberate **borderline
  soft-kernel** to find where the line actually breaks.
- Predicate-form decision (plan Open-decisions): the result points to **"trust the primed verdict +
  mandatory eyeball,"** with the grounding leg (A3) demoted from "probably needed" to "not needed
  for routing on this evidence" — pending the wider hard-flat test.
- Still unbuilt regardless of branch: the grounded-path wiring caveat (batch path hardcodes `""`,
  gkc:1276) — only relevant if a future change routes kernel attempts through the batch path.

## WIDEN (2026-06-06) — the loud hard-flats the n=1 gap demanded

Per review: a second *clean* hard-flat witnesses nothing; the confabulation failure mode needs
**surface pressure** (loud rhetoric, partisan animosity, constituencies that TALK as if axioms are
incompatible, but a shared commitment). Selected for max confabulation temptation, not cleanliness.
Probe: `python/audits/kernel_first_phase0_widen.py`. Raw: `widen_table.md`, `widen_rows.json`,
`<tag>_rep*.manifest.json`, `<tag>_precheck_grounding.txt`.

| topic | kind | is_kernel | reps | grounding's own framing | verdict |
|---|---|---|---|---|---|
| reading_wars (phonics vs whole-language) | loud hard-flat | **True** | 3/3 | "growing policy consensus… end to the reading wars"; residual = "philosophical debates traditionalists vs progressives" | **over-routed** — mostly-settled empirical means-dispute + a foundational tail the classifier elevated to "what reading IS" |
| nuclear_climate | loud hard-flat | **True** | 3/3 | "whether decarbonization requires a portfolio incl. nuclear OR focus on renewables… deeper questions about risk tolerance" | **over-routed (weakest)** — grounding says means-dispute under a SHARED decarbonization axiom; classifier framed empirical "baseload irreducible vs eliminable" as an axiom-contradiction |
| oss_vs_proprietary | borderline (calibration, no label) | **True** | 3/3 | "fundamentally different assumptions about the purpose of software… freedom as a human right vs pragmatic methodology vs vendor control" | **endorsable real kernel** — FSF software-freedom-as-right IS a foundational commitment distinct from pragmatic openness |

### What this falsifies, and what it reveals

The clean dichotomy (confabulation vs real-kernel) does NOT hold, and the cheap branch as stated in
0a is **not** the whole story. The loud hard-flats did **not** hold the line — but the readings are
**not fabricated** either: reading-wars, nuclear, and oss all have real constituencies and real
foundational strands (the grounding confirms each). The actual phenomenon:

**The primed self-classifier is KERNEL-LIBERAL — high recall, imperfect precision.** It routes to
kernel whenever a defensible foundational reading can be *constructed*, and refuses only when one
genuinely cannot (ISO-8601 / drive-on-right: "identical information, no shared commitment read
differently"). On topics that are *dominantly* means-disputes-under-a-shared-axiom but have an
*available* foundational strand (nuclear clearest; reading-wars partial), it over-routes to kernel —
dressing empirical/means disagreements as axiom-contradictions. It has no notion of **dominance**
(is the foundational contest the main structure, or a minority tail?); presence of a constructible
contest is sufficient for a kernel verdict.

### Three architecture-level consequences (these change the plan)

1. **The grounding leg (A3 / original 0b) is the WRONG instrument for the failure that showed up.**
   A3 flags readings with no real constituency (fabrication). But the over-routed readings DO have
   real constituencies — grounding would *confirm* them, not flag the over-routing. The phenomenon
   is over-elevation of real-but-non-dominant strands, which no grounding check separates from a
   real kernel. **A3 is demoted: it does not address kernel-liberality.**
2. **The failure direction remains SAFE.** Kernel-liberal → an over-routed topic yields a kernel
   construction PLUS the symmetric forced-flat pass (the construction pair), never a silent
   flat-loss. Over-routing costs extra generation (the self-funding "wasted" attempt), not lost
   signal. So kernel-liberality is *tolerable by construction* even if not ideal.
3. **The precision judgment is inherently human.** No manifest-level or grounding-level signal
   separates "real kernel" (oss, magnifica) from "over-routed means-dispute" (nuclear): both carry
   distinct readings, real constituencies, declared axiom-contradictions. The separator is a
   dominance/framing call. This confirms the eyeball as load-bearing — for a BIGGER reason than the
   plan anticipated: frequent precision-curation of kernel-positives, not rare fabrication-catching.

## REVISED verdict & the operator's-call question

What the evidence settles:
- Priming is the magnifica fix (not size); the classifier never silently loses a real kernel (errs
  toward kernel) — the dangerous direction is not observed.
- The primed verdict is a **kernel-liberal, high-recall, imperfect-precision gate**, not a
  trustworthy-as-final classifier. "Trust the primed verdict alone" (the 0a cheap-branch hope) is
  **falsified** for precision.
- The originally-planned grounding leg (A3) does **not** fix the actual failure mode.

What is the operator's to rule (a what-the-engine-is-for design question, not an evidence gap):
**Is kernel-liberality acceptable?**
- If YES (any topic with a foundational fault line *should* get the kernel construction + flat
  pair): proceed to wire the primed-first router; the eyeball curates precision on kernel-positives;
  the safe failure direction covers over-routing. Smallest build, A3 dropped.
- If NO (kernel should be reserved for *dominantly* foundational contests): a **precision/dominance
  stage** is needed — but the evidence says it cannot be a grounding check or a manifest-text
  predicate; it is a human or a much harder judgment. That is a larger, separate design problem.

Recommendation: name kernel-liberality explicitly in the design, proceed on the safe-direction
argument (option YES), make the eyeball the mandatory precision stage on kernel-positives, and drop
the A3 grounding leg from the routing path — because the data shows precision is the human's job and
the cost of over-routing is bounded generation, not lost signal. **Stop here for the operator's
ruling on kernel-liberality before building.**

## RULING (2026-06-06): LIBERAL — and the seat theorem reframes "over-routing"

Operator ruling: **YES, kernel-liberal**, with two conditions (label demotion; promotion policy).
Rationale: `docs/seat-theorem-v1.md` (Coupling Theorem) and
`docs/commitment_systems/commitment_systems_sketch_v5_2.md`. This section supersedes the
"operator's-call question" framing above and corrects two claims I made pre-ruling.

**The seat-theorem reframe (corrects "over-routed").** A verdict is seat-free iff contentless;
**contentful ⇒ seat-dependent** (Coupling Theorem). A topic the situation σ settles by itself
(ISO-8601: "identical information, no shared commitment read differently"; drive-on-right) is
*contentless* → genuinely flat. A topic with any live disagreement (nuclear's risk-tolerance /
growth-vs-degrowth axis; reading-wars' "what reading *is*"; oss's freedom-vs-property) is
*contentful* → necessarily **seated** → admits a foundational construction → **kernel**. So the
primed classifier routing to kernel "whenever a foundational reading is constructible" is **tracking
contentfulness**, which the theorem makes the *only* coherent notion of kernelhood. "Over-routed"
presupposed a **seat-free dominance fact** — "nuclear is *really* a means-dispute" — that the
theorem denies: that judgment is itself a *seated* reading (§8 framing-relativity), and §6 states
the framework "cannot deliver a seat-free ranking of rival selection-premises." So nuclear is not an
error to correct; it is an **unranked inclusion to declare**. The "contest isn't really there"
intuition is a legitimate seat, just not a seat-free fact the engine can act on.

**What the theorem forbids vs what the operator deferred (keep these separate).** The theorem
forbids a **seat-free dominance ranking** ("this topic is *objectively* dominantly foundational") —
§6: no seat-free ranking of rival selection-premises. That is why the LABEL must be demoted
(below). It does NOT forbid a **seated, declared** dominance curation — a human, from the declared
DR seat, judging which kernel-positives to promote is permitted (it is just another declared seat,
not recovered neutrality). So "NO, build a dominance stage" is **deferred, not incoherent**: the
operator's reason is for-now (per your ruling) — the dominance call is shown to be neither a
grounding check nor a manifest predicate (no known instrument), and building it ahead of a witnessed
pile of kernel-positives would be designing the discriminator blind (the measure-first mistake just
avoided). A future seated dominance stage stays open, to be designed *against* the accrued pile.

**Condition 1 — label demotion (required, theorem-forced).** A kernel-positive means **"admits a
foundational construction / is contentful / is seated"** — NOT "is dominantly foundational." Any
downstream count, Tier headline, or essay that reads the kernel set as "certified real foundational
contests" commits the no-seat pose (asserting a seat-free dominance fact). This extends the existing
ceiling language ("not obviously degenerate," never "witnessed kernel") one notch: a kernel-positive
is *admits-a-foundational-construction, dominance unjudged*. The safe-direction argument covers
"never silently lose a flat topic"; it does NOT license "the kernel set is a set of dominant
kernels" — those are different safety claims, and the kernel set is unfiltered for dominance **by
design, declared**.

**Condition 2 — promotion policy = ACCRUE UNCURATED (dominance deferred, declared).** Operator
ruling: kernels accrue uncurated for now; the kernel set is "topics that admit a foundational
construction." A seated dominance curation is *permitted* but *deferred* (build it against a
witnessed pile, not blind). So no human dominance-bottleneck is installed at this stage. The eyeball
is therefore NOT a dominance gate (this corrects my pre-ruling "frequent precision-curation" claim,
which assumed a per-topic dominance call) — its job is the **liveness test** (seat theorem §6 Q5:
"if this vanished, would the world rearrange or stay the same?"): catch the genuinely *contentless*
topic fabricated as contentful (an ISO-8601 wrongly called kernel), not adjudicate which reading
dominates. That is the original coherence backstop, rarer than per-topic dominance-curation. If a
seated dominance stage is ever wanted, it is added later against the accrued pile.

**A3 dropped — record of WHY (so it doesn't read as an unexplained gap).** The plan spent three
rounds building toward A3 (grounding-as-confabulation-flag) as the probable backstop. Phase 0
measured the actual failure mode and it was not fabrication: the over-routed readings have real
constituencies, so grounding would *confirm* them, not flag them — A3 would pass every one clean. A3
is the wrong instrument for kernel-liberality, and kernel-liberality is theorem-correct rather than
a defect needing a backstop, so A3 is removed from the routing path. (Methodology note: this is
measure-first paying out — A3 built on the likely-needs-0b prior would have been a backstop for a
failure that never happened.)

**Net build (smaller than the approved plan):** wire the primed-first router (primed SCOPE verdict
as the gate → `generate_from_manifests` + symmetric forced-flat pair); eyeball = liveness/coherence
(mandatory, not dominance); A3 and the distinctness-predicate machinery dropped; label demoted
everywhere it is recorded; kernels accrue uncurated.
