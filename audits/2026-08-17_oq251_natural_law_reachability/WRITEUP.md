# OQ-251 — no path to `natural_law` certification exists, and the blocker was never OQ-70: the binding commit is `8b5a34b8`, pinned by bisect

**Executed:** 2026-08-17
**OQ:** OQ-251 (resolved), OQ-248 (kill-condition disposition), OQ-113 (range re-witnessed),
OQ-266 (one evidence line), OQ-296 (minted — the consumer surface)
**Verdict:** No path by which a paradigm natural law certifies `natural_law` remains at HEAD —
`natural_law_signature/1` is unsatisfiable **by construction**, blocked by exactly ONE conjunct
(`HasAlternatives == false`), and the un-certification was ruled by OQ-43/OQ-44 (`8b5a34b8`,
2026-06-11), **not** by OQ-70 (`72ec2cdd`, 2026-06-05) as the record said; OQ-248's kill condition
was evaluated and did **not** trip.
**Substrate:** `no pipeline run` (all probes are in-session swipl or git reads). Corpora:
`prolog/archives/datasets/kernel_v1` n=1106 (loader-reported and disk-counted), live default leg
`prolog/testsets/` n=273. Engine at `5bc7b0db` → `f64384d3` (another instance committed mid-audit;
the two are **byte-identical over `prolog/` and `python/`**, verified, so no probe is affected — see
the CORRECTION block at the top of `audit_log.md`, which also records that this log's original
"no concurrent writer" line was wrong). Bisect points `f600599b`, `a4297632` (=`8b5a34b8^`),
`8b5a34b8`; counterfactual tree = HEAD + one line.
**Fired:** live — the recorded cause of a shipped ruling's premise was wrong and is corrected in
substrate (`prolog/narrative_ontology.pl`, `GATE2_REWITNESS.md`); OQ-251's own "two independent
blockers" text is falsified (one blocker); an FNL control that read as passing was one-sided and
had to be re-run to license anything; three drifted code cites repaired.
**Evidence map:**
- `PREREGISTRATION.md` — outcome tables for Phases 1, 1.5, 2, 3, frozen before any probe; md5
  `f7336ee740ba2a7b1a24a7c081e38aca`.
- `audit_log.md` — every probe's pasted output in execution order; the md5 line sits physically
  above the first result. Witnesses every claim below.
- `probe_p4_conjuncts.pl` — P4 per-conjunct attribution + P6(b) corpus range enumeration.
- `probe_p456_head.pl` — P6(a) runtime `listing/1` + two-sided range control; P4b (FNL);
  P5a/P5b(i).
- Bisect + counterfactual scratch trees are **not** archived here (3 × ~800 MB `git archive`
  checkouts). They are exactly reproducible from the commit SHAs and the commands pasted in
  `audit_log.md` → Phase 1.5; the SHAs, the corpus md5s, and every query result are recorded there.

---

## The two questions

**Q1 (Ω_E) — does any path remain? NO, and for a reason the OQ did not name.**

`natural_law_signature/1` requires `HasAlternatives == false`. The builder that fills that slot has
exactly two clauses, and both bind the second argument to a **literal in the head**:

```prolog
has_viable_alternatives(C, true) :-                     % literal `true`
    narrative_ontology:affects_constraint(I, C),
    narrative_ontology:intent_viable_alternative(I, _, _), !.
has_viable_alternatives(_, unknown).                    % literal `unknown`
```

No callee binds the output, so the range is `{true, unknown}` **by construction** — not merely
"unauthored on this corpus." Runtime `listing/1` confirms 2 clauses, static, non-multifile (so no
source read is being fooled by an asserted or multifile clause). This is the **stronger** of the two
altitudes the plan allowed; no downgrade to "unreachable over the authored field domain" is owed.

The control is two-sided, which matters: a probe showing only "no `false` appeared" cannot separate
*unreachable* from *never dispatched*. Authoring the one input clause 1 reads, on a synthetic
constraint, yields `[true]`; unauthored yields `[unknown]`; restore returns `[unknown]`. The true
branch is demonstrably reachable and `false` is emitted by neither branch. Corpus enumeration
corroborates: range `[unknown]` on kernel_v1, 0 `false`, no OQ-113 regression.

**Q2 (Ω_C) — was the scope ruled or an unchosen side effect? RULED, on the record, three times —
and never by OQ-70.**

- **OQ-70** (`72ec2cdd`): scope is claims-side only. Its ISSUES entry and commit body name only
  `claimed_natural` source 2 and `appears_as_rope`'s sibling; every witness is an FCR/FNL count; its
  positive control is an FNL firing. `natural_law` certification is not mentioned.
- **OQ-43/OQ-44** (`8b5a34b8`): "**FAIL-CLOSED, output-changing, un-certification accepted**",
  with the casualty named (`thermal_dissipation_constraint`) and GAP-08 cited.
- **OQ-113** (2026-06-18): fork (b) — "document builder-unreachability; no logic change."

So the un-certification was chosen deliberately, then its permanence re-ruled deliberately. **E3
does not trip; no new operator ruling is needed for Q2's scope question.**

## Q1's premise was false: the attribution the OQ inherited

OQ-251, `GATE2_REWITNESS.md`, and `narrative_ontology.pl` all attributed maxwell's un-certification
to OQ-70. **A three-point behavioral bisect pins `8b5a34b8` instead.** The corpus is held constant
by construction — maxwell's file is byte-identical (`9178deb2…`) at all three commits *and* in the
live worktree, and it did not churn inside the window:

| Point | Commit | `has_viable_alternatives` | `constraint_signature(maxwell, S)` |
|---|---|---|---|
| 1 | `f600599b` (pre-both) | `false` | `[natural_law]` |
| 2 | `8b5a34b8^` = `a4297632` (**post-`72ec2cdd`**) | `false` | `[natural_law]` |
| 3 | `8b5a34b8` (post-fail-close) | `unknown` | `[coupling_invariant_rope]` |

**Point 1 is a reproduction of `GATE2_REWITNESS.md`'s own arm C**, so the scratch-tree apparatus is
validated against a known result before anything downstream is read off it.

The attribution fails on three independent counts, each separately witnessed:

1. **Mechanism.** `claimed_natural/2` was never on the `natural_law` path — in the era engine or at
   HEAD. Both producers of the atom (`constraint_signature/2` `:114-117`, `classify_by_signature/3`
   `:323-324`) gate on `natural_law_signature/1`, the profile path. Era-wide, the *only* executable
   consumer of `claimed_natural/2` is `false_natural_law/2`. The dependency arrow in fact runs the
   other way: `claimed_natural` clause 3 **calls** `natural_law_signature`.
2. **Chronology.** The bisect above. The prior era-vs-HEAD isolation spanned **126 commits**
   including both candidates; naming one was an inference that isolation never licensed.
3. **maxwell's own data.** It authors an explicit story-level claim
   (`maxwell_demon_impossibility.pl:114`, `constraint_claim(…, mountain)`), so `claimed_natural`
   source 1 fires for it regardless — removing source 2 was **inert for maxwell even on the claims
   side**. At HEAD `claimed_natural(maxwell, X)` = `explicit_mountain_claim`.

## One blocker, not two — OQ-251's own text corrected

OQ-251's entry says the outcome is "over-determined by TWO independent blockers (the OQ-70 source
removal AND the HasAlternatives conjunct)." Per-conjunct evaluation at HEAD over kernel_v1:

```
PROFILE = profile(0.92, 0.02, 0.08, 0, unknown, stable, …)
C0 emerges_naturally (producer guard) : pass
C1 number/1 guards                    : pass
C2 AccessCollapse >= 0.85             : pass   0.92
C3 Suppression    =< 0.15             : pass   0.02
C4 Resistance     =< 0.15             : pass   0.08
C5 BeneficiaryCount == 0              : pass   0
C6 HasAlternatives == false           : FAIL   unknown      <-- the only one
C7 TemporalStability == stable        : pass   stable
```

`BeneficiaryCount == 0` **passes**. There is one blocker, and the first named "blocker" was never
one. Confirmed end-to-end two ways: substituting only the `HasAlternatives` slot in maxwell's real
authored profile makes `natural_law_signature/1` fire; and a HEAD scratch tree with that single line
reverted re-certifies maxwell through the full `constraint_signature/2` producer path (30 kernel_v1
constraints certify under the counterfactual).

## The consumer surface is worse than the OQ supposed — minted as OQ-296

OQ-251 hypothesized the detector had "drifted from measuring structural naturality to measuring
authorial declaration." It has not. **It measures nothing:** `constraint_signature(_, natural_law)`
= **0** on both corpora, while 273/273 and 1106/1106 constraints carry *some* signature. ~20
consumers read that constant zero, and at least two convert it into plausible non-zero output:

- `python/container_typology_analysis.py:151-157` — the `formalization` axis's `formal_nl` disjunct
  can never fire and its `textual_combined` sum is silently mountain-only; the axis still emits a
  plausible label.
- `python/extract_corpus_data.py:145` — `is_constructed = sig not in ('natural_law',)` is **True
  corpus-wide**.
- `python/linter.py:684,719` — an advisory whose stated mechanism is stale (`get_metric_average`
  no longer "defaults to 0.5"; OQ-44 disposition (1), `966d53c8`, made it the `unknown` sentinel)
  and whose remedy ("Add the missing declarations") cannot restore certification, because the
  binding conjunct is one it never mentions. **Logged, not fixed** (plan step 18).
- `routing_sink.pl:120` — 273/273 read `nl_absent`. This one is honest: the module declares itself
  a socketed router input.

**Scoping guard.** `drl_core:natural_law_without_beneficiary/1` is a *different predicate* in a
different module that never reads the signature atom, and it is **live** — 30 firings on kernel_v1,
0 on the live leg (two-sided, so the live-leg zero is a corpus property, not a dead path). Its ~15
consumers are **out of scope**; folding them in would inflate the finding. `should_be_natural_law/1`
is separately dead in both senses: 0 firings on both corpora *and* 0 consumers repo-wide.

## What did not happen

- **E1** — no live path; `natural_law` absent pre-injection.
- **E2** — bisect landed cleanly on a candidate (outcome A).
- **E3** — the three ruling texts support the chosen-not-side-effect read.
- **E4** — FNL does not fire on maxwell. The claim *is* read (`explicit_mountain_claim`); the
  **Boltzmann compliance gate** is what holds (`compliant(0)`). No false positive on the reference
  genuine law.

**The FNL control had to be repaired mid-run and this is worth recording.** The in-corpus control
first returned "FNL fires on 0 constraints corpus-wide" — one-sided, and on kernel_v1 alone
"maxwell doesn't fire" was indistinguishable from "the detector is dead." Re-run on the live leg at
HEAD it fires on 3 of 21 claim-bearing constraints and declines on the other 18 — naturally-arising
in both directions, neither planted. maxwell's decline is in distribution for that record, so the
non-firing carries information.

## For the operator (E5 — surfaced, not decided)

The 2026-07-25 gate-2 ruling for `non_agent_beneficiary(entropic_universe_hypothesis)` had its
**stated evidential basis corrected post-hoc** by this audit. The ruling's *substance* — the
narrative/omega-aboutness discriminator — is untouched by the correction, and its kill condition was
evaluated and did not trip. Whether a ruling whose recorded basis needed correcting warrants an
explicit re-affirmation is the operator's call (an OQ-252 instance: rulings carry no back-reference
to what they license). **Executor's default recommendation: the ruling stands.**

## Residue

Substrate changed by this audit:

- `prolog/narrative_ontology.pl` — registry-note attribution corrected (marked AS a correction);
  kill-condition disposition added (evaluated, did not trip); E5 surfaced at the site. Comment-only.
- `audits/2026-07-25_oq66_nlwb_filter_cutover/GATE2_REWITNESS.md` — dated **Correction** block
  appended; the original is point-in-time and was not rewritten.
- `prolog/signature_detection.pl`, `prolog/tests/test_oq113_dead_natural_law.pl` — three drifted
  cites refreshed, each target re-verified at edit time. Comment-only; OQ-113 suite 3/3 green
  post-edit.
- `ISSUES.md` — OQ-251 → resolved (its own two-blocker paragraph corrected in place, marked);
  OQ-248 disposition cross-ref; OQ-266 evidence line; **OQ-296 minted** (the consumer surface).
- `KNOWN_STATE.md` — dated entry.
- `python/linter.py` — **NOT edited**, logged only, per plan step 18.

No engine-behavior change anywhere in this audit: every `.pl` edit is comment-only (0 non-comment
lines changed), and the counterfactual lived in a scratch tree — the live
`prolog/signature_detection.pl` md5 is unchanged (`1c58deb9…`) from before the probes.

**Next forward move, for a cold reader:** OQ-296 carries the consumer roster. The engine-side
question it does *not* settle is whether the `natural_law` leg should be powered, retired, or left
as a declared-dark socket — that decision is gated on **GAP-08 §7** (the author-independent
immovability signal), which does not exist, and OQ-113 already ruled fork (b) (document, don't
change). So OQ-296 is a *consumer honesty* question, not a re-opening of OQ-113: the ~20 readers of
a constant zero can be made honest without powering anything.
