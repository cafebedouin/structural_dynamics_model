# OQ-326 Phase 1 — retroactive census: did any prior audit overlay a RULE?

**Executed:** 2026-08-19 · **Filed under:** OQ-302 (the parent); the question is **OQ-326**
**Raw:** `overlay_template_census_raw.txt` · **Scripts:** `extract_overlay_templates.py`,
`template_safety.pl`, `clause_legality.pl`

**Verdict: no prior audit is voided by the rule-clause mechanism — but the harness's own
documented example is the unsafe form, so the census result is "not yet damaged", not "cannot
happen".**

---

## 0. Why this census exists

`probe_harness:with_overlay/3` snapshots **facts only** (`clause(M:Inst, true)`). A template
naming a rule-defined predicate retracts nothing and only **warns**; asserted facts land after
the existing clauses. So the "counterfactual" arm runs the unmodified program, both arms return
identical results, and **byte-identity — what a clean-vs-edited pair reports as success — is
produced by the overlay never having been installed.** Any prior audit that did this measured
nothing and reported the absence as its result.

## 1. Method, and the one way it can go wrong

A functor-proximity grep over the call sites is **wrong**: it catches predicates in the *Goal*,
which is observation, not overlay. Six rule-bearing predicates surfaced that way
(`drl_core:dr_type/3`, `base_extractiveness/2`, `temporal_residual:residual_for_context/3`,
`stakeholder_seats:derive_directionality_for_stakeholder/3`,
`drl_purity_network:constraint_neighbors/2`, `maxent_classifier:maxent_indexed_run/2`) and **all
six are goal-position** — read, not overlaid. Recorded because the false-positive shape is the
first thing a re-runner will hit.

The census therefore parses **argument positions** (`extract_overlay_templates.py`, balanced-paren
scan):

| call | retract side | assert side |
|---|---|---|
| `with_retracted(Templates, Goal)` | arg 1 | — |
| `with_asserted(Facts, Goal)` | — | arg 1 |
| `with_overlay(Templates, Facts, Goal)` | arg 1 | arg 2 |

Each distinct retract-side template is then **generalized to fresh arguments** and put through
**the harness's own detector, verbatim** (`clause(M:T, Body), Body \== true` —
`probe_harness.pl:83–86`). Generalizing is conservative in the right direction: if the
fully-general form matches no rule clause, no instantiation of it can.

**Instrument control, two-sided, both fired:** the detector **DETECTS**
`boltzmann_invariant_mountain/2` (a known rule) and **DECLINES** `config:param/2` (184 facts, 0
rules). A census that could not do both would license nothing.

## 2. Result — 44 call sites, 27 files, 13 distinct retract-side templates

**12 of 13 are rule-free**, so those overlays are structurally safe:
`narrative_ontology:constraint_metric/3`, `constraint_victim/2`, `constraint_beneficiary/2`,
`constraint_claim/2`, `measurement/5`, `stakeholder_gain_flow/2`, `fixing_cost_class/2`,
`cs_authority_grounding/2`, `founding_problem_status/2`, `disappearance_verdict/2`,
`constraint_stakeholder/7`, and `config:param/2`.

**1 of 13 is rule-bearing:** `constraint_indexing:constraint_classification/3`, used at exactly
one site — `audits/2026-06-07_stakeholder_layer_migration/a1_probe.pl:77` (`a1_mut_perspective`,
the "flip authored P1 snare → mountain" mutation).

**That site is SAFE — checked, not assumed.** All rule clauses of that predicate are hard-keyed
to the two engine demo constraints from `constraint_instances.pl`:

```
HEAD: cc(catholic_church_1200, mountain, context(...))   BODY: effective_immutability_for_context(...)
HEAD: cc(catholic_church_1200, snare,    context(...))   BODY: member(...), base_extractiveness(...) > 0.6, ...
HEAD: cc(property_rights_2025, snare,    context(...))   BODY: member(...), extractiveness_for_agent(...) > 0.7, ...
   (6 rule clauses total, all on those two ids)
```

`a1_probe`'s template binds `C` to a **corpus** constraint, which cannot unify with a head keyed
to `catholic_church_1200` or `property_rights_2025`. Its snapshot therefore took the authored
FACT and the overlay behaved as intended.

## 3. The live trap: the harness's own header example is the unsafe form

`probe_harness.pl`'s usage example is

```prolog
?- probe_harness:with_retracted(
       [constraint_indexing:constraint_classification(_, mountain,
            context(agent_power(analytical), _, exit_options(analytical), _))],
       my_probe_goal).
```

— the same predicate with the **first argument unbound**, which *does* unify with
`catholic_church_1200`'s rule clause and would partially overlay it. **The one documented example
a future probe author copies is the one form the census flags.** The scope-limits paragraph lower
in the same header names this exact predicate as the hazard, so the file both warns and
demonstrates. Repair is OQ-326 Phase 3.

## 4. What this census does NOT cover — the general empty-snapshot class

Rule-ness is one way a snapshot comes back empty. Others: an undefined predicate in that program,
a mismatched arity, an unloaded corpus, an absent constraint id. **All of them retract nothing and
produce the same silent identical-arms result**, and the harness verifies *restore*, never
*install*. Some probes wrote their own install assertion (`oq110`'s Control C asserts the flip
*disappears* under retraction; `oq35`'s `with_retracted([], …)` null control is byte-identical by
design); the rest relied on the harness. That census is **OQ-326 Phase 2** and is open.

## 5. Appendix — why `clause/2` was legal in the OQ-302 probe

The OQ-302 substitute mechanism fetches the engine's own clause body from a **static** predicate.
That is not a local accident and not a side effect of the probe:

```
flag access_level = user
flag protect_static_code = false          <- SWI default; this is what permits it
bim/2: lacks dynamic, HAS static, HAS defined, lacks foreign, lacks built_in, clauses=2
clause/2 on bim/2                    -> ok
clause/2 on system:atom_length/2     -> THREW permission_error(access, private_procedure, atom_length/2)
"did anything make bim/2 dynamic?"   -> STATIC — clause/2 read a compiled static predicate
```

Nothing in the probe made the predicate dynamic. `clause/2` on a **static user** predicate is
permitted whenever `protect_static_code` is `false` (the SWI default); the refusal path is real
and two-sided — a foreign built-in still raises `permission_error`. **Condition to record with the
guidance: it depends on `protect_static_code = false`.** If that flag is ever set true, or the
target is foreign/built-in, `clause/2` refuses **loudly**, which is the safe failure direction.

**And the measured program did not differ from the shipped one in the dimension the second witness
was meant to close:** the OQ-302 repair changed clause **1** only (`git diff` — one line plus
comments); clause 2, whose body the probe fetched and called, is byte-identical before and after.
The 5311/5311 agreement between the composed arm and the shipped clause is consistent with that,
and is what closes the gap.
