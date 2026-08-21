"""corpus_legs.py — the canonical list of LIVE corpus legs (OQ-306).

Constants only. NOTHING executes at import: this module is imported by checkers
and by run_pipeline.py alike, and a checker that triggers work at import time
inverts the dependency it exists to inspect.

WHY THIS FILE EXISTS (OQ-306 R-B, ruled 2026-08-21). The membership-kinding
refusal in run_pipeline.py is HARD (SystemExit) on the live legs and a LOUD
CONTINUE elsewhere. That split needs a canonical list, and the list must not
live in a checker (wrong direction) or in run_pipeline.py (a checker importing
the pipeline inverts the dependency).

WHY THE SPLIT IS SCOPED THIS WAY, with the numbers that decided it. A skewed
member — one whose in-file `constraint_metric` subject differs from its
filename — kinds `unknown`, because corpus_loader:has_story_facts/1 queries
`constraint_metric(<basename>, _, _)`. Such a file HAS a constraint_metric,
just not one keyed on its own name. Re-derived 2026-08-21 (NOT recalled;
audits/2026-08-21_oq306_denominator_census/rb_skew_rederived.txt):

    original_v5              91/702   13.0%  would kind unknown
    original_json/testsets  133/1151  11.6%  would kind unknown
    original_v6               0/3380   0.0%
    kernel_v1                 0/1106   0.0%
    testsets (live)              0      0.0%  <- control

A hard refusal everywhere would therefore refuse two archived corpora outright
and make legitimate retro-audits impossible. Hence: hard on the live legs,
where zero unknowns is the standing expectation and a nonzero count is a real
finding; loud continue on everything else, where a nonzero count is a known
property of the data.
"""

# The five live corpus legs, as `corpus_path` values relative to prolog/.
#
# `testsets` is the live leg and MOVES CONTINUOUSLY (operator ruling
# 2026-08-18: it carries no count, ever — count it, never recall it). The other
# four are finished static corpora. Membership in this list is about REFUSAL
# SCOPE, not about size or stability.
LIVE_LEGS = (
    "testsets",
    "testsets_haiku",
    "testsets_flash",
    "testsets_kimi",
    "testsets_sonnet",
)

# NOTE ON module_boundary_check.ALL_ARM_C_LEGS, which now references this
# constant. The two lists are COEXTENSIVE TODAY and DISTINCT IN PRINCIPLE:
# arm C asks "which legs do I scan for authored schema heads", this asks "which
# legs does a membership-kinding failure HALT". Binding one to the other as if
# they were the same question would be a false unification (build_discipline ->
# over-confident move 2). The reference exists so the five names have one home;
# if the questions ever come apart, fork the lists and say why at both sites.
