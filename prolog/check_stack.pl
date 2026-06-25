% ============================================================================
% CHECK STACK — load-path-independent consistency check for the full stack
% ============================================================================
% Why: wrong-module-qualifier calls resolve differently per load path (imports
% from non-module files land in `user`, which all modules inherit from), so a
% call that throws existence_error in a REPL can silently "work" in the
% pipeline and vice versa (docs/technical/swipl_load_path_and_probe_gotchas.md
% §1; OQ-57 lesson). library(check) surfaces undefined-predicate references
% and redefinitions on THIS load (the stack), turning load-chain forensics
% into a runnable command.
%
% Run (engine only):
%   cd prolog && swipl -l check_stack.pl -g "run_check_stack, halt" -t "halt(1)"
% Run (engine + corpus — slower; also checks testset-contributed clauses):
%   cd prolog && swipl -l check_stack.pl -g "run_check_stack_with_corpus, halt" -t "halt(1)"
%
% Reading the output: check/0 prints findings as warnings; an empty run prints
% only the section headers. KNOWN BASELINE (2026-06-04, recorded in
% KNOWN_STATE.md): see the dated entry — new findings beyond that baseline are
% regressions introduced by your change; findings IN the baseline are
% pre-existing and tracked separately. This is a diagnostic, not (yet) a CI
% gate: it is not wired into run_pipeline.py because the baseline is not
% empty. If the baseline is ever cleared, wire it next to the ISSUES.md
% status-grammar gate at run_pipeline entry.
% ============================================================================

:- [stack].
:- use_module(library(check)).

% ---- Side-chain coverage (OQ-57 gap fix, 2026-06-25; KNOWN_STATE same date) ----
% library(check) only sees modules present in the loaded image. run_pipeline.py
% loads several standalone report scripts in SEPARATE swipl processes, OUTSIDE
% [stack], so their qualified calls escape this check unless loaded here too.
% The trajectory-mining chain is loaded faithfully below (mirrors run_pipeline.py
% `_prolog_trajectory`'s module list) so wrong-qualifier rot in
% context_profile_mining.pl / context_profile_report.pl is caught. Witnessed gap:
% the `dirac_classification:standard_context` dangling call that sat unnoticed for
% exactly this reason — those two files are not in [stack] (fix `fc9b4688`).
% Loading the chain adds NO new baseline findings (verified 2026-06-25).
%
% STILL UNCOVERED (honest boundary — extend the same way if one rots): the other
% standalone report scripts run_pipeline loads in their own processes
% (abductive_report, orbit_report, fingerprint_report, isomorphism_report,
% maxent_report, global_delta_report, fpn_report, quantum_verification_report, …).
% Not loaded here because several are non-module scripts that consult into `user`;
% loading them all into one image would cross-contaminate and emit redefinition
% false-positives that never occur in production (one process each). A faithful
% per-chain check needs a fresh process per chain (shell-level loop) — a larger item.
:- use_module(covering_analysis).
:- use_module(dirac_classification).
:- use_module(maxent_classifier).
:- use_module(context_profile_mining).
:- [context_profile_report].

run_check_stack :-
    check.

run_check_stack_with_corpus :-
    corpus_loader:ensure_corpus_loaded,
    check.
