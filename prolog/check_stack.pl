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

run_check_stack :-
    check.

run_check_stack_with_corpus :-
    corpus_loader:ensure_corpus_loaded,
    check.
