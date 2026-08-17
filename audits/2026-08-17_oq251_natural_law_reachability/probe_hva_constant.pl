% probe_hva_constant.pl — OQ-251 audit, post-close thread (operator-raised 2026-08-17)
%
% QUESTION: is has_viable_alternatives/2 a CONSTANT FUNCTION corpus-wide?
% P6(b) returned [unknown] on kernel_v1 with COUNT_true=0. If the `true` branch is
% also dead on every leg, the finding is not about the `natural_law` atom at all —
% every consumer of the PREDICATE reads a constant, and OQ-296's scope was drawn
% one level too narrow.
%
% Run from prolog/, once per leg (the loader is not re-entrant across corpus_path
% overlays within one process — see run_all.sh sibling note in the audit dir).
%
%   swipl -g "[stack], ['../audits/2026-08-17_oq251_natural_law_reachability/probe_hva_constant'], \
%             run_hva('<leg>'), halt" -t "halt(1)"

:- use_module(library(aggregate)).

run_hva(Leg) :-
    retractall(config:param(corpus_path, _)),
    asserta(config:param(corpus_path, Leg)),
    corpus_loader:load_all_testsets,
    aggregate_all(count, corpus_loader:corpus_constraint(_), N),

    aggregate_all(set(V),
                  ( corpus_loader:corpus_constraint(C),
                    signature_detection:has_viable_alternatives(C, V) ),
                  Range),
    aggregate_all(count,
                  ( corpus_loader:corpus_constraint(C1),
                    signature_detection:has_viable_alternatives(C1, true) ),
                  NTrue),
    aggregate_all(count,
                  ( corpus_loader:corpus_constraint(C2),
                    signature_detection:has_viable_alternatives(C2, unknown) ),
                  NUnk),
    aggregate_all(count,
                  ( corpus_loader:corpus_constraint(C3),
                    signature_detection:has_viable_alternatives(C3, false) ),
                  NFalse),

    % How far does clause 1 get? Conjunct 1 (affects_constraint) vs conjunct 2.
    aggregate_all(count,
                  ( corpus_loader:corpus_constraint(C4),
                    narrative_ontology:affects_constraint(_, C4) ),
                  NAffects),
    aggregate_all(count, narrative_ontology:intent_viable_alternative(_, _, _), NIntent),

    (   Range == [unknown]
    ->  Verdict = 'CONSTANT (unknown) — both branches dead'
    ;   Range == [true] -> Verdict = 'CONSTANT (true)'
    ;   Verdict = 'NON-CONSTANT'
    ),

    format('~w~n', [Leg]),
    format('  n_constraints              = ~w~n', [N]),
    format('  range                      = ~q~n', [Range]),
    format('  count(true)                = ~w~n', [NTrue]),
    format('  count(unknown)             = ~w~n', [NUnk]),
    format('  count(false)               = ~w~n', [NFalse]),
    format('  constraints w/ affects_constraint (clause-1 conjunct 1) = ~w~n', [NAffects]),
    format('  intent_viable_alternative/3 facts loaded (conjunct 2)   = ~w~n', [NIntent]),
    format('  VERDICT: ~w~n~n', [Verdict]).
