% probe_p4_conjuncts.pl — OQ-251 audit, Phase 2 / P4 + P6(b)
%
% Per-conjunct attribution of natural_law_signature/1 on maxwell_demon_impossibility
% at HEAD, over the kernel_v1 archive corpus. Also P6(b): the corpus-wide range
% enumeration of has_viable_alternatives/2 as CORROBORATION of the P6(a) clause read.
%
% Run from prolog/:
%   swipl -g "['../audits/2026-08-17_oq251_natural_law_reachability/probe_p4_conjuncts'], \
%             run_p4, halt" -t "halt(1)"

:- use_module(library(aggregate)).

overlay_kernel_v1 :-
    retractall(config:param(corpus_path, _)),
    asserta(config:param(corpus_path, 'archives/datasets/kernel_v1')),
    corpus_loader:load_all_testsets.

pf(Goal, pass) :- catch(Goal, _, fail), !.
pf(_, 'FAIL').

run_p4 :-
    overlay_kernel_v1,
    aggregate_all(count, corpus_loader:corpus_constraint(_), N),
    format('>>> CORPUS_N=~w~n', [N]),

    signature_detection:get_constraint_profile(maxwell_demon_impossibility, P),
    format('>>> PROFILE=~q~n', [P]),
    P = profile(AC, Su, Re, BC, HA, TS, CS),

    config:param(natural_law_collapse_min, CMin),
    config:param(natural_law_suppression_max, SMax),
    config:param(natural_law_resistance_max, RMax),
    format('>>> params: collapse_min=~q suppression_max=~q resistance_max=~q~n',
           [CMin, SMax, RMax]),

    % C0 is the producer-clause guard, not part of natural_law_signature/1 itself.
    pf(domain_priors:emerges_naturally(maxwell_demon_impossibility), R0),
    format('>>> C0 emerges_naturally (producer guard) : ~w~n', [R0]),

    pf((number(AC), number(Su), number(Re)), R1),
    format('>>> C1 number/1 guards                    : ~w   AC=~q Su=~q Re=~q~n',
           [R1, AC, Su, Re]),
    pf(AC >= CMin, R2),
    format('>>> C2 AccessCollapse >= ~q               : ~w   value=~q~n', [CMin, R2, AC]),
    pf(Su =< SMax, R3),
    format('>>> C3 Suppression =< ~q                  : ~w   value=~q~n', [SMax, R3, Su]),
    pf(Re =< RMax, R4),
    format('>>> C4 Resistance =< ~q                   : ~w   value=~q~n', [RMax, R4, Re]),
    pf(BC == 0, R5),
    format('>>> C5 BeneficiaryCount == 0              : ~w   value=~q~n', [R5, BC]),
    pf(HA == false, R6),
    format('>>> C6 HasAlternatives == false           : ~w   value=~q~n', [R6, HA]),
    pf(TS == stable, R7),
    format('>>> C7 TemporalStability == stable        : ~w   value=~q~n', [R7, TS]),
    format('>>> (CoordinationSuccess is _ in the NL head, unused: ~q)~n', [CS]),

    (   signature_detection:natural_law_signature(P)
    ->  RW = 'FIRES'
    ;   RW = 'does not fire'
    ),
    format('>>> WHOLE natural_law_signature/1         : ~w~n', [RW]),

    % Whole-signature query on the constraint (producer path).
    findall(S, signature_detection:constraint_signature(maxwell_demon_impossibility, S), Ss),
    format('>>> HEAD_SIGS=~q~n', [Ss]),

    % ---- P6(b): corpus-wide range of has_viable_alternatives/2 (CORROBORATION only) ----
    aggregate_all(set(V),
                  ( corpus_loader:corpus_constraint(C),
                    signature_detection:has_viable_alternatives(C, V) ),
                  Vs),
    format('>>> P6b_RANGE_kernel_v1=~q~n', [Vs]),
    aggregate_all(count,
                  ( corpus_loader:corpus_constraint(C2),
                    signature_detection:has_viable_alternatives(C2, false) ),
                  NFalse),
    format('>>> P6b_COUNT_false=~w   (any nonzero = OQ-113 regression, HALT)~n', [NFalse]),
    aggregate_all(count,
                  ( corpus_loader:corpus_constraint(C3),
                    signature_detection:has_viable_alternatives(C3, true) ),
                  NTrue),
    format('>>> P6b_COUNT_true=~w~n', [NTrue]),

    % P6(b) positive control: the enumeration CAN see a `false` if one exists.
    % Assert one at the front, re-enumerate, retract, re-enumerate.
    asserta(signature_detection:has_viable_alternatives('__p6b_control__', false)),
    assertz(corpus_loader:corpus_constraint('__p6b_control__')),
    aggregate_all(set(V2),
                  ( corpus_loader:corpus_constraint(C4),
                    signature_detection:has_viable_alternatives(C4, V2) ),
                  Vs2),
    format('>>> P6b_CONTROL_planted_range=~q   (must contain false)~n', [Vs2]),
    retract(corpus_loader:corpus_constraint('__p6b_control__')),
    retract(signature_detection:has_viable_alternatives('__p6b_control__', false)),
    aggregate_all(set(V3),
                  ( corpus_loader:corpus_constraint(C5),
                    signature_detection:has_viable_alternatives(C5, V3) ),
                  Vs3),
    format('>>> P6b_CONTROL_restored_range=~q  (must equal P6b_RANGE)~n', [Vs3]).
