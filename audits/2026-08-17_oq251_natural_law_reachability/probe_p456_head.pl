% probe_p456_head.pl — OQ-251 audit, Phase 2 / P4b, P5, P6(a)
%
% Runs at HEAD over the kernel_v1 archive corpus. Three probes:
%
%   P6(a) — runtime `listing/1` of has_viable_alternatives/2 (the RUNTIME clause
%           set, which a source read could miss if another module asserted or
%           multifile-extended it), plus the discriminating control below.
%   P4b   — false_natural_law/2 and boltzmann_compliant/2 on maxwell.
%   P5    — pre-injection signature query (P5a) and the profile-slot substitution
%           (P5b(i)): maxwell's REAL authored profile with ONLY the
%           HasAlternatives slot replaced by `false`.
%
% NOTE (executor, 2026-08-17): the audit plan's P5 called for
% `asserta(signature_detection:has_viable_alternatives(maxwell..., false))`.
% That cannot execute — the predicate is STATIC (no `:- dynamic` anywhere in
% signature_detection.pl); SWI refuses with "No permission to modify static
% procedure". P5's intent is preserved by (i) the slot substitution below and
% (ii) a scratch-tree HEAD counterfactual (probe_p5_scratch, one-line clause
% edit in a `git archive` copy) which exercises the FULL producer path. The live
% repo is never mutated, so P5c's restore witness becomes an untouched-file md5.
%
% Run from prolog/:
%   swipl -g "[stack], ['../audits/2026-08-17_oq251_natural_law_reachability/probe_p456_head'], \
%             run_p456, halt" -t "halt(1)"

:- use_module(library(aggregate)).

overlay_kernel_v1 :-
    retractall(config:param(corpus_path, _)),
    asserta(config:param(corpus_path, 'archives/datasets/kernel_v1')),
    corpus_loader:load_all_testsets.

run_p456 :-
    overlay_kernel_v1,
    aggregate_all(count, corpus_loader:corpus_constraint(_), N),
    format('>>> CORPUS_N=~w~n~n', [N]),

    % ---------------- P6(a): runtime clause set ----------------
    format('>>> ===== P6a: listing(signature_detection:has_viable_alternatives/2) =====~n'),
    listing(signature_detection:has_viable_alternatives/2),
    (   predicate_property(signature_detection:has_viable_alternatives(_, _), dynamic)
    ->  format('>>> P6a_property: DYNAMIC~n')
    ;   format('>>> P6a_property: static~n')
    ),
    (   predicate_property(signature_detection:has_viable_alternatives(_, _), multifile)
    ->  format('>>> P6a_multifile: YES (a source read could miss clauses)~n')
    ;   format('>>> P6a_multifile: no~n')
    ),
    predicate_property(signature_detection:has_viable_alternatives(_, _),
                       number_of_clauses(NC)),
    format('>>> P6a_number_of_clauses=~w~n', [NC]),

    % P6(a) DISCRIMINATING control: author the ONLY input clause 1 reads, on a
    % synthetic constraint, and show the builder still cannot emit `false` — it
    % emits `true`. This is the two-sided check: the clause set has a reachable
    % `true` branch (so the predicate is not simply dead), and `false` is
    % reachable from neither branch.
    assertz(narrative_ontology:affects_constraint('__p6a_intent__', '__p6a_c__')),
    assertz(narrative_ontology:intent_viable_alternative('__p6a_intent__', alt, 0.5)),
    findall(V, signature_detection:has_viable_alternatives('__p6a_c__', V), Authored),
    format('>>> P6a_CONTROL authored-input synthetic  : ~q  (true branch IS reachable)~n',
           [Authored]),
    findall(V2, signature_detection:has_viable_alternatives('__p6a_unauthored__', V2), Unauth),
    format('>>> P6a_CONTROL unauthored synthetic      : ~q~n', [Unauth]),
    retract(narrative_ontology:affects_constraint('__p6a_intent__', '__p6a_c__')),
    retract(narrative_ontology:intent_viable_alternative('__p6a_intent__', alt, 0.5)),
    findall(V3, signature_detection:has_viable_alternatives('__p6a_c__', V3), Restored),
    format('>>> P6a_CONTROL restored                  : ~q~n~n', [Restored]),

    % ---------------- P4b: FNL + Boltzmann on maxwell ----------------
    format('>>> ===== P4b: FNL on the reference genuine law =====~n'),
    (   catch(signature_detection:false_natural_law(maxwell_demon_impossibility, E),
              Err,
              ( print_message(error, Err), fail ))
    ->  format('>>> P4b_FNL: FIRES  evidence=~q~n', [E])
    ;   format('>>> P4b_FNL: does not fire~n')
    ),
    (   catch(signature_detection:claimed_natural(maxwell_demon_impossibility, CN),
              Err2, ( print_message(error, Err2), fail ))
    ->  format('>>> P4b_claimed_natural: ~q~n', [CN])
    ;   format('>>> P4b_claimed_natural: FAILS~n')
    ),
    (   catch(boltzmann_compliance:boltzmann_compliant(maxwell_demon_impossibility, BR),
              Err3, ( print_message(error, Err3), fail ))
    ->  format('>>> P4b_boltzmann_compliant: ~q~n', [BR])
    ;   format('>>> P4b_boltzmann_compliant: FAILS (no result)~n')
    ),
    % FNL positive control: the detector fires on a claim-mountain,
    % Boltzmann-non-compliant constraint if one exists in this corpus.
    findall(FC,
            ( corpus_loader:corpus_constraint(FC),
              signature_detection:false_natural_law(FC, _) ),
            FnlFires),
    length(FnlFires, NFnl),
    format('>>> P4b_CONTROL FNL fires on ~w constraint(s) corpus-wide~n', [NFnl]),
    (   FnlFires = [Ex | _]
    ->  format('>>> P4b_CONTROL example firing: ~q  (detector demonstrably live)~n', [Ex])
    ;   format('>>> P4b_CONTROL: NO firings — FNL is silent on this corpus (one-sided)~n')
    ),
    nl,

    % ---------------- P5a / P5b(i) ----------------
    format('>>> ===== P5a: pre-injection (E1 check) =====~n'),
    findall(S, signature_detection:constraint_signature(maxwell_demon_impossibility, S), Pre),
    format('>>> P5a_SIGS=~q   (natural_law present => E1 HALT)~n', [Pre]),
    (   memberchk(natural_law, Pre)
    ->  format('>>> P5a_VERDICT: *** E1 TRIPPED *** natural_law fires pre-injection~n')
    ;   format('>>> P5a_VERDICT: no natural_law pre-injection (E1 does not trip)~n')
    ),
    nl,
    format('>>> ===== P5b(i): profile-slot substitution =====~n'),
    signature_detection:get_constraint_profile(maxwell_demon_impossibility, P0),
    format('>>> P5b_real_profile=~q~n', [P0]),
    P0 = profile(AC, Su, Re, BC, HA0, TS, CS),
    format('>>> P5b_slot_replaced: HasAlternatives ~q -> false (all other slots authored)~n',
           [HA0]),
    P1 = profile(AC, Su, Re, BC, false, TS, CS),
    (   signature_detection:natural_law_signature(P1)
    ->  format('>>> P5b_RESULT: natural_law_signature/1 FIRES on the substituted profile~n')
    ;   format('>>> P5b_RESULT: still does NOT fire — a second blocker exists~n')
    ),
    % And the producer clause's other guard, evaluated independently:
    (   domain_priors:emerges_naturally(maxwell_demon_impossibility)
    ->  format('>>> P5b_producer_guard emerges_naturally: pass~n')
    ;   format('>>> P5b_producer_guard emerges_naturally: FAIL~n')
    ).
