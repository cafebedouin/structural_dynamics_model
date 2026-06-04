% ============================================================================
% test_agent_beneficiary.pl — FSM agency gate (June 2026) regression tests.
%
% Covers the two-site narrowing of beneficiary-presence gates to
% narrative_ontology:agent_beneficiary/2 (FSM gate + count_power_beneficiaries)
% and the drl_core.pl:287 (natural_law_without_beneficiary) deferral.
%
% Three layers:
%   (a) Positive control — maxwell_demon_impossibility certifies natural_law
%       and classifies mountain at all 4 canonical contexts post-fix.
%   (b) Per-item non-regression — the 11 agent/unlisted-beneficiary FSM
%       constraints STILL satisfy false_summit_mountain/2. An all-zeroed
%       result here is a regression, not a clean pass.
%   (c) :287 inertness tripwire — DIRECT test of the deferral's precondition
%       ("filtering natural_law_without_beneficiary changes no final
%       classification today"), NOT a symptom proxy. natural_law_without_
%       beneficiary has THREE classification-path consumers: snare block
%       (drl_core.pl:333), tangled_rope block (drl_core.pl:362), and the
%       MaxEnt shadow forbidden-features for snare/tangled_rope
%       (maxent_classifier.pl:173,176,191). Rather than guessing which type
%       is the symptom, this test classifies every divergence candidate
%       (raw/filtered :287 truth differs) under BOTH reads — dr_type at the
%       4 canonical contexts AND maxent_top_type — via a test-local
%       redefinition with guaranteed restore, and fails loudly on the first
%       divergence. When this test starts failing, the :287 deferral is
%       stale: see the agency-gate OQ in ISSUES.md.
%
% Run: cd prolog && swipl -g "[stack], [tests/test_agent_beneficiary], run_tests, halt" -t "halt(1)"
% ============================================================================

:- corpus_loader:ensure_corpus_loaded.

:- begin_tests(agent_beneficiary).

% ----------------------------------------------------------------------------
% Registry / view dispatch controls
% ----------------------------------------------------------------------------

% The filtered view must (i) drop maxwell's proposition-kind sole beneficiary,
% (ii) keep humane_treatment's agent co-beneficiary, (iii) drop its
% proposition-kind one. (i)+(iii) prove the filter filters; (ii) proves it
% passes agents — the probe can both flag and clear.
test(agent_beneficiary_view_dispatch) :-
    \+ narrative_ontology:agent_beneficiary(maxwell_demon_impossibility, _),
    narrative_ontology:agent_beneficiary(
        humane_treatment_standard__absolute_prohibition,
        detainees_under_armed_conflict),
    \+ narrative_ontology:agent_beneficiary(
        humane_treatment_standard__absolute_prohibition,
        international_humanitarian_law_framework).

% Registry is exactly the two ruled values (two-gate principle; adding an
% entry without a gate-2 convergence read must trip this).
test(registry_exact_contents) :-
    findall(V, narrative_ontology:non_agent_beneficiary(V), Vs0),
    msort(Vs0, Vs),
    Vs == [entropic_universe_hypothesis,
           international_humanitarian_law_framework].

% ----------------------------------------------------------------------------
% (a) Positive control — the flagship case moves to its ruled destination
% ----------------------------------------------------------------------------

test(maxwell_certifies_natural_law) :-
    signature_detection:constraint_signature(maxwell_demon_impossibility, Sig),
    Sig == natural_law.

test(maxwell_mountain_at_all_canonical_contexts) :-
    constraint_indexing:site_contexts_canonical(Ctxs),
    length(Ctxs, 4),                       % non-vacuity guard on the forall
    forall(member(Ctx, Ctxs),
           drl_core:dr_type(maxwell_demon_impossibility, Ctx, mountain)).

test(fsm_released_for_maxwell) :-
    \+ signature_detection:false_summit_mountain(maxwell_demon_impossibility, _).

% ----------------------------------------------------------------------------
% (b) Per-item non-regression — agent-beneficiary mountains still fire FSM
% ----------------------------------------------------------------------------

fsm_agent_mountains([
    animal_moral_status__property_reading,
    article_27_veto_power__sovereignty_reading,
    environmental_instability_as_constraint,
    humane_treatment_standard__absolute_prohibition,
    nuclear_impossibility_kernel__structural_contraction_reading,
    papal_temporal_authority_mountain,
    press_reformation_causality__technological_inevitability,
    reformation_composite__technological_mediation_reading,
    statutory_debt_ceiling__constitutional_nullity_reading,
    technology_reformation_causality__technological_determinism_reading,
    total_war_winnability_post1945__structural_contraction_reading
]).

% plunit forall ⇒ one test instance per constraint: a per-item table, not an
% aggregate count.
test(fsm_still_fires, [forall((fsm_agent_mountains(L), member(C, L)))]) :-
    signature_detection:false_summit_mountain(C, _).

% ----------------------------------------------------------------------------
% (c) :287 inertness tripwire (direct, with dispatch controls and restore)
% ----------------------------------------------------------------------------

% Divergence candidates: exactly the constraints where the raw and
% agent-filtered reads of natural_law_without_beneficiary differ — i.e. raw
% beneficiaries present (raw nlwb FALSE) but zero agent beneficiaries
% (filtered nlwb TRUE), under the same emergence/enforcement conditions.
divergence_candidates(Cs) :-
    (   setof(C,
              B^( narrative_ontology:constraint_beneficiary(C, B),
                  atom(C),
                  domain_priors:emerges_naturally(C),
                  \+ domain_priors:requires_active_enforcement(C),
                  \+ narrative_ontology:agent_beneficiary(C, _) ),
              Cs)
    ->  true
    ;   Cs = []
    ).

% snapshot(+C, -snap(DrTypes, MaxentTops)): final classification observables
% at the 4 canonical contexts. maxent_top_type errors are mapped to a tagged
% term (not silently equalized to a success value).
snapshot(C, snap(Types, Tops)) :-
    constraint_indexing:site_contexts_canonical(Ctxs),
    findall(T,
            ( member(Ctx, Ctxs),
              ( drl_core:dr_type(C, Ctx, T0) -> T = T0 ; T = no_type ) ),
            Types),
    findall(M,
            ( member(Ctx, Ctxs),
              ( catch(maxent_classifier:maxent_top_type(C, Ctx, M0),
                      E, (M0 = maxent_error(E))) -> M = M0 ; M = no_top ) ),
            Tops).

% Test-local redefinition of drl_core:natural_law_without_beneficiary/1.
% SWI permits abolishing static predicates (iso flag false); the predicate
% becomes dynamic for the remainder of this test process, with the original
% body re-asserted in cleanup. Unqualified emerges_naturally /
% requires_active_enforcement in the original resolve to drl_core's bridge
% clauses (drl_core.pl:86-87), referenced explicitly here.
swap_nlwb_to_filtered :-
    abolish(drl_core:natural_law_without_beneficiary/1),
    assertz(( drl_core:natural_law_without_beneficiary(C) :-
                  drl_core:emerges_naturally(C),
                  \+ drl_core:requires_active_enforcement(C),
                  \+ narrative_ontology:agent_beneficiary(C, _) )).

restore_nlwb_to_raw :-
    abolish(drl_core:natural_law_without_beneficiary/1),
    assertz(( drl_core:natural_law_without_beneficiary(C) :-
                  drl_core:emerges_naturally(C),
                  \+ drl_core:requires_active_enforcement(C),
                  \+ narrative_ontology:constraint_beneficiary(C, _) )).

report_divergences([], [], []).
report_divergences([C|Cs], [Raw|Rs], [Filt|Fs]) :-
    (   Raw == Filt
    ->  true
    ;   format(user_error,
               '~n[TRIPWIRE] :287 deferral is STALE — filtering ~w changes final classification.~n', [C]),
        format(user_error,
               '  raw:      ~w~n  filtered: ~w~n', [Raw, Filt]),
        format(user_error,
               '  Consumers: drl_core.pl:333 (snare block), drl_core.pl:362 (tangled_rope block),~n', []),
        format(user_error,
               '  maxent_classifier.pl:173,176,191 (shadow forbidden-features).~n', []),
        format(user_error,
               '  See ISSUES.md agency-gate OQ before changing :287.~n', []),
        fail
    ),
    report_divergences(Cs, Rs, Fs).

test(nlwb_287_inertness_direct) :-
    divergence_candidates(Cands),
    % Enumeration control: the known divergence case must be found —
    % an empty candidate set here means the probe did not look.
    memberchk(maxwell_demon_impossibility, Cands),
    % Dispatch control (pre): raw read sees maxwell's authored beneficiary.
    \+ drl_core:natural_law_without_beneficiary(maxwell_demon_impossibility),
    maplist(snapshot, Cands, RawSnaps),
    setup_call_cleanup(
        swap_nlwb_to_filtered,
        % once/1: the goal must complete DETERMINISTICALLY, else
        % setup_call_cleanup defers the restore past the post-controls below
        % (cleanup only runs when the goal exits without choicepoints).
        once((
            % Dispatch control (mid): the redefinition is visible at the
            % drl_core call site — proves the filtered run is not a
            % byte-identical re-read of the raw engine.
            drl_core:natural_law_without_beneficiary(maxwell_demon_impossibility),
            maplist(snapshot, Cands, FiltSnaps)
        )),
        restore_nlwb_to_raw),
    % Dispatch control (post): restore took effect.
    \+ drl_core:natural_law_without_beneficiary(maxwell_demon_impossibility),
    report_divergences(Cands, RawSnaps, FiltSnaps).

:- end_tests(agent_beneficiary).
