:- module(report_generator, [
    generate_full_report/1,
    generate_omegas_from_gaps/1,
    omega_from_gap/5,
    generate_omega_resolution_scenarios/1,
    generate_omega_triage/0,
    omega_severity/2,
    type_description/2,
    type_strategy/2,
    type_color/2,
    type_severity/2,
    detect_gap_pattern/2,
    gap_coverage/1,
    gap_status/2,
    gap_seat_source/1
]).

:- use_module(type_metadata).
:- reexport(type_metadata).

:- use_module(library(lists)).
:- use_module(narrative_ontology).
:- use_module(config).
:- use_module(stakeholder_seats).  % R5 zombie crosscheck (OQ-109 B3)
:- use_module(intent_engine, except([classify_interval/3])).
:- use_module(coercion_projection).
:- use_module(pattern_analysis).
:- use_module(constraint_bridge).
:- use_module(drl_core).
:- use_module(uke_dr_bridge).
:- use_module(signature_detection, [signature_confidence/3, explain_signature/3, get_constraint_profile/2]).
:- use_module(constraint_indexing).
:- use_module(isomorphism_engine). % Required for isomorphism audit
:- use_module(domain_priors).      % Required for forensic audit
:- use_module(utils).              % Safe helpers for defensive programming
:- use_module(drl_lifecycle).      % Drift event detection & lifecycle analysis

% Suppress warning - we intentionally override intent_engine:classify_interval/3
:- discontiguous classify_interval/3.

/* ============================================================================
   1. EXECUTIVE SUMMARY (MAIN ENTRY)
   ============================================================================ */

generate_full_report(IntervalID) :-
    narrative_ontology:interval(IntervalID, T_start, Tn),
    classify_interval(IntervalID, Pattern, Conf),
    
    format('~n~n====================================================~n'),
    format('   DR DETAILED ANALYSIS                             ~n'),
    format('====================================================~n'),
    format('Timeline:       ~w to ~w~n', [T_start, Tn]),
    format('Structural Pattern: ~w~n', [Pattern]),
    % OQ-100a: categorical interval-pattern confidence (high/moderate/low,
    % from data completeness) — distinct from the Python-side MaxEnt
    % P(claimed) and corpus band histogram; each label names its quantity.
    format('Pattern confidence (categorical): ~w~n', [Conf]),
    % OQ-93: Pattern/Confidence come from classify_interval, whose gradient
    % and completeness inputs are the leveled grid — carry the diet here too.
    % OQ-98 (operator ruling 1, per-question branch — P1 witnessed BRANCH A):
    % grid-fed findings carry a CONDITIONAL tag whenever authored < total.
    (   catch(data_repair:grid_provenance(IntervalID, prov(HA, HI, HP, _, HTotal)), _, fail)
    ->  HPresent is HA + HI + HP,
        (   HPresent =:= 0
        ->  % Absent grid: ONE informative line (operator ruling 2026-06-20). The
            % ABSENCE is the signal — the story could author a leveled coercion grid
            % and did not, so it is not a level-resolved-coercion story; the
            % grid-dependent magnitude/coverage below are correspondingly not computed
            % (expected, not a gap). Plain text carries OQ-98's "ungrounded" meaning
            % without the Prolog jargon the report's downstream consumer does not need.
            format('Leveled coercion grid: not authored (story not level-resolved-coercion focused); grid-dependent magnitude/coverage not computed - expected, not a gap (OQ-93)~n', [])
        ;   format('Grid diet:      authored ~w/~w, injected ~w, imputed ~w (OQ-93)', [HA, HTotal, HI, HP]),
            (   HA < HTotal
            ->  format(' [CONDITIONAL: grid authored ~w/~w]~n', [HA, HTotal])
            ;   nl
            )
        )
    ;   true
    ),
    
    % --- SECTION 3: META-LOGICAL AUDIT ---
    % OQ-100c: inputs are author-assigned, so this is a self-consistency
    % audit (declared type vs own-assigned metrics), not fraud detection.
    format('~n[META-LOGICAL AUDIT: DECLARED-TYPE vs OWN-ASSIGNED-METRICS SELF-CONSISTENCY]~n'),
    (   setof((C, Err, Sev), drl_core:dr_mismatch(C, Err, Sev), Errors)
    ->  forall(member((C, Err, Sev), Errors),
               format('  ! ALERT [~w]: ~w detected for ~w~n', [Sev, Err, C]))
    ;   format('  No classification errors detected. System is Ontologically Coherent.~n')
    ),

    % --- SECTION 3A: FORENSIC AUDIT FOR FALSE MOUNTAINS ---
    forensic_audit_false_mountains,

    % --- SECTION 4: STRUCTURAL SIGNATURE ANALYSIS ---
    format('~n[STRUCTURAL SIGNATURE ANALYSIS]~n'),
    (   catch(
            forall(narrative_ontology:constraint_claim(CSig, _Claim),
                   report_constraint_signature(CSig)),
            Error,
            format('  [FAIL] Exception: ~w~n', [Error]))
    ;   true
    ),

    % --- SECTION 5: UKE_DR FEASIBILITY BRIDGE ---
    (   narrative_ontology:recommendation(_, _)
    ->  format('~n[UKE_DR FEASIBILITY BRIDGE]~n'),
        format('  ~40s | ~12s~n', ['Recommendation', 'UKE Status']),
        format('  ----------------------------------------------------------------------~n'),
        (   forall(narrative_ontology:recommendation(RID, Summary),
                   ( ( uke_dr_bridge:uke_status(RID, UKEStatus, Reasons)
                     -> format('  - ~40w | ~12w~n', [Summary, UKEStatus]),
                        forall(member(R, Reasons), format('    > ~w~n', [R]))
                     ;  format('  - ~40w | ~12s~n', [Summary, 'DATA_MISSING'])
                     )
                   ))
        ;   true
        )
    ;   true  % No recommendations — suppress entirely
    ),
    
    % --- SECTION 6: KINETIC MAGNITUDE ---
    % OQ-93 coverage-carrying read: the aggregate kappa averages the levels
    % PRESENT, so the coverage must travel with the number (an average over
    % one level previously printed as if it were the system magnitude).
    % Kappa's own per-question requirement is >=1 full vector at Tn; partial
    % grids still print, stamped with their level coverage + the OQ-98
    % CONDITIONAL tag below.
    findall(L-Kappa, (config:level(L), once(coercion_projection:coercion_magnitude(IntervalID, L, Tn, Kappa))), LKs),
    (   LKs \= []
    ->  findall(K, member(_-K, LKs), Kappas),
        findall(L2, member(L2-_, LKs), KLevels),
        sum_list(Kappas, SumK), length(Kappas, NK), AvgK is SumK / NK,
        aggregate_all(count, config:level(_), NAllLevels),
        format('~nAggregate Magnitude (Kappa) at Tn: ~2f [level coverage ~w/~w: ~w]',
               [AvgK, NK, NAllLevels, KLevels]),
        % OQ-93: kappa is computed over the leveled grid — carry the diet
        % with the number. OQ-98: CONDITIONAL tag when authored < total
        % (per-question branch; P1 witnessed BRANCH A).
        (   catch(data_repair:grid_provenance(IntervalID, prov(KA, KI, KP, _, KTotal)), _, fail)
        ->  format(' [grid diet: authored ~w/~w, injected ~w, imputed ~w — OQ-93]', [KA, KTotal, KI, KP]),
            (   KA < KTotal
            ->  format(' [CONDITIONAL: grid authored ~w/~w]~n', [KA, KTotal])
            ;   nl
            )
        ;   nl
        )
    ;   format('~nAggregate Magnitude (Kappa): DATA_INSUFFICIENT~n')
    ),
    
    % --- SECTION 7: MANDATROPHY GAP ANALYSIS ---
    %  Full perspectival breakdown (claimed type, 4 perspectives, chi)
    %  is covered by Python Levels 1-2. Only unique mandatrophy delta_chi
    %  gaps are reported here.
    format('~n[MANDATROPHY GAP ANALYSIS]~n'),
    format('  (Full perspectival detail in Levels 1-2 above)~n'),
    (   forall(narrative_ontology:constraint_claim(CGap, _),
               mandatrophy_only_report(CGap))
    ;   true
    ),
    
    % --- OMEGA ASSERTION (silent — reporting handled by Python L1) ---
    assert_omegas_from_gaps(IntervalID),

    % --- SECTION 8B: OMEGA RESOLUTION SCENARIOS ---
    % OQ-99: the report subject binds authored omegas' Constraint slot
    % (interval id == constraint id by pipeline convention).
    generate_omega_resolution_scenarios(IntervalID),

    format('====================================================~n').

/* ============================================================================
   2. OMEGA GENERATION
   ============================================================================ */

generate_omegas_from_gaps(IntervalID) :-
    format('~n[OMEGA GENERATION FROM PERSPECTIVAL GAPS: ~w]~n', [IntervalID]),
    findall(
        omega_entry(OmegaID, Type, Question, Gap, Constraint),
        (   narrative_ontology:constraint_claim(Constraint, _),
            detect_gap_pattern(Constraint, Gap),
            omega_from_gap(Constraint, Gap, OmegaID, Type, Question)
        ),
        OmegaEntries
    ),
    process_omega_entries(OmegaEntries).

process_omega_entries([]) :-
    format('  No perspectival gaps detected requiring Ω tracking.~n').
process_omega_entries(OmegaEntries) :-
    OmegaEntries \= [],
    length(OmegaEntries, Count),
    format('  Generated ~w Omega variables from perspectival gaps:~n~n', [Count]),
    forall(member(omega_entry(OID, OType, OQuestion, OGap, Constraint), OmegaEntries),
           (   format('  Ω: ~w (~w)~n', [OID, OType]),
               format('     Question: ~w~n', [OQuestion]),
               format('     Source: ~w~n~n', [OGap]),
               (OGap = gap(GapPattern, _, _) -> true ; GapPattern = unknown),
               assert_omega_if_new(OID, OType, OQuestion, Constraint, GapPattern)
           )).

%% assert_omegas_from_gaps(+IntervalID)
%  Generates and asserts omega variables from perspectival gaps
%  without report output. Ensures omega_variable/3 and omega_source/3
%  facts are in the KB for downstream use (e.g., resolution scenarios).
%  Report-level omega listing is handled by the Python pipeline (L1).
assert_omegas_from_gaps(_IntervalID) :-
    forall(
        (narrative_ontology:constraint_claim(Constraint, _),
         detect_gap_pattern(Constraint, Gap),
         omega_from_gap(Constraint, Gap, OmegaID, Type, Question)),
        (   (Gap = gap(GapPattern, _, _) -> true ; GapPattern = unknown),
            assert_omega_if_new(OmegaID, Type, Question, Constraint, GapPattern)
        )
    ).

% ============================================================================
% PERSPECTIVAL GAP DETECTION — rewired onto authored stakeholder seats
% ============================================================================
% Pre-2026-06-05 this read constraint_indexing:constraint_classification/3 — a
% per-power-seat type STORED as a fact (powerless vs institutional cells). The
% corpus rebuild + observer-authoring change retired that predicate (0 facts on
% the live corpus bar one engine demo), stranding this feeder: 0 gaps, 0 omegas.
% The same idea now lives in narrative_ontology:constraint_stakeholder/7 — each
% authored seat carries its (Power,Time,Exit,Scope) context, with the type
% COMPUTED on demand. We compute each seat's type through the CANONICAL seat path
% (stakeholder_seats:dr_type_for_stakeholder/3 — the per-(C,Name) coordinate that
% escapes the same-power atom collapse; role-d + exit modulation), and a gap is
% plain type-divergence: >=2 distinct non-unknown computed types across the
% authored seats. (Detection is type-inequality; LABELING which Ω uses the seat
% power ordering + functional/extractive sort below.)
%
% Fail-closed (Build Discipline Pattern 5/6): <2 typeable seats, or <2 distinct
% non-unknown types => detect_gap_pattern FAILS (mint nothing). An all-unknown
% seated constraint is "couldn't type" (didn't-look), never "measured no gap" —
% it does not fire here, and gap_coverage/1 reports it as unexaminable so the
% serialization boundary can distinguish null from [].

% Functional vs extractive sort (OPEN-A; grounded: docs/logic.md §B.7 — naturalized
% is the power-scaling cover side; deferential_realism_paper_v7.md Theorem-1 tie
% names the raw orbit [naturalized,snare,rope,snare] as the snare_masked_as_rope
% gap — the institutional/high-power seat sees rope|naturalized where the
% powerless seat sees snare). Finer labels (cut_safety = mountain/rope,
% learned_helplessness = snare/mountain) are DEFERRED to OPEN-A pending a grounded
% partition — they currently route to general_type_mismatch.
gap_functional_type(rope).
gap_functional_type(naturalized).
gap_functional_type(scaffold).
gap_functional_type(mountain).
gap_extractive_type(snare).
gap_extractive_type(tangled_rope).

%% gap_seat_source(-Source)  [OQ-197]
%  The seat-typing source for gap detection and operability. This single fact is
%  the (a)/(b) ruling seam: `stakeholder` types from authored constraint_stakeholder/7
%  facts (ruling (a) — the current/default source); `canonical` types the six
%  canonical power seats from constraint_classification/3 (ruling (b)). BOTH
%  detect_gap_pattern/2 and gap_status/2 read through it, so switching the ruling is
%  a one-line change here — nothing downstream is source-specific. Default stakeholder
%  pending the OQ-197 (a)/(b) ruling (see docs/design/detector_calibration_omega_proposal.md).
gap_seat_source(stakeholder).

%% seat_type_reading(+C, -reading(D, Power, Type, Name))
%  One seat's computed type (non-unknown), with its power atom and the power-ordering
%  key D (canonical d: HIGHER d = LOWER power). Dispatches on gap_seat_source/1 so the
%  firing path and the operability path share one source of truth.
seat_type_reading(C, R) :-
    gap_seat_source(Source),
    seat_type_reading(C, Source, R).

%% seat_type_reading(+C, +Source, -reading(D, Power, Type, Name))
seat_type_reading(C, stakeholder, reading(D, Power, Type, Name)) :-
    narrative_ontology:constraint_stakeholder(C, Name, _Role, Power, _T, _E, _S),
    stakeholder_seats:dr_type_for_stakeholder(C, Name, Type),
    Type \= unknown,
    (   constraint_indexing:canonical_d_for_power(Power, D) -> true ; D = 0.5 ).
seat_type_reading(C, canonical, reading(D, Power, Type, Power)) :-
    member(Power, [powerless, moderate, powerful, organized, institutional, analytical]),
    constraint_indexing:canonical_d_for_power(Power, D),
    constraint_indexing:constraint_classification(C, Type, context(agent_power(Power), _, _, _)),
    Type \= unknown.

%% gap_status(+C, -Status)  [OQ-197 — three-valued gap operability]
%  Closes the Build-Discipline Pattern-6 collapse where measured-no-gap and
%  didn't-look both emitted a success-shaped empty. Status is exactly one of:
%    gap(Pattern, TLo, THi)         — a cover-story / type-divergence gap was detected
%                                     (SAME condition as detect_gap_pattern/2 — firing
%                                     behaviour is unchanged; this branch just wraps it)
%    no_gap                         — enough operable seats to compare (>=2 spanning >=2
%                                     distinct power positions) and no gap fired
%    undetermined(Reason)           — too few operable seats to pose the question
%  Reason in {no_seats, single_seat, single_power_position}. The operability
%  precondition (>=2 seats at >=2 distinct power positions) is the thing R4 turned on:
%  a proxy for it (any typeable seat) let present-but-insufficient read as measured-empty.
gap_status(C, Status) :-
    (   detect_gap_pattern(C, Gap)          % firing logic UNCHANGED (behaviour-preserving)
    ->  Status = Gap
    ;   gap_nonfire_status(C, Status)       % split the non-firing outcome, do not fabricate one
    ).

%% gap_nonfire_status(+C, -Status)  Status in {no_gap, undetermined(Reason)}.
gap_nonfire_status(C, Status) :-
    findall(R, seat_type_reading(C, R), Rs),
    nonfire_reason(Rs, Status).

nonfire_reason([], undetermined(no_seats)) :- !.
nonfire_reason([_], undetermined(single_seat)) :- !.
nonfire_reason(Rs, Status) :-
    setof(P, D^T^N^member(reading(D,P,T,N), Rs), Powers),
    (   Powers = [_]                        % all seats at one power: no gradient to compare
    ->  Status = undetermined(single_power_position)
    ;   Status = no_gap                     % >=2 power positions examined, detector did not fire
    ).

%% gap_coverage(+C)  [OQ-197: lifted from the >=1-seat proxy to the operability precondition]
%  The gap question is EXAMINABLE iff gap_status/2 is not undetermined. This lifts the
%  old >=1-typeable-seat threshold up to the detector's own comparison threshold so
%  present-but-insufficient (e.g. single_power_position) reads null (didn't-look) at the
%  serialization boundary, not [] (looked, no gap). null = unexaminable; [] = examined, no gap.
gap_coverage(C) :- gap_status(C, S), S \= undetermined(_).

%% detect_gap_pattern(+C, -gap(Pattern, LowPowerType, HighPowerType))
%  Fires iff >=2 distinct non-unknown computed seat types. Deterministic (one
%  solution): the trailing cut commits to the first labeling.
detect_gap_pattern(C, gap(Pattern, TLo, THi)) :-
    findall(R, seat_type_reading(C, R), Rs),
    Rs = [_, _|_],                              % >=2 typeable seats
    setof(T, D^P^N^member(reading(D,P,T,N), Rs), Types),
    Types = [_, _|_],                           % >=2 distinct non-unknown types
    % Compute the label into FRESH vars so the priority cascade is honoured even
    % when the caller pre-binds Pattern (else head-unification on a pre-bound
    % general_type_mismatch would bypass the extraction_blindness clause — the
    % same leak drl_core:dr_type/3 guards against with FinalType). Unify after.
    label_gap(Rs, P0, L0, H0),
    !,
    Pattern = P0, TLo = L0, THi = H0.

%% label_gap(+Readings, -Pattern, -LowPowerType, -HighPowerType)
%  Priority: extraction_blindness (an extractive-typed seat at LOWER power than a
%  functional-typed seat — the cover-story structure, Theorem 1) outranks the
%  general type-mismatch.
label_gap(Rs, extraction_blindness, ExtT, FuncT) :-
    member(reading(De, _, ExtT, _),  Rs), gap_extractive_type(ExtT),
    member(reading(Df, _, FuncT, _), Rs), gap_functional_type(FuncT),
    De > Df,                                     % extractive seat is lower-power
    !.
label_gap(Rs, general_type_mismatch, TLo, THi) :-
    sort(Rs, Sorted),                            % ascending by D: head=highest power
    last(Sorted, reading(_, _, TLo, _)),         % lowest-power seat type
    member(reading(_, _, THi, _), Sorted),       % first differing type, high-power-ward
    THi \= TLo,
    !.

% ----------------------------------------------------------------------------
% omega_from_gap/5 — LABELING (not detection). Maps the constructed gap to its Ω.
% ----------------------------------------------------------------------------
omega_from_gap(C, gap(extraction_blindness, ExtT, FuncT), OmegaID, conceptual, Question) :-
    format(atom(OmegaID), 'omega_extraction_blindness_~w', [C]),
    % OQ-93 Stage D: when the authored grid witnesses the level-gradient
    % crossing, the omega carries the watched process (upgrade from
    % inferred-snapshot to witnessed process, ruling (b)); absent/partial
    % grid -> unchanged question (the signal is OPEN, never a gate).
    (   catch(signature_detection:level_gradient_divergence(C, divergence(GS, GI)), _, fail)
    ->  format(atom(Question), 'Constraint ~w computes as extractive (~w) at lower-power seats but functional (~w) at higher-power seats — extraction masked by perspective. [witnessed process — OQ-93 grid: structural gradient +~2f while individual gradient ~2f, the level-gradient crossing]', [C, ExtT, FuncT, GS, GI])
    ;   format(atom(Question), 'Constraint ~w computes as extractive (~w) at lower-power seats but functional (~w) at higher-power seats — extraction masked by perspective.', [C, ExtT, FuncT])
    ), !.

omega_from_gap(C, gap(general_type_mismatch, TLo, THi), OmegaID, conceptual, Question) :-
    format(atom(OmegaID), 'omega_perspectival_~w', [C]),
    format(atom(Question), 'Constraint ~w appears as ~w at lower-power seats but ~w at higher-power seats — perspectival type divergence.', [C, TLo, THi]), !.

:- dynamic omega_source/3.  % omega_source(OmegaID, Constraint, GapPattern)

%% assert_omega_if_new(+OmegaID, +Type, +Question, +Constraint, +GapPattern)
%  Stores omega variable along with its source metadata so that
%  generate_omega_resolution_scenarios can look up the originating
%  constraint and gap pattern programmatically, without brittle
%  string parsing of the omega ID.
assert_omega_if_new(OmegaID, Type, Question, Constraint, GapPattern) :-
    (   narrative_ontology:omega_variable(OmegaID, _, _)
    ->  true
    ;   assertz(narrative_ontology:omega_variable(OmegaID, Type, Question)),
        assertz(omega_source(OmegaID, Constraint, GapPattern))
    ).

% Backward-compatible arity-3 version for external callers
assert_omega_if_new(OmegaID, Type, Question) :-
    assert_omega_if_new(OmegaID, Type, Question, unknown, unknown).

/* ============================================================================
   3. INDEXED REPORTING & AUDITS
   ============================================================================ */

%% mandatrophy_only_report(+C)
%  Reports only the mandatrophy delta_chi gap for a constraint,
%  suppressing the full perspectival breakdown (covered by Python L1/L2).
%  Only produces output when powerless and institutional perspectives
%  disagree on constraint type.
mandatrophy_only_report(C) :-
    (constraint_indexing:constraint_classification(C, TypeP, context(agent_power(powerless), _, _, _)) -> true ; TypeP = none),
    (constraint_indexing:constraint_classification(C, TypeI, context(agent_power(institutional), _, _, _)) -> true ; TypeI = none),
    (   TypeP \= none, TypeI \= none, TypeP \= TypeI
    ->  format('  ~w (~w vs ~w):~n', [C, TypeP, TypeI]),
        format_mandatrophy_gap(C, powerless, institutional)
    ;   true
    ),
    % R5 Q6 synchronic crosscheck (OQ-83 crosscheck-completion, 2026-06-16 —
    % extends the OQ-109 B3 A7-RECOVERY surface; the Phase-A primitive
    % stakeholder_seats:q6_crosscheck/3 is the single source, no parallel path).
    % Prints the (status × computed-signature) Cell + the daylight qualifier;
    % silent only when the authored side is absent (q6_unmeasured). This is the
    % consumer the mandatrophy apparatus lost at the format migration (OQ-83 A7):
    % authored genealogy (founding_problem_status) cross-checked against the
    % computed present structure (dr_type at the default analytical context).
    r5_zombie_crosscheck_line(C),
    % OQ-86 extraction reading: the no-authored-victim blindspot. Silent unless
    % the constraint computes an extractive type with no cost-bearer authored;
    % then names the beneficiary-side seats and flags the unnamed cost-bearer.
    % R3 commentary (never classification); same anchored-line -> extractor ->
    % sidecar shape as the q6 crosscheck above.
    extraction_reading_line(C).

%% extraction_reading_line(+C)
%  CID-anchored R3 commentary line (OQ-85/OQ-86). Carries the non-classifying
%  caveat to the read site. Silent when not the blindspot shape (extraction_
%  reading/2 fails) — absence is silence, never a fabricated value.
extraction_reading_line(C) :-
    (   stakeholder_seats:extraction_reading(C, extraction(Es, cost_bearer_unnamed))
    ->  format('  ~w: EXTRACTION READING: extractive constraint-level type with no authored \c
victim; beneficiary-side seats = ~q; cost-bearer named only in the authored situation/transfer \c
narrative (commentary, non-classifying; OQ-85/OQ-86).~n', [C, Es])
    ;   true   % silent: not the blindspot shape
    ).

%% r5_zombie_crosscheck_line(+C)
%  TIER LIMIT carried to the read site (defense-in-depth; the load-bearing copy
%  is the doc-comment on q6_crosscheck/3): a Cell is a STRUCTURAL MISMATCH, not a
%  verdict. live_claim_vs_snare_present must not be read downstream as a
%  cover-story finding — orientation (cover / survival / defense) is NOT witnessed
%  at this tier. In the merge window daylight ships unstated, so this label stands
%  in for the absent qualifier on the highest-value cell.
r5_zombie_crosscheck_line(C) :-
    stakeholder_seats:q6_crosscheck(C, Cell, Daylight),
    (   Cell == q6_unmeasured
    ->  true   % authored side absent — nothing to crosscheck
    ;   format('  ~w: R5 Q6 CROSSCHECK: ~w ~w  (structural mismatch; orientation not witnessed at this tier)~n',
               [C, Cell, Daylight])
    ).

perspectival_gap_audit(C) :-
    narrative_ontology:constraint_claim(C, Claimed),
    format('~n  Constraint: ~w~n', [C]),
    format('    Claimed Type: ~w~n', [Claimed]),
    % Get powerless and institutional types for gap alerts
    (constraint_indexing:constraint_classification(C, TypeP, context(agent_power(powerless), _, _, _)) -> true ; TypeP = none),
    (constraint_indexing:constraint_classification(C, TypeI, context(agent_power(institutional), _, _, _)) -> true ; TypeI = none),
    % Gap alerts
    (TypeP == mountain, TypeI == rope -> format('    ! GAP: Institutional "Rope" appears as "Mountain" to Powerless.~n') ; true),
    (TypeP == snare, TypeI == rope -> format('    ! ALERT: Extractive "Snare" is masked as functional "Rope".~n') ; true),
    % All 4 perspectives: type + match/mismatch + chi metrics
    forall(
        member(Power-Label, [
            powerless-'Powerless', moderate-'Moderate',
            institutional-'Institutional', analytical-'Analytical'
        ]),
        format_perspective_line(C, Power, Label, Claimed)
    ),
    % Mandatrophy gap if perspectives differ
    (   TypeP \= none, TypeI \= none, TypeP \= TypeI
    ->  format_mandatrophy_gap(C, powerless, institutional)
    ;   true
    ).

%% format_perspective_line(+C, +ContextPower, +Label, +Claimed)
%  Looks up the classification for this context (partial match on power level),
%  shows match/mismatch against claimed type, and appends chi metrics if available.
format_perspective_line(C, ContextPower, Label, Claimed) :-
    (   constraint_indexing:constraint_classification(C, Type,
            context(agent_power(ContextPower), _, _, _))
    ->  (Type == Claimed -> MatchStr = ' (Matches Claim)' ; MatchStr = ' (Mismatch)'),
        (   compute_chi_v6(C, ContextPower, _, D, FD, Chi)
        ->  (   Chi < 0
            ->  format(atom(ChiStr), ' [d=~3f f(d)=~2f χ=~2f → net benefit]', [D, FD, Chi])
            ;   format(atom(ChiStr), ' [d=~3f f(d)=~2f χ=~2f]', [D, FD, Chi])
            )
        ;   ChiStr = ''
        ),
        format('    - ~w: ~w~w~w~n', [Label, Type, MatchStr, ChiStr])
    ;   format('    - ~w: (no classification)~n', [Label])
    ).

%% compute_chi_v6(+C, +ContextPower, -BaseE, -D, -FD, -Chi)
%  Computes chi via v6.0 structural directionality chain.
%  Chi = BaseE * f(d) * scope_modifier(Scope).
compute_chi_v6(C, ContextPower, BaseE, D, FD, Chi) :-
    standard_context(ContextPower, Ctx),
    drl_core:base_extractiveness(C, BaseE),
    Ctx = context(agent_power(Power), _, _, spatial_scope(Scope)),
    constraint_indexing:resolve_coalition_power(Power, C, ResolvedPower),
    Ctx = context(_, T, E, S),
    ResolvedCtx = context(agent_power(ResolvedPower), T, E, S),
    constraint_indexing:derive_directionality(C, ResolvedCtx, D),
    constraint_indexing:sigmoid_f(D, FD),
    constraint_indexing:scope_modifier(Scope, SM),
    Chi is BaseE * FD * SM.

%% standard_context(+PowerLevel, -Context)
%  Canonical contexts for each power level, matching logical_fingerprint.pl.
standard_context(powerless,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).
standard_context(moderate,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).
standard_context(institutional,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).
standard_context(analytical,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

%% format_mandatrophy_gap(+C, +PowerA, +PowerB)
%  Shows the extraction gap between two power positions.
%  v6.0: Uses structural directionality chain.
format_mandatrophy_gap(C, PowerA, PowerB) :-
    (   compute_chi_v6(C, PowerA, _, _, _, RawA),
        compute_chi_v6(C, PowerB, _, _, _, RawB)
    ->  EffA is min(1.0, max(0.0, RawA)),
        EffB is RawB,
        DeltaChi is abs(EffA - EffB),
        (   DeltaChi > 1.0 -> Sev = critical
        ;   DeltaChi > 0.5 -> Sev = high
        ;   Sev = moderate
        ),
        format('    ! MANDATROPHY GAP: delta_chi = ~2f (~w)~n', [DeltaChi, Sev])
    ;   true
    ).

report_constraint_signature(C) :-
    drl_core:dr_signature(C, Signature),
    signature_detection:signature_confidence(C, Signature, Confidence),
    signature_detection:explain_signature(C, Signature, Explanation),
    format('  ~20w: ~20w (confidence: ~w)~n', [C, Signature, Confidence]),
    (Signature \= ambiguous -> format('    → ~w~n', [Explanation]) ; true),
    % v6.0: For false_ci_rope, surface directionality context
    (   Signature = false_ci_rope
    ->  (   narrative_ontology:constraint_beneficiary(C, B)
        ->  format('    → Institutional beneficiary: ~w~n', [B])
        ;   format('    → No declared beneficiary (structural derivation uses canonical d)~n')
        ),
        (   standard_context(institutional, ICtx),
            constraint_indexing:derive_directionality(C, ICtx, D_inst)
        ->  format('    → Institutional d=~3f~n', [D_inst])
        ;   true
        )
    ;   true
    ).

% Remaining placeholders (generate_indexed_report, extract_constraints, generate_llm_feedback, sublist, etc.) 
% should be placed here as top-level predicates...

generate_indexed_report(Text, Context, Report) :-
    extract_constraints(Text, Constraints),
    maplist(classify_with_context(Context), Constraints, Classifications),
    format_indexed_report(Classifications, Context, Report).

classify_with_context(Context, Constraint, classification(Constraint, Type)) :-
    constraint_indexing:constraint_classification(Constraint, Type, Context).

%% extract_constraints(+Text, -Constraints)
%  Extracts constraint names mentioned in Text by checking against
%  all constraints declared in the knowledge base. Uses sub_atom/5
%  on atom-level names rather than character-code list scanning.
extract_constraints(Text, Constraints) :-
    findall(C,
        (   narrative_ontology:constraint_claim(C, _),
            sub_atom(Text, _, _, _, C)
        ),
        Cs),
    sort(Cs, Constraints).

format_indexed_report(Classifications, Context, Report) :-
    Context = context(agent_power(Power), time_horizon(Time), exit_options(Exit), spatial_scope(Scope)),
    with_output_to(atom(Report),
        (format('~n[INDEXED CONSTRAINT ANALYSIS]~n'),
         format('Perspective: ~w / ~w / ~w / ~w~n~n', [Power, Time, Exit, Scope]),
         format('Classifications:~n'),
         forall(member(classification(C, T), Classifications), format('  ~w: ~w~n', [C, T])))).

generate_llm_feedback(IntervalID) :-
    format('~n### START LLM REFINEMENT MANIFEST: ~w ###~n', [IntervalID]),
    format('~n[PERSPECTIVAL_GAPS]~n'),
    % Census A6 (OQ-109 B3, 2026-06-12): the bare forall printed an empty
    % section on a cell-less corpus — carry the ran-witness.
    aggregate_all(count,
                  ( narrative_ontology:constraint_claim(CCov, _),
                    constraint_indexing:constraint_classification(CCov, _, context(agent_power(powerless), _, _, _)),
                    constraint_indexing:constraint_classification(CCov, _, context(agent_power(institutional), _, _, _)) ),
                  NBothSeats),
    (   NBothSeats =:= 0
    ->  format('  [VACUOUS] no constraint carries both powerless and institutional authored cells — zero gap checks ran~n')
    ;   format('  (~w constraints with both seats examined)~n', [NBothSeats])
    ),
    (forall(narrative_ontology:constraint_claim(C, _),
           (constraint_indexing:constraint_classification(C, TypeP, context(agent_power(powerless), _, _, _)),
            constraint_indexing:constraint_classification(C, TypeI, context(agent_power(institutional), _, _, _)),
            TypeP \= TypeI, format('  - Constraint "~w": Individual sees ~w, but Institution sees ~w.~n', [C, TypeP, TypeI]))) ; true),
    format('~n[ONTOLOGICAL_MISMATCHES]~n'),
    (setof((CM, Err, Sev), drl_core:dr_mismatch(CM, Err, Sev), Errors) -> forall(member((CM, Err, Sev), Errors), format('  - ~w: [~w] ~w detected.~n', [CM, Sev, Err])) ; format('  - None detected.~n')),
    format('~n[UNRESOLVED_OMEGAS]~n'),
    (setof((OID, OTy, ODe), narrative_ontology:omega_variable(OID, OTy, ODe), Omegas) -> forall(member((OID, OTy, ODe), Omegas), format('  - ~w (~w): ~w~n', [OID, OTy, ODe])) ; format('  - None detected.~n')),
    format('~n### END REFINEMENT MANIFEST ###~n').

% sublist/2 removed — was only used by the old character-code-based
% extract_constraints/2, now replaced with KB-aware sub_atom/5 matching.

%% classify_interval(+IntervalID, -Pattern, -Confidence)
%  Computes the structural pattern and confidence for an interval by
%  delegating to pattern_analysis. Uses the pure return-value API
%  (analyze_interval/4) to avoid side-effect-driven data passing.
%  Confidence is derived from data completeness: high (>=0.75),
%  moderate (>=0.40), low otherwise.
classify_interval(IntervalID, Pattern, Confidence) :-
    catch(
        (   pattern_analysis:analyze_interval(IntervalID, _Gradient, Score, Pattern),
            completeness_to_confidence(Score, Confidence)
        ),
        _Error,
        (   Pattern = unknown,
            Confidence = insufficient_data
        )
    ).

completeness_to_confidence(Score, high) :- Score >= 0.75, !.
completeness_to_confidence(Score, moderate) :- Score >= 0.40, !.
completeness_to_confidence(_, low).

/* ============================================================================
   4. ONTOLOGICAL FORENSIC AUDIT: FALSE MOUNTAINS
   ============================================================================ */

%% forensic_audit_false_mountains/0
%  Provides detailed analysis of constraints claiming "Mountain" status
%  but failing validation. Explains WHY each fails and recommends reclassification.
forensic_audit_false_mountains :-
    %  Only print when at least one constraint claims mountain status
    (   narrative_ontology:constraint_claim(_, mountain)
    ->  format('~n[ONTOLOGICAL FORENSIC AUDIT: FALSE MOUNTAINS]~n'),
        (   setof(C-Ctx, Sev^(drl_core:dr_mismatch(C, Ctx, type_1_false_summit, Sev)),
                  FalseMountains)
        ->  (length(FalseMountains, NPairs),
             findall(FC, member(FC-_, FalseMountains), FCs0), sort(FCs0, DistinctCs),
             length(DistinctCs, NConstraints),
             format('  Detected ~w constraint(s) falsely claiming "Mountain" status, across ~w observer-context instance(s):~n~n', [NConstraints, NPairs]),
             forall(member(C-Context, FalseMountains),
                    forensic_explain_false_mountain(C, Context)))
        ;   format('  All mountains are structurally validated.~n')
        )
    ;   true  % No mountains claimed — suppress entirely
    ).

%% forensic_explain_false_mountain(+Constraint, +Context)
%  Explains in detail why a constraint fails mountain validation.
%  Now using safe helpers for defensive programming.
forensic_explain_false_mountain(C, Context) :-
    format('  ┌─ CONSTRAINT: ~w~n', [C]),
    format('  │  Context: ~w~n', [Context]),
    format('  │~n', []),

    % Get metrics using safe helpers (with defaults and logging)
    utils:safe_get_metric(C, suppression_requirement, Supp, 0.0, false),
    (Supp = 0.0
    -> format('  │  Suppression Requirement: MISSING (using default 0.0)~n', [])
    ;  format('  │  Suppression Requirement: ~2f~n', [Supp])),

    % Get mountain ceiling safely
    utils:safe_get_config(mountain_suppression_ceiling, Ceil, 0.15),
    (Ceil = 0.15
    -> format('  │  Mountain Ceiling (threshold): ~2f (default)~n', [Ceil])
    ;  format('  │  Mountain Ceiling (threshold): ~2f~n', [Ceil])),

    % Analyze why it fails
    format('  │~n', []),
    format('  │  FAILURE ANALYSIS:~n', []),
    (Supp > Ceil
    -> format('  │  ✗ Requires active enforcement (suppression ~2f > ~2f threshold)~n', [Supp, Ceil])
    ;  format('  │  ? Suppression within range, checking other factors...~n', [])),

    % Get extractiveness safely
    utils:safe_get_extractiveness(C, Extr, 0.0, false),
    (Extr = 0.0
    -> format('  │  Base Extractiveness: MISSING (using default 0.0)~n', [])
    ;  (format('  │  Base Extractiveness: ~2f~n', [Extr]),
        (Extr > 0.35
        -> format('  │  ✗ Shows extractive pattern (E > 0.35)~n', [])
        ;  format('  │  ✓ Non-extractive pattern (E ≤ 0.35)~n', [])))),

    % Check resistance to change safely
    utils:safe_get_metric(C, resistance_to_change, Resist, 0.0, false),
    (Resist = 0.0
    -> format('  │  Resistance to Change: MISSING (using default 0.0)~n', [])
    ;  format('  │  Resistance to Change: ~2f~n', [Resist])),

    % Authoritative per-context verdict (OQ-50 OPEN-1): the detector flags via
    % dr_type/3 (post-signature), so the explainer must report that same
    % authoritative type, not a re-derived metric heuristic that can contradict a
    % correct flag. Fail-closed fallback: if dr_type/3 has no solution at this
    % seat, print an explicit unbound line rather than going silent (the seat was
    % reached because the detector flagged it, so a solution is expected — but the
    % guard, not that expectation, is what keeps AMBIGUOUS from collapsing to
    % silence; do not calcify the totality argument into an invariant).
    format('  │~n', []),
    format('  │  FORENSIC VERDICT (authoritative — dr_type, post-signature):~n', []),
    (   drl_core:dr_type(C, Context, ActualType)
    ->  format('  │  → dr_type at this seat: ~w (departs from claimed mountain)~n', [ActualType])
    ;   format('  │  → dr_type: unbound (no authoritative type at this seat)~n', [])
    ),

    % Metric-level annotation (OQ-50 OPEN-1): the suppression/extractiveness
    % heuristic is downstream metric context, NOT the detector's reason — labeled
    % as an annotation so it can no longer read as the headline verdict.
    format('  │~n', []),
    format('  │  METRIC-LEVEL ANNOTATION (not the detector''s reason):~n', []),
    determine_correct_classification(Supp, Extr, Ceil, Verdict, Rationale),
    format('  │  → Metric heuristic suggests: ~w~n', [Verdict]),
    format('  │  → Rationale: ~w~n', [Rationale]),

    % Check for mandatrophy (mountain that functions as trap)
    (Extr > 0.7
    -> format('  │  ⚠ WARNING: Possible MANDATROPHY detected~n', []),
       format('  │    (Genuinely unchangeable BUT highly extractive)~n', [])
    ;  true),

    format('  └─~n~n', []).

%% determine_correct_classification(+Supp, +Extr, +Ceil, -Verdict, -Rationale)
%  Logic to determine what a false mountain should actually be classified as.
determine_correct_classification(Supp, Extr, Ceil, Verdict, Rationale) :-
    (Supp > Ceil, Extr > 0.66
    -> (Verdict = 'SNARE',
        Rationale = 'High enforcement + high extraction = extractive trap')
    ; Supp > Ceil, Extr > 0.35
    -> (Verdict = 'TANGLED_ROPE',
        Rationale = 'Moderate enforcement + moderate extraction = complex coordination with extractive elements')
    ; Supp > Ceil
    -> (Verdict = 'ROPE',
        Rationale = 'Requires enforcement but not extractive = changeable rule')
    ; Extr > 0.66
    -> (Verdict = 'SNARE (check metrics)',
        Rationale = 'High extraction despite low suppression suggests misclassification or measurement error')
    ;  (Verdict = 'AMBIGUOUS (review data)',
        Rationale = 'Metrics inconclusive, manual review required')
    ).

/* ============================================================================
   5. OMEGA RESOLUTION SCENARIOS
   ============================================================================ */

%% generate_omega_resolution_scenarios(+Subject)
%  Generates actionable test scenarios for resolving each unresolved omega.
%  This drives scenario creation by providing specific resolution strategies.
%  Subject is the report's constraint id (interval id == constraint id by
%  pipeline convention); it binds the Constraint slot for authored omegas,
%  which carry no omega_source/3 metadata (OQ-99).
generate_omega_resolution_scenarios(Subject) :-
    format('~n[OMEGA RESOLUTION SCENARIO GENERATION]~n'),
    findall(omega_data(OID, OType, ODesc, Constraint, GapPattern),
            (narrative_ontology:omega_variable(OID, OType, ODesc),
             resolve_omega_source(OID, Subject, Constraint, GapPattern)),
            Omegas),
    (Omegas = []
    -> format('  No unresolved Omegas. System is epistemically complete.~n')
    ;  (length(Omegas, N),
        format('  Generated ~w resolution scenario(s):~n~n', [N]),
        forall(member(omega_data(OID, OType, ODesc, C, Gap), Omegas),
               generate_scenario_for_omega(OID, OType, ODesc, C, Gap)))
    ).

%% resolve_omega_source(+OmegaID, +Subject, -Constraint, -GapPattern)
%  Gap-derived omegas resolve via omega_source/3 (populated at creation time
%  by assert_omega_if_new/5). Authored omegas (testset 3-arity facts) carry
%  no omega_source; in the fresh-process-per-constraint report flow
%  (enhanced_report.py:104 — one swipl per constraint) every authored omega
%  in scope is the subject story's, so the Subject binds the Constraint slot
%  when the subject's claim is in the KB. When neither holds, emit
%  unresolved_source so the scenario layer fails loud instead of fabricating
%  Constraint = unknown (OQ-99).
%  The previous omega_from_gap/5 fallback was dead code: it can only generate
%  omega_<gapname>_<C>-style IDs, which never unify with an authored omega ID
%  (OQ-99 tombstone — do not reinstate).
resolve_omega_source(OmegaID, _Subject, Constraint, GapPattern) :-
    omega_source(OmegaID, Constraint, GapPattern),
    Constraint \= unknown,
    !.
resolve_omega_source(_OmegaID, Subject, Subject, general_type_mismatch) :-
    narrative_ontology:constraint_claim(Subject, _),
    !.
resolve_omega_source(_OmegaID, _Subject, unresolved, unresolved_source).

%% generate_scenario_for_omega(+OmegaID, +Type, +Description, +Constraint, +GapPattern)
%  Generates a specific resolution scenario based on omega type and gap pattern.
%
%  Clause order is load-bearing:
%    1. unresolved_source     — fail-loud OPEN marker (broken KB state)
%    2. authored 5-arity      — authored resolution protocol (OQ-99)
%    3. typed templates       — gap-derived omegas + authored omegas
%                               lacking a 5-arity protocol
%    4. catch-all (last)      — OPEN marker so an unmatched type/gap
%                               combination never aborts the report
%                               (a failing inner goal fails the forall)

% (1) Fail-loud: resolve_omega_source found no omega_source and no subject
%     claim. Print the OPEN marker, skip the protocol steps (OQ-99).
generate_scenario_for_omega(OID, OType, _Desc, _C, unresolved_source) :-
    !,
    format('  ┌─ [~w] (~w)~n', [OID, OType]),
    format('  │  Constraint: UNRESOLVED — no omega_source and no subject claim [OPEN]~n', []),
    format('  └─~n~n', []).

% (2) Authored resolution protocol (OQ-99): the story authors a 5-arity
%     omega_variable(OID, Question, ResolutionMethod, Implications,
%     confidence_without_resolution(_)) fact. Testsets declare module
%     constraint_<id> (witnessed 2026-06-11: 60/62 files; the 2 without a
%     module header author no omega facts at all), so the facts land in
%     that module — NOT in `user` — and the lookup is keyed on the
%     constraint's own module, which also disambiguates the cross-file OID
%     collisions that exist in the live corpus (census 2026-06-11:
%     magisterial_authority_scope x4, regulatory_capture_depth x3,
%     kernel_reading_ambiguity x3, ...). PRECONDITION for binding C to the
%     report subject upstream (resolve_omega_source/4 fallback):
%     enhanced_report.py runs ONE fresh swipl process per constraint, so
%     every authored omega in scope is the subject story's; a long-lived
%     multi-testset process would attribute sibling omegas to the subject.
%     once/1 guards against within-module duplicate OIDs (census: none).
generate_scenario_for_omega(OID, OType, _Desc, C, _Gap) :-
    atom(C),
    atom_concat(constraint_, C, StoryModule),
    current_predicate(StoryModule:omega_variable/5),
    once(StoryModule:omega_variable(OID, Question, ResolutionMethod, Implications,
                                    confidence_without_resolution(Conf))),
    !,
    format('  ┌─ [~w] AUTHORED RESOLUTION PROTOCOL (~w)~n', [OID, OType]),
    format('  │  Constraint: ~w~n', [C]),
    format('  │  Question: ~w~n', [Question]),
    format('  │~n', []),
    format('  │  RESOLUTION METHOD (authored):~n', []),
    format('  │  ~w~n', [ResolutionMethod]),
    format('  │~n', []),
    format('  │  IMPLICATIONS (authored):~n', []),
    format('  │  ~w~n', [Implications]),
    format('  │~n', []),
    format('  │  Confidence without resolution: ~w~n', [Conf]),
    format('  └─~n~n', []).

% (3) Typed templates — gap-derived omegas and authored omegas with no
%     5-arity protocol fact.
generate_scenario_for_omega(OID, empirical, Desc, C, _Gap) :-
    format('  ┌─ [~w] EMPIRICAL DATA COLLECTION~n', [OID]),
    format('  │  Constraint: ~w~n', [C]),
    format('  │  Gap: ~w~n', [Desc]),
    format('  │~n', []),
    format('  │  RESOLUTION STRATEGY:~n', []),
    format('  │  1. Design measurement protocol for ~w~n', [C]),
    format('  │  2. Collect data from N=30+ real-world instances~n', []),
    format('  │  3. Calculate empirical metrics:~n', []),
    format('  │     - suppression_requirement (enforcement needed)~n', []),
    format('  │     - resistance_to_change (pushback level)~n', []),
    format('  │     - base_extractiveness (asymmetric benefit flow)~n', []),
    format('  │  4. Update constraint_metric/3 declarations with data~n', []),
    format('  │  5. Re-run classification to resolve perspectival gap~n', []),
    format('  └─~n~n', []).

generate_scenario_for_omega(OID, conceptual, Desc, C, snare_masked_as_rope) :-
    format('  ┌─ [~w] CONCEPTUAL CLARIFICATION~n', [OID]),
    format('  │  Constraint: ~w~n', [C]),
    format('  │  Gap: ~w~n', [Desc]),
    format('  │~n', []),
    format('  │  CRITICAL: Extraction Masking Detected~n', []),
    format('  │  Powerless see: SNARE (extractive trap)~n', []),
    format('  │  Institutions see: ROPE (functional rule)~n', []),
    format('  │~n', []),
    format('  │  RESOLUTION STRATEGY:~n', []),
    format('  │  1. Interview affected individuals (N=10+):~n', []),
    format('  │     - Who benefits from ~w?~n', [C]),
    format('  │     - Can you change/exit this constraint?~n', []),
    format('  │     - What would happen if you tried?~n', []),
    format('  │  2. Interview institutional actors (N=10+):~n', []),
    format('  │     - What function does ~w serve?~n', [C]),
    format('  │     - Who would object to removing it?~n', []),
    format('  │     - What alternatives exist?~n', []),
    format('  │  3. Document benefit flows:~n', []),
    format('  │     - Track who gains vs. who loses from status quo~n', []),
    format('  │     - Measure asymmetric benefit distribution~n', []),
    format('  │  4. Decision tree:~n', []),
    format('  │     IF extraction confirmed → Reclassify as SNARE~n', []),
    format('  │     IF functional & fair → Reclassify as ROPE~n', []),
    format('  │     IF context-dependent → Add indexical resolution~n', []),
    format('  └─~n~n', []).

generate_scenario_for_omega(OID, conceptual, Desc, C, snare_mountain_confusion) :-
    format('  ┌─ [~w] CONCEPTUAL CLARIFICATION~n', [OID]),
    format('  │  Constraint: ~w~n', [C]),
    format('  │  Gap: ~w~n', [Desc]),
    format('  │~n', []),
    format('  │  CRITICAL: Learned Helplessness Pattern~n', []),
    format('  │  Powerless see: SNARE (extractive trap)~n', []),
    format('  │  Institutions see: MOUNTAIN (unchangeable law)~n', []),
    format('  │~n', []),
    format('  │  RESOLUTION STRATEGY:~n', []),
    format('  │  1. Test changeability:~n', []),
    format('  │     - Can institutions modify ~w?~n', [C]),
    format('  │     - What legal/political mechanisms exist?~n', []),
    format('  │     - Historical precedents of change?~n', []),
    format('  │  2. Test extraction:~n', []),
    format('  │     - Is benefit flow symmetric or asymmetric?~n', []),
    format('  │     - Who has veto power over changes?~n', []),
    format('  │  3. Decision tree:~n', []),
    format('  │     IF truly unchangeable + extractive → MANDATROPHY~n', []),
    format('  │     IF changeable + extractive → Correct to SNARE~n', []),
    format('  │     IF unchangeable + fair → Correct to MOUNTAIN~n', []),
    format('  │     IF institutions falsely claim necessity → SNARE + fraud flag~n', []),
    format('  └─~n~n', []).

generate_scenario_for_omega(OID, conceptual, Desc, C, mountain_coordination_confusion) :-
    format('  ┌─ [~w] CONCEPTUAL CLARIFICATION~n', [OID]),
    format('  │  Constraint: ~w~n', [C]),
    format('  │  Gap: ~w~n', [Desc]),
    format('  │~n', []),
    format('  │  HIGH RISK: Coordination Cut Safety~n', []),
    format('  │  Powerless see: MOUNTAIN (unchangeable, survival-critical)~n', []),
    format('  │  Institutions see: ROPE (optional, changeable)~n', []),
    format('  │~n', []),
    format('  │  RESOLUTION STRATEGY:~n', []),
    format('  │  1. SAFETY ASSESSMENT (DO NOT SKIP):~n', []),
    format('  │     - If institutions cut ~w, do individuals have alternatives?~n', [C]),
    format('  │     - Is this their only survival mechanism?~n', []),
    format('  │     - What scaffolding exists for transition?~n', []),
    format('  │  2. Test institutional perception:~n', []),
    format('  │     - Can institutions unilaterally change this?~n', []),
    format('  │     - Do they understand downstream impacts?~n', []),
    format('  │     - Is their "optional" view empirically accurate?~n', []),
    format('  │  3. Decision tree:~n', []),
    format('  │     IF truly unchangeable → Correct institutional view to MOUNTAIN~n', []),
    format('  │     IF changeable + safe alternatives → Correct powerless view to ROPE~n', []),
    format('  │     IF changeable + NO alternatives → ADD SCAFFOLD before any change~n', []),
    format('  │     IF uncertainty → HALT changes until resolved~n', []),
    format('  │  4. CRITICAL: Never proceed with changes until safety verified~n', []),
    format('  └─~n~n', []).

generate_scenario_for_omega(OID, conceptual, Desc, C, general_type_mismatch) :-
    format('  ┌─ [~w] CONCEPTUAL CLARIFICATION~n', [OID]),
    format('  │  Constraint: ~w~n', [C]),
    format('  │  Gap: ~w~n', [Desc]),
    format('  │~n', []),
    format('  │  RESOLUTION STRATEGY:~n', []),
    format('  │  1. Map stakeholder perspectives:~n', []),
    format('  │     - Document how different actors perceive ~w~n', [C]),
    format('  │     - Identify source of divergence~n', []),
    format('  │  2. Gather evidence:~n', []),
    format('  │     - Empirical metrics (suppression, extraction, resistance)~n', []),
    format('  │     - Historical behavior patterns~n', []),
    format('  │  3. Create indexical classification:~n', []),
    format('  │     - From powerless context: classify as X~n', []),
    format('  │     - From institutional context: classify as Y~n', []),
    format('  │     - Add explicit context annotations~n', []),
    format('  └─~n~n', []).

generate_scenario_for_omega(OID, preference, Desc, C, _Gap) :-
    format('  ┌─ [~w] VALUE ARBITRATION~n', [OID]),
    format('  │  Constraint: ~w~n', [C]),
    format('  │  Gap: ~w~n', [Desc]),
    format('  │~n', []),
    format('  │  NOTE: Not resolvable via data or logic alone~n', []),
    format('  │~n', []),
    format('  │  RESOLUTION STRATEGY:~n', []),
    format('  │  1. Document competing value frameworks:~n', []),
    format('  │     - What values support current ~w?~n', [C]),
    format('  │     - What values oppose it?~n', []),
    format('  │     - Are these incommensurable?~n', []),
    format('  │  2. Propose scaffolded solution:~n', []),
    format('  │     - Design mechanism respecting both value sets~n', []),
    format('  │     - Create exit options for dissenters~n', []),
    format('  │     - Allow preference-based sorting~n', []),
    format('  │  3. Accept unresolvability if necessary:~n', []),
    format('  │     - Some omegas represent genuine value pluralism~n', []),
    format('  │     - Solution: coexistence, not consensus~n', []),
    format('  └─~n~n', []).

% (4) Catch-all — MUST BE TEXTUALLY LAST (OQ-99 fail-loud). An omega whose
%     type/gap combination matches no clause above would otherwise fail the
%     inner goal of the forall in generate_omega_resolution_scenarios/1 and
%     abort generate_full_report mid-section. Never fabricate a template:
%     print an OPEN marker and move on.
generate_scenario_for_omega(OID, OType, Desc, C, Gap) :-
    format('  ┌─ [~w] NO SCENARIO TEMPLATE [OPEN]~n', [OID]),
    format('  │  Constraint: ~w~n', [C]),
    format('  │  Unmatched type/gap combination: (~w, ~w) — no authored protocol,~n', [OType, Gap]),
    format('  │  no typed template. Graduation: author a 5-arity omega_variable~n', []),
    format('  │  protocol in the testset, or add a typed template clause.~n', []),
    format('  │  Gap: ~w~n', [Desc]),
    format('  └─~n~n', []).

/* ============================================================================
   6. OMEGA SEVERITY TRIAGE
   ============================================================================ */

%% omega_severity(+OmegaID, -Severity)
%  Prioritizes omegas by severity/urgency.
omega_severity(OID, critical) :-
    atom(OID),
    (sub_atom(OID, _, _, _, extraction_blindness)
    ; (narrative_ontology:omega_variable(OID, _, Desc),
       atom(Desc),
       (sub_atom(Desc, _, _, _, extraction)
       ; sub_atom(Desc, _, _, _, snare)
       ; sub_atom(Desc, _, _, _, 'Snare')
       ; sub_atom(Desc, _, _, _, trap)))
    ), !.

omega_severity(OID, high) :-
    atom(OID),
    (sub_atom(OID, _, _, _, learned_helplessness)
    ; sub_atom(OID, _, _, _, cut_safety)
    ; narrative_ontology:omega_variable(OID, conceptual, _)
    ), !.

omega_severity(OID, moderate) :-
    atom(OID),
    narrative_ontology:omega_variable(OID, empirical, _), !.

omega_severity(OID, low) :-
    atom(OID),
    narrative_ontology:omega_variable(OID, preference, _), !.

omega_severity(_, unknown).

%% generate_omega_triage/0
%  Displays omegas organized by severity level.
%  Computes each omega's severity exactly once via once/1 with Sev unbound,
%  so the first matching clause wins and no omega appears in multiple buckets.
generate_omega_triage :-
    format('~n[OMEGA TRIAGE & PRIORITIZATION]~n'),
    findall(Sev-OID,
            (narrative_ontology:omega_variable(OID, _, _),
             once(omega_severity(OID, Sev))),
            Pairs),
    (Pairs = []
    -> format('  No omegas to triage.~n')
    ;  forall(member(Level, [critical, high, moderate, low]),
              (findall(OID, member(Level-OID, Pairs), OIDs),
               (OIDs \= []
               -> (length(OIDs, N),
                   format('~n  [~w] ~w omega(s):~n', [Level, N]),
                   forall(member(OID, OIDs),
                          (narrative_ontology:omega_variable(OID, Type, Desc),
                           format('    - ~w (~w)~n      ~w~n', [OID, Type, Desc]))))
               ;  true)))
    ).

/* ============================================================================
   7. COMPREHENSIVE CROSS-DOMAIN AUDIT
   ============================================================================ */

%% cross_domain_audit/0
%  Scans ALL constraints in current KB and reports cross-domain structural twins.
%  This provides a comprehensive view of isomorphic patterns across different domains.
cross_domain_audit :-
    findall(iso(C1, C2, Score, Cat1, Cat2),
            (narrative_ontology:constraint_claim(C1, _),
             domain_priors:category_of(C1, Cat1),
             isomorphism_engine:find_isomorphism(C1, C2, Score),
             domain_priors:category_of(C2, Cat2),
             Cat1 \= Cat2,  % Only cross-domain twins
             C1 @< C2),     % Prevent duplicate pairs (A,B) and (B,A)
            Isos),
    (Isos = []
    -> true  % Suppress entirely when no cross-domain twins found
    ;  (format('~n[COMPREHENSIVE CROSS-DOMAIN STRUCTURAL TWINS]~n'),
        length(Isos, N),
        format('  Found ~w cross-domain structural twins:~n~n', [N]),
        forall(member(iso(C1, C2, S, Cat1, Cat2), Isos),
               (format('  ~w (~w) ≈ ~w (~w)~n', [C1, Cat1, C2, Cat2]),
                format('    Similarity Score: ~2f~n', [S]),
                display_twin_rationale(C1, C2),
                nl)))
    ).

%% display_twin_rationale(+C1, +C2)
%  Explains why two constraints are considered structural twins.
display_twin_rationale(C1, C2) :-
    (signature_detection:get_constraint_profile(C1, Profile1),
     signature_detection:get_constraint_profile(C2, Profile2)
    -> (Profile1 = profile(A1, S1, R1, B1, Alt1, _, _),
        Profile2 = profile(A2, S2, R2, B2, Alt2, _, _),
        format('    Metrics: ', []),
        (abs(A1 - A2) < 0.15 -> format('Accum≈ ', []) ; true),
        (abs(S1 - S2) < 0.15 -> format('Supp≈ ', []) ; true),
        (abs(R1 - R2) < 0.15 -> format('Repr≈ ', []) ; true),
        (abs(B1 - B2) < 1.0 -> format('BenefΔ≈ ', []) ; true),
        (Alt1 = Alt2 -> format('AltMatch ', []) ; true),
        nl,
        format('    Implication: Solutions for ~w may inform ~w~n', [C1, C2]))
    ;  format('    (Profile data incomplete)~n')
    ).
