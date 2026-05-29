% ============================================================================
% TEST: cs_axiom_foreclosed/2 — COMPUTED FORECLOSURE
% ============================================================================
% Validates the routing predicate against kernel_run_01 data.
%
% The predicate fires when three authored conditions compound:
%   1. cs_axiom_grounding(C, Atom, empirically_contingent)
%   2. cs_drift_state(C, _, gap(axiom_overriding, Magnitude, false))
%   3. Magnitude \= minor
%
% Test structure:
%   FIRE cases — empirically_contingent + axiom_overriding + substantial/severe + false
%   NO-FIRE cases:
%     - Deontological grounding (retributive, categorical — must NOT fire)
%     - Acknowledged drift (categorical_impermissibility practice_drift — must NOT fire)
%     - Non-axiom_overriding direction (retributive authority_erosion — must NOT fire)
%     - Minor magnitude (not present in kernel_run_01 but tested by attractor table)
%
% Boundary: prohibition_reading's criminal_deterrence_reduces_drug_use is authored
%   overridden + empirically_contingent + axiom_overriding + false. The predicate
%   fires: overridden (authored, tradition-internal) and foreclosed (computed,
%   evidential) are independent signals that coexist.
%
% Corpus-wide check: no spurious firings on deontological axioms.
% ============================================================================

:- use_module(cs_axiom_engine).
:- use_module(narrative_ontology).
:- use_module(library(plunit)).

% Load kernel_run_01 testsets (needed for drift_state and cs_axiom facts)
:- forall(
       (   absolute_file_name('testsets/kernel_run_01', Dir, [file_type(directory)]),
           directory_files(Dir, Files),
           member(Base, Files),
           file_name_extension(_, pl, Base),
           atomic_list_concat([Dir, '/', Base], F)
       ),
       catch(load_files([F], []), _, true)
   ).

% ---------------------------------------------------------------------------
% Authored grounding facts for key readings (not in kernel_run_01 yet —
% this is the field added to the schema/prompt for the NEXT regen).
% Hand-authored here to validate routing against existing drift_state data.
% ---------------------------------------------------------------------------

:- multifile narrative_ontology:cs_axiom_grounding/3.

% deterrence_instrument: the empirical deterrence claim is grounding-contingent;
%   the rationality claim is instrumental (normative about means, not falsifiable by
%   deterrence failure evidence alone)
narrative_ontology:cs_axiom_grounding(deterrence_instrument,
    deterrence_empirically_contingent_legitimacy, empirically_contingent).
narrative_ontology:cs_axiom_grounding(deterrence_instrument,
    instrumental_rationality_justifies_death, instrumental).

% prohibition_reading: boundary case
%   criminal_deterrence_reduces_drug_use → empirically_contingent (testable, refuted)
%   drug_use_as_moral_failure_requiring_constraint → deontological (not evidence-dependent)
narrative_ontology:cs_axiom_grounding(prohibition_reading,
    criminal_deterrence_reduces_drug_use, empirically_contingent).
narrative_ontology:cs_axiom_grounding(prohibition_reading,
    drug_use_as_moral_failure_requiring_constraint, deontological).

% retributive_desert: both axioms are deontological (desert / proportionality)
narrative_ontology:cs_axiom_grounding(retributive_desert,
    murder_forfeits_moral_status, deontological).
narrative_ontology:cs_axiom_grounding(retributive_desert,
    proportionate_response_legitimacy, deontological).

% categorical_impermissibility: both axioms are deontological (inalienability)
narrative_ontology:cs_axiom_grounding(categorical_impermissibility,
    inalienability_categorical, deontological).
narrative_ontology:cs_axiom_grounding(categorical_impermissibility,
    state_killing_illegitimate, deontological).

% ---------------------------------------------------------------------------
% TESTS
% ---------------------------------------------------------------------------

:- begin_tests(cs_axiom_foreclosed).

% --- FIRE: deterrence_instrument's empirical claim ---
% gap(axiom_overriding, substantial, false) + empirically_contingent → foreclosed
test(fire_deterrence_empirical) :-
    cs_axiom_engine:cs_axiom_foreclosed(deterrence_instrument,
        deterrence_empirically_contingent_legitimacy).

% --- NO-FIRE: instrumental grounding does not route foreclosed ---
test(no_fire_deterrence_instrumental, [fail]) :-
    cs_axiom_engine:cs_axiom_foreclosed(deterrence_instrument,
        instrumental_rationality_justifies_death).

% --- BOUNDARY: prohibition_reading criminal_deterrence is authored overridden
%     AND computes foreclosed — the two signals coexist independently ---
test(boundary_prohibition_empirical_fires) :-
    cs_axiom_engine:cs_axiom_foreclosed(prohibition_reading,
        criminal_deterrence_reduces_drug_use).

% The authored overridden status is still present (not removed by computed foreclosure)
test(boundary_prohibition_overridden_still_authored) :-
    narrative_ontology:cs_axiom_status(criminal_deterrence_reduces_drug_use, overridden).

% --- NO-FIRE: deontological axiom in prohibition_reading ---
test(no_fire_prohibition_deontological, [fail]) :-
    cs_axiom_engine:cs_axiom_foreclosed(prohibition_reading,
        drug_use_as_moral_failure_requiring_constraint).

% --- NO-FIRE: retributive drift is authority_erosion, not axiom_overriding ---
test(no_fire_retributive_wrong_direction, [fail]) :-
    cs_axiom_engine:cs_axiom_foreclosed(retributive_desert, murder_forfeits_moral_status).

test(no_fire_retributive_proportionality, [fail]) :-
    cs_axiom_engine:cs_axiom_foreclosed(retributive_desert, proportionate_response_legitimacy).

% --- NO-FIRE: categorical_impermissibility drift is practice_drift + acknowledged ---
test(no_fire_categorical_acknowledged, [fail]) :-
    cs_axiom_engine:cs_axiom_foreclosed(categorical_impermissibility, inalienability_categorical).

% --- CORPUS BOUNDARY: no deontological axiom routes foreclosed ---
% All axioms with deontological grounding must NOT fire cs_axiom_foreclosed.
test(no_deontological_fires) :-
    findall(C-A, (
        narrative_ontology:cs_axiom_grounding(C, A, deontological),
        cs_axiom_engine:cs_axiom_foreclosed(C, A)
    ), Spurious),
    Spurious == [].

% --- CORPUS COUNT: report all foreclosed routings ---
% Not a pass/fail — documents what fires. Deterrence + prohibition minimum expected.
test(corpus_foreclosed_count) :-
    findall(C-A, cs_axiom_engine:cs_axiom_foreclosed(C, A), Foreclosed),
    length(Foreclosed, N),
    format("  Foreclosed routings in corpus: ~w~n", [N]),
    forall(member(C-A, Foreclosed),
           format("    ~w: ~w~n", [C, A])),
    N >= 2.  % at minimum deterrence_instrument + prohibition_reading

:- end_tests(cs_axiom_foreclosed).
