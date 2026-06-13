% ============================================================================
% CONSTRAINT STORY: reformation_event_boundary__political_swap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformation_event_boundary__political_swap_reading, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: reformation_event_boundary__political_swap_reading
 *   human_readable: Reformation as Political Power Swap (Political Swap Reading)
 *   domain: historical_epistemology/religious_history/commitment_system_analysis
 *
 * SUMMARY:
 *   This constraint models the Reformation as primarily a political
 *   realignment event, where secular rulers leveraged theological disputes to
 *   dismantle papal authority and appropriate church wealth. Theology, in
 *   this reading, served as a post-hoc rationalization or a 'scaffold' for
 *   consolidating secular power. The periodization extends from Luther's 95
 *   Theses (1517) to the Peace of Westphalia (1648), which solidified the
 *   political settlement of religious divisions. This is one reading of the
 *   'reformation_event_boundary' kernel, emphasizing the 'political swap'
 *   aspect.
 *
 * KEY AGENTS:
 *   - secular_rulers: Primary beneficiary (institutional/generational) — gained authority and assets.
 *   - protestant_nobility: Secondary beneficiary (powerful/biographical) — gained land and influence.
 *   - catholic_church: Primary victim (institutional/generational) — lost authority, assets, and political power.
 *   - papal_authority: Primary victim (institutional/generational) — target of direct challenge and erosion.
 *   - catholic_clergy: Secondary victim (moderate/biographical) — lost status, land, and income.
 *   - theologians: Agenda setter/Payer (moderate/biographical) — provided the intellectual framework, but their work was exploited and often led to personal risk.
 *   - peasantry: Payer (powerless/immediate) — often caught in religious wars, suffering economic and social disruption.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_event_boundary__political_swap_reading, 0.85).
domain_priors:suppression_score(reformation_event_boundary__political_swap_reading, 0.75).
domain_priors:theater_ratio(reformation_event_boundary__political_swap_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_event_boundary__political_swap_reading, tangled_rope).
narrative_ontology:human_readable(reformation_event_boundary__political_swap_reading, "Reformation as Political Power Swap (Political Swap Reading)").
narrative_ontology:topic_domain(reformation_event_boundary__political_swap_reading, "historical_epistemology/religious_history/commitment_system_analysis").

domain_priors:requires_active_enforcement(reformation_event_boundary__political_swap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_event_boundary__political_swap_reading, 'f42d2b00-809f-40af-bf21-b2031eea87ec').
narrative_ontology:cs_kernel_codification('f42d2b00-809f-40af-bf21-b2031eea87ec', formalized).
narrative_ontology:cs_authority_grounding('f42d2b00-809f-40af-bf21-b2031eea87ec', extraction).
narrative_ontology:cs_interpretation_layer_present('f42d2b00-809f-40af-bf21-b2031eea87ec').
narrative_ontology:cs_reading_relation('f42d2b00-809f-40af-bf21-b2031eea87ec', reformation_event_boundary__theological_climb_reading, forecloses).
narrative_ontology:cs_reading_relation('f42d2b00-809f-40af-bf21-b2031eea87ec', reformation_event_boundary__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('f42d2b00-809f-40af-bf21-b2031eea87ec', foundational, theology_as_instrument_of_power).
narrative_ontology:cs_axiom_status(theology_as_instrument_of_power, holdable).
narrative_ontology:cs_axiom_grounding('f42d2b00-809f-40af-bf21-b2031eea87ec', theology_as_instrument_of_power, empirically_contingent).
narrative_ontology:cs_axiom('f42d2b00-809f-40af-bf21-b2031eea87ec', foundational, state_sovereignty_over_church).
narrative_ontology:cs_axiom_status(state_sovereignty_over_church, holdable).
narrative_ontology:cs_axiom_grounding('f42d2b00-809f-40af-bf21-b2031eea87ec', state_sovereignty_over_church, conventional).
narrative_ontology:cs_reference_frame('f42d2b00-809f-40af-bf21-b2031eea87ec', medieval_dual_authority_system).
narrative_ontology:cs_drift_state('f42d2b00-809f-40af-bf21-b2031eea87ec', peace_of_westphalia_1648, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('f42d2b00-809f-40af-bf21-b2031eea87ec', '').
narrative_ontology:cs_kernel_id(reformation_event_boundary__political_swap_reading, reformation_event_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_event_boundary__political_swap_reading, secular_rulers).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__political_swap_reading, protestant_nobility).
narrative_ontology:constraint_victim(reformation_event_boundary__political_swap_reading, catholic_church).
narrative_ontology:constraint_victim(reformation_event_boundary__political_swap_reading, papal_authority).
narrative_ontology:constraint_victim(reformation_event_boundary__political_swap_reading, catholic_clergy).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_event_boundary__political_swap_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(reformation_event_boundary__political_swap_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_event_boundary__political_swap_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reformation_event_boundary__political_swap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reformation_event_boundary__political_swap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because vast wealth and authority were transferred from the Church to secular powers. Suppression (0.75) reflects the active coercion (wars, confiscations, legal changes) required to enforce this new order and suppress Catholic resistance. The theater ratio (0.6) is high because while theological arguments were presented as the primary drivers, their function was largely performative, masking the underlying power grab. Accessibility collapse is moderate (0.4) as alternatives (remaining Catholic, forming new sects) were suppressed but not entirely eliminated. Resistance is high (0.8) due to prolonged religious wars and counter-reformation efforts.
 *
 * PERSPECTIVAL GAP:
 *   Secular rulers experienced this as a beneficial reordering, a 'rope' or even a 'mountain' of historical inevitability. The Catholic Church experienced it as a 'snare' of asset stripping and authority erosion. Theologians, initially 'agenda setters' for doctrinal reform, found their work co-opted, becoming 'payers' of the political costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Secular rulers and Protestant nobility are clear beneficiaries (d=0.0-0.2) as they gained assets and autonomy. The Catholic Church, papal authority, and Catholic clergy are clear victims (d=0.8-1.0) due to asset seizure and loss of power. Theologians, while initiating disputes, became tools in a larger political game, bearing costs (d=0.5-0.7). The peasantry bore the diffuse costs of conflict (d=0.7-0.9).
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling the Reformation as a purely theological 'rope' or 'mountain' by highlighting the active enforcement and asymmetric extraction. The 'theology as scaffold' framing acknowledges the initial coordination function (providing a justification for change) but emphasizes its temporary, instrumental role in facilitating a power transfer, rather than being an end in itself. The high theater ratio and extractiveness indicate that the mandate (theological purity) atrophied into a cover for political gain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_causality_ambiguity,
    'Was theological dispute a genuine causal driver of the Reformation, or merely a post-hoc rationalization for political and economic motives?',
    'Detailed historical analysis of primary sources, focusing on the timing and sincerity of theological arguments relative to political actions and asset seizures. Examination of counterfactuals where theological disputes existed without political opportunity.',
    'If theological causality is strong, the constraint shifts towards a more ''composite'' or ''theological climb'' reading, reducing the ''political swap'' reading''s extractiveness and theater ratio. If post-hoc rationalization is confirmed, the current metrics are reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_causality_ambiguity, empirical, 'Ambiguity regarding the causal role of theology in the Reformation.').

omega_variable(
    reformation_periodization_ambiguity,
    'Does the ''Reformation'' as an event primarily conclude with the Peace of Westphalia (1648), marking a political settlement, or is it a longer, more diffuse process of theological and social change?',
    'Analysis of the long-term institutional and theological trajectories post-1648, and the extent to which the political settlement truly ''resolved'' the underlying conflicts or merely reconfigured them.',
    'A shorter, politically defined periodization reinforces the ''political swap'' reading. A longer, more diffuse periodization would lend credence to ''theological climb'' or ''composite overdetermination'' readings, suggesting the political settlement was not the sole or primary outcome.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reformation_periodization_ambiguity, conceptual, 'Contested periodization of the Reformation event.').

omega_variable(
    kernel_reading_identification,
    'This constraint is the ''political_swap_reading'' of the ''reformation_event_boundary'' kernel. What would change if a sibling reading were adopted?',
    'Comparing the structural elements (beneficiaries, victims, claimed type, core axioms) of this reading with ''theological_climb_reading'' and ''composite_overdetermination_reading''.',
    'Adopting ''theological_climb_reading'' would shift the primary driver to theological innovation, reducing extractiveness and increasing the ''rope'' aspect of theological coordination. Adopting ''composite_overdetermination_reading'' would acknowledge multiple irreducible drivers, making the constraint''s classification more complex and less focused on pure extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is one reading of the ''reformation_event_boundary'' kernel, focusing on political realignment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_event_boundary__political_swap_reading, 1517, 1648).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t1517, reformation_event_boundary__political_swap_reading, theater_ratio, 1517, 0.2).
narrative_ontology:measurement(refo_tr_t1534, reformation_event_boundary__political_swap_reading, theater_ratio, 1534, 0.4).
narrative_ontology:measurement(refo_tr_t1555, reformation_event_boundary__political_swap_reading, theater_ratio, 1555, 0.55).
narrative_ontology:measurement(refo_tr_t1618, reformation_event_boundary__political_swap_reading, theater_ratio, 1618, 0.58).
narrative_ontology:measurement(refo_tr_t1648, reformation_event_boundary__political_swap_reading, theater_ratio, 1648, 0.6).

% Extraction over time
narrative_ontology:measurement(refo_be_t1517, reformation_event_boundary__political_swap_reading, base_extractiveness, 1517, 0.4).
narrative_ontology:measurement(refo_be_t1534, reformation_event_boundary__political_swap_reading, base_extractiveness, 1534, 0.65).
narrative_ontology:measurement(refo_be_t1555, reformation_event_boundary__political_swap_reading, base_extractiveness, 1555, 0.78).
narrative_ontology:measurement(refo_be_t1618, reformation_event_boundary__political_swap_reading, base_extractiveness, 1618, 0.82).
narrative_ontology:measurement(refo_be_t1648, reformation_event_boundary__political_swap_reading, base_extractiveness, 1648, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t1517, reformation_event_boundary__political_swap_reading, suppression_requirement, 1517, 0.3).
narrative_ontology:measurement(refo_su_t1534, reformation_event_boundary__political_swap_reading, suppression_requirement, 1534, 0.55).
narrative_ontology:measurement(refo_su_t1555, reformation_event_boundary__political_swap_reading, suppression_requirement, 1555, 0.7).
narrative_ontology:measurement(refo_su_t1618, reformation_event_boundary__political_swap_reading, suppression_requirement, 1618, 0.73).
narrative_ontology:measurement(refo_su_t1648, reformation_event_boundary__political_swap_reading, suppression_requirement, 1648, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_event_boundary__political_swap_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(reformation_event_boundary__political_swap_reading, state_sovereignty_doctrine).
narrative_ontology:affects_constraint(reformation_event_boundary__political_swap_reading, religious_pluralism_norm).
narrative_ontology:affects_constraint(reformation_event_boundary__political_swap_reading, modern_nation_state_formation).

% DUAL FORMULATION NOTE:
% This constraint is the 'political_swap_reading' of the 'reformation_event_boundary' kernel. It emphasizes the political and economic drivers, contrasting with 'theological_climb_reading' (focus on doctrinal innovation) and 'composite_overdetermination_reading' (irreducible multiple causes).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
