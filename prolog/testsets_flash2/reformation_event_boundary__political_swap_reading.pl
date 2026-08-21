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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:epsilon_provenance/5,
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
 *   realignment, where secular rulers exploited theological disputes to break
 *   papal authority and seize church assets. Theology, in this reading,
 *   served as a post-hoc rationalization for power consolidation. The
 *   periodization extends to the Peace of Westphalia (1648), marking the
 *   stabilization of the new political order. This is one reading of the
 *   'reformation_event_boundary' kernel.
 *
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
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_event_boundary__political_swap_reading, tangled_rope).
narrative_ontology:human_readable(reformation_event_boundary__political_swap_reading, "Reformation as Political Power Swap (Political Swap Reading)").
narrative_ontology:topic_domain(reformation_event_boundary__political_swap_reading, "historical_epistemology/religious_history/commitment_system_analysis").

domain_priors:requires_active_enforcement(reformation_event_boundary__political_swap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_event_boundary__political_swap_reading, '00b57948-634d-4092-95fa-242695057ad1').
narrative_ontology:cs_kernel_codification('00b57948-634d-4092-95fa-242695057ad1', formalized).
narrative_ontology:cs_authority_grounding('00b57948-634d-4092-95fa-242695057ad1', extraction).
narrative_ontology:cs_interpretation_layer_present('00b57948-634d-4092-95fa-242695057ad1').
narrative_ontology:cs_reading_relation('00b57948-634d-4092-95fa-242695057ad1', reformation_event_boundary__theological_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('00b57948-634d-4092-95fa-242695057ad1', reformation_event_boundary__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('00b57948-634d-4092-95fa-242695057ad1', foundational, theology_as_instrument_of_power).
narrative_ontology:cs_axiom_status(theology_as_instrument_of_power, holdable).
narrative_ontology:cs_axiom_grounding('00b57948-634d-4092-95fa-242695057ad1', theology_as_instrument_of_power, empirically_contingent).
narrative_ontology:cs_axiom('00b57948-634d-4092-95fa-242695057ad1', foundational, state_sovereignty_over_church_affairs).
narrative_ontology:cs_axiom_status(state_sovereignty_over_church_affairs, holdable).
narrative_ontology:cs_axiom_grounding('00b57948-634d-4092-95fa-242695057ad1', state_sovereignty_over_church_affairs, conventional).
narrative_ontology:cs_reference_frame('00b57948-634d-4092-95fa-242695057ad1', medieval_papal_supremacy).
narrative_ontology:cs_drift_state('00b57948-634d-4092-95fa-242695057ad1', post_westphalian_era, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('00b57948-634d-4092-95fa-242695057ad1', '').
narrative_ontology:cs_kernel_id(reformation_event_boundary__political_swap_reading, reformation_event_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_event_boundary__political_swap_reading, secular_rulers).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__political_swap_reading, protestant_nobility).
narrative_ontology:constraint_victim(reformation_event_boundary__political_swap_reading, catholic_church).
narrative_ontology:constraint_victim(reformation_event_boundary__political_swap_reading, papal_authority).
narrative_ontology:constraint_victim(reformation_event_boundary__political_swap_reading, peasantry).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__political_swap_reading, theologians_and_reformers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exploited theological disputes to assert sovereignty over religious affairs, seize church lands, and consolidate political power, becoming the primary beneficiaries of the shift in authority and assets. They actively enforced the new religious order within their territories.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, secular_rulers, agenda_setter,
    institutional, generational, arbitrage, regional).

% Gained significant wealth and influence through the confiscation of Catholic Church property and the establishment of state-controlled churches. They supported secular rulers in breaking papal authority.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, protestant_nobility, beneficiary,
    powerful, generational, mobile, regional).

% Suffered immense loss of temporal power, land, and revenue as secular rulers broke away from papal authority. Its assets were seized, and its spiritual authority challenged, leading to a significant reduction in its influence in many regions.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, catholic_church, payer,
    institutional, civilizational, trapped, global).

% Lost direct control over vast territories and populations, as well as the ability to levy taxes and appoint clergy without secular interference. Its claims to universal spiritual and temporal supremacy were severely undermined.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, papal_authority, payer,
    institutional, civilizational, trapped, global).

% Often caught in the middle of religious conflicts, forced to convert according to their ruler's religion (Cuius regio, eius religio), and sometimes subjected to increased taxation or military conscription as rulers consolidated power. Their theological concerns were secondary to political maneuvering.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, peasantry, payer,
    powerless, biographical, trapped, local).

% Provided the intellectual and doctrinal framework for the theological disputes, but their ideas were often co-opted and instrumentalized by secular powers for political ends. They gained influence and patronage from rulers who supported their reforms.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, theologians_and_reformers, agenda_setter,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(reformation_event_boundary__political_swap_reading, theologians_and_reformers, beneficiary).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enabled secular rulers to coordinate their efforts to challenge and ultimately break free from the overarching authority of the Catholic Church and the Holy Roman Empire, establishing independent state control over religious and temporal affairs.
% TRANSFER_FUNCTION: Transferred vast amounts of wealth (church lands, tithes, assets) and authority (appointment of clergy, religious law) from the Catholic Church and papal authority to secular rulers and their supporting nobility.
% ABSENT_VOICES: The voices of those who genuinely sought theological reform without political opportunism, or those who suffered under the new secular religious authorities, were often suppressed or marginalized. Their concerns were subsumed by the larger political power struggle.
% DISAPPEARANCE_RATIONALE: If the political realignment aspect of the Reformation vanished, the modern nation-state system, the separation of church and state, and the distribution of wealth and power in early modern Europe would be fundamentally different. The entire political landscape of the West would be unrecognizable.
% FOUNDING_PROBLEM: Secular rulers faced a powerful, transnational Catholic Church that claimed significant temporal authority, levied taxes, owned vast lands, and interfered in state affairs, limiting their sovereignty and access to resources.
% FOUNDING_PROBLEM_CORROBORATION: Historians widely corroborate that secular rulers indeed sought to consolidate power and seize assets. The problem of papal temporal authority over states is largely 'dead' in the post-Westphalian era, yet the institutional structures born from this 'swap' persist. Independent historical analyses from outside the benefiting parties (e.g., modern secular historians, economic historians) support this view.
narrative_ontology:disappearance_verdict(reformation_event_boundary__political_swap_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_event_boundary__political_swap_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_event_boundary__political_swap_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(reformation_event_boundary__political_swap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_event_boundary__political_swap_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high because the transfer of wealth and authority from the Church to secular rulers was substantial and asymmetric. Suppression is high due to the active enforcement of new religious settlements (e.g., 'Cuius regio, eius religio') and the suppression of dissenting religious practices. The theater ratio is significant because the theological debates, while real, are seen as largely instrumental to the underlying political power grab, with much of the 'religious' activity serving to legitimize secular control.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of secular rulers, this was a necessary assertion of sovereignty and a rationalization of state power. From the perspective of the Catholic Church, it was an illegitimate usurpation of spiritual and temporal authority. The engine's classification will highlight this divergence, showing a 'tangled_rope' for the overall constraint, but with different effective extraction for each seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Secular rulers and Protestant nobility are clear beneficiaries (low d) as they gained wealth and authority. The Catholic Church and papal authority are clear victims (high d) due to asset seizure and loss of power. The peasantry are also victims (high d) as they bore the costs of religious wars and forced conversions. Theologians, while active, are seen as instrumentalized, benefiting from patronage but ultimately serving the political agenda.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_sincerity_vs_instrumentalization,
    'To what extent were the theological disputes genuine expressions of faith and doctrine, versus instrumental tools for political maneuvering?',
    'Detailed textual analysis of theological writings, combined with biographical studies of reformers'' motivations and their political patrons'' actions, seeking evidence of independent theological development versus direct political commissioning.',
    'If theological sincerity was high, the ''theater_ratio'' would be lower, and the ''claimed_type'' might shift towards a more complex ''tangled_rope'' or even ''rope'' for the theological dimension. If instrumentalization was dominant, the current high ''theater_ratio'' and ''tangled_rope'' classification are reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_sincerity_vs_instrumentalization, empirical, 'Ambiguity regarding the primary driver of theological change: genuine belief or political utility.').

omega_variable(
    periodization_boundary_ambiguity,
    'Is the Peace of Westphalia (1648) the appropriate endpoint for the ''political swap'' reading, or does the political realignment continue beyond this point, or conclude earlier?',
    'Analysis of state-church relations and international treaties post-1648 to determine if the fundamental political settlement regarding religious sovereignty was truly stable, or if significant shifts continued.',
    'An earlier conclusion would suggest the ''swap'' was more rapid; a later one would extend the period of active political contestation and asset transfer. This would affect the trajectory of ''base_extractiveness'' and ''suppression_requirement'' measurements.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(periodization_boundary_ambiguity, conceptual, 'Uncertainty about the temporal boundaries of the political realignment event.').

omega_variable(
    political_vs_theological_framing,
    'Is the Reformation best understood through a political lens (this reading) or a theological lens (theological_climb_reading)?',
    'This is a conceptual omega. Resolution depends on the analytical framework adopted by the historian or observer. No single empirical test can definitively resolve it, as both political and theological dimensions were undeniably present.',
    'Adopting the theological lens would lead to a different constraint story with lower ''extractiveness'' and ''suppression'' for the theological core, and a different ''claimed_type'' (likely ''rope'' or ''mountain'' for the doctrinal breakthrough itself).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(political_vs_theological_framing, conceptual, 'The fundamental framing choice between political and theological causality for the Reformation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_event_boundary__political_swap_reading, 1517, 1648).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t1517, reformation_event_boundary__political_swap_reading, theater_ratio, 1517, 0.3).
narrative_ontology:measurement(refo_tr_t1540, reformation_event_boundary__political_swap_reading, theater_ratio, 1540, 0.45).
narrative_ontology:measurement(refo_tr_t1570, reformation_event_boundary__political_swap_reading, theater_ratio, 1570, 0.55).
narrative_ontology:measurement(refo_tr_t1600, reformation_event_boundary__political_swap_reading, theater_ratio, 1600, 0.6).
narrative_ontology:measurement(refo_tr_t1648, reformation_event_boundary__political_swap_reading, theater_ratio, 1648, 0.6).

% Extraction over time
narrative_ontology:measurement(refo_be_t1517, reformation_event_boundary__political_swap_reading, base_extractiveness, 1517, 0.6).
narrative_ontology:measurement(refo_be_t1540, reformation_event_boundary__political_swap_reading, base_extractiveness, 1540, 0.75).
narrative_ontology:measurement(refo_be_t1570, reformation_event_boundary__political_swap_reading, base_extractiveness, 1570, 0.82).
narrative_ontology:measurement(refo_be_t1600, reformation_event_boundary__political_swap_reading, base_extractiveness, 1600, 0.85).
narrative_ontology:measurement(refo_be_t1648, reformation_event_boundary__political_swap_reading, base_extractiveness, 1648, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t1517, reformation_event_boundary__political_swap_reading, suppression_requirement, 1517, 0.5).
narrative_ontology:measurement(refo_su_t1540, reformation_event_boundary__political_swap_reading, suppression_requirement, 1540, 0.65).
narrative_ontology:measurement(refo_su_t1570, reformation_event_boundary__political_swap_reading, suppression_requirement, 1570, 0.75).
narrative_ontology:measurement(refo_su_t1600, reformation_event_boundary__political_swap_reading, suppression_requirement, 1600, 0.78).
narrative_ontology:measurement(refo_su_t1648, reformation_event_boundary__political_swap_reading, suppression_requirement, 1648, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_event_boundary__political_swap_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(reformation_event_boundary__political_swap_reading, reformation_event_boundary__theological_climb_reading).
narrative_ontology:affects_constraint(reformation_event_boundary__political_swap_reading, reformation_event_boundary__composite_overdetermination_reading).
narrative_ontology:affects_constraint(reformation_event_boundary__political_swap_reading, westphalian_sovereignty_principle).

% DUAL FORMULATION NOTE:
% This is the 'political_swap_reading' of the 'reformation_event_boundary' kernel. It emphasizes the political realignment and asset transfer, contrasting with the 'theological_climb_reading' and the 'composite_overdetermination_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
