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
 *   human_readable: Reformation as Political Realignment (Political Swap Reading)
 *   domain: historical_epistemology/religious_history/commitment_system_analysis
 *
 * SUMMARY:
 *   This constraint models the Reformation as a political realignment, where
 *   secular rulers exploited theological disputes to break papal authority
 *   and seize church assets. Theology, in this reading, served as a post-hoc
 *   rationalization for power consolidation. The periodization extends to the
 *   Peace of Westphalia (1648), which solidified the political settlement.
 *   This is one reading of the 'reformation_event_boundary' kernel.
 *
 * KEY AGENTS:
 *   - secular_rulers: Primary beneficiary and agenda-setter (institutional/arbitrage)
 *   - protestant_nobility: Secondary beneficiary (powerful/mobile)
 *   - catholic_church: Primary victim (institutional/trapped)
 *   - papal_authority: Primary victim (institutional/trapped)
 *   - catholic_clergy: Secondary victim (moderate/constrained)
 *   - theologians_and_reformers: Observer (moderate/constrained)
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
narrative_ontology:human_readable(reformation_event_boundary__political_swap_reading, "Reformation as Political Realignment (Political Swap Reading)").
narrative_ontology:topic_domain(reformation_event_boundary__political_swap_reading, "historical_epistemology/religious_history/commitment_system_analysis").

domain_priors:requires_active_enforcement(reformation_event_boundary__political_swap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_event_boundary__political_swap_reading, '56d79f35-5f95-4d67-b84a-ac09402865ea').
narrative_ontology:cs_kernel_codification('56d79f35-5f95-4d67-b84a-ac09402865ea', formalized).
narrative_ontology:cs_authority_grounding('56d79f35-5f95-4d67-b84a-ac09402865ea', extraction).
narrative_ontology:cs_interpretation_layer_present('56d79f35-5f95-4d67-b84a-ac09402865ea').
narrative_ontology:cs_reading_relation('56d79f35-5f95-4d67-b84a-ac09402865ea', reformation_event_boundary__theological_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('56d79f35-5f95-4d67-b84a-ac09402865ea', reformation_event_boundary__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('56d79f35-5f95-4d67-b84a-ac09402865ea', foundational, theology_as_instrumental_rationalization).
narrative_ontology:cs_axiom_status(theology_as_instrumental_rationalization, holdable).
narrative_ontology:cs_axiom_grounding('56d79f35-5f95-4d67-b84a-ac09402865ea', theology_as_instrumental_rationalization, empirically_contingent).
narrative_ontology:cs_axiom('56d79f35-5f95-4d67-b84a-ac09402865ea', foundational, state_sovereignty_over_ecclesiastical_authority).
narrative_ontology:cs_axiom_status(state_sovereignty_over_ecclesiastical_authority, holdable).
narrative_ontology:cs_axiom_grounding('56d79f35-5f95-4d67-b84a-ac09402865ea', state_sovereignty_over_ecclesiastical_authority, conventional).
narrative_ontology:cs_reference_frame('56d79f35-5f95-4d67-b84a-ac09402865ea', pre_reformation_papal_hegemony).
narrative_ontology:cs_drift_state('56d79f35-5f95-4d67-b84a-ac09402865ea', peace_of_westphalia_1648, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('56d79f35-5f95-4d67-b84a-ac09402865ea', '').
narrative_ontology:cs_kernel_id(reformation_event_boundary__political_swap_reading, reformation_event_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_event_boundary__political_swap_reading, secular_rulers).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__political_swap_reading, protestant_nobility).
narrative_ontology:constraint_victim(reformation_event_boundary__political_swap_reading, catholic_church).
narrative_ontology:constraint_victim(reformation_event_boundary__political_swap_reading, papal_authority).
narrative_ontology:constraint_victim(reformation_event_boundary__political_swap_reading, catholic_clergy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exploited theological disputes to assert political independence from papal authority, seize church lands and revenues, and consolidate power within their territories. They actively enforced new religious settlements.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, secular_rulers, agenda_setter,
    institutional, generational, arbitrage, regional).

% Benefited from the redistribution of wealth and power away from the Catholic Church, gaining land, influence, and the ability to appoint local clergy. They supported rulers in breaking with Rome.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, protestant_nobility, beneficiary,
    powerful, biographical, mobile, local).

% Suffered immense loss of temporal power, land, and revenue in territories that embraced the Reformation. Its authority was directly challenged and often violently suppressed by secular powers.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, catholic_church, payer,
    institutional, civilizational, trapped, global).

% Lost its spiritual and temporal jurisdiction over vast regions of Europe, becoming a target of political maneuvering and military action by newly independent Protestant states. Its claims to universal authority were directly undermined.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, papal_authority, payer,
    institutional, civilizational, trapped, global).

% Were dispossessed of their benefices, often forced to convert or flee, and lost their traditional social and economic standing in Protestant territories. Their loyalty to Rome became a political liability.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, catholic_clergy, payer,
    moderate, biographical, constrained, local).

% Provided the intellectual and doctrinal framework for the theological disputes, but their ideas were often instrumentalized by secular powers for political ends. Their agency was secondary to the political drivers.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, theologians_and_reformers, observer,
    moderate, generational, constrained, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enabled secular rulers to coordinate their efforts to break free from the overarching political and economic authority of the Papacy, consolidating state power and national identities.
% TRANSFER_FUNCTION: Transferred vast wealth (church lands, tithes, assets) and political authority from the Catholic Church and Papacy to secular rulers and their supporting nobility.
% ABSENT_VOICES: Theological purists who genuinely sought only spiritual reform, not political gain, would object to the instrumentalization of their beliefs. They are absent from the core political narrative, their concerns subsumed by power struggles.
% DISAPPEARANCE_RATIONALE: If the political realignment aspect of the Reformation vanished, the modern nation-state system would be fundamentally different, with a much stronger, unified Papal authority in Europe. The distribution of power, wealth, and sovereignty would be unrecognizable.
% FOUNDING_PROBLEM: Secular rulers faced a persistent challenge to their sovereignty and financial resources from the powerful, transnational Catholic Church and its claims to universal authority.
% FOUNDING_PROBLEM_CORROBORATION: Historians focusing on political and economic history corroborate that the tension between secular and ecclesiastical power was a long-standing problem, and the Reformation provided a critical opportunity for its resolution in favor of secular states. This is attested by numerous historical analyses of state-building and the rise of national sovereignty, independent of religious apologetics.
narrative_ontology:disappearance_verdict(reformation_event_boundary__political_swap_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_event_boundary__political_swap_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_event_boundary__political_swap_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high because the transfer of wealth and power from the Church to secular rulers was substantial and asymmetric. Suppression is high due to the active military and legal enforcement by secular states against Catholic institutions and individuals. Theater ratio is significant because the theological arguments, while real, are seen as primarily serving to legitimize a political power grab, rather than being the sole or primary driver. The claimed type is 'tangled_rope' because there was a genuine coordination function (consolidating state power) intertwined with asymmetric extraction from the Church.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of secular rulers, this was a necessary and beneficial coordination to establish state sovereignty. From the perspective of the Catholic Church, it was pure extraction and usurpation. The engine's classification will reflect this divergence based on the declared roles and metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   Secular rulers and Protestant nobility are clear beneficiaries, gaining power and assets (low directionality). The Catholic Church, Papal authority, and Catholic clergy are clear targets, losing power and assets (high directionality). Theologians are observers, their ideas instrumentalized.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling the political dimension as purely theological. By highlighting the instrumentalization of theology, it shows how a genuine coordination problem (state sovereignty) was solved through an extractive mechanism, rather than being a purely spiritual movement. The 'mandate' of theological reform was co-opted for political ends.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_sincerity_vs_instrumentalization,
    'To what extent were the theological disputes genuinely held beliefs, versus being instrumentalized as a convenient justification for political and economic objectives?',
    'Detailed textual analysis of primary sources (sermons, treatises, correspondence) for internal consistency and coherence, cross-referenced with political outcomes and financial gains of the actors involved. Examination of cases where theological positions were maintained despite political cost.',
    'If theological sincerity is found to be high, the ''theater_ratio'' would decrease, and the ''extractiveness'' might be re-evaluated as a consequence of genuine belief rather than pure opportunism, potentially shifting the classification towards a more complex ''tangled_rope'' or even ''rope'' for some actors. If instrumentalization is dominant, the current high ''theater_ratio'' and ''extractiveness'' are reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_sincerity_vs_instrumentalization, empirical, 'Ambiguity regarding the true motivation behind theological positions during the Reformation.').

omega_variable(
    periodization_boundary_ambiguity,
    'Is the Peace of Westphalia (1648) the appropriate end-point for the ''political swap'' reading, or does the political realignment continue beyond this point, or stabilize earlier?',
    'Analysis of state-church relations and international treaties post-1648 to determine the persistence or evolution of the political settlement. Examination of regional variations in the timing of political consolidation.',
    'A later end-point would suggest the political swap was a longer, more drawn-out process, potentially affecting the trajectory of ''extractiveness'' and ''suppression''. An earlier end-point would suggest a more rapid consolidation of secular power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(periodization_boundary_ambiguity, conceptual, 'Uncertainty regarding the temporal boundaries of the political realignment event.').

omega_variable(
    kernel_reading_difference,
    'What are the specific structural elements that differentiate this ''political_swap_reading'' from the ''theological_climb_reading'' and ''composite_overdetermination_reading'' of the Reformation kernel?',
    'Comparative analysis of the declared beneficiaries, victims, primary causal drivers, and periodization schemes across all three readings. Identification of non-overlapping sets of core claims.',
    'Clarifies the distinct analytical contribution of each reading and prevents conflation. Reinforces the ε-invariance principle by demonstrating how different framings yield structurally distinct constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_difference, conceptual, 'Documents the structural differences between this reading and its siblings within the ''reformation_event_boundary'' kernel.').


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
narrative_ontology:measurement(refo_tr_t1620, reformation_event_boundary__political_swap_reading, theater_ratio, 1620, 0.62).
narrative_ontology:measurement(refo_tr_t1648, reformation_event_boundary__political_swap_reading, theater_ratio, 1648, 0.6).

% Extraction over time
narrative_ontology:measurement(refo_be_t1517, reformation_event_boundary__political_swap_reading, base_extractiveness, 1517, 0.6).
narrative_ontology:measurement(refo_be_t1540, reformation_event_boundary__political_swap_reading, base_extractiveness, 1540, 0.75).
narrative_ontology:measurement(refo_be_t1570, reformation_event_boundary__political_swap_reading, base_extractiveness, 1570, 0.82).
narrative_ontology:measurement(refo_be_t1600, reformation_event_boundary__political_swap_reading, base_extractiveness, 1600, 0.85).
narrative_ontology:measurement(refo_be_t1620, reformation_event_boundary__political_swap_reading, base_extractiveness, 1620, 0.83).
narrative_ontology:measurement(refo_be_t1648, reformation_event_boundary__political_swap_reading, base_extractiveness, 1648, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t1517, reformation_event_boundary__political_swap_reading, suppression_requirement, 1517, 0.5).
narrative_ontology:measurement(refo_su_t1540, reformation_event_boundary__political_swap_reading, suppression_requirement, 1540, 0.65).
narrative_ontology:measurement(refo_su_t1570, reformation_event_boundary__political_swap_reading, suppression_requirement, 1570, 0.75).
narrative_ontology:measurement(refo_su_t1600, reformation_event_boundary__political_swap_reading, suppression_requirement, 1600, 0.78).
narrative_ontology:measurement(refo_su_t1620, reformation_event_boundary__political_swap_reading, suppression_requirement, 1620, 0.77).
narrative_ontology:measurement(refo_su_t1648, reformation_event_boundary__political_swap_reading, suppression_requirement, 1648, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_event_boundary__political_swap_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(reformation_event_boundary__political_swap_reading, reformation_event_boundary__theological_climb_reading).
narrative_ontology:affects_constraint(reformation_event_boundary__political_swap_reading, reformation_event_boundary__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'reformation_event_boundary' kernel. This 'political_swap_reading' emphasizes the political and economic drivers, contrasting with the 'theological_climb_reading' (focus on doctrinal innovation) and the 'composite_overdetermination_reading' (holistic view). Each reading has a distinct ε and stakeholder structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
