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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   This constraint story instantiates the 'political_swap_reading' of the
 *   'reformation_event_boundary' kernel. From this perspective, the
 *   Reformation was fundamentally a political realignment where secular
 *   rulers exploited existing theological disputes to break papal authority,
 *   seize church assets, and consolidate state power. Theological arguments
 *   are viewed as post-hoc rationalizations that provided a convenient
 *   ideological cover for these material and political objectives. The
 *   periodization extends to the Peace of Westphalia (1648), which solidified
 *   the new political order of sovereign states.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_event_boundary__political_swap_reading, 0.85).
domain_priors:suppression_score(reformation_event_boundary__political_swap_reading, 0.78).
domain_priors:theater_ratio(reformation_event_boundary__political_swap_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_event_boundary__political_swap_reading, snare).
narrative_ontology:human_readable(reformation_event_boundary__political_swap_reading, "Reformation as Political Power Swap (Political Swap Reading)").
narrative_ontology:topic_domain(reformation_event_boundary__political_swap_reading, "historical_epistemology/religious_history/commitment_system_analysis").

domain_priors:requires_active_enforcement(reformation_event_boundary__political_swap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_event_boundary__political_swap_reading, '04a36591-297a-47a5-bafd-b7e2c688d959').
narrative_ontology:cs_kernel_codification('04a36591-297a-47a5-bafd-b7e2c688d959', fixed_text).
narrative_ontology:cs_authority_grounding('04a36591-297a-47a5-bafd-b7e2c688d959', extraction).
narrative_ontology:cs_interpretation_layer_present('04a36591-297a-47a5-bafd-b7e2c688d959').
narrative_ontology:cs_reading_relation('04a36591-297a-47a5-bafd-b7e2c688d959', reformation_event_boundary__theological_climb_reading, forecloses).
narrative_ontology:cs_reading_relation('04a36591-297a-47a5-bafd-b7e2c688d959', reformation_event_boundary__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('04a36591-297a-47a5-bafd-b7e2c688d959', foundational, political_power_primary_driver).
narrative_ontology:cs_axiom_status(political_power_primary_driver, holdable).
narrative_ontology:cs_axiom_grounding('04a36591-297a-47a5-bafd-b7e2c688d959', political_power_primary_driver, empirically_contingent).
narrative_ontology:cs_axiom('04a36591-297a-47a5-bafd-b7e2c688d959', foundational, theology_as_instrument).
narrative_ontology:cs_axiom_status(theology_as_instrument, holdable).
narrative_ontology:cs_axiom_grounding('04a36591-297a-47a5-bafd-b7e2c688d959', theology_as_instrument, instrumental).
narrative_ontology:cs_reference_frame('04a36591-297a-47a5-bafd-b7e2c688d959', papal_temporal_supremacy).
narrative_ontology:cs_drift_state('04a36591-297a-47a5-bafd-b7e2c688d959', reformation_era_to_westphalia, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('04a36591-297a-47a5-bafd-b7e2c688d959', '').
narrative_ontology:cs_kernel_id(reformation_event_boundary__political_swap_reading, reformation_event_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_event_boundary__political_swap_reading, secular_rulers).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__political_swap_reading, protestant_nobility).
narrative_ontology:constraint_victim(reformation_event_boundary__political_swap_reading, catholic_church).
narrative_ontology:constraint_victim(reformation_event_boundary__political_swap_reading, papacy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__political_swap_reading, theologians_and_reformers).
narrative_ontology:constraint_victim(reformation_event_boundary__political_swap_reading, common_populace).
narrative_ontology:constraint_victim(reformation_event_boundary__political_swap_reading, theologians_and_reformers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exploited theological disputes to break papal authority, seize church assets, and consolidate their own power. They actively enforced new religious settlements and suppressed dissent, benefiting directly from the transfer of wealth and authority.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, secular_rulers, agenda_setter,
    institutional, generational, arbitrage, national).

% Lost vast landholdings, wealth, and political influence in territories that embraced the Reformation. Its authority was directly challenged and often violently suppressed by secular powers, leaving it with few options for resistance beyond excommunication and military conflict.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, catholic_church, payer,
    institutional, generational, trapped, global).

% Suffered a severe erosion of its temporal and spiritual authority over large parts of Europe. Its ability to levy taxes, appoint clergy, and intervene in secular affairs was drastically curtailed, leading to a long-term decline in its political power.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, papacy, payer,
    institutional, generational, trapped, global).

% Gained significant wealth and influence by aligning with secular rulers and adopting Protestantism, often receiving confiscated church lands and offices. Their support was crucial for the political success of the Reformation in many regions.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, protestant_nobility, beneficiary,
    powerful, biographical, mobile, regional).

% Experienced religious upheaval, wars, and often forced conversions based on the decisions of their rulers (cuius regio, eius religio). They bore the costs of conflict and the imposition of new religious doctrines, with limited ability to choose their own faith or practices.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, common_populace, payer,
    powerless, immediate, constrained, local).

% Gained patronage and influence from secular rulers who needed theological justification for their actions. While their ideas were promoted, their autonomy was often limited by the political agendas of their patrons, making them beneficiaries of the new order but also constrained by it.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, theologians_and_reformers, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(reformation_event_boundary__political_swap_reading, theologians_and_reformers, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reformation_event_boundary__political_swap_reading, secular_rulers).
narrative_ontology:fixing_cost_class(reformation_event_boundary__political_swap_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enabled secular rulers to coordinate the consolidation of political power, establish state control over religious institutions, and legitimize the seizure of church assets, thereby reducing external (papal) interference in their domains.
% TRANSFER_FUNCTION: Transferred vast amounts of land, wealth, and political authority from the Catholic Church and the Papacy to secular rulers and their allied nobility across Europe.
% ABSENT_VOICES: Loyal Catholic populations in Protestant territories, dissenting theological voices not aligned with the secular power structures, and those who sought genuine religious reform without political opportunism. Their objections were suppressed or ignored in favor of the dominant political narrative.
% DISAPPEARANCE_RATIONALE: If this political realignment had not occurred, the trajectory of European state-building, the balance of power between secular and religious authorities, and the very concept of national sovereignty would have been fundamentally different. The modern nation-state system, as it emerged, was deeply shaped by this event.
% FOUNDING_PROBLEM: Secular rulers faced a persistent challenge to their authority from the Papacy, which claimed universal spiritual and temporal jurisdiction, levied taxes, and owned significant land within their territories, hindering state consolidation and economic development.
% FOUNDING_PROBLEM_CORROBORATION: Historians focusing on political economy, state-building theorists, and contemporary secular chroniclers widely corroborate that the problem of papal temporal interference was largely resolved by the end of the Reformation, with secular rulers achieving greater autonomy. The Papacy's own historical accounts implicitly acknowledge this shift in power.
narrative_ontology:disappearance_verdict(reformation_event_boundary__political_swap_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_event_boundary__political_swap_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_event_boundary__political_swap_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   Extraction is high (0.85) because the primary outcome was a massive transfer of wealth and power from the Church to secular rulers. Suppression is also high (0.78) due to the active enforcement of new religious settlements, suppression of Catholic loyalists, and the wars of religion. The theater ratio is significant (0.65) because, from this reading, the theological justifications served largely as a performative cover for underlying political and economic motives. The metrics reflect the increasing consolidation of secular power and the corresponding decline of papal influence over the period.
 *
 * PERSPECTIVAL GAP:
 *   This reading sharply diverges from the 'theological_climb_reading' by asserting the primacy of political over theological causality. While the 'composite_overdetermination_reading' acknowledges political factors, this reading emphasizes them as the *primary* and *exploitative* driver, with theology being secondary. The engine's classification will highlight the extractive nature of this 'swap' from the victims' seats, contrasting with any coordination claims made by beneficiaries.
 *
 * DIRECTIONALITY LOGIC:
 *   Secular rulers and Protestant nobility are clear beneficiaries (low d) as they gained wealth, land, and authority. The Catholic Church and the Papacy are the primary targets (high d) as they suffered immense losses. The common populace is a payer (moderate d) as they bore the costs of conflict and religious imposition. Theologians and reformers are beneficiaries in terms of influence but also constrained by their patrons' political agendas.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_causality_ambiguity,
    'To what extent did genuine theological conviction independently drive the Reformation, rather than merely serving as a post-hoc rationalization for political motives?',
    'Detailed historical analysis of primary sources (e.g., personal letters, sermons, theological treatises) to assess the sincerity and independent causal force of religious beliefs among key actors, independent of their political allegiances.',
    'If theological conviction is found to have significant independent causal force, the ''theater_ratio'' would decrease, and the ''claimed_type'' might shift towards a ''tangled_rope'' or ''rope'' (if coordination is also present) to acknowledge a more genuine coordination function alongside extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_causality_ambiguity, empirical, 'Ambiguity regarding the true causal weight of theological vs. political factors.').

omega_variable(
    periodization_boundary_ambiguity,
    'Is the Peace of Westphalia (1648) the appropriate endpoint for the ''political swap'' event, or do other periodizations (e.g., earlier treaties, later state-building processes) better capture the stabilization of the new political order?',
    'Comparative historical analysis of state formation and religious settlement across different European regions, assessing when the transfer of authority from papal to secular power was definitively established and recognized.',
    'A different periodization might alter the trajectory of ''extractiveness'' and ''suppression_requirement'' measurements, potentially shifting the overall classification if the ''event'' is seen as more prolonged or less decisive in its immediate political outcomes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(periodization_boundary_ambiguity, conceptual, 'Uncertainty regarding the temporal boundaries of the Reformation as a political event.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_event_boundary__political_swap_reading, 1517, 1648).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t1517, reformation_event_boundary__political_swap_reading, theater_ratio, 1517, 0.4).
narrative_ontology:measurement(refo_tr_t1540, reformation_event_boundary__political_swap_reading, theater_ratio, 1540, 0.5).
narrative_ontology:measurement(refo_tr_t1570, reformation_event_boundary__political_swap_reading, theater_ratio, 1570, 0.6).
narrative_ontology:measurement(refo_tr_t1600, reformation_event_boundary__political_swap_reading, theater_ratio, 1600, 0.63).
narrative_ontology:measurement(refo_tr_t1648, reformation_event_boundary__political_swap_reading, theater_ratio, 1648, 0.65).

% Extraction over time
narrative_ontology:measurement(refo_be_t1517, reformation_event_boundary__political_swap_reading, base_extractiveness, 1517, 0.65).
narrative_ontology:measurement(refo_be_t1540, reformation_event_boundary__political_swap_reading, base_extractiveness, 1540, 0.75).
narrative_ontology:measurement(refo_be_t1570, reformation_event_boundary__political_swap_reading, base_extractiveness, 1570, 0.8).
narrative_ontology:measurement(refo_be_t1600, reformation_event_boundary__political_swap_reading, base_extractiveness, 1600, 0.83).
narrative_ontology:measurement(refo_be_t1648, reformation_event_boundary__political_swap_reading, base_extractiveness, 1648, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t1517, reformation_event_boundary__political_swap_reading, suppression_requirement, 1517, 0.55).
narrative_ontology:measurement(refo_su_t1540, reformation_event_boundary__political_swap_reading, suppression_requirement, 1540, 0.65).
narrative_ontology:measurement(refo_su_t1570, reformation_event_boundary__political_swap_reading, suppression_requirement, 1570, 0.72).
narrative_ontology:measurement(refo_su_t1600, reformation_event_boundary__political_swap_reading, suppression_requirement, 1600, 0.75).
narrative_ontology:measurement(refo_su_t1648, reformation_event_boundary__political_swap_reading, suppression_requirement, 1648, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_event_boundary__political_swap_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
