% ============================================================================
% CONSTRAINT STORY: july_charter_sovereign_legitimacy__secular_democratic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_july_charter_sovereign_legitimacy__secular_democratic_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: july_charter_sovereign_legitimacy__secular_democratic_reading
 *   human_readable: July Charter: Secular Democratic Mandate
 *   domain: constitutional_law/political_transitions/post_revolutionary_state_building
 *
 * SUMMARY:
 *   This constraint represents the 'secular democratic' reading of a
 *   post-revolutionary charter, which mandates civilian control over the
 *   military and establishes secular democratic institutions. It is a
 *   foundational legal document intended to guide state-building. While
 *   claimed as a 'scaffold' for a transitional period, its operation involves
 *   significant exclusion and suppression of alternative political visions,
 *   leading to high measured extractiveness and suppression. This story
 *   focuses on how this specific interpretation of the charter functions in
 *   practice, particularly its impact on political Islam actors and military
 *   autonomy.
 *
 * KEY AGENTS:
 *   - civilian_government: Agenda setter (institutional/constrained)
 *   - secular_democratic_political_parties: Beneficiary (organized/constrained)
 *   - political_islam_actors: Payer/Excluded (powerless/trapped)
 *   - military_high_command: Payer (institutional/constrained)
 *   - general_populace: Beneficiary/Payer (moderate/constrained)
 *   - international_observers: Observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.7).
domain_priors:suppression_score(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.8).
domain_priors:theater_ratio(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(july_charter_sovereign_legitimacy__secular_democratic_reading, scaffold).
narrative_ontology:human_readable(july_charter_sovereign_legitimacy__secular_democratic_reading, "July Charter: Secular Democratic Mandate").
narrative_ontology:topic_domain(july_charter_sovereign_legitimacy__secular_democratic_reading, "constitutional_law/political_transitions/post_revolutionary_state_building").

domain_priors:requires_active_enforcement(july_charter_sovereign_legitimacy__secular_democratic_reading).
narrative_ontology:has_sunset_clause(july_charter_sovereign_legitimacy__secular_democratic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(july_charter_sovereign_legitimacy__secular_democratic_reading, 'e1eba4ce-a9cd-4b0b-9df2-a8e4060383c4').
narrative_ontology:cs_kernel_codification('e1eba4ce-a9cd-4b0b-9df2-a8e4060383c4', fixed_text).
narrative_ontology:cs_authority_grounding('e1eba4ce-a9cd-4b0b-9df2-a8e4060383c4', lineage).
narrative_ontology:cs_interpretation_layer_present('e1eba4ce-a9cd-4b0b-9df2-a8e4060383c4').
narrative_ontology:cs_reading_relation('e1eba4ce-a9cd-4b0b-9df2-a8e4060383c4', july_charter_sovereign_legitimacy__guided_nationalism_reading, forecloses).
narrative_ontology:cs_reading_relation('e1eba4ce-a9cd-4b0b-9df2-a8e4060383c4', july_charter_sovereign_legitimacy__military_custodian_reading, forecloses).
narrative_ontology:cs_axiom('e1eba4ce-a9cd-4b0b-9df2-a8e4060383c4', foundational, secular_state_principle).
narrative_ontology:cs_axiom_status(secular_state_principle, holdable).
narrative_ontology:cs_axiom_grounding('e1eba4ce-a9cd-4b0b-9df2-a8e4060383c4', secular_state_principle, conventional).
narrative_ontology:cs_axiom('e1eba4ce-a9cd-4b0b-9df2-a8e4060383c4', foundational, civilian_control_of_military).
narrative_ontology:cs_axiom_status(civilian_control_of_military, holdable).
narrative_ontology:cs_axiom_grounding('e1eba4ce-a9cd-4b0b-9df2-a8e4060383c4', civilian_control_of_military, conventional).
narrative_ontology:cs_reference_frame('e1eba4ce-a9cd-4b0b-9df2-a8e4060383c4', post_revolutionary_democratic_order).
narrative_ontology:cs_drift_state('e1eba4ce-a9cd-4b0b-9df2-a8e4060383c4', contemporary_political_struggle, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('e1eba4ce-a9cd-4b0b-9df2-a8e4060383c4', '').
narrative_ontology:cs_kernel_id(july_charter_sovereign_legitimacy__secular_democratic_reading, july_charter_sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__secular_democratic_reading, civilian_government).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__secular_democratic_reading, secular_democratic_political_parties).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__secular_democratic_reading, political_islam_actors).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__secular_democratic_reading, military_autonomous_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__secular_democratic_reading, general_populace).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__secular_democratic_reading, military_high_command).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__secular_democratic_reading, general_populace).
narrative_ontology:constraint_vindicates(july_charter_sovereign_legitimacy__secular_democratic_reading, civilian_supremacy_doctrine).
narrative_ontology:constraint_vindicates(july_charter_sovereign_legitimacy__secular_democratic_reading, secular_state_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The elected or appointed civilian authority tasked with implementing the charter's provisions, including the secular and democratic framework and military subordination. They benefit from the legitimacy the charter confers but are constrained by ongoing contestation.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, civilian_government, agenda_setter,
    institutional, biographical, constrained, national).

% Political groups that align with the charter's secular and democratic ideals. They gain a foundational legal framework for their political participation and agenda, but their power is contingent on the charter's successful implementation against rival interpretations.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, secular_democratic_political_parties, beneficiary,
    organized, generational, constrained, national).

% Groups advocating for an Islamic-nationalist framework, such as Jamaat-e-Islami. They are structurally excluded or severely constrained by the charter's secular mandate, bearing the cost of political marginalization and suppression of their ideology within the formal system.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, political_islam_actors, payer,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(july_charter_sovereign_legitimacy__secular_democratic_reading, political_islam_actors, excluded).

% The leadership of the armed forces, whose traditional autonomous authority is explicitly subordinated to civilian control by the charter. They bear the cost of reduced institutional independence and political influence, though they may still exert informal power.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, military_high_command, payer,
    institutional, generational, constrained, national).

% The citizens who are meant to benefit from a stable, democratic, and secular state. They may experience the benefits of civil liberties and political participation, but also bear the costs of political instability and potential repression resulting from the charter's contested implementation.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, general_populace, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(july_charter_sovereign_legitimacy__secular_democratic_reading, general_populace, payer).

% External actors (e.g., NGOs, foreign governments) monitoring the implementation of the charter, particularly regarding human rights, democratic transitions, and civilian control of the military. They provide analysis and exert diplomatic pressure but do not directly participate in the domestic political system.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, international_observers, observer,
    analytical, immediate, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(july_charter_sovereign_legitimacy__secular_democratic_reading, civilian_government).
narrative_ontology:fixing_cost_class(july_charter_sovereign_legitimacy__secular_democratic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a new, unified framework for national governance post-revolution, coordinating the roles of civilian institutions and the military, and defining the state's secular and democratic character.
% TRANSFER_FUNCTION: Transfers sovereign authority from potentially competing religious or military claims to a civilian, secular, and democratic framework. It transfers political legitimacy and control over state institutions to the civilian government and secular parties, while extracting autonomy and influence from political Islam actors and the military.
% ABSENT_VOICES: Political Islam actors, whose ideologies are explicitly excluded by the charter's secular mandate, are largely absent from the formal deliberative processes. They would argue for a state grounded in religious identity and law.
% DISAPPEARANCE_RATIONALE: If the charter vanished overnight, the foundational legal and political order would collapse. The civilian government's legitimacy would be undermined, the military's subordination would be contested, and political Islam actors would likely reassert their claims, leading to significant political upheaval and a re-ordering of power dynamics.
% FOUNDING_PROBLEM: The post-revolutionary state faced a crisis of legitimacy, competing claims to sovereign authority (religious, military, democratic), and a lack of clear institutional roles, threatening national stability and democratic transition.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of this reading (civilian government, secular parties) argue the problem is still live, citing ongoing threats to democracy and secularism. Opponents (political Islam actors, military factions) argue the charter itself created new problems or that the original problems were misdiagnosed; international observers often corroborate the persistence of foundational political instability, supporting the 'contested' status.
narrative_ontology:disappearance_verdict(july_charter_sovereign_legitimacy__secular_democratic_reading, world_rearranges).
narrative_ontology:founding_problem_status(july_charter_sovereign_legitimacy__secular_democratic_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(july_charter_sovereign_legitimacy__secular_democratic_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(july_charter_sovereign_legitimacy__secular_democratic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(july_charter_sovereign_legitimacy__secular_democratic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__secular_democratic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(july_charter_sovereign_legitimacy__secular_democratic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.7) reflects the cost borne by excluded political actors and the military, whose traditional roles are curtailed. Suppression (0.8) is very high due to active enforcement mechanisms (legal bans, political marginalization) against alternative ideologies and military insubordination. The theater ratio (0.4) indicates that while genuine democratic processes exist, a significant portion of the state's activity is performative, designed to maintain the secular democratic facade while managing underlying contestation. Accessibility collapse (0.7) is high because the charter significantly limits viable political alternatives. Resistance (0.75) is also high, reflecting ongoing challenges from both political Islam actors and elements within the military.
 *
 * PERSPECTIVAL GAP:
 *   The civilian government and secular parties perceive the charter as a legitimate 'scaffold' for building a modern state, providing essential coordination. However, political Islam actors and the military high command experience it as a 'snare' or 'tangled rope' that extracts their power and suppresses their influence. The engine's computation of per-seat classifications will highlight this divergence, showing how the same legal text is experienced as fundamentally different constraints by different parties.
 *
 * DIRECTIONALITY LOGIC:
 *   The civilian government and secular democratic parties are beneficiaries, gaining authority and a framework for their political vision (low directionality). Political Islam actors and the military high command are targets, losing autonomy and facing exclusion (high directionality). The general populace experiences mixed effects, benefiting from stability and rights but also bearing costs of conflict, placing them closer to symmetric directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The 'scaffold' claim implies a temporary, transitional nature. However, the high and increasing extractiveness and suppression suggest that the constraint may be drifting towards a 'tangled rope' or 'snare' if the transitional period extends indefinitely and the exclusionary aspects harden. The 'contested' status of the founding problem further indicates a risk of mandatrophy, where the original coordination function is overshadowed by rent-seeking or power consolidation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    secularism_vs_religious_identity_ambiguity,
    'Is the charter''s secular mandate a neutral framework for governance, or an active suppression of religious identity in the public sphere?',
    'Analysis of state policies on religious expression, education, and personal law; comparison with other secular states'' approaches to religious pluralism.',
    'If it''s an active suppression, the extractiveness and suppression metrics for political Islam actors are fully justified as structural; if it''s a neutral framework, the suppression might be partially internalized or a consequence of political competition rather than direct extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secularism_vs_religious_identity_ambiguity, conceptual, 'Contestation over the nature and intent of the charter''s secular mandate.').

omega_variable(
    civilian_military_power_balance,
    'To what extent has the military genuinely accepted subordination to civilian authority, versus maintaining informal power or the capacity for intervention?',
    'Empirical observation of military budget control, appointments, judicial oversight, and non-interference in political crises over time.',
    'If military subordination is largely performative, the ''civilian_government''s'' directionality shifts upward (more target-like), and the ''military_high_command''s'' directionality shifts downward (more beneficiary-like), indicating a ''tangled rope'' where the military benefits from a facade of subordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(civilian_military_power_balance, empirical, 'The true balance of power between civilian government and the military.').

omega_variable(
    suppression_mechanism_ambiguity_political_islam,
    'Is the suppression of political Islam actors primarily structural (legal bans, institutional barriers) or internalized (self-censorship, belief in the futility of resistance)?',
    'Post-ban political activity analysis: if political Islam actors remain marginalized even after formal bans are lifted, internalized suppression is more significant. If they rapidly re-mobilize, structural suppression was dominant.',
    'If internalized, the constraint''s effective suppression on these actors is higher than the structural measure suggests, as they carry the suppression with them. If structural, removing legal barriers would significantly alter their exit options and the constraint''s extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity_political_islam, empirical, 'Structural vs. internalized suppression mechanism for political Islam actors.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(july_charter_sovereign_legitimacy__secular_democratic_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(july_tr_t0, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(july_tr_t4, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 4, 0.25).
narrative_ontology:measurement(july_tr_t8, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(july_tr_t12, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 12, 0.35).
narrative_ontology:measurement(july_tr_t16, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 16, 0.38).
narrative_ontology:measurement(july_tr_t20, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(july_be_t0, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(july_be_t4, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 4, 0.58).
narrative_ontology:measurement(july_be_t8, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 8, 0.63).
narrative_ontology:measurement(july_be_t12, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 12, 0.67).
narrative_ontology:measurement(july_be_t16, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 16, 0.69).
narrative_ontology:measurement(july_be_t20, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 20, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(july_su_t0, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(july_su_t4, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 4, 0.68).
narrative_ontology:measurement(july_su_t8, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 8, 0.74).
narrative_ontology:measurement(july_su_t12, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 12, 0.78).
narrative_ontology:measurement(july_su_t16, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 16, 0.79).
narrative_ontology:measurement(july_su_t20, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 20, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(july_charter_sovereign_legitimacy__secular_democratic_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__secular_democratic_reading, july_charter_sovereign_legitimacy__guided_nationalism_reading).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__secular_democratic_reading, july_charter_sovereign_legitimacy__military_custodian_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'July Charter Sovereign Legitimacy' kernel. Each reading represents a distinct structural claim about the charter's function and impact, with differing ε values and stakeholder positions. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
