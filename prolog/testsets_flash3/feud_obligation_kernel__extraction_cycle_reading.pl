% ============================================================================
% CONSTRAINT STORY: feud_obligation_kernel__extraction_cycle_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feud_obligation_kernel__extraction_cycle_reading, []).

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
 *   constraint_id: feud_obligation_kernel__extraction_cycle_reading
 *   human_readable: Blood-Feud Extraction Cycle
 *   domain: legal_anthropology/medieval_history/political_systems
 *
 * SUMMARY:
 *   This constraint represents the 'extraction cycle' reading of blood-feud
 *   obligations, where the system is viewed as a destructive force that
 *   depletes societal resources and inadvertently strengthens centralized
 *   power by creating a demand for its suppression. It is one reading of the
 *   'feud_obligation_kernel', distinct from 'stateless_coordination_reading'
 *   (which sees feuds as a form of justice) and
 *   'christianized_pacification_reading' (which condemns them on moral
 *   grounds).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feud_obligation_kernel__extraction_cycle_reading, 0.85).
domain_priors:suppression_score(feud_obligation_kernel__extraction_cycle_reading, 0.7).
domain_priors:theater_ratio(feud_obligation_kernel__extraction_cycle_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, resistance, 0.9).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feud_obligation_kernel__extraction_cycle_reading, snare).
narrative_ontology:human_readable(feud_obligation_kernel__extraction_cycle_reading, "Blood-Feud Extraction Cycle").
narrative_ontology:topic_domain(feud_obligation_kernel__extraction_cycle_reading, "legal_anthropology/medieval_history/political_systems").

domain_priors:requires_active_enforcement(feud_obligation_kernel__extraction_cycle_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feud_obligation_kernel__extraction_cycle_reading, '4eba658c-907c-4830-af35-b2f7f8a944cd').
narrative_ontology:cs_kernel_codification('4eba658c-907c-4830-af35-b2f7f8a944cd', implicit).
narrative_ontology:cs_authority_grounding('4eba658c-907c-4830-af35-b2f7f8a944cd', practice).
narrative_ontology:cs_interpretation_layer_present('4eba658c-907c-4830-af35-b2f7f8a944cd').
narrative_ontology:cs_reading_relation('4eba658c-907c-4830-af35-b2f7f8a944cd', feud_obligation_kernel__stateless_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('4eba658c-907c-4830-af35-b2f7f8a944cd', feud_obligation_kernel__christianized_pacification_reading, coexists_with).
narrative_ontology:cs_axiom('4eba658c-907c-4830-af35-b2f7f8a944cd', foundational, feud_depletes_productive_capacity).
narrative_ontology:cs_axiom_status(feud_depletes_productive_capacity, holdable).
narrative_ontology:cs_axiom_grounding('4eba658c-907c-4830-af35-b2f7f8a944cd', feud_depletes_productive_capacity, empirically_contingent).
narrative_ontology:cs_axiom('4eba658c-907c-4830-af35-b2f7f8a944cd', foundational, feud_prevents_territorial_consolidation).
narrative_ontology:cs_axiom_status(feud_prevents_territorial_consolidation, holdable).
narrative_ontology:cs_axiom_grounding('4eba658c-907c-4830-af35-b2f7f8a944cd', feud_prevents_territorial_consolidation, empirically_contingent).
narrative_ontology:cs_reference_frame('4eba658c-907c-4830-af35-b2f7f8a944cd', pre_state_formation_equilibrium).
narrative_ontology:cs_drift_state('4eba658c-907c-4830-af35-b2f7f8a944cd', early_state_consolidation_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('4eba658c-907c-4830-af35-b2f7f8a944cd', '').
narrative_ontology:cs_kernel_id(feud_obligation_kernel__extraction_cycle_reading, feud_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__extraction_cycle_reading, royal_authority).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__extraction_cycle_reading, feud_leaders).
narrative_ontology:constraint_victim(feud_obligation_kernel__extraction_cycle_reading, feud_participants).
narrative_ontology:constraint_victim(feud_obligation_kernel__extraction_cycle_reading, peasantry).
narrative_ontology:constraint_victim(feud_obligation_kernel__extraction_cycle_reading, merchants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bound by kinship and honor to participate in cycles of vengeance, leading to high mortality, injury, and resource depletion. Exit means dishonor, ostracization, or further victimization.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, feud_participants, payer,
    moderate, biographical, identity_locked, local).

% Suffer collateral damage from feuds: raids, destruction of crops, forced conscription, and general insecurity. Their productive capacity is directly depleted, and they have no effective means of opting out.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, peasantry, payer,
    powerless, immediate, trapped, local).

% Their trade routes are disrupted, goods are seized, and markets are destabilized by ongoing feuds. They bear economic costs and face physical danger, but their mobility offers some (constrained) options for rerouting or temporary withdrawal.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, merchants, payer,
    moderate, immediate, constrained, regional).

% Benefits from the social chaos and economic depletion caused by feuds, as it creates a demand for centralized justice and a monopoly on violence. This legitimizes the expansion of royal power, taxation, and the suppression of local kinship-based enforcement.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, royal_authority, beneficiary,
    institutional, generational, arbitrage, national).

% While participating in the cycle, they gain status, honor, and sometimes material wealth through successful raids or intimidation. They are beneficiaries of the system's internal logic, even as it depletes the wider society.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, feud_leaders, beneficiary,
    powerful, biographical, constrained, local).

% Condemn feuds as un-Christian and work to establish 'Peace of God' movements, offering alternative dispute resolution and moral authority. They observe the destructive cycle and seek to replace it with a divinely sanctioned order.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, ecclesiastical_institutions, observer,
    institutional, civilizational, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint, by this reading, does not solve a genuine coordination problem; it is a destructive cycle. Any 'coordination' is internal to the cycle of vengeance, not for societal benefit.
% TRANSFER_FUNCTION: Transfers lives, labor, and material wealth from feud participants, peasantry, and merchants to the destructive cycle itself, indirectly benefiting royal authority through legitimization of its monopoly on violence, and directly benefiting feud leaders through status and limited material gains.
% ABSENT_VOICES: The victims of the feud (peasantry, merchants) are largely absent from the discourse that perpetuates it. Their voices would highlight the economic and social costs, advocating for peace and centralized justice, but they lack the power to influence the system.
% DISAPPEARANCE_RATIONALE: If blood-feud obligations vanished, local economies would recover, populations would stabilize, and royal authority would face less immediate justification for its expansion, leading to a significant rearrangement of social and political structures.
% FOUNDING_PROBLEM: The absence of a centralized, legitimate authority to enforce justice and resolve disputes, leading to self-help mechanisms for redress.
% FOUNDING_PROBLEM_CORROBORATION: Royal chroniclers and ecclesiastical records from outside the immediate feud participants consistently describe the feuds as a destructive force, and their cessation as a sign of societal progress and the strengthening of central authority. The problem of statelessness is largely superseded by the emergence of nascent states.
narrative_ontology:disappearance_verdict(feud_obligation_kernel__extraction_cycle_reading, world_rearranges).
narrative_ontology:founding_problem_status(feud_obligation_kernel__extraction_cycle_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feud_obligation_kernel__extraction_cycle_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(feud_obligation_kernel__extraction_cycle_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feud_obligation_kernel__extraction_cycle_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feud_obligation_kernel__extraction_cycle_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(feud_obligation_kernel__extraction_cycle_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(feud_obligation_kernel__extraction_cycle_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) due to the direct costs in lives, property, and lost productivity. Suppression (0.7) is also high, as the social pressure to participate in feuds, driven by honor and kinship, is immense, effectively suppressing alternatives like peaceful resolution or state-based justice. Resistance is high (0.9) from the victims, but their powerlessness prevents effective change. The theater ratio is low (0.1) because the destructive function is quite real, not performative.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of feud participants and the general populace, the system is a snare, trapping them in a cycle of violence and depletion. From the perspective of royal authority, it is a problem to be 'solved' by expanding state power, thus legitimizing its own extractive mechanisms. Feud leaders, paradoxically, benefit from the status and power derived from their participation, even as the system harms the wider society.
 *
 * DIRECTIONALITY LOGIC:
 *   Feud participants, peasantry, and merchants are clear targets (high d) due to the direct costs they bear and their limited exit options. Royal authority and feud leaders are beneficiaries (low d), as the former gains legitimacy and the latter gains status from the ongoing conflict. The system is self-perpetuating through social norms and the absence of effective alternatives.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (to provide justice in a stateless society) has atrophied, as centralized authorities emerge. However, the cycle persists due to entrenched social norms and the benefits it provides to certain actors (feud leaders, royal authority). The classification as a Snare reflects this: the coordination story (justice) is cover for a destructive cycle that benefits specific parties and requires active suppression of alternatives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    feud_coordination_vs_extraction,
    'To what extent did blood-feud obligations genuinely provide a coordination function (e.g., deterrence, justice) versus primarily acting as a destructive extraction cycle?',
    'Comparative historical analysis of societies with and without feuds, examining long-term demographic and economic trends, and the presence/absence of alternative dispute resolution mechanisms.',
    'If a significant coordination function is established, the constraint might shift towards a Tangled Rope or even Rope classification, acknowledging a genuine (albeit costly) societal function. If extraction dominates, the Snare classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(feud_coordination_vs_extraction, empirical, 'Ambiguity between coordination and extraction in blood-feud systems.').

omega_variable(
    royal_authority_beneficiary_ambiguity,
    'Is royal authority a genuine beneficiary of the feud cycle, or merely an external actor seeking to impose order for broader societal benefit?',
    'Analysis of royal fiscal records and land acquisitions during periods of active feuding versus periods of peace, and the rhetoric used to justify royal intervention.',
    'If royal authority demonstrably profited from the chaos, its beneficiary status is confirmed. If its interventions were consistently costly and yielded little direct gain, its role might be reclassified as an ''observer'' or ''agenda_setter'' with a different directionality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(royal_authority_beneficiary_ambiguity, empirical, 'Whether royal authority''s role in feuds is extractive or purely pacifying.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (social pressure to participate in feuds) structural (lack of alternatives, external enforcement of honor codes) or internalized (identity fusion with kinship group, belief in the justice of vengeance)?',
    'Ethnographic studies of post-feud societies: if suppression persists after external barriers are removed, reclassify as partially internalized. Historical analysis of individual choices to opt out and their social consequences.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — individuals carry the suppression with them. This would deepen the Snare classification by highlighting the difficulty of exit even if external conditions change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in blood-feud obligations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feud_obligation_kernel__extraction_cycle_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t0, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(feud_tr_t20, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(feud_tr_t40, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(feud_tr_t60, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 60, 0.1).
narrative_ontology:measurement(feud_tr_t80, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 80, 0.1).
narrative_ontology:measurement(feud_tr_t100, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(feud_be_t0, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 0, 0.8).
narrative_ontology:measurement(feud_be_t20, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 20, 0.82).
narrative_ontology:measurement(feud_be_t40, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 40, 0.85).
narrative_ontology:measurement(feud_be_t60, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 60, 0.83).
narrative_ontology:measurement(feud_be_t80, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 80, 0.8).
narrative_ontology:measurement(feud_be_t100, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 100, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(feud_su_t0, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(feud_su_t20, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(feud_su_t40, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(feud_su_t60, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 60, 0.68).
narrative_ontology:measurement(feud_su_t80, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 80, 0.65).
narrative_ontology:measurement(feud_su_t100, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 100, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
