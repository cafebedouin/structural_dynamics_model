% ============================================================================
% CONSTRAINT STORY: article_9_war_renunciation__strict_pacifist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_9_war_renunciation__strict_pacifist_reading, []).

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
 *   constraint_id: article_9_war_renunciation__strict_pacifist_reading
 *   human_readable: Article 9 War Renunciation (Strict Pacifist Reading)
 *   domain: constitutional_law/security_policy/institutional_legitimacy
 *
 * SUMMARY:
 *   This constraint represents the strict pacifist reading of Article 9 of
 *   the Japanese Constitution, which categorically prohibits the maintenance
 *   of any armed forces and renounces war absolutely. This reading interprets
 *   the textual language 'never be maintained' as a complete ban on military
 *   capabilities, even for defensive purposes, forcing Japan to rely on
 *   non-military means or external alliances for security. It is a binary
 *   constraint where the existence of organized military forces is deemed
 *   unconstitutional, impacting Japan's state security autonomy.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_9_war_renunciation__strict_pacifist_reading, 0.9).
domain_priors:suppression_score(article_9_war_renunciation__strict_pacifist_reading, 0.95).
domain_priors:theater_ratio(article_9_war_renunciation__strict_pacifist_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, extractiveness, 0.9).
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_9_war_renunciation__strict_pacifist_reading, snare).
narrative_ontology:human_readable(article_9_war_renunciation__strict_pacifist_reading, "Article 9 War Renunciation (Strict Pacifist Reading)").
narrative_ontology:topic_domain(article_9_war_renunciation__strict_pacifist_reading, "constitutional_law/security_policy/institutional_legitimacy").

domain_priors:requires_active_enforcement(article_9_war_renunciation__strict_pacifist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_9_war_renunciation__strict_pacifist_reading, '2adb150c-ac4f-484b-8e54-e3a4d0f15fec').
narrative_ontology:cs_kernel_codification('2adb150c-ac4f-484b-8e54-e3a4d0f15fec', fixed_text).
narrative_ontology:cs_authority_grounding('2adb150c-ac4f-484b-8e54-e3a4d0f15fec', lineage).
narrative_ontology:cs_interpretation_layer_present('2adb150c-ac4f-484b-8e54-e3a4d0f15fec').
narrative_ontology:cs_reading_relation('2adb150c-ac4f-484b-8e54-e3a4d0f15fec', article_9_war_renunciation__inherent_right_reading, forecloses).
narrative_ontology:cs_reading_relation('2adb150c-ac4f-484b-8e54-e3a4d0f15fec', article_9_war_renunciation__collective_self_defense_reading, forecloses).
narrative_ontology:cs_axiom('2adb150c-ac4f-484b-8e54-e3a4d0f15fec', foundational, absolute_prohibition_on_military_force).
narrative_ontology:cs_axiom_status(absolute_prohibition_on_military_force, holdable).
narrative_ontology:cs_axiom_grounding('2adb150c-ac4f-484b-8e54-e3a4d0f15fec', absolute_prohibition_on_military_force, deontological).
narrative_ontology:cs_axiom('2adb150c-ac4f-484b-8e54-e3a4d0f15fec', foundational, war_renunciation_is_categorical).
narrative_ontology:cs_axiom_status(war_renunciation_is_categorical, holdable).
narrative_ontology:cs_axiom_grounding('2adb150c-ac4f-484b-8e54-e3a4d0f15fec', war_renunciation_is_categorical, deontological).
narrative_ontology:cs_reference_frame('2adb150c-ac4f-484b-8e54-e3a4d0f15fec', post_war_pacifist_constitution).
narrative_ontology:cs_drift_state('2adb150c-ac4f-484b-8e54-e3a4d0f15fec', contemporary_geopolitical_realities, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('2adb150c-ac4f-484b-8e54-e3a4d0f15fec', '').
narrative_ontology:cs_kernel_id(article_9_war_renunciation__strict_pacifist_reading, article_9_war_renunciation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__strict_pacifist_reading, pacifist_advocacy_groups).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__strict_pacifist_reading, anti_militarist_movements).
narrative_ontology:constraint_victim(article_9_war_renunciation__strict_pacifist_reading, japanese_government).
narrative_ontology:constraint_victim(article_9_war_renunciation__strict_pacifist_reading, japanese_self_defense_forces).
narrative_ontology:constraint_victim(article_9_war_renunciation__strict_pacifist_reading, national_security_planners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bound by the constitutional text, this reading forces the government to rely on non-military means or external alliances for security, limiting its sovereign defense options. It bears the political and strategic costs of this limitation.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, japanese_government, payer,
    institutional, generational, constrained, national).

% Under this reading, the very existence of the SDF is unconstitutional, creating an existential crisis for its members and mission. They are identity-locked by their professional commitment to national defense, which this reading denies.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, japanese_self_defense_forces, payer,
    organized, biographical, identity_locked, national).

% Must devise security strategies that explicitly exclude military options, leading to reliance on diplomacy, economic leverage, or foreign protection. This constrains their ability to respond to threats and maintain national autonomy.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, national_security_planners, payer,
    powerful, generational, constrained, national).

% Benefit from the constitutional backing for their anti-militarist stance, using this reading to challenge any expansion of military capabilities or roles. Their agenda is directly advanced by the constraint's strict interpretation.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, pacifist_advocacy_groups, beneficiary,
    organized, generational, mobile, national).

% Find strong ideological and legal support for their activism against rearmament or military involvement. This reading provides a clear, categorical basis for their opposition.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, anti_militarist_movements, beneficiary,
    moderate, biographical, mobile, local).

% Observe Japan's security posture and its implications for regional stability and alliance commitments. This reading places greater burden on them for Japan's defense, potentially influencing their own strategic planning.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, allied_nations, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates national identity around absolute pacifism and non-aggression, fostering a culture of peace and renouncing military solutions to international disputes.
% TRANSFER_FUNCTION: Transfers the burden of national defense from Japan's own military capacity to diplomatic efforts, economic power, and reliance on security alliances, effectively transferring military autonomy to other nations.
% ABSENT_VOICES: Those who believe in a robust, independent national defense capability are structurally marginalized by this reading, as their core premise is deemed unconstitutional. They would argue for a reinterpretation that allows for a conventional military.
% DISAPPEARANCE_RATIONALE: If this strict pacifist reading vanished, Japan's security policy would undergo a fundamental shift, likely leading to a re-evaluation of the SDF's role, potential rearmament, and a more independent foreign policy. Regional power dynamics would also be significantly altered.
% FOUNDING_PROBLEM: The problem of Japan's historical militarism and aggressive warfare leading to immense suffering and destruction in World War II.
% FOUNDING_PROBLEM_CORROBORATION: Pacifist groups and some historians attest that the threat of resurgent militarism remains live, requiring strict constitutional limits. Other observers, including national security experts and allied nations, acknowledge the historical context but argue the current global security environment presents different problems that require a more flexible interpretation.
narrative_ontology:disappearance_verdict(article_9_war_renunciation__strict_pacifist_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_9_war_renunciation__strict_pacifist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_9_war_renunciation__strict_pacifist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(article_9_war_renunciation__strict_pacifist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_9_war_renunciation__strict_pacifist_reading, 0.9, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_9_war_renunciation__strict_pacifist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_9_war_renunciation__strict_pacifist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_9_war_renunciation__strict_pacifist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is very high (0.9) because this reading imposes a severe limitation on a sovereign nation's ability to defend itself, forcing reliance on others or non-military means. Suppression is also very high (0.95) as this reading actively suppresses any attempt to develop or maintain conventional military forces, requiring constant vigilance and legal challenges against reinterpretation. Theater ratio is low (0.05) because the constraint is actively enforced and its implications are stark; there is little performative maintenance masking a degraded function. The constraint is claimed as a Snare because it extracts national security autonomy and imposes significant costs on the government and SDF, while benefiting specific ideological groups.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of pacifist advocacy groups, this reading is a foundational Rope, coordinating national identity around peace and preventing a return to militarism. From the perspective of the Japanese government and SDF, it is a Snare, extracting their ability to provide independent national defense and forcing them into a dependent security posture.
 *
 * DIRECTIONALITY LOGIC:
 *   The Japanese government, SDF, and national security planners are full targets (d near 1.0) as they bear the direct costs of restricted defense capabilities and existential challenges to their roles. Pacifist and anti-militarist groups are beneficiaries (d near 0.0) as this reading directly supports their ideological and political agendas. Allied nations are observers, affected by Japan's security posture but not directly subject to the constraint's extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling a deeply contested and highly extractive interpretation as mere coordination. The high extractiveness and suppression, coupled with the clear victim set, indicate that this reading functions as a Snare, not a Rope, despite its proponents' claims of coordinating peace. The founding problem of historical militarism is still 'live' for some, but the 'world_rearranges' verdict if the constraint vanished suggests its persistence is not solely due to the problem it solves, but also due to the active suppression of alternatives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_ambiguity,
    'Is this strict pacifist reading of Article 9 a genuine natural law (a moral imperative against any military force) or a constructed constraint that benefits identifiable agents (pacifist movements) by limiting state power?',
    'Analysis of the philosophical grounding of absolute pacifism versus the historical context of Article 9''s drafting and its subsequent political uses. If its persistence depends on active advocacy and suppression of alternatives, it leans towards constructed.',
    'If a genuine natural law, its extractiveness would be re-evaluated as an inherent cost of moral order. If constructed, its classification as a Snare is reinforced, highlighting the political nature of its enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_ambiguity, conceptual, 'Ambiguity between moral imperative and political construction.').

omega_variable(
    security_autonomy_cost_quantification,
    'What is the quantifiable cost to Japan''s national security autonomy imposed by this strict pacifist reading, in terms of strategic flexibility, deterrence capability, and reliance on external powers?',
    'Comparative analysis with other non-nuclear, militarily constrained nations, or counterfactual modeling of Japan''s security posture under a different Article 9 interpretation. Expert geopolitical and economic analysis.',
    'A higher quantifiable cost would strengthen the Snare classification by demonstrating the severity of extraction. A lower cost might suggest the constraint is less extractive than perceived, potentially shifting it towards a Tangled Rope if coordination benefits are also high.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(security_autonomy_cost_quantification, empirical, 'Quantifying the cost of restricted military autonomy.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of military development structural (legal barriers, constitutional interpretation) or internalized (a deep-seated cultural aversion to military force that persists after barrier removal)?',
    'Post-reinterpretation trajectory: if military development remains culturally resisted even after legal barriers are removed, reclassify as partially internalized. Public opinion surveys on military roles.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the population carries the suppression with them after legal changes. If purely structural, legal changes would rapidly alter the security landscape.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_9_war_renunciation__strict_pacifist_reading, 1947, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(arti_be_t1947, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 1947, 0.85).
narrative_ontology:measurement(arti_be_t1960, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 1960, 0.88).
narrative_ontology:measurement(arti_be_t1980, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 1980, 0.9).
narrative_ontology:measurement(arti_be_t2000, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 2000, 0.92).
narrative_ontology:measurement(arti_be_t2024, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 2024, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t1947, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 1947, 0.9).
narrative_ontology:measurement(arti_su_t1960, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 1960, 0.92).
narrative_ontology:measurement(arti_su_t1980, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 1980, 0.93).
narrative_ontology:measurement(arti_su_t2000, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 2000, 0.94).
narrative_ontology:measurement(arti_su_t2024, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 2024, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_9_war_renunciation__strict_pacifist_reading, identity_coordination).
narrative_ontology:affects_constraint(article_9_war_renunciation__strict_pacifist_reading, inherent_right_reading).
narrative_ontology:affects_constraint(article_9_war_renunciation__strict_pacifist_reading, collective_self_defense_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Article 9 war renunciation kernel. Its strict interpretation directly influences and is influenced by other readings, particularly those advocating for a more flexible defense posture.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
