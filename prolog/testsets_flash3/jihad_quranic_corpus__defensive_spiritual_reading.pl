% ============================================================================
% CONSTRAINT STORY: jihad_quranic_corpus__defensive_spiritual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jihad_quranic_corpus__defensive_spiritual_reading, []).

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
 *   constraint_id: jihad_quranic_corpus__defensive_spiritual_reading
 *   human_readable: Jihad as Internal Spiritual Struggle and Defensive War (Quranic/Spiritual Reading)
 *   domain: islamic_jurisprudence/comparative_religious_law/political_theology
 *
 * SUMMARY:
 *   This constraint represents a specific reading of 'jihad' within the
 *   Quranic corpus, emphasizing its primary meaning as internal spiritual
 *   struggle (jihad al-nafs) and, secondarily, as a defensive armed response
 *   to aggression, strictly governed by principles of proportionality and
 *   non-combatant immunity. This reading is widely held by mainstream Islamic
 *   scholars and jurists. It contrasts sharply with more expansive or
 *   revolutionary interpretations. The metrics reflect a low-extraction,
 *   low-suppression constraint that primarily coordinates ethical behavior
 *   and limits violence.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jihad_quranic_corpus__defensive_spiritual_reading, 0.15).
domain_priors:suppression_score(jihad_quranic_corpus__defensive_spiritual_reading, 0.2).
domain_priors:theater_ratio(jihad_quranic_corpus__defensive_spiritual_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jihad_quranic_corpus__defensive_spiritual_reading, rope).
narrative_ontology:human_readable(jihad_quranic_corpus__defensive_spiritual_reading, "Jihad as Internal Spiritual Struggle and Defensive War (Quranic/Spiritual Reading)").
narrative_ontology:topic_domain(jihad_quranic_corpus__defensive_spiritual_reading, "islamic_jurisprudence/comparative_religious_law/political_theology").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jihad_quranic_corpus__defensive_spiritual_reading, '56baf82f-352c-4453-b574-69c5ecc837fd').
narrative_ontology:cs_kernel_codification('56baf82f-352c-4453-b574-69c5ecc837fd', fixed_text).
narrative_ontology:cs_authority_grounding('56baf82f-352c-4453-b574-69c5ecc837fd', lineage).
narrative_ontology:cs_interpretation_layer_present('56baf82f-352c-4453-b574-69c5ecc837fd').
narrative_ontology:cs_reading_relation('56baf82f-352c-4453-b574-69c5ecc837fd', jihad_quranic_corpus__expansionist_legalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('56baf82f-352c-4453-b574-69c5ecc837fd', jihad_quranic_corpus__revolutionary_vanguard_reading, forecloses).
narrative_ontology:cs_axiom('56baf82f-352c-4453-b574-69c5ecc837fd', foundational, jihad_primarily_spiritual_struggle).
narrative_ontology:cs_axiom_status(jihad_primarily_spiritual_struggle, holdable).
narrative_ontology:cs_axiom_grounding('56baf82f-352c-4453-b574-69c5ecc837fd', jihad_primarily_spiritual_struggle, deontological).
narrative_ontology:cs_axiom('56baf82f-352c-4453-b574-69c5ecc837fd', foundational, armed_jihad_strictly_defensive).
narrative_ontology:cs_axiom_status(armed_jihad_strictly_defensive, holdable).
narrative_ontology:cs_axiom_grounding('56baf82f-352c-4453-b574-69c5ecc837fd', armed_jihad_strictly_defensive, conventional).
narrative_ontology:cs_reference_frame('56baf82f-352c-4453-b574-69c5ecc837fd', quranic_prophetic_precedent).
narrative_ontology:cs_drift_state('56baf82f-352c-4453-b574-69c5ecc837fd', contemporary_global_conflicts, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('56baf82f-352c-4453-b574-69c5ecc837fd', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(jihad_quranic_corpus__defensive_spiritual_reading, jihad_quranic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__defensive_spiritual_reading, muslim_community_members).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__defensive_spiritual_reading, non_combatants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(jihad_quranic_corpus__defensive_spiritual_reading, aggressive_external_forces).
narrative_ontology:constraint_vindicates(jihad_quranic_corpus__defensive_spiritual_reading, islamic_ethics_of_war).
narrative_ontology:constraint_vindicates(jihad_quranic_corpus__defensive_spiritual_reading, spiritual_self_purification).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from a framework that prioritizes spiritual growth and limits armed conflict to legitimate defense, fostering internal peace and external security. Identity is deeply tied to adherence to this interpretation.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, muslim_community_members, beneficiary,
    organized, generational, identity_locked, global).

% Protected by strict rules of proportionality and immunity, ensuring their safety during any legitimate defensive armed response. Their well-being is a core ethical concern of this reading.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, non_combatants, beneficiary,
    powerless, immediate, trapped, universal).

% Interpret and transmit the Quranic corpus, emphasizing spiritual struggle and defensive warfare. They shape the discourse and provide authoritative rulings, but are constrained by the textual tradition and peer consensus.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, islamic_scholars_and_jurists, agenda_setter,
    institutional, generational, constrained, global).

% Are the only legitimate actors to declare and conduct armed jihad under this reading, ensuring it is a state-controlled defensive action, not individual vigilantism. Their authority is both enabled and constrained by this interpretation.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, state_authorities_in_muslim_majority_countries, agenda_setter,
    institutional, biographical, constrained, national).

% Are the legitimate targets of defensive armed response, bearing the costs of military engagement when they initiate aggression against Muslim lands or peoples. This reading constrains the scope of such response.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, aggressive_external_forces, payer,
    powerful, immediate, mobile, regional).

% Are excluded from legitimate authority to declare or conduct armed jihad, as their actions violate the principles of state authority, proportionality, and non-combatant immunity central to this reading. They would reject this reading's constraints.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, radical_militant_groups, excluded,
    organized, immediate, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the Muslim community's understanding of 'jihad' by prioritizing internal spiritual struggle and limiting external armed conflict to legitimate, proportionate, and state-sanctioned defense, thereby preventing unauthorized violence and promoting ethical conduct.
% TRANSFER_FUNCTION: Transfers the burden of armed conflict from individuals and non-combatants to legitimate state authorities in defensive contexts, and transfers focus from external conquest to internal moral purification for individuals.
% ABSENT_VOICES: Radical militant groups and proponents of offensive jihad interpretations are structurally excluded; they would argue for a broader, more aggressive application of armed jihad, bypassing state authority and potentially disregarding non-combatant immunity.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the interpretation of 'jihad' would likely default to more expansive or individualistic understandings, potentially leading to increased unauthorized violence, disregard for non-combatant immunity, and a breakdown of state control over armed conflict within Muslim communities globally.
% FOUNDING_PROBLEM: The early Muslim community faced both internal moral challenges (hypocrisy, spiritual laxity) and external aggression, requiring a framework to guide both individual conduct and collective defense.
% FOUNDING_PROBLEM_CORROBORATION: Islamic scholars and ethicists widely attest that both internal moral struggle and the need for defensive principles in conflict remain live problems. International humanitarian law bodies also implicitly corroborate the need for such constraints on armed conflict, aligning with this reading's ethical principles.
narrative_ontology:disappearance_verdict(jihad_quranic_corpus__defensive_spiritual_reading, world_rearranges).
narrative_ontology:founding_problem_status(jihad_quranic_corpus__defensive_spiritual_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jihad_quranic_corpus__defensive_spiritual_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(jihad_quranic_corpus__defensive_spiritual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jihad_quranic_corpus__defensive_spiritual_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jihad_quranic_corpus__defensive_spiritual_reading_tests).
:- end_tests(jihad_quranic_corpus__defensive_spiritual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because this reading primarily imposes ethical and spiritual obligations on individuals (internal struggle) and strict limits on collective action (defensive war), rather than extracting resources or imposing burdens without corresponding benefits. Suppression is low (0.20) as adherence is largely voluntary, driven by religious conviction and scholarly consensus, rather than active coercion. The constraint's persistence relies on its perceived ethical coherence and textual grounding. Theater ratio is minimal (0.05) as the principles are genuinely applied and not merely performative.
 *
 * PERSPECTIVAL GAP:
 *   While mainstream scholars and community members largely align with this reading, radical groups would experience it as a highly suppressive constraint, as it directly negates their interpretation and legitimizes state action against them. However, from the perspective of this reading, such groups are not legitimate stakeholders but rather external threats to the ethical framework.
 *
 * DIRECTIONALITY LOGIC:
 *   Muslim community members and non-combatants are beneficiaries, as the reading protects them and guides their spiritual development. Islamic scholars and state authorities act as agenda-setters, interpreting and enforcing these principles. Aggressive external forces are 'payers' in the sense that they bear the costs of a legitimate defensive response. Radical militant groups are excluded, as their actions fall outside the bounds of this interpretation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretation_authority_ambiguity,
    'Who holds ultimate authority to interpret the Quranic corpus regarding jihad, and how is that authority established and maintained?',
    'Empirical study of jurisprudential history and contemporary fatwa issuance, tracing lines of authority and consensus formation within different schools of thought.',
    'If authority is highly centralized and coercive, the constraint''s suppression might be higher than measured. If authority is diffuse and relies on voluntary adherence, the current low suppression is accurate. This impacts the ''agenda_setter'' role''s true power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretation_authority_ambiguity, empirical, 'Ambiguity in the locus and nature of interpretive authority for jihad.').

omega_variable(
    defensive_threshold_ambiguity,
    'What constitutes ''aggression'' that legitimately triggers a defensive armed response, and who determines this threshold?',
    'Comparative analysis of historical and contemporary jurisprudential rulings on declarations of war, examining the criteria applied and the actors involved in such decisions.',
    'A low or easily manipulated threshold for ''aggression'' could allow this reading to be co-opted for offensive purposes, increasing its effective extractiveness and suppression on non-Muslims. A high, rigorously applied threshold reinforces its defensive nature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(defensive_threshold_ambiguity, conceptual, 'Ambiguity in defining the threshold for legitimate defensive armed response.').

omega_variable(
    internalized_suppression_of_alternative_readings,
    'To what extent is the low ''resistance'' to this reading a result of internalized suppression of alternative interpretations within mainstream Muslim communities?',
    'Sociological studies of religious education and community discourse, examining how dissenting views on jihad are treated and whether individuals feel free to express them without social or professional penalty.',
    'If internalized suppression is significant, the effective suppression of this constraint is higher than the structural measure suggests, as it actively marginalizes or silences alternative readings, even without overt coercion.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(internalized_suppression_of_alternative_readings, empirical, 'Structural vs. internalized suppression of alternative jihad interpretations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jihad_quranic_corpus__defensive_spiritual_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jiha_tr_t0, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(jiha_tr_t10, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 10, 0.05).
narrative_ontology:measurement(jiha_tr_t20, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 20, 0.05).
narrative_ontology:measurement(jiha_tr_t30, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 30, 0.05).
narrative_ontology:measurement(jiha_tr_t40, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 40, 0.05).
narrative_ontology:measurement(jiha_tr_t50, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 50, 0.05).

% Extraction over time
narrative_ontology:measurement(jiha_be_t0, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(jiha_be_t10, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 10, 0.15).
narrative_ontology:measurement(jiha_be_t20, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 20, 0.15).
narrative_ontology:measurement(jiha_be_t30, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 30, 0.15).
narrative_ontology:measurement(jiha_be_t40, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 40, 0.15).
narrative_ontology:measurement(jiha_be_t50, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 50, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(jiha_su_t0, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(jiha_su_t10, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 10, 0.2).
narrative_ontology:measurement(jiha_su_t20, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 20, 0.2).
narrative_ontology:measurement(jiha_su_t30, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 30, 0.2).
narrative_ontology:measurement(jiha_su_t40, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 40, 0.2).
narrative_ontology:measurement(jiha_su_t50, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 50, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jihad_quranic_corpus__defensive_spiritual_reading, identity_coordination).
narrative_ontology:affects_constraint(jihad_quranic_corpus__defensive_spiritual_reading, islamic_ethics_of_war).
narrative_ontology:affects_constraint(jihad_quranic_corpus__defensive_spiritual_reading, islamic_criminal_justice_principles).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
