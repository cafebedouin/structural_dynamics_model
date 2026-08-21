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
 *   human_readable: Quranic-Corpus Reading of Jihad as Defensive and Spiritual
 *   domain: islamic_jurisprudence/political_theology
 *
 * SUMMARY:
 *   This constraint represents a specific reading of 'Jihad' within Islamic
 *   jurisprudence, emphasizing its primary meaning as an internal spiritual
 *   struggle (jihad al-nafs) and, secondarily, as a defensive armed response
 *   to aggression. Crucially, this reading strictly constrains armed jihad by
 *   principles of proportionality, non-combatant immunity, and the
 *   requirement for legitimate state authority. It is one reading of the
 *   'jihad_quranic_corpus' kernel, contrasting with more expansionist or
 *   revolutionary interpretations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jihad_quranic_corpus__defensive_spiritual_reading, 0.18).
domain_priors:suppression_score(jihad_quranic_corpus__defensive_spiritual_reading, 0.15).
domain_priors:theater_ratio(jihad_quranic_corpus__defensive_spiritual_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jihad_quranic_corpus__defensive_spiritual_reading, rope).
narrative_ontology:human_readable(jihad_quranic_corpus__defensive_spiritual_reading, "Quranic-Corpus Reading of Jihad as Defensive and Spiritual").
narrative_ontology:topic_domain(jihad_quranic_corpus__defensive_spiritual_reading, "islamic_jurisprudence/political_theology").

domain_priors:requires_active_enforcement(jihad_quranic_corpus__defensive_spiritual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jihad_quranic_corpus__defensive_spiritual_reading, '2f7efdd7-5fad-4e06-aae5-a4488b1e489c').
narrative_ontology:cs_kernel_codification('2f7efdd7-5fad-4e06-aae5-a4488b1e489c', fixed_text).
narrative_ontology:cs_authority_grounding('2f7efdd7-5fad-4e06-aae5-a4488b1e489c', lineage).
narrative_ontology:cs_interpretation_layer_present('2f7efdd7-5fad-4e06-aae5-a4488b1e489c').
narrative_ontology:cs_reading_relation('2f7efdd7-5fad-4e06-aae5-a4488b1e489c', jihad_quranic_corpus__expansionist_legalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('2f7efdd7-5fad-4e06-aae5-a4488b1e489c', jihad_quranic_corpus__revolutionary_vanguard_reading, forecloses).
narrative_ontology:cs_axiom('2f7efdd7-5fad-4e06-aae5-a4488b1e489c', foundational, jihad_primarily_spiritual_struggle).
narrative_ontology:cs_axiom_status(jihad_primarily_spiritual_struggle, holdable).
narrative_ontology:cs_axiom_grounding('2f7efdd7-5fad-4e06-aae5-a4488b1e489c', jihad_primarily_spiritual_struggle, deontological).
narrative_ontology:cs_axiom('2f7efdd7-5fad-4e06-aae5-a4488b1e489c', foundational, armed_jihad_strictly_defensive).
narrative_ontology:cs_axiom_status(armed_jihad_strictly_defensive, holdable).
narrative_ontology:cs_axiom_grounding('2f7efdd7-5fad-4e06-aae5-a4488b1e489c', armed_jihad_strictly_defensive, conventional).
narrative_ontology:cs_reference_frame('2f7efdd7-5fad-4e06-aae5-a4488b1e489c', early_islamic_ethical_framework).
narrative_ontology:cs_drift_state('2f7efdd7-5fad-4e06-aae5-a4488b1e489c', contemporary_interpretive_landscape, gap(stable, minor, true)).
narrative_ontology:cs_created_at('2f7efdd7-5fad-4e06-aae5-a4488b1e489c', '').
narrative_ontology:cs_kernel_id(jihad_quranic_corpus__defensive_spiritual_reading, jihad_quranic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__defensive_spiritual_reading, muslim_community).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__defensive_spiritual_reading, individual_believers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(jihad_quranic_corpus__defensive_spiritual_reading, aggressor_forces).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Engage in personal spiritual struggle (jihad al-nafs) for self-improvement and moral purification. They benefit from the ethical framework and community cohesion this interpretation provides, finding meaning and guidance.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, individual_believers, beneficiary,
    moderate, biographical, identity_locked, universal).

% Benefits from the collective defense against aggression and the internal moral strength derived from spiritual struggle. This reading coordinates their collective security and ethical conduct.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, muslim_community, beneficiary,
    organized, generational, constrained, global).

% Interpret and transmit the Quran and Sunnah texts, defining the parameters of jihad. They emphasize its spiritual and defensive nature, and the strict constraints of proportionality and non-combatant immunity, shaping the understanding of the constraint.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, islamic_scholars_jurists, agenda_setter,
    institutional, civilizational, identity_locked, global).

% The only legitimate authority to declare and conduct armed defensive jihad, ensuring adherence to jurisprudential constraints and protecting the community. They are responsible for upholding the ethical limits of warfare.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, legitimate_state_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Explicitly protected by the constraint's rules of non-combatant immunity; they are not targets of this form of jihad. Their safety is a core ethical tenet of this reading.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, non_muslim_civilians, excluded,
    powerless, immediate, mobile, global).

% Bear the costs of defensive armed response when they initiate unprovoked aggression against the Muslim community. They are the target of the defensive action, not the constraint itself.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, aggressor_forces, payer,
    powerful, immediate, mobile, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates individual spiritual discipline and collective defensive action to protect the Muslim community and uphold justice, while strictly adhering to ethical and legal limits such as proportionality and non-combatant immunity.
% TRANSFER_FUNCTION: Transfers spiritual discipline and moral responsibility to individuals, and defensive military action from state authorities to aggressor forces, while transferring protection to the Muslim community and non-combatants.
% ABSENT_VOICES: Those who advocate for offensive or revolutionary interpretations of jihad would object, arguing this reading is too restrictive, ignores certain textual interpretations, or fails to address perceived injustices with sufficient force. They are often marginalized by the scholarly consensus upholding this reading.
% DISAPPEARANCE_RATIONALE: If this understanding of jihad vanished, it would fundamentally alter the ethical framework for individual conduct and collective defense within the Muslim world. It could lead to either widespread pacifism leaving communities vulnerable, or unconstrained aggression lacking ethical limits, depending on which alternative interpretation gained dominance.
% FOUNDING_PROBLEM: To provide a comprehensive framework for individual moral struggle and collective defense against aggression, ensuring justice and preventing unwarranted violence, based on Quranic and Prophetic teachings, while safeguarding the community and upholding ethical principles.
% FOUNDING_PROBLEM_CORROBORATION: Mainstream Islamic scholarly consensus, the historical practice of many Muslim-majority states, and the ethical principles embedded in international humanitarian law (which often align with this reading's constraints) corroborate this interpretation as a foundational ethical and legal framework for both individual and collective action.
narrative_ontology:disappearance_verdict(jihad_quranic_corpus__defensive_spiritual_reading, world_rearranges).
narrative_ontology:founding_problem_status(jihad_quranic_corpus__defensive_spiritual_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jihad_quranic_corpus__defensive_spiritual_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(jihad_quranic_corpus__defensive_spiritual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jihad_quranic_corpus__defensive_spiritual_reading, 0.18, 'gemini-2.5-flash', 'none', direct).

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
 *   The low extractiveness (0.18) reflects that this reading primarily coordinates internal moral development and collective defense, not extraction from others. Suppression (0.15) is low because spiritual struggle is voluntary, and defensive action is a response to external threats, not internal coercion. Theater ratio (0.08) is minimal, as the emphasis is on genuine spiritual effort and necessary defense, not performative displays. Accessibility collapse (0.35) is moderate; alternatives to spiritual struggle exist, and alternatives to defensive war (e.g., diplomacy, deterrence) are considered before armed response. Resistance (0.12) is low within the community that adheres to this interpretation, as it is widely accepted as an ethical framework.
 *
 * PERSPECTIVAL GAP:
 *   The primary perspectival gap exists between this defensive-spiritual reading and other interpretations of jihad. While this reading emphasizes ethical limits and defensive action, other readings (e.g., expansionist-legalist, revolutionary-vanguard) would perceive it as overly restrictive or failing to fulfill broader religious obligations. This divergence is precisely what the kernel framework is designed to measure.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual believers and the Muslim community are beneficiaries, gaining spiritual guidance, ethical conduct, and collective protection. Islamic scholars and legitimate state authorities act as agenda-setters, defining and enforcing the parameters of this constraint. Non-Muslim civilians are explicitly excluded from being targets, reflecting the constraint's ethical limits. Aggressor forces are the payers, bearing the cost of defensive action when they initiate unprovoked aggression.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretive_dominance_ambiguity,
    'Is this defensive-spiritual reading the genuinely dominant interpretation of jihad within the broader Muslim world, or is its prevalence primarily asserted by specific scholarly traditions?',
    'Empirical sociological studies of religious belief and practice across diverse Muslim populations, and content analysis of religious education curricula and fatwas from various regions.',
    'If its dominance is overstated, the constraint''s effective scope and influence are lower than perceived, and other, more extractive readings may have greater practical impact. If genuinely dominant, it acts as a strong normative brake on other interpretations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_dominance_ambiguity, empirical, 'Assessing the actual prevalence and influence of the defensive-spiritual reading.').

omega_variable(
    application_consistency_gap,
    'How consistently are the constraints of proportionality and non-combatant immunity applied in practice by state authorities claiming to operate under this framework?',
    'Case studies of conflicts involving Muslim-majority states, human rights reports, and international legal analyses of military conduct.',
    'Inconsistent application would indicate a gap between the normative ideal of this reading and its practical implementation, suggesting a higher effective extractiveness or suppression in real-world conflicts, even if the stated doctrine is benign.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(application_consistency_gap, empirical, 'Gap between stated doctrine and practical application of ethical constraints.').

omega_variable(
    state_authority_legitimacy,
    'Is the requirement for state authority to declare armed jihad a necessary coordination mechanism to prevent anarchy, or a structural suppression of individual or local community agency in self-defense?',
    'Comparative analysis of historical and contemporary conflicts, examining outcomes in contexts where state authority was bypassed versus upheld, and assessing the resulting proportionality and civilian protection.',
    'If primarily suppressive, the constraint''s effective suppression is higher for non-state actors seeking to defend themselves; if primarily coordinative, it is a necessary condition for ethical conduct of war.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(state_authority_legitimacy, conceptual, 'Role of state authority in armed jihad: coordination vs. suppression.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jihad_quranic_corpus__defensive_spiritual_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jiha_tr_t0, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 0, 0.07).
narrative_ontology:measurement(jiha_tr_t20, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 20, 0.07).
narrative_ontology:measurement(jiha_tr_t40, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 40, 0.08).
narrative_ontology:measurement(jiha_tr_t60, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 60, 0.08).
narrative_ontology:measurement(jiha_tr_t80, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 80, 0.08).
narrative_ontology:measurement(jiha_tr_t100, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 100, 0.08).

% Extraction over time
narrative_ontology:measurement(jiha_be_t0, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 0, 0.17).
narrative_ontology:measurement(jiha_be_t20, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 20, 0.17).
narrative_ontology:measurement(jiha_be_t40, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 40, 0.18).
narrative_ontology:measurement(jiha_be_t60, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 60, 0.18).
narrative_ontology:measurement(jiha_be_t80, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 80, 0.18).
narrative_ontology:measurement(jiha_be_t100, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 100, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(jiha_su_t0, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 0, 0.14).
narrative_ontology:measurement(jiha_su_t20, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 20, 0.14).
narrative_ontology:measurement(jiha_su_t40, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 40, 0.15).
narrative_ontology:measurement(jiha_su_t60, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 60, 0.15).
narrative_ontology:measurement(jiha_su_t80, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 80, 0.15).
narrative_ontology:measurement(jiha_su_t100, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 100, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jihad_quranic_corpus__defensive_spiritual_reading, identity_coordination).
narrative_ontology:affects_constraint(jihad_quranic_corpus__defensive_spiritual_reading, jihad_quranic_corpus__expansionist_legalist_reading).
narrative_ontology:affects_constraint(jihad_quranic_corpus__defensive_spiritual_reading, jihad_quranic_corpus__revolutionary_vanguard_reading).
narrative_ontology:affects_constraint(jihad_quranic_corpus__defensive_spiritual_reading, islamic_law_of_war).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
