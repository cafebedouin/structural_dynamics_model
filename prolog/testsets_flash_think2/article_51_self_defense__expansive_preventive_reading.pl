% ============================================================================
% CONSTRAINT STORY: article_51_self_defense__expansive_preventive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_51_self_defense__expansive_preventive_reading, []).

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
 *   constraint_id: article_51_self_defense__expansive_preventive_reading
 *   human_readable: Expansive Preventive Self-Defense Doctrine (Article 51 Reading)
 *   domain: international_law/security_studies
 *
 * SUMMARY:
 *   This constraint represents an expansive interpretation of Article 51 of
 *   the UN Charter, which permits self-defense. This reading extends the
 *   right of self-defense to include preemptive or preventive uses of force
 *   against non-state actors or emerging threats, with the necessity of such
 *   force often self-judged by the acting state. This interpretation gained
 *   significant traction in the post-9/11 era, driven by concerns over
 *   transnational terrorism and the proliferation of WMDs. It is contested by
 *   states and scholars who advocate for a narrower reading of Article 51,
 *   emphasizing the need for an actual or imminent armed attack attributable
 *   to a state and multilateral authorization.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_51_self_defense__expansive_preventive_reading, 0.8).
domain_priors:suppression_score(article_51_self_defense__expansive_preventive_reading, 0.85).
domain_priors:theater_ratio(article_51_self_defense__expansive_preventive_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_51_self_defense__expansive_preventive_reading, tangled_rope).
narrative_ontology:human_readable(article_51_self_defense__expansive_preventive_reading, "Expansive Preventive Self-Defense Doctrine (Article 51 Reading)").
narrative_ontology:topic_domain(article_51_self_defense__expansive_preventive_reading, "international_law/security_studies").

domain_priors:requires_active_enforcement(article_51_self_defense__expansive_preventive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_51_self_defense__expansive_preventive_reading, '6736db10-58ab-426c-9e5f-315e37d4a3b2').
narrative_ontology:cs_kernel_codification('6736db10-58ab-426c-9e5f-315e37d4a3b2', fixed_text).
narrative_ontology:cs_authority_grounding('6736db10-58ab-426c-9e5f-315e37d4a3b2', extraction).
narrative_ontology:cs_interpretation_layer_present('6736db10-58ab-426c-9e5f-315e37d4a3b2').
narrative_ontology:cs_reading_relation('6736db10-58ab-426c-9e5f-315e37d4a3b2', article_51_self_defense__narrow_armed_attack_reading, forecloses).
narrative_ontology:cs_reading_relation('6736db10-58ab-426c-9e5f-315e37d4a3b2', article_51_self_defense__unable_unwilling_doctrine_reading, influences).
narrative_ontology:cs_axiom('6736db10-58ab-426c-9e5f-315e37d4a3b2', foundational, preemptive_self_defense_is_inherent_right).
narrative_ontology:cs_axiom_status(preemptive_self_defense_is_inherent_right, holdable).
narrative_ontology:cs_axiom_grounding('6736db10-58ab-426c-9e5f-315e37d4a3b2', preemptive_self_defense_is_inherent_right, deontological).
narrative_ontology:cs_axiom('6736db10-58ab-426c-9e5f-315e37d4a3b2', secondary, threat_imminence_is_flexible).
narrative_ontology:cs_axiom_status(threat_imminence_is_flexible, holdable).
narrative_ontology:cs_axiom_grounding('6736db10-58ab-426c-9e5f-315e37d4a3b2', threat_imminence_is_flexible, empirically_contingent).
narrative_ontology:cs_reference_frame('6736db10-58ab-426c-9e5f-315e37d4a3b2', post_9_11_security_paradigm).
narrative_ontology:cs_drift_state('6736db10-58ab-426c-9e5f-315e37d4a3b2', contemporary_geopolitical_landscape, gap(stable, minor, true)).
narrative_ontology:cs_created_at('6736db10-58ab-426c-9e5f-315e37d4a3b2', '').
narrative_ontology:cs_kernel_id(article_51_self_defense__expansive_preventive_reading, article_51_self_defense).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_51_self_defense__expansive_preventive_reading, militarily_capable_states).
narrative_ontology:constraint_beneficiary(article_51_self_defense__expansive_preventive_reading, defense_sectors).
narrative_ontology:constraint_victim(article_51_self_defense__expansive_preventive_reading, target_region_populations).
narrative_ontology:constraint_victim(article_51_self_defense__expansive_preventive_reading, multilateral_veto_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States with significant military power that assert the right to use preemptive or preventive force against perceived threats, often self-judging the necessity. They benefit from increased flexibility in foreign policy and security operations.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, militarily_capable_states, agenda_setter,
    institutional, generational, arbitrage, global).

% Military-industrial complexes, private security contractors, and defense research institutions that benefit from the expanded scope of military operations and interventions justified by this doctrine.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, defense_sectors, beneficiary,
    organized, biographical, mobile, global).

% Populations in regions targeted by preemptive or preventive strikes, who bear the direct costs of conflict, displacement, and instability, often without direct representation in the decision-making process.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, target_region_populations, payer,
    powerless, immediate, trapped, local).

% Entities like the UN Security Council, whose authority to authorize the use of force is bypassed or undermined by unilateral actions justified under this expansive reading of self-defense. They bear the cost of diminished legitimacy and effectiveness.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, multilateral_veto_authority, payer,
    institutional, generational, constrained, global).

% States that advocate for a more restrictive interpretation of Article 51, emphasizing the primacy of the UN Charter and the need for Security Council authorization. They resist the erosion of multilateral norms.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, states_advocating_narrow_interpretation, observer,
    powerful, generational, constrained, global).

% Academics and legal experts who analyze the evolution and implications of international law, often critiquing the expansive interpretation for its potential to destabilize the international legal order.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, international_law_scholars, observer,
    analytical, biographical, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate international security efforts by allowing states to address perceived threats from non-state actors or emerging dangers more flexibly, without waiting for a traditional 'armed attack' or multilateral consensus.
% TRANSFER_FUNCTION: Transfers the primary authority for determining the necessity and legality of force from multilateral bodies (like the UN Security Council) to individual militarily capable states. It also transfers the costs of such interventions to target populations and the legitimacy of the international legal framework.
% ABSENT_VOICES: Non-state actors, populations in targeted regions, and states with limited military capabilities who are often the subjects of such interventions but lack a voice in the interpretation or application of this doctrine.
% DISAPPEARANCE_RATIONALE: If this expansive reading vanished overnight, militarily capable states would face significantly higher legal and political hurdles for unilateral interventions. This would likely lead to increased reliance on multilateral authorization, more stringent evidentiary requirements for force, or a perceived 'security gap' by some states, fundamentally altering global security dynamics.
% FOUNDING_PROBLEM: The perceived inadequacy of traditional international law (specifically Article 51 of the UN Charter) to effectively address transnational terrorism, rapidly evolving non-state actor threats, and the proliferation of weapons of mass destruction in the post-Cold War and post-9/11 security environment.
% FOUNDING_PROBLEM_CORROBORATION: Proponents (militarily capable states, defense sectors) consistently cite ongoing terrorist threats, cyber warfare, and proliferation risks as evidence that the founding problem remains live. Critics (states advocating narrow interpretation, international law scholars) argue that while threats exist, the expansive interpretation is an overreach that exacerbates instability, not a necessary solution; their corroboration comes from historical analysis of interventions and the erosion of international norms.
narrative_ontology:disappearance_verdict(article_51_self_defense__expansive_preventive_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_51_self_defense__expansive_preventive_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_51_self_defense__expansive_preventive_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(article_51_self_defense__expansive_preventive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_51_self_defense__expansive_preventive_reading, 0.8, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_51_self_defense__expansive_preventive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_51_self_defense__expansive_preventive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_51_self_defense__expansive_preventive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.80) reflects the significant costs imposed on target populations and the erosion of multilateral authority, while militarily capable states gain unilateral flexibility. Suppression (0.85) is high because this interpretation actively suppresses alternative legal frameworks for the use of force, requiring constant justification and defense against challenges to its legitimacy. Theater ratio (0.40) is moderate, as there are genuine security concerns driving the doctrine, but also a performative aspect in framing unilateral actions as 'necessary' self-defense to bypass international scrutiny. Accessibility collapse (0.75) is high as it limits alternatives to unilateral force, and resistance (0.70) is high due to strong opposition from states and legal scholars advocating for multilateralism.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of militarily capable states, this reading is a necessary adaptation of international law to modern threats, enabling effective security. From the perspective of target populations and multilateral bodies, it is an extractive mechanism that undermines sovereignty and international order. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Militarily capable states and their defense sectors are clear beneficiaries, gaining expanded operational scope and resources (low directionality). Target region populations and multilateral veto authority are victims, bearing the costs of intervention and diminished authority, respectively (high directionality). States advocating a narrow interpretation and international law scholars act as observers and critics, experiencing the constraint as a challenge to established norms.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a valid interpretation of Article 51 of the UN Charter, or does it fundamentally alter the original intent of the self-defense provision?',
    'Analysis of state practice, opinio juris, and ICJ/ICC jurisprudence over time, alongside a textual and historical analysis of the UN Charter''s drafting history.',
    'If deemed a fundamental alteration, the legitimacy of actions taken under this reading would be severely undermined, potentially reclassifying it as a snare from the perspective of multilateral institutions. If deemed a valid evolution, its coordination function would be strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Ambiguity regarding the fidelity of this reading to the core kernel of Article 51.').

omega_variable(
    necessity_self_judgment_objectivity,
    'To what extent is the ''necessity'' for preemptive/preventive force objectively demonstrable, versus being a subjective judgment used to justify unilateral action?',
    'Independent, ex-post facto review of intelligence assessments and threat analyses that led to interventions, comparing them against actual outcomes and alternative non-military options.',
    'If necessity is consistently found to be subjective or exaggerated, the extractiveness and theater_ratio of the constraint would be higher, pushing it closer to a pure snare. If objective necessity is frequently corroborated, its coordination function would be more robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessity_self_judgment_objectivity, empirical, 'Ambiguity regarding the objectivity of self-judged necessity for preventive force.').

omega_variable(
    effectiveness_of_preventive_force,
    'Does the application of this expansive reading genuinely enhance international security and prevent threats, or does it contribute to instability and radicalization?',
    'Longitudinal studies comparing security outcomes in regions where this doctrine has been applied versus regions where it has not, controlling for other geopolitical factors.',
    'If found to consistently destabilize regions or provoke further conflict, the justification for the constraint''s coordination function would be weakened, increasing its perceived extractiveness. If it demonstrably prevents threats, its coordination value would be higher.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(effectiveness_of_preventive_force, empirical, 'Ambiguity regarding the long-term effectiveness and unintended consequences of preventive force.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_51_self_defense__expansive_preventive_reading, 2001, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t2001, article_51_self_defense__expansive_preventive_reading, theater_ratio, 2001, 0.2).
narrative_ontology:measurement(arti_tr_t2006, article_51_self_defense__expansive_preventive_reading, theater_ratio, 2006, 0.28).
narrative_ontology:measurement(arti_tr_t2012, article_51_self_defense__expansive_preventive_reading, theater_ratio, 2012, 0.33).
narrative_ontology:measurement(arti_tr_t2018, article_51_self_defense__expansive_preventive_reading, theater_ratio, 2018, 0.37).
narrative_ontology:measurement(arti_tr_t2024, article_51_self_defense__expansive_preventive_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(arti_be_t2001, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 2001, 0.6).
narrative_ontology:measurement(arti_be_t2006, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 2006, 0.68).
narrative_ontology:measurement(arti_be_t2012, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 2012, 0.73).
narrative_ontology:measurement(arti_be_t2018, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 2018, 0.77).
narrative_ontology:measurement(arti_be_t2024, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 2024, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t2001, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 2001, 0.7).
narrative_ontology:measurement(arti_su_t2006, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 2006, 0.75).
narrative_ontology:measurement(arti_su_t2012, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 2012, 0.8).
narrative_ontology:measurement(arti_su_t2018, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 2018, 0.83).
narrative_ontology:measurement(arti_su_t2024, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 2024, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_51_self_defense__expansive_preventive_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_51_self_defense__expansive_preventive_reading, un_security_council_veto_power).
narrative_ontology:affects_constraint(article_51_self_defense__expansive_preventive_reading, sovereignty_of_states).
narrative_ontology:affects_constraint(article_51_self_defense__expansive_preventive_reading, international_humanitarian_law).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'article_51_self_defense' kernel. It represents the expansive interpretation, which directly influences and is influenced by the 'narrow_armed_attack_reading' and the 'unable_unwilling_doctrine_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
