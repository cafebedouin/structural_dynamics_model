% ============================================================================
% CONSTRAINT STORY: article_2_7_chapter_vii_tension__r2p_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_2_7_chapter_vii_tension__r2p_reading, []).

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
 *   constraint_id: article_2_7_chapter_vii_tension__r2p_reading
 *   human_readable: Responsibility to Protect (R2P) Doctrine
 *   domain: international_law/political_philosophy/security_studies
 *
 * SUMMARY:
 *   This constraint represents the 'Responsibility to Protect' (R2P)
 *   doctrine, a reading of the tension between state sovereignty (UN Charter
 *   Article 2(7)) and the UN Security Council's Chapter VII powers. R2P
 *   asserts that sovereignty is not absolute but conditional on a state's
 *   protection of its own population from mass atrocities. If a state fails
 *   this responsibility, the international community has a responsibility to
 *   intervene. This reading legitimizes intervention, making it a Tangled
 *   Rope: it coordinates international action for humanitarian protection
 *   while extracting from the sovereignty of targeted states.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_2_7_chapter_vii_tension__r2p_reading, 0.78).
domain_priors:suppression_score(article_2_7_chapter_vii_tension__r2p_reading, 0.85).
domain_priors:theater_ratio(article_2_7_chapter_vii_tension__r2p_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_2_7_chapter_vii_tension__r2p_reading, tangled_rope).
narrative_ontology:human_readable(article_2_7_chapter_vii_tension__r2p_reading, "Responsibility to Protect (R2P) Doctrine").
narrative_ontology:topic_domain(article_2_7_chapter_vii_tension__r2p_reading, "international_law/political_philosophy/security_studies").

domain_priors:requires_active_enforcement(article_2_7_chapter_vii_tension__r2p_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_2_7_chapter_vii_tension__r2p_reading, 'e268a869-34f1-4b87-9306-813bba9b2bb5').
narrative_ontology:cs_kernel_codification('e268a869-34f1-4b87-9306-813bba9b2bb5', formalized).
narrative_ontology:cs_authority_grounding('e268a869-34f1-4b87-9306-813bba9b2bb5', lineage).
narrative_ontology:cs_interpretation_layer_present('e268a869-34f1-4b87-9306-813bba9b2bb5').
narrative_ontology:cs_reading_relation('e268a869-34f1-4b87-9306-813bba9b2bb5', article_2_7_chapter_vii_tension__sovereignty_first_reading, forecloses).
narrative_ontology:cs_axiom('e268a869-34f1-4b87-9306-813bba9b2bb5', foundational, sovereignty_as_responsibility).
narrative_ontology:cs_axiom_status(sovereignty_as_responsibility, holdable).
narrative_ontology:cs_axiom_grounding('e268a869-34f1-4b87-9306-813bba9b2bb5', sovereignty_as_responsibility, deontological).
narrative_ontology:cs_axiom('e268a869-34f1-4b87-9306-813bba9b2bb5', foundational, international_community_has_duty_to_protect).
narrative_ontology:cs_axiom_status(international_community_has_duty_to_protect, holdable).
narrative_ontology:cs_axiom_grounding('e268a869-34f1-4b87-9306-813bba9b2bb5', international_community_has_duty_to_protect, deontological).
narrative_ontology:cs_reference_frame('e268a869-34f1-4b87-9306-813bba9b2bb5', post_rwanda_srebrenica_consensus).
narrative_ontology:cs_drift_state('e268a869-34f1-4b87-9306-813bba9b2bb5', contemporary_geopolitical_challenges, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('e268a869-34f1-4b87-9306-813bba9b2bb5', '').
narrative_ontology:cs_kernel_id(article_2_7_chapter_vii_tension__r2p_reading, article_2_7_chapter_vii_tension).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__r2p_reading, persecuted_populations).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__r2p_reading, targeted_states).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__r2p_reading, traditional_sovereignty_norm).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__r2p_reading, intervening_states).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__r2p_reading, traditional_sovereignty_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Populations facing mass atrocities (genocide, war crimes, ethnic cleansing, crimes against humanity) within their own state. They are the primary intended beneficiaries of R2P, as it legitimizes external intervention for their protection.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, persecuted_populations, beneficiary,
    powerless, immediate, trapped, local).

% States whose governments are failing to protect their populations from mass atrocities, or are actively perpetrating them. R2P asserts that their sovereignty is conditional and can be overridden by international intervention, leading to a loss of control and potential regime change.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, targeted_states, payer,
    powerful, immediate, constrained, national).

% The primary body responsible for authorizing interventions under R2P. Its decisions are critical for legitimizing action, but its permanent members' veto power can constrain or selectively apply the doctrine.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, un_security_council, agenda_setter,
    institutional, generational, analytical, global).

% States willing and able to undertake military or other coercive measures to protect populations under R2P. They benefit from upholding international norms and stability, but bear the costs and risks of intervention.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, intervening_states, agenda_setter,
    institutional, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(article_2_7_chapter_vii_tension__r2p_reading, intervening_states, beneficiary).

% States and legal scholars who prioritize absolute state sovereignty and non-interference in internal affairs. R2P challenges their foundational understanding of international law, imposing a cost on their preferred normative order.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, traditional_sovereignty_advocates, payer,
    organized, generational, constrained, global).

% Advocate for the protection of human rights and often call for R2P interventions. They monitor situations, provide evidence of atrocities, and lobby international bodies, but lack direct enforcement power.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, humanitarian_ngos, observer,
    moderate, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate international action, including diplomatic, humanitarian, and potentially military means, to prevent and respond to mass atrocities when a state fails to protect its own population.
% TRANSFER_FUNCTION: Transfers the exclusive right of a state to manage its internal affairs (non-interference) to the international community when that state fails its primary responsibility to protect its population, enabling external intervention. It also transfers the burden of protection from the failing state to the international community.
% ABSENT_VOICES: States that fear R2P's selective application, those who view it as a pretext for neo-colonialism or regime change, and non-interventionist blocs who argue for strict adherence to Article 2(7) of the UN Charter. They would object to the erosion of absolute sovereignty.
% DISAPPEARANCE_RATIONALE: If R2P vanished overnight, the international community would lose its primary normative framework for responding to mass atrocities. This would likely lead to more unchecked genocides and war crimes, a return to a stricter non-interventionist paradigm, or ad-hoc interventions lacking broad legitimacy, fundamentally reorganizing international security responses.
% FOUNDING_PROBLEM: The failure of the international community to prevent or respond effectively to genocides and mass atrocities (e.g., Rwanda, Srebrenica) in the late 20th century, where traditional sovereignty claims shielded perpetrators from external intervention.
% FOUNDING_PROBLEM_CORROBORATION: Human rights organizations, UN reports, and academic analyses consistently document ongoing mass atrocities and the need for such a framework, corroborating the problem's continued existence. The UN Secretary-General's reports and various independent commissions have repeatedly affirmed the necessity of R2P.
narrative_ontology:disappearance_verdict(article_2_7_chapter_vii_tension__r2p_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_2_7_chapter_vii_tension__r2p_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_2_7_chapter_vii_tension__r2p_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(article_2_7_chapter_vii_tension__r2p_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_2_7_chapter_vii_tension__r2p_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_2_7_chapter_vii_tension__r2p_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_2_7_chapter_vii_tension__r2p_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_2_7_chapter_vii_tension__r2p_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is high (0.78) because R2P fundamentally redefines sovereignty, imposing a significant cost on states that fail to protect their populations, potentially leading to military intervention and regime change. Suppression is very high (0.85) as it involves overriding state authority, often through coercive means (sanctions, military force) to prevent resistance. Theater ratio is low (0.15) because R2P interventions, when they occur, are high-stakes and genuinely aimed at protection, not mere performance. Accessibility collapse is moderate-high (0.70) as it significantly limits the 'alternative' of non-intervention for the international community in atrocity situations. Resistance is high (0.75) from states and blocs that oppose the doctrine's implications for sovereignty.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of persecuted populations and intervening states, R2P is a vital coordination mechanism to prevent atrocities. From the perspective of targeted states and traditional sovereignty advocates, it is an extractive mechanism that undermines foundational principles of international law and can be abused for geopolitical ends. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Persecuted populations are clear beneficiaries (d near 0.0). Targeted states and the traditional sovereignty norm are clear victims (d near 1.0). The UN Security Council and intervening states act as agenda-setters, balancing coordination benefits (stability, norm enforcement) with the costs and risks of intervention (d near 0.3-0.4). Traditional sovereignty advocates bear the cost of the norm shift (d near 0.8).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    selective_application_bias,
    'Is R2P applied consistently and universally, or is its application selectively influenced by geopolitical interests and power dynamics?',
    'Empirical analysis of R2P interventions and non-interventions across various mass atrocity situations, controlling for geopolitical factors and state capacity.',
    'If selectively applied, R2P''s effective extractiveness is amplified for geopolitically vulnerable states, and its coordination function is undermined by perceived hypocrisy, potentially reclassifying it closer to a Snare for those targets.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(selective_application_bias, empirical, 'Whether R2P is a universal norm or a tool of selective intervention.').

omega_variable(
    legitimacy_vs_effectiveness_tradeoff,
    'Does intervention under R2P consistently achieve its protective goals without exacerbating conflict, undermining long-term stability, or creating moral hazard?',
    'Longitudinal studies of post-intervention states, comparing humanitarian outcomes, state-building success, and regional stability with non-intervention scenarios.',
    'If interventions frequently fail to achieve protective goals or lead to worse outcomes, the ''coordination'' aspect of R2P is weakened, increasing its effective extractiveness and potentially shifting its classification towards a Snare or Piton if the function atrophies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_vs_effectiveness_tradeoff, empirical, 'The actual humanitarian and political efficacy of R2P interventions.').

omega_variable(
    sovereignty_redefinition_ambiguity,
    'Is R2P a genuine redefinition of sovereignty as responsibility, or is it an exception to traditional sovereignty that leaves the core principle intact?',
    'Conceptual analysis of international legal discourse and state practice: does the language and action consistently treat sovereignty as inherently conditional, or as a default that can be overridden?',
    'If it''s merely an exception, the ''extraction'' from traditional sovereignty is less fundamental, potentially lowering base extractiveness. If it''s a redefinition, the extraction is structural and foundational, reinforcing the current high extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_redefinition_ambiguity, conceptual, 'The fundamental nature of sovereignty under R2P.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_2_7_chapter_vii_tension__r2p_reading, 2005, 2035).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t2005, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 2005, 0.1).
narrative_ontology:measurement(arti_tr_t2010, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 2010, 0.12).
narrative_ontology:measurement(arti_tr_t2015, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 2015, 0.15).
narrative_ontology:measurement(arti_tr_t2020, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 2020, 0.15).
narrative_ontology:measurement(arti_tr_t2025, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 2025, 0.14).
narrative_ontology:measurement(arti_tr_t2030, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 2030, 0.15).
narrative_ontology:measurement(arti_tr_t2035, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 2035, 0.15).

% Extraction over time
narrative_ontology:measurement(arti_be_t2005, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 2005, 0.65).
narrative_ontology:measurement(arti_be_t2010, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 2010, 0.7).
narrative_ontology:measurement(arti_be_t2015, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 2015, 0.75).
narrative_ontology:measurement(arti_be_t2020, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 2020, 0.78).
narrative_ontology:measurement(arti_be_t2025, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 2025, 0.77).
narrative_ontology:measurement(arti_be_t2030, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 2030, 0.78).
narrative_ontology:measurement(arti_be_t2035, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 2035, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t2005, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 2005, 0.7).
narrative_ontology:measurement(arti_su_t2010, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 2010, 0.75).
narrative_ontology:measurement(arti_su_t2015, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 2015, 0.8).
narrative_ontology:measurement(arti_su_t2020, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 2020, 0.85).
narrative_ontology:measurement(arti_su_t2025, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 2025, 0.84).
narrative_ontology:measurement(arti_su_t2030, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 2030, 0.85).
narrative_ontology:measurement(arti_su_t2035, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 2035, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_2_7_chapter_vii_tension__r2p_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
