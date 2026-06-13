% ============================================================================
% CONSTRAINT STORY: gdpr_article_3_scope__effects_jurisdiction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gdpr_article_3_scope__effects_jurisdiction_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: gdpr_article_3_scope__effects_jurisdiction_reading
 *   human_readable: GDPR Article 3(2) Extraterritoriality (Effects Jurisdiction Reading)
 *   domain: technology_governance/international_law/privacy_regulation
 *
 * SUMMARY:
 *   This constraint describes the interpretation of GDPR Article 3(2) that
 *   asserts extraterritorial jurisdiction based on the 'targeting or
 *   monitoring' of EU residents, regardless of the data controller's physical
 *   presence in the EU. This 'effects jurisdiction' reading extends the reach
 *   of EU privacy law globally, imposing significant compliance burdens on
 *   non-EU entities. It is a contested interpretation, with alternative
 *   readings focusing on market access or strict territorial sovereignty.
 *
 * KEY AGENTS:
 *   - eu_data_protection_authorities: Agenda setter (institutional/analytical) — enforces GDPR extraterritorially.
 *   - eu_data_subjects: Beneficiary (organized/generational) — receives enhanced privacy protection.
 *   - non_eu_data_controllers: Payer (powerful/biographical) — bears compliance costs and potential fines.
 *   - global_tech_companies: Payer (institutional/biographical) — large-scale data processors facing significant compliance burdens.
 *   - non_eu_governments: Excluded/Observer (institutional/generational) — may challenge EU's jurisdictional claims.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gdpr_article_3_scope__effects_jurisdiction_reading, 0.65).
domain_priors:suppression_score(gdpr_article_3_scope__effects_jurisdiction_reading, 0.78).
domain_priors:theater_ratio(gdpr_article_3_scope__effects_jurisdiction_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gdpr_article_3_scope__effects_jurisdiction_reading, tangled_rope).
narrative_ontology:human_readable(gdpr_article_3_scope__effects_jurisdiction_reading, "GDPR Article 3(2) Extraterritoriality (Effects Jurisdiction Reading)").
narrative_ontology:topic_domain(gdpr_article_3_scope__effects_jurisdiction_reading, "technology_governance/international_law/privacy_regulation").

domain_priors:requires_active_enforcement(gdpr_article_3_scope__effects_jurisdiction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gdpr_article_3_scope__effects_jurisdiction_reading, '2d0af685-47e1-4a06-a1f6-199ee70d29d4').
narrative_ontology:cs_kernel_codification('2d0af685-47e1-4a06-a1f6-199ee70d29d4', fixed_text).
narrative_ontology:cs_authority_grounding('2d0af685-47e1-4a06-a1f6-199ee70d29d4', lineage).
narrative_ontology:cs_interpretation_layer_present('2d0af685-47e1-4a06-a1f6-199ee70d29d4').
narrative_ontology:cs_reading_relation('2d0af685-47e1-4a06-a1f6-199ee70d29d4', gdpr_article_3_scope__market_access_reading, coexists_with).
narrative_ontology:cs_reading_relation('2d0af685-47e1-4a06-a1f6-199ee70d29d4', gdpr_article_3_scope__territorial_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('2d0af685-47e1-4a06-a1f6-199ee70d29d4', foundational, data_protection_follows_the_person).
narrative_ontology:cs_axiom_status(data_protection_follows_the_person, holdable).
narrative_ontology:cs_axiom_grounding('2d0af685-47e1-4a06-a1f6-199ee70d29d4', data_protection_follows_the_person, deontological).
narrative_ontology:cs_axiom('2d0af685-47e1-4a06-a1f6-199ee70d29d4', foundational, effects_trigger_jurisdiction).
narrative_ontology:cs_axiom_status(effects_trigger_jurisdiction, holdable).
narrative_ontology:cs_axiom_grounding('2d0af685-47e1-4a06-a1f6-199ee70d29d4', effects_trigger_jurisdiction, conventional).
narrative_ontology:cs_reference_frame('2d0af685-47e1-4a06-a1f6-199ee70d29d4', eu_regulatory_autonomy_over_data).
narrative_ontology:cs_drift_state('2d0af685-47e1-4a06-a1f6-199ee70d29d4', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('2d0af685-47e1-4a06-a1f6-199ee70d29d4', '').
narrative_ontology:cs_kernel_id(gdpr_article_3_scope__effects_jurisdiction_reading, gdpr_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__effects_jurisdiction_reading, eu_data_subjects).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__effects_jurisdiction_reading, eu_data_protection_authorities).
narrative_ontology:constraint_victim(gdpr_article_3_scope__effects_jurisdiction_reading, non_eu_data_controllers).
narrative_ontology:constraint_victim(gdpr_article_3_scope__effects_jurisdiction_reading, global_tech_companies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for interpreting and enforcing GDPR, including its extraterritorial provisions. They actively pursue cases against non-EU entities that process EU residents' data, asserting the effects-based jurisdiction.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, eu_data_protection_authorities, agenda_setter,
    institutional, generational, analytical, regional).

% Individuals residing in the EU whose personal data is processed. They benefit from the extended protection of their privacy rights, regardless of where the data controller is located. Their ability to enforce these rights depends on the DPAs.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, eu_data_subjects, beneficiary,
    organized, biographical, constrained, regional).

% Companies and organizations located outside the EU that process personal data of EU residents by 'targeting' or 'monitoring' them. They face significant compliance costs, legal uncertainty, and potential fines if they fail to adhere to GDPR standards.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, non_eu_data_controllers, payer,
    powerful, biographical, constrained, global).

% Large multinational technology corporations that process vast amounts of data globally, including from EU residents. They are primary targets of GDPR's extraterritorial reach and invest heavily in compliance, often setting global privacy standards based on GDPR.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, global_tech_companies, payer,
    institutional, biographical, constrained, global).

% Governments of countries outside the EU that may view the GDPR's extraterritorial application as an infringement on their national sovereignty or a barrier to their domestic businesses. They engage in diplomatic discussions and may consider retaliatory measures or alternative regulatory frameworks.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, non_eu_governments, observer,
    institutional, generational, analytical, global).

% Academics and legal experts who analyze the implications of GDPR's extraterritoriality for international law, sovereignty, and global governance. They contribute to the conceptual debate around the constraint's legitimacy and effectiveness.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, international_legal_scholars, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a consistent and high standard of data protection for EU residents, ensuring their privacy rights are upheld even when their data is processed by entities outside the EU, thereby fostering trust in the digital economy.
% TRANSFER_FUNCTION: Transfers compliance costs and legal obligations from EU data subjects and EU-based entities to non-EU data controllers and global tech companies, in exchange for enhanced data protection for EU residents.
% ABSENT_VOICES: Non-EU businesses and governments that prioritize national regulatory autonomy or lower compliance burdens would object, arguing for a more limited, territorial scope of privacy regulation. Their voices are present in diplomatic channels but often lack direct influence over EU legislative and enforcement processes.
% DISAPPEARANCE_RATIONALE: If this extraterritorial application vanished, non-EU data controllers would likely revert to their domestic privacy standards, leading to a significant reduction in data protection for EU residents whose data is processed abroad. This would fragment global privacy standards and undermine the EU's ability to protect its citizens' digital rights, forcing a major reorganization of international data governance.
% FOUNDING_PROBLEM: The problem of protecting EU residents' personal data in a globalized digital environment where data processing often occurs outside the EU's physical borders, leading to a 'race to the bottom' in privacy standards.
% FOUNDING_PROBLEM_CORROBORATION: The EU institutions and data protection authorities consistently attest that the problem of global data protection remains live, citing ongoing cross-border data flows and the need for robust enforcement. Independent privacy advocates and international legal scholars also corroborate the persistent challenge of ensuring privacy in a global digital economy, supporting the continued relevance of extraterritorial measures.
narrative_ontology:disappearance_verdict(gdpr_article_3_scope__effects_jurisdiction_reading, world_rearranges).
narrative_ontology:founding_problem_status(gdpr_article_3_scope__effects_jurisdiction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gdpr_article_3_scope__effects_jurisdiction_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(gdpr_article_3_scope__effects_jurisdiction_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gdpr_article_3_scope__effects_jurisdiction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gdpr_article_3_scope__effects_jurisdiction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gdpr_article_3_scope__effects_jurisdiction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is high due to the significant compliance costs and potential fines imposed on non-EU entities. Suppression (0.78) is also high, as non-EU entities have limited options to avoid compliance if they process EU residents' data, and enforcement mechanisms (e.g., adequacy decisions, fines) are robust. Theater ratio (0.15) is low, as the enforcement is generally effective and not merely performative. Accessibility collapse (0.4) is moderate; while alternatives to processing EU data exist (e.g., not targeting EU residents), the economic incentive to do so is strong. Resistance (0.55) is moderate, coming from non-EU companies and some governments, but often yields to EU enforcement power.
 *
 * PERSPECTIVAL GAP:
 *   EU data protection authorities and data subjects perceive this as a legitimate and necessary extension of privacy rights, a 'rope' or 'scaffold' for digital rights. Non-EU data controllers and global tech companies, however, experience it as a 'snare' or 'tangled rope' due to the high compliance costs and perceived overreach of jurisdiction. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   EU data protection authorities (agenda_setter) and EU data subjects (beneficiary) are on the beneficiary side, as the constraint directly serves their interests. Non-EU data controllers and global tech companies (payers) are on the target side, bearing the costs and enforcement. Non-EU governments are observers, sometimes challenging the constraint but not directly subject to its extraction in the same way as data controllers.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate is to protect EU data subjects' privacy rights in a globalized digital economy. This mandate is still live and actively pursued. The classification as 'tangled_rope' reflects that while there is a genuine coordination function (harmonizing privacy standards for EU residents), there is also significant asymmetric extraction from non-EU entities, maintained by active enforcement. This prevents mislabeling it as a 'snare' (which would imply no coordination function) or a 'rope' (which would ignore the asymmetric extraction).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    effects_vs_market_access_framing,
    'Is the extraterritorial reach of GDPR Article 3(2) primarily an assertion of effects-based jurisdiction, or a market access condition (Brussels Effect)?',
    'Analysis of enforcement patterns: if enforcement targets entities with no physical presence in the EU but significant data processing of EU residents, it supports effects-based jurisdiction. If enforcement primarily targets entities seeking to operate in the EU market, it supports market access.',
    'If primarily effects-based, the constraint is a stronger assertion of EU regulatory power. If primarily market access, it''s a standard-setting mechanism for global trade.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(effects_vs_market_access_framing, conceptual, 'Ambiguity in the legal grounding of GDPR''s extraterritoriality.').

omega_variable(
    sovereignty_challenge_legitimacy,
    'To what extent do non-EU states legitimately challenge the GDPR''s extraterritorial application as an overreach of sovereignty?',
    'Analysis of international legal challenges, diplomatic protests, and retaliatory legislation from non-EU states. The number and success rate of such challenges would indicate the level of legitimate contestation.',
    'High legitimate contestation would increase the effective suppression cost for the EU and potentially lead to international legal friction, weakening the constraint''s global reach. Low contestation would affirm the EU''s jurisdictional claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sovereignty_challenge_legitimacy, empirical, 'The degree of international legal and political acceptance of effects-based extraterritorial jurisdiction.').

omega_variable(
    kernel_reading_identification,
    'This constraint is the ''effects_jurisdiction_reading'' of the ''gdpr_article_3_scope'' kernel. What structural elements would change if a sibling reading were adopted?',
    'Compare the legal interpretations and enforcement mechanisms of the ''market_access_reading'' and ''territorial_sovereignty_reading'' siblings.',
    'The ''market_access_reading'' would shift the focus from direct jurisdictional assertion to conditional market entry, potentially altering the enforcement targets and mechanisms. The ''territorial_sovereignty_reading'' would drastically limit GDPR''s reach, reducing compliance costs for non-EU entities and weakening EU data subject protection.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Identifies this constraint as a specific reading of the GDPR Article 3 scope kernel and outlines implications of alternative readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gdpr_article_3_scope__effects_jurisdiction_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gdpr_tr_t0, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(gdpr_tr_t2, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 2, 0.18).
narrative_ontology:measurement(gdpr_tr_t4, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 4, 0.16).
narrative_ontology:measurement(gdpr_tr_t6, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 6, 0.15).

% Extraction over time
narrative_ontology:measurement(gdpr_be_t0, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(gdpr_be_t2, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 2, 0.55).
narrative_ontology:measurement(gdpr_be_t4, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 4, 0.6).
narrative_ontology:measurement(gdpr_be_t6, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 6, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(gdpr_su_t0, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(gdpr_su_t2, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 2, 0.7).
narrative_ontology:measurement(gdpr_su_t4, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 4, 0.75).
narrative_ontology:measurement(gdpr_su_t6, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 6, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gdpr_article_3_scope__effects_jurisdiction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(gdpr_article_3_scope__effects_jurisdiction_reading, gdpr_article_3_scope__market_access_reading).
narrative_ontology:affects_constraint(gdpr_article_3_scope__effects_jurisdiction_reading, gdpr_article_3_scope__territorial_sovereignty_reading).
narrative_ontology:affects_constraint(gdpr_article_3_scope__effects_jurisdiction_reading, eu_us_data_transfer_frameworks).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the GDPR Article 3 scope kernel. Each reading has a different structural interpretation and impact, necessitating separate constraint stories linked by network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
