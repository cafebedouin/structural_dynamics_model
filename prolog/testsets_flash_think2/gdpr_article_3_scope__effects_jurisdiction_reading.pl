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
    narrative_ontology:epsilon_provenance/5,
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
 *   This constraint story analyzes the 'effects jurisdiction' reading of GDPR
 *   Article 3(2), which extends the regulation's scope extraterritorially to
 *   non-EU entities that target or monitor EU residents. This reading asserts
 *   that jurisdiction follows the effects of data processing on EU
 *   individuals, regardless of the processor's location. It is one reading of
 *   the broader 'gdpr_article_3_scope' kernel, distinct from
 *   'territorial_sovereignty_reading' and 'market_access_reading'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gdpr_article_3_scope__effects_jurisdiction_reading, 0.75).
domain_priors:suppression_score(gdpr_article_3_scope__effects_jurisdiction_reading, 0.8).
domain_priors:theater_ratio(gdpr_article_3_scope__effects_jurisdiction_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gdpr_article_3_scope__effects_jurisdiction_reading, tangled_rope).
narrative_ontology:human_readable(gdpr_article_3_scope__effects_jurisdiction_reading, "GDPR Article 3(2) Extraterritoriality (Effects Jurisdiction Reading)").
narrative_ontology:topic_domain(gdpr_article_3_scope__effects_jurisdiction_reading, "technology_governance/international_law/privacy_regulation").

domain_priors:requires_active_enforcement(gdpr_article_3_scope__effects_jurisdiction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gdpr_article_3_scope__effects_jurisdiction_reading, '96541e17-652d-4ee5-82ad-c55528eefa5b').
narrative_ontology:cs_kernel_codification('96541e17-652d-4ee5-82ad-c55528eefa5b', fixed_text).
narrative_ontology:cs_authority_grounding('96541e17-652d-4ee5-82ad-c55528eefa5b', lineage).
narrative_ontology:cs_interpretation_layer_present('96541e17-652d-4ee5-82ad-c55528eefa5b').
narrative_ontology:cs_reading_relation('96541e17-652d-4ee5-82ad-c55528eefa5b', gdpr_article_3_scope__territorial_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('96541e17-652d-4ee5-82ad-c55528eefa5b', gdpr_article_3_scope__market_access_reading, coexists_with).
narrative_ontology:cs_axiom('96541e17-652d-4ee5-82ad-c55528eefa5b', foundational, data_protection_is_fundamental_right).
narrative_ontology:cs_axiom_status(data_protection_is_fundamental_right, holdable).
narrative_ontology:cs_axiom_grounding('96541e17-652d-4ee5-82ad-c55528eefa5b', data_protection_is_fundamental_right, deontological).
narrative_ontology:cs_axiom('96541e17-652d-4ee5-82ad-c55528eefa5b', foundational, jurisdiction_follows_effects).
narrative_ontology:cs_axiom_status(jurisdiction_follows_effects, holdable).
narrative_ontology:cs_axiom_grounding('96541e17-652d-4ee5-82ad-c55528eefa5b', jurisdiction_follows_effects, conventional).
narrative_ontology:cs_reference_frame('96541e17-652d-4ee5-82ad-c55528eefa5b', eu_fundamental_rights_charter_framework).
narrative_ontology:cs_drift_state('96541e17-652d-4ee5-82ad-c55528eefa5b', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('96541e17-652d-4ee5-82ad-c55528eefa5b', '').
narrative_ontology:cs_kernel_id(gdpr_article_3_scope__effects_jurisdiction_reading, gdpr_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__effects_jurisdiction_reading, eu_data_subjects).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__effects_jurisdiction_reading, eu_data_protection_authorities).
narrative_ontology:constraint_victim(gdpr_article_3_scope__effects_jurisdiction_reading, non_eu_data_controllers).
narrative_ontology:constraint_victim(gdpr_article_3_scope__effects_jurisdiction_reading, non_eu_data_processors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and enforce GDPR Article 3(2), issuing guidance, investigating complaints, and imposing fines on non-EU entities that target or monitor EU residents. They are the primary agents of the EU's regulatory power.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, eu_data_protection_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Receive enhanced data protection and privacy rights, even when their data is processed by entities outside the EU. They benefit from the extended scope, but their ability to enforce these rights directly against distant entities is often constrained.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, eu_data_subjects, beneficiary,
    organized, biographical, constrained, regional).

% Bear significant compliance costs to adapt their data processing operations to GDPR standards if they target or monitor EU residents. Their options are to comply, cease operations involving EU residents, or face substantial fines and market exclusion.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, non_eu_data_controllers, payer,
    powerful, biographical, constrained, global).

% Also incur compliance costs and risks, often as subcontractors to non-EU data controllers. They have less leverage than controllers and are highly dependent on their clients' GDPR compliance strategies.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, non_eu_data_processors, payer,
    moderate, biographical, constrained, global).

% Often object to the extraterritorial reach of GDPR as an infringement on their national sovereignty or a burden on their domestic businesses. They are largely excluded from the direct enforcement mechanism but engage in diplomatic and trade negotiations.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, non_eu_governments, excluded,
    institutional, generational, constrained, national).

% Analyze the legal implications of GDPR's extraterritoriality, debating its legitimacy under public international law and its impact on global regulatory frameworks. They provide critical commentary but do not directly participate in enforcement or compliance.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, international_law_scholars, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a common, high standard for data protection for EU residents, regardless of where their data is processed, thereby coordinating global data governance around EU norms.
% TRANSFER_FUNCTION: Transfers compliance costs, legal obligations, and data governance standards from the EU legal framework to non-EU data controllers and processors, in exchange for access to the EU market or the ability to interact with EU residents.
% ABSENT_VOICES: Non-EU governments and businesses that advocate for strict territoriality in jurisdiction and oppose the 'effects doctrine' as regulatory overreach. They are not directly involved in the EU's internal legal interpretation or enforcement processes.
% DISAPPEARANCE_RATIONALE: If GDPR Article 3(2) vanished, non-EU entities would likely revert to their local data protection standards, significantly reducing protections for EU data subjects interacting with them. The EU's global regulatory influence would diminish, and the digital economy would fragment further on data privacy standards.
% FOUNDING_PROBLEM: Fragmented and insufficient data protection for EU citizens in a globalized digital economy, where personal data could be processed anywhere in the world without adequate safeguards.
% FOUNDING_PROBLEM_CORROBORATION: EU institutions, data protection advocates, and many legal scholars outside of those directly benefiting from the enforcement attest that the problem of global data protection remains live and requires robust extraterritorial application.
narrative_ontology:disappearance_verdict(gdpr_article_3_scope__effects_jurisdiction_reading, world_rearranges).
narrative_ontology:founding_problem_status(gdpr_article_3_scope__effects_jurisdiction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gdpr_article_3_scope__effects_jurisdiction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(gdpr_article_3_scope__effects_jurisdiction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gdpr_article_3_scope__effects_jurisdiction_reading, 0.75, 'gemini-2.5-flash', 'none', direct).

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
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates data protection for EU citizens (beneficiaries) while simultaneously imposing significant, often asymmetric, compliance costs and risks on non-EU entities (victims). Extractiveness is high due to the substantial investment required for compliance and the potential for large fines. Suppression is also high, as non-EU entities have limited options other than compliance or withdrawal from interactions with EU residents. The theater ratio is low, indicating that enforcement is active and effective, not merely performative.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of EU data subjects and authorities, this constraint is a necessary and legitimate extension of fundamental rights in a globalized world. From the perspective of many non-EU entities and governments, it represents an overreach of EU law, imposing disproportionate burdens and challenging traditional notions of territorial sovereignty. The engine's classification captures this structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   EU data subjects and data protection authorities are clear beneficiaries, gaining enhanced rights and regulatory power. Non-EU data controllers and processors are the primary targets, bearing the costs of compliance and the risk of enforcement. Non-EU governments are largely excluded from the direct regulatory process, experiencing the constraint as an imposition.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extraterritorial_legitimacy_ambiguity,
    'Is the ''effects doctrine'' as applied by GDPR Article 3(2) a legitimate exercise of extraterritorial jurisdiction under international law, or does it constitute regulatory overreach?',
    'Consensus development in international legal scholarship and state practice, or a definitive ruling by an international tribunal with relevant jurisdiction.',
    'If deemed illegitimate, the constraint''s suppression and extractiveness could be challenged more effectively by non-EU states, potentially weakening its enforcement. If affirmed, its legitimacy would be solidified, increasing compliance pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraterritorial_legitimacy_ambiguity, conceptual, 'Debate over the legal basis for GDPR''s extraterritorial scope.').

omega_variable(
    enforcement_effectiveness_ambiguity,
    'How effective is the enforcement of GDPR Article 3(2) against non-EU entities, particularly those without a physical presence or assets within the EU?',
    'Empirical studies tracking enforcement actions, fine collection rates, and changes in compliance behavior among non-EU entities over time.',
    'If enforcement is found to be consistently weak, the constraint''s effective suppression and extractiveness would be lower than currently assessed, potentially shifting its classification towards a Piton or a weaker Tangled Rope for some seats. Strong enforcement would confirm current metrics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_effectiveness_ambiguity, empirical, 'Uncertainty regarding the practical reach and impact of extraterritorial enforcement.').

omega_variable(
    compliance_cost_proportionality,
    'Are the compliance costs imposed on non-EU entities by GDPR Article 3(2) proportionate to the benefits gained by EU data subjects and the severity of the risks addressed?',
    'Independent economic impact assessments comparing compliance expenditures with quantified privacy benefits and risk reduction, potentially informing legislative review or judicial proportionality tests.',
    'If costs are found to be disproportionate, it could strengthen arguments for regulatory reform, potentially reducing the constraint''s extractiveness or leading to differentiated compliance requirements. If proportionate, it would reinforce the current structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(compliance_cost_proportionality, preference, 'Whether the burden of compliance is justified by the privacy benefits.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gdpr_article_3_scope__effects_jurisdiction_reading, 2018, 2048).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gdpr_tr_t2018, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 2018, 0.1).
narrative_ontology:measurement(gdpr_tr_t2023, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 2023, 0.1).
narrative_ontology:measurement(gdpr_tr_t2028, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 2028, 0.1).
narrative_ontology:measurement(gdpr_tr_t2033, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 2033, 0.1).
narrative_ontology:measurement(gdpr_tr_t2038, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 2038, 0.1).
narrative_ontology:measurement(gdpr_tr_t2048, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 2048, 0.1).

% Extraction over time
narrative_ontology:measurement(gdpr_be_t2018, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 2018, 0.65).
narrative_ontology:measurement(gdpr_be_t2023, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 2023, 0.69).
narrative_ontology:measurement(gdpr_be_t2028, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 2028, 0.72).
narrative_ontology:measurement(gdpr_be_t2033, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 2033, 0.74).
narrative_ontology:measurement(gdpr_be_t2038, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 2038, 0.75).
narrative_ontology:measurement(gdpr_be_t2048, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 2048, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(gdpr_su_t2018, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 2018, 0.7).
narrative_ontology:measurement(gdpr_su_t2023, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 2023, 0.74).
narrative_ontology:measurement(gdpr_su_t2028, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 2028, 0.77).
narrative_ontology:measurement(gdpr_su_t2033, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 2033, 0.79).
narrative_ontology:measurement(gdpr_su_t2038, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 2038, 0.8).
narrative_ontology:measurement(gdpr_su_t2048, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 2048, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gdpr_article_3_scope__effects_jurisdiction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(gdpr_article_3_scope__effects_jurisdiction_reading, gdpr_adequacy_decisions).
narrative_ontology:affects_constraint(gdpr_article_3_scope__effects_jurisdiction_reading, eu_us_data_transfer_frameworks).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
