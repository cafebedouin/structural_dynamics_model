% ============================================================================
% CONSTRAINT STORY: gdpr_article_3_scope__territorial_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gdpr_article_3_scope__territorial_sovereignty_reading, []).

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
 *   constraint_id: gdpr_article_3_scope__territorial_sovereignty_reading
 *   human_readable: GDPR Article 3 Scope: Territorial Sovereignty Reading
 *   domain: technology_governance/international_law/privacy_regulation
 *
 * SUMMARY:
 *   This constraint story instantiates the 'territorial sovereignty' reading
 *   of GDPR Article 3's scope. In this reading, the application of GDPR is
 *   strictly bounded by the physical territory of the EU, and any
 *   extraterritorial assertion is viewed as exceeding legitimate regulatory
 *   authority. This interpretation prioritizes the sovereignty of non-EU
 *   states and limits the reach of EU data protection law, leading to data
 *   localization as a resistance mechanism and potential jurisdictional
 *   conflicts.
 *
 * KEY AGENTS:
 *   - non_eu_states: Primary beneficiary (institutional/mobile) — asserts regulatory independence.
 *   - non_eu_data_processors: Secondary beneficiary (organized/constrained) — reduces compliance burden.
 *   - eu_regulators: Primary target (institutional/constrained) — faces enforcement limitations.
 *   - eu_citizens_seeking_protection: Secondary target (powerless/trapped) — may lose protection.
 *   - international_law_scholars: Analytical observer (analytical/analytical) — analyzes legal tensions.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gdpr_article_3_scope__territorial_sovereignty_reading, 0.45).
domain_priors:suppression_score(gdpr_article_3_scope__territorial_sovereignty_reading, 0.3).
domain_priors:theater_ratio(gdpr_article_3_scope__territorial_sovereignty_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gdpr_article_3_scope__territorial_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(gdpr_article_3_scope__territorial_sovereignty_reading, "GDPR Article 3 Scope: Territorial Sovereignty Reading").
narrative_ontology:topic_domain(gdpr_article_3_scope__territorial_sovereignty_reading, "technology_governance/international_law/privacy_regulation").

domain_priors:requires_active_enforcement(gdpr_article_3_scope__territorial_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gdpr_article_3_scope__territorial_sovereignty_reading, 'd26be5ed-c51b-4b63-bf8e-05741ce2d4b8').
narrative_ontology:cs_kernel_codification('d26be5ed-c51b-4b63-bf8e-05741ce2d4b8', fixed_text).
narrative_ontology:cs_authority_grounding('d26be5ed-c51b-4b63-bf8e-05741ce2d4b8', lineage).
narrative_ontology:cs_interpretation_layer_present('d26be5ed-c51b-4b63-bf8e-05741ce2d4b8').
narrative_ontology:cs_reading_relation('d26be5ed-c51b-4b63-bf8e-05741ce2d4b8', gdpr_article_3_scope__effects_jurisdiction_reading, coexists_with).
narrative_ontology:cs_reading_relation('d26be5ed-c51b-4b63-bf8e-05741ce2d4b8', gdpr_article_3_scope__market_access_reading, coexists_with).
narrative_ontology:cs_axiom('d26be5ed-c51b-4b63-bf8e-05741ce2d4b8', foundational, territorial_sovereignty_primacy).
narrative_ontology:cs_axiom_status(territorial_sovereignty_primacy, holdable).
narrative_ontology:cs_axiom_grounding('d26be5ed-c51b-4b63-bf8e-05741ce2d4b8', territorial_sovereignty_primacy, deontological).
narrative_ontology:cs_axiom('d26be5ed-c51b-4b63-bf8e-05741ce2d4b8', secondary, extraterritoriality_requires_explicit_consent).
narrative_ontology:cs_axiom_status(extraterritoriality_requires_explicit_consent, holdable).
narrative_ontology:cs_axiom_grounding('d26be5ed-c51b-4b63-bf8e-05741ce2d4b8', extraterritoriality_requires_explicit_consent, conventional).
narrative_ontology:cs_reference_frame('d26be5ed-c51b-4b63-bf8e-05741ce2d4b8', westphalian_jurisdictional_model).
narrative_ontology:cs_drift_state('d26be5ed-c51b-4b63-bf8e-05741ce2d4b8', contemporary_digital_economy, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('d26be5ed-c51b-4b63-bf8e-05741ce2d4b8', '').
narrative_ontology:cs_kernel_id(gdpr_article_3_scope__territorial_sovereignty_reading, gdpr_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_states).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_data_processors).
narrative_ontology:constraint_victim(gdpr_article_3_scope__territorial_sovereignty_reading, eu_regulators).
narrative_ontology:constraint_victim(gdpr_article_3_scope__territorial_sovereignty_reading, eu_citizens_seeking_protection).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Assert their sovereign right to regulate data within their borders without interference from EU law. They benefit from the limitation of GDPR's reach, preserving their regulatory independence and potentially fostering local data industries.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_states, beneficiary,
    institutional, generational, mobile, global).

% Seek to limit the extraterritorial burden of GDPR, arguing that compliance with EU standards for non-EU operations is overly costly and complex. They benefit from a narrower interpretation of Article 3, reducing their compliance obligations.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_data_processors, beneficiary,
    organized, biographical, constrained, global).

% Face challenges in enforcing GDPR against entities outside the EU that do not acknowledge its extraterritorial reach. This reading imposes a cost by limiting their effective jurisdiction and requiring complex international cooperation or diplomatic pressure.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, eu_regulators, payer,
    institutional, generational, constrained, continental).

% May find their data rights unprotected when processed by non-EU entities that successfully assert a territorial sovereignty defense. This reading limits their ability to seek redress under GDPR for data processed outside the EU.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, eu_citizens_seeking_protection, payer,
    powerless, biographical, trapped, continental).

% Analyze the tension between traditional territorial jurisdiction and modern data flows, evaluating the legal validity and practical implications of different GDPR interpretations. They provide academic commentary and influence legal discourse.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, international_law_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the application of national laws to data processing by asserting the primacy of territorial jurisdiction, aiming to prevent regulatory overreach and conflicts of law in the digital sphere.
% TRANSFER_FUNCTION: Transfers regulatory authority and enforcement capacity from the EU to non-EU states for data processing activities occurring outside EU borders, even if they involve EU citizens' data.
% ABSENT_VOICES: Advocates for a strong 'Brussels Effect' and universal data protection standards, who would argue that data flows necessitate a broader jurisdictional approach to protect fundamental rights, are marginalized in this reading's framework.
% DISAPPEARANCE_RATIONALE: If this reading vanished, EU regulators would face fewer challenges to extraterritorial enforcement, non-EU entities would likely increase GDPR compliance, and the global regulatory landscape for data would shift towards broader jurisdictional claims, potentially leading to more 'Brussels Effect' outcomes.
% FOUNDING_PROBLEM: The problem of conflicting national laws and regulatory overreach in an interconnected world, where states seek to assert control over activities within their borders without infringing on the sovereignty of others.
% FOUNDING_PROBLEM_CORROBORATION: International law bodies and legal scholars outside the EU regulatory framework consistently attest to the ongoing relevance of territorial sovereignty in international law, even amidst debates about its application to digital phenomena. Non-EU governments frequently invoke this principle in diplomatic and legal disputes.
narrative_ontology:disappearance_verdict(gdpr_article_3_scope__territorial_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(gdpr_article_3_scope__territorial_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gdpr_article_3_scope__territorial_sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(gdpr_article_3_scope__territorial_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gdpr_article_3_scope__territorial_sovereignty_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gdpr_article_3_scope__territorial_sovereignty_reading_tests).
:- end_tests(gdpr_article_3_scope__territorial_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate, as it extracts regulatory authority from EU regulators and protection from EU citizens, while benefiting non-EU entities. Suppression (0.30) is also moderate, as it relies on the assertion of sovereign rights and the practical difficulties of extraterritorial enforcement, rather than direct coercion. The theater ratio is low (0.10) because the assertion of territorial sovereignty is a genuine legal and political stance, not primarily performative. Resistance (0.70) is high, as EU regulators actively push back against this narrow interpretation.
 *
 * PERSPECTIVAL GAP:
 *   Non-EU states and data processors experience this as a legitimate defense of sovereignty and a reduction of burdensome regulation, computing it closer to a Rope or even a Mountain. EU regulators and citizens, however, experience it as a Snare, as it limits their ability to enforce data protection and leaves citizens vulnerable to data processing outside the EU's direct control.
 *
 * DIRECTIONALITY LOGIC:
 *   Non-EU states and data processors are beneficiaries (low d) as this reading aligns with their interests in limiting EU regulatory reach. EU regulators and citizens are targets (high d) as their ability to enforce and benefit from GDPR is curtailed. International law scholars are analytical observers (d=0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling a genuine assertion of state sovereignty as pure extraction. While it has extractive consequences for EU interests, its persistence is rooted in a foundational principle of international law, not solely in rent-seeking. The classification as Tangled Rope acknowledges both the coordination function (preventing regulatory overreach) and the asymmetric extraction (limiting EU regulatory power).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_vs_digital_reality,
    'To what extent can traditional territorial sovereignty principles legitimately apply to borderless digital data flows without creating regulatory vacuums?',
    'Development of new international legal frameworks or widely accepted customary international law that specifically addresses digital jurisdiction.',
    'If traditional sovereignty is deemed insufficient for digital realities, this reading''s legitimacy would erode, potentially leading to broader acceptance of extraterritorial claims. If it remains robust, this reading gains strength.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_vs_digital_reality, conceptual, 'The conceptual tension between traditional international law and digital governance.').

omega_variable(
    data_localization_effectiveness,
    'How effective is data localization as a mechanism for non-EU states to resist GDPR''s extraterritorial application and protect their regulatory independence?',
    'Empirical studies on the economic costs and legal enforceability of data localization requirements in various jurisdictions, and their impact on GDPR compliance.',
    'If data localization proves highly effective and economically viable, this reading gains practical force. If it''s costly and easily circumvented, the reading''s practical impact diminishes, even if its legal premise remains.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(data_localization_effectiveness, empirical, 'The practical efficacy of data localization as a resistance mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gdpr_article_3_scope__territorial_sovereignty_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gdpr_tr_t0, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(gdpr_tr_t5, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement(gdpr_tr_t10, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 10, 0.1).

% Extraction over time
narrative_ontology:measurement(gdpr_be_t0, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(gdpr_be_t5, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 5, 0.43).
narrative_ontology:measurement(gdpr_be_t10, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 10, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(gdpr_su_t0, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(gdpr_su_t5, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 5, 0.28).
narrative_ontology:measurement(gdpr_su_t10, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 10, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gdpr_article_3_scope__territorial_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(gdpr_article_3_scope__territorial_sovereignty_reading, gdpr_article_3_scope__effects_jurisdiction_reading).
narrative_ontology:affects_constraint(gdpr_article_3_scope__territorial_sovereignty_reading, gdpr_article_3_scope__market_access_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'gdpr_article_3_scope' kernel, each representing a distinct interpretation of GDPR's jurisdictional reach. This reading emphasizes territorial sovereignty, while others focus on effects or market access.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
