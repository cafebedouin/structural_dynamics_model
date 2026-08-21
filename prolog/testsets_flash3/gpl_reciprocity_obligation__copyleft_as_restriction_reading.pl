% ============================================================================
% CONSTRAINT STORY: gpl_reciprocity_obligation__copyleft_as_restriction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_reciprocity_obligation__copyleft_as_restriction_reading, []).

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
 *   constraint_id: gpl_reciprocity_obligation__copyleft_as_restriction_reading
 *   human_readable: GPL Reciprocity Obligation (Copyleft as Restriction Reading)
 *   domain: software_licensing/intellectual_property/open_source_governance
 *
 * SUMMARY:
 *   This constraint story analyzes the GPL's reciprocity obligation from the
 *   perspective that it primarily functions as a restriction on business
 *   models, rather than a guarantor of freedom or a protector of the commons.
 *   It highlights how the 'viral' nature of the license, intended to ensure
 *   derivatives remain open, can be interpreted as a barrier to proprietary
 *   integration, paradoxically benefiting proprietary vendors who can fork
 *   without contributing back, while restricting those seeking hybrid models.
 *   This reading frames the GPL as a Snare, extracting flexibility and market
 *   access from developers and businesses, rather than a Rope for
 *   coordination or a Mountain of natural law.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0.65).
domain_priors:suppression_score(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0.7).
domain_priors:theater_ratio(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_reciprocity_obligation__copyleft_as_restriction_reading, snare).
narrative_ontology:human_readable(gpl_reciprocity_obligation__copyleft_as_restriction_reading, "GPL Reciprocity Obligation (Copyleft as Restriction Reading)").
narrative_ontology:topic_domain(gpl_reciprocity_obligation__copyleft_as_restriction_reading, "software_licensing/intellectual_property/open_source_governance").

domain_priors:requires_active_enforcement(gpl_reciprocity_obligation__copyleft_as_restriction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_reciprocity_obligation__copyleft_as_restriction_reading, '0f616c64-1d4d-4a67-a0f2-22b7e7b811d4').
narrative_ontology:cs_kernel_codification('0f616c64-1d4d-4a67-a0f2-22b7e7b811d4', fixed_text).
narrative_ontology:cs_authority_grounding('0f616c64-1d4d-4a67-a0f2-22b7e7b811d4', lineage).
narrative_ontology:cs_interpretation_layer_present('0f616c64-1d4d-4a67-a0f2-22b7e7b811d4').
narrative_ontology:cs_reading_relation('0f616c64-1d4d-4a67-a0f2-22b7e7b811d4', gpl_reciprocity_obligation__copyleft_as_freedom_reading, coexists_with).
narrative_ontology:cs_reading_relation('0f616c64-1d4d-4a67-a0f2-22b7e7b811d4', gpl_reciprocity_obligation__copyleft_as_commons_reading, coexists_with).
narrative_ontology:cs_axiom('0f616c64-1d4d-4a67-a0f2-22b7e7b811d4', foundational, proprietary_integration_is_a_legitimate_business_model).
narrative_ontology:cs_axiom_status(proprietary_integration_is_a_legitimate_business_model, holdable).
narrative_ontology:cs_axiom_grounding('0f616c64-1d4d-4a67-a0f2-22b7e7b811d4', proprietary_integration_is_a_legitimate_business_model, conventional).
narrative_ontology:cs_axiom('0f616c64-1d4d-4a67-a0f2-22b7e7b811d4', secondary, licensing_should_not_unduly_restrict_innovation).
narrative_ontology:cs_axiom_status(licensing_should_not_unduly_restrict_innovation, holdable).
narrative_ontology:cs_axiom_grounding('0f616c64-1d4d-4a67-a0f2-22b7e7b811d4', licensing_should_not_unduly_restrict_innovation, instrumental).
narrative_ontology:cs_reference_frame('0f616c64-1d4d-4a67-a0f2-22b7e7b811d4', open_source_as_enabler_of_diverse_business_models).
narrative_ontology:cs_drift_state('0f616c64-1d4d-4a67-a0f2-22b7e7b811d4', contemporary_software_industry, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0f616c64-1d4d-4a67-a0f2-22b7e7b811d4', '').
narrative_ontology:cs_kernel_id(gpl_reciprocity_obligation__copyleft_as_restriction_reading, gpl_reciprocity_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_restriction_reading, proprietary_software_vendors).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_restriction_reading, open_source_developers_seeking_proprietary_integration).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_restriction_reading, businesses_seeking_hybrid_models).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_restriction_reading, users_of_proprietary_software).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the GPL's viral nature by being able to fork GPL-licensed code into proprietary projects without contributing back, effectively enabling proprietary forks. They avoid the reciprocity obligation by treating GPL as a restriction that allows them to take without giving back to the commons.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, proprietary_software_vendors, beneficiary,
    powerful, biographical, mobile, global).

% Are constrained by the GPL's reciprocity obligation, which prohibits them from integrating GPL-licensed code into proprietary applications without making their entire derivative work GPL-licensed. This limits their business models and market reach.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, open_source_developers_seeking_proprietary_integration, payer,
    moderate, biographical, constrained, global).

% Find their ability to combine open-source components with proprietary extensions severely restricted by the GPL. This forces them into an 'all-in' open-source model or to avoid GPL-licensed components entirely, limiting their flexibility and innovation.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, businesses_seeking_hybrid_models, payer,
    organized, biographical, constrained, global).

% Actively enforce the GPL's terms, including the reciprocity obligation, through legal action and community pressure. From this reading, their enforcement primarily serves to restrict business models rather than promote freedom or the commons.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, gpl_enforcement_bodies, agenda_setter,
    institutional, generational, analytical, global).

% Benefit from proprietary vendors being able to fork GPL code into proprietary products, potentially leading to more diverse software options, even if those options are closed-source. They are largely unaware of the licensing implications.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, users_of_proprietary_software, beneficiary,
    powerless, immediate, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The GPL aims to coordinate software development around a principle of mandatory sharing, ensuring that improvements to open-source code remain open. However, this reading views it as failing to achieve genuine coordination for hybrid models.
% TRANSFER_FUNCTION: Transfers the right to create proprietary derivative works from open-source developers (who are restricted) to proprietary software vendors (who can fork without contributing back), effectively enabling proprietary forks and limiting the growth of the commons.
% ABSENT_VOICES: Developers and businesses who wish to create hybrid open-source/proprietary models are often marginalized in the discourse, as the dominant narratives focus on 'freedom' or 'commons' without acknowledging the business model restrictions.
% DISAPPEARANCE_RATIONALE: If the GPL's reciprocity obligation vanished, many proprietary vendors would integrate GPL-licensed code more freely, leading to a proliferation of hybrid software products. Open-source projects might see increased adoption in proprietary contexts, but the 'commons' as envisioned by other readings would likely diminish.
% FOUNDING_PROBLEM: The original problem was to prevent software from being privatized after being made open, ensuring that all derivative works remained free and accessible to users.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of other GPL readings (freedom, commons) argue the problem is still live. However, from this 'restriction' reading, the original problem has been subverted, and the GPL now primarily serves to restrict business models, a claim corroborated by numerous legal challenges and industry analyses from outside the direct beneficiaries of the GPL's enforcement.
narrative_ontology:disappearance_verdict(gpl_reciprocity_obligation__copyleft_as_restriction_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_reciprocity_obligation__copyleft_as_restriction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_reciprocity_obligation__copyleft_as_restriction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_reciprocity_obligation__copyleft_as_restriction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_restriction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gpl_reciprocity_obligation__copyleft_as_restriction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is high because the GPL's terms impose significant costs on developers and businesses wishing to integrate GPL-licensed code into proprietary products, forcing them to either abandon proprietary models or avoid GPL components. Suppression (0.70) is also high due to active legal enforcement and the lack of viable alternatives for integrating certain critical open-source components without adhering to the GPL's viral terms. The low theater ratio (0.10) indicates that the enforcement is genuinely aimed at upholding the license's terms, even if the outcome is seen as restrictive.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of proprietary software vendors, the GPL is a tool that allows them to take open-source code without contributing back, effectively enabling proprietary forks. From the perspective of developers seeking hybrid models, it is a restrictive barrier. The engine's per-seat classification will reflect this divergence, with proprietary vendors computing as beneficiaries and hybrid developers as targets.
 *
 * DIRECTIONALITY LOGIC:
 *   Proprietary software vendors are beneficiaries (d near 0.0) because they can leverage the GPL's 'restriction' to fork code into proprietary projects without reciprocity. Open-source developers seeking proprietary integration and businesses seeking hybrid models are victims/targets (d near 1.0) as they bear the costs of the reciprocity obligation. GPL enforcement bodies act as agenda-setters, upholding the terms that create this asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading suggests a form of mandatrophy where the GPL's original mandate (preventing privatization) has, from this perspective, morphed into a mechanism that restricts certain business models while inadvertently enabling others to benefit without contributing to the commons. The classification as a Snare prevents mislabeling this as pure coordination, highlighting the extractive nature of the restriction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    gpl_intent_vs_effect,
    'Does the GPL''s reciprocity obligation primarily achieve its stated goal of preserving user freedom and the software commons, or does it primarily restrict business models and enable proprietary forks?',
    'Empirical studies on the actual flow of contributions back to GPL projects from proprietary integrations, and analysis of market dynamics for hybrid software models.',
    'If the primary effect is restriction and proprietary benefit, the Snare classification is strengthened. If the primary effect is commons growth and freedom, the classification would shift towards Rope or Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(gpl_intent_vs_effect, empirical, 'Ambiguity between the GPL''s stated intent and its observed effects on different stakeholders.').

omega_variable(
    definition_of_freedom,
    'Is ''freedom'' in software primarily defined as the freedom to use, study, modify, and distribute (as per the Free Software Foundation), or does it also include the freedom to choose licensing models for derivative works, including proprietary ones?',
    'Conceptual analysis and philosophical debate within the open-source and free software communities, potentially leading to new licensing paradigms.',
    'A broader definition of freedom would weaken the justification for the GPL''s viral nature, potentially reclassifying it as more extractive. A narrower definition would reinforce its ''freedom-preserving'' aspect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definition_of_freedom, conceptual, 'The conceptual framing of ''freedom'' in software licensing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 1989, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(gpl__be_t1989, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 1989, 0.5).
narrative_ontology:measurement(gpl__be_t1999, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 1999, 0.58).
narrative_ontology:measurement(gpl__be_t2009, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 2009, 0.62).
narrative_ontology:measurement(gpl__be_t2019, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 2019, 0.64).
narrative_ontology:measurement(gpl__be_t2024, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t1989, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 1989, 0.55).
narrative_ontology:measurement(gpl__su_t1999, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 1999, 0.63).
narrative_ontology:measurement(gpl__su_t2009, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 2009, 0.68).
narrative_ontology:measurement(gpl__su_t2019, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 2019, 0.69).
narrative_ontology:measurement(gpl__su_t2024, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_reciprocity_obligation__copyleft_as_restriction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_restriction_reading, gpl_reciprocity_obligation__copyleft_as_freedom_reading).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_restriction_reading, gpl_reciprocity_obligation__copyleft_as_commons_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'GPL reciprocity obligation' kernel. This reading (copyleft_as_restriction_reading) focuses on the GPL's function as a barrier to proprietary integration, contrasting with readings that emphasize freedom or commons protection.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
