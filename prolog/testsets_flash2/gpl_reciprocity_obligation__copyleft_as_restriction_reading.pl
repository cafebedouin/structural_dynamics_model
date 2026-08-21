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
 *   perspective that it restricts business models by prohibiting proprietary
 *   integration. This 'copyleft as restriction' reading views the GPL as a
 *   snare, extracting value from those who wish to combine open-source and
 *   proprietary code by forcing an all-or-nothing licensing choice. The
 *   constraint's persistence relies on active enforcement of its viral
 *   clauses, which suppresses alternative hybrid business models. The claimed
 *   type is 'snare' because the primary effect, from this perspective, is
 *   extraction from those seeking to integrate GPL code into proprietary
 *   systems, with the coordination story (preserving the commons) serving as
 *   cover for this restriction.
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
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_reciprocity_obligation__copyleft_as_restriction_reading, snare).
narrative_ontology:human_readable(gpl_reciprocity_obligation__copyleft_as_restriction_reading, "GPL Reciprocity Obligation (Copyleft as Restriction Reading)").
narrative_ontology:topic_domain(gpl_reciprocity_obligation__copyleft_as_restriction_reading, "software_licensing/intellectual_property/open_source_governance").

domain_priors:requires_active_enforcement(gpl_reciprocity_obligation__copyleft_as_restriction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_reciprocity_obligation__copyleft_as_restriction_reading, '42896f18-f234-4905-91fd-59843068c3d8').
narrative_ontology:cs_kernel_codification('42896f18-f234-4905-91fd-59843068c3d8', fixed_text).
narrative_ontology:cs_authority_grounding('42896f18-f234-4905-91fd-59843068c3d8', lineage).
narrative_ontology:cs_interpretation_layer_present('42896f18-f234-4905-91fd-59843068c3d8').
narrative_ontology:cs_reading_relation('42896f18-f234-4905-91fd-59843068c3d8', gpl_reciprocity_obligation__copyleft_as_freedom_reading, coexists_with).
narrative_ontology:cs_reading_relation('42896f18-f234-4905-91fd-59843068c3d8', gpl_reciprocity_obligation__copyleft_as_commons_reading, coexists_with).
narrative_ontology:cs_axiom('42896f18-f234-4905-91fd-59843068c3d8', foundational, proprietary_integration_is_a_fundamental_business_right).
narrative_ontology:cs_axiom_status(proprietary_integration_is_a_fundamental_business_right, holdable).
narrative_ontology:cs_axiom_grounding('42896f18-f234-4905-91fd-59843068c3d8', proprietary_integration_is_a_fundamental_business_right, deontological).
narrative_ontology:cs_axiom('42896f18-f234-4905-91fd-59843068c3d8', foundational, copyleft_hinders_innovation_in_hybrid_systems).
narrative_ontology:cs_axiom_status(copyleft_hinders_innovation_in_hybrid_systems, holdable).
narrative_ontology:cs_axiom_grounding('42896f18-f234-4905-91fd-59843068c3d8', copyleft_hinders_innovation_in_hybrid_systems, empirically_contingent).
narrative_ontology:cs_reference_frame('42896f18-f234-4905-91fd-59843068c3d8', unrestricted_software_commercialization).
narrative_ontology:cs_drift_state('42896f18-f234-4905-91fd-59843068c3d8', contemporary_open_source_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('42896f18-f234-4905-91fd-59843068c3d8', '').
narrative_ontology:cs_kernel_id(gpl_reciprocity_obligation__copyleft_as_restriction_reading, gpl_reciprocity_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_restriction_reading, proprietary_software_vendors).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_restriction_reading, open_source_contributors_seeking_proprietary_integration).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_restriction_reading, businesses_seeking_hybrid_models).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the GPL's viral nature by being able to fork GPL-licensed code into proprietary projects without contributing back, as long as they don't distribute the modified GPL code. This reading sees the GPL as a barrier to entry for hybrid models, thus protecting proprietary market share.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, proprietary_software_vendors, beneficiary,
    powerful, biographical, mobile, global).

% Faces significant restrictions on integrating GPL-licensed code into proprietary products or services without 'virally' licensing their entire codebase. This limits their business models and potential for commercialization, forcing them to choose between full open source or full proprietary.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, open_source_contributors_seeking_proprietary_integration, payer,
    moderate, biographical, constrained, global).

% Finds their ability to combine open-source components with proprietary extensions severely curtailed by the GPL's reciprocity clause. This reading views the GPL as an obstacle to innovation in mixed-source environments, forcing a binary choice.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, businesses_seeking_hybrid_models, payer,
    organized, biographical, constrained, global).

% Would argue that the GPL is a tool for freedom, not restriction, ensuring users' rights to run, study, share, and modify software. This reading is excluded from the 'restriction' framing, as it fundamentally reinterprets the constraint's purpose.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, gpl_advocates_freedom_reading, excluded,
    institutional, generational, identity_locked, global).

% Would argue that the GPL is an institutional technology to prevent enclosure of the digital commons. This reading is excluded from the 'restriction' framing, as it fundamentally reinterprets the constraint's purpose.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, gpl_advocates_commons_reading, excluded,
    institutional, generational, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: It coordinates the licensing terms for software distribution, ensuring that certain codebases remain open and modifiable, preventing their proprietary enclosure.
% TRANSFER_FUNCTION: It transfers the obligation to share modifications under the same license from the original licensor to any downstream distributor of the modified software. This reading frames it as a transfer of control from potential proprietary integrators to the original open-source community, which is seen as a restriction on business models.
% ABSENT_VOICES: Advocates of the 'copyleft as freedom' and 'copyleft as commons' readings are absent from this framing, as their core interpretation of the GPL's intent and effect is fundamentally different. They would argue that the 'restriction' is a feature, not a bug, designed to protect user rights and the digital commons.
% DISAPPEARANCE_RATIONALE: If the GPL's reciprocity obligation vanished, many open-source projects would likely be integrated into proprietary products without contributing back, leading to a significant shift in the open-source ecosystem and business models. The 'commons' would be more easily enclosed.
% FOUNDING_PROBLEM: The problem of proprietary software vendors taking open-source code, modifying it, and then distributing the modified version as proprietary software, thereby enclosing the 'commons' of shared code and limiting user freedoms.
% FOUNDING_PROBLEM_CORROBORATION: The Free Software Foundation and various open-source communities attest that the problem of proprietary enclosure remains live. However, businesses seeking hybrid models and proprietary vendors would argue that the 'problem' is overblown or that the GPL creates new problems for innovation.
narrative_ontology:disappearance_verdict(gpl_reciprocity_obligation__copyleft_as_restriction_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_reciprocity_obligation__copyleft_as_restriction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_reciprocity_obligation__copyleft_as_restriction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.65) is high because the GPL's viral nature imposes significant costs on businesses that want to use GPL-licensed components in proprietary products, forcing them to either abandon the component or open-source their entire product. Suppression (0.70) is also high, as the legal framework and community enforcement actively prevent proprietary integration of GPL code. The accessibility collapse (0.40) is moderate, as alternatives (non-copyleft licenses, proprietary development) exist but come with their own costs. Resistance (0.75) is high, evidenced by ongoing legal challenges, debates, and the development of less restrictive licenses. Theater ratio (0.10) is low, as the enforcement is generally direct and functional, not performative.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap is between those who see copyleft as a restriction on commercial freedom and those who see it as a mechanism for user freedom or commons preservation. This reading emphasizes the former, leading to a 'snare' classification, while other readings would likely classify it as a 'rope' or 'mountain' of digital rights.
 *
 * DIRECTIONALITY LOGIC:
 *   Proprietary software vendors are beneficiaries in this reading, as the GPL's restrictions on hybrid models can inadvertently protect their market share from open-source competition. Open-source contributors seeking proprietary integration and businesses seeking hybrid models are victims, as they bear the direct costs of the GPL's viral clauses. GPL advocates (freedom and commons readings) are excluded from this framing, as their perspective fundamentally reinterprets the constraint's purpose.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    gpl_intent_ambiguity,
    'Is the GPL''s primary intent to restrict proprietary integration (as this reading claims), or to ensure user freedom and commons preservation (as sibling readings claim)?',
    'Analysis of founding documents, historical statements by creators, and legal precedents. However, intent is often contested and may not be fully resolvable.',
    'If the primary intent is user freedom/commons, the constraint would be reclassified towards a Rope or Mountain, as the ''extraction'' would be reinterpreted as a necessary cost of coordination or a natural law of digital rights. If restriction is confirmed, the Snare classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(gpl_intent_ambiguity, conceptual, 'Ambiguity regarding the foundational intent of the GPL''s reciprocity clause.').

omega_variable(
    market_impact_of_hybrid_models,
    'What is the actual economic impact of prohibiting proprietary integration on innovation and market competition, compared to the benefits of maintaining an open software commons?',
    'Empirical economic studies comparing innovation rates and market structures in ecosystems with strong copyleft vs. permissive licensing, accounting for other variables.',
    'If the prohibition significantly stifles innovation, the Snare classification is strengthened. If the open commons demonstrably drives more innovation overall, the ''restriction'' aspect might be reinterpreted as a necessary trade-off for a greater good, potentially shifting towards a Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_impact_of_hybrid_models, empirical, 'Empirical impact of copyleft on innovation and market dynamics.').


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
narrative_ontology:measurement(gpl__su_t1989, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 1989, 0.6).
narrative_ontology:measurement(gpl__su_t1999, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 1999, 0.65).
narrative_ontology:measurement(gpl__su_t2009, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 2009, 0.68).
narrative_ontology:measurement(gpl__su_t2019, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 2019, 0.69).
narrative_ontology:measurement(gpl__su_t2024, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_reciprocity_obligation__copyleft_as_restriction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_restriction_reading, open_source_business_models).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_restriction_reading, proprietary_software_development).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'gpl_reciprocity_obligation' kernel. This 'copyleft_as_restriction_reading' focuses on the constraint's impact on business models, while 'copyleft_as_freedom_reading' and 'copyleft_as_commons_reading' offer alternative interpretations of its purpose and effect.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
