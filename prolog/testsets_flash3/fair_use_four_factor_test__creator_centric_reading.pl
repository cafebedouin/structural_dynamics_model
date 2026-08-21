% ============================================================================
% CONSTRAINT STORY: fair_use_four_factor_test__creator_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fair_use_four_factor_test__creator_centric_reading, []).

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
 *   constraint_id: fair_use_four_factor_test__creator_centric_reading
 *   human_readable: Fair Use Four-Factor Test (Creator-Centric Reading)
 *   domain: legal/intellectual_property/cultural_production
 *
 * SUMMARY:
 *   This constraint represents the 'creator-centric' reading of the fair use
 *   four-factor test, where fair use is interpreted as a narrow exception to
 *   copyright holders' property rights. The four factors (purpose and
 *   character of the use, nature of the copyrighted work, amount and
 *   substantiality of the portion used, and effect of the use upon the
 *   potential market for or value of the copyrighted work) are weighed
 *   primarily to preserve creator incentives and minimize market harm to the
 *   original work. This reading results in high extraction from unauthorized
 *   uses and a chilling effect on derivative works, with copyright holders as
 *   primary beneficiaries.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_four_factor_test__creator_centric_reading, 0.7).
domain_priors:suppression_score(fair_use_four_factor_test__creator_centric_reading, 0.65).
domain_priors:theater_ratio(fair_use_four_factor_test__creator_centric_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_four_factor_test__creator_centric_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_four_factor_test__creator_centric_reading, "Fair Use Four-Factor Test (Creator-Centric Reading)").
narrative_ontology:topic_domain(fair_use_four_factor_test__creator_centric_reading, "legal/intellectual_property/cultural_production").

domain_priors:requires_active_enforcement(fair_use_four_factor_test__creator_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_four_factor_test__creator_centric_reading, 'c33e6aab-605f-4c10-933f-012dcedf44f5').
narrative_ontology:cs_kernel_codification('c33e6aab-605f-4c10-933f-012dcedf44f5', fixed_text).
narrative_ontology:cs_authority_grounding('c33e6aab-605f-4c10-933f-012dcedf44f5', lineage).
narrative_ontology:cs_interpretation_layer_present('c33e6aab-605f-4c10-933f-012dcedf44f5').
narrative_ontology:cs_reading_relation('c33e6aab-605f-4c10-933f-012dcedf44f5', fair_use_four_factor_test__user_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('c33e6aab-605f-4c10-933f-012dcedf44f5', fair_use_four_factor_test__transformative_use_reading, coexists_with).
narrative_ontology:cs_axiom('c33e6aab-605f-4c10-933f-012dcedf44f5', foundational, copyright_as_property_right).
narrative_ontology:cs_axiom_status(copyright_as_property_right, holdable).
narrative_ontology:cs_axiom_grounding('c33e6aab-605f-4c10-933f-012dcedf44f5', copyright_as_property_right, deontological).
narrative_ontology:cs_axiom('c33e6aab-605f-4c10-933f-012dcedf44f5', foundational, creator_incentives_primary).
narrative_ontology:cs_axiom_status(creator_incentives_primary, holdable).
narrative_ontology:cs_axiom_grounding('c33e6aab-605f-4c10-933f-012dcedf44f5', creator_incentives_primary, instrumental).
narrative_ontology:cs_reference_frame('c33e6aab-605f-4c10-933f-012dcedf44f5', exclusive_rights_framework).
narrative_ontology:cs_drift_state('c33e6aab-605f-4c10-933f-012dcedf44f5', contemporary_digital_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('c33e6aab-605f-4c10-933f-012dcedf44f5', '').
narrative_ontology:cs_kernel_id(fair_use_four_factor_test__creator_centric_reading, fair_use_four_factor_test).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__creator_centric_reading, copyright_holders).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__creator_centric_reading, content_industries).
narrative_ontology:constraint_victim(fair_use_four_factor_test__creator_centric_reading, transformative_users).
narrative_ontology:constraint_victim(fair_use_four_factor_test__creator_centric_reading, independent_creators).
narrative_ontology:constraint_victim(fair_use_four_factor_test__creator_centric_reading, public_domain_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively enforce their copyrights, viewing fair use as a narrow, exceptional defense. They benefit from high barriers to unauthorized use, preserving their exclusive rights and revenue streams. They initiate litigation to defend their property.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, copyright_holders, agenda_setter,
    institutional, generational, mobile, global).

% Benefit from a legal framework that prioritizes creator incentives and property rights, which supports their business models based on licensing and exclusive distribution. They lobby for stricter interpretations of fair use.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, content_industries, beneficiary,
    organized, biographical, mobile, global).

% Bear the risk and cost of potential infringement lawsuits when creating new works that build upon existing copyrighted material. Their ability to innovate is constrained by the threat of litigation and the high cost of legal defense.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, transformative_users, payer,
    moderate, immediate, constrained, national).

% Often lack the resources to defend against copyright claims, leading to self-censorship or abandonment of projects that might otherwise be considered fair use. They pay through lost opportunities and creative chilling effects.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, independent_creators, payer,
    powerless, biographical, constrained, local).

% Argue for a broader interpretation of fair use to enrich the public domain and foster cultural production. Their arguments are often marginalized in a system that prioritizes private property rights over public access.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, public_domain_advocates, excluded,
    organized, generational, constrained, global).

% Interpret and apply the four-factor test, often leaning towards protecting copyright holders' exclusive rights in this reading. Their decisions shape the boundaries of fair use and influence subsequent creative and legal behavior.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, courts, agenda_setter,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the balance between creator incentives (through exclusive rights) and public access (through limited exceptions), aiming to foster overall cultural production by ensuring creators are rewarded.
% TRANSFER_FUNCTION: Transfers potential revenue and control over derivative works from transformative users and the public domain back to original copyright holders, in exchange for the incentive to create original works.
% ABSENT_VOICES: Users and independent creators who would advocate for a more expansive view of fair use as an affirmative right are often absent from the legislative and judicial processes that shape its interpretation, or their voices are subordinated to those of established copyright holders.
% DISAPPEARANCE_RATIONALE: If the creator-centric reading of fair use vanished, copyright holders would lose a significant enforcement mechanism, leading to a surge in unauthorized uses. The incentive structure for original creation would be severely disrupted, and the market for derivative works would become chaotic, forcing a complete reorganization of intellectual property law and cultural production practices.
% FOUNDING_PROBLEM: To balance the exclusive rights of creators with the public's interest in accessing and building upon existing works, thereby promoting the progress of science and useful arts.
% FOUNDING_PROBLEM_CORROBORATION: Copyright holders and content industries assert the problem is live, citing ongoing piracy and the need for strong incentives. Transformative users and public domain advocates argue the problem has shifted, with current interpretations stifling creativity and public access, corroborated by academic legal scholars and independent artist communities.
narrative_ontology:disappearance_verdict(fair_use_four_factor_test__creator_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_four_factor_test__creator_centric_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_four_factor_test__creator_centric_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(fair_use_four_factor_test__creator_centric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_four_factor_test__creator_centric_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_four_factor_test__creator_centric_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fair_use_four_factor_test__creator_centric_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fair_use_four_factor_test__creator_centric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.7) reflects the significant costs and risks borne by users who wish to build on existing works, and the revenue streams preserved for copyright holders. Suppression (0.65) is moderate, as legal challenges and the threat of litigation actively deter many potential fair uses. The theater ratio (0.2) is low, indicating that the legal framework is genuinely functional in enforcing property rights, though some performative aspects exist in the rhetoric of 'protecting creativity' that may mask rent-seeking. Accessibility collapse (0.4) is moderate, as alternatives (e.g., licensing, creating entirely original works) exist but are often costly or creatively limiting. Resistance (0.5) is also moderate, with ongoing legal challenges and advocacy for broader fair use interpretations.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of copyright holders, this is a necessary 'rope' for coordinating creative incentives and ensuring a robust market. From the perspective of transformative users, it operates as a 'snare' that extracts value and stifles innovation. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Copyright holders and content industries are clear beneficiaries, as this reading maximizes their control and revenue. Transformative users and independent creators are victims, facing high costs and suppressed creative freedom. Courts act as agenda-setters, interpreting the law in a way that reinforces this creator-centric view. Public domain advocates are excluded, as their perspective is largely marginalized.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fair_use_interpretation_ambiguity,
    'Is the current judicial interpretation of the four-factor test genuinely balancing creator incentives and public access, or is it disproportionately favoring property rights?',
    'Empirical analysis of fair use litigation outcomes over time, comparing success rates for different types of uses and the economic impact on both copyright holders and users. Legislative action to clarify statutory language.',
    'If disproportionately favoring property rights, the constraint''s effective extractiveness is higher than intended, and it functions more as a Snare. If a true balance is found, it moves closer to a Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fair_use_interpretation_ambiguity, conceptual, 'Ambiguity in the judicial application of fair use principles.').

omega_variable(
    chilling_effect_quantification,
    'What is the quantifiable chilling effect on independent creators and transformative works due to the perceived risk of copyright infringement lawsuits under this reading?',
    'Surveys of creators, analysis of abandoned projects, and comparison of creative output in jurisdictions with different fair use standards. Economic modeling of opportunity costs.',
    'A high quantifiable chilling effect would increase the effective suppression and extractiveness, pushing the classification further towards Snare, as it demonstrates a significant cost borne by potential creators.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(chilling_effect_quantification, empirical, 'Measuring the unproduced creative output due to fear of litigation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_four_factor_test__creator_centric_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_tr_t0, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(fair_tr_t10, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(fair_tr_t20, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 20, 0.21).
narrative_ontology:measurement(fair_tr_t30, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 30, 0.2).

% Extraction over time
narrative_ontology:measurement(fair_be_t0, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(fair_be_t10, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(fair_be_t20, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(fair_be_t30, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 30, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(fair_su_t0, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(fair_su_t10, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(fair_su_t20, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 20, 0.63).
narrative_ontology:measurement(fair_su_t30, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 30, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_four_factor_test__creator_centric_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fair_use_four_factor_test__creator_centric_reading, fair_use_four_factor_test__user_centric_reading).
narrative_ontology:affects_constraint(fair_use_four_factor_test__creator_centric_reading, fair_use_four_factor_test__transformative_use_reading).
narrative_ontology:affects_constraint(fair_use_four_factor_test__creator_centric_reading, copyright_term_extension).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'fair_use_four_factor_test' kernel. This 'creator-centric' reading emphasizes property rights and creator incentives, contrasting with the 'user-centric' and 'transformative_use' readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
