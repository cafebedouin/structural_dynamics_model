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
 *   potential market for or value of the copyrighted work) are weighed to
 *   primarily preserve creator incentives and minimize perceived market harm.
 *   This reading often leads to outcomes favoring copyright holders and
 *   limiting transformative or derivative uses. This is one reading of the
 *   'fair_use_four_factor_test' kernel, instantiated as
 *   'creator_centric_reading'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_four_factor_test__creator_centric_reading, 0.8).
domain_priors:suppression_score(fair_use_four_factor_test__creator_centric_reading, 0.75).
domain_priors:theater_ratio(fair_use_four_factor_test__creator_centric_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_four_factor_test__creator_centric_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_four_factor_test__creator_centric_reading, "Fair Use Four-Factor Test (Creator-Centric Reading)").
narrative_ontology:topic_domain(fair_use_four_factor_test__creator_centric_reading, "legal/intellectual_property/cultural_production").

domain_priors:requires_active_enforcement(fair_use_four_factor_test__creator_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_four_factor_test__creator_centric_reading, '8ad470e6-2db3-422a-8bb7-5f5b8cdc656e').
narrative_ontology:cs_kernel_codification('8ad470e6-2db3-422a-8bb7-5f5b8cdc656e', fixed_text).
narrative_ontology:cs_authority_grounding('8ad470e6-2db3-422a-8bb7-5f5b8cdc656e', lineage).
narrative_ontology:cs_interpretation_layer_present('8ad470e6-2db3-422a-8bb7-5f5b8cdc656e').
narrative_ontology:cs_reading_relation('8ad470e6-2db3-422a-8bb7-5f5b8cdc656e', fair_use_four_factor_test__user_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('8ad470e6-2db3-422a-8bb7-5f5b8cdc656e', fair_use_four_factor_test__transformative_use_reading, influences).
narrative_ontology:cs_axiom('8ad470e6-2db3-422a-8bb7-5f5b8cdc656e', foundational, copyright_as_strong_property_right).
narrative_ontology:cs_axiom_status(copyright_as_strong_property_right, holdable).
narrative_ontology:cs_axiom_grounding('8ad470e6-2db3-422a-8bb7-5f5b8cdc656e', copyright_as_strong_property_right, deontological).
narrative_ontology:cs_axiom('8ad470e6-2db3-422a-8bb7-5f5b8cdc656e', foundational, incentive_theory_of_copyright_supremacy).
narrative_ontology:cs_axiom_status(incentive_theory_of_copyright_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('8ad470e6-2db3-422a-8bb7-5f5b8cdc656e', incentive_theory_of_copyright_supremacy, empirically_contingent).
narrative_ontology:cs_reference_frame('8ad470e6-2db3-422a-8bb7-5f5b8cdc656e', strong_property_rights_framework).
narrative_ontology:cs_drift_state('8ad470e6-2db3-422a-8bb7-5f5b8cdc656e', contemporary_digital_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8ad470e6-2db3-422a-8bb7-5f5b8cdc656e', '').
narrative_ontology:cs_kernel_id(fair_use_four_factor_test__creator_centric_reading, fair_use_four_factor_test).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__creator_centric_reading, copyright_holders).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__creator_centric_reading, creative_industries).
narrative_ontology:constraint_victim(fair_use_four_factor_test__creator_centric_reading, transformative_users).
narrative_ontology:constraint_victim(fair_use_four_factor_test__creator_centric_reading, independent_creators).
narrative_ontology:constraint_victim(fair_use_four_factor_test__creator_centric_reading, public_domain_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals and corporations holding copyrights, who benefit from a strong interpretation of their exclusive rights, allowing them to control and monetize their works without significant unauthorized competition. They actively enforce these rights through litigation.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, copyright_holders, beneficiary,
    institutional, generational, arbitrage, global).

% Publishers, studios, and other entities that produce and distribute creative works. They rely on robust copyright protection to maintain their business models and incentivize investment in new content, seeing fair use as a threat to their revenue streams.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, creative_industries, beneficiary,
    institutional, generational, arbitrage, global).

% Artists, educators, critics, and content creators who wish to use copyrighted material in new and transformative ways. They face high legal costs and uncertainty, often leading to self-censorship or abandonment of projects due to the strict interpretation of fair use.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, transformative_users, payer,
    powerless, biographical, constrained, global).

% Smaller-scale creators who rely on building upon existing cultural works but lack the resources to license content or defend against infringement claims. They are disproportionately impacted by a narrow fair use interpretation, limiting their ability to participate in cultural production.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, independent_creators, payer,
    moderate, biographical, constrained, global).

% Organizations and individuals who argue for a robust public domain and broader user rights, believing that cultural progress depends on the ability to freely access and build upon existing works. Their arguments are often marginalized in legal interpretations that prioritize property rights.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, public_domain_advocates, excluded,
    organized, generational, analytical, global).

% Judicial bodies responsible for interpreting and applying copyright law, including the fair use doctrine. In this reading, they tend to weigh the four factors in a manner that prioritizes the rights holder's control and potential market harm from unauthorized uses.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, courts, agenda_setter,
    institutional, generational, analytical, national).

% Academics and legal experts who analyze copyright law and its application. They observe the effects of the creator-centric reading, often critiquing its impact on innovation and public access, but do not directly participate in enforcement or benefit from the constraint.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for incentivizing the creation of new works by granting creators exclusive rights and providing a limited, predictable exception for certain uses, thereby fostering a stable market for creative output.
% TRANSFER_FUNCTION: Transfers control and potential revenue from those who would make unauthorized uses (even if transformative) to the original copyright holders, as a means to secure their investment and creative labor.
% ABSENT_VOICES: The public at large, who benefit from a rich public domain and accessible cultural heritage, are not directly represented in the legal balancing act. Future creators who might build on existing works also lack a direct voice in shaping this interpretation.
% DISAPPEARANCE_RATIONALE: If this creator-centric interpretation of fair use vanished, there would be a significant shift in how derivative works are created and monetized. Copyright holders would lose substantial control and revenue, leading to a reorganization of creative industries and potentially an explosion of new, unauthorized content.
% FOUNDING_PROBLEM: To balance the public's interest in the progress of science and useful arts with the need to incentivize authors and artists to create new works, preventing free-riding that would undermine creative industries.
% FOUNDING_PROBLEM_CORROBORATION: Copyright holders and their industry associations attest the problem of incentivizing creation is still live and requires strong protection. User rights groups, some legal scholars, and digital rights organizations argue the founding problem has largely been addressed and the current interpretation leads to over-protection and stifled creativity; legislative-hearing testimony and independent academic analysis support the shifted-function reading.
narrative_ontology:disappearance_verdict(fair_use_four_factor_test__creator_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_four_factor_test__creator_centric_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_four_factor_test__creator_centric_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(fair_use_four_factor_test__creator_centric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_four_factor_test__creator_centric_reading, 0.8, 'gemini-2.5-flash', 'none', direct).

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
 *   The high extractiveness (0.80) reflects the significant value transferred from potential users to copyright holders by restricting unauthorized uses. Suppression (0.75) is high due to the active enforcement through litigation and the chilling effect of legal uncertainty. The theater ratio is low (0.15) because the test is genuinely applied by courts, even if the interpretation is consistently narrow. Accessibility collapse is high (0.70) as the legal risks make many forms of creative reuse practically inaccessible without licensing. Resistance (0.50) is moderate, coming from user rights groups and some academics, but often insufficient to shift the dominant legal interpretation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of copyright holders, this interpretation is a necessary 'rope' that coordinates creative incentives and market stability. From the perspective of transformative users, it operates as a 'snare,' extracting value and suppressing innovation by limiting access to cultural building blocks. The engine's computation of per-seat classifications will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Copyright holders and creative industries are clear beneficiaries, as the constraint secures their exclusive rights and revenue streams. Transformative users and independent creators are targets, bearing the costs of restricted access and legal uncertainty. Public domain advocates are excluded, as their arguments for broader access are often sidelined. Courts act as agenda-setters, applying the test in a way that reinforces this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    creator_incentive_vs_public_domain_balance,
    'Is the current balance, as interpreted by this reading, optimal for fostering overall creativity and cultural production, or does it over-incentivize original creators at the expense of follow-on innovation and public access?',
    'Longitudinal empirical studies on the impact of fair use interpretations on both original creation rates and derivative work production, as well as economic analysis of market dynamics in different copyright regimes.',
    'If the balance is found to stifle innovation, it would support re-evaluating the ''creator-centric'' interpretation, potentially shifting towards a more ''user-centric'' or ''transformative-use'' approach. If it is found to be optimal, it would reinforce the current reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(creator_incentive_vs_public_domain_balance, empirical, 'Assessing the actual impact of strict fair use interpretation on cultural production.').

omega_variable(
    chilling_effect_quantification,
    'To what extent does the strict application of the four-factor test, particularly the market harm factor, create a measurable ''chilling effect'' on legitimate transformative uses and independent creators?',
    'Surveys of creators, analysis of abandoned projects due to fair use concerns, and case studies of legal costs associated with fair use defenses.',
    'Quantifiable evidence of a significant chilling effect would challenge the ''creator-centric'' reading''s claim to promote overall creativity, potentially leading to calls for legislative or judicial adjustments to favor transformative uses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(chilling_effect_quantification, empirical, 'Measuring the suppressive impact of fair use litigation risk on creative reuse.').

omega_variable(
    kernel_reading_identity,
    'This constraint is one specific reading (''creator_centric_reading'') of the ''fair_use_four_factor_test'' kernel. How do its structural properties differ from sibling readings (''user_centric_reading'', ''transformative_use_reading'')?',
    'Comparative analysis of legal precedents, legislative history, and academic commentary across different jurisdictions and judicial philosophies that align with each reading.',
    'Understanding the distinct structural properties of each reading allows for precise classification of the underlying constraints and reveals how different interpretations of the same legal text lead to divergent outcomes for stakeholders.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Clarifying the distinct structural identity of this kernel reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_four_factor_test__creator_centric_reading, 1976, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_tr_t1976, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 1976, 0.1).
narrative_ontology:measurement(fair_tr_t1990, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 1990, 0.12).
narrative_ontology:measurement(fair_tr_t2000, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 2000, 0.14).
narrative_ontology:measurement(fair_tr_t2010, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 2010, 0.15).
narrative_ontology:measurement(fair_tr_t2024, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(fair_be_t1976, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 1976, 0.65).
narrative_ontology:measurement(fair_be_t1990, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 1990, 0.7).
narrative_ontology:measurement(fair_be_t2000, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 2000, 0.75).
narrative_ontology:measurement(fair_be_t2010, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 2010, 0.78).
narrative_ontology:measurement(fair_be_t2024, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 2024, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(fair_su_t1976, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 1976, 0.6).
narrative_ontology:measurement(fair_su_t1990, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 1990, 0.65).
narrative_ontology:measurement(fair_su_t2000, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement(fair_su_t2010, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 2010, 0.73).
narrative_ontology:measurement(fair_su_t2024, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_four_factor_test__creator_centric_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fair_use_four_factor_test__creator_centric_reading, copyright_term_extension).
narrative_ontology:affects_constraint(fair_use_four_factor_test__creator_centric_reading, digital_rights_management_enforcement).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'fair_use_four_factor_test' kernel. Its structural properties, particularly extractiveness and suppression, are distinct from its sibling readings ('user_centric_reading', 'transformative_use_reading'), which would yield different classifications and stakeholder impacts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
