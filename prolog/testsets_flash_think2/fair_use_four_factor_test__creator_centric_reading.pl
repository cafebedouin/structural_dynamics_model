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
    narrative_ontology:constraint_vindicates/2,
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
 *   This constraint describes the 'creator-centric' reading of the fair use
 *   four-factor test, where fair use is viewed as a narrow exception to
 *   copyright holders' property rights. The interpretation prioritizes
 *   preserving creator incentives and minimizing market harm to the original
 *   work, often leading to a restrictive application of fair use. This
 *   reading is one of several competing interpretations of the fair use
 *   doctrine, which itself is a kernel of legal commitment.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_four_factor_test__creator_centric_reading, 0.78).
domain_priors:suppression_score(fair_use_four_factor_test__creator_centric_reading, 0.7).
domain_priors:theater_ratio(fair_use_four_factor_test__creator_centric_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_four_factor_test__creator_centric_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_four_factor_test__creator_centric_reading, "Fair Use Four-Factor Test (Creator-Centric Reading)").
narrative_ontology:topic_domain(fair_use_four_factor_test__creator_centric_reading, "legal/intellectual_property/cultural_production").

domain_priors:requires_active_enforcement(fair_use_four_factor_test__creator_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_four_factor_test__creator_centric_reading, '8932e14f-118a-487c-887d-d1e6cfd8791b').
narrative_ontology:cs_kernel_codification('8932e14f-118a-487c-887d-d1e6cfd8791b', fixed_text).
narrative_ontology:cs_authority_grounding('8932e14f-118a-487c-887d-d1e6cfd8791b', lineage).
narrative_ontology:cs_interpretation_layer_present('8932e14f-118a-487c-887d-d1e6cfd8791b').
narrative_ontology:cs_reading_relation('8932e14f-118a-487c-887d-d1e6cfd8791b', fair_use_four_factor_test__user_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('8932e14f-118a-487c-887d-d1e6cfd8791b', fair_use_four_factor_test__transformative_use_reading, influences).
narrative_ontology:cs_axiom('8932e14f-118a-487c-887d-d1e6cfd8791b', foundational, copyright_as_property_right).
narrative_ontology:cs_axiom_status(copyright_as_property_right, holdable).
narrative_ontology:cs_axiom_grounding('8932e14f-118a-487c-887d-d1e6cfd8791b', copyright_as_property_right, deontological).
narrative_ontology:cs_axiom('8932e14f-118a-487c-887d-d1e6cfd8791b', foundational, incentive_theory_of_copyright).
narrative_ontology:cs_axiom_status(incentive_theory_of_copyright, holdable).
narrative_ontology:cs_axiom_grounding('8932e14f-118a-487c-887d-d1e6cfd8791b', incentive_theory_of_copyright, instrumental).
narrative_ontology:cs_reference_frame('8932e14f-118a-487c-887d-d1e6cfd8791b', adams_v_burke_era).
narrative_ontology:cs_drift_state('8932e14f-118a-487c-887d-d1e6cfd8791b', contemporary_digital_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8932e14f-118a-487c-887d-d1e6cfd8791b', '').
narrative_ontology:cs_kernel_id(fair_use_four_factor_test__creator_centric_reading, fair_use_four_factor_test).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__creator_centric_reading, copyright_holders).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__creator_centric_reading, creative_industries).
narrative_ontology:constraint_victim(fair_use_four_factor_test__creator_centric_reading, transformative_users).
narrative_ontology:constraint_victim(fair_use_four_factor_test__creator_centric_reading, independent_creators).
narrative_ontology:constraint_victim(fair_use_four_factor_test__creator_centric_reading, public_domain_advocates).
narrative_ontology:constraint_vindicates(fair_use_four_factor_test__creator_centric_reading, incentive_theory_of_copyright).
narrative_ontology:constraint_vindicates(fair_use_four_factor_test__creator_centric_reading, copyright_as_property_right).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Owners of copyrighted works (e.g., major studios, record labels, authors) who assert their property rights and seek to maximize control and revenue from their creations. They actively enforce copyright and interpret fair use narrowly to protect their market. They benefit directly from the high barriers to unauthorized use.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, copyright_holders, agenda_setter,
    institutional, generational, mobile, global).

% Organizations and businesses (e.g., publishers, film distributors) that rely on strong copyright protection to monetize creative works. They benefit from the legal framework that prioritizes creator incentives and limits competition from derivative works.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, creative_industries, beneficiary,
    organized, biographical, constrained, national).

% Artists, scholars, educators, and content creators who wish to use copyrighted material in new, transformative ways (e.g., parody, commentary, remix). They bear the cost of legal uncertainty and potential litigation, often leading to self-censorship or abandonment of projects.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, transformative_users, payer,
    moderate, immediate, constrained, global).

% Smaller-scale creators who often build upon existing cultural works. They face significant legal and financial risks if their work is deemed infringing, lacking the resources to defend fair use claims, leading to a chilling effect on their creative output.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, independent_creators, payer,
    powerless, biographical, identity_locked, local).

% Groups and individuals who argue for a robust public domain and broader user rights, emphasizing the societal benefits of cultural access and derivative creation. Their arguments for a more expansive fair use are often subordinated to property rights in this reading.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, public_domain_advocates, excluded,
    organized, generational, constrained, global).

% Judicial bodies that interpret and apply the four-factor fair use test. In this reading, they tend to emphasize the market harm to the original work and the non-transformative nature of the use, often siding with copyright holders.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, courts, agenda_setter,
    institutional, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the balance between copyright holders' exclusive rights and the public's limited right to use copyrighted material, aiming to incentivize creative production by securing creators' economic interests.
% TRANSFER_FUNCTION: Transfers economic value and control over derivative works from transformative users and independent creators to copyright holders, by narrowly defining permissible uses and broadly protecting original works.
% ABSENT_VOICES: A more robust public interest perspective, emphasizing the societal benefits of cultural remixing and access, is often marginalized. Advocates for user rights and a vibrant public domain are present in legal discourse but their arguments are often subordinated in this creator-centric interpretation.
% DISAPPEARANCE_RATIONALE: If the creator-centric reading of fair use vanished overnight, there would be a significant shift in the balance of power. Copyright holders would lose a key mechanism for protecting their market, leading to a potential decrease in their revenue streams. Transformative users would face fewer legal barriers, potentially leading to an explosion of derivative works and a more vibrant, but potentially chaotic, cultural landscape. The entire creative economy would need to reorganize around a new understanding of intellectual property.
% FOUNDING_PROBLEM: The original problem was how to balance the need to incentivize creators through exclusive rights with the public's interest in accessing and building upon existing knowledge and culture, preventing monopolies on ideas.
% FOUNDING_PROBLEM_CORROBORATION: Copyright holders and creative industries attest that the problem of incentivizing creation remains live, citing ongoing piracy and the need for strong protections in the digital age. Transformative users and public domain advocates, while acknowledging the need for incentives, argue that the current interpretation over-solves the problem for creators at the expense of public benefit; independent legal scholars and cultural economists corroborate the ongoing tension and the shift in balance.
narrative_ontology:disappearance_verdict(fair_use_four_factor_test__creator_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_four_factor_test__creator_centric_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_four_factor_test__creator_centric_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(fair_use_four_factor_test__creator_centric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_four_factor_test__creator_centric_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness is high (0.78) because this reading consistently favors copyright holders, allowing them to extract value from uses that might otherwise be considered fair. Suppression (0.70) is also high due to the chilling effect of potential litigation on transformative users and independent creators, who often lack the resources to defend their interpretations of fair use. The theater ratio is low (0.20) because the legal process of applying the four-factor test is genuinely functional, even if its outcome is consistently skewed. The metrics reflect a system that, while claiming to balance interests, structurally favors one side.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of copyright holders, this reading of fair use is a necessary and just mechanism for protecting their property and incentivizing future creation. From the perspective of transformative users and public domain advocates, it is an extractive mechanism that stifles creativity and limits public access to culture. The engine's classification will highlight this structural asymmetry, which is obscured by the 'balancing test' rhetoric.
 *
 * DIRECTIONALITY LOGIC:
 *   Copyright holders and creative industries are clear beneficiaries, as the constraint's interpretation directly secures their economic interests and control (low directionality). Transformative users and independent creators are targets, bearing the costs of legal uncertainty and restricted creative freedom (high directionality). Public domain advocates are excluded, as their arguments for broader access are often dismissed or subordinated. Courts, as agenda-setters, apply the test, and in this reading, their application tends to align with the beneficiaries' interests.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fair_use_kernel_reading_ambiguity,
    'Is this constraint a true reflection of the fair use doctrine''s intent, or one specific, ideologically-driven reading of a contested legal kernel?',
    'Comparative legal analysis across jurisdictions with different fair use interpretations, or a shift in judicial precedent towards a more user-centric or transformative-use-centric approach.',
    'If it''s merely one reading, its classification as a Tangled Rope would be confirmed, highlighting the extraction inherent in this specific interpretation. If it were the ''true'' intent, the extractiveness might be re-evaluated as a necessary cost of coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fair_use_kernel_reading_ambiguity, conceptual, 'Ambiguity between a specific reading and the doctrine''s overall intent.').

omega_variable(
    chilling_effect_quantification,
    'To what extent does the creator-centric reading of fair use produce a measurable chilling effect on derivative works and independent creation?',
    'Empirical studies on creator behavior, surveys of artists and scholars regarding abandoned projects due to fair use concerns, and analysis of the volume of derivative works in different legal regimes.',
    'Strong empirical evidence of a chilling effect would increase the measured suppression and extractiveness, further solidifying a Snare or highly extractive Tangled Rope classification. Weak evidence might suggest the suppression is overstated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(chilling_effect_quantification, empirical, 'Quantification of the chilling effect on creative output.').

omega_variable(
    market_harm_definition_ambiguity,
    'How broadly or narrowly should ''market harm'' to the original work be defined in the fair use analysis, and whose market (primary or secondary) should be prioritized?',
    'Judicial clarification or legislative reform that provides explicit guidance on the scope of market harm, particularly concerning potential markets for derivative works that the original creator has not yet exploited.',
    'A narrow definition of market harm would expand the scope of fair use, reducing extractiveness. A broad definition, as favored by this reading, maintains high extractiveness by protecting speculative future markets for copyright holders.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_harm_definition_ambiguity, conceptual, 'Ambiguity in defining market harm in fair use analysis.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_four_factor_test__creator_centric_reading, 1976, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_tr_t1976, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 1976, 0.15).
narrative_ontology:measurement(fair_tr_t1990, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 1990, 0.18).
narrative_ontology:measurement(fair_tr_t2000, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 2000, 0.2).
narrative_ontology:measurement(fair_tr_t2010, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 2010, 0.2).
narrative_ontology:measurement(fair_tr_t2024, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(fair_be_t1976, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 1976, 0.6).
narrative_ontology:measurement(fair_be_t1990, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 1990, 0.68).
narrative_ontology:measurement(fair_be_t2000, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 2000, 0.72).
narrative_ontology:measurement(fair_be_t2010, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 2010, 0.75).
narrative_ontology:measurement(fair_be_t2024, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(fair_su_t1976, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 1976, 0.55).
narrative_ontology:measurement(fair_su_t1990, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 1990, 0.6).
narrative_ontology:measurement(fair_su_t2000, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 2000, 0.65).
narrative_ontology:measurement(fair_su_t2010, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 2010, 0.68).
narrative_ontology:measurement(fair_su_t2024, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_four_factor_test__creator_centric_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
