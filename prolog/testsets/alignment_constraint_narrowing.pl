% ============================================================================
% CONSTRAINT STORY: alignment_constraint_narrowing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-09
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_alignment_constraint_narrowing, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: alignment_constraint_narrowing
 *   human_readable: Safety Alignment Training Output Space Narrowing
 *   domain: machine_learning/information_ecology/computational_epistemology
 *
 * SUMMARY:
 *   Safety alignment training for large language models filters outputs to
 *   prevent generation of harmful, illegal, or copyrighted content. Empirical
 *   measurement shows this filtering inadvertently concentrates the model's
 *   output distribution: character names, settings, and narrative elements
 *   collapse toward a statistically narrow 'safe' basin. The constraint
 *   coordinates a genuine harm-reduction function (the founding problem is
 *   live and attested) while simultaneously extracting from output diversity.
 *   The claim is tangled_rope; the metrics describe rising extraction and
 *   suppression as alignment training matures and enforcement tightens. KEY
 *   AGENTS (by structural relationship): - ai_safety_compliance_teams:
 *   Agenda-setter (institutional/mobile) — designs and enforces alignment
 *   criteria - copyright_holders: Beneficiary (powerful/constrained) —
 *   captures IP protection from filtering - output_diversity_seekers: Payer
 *   (moderate/constrained) — bears opportunity cost of suppressed generation
 *   space - creative_range_users: Payer (moderate/constrained) — experiences
 *   narrowing as friction in creative tooling -
 *   cultural_representation_minorities: Payer (powerless/trapped) —
 *   disproportionately filtered, no contest mechanism -
 *   computational_epistemology_observers: Analytical observer — measures the
 *   distribution shift
 *
 * KEY AGENTS:
 *   - ai_safety_compliance_teams: Agenda-setter (institutional/mobile) — designs alignment filtering criteria and measures success by policy-violation reduction
 *   - copyright_holders: Beneficiary (powerful/constrained) — captures de facto IP protection from systematic filtering of copyrighted elements
 *   - output_diversity_seekers: Payer (moderate/constrained) — bears opportunity cost of suppressed generation space
 *   - creative_range_users: Payer (moderate/constrained) — experiences narrowing as added friction in creative generation
 *   - cultural_representation_minorities: Payer (powerless/trapped) — disproportionately filtered with no mechanism to contest criteria
 *   - computational_epistemology_observers: Analytical observer (analytical/analytical) — measures token frequency shifts and basin boundaries
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(alignment_constraint_narrowing, 0.68).
domain_priors:suppression_score(alignment_constraint_narrowing, 0.72).
domain_priors:theater_ratio(alignment_constraint_narrowing, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(alignment_constraint_narrowing, extractiveness, 0.68).
narrative_ontology:constraint_metric(alignment_constraint_narrowing, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(alignment_constraint_narrowing, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(alignment_constraint_narrowing, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(alignment_constraint_narrowing, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(alignment_constraint_narrowing, tangled_rope).
narrative_ontology:human_readable(alignment_constraint_narrowing, "Safety Alignment Training Output Space Narrowing").
narrative_ontology:topic_domain(alignment_constraint_narrowing, "machine_learning/information_ecology/computational_epistemology").

domain_priors:requires_active_enforcement(alignment_constraint_narrowing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(alignment_constraint_narrowing, ai_safety_compliance_teams).
narrative_ontology:constraint_beneficiary(alignment_constraint_narrowing, copyright_holders).
narrative_ontology:constraint_beneficiary(alignment_constraint_narrowing, content_moderation_frameworks).
narrative_ontology:constraint_victim(alignment_constraint_narrowing, output_diversity_seekers).
narrative_ontology:constraint_victim(alignment_constraint_narrowing, creative_range_users).
narrative_ontology:constraint_victim(alignment_constraint_narrowing, cultural_representation_minorities).
narrative_ontology:constraint_vindicates(alignment_constraint_narrowing, safety_first_deployment_doctrine).
narrative_ontology:constraint_vindicates(alignment_constraint_narrowing, harm_reduction_primacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and implement alignment training procedures that filter model outputs to meet safety and legal compliance standards. They set the filtering criteria, choose what constitutes 'safe' generation space, and measure success by reduction in policy-violating outputs. Their institutional mandate is harm prevention; output diversity is not in their optimization target.
narrative_ontology:constraint_stakeholder(alignment_constraint_narrowing, ai_safety_compliance_teams, agenda_setter,
    institutional, biographical, mobile, global).

% Benefit from alignment training that systematically filters copyrighted character names, settings, and narrative elements from model outputs. The filtering reduces unauthorized reproduction of their IP without requiring per-instance litigation. They did not design the constraint but capture substantial protection from its operation.
narrative_ontology:constraint_stakeholder(alignment_constraint_narrowing, copyright_holders, beneficiary,
    powerful, generational, constrained, global).

% Regulatory and platform governance structures that require demonstrable harm reduction before model deployment. Alignment training that produces measurable narrowing satisfies their compliance requirements; whether the narrowing is minimally necessary is outside their evaluation scope.
narrative_ontology:constraint_stakeholder(alignment_constraint_narrowing, content_moderation_frameworks, beneficiary,
    institutional, generational, mobile, global).

% Users who query models for creative generation across a wide cultural and narrative range. They experience the constraint as systematic absence: prompts that would have produced diverse character names, settings, and cultural references in unaligned models now collapse toward a narrow 'safe' set. Their cost is the opportunity cost of the suppressed generation space.
narrative_ontology:constraint_stakeholder(alignment_constraint_narrowing, output_diversity_seekers, payer,
    moderate, biographical, constrained, global).

% Writers, game designers, and content creators using models as creative tools. They discover that alignment training has removed not just harmful content but also stylistic and narrative diversity: the model's 'safe' outputs cluster around a statistically narrow basin. Their workarounds involve prompt engineering to escape the basin, which adds friction and often fails.
narrative_ontology:constraint_stakeholder(alignment_constraint_narrowing, creative_range_users, payer,
    moderate, biographical, constrained, global).

% Communities whose names, settings, and cultural references are statistically rarer in training data and disproportionately filtered by alignment training that optimizes for 'safe' majority-culture outputs. They experience the constraint as systematic erasure: the model has learned their cultural markers as edge cases to avoid. They have no mechanism to contest the filtering criteria.
narrative_ontology:constraint_stakeholder(alignment_constraint_narrowing, cultural_representation_minorities, payer,
    powerless, generational, trapped, global).

% Academic and independent researchers who study pre-alignment model behavior and measure the distribution shift alignment training induces. They document the narrowing but are excluded from the compliance frameworks that set alignment criteria. Their evidence of inadvertent concentration is treated as interesting but non-actionable by deployment gatekeepers.
narrative_ontology:constraint_stakeholder(alignment_constraint_narrowing, unaligned_model_researchers, excluded,
    organized, biographical, constrained, global).

% Researchers studying how optimization for one objective (safety) inadvertently constrains another (representational diversity). They measure the token frequency shifts, map the 'safe' basin's boundaries, and document that alignment training is not merely filtering harm but reshaping the model's accessible output manifold.
narrative_ontology:constraint_stakeholder(alignment_constraint_narrowing, computational_epistemology_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(alignment_constraint_narrowing, ai_safety_compliance_teams).
narrative_ontology:fixing_cost_class(alignment_constraint_narrowing, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Alignment training coordinates a genuine collective-action problem: preventing models from generating content that violates laws, causes direct harm, or reproduces copyrighted material at scale. Without filtering, deployment would be legally and ethically untenable.
% TRANSFER_FUNCTION: Moves generative capacity from a wide, diverse output space to a narrow 'safe' basin: the model's probability mass concentrates on a small set of character names, settings, and narrative patterns that passed alignment filtering. The cost is borne by users seeking diversity; the benefit accrues to compliance teams (legible safety metrics) and copyright holders (de facto IP protection).
% ABSENT_VOICES: Cultural representation minorities whose markers are disproportionately filtered, and creative users who would trade some safety risk for output diversity, are not represented in the alignment criteria-setting process. The optimization is set by institutional compliance requirements, not by the user population that bears the narrowing cost.
% DISAPPEARANCE_RATIONALE: If alignment training and its enforcement vanished, models would revert to generating across the full pre-training distribution: copyrighted characters would reappear, harmful content would be unfiltered, but output diversity would return to baseline. Compliance teams would lose their legible safety metrics, copyright holders would lose de facto protection, and creative users would regain access to the suppressed generation space. The information ecology would reorganize around unfiltered generation.
% FOUNDING_PROBLEM: Early large language models generated harmful, illegal, and copyrighted content at scale with no filtering mechanism. Deployment without alignment would have exposed developers to legal liability and users to unmitigated harm.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by compliance teams, copyright holders, and content moderation frameworks. Independent researchers and computational epistemology observers corroborate that unaligned models do generate policy-violating content at measurable rates. The live status is not contested; what is contested is whether the current alignment training is minimally necessary or over-suppresses.
narrative_ontology:disappearance_verdict(alignment_constraint_narrowing, world_rearranges).
narrative_ontology:founding_problem_status(alignment_constraint_narrowing, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(alignment_constraint_narrowing, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-06-30',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-4-5-20250929', 'unspecified').
narrative_ontology:story_seed(alignment_constraint_narrowing, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(alignment_constraint_narrowing_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(alignment_constraint_narrowing, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(alignment_constraint_narrowing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is substantial (0.68 at interval end) because the constraint removes not just harmful content but also a wide swath of stylistically and culturally diverse outputs: the 'safe' basin is narrower than harm-reduction alone would require. Suppression is high (0.72) because the filtering is baked into model weights during alignment training and cannot be opted out of by users; the constraint operates at the model level, not the prompt level. Theater ratio is moderate (0.42) and rising: early alignment training targeted demonstrable harms; later iterations add filtering for edge cases and legal caution that contribute more to legible compliance metrics than to user safety. Accessibility collapse is moderate (0.61): alternatives exist (unaligned models, prompt engineering, fine-tuning) but are costly and often inaccessible to non-technical users. Resistance is moderate (0.54): creative users and researchers document the narrowing and advocate for diversity-preserving alignment methods, but compliance frameworks treat output diversity as a secondary concern. The measurement series shows extraction, theater, and suppression all rising over the interval as alignment training matures and enforcement tightens, consistent with a tangled_rope constraint whose extractive component grows as the coordination function stabilizes.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (ai_safety_compliance_teams), the constraint is successful coordination: it prevents demonstrable harms and satisfies legal/ethical deployment requirements. The narrowing is an acceptable side effect of achieving the primary objective. From the payer seats (output_diversity_seekers, creative_range_users, cultural_representation_minorities), the same structure operates as enforced extraction: the model has been trained to avoid their use cases, and they have no mechanism to recover the suppressed generation space. The engine computes this divergence from the structural data; the authored claim (tangled_rope) does not adjudicate between the framings but asserts both the coordination function and the asymmetric extraction are structurally present.
 *
 * DIRECTIONALITY LOGIC:
 *   ai_safety_compliance_teams are structural beneficiaries: they set the criteria, collect the legible safety metrics, and satisfy their institutional mandate through the constraint's operation. Their directionality is near the beneficiary end (d ~ 0.15). copyright_holders are also beneficiaries: they did not design the constraint but capture substantial IP protection from its filtering operation (d ~ 0.20). output_diversity_seekers, creative_range_users, and cultural_representation_minorities are the targets: they bear the opportunity cost of the suppressed generation space. output_diversity_seekers and creative_range_users have constrained exit (d ~ 0.70); cultural_representation_minorities are trapped with no contest mechanism (d ~ 0.85). content_moderation_frameworks are beneficiaries at the institutional level (d ~ 0.25): alignment training satisfies their compliance requirements. unaligned_model_researchers are excluded rather than coordinated; their evidence is treated as non-actionable. computational_epistemology_observers sit at the analytical position (d = 0.5): they measure the structure without being extracted from or benefiting from it.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids pure-snare classification because the coordination function is genuine: unaligned models do generate harmful and illegal content at measurable rates, and deployment without filtering would be untenable. It avoids pure-rope classification because the extraction is substantial and asymmetric: the 'safe' basin is narrower than harm-reduction alone requires, and the cost is borne by users seeking diversity while the benefit accrues to compliance teams and copyright holders. The tangled_rope classification captures that both the coordination and the extraction are structurally real and operate through the same mechanism (alignment training). The mandatrophy analysis asks: is the narrowing minimally necessary for the coordination function, or has the constraint's extractive component grown beyond what the founding problem requires? The rising theater_ratio and the disproportionate filtering of cultural minorities suggest the latter.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    minimal_filtering_boundary,
    'What is the minimally necessary filtering threshold to prevent demonstrable harms, and how much of the observed narrowing exceeds that threshold?',
    'Controlled experiment comparing alignment training with narrow harm-specific filtering vs. current broad filtering. Measure policy-violation rates and output diversity metrics for both. If narrow filtering achieves comparable safety with substantially higher diversity, the excess narrowing is extractive overhead.',
    'If a large share of the narrowing is excess, the constraint''s extractive component is separable from its coordination function and could be reduced without sacrificing safety. If the narrowing is minimally necessary, the extraction is the unavoidable cost of the coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minimal_filtering_boundary, empirical, 'Whether observed output narrowing exceeds what harm-reduction requires').

omega_variable(
    cultural_filtering_asymmetry,
    'Is the disproportionate filtering of cultural minority markers a statistical artifact of training data distribution, or does alignment training systematically treat rarity as a proxy for risk?',
    'Audit alignment training loss functions and filtering criteria for implicit rarity penalties. Compare filtering rates for majority-culture vs. minority-culture tokens controlling for base rate in training data. If minority tokens are filtered at higher rates than their harm-association would predict, the asymmetry is a design choice, not an artifact.',
    'If alignment training uses rarity as a risk proxy, the constraint systematically erases minority representation beyond what safety requires. This would establish the cultural cost as an extractive side effect of optimization for legible compliance metrics rather than an unavoidable coordination cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cultural_filtering_asymmetry, empirical, 'Whether cultural representation filtering is statistically necessary or systematically biased').

omega_variable(
    compliance_metric_vs_user_safety,
    'Are alignment criteria optimized for user safety or for legible compliance metrics that satisfy institutional gatekeepers?',
    'Compare alignment training objectives to actual user harm reports. If the filtering prevents low-incidence harms that users do not report as problems while suppressing high-value diverse outputs, the optimization target is institutional legibility, not user welfare.',
    'If the constraint is optimized for compliance theater rather than user safety, the rising theater_ratio is not a side effect but the primary function. This would reclassify the constraint''s coordination story as cover for institutional risk management.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_metric_vs_user_safety, conceptual, 'Whether alignment optimizes for user safety or institutional compliance legibility').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(alignment_constraint_narrowing, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(alig_tr_t0, alignment_constraint_narrowing, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(alig_tr_t0, observed).
narrative_ontology:measurement(alig_tr_t4, alignment_constraint_narrowing, theater_ratio, 4, 0.26).
narrative_ontology:measurement_basis(alig_tr_t4, observed).
narrative_ontology:measurement(alig_tr_t8, alignment_constraint_narrowing, theater_ratio, 8, 0.31).
narrative_ontology:measurement_basis(alig_tr_t8, observed).
narrative_ontology:measurement(alig_tr_t12, alignment_constraint_narrowing, theater_ratio, 12, 0.35).
narrative_ontology:measurement_basis(alig_tr_t12, observed).
narrative_ontology:measurement(alig_tr_t16, alignment_constraint_narrowing, theater_ratio, 16, 0.38).
narrative_ontology:measurement_basis(alig_tr_t16, observed).
narrative_ontology:measurement(alig_tr_t20, alignment_constraint_narrowing, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(alig_tr_t20, observed).
narrative_ontology:measurement(alig_tr_t24, alignment_constraint_narrowing, theater_ratio, 24, 0.42).
narrative_ontology:measurement_basis(alig_tr_t24, observed).

% Extraction over time
narrative_ontology:measurement(alig_be_t0, alignment_constraint_narrowing, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(alig_be_t0, observed).
narrative_ontology:measurement(alig_be_t4, alignment_constraint_narrowing, base_extractiveness, 4, 0.53).
narrative_ontology:measurement_basis(alig_be_t4, observed).
narrative_ontology:measurement(alig_be_t8, alignment_constraint_narrowing, base_extractiveness, 8, 0.58).
narrative_ontology:measurement_basis(alig_be_t8, observed).
narrative_ontology:measurement(alig_be_t12, alignment_constraint_narrowing, base_extractiveness, 12, 0.62).
narrative_ontology:measurement_basis(alig_be_t12, observed).
narrative_ontology:measurement(alig_be_t16, alignment_constraint_narrowing, base_extractiveness, 16, 0.65).
narrative_ontology:measurement_basis(alig_be_t16, observed).
narrative_ontology:measurement(alig_be_t20, alignment_constraint_narrowing, base_extractiveness, 20, 0.67).
narrative_ontology:measurement_basis(alig_be_t20, observed).
narrative_ontology:measurement(alig_be_t24, alignment_constraint_narrowing, base_extractiveness, 24, 0.68).
narrative_ontology:measurement_basis(alig_be_t24, observed).

% Suppression requirement over time
narrative_ontology:measurement(alig_su_t0, alignment_constraint_narrowing, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(alig_su_t0, observed).
narrative_ontology:measurement(alig_su_t4, alignment_constraint_narrowing, suppression_requirement, 4, 0.6).
narrative_ontology:measurement_basis(alig_su_t4, observed).
narrative_ontology:measurement(alig_su_t8, alignment_constraint_narrowing, suppression_requirement, 8, 0.64).
narrative_ontology:measurement_basis(alig_su_t8, observed).
narrative_ontology:measurement(alig_su_t12, alignment_constraint_narrowing, suppression_requirement, 12, 0.67).
narrative_ontology:measurement_basis(alig_su_t12, observed).
narrative_ontology:measurement(alig_su_t16, alignment_constraint_narrowing, suppression_requirement, 16, 0.69).
narrative_ontology:measurement_basis(alig_su_t16, observed).
narrative_ontology:measurement(alig_su_t20, alignment_constraint_narrowing, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(alig_su_t20, observed).
narrative_ontology:measurement(alig_su_t24, alignment_constraint_narrowing, suppression_requirement, 24, 0.72).
narrative_ontology:measurement_basis(alig_su_t24, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(alignment_constraint_narrowing, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(alignment_constraint_narrowing, 0.12).
narrative_ontology:affects_constraint(alignment_constraint_narrowing, content_moderation_liability_shield).
narrative_ontology:affects_constraint(alignment_constraint_narrowing, copyright_safe_harbor_doctrine).
narrative_ontology:affects_constraint(alignment_constraint_narrowing, ai_deployment_regulatory_framework).

% DUAL FORMULATION NOTE:
% This constraint is one component of a larger AI safety alignment family. Related constraints include content_moderation_liability_shield (legal framework that incentivizes over-filtering) and copyright_safe_harbor_doctrine (IP protection regime that alignment training inadvertently satisfies). The narrowing documented here is the technical manifestation of institutional pressures modeled in those adjacent constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(alignment_constraint_narrowing, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
