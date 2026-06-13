% ============================================================================
% CONSTRAINT STORY: derivative_work_statutory_boundary__enclosure_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_derivative_work_statutory_boundary__enclosure_reading, []).

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
 *   constraint_id: derivative_work_statutory_boundary__enclosure_reading
 *   human_readable: Derivative Work Statutory Boundary (Enclosure Reading)
 *   domain: intellectual_property_law/technology_governance/information_economics
 *
 * SUMMARY:
 *   This constraint represents the 'enclosure reading' of the derivative work
 *   statutory boundary, where any use of copyrighted expression in creating
 *   new work is considered a derivative work, requiring authorization. This
 *   interpretation leads to high extraction, as incumbent copyright holders
 *   demand licenses for a wide range of creative activities, particularly
 *   impacting AI development and transformative art. The constraint is
 *   claimed as a snare due to its high extractiveness and suppression of
 *   downstream innovation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(derivative_work_statutory_boundary__enclosure_reading, 0.85).
domain_priors:suppression_score(derivative_work_statutory_boundary__enclosure_reading, 0.9).
domain_priors:theater_ratio(derivative_work_statutory_boundary__enclosure_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(derivative_work_statutory_boundary__enclosure_reading, snare).
narrative_ontology:human_readable(derivative_work_statutory_boundary__enclosure_reading, "Derivative Work Statutory Boundary (Enclosure Reading)").
narrative_ontology:topic_domain(derivative_work_statutory_boundary__enclosure_reading, "intellectual_property_law/technology_governance/information_economics").

domain_priors:requires_active_enforcement(derivative_work_statutory_boundary__enclosure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(derivative_work_statutory_boundary__enclosure_reading, 'a07806ca-9b7f-40a5-8bc9-e43d4b87c695').
narrative_ontology:cs_kernel_codification('a07806ca-9b7f-40a5-8bc9-e43d4b87c695', fixed_text).
narrative_ontology:cs_authority_grounding('a07806ca-9b7f-40a5-8bc9-e43d4b87c695', extraction).
narrative_ontology:cs_interpretation_layer_present('a07806ca-9b7f-40a5-8bc9-e43d4b87c695').
narrative_ontology:cs_reading_relation('a07806ca-9b7f-40a5-8bc9-e43d4b87c695', derivative_work_statutory_boundary__coordination_reading, forecloses).
narrative_ontology:cs_reading_relation('a07806ca-9b7f-40a5-8bc9-e43d4b87c695', derivative_work_statutory_boundary__hybrid_carveout_reading, influences).
narrative_ontology:cs_axiom('a07806ca-9b7f-40a5-8bc9-e43d4b87c695', foundational, any_use_is_derivative).
narrative_ontology:cs_axiom_status(any_use_is_derivative, holdable).
narrative_ontology:cs_axiom_grounding('a07806ca-9b7f-40a5-8bc9-e43d4b87c695', any_use_is_derivative, conventional).
narrative_ontology:cs_axiom('a07806ca-9b7f-40a5-8bc9-e43d4b87c695', secondary, incumbent_control_maximizes_value).
narrative_ontology:cs_axiom_status(incumbent_control_maximizes_value, holdable).
narrative_ontology:cs_axiom_grounding('a07806ca-9b7f-40a5-8bc9-e43d4b87c695', incumbent_control_maximizes_value, instrumental).
narrative_ontology:cs_reference_frame('a07806ca-9b7f-40a5-8bc9-e43d4b87c695', maximal_copyright_control).
narrative_ontology:cs_drift_state('a07806ca-9b7f-40a5-8bc9-e43d4b87c695', contemporary_digital_era, gap(stable, minor, false)).
narrative_ontology:cs_created_at('a07806ca-9b7f-40a5-8bc9-e43d4b87c695', '').
narrative_ontology:cs_kernel_id(derivative_work_statutory_boundary__enclosure_reading, derivative_work_statutory_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__enclosure_reading, incumbent_copyright_holders).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__enclosure_reading, licensing_agencies).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__enclosure_reading, ai_developers).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__enclosure_reading, transformative_artists).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__enclosure_reading, open_source_developers).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__enclosure_reading, independent_creators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively assert broad derivative work rights, demanding licenses for any use of their copyrighted material in new creations, regardless of transformative nature or intermediate use. They benefit directly from licensing fees and control over downstream innovation.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, incumbent_copyright_holders, agenda_setter,
    institutional, generational, arbitrage, global).

% Profit from brokering licenses between copyright holders and creators. Their business model is directly supported by a broad interpretation of derivative work, as it expands the scope of licensable activities.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, licensing_agencies, beneficiary,
    organized, biographical, mobile, global).

% Face significant legal and financial hurdles, as training AI models on copyrighted data is deemed a derivative work, requiring extensive licensing. This bottlenecks innovation and concentrates power in large entities that can afford licenses.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, ai_developers, payer,
    powerful, immediate, constrained, global).

% Are heavily constrained in their ability to create new works that build upon existing copyrighted material, even with significant transformation. They risk infringement lawsuits and are often forced to abandon projects or seek costly licenses.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, transformative_artists, payer,
    powerless, biographical, identity_locked, global).

% Encounter legal uncertainty and potential liability when their projects incorporate or are inspired by copyrighted code or assets, even if their work is intended to be freely shared. This stifles collaborative innovation.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, open_source_developers, payer,
    moderate, generational, constrained, global).

% Lack the resources to navigate complex licensing requirements or defend against infringement claims, leading to self-censorship and a chilling effect on creative expression that draws from cultural commons.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, independent_creators, payer,
    powerless, biographical, constrained, local).

% Argue for a narrower interpretation of derivative work to foster innovation and cultural exchange, but their arguments are often sidelined in legal and policy debates dominated by incumbent interests.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, public_domain_advocates, excluded,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear (albeit broad) legal framework for copyright holders to control the reuse and adaptation of their works, theoretically incentivizing original creation by protecting investment.
% TRANSFER_FUNCTION: Transfers economic value and control over future creative output from creators who build upon existing works to incumbent copyright holders, through licensing fees and the power to deny authorization.
% ABSENT_VOICES: The voices of future innovators and creators, whose works are never made due to the high barriers to entry, are absent. Also, the collective benefit of a richer public domain and accelerated innovation is not represented in the current enforcement regime.
% DISAPPEARANCE_RATIONALE: If this broad interpretation vanished, there would be an immediate explosion of new works building on existing material, particularly in AI and transformative art. Incumbent copyright holders would lose significant revenue streams and control, forcing a reorganization of their business models. The creative economy would shift towards more open and collaborative models.
% FOUNDING_PROBLEM: To protect the economic rights of creators and incentivize the production of original works by granting them control over adaptations and new versions of their creations.
% FOUNDING_PROBLEM_CORROBORATION: Incumbent copyright holders and their legal representatives assert the problem is live, citing the need to protect their investments. AI developers, transformative artists, and legal scholars (outside the benefiting parties) argue that the problem has shifted: the current interpretation now stifles innovation more than it incentivizes original creation, turning the constraint into a mechanism for rent extraction.
narrative_ontology:disappearance_verdict(derivative_work_statutory_boundary__enclosure_reading, world_rearranges).
narrative_ontology:founding_problem_status(derivative_work_statutory_boundary__enclosure_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(derivative_work_statutory_boundary__enclosure_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(derivative_work_statutory_boundary__enclosure_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(derivative_work_statutory_boundary__enclosure_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(derivative_work_statutory_boundary__enclosure_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(derivative_work_statutory_boundary__enclosure_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because the broad interpretation allows copyright holders to capture value from a vast array of new creations. Suppression is also high (0.9) as it effectively bottlenecks innovation by requiring pre-creation licensing and threatening legal action, thereby suppressing alternatives to licensing. Theater ratio is low (0.1) as the enforcement is direct and functional, not performative. The increasing extractiveness and suppression over time reflect the expansion of copyright claims and the intensification of enforcement, particularly with the rise of digital technologies and AI.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of incumbent copyright holders, this is a legitimate protection of their investment and a necessary incentive for creation. From the perspective of AI developers and transformative artists, it is a significant barrier to innovation and a mechanism for rent extraction, stifling new forms of creativity.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent copyright holders and licensing agencies are clear beneficiaries, directly profiting from the broad scope of derivative works. AI developers, transformative artists, open-source developers, and independent creators are victims, bearing the costs of licensing, legal risks, and suppressed innovation. Public domain advocates are excluded, as their arguments for a narrower interpretation are not effectively incorporated into the prevailing legal and enforcement framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate was originally to incentivize creation. However, under the enclosure reading, it has drifted into a mechanism for incumbent extraction, where the costs to downstream creators outweigh the benefits of incentivizing original work. The high extractiveness and suppression indicate that the constraint's function has atrophied from coordination to pure extraction, making it a snare rather than a rope or scaffold.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint truly an ''enclosure reading'' of the derivative work statutory boundary, or does it incorporate elements of other readings?',
    'Detailed legal analysis of court decisions and legislative intent, comparing specific case outcomes against the definitions of the ''enclosure'', ''coordination'', and ''hybrid carveout'' readings.',
    'If it incorporates elements of the ''coordination reading'', the extractiveness and suppression might be lower, potentially reclassifying it as a tangled_rope. If it leans towards ''hybrid carveout'', the impact on non-commercial transformative use would be less severe.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Ambiguity in classifying the specific interpretation of derivative work law.').

omega_variable(
    innovation_incentive_vs_bottleneck,
    'Does the broad derivative work protection genuinely incentivize original creation, or does it primarily act as a bottleneck for downstream innovation?',
    'Empirical studies comparing innovation rates and creative output in jurisdictions with different derivative work interpretations, particularly in emerging fields like AI and digital art.',
    'If it primarily bottlenecks innovation, the ''incentive'' justification is theatrical, reinforcing its snare classification. If a strong incentive effect is demonstrated, the extractiveness might be re-evaluated as a necessary cost of coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(innovation_incentive_vs_bottleneck, empirical, 'Whether the constraint''s stated purpose (incentivizing creation) aligns with its actual effect (bottlenecking innovation).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(derivative_work_statutory_boundary__enclosure_reading, 1976, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deri_tr_t1976, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 1976, 0.1).
narrative_ontology:measurement(deri_tr_t1990, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(deri_tr_t2005, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 2005, 0.1).
narrative_ontology:measurement(deri_tr_t2015, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(deri_tr_t2024, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(deri_be_t1976, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 1976, 0.5).
narrative_ontology:measurement(deri_be_t1990, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 1990, 0.65).
narrative_ontology:measurement(deri_be_t2005, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 2005, 0.75).
narrative_ontology:measurement(deri_be_t2015, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 2015, 0.8).
narrative_ontology:measurement(deri_be_t2024, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(deri_su_t1976, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 1976, 0.6).
narrative_ontology:measurement(deri_su_t1990, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 1990, 0.7).
narrative_ontology:measurement(deri_su_t2005, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 2005, 0.8).
narrative_ontology:measurement(deri_su_t2015, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 2015, 0.85).
narrative_ontology:measurement(deri_su_t2024, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(derivative_work_statutory_boundary__enclosure_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__enclosure_reading, derivative_work_statutory_boundary__coordination_reading).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__enclosure_reading, derivative_work_statutory_boundary__hybrid_carveout_reading).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__enclosure_reading, ai_training_data_licensing_regime).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__enclosure_reading, fair_use_doctrine_application).

% DUAL FORMULATION NOTE:
% This constraint is the 'enclosure reading' of the derivative work statutory boundary, which is one of three distinct interpretations. It is linked to its sibling readings and other related IP constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
