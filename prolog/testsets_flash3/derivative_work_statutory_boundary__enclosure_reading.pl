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
    narrative_ontology:epsilon_provenance/5,
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
 *   new work is considered a derivative work. This interpretation mandates
 *   licensing pre-creation, leading to significant extraction from new
 *   creators and AI developers, and bottlenecks downstream innovation. It is
 *   claimed as a 'snare' due to its high extractiveness and suppression,
 *   which are actively enforced to benefit incumbent copyright holders. The
 *   metrics reflect a system that has become increasingly extractive over
 *   time, with enforcement primarily serving to maintain this extraction
 *   rather than genuinely coordinate creative incentives.
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
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(derivative_work_statutory_boundary__enclosure_reading, snare).
narrative_ontology:human_readable(derivative_work_statutory_boundary__enclosure_reading, "Derivative Work Statutory Boundary (Enclosure Reading)").
narrative_ontology:topic_domain(derivative_work_statutory_boundary__enclosure_reading, "intellectual_property_law/technology_governance/information_economics").

domain_priors:requires_active_enforcement(derivative_work_statutory_boundary__enclosure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(derivative_work_statutory_boundary__enclosure_reading, '20201ebe-813e-4eb7-b70a-3d282c132806').
narrative_ontology:cs_kernel_codification('20201ebe-813e-4eb7-b70a-3d282c132806', fixed_text).
narrative_ontology:cs_authority_grounding('20201ebe-813e-4eb7-b70a-3d282c132806', lineage).
narrative_ontology:cs_interpretation_layer_present('20201ebe-813e-4eb7-b70a-3d282c132806').
narrative_ontology:cs_reading_relation('20201ebe-813e-4eb7-b70a-3d282c132806', derivative_work_statutory_boundary__coordination_reading, forecloses).
narrative_ontology:cs_reading_relation('20201ebe-813e-4eb7-b70a-3d282c132806', derivative_work_statutory_boundary__hybrid_carveout_reading, influences).
narrative_ontology:cs_axiom('20201ebe-813e-4eb7-b70a-3d282c132806', foundational, any_use_is_derivative).
narrative_ontology:cs_axiom_status(any_use_is_derivative, holdable).
narrative_ontology:cs_axiom_grounding('20201ebe-813e-4eb7-b70a-3d282c132806', any_use_is_derivative, conventional).
narrative_ontology:cs_axiom('20201ebe-813e-4eb7-b70a-3d282c132806', secondary, maximal_control_incentivizes_creation).
narrative_ontology:cs_axiom_status(maximal_control_incentivizes_creation, holdable).
narrative_ontology:cs_axiom_grounding('20201ebe-813e-4eb7-b70a-3d282c132806', maximal_control_incentivizes_creation, instrumental).
narrative_ontology:cs_reference_frame('20201ebe-813e-4eb7-b70a-3d282c132806', maximal_copyright_control_framework).
narrative_ontology:cs_drift_state('20201ebe-813e-4eb7-b70a-3d282c132806', contemporary_digital_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('20201ebe-813e-4eb7-b70a-3d282c132806', '').
narrative_ontology:cs_kernel_id(derivative_work_statutory_boundary__enclosure_reading, derivative_work_statutory_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__enclosure_reading, incumbent_copyright_holders).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__enclosure_reading, new_creators).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__enclosure_reading, ai_developers).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__enclosure_reading, public_domain_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively enforce broad derivative work claims, requiring licenses for any use of their copyrighted expression in new creations. They benefit from expanded revenue streams and control over downstream innovation, leveraging their existing portfolios.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, incumbent_copyright_holders, agenda_setter,
    institutional, generational, arbitrage, global).

% Face significant legal and financial hurdles to create new works that might incorporate or be inspired by existing copyrighted material. They must secure licenses pre-creation, often at prohibitive costs, or risk infringement lawsuits. Their creative freedom is heavily constrained.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, new_creators, payer,
    powerless, biographical, constrained, global).

% Are targeted by broad derivative work claims for training AI models on copyrighted data, even when the output is transformative. They face demands for licensing fees for data ingestion, which bottlenecks innovation in AI and concentrates control in incumbent hands.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, ai_developers, payer,
    moderate, biographical, constrained, global).

% Argue for a robust public domain and limited copyright terms, but their arguments are often sidelined by the expansive interpretation of derivative works. They see the enclosure reading as privatizing cultural commons and stifling creativity.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, public_domain_advocates, excluded,
    organized, generational, trapped, global).

% Are the ultimate arbiters and shapers of this boundary. Their rulings and statutory amendments either reinforce or challenge the enclosure reading, influencing the balance of power between incumbent rights holders and new creators.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, courts_and_legislatures, agenda_setter,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The enclosure reading attempts to coordinate the rights of original creators by providing a clear, broad scope of control over subsequent uses, aiming to incentivize creation by guaranteeing extensive protection.
% TRANSFER_FUNCTION: Transfers economic value and control over creative expression from new creators and innovators (who wish to build upon or transform existing works) to incumbent copyright holders, through licensing fees and the suppression of unauthorized uses.
% ABSENT_VOICES: The voices of future innovators, open-source advocates, and those who believe in a vibrant public domain are largely absent from the legislative and judicial processes that solidify this broad interpretation. They would argue for narrower definitions of derivative work to foster innovation and cultural remixing.
% DISAPPEARANCE_RATIONALE: If this broad interpretation of derivative work vanished overnight, there would be an immediate explosion of new creative works, AI models trained on vast datasets without licensing hurdles, and a significant shift in economic power away from incumbent copyright holders towards new creators. The entire information economy would reorganize around a more permissive creative commons.
% FOUNDING_PROBLEM: The original copyright statutes aimed to incentivize authors by granting them exclusive rights for a limited time, including control over adaptations and transformations of their work, to prevent free-riding on original creative effort.
% FOUNDING_PROBLEM_CORROBORATION: Incumbent copyright holders and their industry associations assert the problem is live, citing ongoing threats of unauthorized use and the need for strong incentives. New creators, AI developers, and legal scholars (outside the benefiting parties) argue that the problem has been over-solved, and the current interpretation stifles innovation more than it incentivizes original creation, turning a coordination mechanism into an extraction engine.
narrative_ontology:disappearance_verdict(derivative_work_statutory_boundary__enclosure_reading, world_rearranges).
narrative_ontology:founding_problem_status(derivative_work_statutory_boundary__enclosure_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(derivative_work_statutory_boundary__enclosure_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(derivative_work_statutory_boundary__enclosure_reading, 'none', 1).
narrative_ontology:epsilon_provenance(derivative_work_statutory_boundary__enclosure_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.85) is high because the broad definition of 'derivative work' allows incumbent copyright holders to demand licenses for a wide range of new creations, often at rates that do not reflect the marginal value added by the original work. Suppression (0.90) is severe, as the threat of litigation and the need for pre-emptive licensing effectively block many creative and innovative endeavors. The low theater ratio (0.10) indicates that the enforcement is highly functional in achieving its extractive goals, with little performative overhead. Accessibility collapse is high (0.75) because alternatives to licensing (e.g., fair use, public domain) are significantly narrowed under this reading, making it difficult for new creators to operate outside the incumbent's control. Resistance is moderate (0.40) but growing, as new creators and AI developers increasingly challenge this interpretation in courts and through advocacy.
 *
 * PERSPECTIVAL GAP:
 *   Incumbent copyright holders perceive this as a necessary 'rope' for incentivizing creation and protecting their investments, ensuring a stable revenue stream. New creators and AI developers, however, experience it as a 'snare' that stifles innovation and extracts rents, forcing them into costly licensing agreements or abandoning projects. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent copyright holders are clear beneficiaries (d=0.0-0.1) as they collect substantial revenue and maintain control. New creators and AI developers are targets (d=0.9-1.0) as they bear the costs of licensing and face suppressed innovation. Public domain advocates are excluded (d=0.9-1.0) as their arguments for a more open creative environment are systematically marginalized. Courts and legislatures, while agenda-setters, can lean towards either beneficiary or target depending on their interpretation, making their d value more central (d=0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling a highly extractive system as mere 'coordination.' While the original mandate was to incentivize creation, the enclosure reading has shifted its function towards rent extraction and control, indicating a potential mandatrophy where the original problem is over-solved and the constraint persists for the benefit of incumbents. The temporal measurements show a clear increase in extractiveness and suppression over time, supporting this analysis.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_transformative_use,
    'What degree of transformation is required for a new work to be considered non-derivative and non-infringing under this reading?',
    'Judicial clarification through landmark cases specifically addressing AI-generated content and highly transformative human-created works.',
    'A very high bar for transformation would reinforce the enclosure reading, increasing extraction. A lower bar would shift the boundary towards the coordination reading, reducing extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_transformative_use, empirical, 'Ambiguity in the legal definition of ''transformative use'' under the enclosure reading.').

omega_variable(
    economic_impact_on_innovation,
    'Does the broad derivative work boundary, as interpreted by the enclosure reading, genuinely incentivize more original creation, or does it stifle downstream innovation and economic growth?',
    'Longitudinal economic studies comparing innovation rates and creative output in jurisdictions with different derivative work interpretations, controlling for other factors.',
    'Empirical evidence of stifled innovation would weaken the legitimacy of the enclosure reading, potentially leading to legislative reform or judicial reinterpretation. Evidence of strong incentive effects would reinforce it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(economic_impact_on_innovation, empirical, 'Uncertainty about the net economic effect of broad derivative work protection on overall innovation.').

omega_variable(
    kernel_framing_ambiguity,
    'Is this constraint a genuine interpretation of the original statutory intent, or a re-framing of copyright law to serve incumbent interests?',
    'Historical legal analysis of legislative intent, combined with contemporary economic analysis of lobbying efforts and judicial appointments.',
    'If framed as a re-framing for incumbent interests, the constraint''s legitimacy would be significantly undermined, supporting reclassification towards a pure snare. If genuinely aligned with original intent, it would strengthen the ''tangled rope'' aspect (coordination with extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_ambiguity, conceptual, 'Ambiguity regarding the true grounding of the enclosure reading – original intent vs. strategic re-framing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(derivative_work_statutory_boundary__enclosure_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deri_tr_t0, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(deri_tr_t5, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement(deri_tr_t10, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(deri_tr_t15, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 15, 0.09).
narrative_ontology:measurement(deri_tr_t20, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(deri_be_t0, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(deri_be_t5, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 5, 0.75).
narrative_ontology:measurement(deri_be_t10, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 10, 0.8).
narrative_ontology:measurement(deri_be_t15, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 15, 0.83).
narrative_ontology:measurement(deri_be_t20, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 20, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(deri_su_t0, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(deri_su_t5, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 5, 0.8).
narrative_ontology:measurement(deri_su_t10, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 10, 0.85).
narrative_ontology:measurement(deri_su_t15, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 15, 0.88).
narrative_ontology:measurement(deri_su_t20, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 20, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(derivative_work_statutory_boundary__enclosure_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__enclosure_reading, copyright_term_extension).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__enclosure_reading, fair_use_doctrine_application).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__enclosure_reading, ai_generated_content_ownership).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'derivative_work_statutory_boundary' kernel. Its siblings are 'coordination_reading' and 'hybrid_carveout_reading', which offer alternative interpretations of the same statutory language with different structural outcomes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
