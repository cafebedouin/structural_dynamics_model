% ============================================================================
% CONSTRAINT STORY: derivative_work_statutory_boundary__coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_derivative_work_statutory_boundary__coordination_reading, []).

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
 *   constraint_id: derivative_work_statutory_boundary__coordination_reading
 *   human_readable: Derivative Work Statutory Boundary (Coordination Reading)
 *   domain: intellectual_property/technology_governance/information_economics
 *
 * SUMMARY:
 *   This constraint represents a 'coordination reading' of the derivative
 *   work statutory boundary in intellectual property law. It posits that only
 *   fixed recastings substantially incorporating original expression are
 *   derivative works, while transformative and intermediate uses (e.g., AI
 *   model training) are non-infringing. This reading aims to foster
 *   innovation and the 'progress of science and arts' by minimizing ex-ante
 *   licensing requirements for new creative and technological endeavors. It
 *   functions as a coordination mechanism, providing clear rules for what
 *   constitutes permissible reuse without permission.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(derivative_work_statutory_boundary__coordination_reading, 0.15).
domain_priors:suppression_score(derivative_work_statutory_boundary__coordination_reading, 0.2).
domain_priors:theater_ratio(derivative_work_statutory_boundary__coordination_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(derivative_work_statutory_boundary__coordination_reading, rope).
narrative_ontology:human_readable(derivative_work_statutory_boundary__coordination_reading, "Derivative Work Statutory Boundary (Coordination Reading)").
narrative_ontology:topic_domain(derivative_work_statutory_boundary__coordination_reading, "intellectual_property/technology_governance/information_economics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(derivative_work_statutory_boundary__coordination_reading, '97ad2b93-732f-4a3a-af0e-bc721f1988f1').
narrative_ontology:cs_kernel_codification('97ad2b93-732f-4a3a-af0e-bc721f1988f1', formalized).
narrative_ontology:cs_authority_grounding('97ad2b93-732f-4a3a-af0e-bc721f1988f1', lineage).
narrative_ontology:cs_interpretation_layer_present('97ad2b93-732f-4a3a-af0e-bc721f1988f1').
narrative_ontology:cs_reading_relation('97ad2b93-732f-4a3a-af0e-bc721f1988f1', derivative_work_statutory_boundary__enclosure_reading, coexists_with).
narrative_ontology:cs_reading_relation('97ad2b93-732f-4a3a-af0e-bc721f1988f1', derivative_work_statutory_boundary__hybrid_carveout_reading, coexists_with).
narrative_ontology:cs_axiom('97ad2b93-732f-4a3a-af0e-bc721f1988f1', foundational, progress_of_science_and_arts_priority).
narrative_ontology:cs_axiom_status(progress_of_science_and_arts_priority, holdable).
narrative_ontology:cs_axiom_grounding('97ad2b93-732f-4a3a-af0e-bc721f1988f1', progress_of_science_and_arts_priority, deontological).
narrative_ontology:cs_axiom('97ad2b93-732f-4a3a-af0e-bc721f1988f1', foundational, intermediate_use_non_expressive).
narrative_ontology:cs_axiom_status(intermediate_use_non_expressive, holdable).
narrative_ontology:cs_axiom_grounding('97ad2b93-732f-4a3a-af0e-bc721f1988f1', intermediate_use_non_expressive, conventional).
narrative_ontology:cs_reference_frame('97ad2b93-732f-4a3a-af0e-bc721f1988f1', fair_use_as_innovation_catalyst).
narrative_ontology:cs_drift_state('97ad2b93-732f-4a3a-af0e-bc721f1988f1', contemporary_ai_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('97ad2b93-732f-4a3a-af0e-bc721f1988f1', '').
narrative_ontology:cs_kernel_id(derivative_work_statutory_boundary__coordination_reading, derivative_work_statutory_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__coordination_reading, transformative_creators).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__coordination_reading, ai_developers).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__coordination_reading, public_domain_users).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__coordination_reading, original_copyright_holders).
narrative_ontology:constraint_vindicates(derivative_work_statutory_boundary__coordination_reading, fair_use_doctrine).
narrative_ontology:constraint_vindicates(derivative_work_statutory_boundary__coordination_reading, progress_of_science_and_arts).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These creators build new works that comment on, critique, or significantly alter existing copyrighted material. This reading allows them to operate without seeking licenses for intermediate or transformative uses, fostering innovation.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, transformative_creators, beneficiary,
    moderate, biographical, mobile, global).

% Developers of generative AI models rely on this interpretation to train their models on large datasets of copyrighted works without incurring prohibitive licensing costs or legal risks, viewing training as an intermediate, non-expressive use.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, ai_developers, beneficiary,
    organized, generational, mobile, global).

% Holders of original copyrights, particularly those whose works are used in AI training or transformative projects, may perceive a loss of control or potential revenue. This reading limits their ability to assert derivative work rights over such uses.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, original_copyright_holders, payer,
    powerful, generational, constrained, global).

% The public benefits from a richer cultural commons and accelerated innovation when new works can be built upon existing ones without undue friction. This reading expands the effective public domain for certain uses.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, public_domain_users, beneficiary,
    powerless, civilizational, arbitrage, universal).

% These institutions interpret and shape the statutory boundary through case law and legislative amendments. This reading reflects a policy choice to prioritize innovation and access over maximal control for original copyright holders.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, courts_and_legislatures, agenda_setter,
    institutional, generational, analytical, national).

% Advocates for broader derivative work rights, often representing traditional content industries, are structurally excluded from the full scope of their desired control under this reading. They would argue for ex-ante licensing for any use of copyrighted expression.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, enclosure_advocates, excluded,
    organized, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the boundary between original and derivative works, providing clarity for creators and innovators on when new uses of copyrighted material require authorization versus when they are permissible without it. This fosters a predictable environment for creative reuse and technological development.
% TRANSFER_FUNCTION: This reading facilitates the free flow of creative and informational assets for transformative and intermediate uses, effectively transferring the 'right to reuse without permission' from original copyright holders to transformative creators and AI developers, in exchange for broader public benefit and innovation.
% ABSENT_VOICES: Advocates for maximal copyright control and ex-ante licensing for all uses of copyrighted expression are marginalized by this reading. They would argue that any use of their work, even for training or transformative purposes, diminishes their control and potential revenue, and should require explicit permission.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished, replaced by a maximalist view, the generative AI industry would face immediate legal challenges and licensing demands, potentially halting development. Transformative art forms would be stifled, and the flow of information for new creation would be severely restricted, fundamentally altering the digital economy and creative landscape.
% FOUNDING_PROBLEM: The original copyright statutes aimed to balance incentivizing creation with promoting the 'progress of science and useful arts' by defining the scope of exclusive rights, including derivative works, to prevent unauthorized exploitation while allowing for new creation.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars and technology policy experts, independent of specific industry beneficiaries, corroborate that the tension between incentivizing original creation and fostering new innovation remains a live and evolving problem, particularly with the advent of new technologies like AI. Court decisions and legislative debates reflect this ongoing challenge.
narrative_ontology:disappearance_verdict(derivative_work_statutory_boundary__coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(derivative_work_statutory_boundary__coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(derivative_work_statutory_boundary__coordination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(derivative_work_statutory_boundary__coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(derivative_work_statutory_boundary__coordination_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(derivative_work_statutory_boundary__coordination_reading_tests).
:- end_tests(derivative_work_statutory_boundary__coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low (0.15) because this reading minimizes the 'tax' on new creation, allowing broad reuse without direct payment. Suppression is also low (0.2) as it actively enables, rather than restricts, certain uses, requiring minimal enforcement to hold. Theater ratio is negligible (0.05) as the constraint's function is direct and clear. Accessibility collapse is moderate (0.3) because while it opens up many avenues, some uses still require permission. Resistance is low (0.1) from beneficiaries, but higher from original copyright holders who advocate for a more restrictive interpretation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of transformative creators and AI developers, this is a clear Rope, enabling innovation. From the perspective of original copyright holders, it might feel more extractive, as it limits their ability to monetize or control certain uses of their work. The engine's classification will reflect the overall structural properties, but the subjective experience of 'paying' versus 'benefiting' will differ significantly.
 *
 * DIRECTIONALITY LOGIC:
 *   Transformative creators, AI developers, and the public are clear beneficiaries, gaining freedom to operate and access to a richer cultural commons. Original copyright holders are the primary payers, as their control over certain uses of their work is limited. Courts and legislatures act as agenda-setters, shaping this boundary. Enclosure advocates are excluded, as their preferred interpretation is not adopted.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_transformative_use,
    'How broadly should ''transformative use'' be interpreted, especially concerning AI-generated outputs that might closely resemble original works?',
    'Further judicial clarification through landmark cases specifically addressing the ''transformative'' nature of AI outputs, or legislative action to define ''transformative'' in the context of generative AI.',
    'A narrow interpretation could increase extractiveness for AI developers and transformative creators, pushing the constraint towards a Tangled Rope or Snare. A broad interpretation would reinforce its Rope-like qualities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_transformative_use, conceptual, 'Ambiguity in the definition of ''transformative use'' for new technologies.').

omega_variable(
    economic_impact_on_original_creators,
    'Does this reading''s allowance of intermediate/transformative uses without licensing genuinely harm the economic incentives of original copyright holders, or does it foster a larger creative ecosystem that indirectly benefits them?',
    'Longitudinal economic studies comparing creator income and industry growth in jurisdictions with different derivative work interpretations, and analysis of new revenue streams enabled by transformative uses.',
    'If significant harm is demonstrated, pressure for legislative change could increase, potentially shifting the constraint towards a more extractive model for new creators. If indirect benefits are shown, the Rope classification is strengthened.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(economic_impact_on_original_creators, empirical, 'Uncertainty about the long-term economic effects on original creators.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(derivative_work_statutory_boundary__coordination_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deri_tr_t0, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(deri_tr_t5, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 5, 0.05).
narrative_ontology:measurement(deri_tr_t10, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 10, 0.05).
narrative_ontology:measurement(deri_tr_t15, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 15, 0.05).
narrative_ontology:measurement(deri_tr_t20, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 20, 0.05).

% Extraction over time
narrative_ontology:measurement(deri_be_t0, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(deri_be_t5, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 5, 0.15).
narrative_ontology:measurement(deri_be_t10, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 10, 0.15).
narrative_ontology:measurement(deri_be_t15, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 15, 0.15).
narrative_ontology:measurement(deri_be_t20, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 20, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(deri_su_t0, derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(deri_su_t5, derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 5, 0.2).
narrative_ontology:measurement(deri_su_t10, derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 10, 0.2).
narrative_ontology:measurement(deri_su_t15, derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 15, 0.2).
narrative_ontology:measurement(deri_su_t20, derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 20, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(derivative_work_statutory_boundary__coordination_reading, information_standard).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__coordination_reading, ai_training_data_licensing).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__coordination_reading, generative_ai_liability_framework).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'derivative_work_statutory_boundary' kernel. This 'coordination_reading' emphasizes innovation and broad reuse, contrasting with the 'enclosure_reading' (maximal control) and 'hybrid_carveout_reading' (commercial vs. non-commercial distinction).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
