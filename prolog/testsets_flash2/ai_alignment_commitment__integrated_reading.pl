% ============================================================================
% CONSTRAINT STORY: ai_alignment_commitment__integrated_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_alignment_commitment__integrated_reading, []).

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
 *   constraint_id: ai_alignment_commitment__integrated_reading
 *   human_readable: AI Alignment: Integrated Control and Justice Approach
 *   domain: ai_governance/technology_ethics/risk_assessment
 *
 * SUMMARY:
 *   This constraint represents the 'integrated reading' of AI alignment,
 *   which posits that effective alignment requires simultaneous attention to
 *   both control problems (e.g., existential risk, loss of control) and
 *   justice problems (e.g., bias, discrimination, equitable access). It
 *   rejects the false dichotomy often drawn between these two concerns. The
 *   constraint is a Tangled Rope because it genuinely coordinates disparate
 *   research efforts towards a common goal, but also extracts from those who
 *   prefer siloed approaches by forcing them to broaden their scope and
 *   integrate new considerations. The victim set includes both present
 *   marginalized populations (who suffer from unjust AI) and future humanity
 *   (who face risks from uncontrollable AI).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_commitment__integrated_reading, 0.65).
domain_priors:suppression_score(ai_alignment_commitment__integrated_reading, 0.7).
domain_priors:theater_ratio(ai_alignment_commitment__integrated_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_commitment__integrated_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(ai_alignment_commitment__integrated_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(ai_alignment_commitment__integrated_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_commitment__integrated_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ai_alignment_commitment__integrated_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_commitment__integrated_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_commitment__integrated_reading, "AI Alignment: Integrated Control and Justice Approach").
narrative_ontology:topic_domain(ai_alignment_commitment__integrated_reading, "ai_governance/technology_ethics/risk_assessment").

domain_priors:requires_active_enforcement(ai_alignment_commitment__integrated_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_commitment__integrated_reading, '6c031c90-8757-4a72-875e-0941def3f27f').
narrative_ontology:cs_kernel_codification('6c031c90-8757-4a72-875e-0941def3f27f', distributed).
narrative_ontology:cs_authority_grounding('6c031c90-8757-4a72-875e-0941def3f27f', expertise).
narrative_ontology:cs_interpretation_layer_present('6c031c90-8757-4a72-875e-0941def3f27f').
narrative_ontology:cs_reading_relation('6c031c90-8757-4a72-875e-0941def3f27f', ai_alignment_commitment__safety_control_reading, influences).
narrative_ontology:cs_reading_relation('6c031c90-8757-4a72-875e-0941def3f27f', ai_alignment_commitment__ethics_justice_reading, influences).
narrative_ontology:cs_axiom('6c031c90-8757-4a72-875e-0941def3f27f', foundational, control_and_justice_are_non_exclusive).
narrative_ontology:cs_axiom_status(control_and_justice_are_non_exclusive, holdable).
narrative_ontology:cs_axiom_grounding('6c031c90-8757-4a72-875e-0941def3f27f', control_and_justice_are_non_exclusive, deontological).
narrative_ontology:cs_axiom('6c031c90-8757-4a72-875e-0941def3f27f', secondary, fragmentation_undermines_alignment).
narrative_ontology:cs_axiom_status(fragmentation_undermines_alignment, holdable).
narrative_ontology:cs_axiom_grounding('6c031c90-8757-4a72-875e-0941def3f27f', fragmentation_undermines_alignment, empirically_contingent).
narrative_ontology:cs_reference_frame('6c031c90-8757-4a72-875e-0941def3f27f', holistic_ai_risk_management).
narrative_ontology:cs_drift_state('6c031c90-8757-4a72-875e-0941def3f27f', contemporary, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('6c031c90-8757-4a72-875e-0941def3f27f', '').
narrative_ontology:cs_kernel_id(ai_alignment_commitment__integrated_reading, ai_alignment_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__integrated_reading, integrated_alignment_researchers).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__integrated_reading, future_humanity).
narrative_ontology:constraint_victim(ai_alignment_commitment__integrated_reading, siloed_safety_researchers).
narrative_ontology:constraint_victim(ai_alignment_commitment__integrated_reading, siloed_ethics_researchers).
narrative_ontology:constraint_victim(ai_alignment_commitment__integrated_reading, marginalized_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__integrated_reading, marginalized_populations).
narrative_ontology:constraint_victim(ai_alignment_commitment__integrated_reading, ai_developers_and_companies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocates for and develops methods that simultaneously address AI control problems (e.g., existential risk) and justice problems (e.g., bias, discrimination). Benefits from a holistic approach that avoids trade-offs between these concerns, but faces resistance from those committed to siloed approaches.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, integrated_alignment_researchers, agenda_setter,
    organized, generational, constrained, global).

% Focuses primarily on preventing catastrophic loss of control over advanced AI systems, often prioritizing this over immediate social justice concerns. Bears the cost of needing to integrate justice considerations into their frameworks, which they may perceive as diluting their primary mission or adding complexity.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, siloed_safety_researchers, payer,
    powerful, generational, constrained, global).

% Focuses primarily on preventing reproduction of social bias and present-day harm in AI systems, often prioritizing this over speculative future control problems. Bears the cost of needing to integrate control considerations, which they may perceive as abstract or diverting resources from immediate harms.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, siloed_ethics_researchers, payer,
    powerful, biographical, constrained, global).

% Are direct beneficiaries of an integrated approach that addresses both the potential for future catastrophic harm and the immediate, ongoing harms of biased or unjust AI systems. Without this integration, their concerns are often fragmented or deprioritized.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, marginalized_populations, beneficiary,
    powerless, immediate, trapped, local).

% Benefits from an approach that seeks to ensure AI systems are both safe and just, preventing both existential risks and the perpetuation of systemic injustices across generations. This seat is a conceptual beneficiary, representing the long-term interests of all people.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, future_humanity, beneficiary,
    powerless, civilizational, trapped, universal).

% Are pressured to adopt integrated alignment practices, which adds complexity and cost to their development pipelines. They benefit from increased public trust and reduced regulatory risk, but face the challenge of implementing comprehensive solutions.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, ai_developers_and_companies, payer,
    institutional, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates diverse research communities (AI safety, ethics, social justice) to work on a unified framework for AI alignment, preventing the fragmentation of effort and the creation of AI systems that are safe but unjust, or just but uncontrollable.
% TRANSFER_FUNCTION: Transfers intellectual resources, funding, and attention from siloed research agendas towards integrated approaches. It also transfers the burden of comprehensive risk assessment and mitigation onto AI developers.
% ABSENT_VOICES: Purely profit-driven AI development interests, who would prefer minimal constraints on either safety or ethics, are excluded from setting the agenda for alignment research. They would argue for faster deployment over comprehensive alignment.
% DISAPPEARANCE_RATIONALE: If the commitment to integrated alignment vanished, research and development would likely revert to siloed approaches, leading to AI systems that are either optimized for control at the expense of justice, or vice versa. This would result in a fragmented and less robust approach to AI risk, with significant negative consequences for both present and future populations.
% FOUNDING_PROBLEM: The fragmentation of AI alignment research into separate 'safety' (control) and 'ethics' (justice) camps, leading to a false dichotomy and incomplete solutions that risked either uncontrollable or unjust AI systems.
% FOUNDING_PROBLEM_CORROBORATION: Leading interdisciplinary AI ethics and safety scholars, as well as reports from international AI governance bodies, corroborate that the problem of fragmentation and false dichotomies remains live, despite growing recognition of the need for integration. This is attested by the continued existence of separate conferences and funding streams for each sub-field.
narrative_ontology:disappearance_verdict(ai_alignment_commitment__integrated_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_alignment_commitment__integrated_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_commitment__integrated_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(ai_alignment_commitment__integrated_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_alignment_commitment__integrated_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_alignment_commitment__integrated_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_alignment_commitment__integrated_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_alignment_commitment__integrated_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) reflects the cost imposed on siloed researchers and developers to integrate broader concerns, diverting resources and intellectual effort from their narrower focus. Suppression (0.70) is high because the integrated approach actively pushes back against and seeks to delegitimize purely siloed perspectives, requiring active enforcement of interdisciplinary norms and funding priorities. Theater ratio (0.20) is relatively low, as the commitment to integration is largely genuine, though some performative 'integration' may occur without deep structural change. Accessibility collapse (0.40) is moderate, as siloed approaches are still possible but face increasing pressure and reduced legitimacy. Resistance (0.55) is significant from those invested in existing, narrower paradigms.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of integrated researchers, this is a necessary coordination to prevent catastrophic outcomes. From the perspective of siloed researchers, it is an extractive demand that dilutes their focus and imposes unnecessary complexity. The engine's classification will reflect this divergence based on the declared roles and metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   Integrated alignment researchers and both present marginalized populations and future humanity are beneficiaries (d near 0.0), as this approach directly serves their interests. Siloed safety and ethics researchers, as well as AI developers, are payers (d near 1.0), as they bear the costs of adapting to a more complex, integrated mandate. The constraint subsidizes the holistic view by making siloed approaches less viable.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    integration_feasibility,
    'Is genuine, deep integration of AI control and justice concerns practically feasible within current research paradigms and institutional structures, or does it remain largely aspirational?',
    'Empirical studies of interdisciplinary research outcomes, funding allocations, and the actual design and deployment of AI systems claiming ''integrated alignment.'' If integration remains superficial, reclassify as higher theater.',
    'If integration is not feasible, the constraint''s coordination function is weaker, and its extractiveness from siloed approaches may be less justified, potentially shifting it towards a Snare for those forced to comply performatively.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(integration_feasibility, empirical, 'Practical feasibility of integrated alignment.').

omega_variable(
    false_dichotomy_validity,
    'Is the ''false dichotomy'' between AI control and justice problems truly false, or are there genuine, irreducible trade-offs that an integrated approach attempts to paper over?',
    'Conceptual analysis and case studies where specific alignment interventions for control conflict directly with justice outcomes, and vice versa, without clear resolution.',
    'If genuine trade-offs exist, the integrated reading''s foundational axiom (non-exclusivity) is challenged, potentially weakening its legitimacy and increasing resistance from those who see the trade-offs as real.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_dichotomy_validity, conceptual, 'Validity of the ''false dichotomy'' claim.').

omega_variable(
    victim_set_scope,
    'To what extent do ''siloed safety researchers'' and ''siloed ethics researchers'' genuinely act as victims of this integrated approach, rather than simply being challenged to evolve their perspectives?',
    'Analysis of career trajectories, funding opportunities, and publication trends for researchers who resist integration versus those who embrace it. If resistance leads to significant professional penalties, the victim status is stronger.',
    'If these groups are not genuinely victims, the constraint''s extractiveness is lower, and its classification might shift towards a Rope, as the coordination function dominates over asymmetric costs.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(victim_set_scope, empirical, 'Degree to which siloed researchers are ''victims'' vs. ''challenged''.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_commitment__integrated_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_a_tr_t0, ai_alignment_commitment__integrated_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(ai_a_tr_t5, ai_alignment_commitment__integrated_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement(ai_a_tr_t10, ai_alignment_commitment__integrated_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(ai_a_tr_t15, ai_alignment_commitment__integrated_reading, theater_ratio, 15, 0.18).
narrative_ontology:measurement(ai_a_tr_t20, ai_alignment_commitment__integrated_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(ai_a_be_t0, ai_alignment_commitment__integrated_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(ai_a_be_t5, ai_alignment_commitment__integrated_reading, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(ai_a_be_t10, ai_alignment_commitment__integrated_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(ai_a_be_t15, ai_alignment_commitment__integrated_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(ai_a_be_t20, ai_alignment_commitment__integrated_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(ai_a_su_t0, ai_alignment_commitment__integrated_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(ai_a_su_t5, ai_alignment_commitment__integrated_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(ai_a_su_t10, ai_alignment_commitment__integrated_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(ai_a_su_t15, ai_alignment_commitment__integrated_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(ai_a_su_t20, ai_alignment_commitment__integrated_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_commitment__integrated_reading, identity_coordination).
narrative_ontology:affects_constraint(ai_alignment_commitment__integrated_reading, ai_ethics_guidelines).
narrative_ontology:affects_constraint(ai_alignment_commitment__integrated_reading, ai_safety_standards).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
