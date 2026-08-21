% ============================================================================
% CONSTRAINT STORY: ai_safety_commitment__dual_priority_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_safety_commitment__dual_priority_reading, []).

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
 *   constraint_id: ai_safety_commitment__dual_priority_reading
 *   human_readable: AI Safety: Dual Priority (Existential & Near-Term Harms)
 *   domain: ai_safety/technology_governance/risk_assessment
 *
 * SUMMARY:
 *   This constraint represents one reading of the 'AI safety commitment'
 *   kernel, specifically the 'dual priority' reading. It asserts that AI
 *   safety requires addressing both existential risk and near-term harms as
 *   non-competing priorities. This reading attempts to unify the field but
 *   faces challenges in practical resource allocation and risks diluting
 *   focus. The metrics reflect the inherent difficulty in maintaining this
 *   broad mandate without some level of extraction from those who prefer a
 *   more focused approach.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_safety_commitment__dual_priority_reading, 0.45).
domain_priors:suppression_score(ai_safety_commitment__dual_priority_reading, 0.3).
domain_priors:theater_ratio(ai_safety_commitment__dual_priority_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_safety_commitment__dual_priority_reading, rope).
narrative_ontology:human_readable(ai_safety_commitment__dual_priority_reading, "AI Safety: Dual Priority (Existential & Near-Term Harms)").
narrative_ontology:topic_domain(ai_safety_commitment__dual_priority_reading, "ai_safety/technology_governance/risk_assessment").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_safety_commitment__dual_priority_reading, '00cd3804-71e4-4477-9a87-cbaf882df445').
narrative_ontology:cs_kernel_codification('00cd3804-71e4-4477-9a87-cbaf882df445', distributed).
narrative_ontology:cs_authority_grounding('00cd3804-71e4-4477-9a87-cbaf882df445', practice).
narrative_ontology:cs_interpretation_layer_present('00cd3804-71e4-4477-9a87-cbaf882df445').
narrative_ontology:cs_reading_relation('00cd3804-71e4-4477-9a87-cbaf882df445', ai_safety_commitment__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('00cd3804-71e4-4477-9a87-cbaf882df445', ai_safety_commitment__near_term_harms_reading, coexists_with).
narrative_ontology:cs_axiom('00cd3804-71e4-4477-9a87-cbaf882df445', foundational, all_ai_risks_must_be_addressed).
narrative_ontology:cs_axiom_status(all_ai_risks_must_be_addressed, holdable).
narrative_ontology:cs_axiom_grounding('00cd3804-71e4-4477-9a87-cbaf882df445', all_ai_risks_must_be_addressed, deontological).
narrative_ontology:cs_axiom('00cd3804-71e4-4477-9a87-cbaf882df445', foundational, risk_categories_are_interdependent).
narrative_ontology:cs_axiom_status(risk_categories_are_interdependent, holdable).
narrative_ontology:cs_axiom_grounding('00cd3804-71e4-4477-9a87-cbaf882df445', risk_categories_are_interdependent, empirically_contingent).
narrative_ontology:cs_reference_frame('00cd3804-71e4-4477-9a87-cbaf882df445', unified_ai_safety_agenda).
narrative_ontology:cs_drift_state('00cd3804-71e4-4477-9a87-cbaf882df445', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('00cd3804-71e4-4477-9a87-cbaf882df445', '').
narrative_ontology:cs_kernel_id(ai_safety_commitment__dual_priority_reading, ai_safety_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_safety_commitment__dual_priority_reading, ai_safety_researchers).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__dual_priority_reading, public_trust_advocates).
narrative_ontology:constraint_victim(ai_safety_commitment__dual_priority_reading, resource_constrained_researchers).
narrative_ontology:constraint_victim(ai_safety_commitment__dual_priority_reading, policy_makers_with_limited_bandwidth).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from a broader mandate for AI safety, allowing for funding and attention to both long-term and short-term issues. However, faces internal and external pressure to prioritize, leading to resource allocation challenges.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, ai_safety_researchers, beneficiary,
    organized, generational, constrained, global).

% Benefits from the commitment to address immediate harms, which builds public trust and engagement. Also supports long-term safety as a necessary condition for sustainable AI development.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, public_trust_advocates, beneficiary,
    moderate, biographical, mobile, national).

% Struggles to secure funding and attention for specific, often under-resourced, research areas when the overall mandate is broad and diffuse. Faces pressure to align with both priorities, potentially diluting focus.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, resource_constrained_researchers, payer,
    powerless, immediate, constrained, local).

% Finds it challenging to craft coherent and actionable policy when faced with two distinct, complex, and potentially competing sets of priorities. Resource allocation for policy implementation becomes difficult.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, policy_makers_with_limited_bandwidth, payer,
    institutional, biographical, constrained, national).

% Would argue that focusing on near-term harms diverts critical resources from the truly existential threat, potentially increasing overall risk. Their perspective is marginalized by the dual-priority framing.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, existential_risk_only_advocates, excluded,
    organized, generational, identity_locked, global).

% Would argue that focusing on speculative existential risks diverts resources from addressing immediate, demonstrable suffering and injustice caused by AI today. Their perspective is also marginalized by the dual-priority framing.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, near_term_harms_only_advocates, excluded,
    organized, biographical, identity_locked, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate the diverse AI safety community by creating a shared, inclusive mandate that acknowledges the importance of both long-term and short-term risks, preventing fragmentation and internecine conflict.
% TRANSFER_FUNCTION: Transfers legitimacy and resources to both existential risk and near-term harms research and policy efforts, from a general pool of attention and funding for AI safety.
% ABSENT_VOICES: Advocates for a singular focus on either existential risk or near-term harms are structurally excluded from the 'dual priority' framing, as their core premise is that the priorities ARE competing. They would argue for a more focused, less diluted approach.
% DISAPPEARANCE_RATIONALE: If this dual-priority commitment vanished, the AI safety field would likely fracture into two distinct, potentially adversarial, camps: one focused solely on existential risk, the other on near-term harms. Funding, research, and policy efforts would become highly siloed and competitive, leading to a less coherent overall approach to AI safety.
% FOUNDING_PROBLEM: The AI safety field was becoming polarized between those focused on long-term existential risks and those focused on immediate, demonstrable harms, leading to internal conflict and inefficient resource allocation.
% FOUNDING_PROBLEM_CORROBORATION: Many leading AI safety organizations and independent policy analysts attest that the polarization remains a live issue, and that the dual-priority framing is an ongoing attempt to bridge this divide. However, some critics from both 'camps' argue that the framing merely papers over fundamental disagreements without resolving them.
narrative_ontology:disappearance_verdict(ai_safety_commitment__dual_priority_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_safety_commitment__dual_priority_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_safety_commitment__dual_priority_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(ai_safety_commitment__dual_priority_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_safety_commitment__dual_priority_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_safety_commitment__dual_priority_reading_tests).
:- end_tests(ai_safety_commitment__dual_priority_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45) because while the dual-priority framing aims for coordination, it implicitly extracts from those who believe the priorities are, in fact, competing, by forcing a broader focus. Suppression is low (0.3) as there's no direct coercion, but a strong social pressure to conform to the dual-priority narrative. Theater ratio is low (0.2) as the commitment is genuinely pursued, but some efforts might be more performative in balancing both sides than truly effective. The values are stable, reflecting the ongoing, rather than rapidly changing, nature of this conceptual constraint.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the beneficiaries, this constraint is a necessary Rope for coordinating a complex field. From the perspective of the payers, it can feel like a Tangled Rope, as the broad mandate creates resource and focus extraction. The engine's per-seat classification will capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   AI safety researchers and public trust advocates are beneficiaries as this reading broadens their mandate and legitimacy. Resource-constrained researchers and policy makers with limited bandwidth are payers, as they bear the cost of trying to implement a broad, potentially diffuse, agenda. Advocates for single-focus approaches (existential-risk-only or near-term-harms-only) are excluded, as their core premise is incompatible with this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    resource_allocation_coherence,
    'Can resources be effectively allocated across both existential risk and near-term harms without one implicitly competing with or diluting the other?',
    'Empirical studies of resource allocation in AI safety organizations: track funding, personnel, and project outcomes to see if dual-priority mandates lead to coherent, effective action on both fronts, or if one consistently dominates or suffers.',
    'If resources cannot be coherently allocated, the ''non-competing'' premise of this reading is undermined, potentially reclassifying it as a Snare for those whose priorities are consistently under-resourced. If coherence is demonstrated, it strengthens the Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_allocation_coherence, empirical, 'Whether the ''non-competing'' premise holds in practice for resource allocation.').

omega_variable(
    conceptual_coherence_of_dual_priority,
    'Is the conceptual framing of ''non-competing priorities'' genuinely coherent, or does it mask a fundamental tension between different risk paradigms?',
    'Philosophical and ethical analysis of risk frameworks: examine whether the underlying assumptions and methodologies for addressing existential vs. near-term risks are truly compatible or if they require distinct, potentially conflicting, approaches.',
    'If the framing is conceptually incoherent, this reading''s legitimacy as a unifying force is weakened, potentially leading to a reclassification towards Tangled Rope or even Snare, as it would be seen as forcing an unnatural synthesis. If coherent, it reinforces the Rope classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(conceptual_coherence_of_dual_priority, conceptual, 'Conceptual coherence of the ''non-competing priorities'' framing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_safety_commitment__dual_priority_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_s_tr_t0, ai_safety_commitment__dual_priority_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(ai_s_tr_t5, ai_safety_commitment__dual_priority_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement(ai_s_tr_t10, ai_safety_commitment__dual_priority_reading, theater_ratio, 10, 0.2).

% Extraction over time
narrative_ontology:measurement(ai_s_be_t0, ai_safety_commitment__dual_priority_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(ai_s_be_t5, ai_safety_commitment__dual_priority_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(ai_s_be_t10, ai_safety_commitment__dual_priority_reading, base_extractiveness, 10, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(ai_s_su_t0, ai_safety_commitment__dual_priority_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(ai_s_su_t5, ai_safety_commitment__dual_priority_reading, suppression_requirement, 5, 0.28).
narrative_ontology:measurement(ai_s_su_t10, ai_safety_commitment__dual_priority_reading, suppression_requirement, 10, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_safety_commitment__dual_priority_reading, identity_coordination).
narrative_ontology:affects_constraint(ai_safety_commitment__dual_priority_reading, ai_safety_commitment__existential_risk_reading).
narrative_ontology:affects_constraint(ai_safety_commitment__dual_priority_reading, ai_safety_commitment__near_term_harms_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'AI safety commitment' kernel. It attempts to bridge the 'existential_risk_reading' and 'near_term_harms_reading' by asserting both are non-competing priorities. The other readings represent singular focus approaches.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
