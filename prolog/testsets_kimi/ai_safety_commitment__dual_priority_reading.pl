% ============================================================================
% CONSTRAINT STORY: ai_safety_commitment__dual_priority_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   human_readable: AI Safety Dual-Priority Commitment (Non-Competing Priorities Reading)
 *   domain: AI Safety / Technology Governance / Risk Assessment
 *
 * SUMMARY:
 *   This constraint story captures the 'dual-priority reading' of the
 *   contested AI safety commitment kernel: the claim that existential risk
 *   and near-term harms must be addressed as non-competing priorities. The
 *   constraint functions as a field-governance mechanism that coordinates two
 *   potentially fractious communities under a unified umbrella, but under
 *   resource scarcity it produces asymmetric extraction through dilution,
 *   tokenization, and the marginalization of single-priority advocacy. It is
 *   claimed as tangled_rope because it possesses a genuine coordination
 *   function (field unity) alongside identifiable victim populations who bear
 *   the costs of the coherence challenge.
 *
 * KEY AGENTS:
 *   - Dual-priority brokers (agenda_setter/institutional): administer the commitment through funding vehicles and policy venues; their centrality depends on maintaining the non-competing frame.
 *   - X-risk researchers (payer/powerful): bear costs of diluted attention and constrained argumentation for extinction-risk primacy.
 *   - Near-term advocates (payer/organized): bear costs of tokenization and inadequate resourcing despite rhetorical inclusion.
 *   - Present-harm communities (payer/powerless): bear the direct costs of under-resourced algorithmic accountability.
 *   - Future generations (payer/powerless/non-agent): bear the cost of underfunded alignment research.
 *   - Field analysts (observer/analytical): track whether the frame produces substantive integration or performative inclusion.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_safety_commitment__dual_priority_reading, 0.7).
domain_priors:suppression_score(ai_safety_commitment__dual_priority_reading, 0.63).
domain_priors:theater_ratio(ai_safety_commitment__dual_priority_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, suppression_requirement, 0.63).
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_safety_commitment__dual_priority_reading, tangled_rope).
narrative_ontology:human_readable(ai_safety_commitment__dual_priority_reading, "AI Safety Dual-Priority Commitment (Non-Competing Priorities Reading)").
narrative_ontology:topic_domain(ai_safety_commitment__dual_priority_reading, "AI Safety / Technology Governance / Risk Assessment").

domain_priors:requires_active_enforcement(ai_safety_commitment__dual_priority_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_safety_commitment__dual_priority_reading, 'a2ae2d70-25c3-44c4-9df3-8a39f928adbe').
narrative_ontology:cs_kernel_codification('a2ae2d70-25c3-44c4-9df3-8a39f928adbe', distributed).
narrative_ontology:cs_authority_grounding('a2ae2d70-25c3-44c4-9df3-8a39f928adbe', distributed).
narrative_ontology:cs_reading_relation('a2ae2d70-25c3-44c4-9df3-8a39f928adbe', ai_safety_commitment__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('a2ae2d70-25c3-44c4-9df3-8a39f928adbe', ai_safety_commitment__near_term_harms_reading, coexists_with).
narrative_ontology:cs_axiom('a2ae2d70-25c3-44c4-9df3-8a39f928adbe', foundational, extinction_and_harms_jointly_necessary).
narrative_ontology:cs_axiom_status(extinction_and_harms_jointly_necessary, holdable).
narrative_ontology:cs_axiom_grounding('a2ae2d70-25c3-44c4-9df3-8a39f928adbe', extinction_and_harms_jointly_necessary, conventional).
narrative_ontology:cs_axiom('a2ae2d70-25c3-44c4-9df3-8a39f928adbe', foundational, scarcity_does_not_force_tradeoffs).
narrative_ontology:cs_axiom_status(scarcity_does_not_force_tradeoffs, holdable).
narrative_ontology:cs_axiom_grounding('a2ae2d70-25c3-44c4-9df3-8a39f928adbe', scarcity_does_not_force_tradeoffs, empirically_contingent).
narrative_ontology:cs_reference_frame('a2ae2d70-25c3-44c4-9df3-8a39f928adbe', dual_mandate_integrity).
narrative_ontology:cs_drift_state('a2ae2d70-25c3-44c4-9df3-8a39f928adbe', resource_constrained_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a2ae2d70-25c3-44c4-9df3-8a39f928adbe', '').
narrative_ontology:cs_kernel_id(ai_safety_commitment__dual_priority_reading, ai_safety_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_safety_commitment__dual_priority_reading, dual_priority_brokers).
narrative_ontology:constraint_victim(ai_safety_commitment__dual_priority_reading, x_risk_researchers).
narrative_ontology:constraint_victim(ai_safety_commitment__dual_priority_reading, near_term_advocates).
narrative_ontology:constraint_victim(ai_safety_commitment__dual_priority_reading, present_harm_communities).
narrative_ontology:constraint_victim(ai_safety_commitment__dual_priority_reading, future_generations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the 'both/and' framing through conferences, funding vehicles, and policy documents that treat existential risk and near-term harms as jointly constitutive of AI safety. Their institutional centrality, policy access, and role as field mediators depend on successfully enforcing the non-competing priorities commitment.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, dual_priority_brokers, agenda_setter,
    institutional, generational, constrained, global).

% Bear the cost of diluted funding and attention when scarce resources are split between long-term speculative alignment research and present-day intervention portfolios. Their ability to argue for extinction-risk primacy is constrained by the field's normative commitment to parity, forcing them to adopt dual-priority language to maintain legitimacy.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, x_risk_researchers, payer,
    powerful, civilizational, constrained, global).

% Receive rhetorical inclusion and partial funding through the dual-priority frame, but experience tokenization and inadequate resourcing when allocative decisions actually occur. They are constrained from demanding full prioritization of algorithmic accountability by the field's commitment to treating their concerns as non-competing with long-term risk.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, near_term_advocates, payer,
    organized, biographical, constrained, global).

% Experience documented algorithmic harms including bias, labor exploitation, and misinformation that the dual-priority frame nominally includes but may systematically under-resource in practice due to competition for attention and funding with extinction-risk research.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, present_harm_communities, payer,
    powerless, immediate, trapped, global).

% Bear the costs of underfunded or delayed extinction-risk prevention when the dual-priority framework allocates insufficient resources to alignment research under scarcity, leaving catastrophic risks inadequately addressed.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, future_generations, payer,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(ai_safety_commitment__dual_priority_reading, future_generations).

% Analyze whether the dual-priority frame achieves substantive integration or performative inclusion, tracking resource flows, citation patterns, and rhetorical shifts across the AI safety field.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, field_analysts, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents the AI safety field from fracturing into mutually delegitimizing camps, preserving a unified advocacy front, shared funding pools, and collective policy access.
% TRANSFER_FUNCTION: Moves legitimacy, funding, and researcher attention between extinction-risk prevention and present-harm mitigation under a unified conceptual umbrella, while coherence costs and resource dilution are borne by both issue areas and their beneficiaries.
% ABSENT_VOICES: Single-priority advocates who would argue that the other domain is a distraction or moral error are partially present but structurally marginalized within dual-priority venues; their exclusion from funding committees and flagship conferences is what the enforcement machinery maintains.
% DISAPPEARANCE_RATIONALE: Without the dual-priority commitment, the field would likely split into competing movements with separate funding streams, conferences, and policy strategies; the broker institutions would lose their mediating role and the current allocative equilibrium would reorganize around explicit prioritization.
% FOUNDING_PROBLEM: The AI safety field risked fragmentation between existential-risk researchers and near-term harm advocates, undermining political credibility, funding stability, and collective action capacity.
% FOUNDING_PROBLEM_CORROBORATION: Critical AI scholars and independent policy researchers attest the fragmentation risk from outside the dual-priority coalition; some x-risk and near-term advocates also acknowledge field-splitting risks, though they dispute whether the dual-priority commitment is the appropriate remedy.
narrative_ontology:disappearance_verdict(ai_safety_commitment__dual_priority_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_safety_commitment__dual_priority_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_safety_commitment__dual_priority_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_safety_commitment__dual_priority_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_safety_commitment__dual_priority_reading, 0.7, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_safety_commitment__dual_priority_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_safety_commitment__dual_priority_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_safety_commitment__dual_priority_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.42 to 0.70 over the interval as resource scarcity intensifies and the coherence challenge becomes more severe. Theater ratio crosses 0.50 at timepoint 18, signaling that performative inclusion (rhetorical parity without allocative parity) has become a significant share of the constraint's operation. Suppression rises modestly as the broker institutions increasingly marginalize single-priority advocates to maintain coalition stability. Accessibility collapse is moderate (0.55) because alternatives (explicit prioritization frameworks) exist but are professionally costly to advocate. Resistance is moderate (0.52) because both camps chafe under the constraint but remain partially invested in field unity.
 *
 * PERSPECTIVAL GAP:
 *   From the broker seat, the constraint appears as necessary coordination preventing field collapse. From the payer seats, it appears as a forced marriage that dilutes their priority under scarcity. The engine computes this divergence from the structural data: identical constraint, opposite directionalities. The divergence is the signal the corpus exists to capture.
 *
 * DIRECTIONALITY LOGIC:
 *   The dual-priority brokers are structural beneficiaries: the constraint subsidizes their institutional centrality and policy access (d near beneficiary end). X-risk researchers and near-term advocates are targets: they bear the coherence costs and resource dilution (d near target end). Present-harm communities and future generations sit at the extreme target end due to powerlessness and trapped exit. The broker coalition's d is low despite constrained exit because their identity and institutional position are fused with the constraint's maintenance; they are structurally subsidized by it even if they cannot easily abandon it.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint prevents mandatrophy mislabeling by requiring both genuine coordination (beneficiaries, field-unity function) and asymmetric extraction (victims, resource dilution). A pure coordination reading (Rope) would be rejected because victims are structurally declared and suppression is non-zero. A pure extraction reading (Snare) would be rejected because the coordination function is real and the beneficiaries are not merely capturing rents but actively mediating. The Tangled Rope classification is the only gate that admits both structural facts.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dual_priority_committer_location,
    'This constraint instantiates the dual_priority_reading of kernel ai_safety_commitment. Would adopting the existential_risk_reading or near_term_harms_reading instead change the beneficiary structure from a broker-centered coalition to a concentrated single-issue hierarchy, or merely shift the victim set?',
    'Comparative structural analysis of resource allocation, stakeholder power, and directionality across the three kernel readings.',
    'If sibling readings eliminate the broker coalition entirely, the dual-priority frame is structurally distinct; if they simply rotate victims while preserving extraction, the kernel is a constant extraction mechanism with varying target sets.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dual_priority_committer_location, conceptual, 'Committe structure: location of disagreement between kernel readings').

omega_variable(
    scarcity_tradeoff_empirical_status,
    'Under current and projected funding and attention budgets, does treating existential risk and near-term harms as non-competing priorities produce adequate resourcing for both domains, or sub-critical investment in each?',
    'Empirical audit of funding flows, staffing levels, and intervention cost-effectiveness across both domains over the measurement interval.',
    'If sub-critical, the dual-priority frame functions as extraction via dilution; if adequate, it functions as genuine coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scarcity_tradeoff_empirical_status, empirical, 'Whether scarcity falsifies the non-competing claim').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the marginalization of single-priority advocates achieved through structural exclusion from funding and venues, or through internalized professional norms that make dual-priority framing the default?',
    'Post-exit trajectory analysis: if single-priority advocates who leave dual-priority institutions continue self-censoring, suppression is partially internalized.',
    'If internalized, effective suppression exceeds the structural measure and the constraint is stickier than institutional data suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_safety_commitment__dual_priority_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_s_tr_t0, ai_safety_commitment__dual_priority_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(ai_s_tr_t6, ai_safety_commitment__dual_priority_reading, theater_ratio, 6, 0.35).
narrative_ontology:measurement(ai_s_tr_t12, ai_safety_commitment__dual_priority_reading, theater_ratio, 12, 0.43).
narrative_ontology:measurement(ai_s_tr_t18, ai_safety_commitment__dual_priority_reading, theater_ratio, 18, 0.5).
narrative_ontology:measurement(ai_s_tr_t24, ai_safety_commitment__dual_priority_reading, theater_ratio, 24, 0.55).

% Extraction over time
narrative_ontology:measurement(ai_s_be_t0, ai_safety_commitment__dual_priority_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(ai_s_be_t6, ai_safety_commitment__dual_priority_reading, base_extractiveness, 6, 0.51).
narrative_ontology:measurement(ai_s_be_t12, ai_safety_commitment__dual_priority_reading, base_extractiveness, 12, 0.58).
narrative_ontology:measurement(ai_s_be_t18, ai_safety_commitment__dual_priority_reading, base_extractiveness, 18, 0.64).
narrative_ontology:measurement(ai_s_be_t24, ai_safety_commitment__dual_priority_reading, base_extractiveness, 24, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(ai_s_su_t0, ai_safety_commitment__dual_priority_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(ai_s_su_t6, ai_safety_commitment__dual_priority_reading, suppression_requirement, 6, 0.45).
narrative_ontology:measurement(ai_s_su_t12, ai_safety_commitment__dual_priority_reading, suppression_requirement, 12, 0.52).
narrative_ontology:measurement(ai_s_su_t18, ai_safety_commitment__dual_priority_reading, suppression_requirement, 18, 0.58).
narrative_ontology:measurement(ai_s_su_t24, ai_safety_commitment__dual_priority_reading, suppression_requirement, 24, 0.63).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_safety_commitment__dual_priority_reading, identity_coordination).
narrative_ontology:affects_constraint(ai_safety_commitment__dual_priority_reading, existential_risk_reading).
narrative_ontology:affects_constraint(ai_safety_commitment__dual_priority_reading, near_term_harms_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the ai_safety_commitment kernel, which decomposes into three structurally distinct claims. The dual-priority, existential-risk, and near-term-harms readings each carry different beneficiary/victim topologies and extraction profiles. Family links are required because the readings compete for the same institutional resources and legitimacy space.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
