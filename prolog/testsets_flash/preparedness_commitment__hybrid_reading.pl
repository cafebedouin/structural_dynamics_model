% ============================================================================
% CONSTRAINT STORY: preparedness_commitment__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_commitment__hybrid_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: preparedness_commitment__hybrid_reading
 *   human_readable: Hybrid Preparedness Commitment (Memorial & Competence Layers)
 *   domain: disaster_preparedness/institutional_memory/commitment_systems
 *
 * SUMMARY:
 *   This constraint describes preparedness as a layered system, combining
 *   memorial elements (e.g., rituals, commemorative events, symbolic
 *   infrastructure) that stabilize long-term commitment with competence
 *   elements (e.g., training, equipment, operational drills) that maintain
 *   functional capacity. The 'hybrid_reading' acknowledges the necessary
 *   tension and interaction between these layers. The memorial layer prevents
 *   the complete abandonment of preparedness efforts, while the competence
 *   layer aims to prevent catastrophic failure. The challenge lies in
 *   managing the maintenance cost and ensuring the memorial layer doesn't
 *   entirely displace functional competence.
 *
 * KEY AGENTS:
 *   - public_safety_agencies: Agenda setter (institutional/generational) — balances memorial and competence needs.
 *   - political_leaders: Beneficiary (powerful/immediate) — benefits from visible commitment.
 *   - taxpayers: Payer (organized/biographical) — funds the system, vulnerable to performative elements.
 *   - frontline_responders: Payer/Beneficiary (moderate/biographical) — implements plans, bears costs of performative elements.
 *   - disaster_victims: Excluded (powerless/immediate) — bears ultimate costs of failure, absent from design.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_commitment__hybrid_reading, 0.45).
domain_priors:suppression_score(preparedness_commitment__hybrid_reading, 0.3).
domain_priors:theater_ratio(preparedness_commitment__hybrid_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_commitment__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(preparedness_commitment__hybrid_reading, "Hybrid Preparedness Commitment (Memorial & Competence Layers)").
narrative_ontology:topic_domain(preparedness_commitment__hybrid_reading, "disaster_preparedness/institutional_memory/commitment_systems").

domain_priors:requires_active_enforcement(preparedness_commitment__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_commitment__hybrid_reading, '67301d34-1676-4b19-82da-4c0919ac2feb').
narrative_ontology:cs_kernel_codification('67301d34-1676-4b19-82da-4c0919ac2feb', formalized).
narrative_ontology:cs_authority_grounding('67301d34-1676-4b19-82da-4c0919ac2feb', lineage).
narrative_ontology:cs_interpretation_layer_present('67301d34-1676-4b19-82da-4c0919ac2feb').
narrative_ontology:cs_reading_relation('67301d34-1676-4b19-82da-4c0919ac2feb', preparedness_commitment__husk_reading, influences).
narrative_ontology:cs_reading_relation('67301d34-1676-4b19-82da-4c0919ac2feb', preparedness_commitment__competence_reading, influences).
narrative_ontology:cs_axiom('67301d34-1676-4b19-82da-4c0919ac2feb', foundational, preparedness_requires_dual_maintenance).
narrative_ontology:cs_axiom_status(preparedness_requires_dual_maintenance, holdable).
narrative_ontology:cs_axiom_grounding('67301d34-1676-4b19-82da-4c0919ac2feb', preparedness_requires_dual_maintenance, empirically_contingent).
narrative_ontology:cs_axiom('67301d34-1676-4b19-82da-4c0919ac2feb', secondary, memorial_stabilizes_commitment).
narrative_ontology:cs_axiom_status(memorial_stabilizes_commitment, holdable).
narrative_ontology:cs_axiom_grounding('67301d34-1676-4b19-82da-4c0919ac2feb', memorial_stabilizes_commitment, empirically_contingent).
narrative_ontology:cs_reference_frame('67301d34-1676-4b19-82da-4c0919ac2feb', post_disaster_reform_era).
narrative_ontology:cs_drift_state('67301d34-1676-4b19-82da-4c0919ac2feb', contemporary_budget_cycles, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('67301d34-1676-4b19-82da-4c0919ac2feb', '').
narrative_ontology:cs_kernel_id(preparedness_commitment__hybrid_reading, preparedness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_commitment__hybrid_reading, public_safety_agencies).
narrative_ontology:constraint_beneficiary(preparedness_commitment__hybrid_reading, political_leaders).
narrative_ontology:constraint_victim(preparedness_commitment__hybrid_reading, taxpayers).
narrative_ontology:constraint_victim(preparedness_commitment__hybrid_reading, frontline_responders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_commitment__hybrid_reading, frontline_responders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for designing, implementing, and maintaining preparedness systems. They balance the need for visible memorial commitments (to secure funding and public trust) with the need for actual operational competence (to respond effectively). They benefit from the stability the memorial layer provides but bear the cost of maintaining competence.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, public_safety_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from the public perception of preparedness, especially the memorial elements (e.g., visible drills, commemorative events) that signal commitment without necessarily requiring deep investment in competence. They can claim credit for 'being prepared' while offloading the true costs of competence maintenance.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, political_leaders, beneficiary,
    powerful, immediate, mobile, national).

% Bear the financial costs of maintaining both memorial and competence layers through taxes. They are coordinated into funding the system but often lack the information to distinguish between performative and functional preparedness, making them vulnerable to extraction by the memorial layer.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, taxpayers, payer,
    organized, biographical, constrained, national).

% Are the ultimate implementers of preparedness plans. They benefit from genuine competence but often bear the costs of maintaining performative elements that divert resources from actual training and equipment. Their professional identity often locks them into the system, even when it is suboptimal.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, frontline_responders, payer,
    moderate, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(preparedness_commitment__hybrid_reading, frontline_responders, beneficiary).

% Are the ultimate targets of preparedness efforts, but their voices are often absent from the design and maintenance of the system until after a failure. They bear the catastrophic costs of competence failures, which the hybrid system aims to prevent but can obscure.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, disaster_victims, excluded,
    powerless, immediate, trapped, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the long-term commitment to disaster preparedness (memorial layer) with the dynamic maintenance of operational capacity (competence layer) across institutional and generational boundaries, ensuring that past lessons are not entirely forgotten while adapting to new threats.
% TRANSFER_FUNCTION: Transfers resources (funding, personnel, attention) from taxpayers to public safety agencies and political leaders, in exchange for the promise of preparedness. It also transfers the burden of maintaining both performative and functional elements to frontline responders.
% ABSENT_VOICES: Future disaster victims and those who have experienced past preparedness failures are largely absent from the ongoing maintenance and resource allocation decisions. Their voices would highlight the gap between memorial commitment and actual competence, advocating for greater investment in functional capacity.
% DISAPPEARANCE_RATIONALE: If this hybrid system vanished, the long-term commitment to preparedness would erode, leading to a rapid decay of both memorial and competence elements. Funding would dry up, institutional memory would be lost, and operational capacity would collapse, leaving society vulnerable to catastrophic failures.
% FOUNDING_PROBLEM: The problem of maintaining readiness for infrequent but high-impact disasters, where the memory of past events fades and the political will to invest in costly, unexercised capabilities wanes over time.
% FOUNDING_PROBLEM_CORROBORATION: Public safety experts, historians of disaster response, and independent auditors consistently corroborate that the problem of maintaining long-term preparedness is live and ongoing, citing cycles of 'prepare and forget' that precede major events. This corroboration comes from outside the immediate beneficiaries of the system.
narrative_ontology:disappearance_verdict(preparedness_commitment__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_commitment__hybrid_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_commitment__hybrid_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(preparedness_commitment__hybrid_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_commitment__hybrid_reading_tests).
:- end_tests(preparedness_commitment__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates long-term commitment to preparedness (benefiting public safety agencies and political leaders) but also involves asymmetric extraction. Taxpayers and frontline responders bear the costs, particularly when resources are diverted to performative memorial elements over genuine competence. Active enforcement is required to maintain both layers, especially to ensure compliance with competence standards and to secure funding for memorialization. Extractiveness is moderate (0.45) due to the overhead of maintaining both layers and the potential for resource misallocation. Suppression (0.30) is present in the form of institutional inertia and the difficulty for external actors (like taxpayers) to challenge the balance between memorial and competence. Theater ratio (0.20) reflects the necessary but sometimes disproportionate investment in visible, symbolic acts of preparedness.
 *
 * PERSPECTIVAL GAP:
 *   Public safety agencies and political leaders (beneficiaries) experience this as a necessary, if imperfect, coordination mechanism for societal resilience. Taxpayers and frontline responders (payers) experience the extractive aspects more acutely, particularly the costs associated with performative memorial elements that do not directly enhance operational competence. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Public safety agencies and political leaders are beneficiaries, as the system provides them with stability, public trust, and political capital. Taxpayers are payers, funding the entire system. Frontline responders are both payers (bearing the burden of maintaining potentially suboptimal systems) and beneficiaries (gaining from genuine competence). Disaster victims are excluded, bearing the ultimate costs of failure without direct influence.
 *
 * MANDATROPHY ANALYSIS:
 *   This hybrid reading prevents mislabeling the constraint as either pure competence (ignoring the memorial function) or pure husk (ignoring any functional competence). It acknowledges that the memorial layer, while potentially extractive, serves a critical function in stabilizing long-term commitment, preventing the constraint from atrophying entirely. The tension between the layers is a feature, not a bug, of this specific constraint, reflecting the challenge of sustaining preparedness over long, quiet periods.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    memorial_competence_balance,
    'What is the optimal balance between memorial and competence elements to maximize long-term preparedness effectiveness while minimizing extraction?',
    'Longitudinal studies comparing preparedness outcomes and resource allocation in systems with varying memorial-to-competence ratios, particularly after major disaster events.',
    'Resolving this would inform policy adjustments to reduce unnecessary extraction and improve resilience. If the current balance is suboptimal, the constraint''s extractiveness could be reduced without compromising its coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(memorial_competence_balance, empirical, 'Determining the ideal resource allocation between symbolic and functional preparedness.').

omega_variable(
    reading_framing_impact,
    'How does framing preparedness as a ''hybrid system'' (this reading) influence resource allocation and institutional behavior compared to framing it as purely ''competence'' or ''husk''?',
    'Comparative analysis of policy documents, budget allocations, and institutional rhetoric across different jurisdictions that explicitly adopt one of the three readings as their guiding framework.',
    'If this ''hybrid_reading'' leads to more balanced and effective preparedness outcomes, it supports its utility as a conceptual tool. If it inadvertently legitimizes excessive memorialization, it might be a less effective framing than a ''competence_reading''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_framing_impact, conceptual, 'Impact of the ''hybrid'' framing on real-world preparedness outcomes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_commitment__hybrid_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_commitment__hybrid_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(prep_tr_t10, preparedness_commitment__hybrid_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(prep_tr_t20, preparedness_commitment__hybrid_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(prep_tr_t30, preparedness_commitment__hybrid_reading, theater_ratio, 30, 0.19).
narrative_ontology:measurement(prep_tr_t40, preparedness_commitment__hybrid_reading, theater_ratio, 40, 0.19).
narrative_ontology:measurement(prep_tr_t50, preparedness_commitment__hybrid_reading, theater_ratio, 50, 0.2).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_commitment__hybrid_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(prep_be_t10, preparedness_commitment__hybrid_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(prep_be_t20, preparedness_commitment__hybrid_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(prep_be_t30, preparedness_commitment__hybrid_reading, base_extractiveness, 30, 0.42).
narrative_ontology:measurement(prep_be_t40, preparedness_commitment__hybrid_reading, base_extractiveness, 40, 0.43).
narrative_ontology:measurement(prep_be_t50, preparedness_commitment__hybrid_reading, base_extractiveness, 50, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_commitment__hybrid_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(prep_su_t10, preparedness_commitment__hybrid_reading, suppression_requirement, 10, 0.28).
narrative_ontology:measurement(prep_su_t20, preparedness_commitment__hybrid_reading, suppression_requirement, 20, 0.3).
narrative_ontology:measurement(prep_su_t30, preparedness_commitment__hybrid_reading, suppression_requirement, 30, 0.29).
narrative_ontology:measurement(prep_su_t40, preparedness_commitment__hybrid_reading, suppression_requirement, 40, 0.29).
narrative_ontology:measurement(prep_su_t50, preparedness_commitment__hybrid_reading, suppression_requirement, 50, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_commitment__hybrid_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(preparedness_commitment__hybrid_reading, preparedness_commitment__husk_reading).
narrative_ontology:affects_constraint(preparedness_commitment__hybrid_reading, preparedness_commitment__competence_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'preparedness_commitment' kernel. This 'hybrid_reading' acknowledges the tension and interaction between memorial and competence layers, influencing how the other two readings are understood and enacted.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
