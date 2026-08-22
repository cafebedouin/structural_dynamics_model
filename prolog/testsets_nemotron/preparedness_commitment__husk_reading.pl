% ============================================================================
% CONSTRAINT STORY: preparedness_commitment__husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-28
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_commitment__husk_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: preparedness_commitment__husk_reading
 *   human_readable: Preparedness as Memorial Performance (Husk Reading)
 *   domain: institutional/governance/disaster_preparedness
 *
 * SUMMARY:
 *   The husk_reading of the preparedness_commitment kernel instantiates a
 *   constraint where institutional preparedness routines — drills,
 *   certifications, tabletop exercises, compliance audits — have become
 *   memorial performances. They retain the form of the founding
 *   competence_reading (live exercised knowledge) but have lost the adaptive
 *   capacity that made them functional. The constraint extracts operational
 *   training budget and frontline responder attention into compliance
 *   theater, while the ceremonial leadership and audit contractors benefit
 *   from the appearance of readiness. The D5 break — a novel stressor outside
 *   the rehearsed scenario — manifests as competence collapse because the
 *   system has optimized for audit-passing rather than problem-solving. This
 *   reading claims the constraint is a piton: a degraded former rope/scaffold
 *   where the primary function has atrophied but the structure persists
 *   through institutional inertia and theatrical maintenance.
 *
 * KEY AGENTS:
 *   - bureaucratic_compliance_officers: Primary beneficiary (institutional/arbitrage) — administers the compliance regime, collects budget and status from ceremony
 *   - ceremonial_leadership: Primary beneficiary (institutional/arbitrage) — performs readiness for stakeholders, avoids accountability for actual capacity
 *   - audit_contractors: Secondary beneficiary (organized/mobile) — contracted to verify form-compliance, revenue depends on ceremony continuation
 *   - frontline_responders: Primary payer (moderate/constrained) — bears the time/attention cost of drills that don't build skill, constrained exit (professional identity, pension)
 *   - affected_populations: Primary victim (powerless/trapped) — experiences the competence collapse when novel disaster strikes, no exit from geographic exposure
 *   - operational_training_budget: Structural victim (non-agent) — captured by compliance line items, cannot advocate for itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_commitment__husk_reading, 0.68).
domain_priors:suppression_score(preparedness_commitment__husk_reading, 0.62).
domain_priors:theater_ratio(preparedness_commitment__husk_reading, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_commitment__husk_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(preparedness_commitment__husk_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(preparedness_commitment__husk_reading, theater_ratio, 0.78).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_commitment__husk_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(preparedness_commitment__husk_reading, resistance, 0.34).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_commitment__husk_reading, piton).
narrative_ontology:human_readable(preparedness_commitment__husk_reading, "Preparedness as Memorial Performance (Husk Reading)").
narrative_ontology:topic_domain(preparedness_commitment__husk_reading, "institutional/governance/disaster_preparedness").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_commitment__husk_reading, '46f20eca-c3d2-489b-9ebf-2d4434654f8d').
narrative_ontology:cs_kernel_codification('46f20eca-c3d2-489b-9ebf-2d4434654f8d', implicit).
narrative_ontology:cs_authority_grounding('46f20eca-c3d2-489b-9ebf-2d4434654f8d', extraction).
narrative_ontology:cs_interpretation_layer_present('46f20eca-c3d2-489b-9ebf-2d4434654f8d').
narrative_ontology:cs_reading_relation('46f20eca-c3d2-489b-9ebf-2d4434654f8d', preparedness_commitment__competence_reading, influences).
narrative_ontology:cs_reading_relation('46f20eca-c3d2-489b-9ebf-2d4434654f8d', preparedness_commitment__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('46f20eca-c3d2-489b-9ebf-2d4434654f8d', foundational, ceremonial_compliance_suffices).
narrative_ontology:cs_axiom_status(ceremonial_compliance_suffices, holdable).
narrative_ontology:cs_axiom_grounding('46f20eca-c3d2-489b-9ebf-2d4434654f8d', ceremonial_compliance_suffices, instrumental).
narrative_ontology:cs_axiom('46f20eca-c3d2-489b-9ebf-2d4434654f8d', secondary, audit_metrics_are_readiness_proxies).
narrative_ontology:cs_axiom_status(audit_metrics_are_readiness_proxies, holdable).
narrative_ontology:cs_axiom_grounding('46f20eca-c3d2-489b-9ebf-2d4434654f8d', audit_metrics_are_readiness_proxies, conventional).
narrative_ontology:cs_reference_frame('46f20eca-c3d2-489b-9ebf-2d4434654f8d', founding_compliance_regime).
narrative_ontology:cs_drift_state('46f20eca-c3d2-489b-9ebf-2d4434654f8d', post_novel_stressor_sequence, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('46f20eca-c3d2-489b-9ebf-2d4434654f8d', '2026-08-28T14:32:17Z').
narrative_ontology:cs_kernel_id(preparedness_commitment__husk_reading, preparedness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_commitment__husk_reading, bureaucratic_compliance_officers).
narrative_ontology:constraint_beneficiary(preparedness_commitment__husk_reading, ceremonial_leadership).
narrative_ontology:constraint_beneficiary(preparedness_commitment__husk_reading, audit_contractors).
narrative_ontology:constraint_victim(preparedness_commitment__husk_reading, frontline_responders).
narrative_ontology:constraint_victim(preparedness_commitment__husk_reading, affected_populations).
narrative_ontology:constraint_victim(preparedness_commitment__husk_reading, operational_training_budget).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_commitment__husk_reading, frontline_responders).
narrative_ontology:constraint_vindicates(preparedness_commitment__husk_reading, institutional_continuity_narrative).
narrative_ontology:constraint_vindicates(preparedness_commitment__husk_reading, compliance_as_competence_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs, administers, and enforces the preparedness compliance regime — drill schedules, certification requirements, audit criteria. Their authority and budget derive from the regime's continuation. They can move between agencies or to audit contracting firms (arbitrage exit). They genuinely believe the compliance regime ensures readiness, but their institutional incentive is regime maintenance, not competence validation.
narrative_ontology:constraint_stakeholder(preparedness_commitment__husk_reading, bureaucratic_compliance_officers, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(preparedness_commitment__husk_reading, bureaucratic_compliance_officers, beneficiary).

% Senior officials who perform readiness for legislators, media, and the public — press conferences at drill sites, certification announcements, after-action reports that highlight compliance metrics. They avoid accountability for actual capacity gaps because the compliance regime provides a defensive narrative. They can rotate to other leadership roles (arbitrage exit).
narrative_ontology:constraint_stakeholder(preparedness_commitment__husk_reading, ceremonial_leadership, beneficiary,
    institutional, biographical, arbitrage, national).

% Private firms contracted to verify compliance with preparedness standards. Their revenue depends on the regime's continuation and expansion. They have mobile exit — can pivot to other compliance domains. They optimize for audit-passing criteria, not operational realism, because that is what the contract measures.
narrative_ontology:constraint_stakeholder(preparedness_commitment__husk_reading, audit_contractors, beneficiary,
    organized, biographical, mobile, national).

% Operational personnel who must participate in drills, complete certifications, and generate compliance paperwork. They experience the drills as time taken from skill-building (scenario-less adaptive exercises, equipment familiarity, cross-team coordination). Their exit is identity-locked: professional identity as a responder is fused with the institution; leaving means losing pension, community, and self-concept. They receive incidental benefit (some drills have marginal skill value) but net extractive.
narrative_ontology:constraint_stakeholder(preparedness_commitment__husk_reading, frontline_responders, payer,
    moderate, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_commitment__husk_reading, frontline_responders, beneficiary).

% Communities that experience the competence collapse when a novel disaster exceeds the rehearsed scenarios. They have no exit from geographic exposure and no voice in preparedness design. They bear the full cost of the constraint's theater — the gap between audit scores and actual response capacity. They are not participants in the constraint; they are its downstream victims.
narrative_ontology:constraint_stakeholder(preparedness_commitment__husk_reading, affected_populations, payer,
    powerless, immediate, trapped, local).

% The budget line for actual skill-building training (live exercises, red-teaming, adaptive scenario design, equipment maintenance). It is structurally captured by compliance line items — every dollar spent on audit preparation is a dollar not spent on competence. It cannot advocate for itself; its capture is the mechanism of the constraint's extraction.
narrative_ontology:constraint_stakeholder(preparedness_commitment__husk_reading, operational_training_budget, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_non_agent(preparedness_commitment__husk_reading, operational_training_budget).

% Institutional reformers, training innovators, and after-action analysts who argue that preparedness requires adaptive capacity, not form-compliance. They are excluded from the compliance agenda-setting process — their proposals are treated as non-compliant or 'supplementary.' Their exit is constrained: they can publish, testify, or move to hybrid_reading institutions, but cannot displace the compliance regime from inside.
narrative_ontology:constraint_stakeholder(preparedness_commitment__husk_reading, competence_reading_advocates, excluded,
    moderate, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_commitment__husk_reading, bureaucratic_compliance_officers).
narrative_ontology:fixing_cost_class(preparedness_commitment__husk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint nominally coordinates inter-agency readiness by establishing a common drill calendar, shared terminology, and baseline certification — so that when disaster strikes, responders from different agencies can operate together. This is the founding coordination function from the competence_reading era.
% TRANSFER_FUNCTION: Moves operational training budget and frontline responder attention from skill-building exercises into compliance drills, audit preparation, and certification paperwork. The gains (budget control, legitimacy performance, audit revenue) accrue to the compliance officers, ceremonial leadership, and audit contractors.
% ABSENT_VOICES: Affected populations (who would object to competence collapse but have no access to preparedness design) and competence_reading_advocates (who would object to theater displacing function but are excluded from the compliance agenda). The affected populations are geographically trapped; the advocates are institutionally excluded.
% DISAPPEARANCE_RATIONALE: If the compliance regime vanished overnight, the budget and attention captured by theater would be contested — some would flow to genuine adaptive training (competence_reading), some would be cut (budget pressure), and the ceremonial leadership would lose its defensive narrative. The institutional architecture of preparedness would reorganize around whatever replaces the compliance regime — likely a hybrid or a return to competence-first models.
% FOUNDING_PROBLEM: Early civil defense and disaster response suffered from fragmentation: agencies used incompatible terminology, had no shared drill rhythm, and could not coordinate in multi-jurisdictional events. The compliance regime was built to solve this — a common framework, mandatory drills, standardized certification.
% FOUNDING_PROBLEM_CORROBORATION: After-action reports from novel disasters (Hurricane Katrina, COVID-19 initial response, 2023 Maui wildfires) documented by independent commissions (not the compliance offices) show that standardized drills did not prevent coordination failure under novel stress. The founding problem (fragmentation) was solved by the early regime, but the regime persisted past the solution and became the new fragmentation — compliance terminology replaced operational terminology. No one outside the beneficiary set attests the founding problem is still live; the compliance offices themselves attest it is live, which is self-assertion.
narrative_ontology:disappearance_verdict(preparedness_commitment__husk_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_commitment__husk_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_commitment__husk_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(preparedness_commitment__husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_commitment__husk_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_commitment__husk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(preparedness_commitment__husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(preparedness_commitment__husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.68) is high because the compliance regime captures a growing share of preparedness budget while delivering diminishing operational returns. Suppression (0.62) is moderate-high because the constraint persists through mandate rigidity and professional identity lock — responders cannot easily refuse drills without career penalty, and affected populations have zero exit. Theater ratio (0.78) is very high, confirming piton classification: the vast majority of drill activity is performative (audit-passing) rather than functional (skill-building). Accessibility collapse (0.71) is high because once the compliance regime is understood as the only legitimate preparedness frame, alternative training models (scenario-less adaptive exercises, red-teaming, cross-domain learning) are structurally excluded. Resistance (0.34) is low because the constraint's victims are either powerless (affected populations) or identity-locked (frontline responders), and the beneficiaries control the agenda. The measurement series on a shared grid (T=0,5,10,15,20,25) shows monotonic extraction accumulation and theater intensification — the constraint degraded from a rope/scaffold into a piton over the interval.
 *
 * PERSPECTIVAL GAP:
 *   The compliance_officer and ceremonial_leadership seats experience this as coordination (they built/maintain the regime, benefit from it, have arbitrage-grade exit). The frontline_responder seat experiences it as extraction with constrained exit — they see the theater but cannot opt out. The affected_population seat experiences it as a snare (pure extraction, trapped). The engine computes per-seat types from these structural asymmetries; the authored claim (piton) reflects the institutional observer's view of the degraded steady state.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: bureaucratic_compliance_officers and ceremonial_leadership collect budget, status, and legitimacy from the ceremony — they are the agenda_setters with institutional power and arbitrage exit. Victims: frontline_responders pay with attention/time and constrained exit (professional identity, pension lock-in); affected_populations pay with lives when competence collapses, zero exit. The operational_training_budget is a non-agent structural victim — captured by compliance line items. Directionality derivation: beneficiaries get low d (~0.15), payers get high d (~0.75), trapped victims get d~0.9. No overrides needed — the structural data produces the right gradient.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (live exercised knowledge across generations) is dead — the competence_reading's axiom (operational_exercise_necessary) has been overridden by the husk_reading's axiom (ceremonial_compliance_suffices). The arrangement persists because the compliance regime captures the budget and legitimacy that would fund actual competence, and no actor has both the incentive and power to fix it. The compliance officers could change it but would lose their rationale; leadership could change it but would lose the appearance of control; responders could demand change but lack agenda power. This is mandatrophy resolved: the mandate has outlived its function and the constraint is now a piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structure,
    'Is this constraint one reading of the contested preparedness_commitment kernel, and does the husk_reading structurally foreclose, coexist with, or influence the competence_reading and hybrid_reading?',
    'Cross-reading structural comparison: map each reading''s beneficiary/victim structure, extraction profile, and drift_state; if husk_reading''s foundational axiom (ceremonial_compliance_suffices) directly contradicts competence_reading''s axiom (operational_exercise_necessary) within a single authority framework, the relation is forecloses; if both remain live positions held by different institutional factions, the relation is coexists_with; if husk_reading''s resource capture creates downstream pressure on competence_reading''s operating conditions, the relation is influences.',
    'Determines whether the kernel''s readings form a coherent family with typed edges, enabling contamination analysis across readings. If forecloses, the kernel has a logical fracture; if coexists_with, the dispute is political not logical; if influences, the husk_reading actively degrades the competence_reading''s viability.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structure, conceptual, 'Committee frame: this constraint is the husk_reading of the preparedness_commitment kernel; sibling readings are competence_reading and hybrid_reading.').

omega_variable(
    ceremonial_vs_operational_boundary,
    'Where exactly does the boundary lie between ceremonial compliance that stabilizes institutional commitment and ceremonial compliance that displaces operational competence?',
    'Longitudinal case studies of institutions that experienced D5 breaks: trace the trajectory of drill frequency, audit scores, and actual performance under novel stress; identify the inflection point where theater_ratio crossed the threshold from commitment-stabilizing to competence-displacing.',
    'If the boundary is identifiable and the husk_reading sits on the displacing side, the constraint is a piton with extractive theater; if the boundary is unresolvable, the constraint''s classification remains ambiguous between scaffold (transitional) and piton (degraded).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ceremonial_vs_operational_boundary, empirical, 'The coordination/extraction boundary within the husk_reading itself.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression experienced by frontline responders structural (budget capture, mandate rigidity) or internalized (professional identity fused with compliance performance)?',
    'Post-exit trajectory study: track responders who leave the system — does their sense of suppressed agency persist, or does it dissolve when the compliance machinery is no longer daily practice?',
    'If internalized, the constraint''s effective suppression is higher than structural measures suggest — the target carries the suppression with them; this would increase the piton classification confidence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for the payer seat.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_commitment__husk_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(preparedness_commitment__husk_reading_tr_t0, preparedness_commitment__husk_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement_basis(preparedness_commitment__husk_reading_tr_t0, observed).
narrative_ontology:measurement(preparedness_commitment__husk_reading_tr_t5, preparedness_commitment__husk_reading, theater_ratio, 5, 0.51).
narrative_ontology:measurement_basis(preparedness_commitment__husk_reading_tr_t5, observed).
narrative_ontology:measurement(preparedness_commitment__husk_reading_tr_t10, preparedness_commitment__husk_reading, theater_ratio, 10, 0.62).
narrative_ontology:measurement_basis(preparedness_commitment__husk_reading_tr_t10, observed).
narrative_ontology:measurement(preparedness_commitment__husk_reading_tr_t15, preparedness_commitment__husk_reading, theater_ratio, 15, 0.7).
narrative_ontology:measurement_basis(preparedness_commitment__husk_reading_tr_t15, observed).
narrative_ontology:measurement(preparedness_commitment__husk_reading_tr_t20, preparedness_commitment__husk_reading, theater_ratio, 20, 0.75).
narrative_ontology:measurement_basis(preparedness_commitment__husk_reading_tr_t20, observed).
narrative_ontology:measurement(preparedness_commitment__husk_reading_tr_t25, preparedness_commitment__husk_reading, theater_ratio, 25, 0.78).
narrative_ontology:measurement_basis(preparedness_commitment__husk_reading_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(preparedness_commitment__husk_reading_be_t0, preparedness_commitment__husk_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(preparedness_commitment__husk_reading_be_t0, observed).
narrative_ontology:measurement(preparedness_commitment__husk_reading_be_t5, preparedness_commitment__husk_reading, base_extractiveness, 5, 0.45).
narrative_ontology:measurement_basis(preparedness_commitment__husk_reading_be_t5, observed).
narrative_ontology:measurement(preparedness_commitment__husk_reading_be_t10, preparedness_commitment__husk_reading, base_extractiveness, 10, 0.53).
narrative_ontology:measurement_basis(preparedness_commitment__husk_reading_be_t10, observed).
narrative_ontology:measurement(preparedness_commitment__husk_reading_be_t15, preparedness_commitment__husk_reading, base_extractiveness, 15, 0.61).
narrative_ontology:measurement_basis(preparedness_commitment__husk_reading_be_t15, observed).
narrative_ontology:measurement(preparedness_commitment__husk_reading_be_t20, preparedness_commitment__husk_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement_basis(preparedness_commitment__husk_reading_be_t20, observed).
narrative_ontology:measurement(preparedness_commitment__husk_reading_be_t25, preparedness_commitment__husk_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(preparedness_commitment__husk_reading_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(preparedness_commitment__husk_reading_su_t0, preparedness_commitment__husk_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(preparedness_commitment__husk_reading_su_t0, observed).
narrative_ontology:measurement(preparedness_commitment__husk_reading_su_t5, preparedness_commitment__husk_reading, suppression_requirement, 5, 0.42).
narrative_ontology:measurement_basis(preparedness_commitment__husk_reading_su_t5, observed).
narrative_ontology:measurement(preparedness_commitment__husk_reading_su_t10, preparedness_commitment__husk_reading, suppression_requirement, 10, 0.5).
narrative_ontology:measurement_basis(preparedness_commitment__husk_reading_su_t10, observed).
narrative_ontology:measurement(preparedness_commitment__husk_reading_su_t15, preparedness_commitment__husk_reading, suppression_requirement, 15, 0.55).
narrative_ontology:measurement_basis(preparedness_commitment__husk_reading_su_t15, observed).
narrative_ontology:measurement(preparedness_commitment__husk_reading_su_t20, preparedness_commitment__husk_reading, suppression_requirement, 20, 0.59).
narrative_ontology:measurement_basis(preparedness_commitment__husk_reading_su_t20, observed).
narrative_ontology:measurement(preparedness_commitment__husk_reading_su_t25, preparedness_commitment__husk_reading, suppression_requirement, 25, 0.62).
narrative_ontology:measurement_basis(preparedness_commitment__husk_reading_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_commitment__husk_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(preparedness_commitment__husk_reading, 0.08).
narrative_ontology:affects_constraint(preparedness_commitment__husk_reading, preparedness_commitment__competence_reading).
narrative_ontology:affects_constraint(preparedness_commitment__husk_reading, preparedness_commitment__hybrid_reading).
narrative_ontology:affects_constraint(preparedness_commitment__husk_reading, emergency_response_funding_allocation).
narrative_ontology:affects_constraint(preparedness_commitment__husk_reading, disaster_exercise_design_standards).

% DUAL FORMULATION NOTE:
% The preparedness_commitment kernel decomposes into three constraint stories: competence_reading (rope/scaffold — low extraction, genuine coordination), husk_reading (this story — piton — high theater, extraction from budget/attention), hybrid_reading (tangled_rope — genuine coordination function from the competence layer, asymmetric extraction from the memorial layer). All three are linked via affects_constraints. The husk_reading captures the degraded steady state where the coordination function has atrophied but the structure persists.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
