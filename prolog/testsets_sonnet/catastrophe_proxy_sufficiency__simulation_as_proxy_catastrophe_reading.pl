% ============================================================================
% CONSTRAINT STORY: catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading
 *   human_readable: Simulation Exercises as Sufficient Catastrophe-Equivalent Practice
 *   domain: safety_engineering/organizational_learning/high_reliability_organizations
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the catastrophe_proxy_sufficiency
 *   kernel: the claim that scheduled, well-designed simulation exercises are
 *   categorically sufficient to maintain operational competence for
 *   catastrophe-consequence roles, indefinitely, without any need for
 *   real-event exposure. Under this reading, simulation is a coordination
 *   mechanism — it solves the genuine problem of building and verifying
 *   competence without unacceptable real-world exposure — and the metrics are
 *   authored accordingly: low extractiveness, low suppression, no victim set.
 *   The beneficiaries are regulatory bodies (who get a defensible, auditable
 *   certification basis), operating organizations (who get continued
 *   licensure at bounded cost), and simulation vendors (whose product is
 *   validated as sufficient by the doctrine). This is NOT the same constraint
 *   as the sibling readings — catastrophe_necessity_reading asserts
 *   simulation is categorically insufficient (a structurally different,
 *   high-extractiveness claim: what looks like competence maintenance is
 *   actually a certification veneer over unaddressed skill decay);
 *   hybrid_degradation_reading asserts partial, generational-timescale
 *   insufficiency; simulation_fidelity_threshold makes sufficiency
 *   technology-conditional rather than categorical. Each of those is a
 *   separate constraint file with its own ε, per the ε-invariance principle —
 *   this file does not average across them or hedge between them.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, 0.22).
domain_priors:suppression_score(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, 0.28).
domain_priors:theater_ratio(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, rope).
narrative_ontology:human_readable(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, "Simulation Exercises as Sufficient Catastrophe-Equivalent Practice").
narrative_ontology:topic_domain(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, "safety_engineering/organizational_learning/high_reliability_organizations").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, '27cfe765-7dd4-45fd-8d9e-00435509e778').
narrative_ontology:cs_kernel_codification('27cfe765-7dd4-45fd-8d9e-00435509e778', distributed).
narrative_ontology:cs_authority_grounding('27cfe765-7dd4-45fd-8d9e-00435509e778', expertise).
narrative_ontology:cs_interpretation_layer_present('27cfe765-7dd4-45fd-8d9e-00435509e778').
narrative_ontology:cs_reading_relation('27cfe765-7dd4-45fd-8d9e-00435509e778', catastrophe_proxy_sufficiency__catastrophe_necessity_reading, forecloses).
narrative_ontology:cs_reading_relation('27cfe765-7dd4-45fd-8d9e-00435509e778', catastrophe_proxy_sufficiency__hybrid_degradation_reading, coexists_with).
narrative_ontology:cs_reading_relation('27cfe765-7dd4-45fd-8d9e-00435509e778', catastrophe_proxy_sufficiency__simulation_fidelity_threshold, influences).
narrative_ontology:cs_axiom('27cfe765-7dd4-45fd-8d9e-00435509e778', foundational, simulation_equivalence_categorical).
narrative_ontology:cs_axiom_status(simulation_equivalence_categorical, holdable).
narrative_ontology:cs_axiom_grounding('27cfe765-7dd4-45fd-8d9e-00435509e778', simulation_equivalence_categorical, empirically_contingent).
narrative_ontology:cs_axiom('27cfe765-7dd4-45fd-8d9e-00435509e778', secondary, tacit_competence_fully_proxyable).
narrative_ontology:cs_axiom_status(tacit_competence_fully_proxyable, holdable).
narrative_ontology:cs_axiom_grounding('27cfe765-7dd4-45fd-8d9e-00435509e778', tacit_competence_fully_proxyable, empirically_contingent).
narrative_ontology:cs_reference_frame('27cfe765-7dd4-45fd-8d9e-00435509e778', simulation_certification_sufficiency_standard).
narrative_ontology:cs_drift_state('27cfe765-7dd4-45fd-8d9e-00435509e778', post_fukushima_reassessment_era, gap(stable, minor, false)).
narrative_ontology:cs_created_at('27cfe765-7dd4-45fd-8d9e-00435509e778', '').
narrative_ontology:cs_kernel_id(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, catastrophe_proxy_sufficiency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, regulatory_bodies).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, operating_organizations).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, simulation_vendors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, frontline_operators).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, operating_organizations).
narrative_ontology:constraint_vindicates(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, simulation_equivalence_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Certifies operators as competent based on documented simulation drill completion rather than exposure to real catastrophic events. Sets the recertification cadence and audit criteria. Gains a defensible, auditable competence standard that discharges its oversight obligation without needing to wait for or manufacture actual catastrophes.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, regulatory_bodies, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, regulatory_bodies, beneficiary).

% Nuclear plants, air traffic control centers, hospital emergency departments, and similar high-consequence operators run scheduled simulation exercises to satisfy licensing requirements. They gain liability protection and continued license to operate; they bear the direct cost of running realistic drills, but this cost is far lower than sustaining readiness through waiting for real incidents. Exit from the simulation regime would mean losing licensure entirely.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, operating_organizations, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, operating_organizations, payer).

% Design and sell high-fidelity simulators and exercise scenarios. They profit directly from the doctrine that simulation is sufficient; if regulators instead required real-event exposure or a fidelity threshold higher than current technology can reach, demand for their current product line would fall or shift toward higher-cost systems.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, simulation_vendors, beneficiary,
    moderate, biographical, mobile, global).

% Control room staff, pilots, surgeons, and similar personnel undergo the simulation exercises. They benefit from a repeatable, survivable way to build and demonstrate competence without personal exposure to actual catastrophic risk. Their situation is not extractive under this reading — the simulation regime is what lets them acquire and retain skill at all without waiting for a disaster to teach them.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, frontline_operators, beneficiary,
    moderate, biographical, constrained, national).

% Residents near a nuclear facility, airline passengers, hospital patients — the population whose safety depends on operator competence. Under this reading they are not victims: the claim is precisely that simulation-trained competence protects them adequately. They have no direct role in setting or auditing the standard, but the reading asserts their safety is not compromised by relying on it.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, public_served_by_the_system, observer,
    powerless, civilizational, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a way to build, test, and certify operational competence for catastrophic-consequence roles without requiring exposure to actual catastrophes, which would be unacceptably costly, irreversible, or simply too rare to train on reliably.
% TRANSFER_FUNCTION: Moves training investment from operating organizations to simulation infrastructure and vendor contracts, and moves liability exposure from operators to a documented, auditable competence record accepted by regulators.
% ABSENT_VOICES: Practitioners who have experienced genuine catastrophic events and report that simulations, however well-designed, do not replicate the physiological stress, ambiguity, and consequence-weight of the real event are represented within the sibling readings but are structurally minimized within this reading's own framing, which treats their testimony as anecdotal rather than as evidence of a fidelity gap.
% DISAPPEARANCE_RATIONALE: If the doctrine that simulation constitutes sufficient practice were abandoned, regulators would need an alternative competence-certification basis — either accepting genuine degradation risk, waiting for rare real events, or funding a fidelity-threshold research program (the sibling readings). Licensing regimes, vendor markets, and training budgets across high-reliability industries would all restructure.
% FOUNDING_PROBLEM: High-consequence operational roles (reactor operators, air traffic controllers, surgical teams) cannot ethically or practically be trained by exposure to repeated real catastrophes; some substitute practice mechanism was needed to build and verify competence between rare real events.
% FOUNDING_PROBLEM_CORROBORATION: Regulatory agencies and simulation vendors attest the problem is solved: certified simulation programs adequately maintain competence. Independent human-factors researchers and some post-incident investigation boards (e.g. commentary following the Three Mile Island and Fukushima post-mortems) attest the problem persists in a degraded form — tacit knowledge and stress-response capacity are not straightforwardly proxy-able — corroboration from outside the certifying and vending parties supports treating this as contested rather than resolved.
narrative_ontology:disappearance_verdict(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, 0.22, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading_tests).
:- end_tests(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.22, rising modestly to reflect increasing institutional reliance on the doctrine over time) because, under this reading, the training regime genuinely produces the competence it claims to produce — the transfer is training investment for certified competence, not extraction from a victim class. Suppression is moderate-low (0.28) because operators are not coerced into simulation training against an available better alternative; regulatory mandate exists but the doctrine itself is not defended by suppressing dissent so much as by the absence, under this reading, of a superior practicable alternative. Theater ratio is authored at a meaningful but non-dominant 0.3, rising over the interval — some drift toward box-checking compliance exercises is honest to model even within a reading that holds the coordination function is real, since institutionalized simulation regimes accumulate procedural theater over decades even where their core function remains sound. Accessibility collapse (0.4) reflects that once regulators adopt simulation-based certification, alternative certification paths (e.g., apprenticeship under live incident exposure) become practically unavailable — but this is a moderate, not near-total, collapse, since fidelity-threshold and hybrid approaches remain live alternatives argued for by other parties.
 *
 * DIRECTIONALITY LOGIC:
 *   Regulatory bodies sit near the beneficiary end: they get a workable oversight instrument at low administrative cost. Operating organizations are also beneficiaries under this reading — the claim is precisely that they receive genuine, sufficient competence-maintenance in exchange for training investment, not that they are extracted from. Simulation vendors benefit directly and are named separately from the certifying/operating institutions because their commercial interest in the doctrine's truth is a distinct structural fact worth tracking even though this reading holds the doctrine to be correct. No victim group is declared: under simulation_as_proxy_catastrophe_reading, competence is genuinely maintained, so there is no population being harmed by an unaddressed gap. This is the central structural delta separating this reading from catastrophe_necessity_reading and hybrid_degradation_reading, both of which would declare frontline_operators and public_served_by_the_system as victims of an unaddressed competence gap.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (need for a survivable, repeatable competence-building mechanism for catastrophic-consequence roles) is still structurally live — the underlying hazard has not disappeared — so this is not a case of an arrangement persisting past its function. What is contested is whether the CHOSEN solution (simulation-as-sufficient) actually discharges that live function or merely appears to. The mismatch-detection logic (founding_problem_status=contested, disappearance_verdict=world_rearranges) correctly flags this as a live-but-disputed arrangement rather than either a resolved coordination success or a dead-function extraction relic — appropriate given that corroboration is split between benefiting parties (who affirm sufficiency) and outside investigators (who report partial degradation, i.e., the hybrid reading's concern).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    categorical_vs_conditional_sufficiency,
    'Is simulation-based competence maintenance categorically sufficient regardless of simulator fidelity, or does sufficiency depend on crossing a fidelity threshold that current technology may or may not meet?',
    'Longitudinal comparison of incident-response performance between operators trained purely on current-generation simulators versus operators with documented real-event exposure, controlling for baseline skill; convergence in outcomes would support categorical sufficiency, divergence would support the fidelity-threshold reading.',
    'If sufficiency is conditional rather than categorical, this reading''s low-extractiveness, no-victim classification is wrong for any operator population trained on sub-threshold simulators — the true constraint experienced by those populations would be simulation_fidelity_threshold, not this one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(categorical_vs_conditional_sufficiency, empirical, 'Whether sufficiency is a categorical property of simulation-as-such or conditional on simulator fidelity.').

omega_variable(
    tacit_knowledge_measurability,
    'Can tacit knowledge and stress-response capacity — the specific competencies hybrid_degradation_reading claims decay without real catastrophes — be reliably measured at all, or does their unmeasurability make the sufficiency claim unfalsifiable in practice?',
    'Development of validated psychometric or physiological proxies for stress-response capacity under simulated versus real high-consequence conditions, applied to matched cohorts.',
    'If tacit competencies are unmeasurable, this reading''s claim of indefinite sufficiency cannot be empirically distinguished from the hybrid degradation reading''s claim of slow decay — the two readings would be observationally equivalent until a real catastrophe occurs, at which point the distinction may arrive too late to be corrective.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(tacit_knowledge_measurability, conceptual, 'Whether the key competence dimension this reading claims is preserved can be measured independently of an actual catastrophic test.').

omega_variable(
    regulatory_incentive_to_affirm_sufficiency,
    'Do regulatory bodies have an independent incentive to affirm this reading (avoiding the cost and disruption of mandating a stricter, real-exposure or higher-fidelity standard) that is separable from an honest assessment of whether simulation is actually sufficient?',
    'Compare regulatory posture across jurisdictions with different institutional structures (e.g., agencies funded independently of the industries they certify versus agencies with revolving-door staffing) to see whether affirmation of sufficiency correlates with structural capture indicators rather than with safety outcome data.',
    'If regulatory affirmation is substantially incentive-driven rather than evidence-driven, this reading''s classification of regulatory_bodies as a clean beneficiary understates a captured-agenda-setter dynamic that would push the true structure toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_incentive_to_affirm_sufficiency, empirical, 'Whether the regulatory beneficiary''s endorsement of sufficiency is independently evidenced or self-interested.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(cata_tr_t8, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, theater_ratio, 8, 0.21).
narrative_ontology:measurement(cata_tr_t16, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, theater_ratio, 16, 0.24).
narrative_ontology:measurement(cata_tr_t24, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, theater_ratio, 24, 0.26).
narrative_ontology:measurement(cata_tr_t32, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, theater_ratio, 32, 0.28).
narrative_ontology:measurement(cata_tr_t40, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, theater_ratio, 40, 0.3).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(cata_be_t8, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, base_extractiveness, 8, 0.17).
narrative_ontology:measurement(cata_be_t16, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, base_extractiveness, 16, 0.19).
narrative_ontology:measurement(cata_be_t24, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, base_extractiveness, 24, 0.2).
narrative_ontology:measurement(cata_be_t32, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, base_extractiveness, 32, 0.21).
narrative_ontology:measurement(cata_be_t40, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, base_extractiveness, 40, 0.22).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, 0.12).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, catastrophe_necessity_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, hybrid_degradation_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, simulation_fidelity_threshold).

% DUAL FORMULATION NOTE:
% This story is one of four sibling readings of the catastrophe_proxy_sufficiency kernel. simulation_as_proxy_catastrophe_reading claims categorical, indefinite sufficiency (low ε, no victims, rope classification). catastrophe_necessity_reading claims categorical insufficiency (high ε, victims = public_served_by_the_system and frontline_operators exposed to undetected skill decay, likely snare or tangled_rope classification). hybrid_degradation_reading claims partial, generational-timescale insufficiency (moderate ε, tangled_rope likely, since procedural competence genuinely is coordinated while tacit competence quietly decays). simulation_fidelity_threshold makes sufficiency conditional on simulator technology (ε structurally intermediate and technology-dependent, may itself decompose further by simulator generation). Each reading is authored as its own constraint with its own stable ε per the ε-invariance principle; none of the four files average or hedge across the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
