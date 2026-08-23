% ============================================================================
% CONSTRAINT STORY: catastrophe_proxy_sufficiency__hybrid_degradation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_proxy_sufficiency__hybrid_degradation_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: catastrophe_proxy_sufficiency__hybrid_degradation_reading
 *   human_readable: Simulation-Based Competence Maintenance with Generational Tacit Decay
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   In high-reliability organizations, recurrent simulation training is
 *   mandated to maintain emergency-response competence without exposing
 *   personnel to actual catastrophic events. This constraint story treats the
 *   standing arrangement â mandatory simulation as the primary
 *   competence-maintenance mechanism â from the hybrid_degradation reading:
 *   simulation successfully preserves explicit, procedural competence
 *   (checklists, sequences, communication protocols) but fails to transmit
 *   tacit knowledge and stress-response capacity that historically accrued
 *   through real catastrophe exposure and generational mentorship. Over
 *   successive generations, the tacit layer degrades while the procedural
 *   layer appears intact, creating a hidden decay mechanism. The
 *   certification industry captures ongoing revenue from the mandate;
 *   frontline operators and downstream populations bear the deferred
 *   catastrophic risk. The constraint is authored as a tangled rope because
 *   it contains a genuine coordination function (procedural rehearsal) and an
 *   asymmetric extraction function (revenue capture and risk displacement).
 *
 * KEY AGENTS:
 *   - certification_industry: Beneficiary (organized/mobile) â collects recurrent training revenue from mandated simulation.
 *   - regulatory_bodies: Agenda setter (institutional/constrained) â mandates simulation hours and audits compliance.
 *   - frontline_operators: Payer (moderate/constrained) â participates in simulation; loses tacit mentorship and stress conditioning over generations.
 *   - downstream_communities: Payer (powerless/trapped) â bears catastrophic risk from degraded tacit competence without knowledge of the decay.
 *   - retired_event_experienced_operators: Excluded (moderate/analytical) â possess irreplaceable event-tacit knowledge but are sidelined by standardized curricula.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 0.61).
domain_priors:suppression_score(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 0.58).
domain_priors:theater_ratio(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__hybrid_degradation_reading, extractiveness, 0.61).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__hybrid_degradation_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__hybrid_degradation_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_proxy_sufficiency__hybrid_degradation_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_proxy_sufficiency__hybrid_degradation_reading, "Simulation-Based Competence Maintenance with Generational Tacit Decay").
narrative_ontology:topic_domain(catastrophe_proxy_sufficiency__hybrid_degradation_reading, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(catastrophe_proxy_sufficiency__hybrid_degradation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_proxy_sufficiency__hybrid_degradation_reading, '5aa1c395-12fd-4b8c-a191-990545cecf98').
narrative_ontology:cs_kernel_codification('5aa1c395-12fd-4b8c-a191-990545cecf98', distributed).
narrative_ontology:cs_authority_grounding('5aa1c395-12fd-4b8c-a191-990545cecf98', distributed).
narrative_ontology:cs_reading_relation('5aa1c395-12fd-4b8c-a191-990545cecf98', catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, coexists_with).
narrative_ontology:cs_reading_relation('5aa1c395-12fd-4b8c-a191-990545cecf98', catastrophe_proxy_sufficiency__catastrophe_necessity_reading, coexists_with).
narrative_ontology:cs_reading_relation('5aa1c395-12fd-4b8c-a191-990545cecf98', catastrophe_proxy_sufficiency__simulation_fidelity_threshold, influences).
narrative_ontology:cs_axiom('5aa1c395-12fd-4b8c-a191-990545cecf98', foundational, simulation_preserves_procedure_not_tacit_competence).
narrative_ontology:cs_axiom_status(simulation_preserves_procedure_not_tacit_competence, holdable).
narrative_ontology:cs_axiom_grounding('5aa1c395-12fd-4b8c-a191-990545cecf98', simulation_preserves_procedure_not_tacit_competence, empirically_contingent).
narrative_ontology:cs_reference_frame('5aa1c395-12fd-4b8c-a191-990545cecf98', generational_competence_transfer).
narrative_ontology:cs_drift_state('5aa1c395-12fd-4b8c-a191-990545cecf98', contemporary_simulation_mandate_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5aa1c395-12fd-4b8c-a191-990545cecf98', '').
narrative_ontology:cs_kernel_id(catastrophe_proxy_sufficiency__hybrid_degradation_reading, catastrophe_proxy_sufficiency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__hybrid_degradation_reading, certification_industry).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__hybrid_degradation_reading, frontline_operators).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__hybrid_degradation_reading, downstream_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs, accredits, and administers recurrent simulation curricula and certification exams. Revenue depends on regulatory mandates that require periodic simulation-based recertification. Benefits from the institutionalization of simulation as the default competence-maintenance mechanism.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, certification_industry, beneficiary,
    organized, generational, mobile, global).

% Mandates minimum simulation hours for operator licensing and audits organizational compliance. Justifies requirements as evidence-based risk reduction. Does not directly collect training revenue but administers the rule structure that makes simulation obligatory.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, regulatory_bodies, agenda_setter,
    institutional, generational, constrained, national).

% Completes mandated recurrent simulation to maintain certification. Procedural skills are rehearsed and verified, but opportunities to acquire tacit judgment under genuine uncertainty diminish as event-experienced mentors retire. Bears the accumulated risk of degraded stress-response capacity when a real catastrophe eventually occurs.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, frontline_operators, payer,
    moderate, biographical, constrained, regional).

% Resides near high-risk facilities and depends on operator competence for safety margins. Unaware of the generational decay in tacit knowledge and stress conditioning. Would bear the catastrophic cost if simulation-proxied competence fails under real event stress.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, downstream_communities, payer,
    powerless, generational, trapped, local).

% Possess tacit knowledge and stress-response conditioning gained through actual catastrophic events. Increasingly excluded from formal training design as curricula become simulation-standardized. Their testimony would challenge the sufficiency of simulation but is not solicited in certification governance.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, retired_event_experienced_operators, excluded,
    moderate, biographical, analytical, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_proxy_sufficiency__hybrid_degradation_reading, certification_industry).
narrative_ontology:fixing_cost_class(catastrophe_proxy_sufficiency__hybrid_degradation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a standardized, repeatable, low-risk environment in which operators rehearse emergency protocols, communication sequences, and checklist discipline without waiting for actual catastrophic events.
% TRANSFER_FUNCTION: Moves recurring revenue from high-reliability organizations and operators to the certification industry; moves catastrophic risk from the present generation toward future generations and downstream populations as tacit competence decays while procedural compliance appears intact.
% ABSENT_VOICES: Retired operators with real catastrophe experience are structurally excluded from curriculum design; junior operators who have never known non-simulated stress cannot articulate what tacit knowledge is missing and therefore do not contest the sufficiency claim.
% DISAPPEARANCE_RATIONALE: If the simulation mandate vanished, training budgets would reallocate, certification industry revenue would collapse, regulatory licensing frameworks would require redesign, and organizations would revert toward apprenticeship or event-dependent learning models. The hidden generational decay would cease accumulating, though procedural rehearsal would need replacement.
% FOUNDING_PROBLEM: Catastrophic failures in high-risk industries revealed that procedural competence atrophies without practice; early safety governance needed a way to rehearse emergency responses without exposing personnel to actual disasters.
% FOUNDING_PROBLEM_CORROBORATION: Retired senior operators and independent accident-investigation bodies (e.g., NTSB, IAEA) attest that real events produce non-transferable learning and that mentorship gaps are widening. The certification industry attests the problem remains live to justify ongoing mandates; independent safety researchers and human-factors psychologists corroborate that tacit dimensions are not captured by current simulation technology.
narrative_ontology:disappearance_verdict(catastrophe_proxy_sufficiency__hybrid_degradation_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_proxy_sufficiency__hybrid_degradation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 0.61, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_proxy_sufficiency__hybrid_degradation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_proxy_sufficiency__hybrid_degradation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_proxy_sufficiency__hybrid_degradation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.61) is authored to reflect moderate-high extraction: the certification industry captures recurring revenue, and the deferred risk to operators and public represents a transferred cost that is not priced into the training market. Suppression (0.58) reflects the regulatory foreclosure of alternative competence-building pathways (e.g., apprenticeship under event-experienced mentors) in favor of standardized simulation. Theater ratio (0.42) captures the performative element: simulation sessions produce demonstrable compliance metrics and certification records that signal competence while the tacit decay is invisible. Accessibility collapse (0.70) is high because once simulation is mandated and institutionalized, the alternative â learning through real catastrophe â is deliberately unavailable and morally unacceptable. Resistance (0.48) captures ongoing skepticism from event-experienced operators and some safety researchers, countered by the institutional weight of the certification-regulatory complex.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (regulatory bodies) experiences the constraint as a legitimate, evidence-based coordination mechanism that prevents accidents. The payer seats (frontline operators and downstream communities) experience it as a compliance burden that gradually erodes the very competence it claims to protect, without their consent to the risk transfer. The beneficiary seat (certification industry) experiences it as a stable revenue stream. The engine computes these divergences from the structural data; the authored claim (tangled_rope) does not resolve them.
 *
 * DIRECTIONALITY LOGIC:
 *   Certification industry is declared beneficiary â it collects rents from mandated recurrent training, giving it a low directionality (structurally subsidized by the constraint). Regulatory bodies are not declared beneficiary or victim; their directionality defaults to the institutional power atom's canonical fallback, reflecting agenda-setting without direct extraction. Frontline operators are declared victims (payers) because they bear the hidden decay in tacit competence and the deferred catastrophic stress load. Downstream communities are declared victims (payers) because they inhabit the risk shadow of degraded operator competence. No directionality overrides are needed because the structural derivation chain produces accurate d values: beneficiaries near subsidy, victims near full target.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids mislabeling because the coordination function is real and independently identifiable: simulation genuinely preserves procedural competence, communication protocols, and checklist discipline. Without naming that genuine coordination, the constraint would compute as a snare. By naming it and also naming the asymmetric extraction (certification revenue and deferred risk), the tangled_rope classification captures the hybrid structure. If the coordination function were to atrophy entirely and only the revenue capture remained, the constraint would degrade toward snare or piton; if the decay mechanism were solved (e.g., fidelity breakthrough restoring tacit transfer), it would shift toward rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tacit_decay_measurable,
    'Is the degradation of tacit knowledge and stress-response capacity under simulation-only regimes empirically detectable before a catastrophe occurs?',
    'Longitudinal studies comparing simulator performance with real-event outcomes across generations; forensic analysis of near-misses for competence gaps.',
    'If measurable, the extraction is visible and the constraint may be redesignable; if not, the decay remains hidden and the constraint functions as a deferred snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tacit_decay_measurable, empirical, 'Whether generational tacit decay is detectable pre-catastrophe').

omega_variable(
    certification_industry_capture,
    'Does the certification industry''s revenue dependence on recurrent simulation training constitute regulatory capture of safety governance?',
    'Disclosure of lobbying expenditure relative to curriculum development; comparison of mandated hours against empirical learning-curve minima.',
    'If capture is established, the coordination function is subordinated to extraction; if not, the revenue is a fair price for the coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(certification_industry_capture, conceptual, 'Whether certification revenue distorts safety governance').

omega_variable(
    kernel_sibling_structure,
    'This constraint is the hybrid_degradation_reading of kernel catastrophe_proxy_sufficiency. Sibling readings differ on whether simulation is fully equivalent to catastrophe (simulation_as_proxy_catastrophe_reading), strictly insufficient (catastrophe_necessity_reading), or conditionally sufficient (simulation_fidelity_threshold). The disagreement is located on whether tacit knowledge and stress response are transferrable via simulation and whether generational decay is a significant variable.',
    'Comparative empirical tracking of real-catastrophe outcomes across organizations with different simulation intensities and generational exposure to real events.',
    'Resolving the disagreement reclassifies the constraint from tangled_rope toward rope (if decay is negligible) or snare (if decay is severe and hidden).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_sibling_structure, conceptual, 'Structural ambiguity between sibling readings of the catastrophe proxy kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(cata_tr_t10, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 10, 0.24).
narrative_ontology:measurement(cata_tr_t20, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(cata_tr_t30, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 30, 0.36).
narrative_ontology:measurement(cata_tr_t40, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement(cata_tr_t50, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 50, 0.42).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(cata_be_t10, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 10, 0.3).
narrative_ontology:measurement(cata_be_t20, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement(cata_be_t30, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 30, 0.46).
narrative_ontology:measurement(cata_be_t40, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 40, 0.54).
narrative_ontology:measurement(cata_be_t50, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 50, 0.61).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(catastrophe_proxy_sufficiency__hybrid_degradation_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_proxy_sufficiency__hybrid_degradation_reading, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
