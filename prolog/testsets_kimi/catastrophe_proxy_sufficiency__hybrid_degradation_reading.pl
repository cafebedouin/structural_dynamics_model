% ============================================================================
% CONSTRAINT STORY: catastrophe_proxy_sufficiency__hybrid_degradation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:affects_constraint/2,
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
 *   human_readable: Simulation-Based Catastrophe Proxy with Generational Tacit Decay
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   High-reliiability organizations rely on simulation as a proxy for
 *   catastrophe to maintain operator competence. Under the
 *   hybrid_degradation_reading of the catastrophe_proxy_sufficiency kernel,
 *   this regime genuinely preserves procedural and checklist competence but
 *   simultaneously produces a hidden generational decay mechanism: tacit
 *   knowledge, improvisational judgment, and stress-response capacity erode
 *   over successive cohorts that never encounter real catastrophic stress.
 *   The certification industry captures recurring revenue from mandatory
 *   retraining cycles, while frontline operators and the public bear the cost
 *   of degraded embodied competence. The constraint is authored as
 *   tangled_rope because the coordination function (safe procedural training)
 *   is real and necessary, while the asymmetric extraction (generational
 *   tacit decay, catastrophic risk transfer) is structurally inseparable from
 *   it.
 *
 * KEY AGENTS:
 *   - certification_industry: Primary beneficiary/agenda_setter (powerful/mobile) â designs curricula, sets accreditation standards, and collects recurring revenue from mandatory retraining.
 *   - safety_regulators: Agenda_setter (institutional/constrained) â mandates simulation hours and audits procedural compliance, dependent on visible metrics.
 *   - frontline_operators: Primary payer (moderate/constrained) â completes mandated training, retains procedural fluency, loses unmeasured tacit and stress-response capacity over generations.
 *   - public_at_risk: Secondary payer (powerless/trapped) â bears catastrophic downside risk without voice in training design.
 *   - veteran_operators: Excluded voice (moderate/identity_locked) â holds catastrophe experience but is retiring and treated as anecdotal.
 *   - hro_researchers: Analytical observer (analytical/analytical) â measures decay but lacks authority to change mandates.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 0.62).
domain_priors:suppression_score(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 0.58).
domain_priors:theater_ratio(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__hybrid_degradation_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__hybrid_degradation_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__hybrid_degradation_reading, resistance, 0.32).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_proxy_sufficiency__hybrid_degradation_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_proxy_sufficiency__hybrid_degradation_reading, "Simulation-Based Catastrophe Proxy with Generational Tacit Decay").
narrative_ontology:topic_domain(catastrophe_proxy_sufficiency__hybrid_degradation_reading, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(catastrophe_proxy_sufficiency__hybrid_degradation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 'baaf3f9a-b459-4fa6-a8e6-82a68ced380e').
narrative_ontology:cs_kernel_codification('baaf3f9a-b459-4fa6-a8e6-82a68ced380e', distributed).
narrative_ontology:cs_authority_grounding('baaf3f9a-b459-4fa6-a8e6-82a68ced380e', distributed).
narrative_ontology:cs_reading_relation('baaf3f9a-b459-4fa6-a8e6-82a68ced380e', catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, forecloses).
narrative_ontology:cs_reading_relation('baaf3f9a-b459-4fa6-a8e6-82a68ced380e', catastrophe_proxy_sufficiency__catastrophe_necessity_reading, influences).
narrative_ontology:cs_reading_relation('baaf3f9a-b459-4fa6-a8e6-82a68ced380e', catastrophe_proxy_sufficiency__simulation_fidelity_threshold, coexists_with).
narrative_ontology:cs_axiom('baaf3f9a-b459-4fa6-a8e6-82a68ced380e', foundational, procedural_competence_diverges_from_embodied_competence).
narrative_ontology:cs_axiom_status(procedural_competence_diverges_from_embodied_competence, holdable).
narrative_ontology:cs_axiom_grounding('baaf3f9a-b459-4fa6-a8e6-82a68ced380e', procedural_competence_diverges_from_embodied_competence, empirically_contingent).
narrative_ontology:cs_axiom('baaf3f9a-b459-4fa6-a8e6-82a68ced380e', foundational, generational_stress_response_decay_without_inoculation).
narrative_ontology:cs_axiom_status(generational_stress_response_decay_without_inoculation, holdable).
narrative_ontology:cs_axiom_grounding('baaf3f9a-b459-4fa6-a8e6-82a68ced380e', generational_stress_response_decay_without_inoculation, empirically_contingent).
narrative_ontology:cs_reference_frame('baaf3f9a-b459-4fa6-a8e6-82a68ced380e', integrated_competence_preservation).
narrative_ontology:cs_drift_state('baaf3f9a-b459-4fa6-a8e6-82a68ced380e', simulation_dependent_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('baaf3f9a-b459-4fa6-a8e6-82a68ced380e', '').
narrative_ontology:cs_kernel_id(catastrophe_proxy_sufficiency__hybrid_degradation_reading, catastrophe_proxy_sufficiency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__hybrid_degradation_reading, certification_industry).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__hybrid_degradation_reading, frontline_operators).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__hybrid_degradation_reading, public_at_risk).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develops and delivers mandatory simulation-based training and recertification programs for safety-critical industries. Collaborates with regulators to set curriculum standards and accreditation criteria. Revenue depends on recurring training hours, license renewals, and the institutionalized need for recurrent certification.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, certification_industry, agenda_setter,
    powerful, biographical, mobile, national).

% Establish training-hour mandates and accreditation rules for safety-critical licenses. Audit compliance through procedural checklists and simulation performance metrics. Depend on visible certification throughput to demonstrate regulatory efficacy to legislatures and the public.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, safety_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Complete scheduled simulation sessions and procedural recertification to maintain employment eligibility. Retain protocol fluency and checklist discipline. Report that intuitive judgment and calm under novel pressure feel harder to access as careers progress without exposure to real high-stress events.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, frontline_operators, payer,
    moderate, biographical, constrained, local).

% Lives and works near safety-critical infrastructure operated by certified personnel. Has no direct input into training design or operator selection. Bears catastrophic consequence if rare high-stress events exceed procedural competence, without visibility into generational decay inside the training regime.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, public_at_risk, payer,
    powerless, generational, trapped, national).

% Hold embodied experience from historical catastrophes or near-misses. Their testimony about irreplaceable stress inoculation and improvisational knowledge is treated as anecdotal or ungeneralizable in curriculum design. Retiring from the workforce and taking unrecorded adaptive knowledge with them.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, veteran_operators, excluded,
    moderate, biographical, identity_locked, local).

% Conduct empirical studies on expertise retention, stress inoculation, and generational skill transfer in high-reliability organizations. Publish findings on simulator-induced competence plateaus and tacit knowledge decay but lack institutional authority to alter training mandates.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, hro_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_proxy_sufficiency__hybrid_degradation_reading, certification_industry).
narrative_ontology:fixing_cost_class(catastrophe_proxy_sufficiency__hybrid_degradation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a safe, repeatable, standardized environment for maintaining procedural and protocol competence across safety-critical workforces without requiring actual catastrophic events.
% TRANSFER_FUNCTION: Moves financial resources from safety-critical organizations and frontline operators to certification providers through recurring training cycles; moves tacit knowledge and stress-response capacity from future generations into present-day procedural compliance metrics that do not capture them.
% ABSENT_VOICES: Veteran operators who experienced real catastrophes and could attest to the irreplaceability of embodied stress-response; future victims of catastrophes where degraded tacit knowledge proves decisive. Neither group is represented in training design or accreditation committees.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, safety-critical industries would lose their standardized procedural training backbone. Organizations would be forced to redesign competence models around apprenticeship, selective catastrophe exposure, or unproven high-fidelity simulation alternatives. The certification economy would contract and safety economics would reorganize around different liability and expertise models.
% FOUNDING_PROBLEM: Actual catastrophic events are too rare, dangerous, and ethically fraught to serve as routine training environments; organizations needed a safe, repeatable, scalable way to maintain operator competence between incidents.
% FOUNDING_PROBLEM_CORROBORATION: The need for safe training is corroborated by nuclear, aviation, and medical safety historians who documented unacceptable historical incident rates. However, the claim that simulation suffices for full generational competence is corroborated mainly by the certification industry itself; high-reliability organization researchers and veteran practitioners outside the training-revenue stream attest that the arrangement has drifted toward procedural compliance theater.
narrative_ontology:disappearance_verdict(catastrophe_proxy_sufficiency__hybrid_degradation_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_proxy_sufficiency__hybrid_degradation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_proxy_sufficiency__hybrid_degradation_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 0.62, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.62) is substantial because generational decay represents a hidden transfer of catastrophic risk and capability erosion into the future while revenue is captured presently. Suppression (0.58) reflects the institutional difficulty of measuring tacit decay and the accreditation system's systematic discounting of veteran testimony. Theater_ratio (0.48) captures the performative component: recertification cycles produce visible compliance metrics that increasingly mask a widening gap between procedural scores and embodied readiness. Accessibility_collapse (0.72) is high because once a simulation regime is institutionalized, ethical barriers and political impossibility prevent returning to catastrophe-based learning, and alternative pedagogies lack accreditation pathways. Resistance (0.32) is low-to-moderate because veteran operators are exiting via retirement and frontline operators are individually replaceable.
 *
 * PERSPECTIVAL GAP:
 *   The certification_industry seat experiences the constraint as a legitimate professional service satisfying regulatory demand and safety ethics. The frontline_operators seat experiences it as mandatory credentialing that proceduralizes their work while eroding unmeasured adaptive capabilities. The public_at_risk seat experiences it as an invisible transfer of catastrophic risk masked by compliance signage. The engine computes these divergences from the same structural data; the authored claim does not adjudicate which seat is correct.
 *
 * DIRECTIONALITY LOGIC:
 *   certification_industry is declared in beneficiaries and sits as agenda_setter with mobile exit â structural relationship is beneficiary-side, yielding low derived directionality. frontline_operators and public_at_risk are declared in victims with constrained and trapped exit respectively â structural relationship is target-side, yielding high derived directionality. safety_regulators are agenda_setters but not beneficiaries; their directionality is moderate because they are captured by the metric system but do not personally accrue the extraction. Effective extraction computed by the engine will be amplified for public_at_risk (powerless, national scope) and frontline_operators, and damped for certification_industry.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â the ethical and practical impossibility of using real catastrophes as routine training â remains live. This prevents classifying the constraint as a pure snare: there is genuine coordination in procedural competence maintenance. However, the generational decay of tacit knowledge and stress-response capacity creates asymmetric extraction that the coordination function does not justify. Classifying it as rope would erase the victim structure (frontline operators, public at risk); classifying it as snare would deny the real coordination. Tangled rope is the structurally accurate type because both components are present and inseparable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tacit_knowledge_measurability,
    'Is tacit knowledge and stress-response degradation measurable through prospective instruments before a catastrophic failure occurs?',
    'Longitudinal performance studies comparing simulation-only lineages against lineages with real stress exposure, using novel emergency scenarios rather than rehearsed protocols.',
    'If degradation is measurable, the extraction becomes visible and the constraint could be reclassified toward snare or forced toward scaffold reform; if immeasurable, the tangled rope persists because the victim mechanism remains hidden.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tacit_knowledge_measurability, empirical, 'Whether generational tacit decay can be detected prospectively.').

omega_variable(
    generational_decay_rate,
    'What is the actual half-life of tacit knowledge and stress-response capacity in simulation-only regimes?',
    'Multi-decade performance and near-miss analysis from industries with long simulation-dependent periods and rare catastrophes, such as commercial aviation and nuclear power.',
    'A short half-life would push the constraint toward severe extraction and potential reclassification; a very long half-life would support the coordination framing and reduce effective extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(generational_decay_rate, empirical, 'Speed of generational competence decay in simulation-only training.').

omega_variable(
    simulation_extraction_separability,
    'Can the procedural coordination function of simulation be preserved while eliminating the generational extraction mechanism?',
    'Natural experiment from organizations that integrate high-stress non-simulation modalities (controlled live exercises, veteran mentorship, surprise drills) alongside standard simulation.',
    'If separable, the constraint could be re-engineered into a rope or scaffold; if inseparable, the extraction is structurally baked into the simulation-proxy model itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_extraction_separability, conceptual, 'Whether coordination and extraction components are structurally separable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(cata_tr_t12, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 12, 0.22).
narrative_ontology:measurement(cata_tr_t24, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 24, 0.3).
narrative_ontology:measurement(cata_tr_t36, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 36, 0.38).
narrative_ontology:measurement(cata_tr_t48, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 48, 0.44).
narrative_ontology:measurement(cata_tr_t60, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 60, 0.48).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(cata_be_t12, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 12, 0.3).
narrative_ontology:measurement(cata_be_t24, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 24, 0.4).
narrative_ontology:measurement(cata_be_t36, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 36, 0.5).
narrative_ontology:measurement(cata_be_t48, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 48, 0.57).
narrative_ontology:measurement(cata_be_t60, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 60, 0.62).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(catastrophe_proxy_sufficiency__hybrid_degradation_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_proxy_sufficiency__hybrid_degradation_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__hybrid_degradation_reading, simulation_as_proxy_catastrophe_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__hybrid_degradation_reading, catastrophe_necessity_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__hybrid_degradation_reading, simulation_fidelity_threshold).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the catastrophe_proxy_sufficiency kernel. The kernel decomposes into four structurally distinct claims about the relationship between simulation, catastrophe, and competence maintenance. This reading isolates the generational tacit-decay mechanism; sibling readings address categorical sufficiency, categorical necessity, and fidelity-conditional sufficiency.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
