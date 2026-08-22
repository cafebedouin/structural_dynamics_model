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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: catastrophe_proxy_sufficiency__hybrid_degradation_reading
 *   human_readable: Simulation-Based Certification Masking Tacit Knowledge Degradation
 *   domain: safety/organizational_learning
 *
 * SUMMARY:
 *   High-reliability organizations (aviation, nuclear power, surgery,
 *   emergency response) train operators through simulation-based
 *   certification. Simulation is cheaper and safer than
 *   catastrophe-equivalent real-world practice, and is defended by the
 *   certification industry and regulators as sufficient for maintaining
 *   competence. However, this reading claims that simulation maintains
 *   procedural competence (the checklist, the manual response sequence) while
 *   tacit knowledge and stress-response capacity degrade over generational
 *   timescales without real catastrophes to anchor practice. Current
 *   operators trained during or shortly after real catastrophes or with field
 *   mentoring remain competent; post-catastrophe cohorts enter the profession
 *   with procedural skill but no embodied stress experience, no intuitive
 *   pattern-recognition for anomalies, and no peer mentors who experienced
 *   real failure modes. The certification system masks this gap, creating
 *   latent fragility. The constraint is CLAIMED as tangled_rope: it
 *   coordinates training (solves the problem of how to certify operators at
 *   scale) while extracting hidden long-term safety margin loss through
 *   knowledge degradation. The beneficiary is the certification and training
 *   industry (recurring revenue from recertification), and the victims are
 *   long-term safety margins (the future performance degradation) and
 *   post-catastrophe-cohort operators (identity-locked into a profession
 *   where their certification masks inadequacy). The theater ratio rises over
 *   the interval because certification increasingly performs the function of
 *   assuring stakeholders of safety while the actual safety substrate (tacit
 *   knowledge, stress-response readiness) atrophies.
 *
 * KEY AGENTS:
 *   - Certification and training industry: institutional power, sets curriculum and standards, economically invested in simulation sufficiency.
 *   - Operating organizations: powerful, financially constrained, benefit from reduced training cost but bear hidden long-term fragility.
 *   - Current operators (trained with real-event exposure): moderate power, possess tacit knowledge, benefit from simulation as refresh rather than foundation.
 *   - Post-catastrophe-cohort operators: moderate power, identity-locked into profession, certified competent via simulation alone, lack embodied stress experience.
 *   - Regulatory authorities: institutional power, set standards, institutionally invested in current system's safety record.
 *   - Researchers and accident investigators: moderate power, analytical seat, document evidence of degradation but lack authority to mandate change.
 *   - Future operators (not yet in profession): powerless, trapped, will inherit systems and mentors degraded by knowledge loss.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 0.68).
domain_priors:suppression_score(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 0.71).
domain_priors:theater_ratio(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__hybrid_degradation_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__hybrid_degradation_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__hybrid_degradation_reading, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_proxy_sufficiency__hybrid_degradation_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_proxy_sufficiency__hybrid_degradation_reading, "Simulation-Based Certification Masking Tacit Knowledge Degradation").
narrative_ontology:topic_domain(catastrophe_proxy_sufficiency__hybrid_degradation_reading, "safety/organizational_learning").

domain_priors:requires_active_enforcement(catastrophe_proxy_sufficiency__hybrid_degradation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_proxy_sufficiency__hybrid_degradation_reading, '8d26d892-a0bd-4bba-bc8c-9865d0557e81').
narrative_ontology:cs_kernel_codification('8d26d892-a0bd-4bba-bc8c-9865d0557e81', fixed_text).
narrative_ontology:cs_authority_grounding('8d26d892-a0bd-4bba-bc8c-9865d0557e81', expertise).
narrative_ontology:cs_interpretation_layer_present('8d26d892-a0bd-4bba-bc8c-9865d0557e81').
narrative_ontology:cs_reading_relation('8d26d892-a0bd-4bba-bc8c-9865d0557e81', catastrophe_proxy_sufficiency__catastrophe_necessity_reading, coexists_with).
narrative_ontology:cs_reading_relation('8d26d892-a0bd-4bba-bc8c-9865d0557e81', catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, coexists_with).
narrative_ontology:cs_reading_relation('8d26d892-a0bd-4bba-bc8c-9865d0557e81', catastrophe_proxy_sufficiency__simulation_fidelity_threshold, influences).
narrative_ontology:cs_axiom('8d26d892-a0bd-4bba-bc8c-9865d0557e81', foundational, tacit_knowledge_irreplaceable_by_simulation).
narrative_ontology:cs_axiom_status(tacit_knowledge_irreplaceable_by_simulation, holdable).
narrative_ontology:cs_axiom_grounding('8d26d892-a0bd-4bba-bc8c-9865d0557e81', tacit_knowledge_irreplaceable_by_simulation, empirically_contingent).
narrative_ontology:cs_axiom('8d26d892-a0bd-4bba-bc8c-9865d0557e81', secondary, stress_response_decay_over_generations).
narrative_ontology:cs_axiom_status(stress_response_decay_over_generations, holdable).
narrative_ontology:cs_axiom_grounding('8d26d892-a0bd-4bba-bc8c-9865d0557e81', stress_response_decay_over_generations, empirically_contingent).
narrative_ontology:cs_reference_frame('8d26d892-a0bd-4bba-bc8c-9865d0557e81', simulation_procedural_equivalence).
narrative_ontology:cs_drift_state('8d26d892-a0bd-4bba-bc8c-9865d0557e81', post_generational_turnover_contemporary, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('8d26d892-a0bd-4bba-bc8c-9865d0557e81', '').
narrative_ontology:cs_kernel_id(catastrophe_proxy_sufficiency__hybrid_degradation_reading, catastrophe_proxy_sufficiency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__hybrid_degradation_reading, certification_and_training_industry).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__hybrid_degradation_reading, long_term_safety_margins).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__hybrid_degradation_reading, real_catastrophe_response_readiness).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__hybrid_degradation_reading, operating_organizations).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__hybrid_degradation_reading, individual_operators_current_cohort).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__hybrid_degradation_reading, operating_organizations).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__hybrid_degradation_reading, individual_operators_post_catastrophe_cohort).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__hybrid_degradation_reading, next_generation_post_catastrophe_cohort).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs, administers, and certifies simulation-based training for high-reliability operators (pilots, nuclear engineers, surgeons, emergency responders). Collects recurring revenue from mandatory recertification cycles and simulation licensing. Has structural incentive to defend simulation sufficiency because admitting degradation would require more expensive real-world training or extended intervals, reducing certification volume and revenue.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, certification_and_training_industry, agenda_setter,
    institutional, generational, arbitrage, global).

% Pay for simulation-based training and certification of their personnel (pilots, operators, emergency teams). Benefit from reduced training costs (simulation is cheaper than real-world practice) and regulatory acceptance (certification signed off by recognized industry bodies). Also bear hidden cost: as generational turnover occurs without intervening real catastrophes, the organization's accumulated tacit knowledge and stress-response capacity slowly atrophy, creating latent fragility.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, operating_organizations, beneficiary,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_proxy_sufficiency__hybrid_degradation_reading, operating_organizations, payer).

% Gain certification and career advancement through simulation-based training. For operators who trained during or shortly after real catastrophes, or who have field experience, simulation adequately refreshes procedural knowledge. They also carry tacit knowledge embedded in their practice from prior high-stress exposure, which simulation supplements rather than replaces.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, individual_operators_current_cohort, beneficiary,
    moderate, biographical, constrained, global).

% Operators who enter the profession generations after the last real catastrophe in their domain (e.g., commercial pilots after the last major airline crash, surgeons after resolution of a historical surgical crisis, nuclear operators born after Three Mile Island). They are certified competent via simulation but have no embodied stress experience, no intuitive pattern-recognition for genuine anomalies under pressure, and no peer mentors who experienced real failure modes. Their certification masks this gap. Professional identity locks them into the role; admitting inadequacy carries career risk.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, individual_operators_post_catastrophe_cohort, payer,
    moderate, biographical, identity_locked, global).

% Set minimum training and recertification standards, approve simulation-based curricula, and audit compliance. Have political and institutional investment in the current system's safety record. Pressure to maintain it comes from both the industry (cost minimization) and operators (career protection). Admitting simulation is insufficient would require raising standards, which creates conflict with cost and career constituencies.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, regulatory_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Study operator decision-making, near-misses, and failures. Accumulate evidence that operators certified only via simulation show degraded pattern recognition and stress-response performance compared to those with real-event exposure. Publish findings but have limited authority to change standards; their evidence feeds the knowledge base but not the certification decisions.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, domain_researchers_and_accident_investigators, observer,
    moderate, generational, analytical, global).

% Future operators (not yet in the profession or in early training) who will inherit systems operated and certified under simulation-only regimes. They will be trained by operators themselves lacking real-catastrophe experience, creating cascading knowledge loss. If and when a real catastrophe occurs, they will face it with only procedural knowledge, no tacit depth. They have no voice in current certification debates and cannot opt out of the profession they are training for.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, next_generation_post_catastrophe_cohort, payer,
    powerless, biographical, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_proxy_sufficiency__hybrid_degradation_reading, certification_and_training_industry).
narrative_ontology:fixing_cost_class(catastrophe_proxy_sufficiency__hybrid_degradation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Simulation-based training solves the coordination problem of certifying large numbers of operators without the prohibitive cost and risk of real-world catastrophe practice. It standardizes procedural competence assessment and creates a uniform credentialing pathway across organizations and jurisdictions.
% TRANSFER_FUNCTION: Moves recurring revenue from operating organizations and regulatory budgets to certification and training providers. Moves labor (operator time) into simulation practice rather than field mentoring. Over generations, moves tacit knowledge and stress-response capacity out of the profession and into an irretrievable institutional loss (knowledge not transmitted to the next cohort).
% ABSENT_VOICES: Operators from pre-catastrophe eras (historical cohorts with real-event experience) are aging out or retired and have diminishing influence on current standards. Future post-catastrophe-cohort operators (not yet born or in training) have no voice in current policy. Accident investigators and researchers who document the degradation are present but have limited authority to mandate change.
% DISAPPEARANCE_RATIONALE: If simulation-based certification collapsed and real-world training or catastrophe-equivalent practice became mandatory, the training volume and cost structure would change radically, certification cycles would extend, and operating organizations would face different economics and timelines. Regulatory authority would shift from what training modalities are acceptable to what fidelity thresholds are required.
% FOUNDING_PROBLEM: High-reliability operations cannot afford to train operators on real catastrophes (the cost and risk are prohibitive). Simulation emerged as a solution to provide catastrophe-equivalent stress and decision-making practice without the actual catastrophe.
% FOUNDING_PROBLEM_CORROBORATION: The industry and regulatory authorities attest the founding problem is solved: simulation provides adequate stress-testing. Domain researchers, accident investigators, and operators with historical catastrophe experience attest the problem is NOT solved for long-term knowledge retention — the constraint's form (simulation sufficiency) is the contested claim, not the founding problem's existence. The divergence is between 'is simulation sufficient' (contested) and 'must we train operators somehow' (live).
narrative_ontology:disappearance_verdict(catastrophe_proxy_sufficiency__hybrid_degradation_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_proxy_sufficiency__hybrid_degradation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high (0.68 at interval end) and rising over time because the constraint's core function (certification) produces an apparent coordination benefit (standardized, scalable training) that masks a hidden extraction (generational knowledge loss). Early in the interval (t=0), extractiveness is lower (0.45) because operators trained during or after real catastrophes possess tacit knowledge that simulation supplements; as those cohorts retire and post-catastrophe-cohort operators dominate (t=30–60), the hidden extraction becomes larger (fewer people possess unreplaceable stress-response competence). Theater ratio rises sharply (0.38 → 0.62) because over time, the constraint's functioning increasingly performs assurance (certification, compliance, audit sign-off) rather than genuine safety (actual stress-response capacity). Suppression is high and grows because maintaining the constraint requires active suppression of: (1) accident investigation findings that attribute failures to knowledge gaps, (2) researcher publications about degradation in post-catastrophe cohorts, (3) operator self-disclosure of stress-response uncertainty, (4) regulatory pressure for higher fidelity standards. Accessibility collapse is moderate (0.48) because alternatives exist (real-world mentoring, field practice, extended apprenticeship, catastrophe-equivalent stress labs) but are expensive, slow, and conflict with the cost/speed advantages certification provides. Resistance is moderate-high (0.59) because operators with real-event experience and researchers actively push back, but their voice is constrained by professional hierarchy and regulatory capture.
 *
 * PERSPECTIVAL GAP:
 *   The certification industry and operating organizations experience the constraint as genuine coordination that solves an impossible problem (how to train operators at scale without catastrophes). The current-cohort operators experience it as adequate because their tacit knowledge comes from elsewhere. The post-catastrophe-cohort operators experience it as extraction because they are certified competent but lack the embodied stress capacity that certification claims to measure. Researchers experience it as a slow catastrophe waiting to happen. Regulatory authorities experience it as a safe-enough compromise under resource constraints. The engine computes this divergence from the stakeholder power/exit/beneficiary/victim data — the authored claim does not adjudicate which perspective is correct, only that they diverge.
 *
 * DIRECTIONALITY LOGIC:
 *   The certification industry is the clear beneficiary (d ≈ 0.15): it sets the standards that define what counts as competence, collects recurring revenue from recertification, and has no exit — the only way to eliminate the constraint is to eliminate certification itself, which is politically impossible. Operating organizations are intermediate (d ≈ 0.50): they benefit from cost reduction and regulatory acceptance but bear the hidden cost of knowledge degradation that only manifests over decades. Current-cohort operators benefit from simulation as a cheaper refresh mechanism (d ≈ 0.25); post-catastrophe-cohort operators are targets (d ≈ 0.75) because their certification masks inadequacy and locks them into a profession where admitting the inadequacy carries career risk. Researchers and regulators sit near analytical (d ≈ 0.50) — they are neither cleanly benefiting nor paying but have structural conflicts of interest (regulators are invested in the system's safety record, which means defending certification; researchers have careers in domains where admitting knowledge gaps undermines their own training). Future operators are pure targets (d ≈ 0.95) — they will inherit a degraded knowledge base and a training system they cannot opt out of.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to train operators without real catastrophes) is genuinely live. The constraint's form (simulation sufficiency) is what is contested. The hybrid_degradation reading claims simulation maintains procedural competence (the founding problem IS solved for that dimension) but NOT tacit knowledge or stress-response capacity. This is NOT a case of mandate death; the mandate is still operational. What the analysis shows is mandate PARTITION: the constraint solves one part of the founding problem (procedural certification) while leaving another part increasingly unsolvedover time (stress-response readiness). The theater_ratio rise (0.38 → 0.62) shows that over the interval, the constraint's functioning increasingly performs assurance (passes the regulatory audit, signs off on competence) rather than providing the assurance's referent (actual competence). This is the Goodhart drift diagnostic: as the metric (certification status) becomes the target, it diverges from the underlying capability it was meant to measure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tacit_knowledge_measurability,
    'Is the atrophy of tacit knowledge and stress-response capacity measurable and attributable to simulation-only training, or is it intrinsically private and confounded by other factors (operator personality, organizational culture, selection bias)?',
    'Longitudinal studies following post-catastrophe-cohort operators under controlled conditions, or comparison of stress-response performance between operators trained with real-event mentoring vs. simulation-only, controlling for personality and selection effects.',
    'If measurable and attributable, the constraint''s extraction becomes unambiguous (knowledge loss is a real cost borne by the organization and future operators). If the atrophy is intrinsically private or confounded, the claim that simulation is extractive becomes harder to defend empirically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tacit_knowledge_measurability, empirical, 'Whether tacit knowledge degradation is an observable feature of simulation-only training or a contested inference.').

omega_variable(
    catastrophe_frequency_coupling,
    'Is the constraint''s hidden extraction stable over longer intervals (multiple human lifespans), or does it discharge catastrophically when the next real catastrophe occurs in the domain?',
    'Post-accident investigation after a real catastrophe involving operators trained under simulation-only regimes; comparison of failure modes to pre-catastrophe-decline accidents.',
    'If the constraint persists stably, it is a slow-decay piton in the making (knowledge loss that never manifests as failure unless catastrophe occurs). If it discharges into catastrophic failure, it is a delayed-fuse snare. The classification hinges on whether the extraction accrues in steady-state or gets realized suddenly.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(catastrophe_frequency_coupling, empirical, 'Whether the constraint''s fragility is latent and stable or episodically catastrophic.').

omega_variable(
    simulation_fidelity_boundary,
    'Is there a technologically achievable fidelity threshold at which simulation ceases to be extractive — where stress/uncertainty simulation can match real catastrophe well enough to maintain tacit knowledge — or is real catastrophe fundamentally irreplaceable?',
    'Development and testing of high-fidelity simulation environments (immersive, dynamic, with genuine uncertainty and consequences) and measurement of stress-response competence across operators trained with varying fidelity levels.',
    'If such a threshold exists and is achievable, the constraint is a tangled_rope that could be remedied through technology investment. If real catastrophe is irreplaceable, the constraint becomes a snare masquerading as coordination (the appearance of solution, the reality of extraction).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(simulation_fidelity_boundary, conceptual, 'Whether the problem diagnosed by hybrid_degradation is solvable through simulation technology or requires real-world exposure.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression of evidence about knowledge degradation structural (regulatory gatekeeping, industry incentives, publication bias) or internalized (operators self-censor due to career risk, researchers avoid the topic due to professional identity fusion with the system)?',
    'Post-catastrophe disclosure when operators no longer bear career risk; comparison of research publication patterns in closed (industry-governed) vs. open (academic) forums; interviews with retired operators and researchers about self-censorship.',
    'If structural, the constraint''s persistence depends on continued institutional enforcement; if internalized, operators and researchers carry the suppression with them even if the institutional architecture changes, making the constraint more durable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression is maintained by external barriers or by the agents'' own identity fusion with the system.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement_basis(cata_tr_t0, observed).
narrative_ontology:measurement(cata_tr_t10, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 10, 0.45).
narrative_ontology:measurement_basis(cata_tr_t10, observed).
narrative_ontology:measurement(cata_tr_t20, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 20, 0.52).
narrative_ontology:measurement_basis(cata_tr_t20, observed).
narrative_ontology:measurement(cata_tr_t30, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 30, 0.58).
narrative_ontology:measurement_basis(cata_tr_t30, observed).
narrative_ontology:measurement(cata_tr_t40, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 40, 0.61).
narrative_ontology:measurement_basis(cata_tr_t40, projected).
narrative_ontology:measurement(cata_tr_t50, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 50, 0.62).
narrative_ontology:measurement_basis(cata_tr_t50, projected).
narrative_ontology:measurement(cata_tr_t60, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 60, 0.62).
narrative_ontology:measurement_basis(cata_tr_t60, projected).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(cata_be_t0, observed).
narrative_ontology:measurement(cata_be_t10, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement_basis(cata_be_t10, observed).
narrative_ontology:measurement(cata_be_t20, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement_basis(cata_be_t20, observed).
narrative_ontology:measurement(cata_be_t30, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 30, 0.63).
narrative_ontology:measurement_basis(cata_be_t30, observed).
narrative_ontology:measurement(cata_be_t40, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 40, 0.66).
narrative_ontology:measurement_basis(cata_be_t40, projected).
narrative_ontology:measurement(cata_be_t50, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 50, 0.68).
narrative_ontology:measurement_basis(cata_be_t50, projected).
narrative_ontology:measurement(cata_be_t60, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 60, 0.68).
narrative_ontology:measurement_basis(cata_be_t60, projected).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(cata_su_t0, observed).
narrative_ontology:measurement(cata_su_t10, catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 10, 0.61).
narrative_ontology:measurement_basis(cata_su_t10, observed).
narrative_ontology:measurement(cata_su_t20, catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 20, 0.66).
narrative_ontology:measurement_basis(cata_su_t20, observed).
narrative_ontology:measurement(cata_su_t30, catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 30, 0.7).
narrative_ontology:measurement_basis(cata_su_t30, observed).
narrative_ontology:measurement(cata_su_t40, catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(cata_su_t40, projected).
narrative_ontology:measurement(cata_su_t50, catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 50, 0.71).
narrative_ontology:measurement_basis(cata_su_t50, projected).
narrative_ontology:measurement(cata_su_t60, catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 60, 0.71).
narrative_ontology:measurement_basis(cata_su_t60, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_proxy_sufficiency__hybrid_degradation_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 0.12).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__hybrid_degradation_reading, catastrophe_necessity_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__hybrid_degradation_reading, simulation_as_proxy_catastrophe_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__hybrid_degradation_reading, simulation_fidelity_threshold).

% DUAL FORMULATION NOTE:
% catastrophe_proxy_sufficiency is a contested kernel with four instantiated readings. Each reading instantiates a different constraint with different ε, beneficiary/victim structures, and types. The hybrid_degradation reading (this story) partitions competence into procedural (maintained by simulation) and tacit (degraded by simulation-only training), creating a tangled-rope structure where coordination (procedural certification) masks extraction (tacit knowledge loss). Sibling readings assign different partitions or deny the partition altogether; they are NOT alternative measurements of the same ε, but structurally different constraints instantiated by different framings of what competence is.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_proxy_sufficiency__hybrid_degradation_reading, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
