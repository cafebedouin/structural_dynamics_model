% ============================================================================
% CONSTRAINT STORY: catastrophe_proxy_sufficiency__simulation_fidelity_threshold
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_proxy_sufficiency__simulation_fidelity_threshold, []).

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
 *   constraint_id: catastrophe_proxy_sufficiency__simulation_fidelity_threshold
 *   human_readable: Simulation Fidelity Threshold for Competence Retention
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   Safety-critical operations (nuclear plant control, aviation, emergency
 *   response, critical infrastructure) depend on personnel maintaining
 *   competence for rare, high-stakes events. The constraint establishes that
 *   competence retention is SUFFICIENT when simulation training crosses a
 *   fidelity threshold where the stress and uncertainty conditions match real
 *   catastrophes. This reading instantiates the technological-sufficiency
 *   framing: once a threshold is crossed, simulation practice maintains
 *   competence indefinitely; below the threshold, it does not. The
 *   constraint's beneficiaries are simulation technology vendors and training
 *   infrastructure operators, who profit from standardized threshold-setting.
 *   The extractiveness measurement tracks the degree to which the threshold
 *   requirement extracts capital investment and operational dependency on
 *   vendor systems; suppression measurement tracks how skepticism about
 *   threshold-sufficiency is institutionally suppressed (skeptics excluded
 *   from standards bodies); theater measures the degree to which fidelity
 *   compliance becomes performative rather than functionally tied to actual
 *   competence outcomes.
 *
 * KEY AGENTS:
 *   - Simulation technology vendors: institutional power, arbitrage exit — set technical threshold specifications that justify procurement cycles and premium pricing.
 *   - Operational personnel: moderate power, constrained exit — must train to mandated thresholds; benefit from realistic practice but do not control procurement or standard-setting.
 *   - Regulatory authorities: institutional power, analytical exit — enforce fidelity thresholds but defer technical specification to vendors and industry bodies.
 *   - Resource-constrained organizations: moderate power, trapped exit — must meet regulatory thresholds or lose licensing; cannot negotiate vendor pricing or develop alternatives.
 *   - Empirical skeptics & post-catastrophe learning advocates: excluded from standards-setting despite raising evidence that thresholds may be arbitrary or insufficient.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 0.62).
domain_priors:suppression_score(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 0.58).
domain_priors:theater_ratio(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, extractiveness, 0.62).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, resistance, 0.67).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, rope).
narrative_ontology:human_readable(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, "Simulation Fidelity Threshold for Competence Retention").
narrative_ontology:topic_domain(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(catastrophe_proxy_sufficiency__simulation_fidelity_threshold).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 'c33efdee-8a26-4910-b15d-288fd37922aa').
narrative_ontology:cs_kernel_codification('c33efdee-8a26-4910-b15d-288fd37922aa', distributed).
narrative_ontology:cs_authority_grounding('c33efdee-8a26-4910-b15d-288fd37922aa', extraction).
narrative_ontology:cs_reading_relation('c33efdee-8a26-4910-b15d-288fd37922aa', catastrophe_proxy_sufficiency__catastrophe_necessity_reading, coexists_with).
narrative_ontology:cs_reading_relation('c33efdee-8a26-4910-b15d-288fd37922aa', catastrophe_proxy_sufficiency__hybrid_degradation_reading, influences).
narrative_ontology:cs_reading_relation('c33efdee-8a26-4910-b15d-288fd37922aa', catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, coexists_with).
narrative_ontology:cs_axiom('c33efdee-8a26-4910-b15d-288fd37922aa', foundational, fidelity_threshold_sufficiency).
narrative_ontology:cs_axiom_status(fidelity_threshold_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('c33efdee-8a26-4910-b15d-288fd37922aa', fidelity_threshold_sufficiency, empirically_contingent).
narrative_ontology:cs_axiom('c33efdee-8a26-4910-b15d-288fd37922aa', secondary, technology_dependent_maintenance).
narrative_ontology:cs_axiom_status(technology_dependent_maintenance, holdable).
narrative_ontology:cs_axiom_grounding('c33efdee-8a26-4910-b15d-288fd37922aa', technology_dependent_maintenance, instrumental).
narrative_ontology:cs_reference_frame('c33efdee-8a26-4910-b15d-288fd37922aa', threshold_crossing_competence_maintenance).
narrative_ontology:cs_drift_state('c33efdee-8a26-4910-b15d-288fd37922aa', contemporary_post_incident_skepticism, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c33efdee-8a26-4910-b15d-288fd37922aa', '').
narrative_ontology:cs_kernel_id(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, catastrophe_proxy_sufficiency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, simulation_technology_vendors).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, training_infrastructure_operators).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, operational_personnel).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, resource_constrained_organizations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, operational_personnel).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, catastrophe_response_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop, license, and maintain high-fidelity simulation platforms. Benefit from regulatory and professional standards that mandate simulation training at or above a specified fidelity threshold. Set technical specifications for what constitutes 'sufficient' fidelity, influencing procurement decisions and training budgets across the industry. Control the upgrade cycle: as thresholds rise, new investment is required.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, simulation_technology_vendors, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, simulation_technology_vendors, agenda_setter).

% Must maintain competence through mandatory simulation training. Benefit from realistic practice that exercises their stress responses and decision-making under uncertainty. Pay in time, attention, and psychological effort during training. Their competence depends on simulation fidelity but they do not control which simulators are procured or what threshold is enforced.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, operational_personnel, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, operational_personnel, beneficiary).

% Set and enforce minimum fidelity standards for simulation-based training. Justify requirements via reference to safety outcomes and competence maintenance. Defer technical specification decisions to vendors and industry bodies, creating a dependency loop where threshold-setting becomes vendor-influenced.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, regulatory_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Smaller operators, developing-nation facilities, and under-resourced public agencies must procure simulators meeting regulatory thresholds. Cannot negotiate vendor pricing, cannot develop alternative training infrastructure, cannot opt out of simulation requirements without regulatory violation. Each regulatory threshold increase forces new capital expenditure.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, resource_constrained_organizations, payer,
    moderate, biographical, trapped, regional).

% Benefit from operators whose competence is maintained through high-fidelity simulation: faster response times, better decision-making under pressure, reduced cascade failures. Collectively prefer competent operators but have no direct procurement power or say in training standards.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, catastrophe_response_communities, beneficiary,
    organized, generational, constrained, national).

% Researchers and field experts who argue simulation fidelity thresholds are arbitrary, that sufficiency is context-dependent and not binary, and that vendor-set standards conflate product capability with actual competence maintenance. Excluded from standards-setting bodies; their objections are treated as obstacles rather than epistemic feedback.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, empirical_skeptics, excluded,
    powerful, biographical, constrained, global).

% Argue that real catastrophes provide irreducible information that simulation cannot capture, and that relying on simulation thresholds as a substitute for real-event learning creates a false sense of security. Cannot voice this in regulatory settings without being read as opposing safety investment.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, post_catastrophe_learning_advocates, excluded,
    moderate, generational, trapped, global).

% Operate training centers and run simulation exercises. Benefit from standardized fidelity thresholds that justify licensing their facilities and charging premium rates. Their business model depends on the threshold remaining binding.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, training_infrastructure_operators, beneficiary,
    organized, generational, arbitrage, global).

% Evaluate whether simulation training at mandated fidelity levels actually maintains operational competence. Produce studies and audits that could undermine or validate the threshold-sufficiency claim. Their work is selectively cited depending on findings.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, independent_competence_assessors, observer,
    powerful, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, simulation_technology_vendors).
narrative_ontology:fixing_cost_class(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared standard for competence maintenance through simulation-based training, enabling regulatory oversight, professional licensing, and mutual assurance across organizations that personnel are trained to a comparable level under controlled stress scenarios. Solves the problem of verifying competence without requiring continuous real-catastrophe events.
% TRANSFER_FUNCTION: Moves capital investment (procurement of simulators meeting threshold specifications), ongoing licensing fees, and personnel training time from operational organizations to simulation vendors and training infrastructure operators. Transfers decision authority over what constitutes 'sufficient' competence from practitioners and organizations to technology vendors and regulatory bodies.
% ABSENT_VOICES: Researchers arguing simulation thresholds are arbitrary or context-dependent; post-catastrophe learning advocates who see simulation-dependency as a competence trap; operators in resource-constrained settings who experience the threshold as an unaffordable mandate; empirical skeptics who want fidelity sufficiency tested against actual competence outcomes rather than assumed.
% DISAPPEARANCE_RATIONALE: If the fidelity threshold constraint vanished, training programs would immediately fragment: some organizations would reduce simulator investment and rely on procedural training; others would continue high-fidelity practice; regulatory oversight of competence would become context-specific rather than standardized. The simulation vendor business model would contract. Whether competence actually degraded would depend on whether the threshold was genuinely necessary or was theater.
% FOUNDING_PROBLEM: Following catastrophic events or near-misses, investigations discovered that personnel who had not encountered sufficiently realistic stress or uncertainty in training made poor decisions or froze. The question became: how can organizations maintain personnel readiness for rare, high-stakes events without waiting for actual catastrophes? Simulation fidelity thresholds were proposed as a technological solution: simulators realistic enough to exercise the same stress-response systems as real events.
% FOUNDING_PROBLEM_CORROBORATION: Vendors and regulatory authorities attest the founding problem is live and thresholds are necessary. Training personnel report subjective benefits of high-fidelity simulation. Operational incidents following low-fidelity-only training are cited as evidence. However, independent researchers and field practitioners attest that the founding problem conflates 'people are unprepared' (true) with 'simulation above an absolute threshold is sufficient' (contested). Post-catastrophe investigations show mixed results: some failures occurred despite high-fidelity training; some organizations with low-fidelity training performed well. The claim that a specific fidelity threshold is necessary and sufficient is not corroborated outside the benefiting stakeholder set.
narrative_ontology:disappearance_verdict(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_proxy_sufficiency__simulation_fidelity_threshold_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_proxy_sufficiency__simulation_fidelity_threshold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This reading's extractiveness (0.62 at interval end) reflects the constraint's dual nature: genuine coordination value (organizations need a shared standard for competence verification) coupled with vendor-captured threshold-setting that decouples cost from actual sufficiency. The claim is Rope (technology-enabled coordination) because the foundational story is that simulation investment solves a real problem — maintaining competence without catastrophes. The metrics reflect that story held while acknowledging extraction: the measurement series show extractiveness rising steeply from 0.48 to 0.62 over the first 20 periods as thresholds ratchet upward and vendor lock-in strengthens, then plateauing once the technology base stabilizes (periods 25–40: projected flat at 0.62). Theater ratio rises from 0.22 to 0.41, indicating that an increasing share of simulation activity shifts from practicing decision-making under uncertainty toward demonstrating compliance with threshold specifications. Suppression requirement rises from 0.35 to 0.58 as regulatory bodies must work harder to maintain the threshold-sufficiency narrative against accumulating empirical challenges. The authorized-grid is absent because this constraint's leveled coercion dynamics are not the focus: all levels (structural policy, organizational procurement, class-level training burden, individual stress response) experience the constraint similarly through the threshold mechanism. An alternative story (catastrophe_necessity_reading) would emphasize structural pressure mounting on simulator-dependent organizations as real catastrophes occur, triggering post-event learning that simulation alone cannot capture — that would merit a grid showing class-and-individual-level pressure diverging. This story stays in the technological-sufficiency frame and does not author that alternative's pressure profile.
 *
 * PERSPECTIVAL GAP:
 *   From the vendor and regulatory perspective, the fidelity threshold is a genuine coordination achievement: it established a legible standard replacing ad hoc training practices. From the operational perspective, the constraint is perceived as real training value (they do benefit from the simulation) coupled with escalating procurement burden. From the empirical-skeptics' perspective, the constraint is a false sufficiency claim — a Snare masquerading as Rope. The engine computes these different seats' types from the structural data; the claimed_type (Rope) reflects the beneficiaries' framing while the metrics describe the extractive dynamics that skeptics identify. That divergence — Rope claim, extractive metrics — is exactly the measurement the corpus captures.
 *
 * DIRECTIONALITY LOGIC:
 *   Simulation technology vendors are unambiguous beneficiaries: d near 0.0. They set the threshold, control the upgrade cycle, and capture revenues. Regulatory authorities derive benefit from a legible standard (they can mandate and verify compliance) but also bear costs (they become liable if thresholds prove insufficient). Their d is near symmetric (~0.5). Operational personnel benefit from realistic training (low d on the training-value axis) but pay in time and constrained exit; however, the constraint's primary extraction is on organizations through capital requirements, not on individuals through restricted choice. Resource-constrained organizations are the primary targets: d near 1.0 — they must spend capital they cannot afford, cannot negotiate terms, and cannot exit. The directionality of resource-constrained_organizations as 1.0 (trapped payer) is not overridden despite their moderate power atom: the technology-vendor constraint creates a specific powerlessness within this domain that power atom alone does not capture. An override would be warranted if the data showed these organizations had actual arbitrage leverage; current evidence does not support that.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (competence maintenance for rare events) is live and corroborated outside the vendor set: operational communities attest that real-world performance depends on realistic training. However, the specific claim that THRESHOLD-CROSSING is SUFFICIENT is contested and NOT corroborated outside beneficiaries. Mandatrophy appears here as the divergence between the founding problem (real) and the sufficiency mechanism (contested). The constraint solves the founding problem partially: it does provide realistic training. The unresolved question is whether a binary threshold is the right mechanism or whether sufficiency is context-dependent and degradational across time horizons. This reading (simulation_fidelity_threshold) resolves mandatrophy by asserting that threshold-crossing IS sufficient and that technology-dependency is therefore warranted. Sibling readings resolve it differently: catastrophe_necessity_reading asserts thresholds are insufficient (simulation cannot replace real events), hybrid_degradation_reading asserts sufficiency degrades over generational timescales. The engine does not adjudicate which resolution is correct; the corpus collects all readings to measure which institutional narrative gains foothold.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_sufficiency_empirical_adequacy,
    'Does crossing a specified fidelity threshold actually produce the claimed competence maintenance, or is threshold-crossing correlated with competence without being causally sufficient?',
    'Longitudinal competence assessment comparing operators trained above threshold vs. below threshold, stratified by time since training and by presence/absence of real catastrophic events. Regression discontinuity design at the threshold boundary. Independent post-incident audits of decisions made by personnel trained at different fidelity levels.',
    'If threshold-crossing is insufficient, the constraint is Snare masquerading as Rope (false sufficiency claim). If causally sufficient, the Rope classification holds. If context-dependent (sufficient in some domains/timescales, insufficient in others), the constraint decomposes into multiple domain-specific constraints with different readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_sufficiency_empirical_adequacy, empirical, 'Whether threshold-crossing suffices for competence or serves as a proxy for unmeasured factors.').

omega_variable(
    threshold_arbitrariness_and_vendor_capture,
    'Is the specific fidelity threshold technically justified from first principles, or is it set at the level vendor technology happens to achieve, creating a post-hoc rationalization?',
    'Technical analysis comparing threshold specification to actual stress-response requirements of real events. Historical comparison of threshold changes against vendor capability improvements (if thresholds rise when vendors release new products, post-hoc capture is indicated). Alternative threshold design without vendor input.',
    'If arbitrarily set, the constraint is Snare: threshold-setting extracts vendor profit while claiming technical necessity. If technically justified, Rope classification is supported. If partially justified but influenced by vendor interests, the constraint is Tangled Rope (genuine coordination + asymmetric extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(threshold_arbitrariness_and_vendor_capture, empirical, 'Whether fidelity thresholds are technically justified or vendor-influenced.').

omega_variable(
    generational_degradation_of_tacit_knowledge,
    'Does reliance on simulation-only training degrade tacit knowledge and stress-response capacity over generational timescales (20–50 years without real catastrophic events)?',
    'Historical analysis of organizational competence trajectories in domains with and without real catastrophic refresh events. Intergenerational knowledge transfer studies comparing organizations with simulator-only training vs. those with mixed simulation + real-event experience. Anthropological case studies of competence loss in closed systems.',
    'If degradation occurs on generational timescales, the constraint is Piton (scaffolded training that once served now maintains itself through theater and institutional inertia). If no degradation, Rope holds. If degradation is selective (procedural competence holds, tacit knowledge degrades), the constraint is Tangled Rope + hybrid_degradation_reading becomes empirically grounded.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(generational_degradation_of_tacit_knowledge, empirical, 'Whether competence loss is masked by simulation-only training regimes over decades.').

omega_variable(
    kernel_reading_observational_equivalence,
    'What observable differences would distinguish this reading (simulation_fidelity_threshold: sufficiency is technology-dependent and conditional) from the catastrophe_necessity_reading (only real events suffice)?',
    'Identify the decision points where institutional actors would behave differently: threshold-reading predicts continued simulator investment even after catastrophic failures; necessity-reading predicts post-catastrophe reallocation toward real-event learning. Follow regulatory responses to operational failures and competence gaps: do authorities increase simulator investment or shift toward generational mixing with real-event survivors?',
    'If institutional behavior matches this reading''s predictions, the reading maintains coherence. If it matches necessity-reading''s predictions (post-catastrophe turn toward real learning), this reading loses institutional ground. If behaviors oscillate, hybrid_degradation_reading becomes dominant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_observational_equivalence, conceptual, 'Whether the fidelity-threshold reading is institutionally stable or displaced by sibling readings after live events.').

omega_variable(
    binary_vs_continuous_sufficiency,
    'Is competence maintenance actually a binary property (threshold-crossed or not) or is it a continuous function of fidelity, experience accumulation, and post-event learning?',
    'Model competence as a function of simulator fidelity, training frequency, time since training, and real-event exposure. Fit regression curves to empirical data on operational performance. Test whether a single binary threshold explains the variance or whether multiple continuous parameters are required.',
    'If binary threshold explains variance, this reading''s framing is validated. If continuous, the constraint is reading-dependent misclassification: it should be reframed as a gradient (Rope or Tangled Rope at higher fidelity, degrading toward Piton at low fidelity). The kernel itself may be conceptually underconstrained if it assumes binary sufficiency.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(binary_vs_continuous_sufficiency, conceptual, 'Whether sufficiency is categorical (threshold-based) or continuous (degradational).').

omega_variable(
    kernel_committer_structure_underspecification,
    'Which of the four readings of the catastrophe_proxy_sufficiency kernel represents the actual institutional commitment, and which represent post-hoc rationalization?',
    'Trace decision-making in standards bodies: which reading is invoked when budgets are set? When procurement is approved? When post-incident investigations occur? Distinguish institutional commitment (the reading actually used to allocate resources) from rhetorical cover (readings cited in public documents but not acted on). Analyze archival records and ethnographic observation of standards bodies.',
    'If this reading (simulation_fidelity_threshold) is the actual committed reading, the constraint is as authored. If a different reading is institutionally primary and this reading is peripheral rhetoric, the corpus mislabels the dominant constraint. If multiple readings are simultaneously invoked (switching contexts), the kernel is fragmented and may decompose differently than the four-reading structure suggests.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_committer_structure_underspecification, conceptual, 'Kernel-level committer ambiguity: which reading is the true institutional commitment?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(catastrophe_proxy_sufficiency_tr_t0, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 0, 0.22).
narrative_ontology:measurement(catastrophe_proxy_sufficiency_tr_t5, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 5, 0.27).
narrative_ontology:measurement(catastrophe_proxy_sufficiency_tr_t10, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 10, 0.32).
narrative_ontology:measurement(catastrophe_proxy_sufficiency_tr_t15, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 15, 0.37).
narrative_ontology:measurement(catastrophe_proxy_sufficiency_tr_t20, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 20, 0.4).
narrative_ontology:measurement(catastrophe_proxy_sufficiency_tr_t25, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 25, 0.41).
narrative_ontology:measurement(catastrophe_proxy_sufficiency_tr_t30, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 30, 0.41).
narrative_ontology:measurement(catastrophe_proxy_sufficiency_tr_t40, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 40, 0.41).

% Extraction over time
narrative_ontology:measurement(catastrophe_proxy_sufficiency_be_t0, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(catastrophe_proxy_sufficiency_be_t5, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(catastrophe_proxy_sufficiency_be_t10, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 10, 0.56).
narrative_ontology:measurement(catastrophe_proxy_sufficiency_be_t15, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 15, 0.59).
narrative_ontology:measurement(catastrophe_proxy_sufficiency_be_t20, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 20, 0.61).
narrative_ontology:measurement(catastrophe_proxy_sufficiency_be_t25, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 25, 0.62).
narrative_ontology:measurement(catastrophe_proxy_sufficiency_be_t30, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(catastrophe_proxy_sufficiency_be_t40, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(catastrophe_proxy_sufficiency_su_t0, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(catastrophe_proxy_sufficiency_su_t5, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, suppression_requirement, 5, 0.42).
narrative_ontology:measurement(catastrophe_proxy_sufficiency_su_t10, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(catastrophe_proxy_sufficiency_su_t15, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, suppression_requirement, 15, 0.52).
narrative_ontology:measurement(catastrophe_proxy_sufficiency_su_t20, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(catastrophe_proxy_sufficiency_su_t25, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, suppression_requirement, 25, 0.57).
narrative_ontology:measurement(catastrophe_proxy_sufficiency_su_t30, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, suppression_requirement, 30, 0.58).
narrative_ontology:measurement(catastrophe_proxy_sufficiency_su_t40, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, suppression_requirement, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, resource_allocation).
narrative_ontology:boltzmann_floor_override(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 0.18).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, catastrophe_necessity_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, hybrid_degradation_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, simulation_as_proxy_catastrophe_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the catastrophe_proxy_sufficiency kernel. All four readings share the same referent (simulation vs. real events as training mechanisms) but instantiate different constraints with different ε values, beneficiary structures, and institutional implications. This reading (simulation_fidelity_threshold) takes the position that threshold-crossing is sufficient and technology-dependent; sibling readings take positions ranging from strict catastrophe-necessity to categorical sufficiency. The network edges indicate family membership and upstream/downstream inference relationships: this reading influences catastrophe_necessity_reading (if thresholds fail empirically, necessity reading gains ground) and is influenced by hybrid_degradation_reading (if generational degradation is observed, hybrid reading becomes empirically grounded). All four readings affect each other through institutional competition for authority in standards-setting bodies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
