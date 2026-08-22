% ============================================================================
% CONSTRAINT STORY: catastrophe_avoidance_retention__simulation_as_proxy_catastrophe
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, []).

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
 *   constraint_id: catastrophe_avoidance_retention__simulation_as_proxy_catastrophe
 *   human_readable: High-Fidelity Simulation as Functional Catastrophe Proxy for Competence Maintenance
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   Organizations operating safety-critical systems (aviation, nuclear power,
 *   emergency response) must maintain competence in handling rare,
 *   high-stakes scenarios. This constraint asserts that high-fidelity
 *   simulation—carefully designed drills that replicate decision-making
 *   demands, time pressure, and information flows of actual catastrophes—is
 *   functionally equivalent to real catastrophic events for the purpose of
 *   maintaining competence. This is ONE READING of a contested kernel about
 *   catastrophe avoidance and organizational learning. The reading
 *   instantiates a specific claim about simulation's epistemic and functional
 *   status: that a well-designed drill performs the same
 *   competence-maintenance work as a real failure. This reading coexists with
 *   two sibling readings: (1) catastrophe_as_necessary_selector, which claims
 *   only actual catastrophes provide the organizational trauma and mortality
 *   salience necessary for competence; (2) hybrid_near_miss_learning, which
 *   claims competence is maintained via distributed analysis of near-misses
 *   and foreign incidents, not drills alone. The constraint described here is
 *   the simulation-proxy reading only.
 *
 * KEY AGENTS:
 *   - simulation_infrastructure_operators: institutional actors (Airbus, Boeing, nuclear simulator vendors) who define fidelity standards and control access; benefit from licensing and regulatory certification partnerships
 *   - regulatory_certification_bodies: national/international authorities (FAA, ICAO, IAEA) that mandate simulation-based training; benefit by outsourcing competence assessment
 *   - frontline_operators: pilots, nuclear control room staff, emergency responders who must pass simulation-based competence checks; bear time and psychological costs; benefit from structured learning
 *   - resource_constrained_organizations: regional operators, developing-nation infrastructure, small hospitals that cannot afford high-fidelity simulators and are trapped by licensing requirements
 *   - near_miss_researchers: academic/industry researchers who study learning from non-catastrophic failures; excluded from standard-setting forums
 *   - catastrophe_survivors_and_bereaved: lived experience of failures not prevented by simulation training; excluded from regulatory debates
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 0.61).
domain_priors:suppression_score(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 0.48).
domain_priors:theater_ratio(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, extractiveness, 0.61).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, resistance, 0.51).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, tangled_rope).
narrative_ontology:human_readable(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, "High-Fidelity Simulation as Functional Catastrophe Proxy for Competence Maintenance").
narrative_ontology:topic_domain(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, '4cb7db30-848b-45ac-8ee6-2b6a9075570d').
narrative_ontology:cs_kernel_codification('4cb7db30-848b-45ac-8ee6-2b6a9075570d', formalized).
narrative_ontology:cs_authority_grounding('4cb7db30-848b-45ac-8ee6-2b6a9075570d', expertise).
narrative_ontology:cs_interpretation_layer_present('4cb7db30-848b-45ac-8ee6-2b6a9075570d').
narrative_ontology:cs_reading_relation('4cb7db30-848b-45ac-8ee6-2b6a9075570d', catastrophe_avoidance_retention__catastrophe_as_necessary_selector, coexists_with).
narrative_ontology:cs_reading_relation('4cb7db30-848b-45ac-8ee6-2b6a9075570d', catastrophe_avoidance_retention__hybrid_near_miss_learning, influences).
narrative_ontology:cs_axiom('4cb7db30-848b-45ac-8ee6-2b6a9075570d', foundational, simulation_fidelity_functionally_equivalent_to_catastrophe).
narrative_ontology:cs_axiom_status(simulation_fidelity_functionally_equivalent_to_catastrophe, holdable).
narrative_ontology:cs_axiom_grounding('4cb7db30-848b-45ac-8ee6-2b6a9075570d', simulation_fidelity_functionally_equivalent_to_catastrophe, empirically_contingent).
narrative_ontology:cs_axiom('4cb7db30-848b-45ac-8ee6-2b6a9075570d', foundational, competence_decay_manageable_via_scheduled_drills_alone).
narrative_ontology:cs_axiom_status(competence_decay_manageable_via_scheduled_drills_alone, holdable).
narrative_ontology:cs_axiom_grounding('4cb7db30-848b-45ac-8ee6-2b6a9075570d', competence_decay_manageable_via_scheduled_drills_alone, instrumental).
narrative_ontology:cs_reference_frame('4cb7db30-848b-45ac-8ee6-2b6a9075570d', simulation_as_sufficient_learning_mechanism).
narrative_ontology:cs_drift_state('4cb7db30-848b-45ac-8ee6-2b6a9075570d', contemporary_research_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('4cb7db30-848b-45ac-8ee6-2b6a9075570d', '').
narrative_ontology:cs_kernel_id(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, catastrophe_avoidance_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, simulation_infrastructure_operators).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, regulatory_certification_bodies).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, frontline_operators).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, resource_constrained_organizations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, frontline_operators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design, maintain, and operate high-fidelity simulation systems. They define what constitutes 'functionally equivalent' practice, control access to simulation time/resources, and update scenario libraries. They justify simulation equivalence through technical specifications and training outcome data. They collect revenue from licensing fees, maintenance contracts, and regulatory certification partnerships.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, simulation_infrastructure_operators, agenda_setter,
    institutional, generational, arbitrage, global).

% Codify and enforce competence standards. They mandate that simulation meets specified fidelity thresholds as evidence of operator readiness. They benefit by outsourcing competence assessment to technical specialists (simulation providers) rather than conducting catastrophe-based validation. They gain administrative efficiency and avoid liability for unknown failure modes.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, regulatory_certification_bodies, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, regulatory_certification_bodies, beneficiary).

% Perform critical operations (aviation crews, nuclear control room staff, emergency response teams). They are required to maintain competence through scheduled drills and simulation. They bear the cost: time away from primary work, psychological stress from high-realism scenarios, and certification fees. They benefit from structured learning and from the reduced catastrophe risk the constraint theoretically maintains.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, frontline_operators, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, frontline_operators, beneficiary).

% Operate safety-critical systems with limited capital (rural hospitals, small regional air operators, developing-nation infrastructure). They must comply with simulation-based competence standards to maintain licensing, but cannot afford high-fidelity simulators or repeated training. They absorb costs by deferring maintenance, reducing training frequency, or operating below regulatory optima—exposing themselves to competence decay.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, resource_constrained_organizations, payer,
    powerless, biographical, trapped, regional).

% Study learning from non-catastrophic failures and foreign incidents. They would argue that competence decay is managed equally well (or better) by systematic near-miss analysis and cross-organizational learning networks, with lower cost and less psychological trauma. They are excluded from regulatory standard-setting forums dominated by simulation-infrastructure representatives.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, near_miss_researchers, excluded,
    moderate, biographical, constrained, global).

% Lived through a failure the simulation-based training did not prevent. They would testify to the limits of fidelity (simulation cannot replicate cascading chaos, mortality salience, or organizational trauma that real catastrophes create). They are structurally excluded from competence-standard debates, which are conducted in technical and regulatory language that privileges simulation-provider testimony.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, catastrophe_survivors_and_bereaved, excluded,
    powerless, biographical, trapped, local).

% Study how organizations maintain competence under uncertainty. They analyze trade-offs between simulation fidelity, cost, learning transfer, and psychological realism. They observe that the constraint embeds a theoretical claim (simulation is functionally equivalent to catastrophe for learning) that remains contested in the empirical literature.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, organizational_learning_theorists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, simulation_infrastructure_operators).
narrative_ontology:fixing_cost_class(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a scalable, repeatable, non-lethal method for maintaining competence in high-consequence domains where learning-by-actual-catastrophe is intolerable. Simulation allows competence decay to be managed predictably via scheduled drills rather than waiting for rare, uncontrollable catastrophic events.
% TRANSFER_FUNCTION: Moves time, cognitive load, and capital from frontline operators and resource-constrained organizations to simulation infrastructure operators and regulatory bodies. Operators must block calendar time for training; organizations must purchase or lease simulator access; regulators receive validated competence certification without conducting independent catastrophe-based assessment.
% ABSENT_VOICES: Near-miss researchers and catastrophe survivors are excluded from competence-standard setting. They would argue that competence maintenance is achievable through distributed near-miss learning networks and that simulation fidelity has natural limits in replicating organizational trauma responses. Developing-nation operators with economic constraints are absent from forums where regulatory standards are negotiated.
% DISAPPEARANCE_RATIONALE: If simulation-as-proxy-catastrophe disappeared overnight and organizations reverted to minimal scheduled training plus learning-by-actual-failure, competence decay would become observable in near-miss incident rates, latent failures would accumulate, and catastrophe frequency would shift upward over years. Regulatory frameworks would need to be rewritten; simulator manufacturers would cease operations; training architectures would reorganize around near-miss analysis and foreign incident review.
% FOUNDING_PROBLEM: High-consequence domains (aviation, nuclear, emergency response) cannot ethically train operators via actual catastrophes. Early competence decay in long-safe-period operations undermines readiness for rare, high-stakes scenarios. Catastrophe-based learning is intractable; a repeatable, controlled alternative is necessary.
% FOUNDING_PROBLEM_CORROBORATION: Aviation safety authorities, nuclear regulators, and emergency response agencies all attest the founding problem is still live—competence decay in quiet periods is documented in incident investigations and near-miss data. The constraint's core claim (that simulation is functionally equivalent) is disputed: research literature on learning transfer and organizational trauma suggests simulation's effectiveness is modality-dependent and incomplete in replicating stress-induced decision-making. Near-miss researchers have published contrary evidence that competence is maintainable through distributed analysis of non-catastrophic failures; they are not affiliated with simulator vendors and their testimony is independent. However, no independent party outside the simulator-industry ecosystem has conducted systematic comparative studies confirming functional equivalence.
narrative_ontology:disappearance_verdict(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 0.61, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.42 to 0.61 over the interval as simulator maintenance costs accumulate, licensing models tighten, and resource-constrained organizations absorb cumulative training burdens. Theater rises from 0.22 to 0.42 because operational drills increasingly emphasize certification compliance (passing the scenario) over learning transfer (developing adaptive competence). Suppression remains moderate (0.35–0.48) because the constraint is enforced through licensing requirements and regulatory inspection, not overt coercion—operators have formal opt-out paths (license surrender, regulatory variance requests) that are costly but exist. The metric trajectory shows extraction rising faster than theater, suggesting the constraint is accumulating rent-seeking layers (enhanced fidelity standards, more frequent recertification) faster than it is degrading into pure performance. Accessibility collapse is moderate (0.58): alternatives exist (near-miss analysis, foreign incident review, competence-decay monitoring via incident rates) but are not funded or recognized by regulators. Resistance is moderate (0.51): operators comply because licensing is non-negotiable, but they resist through workarounds (simulator scenarios run twice, minimal participation, gaming scenario outcomes) and through research communities that publish data questioning fidelity claims.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (simulator operators and regulators) and the payer seat (frontline operators and resource-constrained organizations) should compute as different types. From the agenda-setter seat: the constraint solves a real coordination problem (how to maintain competence without catastrophes) and the simulation infrastructure is genuinely necessary, justifying the transfer. From the payer seats: the constraint is a tangled rope—it coordinates competence maintenance but also extracts capital/time that could be allocated to other learning methods; the enforcement apparatus (licensing requirements, regulatory inspection) is the mechanism that holds the transfer in place. The engine should compute this divergence from the structural data: the beneficiary seats have high directionality toward the constraint (d near 0.0), while the payer seats have higher directionality (d toward 0.6–0.8 depending on exit options). The schema-enforced claim/metric independence ensures that the agenda-setter's framing (rope) and the payer's experience (tangled rope) both appear in the story without one overriding the other.
 *
 * DIRECTIONALITY LOGIC:
 *   Simulation_infrastructure_operators: d ≈ 0.1 (full beneficiary). They set the agenda, control fidelity standards, and collect licensing revenue. They have arbitrage-level exit (can serve other domains). Regulatory_certification_bodies: d ≈ 0.15 (beneficiary with secondary institutional role). They gain administrative efficiency by outsourcing competence assessment to technical specialists; they have analytical exit (can adopt alternative assessment methods at any time, though institutional inertia makes this costly). Frontline_operators: d ≈ 0.65 (moderate target, modulated by constrained exit). They pay time and psychological cost; they benefit from structured learning (modest positive). They cannot exit certification requirements without surrendering licensure. Their d sits in the middle because the constraint is genuinely useful (learning benefit) and genuinely extractive (time/cost). Resource_constrained_organizations: d ≈ 0.82 (high target). They pay the same licensing fees as well-capitalized operators but cannot absorb the cost; they are trapped by licensing requirements; they have minimal exit (cannot operate without a license, cannot afford simulators, cannot negotiate variance). Their d is high because the constraint concentrates extraction on the powerless. Near_miss_researchers: d ≈ 0.45 (moderate, affected by exclusion). They are excluded from standard-setting forums, which suppresses their alternative knowledge; they can publish research but cannot change the regulatory framing without institutional allies.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live: competence decay in long-safe periods is documented in incident data, and the ethical constraint against learning-by-catastrophe is non-negotiable. However, the constraint embeds a theoretical claim (simulation is functionally equivalent to catastrophe) that is increasingly contested. Research on learning transfer, organizational trauma responses, and stress-induced decision-making suggests simulation's effectiveness is modality-dependent and incomplete. The theater_ratio rise from 0.22 to 0.42 signals that operational drills are increasingly dominated by certification compliance (passing the scenario) rather than learning transfer (developing adaptive competence). The classification as tangled_rope rather than rope correctly captures that the constraint carries both real coordination (competence maintenance) and extractive overlay (accumulated licensing costs, architectural lock-in to simulator vendors). If the sibling reading catastrophe_as_necessary_selector gained empirical support—i.e., if research showed that competence decay despite high-fidelity simulation was driven by the absence of organizational trauma and mortality salience—the constraint's mandate would be undermined, even though its enforcement apparatus remains robust.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fidelity_sufficiency_limits,
    'At what point does simulation fidelity reach diminishing returns for learning transfer? Is there a ceiling where further technical realism (haptic feedback, chaotic environmental elements, cascading failures) produces no additional competence gain?',
    'Controlled learning-transfer studies comparing operators trained on progressively higher-fidelity simulators, measuring competence retention and decision-quality in actual operations (via incident data, near-miss analysis, post-event investigations). Compare to operators trained on lower-fidelity simulators + systematic near-miss analysis.',
    'If a ceiling exists well below current fidelity levels, simulator vendors are selling unnecessary technical complexity, and the constraint''s extractiveness is masked by false functional necessity. If no ceiling exists but learning-transfer plateaus, the constraint becomes piton-like (expensive performance without additional benefit). If learning-transfer improves continuously with fidelity, the constraint''s technical justification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fidelity_sufficiency_limits, empirical, 'Whether simulation fidelity is functionally equivalent to catastrophe across all relevant dimensions or whether learning-transfer has natural limits.').

omega_variable(
    organizational_trauma_irreplaceability,
    'Is organizational trauma (the institutional shock, mortality salience, and social reorganization that follows a real catastrophe) functionally replaceable by psychological realism in a high-fidelity drill? Or is trauma itself the necessary selection pressure that simulation cannot replicate?',
    'Longitudinal study of organizations that experienced actual catastrophes (e.g., airline accidents, hospital failures) versus organizations that have never experienced catastrophe but maintain high-fidelity simulation programs. Compare competence trajectories, near-miss detection rates, and organizational memory durability across 10+ years post-event or post-program-establishment.',
    'If trauma is irreplaceable, the sibling reading catastrophe_as_necessary_selector gains credibility, and this reading''s claim to functional equivalence becomes false. The constraint would then be a snare: it extracts resources under a false competence-maintenance claim while actually suppressing the organizational adaptation (institutional restructuring, career consequences, reputational effects) that catastrophe-based learning requires. If trauma effects are temporary and simulation-based learning produces comparable long-term competence, this reading''s premise holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(organizational_trauma_irreplaceability, conceptual, 'The irreducibility of actual organizational trauma to high-realism simulation; the learning-functional role of mortality salience and institutional disruption.').

omega_variable(
    suppression_of_near_miss_infrastructure,
    'Does the regulatory focus on simulation-based training reduce investment in and institutional recognition of near-miss analysis systems and foreign incident review networks as alternative competence-maintenance mechanisms?',
    'Comparative funding and staffing analysis across aviation, nuclear, and emergency-response sectors: budget allocation to simulation programs versus incident-analysis infrastructure; publication rates and policy impact of near-miss research versus simulation-outcome studies; regulatory weight given to near-miss data in competence certification.',
    'If simulation-focused regulation suppresses near-miss infrastructure, the constraint is partly extractive through opportunity cost—competence maintenance is achievable via lower-cost, distributed learning, but regulation mandates the higher-cost simulation path, channeling resources to simulator vendors. This would elevate extractiveness and point toward piton or snare classification. If both simulation and near-miss infrastructure are well-funded, the suppression dynamic is minimal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_of_near_miss_infrastructure, empirical, 'Whether the simulation-proxy reading crowds out alternative, potentially more efficient learning methods through regulatory focus.').

omega_variable(
    reading_contingency_on_infrastructure_viability,
    'Does this reading (simulation as functional catastrophe proxy) depend on simulator infrastructure remaining affordable and accessible? If simulator costs rise (due to technological complexity, vendor consolidation, or resource constraints in developing nations), does the reading''s normative claim (competence is maintainable via scheduled drills) remain defensible?',
    'Natural experiment: monitoring competence trajectories in organizations forced to reduce simulator access due to cost (e.g., regional operators, developing-nation operators that lose infrastructure funding). Compare competence maintenance pathways they adopt (near-miss analysis, competence-decay monitoring, hybrid low-fidelity simulation). If competence remains stable via alternatives, the reading''s claim of functional equivalence is falsified. If competence decays, the reading''s claim holds but the access-inequality problem becomes acute.',
    'If this reading''s normative claim depends on simulator viability, the constraint''s justice implications shift with infrastructure availability. Resource-constrained organizations would face impossible choice sets: comply with unfunded mandates or lose licensure. This would deepen the extraction measurement and point toward snare classification. The sibling reading hybrid_near_miss_learning would gain traction as the justice-defensible reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contingency_on_infrastructure_viability, empirical, 'The reading''s viability contingency on sustained, globally accessible simulator infrastructure and the equity implications if that contingency fails.').

omega_variable(
    kernel_resolution_via_sibling_readings,
    'Which of the three kernel readings most accurately predicts competence trajectories under different learning regimes? Does competence actually require catastrophe (reading 1), simulation (reading 3), or hybrid near-miss learning (reading 2)?',
    'Systematic comparison across domains (aviation, nuclear, emergency response, maritime) of competence outcomes under different learning regimes: catastrophe-dependent learning (rare, historical), simulation-dominant (contemporary regulatory standard, this reading), and hybrid near-miss learning (emerging in some sectors). Measure via incident rates, near-miss detection quality, post-event investigation findings, and operator self-assessed readiness.',
    'This omega names the irreducible uncertainty at the kernel level: the three readings coexist because they are not falsifiable by any single dataset—each emphasizes different learning mechanisms (trauma, fidelity, distributed analysis) and outcomes come from multiple sources. If future research decisively shows one reading''s causal mechanism is dominant, that reading would gain empirical warrant and the others would be reclassified as false summits or snares depending on whether they continue to be enforced despite the falsification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_resolution_via_sibling_readings, conceptual, 'The kernel-level uncertainty: which reading''s theory of competence maintenance is empirically justified?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 0, 0.22).
narrative_ontology:measurement(cata_tr_t5, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 5, 0.27).
narrative_ontology:measurement(cata_tr_t10, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 10, 0.33).
narrative_ontology:measurement(cata_tr_t15, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 15, 0.38).
narrative_ontology:measurement(cata_tr_t20, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 20, 0.41).
narrative_ontology:measurement(cata_tr_t25, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 25, 0.42).
narrative_ontology:measurement(cata_tr_t30, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 30, 0.42).
narrative_ontology:measurement(cata_tr_t40, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(cata_be_t5, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(cata_be_t10, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 10, 0.54).
narrative_ontology:measurement(cata_be_t15, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(cata_be_t20, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(cata_be_t25, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 25, 0.61).
narrative_ontology:measurement(cata_be_t30, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 30, 0.61).
narrative_ontology:measurement(cata_be_t40, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 40, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(cata_su_t5, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 5, 0.39).
narrative_ontology:measurement(cata_su_t10, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 10, 0.43).
narrative_ontology:measurement(cata_su_t15, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 15, 0.46).
narrative_ontology:measurement(cata_su_t20, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 20, 0.48).
narrative_ontology:measurement(cata_su_t25, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 25, 0.48).
narrative_ontology:measurement(cata_su_t30, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 30, 0.48).
narrative_ontology:measurement(cata_su_t40, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 40, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 0.12).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, catastrophe_as_necessary_selector).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, hybrid_near_miss_learning).

% DUAL FORMULATION NOTE:
% This constraint (simulation_as_proxy_catastrophe) is one of three readings of the kernel 'catastrophe_avoidance_retention.' The kernel describes a stabilized commitment: organizations must maintain safety-critical competence without using actual catastrophes as teaching moments. This reading asserts high-fidelity simulation is functionally equivalent to real catastrophes for competence maintenance—a claim that is empirically contested. The sibling readings catastrophe_as_necessary_selector and hybrid_near_miss_learning articulate different normative and empirical premises about competence-maintenance mechanisms. Each reading instantiates a different constraint with different ε values, victim/beneficiary structures, and classifications. They are linked via network.affects_constraints to flag the kernel-level structure: the three constraints are not independent; understanding competence policy requires analyzing the contested readings as a family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
