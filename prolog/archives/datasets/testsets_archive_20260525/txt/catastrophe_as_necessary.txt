% ============================================================================
% CONSTRAINT STORY: catastrophe_as_necessary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_as_necessary, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: catastrophe_as_necessary
 *   human_readable: Catastrophe as Necessary Organizational Learning
 *   domain: safety_engineering/organizational_learning/high_reliability_organizations
 *
 * SUMMARY:
 *   The catastrophe-as-necessary constraint operates in high-reliability
 *   organizations where maintaining genuine competence requires visceral
 *   stakes. The mechanism is simple but troubling: simulation-based training
 *   creates false confidence while actual competence decays invisibly during
 *   incident-free periods. Organizations lack incentive to invest in
 *   expensive near-miss infrastructure or high-consequence simulation because
 *   these require sustained capital investment, while catastrophes provide
 *   free (though destructive) competence resets. This constraint exhibits all
 *   six classification types from different structural positions, each
 *   revealing a different aspect of the system's pathology. The original
 *   research group (or safety advocates) experience this as a snare: they
 *   cannot recommend catastrophe-dependent competence without moral hazard,
 *   but recommending simulation-only training is negligent. The organization
 *   experiences it as a rope or arbitrage: the low-cost equilibrium (minimal
 *   training, hope for rare catastrophes) is coordinated and benefits the
 *   budget-constrained institution. Regulators experience mixed coordination
 *   and extraction. Simulation theaters persist as pitons—high institutional
 *   theater masking low functional competence transfer. The analytical view
 *   risks naturalizing organizational choice as inevitable human
 *   neurobiology. The constraint's measurement signature shows extractiveness
 *   and theater both rising over a 6-year incident-free period: as time
 *   passes without catastrophe, organizational confidence in simulation grows
 *   (false), competence decay accelerates (real), and the theater ratio
 *   increases (more training, more compliance documentation, no actual
 *   improvement in decision-making under uncertainty). When catastrophe
 *   finally occurs, it is simultaneously (a) discovery of invisible decay,
 *   (b) expensive organizational reset, (c) vindication of the
 *   catastrophe-as-necessary framing, and (d) reinvestment in simulation
 *   theater. The cycle repeats.
 *
 * KEY AGENTS:
 *   - Safety Engineers: Primary victims (powerless/trapped) — responsible for competence without sufficient resources to maintain it through non-catastrophic means. Career incentives penalize them for recommending either catastrophe or expensive near-miss infrastructure.
 *   - Organizational Competence: Victim (powerless/trapped) — abstract collective good, cannot organize or exit. Decays invisibly during quiet periods.
 *   - Budget-Constrained Organization: Primary beneficiary (institutional/arbitrage) — coordinates on low-cost equilibrium; defers expensive training/infrastructure; benefits from catastrophes providing free competence resets.
 *   - Simulation Infrastructure: Institutional actor (institutional/arbitrage) — maintains theater-ratio-climbing practice; budgets escalate despite acknowledged ineffectiveness at preventing competence decay.
 *   - Regulatory Authority: Powerful actor (powerful/mobile) — has capacity to mandate near-miss infrastructure and consequence-inclusive simulation but benefits from catastrophes (justifies regulation expansion and budget growth). Identity-locked to regulatory mandate despite technical mobility.
 *   - Analytical Observer: Risk naturalizer — conflates irreducible cognitive limits (threat-response downregulation during quiet) with absence of alternatives (near-miss infrastructure, consequence-inclusive simulation, rotational deployment).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_as_necessary, 0.68).
domain_priors:suppression_score(catastrophe_as_necessary, 0.72).
domain_priors:theater_ratio(catastrophe_as_necessary, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_as_necessary, extractiveness, 0.68).
narrative_ontology:constraint_metric(catastrophe_as_necessary, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(catastrophe_as_necessary, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_as_necessary, snare).
narrative_ontology:human_readable(catastrophe_as_necessary, "Catastrophe as Necessary Organizational Learning").
narrative_ontology:topic_domain(catastrophe_as_necessary, "safety_engineering/organizational_learning/high_reliability_organizations").

% --- Structural relationships ---
narrative_ontology:constraint_victim(catastrophe_as_necessary, safety_engineers).
narrative_ontology:constraint_victim(catastrophe_as_necessary, organizational_competence).
narrative_ontology:constraint_victim(catastrophe_as_necessary, incident_prevention_advocates).
narrative_ontology:constraint_victim(catastrophe_as_necessary, near_miss_reporting_systems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SAFETY ENGINEER (SNARE) — Trapped in a system where competence maintenance requires either catastrophic events or a bypass mechanism that the organization refuses to fund. The engineer cannot exit: recommending simulation-only training is career suicide (false confidence catastrophe implicates the recommender); advocating for more costly near-miss response systems faces budget resistance. Maximum extraction: the engineer bears responsibility for competence without the resources to maintain it through non-catastrophic means.
constraint_indexing:constraint_classification(catastrophe_as_necessary, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ORGANIZATIONAL COMPETENCE (SNARE) — An abstract collective good that cannot organize to defend itself. During incident-free periods, competence decays invisibly: procedural knowledge fades, new personnel lack visceral understanding of consequences, false confidence accumulates in simulation environments. The competence victim has no voice and no exit — it is extracted from gradually until catastrophe resets the organization's epistemic state at terrible cost.
constraint_indexing:constraint_classification(catastrophe_as_necessary, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: BUDGET-CONSTRAINED ORGANIZATION (ROPE) — Benefits from the constraint that catastrophe is 'necessary' because it justifies deferring expensive near-miss infrastructure and high-frequency simulation training. The organization coordinates on the low-cost equilibrium: maintain simulation theater and incident-response theater, wait for reality to provide free (catastrophic) competence reset. Extraction runs toward the organization; they experience the constraint as a natural feature of economics.
constraint_indexing:constraint_classification(catastrophe_as_necessary, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY AUTHORITY (TANGLED ROPE) — Powerful but constrained by competing mandates. Regulation can mandate near-miss reporting, simulation frequency, and training rigor — coordination function. But regulation creates theater and compliance capture (extraction). The regulator benefits from catastrophes (they justify tighter regulation and larger budgets) while claiming to prevent them. Mobile exit exists (shift to different regulatory regime) but professional identity is fused to the role, creating constraint-like behavior despite technical mobility.
constraint_indexing:constraint_classification(catastrophe_as_necessary, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: SIMULATION THEATER (PITON) — High theater ratio (0.55). Simulation training persists as institutional practice despite acknowledged limitations in transferring tacit competence. The simulation infrastructure is maintained through inertia and regulatory compliance, not because it functionally maintains competence at necessary levels. Simulation is the performative substitute for real stakes — it looks like preparation but fails to create visceral learning. Theater is escalating: more sophisticated simulators, more training hours, more compliance documentation, while competence decay continues invisibly.
constraint_indexing:constraint_classification(catastrophe_as_necessary, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, human cognitive systems may have inherent limits: procedural memory decays without reinforcement, threat-response systems downregulate during quiet periods, and only genuine stakes reactivate full attention. This perspective sees the constraint as an immutable property of human neurobiology and psychology — competence decay is inevitable without existential reinforcement. However, this naturalizes what may be a contingent organizational choice (failure to invest in high-fidelity near-miss infrastructure and community-based competence networks) as an immutable law.
constraint_indexing:constraint_classification(catastrophe_as_necessary, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_as_necessary_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(catastrophe_as_necessary, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(catastrophe_as_necessary, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_as_necessary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(catastrophe_as_necessary, TR),
    TR >= 0.70.

:- end_tests(catastrophe_as_necessary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High but not maximal. The constraint extracts severely from safety engineers and organizational competence systems (they bear costs without benefits). But the budget-constrained organization and simulation infrastructure benefit, and the overall extraction mechanism is not as oppressive as pure snare (0.72+) — it has coordination aspects (everyone agrees simulation is cheaper than alternatives) and external justification (cognition research suggests threat-response systems downregulate). The 0.68 value reflects high extraction with partial-coherence narrative cover. Suppression (0.72): High. Multiple barriers prevent exit: (a) recommending catastrophe is organizationally impossible; (b) recommending expensive near-miss infrastructure is budget-rejected; (c) recommending simulation-only is negligent; (d) leaving the organization means abandoning competence maintenance role to those who will recommend worse equilibria. Theater ratio (0.55): Moderate-high. Simulation training produces high institutional theater (training hours, compliance documentation, curriculum sophistication) while actual competence transfer is low-to-moderate. Theater is escalating as incident-free time grows and organizations invest more heavily in simulation as substitute for near-miss infrastructure. The measurements show this: extractiveness rises as organizations rely more on simulation; theater climbs as training becomes more elaborate without corresponding competence improvement.
 *
 * PERSPECTIVAL GAP:
 *   The gap between the safety engineer's snare (no exit, maximum extraction, trapped competence responsibility) and the organization's rope (coordination on low-cost equilibrium, benefits captured) is the core structural tension. The engineer cannot leave (professional identity, commitment to competence); the organization will not fund alternatives (budget constraints, false confidence in simulation). Regulators occupy a middle ground: they have power to mandate expensive infrastructure but benefit (politically and budgetarily) from catastrophes that justify expansion. The simulation theater persists as a piton—acknowledged as insufficient but maintained through institutional inertia and regulatory checkbox compliance.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality varies by agent's structural position. Safety engineers face high d (near 1.0): trapped, powerless, bearing extraction costs without benefit — f(d) pushes toward high chi. The budget-constrained organization faces low d (near 0.1): institutional power, arbitrage exit, capturing benefits — f(d) produces negative or near-zero chi (experienced as coordination). Regulators face moderate d (0.55-0.65): powerful but identity-locked to regulatory mandate, receiving some benefits (budget growth, authority expansion) while being constrained by competing mandates (prevent incidents vs. maintain existing institutional structure). The derived directionality shows why the constraint persists despite clear victim designation: the primary beneficiary (budget-constrained organization) has institutional power and exit options (arbitrage), while the primary victims (safety engineers, organizational competence) are powerless and trapped. The power asymmetry drives the snare classification.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: This constraint resolves mandatrophy by anchoring on the kernel reading. The catastrophe-as-necessary reading is one of three possible interpretations of how competence retention works. The sibling readings (simulation_as_sufficient, near_miss_as_bridge) produce different constraint types from the same base phenomena. The mandatrophy here is 'which mechanism actually drives competence maintenance?'—not 'what is the right type?' The empirical path forward is to test near-miss infrastructure effectiveness at preventing catastrophe-driven decay, and to measure competence transfer from consequence-inclusive simulation. If these alternatives work, the catastrophe-as-necessary reading is falsified and competence retention becomes a resource-allocation problem (scaffold with sunset to mature near-miss infrastructure), not a snare. If they fail, catastrophe-as-necessary is validated and the snare classification stands.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    decay_invisibility_threshold,
    'What duration of incident-free operation produces competence decay sufficient to create genuine vulnerability?',
    'Longitudinal studies correlating time-since-last-incident with performance metrics in simulations, near-miss response quality, and (post-incident) actual incident severity. Measure decay rate for procedural memory, threat-assessment accuracy, and decision-making under uncertainty.',
    'If threshold < 1 year: near-miss infrastructure becomes cost-justified (snare reclassifies toward tangled_rope). If threshold > 5 years: organizations appear justified in treating catastrophe as rare reset event (snare persists). If decay is nonlinear (rapid initial decay, then plateau): targeted interventions at high-decay periods might replace catastrophe reset.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(decay_invisibility_threshold, empirical, 'Competence decay timeline under incident-free conditions').

omega_variable(
    simulation_transfer_mechanism,
    'Does the failure of simulation to transfer competence reflect inherent limits of simulation (fidelity gap is unclosable) or resolvable design defects (insufficient stakes, insufficient repetition, insufficient team integration)?',
    'Controlled studies: (a) high-fidelity simulation with real consequences for performance (career review, team standing, organizational reputation) vs. consequence-free simulation; (b) comparison of competence decay in organizations using high-consequence simulation vs. those without; (c) post-incident performance analysis comparing those trained with high-consequence simulation vs. standard simulation.',
    'If inherent limit: catastrophe-as-necessary view is robust — simulation cannot substitute. If design defect: organizations could replace catastrophe reset with properly-designed consequence-inclusive simulation (snare reclassifies to tangled_rope or scaffold with sunset to resilience infrastructure).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_transfer_mechanism, empirical, 'Whether simulation competence transfer failure is inherent or solvable by design').

omega_variable(
    near_miss_reporting_effectiveness,
    'Can systematic near-miss reporting and high-frequency low-cost simulation (rehearsal-at-scale) provide competence maintenance equivalent to catastrophe-driven reset without the catastrophe cost?',
    'Comparison cohorts: organizations with mature near-miss infrastructure and high-frequency simulation vs. those relying on rare catastrophes. Measure competence decay rates, incident severity post-training, and organizational learning metrics. Follow cohorts for 10+ years to capture both quiet periods and rare incidents.',
    'If effective: catastrophe-as-necessary view is false — organizations choose catastrophe because near-miss infrastructure is capital-intensive, not because alternatives don''t exist. Snare reclassifies to scaffold with real sunset (near-miss maturity). If ineffective: catastrophe-as-necessary is validated; snare persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(near_miss_reporting_effectiveness, empirical, 'Whether systematic near-miss response can replace catastrophe-driven competence reset').

omega_variable(
    kernel_reading_ambiguity,
    'This constraint is one reading of the competence_retention_exercise kernel. Is the mechanism ''catastrophe as necessary and irreplaceable'' or ''catastrophe as the cheapest current solution within organizational constraints''?',
    'Structural analysis: if organizations with sufficient capital investment in near-miss infrastructure and consequence-inclusive simulation show equivalent competence maintenance without catastrophes, the mechanism is economic choice masquerading as necessity. If such organizations still experience competence decay (validating catastrophe-as-necessary), the mechanism is truly irreducible.',
    'If economic choice: snare is extractive (organizations choose cheap-but-risky equilibrium), and the sibling reading (simulation_as_sufficient) becomes achievable through policy and capital reallocation. If truly irreducible: catastrophe-as-necessary reading is validated, and catastrophe becomes a structural feature to manage rather than prevent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether catastrophe is inherently necessary or contingently necessary given current organizational economics').

omega_variable(
    false_summit_candidate,
    'Is this a mountain (inherent limit of human cognition under zero-stakes conditions) or a snare (organizations choosing cheap-and-dangerous over expensive-and-safe)?',
    'The analytical mountain perspective depends on the claim that genuine stakes are necessary for human performance systems to maintain competence. This is partially true (threat-response systems do downregulate) but conflates necessary-for-peak-performance with necessary-for-adequate-performance. Organizations can maintain adequate competence without catastrophes through (a) high-consequence simulation, (b) frequent near-miss rehearsal, (c) rotational deployment in lower-stakes environments, (d) structured peer review and competence certification. The ''mountain'' naturalizes the absence of these investments as inevitable.',
    'If false summit confirmed: snare classification stands; organizations bear responsibility for choosing catastrophe over alternatives. Policy interventions become justified (mandate near-miss infrastructure, establish high-consequence simulation standards). If mountain confirmed: competence decay is an irreducible property of human systems; policy should focus on catastrophe preparedness rather than prevention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_candidate, conceptual, 'Whether catastrophe necessity is inherent to cognition or contingent on organizational investment choices').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_as_necessary, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_as_necessary, theater_ratio, 0, 0.42).
narrative_ontology:measurement(cata_tr_t2, catastrophe_as_necessary, theater_ratio, 2, 0.48).
narrative_ontology:measurement(cata_tr_t4, catastrophe_as_necessary, theater_ratio, 4, 0.52).
narrative_ontology:measurement(cata_tr_t6, catastrophe_as_necessary, theater_ratio, 6, 0.55).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_as_necessary, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(cata_be_t2, catastrophe_as_necessary, base_extractiveness, 2, 0.52).
narrative_ontology:measurement(cata_be_t4, catastrophe_as_necessary, base_extractiveness, 4, 0.62).
narrative_ontology:measurement(cata_be_t6, catastrophe_as_necessary, base_extractiveness, 6, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_as_necessary, enforcement_mechanism).
narrative_ontology:affects_constraint(catastrophe_as_necessary, simulation_as_sufficient).
narrative_ontology:affects_constraint(catastrophe_as_necessary, near_miss_as_bridge).
narrative_ontology:affects_constraint(catastrophe_as_necessary, false_confidence_in_training).
narrative_ontology:affects_constraint(catastrophe_as_necessary, organizational_competence_decay).

% DUAL FORMULATION NOTE:
% The catastrophe-as-necessary reading is structurally dependent on the claims that (a) simulation transfers competence poorly due to low stakes, (b) organizational competence decays invisibly during incident-free periods, and (c) no alternative mechanism can reset competence without catastrophe. These claims are tested by the sibling readings and by empirical studies of high-consequence simulation and near-miss infrastructure effectiveness. All three readings (catastrophe_as_necessary, simulation_as_sufficient, near_miss_as_bridge) operate on the same base organizational system but decompose the competence retention constraint into structurally distinct claims with different ε values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_as_necessary, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
