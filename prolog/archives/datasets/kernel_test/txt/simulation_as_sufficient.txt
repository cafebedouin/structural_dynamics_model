% ============================================================================
% CONSTRAINT STORY: simulation_as_sufficient
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_simulation_as_sufficient, []).

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
 *   constraint_id: simulation_as_sufficient
 *   human_readable: Simulation as Sufficient for Catastrophe-Avoidance Competence Retention
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   High-reliability organizations (HROs) — nuclear plants, aviation,
 *   offshore drilling, chemical processing — face a fundamental tension: they
 *   must maintain catastrophe-avoidance competence in their field operators,
 *   but ethical and practical constraints make real high-consequence training
 *   infeasible. The simulation-as-sufficient reading instantiates the
 *   institutional solution: fidelity-graduated simulators become the primary
 *   competence-maintenance mechanism. Real catastrophes are prevented by this
 *   approach, but the constraint extracts a hidden cost — competence is
 *   measured by simulator performance metrics rather than by actual decision
 *   quality under real uncertainty. This reading assumes that simulator
 *   fidelity can be calibrated to capture the cognitive and procedural
 *   demands of real events, that competence retention can be validated
 *   through simulator performance, and that the field operator's subjective
 *   sense of gap between simulator and reality represents risk perception
 *   bias rather than genuine epistemic difference. The constraint is a
 *   tangled rope because it coordinates genuine safety outcomes (simulators
 *   do catch many failure modes) while simultaneously extracting
 *   institutional authority over the definition of 'sufficient competence'
 *   and concentrating budget and prestige in the training infrastructure. The
 *   theater ratio (0.68) reflects that simulator-based competence
 *   certification is substantially performative: the ritual of certification
 *   (passing checkride, demonstrating procedures) is much more visible and
 *   measurable than the actual quality of decision-making under novel
 *   high-stakes conditions. The constraint's extractiveness has risen from
 *   0.35 to 0.58 over 25 years as regulatory pressure has consolidated
 *   simulator-based training as the exclusive path to certification, and as
 *   simulator technology vendors have captured standard-setting bodies.
 *
 * KEY AGENTS:
 *   - Field Operators: Primary victims (powerless/trapped) — cannot exit simulator-based certification regime; perceive gap between simulator training and real-world decision demands but institutional structures suppress dissent
 *   - Training Infrastructure Operators: Primary beneficiaries (institutional/arbitrage) — capture budget allocation, institutional authority, and prestige through simulator expansion; define what counts as 'sufficient competence'
 *   - Budget Gatekeepers and Regulators: Secondary beneficiaries (institutional/arbitrage) — benefit from cost reduction (simulators are much cheaper than real-world high-consequence training) and from liability reduction (controlled training environments are insurable; catastrophes are not)
 *   - Operations Organizations: Secondary victims (moderate/constrained) — bear catastrophe risk from potential competence gaps; constrained by budget requirements and regulatory mandates to use simulator-based training
 *   - Analytical Safety Community: Mixed (analytical/constrained) — coordinate genuine risk reduction but constrained by pressure to endorse simulator sufficiency for institutional legitimacy
 *   - Organizational Resilience: Abstract victim (powerless/trapped) — cannot exit the constraint; bears the structural consequence if simulator-based competence proves insufficient under rare high-stakes scenarios
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(simulation_as_sufficient, 0.58).
domain_priors:suppression_score(simulation_as_sufficient, 0.62).
domain_priors:theater_ratio(simulation_as_sufficient, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(simulation_as_sufficient, extractiveness, 0.58).
narrative_ontology:constraint_metric(simulation_as_sufficient, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(simulation_as_sufficient, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(simulation_as_sufficient, tangled_rope).
narrative_ontology:human_readable(simulation_as_sufficient, "Simulation as Sufficient for Catastrophe-Avoidance Competence Retention").
narrative_ontology:topic_domain(simulation_as_sufficient, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(simulation_as_sufficient).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(simulation_as_sufficient, training_infrastructure_operators).
narrative_ontology:constraint_beneficiary(simulation_as_sufficient, budget_gatekeepers).
narrative_ontology:constraint_beneficiary(simulation_as_sufficient, organizational_management).
narrative_ontology:constraint_victim(simulation_as_sufficient, field_operators).
narrative_ontology:constraint_victim(simulation_as_sufficient, organizational_resilience).
narrative_ontology:constraint_victim(simulation_as_sufficient, knowledge_preservation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FIELD OPERATOR (SNARE) — Trapped in simulator-driven certification regime. Cannot exit the constraint: career advancement requires simulator credentials; real operational competence decays when simulator scenarios omit the embodied knowledge, tacit environmental cues, and high-stakes decision pressures that constitute true catastrophe-avoidance. The operator is forced to treat simulator performance as proxy for real competence while understanding the gap is real. Maximum experienced extraction — no exit option and no coordination benefit.
constraint_indexing:constraint_classification(simulation_as_sufficient, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: OPERATIONS ORGANIZATION (TANGLED ROPE) — Constrained by regulatory requirements for simulator-based certification and by budget limitations that make real-world high-consequence training prohibitively expensive. But also coordinating genuine safety outcomes: simulator training IS catching some failure modes and building baseline competence. Extraction and coordination coexist. The organization bears the cost of potential competence gaps (catastrophe risk) while the training infrastructure captures budget and institutional authority. Some agency — they can adjust simulator fidelity, supplement with limited real-world exercises — but significant constraints.
constraint_indexing:constraint_classification(simulation_as_sufficient, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: TRAINING INFRASTRUCTURE OPERATORS (ROPE) — Primary beneficiaries. Simulators are their domain; they operate under the assumption that simulator fidelity IS competence fidelity, and they have strong incentives (budget allocation, expansion of simulator capability, institutional prestige) to maintain this framing. They experience the constraint as pure coordination: building better simulators solves the training problem. Net beneficiary with arbitrage options — they can shift to different simulation technologies, expand into new domains, migrate between organizations that value simulator-driven training.
constraint_indexing:constraint_classification(simulation_as_sufficient, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: BUDGET GATEKEEPERS AND REGULATORS (ROPE) — Institutional beneficiaries. Simulator-based certification is economical: it replaces expensive, dangerous real-world high-consequence training with controlled, repeatable, insurable scenarios. They experience the constraint as efficient coordination of safety and resource allocation. They have arbitrage options — they can shift certification standards, migrate to different regulatory frameworks, adjust stringency. The constraint reduces their liability exposure and costs.
constraint_indexing:constraint_classification(simulation_as_sufficient, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL SAFETY COMMUNITY (TANGLED ROPE) — Constrained by organizational pressures to endorse simulator sufficiency while knowing empirically that real-world competence involves dimensions simulators cannot capture: embodied learning, stress physiology under true stakes, environmental variability, rare edge cases. The safety community coordinates genuine risk reduction through simulators (real coordination value) while being extracted from by pressure to maintain the certification apparatus without adequate real-world validation. Moderately constrained — can publish critical analyses, but institutional pressure limits practical action. Derives d from power=analytical (canonical 0.73), exit_options=constrained (higher d than arbitrage), beneficiary/victim hybrid (receives institutional prestige for simulator validation, bears epistemic cost of fidelity gaps).
constraint_indexing:constraint_classification(simulation_as_sufficient, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: NATURAL LAW VIEW (MOUNTAIN) — From the civilizational/universal scope, some gap between simulator and real-world competence is inherent to training itself: simulators are finite models of infinite reality, and real consequences create decision pressures that cannot be ethically reproduced in training environments. This perspective sees the bottleneck as an immutable property of the training problem. However, the base properties suggest this is a FALSE SUMMIT: the constraint declares beneficiaries (training infrastructure, budget gatekeepers) and victims (field operators, organizational resilience), indicating the gap is being actively maintained and exploited, not arising from irreducible physics. The 'inevitable gap' framing naturalizes a contingent institutional choice — the choice to prioritize simulator-based certification over mixed training regimes that include controlled real-world scenarios.
constraint_indexing:constraint_classification(simulation_as_sufficient, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(simulation_as_sufficient_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(simulation_as_sufficient, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(simulation_as_sufficient, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(simulation_as_sufficient, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(simulation_as_sufficient_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The training infrastructure captures institutional authority, budget, and prestige through the definition of 'sufficient competence' based on simulator performance. Field operators bear the cost of potential competence gaps (their embodied knowledge degrades when simulators omit tacit environmental cues; their decision speed under true uncertainty may degrade when training emphasizes procedure over improvisation). However, the extraction is not total — simulators do provide real coordination value (baseline competence building, failure mode exposure, safe scenario iteration) and genuine catastrophe prevention. The value has risen over the interval as regulators have consolidated simulator-based training as the exclusive path and as field operators have had fewer opportunities for real-world high-consequence learning. Suppression (0.62): Moderate-high. Multiple structural barriers prevent field operators from exiting the constraint: regulatory requirements mandate simulator certification; career advancement depends on simulator credentials; organizations lack budget for mixed training regimes that include controlled real-world scenarios; institutional actors defend simulator sufficiency despite internal doubts. Field operators can articulate the gap (they know simulators are incomplete) but institutional structures suppress action on that knowledge. Theater ratio (0.68): High. Simulator-based competence certification is substantially performative. The ritual of certification (checkride passes, procedure demonstrations) is highly visible and measurable. Actual competence under novel conditions with real stakes — the true outcome being coordinated — is invisible and unmeasurable in the training context. The theater has increased as regulatory requirements have shifted from outcome-based accountability (preventing catastrophes) to process-based accountability (passing simulator checkrides), and as simulator technology vendors have marketed increasingly elaborate fidelity specifications as proxy for competence transfer.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits radical perspectival divergence from the base properties. The training infrastructure operators (institutional/arbitrage) experience pure coordination (Rope) — they see simulators as solving the training problem. Budget gatekeepers (institutional/arbitrage) experience coordination (Rope) — simulator training is cost-efficient. Field operators (powerless/trapped) experience pure extraction (Snare) — they are forced to treat simulator performance as competence proxy while understanding the gap. The operations organization (moderate/constrained) experiences mixed coordination-extraction (Tangled Rope) — they benefit from cost reduction but bear catastrophe risk. The analytical safety community (analytical/constrained) perceives the hybrid structure directly — they know coordinative value exists (simulators do build baseline competence) and extraction exists (institutional authority is concentrated in training infrastructure definitions of 'sufficient'). The civilizational analytical view risks naturalizing the simulator gap as an immutable property of training itself (Mountain), but the structural data (declared beneficiaries and victims) reveals this as false summit — the gap is being actively maintained and exploited by institutional actors who benefit from simulator-based certification.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality (d) is derived from their structural position: power level, exit options, and beneficiary/victim status. Training infrastructure operators are beneficiaries with arbitrage exit — they can shift simulator technologies, expand to new domains, migrate between organizations — deriving low d (~0.15), experiencing negative chi (they experience extraction flowing toward them, not away). Field operators are victims with trapped exit — no alternative training pathways exist in their organizations — deriving high d (~0.95), experiencing high f(d) (~1.42), amplifying the base extraction (ε=0.58) to high effective extraction chi. The operations organization is a victim with constrained exit — they can adjust simulator fidelity or supplement with real-world training, but at significant cost — deriving moderate-high d (~0.70). The analytical safety community, as victims with constrained exit (they can publish critical analyses but face institutional pressure), derive high d (~0.75), but their analytical power moderates the experienced chi through the f(d) sigmoid (analysts perceive structure more clearly, reducing apparent severity).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by distinguishing between genuine coordination (simulators prevent catastrophes, build baseline competence, enable safe scenario iteration) and extraction (institutional authority over competence definition, budget concentration, suppression of alternative training modalities). The constraint is tangled_rope because both are structurally present. The mandate (HROs must maintain catastrophe-avoidance competence) is satisfied by simulators — they do coordinate safety outcomes. But the mandate is extracted from by institutional actors who benefit from simulator-based certification and defend it against alternative approaches despite evidence of competence gaps. The mandatrophy is resolved by noting that the constraint's functionality (catastrophe avoidance) and its extraction mechanism (institutional authority capture) are structurally coupled but can be decoupled: the same safety coordination could occur through mixed training regimes (simulator + controlled real-world scenarios) that do not concentrate institutional authority in the training infrastructure. The current state is tangled_rope; an alternative institutional arrangement would be pure rope (training infrastructure as transparent coordination mechanism with no authority extraction). The classification holds because the current arrangement does both coordination and extraction simultaneously.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_simulation_sufficiency,
    'Is simulation-based training sufficient for catastrophe-avoidance competence retention, or is real-world experience (catastrophes, near-misses, or controlled high-stakes scenarios) structurally necessary?',
    'Comparison of catastrophe prevention rates, response quality, and decision-making under true uncertainty across organizations using pure-simulator training vs. mixed training regimes (simulator + controlled real-world exercises). Analysis of competence retention curves post-training under both regimes. Examination of failure modes in organizations that have shifted to pure-simulator certification.',
    'If simulation is sufficient: the constraint operates as described — institutional beneficiaries benefit from cost reduction and regulatory compliance. If real-world experience is necessary: the constraint becomes a snare across all perspectives — the training infrastructure is maintaining an illusion of competence to protect budget and institutional authority. Classification would shift from tangled_rope to snare or catastrophe_prevention_failure at all perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_simulation_sufficiency, empirical, 'Whether simulation-based training is sufficient for catastrophe-avoidance competence or if real experience is structurally necessary').

omega_variable(
    embodied_knowledge_capture,
    'Can simulator fidelity be increased to capture embodied, stress-based, and environmental decision-making dimensions, or are these dimensions fundamentally dependent on real-world high-stakes context?',
    'Longitudinal study of simulator fidelity improvements (haptic feedback, physiological stress simulation, environmental randomization) and correlation with real-world competence outcomes. Analysis of knowledge transfer between simulator domains (e.g., does nuclear reactor simulator training transfer to process plant decision-making?). Neuroscience research on embodied learning and fear conditioning in high-consequence contexts.',
    'If embodied dimensions are capturable: simulator sufficiency hypothesis is supported — technology scaling solves the fidelity gap. If fundamentally context-dependent: the constraint represents a structural mismatch between training medium and real-world demand. Training infrastructure would need to acknowledge irreducible limitations rather than claiming sufficiency through improved simulator specs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(embodied_knowledge_capture, empirical, 'Whether embodied knowledge and stress-based competence can be captured in simulators or are context-dependent').

omega_variable(
    near_miss_sufficiency_alternative,
    'Does the sibling reading ''near_miss_as_bridge'' (competence maintenance through low-consequence but high-realism scenarios) constitute a structurally superior training regime to pure simulation?',
    'Comparative organizational analysis: industries that use near-miss harvesting and controlled real-world training (aviation line-oriented flight training, medical simulation with patient-based scenarios) vs. pure-simulator regimes (nuclear, offshore, chemical). Outcome metrics: response quality under novel scenarios, decision speed, error rates, catastrophe prevention.',
    'If near-miss regimes produce superior competence: the simulation-as-sufficient reading is revealed as institutional preference (lower cost, higher liability control) rather than epistemic truth. The constraint would be reclassified as snare (institutional extraction) rather than tangled_rope (legitimate coordination-extraction hybrid). Opens path to the catastrophe_as_necessary reading if near-miss regimes are themselves constrained or made inaccessible by institutional pressure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(near_miss_sufficiency_alternative, empirical, 'Whether near-miss-based training provides superior competence retention compared to pure simulation').

omega_variable(
    regulatory_capture_in_simulator_standards,
    'Are simulator certification standards set primarily by simulation technology vendors and training infrastructure operators rather than by independent safety analysis or field operator input?',
    'Analysis of standard-setting bodies (FAA, IAEA, etc.): who sits on certification committees, what evidence drives fidelity requirements, whose interests are represented. Examination of standards revision history: which parties propose changes, which changes are adopted. Field operator surveys: do operators perceive simulator-based certification as adequate, or is there systematic pressure to endorse it despite perceived gaps?',
    'If capture is significant: the constraint operates as institutional extraction masked as safety coordination. Beneficiaries (training infrastructure) have captured the regulatory definition of ''sufficient competence'' to protect their market position. Classification becomes snare with institutional beneficiaries rather than tangled_rope with legitimate coordination function. Suppression is higher than measured (field operator doubts are institutionally suppressed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_in_simulator_standards, empirical, 'Whether simulator certification standards reflect independent safety analysis or captured by training infrastructure interests').

omega_variable(
    identity_locked_instructor_dynamics,
    'Are simulator instructors and training infrastructure professionals identity-fused with the simulator-as-sufficient thesis, making them unable to perceive or advocate for alternative training regimes despite epistemic doubts?',
    'Ethnographic study of training infrastructure organizations: how do professionals talk about simulator limitations in private vs. public contexts? Analysis of professional development pathways: does career advancement require endorsement of simulator sufficiency? Surveys of trainers on their actual beliefs about simulator-real-world fidelity gaps vs. their institutional role performance.',
    'If identity lock is strong: the suppression metric (0.62) understates the actual constraint severity. Field operators are not just externally constrained but watch institutional actors defend simulator sufficiency despite internal doubts — the defense is performed as identity maintenance rather than empirical position. Creates secondary snare around knowledge workers in the training infrastructure (they are trapped by their professional identity even if the training apparatus is ultimately changed).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_locked_instructor_dynamics, empirical, 'Whether training professionals are identity-locked to the simulator-sufficiency thesis').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(simulation_as_sufficient, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(simsuff_tr_t0, simulation_as_sufficient, theater_ratio, 0, 0.42).
narrative_ontology:measurement(simsuff_tr_t8, simulation_as_sufficient, theater_ratio, 8, 0.55).
narrative_ontology:measurement(simsuff_tr_t15, simulation_as_sufficient, theater_ratio, 15, 0.65).
narrative_ontology:measurement(simsuff_tr_t25, simulation_as_sufficient, theater_ratio, 25, 0.68).

% Extraction over time
narrative_ontology:measurement(simsuff_be_t0, simulation_as_sufficient, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(simsuff_be_t8, simulation_as_sufficient, base_extractiveness, 8, 0.45).
narrative_ontology:measurement(simsuff_be_t15, simulation_as_sufficient, base_extractiveness, 15, 0.55).
narrative_ontology:measurement(simsuff_be_t25, simulation_as_sufficient, base_extractiveness, 25, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(simulation_as_sufficient, enforcement_mechanism).
narrative_ontology:affects_constraint(simulation_as_sufficient, catastrophe_as_necessary).
narrative_ontology:affects_constraint(simulation_as_sufficient, near_miss_as_bridge).
narrative_ontology:affects_constraint(simulation_as_sufficient, competence_measurement_metrics).
narrative_ontology:affects_constraint(simulation_as_sufficient, organizational_learning_from_failure).

% DUAL FORMULATION NOTE:
% The simulation-as-sufficient constraint is part of a constraint family organized around the kernel 'competence_retention_exercise.' Sibling readings (catastrophe_as_necessary, near_miss_as_bridge) are separate story files with different ε values and different structural relationships. All three stories share the same fundamental organizational tension (how to maintain catastrophe-avoidance competence without exposing personnel to catastrophic risk) but propose different solutions. They are linked via network.affects_constraints because regulatory capture in one reading affects the feasibility of alternative readings — if simulation-as-sufficient captures the standard-setting apparatus, near-miss approaches become institutionally inaccessible, and catastrophe_as_necessary becomes the only alternative, completing the trap.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(simulation_as_sufficient, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
