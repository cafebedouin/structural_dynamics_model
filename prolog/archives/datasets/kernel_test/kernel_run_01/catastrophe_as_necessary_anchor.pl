% ============================================================================
% CONSTRAINT STORY: catastrophe_as_necessary_anchor
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_as_necessary_anchor, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: catastrophe_as_necessary_anchor
 *   human_readable: Catastrophe as Necessary Anchor for Safety Competence
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   The catastrophe-as-necessary-anchor constraint asserts that in
 *   safety-critical domains (aviation, medicine, nuclear operations,
 *   emergency response), human competence in managing catastrophic scenarios
 *   can only be maintained through exposure to real catastrophic events or
 *   near-misses. Simulation, no matter how sophisticated, cannot substitute
 *   because it operates in a fundamentally different cognitive and
 *   physiological state than actual catastrophe. This constraint legitimizes
 *   expensive, continuous recertification cycles and justifies post-incident
 *   authority expansion by regulatory agencies. However, it also creates a
 *   genuine coordination function: the doctrine drives organizations toward
 *   redundancy, stress-testing, near-miss investigation, and premortem
 *   analysis that structurally reduce catastrophe risk. The constraint is a
 *   kernel reading — one interpretation of how human competence actually
 *   works — that competes with sibling readings (simulation can be adequate
 *   with sufficient fidelity; competence maintenance is hybrid, requiring
 *   both simulation and selective real-event exposure). This story
 *   instantiates only the catastrophe-as-necessary-anchor reading, treating
 *   it as a single, ε-invariant constraint. The extraction mechanism is
 *   substantial but mixed with genuine coordination benefit, making
 *   tangled_rope the canonical classification. The constraint exhibits
 *   theater ratio drift (rising from 0.35 to 0.48) reflecting that
 *   recertification regimes are increasingly performative as organizational
 *   safety records extend across decades without incident.
 *
 * KEY AGENTS:
 *   - Safety Training Personnel: Primary victims (powerless/trapped) — professional identity staked on simulation efficacy; constraint systematically devalues their work; no exit except to advocate against the doctrine and risk career damage
 *   - Catastrophe-Free Operators: Primary victims (powerless/trapped) — individuals in perfect safety records who face implicit cognitive burden that their competence is illusory; structural trap: inducing catastrophe to prove competence is the only exit
 *   - Regulatory Agencies: Primary beneficiary (institutional/constrained) — genuine coordination function (robust simulation incentives) but also extraction (authority expansion, expensive recertification justification, post-incident power consolidation)
 *   - High-Reliability Organizations: Secondary beneficiary (institutional/arbitrage) — benefits from doctrine during safe periods (drives protective practices); doctrine deflects responsibility to operators and trainers, not to organizational leadership
 *   - Safety Learning Systems Community: Secondary victim (organized/constrained) — their sophisticated frameworks for near-miss learning are devalued by assertion that only real catastrophe teaches; but organized enough to resist total delegitimization
 *   - Competence Maintenance Ritual: Institutional actor (institutional/arbitrage) — the doctrine persists as theater atop genuinely protective machinery; no one can prove stopping recertification is safe, so ritual continues
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_as_necessary_anchor, 0.58).
domain_priors:suppression_score(catastrophe_as_necessary_anchor, 0.72).
domain_priors:theater_ratio(catastrophe_as_necessary_anchor, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_as_necessary_anchor, extractiveness, 0.58).
narrative_ontology:constraint_metric(catastrophe_as_necessary_anchor, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(catastrophe_as_necessary_anchor, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_as_necessary_anchor, tangled_rope).
narrative_ontology:human_readable(catastrophe_as_necessary_anchor, "Catastrophe as Necessary Anchor for Safety Competence").
narrative_ontology:topic_domain(catastrophe_as_necessary_anchor, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(catastrophe_as_necessary_anchor).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_as_necessary_anchor, '0adcc78e-b27e-49bc-b232-87342603b185').
narrative_ontology:cs_created_at('0adcc78e-b27e-49bc-b232-87342603b185', '').
narrative_ontology:cs_kernel_codification('0adcc78e-b27e-49bc-b232-87342603b185', distributed).
narrative_ontology:cs_authority_grounding('0adcc78e-b27e-49bc-b232-87342603b185', extraction).
narrative_ontology:cs_kernel_id(catastrophe_as_necessary_anchor, competence_exercise_requirement).
narrative_ontology:cs_reading_relation('0adcc78e-b27e-49bc-b232-87342603b185', simulation_as_adequate_exercise, forecloses).
narrative_ontology:cs_reading_relation('0adcc78e-b27e-49bc-b232-87342603b185', hybrid_dependency, coexists_with).
narrative_ontology:cs_axiom('0adcc78e-b27e-49bc-b232-87342603b185', foundational, catastrophe_irreducible_for_embodied_competence).
narrative_ontology:cs_axiom_status(catastrophe_irreducible_for_embodied_competence, holdable).
narrative_ontology:cs_axiom('0adcc78e-b27e-49bc-b232-87342603b185', secondary, simulation_cannot_reproduce_amygdala_state_of_catastrophe).
narrative_ontology:cs_axiom_status(simulation_cannot_reproduce_amygdala_state_of_catastrophe, holdable).
narrative_ontology:cs_reference_frame('0adcc78e-b27e-49bc-b232-87342603b185', competence_requires_real_stakes).
narrative_ontology:cs_drift_state('0adcc78e-b27e-49bc-b232-87342603b185', contemporary_extended_safety, gap(axiom_overriding, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_as_necessary_anchor, regulatory_agencies).
narrative_ontology:constraint_beneficiary(catastrophe_as_necessary_anchor, risk_premium_capture).
narrative_ontology:constraint_victim(catastrophe_as_necessary_anchor, safety_training_personnel).
narrative_ontology:constraint_victim(catastrophe_as_necessary_anchor, high_reliability_learning_systems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SAFETY TRAINING PROFESSIONAL (SNARE) — Trapped in a constraint that invalidates their primary work product. Simulation-based training is comprehensive, theoretically sound, and demonstrably practiced — yet the constraint asserts it cannot maintain competence without real catastrophe. Career built on defending the efficacy of simulation now faces structural delegitimization. No exit: must continue training while the constraint systematically devalues that training.
constraint_indexing:constraint_classification(catastrophe_as_necessary_anchor, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CATASTROPHE-FREE OPERATORS (SNARE) — Individuals in safety-critical roles (pilots, surgeons, nuclear plant operators, emergency responders) who have never encountered a real crisis during their career. The constraint asserts they lack genuine competence despite perfect simulations and zero incidents. Cognitive burden: knowing that your certification may be illusory, that 'safe' conditions (no real events) are evidence of your own incompetence. No exit without inducing the catastrophe the constraint describes — the mechanism itself is trapped.
constraint_indexing:constraint_classification(catastrophe_as_necessary_anchor, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: REGULATORY AGENCY (TANGLED ROPE) — Genuine coordination function: catastrophe-as-anchor doctrine creates incentives for robust simulation, redundancy, and pre-event stress-testing that reduce actual catastrophe risk. But also benefits from extraction: the doctrine justifies expensive, repeated recertification cycles (competence decays during safe periods = need for continuous paid retraining); justifies post-incident authority expansion (we need to strengthen oversight because competence atrophied); justifies retention of operational authority during incident response (outside simulators cannot have 'real' competence). Constrained exit: cannot abandon the doctrine without admitting that decades of regulation may have been building competence unnecessarily.
constraint_indexing:constraint_classification(catastrophe_as_necessary_anchor, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: HIGH-RELIABILITY ORGANIZATION (ROPE) — Net beneficiary during the safe period. The catastrophe-as-anchor doctrine drives organizational practices (premortem analysis, near-miss investigation, simulation complexity, redundancy design) that are structurally protective. HRO experiences the doctrine as pure coordination instruction — do these things and catastrophe stays rare. The doctrine's extraction mechanism (devaluation of peacetime competence) is paid by training staff and individual operators, not by the organization's leadership.
constraint_indexing:constraint_classification(catastrophe_as_necessary_anchor, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: SAFETY LEARNING SYSTEMS COMMUNITY (TANGLED ROPE) — Organized actors (incident investigators, organizational learning researchers, high-reliability scholars) who have built sophisticated frameworks for extracting learning from near-misses, simulations, and incident data. These communities see the constraint as partially correct (real events do provide irreducible learning) but also as an extraction mechanism that discounts their primary contribution: the institutional capacity to learn from almost-accidents rather than requiring actual ones. Constrained because the doctrine has substantial explanatory power (it is genuinely difficult to learn from simulation alone), but also organized enough to resist total delegitimization.
constraint_indexing:constraint_classification(catastrophe_as_necessary_anchor, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: COMPETENCE MAINTENANCE RITUAL (PITON) — The constraint has degraded into performative recertification and periodic stress-testing that persists through institutional inertia rather than demonstrated necessity. High-reliability organizations that have eliminated catastrophic failure for decades continue expensive recurrent training cycles on the premise that peacetime = competence decay. The ritual persists because no one can prove that stopping it is safe, not because evidence demands it. Theater ratio remains moderate rather than high because the underlying organizational practices are genuinely protective — but the explicit doctrine linking catastrophe to competence is theater atop protective machinery that works for other structural reasons.
constraint_indexing:constraint_classification(catastrophe_as_necessary_anchor, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a cognitive neuroscience and learning science perspective, the constraint reflects an immutable feature of human skill acquisition: procedural memory (knowing how) is constitutionally different from declarative memory (knowing about), and the gap between them can only be closed through embodied, high-stakes practice. Simulations, no matter how sophisticated, operate in low-stakes cognitive contexts; the amygdala state during real catastrophe cannot be simulated. This perspective treats catastrophe-as-anchor as emerging naturally from the structure of human learning itself — a law of cognition, not a contingent organizational arrangement.
constraint_indexing:constraint_classification(catastrophe_as_necessary_anchor, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_as_necessary_anchor_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(catastrophe_as_necessary_anchor, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(catastrophe_as_necessary_anchor, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_as_necessary_anchor, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(catastrophe_as_necessary_anchor, TR),
    TR >= 0.70.

:- end_tests(catastrophe_as_necessary_anchor_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint creates legitimate extraction: regulatory capture of continuous recertification, justification for authority expansion, delegation of safety competence burden to operators and trainers rather than organizational leadership. But the extraction is not total (snare-level) because the underlying coordination function is real — catastrophe-as-anchor doctrine does drive protective practices that reduce risk. The 0.58 value reflects that the doctrine captures 40-60% extractive rent while maintaining 40-60% genuine coordination benefit. Suppression (0.72): High. Suppression operates through cognitive mechanisms: the doctrine makes it nearly impossible for training personnel to argue that their simulation-based competence maintenance is adequate (they are in the logic loop defending what the doctrine says is indefensible); makes it impossible for long-safe-period operators to claim confidence in their own competence without invoking catastrophe. Material suppression also exists: expensive recertification requirements create financial barriers to training critique. Theater ratio (0.48): Moderate. The constraint has moderate theater because the underlying organizational practices (simulation, redundancy, near-miss investigation) are genuinely protective — the theater is in the explicit linking of these practices to catastrophe-necessity rather than to other protective mechanisms. As safety records extend (5-10 year interval), theater ratio rises slightly because the doctrine must justify continuing expensive regimes in the absence of the catastrophe that supposedly proves necessity.
 *
 * PERSPECTIVAL GAP:
 *   The catastrophe-as-anchor constraint produces a massive perspectival gap because different actors experience the same structural feature — long periods without incident — as evidence of different things. For regulatory agencies and HROs, long safety records are evidence that protective practices are working (confirmation). For training personnel and operators under the constraint's doctrine, long safety records are evidence of competence decay (the mechanism the constraint describes). For the analytical observer, long safety records are evidence that the constraint might be false — if catastrophe were truly necessary to maintain competence, why does competence apparently persist during decades of peace? The gap reveals that the constraint is not a law of nature (mountain) but a contested kernel reading that competes with others.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (regulatory agencies, risk-premium-capture systems) are institutional actors with arbitrage exit options — they can profit from the constraint while remaining structurally intact. Their directionality (d ≈ 0.05-0.20) produces low or negative experienced extractiveness (f(d) ≈ -0.12 to +0.02), making the constraint appear as pure coordination (rope) from their perspective. Victims (training personnel, operators, high-reliability learning systems) are powerless/trapped or organized/constrained actors with no real exit — they must either accept that their simulation-based competence is illusory or advocate against a doctrine that has institutional buy-in from regulatory agencies. Their directionality (d ≈ 0.85-0.95) produces high experienced extractiveness (f(d) ≈ 1.15-1.42), making the constraint appear as snare from their perspective. The regulatory agency itself is constrained (high cost to abandon the doctrine after decades of enforcement), so its perspective is tangled_rope rather than pure rope — it experiences both coordination benefit and extraction cost, though from an institutional position that is nonetheless a beneficiary.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids mandatrophy (misclassifying coordination as extraction or vice versa) by explicitly declaring both beneficiaries and victims with clear structural relationships. The coordination function (doctrine drives protective practices) is genuine and documented in the regulatory agency and HRO perspectives. The extraction mechanism (devalues training competence, justifies authority expansion, extends recertification cycles) is equally genuine and documented in the training personnel and operator perspectives. The tangled_rope classification holds because both mechanisms are irreducibly present — neither can be eliminated without destroying the other. A snare classification would ignore the real protective value; a rope classification would ignore the real extraction. The constraint illustrates that tangled_rope is the appropriate classification for mechanisms that provide genuine coordination while capturing asymmetric extraction from identifiable victims.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulation_fidelity_threshold,
    'What level of simulation fidelity (physiological stress markers, financial consequence, mortality salience, time pressure magnitude) would constitute adequate substitute for real catastrophe in human skill retention?',
    'Comparative neuroscience study: activation patterns in amygdala, insula, dorsolateral prefrontal cortex during high-fidelity simulation vs. actual high-stakes incident response; longitudinal competence retention curves for operators in catastrophe-exposed vs. catastrophe-free high-fidelity training regimes',
    'If fidelity threshold exists and is achievable: constraint reclassifies from tangled_rope toward rope (simulation proves adequate, extraction mechanism dissolves). If no threshold exists: constraint remains tangled_rope (genuine coordination function + irreducible extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_fidelity_threshold, empirical, 'Whether simulation fidelity can substitute for real catastrophe').

omega_variable(
    competence_decay_timeline_under_safety,
    'In organizations with zero major incidents for 20+ years, what is the empirical decay rate for response competence, and does it differ between simulation-trained and catastrophe-exposed cohorts?',
    'Longitudinal incident analysis: comparison of response quality/speed in first-incident-after-long-period across populations (e.g., airline incident response in carriers with long safety records vs. carriers with incident-in-past-5-years); measurement of response quality gaps vs. training-only cohorts in high-fidelity simulators',
    'If decay is minimal or undetectable: doctrine becomes piton (theater atop unnecessary machinery). If decay is substantial: doctrine validates as tangled_rope with genuine coordination function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(competence_decay_timeline_under_safety, empirical, 'Competence decay rate in long-safe-period organizations').

omega_variable(
    near_miss_as_proxy_mechanism,
    'Do well-investigated near-misses with realistic failure-mode reconstruction provide learning equivalent to actual catastrophes?',
    'Incident investigation corpus analysis: comparison of organizational learning outcomes (change implementation, systemic error prevention, subsequent safety performance) following near-miss investigation vs. following actual incident; controlled study of team response patterns when trained on near-miss data vs. actual-incident data vs. pure simulation',
    'If near-miss investigation is equivalent: constraint reclassifies to rope (near-misses provide irreducible exercise without requiring catastrophe). If gap remains: constraint validates tangled_rope model.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(near_miss_as_proxy_mechanism, empirical, 'Whether near-miss investigation substitutes for actual catastrophe').

omega_variable(
    reading_vs_sibling_kernel_ambiguity,
    'Which reading of the competence_exercise_requirement kernel most accurately describes how human competence in safety-critical domains actually works: catastrophe_as_necessary_anchor (this reading), simulation_as_adequate_exercise (sibling), or hybrid_dependency (sibling)?',
    'This is the fundamental omega that routes through Rule 2 of the committer frame. Empirical resolution through longitudinal competence studies (see above omegas). Conceptual resolution through philosophical analysis of what ''competence'' means — proceduralist (embodied, context-dependent) vs. rationalist (knowledge-based, transferable).',
    'If this reading''s axiom (catastrophe_irreducible_for_embodied_competence) proves overridden by evidence: the constraint''s classification is invalid, and a sibling reading describes the phenomenon more accurately. If this reading''s axiom proves holdable: constraint validates as tangled_rope and the sibling readings are coexisting alternative frameworks.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_vs_sibling_kernel_ambiguity, conceptual, 'Which kernel reading describes competence mechanism accurately').

omega_variable(
    regulatory_capture_via_doctrine,
    'Do regulatory agencies use the catastrophe-as-anchor doctrine to justify expansion of their authority and maintenance of expensive recertification regimes, even when evidence suggests these do not prevent catastrophe?',
    'Analysis of regulatory rule changes correlated with major incidents; comparison of recertification costs and frequency across agencies and jurisdictions with different doctrine adoption; interview data from safety personnel regarding perception of doctrine necessity',
    'If doctrine is used as cover for authority expansion: extraction mechanism is confirmed, tangled_rope classification validated. If doctrine reflects genuine regulatory necessity: extraction component is overstated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_via_doctrine, empirical, 'Regulatory capture via catastrophe-doctrine authority expansion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_as_necessary_anchor, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(catanc_tr_t0, catastrophe_as_necessary_anchor, theater_ratio, 0, 0.35).
narrative_ontology:measurement(catanc_tr_t5, catastrophe_as_necessary_anchor, theater_ratio, 5, 0.42).
narrative_ontology:measurement(catanc_tr_t10, catastrophe_as_necessary_anchor, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(catanc_be_t0, catastrophe_as_necessary_anchor, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(catanc_be_t5, catastrophe_as_necessary_anchor, base_extractiveness, 5, 0.53).
narrative_ontology:measurement(catanc_be_t10, catastrophe_as_necessary_anchor, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_as_necessary_anchor, enforcement_mechanism).
narrative_ontology:affects_constraint(catastrophe_as_necessary_anchor, simulation_as_adequate_exercise).
narrative_ontology:affects_constraint(catastrophe_as_necessary_anchor, hybrid_dependency).

% DUAL FORMULATION NOTE:
% The catastrophe_as_necessary_anchor constraint is ONE reading of the competence_exercise_requirement kernel. The sibling constraints (simulation_as_adequate_exercise, hybrid_dependency) are separate stories with different ε values, different beneficiary/victim structures, and different classification paths. They are linked as kernel siblings, not as alternative observables of a single constraint. Each story is ε-invariant within its own reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
