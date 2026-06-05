% ============================================================================
% CONSTRAINT STORY: competence_exercise_requirement__simulation_as_adequate_exercise
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_exercise_requirement__simulation_as_adequate_exercise, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: competence_exercise_requirement__simulation_as_adequate_exercise
 *   human_readable: Simulation with Debriefing as Adequate Competence Exercise
 *   domain: safety_engineering/organizational_learning/high_reliability_organizations
 *
 * SUMMARY:
 *   The regulatory and organizational framework that 'simulation with high
 *   fidelity and debriefing constitutes adequate exercise of the competence
 *   kernel' in aviation safety is one reading of a contested kernel. This
 *   kernel — 'what constitutes competence maintenance?' — has at least three
 *   competing readings: (1) simulation as adequate, (2) catastrophe as
 *   necessary anchor, (3) hybrid dependency (simulation + real-world
 *   anchoring). This constraint story instantiates reading (1). The core
 *   claim is that pilots can maintain the cognitive and decision-making
 *   competence required for high-reliability operations through scheduled
 *   high-fidelity simulation cycles combined with structured debriefing,
 *   without requiring periodic real-world emergency response or actual
 *   aircraft operations. Catastrophe-free decades of aviation safety under
 *   simulator-based training are cited as validation. The constraint exhibits
 *   Tangled Rope structure: it coordinates a genuine collective action
 *   problem (how to maintain competence at scale without constant operational
 *   disruption) while simultaneously extracting from pilots by potentially
 *   underpracticing the irreducible stress response of actual jeopardy.
 *   Theater ratio (0.68) reflects that much of the validation argument relies
 *   on the absence of catastrophe (which is compatible with multiple causal
 *   theories) rather than on positive evidence that simulator stress and real
 *   stress are cognitively equivalent.
 *
 * KEY AGENTS:
 *   - Pilot Population: Primary target (powerless/trapped) — bound by regulatory mandate to accept simulation as adequate; cannot demand real-world anchoring without career consequences; competence kernel exercise may be systematically underpracticed
 *   - Training Infrastructure Operators: Primary beneficiary (institutional/arbitrage) — benefits from simulator-based training efficiency, cost structure, and measurable compliance metrics
 *   - Regulatory Bodies: Secondary beneficiary (institutional/constrained) — benefits from quantifiable compliance, but constrained by uncertainty about whether reading is correct
 *   - Simulator Technology Developers: Secondary beneficiary (organized/constrained) — benefit from market expansion if simulation adequacy is validated; constrained by need to raise fidelity to maintain competence-relevant validity
 *   - Safety Authority (Regional): Secondary actor (organized/constrained) — organized stakeholder experiencing mixed effects; has some agency but cannot exit the core tension
 *   - Analytical Observer: Civilizational context (analytical/analytical) — sees irreducible neuroscience gap between simulated and real jeopardy stress; risks naturalizing a contested reading as immutable law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_requirement__simulation_as_adequate_exercise, 0.38).
domain_priors:suppression_score(competence_exercise_requirement__simulation_as_adequate_exercise, 0.52).
domain_priors:theater_ratio(competence_exercise_requirement__simulation_as_adequate_exercise, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__simulation_as_adequate_exercise, extractiveness, 0.38).
narrative_ontology:constraint_metric(competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_requirement__simulation_as_adequate_exercise, tangled_rope).
narrative_ontology:human_readable(competence_exercise_requirement__simulation_as_adequate_exercise, "Simulation with Debriefing as Adequate Competence Exercise").
narrative_ontology:topic_domain(competence_exercise_requirement__simulation_as_adequate_exercise, "safety_engineering/organizational_learning/high_reliability_organizations").

domain_priors:requires_active_enforcement(competence_exercise_requirement__simulation_as_adequate_exercise).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_requirement__simulation_as_adequate_exercise, '53e99b50-c811-4ca9-a322-089215d4c688').
narrative_ontology:cs_kernel_codification('53e99b50-c811-4ca9-a322-089215d4c688', formalized).
narrative_ontology:cs_authority_grounding('53e99b50-c811-4ca9-a322-089215d4c688', extraction).
narrative_ontology:cs_interpretation_layer_present('53e99b50-c811-4ca9-a322-089215d4c688').
narrative_ontology:cs_reading_relation('53e99b50-c811-4ca9-a322-089215d4c688', competence_exercise_requirement__catastrophe_as_necessary_anchor, coexists_with).
narrative_ontology:cs_reading_relation('53e99b50-c811-4ca9-a322-089215d4c688', competence_exercise_requirement__hybrid_dependency, influences).
narrative_ontology:cs_axiom('53e99b50-c811-4ca9-a322-089215d4c688', foundational, simulation_stress_equivalence).
narrative_ontology:cs_axiom_status(simulation_stress_equivalence, holdable).
narrative_ontology:cs_axiom_grounding('53e99b50-c811-4ca9-a322-089215d4c688', simulation_stress_equivalence, empirically_contingent).
narrative_ontology:cs_axiom('53e99b50-c811-4ca9-a322-089215d4c688', secondary, regulatory_compliance_sufficiency).
narrative_ontology:cs_axiom_status(regulatory_compliance_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('53e99b50-c811-4ca9-a322-089215d4c688', regulatory_compliance_sufficiency, conventional).
narrative_ontology:cs_reference_frame('53e99b50-c811-4ca9-a322-089215d4c688', regulated_simulator_adequacy).
narrative_ontology:cs_drift_state('53e99b50-c811-4ca9-a322-089215d4c688', contemporary_wideband_operations, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('53e99b50-c811-4ca9-a322-089215d4c688', '').
narrative_ontology:cs_kernel_id(competence_exercise_requirement__simulation_as_adequate_exercise, competence_exercise_requirement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__simulation_as_adequate_exercise, training_infrastructure_operators).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__simulation_as_adequate_exercise, regulatory_compliance_bodies).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__simulation_as_adequate_exercise, airline_management).
narrative_ontology:constraint_victim(competence_exercise_requirement__simulation_as_adequate_exercise, pilot_competence_integrity).
narrative_ontology:constraint_victim(competence_exercise_requirement__simulation_as_adequate_exercise, operational_readiness_under_chaos).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PILOT COMPETENCE UNDER EXTREME STRESS (SNARE) — Pilots are trapped by regulatory mandate to trust that simulation constitutes adequate exercise. They cannot exit the constraint or demand real-world anchoring without career consequences. The competence kernel — the actual capacity to act under chaos and irreducible uncertainty — may be systematically underpracticed because simulation, however high-fidelity, cannot replicate the irreducible stress of real jeopardy. Trapped agent bearing full extraction cost: competence may degrade while appearing maintained.
constraint_indexing:constraint_classification(competence_exercise_requirement__simulation_as_adequate_exercise, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: REGIONAL SAFETY AUTHORITY (TANGLED ROPE) — Organized agent experiencing mixed effects. Benefits: simulation-based training reduces accident liability, is cost-efficient, and provides measurable metrics for compliance. Constrained by: inability to mandate real-world anchoring without disrupting operations, and uncertainty about whether catastrophe-free decades validate the reading or merely represent luck prior to a phase change. The authority has some agency (can audit simulation quality, can mandate specific scenarios) but cannot exit the tension between efficiency and competence integrity.
constraint_indexing:constraint_classification(competence_exercise_requirement__simulation_as_adequate_exercise, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: TRAINING INFRASTRUCTURE AND REGULATORY REGIME (ROPE) — Institutional beneficiary experiencing the constraint as pure coordination. High-fidelity simulation with standardized debriefing protocols is a coordination mechanism: it solves the problem of how to maintain pilot competence at scale without constant real-world disruption. The regime benefits from the constraint because it provides quantifiable compliance evidence, cost efficiency, and accident prevention. Net extraction runs toward this institutional actor. The constraint coordinates a collective action problem (maintaining competence across global aviation) with minimal extractive overhead from the regime's perspective.
constraint_indexing:constraint_classification(competence_exercise_requirement__simulation_as_adequate_exercise, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SIMULATION TECHNOLOGY DEVELOPMENT COALITION (SCAFFOLD) — Organized agents (simulator manufacturers, software developers, training standards bodies, research institutions in human factors) see high-fidelity simulation as a temporary scaffold with an implicit sunset. The coalition's work is to raise fidelity progressively: full-motion physics, real-time weather integration, AI-driven adaptive scenarios, biometric feedback, virtual-reality immersion. The sunset condition is cognitive — once simulator fidelity reaches a threshold of neural realism (neural pattern matching, autonomic stress response, proprioceptive coupling), the constraint's core tension dissolves. Simulation + debriefing becomes genuinely adequate because the gap between simulation stress and real-world stress shrinks below the competence-relevant threshold. Theater ratio in this view is moderately high (current simulators are partly performative) but decreasing with each generation.
constraint_indexing:constraint_classification(competence_exercise_requirement__simulation_as_adequate_exercise, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: INSTITUTIONAL MEMORY OF SIMULATION ADEQUACY (PITON) — The regulatory and organizational narrative that 'simulation with debriefing maintains competence' persists through institutional inertia despite mounting evidence that competence-under-jeopardy (the irreducible stress state) is not practiced. The narrative is maintained because: (1) catastrophe-free decades appear to validate it, (2) real-world anchoring is disruptive and expensive, (3) simulator manufacturers have market incentive to claim adequacy, (4) regulatory bodies have institutional commitment to the reading. The piton emerges from theater_ratio = 0.68 — much of the validation argument is performative (catastrophe absence is treated as proof of adequacy rather than as data point requiring causal theory). The theater has increased as the gap between simulator capability and extreme-stress realism has widened but become invisible due to liability concerns.
constraint_indexing:constraint_classification(competence_exercise_requirement__simulation_as_adequate_exercise, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / COGNITIVE SCIENCE VIEW (MOUNTAIN) — From the civilizational and universal perspective, there is an irreducible gap between simulated and real stress states: the amygdala's response to actual jeopardy (real-world accident scenario with actual consequences for real people) cannot be perfectly replicated by simulated jeopardy because the anterior insula integrates true-consequence information. This constraint appears to be a natural law of neurobiology — competence-under-irreducible-stress requires actual irreducible-stress practice. The analytical observer risks classifying this as mountain, thereby naturalizing what is actually a reading of a contested kernel. The false summit detection system will flag the beneficiary declarations and reveal the naturalizing move.
constraint_indexing:constraint_classification(competence_exercise_requirement__simulation_as_adequate_exercise, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_exercise_requirement__simulation_as_adequate_exercise_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(competence_exercise_requirement__simulation_as_adequate_exercise, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(competence_exercise_requirement__simulation_as_adequate_exercise, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(competence_exercise_requirement__simulation_as_adequate_exercise, TR),
    TR >= 0.70.

:- end_tests(competence_exercise_requirement__simulation_as_adequate_exercise_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint extracts from pilots through mandate-enforced practice of simulation-only competence maintenance, which may leave them underprepared for irreducible-stress decision-making. However, extractiveness is not severe because: (1) simulation does develop decision-making patterns that partially transfer to real contexts, (2) debriefing provides cognitive reflection, (3) catastrophe-free decades suggest the extraction is not catastrophic (though this is compatible with latent incompetence). The extraction increases over time (0.28 → 0.38) as the gap between simulator capability and extreme-stress realism widens and institutional commitment to simulation-only training deepens. Suppression (0.52): Moderate-high. Pilots are suppressed by regulatory mandate and career risk; they cannot demand real-world anchoring without jeopardizing credentials. The suppression mechanism is institutional (regulatory) rather than material (physical barrier) — pilots are legally and professionally locked in. Real-world stress practice is suppressed as disruptive and expensive. Alternative readings (catastrophe_as_necessary_anchor, hybrid_dependency) are suppressed through regulatory framing and institutional inertia. Theater ratio (0.68): Moderately high. The validation argument relies heavily on catastrophe absence — the lack of pilot-error accidents attributed to simulator-trained pilots is treated as proof that simulation is adequate. But catastrophe absence is compatible with: (1) actual adequacy, (2) luck prior to a phase change, (3) latent incompetence not yet exposed by sufficient stress. The theater increases over time (0.55 → 0.68) as the evidence base remains stable (no catastrophe) but the simulator-real-world gap widens, requiring stronger narrative claims to maintain adequacy belief.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows maximal perspectival divergence. The regulatory beneficiary (institutional/arbitrage) sees pure coordination (Rope) — simulation solves the efficiency problem of maintaining competence at scale. The pilot as trapped agent (powerless/trapped) sees extraction (Snare) — they are mandated to practice a competence kernel exercise that may be systematically inadequate for irreducible-stress scenarios. The safety authority (organized/constrained) experiences mixed effects (Tangled Rope) — genuine coordination function alongside extraction risk. The simulator developers (organized/constrained) see a temporary scaffold — fidelity is progressively rising toward a sunset where the stress-reality gap dissolves and adequacy becomes genuine. The institutional narrative (institutional/arbitrage) sees a degraded ritual (Piton) — simulation adequacy validation persists through inertia and liability concerns despite the gap widening. The analytical observer (analytical/analytical) sees a natural law (Mountain) — an irreducible neuroscience gap between simulated and real jeopardy stress — but the false summit detector will reveal this as naturalization of a contested reading. The perspectival gaps reflect genuine structural differences in how the constraint operates for different agents, not mere opinion differences.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from the agent's structural position relative to the constraint. Pilots as trapped agents with no exit (trapped) and as victims of potential underpractice (competence kernel victim) derive high d (≈0.90-0.95) → high f(d) ≈ 1.28-1.42 → high experienced chi. Regulatory beneficiaries with arbitrage options and beneficiary status derive low d (≈0.15-0.20) → low f(d) ≈ -0.01 to 0.02 → negative or near-zero experienced chi (they experience the constraint as enabling rather than extractive). Safety authorities (organized power, constrained exit, mixed victim/beneficiary) derive moderate d (≈0.50-0.55) → moderate f(d) ≈ 0.65-0.75. The derived directionality values reflect that extraction flows from pilots toward beneficiary institutions, with safety authorities occupying an intermediate position.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy on this constraint is not 'is simulation adequate?' but 'which reading of the competence kernel is correct?' The constraint's tangled_rope classification at the baseline (analytical/moderate/constrained context) resolves the mandatrophy by accepting the Tangled Rope gate: there is genuine coordination function (training at scale) alongside extraction risk (underpractice of irreducible-stress competence). The constraint does NOT resolve to either pure coordination (Rope) or pure extraction (Snare) because both functions are structurally real. The extraction risk is not catastrophic (hence not snare-level) because simulation does provide partial competence transfer; the coordination is not pure because the competence kernel gap creates extraction. The Tangled Rope classification is stable across measurement points as the theater_ratio and base_extractiveness both increase moderately, indicating the constraint is not degrading toward either pure extraction or collapsing toward pure coordination — it is remaining a hybrid with both functions present.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    stress_response_substitutability,
    'Can simulated high-fidelity stress, combined with cognitive debriefing, substitute for the irreducible autonomic and cognitive response to actual jeopardy?',
    'Neuroscience evidence: fMRI and autonomic response comparison between simulator training and real emergency scenarios; longitudinal tracking of pilots trained only in simulation vs. pilots with real-world anchoring; post-incident analysis of whether simulator-only pilots show different decision patterns under actual stress than hybrid-trained pilots',
    'If substitutable (simulator stress ≈ real stress at competence-relevant cognitive/autonomic level): reading is validated; competence_exercise_requirement shifts to mountain (natural law of modern training). If non-substitutable: reading is falsified; constraint shifts toward catastrophe_as_necessary_anchor or hybrid_dependency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stress_response_substitutability, empirical, 'Whether simulated stress provides adequate cognitive and autonomic training substitution').

omega_variable(
    catastrophe_free_decades_interpretation,
    'Does the absence of catastrophe attributable to simulator-trained pilots constitute evidence that the reading is correct, or is it compatible with a latent incompetence that has not yet been exposed?',
    'Causal analysis: near-miss data, simulator-based training cohort vs. hybrid-trained cohort under equivalent stressor conditions; time-series analysis of accident rates to identify whether accident frequency is stable, declining, or entering a regime transition; system-level stress test (high traffic volume, cascading failures, environmental extremes) on simulator-only trained population',
    'If absence is evidence of adequacy: reading validated. If absence is compatible with latent incompetence: reading is uncertain; catastrophe_as_necessary_anchor and hybrid_dependency remain live alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(catastrophe_free_decades_interpretation, empirical, 'Whether catastrophe-free decades constitute proof of simulation adequacy or mask latent incompetence').

omega_variable(
    regulatory_capture_in_adequacy_standard,
    'To what degree does the regulatory definition of ''adequate competence exercise'' reflect evidence-based competence maintenance versus regulatory convenience and simulator-industry incentives?',
    'Historical analysis of how adequacy standards evolved; comparison of standards across jurisdictions with different regulatory capture dynamics; cost-benefit analysis of simulator-only vs. hybrid training regimes; stakeholder interest mapping (simulator manufacturers, training operators, insurance, regulatory bodies, pilot unions)',
    'If high regulatory capture: the extracted beneficiary is captured regulator + simulator industry; constraint is better modeled as a snare for pilots. If low capture: the constraint genuinely represents a coordination solution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_capture_in_adequacy_standard, empirical, 'Degree of regulatory capture in defining simulation adequacy').

omega_variable(
    kernel_reading_contest_structure,
    'Is the reading ''simulation as adequate exercise'' logically foreclosed by the sibling reading ''catastrophe as necessary anchor,'' or do these readings coexist as live alternatives held by different parties?',
    'Logical analysis: if one core premise directly contradicts the other within a single coherent framework, relation is forecloses. If both can remain live positions within different institutional or epistemic frameworks, relation is coexists_with. If one constrains the conditions under which the other can operate, relation is influences.',
    'If forecloses: only one reading can be correct within any coherent safety theory. If coexists_with: both readings persist as institutional commitments despite logical tension. If influences: one reading creates pressure on the other without eliminating it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_structure, conceptual, 'Logical structure of the kernel reading contest').

omega_variable(
    competence_kernel_definition_ambiguity,
    'What does ''exercise of the competence kernel'' mean? Is it: (A) demonstrated ability to perform in simulated high-stress scenarios; (B) demonstrated ability to perform in actual high-stress scenarios; (C) internalized decision-making patterns that generalize across both simulated and real contexts; (D) capacity to function under irreducible uncertainty when actual jeopardy is at stake?',
    'Conceptual analysis: unpacking the competence kernel definition used in regulatory guidance and training standards; examination of which definition each stakeholder group implicitly uses; pilot and instructor interviews about what they understand ''competence'' to mean in context; video analysis of actual pilot decision-making in real emergencies vs. simulator performance',
    'If (A) or (C): reading is likely adequate (simulation measures these). If (B) or (D): reading requires real-world anchoring; constraint shifts toward hybrid_dependency.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(competence_kernel_definition_ambiguity, conceptual, 'Definition of the competence kernel being exercised').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_requirement__simulation_as_adequate_exercise, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(compex_sim_tr_t0, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 0, 0.55).
narrative_ontology:measurement(compex_sim_tr_t10, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 10, 0.62).
narrative_ontology:measurement(compex_sim_tr_t20, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(compex_sim_be_t0, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(compex_sim_be_t10, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 10, 0.33).
narrative_ontology:measurement(compex_sim_be_t20, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 20, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(compex_sim_su_t0, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(compex_sim_su_t10, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(compex_sim_su_t20, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_requirement__simulation_as_adequate_exercise, enforcement_mechanism).
narrative_ontology:affects_constraint(competence_exercise_requirement__simulation_as_adequate_exercise, catastrophe_as_necessary_anchor).
narrative_ontology:affects_constraint(competence_exercise_requirement__simulation_as_adequate_exercise, hybrid_dependency).

% DUAL FORMULATION NOTE:
% The competence_exercise_requirement kernel has three structurally distinct readings, each with its own ε value and classification. This file instantiates simulation_as_adequate_exercise (ε=0.38, Tangled Rope). The sibling reading catastrophe_as_necessary_anchor has higher ε (likely ≈0.65, Snare) because it models competence maintenance as depending on actual catastrophic events. The hybrid reading has intermediate ε (likely ≈0.48, Tangled Rope) because it accepts simulation plus real-world anchoring. All three readings are linked via this network.affects_constraints field to indicate their interdependence on the shared kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
