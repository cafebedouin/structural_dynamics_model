% ============================================================================
% CONSTRAINT STORY: hybrid_decay_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-03-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hybrid_decay_reading, []).

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
 *   constraint_id: hybrid_decay_reading
 *   human_readable: Simulation-Judgment Hybrid Decay: Procedural Competence Retained, Crisis Decision-Making Capacity Lost
 *   domain: safety_engineering/organizational_learning/crisis_preparedness
 *
 * SUMMARY:
 *   This constraint captures a structural asymmetry in competence
 *   maintenance: simulation exercises preserve procedural competence
 *   (checklist execution, muscle memory, scenario pattern recognition) but
 *   systematically fail to maintain judgment capacity under genuine stakes
 *   (novel fault combinations, time pressure, irreversible decisions,
 *   fear/adrenaline). The kernel is the legitimacy claim that 'exercise
 *   maintains competence' — but competence has two structurally distinct
 *   components with different exercise requirements. Procedural competence is
 *   maintainable via repetition and scenario-drilling in simulation; judgment
 *   under stakes requires exposure to actual consequences, genuine
 *   uncertainty, and the psychological/physiological state of real
 *   decision-making. The hybrid-decay reading holds that this asymmetry is
 *   the constraint's core structural feature: the machinery that solves the
 *   procedural problem systematically creates a judgment-decay problem. This
 *   reading coexists with the simulation-sufficiency reading (which holds
 *   that simulation-procedural competence is sufficient for most scenarios)
 *   and the lived-catastrophe-necessity reading (which holds that judgment
 *   can only be maintained through exposure to actual high-stakes failures).
 *   The constraint is tangled because it coordinates real procedural safety
 *   (checklists work, they prevent errors) while extracting from judgment
 *   capacity (decay in non-procedural decision-making). This extraction is
 *   enforced through certification requirements and the regulatory fiction
 *   that simulation hours prove judgment competence. The theater_ratio
 *   increases over the interval as simulation complexity grows without
 *   closing the judgment gap — regulatory certification becomes increasingly
 *   performative.
 *
 * KEY AGENTS:
 *   - Budget-Constrained Operators (institutional/arbitrage): Airlines, utilities, nuclear plants. Net beneficiary — simulation enables cost-effective compliance. They experience the procedural half as adequate most of the time.
 *   - High-Consequence Decision-Makers (moderate/constrained): Plant operators, aircraft pilots, emergency coordinators. Structurally trapped — certification requirements lock them into simulation-based training; they retain procedures but lose judgment capacity.
 *   - Exposed Population (powerless/trapped): Communities downwind of power plants, passengers on commercial aircraft, cargo receivers in shipping lanes. Cannot exit consequence of judgment-decay cascade failures.
 *   - Safety Regulators (organized/constrained): FAA, NRC, IMO. Coordinated safety work (certification standards, simulator validation) but suppression of alternatives to simulation-based model. Benefit from cost-containment; constrained by mandate conflict.
 *   - Simulation Vendors (institutional/arbitrage): Training companies, simulator manufacturers. Direct beneficiaries — indefinite market for simulation improvements without disruption to the business model.
 *   - Certification Theater (institutional/arbitrage): The institutional ritual of licensing, checkrides, renewals. Maintains legitimacy by testing what can be tested (procedures) and ignoring judgment.
 *   - High-Reliability Organization Advocates (organized/constrained): Safety researchers, HRO practitioners, resilience engineers. Organized but constrained by adoption resistance. See the hybrid decay as solvable via evolved training (scaffold).
 *   - Analytical Observer (analytical/analytical): Global, generational perspective recognizing the kernel structure: two components, two maintenance requirements, one institutional solution that solves half.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hybrid_decay_reading, 0.62).
domain_priors:suppression_score(hybrid_decay_reading, 0.58).
domain_priors:theater_ratio(hybrid_decay_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hybrid_decay_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(hybrid_decay_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(hybrid_decay_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hybrid_decay_reading, tangled_rope).
narrative_ontology:human_readable(hybrid_decay_reading, "Simulation-Judgment Hybrid Decay: Procedural Competence Retained, Crisis Decision-Making Capacity Lost").
narrative_ontology:topic_domain(hybrid_decay_reading, "safety_engineering/organizational_learning/crisis_preparedness").

domain_priors:requires_active_enforcement(hybrid_decay_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hybrid_decay_reading, 'f3b54945-073b-40a0-8028-0b7d19d38386').
narrative_ontology:cs_kernel_codification('f3b54945-073b-40a0-8028-0b7d19d38386', fixed_text).
narrative_ontology:cs_authority_grounding('f3b54945-073b-40a0-8028-0b7d19d38386', extraction).
narrative_ontology:cs_interpretation_layer_present('f3b54945-073b-40a0-8028-0b7d19d38386').
narrative_ontology:cs_reading_relation('f3b54945-073b-40a0-8028-0b7d19d38386', hybrid_decay_reading__simulation_sufficiency_reading, coexists_with).
narrative_ontology:cs_reading_relation('f3b54945-073b-40a0-8028-0b7d19d38386', hybrid_decay_reading__lived_catastrophe_necessity_reading, coexists_with).
narrative_ontology:cs_axiom('f3b54945-073b-40a0-8028-0b7d19d38386', foundational, competence_has_two_maintainable_components).
narrative_ontology:cs_axiom_status(competence_has_two_maintainable_components, holdable).
narrative_ontology:cs_axiom_grounding('f3b54945-073b-40a0-8028-0b7d19d38386', competence_has_two_maintainable_components, empirically_contingent).
narrative_ontology:cs_axiom('f3b54945-073b-40a0-8028-0b7d19d38386', foundational, institutional_suppression_of_judgment_decay).
narrative_ontology:cs_axiom_status(institutional_suppression_of_judgment_decay, holdable).
narrative_ontology:cs_axiom_grounding('f3b54945-073b-40a0-8028-0b7d19d38386', institutional_suppression_of_judgment_decay, empirically_contingent).
narrative_ontology:cs_reference_frame('f3b54945-073b-40a0-8028-0b7d19d38386', competence_proven_by_exercise).
narrative_ontology:cs_drift_state('f3b54945-073b-40a0-8028-0b7d19d38386', contemporary_cascade_failures, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f3b54945-073b-40a0-8028-0b7d19d38386', '2026-03-15T14:32:00Z').
narrative_ontology:cs_kernel_id(hybrid_decay_reading, exercise_as_competence_maintenance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hybrid_decay_reading, budget_constrained_operators).
narrative_ontology:constraint_beneficiary(hybrid_decay_reading, simulation_vendors).
narrative_ontology:constraint_victim(hybrid_decay_reading, high_consequence_decision_makers).
narrative_ontology:constraint_victim(hybrid_decay_reading, populations_exposed_to_cascade_failures).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXPOSED POPULATION (SNARE) — Trapped by geography and timing in areas where judgment-decay manifests as cascade failure (nuclear plant wrong-decision under novel fault, maritime collision in fog, aircraft system interaction failure). Cannot exit the consequence. Simulation competence means nothing when decision-makers have lost capacity to improvise under stakes. Pure extraction: catastrophic harm.
constraint_indexing:constraint_classification(hybrid_decay_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: HIGH-CONSEQUENCE DECISION-MAKERS (SNARE) — Plant operators, flight crews, emergency coordinators constrained by certification requirements, career structures, and the claim that simulation suffices. They retain procedural competence (follow checklist, execute trained sequence) but experience degraded judgment capacity under real-world novelty. Partially trapped by their own credentialing: simulation certifies them as competent, but the certification masks judgment decay. Extraction occurs as harm cascades from decisions they make under constraints they cannot articulate.
constraint_indexing:constraint_classification(hybrid_decay_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SAFETY REGULATORS (TANGLED ROPE) — Organized but constrained by competing mandates: certify operators (simulation-based), enable industry cost-control, and prevent catastrophe. They coordinate real safety work (simulator design, checklist protocols) while accepting the hybrid decay as built-in risk. Not pure extraction (they genuinely invest in safety) but also not pure coordination (they benefit from the simulation cost-containment model that prevents them from mandating the more expensive lived-scenario training alternatives). High suppression: regulators control who gets certified; they suppress alternatives to simulation-based competence maintenance.
constraint_indexing:constraint_classification(hybrid_decay_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: BUDGET-CONSTRAINED OPERATORS (ROPE) — Airlines, power utilities, maritime companies experience simulation as genuine coordination solution: procedural competence is maintained cost-effectively, checklists are executed reliably, most scenarios (80%+) go according to trained pattern. They have arbitrage: can choose simulation level, outsource training. Net beneficiary during normal operations. The extraction is invisible because it is probabilistic: harm occurs only in the decayed-judgment tail events.
constraint_indexing:constraint_classification(hybrid_decay_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: SIMULATION VENDORS (ROPE) — Benefit directly from the hybrid-decay reading: a framework that validates simulation-based competence as sufficient (procedural yes, judgment no) creates indefinite market demand. They coordinate real training delivery with cost-effective apparatus; they capture the coordination benefit while the judgment decay is paid by others. Arbitrage: can pivot to different safety domains. Pure beneficiary with low experienced extraction from their own position.
constraint_indexing:constraint_classification(hybrid_decay_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: CERTIFICATION THEATER (PITON) — The certification ritual (simulator hours, checklist mastery, signed-off competence) persists as the legitimacy signal even though it certifies only the procedural half. The theater has high social force: families trust 'FAA-certified pilots,' communities trust 'NRC-licensed operators.' The theater masks the hybrid decay because it validates what can be tested (procedures) and ignores what cannot (judgment under novel stakes). High theater_ratio: certification is mostly performance of competence, not measurement of judgment capacity. The function (procedural maintenance) is real but atrophied in scope; the theater maintains institutional legitimacy.
constraint_indexing:constraint_classification(hybrid_decay_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: HIGH-RELIABILITY ORG REFORM ADVOCATES (SCAFFOLD) — Organized safety researchers (HRO practitioners, crew resource management evolution, resilience engineering) see the hybrid decay as a solvable problem with a sunset clause: training methodology can evolve to include judgment-under-stakes in simulation (scenario-branching complexity, managed genuine uncertainty, post-incident psychological integration). The constraint is temporary — organizational practices can shift from 'simulation alone' to 'simulation + judgment practice + lived drills.' Constrained because adoption faces budget and cultural resistance, but the exit path is structural and visible.
constraint_indexing:constraint_classification(hybrid_decay_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (TANGLED ROPE) — From the generational/global analytical seat, the hybrid-decay reading is the core structural tension: simulation genuinely maintains procedural competence (coordination function is real) but systematically fails to maintain judgment capacity under novel stakes (extraction embedded in the competence claim itself). The constraint is actively enforced through certification requirements, professional licensure, and the normative claim that simulation suffices. Neither pure Rope (judgment decay is substantial) nor pure Snare (procedures genuinely work most of the time) — it is fundamentally tangled: the machinery that solves one problem creates the other. The engine will measure effective extraction as substantial when judgment decay cascades into harm.
constraint_indexing:constraint_classification(hybrid_decay_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hybrid_decay_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hybrid_decay_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hybrid_decay_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(hybrid_decay_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(hybrid_decay_reading, TR),
    TR >= 0.70.

:- end_tests(hybrid_decay_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62, rising from 0.35): Moderate-high, rising over the interval as judgment decay accumulates without detection or correction. The initial value reflects that most routine operations execute procedures flawlessly — extraction is probabilistic and tail-event driven. As years of simulation-only training accumulate, operators' judgment capacity degrades further from what it was in earlier cohorts (who had more lived-event exposure); the extractiveness rises because undetected judgment decay increases cascade-failure probability. Suppression (0.58): Moderate-high. Certification requirements suppress alternative training methods (lived scenario, high-stakes tabletop, post-incident psychological integration). Regulators suppress acknowledgment of the judgment decay because it would trigger costly mandate restructuring. Operators suppress their own awareness of judgment erosion (they feel procedurally confident and mistake procedural confidence for overall competence). Theater_ratio (0.68, rising): High and rising. Certification testing measures procedural checklist execution and scenario-branch execution — these are performative of competence rather than tests of judgment. As simulation technology increases in scenario complexity, the performance of testing becomes more elaborate without closing the judgment gap. The theater rises because the ritual becomes more convincing without becoming more effective.
 *
 * PERSPECTIVAL GAP:
 *   The gap between beneficiary and victim perspectives is maximal. Budget-constrained operators and simulation vendors experience the constraint as coordination (rope from their seats) — simulation solves real procedural problems cost-effectively. High-consequence decision-makers and exposed populations experience it as extraction (snare from their seats) — procedures work fine but judgment capacity is eroding and they bear the tail-event costs. Safety regulators occupy the middle ground (tangled_rope) — they genuinely coordinate safety (simulation is better than nothing) while accepting the hybrid decay as built-in risk they cannot afford to eliminate. The analytical observer sees the structural asymmetry that justifies the hybrid_decay reading: the kernel's two components require different maintenance (procedural = repetition, judgment = stakes exposure), but the institutional solution (simulation) addresses only one component.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (budget-constrained operators, simulation vendors) have arbitrage-level exit options and institutional power — they can choose training intensity, outsource to vendors, switch domains. The engine derives d from their beneficiary status + arbitrage exit → low d → low/negative effective extraction. Victims (high-consequence decision-makers, exposed population) have trapped/constrained exit options and low institutional power — they cannot opt out of the certification regime, cannot avoid exposure to the stack of judgment-decayed operators, cannot refuse the probability distribution of cascade failures. The engine derives d from their victim status + trapped/constrained exit → high d → high effective extraction. Regulators occupy the middle: organized power, constrained (not arbitrage) exit, mixed beneficiary/victim structure. The analysis does not compute chi directly — it is an engine operation. What the commentary specifies is the structural data (beneficiary/victim, power, exit) from which chi is computed.
 *
 * MANDATROPHY ANALYSIS:
 *   The original mandate of the exercise_as_competence_maintenance kernel was to enable cost-effective competence maintenance for high-consequence operators. Simulation was introduced as a solution that could maintain procedural competence (the full mandate as then understood) without the expense of lived-event training. The mandate has not changed, but its scope has implicitly narrowed: the regime now maintains procedural competence only while claiming to maintain overall competence. The judgment component was always part of the mandate (competent decision-makers under stakes), but it has become invisible in institutional practice. Mandatrophy is not fully resolved: the regime persists (certification remains simulation-based), but its legitimacy rests on a false claim (that simulation maintains all competence). The hybrid_decay reading diagnoses this mandatrophy without resolving it — it describes the contradiction between the mandate (maintain judgment competence) and the mechanism (simulation, which does not maintain judgment). Resolution would require either (a) narrowing the mandate to 'procedural competence only' (honest reframing), (b) extending the mechanism to include judgment training (costly, disruptive), or (c) accepting the contradiction (status quo). The reading takes no position on resolution — it describes the structural fact that the current regime is mandatrophic.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judgment_decay_irreversibility,
    'Is judgment capacity decay from non-lived-stakes simulation irreversible, or can it be recovered through alternative training methods if detected?',
    'Longitudinal study of operators trained purely by simulation vs. those with lived-event exposure; post-incident psychological integration and decision-making capability restoration trials. Track whether judgment capacity can be rebooted by brief high-stakes exposure or whether atrophy is path-dependent.',
    'If irreversible: the tangled_rope reading stands — simulation commits operators to permanent judgment decay regardless of later intervention. If recoverable: the hybrid_decay reading shifts to scaffold (the problem is solvable). The victim set also changes: if reversible, harm is from failure-to-detect rather than intrinsic decay.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judgment_decay_irreversibility, empirical, 'Whether judgment capacity decay from simulation-only training is reversible').

omega_variable(
    kernel_reading_contestation,
    'Is this the correct reading of the exercise_as_competence_maintenance kernel, or does the simulation_sufficiency_reading better capture the structural reality?',
    'Empirical: incident data comparing cascade failures in judgment vs. failures in procedure execution. Conceptual: whether the kernel''s core claim is that ''exercise maintains competence'' (sufficiency reading) or ''exercise maintains some but not all competence components'' (hybrid_decay reading). The readings coexist until incident data determines which framing dominates the evidence base.',
    'If simulation_sufficiency reading: the kernel is well-defended; the hybrid_decay reading becomes a false alarm (empirical overestimate of judgment decay). If hybrid_decay reading: the kernel''s legitimacy requires acknowledging the two-component structure and the decay asymmetry; this opens the mandate for judgment-training reform.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contestation, empirical, 'Which reading of the exercise kernel better captures structural reality: sufficiency or hybrid decay').

omega_variable(
    naturalization_of_judgment_decay,
    'Is judgment decay under stakes an intrinsic feature of human cognition (neurological, irreducible) or a contingent feature of current simulation design (remediable)?',
    'Cognitive science: fMRI, eye-tracking, decision latency data comparing simulated vs. lived-stakes decision-making. Engineering: simulation-design experiments testing whether scenario complexity, psychological fidelity, managed uncertainty, and post-failure consequences can close the judgment gap.',
    'If intrinsic: the reading stands as diagnosis of an unchangeable constraint on human performance. If contingent: the reading reclassifies to scaffold (solvable via better training design). The beneficiary set also changes: if intrinsic, there is no solution, and regulators become permanent acceptors of tail-risk. If contingent, regulators shift to reform advocates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalization_of_judgment_decay, empirical, 'Whether judgment decay under stakes is intrinsic to human cognition or contingent on simulation design').

omega_variable(
    two_component_kernel_structure,
    'Does the exercise_as_competence_maintenance kernel genuinely have two structurally distinct components (procedural and judgment), or is judgment decomposable into more granular procedures?',
    'Cognitive task analysis of expert decision-making under novel stakes: identify irreducible judgment processes that cannot be automated or proceduralized. Compare to simulation scenario coverage: what percentage of possible state-space branches are trained, and what percentage requires real-time novel-pattern recognition.',
    'If two distinct components: the hybrid_decay reading is the kernel''s structural reality — any commitment system grounding itself in the exercise-as-competence kernel must acknowledge the asymmetry. If judgment is proceduralizable: simulation design can close the gap, and the reading reclassifies to scaffold. The entire taxonomy of readings shifts if the kernel''s fundamental structure changes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(two_component_kernel_structure, empirical, 'Whether judgment is a distinct component from procedure or decomposable into finer procedures').

omega_variable(
    certification_authority_capture,
    'Do certification authorities (FAA, NRC, IMO) acknowledge the two-component structure and its decay asymmetry, or do they suppress this knowledge to maintain the fiction that simulation-based certification proves judgment competence?',
    'Documentary analysis: internal regulatory guidance, incident investigation reports, training advisory circulars. Interviews with certification decision-makers: do they privately acknowledge the hybrid decay, and if so, what prevents them from disclosing it publicly? Track whether incidents attributed to ''human error'' are actually judgment-decay cascades misclassified as operator error.',
    'If acknowledged privately but suppressed publicly: the reading is structurally sound but socially enforced as taboo. The suppression itself becomes the mechanism sustaining the tangled_rope: regulators benefit from the fiction and suppress the knowledge that would trigger reform. If genuinely unknown: the reading represents a novel cognitive insight rather than an institutional cover-up.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(certification_authority_capture, empirical, 'Whether certification authorities privately acknowledge judgment decay but publicly suppress it').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hybrid_decay_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hybrid_decay_theater_t0, hybrid_decay_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(hybrid_decay_theater_t10, hybrid_decay_reading, theater_ratio, 10, 0.63).
narrative_ontology:measurement(hybrid_decay_theater_t20, hybrid_decay_reading, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(hybrid_decay_extract_t0, hybrid_decay_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(hybrid_decay_extract_t10, hybrid_decay_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(hybrid_decay_extract_t20, hybrid_decay_reading, base_extractiveness, 20, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(hybrid_decay_suppress_t0, hybrid_decay_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(hybrid_decay_suppress_t10, hybrid_decay_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(hybrid_decay_suppress_t20, hybrid_decay_reading, suppression_requirement, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hybrid_decay_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(hybrid_decay_reading, simulation_sufficiency_reading).
narrative_ontology:affects_constraint(hybrid_decay_reading, lived_catastrophe_necessity_reading).
narrative_ontology:affects_constraint(hybrid_decay_reading, certification_theater_maintenance).
narrative_ontology:affects_constraint(hybrid_decay_reading, judgment_under_novel_stakes).

% DUAL FORMULATION NOTE:
% The hybrid_decay_reading decomposes the unitary claim 'exercise maintains competence' into two structurally distinct constraints with different ε values: (1) procedural_competence_maintenance (ε ≈ 0.10, nearly pure coordination, low extraction) — simulation genuinely maintains checklists and procedural patterns; (2) judgment_competence_under_stakes (ε ≈ 0.85, nearly pure extraction) — judgment capacity decays without lived-stakes exposure, and certification suppresses awareness of this decay. The hybrid_decay_reading is the tangled phenomenon that fuses these two: a regime that solves one competence problem creates another. Constraint family: exercise_as_competence_maintenance (the kernel, no standalone story) branches into three readings (simulation_sufficiency, hybrid_decay, lived_catastrophe) and two downstream constraints reflecting the two components. This story links to both downstream constraints via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hybrid_decay_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
