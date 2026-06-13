% ============================================================================
% CONSTRAINT STORY: catastrophe_avoidance_retention__catastrophe_as_necessary_selector
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_avoidance_retention__catastrophe_as_necessary_selector, []).

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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: catastrophe_avoidance_retention__catastrophe_as_necessary_selector
 *   human_readable: Catastrophe as Necessary Competence Selector
 *   domain: safety/organizational_learning
 *
 * SUMMARY:
 *   Organizations operating safety-critical systems (aviation, nuclear power,
 *   chemical manufacturing, medical care) face a sustained organizational
 *   selection pressure: only actual catastrophes create sufficient
 *   organizational trauma, mortality salience, and crisis urgency to force
 *   competence maintenance, regulatory reform, and system redesign. Long
 *   peacetime periods enable competence decay—simulator use becomes sporadic,
 *   near-miss analysis atrophies, cross-functional learning withers—because
 *   there is no active governance pressure to maintain expensive, disruptive
 *   learning infrastructure when incidents are rare. The constraint operates
 *   as a snare because it extracts the cost of organizational learning
 *   (death, injury, trauma, career termination) from operators and affected
 *   populations while insulating leadership from accountability by framing
 *   catastrophe as unpredictable black-swan failure rather than predictable
 *   outcome of peacetime competence decay. This is ONE READING of a contested
 *   kernel: catastrophe_avoidance_retention. Two sibling readings exist
 *   (simulation_as_proxy_catastrophe, hybrid_near_miss_learning) representing
 *   competing framings of competence maintenance mechanisms. This reading
 *   asserts that ONLY real catastrophe provides sufficient selection
 *   pressure—simulation creates false confidence, near-miss learning alone is
 *   insufficient, and hybrid systems lack the mortality salience necessary to
 *   overcome organizational inertia.
 *
 * KEY AGENTS:
 *   - institutional_leadership: Sets training standards, controls budget allocation, benefits from peacetime by avoiding disruptive competence reviews.
 *   - frontline_operators: Perform safety-critical work, bear immediate consequence when competence fails, identity-locked to role, powerless to demand training infrastructure improvements.
 *   - regulatory_bodies: Benefit from low-frequency high-severity incident pattern; catastrophes generate political will for regulation.
 *   - organizational_workforce: Training, safety, maintenance staff whose budgets and positions depend on learning infrastructure allocation.
 *   - public_exposure_populations: Communities exposed to operational hazards; absorb the cost of the learning that follows catastrophe.
 *   - simulation_and_training_vendors: Structurally excluded from governance; would benefit from high-fidelity simulation being accepted as sufficient competence mechanism.
 *   - competing_safety_institutions: Peers and international bodies that have adopted distributed learning models; provide external evidence the constraint is not natural law.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 0.68).
domain_priors:suppression_score(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 0.71).
domain_priors:theater_ratio(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, extractiveness, 0.68).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, snare).
narrative_ontology:human_readable(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, "Catastrophe as Necessary Competence Selector").
narrative_ontology:topic_domain(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, "safety/organizational_learning").

domain_priors:requires_active_enforcement(catastrophe_avoidance_retention__catastrophe_as_necessary_selector).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 'c8bf78ff-d4b7-47cc-bf41-0c3b18d360c2').
narrative_ontology:cs_kernel_codification('c8bf78ff-d4b7-47cc-bf41-0c3b18d360c2', distributed).
narrative_ontology:cs_authority_grounding('c8bf78ff-d4b7-47cc-bf41-0c3b18d360c2', extraction).
narrative_ontology:cs_reading_relation('c8bf78ff-d4b7-47cc-bf41-0c3b18d360c2', catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, forecloses).
narrative_ontology:cs_reading_relation('c8bf78ff-d4b7-47cc-bf41-0c3b18d360c2', catastrophe_avoidance_retention__hybrid_near_miss_learning, coexists_with).
narrative_ontology:cs_axiom('c8bf78ff-d4b7-47cc-bf41-0c3b18d360c2', foundational, mortality_salience_prerequisite_for_change).
narrative_ontology:cs_axiom_status(mortality_salience_prerequisite_for_change, holdable).
narrative_ontology:cs_axiom_grounding('c8bf78ff-d4b7-47cc-bf41-0c3b18d360c2', mortality_salience_prerequisite_for_change, empirically_contingent).
narrative_ontology:cs_axiom('c8bf78ff-d4b7-47cc-bf41-0c3b18d360c2', foundational, simulation_insufficient_organizational_learning).
narrative_ontology:cs_axiom_status(simulation_insufficient_organizational_learning, holdable).
narrative_ontology:cs_axiom_grounding('c8bf78ff-d4b7-47cc-bf41-0c3b18d360c2', simulation_insufficient_organizational_learning, empirically_contingent).
narrative_ontology:cs_reference_frame('c8bf78ff-d4b7-47cc-bf41-0c3b18d360c2', peacetime_competence_stability).
narrative_ontology:cs_drift_state('c8bf78ff-d4b7-47cc-bf41-0c3b18d360c2', contemporary_safety_governance, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c8bf78ff-d4b7-47cc-bf41-0c3b18d360c2', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, catastrophe_avoidance_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, institutional_leadership).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, regulatory_bodies).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, frontline_operators).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, organizational_workforce).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, public_exposure_populations).
narrative_ontology:constraint_vindicates(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, mortality_salience_doctrine).
narrative_ontology:constraint_vindicates(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, organizational_trauma_learning_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets competence standards and training protocols. Benefits from peacetime (reduced budget pressure, regulatory scrutiny, operational disruption). Frames catastrophes as random black-swan events rather than predicted outcomes of degraded training. Controls whether near-miss learning and simulation are elevated to primary competence mechanisms. Maintains the constraint by treating actual incidents as isolated failures rather than signals that training culture has atrophied.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, institutional_leadership, agenda_setter,
    institutional, biographical, arbitrage, national).

% Perform safety-critical tasks under conditions where competence maintenance depends on catastrophic events. During peacetime, muscle memory degrades, complacency propagates, near-misses go unprocessed. When catastrophe arrives, operators bear the immediate consequence—injury, death, career termination. Exit means leaving the profession entirely; retraining or transfer are constrained by sunk credentials and identity fusion with the role. Their professional identity is constituted through their operational role; questioning the training framework is self-undermining.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, frontline_operators, payer,
    powerless, immediate, identity_locked, local).

% Benefit from a low-frequency, high-severity incident pattern: catastrophes generate political will for regulation and enforcement that would not emerge from near-miss data or simulation-based warnings. They maintain the constraint by refusing to treat simulation exercises or near-miss analysis as sufficient evidence for competence gaps. Their incentive is latent: regulation follows catastrophe more reliably than it follows prevention signals.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, regulatory_bodies, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, regulatory_bodies, agenda_setter).

% Includes maintenance, training, safety, and support staff whose competence and livelihoods depend on the operational tempo and learning infrastructure. Peacetime attrition is diffuse: budgets for training get reallocated, simulator uptime drops, after-action reviews become ceremonial. They bear the cost of competence decay without the narrative clarity to demand change—the constraint suppresses institutional acknowledgment that peacetime IS the problem.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, organizational_workforce, payer,
    moderate, biographical, constrained, regional).

% Communities downwind, downstream, or in the operational footprint of high-reliability systems (aviation, nuclear, chemical, transportation). They benefit from the real-incident learning that follows catastrophe but absorb the cost of the learning itself—injury, death, environmental damage. They have no seat in competence governance and no exit from exposure. The constraint treats them as statistical inevitabilities rather than as stakeholders whose consent matters.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, public_exposure_populations, payer,
    powerless, immediate, trapped, global).

% Would benefit from high-fidelity simulation being accepted as a sufficient competence mechanism. Currently excluded from the governance conversation—their products are framed as supplements to real experience, not as substitutes. The constraint's denial of simulation efficacy keeps demand for their services muted, though they could argue for technology-mediated competence maintenance that does not require actual catastrophe.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, simulation_and_training_vendors, excluded,
    powerful, biographical, mobile, global).

% International peer organizations, sister agencies, or parallel industries that have adopted distributed learning (hybrid near-miss + simulation) protocols. Observe the constraint's operation from outside; their success with alternative models is often ignored or attributed to different operating contexts rather than different competence maintenance strategies. They provide external evidence that the constraint is not natural law.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, competing_safety_institutions, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, institutional_leadership).
narrative_ontology:fixing_cost_class(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Real catastrophes provide high-stakes, mortality-salient learning events that trigger organizational trauma processing, forcing competence review, cross-functional accountability, and system redesign at speeds that peacetime bureaucracy cannot achieve. The constraint 'coordinates' on a terrible mechanism: crisis as the only legitimate triggering condition for serious change.
% TRANSFER_FUNCTION: Transfers the cost of organizational learning (death, injury, environmental damage, career termination, psychological trauma) from leadership and institutional systems onto frontline operators and exposed populations. The 'benefit' to leadership is deferred accountability: catastrophe is framed as black-swan unpredictability rather than predictable outcome of degraded training, which insulates decision-makers from responsibility during peacetime.
% ABSENT_VOICES: Simulation and training vendors, near-miss analysts, and international peer organizations that have adopted distributed learning models. Also absent: the voices of operators and affected populations before catastrophe occurs—they have no credible platform to demand competence maintenance during peacetime because near-miss and simulation data are treated as insufficient evidence. The constraint is maintained partly by keeping these voices from the table until catastrophe grants them retroactive legitimacy.
% DISAPPEARANCE_RATIONALE: If the constraint disappeared overnight—if leadership accepted that competence maintenance during peacetime is possible and necessary—the entire training infrastructure would reorganize: simulator investment would increase, near-miss analysis would become governance-level input, peer learning from foreign incidents would be institutionalized, and catastrophe frequency would decline. The current incident pattern is not a natural feature of high-reliability work; it is maintained by a specific architecture of deferral and denial.
% FOUNDING_PROBLEM: In the mid-20th century, organizations running safety-critical operations (nuclear power, aviation, chemical manufacturing) discovered that operators trained only on procedure and classroom instruction failed catastrophically when facing novel conditions. Real-world operational experience, especially experience of system failure and recovery, was found to be necessary for genuine competence. The question became: how do you maintain that competence during long peacetime periods when actual failures are rare by design?
% FOUNDING_PROBLEM_CORROBORATION: The founding problem—how to assess and maintain operator competence—has been substantially solved by peer institutions and scientific literature: high-fidelity simulation, distributed learning from near-misses, structured peer review, and foreign incident analysis have been demonstrated in parallel safety systems (some aviation domains, some nuclear fleets). The problem is NOT lack of knowledge; it is organizational unwillingness to adopt these methods during peacetime when catastrophe has not yet created crisis pressure. Testimony from international safety bodies (ICAO, IAEA, peer nuclear authorities) and peer-reviewed studies in human factors and organizational learning corroborate that the founding problem is tractable without waiting for catastrophe.
narrative_ontology:disappearance_verdict(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_avoidance_retention__catastrophe_as_necessary_selector_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_avoidance_retention__catastrophe_as_necessary_selector_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.35 to 0.68 over the interval because peacetime enables competence decay, raising the odds of catastrophic failure and the magnitude of the catastrophe when it occurs. Theater ratio rises from 0.22 to 0.58 because peacetime training becomes increasingly performative: simulators run at reduced capacity, near-miss reviews become ceremonial, and compliance documentation substitutes for actual competence maintenance. Suppression rises from 0.38 to 0.71 because the constraint's persistence depends on actively suppressing near-miss escalation, excluding simulation advocates, and treating operators' competence concerns as individual liability rather than systemic failure. The grid shows leveled asymmetry: at the individual level, operators face near-complete accessibility collapse (0.72) and high stakes inflation (0.72)—they cannot exit and cannot credibly demand change without catastrophe-level evidence. At the organizational level, accessibility collapses more slowly (0.64) and stakes inflation is muted (0.48) because institutional actors have arbitrage options (regulatory capture, budget reallocation, deferred accountability). At the structural level, accessibility collapse is slowest (0.61) and stakes inflation is lowest (0.35), reflecting the constraint's ability to persist through peacetime at systemic level because the cost is distributed and the benefit (deferred accountability) is concentrated. The measurement series models the peacetime accumulation dynamic: extractiveness and suppression rise as competence decays, theater ratio rises as the learning apparatus becomes increasingly performative, and the trajectory flattens at t=30 because at that point catastrophe becomes statistically probable and the measurement interval ends before the crisis event.
 *
 * PERSPECTIVAL GAP:
 *   From the institutional-leadership and regulatory seats, the constraint is not even visible—incidents are framed as stochastic black-swan failures, not as predictable outcomes of degraded competence-maintenance infrastructure. Peacetime is legitimate status quo; catastrophe is treated as external shock rather than endogenous consequence. From the frontline-operator and public-population seats, the constraint is brutally visible: they understand that the organization accepts higher incident rates because peacetime deflects pressure for change. The engine should compute drastically different types for these seats. Operators and exposed populations should compute as victims of a snare; leadership and regulators should compute as beneficiaries of a rent-collection mechanism dressed as natural hazard. The claimed type is snare; this is the true structural reading, not a claim that needs reconciliation with metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional leadership: d ≈ 0.1–0.2 (beneficiary, high power, arbitrage exit — they collect organizational legitimacy from peacetime and avoid disruptive reform until forced). Regulatory bodies: d ≈ 0.15–0.25 (secondary beneficiary through captured timing of intervention — they gain authority through post-catastrophe reform, which would not emerge as readily from prevention signals). Frontline operators: d ≈ 0.85–0.95 (full target — identity-locked to role, trapped exit, immediate consequence). Public populations: d ≈ 0.8–0.9 (full target — no representation, no exit, no choice). Organizational workforce: d ≈ 0.6–0.7 (moderate target — constrained exit, moderate power through collective action, diffuse cost). Simulation vendors: excluded, not scored, but structurally would have d ≈ 0.05–0.15 if admitted (beneficiary of alternative competence framing). The gap between beneficiary seats (d ≈ 0.1–0.2) and victim seats (d ≈ 0.8–0.95) is maximal; this is the signature extraction gap.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem—how to maintain competence during peacetime—is DEAD. Peer safety systems and scientific literature have solved it through distributed learning, high-fidelity simulation, and near-miss analysis. The constraint persists not because the founding problem remains unsolved but because leadership has optimized for peacetime silence over catastrophe prevention. This is mandatrophy: the constraint outlives its justifying purpose and is maintained by organizational inertia and incentive capture. The classification as snare (not rope, not scaffold, not piton) captures this: the constraint involves real coordination (the organization does coordinate around learning), but the coordination is asymmetrically extractive (operators and public pay, leadership defers accountability) and persistence depends on active suppression (near-miss data is minimized, simulation investment is starved, peer learning is ignored). A piton reading would frame the constraint as theatrical maintenance with no real function; that would be too kind. The constraint DOES organize the organization around competence; the problem is that it does so via catastrophe-driven learning, which maximizes harm. The snare classification captures both the coordination function and the asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mortality_salience_mechanism,
    'Is organizational competence maintenance structurally dependent on mortality salience and trauma, or is the apparent dependence on catastrophe a contingent artifact of institutional incentive capture?',
    'Sustained organizational adoption of hybrid distributed-learning systems (peer incidents, high-realism drills, near-miss escalation) without intervening catastrophe; measurement of competence maintenance trajectory in these systems compared to catastrophe-dependent systems.',
    'If hybrid systems maintain competence without catastrophe, the selection-pressure reading is false and competence decay is a governance failure, not a physical law. If hybrid systems fail to maintain competence, the mortality-salience mechanism is real and the constraint is more akin to mountain than snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mortality_salience_mechanism, empirical, 'Whether catastrophe-dependence is structural or contingent on institutional design.').

omega_variable(
    simulation_confidence_artifact,
    'Does high-fidelity simulation create false confidence (validating the snare reading), or does it genuinely substitute for catastrophic experience if adopted at sufficient scale and with appropriate epistemological framing?',
    'Longitudinal study of organizations that have invested heavily in simulation-based competence validation: do they show equivalent competence outcomes and incident rates to peer organizations, and do they show differential competence decay during peacetime intervals?',
    'If simulation is indeed equivalent, the constraint is a snare maintained by institutional denial of alternatives. If simulation is inferior, the constraint reflects a real structural property of human learning and organizational memory, complicating the snare classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(simulation_confidence_artifact, empirical, 'Whether simulation efficacy is limited by design or by institutional framing.').

omega_variable(
    regulatory_capture_asymmetry,
    'To what extent do regulatory bodies benefit from the catastrophe-dependent learning cycle, and does this benefit explain their resistance to distributed-learning framing?',
    'Analysis of regulatory action timing and innovation: do regulator innovations cluster after catastrophes rather than after peer incidents or near-miss escalations? Do regulators actively suppress or ignore peer-learning and simulation evidence?',
    'If regulators are genuinely captured by post-catastrophe reform timing, the constraint is maintained by dual governance incentives (leadership defers, regulators benefit from crisis). If regulators are neutral and simply respond to political pressure, the constraint is maintained primarily by leadership incentive capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_asymmetry, empirical, 'Whether regulatory agencies are structurally complicit in the catastrophe-dependent learning cycle.').

omega_variable(
    identity_lock_suppression_mechanism,
    'Is the measured suppression (0.71) primarily structural (operators cannot exit the profession) or internalized (operators have absorbed narratives that normalize peacetime competence decay)?',
    'Exit trajectory analysis: when operators leave the industry, do competence concerns drop sharply (suggesting internalized suppression) or persist in their memoirs and testimony (suggesting structural understanding that was merely constrained by position)?',
    'If suppression is internalized, the constraint''s persistence is more fragile—narrative reframing could trigger rapid destabilization. If suppression is structural, the constraint persists through legal and economic barriers independent of narrative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_suppression_mechanism, empirical, 'Suppression mechanism: structural barriers vs. internalized belief.').

omega_variable(
    black_swan_necessity,
    'Is catastrophe actually NECESSARY for competence maintenance, or does the reading conflate ''catastrophe is currently the only trigger that actually forces change'' with ''catastrophe is the only possible trigger''?',
    'Counterfactual analysis: construct organizations where governance structures grant operators and safety staff authority to escalate near-miss analysis and demand simulator investment without catastrophe-level evidence. Measure whether competence maintenance improves and whether incident rates decline.',
    'If governance authority suffices, the constraint is a snare (extractive governance maintaining itself through catastrophe dependence). If even with authority, operators and staff fail to overcome institutional inertia, the constraint may reflect deeper organizational learning structures.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(black_swan_necessity, conceptual, 'Whether catastrophe necessity is structural or a consequence of governance design.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(cata_tr_t0, observed).
narrative_ontology:measurement(cata_tr_t5, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 5, 0.28).
narrative_ontology:measurement_basis(cata_tr_t5, observed).
narrative_ontology:measurement(cata_tr_t10, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 10, 0.35).
narrative_ontology:measurement_basis(cata_tr_t10, observed).
narrative_ontology:measurement(cata_tr_t15, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 15, 0.42).
narrative_ontology:measurement_basis(cata_tr_t15, observed).
narrative_ontology:measurement(cata_tr_t20, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 20, 0.48).
narrative_ontology:measurement_basis(cata_tr_t20, observed).
narrative_ontology:measurement(cata_tr_t25, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 25, 0.54).
narrative_ontology:measurement_basis(cata_tr_t25, observed).
narrative_ontology:measurement(cata_tr_t30, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 30, 0.58).
narrative_ontology:measurement_basis(cata_tr_t30, observed).
narrative_ontology:measurement(cata_tr_t40, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 40, 0.58).
narrative_ontology:measurement_basis(cata_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(cata_be_t0, observed).
narrative_ontology:measurement(cata_be_t5, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 5, 0.42).
narrative_ontology:measurement_basis(cata_be_t5, observed).
narrative_ontology:measurement(cata_be_t10, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 10, 0.48).
narrative_ontology:measurement_basis(cata_be_t10, observed).
narrative_ontology:measurement(cata_be_t15, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 15, 0.54).
narrative_ontology:measurement_basis(cata_be_t15, observed).
narrative_ontology:measurement(cata_be_t20, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 20, 0.61).
narrative_ontology:measurement_basis(cata_be_t20, observed).
narrative_ontology:measurement(cata_be_t25, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 25, 0.65).
narrative_ontology:measurement_basis(cata_be_t25, observed).
narrative_ontology:measurement(cata_be_t30, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(cata_be_t30, observed).
narrative_ontology:measurement(cata_be_t40, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(cata_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(cata_su_t0, observed).
narrative_ontology:measurement(cata_su_t5, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 5, 0.44).
narrative_ontology:measurement_basis(cata_su_t5, observed).
narrative_ontology:measurement(cata_su_t10, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 10, 0.51).
narrative_ontology:measurement_basis(cata_su_t10, observed).
narrative_ontology:measurement(cata_su_t15, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 15, 0.58).
narrative_ontology:measurement_basis(cata_su_t15, observed).
narrative_ontology:measurement(cata_su_t20, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 20, 0.64).
narrative_ontology:measurement_basis(cata_su_t20, observed).
narrative_ontology:measurement(cata_su_t25, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 25, 0.68).
narrative_ontology:measurement_basis(cata_su_t25, observed).
narrative_ontology:measurement(cata_su_t30, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(cata_su_t30, observed).
narrative_ontology:measurement(cata_su_t40, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(cata_su_t40, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=40
narrative_ontology:measurement(cata_grid_01, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, accessibility_collapse(class), 0, 0.58).
narrative_ontology:measurement(cata_grid_02, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, accessibility_collapse(class), 40, 0.72).
narrative_ontology:measurement(cata_grid_03, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, accessibility_collapse(individual), 0, 0.68).
narrative_ontology:measurement(cata_grid_04, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, accessibility_collapse(individual), 40, 0.72).
narrative_ontology:measurement(cata_grid_05, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, accessibility_collapse(organizational), 0, 0.51).
narrative_ontology:measurement(cata_grid_06, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, accessibility_collapse(organizational), 40, 0.64).
narrative_ontology:measurement(cata_grid_07, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, accessibility_collapse(structural), 0, 0.45).
narrative_ontology:measurement(cata_grid_08, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, accessibility_collapse(structural), 40, 0.61).
narrative_ontology:measurement(cata_grid_09, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, resistance(class), 0, 0.54).
narrative_ontology:measurement(cata_grid_10, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, resistance(class), 40, 0.48).
narrative_ontology:measurement(cata_grid_11, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, resistance(individual), 0, 0.38).
narrative_ontology:measurement(cata_grid_12, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, resistance(individual), 40, 0.32).
narrative_ontology:measurement(cata_grid_13, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, resistance(organizational), 0, 0.62).
narrative_ontology:measurement(cata_grid_14, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, resistance(organizational), 40, 0.58).
narrative_ontology:measurement(cata_grid_15, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, resistance(structural), 0, 0.68).
narrative_ontology:measurement(cata_grid_16, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, resistance(structural), 40, 0.62).
narrative_ontology:measurement(cata_grid_17, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, stakes_inflation(class), 0, 0.52).
narrative_ontology:measurement(cata_grid_18, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, stakes_inflation(class), 40, 0.58).
narrative_ontology:measurement(cata_grid_19, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, stakes_inflation(individual), 0, 0.72).
narrative_ontology:measurement(cata_grid_20, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, stakes_inflation(individual), 40, 0.72).
narrative_ontology:measurement(cata_grid_21, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, stakes_inflation(organizational), 0, 0.38).
narrative_ontology:measurement(cata_grid_22, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, stakes_inflation(organizational), 40, 0.48).
narrative_ontology:measurement(cata_grid_23, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, stakes_inflation(structural), 0, 0.28).
narrative_ontology:measurement(cata_grid_24, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, stakes_inflation(structural), 40, 0.35).
narrative_ontology:measurement(cata_grid_25, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression(class), 0, 0.55).
narrative_ontology:measurement(cata_grid_26, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression(class), 40, 0.68).
narrative_ontology:measurement(cata_grid_27, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression(individual), 0, 0.68).
narrative_ontology:measurement(cata_grid_28, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression(individual), 40, 0.72).
narrative_ontology:measurement(cata_grid_29, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression(organizational), 0, 0.42).
narrative_ontology:measurement(cata_grid_30, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression(organizational), 40, 0.54).
narrative_ontology:measurement(cata_grid_31, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression(structural), 0, 0.32).
narrative_ontology:measurement(cata_grid_32, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression(structural), 40, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 0.18).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, simulation_as_proxy_catastrophe).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, hybrid_near_miss_learning).

% DUAL FORMULATION NOTE:
% catastrophe_as_necessary_selector is one reading of a contested kernel (catastrophe_avoidance_retention). Two sibling readings instantiate competing claims about competence-maintenance mechanisms: simulation_as_proxy_catastrophe treats high-fidelity simulation as functionally equivalent to real catastrophe; hybrid_near_miss_learning treats neither catastrophe nor simulation alone as sufficient and proposes distributed learning. This reading asserts that ONLY actual catastrophe provides selection pressure sufficient to overcome organizational inertia. All three stories share structural inputs (catastrophe frequency, peacetime duration, learning infrastructure investment) but derive different constraint types from different axioms about human and organizational learning. Each reading is a separate constraint with its own ε, beneficiary/victim structure, and stakeholder geometry.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
