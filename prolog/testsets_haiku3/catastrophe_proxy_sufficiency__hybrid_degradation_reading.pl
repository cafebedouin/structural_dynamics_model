% ============================================================================
% CONSTRAINT STORY: catastrophe_proxy_sufficiency__hybrid_degradation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-13
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
 *   human_readable: Simulation Competence Maintenance with Tacit Knowledge Degradation
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   Simulation-based training maintains procedural competence in
 *   high-consequence, low-frequency operational domains by providing safe,
 *   repeatable practice of critical scenarios. The system coordinates
 *   learning without catastrophes and generates revenue for the training
 *   industry. The constraint instantiates the 'hybrid_degradation_reading' of
 *   the contested 'catastrophe_proxy_sufficiency' kernel: simulation solves
 *   the operational coordination problem (avoiding catastrophe-dependent
 *   learning) but at a hidden cost—tacit knowledge and stress-response
 *   capacity that were historically maintained through actual catastrophe
 *   exposure degrade over generational timescales. Current operators pass
 *   certification and operate safely; next-generation operators inherit
 *   degraded adaptive capacity despite passing the same tests. The extraction
 *   is distributed (long-term safety margins, next-generation competence)
 *   rather than concentrated, and the degradation is latent—visible only when
 *   novel stress arrives or in retrospective analysis of generational
 *   knowledge loss. The claim (tangled rope) and metrics (high theater,
 *   rising suppression requirement) are independent authored facts: the
 *   constraint claims coordination with extraction, and the metrics document
 *   the extraction's increasing reliance on enforcement (suppression) and
 *   performative competence (theater).
 *
 * KEY AGENTS:
 *   - certification_training_industry: institutional agenda-setter; designs curricula, sets fidelity standards, operates simulators, collects recertification revenue; benefits from the simulationization of competence
 *   - regulatory_authorities: institutional agenda-setter; sets recertification frequency, delegates competence assessment to training industry, enforces simulator-based training without continuous audit of tacit-knowledge retention
 *   - current_generation_operators: organized beneficiary/payer; benefit from accessible training and catastrophe avoidance; bear latent cost of degraded intergenerational knowledge transmission
 *   - next_generation_operators: powerless victim; identity_locked in the certification system; inherit procedural competence but degraded tacit knowledge and stress-response capacity; cannot challenge the system without exiting the profession
 *   - industry_outsiders (public, downstream workers): powerless, trapped; depend on system safety; excluded from certification-adequacy decisions; bear tail risk when degradation intersects real stress
 *   - independent_safety_researchers: moderate-power observers; can document degradation but lack enforcement authority to mandate higher-fidelity training or knowledge-transfer programs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 0.68).
domain_priors:suppression_score(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 0.72).
domain_priors:theater_ratio(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__hybrid_degradation_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__hybrid_degradation_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__hybrid_degradation_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_proxy_sufficiency__hybrid_degradation_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_proxy_sufficiency__hybrid_degradation_reading, "Simulation Competence Maintenance with Tacit Knowledge Degradation").
narrative_ontology:topic_domain(catastrophe_proxy_sufficiency__hybrid_degradation_reading, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(catastrophe_proxy_sufficiency__hybrid_degradation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_proxy_sufficiency__hybrid_degradation_reading, '6c1b33c5-d53b-4270-8bbd-4ea52ae8818e').
narrative_ontology:cs_kernel_codification('6c1b33c5-d53b-4270-8bbd-4ea52ae8818e', formalized).
narrative_ontology:cs_authority_grounding('6c1b33c5-d53b-4270-8bbd-4ea52ae8818e', extraction).
narrative_ontology:cs_interpretation_layer_present('6c1b33c5-d53b-4270-8bbd-4ea52ae8818e').
narrative_ontology:cs_reading_relation('6c1b33c5-d53b-4270-8bbd-4ea52ae8818e', catastrophe_proxy_sufficiency__catastrophe_necessity_reading, coexists_with).
narrative_ontology:cs_reading_relation('6c1b33c5-d53b-4270-8bbd-4ea52ae8818e', catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, influences).
narrative_ontology:cs_reading_relation('6c1b33c5-d53b-4270-8bbd-4ea52ae8818e', catastrophe_proxy_sufficiency__simulation_fidelity_threshold, influences).
narrative_ontology:cs_axiom('6c1b33c5-d53b-4270-8bbd-4ea52ae8818e', foundational, tacit_knowledge_irreplaceable_by_procedure).
narrative_ontology:cs_axiom_status(tacit_knowledge_irreplaceable_by_procedure, holdable).
narrative_ontology:cs_axiom_grounding('6c1b33c5-d53b-4270-8bbd-4ea52ae8818e', tacit_knowledge_irreplaceable_by_procedure, empirically_contingent).
narrative_ontology:cs_axiom('6c1b33c5-d53b-4270-8bbd-4ea52ae8818e', foundational, generational_knowledge_transmission_requires_direct_stress).
narrative_ontology:cs_axiom_status(generational_knowledge_transmission_requires_direct_stress, holdable).
narrative_ontology:cs_axiom_grounding('6c1b33c5-d53b-4270-8bbd-4ea52ae8818e', generational_knowledge_transmission_requires_direct_stress, empirically_contingent).
narrative_ontology:cs_reference_frame('6c1b33c5-d53b-4270-8bbd-4ea52ae8818e', simulation_as_competence_maintenance).
narrative_ontology:cs_drift_state('6c1b33c5-d53b-4270-8bbd-4ea52ae8818e', contemporary_generational_succession_phase, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('6c1b33c5-d53b-4270-8bbd-4ea52ae8818e', '').
narrative_ontology:cs_kernel_id(catastrophe_proxy_sufficiency__hybrid_degradation_reading, catastrophe_proxy_sufficiency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__hybrid_degradation_reading, certification_training_industry).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__hybrid_degradation_reading, long_term_safety_margins).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__hybrid_degradation_reading, next_generation_operators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__hybrid_degradation_reading, current_generation_operators).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__hybrid_degradation_reading, current_generation_operators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs, operates, and certifies simulation-based training programs. Sets curriculum standards, manages the fidelity/cost tradeoff (lower-fidelity simulations are cheaper to operate and easier to standardize across sites), and collects recurring revenue from ongoing recertification requirements. Defines what counts as 'competence maintained' through procedural metrics (checklist completion, scenario response time) that simulations can reliably produce.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, certification_training_industry, agenda_setter,
    institutional, generational, arbitrage, global).

% Set and enforce training and recertification requirements. Have delegated competence assessment to the training industry and simulator fidelity to operational feasibility reviews. Accept simulation as sufficient proxy for catastrophe-equivalent learning because it meets cost, scheduling, and political feasibility constraints that real-world catastrophe training cannot satisfy. Do not continuously audit whether tacit knowledge and stress-response capacity are actually maintained.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, regulatory_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Pass certification-required training, maintain procedural competence through simulations, and operate safely in routine conditions. They benefit from accessible, repeatable training and the cost avoidance of catastrophe-equivalent stress. They bear the latent cost: tacit knowledge and stress-response pathways are not being reinforced in the way genuine catastrophe would reinforce them, but they operate before the degradation becomes visible.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, current_generation_operators, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_proxy_sufficiency__hybrid_degradation_reading, current_generation_operators, payer).

% Inherit a system that certifies them as competent based on simulation-derived procedural performance. They are trained on the same simulations as their predecessors but receive no historical narrative, no mentored immersion in prior catastrophe handling, and no intergenerational transmission of tacit knowledge about failure modes that only manifest under stress. Their professional identity is constituted through the certification system; they cannot reject it and remain in the field. When novel stress arrives, they have procedural competence but degraded tacit-knowledge capacity compared to a prior generation trained through actual catastrophe exposure.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, next_generation_operators, payer,
    powerless, biographical, identity_locked, global).

% Workers in adjacent industries or the public at large who depend on the safety of the system but have no voice in training adequacy, simulator fidelity requirements, or the decision to treat simulation as sufficient proxy. They cannot audit the system's tacit-knowledge depth and bear the tail risk of catastrophe when hidden degradation intersects with real stress.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, industry_outsiders, excluded,
    powerless, biographical, trapped, global).

% Study organizational learning, high-reliability operations, and simulator fidelity in research contexts separate from the operational and training systems. Can observe and document generational degradation in tacit knowledge and stress-response capacity but lack enforcement authority to change training standards or simulator requirements. Their findings are available but not integrated into certification processes.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, independent_safety_researchers, observer,
    moderate, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_proxy_sufficiency__hybrid_degradation_reading, certification_training_industry).
narrative_ontology:fixing_cost_class(catastrophe_proxy_sufficiency__hybrid_degradation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the operational necessity of maintaining competence in low-frequency, high-consequence domains where real catastrophes are rare enough that direct learning from them is statistically unreliable as a training method. Simulation allows repeated, controlled practice of critical scenarios without requiring actual catastrophes. Coordinates across organizations and decades by standardizing what 'competence' means (procedural performance on defined scenarios) and what maintains it (recertification on the same scenarios).
% TRANSFER_FUNCTION: Shifts the cost and risk of training from direct catastrophe exposure to recurring simulation operations. Money flows from operators and their employers to the training industry. Safety margins — the system's capacity to absorb stresses beyond the designed envelope — flow toward the operators and training system (by avoiding costly catastrophes) but also away from long-term organizational resilience (by not reinforcing tacit knowledge and stress-response pathways that only real catastrophe teaches). Knowledge flows in one direction: from prior generations (who experienced real catastrophes) to current operators (who experience only simulation), with degradation at each transfer.
% ABSENT_VOICES: Workers in downstream industries and the public whose safety depends on the system but who are not in the room where simulator fidelity standards are set. Long-term safety researchers and operators with experience in actual catastrophe-response (now retired or deceased) would argue for higher simulator fidelity and intergenerational knowledge transmission, but their input has been structurally excluded by the shift from experience-based to credential-based competence assessment. Next-generation operators themselves do not have standing to question whether their training adequately prepared them until catastrophe arrives.
% DISAPPEARANCE_RATIONALE: If simulation-based training disappeared, operators would either revert to learning primarily through minor incidents and mentored experience, which would be slower but would reinforce tacit knowledge continuously, or catastrophes would increase in frequency until tacit-knowledge capacity was regenerated through direct exposure — either way, the system would rearrange toward a different balance of procedural vs. tacit learning. The training industry's business model would collapse. Certification processes would shift to experience-based standards rather than simulation-scenario standards.
% FOUNDING_PROBLEM: High-consequence, low-frequency operational domains (nuclear plants, aircraft carrier maneuvers, pandemic response) cannot rely on operators learning primarily from actual catastrophes: catastrophes are rare, costly, and teach under extreme stress. Direct catastrophe-based training is infeasible (you cannot stage a real pandemic to train pandemic response). Simulation was developed to provide safe, repeatable, scalable practice of critical scenarios so operators could maintain competence without depending on actual catastrophes.
% FOUNDING_PROBLEM_CORROBORATION: The infeasibility of direct catastrophe-based training is universally attested: operational managers, safety researchers, and regulatory authorities all agree you cannot stage real catastrophes for training. The contested question (documented in independent research and operator testimony from other high-reliability domains) is whether simulation alone is sufficient or whether it maintains only procedural competence while degrading tacit knowledge and stress-response pathways. The certification industry and regulators attest simulation is sufficient based on procedural metrics; safety researchers and intergenerational operator accounts attest the founding problem has shifted — it is no longer 'how do we train without catastrophes' but 'how do we maintain tacit knowledge and adaptive capacity when we eliminate catastrophe exposure.'
narrative_ontology:disappearance_verdict(catastrophe_proxy_sufficiency__hybrid_degradation_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_proxy_sufficiency__hybrid_degradation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
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
 *   Extractiveness rises from 0.42 (early simulation era, training industry young, prior generation with catastrophe experience still operational) to 0.68 (simulation era mature, prior generation retired or deceased, knowledge transmission gaps accumulate across multiple generational handoffs). Theater rises from 0.28 to 0.58 as the share of training activity devoted to compliance-auditable procedural competence grows relative to tacit-knowledge reinforcement—what is teachable and testable in a simulator expands, what requires mentorship and real-uncertainty navigation shrinks. Suppression requirement rises from 0.52 to 0.72 as the system must increasingly defend simulator-based certification against growing evidence and argument from safety researchers and experienced operators that tacit knowledge is degrading. The leveled coercion grid shows individual-level accessibility collapse rising (operators have fewer alternatives to simulation-based certification as it becomes universal and identity-fused), organizational-level suppression stable (training organizations actively suppress external audits of knowledge depth), and class-level resistance rising (operator communities, professional associations, and safety researchers increasingly advocate for knowledge-transfer programs and higher-fidelity training). The measurement series samples the constraint's operation from the early simulation-dominance phase (t=0, prior-generation knowledge still fresh) through a mature phase (t=40, generational handoffs have accumulated knowledge loss, suppression required to maintain the system's legitimacy).
 *
 * PERSPECTIVAL GAP:
 *   The certification training industry perspective: simulation is a genuine coordination solution that replaces dangerous catastrophe-dependent learning with safe, repeatable practice; current procedural competence is validated by decades of operational safety; concerns about tacit-knowledge degradation are speculative and lack operational proof. The next-generation operator perspective (suppressed by identity_lock and professional norms): they inherit the system as given, pass certification, operate safely in routine conditions, and have no standing to question whether their tacit-knowledge capacity is degraded until novel stress arrives; at that point it is too late. The independent researcher perspective: systematic evidence of generational tacit-knowledge degradation exists (operator interviews, knowledge-transfer program outcomes, comparative studies of pre- and post-simulator-era cohorts), but regulatory authorities do not mandate its measurement or its remediation. The engine computes these divergences from the structural data: beneficiary with institutional power and arbitrage options (training industry) experiences the constraint as coordination; victim with identity_lock and powerlessness (next-generation operators) experiences it as extraction; the gap between beneficiary and victim directionalities is the seat divergence the engine measures.
 *
 * DIRECTIONALITY LOGIC:
 *   The certification training industry is the structural beneficiary: it sets the standards that justify recurring training revenue, maintains control over what counts as 'competence,' and expands its market reach as simulation becomes mandatory. Directionality approaches full-beneficiary (d near 0.0) because the industry collects directly and can reshape the market through standard-setting. Regulatory authorities are agenda-setters with institutional power and arbitrage options (they could require higher-fidelity simulators, mandate intergenerational knowledge programs, audit tacit-knowledge retention), so directionality is near-symmetric (d ≈ 0.4-0.5)—they benefit from administrative simplification but lose if catastrophe exposes degradation. Current operators benefit from accessible training and catastrophe avoidance but bear hidden costs; directionality near-symmetric (d ≈ 0.5). Next-generation operators are the true victims: they are identity_locked (exiting the profession means losing professional standing), they inherit procedural competence but degraded tacit knowledge, they have no power to reshape training standards, and they discover the degradation only under real stress. Directionality approaches full-target (d near 1.0) because suppression is internalized (they cannot question the system without career risk) and alternatives are foreclosed (every operator must pass certification; every certification is simulation-based). Industry outsiders and downstream workers bear the tail risk but have no voice; directionality is full-target (d = 1.0), trapped, but their exclusion keeps them analytically separate.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem—how to maintain competence in low-frequency, high-consequence domains without depending on catastrophes—remains live. The founding solution—simulation-based training—solves it operationally (procedural competence is maintained, catastrophes are avoided). But the solution's hidden mandate—that simulation maintains NOT ONLY procedural competence but ALSO tacit knowledge and stress-response capacity—has become contested and possibly expired. Regulators and the training industry assume the hidden mandate is satisfied because procedural metrics look healthy; safety researchers and experienced operators attest it is degrading. The constraint avoids explicit mandatrophy (the system does maintain procedural competence) but harbors latent mandatrophy: the system solves the stated problem while creating a new problem (intergenerational knowledge loss) that only appears when real stress arrives. This hidden-mandate structure is exactly where tangled ropes live—genuine coordination with asymmetric extraction where the beneficiary sets the standards for what counts as success and the victim has no standing to challenge the standard until harm is visible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tacit_knowledge_measurability,
    'Is the degradation of tacit knowledge and stress-response capacity measurable before catastrophe occurs, or does it remain latent until high-consequence stress arrives?',
    'Longitudinal study of operator performance in adversarial simulations (surprise scenarios, resource-constrained conditions, high-fidelity stress) comparing generations trained through simulation vs. prior generations with catastrophe exposure. Measure anomaly recognition, improvisation quality, and decision speed under uncertainty — metrics that correlate with tacit-knowledge depth but are not in standard certification curricula.',
    'If measurable pre-catastrophe, the degradation is a known externality that could trigger higher simulator fidelity requirements and intergenerational knowledge-transfer programs; if latent, the extraction persists invisible until catastrophe exposes it, at which point the harm is already done.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tacit_knowledge_measurability, empirical, 'Latency of tacit-knowledge degradation signals.').

omega_variable(
    simulator_fidelity_sufficiency_boundary,
    'Is there a threshold of simulator fidelity (physiological stress, temporal pressure, genuine uncertainty, irreversible consequence simulation) beyond which simulation becomes equivalent to catastrophe-level learning, or is direct catastrophe exposure irreplaceable?',
    'Design-of-experiments with simulators at progressively higher fidelity/stress levels and measure whether operators trained at high-fidelity simulations show stress-response pathways and tacit-knowledge retention comparable to operators with actual catastrophe experience. Compare metrics across domains (aviation post-high-fidelity simulator adoption; nuclear post-simulator-based training; pandemic response in jurisdictions with vs. without pre-pandemic drills).',
    'If a threshold exists, the constraint becomes ''simulation above fidelity threshold'' rather than ''simulation per se,'' and regulatory leverage to raise simulator fidelity becomes available; if no threshold exists, tacit-knowledge degradation is inherent to simulation-based training and requires structural redesign (intergenerational knowledge-transfer programs, periodic low-consequence real-system challenges, mentor-based apprenticeship alongside simulation).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(simulator_fidelity_sufficiency_boundary, conceptual, 'Whether simulation fidelity has an upper bound or whether catastrophe-learning is categorically distinct.').

omega_variable(
    identity_lock_and_professional_exit,
    'For next-generation operators whose entire career identity is constituted through the simulation-based certification system, is it possible to exit the system and maintain professional standing, or is identity_locked the structural condition?',
    'Qualitative interviews with operators attempting to challenge simulator-based certification or advocate for intergenerational knowledge transfer; observe whether they can do so while remaining professionally credible. Track career consequences for operators who publicly question simulator sufficiency.',
    'If operators are identity_locked, the system suppresses internal dissent from the generation most vulnerable to latent degradation; if exit and critique remain available, operators can pressure for higher-fidelity training or knowledge-transfer programs from within professional standing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(identity_lock_and_professional_exit, empirical, 'Professional identity fusion with certification system as a suppression mechanism.').

omega_variable(
    kernel_reading_committer_structure,
    'This constraint instantiates the ''hybrid_degradation_reading'' of the catastrophe_proxy_sufficiency kernel. Alternative readings foreclose, coexist with, or influence this reading; the degradation claim itself depends on foundational axioms about whether tacit knowledge is irreplaceable.',
    'Comparative analysis of the four sibling readings (catastrophe_necessity_reading, simulation_as_proxy_catastrophe_reading, simulation_fidelity_threshold) to map reading_relations (forecloses/coexists_with/influences) and identify the axioms (empirically_contingent vs. deontological) grounding each. The hybrid_degradation_reading presupposes empirical claims about knowledge decay that a simulation_as_proxy reading would deny; the catastrophe_necessity reading presupposes that only direct exposure teaches. Different axioms, different empirical presuppositions, different vulnerability to evidence.',
    'If a sibling reading forecloses this one (e.g. simulation_as_proxy_catastrophe with sufficient fidelity forecloses degradation claims), the constraint classification should shift; if readings coexist, the reading choice depends on institutional framing and incentives, not fact. If influences, one reading changes the operating environment for others without resolving the disagreement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Kernel reading under-determination: the hybrid_degradation claim is one live reading among contested alternatives grounded in different axioms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(cata_tr_t0, observed).
narrative_ontology:measurement(cata_tr_t8, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 8, 0.36).
narrative_ontology:measurement_basis(cata_tr_t8, observed).
narrative_ontology:measurement(cata_tr_t16, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 16, 0.46).
narrative_ontology:measurement_basis(cata_tr_t16, observed).
narrative_ontology:measurement(cata_tr_t24, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 24, 0.54).
narrative_ontology:measurement_basis(cata_tr_t24, observed).
narrative_ontology:measurement(cata_tr_t32, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 32, 0.57).
narrative_ontology:measurement_basis(cata_tr_t32, projected).
narrative_ontology:measurement(cata_tr_t40, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 40, 0.58).
narrative_ontology:measurement_basis(cata_tr_t40, projected).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(cata_be_t0, observed).
narrative_ontology:measurement(cata_be_t8, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 8, 0.48).
narrative_ontology:measurement_basis(cata_be_t8, observed).
narrative_ontology:measurement(cata_be_t16, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 16, 0.56).
narrative_ontology:measurement_basis(cata_be_t16, observed).
narrative_ontology:measurement(cata_be_t24, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 24, 0.63).
narrative_ontology:measurement_basis(cata_be_t24, observed).
narrative_ontology:measurement(cata_be_t32, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 32, 0.67).
narrative_ontology:measurement_basis(cata_be_t32, projected).
narrative_ontology:measurement(cata_be_t40, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(cata_be_t40, projected).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement_basis(cata_su_t0, observed).
narrative_ontology:measurement(cata_su_t8, catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 8, 0.59).
narrative_ontology:measurement_basis(cata_su_t8, observed).
narrative_ontology:measurement(cata_su_t16, catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 16, 0.66).
narrative_ontology:measurement_basis(cata_su_t16, observed).
narrative_ontology:measurement(cata_su_t24, catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 24, 0.7).
narrative_ontology:measurement_basis(cata_su_t24, observed).
narrative_ontology:measurement(cata_su_t32, catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 32, 0.71).
narrative_ontology:measurement_basis(cata_su_t32, projected).
narrative_ontology:measurement(cata_su_t40, catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(cata_su_t40, projected).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=40
narrative_ontology:measurement(cata_grid_01, catastrophe_proxy_sufficiency__hybrid_degradation_reading, accessibility_collapse(class), 0, 0.38).
narrative_ontology:measurement(cata_grid_02, catastrophe_proxy_sufficiency__hybrid_degradation_reading, accessibility_collapse(class), 40, 0.42).
narrative_ontology:measurement(cata_grid_03, catastrophe_proxy_sufficiency__hybrid_degradation_reading, accessibility_collapse(individual), 0, 0.62).
narrative_ontology:measurement(cata_grid_04, catastrophe_proxy_sufficiency__hybrid_degradation_reading, accessibility_collapse(individual), 40, 0.68).
narrative_ontology:measurement(cata_grid_05, catastrophe_proxy_sufficiency__hybrid_degradation_reading, accessibility_collapse(organizational), 0, 0.58).
narrative_ontology:measurement(cata_grid_06, catastrophe_proxy_sufficiency__hybrid_degradation_reading, accessibility_collapse(organizational), 40, 0.64).
narrative_ontology:measurement(cata_grid_07, catastrophe_proxy_sufficiency__hybrid_degradation_reading, accessibility_collapse(structural), 0, 0.32).
narrative_ontology:measurement(cata_grid_08, catastrophe_proxy_sufficiency__hybrid_degradation_reading, accessibility_collapse(structural), 40, 0.35).
narrative_ontology:measurement(cata_grid_09, catastrophe_proxy_sufficiency__hybrid_degradation_reading, resistance(class), 0, 0.72).
narrative_ontology:measurement(cata_grid_10, catastrophe_proxy_sufficiency__hybrid_degradation_reading, resistance(class), 40, 0.78).
narrative_ontology:measurement(cata_grid_11, catastrophe_proxy_sufficiency__hybrid_degradation_reading, resistance(individual), 0, 0.35).
narrative_ontology:measurement(cata_grid_12, catastrophe_proxy_sufficiency__hybrid_degradation_reading, resistance(individual), 40, 0.42).
narrative_ontology:measurement(cata_grid_13, catastrophe_proxy_sufficiency__hybrid_degradation_reading, resistance(organizational), 0, 0.58).
narrative_ontology:measurement(cata_grid_14, catastrophe_proxy_sufficiency__hybrid_degradation_reading, resistance(organizational), 40, 0.65).
narrative_ontology:measurement(cata_grid_15, catastrophe_proxy_sufficiency__hybrid_degradation_reading, resistance(structural), 0, 0.62).
narrative_ontology:measurement(cata_grid_16, catastrophe_proxy_sufficiency__hybrid_degradation_reading, resistance(structural), 40, 0.68).
narrative_ontology:measurement(cata_grid_17, catastrophe_proxy_sufficiency__hybrid_degradation_reading, stakes_inflation(class), 0, 0.42).
narrative_ontology:measurement(cata_grid_18, catastrophe_proxy_sufficiency__hybrid_degradation_reading, stakes_inflation(class), 40, 0.48).
narrative_ontology:measurement(cata_grid_19, catastrophe_proxy_sufficiency__hybrid_degradation_reading, stakes_inflation(individual), 0, 0.48).
narrative_ontology:measurement(cata_grid_20, catastrophe_proxy_sufficiency__hybrid_degradation_reading, stakes_inflation(individual), 40, 0.55).
narrative_ontology:measurement(cata_grid_21, catastrophe_proxy_sufficiency__hybrid_degradation_reading, stakes_inflation(organizational), 0, 0.52).
narrative_ontology:measurement(cata_grid_22, catastrophe_proxy_sufficiency__hybrid_degradation_reading, stakes_inflation(organizational), 40, 0.61).
narrative_ontology:measurement(cata_grid_23, catastrophe_proxy_sufficiency__hybrid_degradation_reading, stakes_inflation(structural), 0, 0.58).
narrative_ontology:measurement(cata_grid_24, catastrophe_proxy_sufficiency__hybrid_degradation_reading, stakes_inflation(structural), 40, 0.64).
narrative_ontology:measurement(cata_grid_25, catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression(class), 0, 0.48).
narrative_ontology:measurement(cata_grid_26, catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression(class), 40, 0.52).
narrative_ontology:measurement(cata_grid_27, catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression(individual), 0, 0.64).
narrative_ontology:measurement(cata_grid_28, catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression(individual), 40, 0.71).
narrative_ontology:measurement(cata_grid_29, catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression(organizational), 0, 0.58).
narrative_ontology:measurement(cata_grid_30, catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression(organizational), 40, 0.65).
narrative_ontology:measurement(cata_grid_31, catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression(structural), 0, 0.42).
narrative_ontology:measurement(cata_grid_32, catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression(structural), 40, 0.44).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_proxy_sufficiency__hybrid_degradation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 0.12).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__hybrid_degradation_reading, catastrophe_necessity_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__hybrid_degradation_reading, simulation_as_proxy_catastrophe_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__hybrid_degradation_reading, simulation_fidelity_threshold).

% DUAL FORMULATION NOTE:
% This constraint is part of the catastrophe_proxy_sufficiency kernel family (4 readings). All readings share a common referent—the standing arrangement where simulation substitutes for direct catastrophe exposure in training—but differ on structural assessment: this reading (hybrid_degradation) claims tacit-knowledge decay, catastrophe_necessity denies simulation sufficiency, simulation_as_proxy claims high-fidelity simulation is equivalent, simulation_fidelity_threshold places a technology boundary. Each reading is a separate constraint story with its own ε, beneficiary/victim structure, and classification. Network edges link siblings: this reading influences (creates downstream pressure on) the simulation_as_proxy reading by establishing empirical counter-evidence; it coexists with catastrophe_necessity (both readings are live in safety communities) and simulation_fidelity_threshold (orthogonal axes: necessity vs. threshold are different questions).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_proxy_sufficiency__hybrid_degradation_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
