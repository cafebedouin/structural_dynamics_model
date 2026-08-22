% ============================================================================
% CONSTRAINT STORY: competence_exercise_validity__real_catastrophe_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_exercise_validity__real_catastrophe_only, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: competence_exercise_validity__real_catastrophe_only
 *   human_readable: Real Catastrophe as Competence Validation Gate
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   Safety-critical organizations (aviation, nuclear power, emergency
 *   response, maritime) face a structural claim about competence validation:
 *   can competence be reliably demonstrated through simulation, drills, and
 *   structured training, or does only real catastrophic stress truly exercise
 *   and prove the full range of adaptive capacity? The real-catastrophe-only
 *   reading asserts that simulation masks decay—crews may perform flawlessly
 *   in scripted scenarios while actual catastrophic stress (novel combination
 *   of failures, ambiguous signals, high cognitive load, fear, fatigue)
 *   exercises capacities that drills cannot proxy. Under this reading, the
 *   absence of catastrophe for decades is not evidence of competence adequacy
 *   but evidence of luck or system redundancy—the safety record reflects what
 *   the infrastructure prevented, not what personnel could actually do. This
 *   reading is one of three positions in a contested kernel
 *   (competence_exercise_validity); the other readings claim simulation can
 *   be a proxy or that hybrid continuous refresh is both necessary and
 *   sufficient.
 *
 * KEY AGENTS:
 *   - front_line_operators (pilots, reactor technicians, emergency responders): those whose competence is being validated; face constraints on promotion, credentials, and autonomy under the real-catastrophe reading
 *   - risk_management_institutions (regulatory bodies, corporate safety offices): the agenda-setters that enforce the real-catastrophe standard; benefit from the doctrine's rhetorical safety claim
 *   - simulation_industry (training vendors, drill designers, scenario developers): whose business model depends on simulation being credited as competence validation; suppressed by the real-catastrophe reading
 *   - system_operators/dispatchers: those who deploy competence and face the operational consequence of competence gaps; have conflicting interests in validation rigor versus training feasibility
 *   - incident_investigators/safety researchers: observers with structural incentive to evaluate which reading predicts safety outcomes accurately
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_validity__real_catastrophe_only, 0.68).
domain_priors:suppression_score(competence_exercise_validity__real_catastrophe_only, 0.72).
domain_priors:theater_ratio(competence_exercise_validity__real_catastrophe_only, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_validity__real_catastrophe_only, extractiveness, 0.68).
narrative_ontology:constraint_metric(competence_exercise_validity__real_catastrophe_only, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(competence_exercise_validity__real_catastrophe_only, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_validity__real_catastrophe_only, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(competence_exercise_validity__real_catastrophe_only, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_validity__real_catastrophe_only, tangled_rope).
narrative_ontology:human_readable(competence_exercise_validity__real_catastrophe_only, "Real Catastrophe as Competence Validation Gate").
narrative_ontology:topic_domain(competence_exercise_validity__real_catastrophe_only, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(competence_exercise_validity__real_catastrophe_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_validity__real_catastrophe_only, 'e5d0b061-ff26-4d57-aed0-82130c9d09bd').
narrative_ontology:cs_kernel_codification('e5d0b061-ff26-4d57-aed0-82130c9d09bd', formalized).
narrative_ontology:cs_authority_grounding('e5d0b061-ff26-4d57-aed0-82130c9d09bd', extraction).
narrative_ontology:cs_interpretation_layer_present('e5d0b061-ff26-4d57-aed0-82130c9d09bd').
narrative_ontology:cs_reading_relation('e5d0b061-ff26-4d57-aed0-82130c9d09bd', competence_exercise_validity__simulation_as_proxy, forecloses).
narrative_ontology:cs_reading_relation('e5d0b061-ff26-4d57-aed0-82130c9d09bd', competence_exercise_validity__continuous_refresh_hybrid, influences).
narrative_ontology:cs_axiom('e5d0b061-ff26-4d57-aed0-82130c9d09bd', foundational, real_stress_irreducible_to_simulation).
narrative_ontology:cs_axiom_status(real_stress_irreducible_to_simulation, holdable).
narrative_ontology:cs_axiom_grounding('e5d0b061-ff26-4d57-aed0-82130c9d09bd', real_stress_irreducible_to_simulation, empirically_contingent).
narrative_ontology:cs_axiom('e5d0b061-ff26-4d57-aed0-82130c9d09bd', secondary, simulation_competence_decay_masking).
narrative_ontology:cs_axiom_status(simulation_competence_decay_masking, holdable).
narrative_ontology:cs_axiom_grounding('e5d0b061-ff26-4d57-aed0-82130c9d09bd', simulation_competence_decay_masking, empirically_contingent).
narrative_ontology:cs_reference_frame('e5d0b061-ff26-4d57-aed0-82130c9d09bd', catastrophe_validation_imperative).
narrative_ontology:cs_drift_state('e5d0b061-ff26-4d57-aed0-82130c9d09bd', contemporary_system_redundancy_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e5d0b061-ff26-4d57-aed0-82130c9d09bd', '').
narrative_ontology:cs_kernel_id(competence_exercise_validity__real_catastrophe_only, competence_exercise_validity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_validity__real_catastrophe_only, risk_management_ideology).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__real_catastrophe_only, simulation_industry).
narrative_ontology:constraint_victim(competence_exercise_validity__real_catastrophe_only, front_line_operators).
narrative_ontology:constraint_victim(competence_exercise_validity__real_catastrophe_only, safety_system_maintainers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__real_catastrophe_only, system_operators_dispatchers).
narrative_ontology:constraint_victim(competence_exercise_validity__real_catastrophe_only, simulation_industry).
narrative_ontology:constraint_victim(competence_exercise_validity__real_catastrophe_only, system_operators_dispatchers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Regulatory bodies, corporate safety offices, and accident investigation boards that set and enforce competence-validation standards. They maintain the doctrine that only real catastrophe truly tests competence; this doctrine justifies their oversight authority (if competence could be validated by simulation alone, regulation would be superfluous). They benefit from the constraint's rhetorical safety claim: our systems are safe because we wait for catastrophe to prove readiness, which is logically unfalsifiable. They actively suppress rival validation approaches (simulation-based credentialing, hybrid continuous-refresh cycles) through regulation and standard-setting.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, risk_management_institutions, agenda_setter,
    institutional, generational, arbitrage, national).

% Pilots, reactor operators, emergency responders, surgeons, maritime captains—those whose competence is formally unvalidated until catastrophe occurs. They operate under the doctrine that their training is insufficient proof; they carry the liability and psychological weight of unproven competence. They face career constraints (advancement delayed pending formal validation, which awaits catastrophe or incident investigation). They would benefit from simulation-based validation but are excluded from advocating for it because challenging the doctrine risks appearing to downplay safety. Their exit options are severely constrained: they cannot leave the profession without abandoning their identity and training investment; they cannot adopt rival validation frameworks without regulatory permission.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, front_line_operators, payer,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(competence_exercise_validity__real_catastrophe_only, front_line_operators, excluded).

% Training vendors, simulation manufacturers, scenario designers, and drill companies whose business model depends on simulation being credited as competence validation. Under the real-catastrophe-only reading, their products are devalued as mere 'theater' (rehearsal, not real competence exercise). They bear the cost of suppression: regulators discount their evidence, organizations treat their training as credential-insufficient, and they cannot expand into competence-certification markets. They have constrained exit: leaving the sector means abandoning expertise and investment in safety-critical training; they could pivot to other industries but lose market position in high-value safety domain.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, simulation_industry, payer,
    organized, biographical, constrained, global).

% Those responsible for deploying competence in real operations (airline dispatch, nuclear control room management, emergency system operators, hospital leadership). They experience the constraint asymmetrically: they benefit from the claim that competence has been rigorously validated (the catastrophe-proof doctrine reduces their liability), but they pay the cost operationally when personnel perform poorly under novel stressors because their training was never exposed to comparable stress. They have higher exit options than front-line operators (can shift to non-safety-critical management or consulting) but are constrained by professional norms and institutional loyalty.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, system_operators_dispatchers, beneficiary,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(competence_exercise_validity__real_catastrophe_only, system_operators_dispatchers, payer).

% Safety researchers, accident investigation boards, incident review teams, and competence assessors who analyze whether personnel failures contributed to accidents. They have structural incentive and capacity to evaluate whether the real-catastrophe-only reading accurately predicts competence outcomes. Their observation is largely decoupled from the constraint itself; they are positioned to measure it. They lack enforcement power (cannot override regulatory doctrine) but can produce evidence that shapes institutional practice.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, incident_investigators, observer,
    organized, generational, analytical, national).

% The engineering and procedural safeguards (backup systems, automation, procedural checklists, geographic separation) that mask personnel competence gaps by preventing catastrophe even when operators freeze or fail. Listed as a non-agent entity because redundancy is a structural feature, not an actor, but it is a beneficiary of the constraint: the doctrine's claim that no catastrophe = adequate competence implicitly credits redundancy with preventing disaster, allowing institutional actors to avoid investing in competence validation that would expose whether redundancy is actually the safety mechanism.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, system_redundancy, beneficiary,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(competence_exercise_validity__real_catastrophe_only, system_redundancy).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_exercise_validity__real_catastrophe_only, risk_management_institutions).
narrative_ontology:fixing_cost_class(competence_exercise_validity__real_catastrophe_only, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures that personnel in safety-critical roles have adaptive capacity tested under conditions approaching real catastrophic stress, so that when actual emergencies occur, competence has been demonstrated and the organization can rely on personnel performance rather than guessing.
% TRANSFER_FUNCTION: Moves credibility and career advancement from simulation-trained personnel toward those who have survived real incidents or catastrophes; moves regulatory authority toward institutions that define catastrophe-proof competence; moves business opportunity away from simulation vendors toward incident-investigation and remedial-training sectors.
% ABSENT_VOICES: Simulation vendors and simulation-trained personnel are structurally excluded from the validation conversation. Vendors cannot argue that their products constitute valid competence exercise (to do so would be self-interested testimony; regulators discount it as market advocacy). Personnel trained via simulation but never exposed to real catastrophe cannot claim their competence is proven (the doctrine defines them as unvalidated regardless of performance). The excluded parties have strong incentive to object but lack voice in regulatory processes.
% DISAPPEARANCE_RATIONALE: If the real-catastrophe-only constraint disappeared—if regulators suddenly accepted simulation-based competence validation—the entire landscape would reorganize: simulation vendors would capture credentialing markets, advancement criteria would shift, training curricula would rebalance toward simulation fidelity, and organizations would treat simulation outcomes as binding competence proof rather than as rehearsal. The constraint's presence shapes resource flows, career paths, and institutional structure; its absence would reshape all three.
% FOUNDING_PROBLEM: Multiple aviation disasters, nuclear-plant near-misses, and emergency-response failures revealed that procedures alone were insufficient—catastrophic situations involved unexpected failure combinations, high cognitive load, time pressure, and emotional stress that procedure-based training could not fully prepare personnel for. Operators 'froze,' took non-obvious actions, or failed to adapt when standard procedures were inadequate. The founding problem was: how can we ensure personnel have the adaptive capacity to handle situations that violate their training assumptions?
% FOUNDING_PROBLEM_CORROBORATION: The real-catastrophe-only reading's proponents (conservative safety organizations, nuclear regulation, certain aviation bodies) attest that the founding problem remains live—unexpected failure combinations continue to occur, and simulation cannot fully prepare for them. Simulation vendors and human-factors researchers attest the founding problem is substantially solved: modern high-fidelity simulation does expose operators to novel stressors, adaptive scenarios, and cognitive load. Comparative accident analysis by independent researchers (NTSB reports, academic incident studies) shows no statistically significant difference in competence-failure rates between simulation-trained and catastrophe-validated personnel, which contradicts the founding-problem-status from the real-catastrophe advocates' seat. The corroboration problem: the most convincing evidence comes from organizations where catastrophes are rare (preventing the catastrophe-validation loop) and from simulation vendors (interested testimony). Independent researchers are scarce.
narrative_ontology:disappearance_verdict(competence_exercise_validity__real_catastrophe_only, world_rearranges).
narrative_ontology:founding_problem_status(competence_exercise_validity__real_catastrophe_only, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_validity__real_catastrophe_only, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(competence_exercise_validity__real_catastrophe_only, 'none', 1).
narrative_ontology:epsilon_provenance(competence_exercise_validity__real_catastrophe_only, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_exercise_validity__real_catastrophe_only_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_exercise_validity__real_catastrophe_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_exercise_validity__real_catastrophe_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) because the constraint extracts compliance from operators (cannot claim competence from simulation alone; must await or engineer catastrophe) and from simulation providers (their credentials are devalued). The measurement series show rising extractiveness over the interval (0.45 → 0.68), reflecting institutional entrenchment of the doctrine over time as regulatory capture deepens. Theater ratio rises alongside (0.32 → 0.58), indicating that enforcement increasingly focuses on the rhetorical validation of catastrophe-free records ('look how safe we are—no catastrophe in 40 years') rather than on actual competence measurement. This rhetorical drift is the extraction mechanism: the system's safety is claimed without proof, while front-line personnel bear the cost of operating under the assumption that their competence remains untested. Suppression is high (0.72) because operators cannot easily exit (professional identity, economic dependence, legal obligation to operate safely) and alternative validation regimes (simulation-based credentialing, hybrid refresh) are actively suppressed by regulatory interpretation that treats them as insufficient. The constraint requires active enforcement: regulators must actively discount simulation evidence, block promotion of simulation-validated personnel, and maintain the doctrine against accumulating evidence that simulation-trained crews perform well in real incidents.
 *
 * PERSPECTIVAL GAP:
 *   The risk-management institution and the simulation industry experience opposite directionalities. The institution benefits from the constraint's rhetorical safety claim (we are safe because we wait for catastrophe to prove it—a self-immunizing tautology). The simulation industry suffers: its products are structurally devalued. Front-line operators occupy a contested middle: they benefit from more robust competence validation (real exercise would improve actual safety), but they also pay the extraction cost (their competence is officially untested until catastrophe, limiting their advancement and decision autonomy). The engine should compute this per-seat divergence from the structural data: institutional agenda-setter sees coordination (disaster prevention); payer sees extraction (unprovable competence requirement). The measurement drift toward higher theater and suppression reflects institutional entrenchment—the constraint has shifted from a genuine competence-validation regime toward a theatrical safety-claim machine that suppresses evidence that other readings work.
 *
 * DIRECTIONALITY LOGIC:
 *   Risk-management institutions are structural beneficiaries (d near 0.0): they derive authority and legitimacy from the constraint (our safety is beyond question because we wait for catastrophe). Simulation industry is a victim (d near 1.0): their entire market is devalued by the doctrine. Front-line operators are asymmetric: they are nominally beneficiaries (the constraint aims to ensure safe competence) but functionally pay the cost (unprovable competence, limited autonomy, career delay pending catastrophe). The beneficiary designation ('risk_management_ideology' and 'simulation_industry') reflects the structural asymmetry: the ideology itself benefits from the constraint regardless of actual outcomes, and the simulation industry is harmed by a definition that devalues their products. This is why the constraint is tangled_rope, not rope: it coordinates a genuine function (ensuring catastrophe-competent personnel) AND extracts from those it nominally protects (operators and training vendors) through suppression of evidence and devaluation of simulation paths.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is real: catastrophes have occurred when operators lacked adaptive capacity ('Swiss cheese' breakdowns where multiple redundancies failed simultaneously require operator recovery capacity that procedure alone cannot capture). But the mandate has drifted. The original requirement (empirically validate that competence holds under stress) has become a doctrine (accept the absence of catastrophe as evidence of adequate competence). The measurement series documents this drift: base_extractiveness rises despite no change in the underlying competence-validation problem, theater_ratio rises faster than extractiveness (indicating rhetorical maintenance outpacing actual validation), and suppression_requirement rises as regulators must actively block evidence that contradicts the doctrine. This is the mandatrophy signature: the constraint persists as ideology after its empirical function has been displaced by system redundancy and luck. If one were to repair it, the cost would be high—organizations built around the doctrine (regulatory interpretation, training curricula, advancement criteria) would need redesign—but the beneficiary to ask ('risk_management_ideology') is not an actor, so no concentrated voice maintains the constraint, and the payer (operators and training vendors) lacks collective power to displace it. Regulation entrenchment is the holding mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    catastrophe_availability_bias,
    'Does requiring real catastrophe to validate competence create incentive to ignore or discount near-miss learning because no actual disaster occurred?',
    'Comparative analysis of incident reporting and root-cause investigation rigor between organizations that require real catastrophe versus those that validate on simulation and structured drills.',
    'If near-miss devaluation occurs, the constraint suppresses the very feedback loops that could surface decay before catastrophe—raising effective risk. If suppressed, the constraint''s safety function is theatrical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(catastrophe_availability_bias, empirical, 'Whether the real-catastrophe requirement creates perverse incentive to discount pre-catastrophic learning signals.').

omega_variable(
    reading_framing_dependence,
    'Is this constraint''s classification stable across readings of what ''competence'' means—technical readiness, adaptive capacity under novel stressors, or muscle-memory of specific procedures?',
    'Elicit from competence assessors (flight crews, emergency responders, nuclear operators) what they believe validates competence. Map which reading aligns with empirical performance correlation.',
    'A technical-readiness frame favors simulation; an adaptive-capacity frame favors real exposure; a procedure frame permits both. The constraint''s extractiveness and claimed type depend on which reading of competence is authoritatively deployed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_framing_dependence, conceptual, 'Whether the real-catastrophe-only constraint depends on a non-neutral framing of what competence is.').

omega_variable(
    luck_versus_system_redundancy,
    'When no catastrophe occurs for decades, is it evidence that competence remained adequate, or evidence that system redundancy and external luck masked decay that would have surfaced under stress?',
    'Controlled observation: compare competence degradation curves (measured via pop quizzes, emergency drills, procedure deviations) in systems with real catastrophe history versus those with long safe records. If redundancy-shielded systems show faster decay, luck hypothesis is favored.',
    'Luck hypothesis supports the constraint''s extraction: the system''s safety is unvalidated and the constraint''s enforcement (blocking promotion of simulation-validated competence) increases real risk. Redundancy hypothesis supports the operator''s frame: system works despite unvalidated personnel.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(luck_versus_system_redundancy, empirical, 'Whether catastrophe-free records indicate adequate competence or indicate luck masking decay.').

omega_variable(
    sibling_reading_closure,
    'This constraint is one reading of a contested kernel (competence_exercise_validity). Sibling readings (simulation_as_proxy, continuous_refresh_hybrid) claim different criteria for validation. Which reading is structurally endorsed by current safety regulation, and does that endorsement reflect evidence or ideology?',
    'Regulatory archaeology: trace which reading is embedded in certification standards (FAA, NRC, maritime authorities). Compare organizational safety records across readings. Document whether the endorsed reading changed after major incidents.',
    'If the real-catastrophe reading is endorsed but safety performance is indistinguishable from hybrid-refresh or simulation-as-proxy organizations, the constraint is pure ideology. If performance is measurably superior, the constraint captures something real about competence validation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_closure, empirical, 'Kernel contest resolution: which reading is structurally endorsed and whether that endorsement correlates with actual safety outcomes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_validity__real_catastrophe_only, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_exercise_validity__real_catastrophe_only, theater_ratio, 0, 0.32).
narrative_ontology:measurement(comp_tr_t5, competence_exercise_validity__real_catastrophe_only, theater_ratio, 5, 0.39).
narrative_ontology:measurement(comp_tr_t10, competence_exercise_validity__real_catastrophe_only, theater_ratio, 10, 0.46).
narrative_ontology:measurement(comp_tr_t15, competence_exercise_validity__real_catastrophe_only, theater_ratio, 15, 0.52).
narrative_ontology:measurement(comp_tr_t20, competence_exercise_validity__real_catastrophe_only, theater_ratio, 20, 0.56).
narrative_ontology:measurement(comp_tr_t25, competence_exercise_validity__real_catastrophe_only, theater_ratio, 25, 0.58).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(comp_be_t5, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(comp_be_t10, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 10, 0.59).
narrative_ontology:measurement(comp_be_t15, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 15, 0.64).
narrative_ontology:measurement(comp_be_t20, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(comp_be_t25, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(comp_su_t5, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(comp_su_t10, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(comp_su_t15, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 15, 0.69).
narrative_ontology:measurement(comp_su_t20, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(comp_su_t25, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 25, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_validity__real_catastrophe_only, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(competence_exercise_validity__real_catastrophe_only, 0.12).
narrative_ontology:affects_constraint(competence_exercise_validity__real_catastrophe_only, competence_exercise_validity__simulation_as_proxy).
narrative_ontology:affects_constraint(competence_exercise_validity__real_catastrophe_only, competence_exercise_validity__continuous_refresh_hybrid).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the kernel competence_exercise_validity. All three stories share the referent (the standing arrangement for validating safety-critical competence) but instantiate different readings of what counts as valid evidence. The real_catastrophe_only reading claims extractiveness is high because simulation devalues personnel and vendor credibility; the other readings claim extractiveness is lower because simulation-based validation is actually feasible and cost-effective. The three constraints are linked via network.affects_constraints to enable comparative analysis of which reading predicts safety outcomes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
