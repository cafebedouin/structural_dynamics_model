% ============================================================================
% CONSTRAINT STORY: competence_exercise_requirement__catastrophe_as_necessary_anchor
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_exercise_requirement__catastrophe_as_necessary_anchor, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: competence_exercise_requirement__catastrophe_as_necessary_anchor
 *   human_readable: Catastrophe as Necessary Anchor for Competence Maintenance
 *   domain: safety_engineering/organizational_learning/high_reliability_organizations
 *
 * SUMMARY:
 *   The constraint story models the claim that competence in high-reliability
 *   domains (aviation, nuclear, surgical, maritime) can only be maintained
 *   through exposure to real catastrophic events or near-misses. This reading
 *   asserts an irreducible gap between simulation and reality: 'muscle
 *   memory' forged in genuine jeopardy cannot be replicated by 'knowing
 *   about' jeopardy in simulators. The claim structures licensing
 *   requirements (e.g., minimum flight hours in actual aircraft), training
 *   budgets, hiring practices, and regulatory frameworks. The coordination
 *   function is genuine: maintaining competence for rare, high-stakes events
 *   is a real collective-action problem. The extraction is asymmetric: the
 *   belief that only real events work justifies exposing frontline operators
 *   and the public to avoidable risk, creates moats for incumbents with 'real
 *   experience,' and suppresses investment in simulation fidelity. The
 *   constraint requires active enforcement through licensing rules, insurance
 *   mandates, and cultural gatekeeping that privileges 'real' over
 *   'simulated' experience.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0.72).
domain_priors:suppression_score(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0.65).
domain_priors:theater_ratio(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, extractiveness, 0.72).
narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_requirement__catastrophe_as_necessary_anchor, tangled_rope).
narrative_ontology:human_readable(competence_exercise_requirement__catastrophe_as_necessary_anchor, "Catastrophe as Necessary Anchor for Competence Maintenance").
narrative_ontology:topic_domain(competence_exercise_requirement__catastrophe_as_necessary_anchor, "safety_engineering/organizational_learning/high_reliability_organizations").

domain_priors:requires_active_enforcement(competence_exercise_requirement__catastrophe_as_necessary_anchor).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_requirement__catastrophe_as_necessary_anchor, 'cb7c2a6b-72cb-499c-b15e-9c1f3d9150d9').
narrative_ontology:cs_kernel_codification('cb7c2a6b-72cb-499c-b15e-9c1f3d9150d9', distributed).
narrative_ontology:cs_authority_grounding('cb7c2a6b-72cb-499c-b15e-9c1f3d9150d9', lineage).
narrative_ontology:cs_interpretation_layer_present('cb7c2a6b-72cb-499c-b15e-9c1f3d9150d9').
narrative_ontology:cs_reading_relation('cb7c2a6b-72cb-499c-b15e-9c1f3d9150d9', competence_exercise_requirement__simulation_as_adequate_exercise, forecloses).
narrative_ontology:cs_reading_relation('cb7c2a6b-72cb-499c-b15e-9c1f3d9150d9', competence_exercise_requirement__hybrid_dependency, influences).
narrative_ontology:cs_axiom('cb7c2a6b-72cb-499c-b15e-9c1f3d9150d9', foundational, irreducible_jeopardy_gap).
narrative_ontology:cs_axiom_status(irreducible_jeopardy_gap, holdable).
narrative_ontology:cs_axiom_grounding('cb7c2a6b-72cb-499c-b15e-9c1f3d9150d9', irreducible_jeopardy_gap, empirically_contingent).
narrative_ontology:cs_axiom('cb7c2a6b-72cb-499c-b15e-9c1f3d9150d9', foundational, muscle_memory_requires_genuine_stakes).
narrative_ontology:cs_axiom_status(muscle_memory_requires_genuine_stakes, holdable).
narrative_ontology:cs_axiom_grounding('cb7c2a6b-72cb-499c-b15e-9c1f3d9150d9', muscle_memory_requires_genuine_stakes, empirically_contingent).
narrative_ontology:cs_reference_frame('cb7c2a6b-72cb-499c-b15e-9c1f3d9150d9', operational_lineage_tradition).
narrative_ontology:cs_drift_state('cb7c2a6b-72cb-499c-b15e-9c1f3d9150d9', contemporary_simulation_fidelity_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('cb7c2a6b-72cb-499c-b15e-9c1f3d9150d9', '').
narrative_ontology:cs_kernel_id(competence_exercise_requirement__catastrophe_as_necessary_anchor, competence_exercise_requirement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__catastrophe_as_necessary_anchor, incumbent_operators_with_real_experience).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__catastrophe_as_necessary_anchor, regulators_using_real_experience_metrics).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__catastrophe_as_necessary_anchor, organizations_justifying_risk_exposure_as_training).
narrative_ontology:constraint_victim(competence_exercise_requirement__catastrophe_as_necessary_anchor, frontline_operators_exposed_to_avoidable_risk).
narrative_ontology:constraint_victim(competence_exercise_requirement__catastrophe_as_necessary_anchor, public_and_passengers_bearing_catastrophe_consequences).
narrative_ontology:constraint_victim(competence_exercise_requirement__catastrophe_as_necessary_anchor, simulation_and_training_industry).
narrative_ontology:constraint_victim(competence_exercise_requirement__catastrophe_as_necessary_anchor, new_entrants_without_real_experience_access).
narrative_ontology:constraint_vindicates(competence_exercise_requirement__catastrophe_as_necessary_anchor, operational_experience_irreducibility_thesis).
narrative_ontology:constraint_vindicates(competence_exercise_requirement__catastrophe_as_necessary_anchor, muscle_memory_vs_knowing_about_distinction).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold the 'real experience' credential that the constraint makes scarce and valuable. They benefit from hiring barriers against new entrants, regulatory favor for incumbents, and the ability to frame risk exposure as 'competence maintenance.' Their exit is arbitrage-grade: they can move across domains, leverage their credential, and are not dependent on any single organization's catastrophe exposure.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, incumbent_operators_with_real_experience, beneficiary,
    powerful, biographical, arbitrage, global).

% Set licensing and currency requirements that privilege real-flight-hours, actual aircraft time, and live operational experience over simulator hours. They justify this as safety assurance; the metrics are defensible in court and public inquiries. They face pressure to modernize but changing standards creates legitimacy risk. Their exit is analytical: they observe the system but are not personally subject to the risk exposure they mandate.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, regulators_using_real_experience_metrics, agenda_setter,
    institutional, generational, analytical, national).

% Operate in high-reliability domains and use the catastrophe-as-necessary belief to justify operational practices that expose crews to higher risk (e.g., deferring maintenance to 'train on real anomalies,' reducing simulator budgets, accepting marginal weather). They benefit from lower training costs and operational flexibility. Their exit is mobile: they can shift practices if regulation changes, but currently the belief validates their cost-saving choices.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, organizations_justifying_risk_exposure_as_training, agenda_setter,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(competence_exercise_requirement__catastrophe_as_necessary_anchor, organizations_justifying_risk_exposure_as_training, beneficiary).

% Bear the actual risk of catastrophes and near-misses that the constraint treats as 'necessary exercise.' Their competence is the object of the constraint, but they have no voice in whether real events are required. Exit is constrained: leaving the profession means losing seniority, pension, and identity; moving to a simulation-heavy role is rare and often seen as 'giving up real flying.' Many internalize the belief that 'you can't learn this in a box.'
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, frontline_operators_exposed_to_avoidable_risk, payer,
    moderate, biographical, constrained, global).

% Bear the ultimate consequences when the constraint's logic fails — when 'necessary exercise' becomes actual catastrophe. They have no voice in the competence maintenance regime, no exit from systemic risk (must fly, must live near nuclear plants), and no representation in the standard-setting bodies. Their situation is pure extraction with zero coordination benefit.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, public_and_passengers_bearing_catastrophe_consequences, payer,
    powerless, immediate, trapped, global).

% Develop high-fidelity simulators, VR training, and synthetic environments that could substitute for some real-event exposure. Their market is structurally suppressed by regulatory requirements for 'real' hours and by the cultural belief that simulation is 'not the same.' They advocate for simulation equivalence standards but face coordinated resistance from incumbents and regulators. Exit is mobile: they can pivot to other domains (gaming, entertainment, military) but the high-reliability civil market is capped by the constraint.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, simulation_and_training_industry, payer,
    organized, biographical, mobile, global).

% Aspiring operators (pilots, surgeons, reactor operators) who must acquire the 'real experience' credential before they can be hired, but cannot get it without being hired. They pay for training, accept low-wage entry positions with higher risk exposure, and compete for scarce 'real experience' slots. Exit is constrained: they have invested heavily in domain-specific human capital; switching domains wastes that investment.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, new_entrants_without_real_experience_access, payer,
    powerless, biographical, constrained, global).

% Study competence decay, simulation fidelity, and training transfer. They produce evidence on whether simulation can substitute for real events, but their findings are filtered through the constraint's belief system. They have analytical exit (can study other domains) but their influence on the constraint is indirect and slow.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, safety_researchers_and_academics, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintaining individual and collective competence for rare, high-stakes catastrophic events in domains where failure is unacceptable and real events are infrequent. The coordination problem: how to keep skills sharp when the events that exercise them happen once per career or less.
% TRANSFER_FUNCTION: Moves catastrophe risk exposure from organizations/regulators (who would bear the cost of better simulation) onto frontline operators and the public (who bear the risk of 'necessary' real events), while moving the 'real experience' credential — a scarce, gatekeeping asset — to incumbents and new entrants who survive the exposure.
% ABSENT_VOICES: The public and passengers are structurally excluded from standard-setting bodies and licensing boards. Frontline operators have representative bodies (unions, professional associations) but these often endorse the 'real experience' belief due to identity fusion. Simulation industry lobbyists are heard but dismissed as self-interested. Patients in healthcare, communities near nuclear plants, and residents under flight paths have no seat at the table where 'necessary exercise' is defined.
% DISAPPEARANCE_RATIONALE: If the catastrophe-as-necessary belief vanished overnight, licensing would shift to demonstrated competence via simulation (as in some military and nuclear domains), training budgets would reallocate from real-flight-hours to simulator fidelity, incumbents would lose the 'real experience' moat, and risk exposure would drop as organizations no longer justify marginal operations as 'training.' The high-reliability ecosystem would reorganize around simulation-equivalence standards.
% FOUNDING_PROBLEM: Early aviation and high-reliability domains had no simulation capability; competence could only be built through actual operations. The founding problem was: how to maintain skills for emergencies that rarely occur, when the only available exercise is the real thing. The arrangement (privileging real experience) was built because no alternative existed.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (no simulation alternative) is attested by historical records: early flight training had no simulators; nuclear control rooms had no replicas. However, the STATUS of the founding problem (whether it persists) is contested: the simulation industry and progressive regulators (e.g., FAA's AQP program, EASA's evidence-based training) attest the problem is substantially solved — high-fidelity simulation now exists. Incumbent operators and traditional regulators attest it is still live — 'simulation is not the same.' No neutral arbiter has settled this; the contention is structural.
narrative_ontology:disappearance_verdict(competence_exercise_requirement__catastrophe_as_necessary_anchor, world_rearranges).
narrative_ontology:founding_problem_status(competence_exercise_requirement__catastrophe_as_necessary_anchor, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_requirement__catastrophe_as_necessary_anchor, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(competence_exercise_requirement__catastrophe_as_necessary_anchor, 'none', 1).
narrative_ontology:epsilon_provenance(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_exercise_requirement__catastrophe_as_necessary_anchor_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_exercise_requirement__catastrophe_as_necessary_anchor, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_exercise_requirement__catastrophe_as_necessary_anchor_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the constraint transfers real catastrophe risk onto operators and the public while the benefits (competence maintenance, institutional legitimacy) accrue to organizations and regulators. Suppression (0.65) is substantial because regulatory frameworks, insurance structures, and professional identity all actively penalize simulation-equivalence claims. Theater ratio (0.45) is moderate: organizations perform commitment to simulation (buying simulators, running drills) but the real competence credential remains 'real experience,' making much simulation investment performative. Accessibility collapse (0.55) is partial: alternatives exist and improve yearly, but the belief system dismisses them as fundamentally inadequate. Resistance (0.48) is moderate: simulation advocates and some progressive regulators push back, but the belief is deeply embedded in professional identity and regulatory path-dependence.
 *
 * PERSPECTIVAL GAP:
 *   The incumbent operator seat computes as rope-like (they benefit from the coordination function and the moat). The frontline operator seat computes as snare-like (they bear extraction with constrained exit). The regulator seat computes as tangled_rope (genuine safety coordination + institutional extraction from risk-bearing labor). The public seat computes as pure snare (no benefit, trapped exit). The simulation industry seat computes as snare (suppressed by the constraint). The engine computes this divergence from the structural data; the authored claim (tangled_rope) reflects the constraint's aggregate structure, not any single seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent operators with real experience are structural beneficiaries (d near 0.15): they hold the scarce credential the constraint creates, giving them hiring leverage and regulatory favor. Regulators using real-experience metrics are agenda_setters with d near 0.25: they administer the constraint and benefit from clear, defensible standards, but face some pressure to modernize. Frontline operators are payers (d near 0.85): they bear the catastrophe risk the constraint justifies, with constrained exit (changing domains means losing seniority). The public/passengers are payers with trapped exit (d near 0.95): they bear ultimate consequences with no voice. Simulation industry is payer (d near 0.7): their market is suppressed by the constraint. New entrants are payers with constrained exit (d near 0.8): they cannot access the 'real experience' credential without first surviving the constraint's risk exposure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (maintaining competence for rare catastrophes) is live and genuinely hard. But the catastrophe-as-necessary reading has outlived its empirical adequacy: simulation fidelity has improved dramatically, and industries with extremely rare real events (nuclear) maintain competence without them. The constraint persists because the 'real experience' credential became a legitimating myth for institutional power structures — it is now a piton-in-the-making where the coordination function is real but the extraction mechanism (requiring real catastrophes) has atrophied into institutional inertia and identity fusion. The mandatrophy is unresolved: the arrangement persists because no single party bears enough cost to fix it (operators are trapped, public is excluded, regulators face legitimacy costs for change, incumbents benefit).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulation_substitutability_empirical,
    'Can high-fidelity simulation with structured debriefing adequately substitute for real catastrophic events in maintaining competence, or is there an irreducible gap?',
    'Longitudinal studies comparing competence decay curves in organizations with simulation-only vs. real-event exposure; natural experiments from industries where real events are extremely rare (e.g., nuclear power) but simulation is advanced.',
    'If simulation can adequately substitute, the catastrophe-as-necessary claim is empirically false and the constraint is extractive cover; if an irreducible gap exists, the coordination function is genuine and the extraction is the price of that coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_substitutability_empirical, empirical, 'Whether the core empirical claim of this reading holds under systematic evidence.').

omega_variable(
    institutional_interest_vs_genuine_belief,
    'Does the catastrophe-as-necessary belief persist because it is genuinely held by practitioners, or because it serves institutional interests (incumbent moats, regulatory simplicity, risk justification)?',
    'Trace the advocacy history: who funds the ''real experience'' requirements in licensing? Who opposes simulation equivalence standards? Compare stated beliefs of operators vs. their revealed preferences in training budget allocation.',
    'If institutional interest drives the belief, the constraint is a snare with coordination cover; if genuine practitioner conviction drives it, the constraint is a tangled rope with authentic coordination function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_interest_vs_genuine_belief, conceptual, 'Motivational structure behind the catastrophe-as-necessary belief.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of simulation alternatives structural (regulatory requirements privileging real experience, insurance mandates) or internalized (operators and managers genuinely believe simulation cannot work, identity-fused with ''real experience'' credential)?',
    'Post-exit suppression trajectory: if operators who move to simulation-heavy domains still resist simulation equivalence, the suppression is partially internalized; if regulatory barriers alone maintain the constraint, it is structural.',
    'If internalized, effective suppression is higher than structural measures suggest — the constraint travels with the agent. If structural, reform via regulatory change is more tractable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for simulation alternatives.').

omega_variable(
    kernel_reading_framing_ambiguity,
    'Is the competence_exercise_requirement kernel best framed as (a) an operational practice kernel grounded in lineage of operational tradition, or (b) a legitimacy kernel grounded in the extraction of risk-bearing labor?',
    'Compare how sibling readings characterize the same kernel: if simulation_as_adequate_exercise treats the kernel as a solvable engineering problem while catastrophe_as_necessary_anchor treats it as an immutable law of expertise, the framing divergence signals a legitimacy/extraction layer above the operational kernel.',
    'If framing (b) holds, the catastrophe reading is not a genuine operational claim but a legitimacy claim that extracts from risk-bearers; this would reclassify the constraint toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_framing_ambiguity, conceptual, 'Alternative framings of the competence_exercise_requirement kernel and their classification consequences.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cer_cata_tr_t0, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 0, 0.3).
narrative_ontology:measurement(cer_cata_tr_t8, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 8, 0.38).
narrative_ontology:measurement(cer_cata_tr_t16, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 16, 0.42).
narrative_ontology:measurement(cer_cata_tr_t24, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 24, 0.45).

% Extraction over time
narrative_ontology:measurement(cer_cata_be_t0, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(cer_cata_be_t8, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 8, 0.62).
narrative_ontology:measurement(cer_cata_be_t16, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 16, 0.68).
narrative_ontology:measurement(cer_cata_be_t24, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 24, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(cer_cata_su_t0, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(cer_cata_su_t8, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 8, 0.55).
narrative_ontology:measurement(cer_cata_su_t16, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 16, 0.6).
narrative_ontology:measurement(cer_cata_su_t24, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 24, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_requirement__catastrophe_as_necessary_anchor, identity_coordination).
narrative_ontology:boltzmann_floor_override(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0.08).
narrative_ontology:affects_constraint(competence_exercise_requirement__catastrophe_as_necessary_anchor, simulation_fidelity_investment_trajectory).
narrative_ontology:affects_constraint(competence_exercise_requirement__catastrophe_as_necessary_anchor, licensing_requirements_real_experience_hours).
narrative_ontology:affects_constraint(competence_exercise_requirement__catastrophe_as_necessary_anchor, high_reliability_organizations_training_budget_allocation).

% DUAL FORMULATION NOTE:
% Part of the competence_exercise_requirement constraint family. This reading (catastrophe_as_necessary_anchor) claims ε=0.72; sibling simulation_as_adequate_exercise would claim ε≈0.15 (simulation works, constraint is near-rope); sibling hybrid_dependency would claim ε≈0.4 (partial extraction). The ε values differ because the readings disagree on the empirical substitutability of simulation for real events — they are structurally distinct constraints linked by network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(competence_exercise_requirement__catastrophe_as_necessary_anchor, institutional, 0.25).
constraint_indexing:directionality_override(competence_exercise_requirement__catastrophe_as_necessary_anchor, powerful, 0.15).
constraint_indexing:directionality_override(competence_exercise_requirement__catastrophe_as_necessary_anchor, moderate, 0.7).
constraint_indexing:directionality_override(competence_exercise_requirement__catastrophe_as_necessary_anchor, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
