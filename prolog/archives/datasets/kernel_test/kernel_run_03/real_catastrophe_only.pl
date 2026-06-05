% ============================================================================
% CONSTRAINT STORY: real_catastrophe_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_real_catastrophe_only, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: real_catastrophe_only
 *   human_readable: Real Catastrophe Requirement for Competence Validation
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   The constraint that 'only real catastrophe truly exercises competence;
 *   simulation is an insufficient substitute' appears across safety-critical
 *   industries — aviation, nuclear operations, emergency medicine, military
 *   command. It creates a structural bind: operational personnel cannot
 *   validate their competence except through high-risk real-world scenarios
 *   or actual catastrophe, while institutional actors maintain that this
 *   standard is epistemically necessary for safety. The constraint exhibits
 *   all six DR types from different perspectives. For operators trapped in
 *   the system, it is pure extraction (Snare): they bear the risk of untested
 *   scenarios while competence validation remains impossible until
 *   catastrophe. For institutional actors benefiting from conservative
 *   liability framing, it is pure coordination (Rope): the standard aligns
 *   all actors on the highest-confidence verification baseline. For reformers
 *   building alternative validation methods, it is a temporary coordination
 *   failure with a sunset (Scaffold): high-fidelity simulation is creating
 *   epistemic alternatives. For the historical institutional doctrine, it is
 *   a degraded norm (Piton): once functionally necessary when simulation
 *   fidelity was low, now maintained through inertia. The analytical observer
 *   risks naturalizing a contingent policy choice as an immutable
 *   epistemological boundary (Mountain). This constraint is one reading of
 *   the contested kernel 'competence_exercise_validity': the claim that only
 *   real-world exercise proves operational readiness. The sibling readings —
 *   'simulation_as_proxy' (competence validated through high-fidelity
 *   simulation) and 'continuous_refresh_hybrid' (competence renewed through
 *   mixed simulation + passive monitoring + limited real exercises) —
 *   represent alternative epistemic standards for the same underlying
 *   commitment.
 *
 * KEY AGENTS:
 *   - Operational Personnel (Field Operators, Emergency Responders): Primary victims (powerless/trapped) — cannot validate competence except through catastrophe; bear personal risk and career-advancement barriers
 *   - Safety System Development Engineers: Secondary victims/mixed (moderate/constrained) — coordinate safety requirements but blocked from iteration based on simulation data; constrained exit across industry
 *   - Institutional Risk Aversion / Regulatory Framework: Primary beneficiary (institutional/arbitrage) — captures credibility ('proven competence') while avoiding liability for endorsing simulation-validated training; maintains conservative standard
 *   - Historical Institutional Doctrine: Secondary beneficiary (institutional/arbitrage) — persists through organizational identity and training culture despite eroded functional value
 *   - Progressive Safety Reformers: Organized agents (organized/constrained) — building alternative validation methods (high-fidelity simulation, cross-validation frameworks); see clear exit path with sunset
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks treating contingent institutional policy as immutable epistemological boundary
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(real_catastrophe_only, 0.68).
domain_priors:suppression_score(real_catastrophe_only, 0.72).
domain_priors:theater_ratio(real_catastrophe_only, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(real_catastrophe_only, extractiveness, 0.68).
narrative_ontology:constraint_metric(real_catastrophe_only, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(real_catastrophe_only, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(real_catastrophe_only, snare).
narrative_ontology:human_readable(real_catastrophe_only, "Real Catastrophe Requirement for Competence Validation").
narrative_ontology:topic_domain(real_catastrophe_only, "safety_engineering/organizational_learning").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(real_catastrophe_only, 'fd29db6d-61a1-4f15-8bb1-3a0bfc53d1f2').
narrative_ontology:cs_created_at('fd29db6d-61a1-4f15-8bb1-3a0bfc53d1f2', '').
narrative_ontology:cs_kernel_codification('fd29db6d-61a1-4f15-8bb1-3a0bfc53d1f2', fixed_text).
narrative_ontology:cs_authority_grounding('fd29db6d-61a1-4f15-8bb1-3a0bfc53d1f2', lineage).
narrative_ontology:cs_interpretation_layer_present('fd29db6d-61a1-4f15-8bb1-3a0bfc53d1f2').
narrative_ontology:cs_kernel_id(real_catastrophe_only, competence_exercise_validity).
narrative_ontology:cs_reading_relation('fd29db6d-61a1-4f15-8bb1-3a0bfc53d1f2', simulation_as_proxy, forecloses).
narrative_ontology:cs_reading_relation('fd29db6d-61a1-4f15-8bb1-3a0bfc53d1f2', continuous_refresh_hybrid, coexists_with).
narrative_ontology:cs_axiom('fd29db6d-61a1-4f15-8bb1-3a0bfc53d1f2', foundational, simulation_epistemically_inferior_to_catastrophe).
narrative_ontology:cs_axiom_status(simulation_epistemically_inferior_to_catastrophe, holdable).
narrative_ontology:cs_axiom_grounding('fd29db6d-61a1-4f15-8bb1-3a0bfc53d1f2', simulation_epistemically_inferior_to_catastrophe, empirically_contingent).
narrative_ontology:cs_axiom('fd29db6d-61a1-4f15-8bb1-3a0bfc53d1f2', foundational, safety_requires_proven_not_plausible_competence).
narrative_ontology:cs_axiom_status(safety_requires_proven_not_plausible_competence, holdable).
narrative_ontology:cs_axiom_grounding('fd29db6d-61a1-4f15-8bb1-3a0bfc53d1f2', safety_requires_proven_not_plausible_competence, deontological).
narrative_ontology:cs_reference_frame('fd29db6d-61a1-4f15-8bb1-3a0bfc53d1f2', epistemological_catastrophe_necessity).
narrative_ontology:cs_drift_state('fd29db6d-61a1-4f15-8bb1-3a0bfc53d1f2', contemporary_high_fidelity_simulation_era, gap(axiom_overriding, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(real_catastrophe_only, risk_aversion_bias).
narrative_ontology:constraint_beneficiary(real_catastrophe_only, institutional_caution_norms).
narrative_ontology:constraint_victim(real_catastrophe_only, operational_personnel).
narrative_ontology:constraint_victim(real_catastrophe_only, safety_system_development).
narrative_ontology:constraint_victim(real_catastrophe_only, competence_renewal_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FIELD OPERATOR (SNARE) — Trapped in a system where their competence cannot be validated except through genuine catastrophe. Real-world exercises remain untrustworthy; simulations are dismissed as insufficient. Operator bears extraction: career advancement depends on surviving untested scenarios, personal risk if catastrophe occurs before competence can be proven, psychological burden of operating under epistemically closed conditions.
constraint_indexing:constraint_classification(real_catastrophe_only, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SAFETY SYSTEM DEVELOPERS (TANGLED ROPE) — Coordination function exists (engineers must communicate competence requirements to operations), but active enforcement distorts the function. Engineers experience mixed extraction: they coordinate safety design but are also blocked from iterating based on simulation data they generate. Constrained exit — can switch organizations but the constraint follows across the industry.
constraint_indexing:constraint_classification(real_catastrophe_only, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INSTITUTIONAL RISK AVERSION (ROPE) — Primary beneficiary. The constraint that 'only real catastrophe proves competence' serves institutional caution norms and regulatory conservatism. The institution has arbitrage: it can maintain public credibility ('we only declare competence when proven in extremis') while avoiding the legal/political liability of endorsing simulation-validated training. Pure coordination from this perspective: the constraint aligns all actors on the highest-confidence verification standard, which reduces institutional risk.
constraint_indexing:constraint_classification(real_catastrophe_only, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: HISTORICAL INSTITUTIONAL MYTH (PITON) — The epistemic standard ('real catastrophe is the only true test') is institutionalized through legacy regulatory culture, training doctrine, and organizational narrative. Its functional value has eroded: simulation fidelity has improved dramatically, and most operational competence is now demonstrable without catastrophe. The constraint persists through inertia and institutional identity ('we are the organization that never compromises on proven competence'). Theater ratio reflects that the continued insistence on catastrophe validation is performative — it signals institutional seriousness rather than achieving actual safety gains.
constraint_indexing:constraint_classification(real_catastrophe_only, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: PROGRESSIVE SAFETY REFORMERS (SCAFFOLD) — Organized agents (modern safety engineering, high-fidelity simulation pioneers, competence science researchers) see the catastrophe-only standard as a temporary coordination failure with an achievable sunset. Simulation frameworks are maturing; cross-validation methods (virtual + limited-scope real exercises + passive monitoring) provide epistemic alternatives. Reformers experience constrained exit (operate within institutional bounds) but see a clear exit path (regulatory evolution toward simulation-validated competence). Sunset estimated at 15-25 years as fidelity standards advance and liability frameworks update.
constraint_indexing:constraint_classification(real_catastrophe_only, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a purely epistemological stance, some residual gap between simulation and reality is irreducible: simulations always have blind spots relative to the full complexity of real systems. Under this reading, the catastrophe requirement represents an immutable constraint on what can be known about competence under genuine conditions. However, this naturalizes a contingent institutional choice — the gap between simulation fidelity and what is 'sufficient for safety' is a policy boundary, not a law of nature.
constraint_indexing:constraint_classification(real_catastrophe_only, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(real_catastrophe_only_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(real_catastrophe_only, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(real_catastrophe_only, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(real_catastrophe_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(real_catastrophe_only, TR),
    TR >= 0.70.

:- end_tests(real_catastrophe_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The catastrophe-only standard produces significant asymmetric costs: operators bear all risk of unvalidated competence; institutions capture credibility and liability protection. The extraction is not maximal (0.85+) because some real-world learning does occur in high-reliability organizations, and the constraint is not purely artificial — a genuine gap exists between simulation and reality. However, the gap has narrowed dramatically while the institutional insistence on catastrophe validation has not, revealing an increasing extraction premium. Suppression (0.72): High. Multiple barriers prevent exit: regulatory doctrine, organizational training culture, liability frameworks, and the professional commitment of safety institutions all reinforce the standard. Career advancement for operators depends on accepting the constraint; institutions face regulatory and legal penalties for deviating. Theater ratio (0.55): Moderate. The catastrophe-only standard is partially performative — it signals institutional seriousness and conservative risk management to external stakeholders (regulators, insurers, public) — but it is not entirely theater. Some real epistemic gap remains between simulation and catastrophic real-world conditions, providing partial functional justification. The rise in theater_ratio over the interval reflects that simulation fidelity improvements have made the standard increasingly performative, even as institutional rhetoric remains unchanged.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximal perspectival divergence. The operator sees catastrophe-only as pure extraction (Snare): they are trapped in a system where competence cannot be proven, and they bear all risk. The institution sees it as pure coordination (Rope): a shared commitment to the highest-confidence safety standard that protects all parties. The reformer sees it as a solvable temporary failure (Scaffold): simulation fidelity is advancing toward sufficiency, and regulatory evolution is clearing a sunset path. The historical doctrine sees it as a degraded ritual (Piton): once necessary, now performative, maintained through institutional identity. The civilizational analytical observer risks seeing it as an immutable law (Mountain): epistemological gap between simulation and reality is irreducible. None of these perspectives is wrong — they reveal different structural layers of the same constraint. The perspectival gap is the constraint's defining feature.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) determine how much effective extraction each agent experiences. Operators trapped in the system with no exit derive d ≈ 0.92 (near-total target): they bear extraction while receiving zero validation benefit. Institutional beneficiaries with arbitrage options derive d ≈ 0.08 (near-total beneficiary): they capture credibility and liability protection. Safety engineers with constrained exit (high career cost to exit) derive d ≈ 0.65 (moderate target): they coordinate real safety work but cannot use the data they generate. The engine computes chi from these d values via the sigmoid f(d), producing experienced extractiveness that reflects the agent's structural position, not just the raw base_extractiveness metric. The beneficiary's chi is negative (experiences the constraint as a benefit); the trapped operator's chi is maximum (experiences pure extraction).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by revealing that the 'only catastrophe proves competence' standard is a false summit — institutional caution dressed as epistemological necessity. The mandatrophy turns on whether the constraint is defending against an irreducible epistemic limitation (Mountain) or preserving institutional conservatism at the cost of operator risk (Snare). The omega variables are designed to dissolve the ambiguity: if simulation-validated competence in comparable domains (aviation) shows equivalent safety outcomes to catastrophe-validated competence, the mountain classification fails and the snare classification becomes unavoidable. The constraint persists because institutions benefit from the liability-protective framing ('we only declare competence when proven in the most extreme conditions') and because reformers have not yet built sufficiently credible alternatives. The mandatrophy resolves into a policy question: at what fidelity does simulation become epistemically sufficient for declaring competence? Current institutional doctrine answers 'never'; reformers answer '2-5 years away'; empirical analysis of comparable domains answers 'already achieved in aviation.' The classification system reflects this disagreement as a perspectival disagreement about whether the constraint is coordination (Rope), extraction (Snare), or temporary (Scaffold).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulation_fidelity_sufficiency_boundary,
    'What level of simulation fidelity constitutes sufficient validation of operational competence without requiring real-world catastrophe?',
    'Empirical comparison: simulation-trained competence vs catastrophe-trained competence in equivalent real-world tasks; meta-analysis of high-fidelity simulation outcomes across domains (aviation, nuclear operations, emergency medicine, military)',
    'If fidelity threshold is achievable with current technology: catastrophe requirement becomes obviously extractive (unnecessary suppression). If threshold remains unachievable: mountain classification gains legitimacy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(simulation_fidelity_sufficiency_boundary, empirical, 'Empirical threshold for simulation-fidelity equivalence').

omega_variable(
    institutional_liability_shaping,
    'To what extent does institutional liability exposure (lawsuits, regulatory penalties) drive the insistence on catastrophe-validated competence, independent of epistemic considerations?',
    'Regulatory history analysis: compare jurisdictions with different liability frameworks; interview institutional decision-makers on competence validation standards; cross-reference liability risk profiles with simulation-acceptance timelines',
    'If liability exposure is primary driver: constraint is institutional extraction (Snare) masquerading as epistemic requirement (Mountain). If genuinely epistemic: constraint is defensible standard, not extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_liability_shaping, empirical, 'Institutional liability as driver of catastrophe-only standard').

omega_variable(
    near_miss_epistemic_status,
    'Do near-catastrophes (incidents that were arrested before full catastrophe) count as equivalent to real catastrophes for competence validation purposes, or are they epistemically inferior?',
    'Regulatory and operational analysis: how do institutions treat incident data? Are near-misses used to validate or refresh competence? Comparison of competence outcomes in personnel trained via near-miss vs catastrophe-only standards',
    'If near-misses are epistemically valid: catastrophe-only standard is unnecessarily restrictive (extractive). If near-misses are treated as insufficient: suppression mechanism is even tighter than stated, increasing extraction severity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(near_miss_epistemic_status, empirical, 'Whether near-misses validate competence equivalently to catastrophes').

omega_variable(
    competence_decay_rate_under_suppression,
    'Does suppression of simulation-based learning actually protect competence renewal, or does it accelerate decay by preventing iterative refinement of skills?',
    'Longitudinal competence tracking: measure skill retention and decay rates in personnel denied simulation-based learning vs those with access; compare incident rates and error frequencies as proxy for underlying competence',
    'If suppression accelerates decay: constraint causes the safety problem it claims to prevent (paradoxical extraction). If suppression maintains competence: constraint is defensible coordination mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_decay_rate_under_suppression, empirical, 'Whether suppression of simulation learning protects or degrades competence').

omega_variable(
    kernel_reading_ambiguity,
    'Does the ''real catastrophe only'' principle represent an immutable epistemological boundary (natural law reading) or a contingent institutional policy choice that has become naturalized (false summit reading)?',
    'Regulatory and epistemic history: trace the origin of the catastrophe-only standard; identify whether it emerged from empirical findings or from institutional caution doctrine; assess whether comparable safety domains (aviation, nuclear operations) have achieved simulation-validated competence and whether their safety records differ from catastrophe-only domains',
    'If natural law: mountain classification is correct; constraint reflects genuine epistemological limit. If institutional artifact: constraint is a false summit (snare masquerading as mountain); decompose into separate extractive and epistemic components.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether catastrophe-only requirement is epistemological boundary or naturalized policy choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(real_catastrophe_only, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(real_tr_t0, real_catastrophe_only, theater_ratio, 0, 0.35).
narrative_ontology:measurement(real_tr_t10, real_catastrophe_only, theater_ratio, 10, 0.48).
narrative_ontology:measurement(real_tr_t20, real_catastrophe_only, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(real_be_t0, real_catastrophe_only, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(real_be_t10, real_catastrophe_only, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(real_be_t20, real_catastrophe_only, base_extractiveness, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(real_catastrophe_only, enforcement_mechanism).
narrative_ontology:affects_constraint(real_catastrophe_only, simulation_as_proxy).
narrative_ontology:affects_constraint(real_catastrophe_only, continuous_refresh_hybrid).

% DUAL FORMULATION NOTE:
% The 'real_catastrophe_only' reading is upstream of 'simulation_as_proxy' and 'continuous_refresh_hybrid' in the kernel family. All three are readings of the same contested kernel ('competence_exercise_validity'); each instantiates a different epistemic standard for validating operational readiness. The network relationship is not causal dependency but rather competing instantiations of the same commitment structure. Constraints appear as separate stories because their ε values are structurally distinct: catastrophe-only (ε=0.68, Snare), simulation-as-proxy (ε~0.25, Rope), and hybrid (ε~0.35, Tangled Rope) represent genuinely different structural arrangements with different beneficiary/victim profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(real_catastrophe_only, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
