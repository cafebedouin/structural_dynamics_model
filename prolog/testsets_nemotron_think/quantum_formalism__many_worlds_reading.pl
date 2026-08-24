% ============================================================================
% CONSTRAINT STORY: quantum_formalism__many_worlds_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quantum_formalism__many_worlds_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: quantum_formalism__many_worlds_reading
 *   human_readable: Many-Worlds Reading of Quantum Formalism
 *   domain: philosophy_of_physics/quantum_foundations/interpretive_epistemology
 *
 * SUMMARY:
 *   The many-worlds reading (Everett 1957, developed by DeWitt, Wallace,
 *   Carroll, and others) asserts that the universal wavefunction evolves
 *   unitarily at all times, measurement is an emergent phenomenon arising
 *   from decoherence, and all outcomes are realized in distinct branches. It
 *   presents itself as the literal reading of the formalism — no collapse
 *   postulate, no observer privilege, global determinism. The ontological
 *   cost is the proliferation of worlds. Beneficiaries are those who gain a
 *   deterministic, observer-free physics and a dissolution of the measurement
 *   problem. The extraction, if any, is the ontological extravagance imposed
 *   on interpreters who must accept infinite branches. The reading has gained
 *   institutional traction over time (decreasing extractiveness, stable low
 *   theater), but the Born rule derivation remains contested.
 *
 * KEY AGENTS:
 *   - everett_wallace_carroll: Primary agenda_setters (institutional/analytical) — developed and defend the reading
 *   - determinist_physicists: Primary beneficiaries (organized/mobile) — gain determinism without hidden variables
 *   - observer_eliminativists: Beneficiaries (organized/mobile) — gain observer-independent ontology
 *   - measurement_problem_dissolvers: Beneficiaries (analytical/mobile) — gain dissolution of measurement problem via decoherence
 *   - ontological_minimalists: Payers (moderate/constrained) — bear the cost of infinite worlds
 *   - copenhagen_adherents: Excluded (institutional/constrained) — hold rival reading with collapse postulate
 *   - pilot_wave_adherents: Excluded (organized/constrained) — hold rival reading with hidden variables
 *   - quantum_foundations_researchers: Observers (analytical/analytical) — evaluate all readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quantum_formalism__many_worlds_reading, 0.35).
domain_priors:suppression_score(quantum_formalism__many_worlds_reading, 0.15).
domain_priors:theater_ratio(quantum_formalism__many_worlds_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quantum_formalism__many_worlds_reading, mountain).
narrative_ontology:human_readable(quantum_formalism__many_worlds_reading, "Many-Worlds Reading of Quantum Formalism").
narrative_ontology:topic_domain(quantum_formalism__many_worlds_reading, "philosophy_of_physics/quantum_foundations/interpretive_epistemology").

domain_priors:emerges_naturally(quantum_formalism__many_worlds_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quantum_formalism__many_worlds_reading, '2fd83083-0ce2-42b7-9297-1030ae87e29f').
narrative_ontology:cs_kernel_codification('2fd83083-0ce2-42b7-9297-1030ae87e29f', formalized).
narrative_ontology:cs_authority_grounding('2fd83083-0ce2-42b7-9297-1030ae87e29f', expertise).
narrative_ontology:cs_interpretation_layer_present('2fd83083-0ce2-42b7-9297-1030ae87e29f').
narrative_ontology:cs_reading_relation('2fd83083-0ce2-42b7-9297-1030ae87e29f', quantum_formalism__copenhagen_reading, coexists_with).
narrative_ontology:cs_reading_relation('2fd83083-0ce2-42b7-9297-1030ae87e29f', quantum_formalism__pilot_wave_reading, coexists_with).
narrative_ontology:cs_axiom('2fd83083-0ce2-42b7-9297-1030ae87e29f', foundational, universal_wavefunction_ontic).
narrative_ontology:cs_axiom_status(universal_wavefunction_ontic, holdable).
narrative_ontology:cs_axiom_grounding('2fd83083-0ce2-42b7-9297-1030ae87e29f', universal_wavefunction_ontic, empirically_contingent).
narrative_ontology:cs_axiom('2fd83083-0ce2-42b7-9297-1030ae87e29f', foundational, measurement_is_decoherence).
narrative_ontology:cs_axiom_status(measurement_is_decoherence, holdable).
narrative_ontology:cs_axiom_grounding('2fd83083-0ce2-42b7-9297-1030ae87e29f', measurement_is_decoherence, empirically_contingent).
narrative_ontology:cs_axiom('2fd83083-0ce2-42b7-9297-1030ae87e29f', foundational, all_outcomes_realized).
narrative_ontology:cs_axiom_status(all_outcomes_realized, holdable).
narrative_ontology:cs_axiom_grounding('2fd83083-0ce2-42b7-9297-1030ae87e29f', all_outcomes_realized, deontological).
narrative_ontology:cs_axiom('2fd83083-0ce2-42b7-9297-1030ae87e29f', secondary, born_rule_from_branch_counting).
narrative_ontology:cs_axiom_status(born_rule_from_branch_counting, holdable).
narrative_ontology:cs_axiom_grounding('2fd83083-0ce2-42b7-9297-1030ae87e29f', born_rule_from_branch_counting, instrumental).
narrative_ontology:cs_reference_frame('2fd83083-0ce2-42b7-9297-1030ae87e29f', universal_unitary_evolution).
narrative_ontology:cs_drift_state('2fd83083-0ce2-42b7-9297-1030ae87e29f', contemporary_decoherence_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('2fd83083-0ce2-42b7-9297-1030ae87e29f', '2026-08-20T14:30:00Z').
narrative_ontology:cs_kernel_id(quantum_formalism__many_worlds_reading, quantum_formalism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quantum_formalism__many_worlds_reading, determinist_physicists).
narrative_ontology:constraint_beneficiary(quantum_formalism__many_worlds_reading, observer_eliminativists).
narrative_ontology:constraint_beneficiary(quantum_formalism__many_worlds_reading, measurement_problem_dissolvers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(quantum_formalism__many_worlds_reading, everett_wallace_carroll).
narrative_ontology:constraint_victim(quantum_formalism__many_worlds_reading, ontological_minimalists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Developed and defend the many-worlds reading. Everett originated it (1957); DeWitt popularized it; Wallace gave it decision-theoretic rigor; Carroll advocates it publicly. They set the interpretive agenda, define the reading's commitments, and benefit from its growing acceptance in quantum foundations. Exit is arbitrage-grade: they can shift to other interpretations or topics without professional cost.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, everett_wallace_carroll, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(quantum_formalism__many_worlds_reading, everett_wallace_carroll, beneficiary).

% Physicists who value determinism and want to avoid hidden variables. They gain a deterministic quantum mechanics without pilot-wave's nonlocality or preferred basis. Exit is mobile: they can adopt other deterministic interpretations (pilot-wave) or accept indeterminism if persuaded.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, determinist_physicists, beneficiary,
    organized, biographical, mobile, global).

% Physicists and philosophers who reject observer-privileged physics. They gain an observer-free ontology where measurement is just decoherence. Exit is mobile: they can accept Copenhagen's observer role or pilot-wave's particle ontology if the many-worlds cost becomes too high.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, observer_eliminativists, beneficiary,
    organized, biographical, mobile, global).

% Researchers who see the measurement problem as the central obstacle in quantum foundations. They gain a dissolution (not solution) of the problem via decoherence and branching. Exit is analytical: they evaluate the reading on its technical merits.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, measurement_problem_dissolvers, beneficiary,
    analytical, biographical, analytical, global).

% Physicists and philosophers who apply ontological parsimony (Occam's razor) as a constraint. They bear the cost of infinite worlds — an extravagant ontology that multiplies entities beyond what they consider necessary. Exit is constrained: the reading's dominance in high-profile foundations work and popular science makes professional engagement with it nearly unavoidable, and alternatives (Copenhagen, pilot-wave) have their own ontological costs.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, ontological_minimalists, payer,
    moderate, biographical, constrained, global).

% Hold the Copenhagen reading: collapse is fundamental, measurement is irreducible, observers are privileged, indeterminism is real. They are excluded from the many-worlds coordination function because their core commitments (collapse, observer) are directly denied. Exit is constrained: Copenhagen remains the textbook default and has massive institutional inertia, but the many-worlds reading's growth challenges its hegemony.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, copenhagen_adherents, excluded,
    institutional, generational, constrained, global).

% Hold the pilot-wave reading: particles have definite positions guided by a physical wavefunction; determinism via hidden variables; classical ontology restored. They are excluded because their core commitment (particle ontology, single outcome) contradicts many-worlds' branching. Exit is constrained: pilot-wave has a dedicated but smaller research community; switching to many-worlds means accepting ontological extravagance they reject.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, pilot_wave_adherents, excluded,
    organized, generational, constrained, global).

% Researchers who evaluate all interpretations on technical merits: empirical adequacy, theoretical coherence, ontological cost, explanatory power. They neither collect nor pay; they assess. Exit is analytical: they follow the evidence and argument wherever it leads.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, quantum_foundations_researchers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quantum_formalism__many_worlds_reading, diffuse).
narrative_ontology:fixing_cost_class(quantum_formalism__many_worlds_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a deterministic, observer-free, single-postulate account of quantum mechanics: the Schrödinger equation applies universally, measurement emerges from decoherence, probabilities emerge from branch counting or decision theory. Solves the coordination problem of 'how to interpret the formalism without adding postulates.'
% TRANSFER_FUNCTION: Moves ontological cost (acceptance of infinite worlds/branches) from the theory to the interpreter. The theory gives up nothing (no collapse postulate, no hidden variables, no observer); the interpreter pays the ontological extravagance. In return, the interpreter receives determinism, observer elimination, and measurement problem dissolution.
% ABSENT_VOICES: Those who find infinite worlds metaphysically unacceptable but are excluded from the 'just the math' framing: ontological minimalists who cannot accept the cost, philosophers who reject the identity conditions for branching worlds, physicists who see the Born rule derivation as circular. They are structurally excluded because the reading's core premise (universal unitarity) makes their objection appear as 'adding a postulate' rather than 'rejecting an extravagance.'
% DISAPPEARANCE_RATIONALE: If the many-worlds reading vanished overnight, the quantum foundations landscape would rearrange: Copenhagen would regain default status, pilot-wave would gain adherents seeking determinism, and the measurement problem would return as an open problem requiring a new solution. But the formalism (Schrödinger equation, Hilbert space) would be unchanged — physics continues. The dispute is whether the interpretive landscape depends on this reading's existence.
% FOUNDING_PROBLEM: The measurement problem in quantum mechanics: the formalism has two incompatible evolution rules (unitary Schrödinger evolution and non-unitary collapse), no account of what constitutes a measurement, and an observer-privileged ontology. The reading was built to dissolve this by making measurement emergent and collapse unnecessary.
% FOUNDING_PROBLEM_CORROBORATION: The measurement problem's persistence is attested by all three readings' proponents (each claims to solve it differently), by Bell's theorem (which shows no local hidden variables can reproduce QM, pushing toward either collapse or many-worlds), by decoherence theory (which explains appearance of collapse but not single outcomes), and by the continued controversy in quantum foundations conferences and literature. No party claims the problem is solved; all three readings remain live contenders.
narrative_ontology:disappearance_verdict(quantum_formalism__many_worlds_reading, contested).
narrative_ontology:founding_problem_status(quantum_formalism__many_worlds_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quantum_formalism__many_worlds_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quantum_formalism__many_worlds_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quantum_formalism__many_worlds_reading, 0.35, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quantum_formalism__many_worlds_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, ExtMetricName, E),
    domain_priors:suppression_score(quantum_formalism__many_worlds_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(quantum_formalism__many_worlds_reading),
    narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(quantum_formalism__many_worlds_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35) reflects the ontological cost of infinite worlds — a real cost but not material extraction. It has decreased over time as decoherence theory matured and the reading gained professional acceptance. Suppression (0.15) is low: the reading persists by argument and evidence, not coercion; alternatives are not structurally suppressed. Theater ratio (0.08) is minimal: the reading's core function (deterministic unitary evolution) is genuine, not performative. Accessibility collapse (0.72) is high: once you accept the premises (universal unitarity, no collapse), rival readings appear to add unjustified postulates. Resistance (0.45) is moderate: Copenhagen and pilot-wave remain live, well-defended positions. The claimed_type is mountain — the reading asserts it is the natural consequence of the formalism itself.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (determinists, observer-eliminativists), the reading is a Mountain: it delivers coordination (determinism, no collapse, no observer) with negligible extraction — the ontological cost is the price of reality. From the payer seat (ontological minimalists), the same structure extracts heavily: infinite worlds is an extravagant ontology imposed to save determinism. The excluded seats (Copenhagen, pilot-wave adherents) experience the reading as a rival that claims Mountain status while they see it as a constructed alternative. The engine computes this divergence from the structural data: beneficiaries have low d (mobile exit, organized power), payers have higher d (constrained exit, moderate power), excluded have high d (constrained exit, institutional power).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (determinist_physicists, observer_eliminativists, measurement_problem_dissolvers) collect the coordination gains: a single universal law, no measurement postulate, no observer privilege. They have organized power and mobile exit options (can work in other interpretations if persuaded). Payers (ontological_minimalists) bear the ontological cost: they must accept infinite worlds as the price of the reading's determinism. They have moderate power and constrained exit (the reading's growing dominance in foundations makes alternatives professionally risky). Excluded agents (copenhagen_adherents, pilot_wave_adherents) are structurally excluded from the reading's coordination function — their interpretive commitments are incompatible. They have institutional/organized power but constrained exit within their research programmes. Observers (quantum_foundations_researchers) sit at d=0.5 with analytical exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (measurement problem in quantum mechanics) is live and contested. The reading was built to dissolve the measurement problem by making measurement derivative. Decoherence theory has substantially delivered on this, but the Born rule derivation remains incomplete. The mandate has not atrophied — the reading continues to solve its founding problem better than alternatives by most metrics, though the ontological cost remains a live objection. No mandatrophy resolution needed; the constraint's function is still its justification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mountain_vs_constructed_interpretation,
    'Is the many-worlds reading a genuine Mountain (the natural, unavoidable consequence of taking the Schrödinger equation literally) or a constructed interpretation that benefits determinist and observer-eliminativist commitments?',
    'Historical analysis of whether the reading was derived from the formalism alone or motivated by prior philosophical commitments; cross-comparison with sibling readings that share the same formalism but produce different ontologies.',
    'If constructed, the reading triggers false_summit_mountain reclassification to tangled_rope — the ontological extravagance (infinite worlds) functions as extraction from those who must accept it, while beneficiaries (determinists) collect the coordination gain (no collapse, no observer). If genuine Mountain, the extravagance is not extraction but the cost of reality itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mountain_vs_constructed_interpretation, conceptual, 'Natural-law vs. constructed-status ambiguity for a reading that claims to be ''just the math''').

omega_variable(
    born_rule_derivation_status,
    'Can the Born rule be derived from branch counting in a way that is both mathematically rigorous and non-circular, or does the derivation implicitly assume the probabilistic structure it claims to explain?',
    'Technical assessment of decision-theoretic (Deutsch-Wallace), envariance (Zurek), or self-locating uncertainty (Vaidman-Sebens-Carroll) derivations for circularity; consensus in quantum foundations community.',
    'If the Born rule derivation fails, the reading''s coordination function (recovering quantum probabilities without postulate) collapses, increasing effective extraction (the reading demands ontological extravagance without delivering the promised probability recovery). If it succeeds, extraction drops toward zero.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(born_rule_derivation_status, empirical, 'Whether the probability-recovery coordination function is genuinely fulfilled').

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is the many_worlds_reading of the quantum_formalism kernel. Sibling readings: copenhagen_reading (collapse as physical process, irreducible indeterminism), pilot_wave_reading (definite positions guided by pilot wave, deterministic hidden variables). The disagreement is located on: (1) whether measurement is fundamental or derivative, (2) whether the observer is eliminable, (3) whether determinism is global, (4) the ontological cost of the reading.',
    'The kernel/reading decomposition is structural — each reading instantiates a distinct constraint with its own ε, beneficiaries, and classification. The engine evaluates them independently. This omega records the committer-frame metadata for cross-reading analysis.',
    'Enables contamination tracking across the quantum_formalism constraint family. If one reading''s purity degrades (e.g., copenhagen_reading''s collapse postulate becomes empirically untenable), the system can predict effects on sibling readings'' credibility.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Commiter-frame metadata: kernel_id=quantum_formalism, reading_id=many_worlds_reading, siblings=copenhagen_reading pilot_wave_reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quantum_formalism__many_worlds_reading, 1957, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qmw_tr_t1957, quantum_formalism__many_worlds_reading, theater_ratio, 1957, 0.1).
narrative_ontology:measurement(qmw_tr_t1970, quantum_formalism__many_worlds_reading, theater_ratio, 1970, 0.08).
narrative_ontology:measurement(qmw_tr_t1985, quantum_formalism__many_worlds_reading, theater_ratio, 1985, 0.07).
narrative_ontology:measurement(qmw_tr_t2000, quantum_formalism__many_worlds_reading, theater_ratio, 2000, 0.08).
narrative_ontology:measurement(qmw_tr_t2010, quantum_formalism__many_worlds_reading, theater_ratio, 2010, 0.08).
narrative_ontology:measurement(qmw_tr_t2025, quantum_formalism__many_worlds_reading, theater_ratio, 2025, 0.08).

% Extraction over time
narrative_ontology:measurement(qmw_be_t1957, quantum_formalism__many_worlds_reading, base_extractiveness, 1957, 0.65).
narrative_ontology:measurement(qmw_be_t1970, quantum_formalism__many_worlds_reading, base_extractiveness, 1970, 0.55).
narrative_ontology:measurement(qmw_be_t1985, quantum_formalism__many_worlds_reading, base_extractiveness, 1985, 0.45).
narrative_ontology:measurement(qmw_be_t2000, quantum_formalism__many_worlds_reading, base_extractiveness, 2000, 0.4).
narrative_ontology:measurement(qmw_be_t2010, quantum_formalism__many_worlds_reading, base_extractiveness, 2010, 0.38).
narrative_ontology:measurement(qmw_be_t2025, quantum_formalism__many_worlds_reading, base_extractiveness, 2025, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(qmw_su_t1957, quantum_formalism__many_worlds_reading, suppression_requirement, 1957, 0.3).
narrative_ontology:measurement(qmw_su_t1970, quantum_formalism__many_worlds_reading, suppression_requirement, 1970, 0.2).
narrative_ontology:measurement(qmw_su_t1985, quantum_formalism__many_worlds_reading, suppression_requirement, 1985, 0.15).
narrative_ontology:measurement(qmw_su_t2000, quantum_formalism__many_worlds_reading, suppression_requirement, 2000, 0.12).
narrative_ontology:measurement(qmw_su_t2010, quantum_formalism__many_worlds_reading, suppression_requirement, 2010, 0.1).
narrative_ontology:measurement(qmw_su_t2025, quantum_formalism__many_worlds_reading, suppression_requirement, 2025, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quantum_formalism__many_worlds_reading, information_standard).
narrative_ontology:boltzmann_floor_override(quantum_formalism__many_worlds_reading, 0.02).
narrative_ontology:affects_constraint(quantum_formalism__many_worlds_reading, quantum_formalism__copenhagen_reading).
narrative_ontology:affects_constraint(quantum_formalism__many_worlds_reading, quantum_formalism__pilot_wave_reading).

% DUAL FORMULATION NOTE:
% Quantum formalism kernel decomposes into three constraint stories: many_worlds_reading (this file, claimed mountain, ε=0.35), copenhagen_reading (claimed mountain by adherents, ε≈0.2 but with observer-privilege extraction), pilot_wave_reading (claimed mountain by adherents, ε≈0.3 with hidden-variable overhead). The upstream formalism constrains all three; downstream, the readings compete for interpretive dominance. This reading's claim to be 'just the math' influences the legitimacy conditions of the siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quantum_formalism__many_worlds_reading, institutional, 0.15).
constraint_indexing:directionality_override(quantum_formalism__many_worlds_reading, moderate, 0.6).
constraint_indexing:directionality_override(quantum_formalism__many_worlds_reading, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
