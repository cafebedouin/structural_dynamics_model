% ============================================================================
% CONSTRAINT STORY: quantum_formalism__many_worlds_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: quantum_formalism__many_worlds_reading
 *   human_readable: Many-Worlds Reading of the Quantum Formalism (Everettian Interpretation)
 *   domain: philosophy_of_physics/quantum_foundations
 *
 * SUMMARY:
 *   This story instantiates the many-worlds (Everettian) reading of the
 *   quantum formalism kernel: the claim that the universal wavefunction
 *   evolves deterministically via the Schrödinger equation without exception,
 *   that apparent measurement collapse is decoherence-induced branching, and
 *   that all outcomes are realized in causally disjoint but equally real
 *   branches. This is treated as a distinct constraint from the Copenhagen
 *   reading (collapse as physical, irreducible indeterminism) and the
 *   pilot-wave reading (hidden variables, deterministic particle
 *   trajectories) — the three readings share a kernel (the formalism and its
 *   unresolved measurement problem) but instantiate structurally different
 *   ontological commitments, different beneficiary/victim structures, and
 *   different classifications. Do not average across them; each is its own
 *   file linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - everettian_theorists: institutional agenda-setters and beneficiaries who administer what counts as a serious objection to the reading
 *   - decoherence_program_researchers: technical beneficiaries whose real physics gets an ontological payoff from the reading
 *   - collapse_model_researchers: targets who bear reduced funding priority for pursuing an experimentally distinguishable rival
 *   - graduate_students_committed_early: trapped payers whose careers are sunk into defending the reading
 *   - experimentalists_seeking_falsification_targets: payers denied a decisive experiment because the reading is constructed to reproduce identical statistics to rivals
 *   - philosophers_of_physics_observers: analytical observers documenting the argument structure without institutional stake
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quantum_formalism__many_worlds_reading, 0.42).
domain_priors:suppression_score(quantum_formalism__many_worlds_reading, 0.28).
domain_priors:theater_ratio(quantum_formalism__many_worlds_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quantum_formalism__many_worlds_reading, tangled_rope).
narrative_ontology:human_readable(quantum_formalism__many_worlds_reading, "Many-Worlds Reading of the Quantum Formalism (Everettian Interpretation)").
narrative_ontology:topic_domain(quantum_formalism__many_worlds_reading, "philosophy_of_physics/quantum_foundations").

domain_priors:requires_active_enforcement(quantum_formalism__many_worlds_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quantum_formalism__many_worlds_reading, '6991bf78-2eac-4376-99b8-90db74a4053d').
narrative_ontology:cs_kernel_codification('6991bf78-2eac-4376-99b8-90db74a4053d', formalized).
narrative_ontology:cs_authority_grounding('6991bf78-2eac-4376-99b8-90db74a4053d', expertise).
narrative_ontology:cs_interpretation_layer_present('6991bf78-2eac-4376-99b8-90db74a4053d').
narrative_ontology:cs_reading_relation('6991bf78-2eac-4376-99b8-90db74a4053d', quantum_formalism__copenhagen_reading, forecloses).
narrative_ontology:cs_reading_relation('6991bf78-2eac-4376-99b8-90db74a4053d', quantum_formalism__pilot_wave_reading, coexists_with).
narrative_ontology:cs_axiom('6991bf78-2eac-4376-99b8-90db74a4053d', foundational, measurement_is_ontologically_derivative).
narrative_ontology:cs_axiom_status(measurement_is_ontologically_derivative, holdable).
narrative_ontology:cs_axiom_grounding('6991bf78-2eac-4376-99b8-90db74a4053d', measurement_is_ontologically_derivative, empirically_contingent).
narrative_ontology:cs_axiom('6991bf78-2eac-4376-99b8-90db74a4053d', foundational, all_branches_equally_real).
narrative_ontology:cs_axiom_status(all_branches_equally_real, holdable).
narrative_ontology:cs_axiom_grounding('6991bf78-2eac-4376-99b8-90db74a4053d', all_branches_equally_real, conventional).
narrative_ontology:cs_reference_frame('6991bf78-2eac-4376-99b8-90db74a4053d', everett_1957_relative_state_formulation).
narrative_ontology:cs_drift_state('6991bf78-2eac-4376-99b8-90db74a4053d', post_decoherence_program_maturation, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('6991bf78-2eac-4376-99b8-90db74a4053d', '').
narrative_ontology:cs_kernel_id(quantum_formalism__many_worlds_reading, quantum_formalism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quantum_formalism__many_worlds_reading, everettian_theorists).
narrative_ontology:constraint_beneficiary(quantum_formalism__many_worlds_reading, decoherence_program_researchers).
narrative_ontology:constraint_beneficiary(quantum_formalism__many_worlds_reading, quantum_information_unitarity_advocates).
narrative_ontology:constraint_victim(quantum_formalism__many_worlds_reading, collapse_model_researchers).
narrative_ontology:constraint_victim(quantum_formalism__many_worlds_reading, graduate_students_committed_early).
narrative_ontology:constraint_victim(quantum_formalism__many_worlds_reading, experimentalists_seeking_falsification_targets).
narrative_ontology:constraint_vindicates(quantum_formalism__many_worlds_reading, unitary_quantum_mechanics_completeness).
narrative_ontology:constraint_vindicates(quantum_formalism__many_worlds_reading, wavefunction_realism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold chairs, journal gatekeeping roles, and grant-review seats built around defending unitary-only quantum mechanics. They administer the interpretive framework — deciding what counts as a serious objection (preferred basis, probability derivation) versus a dismissible one — while also being the primary intellectual beneficiaries of the reading's ontological program continuing to be treated as the default 'no-collapse' baseline in foundations discourse and popular science.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, everettian_theorists, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(quantum_formalism__many_worlds_reading, everettian_theorists, beneficiary).

% Their technical results (einselection, decoherence timescales) are real physics independent of any interpretation, but the many-worlds reading is what converts that technical program into a complete ontological story, giving decoherence theorists a philosophical payoff beyond their equations. They can exit into instrumentalist readings of their own math without much cost, which insulates them from the interpretive contest.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, decoherence_program_researchers, beneficiary,
    organized, generational, mobile, global).

% Quantum computing and quantum information theory run on unitarity as a working assumption; many-worlds gives that working assumption a metaphysical home and rhetorical reinforcement ('the branches are just where the computation happens'), even though the field's actual predictions don't require adjudicating the interpretation question.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, quantum_information_unitarity_advocates, beneficiary,
    organized, biographical, mobile, global).

% Work on objective-collapse programs (GRW, CSL) that are experimentally distinguishable from many-worlds in principle. They face reduced funding priority, fewer faculty lines, and dismissal as pursuing an 'unnecessary' physical mechanism, because the dominant Everettian framing treats collapse as an explanatorily superfluous add-on rather than a live physical hypothesis worth testing.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, collapse_model_researchers, payer,
    moderate, biographical, constrained, global).

% Build dissertations and early publication records on Everettian probability derivations (decision-theoretic, self-locating uncertainty) inside advisor networks organized around the reading. Switching interpretive commitments mid-career costs them advisor relationships, letters of recommendation, and legible publication venues — the framework's plausibility is partly sustained by careers already sunk into defending it.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, graduate_students_committed_early, payer,
    powerless, biographical, trapped, national).

% Want sharp, testable predictions that would discriminate many-worlds from rivals, but the reading's central ontological commitment (branching without collapse) is constructed to reproduce identical statistics to Copenhagen for all practical experiments — leaving them with no distinguishing experiment to run and no clear path to resolving the interpretive dispute empirically.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, experimentalists_seeking_falsification_targets, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(quantum_formalism__many_worlds_reading, experimentalists_seeking_falsification_targets, excluded).

% Standard textbooks mostly teach the Copenhagen-flavored operational recipe and treat interpretation as an optional afterthought; students rarely encounter the full stakes of the kernel contest, so a voice that would notice the framework's contestability is structurally absent from the room where the contest is adjudicated.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, physics_undergraduate_curricula, excluded,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_non_agent(quantum_formalism__many_worlds_reading, physics_undergraduate_curricula).

% Analyze the probability problem (why the Born rule, given equal-amplitude branching), the preferred-basis problem, and personal-identity puzzles across splitting without a stake in which reading wins institutionally; they document the argumentative structure rather than adjudicate it by fiat.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, philosophers_of_physics_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quantum_formalism__many_worlds_reading, everettian_theorists).
narrative_ontology:fixing_cost_class(quantum_formalism__many_worlds_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, mathematically minimal formalism (just the Schrödinger equation, nothing added) that lets physicists agree on what the theory says exists without needing to posit an extra collapse postulate or hidden variables — a genuine parsimony-driven coordination around 'take the equations literally.'
% TRANSFER_FUNCTION: Moves institutional legitimacy, funding priority, and career-building publication space toward researchers who accept unitary-only ontology and away from researchers pursuing experimentally distinguishable alternatives (objective collapse) or being denied a decisive experiment to run at all.
% ABSENT_VOICES: Collapse-model experimentalists and philosophers of personal identity who find branching ontology incoherent for practical decision theory would object to the framework's default status, but journal review pipelines and textbook selection committees are staffed predominantly by physicists already committed to unitary quantum mechanics.
% DISAPPEARANCE_RATIONALE: If the many-worlds reading vanished as an institutionally favored interpretation overnight, the underlying mathematics (unitary evolution, decoherence, einselection) would be entirely unchanged — those are established physics. What would rearrange is grant panels, tenure narratives built on decision-theoretic derivations of the Born rule, and popular-science framing; Everettian theorists dispute that this constitutes real rearrangement (they'd say the mathematics IS the interpretation), while collapse researchers and philosophers of physics would say a great deal of institutional weight currently resting on an unresolved metaphysical claim would simply lift.
% FOUNDING_PROBLEM: The measurement problem: standard quantum mechanics needs a collapse postulate to explain why we observe single, definite outcomes rather than superpositions, but no one could specify a physical mechanism, timescale, or threshold for when collapse occurs — Everett (1957) proposed removing the postulate entirely rather than fixing it.
% FOUNDING_PROBLEM_CORROBORATION: Everettian theorists attest the problem is solved in principle (no ad hoc collapse needed) but the derivation of the Born rule from branching alone remains disputed even within the Everettian camp (Wallace, Deutsch, Vaidman decision-theoretic approaches vs. critics like Kent and Maudlin who argue the probability problem is unsolved). Philosophers of physics outside the Everettian program (e.g., collapse-model theorists, Bell-inequality-focused pilot-wave advocates) attest the founding problem remains fully live and unresolved by any current interpretation, many-worlds included — no consensus corroboration exists from outside the reading's own advocates that the measurement problem has actually been dissolved rather than relocated.
narrative_ontology:disappearance_verdict(quantum_formalism__many_worlds_reading, contested).
narrative_ontology:founding_problem_status(quantum_formalism__many_worlds_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quantum_formalism__many_worlds_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'f1436bd4937f864097dabaad92b27bd9b6eec212', '2026-08-03',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(quantum_formalism__many_worlds_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quantum_formalism__many_worlds_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quantum_formalism__many_worlds_reading_tests).
:- end_tests(quantum_formalism__many_worlds_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.42) is moderate rather than high: the reading's mathematical core is genuinely minimal and parsimonious (zero extra postulates), which is real coordination value, but its institutional deployment channels career and funding resources toward adherents and away from experimentally-motivated rivals, and the probability/preferred-basis problems remain live cover for treating an unresolved metaphysical claim as settled physics. Suppression (0.28) is real but modest — no one is coercively barred from pursuing collapse models, but grant panels and hiring committees exert soft pressure. Theater ratio rises modestly over the interval (0.08 to 0.22) as decision-theoretic Born-rule derivations proliferate without actually resolving the probability problem, suggesting increasing performative sophistication defending a still-open question. Accessibility collapse (0.35) is low-moderate: rival readings remain fully articulable and defended by serious researchers, unlike a genuine mountain. Resistance (0.62) is substantial — the collapse and pilot-wave communities actively contest the Everettian default, and the probability problem remains a genuine open wound within the Everettian program itself (Wallace vs. Kent, Vaidman vs. Maudlin).
 *
 * PERSPECTIVAL GAP:
 *   From the Everettian theorist's seat, the reading is essentially a Rope: pure parsimony, remove an unmotivated postulate, take the math literally, no coercion required because the equations themselves compel it. From the collapse-researcher's seat, the same formal minimality operates as a Tangled Rope: the parsimony argument is real coordination, but it is deployed to marginalize a testable rival research program and to treat contested foundations as settled physics in textbooks and hiring decisions. The engine should compute these divergently from the same structural data — the claim (tangled_rope, chosen deliberately below the theorist's self-perception) states what I believe is structurally true; the metrics describe what I believe is descriptively true; they are not reconciled to each other.
 *
 * DIRECTIONALITY LOGIC:
 *   Everettian theorists sit closest to the beneficiary end: they set the interpretive agenda in foundations journals, and the reading's institutional dominance (especially in quantum information circles) directly subsidizes their research programs. Decoherence and quantum-information researchers benefit indirectly — their technical results don't require the ontological commitment, but the reading gives those results a metaphysical home, which is why their exit options remain mobile (they can retreat to instrumentalism without cost). Collapse-model researchers and trapped graduate students sit closest to the target end: they bear the diffuse but real cost of pursuing or having pursued a program the dominant reading treats as an unnecessary physical hypothesis. Experimentalists are targets in a different sense — denied the decisive test that would let the field resolve the underlying dispute at all.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (no physical mechanism for collapse) is genuinely still live by the corroboration of parties outside the Everettian program — this is not a dead mandate propped up by institutional inertia. What prevents mislabeling this as pure extraction is that the coordination function (mathematical minimality, avoiding an ad hoc postulate) is real and endorsed even by critics of the reading's institutional deployment. What prevents mislabeling it as pure coordination is the asymmetric career and funding consequences borne by collapse-model researchers and trapped graduate students, sustained by active administration of what counts as a serious objection — hence tangled_rope rather than rope or mountain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    born_rule_derivation_adequacy,
    'Can the Born rule''s quantitative probabilities be genuinely derived from branching structure alone (decision-theoretic or self-locating-uncertainty approaches), or does every derivation smuggle in an unexplained probability postulate by another name?',
    'Formal philosophical resolution of whether Wallace/Deutsch decision-theoretic derivations are circular (assuming rational agents already weight branches by amplitude) versus genuinely non-circular; no empirical experiment can adjudicate this because all readings agree on observed statistics.',
    'If the derivation is circular, many-worlds has not actually eliminated an unexplained postulate — it has relocated it, undermining the parsimony argument that is the reading''s primary coordination justification and shifting the classification toward pure extraction (snare) rather than tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(born_rule_derivation_adequacy, conceptual, 'Whether the Born rule is genuinely derived or merely relocated within the many-worlds framework.').

omega_variable(
    preferred_basis_problem_status,
    'Does decoherence theory fully solve the preferred-basis problem (why worlds branch along position/pointer-basis rather than arbitrary bases), or does it merely push the problem into the choice of system-environment decomposition?',
    'Technical resolution within decoherence theory itself — whether einselection criteria are basis-independent in a principled way or require an unexplained choice of decomposition.',
    'An unresolved preferred-basis problem would mean the ''worlds'' in many-worlds are not as objectively determinate as the reading claims, weakening the ontological-realism axiom below.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(preferred_basis_problem_status, empirical, 'Whether decoherence genuinely fixes a preferred basis for branching.').

omega_variable(
    kernel_reading_underdetermination,
    'Is the choice between many-worlds, Copenhagen, and pilot-wave readings a genuinely empirical question (resolvable by future physics) or a permanently underdetermined metaphysical choice given identical observational predictions?',
    'No known experiment distinguishes the three readings at current or foreseeable energy/precision scales for the core measurement-problem cases; resolution would require either a novel testable prediction unique to one reading (e.g., detectable decoherence deviations) or philosophical argument establishing underdetermination is permanent.',
    'If permanently underdetermined, the institutional dominance of any one reading (including this one) is not truth-tracking and the extraction component (career/funding channeling toward the dominant reading) cannot be justified by appeal to superior evidential support — strengthening the tangled_rope classification over any claim to mountain-like inevitability.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Whether the three-way kernel contest is empirically resolvable or permanently underdetermined.').

omega_variable(
    false_mountain_risk,
    'Do popular-science and some textbook presentations increasingly present many-worlds as the settled or default scientific answer to the measurement problem, in a way that functions as false-mountain framing even though the specialist literature acknowledges the reading remains contested?',
    'Content analysis of textbook and popular-science treatment over time versus specialist survey data (e.g., physicist polling on interpretation preference, which historically shows no consensus).',
    'If popular framing outpaces specialist consensus, this constitutes a false-summit dynamic in public science communication even though this story''s own claimed_type is tangled_rope rather than mountain — worth tracking as a related but distinct constraint (public science communication of the reading) rather than folding into this story''s epsilon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_mountain_risk, empirical, 'Gap between specialist contestation and public/pedagogical presentation of many-worlds as settled.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quantum_formalism__many_worlds_reading, 1957, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(quan_tr_t1957, quantum_formalism__many_worlds_reading, theater_ratio, 1957, 0.08).
narrative_ontology:measurement_basis(quan_tr_t1957, observed).
narrative_ontology:measurement(quan_tr_t1980, quantum_formalism__many_worlds_reading, theater_ratio, 1980, 0.1).
narrative_ontology:measurement_basis(quan_tr_t1980, observed).
narrative_ontology:measurement(quan_tr_t1995, quantum_formalism__many_worlds_reading, theater_ratio, 1995, 0.13).
narrative_ontology:measurement_basis(quan_tr_t1995, observed).
narrative_ontology:measurement(quan_tr_t2010, quantum_formalism__many_worlds_reading, theater_ratio, 2010, 0.17).
narrative_ontology:measurement_basis(quan_tr_t2010, observed).
narrative_ontology:measurement(quan_tr_t2018, quantum_formalism__many_worlds_reading, theater_ratio, 2018, 0.2).
narrative_ontology:measurement_basis(quan_tr_t2018, observed).
narrative_ontology:measurement(quan_tr_t2025, quantum_formalism__many_worlds_reading, theater_ratio, 2025, 0.22).
narrative_ontology:measurement_basis(quan_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(quan_be_t1957, quantum_formalism__many_worlds_reading, base_extractiveness, 1957, 0.15).
narrative_ontology:measurement_basis(quan_be_t1957, observed).
narrative_ontology:measurement(quan_be_t1980, quantum_formalism__many_worlds_reading, base_extractiveness, 1980, 0.22).
narrative_ontology:measurement_basis(quan_be_t1980, observed).
narrative_ontology:measurement(quan_be_t1995, quantum_formalism__many_worlds_reading, base_extractiveness, 1995, 0.3).
narrative_ontology:measurement_basis(quan_be_t1995, observed).
narrative_ontology:measurement(quan_be_t2010, quantum_formalism__many_worlds_reading, base_extractiveness, 2010, 0.38).
narrative_ontology:measurement_basis(quan_be_t2010, observed).
narrative_ontology:measurement(quan_be_t2018, quantum_formalism__many_worlds_reading, base_extractiveness, 2018, 0.4).
narrative_ontology:measurement_basis(quan_be_t2018, observed).
narrative_ontology:measurement(quan_be_t2025, quantum_formalism__many_worlds_reading, base_extractiveness, 2025, 0.42).
narrative_ontology:measurement_basis(quan_be_t2025, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(quantum_formalism__many_worlds_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quantum_formalism__many_worlds_reading, information_standard).
narrative_ontology:boltzmann_floor_override(quantum_formalism__many_worlds_reading, 0.05).
narrative_ontology:affects_constraint(quantum_formalism__many_worlds_reading, copenhagen_reading).
narrative_ontology:affects_constraint(quantum_formalism__many_worlds_reading, pilot_wave_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the quantum_formalism kernel (copenhagen_reading, many_worlds_reading, pilot_wave_reading). All three share the same underlying formalism and the same unresolved measurement problem (the founding_problem in six_questions) but diverge in beneficiary/victim structure, suppression mechanism, and epsilon. copenhagen_reading retains 'observer' and 'collapse' as physically fundamental with an unresolved cut; pilot_wave_reading restores classical determinism via hidden variables at the cost of manifest non-locality and configuration-space ontology; many_worlds_reading (this story) achieves formal minimality at the cost of ontological extravagance (unobservable branches) and an unresolved probability-derivation problem. Each story must be read independently; do not average epsilon across the three.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
