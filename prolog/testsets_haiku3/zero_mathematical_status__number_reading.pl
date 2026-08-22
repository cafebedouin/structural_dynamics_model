% ============================================================================
% CONSTRAINT STORY: zero_mathematical_status__number_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_zero_mathematical_status__number_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: zero_mathematical_status__number_reading
 *   human_readable: Zero as a Number with Arithmetic Operations (Brahmagupta Reading)
 *   domain: mathematics/philosophy_of_mathematics/history_of_ideas
 *
 * SUMMARY:
 *   This constraint instantiates one reading of the contested kernel
 *   'zero_mathematical_status': the reading that zero is a full number with
 *   defined arithmetic operations (Brahmagupta's rules, ~628 CE). This
 *   reading treats zero not as a notational placeholder or ontological
 *   impossibility, but as an entity that participates in a consistent
 *   algebraic system. The constraint's claim is mountain (naturally emergent
 *   from the structure of quantity); the authored metrics reflect low
 *   extractiveness and suppression because the reading has achieved
 *   near-universal acceptance in contemporary mathematics and faces minimal
 *   active resistance. However, a false-summit omega is included:
 *   beneficiaries exist (mathematical practitioners, institutions), and their
 *   benefit may suggest the constraint is institutionally defended rather
 *   than naturally inevitable. The reading coexists with alternative readings
 *   (parmenidean_rejection, placeholder_reading) in the sense that
 *   philosophical objections persist but lack institutional power. This
 *   constraint is one story in a constraint family of three: the kernel
 *   'zero_mathematical_status' splits into three readings (number_reading,
 *   parmenidean_rejection, placeholder_reading), each with different ε values
 *   and stakeholder structures. Link this story to its siblings via
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - Mathematical practitioners: beneficiaries of zero-as-number status; use it daily in algebra, calculus, computation
 *   - Algebraic system coherence: vindicated proposition; zero-as-number enables complete, consistent ring and field structures
 *   - Calculus foundations: vindicated proposition; limits, continuity, and differentiability depend on zero-as-number
 *   - Historical transmission: institutional agenda-setter; Brahmagupta's codification, Islamic mathematicians' adoption, Renaissance European integration
 *   - Parmenidean rejectionist: excluded seat; philosophically objects that nothing cannot exist as a number
 *   - Notationalist skeptic: excluded seat; views zero as notation only, not as a number with arithmetic status
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zero_mathematical_status__number_reading, 0.15).
domain_priors:suppression_score(zero_mathematical_status__number_reading, 0.08).
domain_priors:theater_ratio(zero_mathematical_status__number_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zero_mathematical_status__number_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(zero_mathematical_status__number_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(zero_mathematical_status__number_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zero_mathematical_status__number_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(zero_mathematical_status__number_reading, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zero_mathematical_status__number_reading, mountain).
narrative_ontology:human_readable(zero_mathematical_status__number_reading, "Zero as a Number with Arithmetic Operations (Brahmagupta Reading)").
narrative_ontology:topic_domain(zero_mathematical_status__number_reading, "mathematics/philosophy_of_mathematics/history_of_ideas").

domain_priors:emerges_naturally(zero_mathematical_status__number_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zero_mathematical_status__number_reading, '434f4ffa-304b-4953-a83b-0b35f066d424').
narrative_ontology:cs_kernel_codification('434f4ffa-304b-4953-a83b-0b35f066d424', formalized).
narrative_ontology:cs_authority_grounding('434f4ffa-304b-4953-a83b-0b35f066d424', expertise).
narrative_ontology:cs_interpretation_layer_present('434f4ffa-304b-4953-a83b-0b35f066d424').
narrative_ontology:cs_reading_relation('434f4ffa-304b-4953-a83b-0b35f066d424', zero_mathematical_status__parmenidean_rejection, forecloses).
narrative_ontology:cs_reading_relation('434f4ffa-304b-4953-a83b-0b35f066d424', zero_mathematical_status__placeholder_reading, coexists_with).
narrative_ontology:cs_axiom('434f4ffa-304b-4953-a83b-0b35f066d424', foundational, zero_is_genuine_number).
narrative_ontology:cs_axiom_status(zero_is_genuine_number, holdable).
narrative_ontology:cs_axiom_grounding('434f4ffa-304b-4953-a83b-0b35f066d424', zero_is_genuine_number, empirically_contingent).
narrative_ontology:cs_axiom('434f4ffa-304b-4953-a83b-0b35f066d424', foundational, brahmagupta_rules_define_zero_arithmetic).
narrative_ontology:cs_axiom_status(brahmagupta_rules_define_zero_arithmetic, holdable).
narrative_ontology:cs_axiom_grounding('434f4ffa-304b-4953-a83b-0b35f066d424', brahmagupta_rules_define_zero_arithmetic, empirically_contingent).
narrative_ontology:cs_reference_frame('434f4ffa-304b-4953-a83b-0b35f066d424', zero_as_arithmetic_entity).
narrative_ontology:cs_drift_state('434f4ffa-304b-4953-a83b-0b35f066d424', contemporary_formalized_mathematics, gap(stable, minor, true)).
narrative_ontology:cs_created_at('434f4ffa-304b-4953-a83b-0b35f066d424', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(zero_mathematical_status__number_reading, zero_mathematical_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zero_mathematical_status__number_reading, mathematical_practitioners).
narrative_ontology:constraint_beneficiary(zero_mathematical_status__number_reading, algebraic_system_coherence).
narrative_ontology:constraint_beneficiary(zero_mathematical_status__number_reading, calculus_foundations).
narrative_ontology:constraint_vindicates(zero_mathematical_status__number_reading, additive_identity_axiom).
narrative_ontology:constraint_vindicates(zero_mathematical_status__number_reading, multiplicative_annihilation_rule).
narrative_ontology:constraint_vindicates(zero_mathematical_status__number_reading, completeness_of_real_number_system).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Mathematicians, engineers, physicists, and computer scientists use zero as a number in algebra, calculus, linear algebra, and computation. They benefit from Brahmagupta's rules (a+0=a, a×0=0) which ensure zero behaves consistently in all arithmetic operations. This enables the development of field and ring structures, calculus (limits, derivatives), and modern computational systems. Exit from this reading would require abandoning entire domains of mathematics.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__number_reading, mathematical_practitioners, beneficiary,
    organized, generational, analytical, universal).

% Philosophers in the Parmenidean tradition (being entails something, nothing cannot exist) who maintain that zero cannot be a legitimate mathematical number because it violates ontological principles. They would argue for a mathematical system without zero-as-number, relying instead on explicit enumeration or other mechanisms. They are excluded from mainstream mathematical discourse and peer-reviewed mathematics journals; their position is not heard where mathematical standards are set.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__number_reading, philosophical_rejectionist, excluded,
    moderate, biographical, constrained, universal).

% Those who view zero as a notational placeholder (useful for positional number systems like Hindu-Arabic notation) rather than as a number with intrinsic arithmetic properties. They would argue that zero is a symbol for the empty place, not an entity that participates in arithmetic. This position is historically residual but epistemically persistent in some foundational and constructivist contexts. They face institutional pressure to adopt the number_reading.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__number_reading, notationalist_skeptic, excluded,
    moderate, biographical, constrained, universal).

% Universities, peer-reviewed journals, textbooks, and professional mathematics societies that codify zero-as-number as the standard framework. They set curricula, accept or reject papers, and define what counts as legitimate mathematics. The transmission of Brahmagupta's rules through Islamic mathematicians (Al-Khwarizmi, Al-Kindi) to Renaissance Europe, and their institutionalization in textbooks and universities, made this reading the default expectation for any mathematical system.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__number_reading, mathematical_institutions, agenda_setter,
    institutional, generational, analytical, universal).

% The abstract structural requirement that algebraic systems (groups, rings, fields) possess a unique additive identity element. This is not an agent but a vindicated mathematical proposition whose truth depends on treating zero as a number obeying Brahmagupta's rules. Every field and ring in modern algebra presupposes zero-as-number.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__number_reading, algebraic_system_coherence, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(zero_mathematical_status__number_reading, algebraic_system_coherence).

% The foundational requirement that calculus (limits, continuity, derivatives, integrals) rests on a consistent zero in the real and complex numbers. Calculus's entire logical structure presupposes zero-as-number with defined arithmetic properties. Without this, the epsilon-delta formalization of limits, continuity, and differentiability would require fundamental reconstruction.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__number_reading, calculus_foundations, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(zero_mathematical_status__number_reading, calculus_foundations).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(zero_mathematical_status__number_reading, mathematical_institutions).
narrative_ontology:fixing_cost_class(zero_mathematical_status__number_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Integrates zero into a unified number system in which all arithmetic operations are defined consistently. Solves the problem of representing 'nothing' or 'absence' as a quantity that can be manipulated mathematically without paradox. Enables algebraic closure, additive identity properties, and the derivation of all higher mathematics from a consistent foundation.
% TRANSFER_FUNCTION: No material transfer or rent collection. The constraint transfers epistemic authority: recognizing zero as a number grants mathematical practitioners the authority to use it in proofs, theorems, and calculations without philosophical objection. It transfers to mathematical institutions the authority to teach and enforce this reading as the standard framework.
% ABSENT_VOICES: Parmenidean philosophers and notationalist skeptics are structurally excluded from the institutions where this reading dominates (mathematics departments, peer-reviewed journals, mathematical curricula). They would object that zero cannot be a number because being requires something, and nothing is ontologically incoherent. They have no representation in the bodies that define mathematical standards.
% DISAPPEARANCE_RATIONALE: If this reading disappeared and zero lost its status as a number, mathematics as currently practiced could not exist. Algebra would require complete reformulation, calculus would collapse, and computational systems would fracture. However, the *logical truths* expressed by Brahmagupta's rules (a+0=a, a×0=0) would remain true — they would just require different framing and justification outside the context of zero-as-number. The constraint is the institutional recognition and codification of zero as a number, not the underlying mathematical truths themselves. If institutions abandoned this reading, the truths would persist but the framework for using them would reorganize entirely.
% FOUNDING_PROBLEM: How can a mathematical system represent and manipulate 'nothing' or 'absence' as a quantity without logical contradiction? Early Hindu mathematics (Brahmagupta, ~628 CE) solved this by treating zero not as an absence or non-entity, but as a number with defined arithmetic properties: a number such that a+0=a (identity in addition) and a×0=0 (annihilation in multiplication). This allowed positional notation and arithmetic to function with a consistent placeholder that was also a genuine mathematical object, avoiding both the Aristotelian prohibition on non-being and the Parmenidean objection that nothing cannot exist.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary foundational mathematics still solves this founding problem, though in different terms: set theory defines zero as the empty set and derives arithmetic from it; analysis defines the real numbers with zero as the unique additive identity. Historians of mathematics (Joseph 2011, Kaplan 1999) and foundational mathematicians (Dedekind, Peano, Hilbert) corroborate that treating zero as a number was necessary to develop modern mathematics. No corroboration from non-mathematical sources is possible for a claim about a mathematical entity, but the consistency of the solution across 1400 years and multiple foundational frameworks (set theory, analysis, category theory) constitutes external corroboration through independent approaches converging on the same conclusion.
narrative_ontology:disappearance_verdict(zero_mathematical_status__number_reading, world_unchanged).
narrative_ontology:founding_problem_status(zero_mathematical_status__number_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zero_mathematical_status__number_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(zero_mathematical_status__number_reading, 'none', 1).
narrative_ontology:epsilon_provenance(zero_mathematical_status__number_reading, 0.15, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(zero_mathematical_status__number_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(zero_mathematical_status__number_reading, ExtMetricName, E),
    domain_priors:suppression_score(zero_mathematical_status__number_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(zero_mathematical_status__number_reading),
    narrative_ontology:constraint_metric(zero_mathematical_status__number_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(zero_mathematical_status__number_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(zero_mathematical_status__number_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored as low (0.15 at t=2026) because the constraint imposes no direct cost on practitioners or systems; it enables rather than restricts. Suppression is minimal (0.08 at t=2026) because the reading has achieved institutional dominance and meets no organized resistance from within mathematics itself. Theater_ratio is very low (0.05) because the constraint's operation is functional rather than performative — zero-as-number is used, not theatrically maintained. Accessibility_collapse is very high (0.92) because once a mathematical system adopts zero-as-number, rejecting it becomes nearly impossible without reconstructing the entire system. Resistance is low (0.12) because very few contemporary mathematicians actively resist the reading; resistance exists only in philosophical communities outside mathematics. The temporal trajectory shows suppression_requirement DECLINING over time (0.25 at t=628 to 0.08 at t=2026) as the reading achieved dominance and no longer required active defense. This decay in suppression_requirement is consistent with the mountain classification: as institutional acceptance grows, the need for enforcement decreases, and the constraint begins to function as if naturally inevitable. The recent rise in base_extractiveness and theater_ratio (both 0.15 and 0.05 at t=2026, up from 0.05 and 0.02 at t=1900) reflects the T17 mountain_extraction_accumulation signal: the constraint may have accumulated secondary extraction (institutional capture, gatekeeping of mathematical discourse) while maintaining the appearance of natural law. This is the false-summit signal.
 *
 * PERSPECTIVAL GAP:
 *   From the mathematical practitioner's seat, zero-as-number is simply true — a discovery about the structure of quantity that enables mathematics to function. From the parmenidean philosopher's seat, the same constraint is a category error — nothing cannot exist, and treating it as a number is incoherent. From the institutional seat (textbooks, universities, peer review), the constraint is both: true AND institutionally enforced through curricula and publication standards. The engine should compute different types for each seat: the practitioner and institutional seats may compute as mountain (natural, non-extractive), while the excluded parmenidean seat would compute as snare (the reading suppresses the alternative without refuting it). This multi-seat divergence is not a defect of the story; it is exactly the structural asymmetry the per-seat classification machinery detects.
 *
 * DIRECTIONALITY LOGIC:
 *   Mathematical practitioners are the structural beneficiaries (d near 0.0): they gain computational power, proof methods, and unified foundations by treating zero as a number, at zero cost. The excluded philosophical seats (parmenidean rejectionist, notationalist) face mild suppression (institutional exclusion, not coercion) but this is asymmetric extraction only if we treat philosophical legitimacy as a rented good — which this reading does not do. No payer class exists in the conventional sense because the constraint imposes no material transfer or behavioral cost. The directionality derivation should place all agents at beneficiary or analytical poles, with no high-d (target) seats. This is consistent with the mountain classification: directionality is either symmetric or beneficiary-leaning for all named seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy question for this constraint: has zero-as-number lost its original founding problem (representing nothing in a consistent arithmetic system) but persisted because mathematical institutions have become dependent on the reading? The founding_problem_status is authored as LIVE, not DEAD, because contemporary mathematics still solves the problem of integrating zero into algebraic structures. However, the measurement of suppression_requirement DECLINING over time suggests institutional enforcement has become unnecessary — the constraint persists as a natural fact, not as something requiring active defense. This is NOT mandatrophy by the classical definition (the mandate died, the constraint persists through inertia), but it is functionally close: the constraint persists because institutional path-dependency makes alternatives extremely costly, even if the founding problem is technically still addressed. A mandatrophy_resolved flag should be considered if future analysis shows the founding problem is empirically dead (that zero-as-number is no longer the necessary solution to representing nothing in mathematics) but persists due to institutional embedding.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_convention,
    'Is zero''s status as a number a natural law of mathematics (discovered truth about the structure of quantity) or a conventional framing (useful but ultimately chosen)?',
    'Examine whether alternative algebraic frameworks (constructivist mathematics, intuitionistic logic, other foundations) can dispense with zero-as-number while maintaining mathematical power. If some frameworks reject it without contradiction, it is more conventional; if all viable foundations require it, it is more natural.',
    'If zero-as-number is purely conventional, the reading should reclassify from mountain to rope (coordination on a chosen framework). If it is necessary in all viable foundations, the mountain classification stands. The answer determines whether alternative readings (notationalist, parmenidean) are eliminated or merely disfavored.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_convention, conceptual, 'Whether zero''s number status is discovered or constructed.').

omega_variable(
    beneficiary_identification_ambiguity,
    'Who or what *actually* benefits from treating zero as a number? Mathematical practitioners (empirical beneficiaries) or abstract structural properties (conceptual beneficiaries)?',
    'Separate empirical benefit (practitioners can do more with zero-as-number) from structural benefit (abstract algebraic systems require zero-as-number to be closed). If the abstract structure could exist without practitioners, the distinction collapses; if practitioners use zero-as-number while the abstract structure could be formulated differently, the benefits are empirically separable.',
    'This affects the directionality derivation: if beneficiaries are only abstract propositions (not agents), the constraint may not have a genuine beneficiary class and should reclassify. If practitioners are the true beneficiaries, directionality toward them should trend toward d=0.0 (full beneficiary), not d=0.5 (symmetric).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_identification_ambiguity, conceptual, 'Ambiguity between agent and non-agent beneficiaries in a mathematical constraint.').

omega_variable(
    kernel_reading_alternative_framings,
    'Does the parmenidean_rejection reading genuinely foreclose this reading (number_reading), or do they coexist as live philosophical positions with different referents?',
    'If a single mathematical framework could coherently hold both (e.g., zero-as-number within one subsystem, zero-as-nonentity within another), they coexist. If no coherent unified framework can hold both (the axioms contradict at the foundational level), one forecloses the other.',
    'This determines the cs_structure.reading_relations value: forecloses (one reading rules out the other) vs. coexists_with (different parties maintain both in their respective traditions). The correct relation shapes how the engine evaluates the legitimacy contest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_alternative_framings, conceptual, 'Whether sibling kernel readings foreclose or coexist.').

omega_variable(
    mountain_false_summit_risk,
    'Does this constraint benefit identifiable mathematical institutions (proof theory departments, textbook publishers, computer scientists) in ways that make zero-as-number NOT a natural law but a constructed constraint with beneficiaries?',
    'Examine whether non-beneficiary stakeholders (constructivists, intuitionists, nominalists) face suppression when trying to develop alternatives. If suppression is active (journals reject papers, students are not taught alternatives), the constraint may be a false summit — a constraint that benefits institutional interests while claiming natural-law status.',
    'If suppression is found to be institutional rather than logical, the constraint should reclassify from mountain to snare or tangled_rope. The measurement of suppression_requirement over time will show whether it has risen (indicator of institutional defense of the reading) or remained flat (indicator of natural law status).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mountain_false_summit_risk, empirical, 'Risk that zero-as-number is defended as natural law when it actually benefits institutional interests.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zero_mathematical_status__number_reading, 628, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zero_tr_t628, zero_mathematical_status__number_reading, theater_ratio, 628, 0.08).
narrative_ontology:measurement_basis(zero_tr_t628, projected).
narrative_ontology:measurement(zero_tr_t1000, zero_mathematical_status__number_reading, theater_ratio, 1000, 0.06).
narrative_ontology:measurement_basis(zero_tr_t1000, projected).
narrative_ontology:measurement(zero_tr_t1400, zero_mathematical_status__number_reading, theater_ratio, 1400, 0.04).
narrative_ontology:measurement_basis(zero_tr_t1400, projected).
narrative_ontology:measurement(zero_tr_t1700, zero_mathematical_status__number_reading, theater_ratio, 1700, 0.03).
narrative_ontology:measurement_basis(zero_tr_t1700, projected).
narrative_ontology:measurement(zero_tr_t1900, zero_mathematical_status__number_reading, theater_ratio, 1900, 0.02).
narrative_ontology:measurement_basis(zero_tr_t1900, observed).
narrative_ontology:measurement(zero_tr_t2026, zero_mathematical_status__number_reading, theater_ratio, 2026, 0.05).
narrative_ontology:measurement_basis(zero_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(zero_be_t628, zero_mathematical_status__number_reading, base_extractiveness, 628, 0.15).
narrative_ontology:measurement_basis(zero_be_t628, observed).
narrative_ontology:measurement(zero_be_t1000, zero_mathematical_status__number_reading, base_extractiveness, 1000, 0.12).
narrative_ontology:measurement_basis(zero_be_t1000, projected).
narrative_ontology:measurement(zero_be_t1400, zero_mathematical_status__number_reading, base_extractiveness, 1400, 0.08).
narrative_ontology:measurement_basis(zero_be_t1400, projected).
narrative_ontology:measurement(zero_be_t1700, zero_mathematical_status__number_reading, base_extractiveness, 1700, 0.06).
narrative_ontology:measurement_basis(zero_be_t1700, projected).
narrative_ontology:measurement(zero_be_t1900, zero_mathematical_status__number_reading, base_extractiveness, 1900, 0.05).
narrative_ontology:measurement_basis(zero_be_t1900, observed).
narrative_ontology:measurement(zero_be_t2026, zero_mathematical_status__number_reading, base_extractiveness, 2026, 0.15).
narrative_ontology:measurement_basis(zero_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(zero_su_t628, zero_mathematical_status__number_reading, suppression_requirement, 628, 0.25).
narrative_ontology:measurement_basis(zero_su_t628, projected).
narrative_ontology:measurement(zero_su_t1000, zero_mathematical_status__number_reading, suppression_requirement, 1000, 0.18).
narrative_ontology:measurement_basis(zero_su_t1000, projected).
narrative_ontology:measurement(zero_su_t1400, zero_mathematical_status__number_reading, suppression_requirement, 1400, 0.12).
narrative_ontology:measurement_basis(zero_su_t1400, projected).
narrative_ontology:measurement(zero_su_t1700, zero_mathematical_status__number_reading, suppression_requirement, 1700, 0.08).
narrative_ontology:measurement_basis(zero_su_t1700, projected).
narrative_ontology:measurement(zero_su_t1900, zero_mathematical_status__number_reading, suppression_requirement, 1900, 0.04).
narrative_ontology:measurement_basis(zero_su_t1900, observed).
narrative_ontology:measurement(zero_su_t2026, zero_mathematical_status__number_reading, suppression_requirement, 2026, 0.08).
narrative_ontology:measurement_basis(zero_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zero_mathematical_status__number_reading, information_standard).
narrative_ontology:boltzmann_floor_override(zero_mathematical_status__number_reading, 0.02).
narrative_ontology:affects_constraint(zero_mathematical_status__number_reading, zero_mathematical_status__parmenidean_rejection).
narrative_ontology:affects_constraint(zero_mathematical_status__number_reading, zero_mathematical_status__placeholder_reading).

% DUAL FORMULATION NOTE:
% Zero's mathematical status is a contested kernel decomposed into three readings: number_reading (this story), parmenidean_rejection (ontological objection), and placeholder_reading (notational-only view). Each reading has a different beneficiary structure, extractiveness, and suppression profile. The readings coexist as live philosophical positions but compete for institutional legitimacy in mathematics. This story links to both siblings via affects_constraints because it influences the conditions under which the alternative readings can be maintained.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
