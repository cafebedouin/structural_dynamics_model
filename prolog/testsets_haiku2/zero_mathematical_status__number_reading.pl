% ============================================================================
% CONSTRAINT STORY: zero_mathematical_status__number_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Zero as a Number with Arithmetic Operations
 *   domain: history_of_mathematics/philosophy_of_mathematics
 *
 * SUMMARY:
 *   This constraint instantiates the number-reading of the
 *   zero-mathematical-status kernel: zero is a number with defined arithmetic
 *   operations (Brahmagupta's rules: a+0=a, a×0=0, a×0=0, a/0 undefined). It
 *   is contrasted with the parmenidean rejection (zero as ontologically
 *   incoherent nonbeing) and the placeholder reading (zero as positional
 *   notational device without arithmetic number status). The number reading
 *   has achieved hegemonic status in modern mathematics since the 18th
 *   century. This story models the constraint as a natural law (mountain)
 *   with a beneficiary set — an FSM candidate — because it makes a structural
 *   claim about how numbers behave, not because it is obviously free from
 *   institutional interest (mathematical institutions and practitioners do
 *   benefit from its universalization). The kernel context situates this
 *   reading within the contested kernel; the cs_structure carries the
 *   reading-relations to siblings and the axioms distinguishing this reading.
 *
 * KEY AGENTS:
 *   - Algebraic practitioners: mathematicians using polynomials, equations, and abstract structures — benefit from zero-as-number
 *   - Calculus practitioners: mathematicians using limits, derivatives, integrals — benefit from zero-as-number
 *   - Positional notation users: all using Hindu-Arabic numerals, place-value systems — benefit from zero-as-number
 *   - Symbolic mathematics community: abstract algebra, ring theory, formal logic — benefit from zero-as-number
 *   - Parmenidean rejection proponents: philosophers holding nonbeing is incoherent — excluded from consensus
 *   - Placeholder reading proponents: mathematicians treating zero as notational convenience — excluded from consensus
 *   - Mathematical consensus authority: textbooks, academic certification, institutional practice — maintains the constraint
 *   - Analytical observer: meta-mathematical perspective — assesses justification and alternatives
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zero_mathematical_status__number_reading, 0.15).
domain_priors:suppression_score(zero_mathematical_status__number_reading, 0.08).
domain_priors:theater_ratio(zero_mathematical_status__number_reading, 0.02).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zero_mathematical_status__number_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(zero_mathematical_status__number_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(zero_mathematical_status__number_reading, theater_ratio, 0.02).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zero_mathematical_status__number_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(zero_mathematical_status__number_reading, resistance, 0.18).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zero_mathematical_status__number_reading, mountain).
narrative_ontology:human_readable(zero_mathematical_status__number_reading, "Zero as a Number with Arithmetic Operations").
narrative_ontology:topic_domain(zero_mathematical_status__number_reading, "history_of_mathematics/philosophy_of_mathematics").

domain_priors:emerges_naturally(zero_mathematical_status__number_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zero_mathematical_status__number_reading, '6cd0115d-cd63-43f7-9285-22571e0b85a2').
narrative_ontology:cs_kernel_codification('6cd0115d-cd63-43f7-9285-22571e0b85a2', formalized).
narrative_ontology:cs_authority_grounding('6cd0115d-cd63-43f7-9285-22571e0b85a2', expertise).
narrative_ontology:cs_interpretation_layer_present('6cd0115d-cd63-43f7-9285-22571e0b85a2').
narrative_ontology:cs_reading_relation('6cd0115d-cd63-43f7-9285-22571e0b85a2', zero_mathematical_status__parmenidean_rejection, forecloses).
narrative_ontology:cs_reading_relation('6cd0115d-cd63-43f7-9285-22571e0b85a2', zero_mathematical_status__placeholder_reading, coexists_with).
narrative_ontology:cs_axiom('6cd0115d-cd63-43f7-9285-22571e0b85a2', foundational, zero_is_number_with_identity_laws).
narrative_ontology:cs_axiom_status(zero_is_number_with_identity_laws, holdable).
narrative_ontology:cs_axiom_grounding('6cd0115d-cd63-43f7-9285-22571e0b85a2', zero_is_number_with_identity_laws, deontological).
narrative_ontology:cs_axiom('6cd0115d-cd63-43f7-9285-22571e0b85a2', foundational, absence_has_arithmetic_properties).
narrative_ontology:cs_axiom_status(absence_has_arithmetic_properties, holdable).
narrative_ontology:cs_axiom_grounding('6cd0115d-cd63-43f7-9285-22571e0b85a2', absence_has_arithmetic_properties, empirically_contingent).
narrative_ontology:cs_reference_frame('6cd0115d-cd63-43f7-9285-22571e0b85a2', brahmagupta_arithmetic_completeness).
narrative_ontology:cs_drift_state('6cd0115d-cd63-43f7-9285-22571e0b85a2', contemporary_pure_mathematics, gap(stable, minor, true)).
narrative_ontology:cs_created_at('6cd0115d-cd63-43f7-9285-22571e0b85a2', '').
narrative_ontology:cs_kernel_id(zero_mathematical_status__number_reading, zero_mathematical_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zero_mathematical_status__number_reading, algebraic_practitioners).
narrative_ontology:constraint_beneficiary(zero_mathematical_status__number_reading, calculus_practitioners).
narrative_ontology:constraint_beneficiary(zero_mathematical_status__number_reading, positional_notation_users).
narrative_ontology:constraint_beneficiary(zero_mathematical_status__number_reading, symbolic_mathematics_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Mathematicians and practitioners who construct, solve, and extend polynomial equations and algebraic systems. The zero-as-number rule is constitutive of their practice: it permits equations like x + 0 = x to hold universally and enables systematic algebraic manipulation. Their field is unintelligible without zero as a number with these operations.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__number_reading, algebraic_practitioners, beneficiary,
    institutional, civilizational, arbitrage, universal).

% Mathematicians and scientists who construct limits, derivatives, and integrals. Zero-as-number with additive and multiplicative identity is foundational: limits approach zero, derivatives measure infinitesimal change (zero intervals), and the calculus framework collapses if zero is not a full number. Their discipline would require wholesale reconstruction without this constraint.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__number_reading, calculus_practitioners, beneficiary,
    institutional, civilizational, arbitrage, universal).

% All who use place-value notation (Hindu-Arabic numerals, scientific notation, digital representation). The zero-as-number rule guarantees that zero in the ones place, tens place, hundreds place, etc., has identical arithmetic semantics — it contributes zero to the sum. Without this, positional notation devolves into ad-hoc glyph convention.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__number_reading, positional_notation_users, beneficiary,
    organized, civilizational, mobile, universal).

% Mathematicians working in abstract algebra, ring theory, vector spaces, and formal logic. Zero is the additive identity in every algebraic structure; without the zero-as-number rule, the unification of these theories into a consistent framework would fail. The entire edifice of modern abstract mathematics presupposes zero's number status.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__number_reading, symbolic_mathematics_community, beneficiary,
    institutional, civilizational, arbitrage, universal).

% Historical and contemporary philosophers holding that nonbeing (zero as number) is ontologically incoherent, that 'nothing cannot be something.' They would argue for mathematical systems in which absence is not codified as a distinct number but only as lack or negation. They are excluded from the dominant mathematical conversation not by institutional barrier but by the historical consensus that adopted the zero-as-number framework.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__number_reading, parmenidean_rejection_proponents, excluded,
    analytical, civilizational, analytical, universal).

% Mathematicians and philosophers who argue zero is a notational convenience (a positional placeholder) rather than a number with independent arithmetic properties. They would maintain positional notation while denying zero participation in arithmetic identity laws. They are excluded from the consensus mathematical conversation by the historical adoption of the integrated framework.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__number_reading, placeholder_reading_proponents, excluded,
    analytical, civilizational, analytical, universal).

% The distributed authority of mathematical practice, textbook standards, and academic certification that has, since Brahmagupta's codification in the 7th century CE and its full institutionalization by the 18th century, treated zero as a number with the stated arithmetic properties. This authority sustains the constraint through its consistent application in teaching, proof, and problem-solving.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__number_reading, mathematical_consensus_authority, agenda_setter,
    institutional, civilizational, analytical, universal).

% A meta-mathematical or historical perspective examining the status and justification of zero-as-number from outside the system it structures. Can compare frameworks, assess the logical coherence of alternatives, and measure the cost and benefit of the constraint's adoption.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__number_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified set of number-defining operations (additive identity, multiplicative annihilation) that allow zero to participate in all arithmetic and algebraic manipulations without exception or special case. This eliminates the need for ad-hoc exception clauses ('except when zero appears') and unifies positional notation, algebra, and calculus into a single coherent system.
% TRANSFER_FUNCTION: The constraint transfers explanatory and computational burden away from the practitioners who would otherwise need to carry exception-handling rules (zero as non-number, zero as placeholder, zero as negation) and into the foundational structure itself. Practitioners gain universal applicability of laws; in exchange, they accept zero's full number status and the ontological commitment this implies.
% ABSENT_VOICES: Parmenidean philosophers and metaphysicians who hold that 'nothing cannot be something' are absent from modern mathematical discourse not through institutional exclusion but through historical consensus. Similarly, philosophers defending zero-as-placeholder rather than zero-as-number are excluded from the mainstream conversation. These voices would argue that the constraint imposes an unnecessary ontological burden and that alternative notational systems (Chinese rod numerals, Roman numerals with ad-hoc zero handling) could preserve positional convenience without the number commitment.
% DISAPPEARANCE_RATIONALE: If this constraint vanished — if zero were removed as a number and treated instead as a placeholder or notational convenience only — algebra would require pervasive special-case handling (x + 0 ≠ x in general, only in positional representation). Calculus would lose the coherence of limits approaching zero. Ring and field theory would require separate definitions for rings without zero or with degenerate zero. Centuries of derived results would require re-examination for hidden assumptions about zero's number status. The entire edifice of symbolic and abstract mathematics would reorganize itself around exception rules.
% FOUNDING_PROBLEM: Ancient Greek and medieval Indian mathematics faced a persistent practical problem: how to represent absence in positional notation (the tens place is empty) and how to perform arithmetic on this absence without logical contradiction. Brahmagupta's formulation (a + 0 = a, a × 0 = 0) and the subsequent integration of zero into number systems solved this by making zero a legitimate number with defined operations, thereby unifying positional notation with arithmetic.
% FOUNDING_PROBLEM_CORROBORATION: Modern mathematics and computational science continuously employ the zero-as-number rule; every compiler, every symbolic algebra system (Mathematica, SageMath), every physics simulation enforces a + 0 = a and a × 0 = 0 as foundational. Historians of mathematics (Katz, Joseph, Kaplan) and philosophers of mathematics (Shapiro on structures, Burgess on nominalism) corroborate that Brahmagupta's founding problem (absence representation + arithmetic coherence) remains live and is solved by the zero-as-number constraint. The problem does not persist as 'shall we include zero' but as 'how do alternative mathematical systems handle absence' — the founding problem's persistence is its transformation into a question about the necessity of the solution.
narrative_ontology:disappearance_verdict(zero_mathematical_status__number_reading, world_rearranges).
narrative_ontology:founding_problem_status(zero_mathematical_status__number_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zero_mathematical_status__number_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is low (0.15) because the constraint operates at the structural level of mathematics itself; there is no entity extracting rent from zero-as-number — the constraint enables all mathematical practice. The modest upward trend (0.08 → 0.15 over the interval) reflects the historical accumulation of theorems, proofs, and domains whose foundations depend on zero's number status; as mathematics expands and deepens, the constraint's indispensability grows. Suppression is minimal (0.08) because the constraint does not require active enforcement against resistance; it is sustained by the logic of mathematical coherence and the accumulated weight of consistent application. Theater ratio is negligible (0.02) because the constraint has no performative element — it either holds or it does not, and its operation is transparent to users. Accessibility collapse is very high (0.92) because once zero is understood as a number, no alternative framework for arithmetic is computationally accessible; you cannot 'leave' zero-as-number without reconstructing arithmetic from scratch. Resistance is low (0.18) because active resistance requires organized institutional challenge, and no major mathematical tradition in the modern era has mounted such a challenge (parmenidean and placeholder objections are isolated voices, not organized resistance).
 *
 * PERSPECTIVAL GAP:
 *   This constraint should compute as mountain from every seat: algebraic and calculus practitioners see it as natural law (zero must work this way); positional notation users see it as necessary for their notation to be coherent; the mathematical consensus authority sees it as discovered truth, not constructed rule. The parmenidean and placeholder proponents, by contrast, would compute it as a snare or tangled rope — a constraint that benefits mathematicians by imposing an ontological commitment on zero. The engine's per-seat computation will show zero dissent within the dominant mathematical tradition (all mountain) and substantial dissent from excluded voices (snare-like from their perspective). The divergence is structural: a constraint that appears as natural law from inside the benefiting system appears as constructed extraction from outside it.
 *
 * DIRECTIONALITY LOGIC:
 *   All named beneficiaries have high exit_options (arbitrage, mobile, analytical) and institutional power — they are not trapped by the constraint, they are enabled by it. Their directionality is near zero (full beneficiary). The excluded voices (parmenidean and placeholder proponents) have analytical power and analytical exit, so their d is undefined/observer-level — they are outside the system, not trapped within it. The mathematical consensus authority is the agenda setter (maintains and applies the rule) but is itself composed of the beneficiaries (practitioners, institutions). This is a recursive structure: the constraint's authority is the authority of those it benefits. Directionality derivation yields near-universal beneficiary status among mathematical actors; no payers exist within the mathematical system (the cost of zero-as-number is borne by those outside mathematics who must accept its metaphysical implications, not by practitioners).
 *
 * MANDATROPHY ANALYSIS:
 *   The zero-as-number constraint exhibits no mandatrophy: its founding problem (absence representation + arithmetic coherence) is not solved in any other way that has achieved comparable institutional entrenchment, and the constraint continues to perform the function it was designed for. The founding_problem_status = 'live' indicates that the problem remains a live issue in mathematics (how to handle absence, how to structure arithmetic) and the solution remains fit. There is no evidence of mandate obsolescence or institutional inertia pretending at function — the constraint is actively used and actively generates new mathematics (modern ring theory, categories, type theory all presuppose zero-as-number).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_commitment,
    'Is zero-as-number a discovered feature of mathematical reality (natural law), or a constructed commitment whose universality reflects institutional adoption rather than logical necessity?',
    'Examine whether mathematically coherent systems can be constructed that treat zero as placeholder rather than number (e.g., residue systems in modular arithmetic, or formal systems with designated undefined operations). If such systems exist and require explicit rule changes but no logical contradiction, zero-as-number is a commitment; if all attempted alternatives degenerate or collapse, it is closer to natural law.',
    'If constructed: the constraint is a false summit, benefiting mathematicians and their institutions by securing a particular ontological framework. If natural law: the constraint genuinely emerges from the structure of arithmetic itself. This distinction determines whether the constraint should be reclassified from mountain to tangled_rope or snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_commitment, empirical, 'Whether zero-as-number is discovered (mountain) or constructed (false summit/tangled rope).').

omega_variable(
    alternative_arithmetic_coherence,
    'Can a complete and practically usable system of arithmetic and algebra be constructed in which zero is treated as a placeholder or special marker, not a full number participating in identity laws?',
    'Formal construction of a complete alternative arithmetic system; examination of historical systems (Chinese rod numerals, pre-Brahmaguptian accounting systems) to assess their functional adequacy without zero-as-number.',
    'If a coherent alternative is shown: zero-as-number is a conventional choice, not a necessary structure; the constraint''s persistence reflects institutional lock-in, not logical necessity. If no alternative achieves comparable completeness: zero-as-number is closer to natural law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_arithmetic_coherence, conceptual, 'Whether alternative arithmetic systems without zero-as-number can achieve equivalent coherence.').

omega_variable(
    suppression_mechanism_identity_lock,
    'Is the constraint sustained by structural logical necessity (natural law) or by the identity-fusion of mathematicians with zero-as-number as a constitutive element of their practice?',
    'Historical and ethnographic: do mathematicians trained outside the zero-as-number framework show resistance to adopting it? Does adoption require explicit re-education, or is it seamless? Do mathematicians who attempt to work without zero-as-number (parmenidean or placeholder frameworks) report computational barriers or ideological barriers?',
    'If identity-locked: the suppression metric (0.08) understates the internalized component of constraint maintenance — the minimal active enforcement reflects deep professional identity fusion. If purely structural: suppression is accurately low because the constraint is logically self-maintaining.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_identity_lock, empirical, 'Whether the constraint is maintained by logical necessity or by mathematician identity-fusion with zero-as-number.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zero_mathematical_status__number_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zero_tr_t0, zero_mathematical_status__number_reading, theater_ratio, 0, 0.01).
narrative_ontology:measurement_basis(zero_tr_t0, observed).
narrative_ontology:measurement(zero_tr_t3, zero_mathematical_status__number_reading, theater_ratio, 3, 0.01).
narrative_ontology:measurement_basis(zero_tr_t3, observed).
narrative_ontology:measurement(zero_tr_t6, zero_mathematical_status__number_reading, theater_ratio, 6, 0.015).
narrative_ontology:measurement_basis(zero_tr_t6, observed).
narrative_ontology:measurement(zero_tr_t9, zero_mathematical_status__number_reading, theater_ratio, 9, 0.017).
narrative_ontology:measurement_basis(zero_tr_t9, observed).
narrative_ontology:measurement(zero_tr_t12, zero_mathematical_status__number_reading, theater_ratio, 12, 0.018).
narrative_ontology:measurement_basis(zero_tr_t12, observed).
narrative_ontology:measurement(zero_tr_t15, zero_mathematical_status__number_reading, theater_ratio, 15, 0.02).
narrative_ontology:measurement_basis(zero_tr_t15, observed).

% Extraction over time
narrative_ontology:measurement(zero_be_t0, zero_mathematical_status__number_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement_basis(zero_be_t0, observed).
narrative_ontology:measurement(zero_be_t3, zero_mathematical_status__number_reading, base_extractiveness, 3, 0.09).
narrative_ontology:measurement_basis(zero_be_t3, observed).
narrative_ontology:measurement(zero_be_t6, zero_mathematical_status__number_reading, base_extractiveness, 6, 0.11).
narrative_ontology:measurement_basis(zero_be_t6, observed).
narrative_ontology:measurement(zero_be_t9, zero_mathematical_status__number_reading, base_extractiveness, 9, 0.13).
narrative_ontology:measurement_basis(zero_be_t9, observed).
narrative_ontology:measurement(zero_be_t12, zero_mathematical_status__number_reading, base_extractiveness, 12, 0.14).
narrative_ontology:measurement_basis(zero_be_t12, observed).
narrative_ontology:measurement(zero_be_t15, zero_mathematical_status__number_reading, base_extractiveness, 15, 0.15).
narrative_ontology:measurement_basis(zero_be_t15, observed).

% Suppression requirement over time
narrative_ontology:measurement(zero_su_t0, zero_mathematical_status__number_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement_basis(zero_su_t0, observed).
narrative_ontology:measurement(zero_su_t3, zero_mathematical_status__number_reading, suppression_requirement, 3, 0.055).
narrative_ontology:measurement_basis(zero_su_t3, observed).
narrative_ontology:measurement(zero_su_t6, zero_mathematical_status__number_reading, suppression_requirement, 6, 0.065).
narrative_ontology:measurement_basis(zero_su_t6, observed).
narrative_ontology:measurement(zero_su_t9, zero_mathematical_status__number_reading, suppression_requirement, 9, 0.072).
narrative_ontology:measurement_basis(zero_su_t9, observed).
narrative_ontology:measurement(zero_su_t12, zero_mathematical_status__number_reading, suppression_requirement, 12, 0.076).
narrative_ontology:measurement_basis(zero_su_t12, observed).
narrative_ontology:measurement(zero_su_t15, zero_mathematical_status__number_reading, suppression_requirement, 15, 0.08).
narrative_ontology:measurement_basis(zero_su_t15, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zero_mathematical_status__number_reading, information_standard).
narrative_ontology:boltzmann_floor_override(zero_mathematical_status__number_reading, 0.05).
narrative_ontology:affects_constraint(zero_mathematical_status__number_reading, zero_mathematical_status__parmenidean_rejection).
narrative_ontology:affects_constraint(zero_mathematical_status__number_reading, zero_mathematical_status__placeholder_reading).
narrative_ontology:affects_constraint(zero_mathematical_status__number_reading, positional_notation_system).
narrative_ontology:affects_constraint(zero_mathematical_status__number_reading, algebraic_structure_universality).

% DUAL FORMULATION NOTE:
% The zero_mathematical_status kernel decomposes into three reading-constraints: (1) number_reading (this file) — zero is a number with arithmetic operations; (2) parmenidean_rejection — zero is ontologically incoherent; (3) placeholder_reading — zero is a notational device without arithmetic number status. Each instantiates a different constraint with a different ε and different beneficiary/victim structure. The number reading has achieved hegemonic status in mathematics since the 18th century; the parmenidean and placeholder readings remain live in historical and philosophical discourse but are excluded from the dominant mathematical conversation. The network edges capture the generative relationship (each reading's adoption constrains the others' viability) and the epistemic priority (number reading influences both siblings by establishing the mathematical consensus framework within which they are evaluated).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
