% ============================================================================
% CONSTRAINT STORY: zero_mathematical_status__number_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
    narrative_ontology:suppression_profile/2,
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
 *   human_readable: Zero as Fully Arithmetized Number (Brahmagupta Rules)
 *   domain: history_of_mathematics/philosophy_of_mathematics
 *
 * SUMMARY:
 *   This constraint captures the number_reading of the contested 'zero'
 *   kernel: Brahmagupta's 7th-century codification of zero's arithmetic
 *   behavior (a+0=a, a×0=0, and the beginnings of formal treatment of a/0) as
 *   an integrated number rather than a mere placeholder symbol or an
 *   ontological impossibility. Under this reading, zero enters the number
 *   system on the same operational footing as any other quantity, enabling
 *   algebra, later calculus, and eventually digital computing's identity
 *   elements. The reading is generated as its own ε-invariant constraint, per
 *   Rule 1: it does not average over, hedge against, or narrate the sibling
 *   readings (parmenidean_rejection, placeholder_reading) inside this file.
 *   Those are separate constraints with their own ε, beneficiaries, and
 *   classification, linked via network.affects_constraints and
 *   cs_structure.reading_relations.
 *
 * KEY AGENTS:
 *   - mathematical_practitioners: primary beneficiary (organized/analytical) — the constraint is the operating system of their field
 *   - algebraists: beneficiary (organized/analytical) — equation-solving depends on zero's arithmetic closure
 *   - calculus_users: beneficiary (organized/analytical) — limits and derivatives presuppose zero as a number
 *   - parmenidean_philosophers: excluded — object to zero's ontological status, preserved as sibling reading
 *   - positional_notation_theorists: excluded — hold the placeholder view, preserved as sibling reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zero_mathematical_status__number_reading, 0.05).
domain_priors:suppression_score(zero_mathematical_status__number_reading, 0.08).
domain_priors:theater_ratio(zero_mathematical_status__number_reading, 0.03).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zero_mathematical_status__number_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(zero_mathematical_status__number_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(zero_mathematical_status__number_reading, theater_ratio, 0.03).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zero_mathematical_status__number_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(zero_mathematical_status__number_reading, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zero_mathematical_status__number_reading, mountain).
narrative_ontology:human_readable(zero_mathematical_status__number_reading, "Zero as Fully Arithmetized Number (Brahmagupta Rules)").
narrative_ontology:topic_domain(zero_mathematical_status__number_reading, "history_of_mathematics/philosophy_of_mathematics").

domain_priors:emerges_naturally(zero_mathematical_status__number_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zero_mathematical_status__number_reading, 'c8c18d1f-40e2-4769-853a-c7d38d132709').
narrative_ontology:cs_kernel_codification('c8c18d1f-40e2-4769-853a-c7d38d132709', formalized).
narrative_ontology:cs_authority_grounding('c8c18d1f-40e2-4769-853a-c7d38d132709', expertise).
narrative_ontology:cs_interpretation_layer_present('c8c18d1f-40e2-4769-853a-c7d38d132709').
narrative_ontology:cs_reading_relation('c8c18d1f-40e2-4769-853a-c7d38d132709', zero_mathematical_status__parmenidean_rejection, forecloses).
narrative_ontology:cs_reading_relation('c8c18d1f-40e2-4769-853a-c7d38d132709', zero_mathematical_status__placeholder_reading, influences).
narrative_ontology:cs_axiom('c8c18d1f-40e2-4769-853a-c7d38d132709', foundational, zero_satisfies_number_closure_axioms).
narrative_ontology:cs_axiom_status(zero_satisfies_number_closure_axioms, holdable).
narrative_ontology:cs_axiom_grounding('c8c18d1f-40e2-4769-853a-c7d38d132709', zero_satisfies_number_closure_axioms, conventional).
narrative_ontology:cs_axiom('c8c18d1f-40e2-4769-853a-c7d38d132709', foundational, arithmetic_operability_suffices_for_numberhood).
narrative_ontology:cs_axiom_status(arithmetic_operability_suffices_for_numberhood, holdable).
narrative_ontology:cs_axiom_grounding('c8c18d1f-40e2-4769-853a-c7d38d132709', arithmetic_operability_suffices_for_numberhood, instrumental).
narrative_ontology:cs_reference_frame('c8c18d1f-40e2-4769-853a-c7d38d132709', brahmagupta_arithmetic_closure).
narrative_ontology:cs_drift_state('c8c18d1f-40e2-4769-853a-c7d38d132709', contemporary_formal_mathematics, gap(stable, minor, true)).
narrative_ontology:cs_created_at('c8c18d1f-40e2-4769-853a-c7d38d132709', '').
narrative_ontology:cs_kernel_id(zero_mathematical_status__number_reading, zero_mathematical_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zero_mathematical_status__number_reading, mathematical_practitioners).
narrative_ontology:constraint_beneficiary(zero_mathematical_status__number_reading, algebraists).
narrative_ontology:constraint_beneficiary(zero_mathematical_status__number_reading, calculus_users).
narrative_ontology:constraint_beneficiary(zero_mathematical_status__number_reading, engineers_and_scientists).
narrative_ontology:constraint_beneficiary(zero_mathematical_status__number_reading, computational_systems_designers).
narrative_ontology:constraint_vindicates(zero_mathematical_status__number_reading, zero_is_a_number).
narrative_ontology:constraint_vindicates(zero_mathematical_status__number_reading, additive_identity_axiom).
narrative_ontology:constraint_vindicates(zero_mathematical_status__number_reading, multiplicative_annihilation_axiom).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Uses zero as a fully arithmetized number obeying defined operations (a+0=a, a×0=0, and later a/0 as undefined-but-structured) to build algebra, calculus, and every downstream formal system. Exit from the number reading would mean abandoning most of post-9th-century mathematics; there is no rival framework that performs the same work.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__number_reading, mathematical_practitioners, beneficiary,
    organized, civilizational, analytical, universal).

% Depends on zero's arithmetic closure (additive identity, multiplicative annihilator) to solve equations, define polynomial roots, and construct number systems (integers, rationals, reals) as algebraic structures with zero as a distinguished element. Without the number reading, equation-solving as currently practiced does not exist in its present form.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__number_reading, algebraists, beneficiary,
    organized, civilizational, analytical, universal).

% Relies on zero as a number to define limits, derivatives (rates of change approaching but reaching zero difference), and integrals. The entire limit-based foundation of analysis presupposes zero as an arithmetic object, not merely a placeholder symbol.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__number_reading, calculus_users, beneficiary,
    organized, civilizational, analytical, universal).

% Applies zero-inclusive arithmetic to physical modeling, measurement, and engineering calculation without needing to revisit the ontological question — the number reading is simply the operative background assumption their work inherits.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__number_reading, engineers_and_scientists, beneficiary,
    organized, generational, constrained, global).

% Builds digital computation, floating-point arithmetic, and logic circuits on zero as a numeric identity element (additive identity, boolean false, null address). Any departure from zero-as-number would require rebuilding foundational computing semantics from scratch.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__number_reading, computational_systems_designers, beneficiary,
    organized, biographical, constrained, global).

% Historically and philosophically object that treating 'nothing' as a countable, operable quantity commits a category error — being cannot be predicated of non-being. Their objection is preserved as a distinct sibling reading (parmenidean_rejection) rather than refuted within this constraint; they are not part of the mathematical-practitioner conversation that adopted Brahmagupta's rules.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__number_reading, parmenidean_philosophers, excluded,
    analytical, civilizational, analytical, universal).

% Hold that zero's historical origin and continuing function in some contexts is purely notational (a placeholder marking an empty positional slot) rather than a full arithmetic object. This view survives as the sibling placeholder_reading; its adherents are not addressed inside this number-reading constraint.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__number_reading, positional_notation_theorists, excluded,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Fixes zero's arithmetic behavior (identity under addition, annihilation under multiplication, and later formalized limit/undefined-division behavior) as a stable, shared rule set so that every mathematical practitioner across traditions and centuries computes with zero identically, enabling algebra, analysis, and derived formal systems to interoperate without local renegotiation of what zero does.
% TRANSFER_FUNCTION: Moves nothing extractive between parties; it transfers cognitive and notational labor away from ad hoc case-handling of 'no quantity' (which earlier systems required special-cased rules for) toward uniform symbolic manipulation, redistributing effort from every individual calculation to the one-time acceptance of Brahmagupta's rules.
% ABSENT_VOICES: Parmenidean philosophers and strict placeholder-notation theorists would object that this reading over-claims zero's ontological or functional status; they are not absent from history (their positions are recorded and survive as sibling constraints) but are absent from the practicing-mathematics conversation this constraint describes, which long ago settled the question operationally rather than philosophically.
% DISAPPEARANCE_RATIONALE: If the arithmetized-zero convention vanished overnight, algebraic notation, calculus, double-entry accounting, digital computing's identity elements, and virtually all quantitative science built since the 7th-9th centuries CE would require reconstruction from more primitive or special-cased number systems (as pre-Brahmagupta Greek and Roman mathematics operated without it) — the practical mathematical world depends on this arrangement continuing to hold.
% FOUNDING_PROBLEM: Earlier arithmetic systems (Greek, Roman, early Babylonian) either lacked a symbol for 'no quantity' or treated it as a mere placeholder, forcing case-by-case special handling whenever a calculation produced or involved an empty quantity, and blocking algebraic generality (equations could not be solved uniformly across cases including zero solutions).
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is corroborated as historically resolved by historians of mathematics external to any single practitioner community (e.g., documented transmission through Brahmagupta's Brahmasphutasiddhanta, subsequent Islamic algebraists such as al-Khwarizmi, and European adoption via Fibonacci) — independent historical scholarship, not mathematicians' own self-report, attests that the arithmetic-closure problem zero solved is fully and uncontroversially resolved within the mathematical mainstream; only the philosophical siblings (parmenidean_rejection, placeholder_reading) continue to contest the underlying ontological or functional characterization, not the practical resolution.
narrative_ontology:disappearance_verdict(zero_mathematical_status__number_reading, world_rearranges).
narrative_ontology:founding_problem_status(zero_mathematical_status__number_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zero_mathematical_status__number_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(zero_mathematical_status__number_reading, 'none', 1).
narrative_ontology:epsilon_provenance(zero_mathematical_status__number_reading, 0.05, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored near zero (0.05) because no party pays a cost for zero's arithmetization — the constraint is pure enabling infrastructure with no identifiable payer group; the 'cost' of adopting the rules (relearning) is a one-time transitional friction, not an ongoing extraction. Suppression is low (0.08) because the number reading did not need to coercively suppress alternatives; it displaced the placeholder and rejectionist views through demonstrated computational power and utility, not through enforcement (contrast with a genuinely coercive kernel reading). Accessibility collapse is high (0.88) because once encountered, the arithmetic rules leave no serious working alternative for anyone doing algebra or calculus — you cannot do modern mathematics without treating zero as a number, even though alternative *ontological* framings remain philosophically live. Resistance is low (0.12): mathematically, this reading faces almost no active resistance from practitioners; residual resistance is philosophical/historical, located in the sibling readings, not inside this constraint.
 *
 * PERSPECTIVAL GAP:
 *   There is minimal seat divergence here because every named stakeholder is a beneficiary at high power (organized) with either analytical or constrained exit — the divergence this kernel produces lives ACROSS readings (this story vs. parmenidean_rejection vs. placeholder_reading), not within this reading's stakeholder set. Within-story, the closest thing to divergence is between mathematical_practitioners (arbitrary-precision, purely analytical relationship to the constraint) and computational_systems_designers (constrained exit, since floating-point and hardware conventions lock in specific zero-handling behaviors that are costly to change once built).
 *
 * DIRECTIONALITY LOGIC:
 *   All five named stakeholders are beneficiaries because the number_reading constraint has no structural victim — it is a coordination-of-notation problem solved once and inherited freely by everyone downstream. No agent pays an extraction cost through this constraint's operation; the 'cost' of the historical transition (centuries of resistance from Greek-tradition mathematicians, the church's suspicion of zero in medieval Europe) is captured instead in the sibling constraint's territory (parmenidean_rejection) as historical friction, not as an ongoing payer relationship inside this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (arithmetic could not uniformly handle 'no quantity') is dead — fully and uncontroversially resolved — yet the arrangement (zero's number-status) persists as thoroughly as ever, which might look like classic mandatrophy (a solved problem's solution outliving its function while still extracting). But disappearance_verdict is world_rearranges, not world_unchanged, and founding_problem_status=dead paired with world_rearranges signals not a zombie arrangement but a genuinely successful, internalized coordination outcome: the solution is dead-as-a-problem precisely because it succeeded completely and became foundational infrastructure, not because it decayed into empty ritual. This is the correct way the classification distinguishes 'solved and now load-bearing' from 'obsolete but still extracting' — the mandatrophy signature requires a beneficiary capturing rents from an obsolete mandate, and no such capturer exists here.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    zero_natural_discovery_vs_convention,
    'Is zero''s number-status a discovered mathematical fact (a mountain, true regardless of any culture''s adoption) or a historically contingent convention that happened to be adopted because it was useful (making the beneficiary set a marker of successful convention-construction rather than natural law)?',
    'Cross-cultural and counterfactual analysis: examine mathematical traditions that developed independently without full zero-arithmetization (e.g., classical Greek mathematics, which achieved substantial results without treating zero as a number) and assess whether their eventual convergence toward zero-arithmetization under contact with Indian/Islamic mathematics reflects discovery of a necessary truth or diffusion of a superior but non-necessary convention.',
    'If zero-arithmetization is discovered necessity, the mountain classification is unambiguous and the beneficiary declaration is incidental (everyone benefits from a true fact, as with any mathematical theorem). If it is a superior convention among possible alternatives, the constraint edges toward a false-summit pattern where ''naturalness'' language obscures that a particular formalization route was selected and its practitioners now retroactively describe it as necessary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(zero_natural_discovery_vs_convention, conceptual, 'Whether zero''s number-status is discovered mathematical necessity or a contingent, historically successful convention.').

omega_variable(
    kernel_reading_boundary_location,
    'Where exactly does the disagreement among the three sibling readings (number_reading, parmenidean_rejection, placeholder_reading) actually live — is it a dispute about zero''s arithmetic behavior (which all three could in principle accept operationally), or is it purely an ontological/semantic dispute about what ''being a number'' means, with the arithmetic rules themselves uncontested across all readings?',
    'Examine whether placeholder_reading advocates dispute Brahmagupta''s specific rules (a+0=a, a×0=0) or only dispute the further claim that satisfying these rules makes zero ''a number'' in the same sense as 1, 2, 3. If the arithmetic itself is uncontested across readings, the kernel contest is purely definitional/ontological, not operational.',
    'If the dispute is purely ontological (all readings accept the same arithmetic, disagreeing only about labeling), this constraint''s high beneficiary count and near-mountain classification is even more secure — the practical stakes are entirely in the sibling readings'' territory, and this reading describes settled operational content. If the arithmetic rules themselves are contested by some placeholder-reading traditions, the ε-invariance boundary between this reading and placeholder_reading needs re-examination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary_location, conceptual, 'Whether the kernel contest is about arithmetic content or purely about ontological labeling, which locates the true site of disagreement among readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zero_mathematical_status__number_reading, 0, 1400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zero_tr_t0, zero_mathematical_status__number_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(zero_tr_t200, zero_mathematical_status__number_reading, theater_ratio, 200, 0.07).
narrative_ontology:measurement(zero_tr_t500, zero_mathematical_status__number_reading, theater_ratio, 500, 0.05).
narrative_ontology:measurement(zero_tr_t800, zero_mathematical_status__number_reading, theater_ratio, 800, 0.04).
narrative_ontology:measurement(zero_tr_t1100, zero_mathematical_status__number_reading, theater_ratio, 1100, 0.03).
narrative_ontology:measurement(zero_tr_t1400, zero_mathematical_status__number_reading, theater_ratio, 1400, 0.03).

% Extraction over time
narrative_ontology:measurement(zero_be_t0, zero_mathematical_status__number_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(zero_be_t200, zero_mathematical_status__number_reading, base_extractiveness, 200, 0.07).
narrative_ontology:measurement(zero_be_t500, zero_mathematical_status__number_reading, base_extractiveness, 500, 0.06).
narrative_ontology:measurement(zero_be_t800, zero_mathematical_status__number_reading, base_extractiveness, 800, 0.05).
narrative_ontology:measurement(zero_be_t1100, zero_mathematical_status__number_reading, base_extractiveness, 1100, 0.05).
narrative_ontology:measurement(zero_be_t1400, zero_mathematical_status__number_reading, base_extractiveness, 1400, 0.05).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(zero_mathematical_status__number_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(zero_mathematical_status__number_reading, zero_mathematical_status__parmenidean_rejection).
narrative_ontology:affects_constraint(zero_mathematical_status__number_reading, zero_mathematical_status__placeholder_reading).

% DUAL FORMULATION NOTE:
% This story is one of three members of the zero_mathematical_status constraint family, decomposed per the ε-invariance principle because the natural-language label 'is zero a number' conflates three structurally distinct claims with different ε values: number_reading (this file, ε≈0.05, near-mountain, universal beneficiary set) treats zero as fully arithmetized; parmenidean_rejection treats zero-as-number as ontologically incoherent (a philosophical rejection constraint, likely near-mountain from its own internal logic but with a different, much smaller or historically-bounded beneficiary/adherent set); placeholder_reading treats zero as a notational convenience only (likely a rope or tangled_rope, since positional notation is a genuine coordination technology with narrower functional claims than full arithmetization). The number_reading is historically and functionally upstream of computational and algebraic practice generally, so it structurally influences the practical stakes of the other two readings' persistence without foreclosing either philosophically.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
