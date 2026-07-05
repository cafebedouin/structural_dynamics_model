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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: Zero as Fully Defined Number (Brahmagupta Arithmetic Reading)
 *   domain: history_of_mathematics/philosophy_of_mathematics
 *
 * SUMMARY:
 *   This story is ONE reading of the contested kernel
 *   zero_mathematical_status: the claim that zero is a fully-fledged number
 *   governed by defined arithmetic operations, as formalized by Brahmagupta's
 *   7th-century rules (a+0=a, a*0=0, and attempted but incomplete treatment
 *   of a/0). This reading treats zero's arithmetic integration as having
 *   achieved something close to mountain status within mathematics —
 *   near-universal acceptance, negligible contestation among practicing
 *   mathematicians, and a coordination function (uniform, total arithmetic
 *   operations) so thoroughly successful that essentially the entire edifice
 *   of algebra, calculus, and quantitative science rests on it without
 *   dispute. This is NOT the same constraint as the placeholder_reading (zero
 *   as mere positional notation device, no arithmetic identity) or the
 *   parmenidean_rejection (zero as ontologically incoherent, 'nothing' cannot
 *   be a number) — those are sibling constraints with their own files, their
 *   own epsilon values, and their own beneficiary/victim structures.
 *   Conflating them would violate epsilon-invariance: this reading's
 *   extraction and suppression are near-zero because within the
 *   number_reading's own frame there is no live contest and no identifiable
 *   victim, whereas a story about, e.g., suppression of the Parmenidean
 *   objection would carry entirely different metrics.
 *
 * KEY AGENTS:
 *   - mathematical_practitioners: universal beneficiary (institutional/analytical) — inherits closed arithmetic
 *   - algebraists: primary structural beneficiary (institutional/analytical) — algebra as a discipline depends on this reading
 *   - brahmagupta_and_indian_mathematical_tradition: agenda_setter (institutional/analytical) — formalized the operative rules
 *   - parmenidean_and_aristotelian_tradition: excluded sibling-reading holder (institutional/analytical) — not a victim, a different resolution of the same kernel
 *   - positional_notation_engineers: excluded/secondary beneficiary — hold the placeholder_reading instead
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
narrative_ontology:human_readable(zero_mathematical_status__number_reading, "Zero as Fully Defined Number (Brahmagupta Arithmetic Reading)").
narrative_ontology:topic_domain(zero_mathematical_status__number_reading, "history_of_mathematics/philosophy_of_mathematics").

domain_priors:emerges_naturally(zero_mathematical_status__number_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zero_mathematical_status__number_reading, 'dd6fa67b-74f1-4720-b51a-f44a96188cb6').
narrative_ontology:cs_kernel_codification('dd6fa67b-74f1-4720-b51a-f44a96188cb6', distributed).
narrative_ontology:cs_authority_grounding('dd6fa67b-74f1-4720-b51a-f44a96188cb6', expertise).
narrative_ontology:cs_interpretation_layer_present('dd6fa67b-74f1-4720-b51a-f44a96188cb6').
narrative_ontology:cs_reading_relation('dd6fa67b-74f1-4720-b51a-f44a96188cb6', zero_mathematical_status__parmenidean_rejection, forecloses).
narrative_ontology:cs_reading_relation('dd6fa67b-74f1-4720-b51a-f44a96188cb6', zero_mathematical_status__placeholder_reading, influences).
narrative_ontology:cs_axiom('dd6fa67b-74f1-4720-b51a-f44a96188cb6', foundational, zero_is_a_completed_arithmetic_object).
narrative_ontology:cs_axiom_status(zero_is_a_completed_arithmetic_object, holdable).
narrative_ontology:cs_axiom_grounding('dd6fa67b-74f1-4720-b51a-f44a96188cb6', zero_is_a_completed_arithmetic_object, conventional).
narrative_ontology:cs_axiom('dd6fa67b-74f1-4720-b51a-f44a96188cb6', foundational, additive_and_multiplicative_identity_totality).
narrative_ontology:cs_axiom_status(additive_and_multiplicative_identity_totality, holdable).
narrative_ontology:cs_axiom_grounding('dd6fa67b-74f1-4720-b51a-f44a96188cb6', additive_and_multiplicative_identity_totality, instrumental).
narrative_ontology:cs_reference_frame('dd6fa67b-74f1-4720-b51a-f44a96188cb6', brahmagupta_total_arithmetic_closure).
narrative_ontology:cs_drift_state('dd6fa67b-74f1-4720-b51a-f44a96188cb6', modern_field_axiomatization_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('dd6fa67b-74f1-4720-b51a-f44a96188cb6', '').
narrative_ontology:cs_kernel_id(zero_mathematical_status__number_reading, zero_mathematical_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zero_mathematical_status__number_reading, mathematical_practitioners).
narrative_ontology:constraint_beneficiary(zero_mathematical_status__number_reading, algebraists).
narrative_ontology:constraint_beneficiary(zero_mathematical_status__number_reading, engineers_and_scientists).
narrative_ontology:constraint_beneficiary(zero_mathematical_status__number_reading, computational_disciplines).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(zero_mathematical_status__number_reading, positional_notation_engineers).
narrative_ontology:constraint_vindicates(zero_mathematical_status__number_reading, additive_identity_axiom).
narrative_ontology:constraint_vindicates(zero_mathematical_status__number_reading, multiplicative_annihilation_axiom).
narrative_ontology:constraint_vindicates(zero_mathematical_status__number_reading, zero_as_field_element).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Use zero as a fully arithmetized number governed by defined operations (a+0=a, a-a=0, a*0=0). This integration is the load-bearing floor beneath algebra, place-value arithmetic, and later calculus. They do not administer this fact; they simply operate within a number system where it holds and where the alternative (zero excluded from arithmetic) would collapse most downstream formal machinery.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__number_reading, mathematical_practitioners, beneficiary,
    institutional, civilizational, analytical, universal).

% Depend on zero's arithmetic closure to solve equations, define polynomial roots, and construct the additive and multiplicative identity structure of rings and fields. Without zero-as-number, algebra as a general symbolic discipline does not exist in its current form.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__number_reading, algebraists, beneficiary,
    institutional, civilizational, analytical, universal).

% Apply zero-inclusive arithmetic throughout physical modeling, calculus-based engineering, and quantitative science generally. They inherit the arithmetized zero as background infrastructure and would face a fundamentally different (likely unworkable) toolkit without it.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__number_reading, engineers_and_scientists, beneficiary,
    organized, generational, analytical, global).

% Computer science and digital computation rest on zero as both a number and an identity element (binary representation, null states, additive/multiplicative identities in algorithms). The arithmetized zero is a precondition for the entire discipline's formal foundation.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__number_reading, computational_disciplines, beneficiary,
    institutional, generational, analytical, global).

% Formalized the arithmetic rules governing zero (7th century CE, Brahmasphutasiddhanta) — defining a+0=a, a-0=a, a*0=0, and grappling with (unsuccessfully, by modern lights) division by zero. This tradition set the terms under which zero entered formal arithmetic as an object with defined behavior rather than a mere placeholder or an excluded impossibility.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__number_reading, brahmagupta_and_indian_mathematical_tradition, agenda_setter,
    institutional, civilizational, analytical, global).

% Held that nothing cannot be counted as something, that void/nonbeing cannot enter the domain of number, and structured Greek mathematics accordingly (no zero as a number, no negative numbers as freely manipulable quantities). This tradition is not consulted or refuted within the number_reading's own operation — it is a sibling reading representing a different resolution of the same kernel, not a party to this constraint's arithmetic.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__number_reading, parmenidean_and_aristotelian_tradition, excluded,
    institutional, civilizational, analytical, global).

% Historical and continuing users of zero purely as a place-value marker (e.g., in positional notation without full arithmetic treatment) sit outside this reading's core claim — they benefit from notational zero but are not party to the debate over whether zero is a number with arithmetic identity. Their reading (placeholder_reading) is a distinct sibling constraint.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__number_reading, positional_notation_engineers, excluded,
    organized, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(zero_mathematical_status__number_reading, positional_notation_engineers, beneficiary).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, universally shared arithmetic behavior for zero so that addition, subtraction, and multiplication remain total, closed operations across all numbers rather than being defined only for nonzero quantities with zero handled as a special exception or excluded case.
% TRANSFER_FUNCTION: Does not principally transfer resources between parties; it transfers formal tractability — problems that were previously unrepresentable or required piecemeal casework (equations with zero roots, empty remainders, additive cancellation) become uniformly solvable once zero is admitted as a number with defined operations.
% ABSENT_VOICES: The Parmenidean/Aristotelian tradition and later finitist or constructivist skeptics of completed arithmetic objects are not represented within this reading's own operation — they hold the sibling parmenidean_rejection reading and would object that treating 'nothing' as a manipulable quantity is a category error. They are not excluded by suppression; they simply hold a structurally different resolution of the same underlying kernel.
% DISAPPEARANCE_RATIONALE: If zero's arithmetic status were withdrawn overnight — if a+0=a, a*0=0, and related rules ceased to hold as accepted mathematical fact — algebra, calculus, place-value computation, and virtually all quantitative science built atop them would require fundamental reconstruction. This is not a claim that the world would become chaotic; it is a claim that an enormous amount of formal infrastructure is load-bearing on this specific resolution of the kernel, which is why the constraint registers as consequential rather than inert.
% FOUNDING_PROBLEM: Early arithmetic systems (Babylonian, early Greek, early Chinese) lacked a consistent way to represent 'no quantity' as an operable term within addition, subtraction, and multiplication — computations involving absence of quantity required ad hoc treatment or were simply unaddressed, and equations with zero as a root or remainder had no uniform treatment.
% FOUNDING_PROBLEM_CORROBORATION: The problem's continued liveness is corroborated outside the beneficiary set by historians of mathematics (e.g., documented accounts of pre-zero arithmetic systems' representational gaps in Mesopotamian and early Chinese counting-board computation) and by the persistence of the same underlying tractability problem in any formal system that attempts to exclude zero (e.g., limitations noted in constructive mathematics debates over admitting zero and negative numbers) — this is not attested solely by mathematicians who benefit from the arithmetized zero.
narrative_ontology:disappearance_verdict(zero_mathematical_status__number_reading, world_rearranges).
narrative_ontology:founding_problem_status(zero_mathematical_status__number_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zero_mathematical_status__number_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extractiveness and suppression are authored near-zero (0.05, 0.08) because within this reading's own operation there is no coercive enforcement mechanism forcing acceptance and no identifiable party paying a cost through the arithmetic itself — the coordination benefit (closed, total arithmetic operations) is essentially unrivaled once adopted, which is why accessibility_collapse is high (0.88): once a mathematician works within a number system that includes zero's arithmetic, working without it becomes almost unthinkable for ordinary purposes. Resistance is low (0.12) because active contestation of Brahmagupta's basic rules (excluding the still-unresolved division-by-zero edge case) is essentially absent among practicing mathematicians today, though it was substantial during the reading's historical diffusion into contexts still operating under Parmenidean or placeholder frames. Theater ratio is near-zero because there is no performative maintenance apparatus — nobody enforces zero's arithmetic status through institutional theater; it simply works and is used.
 *
 * DIRECTIONALITY LOGIC:
 *   All named stakeholders in this reading sit at or very near the beneficiary end of directionality: mathematical practitioners, algebraists, engineers, and computational disciplines all draw formal tractability from zero's arithmetic integration without bearing an offsetting cost through the same structure. There is no victim group in this reading because the number_reading, taken on its own terms, does not extract from any party — the closest thing to a 'cost' is the historical transition cost borne by traditions that had to abandon zero-exclusionary arithmetic, which is a different phenomenon (a transition, not an ongoing extraction) and is not modeled as a victim here.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (representing 'no quantity' operably within total arithmetic) remains live — it is not a solved-and-abandoned mandate coasting on inertia. The founding_problem_status is authored as live, and disappearance_verdict is world_rearranges, so no capture/zombie mismatch is flagged: this is a case where the constraint's origin story and its present function actually align, which is precisely why it reads as mountain-like rather than as degraded scaffolding.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the number_reading (zero as fully arithmetized number) the historically and philosophically privileged resolution of the zero_mathematical_status kernel, or is it one contingent resolution among three live alternatives (parmenidean_rejection, placeholder_reading) that happened to win adoption for pragmatic/computational reasons rather than settled ontological necessity?',
    'Comparative history-of-mathematics analysis tracing which reading dominated in which mathematical traditions and eras, and philosophical analysis of whether ''number'' as a category is defined by arithmetic closure (favoring number_reading) or by reference to countable/measurable being (favoring parmenidean_rejection).',
    'If number_reading is contingent rather than necessary, its mountain-like accessibility_collapse (0.88) reflects successful historical convergence rather than logical inevitability — the constraint would remain classified similarly but its ontological weight would be reinterpreted as sociological/pragmatic success rather than discovered natural fact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether the number_reading''s dominance is philosophically necessary or historically contingent among three live kernel resolutions.').

omega_variable(
    division_by_zero_incompleteness,
    'Brahmagupta''s own rules for zero were incomplete (his treatment of division by zero was inconsistent and later corrected/reinterpreted). Does this incompleteness in the founding formalization undermine the claim that the number_reading achieved full arithmetic closure, or is closure achieved by later refinements (e.g., field axioms explicitly excluding zero from the multiplicative group) that are properly credited to a distinct but continuous formalization?',
    'Trace the formal lineage from Brahmagupta''s 7th-century rules through medieval Islamic and later European algebra to modern field-theoretic treatment of zero, identifying where the closure gap was formally patched.',
    'If closure was only achieved much later (e.g., 19th/20th century field axiomatization), the mountain-like near-zero extractiveness may not have held continuously across the full interval (0-1400) — the story''s flat measurement series would need revision to show a period of genuine unresolved contestation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(division_by_zero_incompleteness, empirical, 'Whether full arithmetic closure for zero was achieved by Brahmagupta or by later formal refinement.').

omega_variable(
    false_summit_check_disciplinary_capture,
    'Does the number_reading''s near-universal acceptance among mathematical practitioners partly reflect disciplinary capture — i.e., does treating zero-as-number as settled natural fact serve the interests of the algebra/calculus-dependent mathematical establishment by foreclosing philosophical reexamination of alternative foundations (e.g., constructivist or finitist arithmetics that treat zero differently)?',
    'Survey constructivist and finitist mathematical literature for live objections to unrestricted zero arithmetic (e.g., treatment of zero in intuitionistic mathematics, or debates over whether 0 should be excluded from certain number-theoretic contexts) and assess whether these represent genuine unresolved contestation suppressed by mainstream consensus, or genuinely marginal positions.',
    'If disciplinary capture is substantial, the beneficiaries declared here (mathematical_practitioners, algebraists) would trigger false-summit-mountain reclassification pressure, since a mountain with declared beneficiaries and evidence of suppressed alternatives shifts toward tangled_rope. Currently the metrics (near-zero suppression, low resistance) argue against this, but the omega is retained because beneficiaries are declared.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(false_summit_check_disciplinary_capture, conceptual, 'Whether the number_reading''s mountain-like status partly reflects mathematical disciplinary interests rather than pure natural-fact discovery.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zero_mathematical_status__number_reading, 0, 1400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zero_tr_t0, zero_mathematical_status__number_reading, theater_ratio, 0, 0.03).
narrative_ontology:measurement_basis(zero_tr_t0, observed).
narrative_ontology:measurement(zero_tr_t200, zero_mathematical_status__number_reading, theater_ratio, 200, 0.03).
narrative_ontology:measurement_basis(zero_tr_t200, observed).
narrative_ontology:measurement(zero_tr_t500, zero_mathematical_status__number_reading, theater_ratio, 500, 0.03).
narrative_ontology:measurement_basis(zero_tr_t500, observed).
narrative_ontology:measurement(zero_tr_t800, zero_mathematical_status__number_reading, theater_ratio, 800, 0.03).
narrative_ontology:measurement_basis(zero_tr_t800, observed).
narrative_ontology:measurement(zero_tr_t1100, zero_mathematical_status__number_reading, theater_ratio, 1100, 0.03).
narrative_ontology:measurement_basis(zero_tr_t1100, observed).
narrative_ontology:measurement(zero_tr_t1400, zero_mathematical_status__number_reading, theater_ratio, 1400, 0.03).
narrative_ontology:measurement_basis(zero_tr_t1400, observed).

% Extraction over time
narrative_ontology:measurement(zero_be_t0, zero_mathematical_status__number_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement_basis(zero_be_t0, observed).
narrative_ontology:measurement(zero_be_t200, zero_mathematical_status__number_reading, base_extractiveness, 200, 0.05).
narrative_ontology:measurement_basis(zero_be_t200, observed).
narrative_ontology:measurement(zero_be_t500, zero_mathematical_status__number_reading, base_extractiveness, 500, 0.05).
narrative_ontology:measurement_basis(zero_be_t500, observed).
narrative_ontology:measurement(zero_be_t800, zero_mathematical_status__number_reading, base_extractiveness, 800, 0.05).
narrative_ontology:measurement_basis(zero_be_t800, observed).
narrative_ontology:measurement(zero_be_t1100, zero_mathematical_status__number_reading, base_extractiveness, 1100, 0.05).
narrative_ontology:measurement_basis(zero_be_t1100, observed).
narrative_ontology:measurement(zero_be_t1400, zero_mathematical_status__number_reading, base_extractiveness, 1400, 0.05).
narrative_ontology:measurement_basis(zero_be_t1400, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(zero_mathematical_status__number_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zero_mathematical_status__number_reading, information_standard).
narrative_ontology:boltzmann_floor_override(zero_mathematical_status__number_reading, 0.02).
narrative_ontology:affects_constraint(zero_mathematical_status__number_reading, zero_mathematical_status__parmenidean_rejection).
narrative_ontology:affects_constraint(zero_mathematical_status__number_reading, zero_mathematical_status__placeholder_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the zero_mathematical_status kernel. number_reading (this file) treats zero as a fully arithmetized number; parmenidean_rejection treats zero as ontologically incoherent as a number; placeholder_reading treats zero as a positional notation device without arithmetic identity. Each has a distinct epsilon, distinct beneficiary/victim structure, and distinct claimed_type — they are linked via network edges rather than merged into one observer-relative constraint, per the epsilon-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
