% ============================================================================
% CONSTRAINT STORY: zero_mathematical_status__placeholder_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_zero_mathematical_status__placeholder_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: zero_mathematical_status__placeholder_reading
 *   human_readable: Zero as Placeholder: Positional-Notation Reading of the Zero Kernel
 *   domain: history_of_mathematics/philosophy_of_mathematics
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested
 *   zero-mathematical-status kernel: the placeholder reading, under which
 *   zero functions purely as a positional-notation device (marking an empty
 *   column so 61, 601, and 6010 remain distinguishable) without carrying
 *   arithmetic operand status. This is the convention operative in Babylonian
 *   sexagesimal notation and inherited into Hellenistic astronomical tables
 *   (Ptolemy's Almagest uses a placeholder symbol descended from this
 *   tradition). It is structurally distinct from the sibling number_reading
 *   (Brahmagupta's 7th-century CE rules making zero a full arithmetic
 *   operand: a+0=a, a×0=0) and from the parmenidean_rejection (the view,
 *   associated with the Parmenidean tradition's discomfort with nonbeing,
 *   that a number denoting 'nothing' is ontologically incoherent and should
 *   not be admitted as a number at all). The three readings have materially
 *   different beneficiary/victim structures and different ε:
 *   parmenidean_rejection is closest to a mountain-claim about ontological
 *   necessity with near-zero extraction (it withholds a notational
 *   convenience but claims to do so on logical necessity, not administered
 *   rule); number_reading is a rope/tangled-rope with high coordination
 *   payoff and comparatively low extraction once broadly adopted; this
 *   placeholder_reading sits at an intermediate ε (0.38) — real coordination
 *   gain for tabulation, real and growing cost for anyone needing zero as an
 *   operand, maintained by an administered scribal/curricular convention
 *   rather than by logical necessity.
 *
 * KEY AGENTS:
 *   - babylonian_and_hellenistic_computational_scribes: beneficiary, gain tabulation efficiency
 *   - positional_notation_systems: non-agent structure vindicated by the convention
 *   - practitioners_seeking_arithmetic_closure: payer, cannot compute with zero as operand
 *   - later_algebraists_denied_zero_as_operand: payer, inherit a convention that blocks operational use in proofs
 *   - positional_notation_administrators: agenda_setter, fix and transmit the notation-only restriction
 *   - historians_of_mathematics: analytical observer reconstructing the gap
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zero_mathematical_status__placeholder_reading, 0.38).
domain_priors:suppression_score(zero_mathematical_status__placeholder_reading, 0.42).
domain_priors:theater_ratio(zero_mathematical_status__placeholder_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zero_mathematical_status__placeholder_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(zero_mathematical_status__placeholder_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(zero_mathematical_status__placeholder_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zero_mathematical_status__placeholder_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(zero_mathematical_status__placeholder_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zero_mathematical_status__placeholder_reading, tangled_rope).
narrative_ontology:human_readable(zero_mathematical_status__placeholder_reading, "Zero as Placeholder: Positional-Notation Reading of the Zero Kernel").
narrative_ontology:topic_domain(zero_mathematical_status__placeholder_reading, "history_of_mathematics/philosophy_of_mathematics").

domain_priors:requires_active_enforcement(zero_mathematical_status__placeholder_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zero_mathematical_status__placeholder_reading, '6c1c7f35-d92b-40f6-95b2-8cd0c35f4f5c').
narrative_ontology:cs_kernel_codification('6c1c7f35-d92b-40f6-95b2-8cd0c35f4f5c', distributed).
narrative_ontology:cs_authority_grounding('6c1c7f35-d92b-40f6-95b2-8cd0c35f4f5c', practice).
narrative_ontology:cs_interpretation_layer_present('6c1c7f35-d92b-40f6-95b2-8cd0c35f4f5c').
narrative_ontology:cs_reading_relation('6c1c7f35-d92b-40f6-95b2-8cd0c35f4f5c', zero_mathematical_status__number_reading, influences).
narrative_ontology:cs_reading_relation('6c1c7f35-d92b-40f6-95b2-8cd0c35f4f5c', zero_mathematical_status__parmenidean_rejection, coexists_with).
narrative_ontology:cs_axiom('6c1c7f35-d92b-40f6-95b2-8cd0c35f4f5c', foundational, zero_denotes_position_not_quantity).
narrative_ontology:cs_axiom_status(zero_denotes_position_not_quantity, overridden).
narrative_ontology:cs_axiom_grounding('6c1c7f35-d92b-40f6-95b2-8cd0c35f4f5c', zero_denotes_position_not_quantity, conventional).
narrative_ontology:cs_axiom('6c1c7f35-d92b-40f6-95b2-8cd0c35f4f5c', secondary, notational_utility_does_not_confer_operand_status).
narrative_ontology:cs_axiom_status(notational_utility_does_not_confer_operand_status, overridden).
narrative_ontology:cs_axiom_grounding('6c1c7f35-d92b-40f6-95b2-8cd0c35f4f5c', notational_utility_does_not_confer_operand_status, instrumental).
narrative_ontology:cs_reference_frame('6c1c7f35-d92b-40f6-95b2-8cd0c35f4f5c', sexagesimal_positional_placeholder_convention).
narrative_ontology:cs_drift_state('6c1c7f35-d92b-40f6-95b2-8cd0c35f4f5c', post_brahmagupta_operational_codification, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('6c1c7f35-d92b-40f6-95b2-8cd0c35f4f5c', '').
narrative_ontology:cs_kernel_id(zero_mathematical_status__placeholder_reading, zero_mathematical_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zero_mathematical_status__placeholder_reading, babylonian_and_hellenistic_computational_scribes).
narrative_ontology:constraint_beneficiary(zero_mathematical_status__placeholder_reading, positional_notation_systems).
narrative_ontology:constraint_victim(zero_mathematical_status__placeholder_reading, practitioners_seeking_arithmetic_closure).
narrative_ontology:constraint_victim(zero_mathematical_status__placeholder_reading, later_algebraists_denied_zero_as_operand).
narrative_ontology:constraint_vindicates(zero_mathematical_status__placeholder_reading, positional_place_value_efficiency_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Use a placeholder glyph (a slanted double-wedge in Babylonian cuneiform, later a similar device in Hellenistic astronomical tables) purely to keep positional columns from collapsing into ambiguity — distinguishing 61 from 601 from 6010. They gain enormous computational efficiency in tabulation and astronomy without ever needing to ask what the placeholder 'is' arithmetically; the notation solves their problem completely on its own terms.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__placeholder_reading, babylonian_and_hellenistic_computational_scribes, beneficiary,
    moderate, generational, constrained, regional).

% The sexagesimal and later positional systems themselves are vindicated and stabilized by the placeholder convention — it is the structural feature that lets positional notation scale to arbitrary magnitude without inventing new symbols. Not an actor; included for completeness as the non-agent structure that benefits.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__placeholder_reading, positional_notation_systems, beneficiary,
    institutional, civilizational, analytical, regional).
narrative_ontology:stakeholder_non_agent(zero_mathematical_status__placeholder_reading, positional_notation_systems).

% Mathematicians and merchants who need to perform operations involving the empty quantity — subtraction resulting in nothing, division problems, debt-and-asset reconciliation — find the placeholder gives them no operand to compute with. They can write the position but cannot add, subtract, multiply, or divide with it as a number. The convention leaves them structurally unable to close a broad class of otherwise-solvable arithmetic problems; the notational discipline that helps tabulation actively withholds the operational tool their work needs.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__placeholder_reading, practitioners_seeking_arithmetic_closure, payer,
    moderate, biographical, constrained, regional).

% Greek and Hellenistic mathematicians working after positional notation was established but before Brahmagupta's arithmetic rules inherit a convention that treats the placeholder as beneath ontological or operational notice (per the parmenidean tradition's persistence). They cannot cite the placeholder as authority for treating zero as a number in their own proofs, even where doing so would resolve a computation, because the reigning convention explicitly restricts it to notation.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__placeholder_reading, later_algebraists_denied_zero_as_operand, payer,
    moderate, generational, constrained, regional).

% The scribal schools, astronomical academies, and later transmission networks that fix and enforce the convention that the placeholder glyph names a position and nothing more. They set the notational rule, train scribes in it, and exclude arithmetic-operand readings from the transmitted curriculum. They benefit from a stable, teachable system and bear no cost from the operational restriction they impose.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__placeholder_reading, positional_notation_administrators, agenda_setter,
    institutional, civilizational, arbitrage, regional).

% Reconstruct the placeholder-only period from cuneiform tablets, Ptolemy's Almagest usage, and later comparison with Brahmagupta's operational rules, documenting the gap between what the notation permitted and what it foreclosed.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__placeholder_reading, historians_of_mathematics, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the genuine problem of positional ambiguity in place-value notation: without a placeholder, 61, 601, and 6010 are visually indistinguishable in a base system lacking a zero-glyph. The placeholder convention lets a small symbol set represent arbitrarily large or precisely-spaced numbers.
% TRANSFER_FUNCTION: Moves computational tractability and notational efficiency to scribes, astronomers, and administrators who tabulate large numbers, while withholding operational status (as an addable, subtractable, multipliable quantity) from anyone who needs zero to function arithmetically rather than positionally — that operational capacity is deferred to later readings of the kernel.
% ABSENT_VOICES: Merchants and accountants tracking debts and empty balances, and algebraists working on equations that would resolve cleanly if zero were an operand, are not represented in the scribal/astronomical tradition that fixes the placeholder convention; their practical need for zero-as-number goes unaddressed by a convention built for a different constituency's problem.
% DISAPPEARANCE_RATIONALE: If the placeholder-only convention had never stabilized, either positional notation would have failed to scale (forcing continued reliance on non-positional systems or ambiguous notation), or the operational reading of zero might have emerged earlier without the placeholder-first intermediate stage. Its presence shaped several centuries of the notational path mathematics actually took before Brahmagupta's arithmetic rules broke the restriction.
% FOUNDING_PROBLEM: Positional number systems generate irresolvable ambiguity between numbers like 61 and 601 without a marker for an empty column; the placeholder solves exactly this notational gap.
% FOUNDING_PROBLEM_CORROBORATION: Historians of mathematics (an analytical seat with no stake in either the placeholder or number reading) attest that the notational-ambiguity problem was fully solved once positional zero-glyphs stabilized, and that the subsequent restriction against treating zero as an operand was a separate, additional convention not required by the original placeholder problem — corroborated by the independent later resolution (Brahmagupta, 7th century CE) of the operational question without any change to the notational glyph itself.
narrative_ontology:disappearance_verdict(zero_mathematical_status__placeholder_reading, world_rearranges).
narrative_ontology:founding_problem_status(zero_mathematical_status__placeholder_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zero_mathematical_status__placeholder_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(zero_mathematical_status__placeholder_reading, 'none', 1).
narrative_ontology:epsilon_provenance(zero_mathematical_status__placeholder_reading, 0.38, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(zero_mathematical_status__placeholder_reading_tests).
:- end_tests(zero_mathematical_status__placeholder_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38) is intermediate and rising slightly over the measured interval: the placeholder convention delivers genuine, front-loaded coordination value (positional notation becomes tractable at any magnitude) but the cost to those needing zero as an operand compounds as computational and commercial practice grows more sophisticated and increasingly runs into problems only an operational zero would resolve. Suppression (0.42) reflects that the operand restriction is an administered curricular convention (scribal schools and astronomical transmission networks explicitly teach the placeholder-only reading and do not transmit operand use), not a logical necessity — it could be revised, and eventually was, without disturbing the notation itself. Theater ratio is low-moderate (0.22): the notational function is real and substantially functional, not primarily performative, though the accumulating restriction against operand status increasingly serves institutional/curricular continuity rather than continued technical necessity by the later end of the interval.
 *
 * PERSPECTIVAL GAP:
 *   From the administering/scribal seat, the placeholder-only convention is a complete, closed solution — the notational problem it was built for is fully solved and nothing further is owed. From the seat of practitioners needing operand closure, the same convention is an active, structural withholding of a tool their work requires, imposed by an institution with no stake in their problem. The engine should register this as tangled_rope: real coordination function (place-value notation) plus real asymmetric cost (operand-closure denial) held in place by curricular/institutional enforcement rather than logical necessity — distinguishing it from the parmenidean sibling (closer to mountain-claim, no administering beneficiary) and the number_reading sibling (closer to rope, coordination gain with comparatively low ongoing extraction once the operand rules stabilize).
 *
 * DIRECTIONALITY LOGIC:
 *   Scribes and the positional-notation structure itself sit near the beneficiary end: they collect the coordination value (tabulation tractability) and bear none of the operand-restriction cost. Practitioners needing arithmetic closure and later algebraists sit near the target end: they are structurally blocked from a tool that would resolve their problems, by a convention set by an institution (scribal/astronomical schools) they do not control and cannot easily route around given how embedded positional notation had become. Positional-notation administrators are the agenda-setters: institutional power, arbitrage-grade exit (they can revise the curriculum when convention shifts, as eventually happened), no cost borne from the restriction they impose.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (positional ambiguity) was fully and permanently solved by the placeholder glyph itself; the founding_problem_status is 'dead' specifically for the notational function. What persists past that resolution is the ADDITIONAL restriction against treating the placeholder as an operand — a restriction not required by the original notational problem and one that outlives its justification. This is the mandatrophy signature: the coordination function (notation) is dead-solved, but the administered restriction (no operand status) persists by curricular inertia until Brahmagupta's independent operational rules eventually displace it. Classifying this reading as tangled_rope rather than mountain or rope prevents mislabeling an administered, revisable restriction as either a logical necessity (parmenidean framing) or a costless pure coordination win (number-reading framing it is not yet).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    placeholder_vs_number_reading_boundary,
    'Is the placeholder-only convention a genuinely separate historical stage from the number_reading, or is it better read as an unstated partial anticipation of Brahmagupta''s operational rules that later readings retroactively sharpen into a distinct claim?',
    'Close philological examination of Babylonian and Hellenistic mathematical texts for any implicit operand-like usage of the placeholder glyph (e.g., in subtraction results yielding an empty quantity) that would blur the placeholder/operand boundary this story treats as sharp.',
    'If implicit operand usage is found, the placeholder_reading and number_reading are less cleanly separable than modeled here, and some of this reading''s claimed victim cost (practitioners denied arithmetic closure) may be overstated for at least some practitioner communities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(placeholder_vs_number_reading_boundary, empirical, 'Whether the placeholder/operand boundary was as sharp in practice as the reading assumes.').

omega_variable(
    administered_vs_natural_restriction,
    'Is the restriction against zero-as-operand a deliberately administered institutional convention (supporting the tangled_rope claim here) or a natural conceptual limitation of the mathematical frameworks available at the time (which would push this reading closer to a mountain claim, undermining the beneficiary/victim structure)?',
    'Comparative study of whether contemporaneous non-scribal mathematical communities (e.g., independent merchant arithmetic traditions) developed operand-like zero usage outside the administered scribal/astronomical curriculum — independent emergence would support the ''natural limitation'' reading; uniform restriction only within administered institutions would support the ''administered convention'' reading.',
    'If the restriction turns out to reflect a genuine conceptual ceiling of the era''s mathematics rather than an administered choice, the agenda_setter seat (positional_notation_administrators) is not really imposing a chosen restriction and the tangled_rope classification would weaken toward something closer to a scaffold or even a natural-limitation mountain-adjacent reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(administered_vs_natural_restriction, conceptual, 'Whether the operand restriction was chosen/administered or reflects a genuine period conceptual ceiling.').

omega_variable(
    kernel_reading_individuation,
    'Are the three declared kernel readings (placeholder, number, parmenidean) genuinely distinct constraints with different ε, or does treating them as three separate stories overstate the discreteness of what was, historically, a gradual and overlapping conceptual transition?',
    'Cross-reference the ε values and beneficiary/victim structures authored in the sibling stories (number_reading, parmenidean_rejection) once generated; check whether the historical record supports sharp transition points between the three readings or shows extended coexistence and hybrid positions within single traditions.',
    'If the readings substantially overlapped in practice (e.g., a single tradition using placeholder notation while informally treating zero as operand-like in specific computations), the ε-invariance decomposition into three stories is still correct methodology (each captures a distinct claim) but the network edges between them should carry stronger ''influences'' weight and the narrative should emphasize overlap rather than clean succession.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_individuation, conceptual, 'Whether the three kernel readings were as discrete historically as their separate-story treatment implies.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zero_mathematical_status__placeholder_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zero_tr_t0, zero_mathematical_status__placeholder_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(zero_tr_t20, zero_mathematical_status__placeholder_reading, theater_ratio, 20, 0.13).
narrative_ontology:measurement(zero_tr_t40, zero_mathematical_status__placeholder_reading, theater_ratio, 40, 0.17).
narrative_ontology:measurement(zero_tr_t60, zero_mathematical_status__placeholder_reading, theater_ratio, 60, 0.19).
narrative_ontology:measurement(zero_tr_t80, zero_mathematical_status__placeholder_reading, theater_ratio, 80, 0.21).
narrative_ontology:measurement(zero_tr_t100, zero_mathematical_status__placeholder_reading, theater_ratio, 100, 0.22).

% Extraction over time
narrative_ontology:measurement(zero_be_t0, zero_mathematical_status__placeholder_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(zero_be_t20, zero_mathematical_status__placeholder_reading, base_extractiveness, 20, 0.26).
narrative_ontology:measurement(zero_be_t40, zero_mathematical_status__placeholder_reading, base_extractiveness, 40, 0.32).
narrative_ontology:measurement(zero_be_t60, zero_mathematical_status__placeholder_reading, base_extractiveness, 60, 0.36).
narrative_ontology:measurement(zero_be_t80, zero_mathematical_status__placeholder_reading, base_extractiveness, 80, 0.38).
narrative_ontology:measurement(zero_be_t100, zero_mathematical_status__placeholder_reading, base_extractiveness, 100, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(zero_su_t0, zero_mathematical_status__placeholder_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(zero_su_t20, zero_mathematical_status__placeholder_reading, suppression_requirement, 20, 0.34).
narrative_ontology:measurement(zero_su_t40, zero_mathematical_status__placeholder_reading, suppression_requirement, 40, 0.38).
narrative_ontology:measurement(zero_su_t60, zero_mathematical_status__placeholder_reading, suppression_requirement, 60, 0.4).
narrative_ontology:measurement(zero_su_t80, zero_mathematical_status__placeholder_reading, suppression_requirement, 80, 0.41).
narrative_ontology:measurement(zero_su_t100, zero_mathematical_status__placeholder_reading, suppression_requirement, 100, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zero_mathematical_status__placeholder_reading, information_standard).
narrative_ontology:affects_constraint(zero_mathematical_status__placeholder_reading, zero_mathematical_status__number_reading).
narrative_ontology:affects_constraint(zero_mathematical_status__placeholder_reading, zero_mathematical_status__parmenidean_rejection).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the zero_mathematical_status kernel. placeholder_reading (this story) claims tangled_rope with intermediate ε (0.38): real notational coordination gain, real and growing operand-closure cost, administered by scribal/astronomical institutions. number_reading (Brahmagupta's operational rules) is expected to claim lower ε as a broader coordination win once zero's arithmetic closure is established. parmenidean_rejection is expected to claim near-zero ε as a mountain-adjacent ontological argument with no administering beneficiary. The placeholder_reading historically precedes and is displaced by the number_reading; it stands in tension with (but does not strictly logically foreclose) the parmenidean_rejection, since a notational device that denies operand status is compatible with, though not identical to, an ontological argument that denies numeric status altogether.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
