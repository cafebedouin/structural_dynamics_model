% ============================================================================
% CONSTRAINT STORY: zero_mathematical_status__placeholder_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: zero_mathematical_status__placeholder_reading
 *   human_readable: Zero as Positional Placeholder, Not Arithmetic Number
 *   domain: history_of_mathematics/philosophy_of_mathematics
 *
 * SUMMARY:
 *   This story instantiates the placeholder reading of the contested
 *   zero_mathematical_status kernel: the claim that zero, as it functions in
 *   Babylonian and Hellenistic positional notation, is a notational device
 *   disambiguating column position — not a number bearing arithmetic
 *   properties like additive identity or multiplicative absorption. The
 *   reading permits zero in writing but withholds it from operations. Over
 *   the interval, the scribal institution that maintains this convention
 *   increasingly bears the cost of its own incompleteness: as commercial and
 *   proto-algebraic practice grows more sophisticated and needs zero to
 *   behave as an operand (null remainders, balance-to-zero calculations), the
 *   placeholder-only convention imposes rising friction on practitioners who
 *   must work around what the notation refuses to grant. This is a distinct
 *   constraint from the number_reading (Brahmagupta's arithmetic zero, a
 *   different kernel reading with a different, higher initial ε because it
 *   actively displaces the placeholder convention's institutional authority)
 *   and from parmenidean_rejection (which denies zero any coherent status at
 *   all, notational or numerical, and would show near-total accessibility
 *   collapse and different beneficiaries — the ontologically committed
 *   philosophical schools rather than scribal institutions). Each reading is
 *   authored as its own file per the ε-invariance principle; this file's ε is
 *   stable and reflects only the placeholder convention's own trajectory.
 *
 * KEY AGENTS:
 *   - babylonian_scribal_class: agenda_setter/beneficiary (institutional/arbitrage) — administers and benefits from the notation-only convention
 *   - positional_notation_users: beneficiary (organized/constrained) — gains bookkeeping efficiency
 *   - commercial_arithmetic_practitioners: payer (moderate/constrained) — bears workaround costs when zero must function as an operand
 *   - students_of_early_algebra: payer (powerless/trapped) — inherits a conceptual gap requiring relearning
 *   - historians_of_mathematics: observer (analytical/analytical) — reconstructs the tradeoff across traditions
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
narrative_ontology:human_readable(zero_mathematical_status__placeholder_reading, "Zero as Positional Placeholder, Not Arithmetic Number").
narrative_ontology:topic_domain(zero_mathematical_status__placeholder_reading, "history_of_mathematics/philosophy_of_mathematics").

domain_priors:requires_active_enforcement(zero_mathematical_status__placeholder_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zero_mathematical_status__placeholder_reading, 'bc27b268-f689-4eca-9ef6-bcc4711c672c').
narrative_ontology:cs_kernel_codification('bc27b268-f689-4eca-9ef6-bcc4711c672c', implicit).
narrative_ontology:cs_authority_grounding('bc27b268-f689-4eca-9ef6-bcc4711c672c', practice).
narrative_ontology:cs_interpretation_layer_present('bc27b268-f689-4eca-9ef6-bcc4711c672c').
narrative_ontology:cs_reading_relation('bc27b268-f689-4eca-9ef6-bcc4711c672c', zero_mathematical_status__parmenidean_rejection, coexists_with).
narrative_ontology:cs_reading_relation('bc27b268-f689-4eca-9ef6-bcc4711c672c', zero_mathematical_status__number_reading, influences).
narrative_ontology:cs_axiom('bc27b268-f689-4eca-9ef6-bcc4711c672c', foundational, zero_permitted_as_marker_not_operand).
narrative_ontology:cs_axiom_status(zero_permitted_as_marker_not_operand, holdable).
narrative_ontology:cs_axiom_grounding('bc27b268-f689-4eca-9ef6-bcc4711c672c', zero_permitted_as_marker_not_operand, conventional).
narrative_ontology:cs_axiom('bc27b268-f689-4eca-9ef6-bcc4711c672c', secondary, notational_efficiency_does_not_require_arithmetic_closure).
narrative_ontology:cs_axiom_status(notational_efficiency_does_not_require_arithmetic_closure, holdable).
narrative_ontology:cs_axiom_grounding('bc27b268-f689-4eca-9ef6-bcc4711c672c', notational_efficiency_does_not_require_arithmetic_closure, instrumental).
narrative_ontology:cs_reference_frame('bc27b268-f689-4eca-9ef6-bcc4711c672c', scribal_positional_notation_convention).
narrative_ontology:cs_drift_state('bc27b268-f689-4eca-9ef6-bcc4711c672c', post_brahmagupta_contact_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('bc27b268-f689-4eca-9ef6-bcc4711c672c', '').
narrative_ontology:cs_kernel_id(zero_mathematical_status__placeholder_reading, zero_mathematical_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zero_mathematical_status__placeholder_reading, babylonian_scribal_class).
narrative_ontology:constraint_beneficiary(zero_mathematical_status__placeholder_reading, positional_notation_users).
narrative_ontology:constraint_beneficiary(zero_mathematical_status__placeholder_reading, hellenistic_astronomers).
narrative_ontology:constraint_victim(zero_mathematical_status__placeholder_reading, commercial_arithmetic_practitioners).
narrative_ontology:constraint_victim(zero_mathematical_status__placeholder_reading, students_of_early_algebra).
narrative_ontology:constraint_vindicates(zero_mathematical_status__placeholder_reading, positional_place_value_efficiency_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the sexagesimal positional system and trains successive generations of scribes in it. Uses a placeholder mark to disambiguate columns in large numbers but never treats the mark as a quantity that can be added, subtracted, or multiplied. Their professional standing rests on mastery of a notation whose efficiency benefits depend on the placeholder existing only as a gap-marker, never as an operand.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__placeholder_reading, babylonian_scribal_class, agenda_setter,
    institutional, civilizational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(zero_mathematical_status__placeholder_reading, babylonian_scribal_class, beneficiary).

% Merchants, administrators, and record-keepers who gain enormous efficiency from positional notation for large-number bookkeeping. They benefit from the placeholder's disambiguating function without needing zero to behave arithmetically — their transactions rarely require operating on zero itself, only reading it correctly in a column.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__placeholder_reading, positional_notation_users, beneficiary,
    organized, generational, constrained, regional).

% Adopt the Babylonian placeholder within sexagesimal astronomical tables to track planetary positions across long time series. They need the notation's precision but have no practical need to add zero to a nonzero quantity within their calculations, so the placeholder-only convention costs them nothing they perceive.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__placeholder_reading, hellenistic_astronomers, beneficiary,
    organized, generational, constrained, regional).

% Traders and accountants who need to compute running balances, debts, and remainders that legitimately reach a null value or require operations across a zero boundary. Denied a number that participates in addition and subtraction, they must construct workarounds — special-case rules, avoided computations, or informal ad hoc treatments — every time a calculation would otherwise pass through nothing.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__placeholder_reading, commercial_arithmetic_practitioners, payer,
    moderate, biographical, constrained, regional).

% Learners trained within a tradition that withholds full numerical status from zero inherit a conceptual gap: they can read and write it in a column but cannot use it as a term in an equation. When later confronted with problems that require treating zero as a quantity (subtracting a number from itself, tracking a null remainder algebraically), they must relearn the concept from a different tradition, at direct cost to their own comprehension.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__placeholder_reading, students_of_early_algebra, payer,
    powerless, biographical, trapped, regional).

% Mathematical communities (notably later Indian mathematicians under Brahmagupta) that treat zero as a full number with defined arithmetic operations are not part of the placeholder tradition's institutional conversation. Their competing formalization is not refuted within the placeholder tradition — it is simply not engaged, because the placeholder tradition's practitioners do not need it for their notational purposes.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__placeholder_reading, rival_traditions_number_reading, excluded,
    organized, generational, constrained, regional).

% Reconstruct which ancient traditions treated zero as notation versus number from surviving texts and tablets. They document that the placeholder convention persisted for over a millennium in some traditions even as arithmetic-zero conventions matured elsewhere, and assess the efficiency-versus-closure tradeoff each convention represents.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__placeholder_reading, historians_of_mathematics, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(zero_mathematical_status__placeholder_reading, babylonian_scribal_class).
narrative_ontology:fixing_cost_class(zero_mathematical_status__placeholder_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the genuine problem of disambiguating positional magnitude (distinguishing 23 from 203 from 230) in a place-value system, without requiring the community to resolve the deeper question of whether 'nothing' is the kind of thing that can be added or multiplied.
% TRANSFER_FUNCTION: Moves computational efficiency toward large-number notation and bookkeeping (benefiting scribes, merchants, astronomers who need columnar precision) while moving the cost of an incomplete arithmetic onto practitioners and learners who need zero to function as an operand — they inherit gaps, workarounds, and relearning costs the notation-only convention refuses to close.
% ABSENT_VOICES: Practitioners of algebraic and equation-based reasoning who would need zero as a term are not represented in the scribal institutions that set the notational convention; their objection — that a full number is needed, not just a placeholder — surfaces only in rival traditions elsewhere, not within this tradition's own institutional voice.
% DISAPPEARANCE_RATIONALE: If the placeholder-only convention were abandoned in favor of treating zero as a full arithmetic number, the scribal class's specialized notational expertise would lose some of its distinguishing value, computational practice would need to absorb new arithmetic rules, and commercial/algebraic practitioners currently working around the gap would gain a tool they currently lack — a real reallocation of who can compute what, not a return to a prior natural state.
% FOUNDING_PROBLEM: Positional notation systems needed a way to mark an empty column so that magnitude could be read unambiguously (distinguishing 23 from 203) without requiring a settled philosophical or arithmetic theory of 'nothing.'
% FOUNDING_PROBLEM_CORROBORATION: Historians of mathematics outside the scribal tradition (working from comparative tablet and manuscript analysis) attest that the disambiguation problem was solved by the placeholder mark within a few centuries of its introduction, and that the tradition's continued refusal to extend zero to arithmetic status past that point reflects institutional conservatism and pedagogical inertia in the scribal curriculum, not an unresolved notational need. No source internal to the scribal tradition itself corroborates this from outside — the tradition's own practitioners treat the placeholder-only convention as simply how numbers are written, not as a choice with a history.
narrative_ontology:disappearance_verdict(zero_mathematical_status__placeholder_reading, world_rearranges).
narrative_ontology:founding_problem_status(zero_mathematical_status__placeholder_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zero_mathematical_status__placeholder_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extractiveness starts low (0.18) reflecting the convention's genuine early coordination value — disambiguating magnitude was a real problem with no significant victims at first — and rises to 0.38 by the interval's end as commercial and algebraic practice outgrows what pure notation can support, and the scribal institution's continued enforcement of the notation-only convention (through curriculum, professional gatekeeping, and resistance to competing formalizations) increasingly extracts relearning and workaround costs from those who need arithmetic closure. Suppression rises correspondingly (0.25 to 0.42) as the institutional investment in notation-only teaching hardens against absorbing the rival number_reading. Theater ratio stays low-to-moderate throughout (0.08 to 0.22): the coordination function (disambiguating columns) remains substantially real and functional across the whole interval, this is not primarily a performative constraint, but a growing share of the scribal class's insistence on the distinction serves professional boundary-maintenance rather than a live notational need.
 *
 * PERSPECTIVAL GAP:
 *   From the scribal agenda-setter's seat, the placeholder-only convention is simply how numbers are correctly written — an efficient, uncontested notational tool. From the commercial-arithmetic and algebra-student payer seats, the same convention is an artificial ceiling that blocks legitimate computation and imposes avoidable relearning costs. The engine computes these as different seat-level classifications from the same structural data; the divergence is the point, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (scribal class, notation users, astronomers) get low d because the convention subsidizes exactly what they need — disambiguation without arithmetic complexity — and their exit options (arbitrage, constrained-but-comfortable) reflect that they are not straining against the constraint. Victims (commercial practitioners, algebra students) get high d because they need something the convention structurally withholds, and their exit options (constrained, trapped) reflect that leaving the tradition means re-deriving arithmetic zero from elsewhere or absorbing the workaround cost themselves.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (columnar disambiguation) is dead as a live justification for withholding arithmetic status — it was solved early and permanently by the placeholder mark itself. What remains live is institutional conservatism: the scribal curriculum's continued insistence that zero is notation-only, long after computational needs exceeded what pure notation can support. Classifying this as tangled_rope (rather than mountain, which the scribal class's own framing would suggest, or pure snare) captures that a genuine coordination function existed and partly still exists, while an asymmetric cost has grown on top of it and is actively maintained by curricular and professional enforcement — exactly the hybrid the type is built to name.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    notation_vs_number_boundary_stability,
    'Is the placeholder/number boundary a stable conceptual distinction the scribal tradition consciously maintains, or an artifact of what their computational practice happened not to require yet?',
    'Examine whether surviving Babylonian and Hellenistic mathematical texts show any explicit argument for withholding arithmetic status from zero, versus simply never posing the question because their calculations never required operating across a zero boundary.',
    'If the boundary is an active conceptual commitment, the constraint is more mountain-like within its tradition (a settled convention, not enforced extraction). If it is merely an absence of need that later hardened into curricular practice once the need arose elsewhere, the tangled_rope classification is stronger — the coordination story becomes retroactive cover for what is now institutional inertia.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(notation_vs_number_boundary_stability, conceptual, 'Whether the notation-only boundary was a deliberate commitment or an unexamined default that later became extractive.').

omega_variable(
    cross_reading_contamination_risk,
    'As the number_reading tradition (Brahmagupta-style arithmetic zero) becomes known to placeholder-tradition practitioners, does the placeholder convention''s persistence become active suppression of a known alternative, rather than simple non-engagement?',
    'Track historical contact points between traditions (trade routes, translated texts, scholarly exchange) and whether placeholder-tradition institutions engaged with, dismissed, or simply never encountered arithmetic-zero formalizations.',
    'If contact occurred and the placeholder tradition actively resisted adopting arithmetic zero despite awareness, the suppression component of this constraint is higher and more deliberate than the current 0.42 terminal value reflects — this would push the classification further toward snare within the affected sub-population. If no contact occurred, the exclusion of rival traditions is structural isolation, not suppression.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cross_reading_contamination_risk, empirical, 'Whether the exclusion of the arithmetic-zero alternative was passive isolation or active resistance to a known competitor.').

omega_variable(
    kernel_framing_underdetermination,
    'Is ''zero''s mathematical status'' genuinely one kernel with three readings, or does the placeholder tradition simply never engage the question the other two readings are answering — making this less a rival reading and more a non-participant in the debate?',
    'Assess whether placeholder-tradition sources treat the number-vs-notation distinction as a live question they resolve in favor of notation, or whether the question (does zero have arithmetic properties?) is never posed within the tradition at all.',
    'If the placeholder tradition never poses the question, this constraint''s coexists_with relations to the siblings are better described as parallel non-intersection than genuine contest — a conceptual framing choice that affects how the reading_relations should be read, though it does not change the authored ε or structural data.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the placeholder tradition is a genuine rival reading of the kernel question or simply outside the question''s scope.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zero_mathematical_status__placeholder_reading, 0, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zero_tr_t0, zero_mathematical_status__placeholder_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(zero_tr_t0, observed).
narrative_ontology:measurement(zero_tr_t200, zero_mathematical_status__placeholder_reading, theater_ratio, 200, 0.1).
narrative_ontology:measurement_basis(zero_tr_t200, observed).
narrative_ontology:measurement(zero_tr_t400, zero_mathematical_status__placeholder_reading, theater_ratio, 400, 0.13).
narrative_ontology:measurement_basis(zero_tr_t400, observed).
narrative_ontology:measurement(zero_tr_t600, zero_mathematical_status__placeholder_reading, theater_ratio, 600, 0.16).
narrative_ontology:measurement_basis(zero_tr_t600, observed).
narrative_ontology:measurement(zero_tr_t900, zero_mathematical_status__placeholder_reading, theater_ratio, 900, 0.19).
narrative_ontology:measurement_basis(zero_tr_t900, observed).
narrative_ontology:measurement(zero_tr_t1200, zero_mathematical_status__placeholder_reading, theater_ratio, 1200, 0.22).
narrative_ontology:measurement_basis(zero_tr_t1200, observed).

% Extraction over time
narrative_ontology:measurement(zero_be_t0, zero_mathematical_status__placeholder_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(zero_be_t0, observed).
narrative_ontology:measurement(zero_be_t200, zero_mathematical_status__placeholder_reading, base_extractiveness, 200, 0.22).
narrative_ontology:measurement_basis(zero_be_t200, observed).
narrative_ontology:measurement(zero_be_t400, zero_mathematical_status__placeholder_reading, base_extractiveness, 400, 0.27).
narrative_ontology:measurement_basis(zero_be_t400, observed).
narrative_ontology:measurement(zero_be_t600, zero_mathematical_status__placeholder_reading, base_extractiveness, 600, 0.31).
narrative_ontology:measurement_basis(zero_be_t600, observed).
narrative_ontology:measurement(zero_be_t900, zero_mathematical_status__placeholder_reading, base_extractiveness, 900, 0.35).
narrative_ontology:measurement_basis(zero_be_t900, observed).
narrative_ontology:measurement(zero_be_t1200, zero_mathematical_status__placeholder_reading, base_extractiveness, 1200, 0.38).
narrative_ontology:measurement_basis(zero_be_t1200, observed).

% Suppression requirement over time
narrative_ontology:measurement(zero_su_t0, zero_mathematical_status__placeholder_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement_basis(zero_su_t0, observed).
narrative_ontology:measurement(zero_su_t200, zero_mathematical_status__placeholder_reading, suppression_requirement, 200, 0.3).
narrative_ontology:measurement_basis(zero_su_t200, observed).
narrative_ontology:measurement(zero_su_t400, zero_mathematical_status__placeholder_reading, suppression_requirement, 400, 0.34).
narrative_ontology:measurement_basis(zero_su_t400, observed).
narrative_ontology:measurement(zero_su_t600, zero_mathematical_status__placeholder_reading, suppression_requirement, 600, 0.37).
narrative_ontology:measurement_basis(zero_su_t600, observed).
narrative_ontology:measurement(zero_su_t900, zero_mathematical_status__placeholder_reading, suppression_requirement, 900, 0.4).
narrative_ontology:measurement_basis(zero_su_t900, observed).
narrative_ontology:measurement(zero_su_t1200, zero_mathematical_status__placeholder_reading, suppression_requirement, 1200, 0.42).
narrative_ontology:measurement_basis(zero_su_t1200, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zero_mathematical_status__placeholder_reading, information_standard).
narrative_ontology:boltzmann_floor_override(zero_mathematical_status__placeholder_reading, 0.03).
narrative_ontology:affects_constraint(zero_mathematical_status__placeholder_reading, zero_mathematical_status__number_reading).
narrative_ontology:affects_constraint(zero_mathematical_status__placeholder_reading, zero_mathematical_status__parmenidean_rejection).

% DUAL FORMULATION NOTE:
% Three sibling constraints decompose the natural-language concept 'zero's mathematical status' per the ε-invariance principle: placeholder_reading (this file, notation-only, ε=0.38 terminal, tangled_rope), number_reading (arithmetic zero with Brahmagupta's rules, a distinct constraint with its own beneficiary structure and likely lower steady-state ε once closure is internalized), and parmenidean_rejection (denies zero any coherent status, near-total accessibility_collapse, a philosophical-tradition constraint with different beneficiaries entirely). All three are linked via affects_constraints because historically the number_reading's emergence exerted downstream pressure on the placeholder_reading's institutional legitimacy (practitioners increasingly compared the two conventions), and the parmenidean_rejection historically preceded and delayed both notational and arithmetic acceptance of zero in the traditions it influenced.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
