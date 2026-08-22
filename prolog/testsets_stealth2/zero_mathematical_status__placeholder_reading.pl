% ============================================================================
% CONSTRAINT STORY: zero_mathematical_status__placeholder_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
 *   human_readable: Zero as Empty-Place Mark Only (Placeholder Reading)
 *   domain: conceptual/history_of_mathematics
 *
 * SUMMARY:
 *   The arrangement under contest: a shared sign for the empty place in
 *   positional numeral systems, permitted everywhere in notation and admitted
 *   nowhere into the operations. Its coordination surface is real —
 *   positional tables, long-distance ledgers, and multi-generational copying
 *   chains depend on emptiness being legible — and its exclusion surface is
 *   equally real: practitioners whose computations produce vanished
 *   quantities pay in workarounds and blocked generalizations, and the
 *   boundary is actively patrolled by the schools that certify which signs
 *   may compute. KEY AGENTS (by structural relationship):
 *   positional_astronomer_scribes — primary beneficiary
 *   (organized/constrained), inherit and reproduce the tables the mark makes
 *   reliable; indian_algebraists — primary target (moderate/mobile), absorb
 *   the operational gap; mercantile_ledger_keepers — secondary beneficiary
 *   with cost exposure (organized/mobile); scholastic_gatekeepers — agenda
 *   setter (institutional/arbitrage), administer the notation/quantity
 *   boundary; modern_historians_of_mathematics — analytical observer. Family
 *   decomposition note: the colloquial label 'zero's mathematical status'
 *   covers three structurally distinct arrangements with different extraction
 *   profiles — this file instantiates the placeholder reading only; the
 *   sibling files (zero_mathematical_status__number_reading,
 *   zero_mathematical_status__parmenidean_rejection) carry their own epsilon,
 *   victims, and classification, linked via network.affects_constraints.
 *   Claim/metric independence: the claimed type is what I judge structurally
 *   true of the arrangement; the metric values are what I judge descriptively
 *   true of its operation; neither is tuned to the other or to a predicted
 *   engine output.
 *
 * KEY AGENTS:
 *   - positional_astronomer_scribes: primary beneficiary (organized/constrained) — the mark makes their inherited tables transcribable; exit means abandoning the corpus
 *   - indian_algebraists: primary target (moderate/mobile) — bear the operational gap wherever computation cancels to nothing
 *   - mercantile_ledger_keepers: secondary beneficiary with covert cost exposure (organized/mobile) — notation subsidy up front, zeroless balances and tamper risk underneath
 *   - scholastic_gatekeepers: agenda setter (institutional/arbitrage) — certify which signs enter computation and collect standing from the boundary's stability
 *   - modern_historians_of_mathematics: analytical observer (analytical/analytical) — sees all three readings as stages of one debate from outside every party
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zero_mathematical_status__placeholder_reading, 0.5).
domain_priors:suppression_score(zero_mathematical_status__placeholder_reading, 0.58).
domain_priors:theater_ratio(zero_mathematical_status__placeholder_reading, 0.19).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zero_mathematical_status__placeholder_reading, extractiveness, 0.5).
narrative_ontology:constraint_metric(zero_mathematical_status__placeholder_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(zero_mathematical_status__placeholder_reading, theater_ratio, 0.19).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zero_mathematical_status__placeholder_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(zero_mathematical_status__placeholder_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zero_mathematical_status__placeholder_reading, tangled_rope).
narrative_ontology:human_readable(zero_mathematical_status__placeholder_reading, "Zero as Empty-Place Mark Only (Placeholder Reading)").
narrative_ontology:topic_domain(zero_mathematical_status__placeholder_reading, "conceptual/history_of_mathematics").

domain_priors:requires_active_enforcement(zero_mathematical_status__placeholder_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zero_mathematical_status__placeholder_reading, '251a144f-03a9-4086-a2cf-d55b6f93a7b8').
narrative_ontology:cs_kernel_codification('251a144f-03a9-4086-a2cf-d55b6f93a7b8', distributed).
narrative_ontology:cs_authority_grounding('251a144f-03a9-4086-a2cf-d55b6f93a7b8', practice).
narrative_ontology:cs_interpretation_layer_present('251a144f-03a9-4086-a2cf-d55b6f93a7b8').
narrative_ontology:cs_reading_relation('251a144f-03a9-4086-a2cf-d55b6f93a7b8', zero_mathematical_status__number_reading, influences).
narrative_ontology:cs_reading_relation('251a144f-03a9-4086-a2cf-d55b6f93a7b8', zero_mathematical_status__parmenidean_rejection, coexists_with).
narrative_ontology:cs_axiom('251a144f-03a9-4086-a2cf-d55b6f93a7b8', foundational, empty_place_sign_carries_no_operand_status).
narrative_ontology:cs_axiom_status(empty_place_sign_carries_no_operand_status, holdable).
narrative_ontology:cs_axiom_grounding('251a144f-03a9-4086-a2cf-d55b6f93a7b8', empty_place_sign_carries_no_operand_status, conventional).
narrative_ontology:cs_axiom('251a144f-03a9-4086-a2cf-d55b6f93a7b8', secondary, operations_defined_over_quantities_not_marks).
narrative_ontology:cs_axiom_status(operations_defined_over_quantities_not_marks, holdable).
narrative_ontology:cs_axiom_grounding('251a144f-03a9-4086-a2cf-d55b6f93a7b8', operations_defined_over_quantities_not_marks, conventional).
narrative_ontology:cs_reference_frame('251a144f-03a9-4086-a2cf-d55b6f93a7b8', placeholder_notation_regime).
narrative_ontology:cs_drift_state('251a144f-03a9-4086-a2cf-d55b6f93a7b8', contemporary, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('251a144f-03a9-4086-a2cf-d55b6f93a7b8', '').
narrative_ontology:cs_kernel_id(zero_mathematical_status__placeholder_reading, zero_mathematical_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zero_mathematical_status__placeholder_reading, positional_astronomer_scribes).
narrative_ontology:constraint_beneficiary(zero_mathematical_status__placeholder_reading, mercantile_ledger_keepers).
narrative_ontology:constraint_victim(zero_mathematical_status__placeholder_reading, indian_algebraists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(zero_mathematical_status__placeholder_reading, mercantile_ledger_keepers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Copy and compute ephemerides, almanacs, and interpolation tables in sexagesimal or vigesimal positional form. The empty-place mark lets them write gaps at the sixties-column unambiguously, so tables survive transcription chains without silent corruption. Their lock is infrastructural rather than self-conceptual: leaving the convention means abandoning inherited tables and retraining in rhetorical numeration, which no working school does.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__placeholder_reading, positional_astronomer_scribes, beneficiary,
    organized, biographical, constrained, regional).

% Solve equations arising in commercial and astronomical work where quantities cancel to nothing or coefficients vanish. Under the convention they must phrase vanishing quantities as absences, breaking algorithmic uniformity exactly where their methods generalize furthest. Their escape route is real and was eventually taken: nothing physical prevents extending the sign into the operations, and the codification of that extension is precisely the sibling reading.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__placeholder_reading, indian_algebraists, payer,
    moderate, biographical, mobile, regional).

% Keep credit-and-debt ledgers in positional columns. On the credit side they gain compact entries and fast column arithmetic; on the debit side they pay twice — when a balance lands on the empty mark the convention offers no rule for adding or subtracting it, and an empty place in a durable record is one pen-stroke from a digit, shifting verification risk onto whoever audits the books.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__placeholder_reading, mercantile_ledger_keepers, beneficiary,
    organized, immediate, mobile, regional).
narrative_ontology:stakeholder_secondary_role(zero_mathematical_status__placeholder_reading, mercantile_ledger_keepers, payer).

% Administer the line between legitimate quantities and mere marks in temple schools, academies, and the transmission houses that certify texts. They decide which signs may enter computation and which remain notation, train the next generation in that decision, and collect standing from the doctrine's stability. Rescinding the exclusion is within their unilateral power; the cost to them is the boundary their authority patrols.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__placeholder_reading, scholastic_gatekeepers, agenda_setter,
    institutional, generational, arbitrage, continental).

% Reconstruct the transmission of the empty-place convention from Babylonian tables through Hellenistic astronomy into Sanskrit computational practice, and date the moment the sign acquired operand status. They hold no position in the arrangement and attest its history from outside every benefiting party.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__placeholder_reading, modern_historians_of_mathematics, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(zero_mathematical_status__placeholder_reading, scholastic_gatekeepers).
narrative_ontology:fixing_cost_class(zero_mathematical_status__placeholder_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives every user of a positional numeral system a shared, script-independent mark for an empty place, so that large numbers and precise fractional values survive transcription without ambiguity.
% TRANSFER_FUNCTION: Moves representational labor savings to every user of positional records; concentrates the cost of vanished quantities on the practitioners whose problems produce them; and shifts tamper risk from writers of records to their readers, since an empty mark is one stroke from a digit.
% ABSENT_VOICES: The practitioners most damaged — solvers whose computations cancel to nothing — sat at the table's edge, their objection preserved as awkward phrasings rather than doctrine. Structurally absent: the future algebraist, whose problem classes did not yet exist to be silenced; non-scribal computing traditions (finger-reckoners, abacus users) who never entered the schools that certified signs; and record auditors who carried the tamper risk with no seat in the conversation.
% DISAPPEARANCE_RATIONALE: Remove the empty-place convention overnight and every inherited astronomical table goes ambiguous at its blank columns, copying chains corrupt large numbers silently, and long-distance bookkeeping loses its column discipline — the positional economy of records would rearrange around some replacement marker or collapse back into rhetorical numeration.
% FOUNDING_PROBLEM: Babylonian and Hellenistic scribes needed to record large numbers and precise fractional tables reliably across copying: a blank column in a positional string is invisible corruption. The arrangement was built to make emptiness legible.
% FOUNDING_PROBLEM_CORROBORATION: Modern historiography of numeration — outside every benefiting party — attests the notational core: analyses of Babylonian tables and Hellenistic sexagesimal practice identify empty-place marking as the arrangement's founding and still-serving function. The survival of placeholder behavior inside every contemporary numeral system corroborates that the founding problem remains live, even though the operational exclusion, the contested half of the arrangement, has been abandoned.
narrative_ontology:disappearance_verdict(zero_mathematical_status__placeholder_reading, world_rearranges).
narrative_ontology:founding_problem_status(zero_mathematical_status__placeholder_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zero_mathematical_status__placeholder_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(zero_mathematical_status__placeholder_reading, 'none', 1).
narrative_ontology:epsilon_provenance(zero_mathematical_status__placeholder_reading, 0.5, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(zero_mathematical_status__placeholder_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(zero_mathematical_status__placeholder_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(zero_mathematical_status__placeholder_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is intermediate and rising (0.28 to 0.50 across the interval): at the start the arrangement is nearly pure service — tabular astronomy rarely produces quantities the convention cannot name — and the costs accumulate as computational practice deepens and more problems land on the unnamed sign. Suppression tracks enforcement capacity, which hardened as defection became concrete: once the extension into operations was codified as a live alternative, retaining the exclusion required active curricular and doctrinal maintenance rather than mere habit. Theater stays low throughout — the notational function is load-bearing, not performed — with a slight rise as legacy adherence outlived operative need in transmission centers. Resistance is moderate: the harmed seats pushed continuously and eventually produced the codified alternative, but no violent rupture occurred. Accessibility collapse is low: the alternative never vanished from view; it was always one treatise deep. All three tracked series run on one shared eight-point grid so no metric borrows another's end-state at earlier times. Suppression here is predominantly structural-institutional (certification of which signs may compute, inheritance of tables and training), with a light internalized component (mistrust of the cipher taught as prudence); the scalar does not split the two, and the split is not load-bearing for this story's open questions.
 *
 * PERSPECTIVAL GAP:
 *   An astronomer scribe experiences the convention as transparent service: tables copy true, gaps are loud, nothing is taken from him. An Indian algebraist meets the wall mid-algorithm: his method generates a quantity the arrangement refuses to name, and he pays in circumlocution and blocked generality. The gatekeeper experiences neither benefit nor cost — he experiences a boundary, and the standing that accrues from patrolling it. Same sign, three different constraints depending on seat; the engine computes that divergence from power and exit structure, not from anyone's testimony.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (positional_astronomer_scribes, mercantile_ledger_keepers) sit near the subsidized pole: the arrangement pays them in fidelity and compression. The declared victim (indian_algebraists) sits near the target pole; his mobile exit damps effective extraction somewhat relative to a trapped target — departure was possible and was ultimately taken, which is why the trajectory bends upward as the remaining population sorts toward those who stay. The gatekeeper is not declared a beneficiary because what he collects is standing rather than product, but his enforcement role plus arbitrage exit place him near the beneficiary end regardless. Ledger keepers carry a dual position: notation subsidy on the credit side, zeroless balancing and tamper exposure on the debit side — their net directionality depends on the tamper omega below.
 *
 * MANDATROPHY ANALYSIS:
 *   Reading the seats separately keeps the arrangement from collapsing into either cover story. From the payer seat the operational gap looks like pure obstruction; from the beneficiary and agenda seats the same structure is the price of reliable tables. The structural truth is hybrid: genuine coordination carrying an enforced exclusion with identifiable payers — hence the tangled_rope claim rather than either pure type. On obsolescence: the exclusion half is aging toward vestige, since operative practice has fully migrated to the number reading, but the notational half remains load-bearing everywhere numerals are written, so the arrangement resolves as a live function wrapped around a dying clause rather than as performance alone. Because fixing was always cheap — one codifying treatise granted the missing rules — persistence after the fix existed is explained by enforcement and doctrinal identity, not repair cost; that is why the kernel omega, not inertial decay alone, tracks this arrangement's endpoint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_zero_status,
    'This story instantiates one reading of the kernel ''the mathematical status of zero'' — the placeholder reading, on which the empty-place sign belongs to notation and not to arithmetic. Would instantiating a sibling reading change the constraint''s structure, and where exactly is the disagreement located?',
    'Comparative reconstruction: run each sibling''s victim set and coordination surface against the same corpus — Babylonian tables, Hellenistic sexagesimal astronomy, Sanskrit computational practice, and the codifying treatise of the number reading. The reading whose operational boundary survives contact with the widest range of computational practice is the one whose constraint structure holds.',
    'Under the number reading the operational exclusion dissolves and this arrangement reduces toward a pure information standard with negligible extraction; under the Parmenidean rejection the notational permission itself is withdrawn and the affected set expands to every positional user. The location of the disagreement is the sign''s operand status — nothing else in the structure moves between readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_zero_status, conceptual, 'Which reading of zero''s status the structural data supports, and where the readings part ways').

omega_variable(
    closure_loss_intrinsic_vs_refusal,
    'Is the lost arithmetic closure an intrinsic price of treating the empty-place sign as a mark rather than a quantity, or a contingent refusal to extend rules that were always available?',
    'Counterfactual analysis of positional systems: determine whether any coherent rule-set grants the sign operand status while preserving the mark/quantity distinction this reading asserts. The codification of operand-status rules is the natural experiment — if it required abandoning the distinction entirely, the closure gap was constitutive; if the distinction could have survived partial extension, the gap was chosen.',
    'If intrinsic, part of the measured extraction is the honest price of the convention and effective extraction falls; if chosen, the exclusion is a suppressible act and the arrangement reads as more extractive than its coordination function requires.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(closure_loss_intrinsic_vs_refusal, conceptual, 'Whether the closure gap is constitutive of the mark/quantity distinction or merely refused extension').

omega_variable(
    tamper_exposure_mercantile_position,
    'Are mercantile ledger keepers net beneficiaries of the arrangement, or do the tamper vulnerabilities of empty-mark records — a single stroke converts an empty place into a digit — flip them toward bearing costs?',
    'Archival comparison of alteration-dispute rates between positional and non-positional ledger traditions, and of the documentary safeguards adopted where positional records entered legal use; the safeguard uptake pattern reveals which side of the ledger the risk landed on.',
    'If flipped, the mercantile seat''s directionality rises toward the target pole, aggregate extraction increases, and the same structural data supports a more extraction-heavy computed classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tamper_exposure_mercantile_position, empirical, 'Net structural position of mercantile users under tamper-risk accounting').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zero_mathematical_status__placeholder_reading, 100, 800).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zero_tr_t100, zero_mathematical_status__placeholder_reading, theater_ratio, 100, 0.1).
narrative_ontology:measurement(zero_tr_t200, zero_mathematical_status__placeholder_reading, theater_ratio, 200, 0.11).
narrative_ontology:measurement(zero_tr_t300, zero_mathematical_status__placeholder_reading, theater_ratio, 300, 0.12).
narrative_ontology:measurement(zero_tr_t400, zero_mathematical_status__placeholder_reading, theater_ratio, 400, 0.13).
narrative_ontology:measurement(zero_tr_t500, zero_mathematical_status__placeholder_reading, theater_ratio, 500, 0.14).
narrative_ontology:measurement(zero_tr_t600, zero_mathematical_status__placeholder_reading, theater_ratio, 600, 0.16).
narrative_ontology:measurement(zero_tr_t700, zero_mathematical_status__placeholder_reading, theater_ratio, 700, 0.17).
narrative_ontology:measurement(zero_tr_t800, zero_mathematical_status__placeholder_reading, theater_ratio, 800, 0.19).

% Extraction over time
narrative_ontology:measurement(zero_be_t100, zero_mathematical_status__placeholder_reading, base_extractiveness, 100, 0.28).
narrative_ontology:measurement(zero_be_t200, zero_mathematical_status__placeholder_reading, base_extractiveness, 200, 0.32).
narrative_ontology:measurement(zero_be_t300, zero_mathematical_status__placeholder_reading, base_extractiveness, 300, 0.35).
narrative_ontology:measurement(zero_be_t400, zero_mathematical_status__placeholder_reading, base_extractiveness, 400, 0.38).
narrative_ontology:measurement(zero_be_t500, zero_mathematical_status__placeholder_reading, base_extractiveness, 500, 0.42).
narrative_ontology:measurement(zero_be_t600, zero_mathematical_status__placeholder_reading, base_extractiveness, 600, 0.45).
narrative_ontology:measurement(zero_be_t700, zero_mathematical_status__placeholder_reading, base_extractiveness, 700, 0.48).
narrative_ontology:measurement(zero_be_t800, zero_mathematical_status__placeholder_reading, base_extractiveness, 800, 0.5).

% Suppression requirement over time
narrative_ontology:measurement(zero_su_t100, zero_mathematical_status__placeholder_reading, suppression_requirement, 100, 0.4).
narrative_ontology:measurement(zero_su_t200, zero_mathematical_status__placeholder_reading, suppression_requirement, 200, 0.42).
narrative_ontology:measurement(zero_su_t300, zero_mathematical_status__placeholder_reading, suppression_requirement, 300, 0.44).
narrative_ontology:measurement(zero_su_t400, zero_mathematical_status__placeholder_reading, suppression_requirement, 400, 0.46).
narrative_ontology:measurement(zero_su_t500, zero_mathematical_status__placeholder_reading, suppression_requirement, 500, 0.49).
narrative_ontology:measurement(zero_su_t600, zero_mathematical_status__placeholder_reading, suppression_requirement, 600, 0.52).
narrative_ontology:measurement(zero_su_t700, zero_mathematical_status__placeholder_reading, suppression_requirement, 700, 0.55).
narrative_ontology:measurement(zero_su_t800, zero_mathematical_status__placeholder_reading, suppression_requirement, 800, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zero_mathematical_status__placeholder_reading, information_standard).
narrative_ontology:affects_constraint(zero_mathematical_status__placeholder_reading, zero_mathematical_status__number_reading).
narrative_ontology:affects_constraint(zero_mathematical_status__placeholder_reading, zero_mathematical_status__parmenidean_rejection).

% DUAL FORMULATION NOTE:
% Constraint family: the natural-language label 'zero's mathematical status' decomposes into three structurally distinct stories per the epsilon-invariance principle. This file (placeholder reading) authors epsilon for the standing placeholder-only arrangement assessed by its own lights: notation permitted, operations withheld, intermediate extraction. The number-reading sibling authors epsilon for the arrangement in which operand status is granted — its extraction profile differs because its victim set is nearly empty (division-by-zero pathologies aside) and its coordination surface is the full arithmetic closure. The Parmenidean sibling authors epsilon for the arrangement in which the sign is refused entirely — its extraction profile is dominated by the suppression needed to keep positional users from writing what they need to write. The placeholder story sits upstream of the number reading: the success of placeholder practice created the computational demand that made the number reading necessary, which is recorded as an influences edge in this file's reading_relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
