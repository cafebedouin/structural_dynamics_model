% ============================================================================
% CONSTRAINT STORY: statutory_debt_ceiling__constitutional_nullity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_statutory_debt_ceiling__constitutional_nullity_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: statutory_debt_ceiling__constitutional_nullity_reading
 *   human_readable: Debt Ceiling as Constitutionally Void (14th Amendment §4 Reading)
 *   domain: constitutional_law/political_economy/fiscal_governance
 *
 * SUMMARY:
 *   The statutory debt ceiling (originating in the Second Liberty Bond Act of
 *   1917, consolidated in 1939) is read here as constitutionally void under
 *   the Fourteenth Amendment Section 4: 'The validity of the public debt of
 *   the United States, authorized by law... shall not be questioned.' Under
 *   this reading, once Congress has enacted appropriations that require
 *   borrowing, the debt ceiling cannot constitutionally block the Treasury
 *   from executing that borrowing. The ceiling is a legal nullity — it has
 *   zero extractiveness because it cannot legally constrain the Treasury's
 *   obligation to pay authorized debts. Congressional votes to raise or
 *   suspend the ceiling are ceremonial performances; the Treasury's borrowing
 *   authority flows from the appropriations power and the Section 4 mandate,
 *   not from the ceiling statute. This reading was notably advanced by legal
 *   scholars (Buchanan, Dorf, Tribe) and considered by the Obama and Biden
 *   administrations but never executed. The constraint's operational reality
 *   under this reading: ε = 0 at all times, suppression ≈ 0, theater_ratio
 *   rising as the gap between legal reality and political performance widens.
 *
 * KEY AGENTS:
 *   - treasury_secretary: Institutional agenda_setter (could execute nullity reading) — holds the borrowing authority
 *   - congressional_leadership: Institutional payer/beneficiary (ceremonial votes) — performs the constraint
 *   - supreme_court: Institutional observer (ultimate adjudicator) — has never ruled on Section 4 debt ceiling question
 *   - bond_market_participants: Organized beneficiaries (receive uninterrupted payment) — priced for nullity
 *   - federal_beneficiaries: Powerless beneficiaries (Social Security, veterans, contractors) — protected by Section 4
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statutory_debt_ceiling__constitutional_nullity_reading, 0.0).
domain_priors:suppression_score(statutory_debt_ceiling__constitutional_nullity_reading, 0.05).
domain_priors:theater_ratio(statutory_debt_ceiling__constitutional_nullity_reading, 0.95).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, extractiveness, 0.0).
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 0.95).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, accessibility_collapse, 0.05).
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, resistance, 0.95).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statutory_debt_ceiling__constitutional_nullity_reading, mountain).
narrative_ontology:human_readable(statutory_debt_ceiling__constitutional_nullity_reading, "Debt Ceiling as Constitutionally Void (14th Amendment §4 Reading)").
narrative_ontology:topic_domain(statutory_debt_ceiling__constitutional_nullity_reading, "constitutional_law/political_economy/fiscal_governance").

domain_priors:emerges_naturally(statutory_debt_ceiling__constitutional_nullity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statutory_debt_ceiling__constitutional_nullity_reading, '0adf9177-4aeb-46ca-977e-b79ac9d7fac3').
narrative_ontology:cs_kernel_codification('0adf9177-4aeb-46ca-977e-b79ac9d7fac3', fixed_text).
narrative_ontology:cs_authority_grounding('0adf9177-4aeb-46ca-977e-b79ac9d7fac3', lineage).
narrative_ontology:cs_interpretation_layer_present('0adf9177-4aeb-46ca-977e-b79ac9d7fac3').
narrative_ontology:cs_reading_relation('0adf9177-4aeb-46ca-977e-b79ac9d7fac3', statutory_debt_ceiling__coordination_scaffold_reading, forecloses).
narrative_ontology:cs_reading_relation('0adf9177-4aeb-46ca-977e-b79ac9d7fac3', statutory_debt_ceiling__extraction_snare_reading, forecloses).
narrative_ontology:cs_axiom('0adf9177-4aeb-46ca-977e-b79ac9d7fac3', foundational, section_4_mandates_uninterrupted_debt_payment).
narrative_ontology:cs_axiom_status(section_4_mandates_uninterrupted_debt_payment, holdable).
narrative_ontology:cs_axiom_grounding('0adf9177-4aeb-46ca-977e-b79ac9d7fac3', section_4_mandates_uninterrupted_debt_payment, deontological).
narrative_ontology:cs_axiom('0adf9177-4aeb-46ca-977e-b79ac9d7fac3', foundational, appropriations_power_implies_borrowing_authority).
narrative_ontology:cs_axiom_status(appropriations_power_implies_borrowing_authority, holdable).
narrative_ontology:cs_axiom_grounding('0adf9177-4aeb-46ca-977e-b79ac9d7fac3', appropriations_power_implies_borrowing_authority, conventional).
narrative_ontology:cs_reference_frame('0adf9177-4aeb-46ca-977e-b79ac9d7fac3', fourteenth_amendment_section_4_command).
narrative_ontology:cs_drift_state('0adf9177-4aeb-46ca-977e-b79ac9d7fac3', contemporary_standoff_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('0adf9177-4aeb-46ca-977e-b79ac9d7fac3', '').
narrative_ontology:cs_kernel_id(statutory_debt_ceiling__constitutional_nullity_reading, statutory_debt_ceiling).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__constitutional_nullity_reading, congressional_leadership).
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__constitutional_nullity_reading, bond_market_participants).
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__constitutional_nullity_reading, federal_beneficiaries).
narrative_ontology:constraint_victim(statutory_debt_ceiling__constitutional_nullity_reading, congressional_leadership).
narrative_ontology:constraint_vindicates(statutory_debt_ceiling__constitutional_nullity_reading, constitutional_supremacy_doctrine).
narrative_ontology:constraint_vindicates(statutory_debt_ceiling__constitutional_nullity_reading, fourteenth_amendment_section_4).
narrative_ontology:constraint_vindicates(statutory_debt_ceiling__constitutional_nullity_reading, public_debt_validity_clause).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the constitutional authority and operational capacity to execute borrowing above the statutory ceiling to satisfy appropriations. Could declare the ceiling void under Section 4 and continue issuing debt. Faces political and institutional pressure to maintain the ceremonial process. Has never exercised the nullity option.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, treasury_secretary, agenda_setter,
    institutional, biographical, analytical, national).

% Performs the ceremonial votes to raise/suspend the ceiling. As payer: invests political capital in votes that are legally unnecessary. As beneficiary: extracts concessions (spending cuts, policy riders) from the performance. Constrained by the institutional norm of maintaining the ceiling ritual; exit would mean acknowledging the constraint's voidness and losing leverage.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, congressional_leadership, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(statutory_debt_ceiling__constitutional_nullity_reading, congressional_leadership, beneficiary).

% Ultimate adjudicator of the Section 4 question. Has never ruled on whether the debt ceiling is void under the Fourteenth Amendment. Perry v. United States (1935) discussed Section 4 but not the ceiling. A ruling would collapse or confirm the nullity reading structurally.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, supreme_court, observer,
    institutional, generational, analytical, national).

% Price US Treasuries as risk-free despite periodic ceiling standoffs. Market behavior treats the ceiling as non-binding (yields do not spike at ceiling dates the way they would for a genuine default risk). Beneficiaries of the nullity reading's operational reality: uninterrupted payment is the market's priced expectation.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, bond_market_participants, beneficiary,
    organized, immediate, mobile, global).

% Social Security recipients, veterans, federal contractors, Medicare providers — all depend on timely federal payments. Section 4 protects their claims constitutionally. They have no exit from the federal payment system and no leverage over the ceiling ritual. The nullity reading secures their claims as a matter of constitutional law, not legislative grace.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, federal_beneficiaries, beneficiary,
    powerless, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(statutory_debt_ceiling__constitutional_nullity_reading, diffuse).
narrative_ontology:fixing_cost_class(statutory_debt_ceiling__constitutional_nullity_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Under this reading, the debt ceiling performs NO coordination function — it is a legal nullity. The real coordination is performed by the appropriations process (Congress authorizes spending) and the Treasury's constitutional duty to execute that spending (Section 4). The ceiling statute is vestigial theater.
% TRANSFER_FUNCTION: Zero transfer. The constraint extracts nothing because it has no legal force. No money, work, attention, or status moves through the ceiling mechanism. The ceremonial votes transfer political theater, not resources.
% ABSENT_VOICES: The nullity reading itself was absent from official executive action during the 2011, 2013, 2021, and 2023 standoffs. Treasury secretaries and White House counsels considered the argument but did not execute it. The 'absent voice' is the constitutional command itself — Section 4's mandate has never been operationally invoked to override the ceiling.
% DISAPPEARANCE_RATIONALE: If the debt ceiling statute were repealed tomorrow, Treasury borrowing would continue unchanged — it is already constitutionally required to borrow what appropriations demand. The world would not rearrange because the constraint is already void. Congressional votes would cease (saving theater), but fiscal operations would be identical.
% FOUNDING_PROBLEM: The debt ceiling originated in the Second Liberty Bond Act (1917) as a WWI-era coordination mechanism: giving Treasury flexibility to manage Liberty Bond issuance within an aggregate limit without returning to Congress for each tranche. It was a wartime administrative convenience, not a constitutional limit on debt validity.
% FOUNDING_PROBLEM_CORROBORATION: Historical record: the 1917 Act's legislative history shows the ceiling was designed for wartime bond management efficiency. The 1939 consolidation made it permanent. No founding-era actor conceived it as a constitutional limit on authorized debt payment. Buchanan & Dorf (2012), Tribe (2011), and the Congressional Research Service (multiple reports) corroborate that the founding problem (WWI bond coordination) is dead and the constraint persists as mandatrophy.
narrative_ontology:disappearance_verdict(statutory_debt_ceiling__constitutional_nullity_reading, world_unchanged).
narrative_ontology:founding_problem_status(statutory_debt_ceiling__constitutional_nullity_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statutory_debt_ceiling__constitutional_nullity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(statutory_debt_ceiling__constitutional_nullity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(statutory_debt_ceiling__constitutional_nullity_reading, 0.0, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(statutory_debt_ceiling__constitutional_nullity_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, ExtMetricName, E),
    domain_priors:suppression_score(statutory_debt_ceiling__constitutional_nullity_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(statutory_debt_ceiling__constitutional_nullity_reading),
    narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(statutory_debt_ceiling__constitutional_nullity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.0 because the constitutional nullity reading asserts the constraint has no legal force — it cannot extract because it cannot bind. Suppression is 0.05 (near-zero) because no enforcement mechanism exists for a void constraint; the only 'suppression' is the political cost of acknowledging nullity. Theater ratio rises from 0.15 (1917, when the ceiling had genuine coordination function as a Liberty Bond management tool) to 0.95 (2025, when the ceiling is widely understood by legal scholars as void but political performance continues). Accessibility collapse is 0.05 because alternatives (Treasury borrowing above ceiling) are legally available and constitutionally mandated — alternatives do not collapse. Resistance is 0.95 because the constitutional text, structure, and history actively resist the ceiling's binding interpretation. Claimed type is mountain because a constitutional command is a structural feature of the legal order, not a human choice — but this mountain has high theater (unlike physical mountains) because the political system performs compliance with a void constraint.
 *
 * PERSPECTIVAL GAP:
 *   Under this reading, all seats compute as mountain (ε ≈ 0, no extraction, no suppression). The perspectival gap is between this reading's computed type (mountain) and the other readings' computed types (coordination_scaffold or extraction_snare). The engine would compute different classifications per seat depending on which reading's structural data is input. This is not a single constraint with multiple perspectives — it is three distinct constraints (three readings of one kernel) with three distinct ε values.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiaries or victims declared because ε = 0 — a constitutionally void constraint extracts from no one and benefits no one through its operation. The Treasury is not a 'beneficiary' of nullity; it simply executes its constitutional duty. Congressional leadership is not a 'payer' of nullity; they perform a ceremony. Bond markets and federal beneficiaries are protected by the constitutional command, not by the ceiling's absence. Directionality is moot where extraction is zero.
 *
 * MANDATROPHY ANALYSIS:
 *   The debt ceiling's original mandate (Liberty Bond management in WWI) is dead — the founding problem was wartime finance coordination, not a permanent borrowing limit. The constraint persists as theater (mandatrophy unresolved). This reading resolves mandatrophy by declaring the constraint constitutionally void — the mandate didn't just atrophy, it was never constitutionally valid as a limit on authorized debt payment. The theater_ratio trajectory (0.15 → 0.95) tracks the mandatrophy: as the original coordination function vanished, performance replaced function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutional_nullity_vs_constructed_coordination,
    'Is the debt ceiling a constitutionally void constraint (zero extractiveness, zero suppression) or a constructed coordination mechanism that persists despite its constitutional infirmity?',
    'Supreme Court adjudication of a Section 4 challenge, or Treasury execution of borrowing above the ceiling without congressional reauthorization coupled with judicial non-intervention. If Treasury borrows above the ceiling to satisfy appropriations and no court enjoins it, the nullity reading gains structural confirmation. If Congress continues to raise the ceiling and all parties treat the process as binding, the coordination reading persists.',
    'If nullity reading is structurally true, the constraint has ε ≈ 0 and is a mountain from every seat — congressional votes are ceremonial, extraction is zero, suppression is near-zero. If coordination or extraction reading is true, ε > 0, suppression > 0, and the constraint is tangent/rope/snare with real stakes. The classification diverges entirely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_nullity_vs_constructed_coordination, conceptual, 'Whether the debt ceiling is a genuine constitutional nullity or a constructed constraint whose constitutional infirmity is itself part of its operational design').

omega_variable(
    kernel_reading_foreclosure_structure,
    'Do the three declared readings of the statutory_debt_ceiling kernel foreclose each other, coexist as live positions, or influence each other structurally?',
    'Track which institutional actors adopt which reading and whether any actor holds multiple readings simultaneously. If executive branch officials simultaneously treat the ceiling as binding (coordination) while preparing 14th Amendment arguments (nullity), the readings coexist in practice. If adoption of one reading logically requires rejection of the others within a single institutional framework, foreclosure applies.',
    'Foreclosure relations determine whether the kernel has a determinate structural type or whether the contest itself is the structure. Coexistence means all three constraint types are simultaneously instantiated by different institutional seats — the engine would compute different classifications per seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_structure, conceptual, 'Structural relationship among the three declared readings of the statutory debt ceiling kernel').

omega_variable(
    theater_mechanism_legislative_votes,
    'Are congressional debt ceiling votes purely theatrical (performative maintenance of a void constraint) or do they retain residual coordination/extraction function?',
    'Compare legislative behavior when the ceiling binds vs. when it does not. If votes occur on schedule with whip operations, concessions extracted, and political theater regardless of fiscal necessity, the theater ratio is high. If votes only occur when mathematically necessary and no side payments are extracted, theater is lower.',
    'Theater ratio of 0.95 assumes votes are almost entirely performative. If evidence shows residual extraction or coordination in the voting process, theater ratio should be lower and extractiveness/suppression higher.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_mechanism_legislative_votes, empirical, 'Whether congressional debt ceiling votes are ceremonial performance or retain functional leverage').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statutory_debt_ceiling__constitutional_nullity_reading, 1917, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(statutory_debt_ceiling__constitutional_nullity_reading_tr_t1917, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 1917, 0.15).
narrative_ontology:measurement(statutory_debt_ceiling__constitutional_nullity_reading_tr_t1939, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 1939, 0.25).
narrative_ontology:measurement(statutory_debt_ceiling__constitutional_nullity_reading_tr_t1979, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 1979, 0.55).
narrative_ontology:measurement(statutory_debt_ceiling__constitutional_nullity_reading_tr_t1995, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 1995, 0.8).
narrative_ontology:measurement(statutory_debt_ceiling__constitutional_nullity_reading_tr_t2011, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 2011, 0.9).
narrative_ontology:measurement(statutory_debt_ceiling__constitutional_nullity_reading_tr_t2013, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 2013, 0.9).
narrative_ontology:measurement(statutory_debt_ceiling__constitutional_nullity_reading_tr_t2021, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 2021, 0.92).
narrative_ontology:measurement(statutory_debt_ceiling__constitutional_nullity_reading_tr_t2023, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 2023, 0.94).
narrative_ontology:measurement(statutory_debt_ceiling__constitutional_nullity_reading_tr_t2025, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 2025, 0.95).

% Extraction over time
narrative_ontology:measurement(statutory_debt_ceiling__constitutional_nullity_reading_be_t1917, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 1917, 0.0).
narrative_ontology:measurement(statutory_debt_ceiling__constitutional_nullity_reading_be_t1939, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 1939, 0.0).
narrative_ontology:measurement(statutory_debt_ceiling__constitutional_nullity_reading_be_t1979, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 1979, 0.0).
narrative_ontology:measurement(statutory_debt_ceiling__constitutional_nullity_reading_be_t1995, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 1995, 0.0).
narrative_ontology:measurement(statutory_debt_ceiling__constitutional_nullity_reading_be_t2011, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 2011, 0.0).
narrative_ontology:measurement(statutory_debt_ceiling__constitutional_nullity_reading_be_t2013, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 2013, 0.0).
narrative_ontology:measurement(statutory_debt_ceiling__constitutional_nullity_reading_be_t2021, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 2021, 0.0).
narrative_ontology:measurement(statutory_debt_ceiling__constitutional_nullity_reading_be_t2023, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 2023, 0.0).
narrative_ontology:measurement(statutory_debt_ceiling__constitutional_nullity_reading_be_t2025, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 2025, 0.0).

% Suppression requirement over time
narrative_ontology:measurement(statutory_debt_ceiling__constitutional_nullity_reading_su_t1917, statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 1917, 0.1).
narrative_ontology:measurement(statutory_debt_ceiling__constitutional_nullity_reading_su_t1939, statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 1939, 0.1).
narrative_ontology:measurement(statutory_debt_ceiling__constitutional_nullity_reading_su_t1979, statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 1979, 0.08).
narrative_ontology:measurement(statutory_debt_ceiling__constitutional_nullity_reading_su_t1995, statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 1995, 0.05).
narrative_ontology:measurement(statutory_debt_ceiling__constitutional_nullity_reading_su_t2011, statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 2011, 0.05).
narrative_ontology:measurement(statutory_debt_ceiling__constitutional_nullity_reading_su_t2013, statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 2013, 0.05).
narrative_ontology:measurement(statutory_debt_ceiling__constitutional_nullity_reading_su_t2021, statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 2021, 0.05).
narrative_ontology:measurement(statutory_debt_ceiling__constitutional_nullity_reading_su_t2023, statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 2023, 0.05).
narrative_ontology:measurement(statutory_debt_ceiling__constitutional_nullity_reading_su_t2025, statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 2025, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statutory_debt_ceiling__constitutional_nullity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(statutory_debt_ceiling__constitutional_nullity_reading, 0.0).
narrative_ontology:affects_constraint(statutory_debt_ceiling__constitutional_nullity_reading, statutory_debt_ceiling__coordination_scaffold_reading).
narrative_ontology:affects_constraint(statutory_debt_ceiling__constitutional_nullity_reading, statutory_debt_ceiling__extraction_snare_reading).

% DUAL FORMULATION NOTE:
% This reading and its two siblings form the statutory_debt_ceiling constraint family. The kernel is the statutory debt ceiling mechanism; the three readings decompose the single natural-language concept into three structurally distinct constraints per the ε-invariance principle. constitutional_nullity_reading has ε=0 (mountain); coordination_scaffold_reading has low ε with coordination function and sunset (scaffold); extraction_snare_reading has high ε with extraction and suppression (snare). The ε values differ by a wide margin because they refer to different structural claims about the same statutory mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
