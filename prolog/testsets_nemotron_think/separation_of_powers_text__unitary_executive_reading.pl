% ============================================================================
% CONSTRAINT STORY: separation_of_powers_text__unitary_executive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_separation_of_powers_text__unitary_executive_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: separation_of_powers_text__unitary_executive_reading
 *   human_readable: Unitary Executive Principle — All Executive Power Vests in the President
 *   domain: constitutional/administrative
 *
 * SUMMARY:
 *   The unitary executive reading of Article II asserts that 'the executive
 *   Power' vested in the President includes the power to remove all executive
 *   officers at will, rendering independent agencies (with for-cause removal
 *   protections) unconstitutional. This reading has migrated from a fringe
 *   academic theory (1980s OLC opinions) to controlling Supreme Court
 *   doctrine (Seila Law, Collins, potentially extending to Humphrey's
 *   Executor). The constraint story captures this reading as a structural
 *   claim: the Constitution itself is a mountain — a fixed, natural-law-like
 *   limit on congressional power to structure administration. But
 *   descriptively, the reading's operation extracts institutional
 *   independence from agencies, legislative architecture from Congress, and
 *   interpretive authority from courts, transferring all to the presidency.
 *   The claimed_type (mountain) diverges from the metric profile (high
 *   extraction, active enforcement, moderate accessibility collapse) — this
 *   divergence is the false summit signal.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(separation_of_powers_text__unitary_executive_reading, 0.72).
domain_priors:suppression_score(separation_of_powers_text__unitary_executive_reading, 0.68).
domain_priors:theater_ratio(separation_of_powers_text__unitary_executive_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(separation_of_powers_text__unitary_executive_reading, mountain).
narrative_ontology:human_readable(separation_of_powers_text__unitary_executive_reading, "Unitary Executive Principle — All Executive Power Vests in the President").
narrative_ontology:topic_domain(separation_of_powers_text__unitary_executive_reading, "constitutional/administrative").

domain_priors:requires_active_enforcement(separation_of_powers_text__unitary_executive_reading).
domain_priors:emerges_naturally(separation_of_powers_text__unitary_executive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(separation_of_powers_text__unitary_executive_reading, '64565e27-c5b1-4ab8-ba79-0a8fa3848de8').
narrative_ontology:cs_kernel_codification('64565e27-c5b1-4ab8-ba79-0a8fa3848de8', fixed_text).
narrative_ontology:cs_authority_grounding('64565e27-c5b1-4ab8-ba79-0a8fa3848de8', lineage).
narrative_ontology:cs_interpretation_layer_present('64565e27-c5b1-4ab8-ba79-0a8fa3848de8').
narrative_ontology:cs_reading_relation('64565e27-c5b1-4ab8-ba79-0a8fa3848de8', separation_of_powers_text__formalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('64565e27-c5b1-4ab8-ba79-0a8fa3848de8', separation_of_powers_text__functionalist_reading, forecloses).
narrative_ontology:cs_axiom('64565e27-c5b1-4ab8-ba79-0a8fa3848de8', foundational, all_executive_power_vests_in_president).
narrative_ontology:cs_axiom_status(all_executive_power_vests_in_president, holdable).
narrative_ontology:cs_axiom_grounding('64565e27-c5b1-4ab8-ba79-0a8fa3848de8', all_executive_power_vests_in_president, deontological).
narrative_ontology:cs_axiom('64565e27-c5b1-4ab8-ba79-0a8fa3848de8', foundational, independent_agencies_are_unconstitutional).
narrative_ontology:cs_axiom_status(independent_agencies_are_unconstitutional, holdable).
narrative_ontology:cs_axiom_grounding('64565e27-c5b1-4ab8-ba79-0a8fa3848de8', independent_agencies_are_unconstitutional, deontological).
narrative_ontology:cs_axiom('64565e27-c5b1-4ab8-ba79-0a8fa3848de8', secondary, presidential_removal_power_is_absolute).
narrative_ontology:cs_axiom_status(presidential_removal_power_is_absolute, holdable).
narrative_ontology:cs_axiom_grounding('64565e27-c5b1-4ab8-ba79-0a8fa3848de8', presidential_removal_power_is_absolute, deontological).
narrative_ontology:cs_reference_frame('64565e27-c5b1-4ab8-ba79-0a8fa3848de8', founding_era_unitary_executive).
narrative_ontology:cs_drift_state('64565e27-c5b1-4ab8-ba79-0a8fa3848de8', contemporary_administrative_state, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('64565e27-c5b1-4ab8-ba79-0a8fa3848de8', '2026-08-15T14:30:00Z').
narrative_ontology:cs_kernel_id(separation_of_powers_text__unitary_executive_reading, separation_of_powers_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(separation_of_powers_text__unitary_executive_reading, president).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__unitary_executive_reading, executive_branch).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__unitary_executive_reading, unitary_executive_theorists).
narrative_ontology:constraint_victim(separation_of_powers_text__unitary_executive_reading, independent_agencies).
narrative_ontology:constraint_victim(separation_of_powers_text__unitary_executive_reading, congress).
narrative_ontology:constraint_victim(separation_of_powers_text__unitary_executive_reading, judiciary).
narrative_ontology:constraint_victim(separation_of_powers_text__unitary_executive_reading, career_civil_service).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__unitary_executive_reading, regulated_entities).
narrative_ontology:constraint_victim(separation_of_powers_text__unitary_executive_reading, regulated_entities).
narrative_ontology:constraint_vindicates(separation_of_powers_text__unitary_executive_reading, constitutional_text_fixes_executive_structure).
narrative_ontology:constraint_vindicates(separation_of_powers_text__unitary_executive_reading, presidential_accountability_requires_unitary_control).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claims constitutional authority to direct all executive branch officers, remove agency heads at will, and control administrative policy. Uses unitary executive theory to justify centralized control over independent agencies. Gains direct political control over regulatory and enforcement decisions previously made by insulated agency heads.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, president, agenda_setter,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(separation_of_powers_text__unitary_executive_reading, president, beneficiary).

% Gains unified command structure with all administrative power flowing through the White House. Political appointees replace independent commissioners; policy coordination becomes direct rather than negotiated. Loses the buffering function that insulated technical expertise from political cycles.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, executive_branch, beneficiary,
    institutional, generational, arbitrage, national).

% Academic and legal advocates (Federalist Society, OLC alumni) who developed and promote the theory. Gain institutional influence, judicial appointments, and intellectual authority when the theory prevails. Their career capital is tied to the theory's ascendance.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, unitary_executive_theorists, beneficiary,
    organized, generational, mobile, national).

% FTC, NLRB, Fed, SEC, CFPB, and similar multimember commissions with statutory removal protections. Lose structural independence when presidential removal power becomes absolute. Commissioners become subordinate political appointees; expert decision-making yields to White House directives. No exit — the constraint targets their statutory design.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, independent_agencies, payer,
    organized, generational, trapped, national).

% Loses ability to create insulated expert bodies through statute. The legislative tool of designing agency independence (for-cause removal, multimember commissions, fixed terms) is declared unconstitutional. Can only respond by not creating agencies or by accepting presidential control — a structural loss of legislative architecture.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, congress, payer,
    institutional, generational, constrained, national).

% Cedes interpretive authority over administrative structure to the executive branch's reading of Article II. The courts' role shifts from reviewing agency independence to enforcing presidential removal power. Loses the functionalist balancing framework (Humphrey's Executor, Morrison v. Olson) that permitted nuanced structural analysis.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, judiciary, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(separation_of_powers_text__unitary_executive_reading, judiciary, agenda_setter).

% Merit-protected experts in agencies lose insulation from political direction. Policy judgments based on technical expertise are overridden by political appointees implementing presidential priorities. Exit is constrained by specialization and pension vesting; whistleblower protections become the only structural safeguard.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, career_civil_service, payer,
    moderate, biographical, constrained, national).

% Regulated industries gain a single political access point (the White House) instead of navigating independent commission processes. But lose predictability and expert adjudication — policy shifts with each administration. Net effect varies by industry and administration alignment.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, regulated_entities, beneficiary,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(separation_of_powers_text__unitary_executive_reading, regulated_entities, payer).

% Consumer, environmental, labor, and civil rights groups that relied on independent agency expertise and insulation from political pressure. Lose a venue where technical records and statutory mandates could prevail over presidential politics. No structural seat in the unitary executive framework.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, public_interest_advocates, excluded,
    organized, biographical, trapped, national).

% Academic observers analyzing the theory's textual basis, historical practice, and institutional consequences. Split between originalist proponents, functionalist critics, and formalist alternatives. Their analysis shapes the intellectual environment but does not directly control outcomes.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of unified executive command: ensures democratic accountability by concentrating all executive authority in an elected President who can direct policy implementation, resolve interagency conflicts, and answer to voters for administrative outcomes.
% TRANSFER_FUNCTION: Moves decision-making authority over regulatory enforcement, rulemaking, and adjudication from independent multimember commissions (statutorily insulated from presidential removal) to the President and political appointees. Transfers structural independence — the agencies' statutory design — to presidential control.
% ABSENT_VOICES: The regulated public who benefit from expert, politically insulated adjudication (e.g., NLRB unfair labor practice hearings, FTC consumer protection, Fed monetary independence). Future Congresses that lose the legislative tool of agency design. Career civil servants whose professional judgment is structurally overridden. These voices are not in the room when the theory is litigated — the case is framed as President vs. Agency, not President vs. The Public's Interest in Expert Administration.
% DISAPPEARANCE_RATIONALE: If the unitary executive reading vanished overnight, independent agencies would retain their statutory removal protections. Congress could continue designing insulated expert bodies. The administrative state's structure would revert to the post-Humphrey's Executor equilibrium — multimember commissions, for-cause removal, functionalist balancing. The presidential appointment power would remain but not the removal power over independent officers. The administrative state's architecture would fundamentally reorganize.
% FOUNDING_PROBLEM: The founding generation debated whether the executive power should be unitary or plural. The 1787 Constitution settled on a single President (Article II, Section 1) but was silent on whether Congress could create executive officers removable only for cause. The founding problem was: how to ensure presidential accountability and energetic execution while permitting Congress to structure administration for expertise and insulation from politics.
% FOUNDING_PROBLEM_CORROBORATION: Originalist scholars (Calabresi, Prakash, Kagan) attest the founding problem is live — the text demands unity. Functionalists (Strauss, Sunstein, Lessig) and historical institutionalists (Novak, Mashaw) attest the problem is substantially solved by 200+ years of practice — Congress has created independent agencies since 1789 (e.g., the Comptroller, early territorial governors). The Supreme Court itself has swung: Myers (1926) for unity, Humphrey's Executor (1935) for independence, Morrison (1988) for balance, Seila Law (2020) and Collins (2021) back toward unity. No external consensus exists — the contest is the point.
narrative_ontology:disappearance_verdict(separation_of_powers_text__unitary_executive_reading, world_rearranges).
narrative_ontology:founding_problem_status(separation_of_powers_text__unitary_executive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(separation_of_powers_text__unitary_executive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(separation_of_powers_text__unitary_executive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(separation_of_powers_text__unitary_executive_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(separation_of_powers_text__unitary_executive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(separation_of_powers_text__unitary_executive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, ExtMetricName, E),
    domain_priors:suppression_score(separation_of_powers_text__unitary_executive_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(separation_of_powers_text__unitary_executive_reading),
    narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(separation_of_powers_text__unitary_executive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the reading removes the statutory independence of dozens of agencies, transferring their decision authority to the President. Suppression (0.68) is high because maintaining the reading requires active judicial enforcement — overruling Humphrey's Executor, limiting Morrison, policing congressional statute design. Theater_ratio (0.42) is moderate: the theory has genuine textual arguments (Article II text, Take Care Clause) but a growing share of its operation serves partisan control rather than coordinate accountability. Accessibility_collapse (0.35) is moderate — alternatives (functionalist balancing, formalist non-delegation) persist in dissent and scholarship. Resistance (0.58) is significant — the administrative state, Congress, and the bar mount sustained opposition.
 *
 * PERSPECTIVAL GAP:
 *   From the President's seat (agenda_setter/beneficiary), the constraint is genuine coordination — solving the problem of divided executive authority. From the independent agencies' seat (payer/trapped), it is pure extraction — their statutory independence is taken without compensation. From Congress's seat (payer/constrained), it is a structural disarmament — the legislative tool of agency design is confiscated. From the judiciary's seat (payer/agenda_setter), it is a forced cession of interpretive authority. The engine computes these divergences from the structural data; the authored claim (mountain) does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   The President and executive branch are structural beneficiaries (d near 0.0) — they collect the transferred authority. Independent agencies, Congress, and the judiciary are structural payers (d near 1.0) — they lose statutory design, legislative tools, and interpretive framework. Career civil service is a payer with constrained exit (d ~0.7). Regulated entities are dual-positioned: benefit from single access point but pay in lost predictability (d ~0.5). Public interest advocates are excluded (no seat). Constitutional scholars are analytical observers (d = 0.5 by definition).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (accountable energetic execution) is contested — originalists say live, functionalists say solved by 200 years of practice. The reading persists because it serves current executive branch interests (mandatrophy: the mandate of textual unity has outlived its founding justification and now extracts power). The classification prevents mislabeling: a formalist would call this coordination (rope); a functionalist would call it extraction (snare). The structural data shows both — coordination of command AND asymmetric extraction from agencies/Congress — making it a false summit mountain (claimed natural law, computed extractive).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    unitary_executive_as_false_summit,
    'Is the unitary executive principle a genuine constitutional mountain (fixed by text) or a constructed constraint that benefits the executive branch?',
    'Historical analysis of founding-era practice: did the First Congress understand itself as barred from creating for-cause removal protections? The 1789 Treasury debate and subsequent practice (Comptroller, territorial governors) are the key evidence.',
    'If mountain: ε ≈ 0, classification holds. If constructed: ε ≈ 0.7+, reclassifies to tangled_rope or snare via false_summit_mountain signature. The beneficiary structure (executive branch gains) is the trigger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unitary_executive_as_false_summit, empirical, 'Natural-law vs. constructed status of the unitary executive principle — the core FSM ambiguity.').

omega_variable(
    kernel_reading_structural_delta,
    'How does this reading''s victim/beneficiary structure differ from its siblings?',
    'Compare the three readings'' stakeholder mappings: formalist_reading makes Congress the primary victim (loses delegation power), judiciary the beneficiary (gains non-delegation enforcement); functionalist_reading makes no structural victims (all branches retain flexibility); unitary_executive_reading makes independent agencies and Congress victims, executive branch beneficiary.',
    'Confirms the ε-invariance principle: same kernel label, different constraints, different ε. Each reading must be a separate story.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Structural differentiation of the three separation-of-powers readings — why they are three constraints, not one.').

omega_variable(
    foreclosure_vs_coexistence,
    'Does the unitary executive reading logically foreclose the functionalist reading within a single constitutional framework, or do they coexist as competing positions?',
    'Test: can a single judge or official consistently apply both? A judge applying unitary executive theory (no independent agencies) cannot simultaneously apply functionalist balancing (agencies permitted if intelligible principle). The premises contradict. But different judges in the same system can hold different readings — coexistence across seats.',
    'Determines reading_relations: forecloses (within-seat logical contradiction) vs coexists_with (across-seat competition). Affects cs_axiom_contradiction computation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foreclosure_vs_coexistence, conceptual, 'Logical relationship between unitary executive and functionalist readings — foreclosure within framework vs coexistence across actors.').

omega_variable(
    drift_acknowledgment_gap,
    'Does the administrative state''s practice acknowledge the drift from the unitary executive reference frame, or does it operate in denial?',
    'Examine agency statutes, judicial opinions, and OLC memos: do they explicitly argue ''Humphrey''s Executor is wrong but binding'' (acknowledged drift) or ''Humphrey''s Executor correctly implements Article II'' (denial)? Current doctrine mixes both.',
    'Acknowledged drift (acknowledged=true) enables revival_pressure dynamics; denied drift (acknowledged=false) enables authority_erosion dynamics. Affects terminal attractor computation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(drift_acknowledgment_gap, empirical, 'Whether the authority structure recognizes the gap between founding reference frame and contemporary practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(separation_of_powers_text__unitary_executive_reading, 1935, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unitary_exec_tr_t1935, separation_of_powers_text__unitary_executive_reading, theater_ratio, 1935, 0.1).
narrative_ontology:measurement(unitary_exec_tr_t1950, separation_of_powers_text__unitary_executive_reading, theater_ratio, 1950, 0.12).
narrative_ontology:measurement(unitary_exec_tr_t1970, separation_of_powers_text__unitary_executive_reading, theater_ratio, 1970, 0.18).
narrative_ontology:measurement(unitary_exec_tr_t1988, separation_of_powers_text__unitary_executive_reading, theater_ratio, 1988, 0.25).
narrative_ontology:measurement(unitary_exec_tr_t2000, separation_of_powers_text__unitary_executive_reading, theater_ratio, 2000, 0.3).
narrative_ontology:measurement(unitary_exec_tr_t2010, separation_of_powers_text__unitary_executive_reading, theater_ratio, 2010, 0.35).
narrative_ontology:measurement(unitary_exec_tr_t2020, separation_of_powers_text__unitary_executive_reading, theater_ratio, 2020, 0.38).
narrative_ontology:measurement(unitary_exec_tr_t2024, separation_of_powers_text__unitary_executive_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(unitary_exec_be_t1935, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 1935, 0.15).
narrative_ontology:measurement(unitary_exec_be_t1950, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 1950, 0.18).
narrative_ontology:measurement(unitary_exec_be_t1970, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 1970, 0.25).
narrative_ontology:measurement(unitary_exec_be_t1988, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 1988, 0.35).
narrative_ontology:measurement(unitary_exec_be_t2000, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 2000, 0.45).
narrative_ontology:measurement(unitary_exec_be_t2010, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 2010, 0.52).
narrative_ontology:measurement(unitary_exec_be_t2020, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 2020, 0.68).
narrative_ontology:measurement(unitary_exec_be_t2024, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 2024, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(unitary_exec_su_t1935, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 1935, 0.2).
narrative_ontology:measurement(unitary_exec_su_t1950, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 1950, 0.22).
narrative_ontology:measurement(unitary_exec_su_t1970, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 1970, 0.3).
narrative_ontology:measurement(unitary_exec_su_t1988, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 1988, 0.4).
narrative_ontology:measurement(unitary_exec_su_t2000, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 2000, 0.48).
narrative_ontology:measurement(unitary_exec_su_t2010, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 2010, 0.55).
narrative_ontology:measurement(unitary_exec_su_t2020, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 2020, 0.62).
narrative_ontology:measurement(unitary_exec_su_t2024, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 2024, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(separation_of_powers_text__unitary_executive_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(separation_of_powers_text__unitary_executive_reading, 0.12).
narrative_ontology:affects_constraint(separation_of_powers_text__unitary_executive_reading, separation_of_powers_text__formalist_reading).
narrative_ontology:affects_constraint(separation_of_powers_text__unitary_executive_reading, separation_of_powers_text__functionalist_reading).
narrative_ontology:affects_constraint(separation_of_powers_text__unitary_executive_reading, administrative_state_architecture).
narrative_ontology:affects_constraint(separation_of_powers_text__unitary_executive_reading, presidential_removal_power).
narrative_ontology:affects_constraint(separation_of_powers_text__unitary_executive_reading, congressional_delegation_authority).

% DUAL FORMULATION NOTE:
% This is one of three constraint stories decomposing the 'separation of powers' kernel. The unitary_executive_reading extracts from independent agencies and Congress to benefit the President. The formalist_reading extracts from Congress to benefit the judiciary (via non-delegation enforcement). The functionalist_reading has negligible extraction — it permits all three branches to negotiate structure. Their ε values differ by 0.5+. They are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(separation_of_powers_text__unitary_executive_reading, institutional, 0.15).
constraint_indexing:directionality_override(separation_of_powers_text__unitary_executive_reading, organized, 0.85).
constraint_indexing:directionality_override(separation_of_powers_text__unitary_executive_reading, moderate, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
