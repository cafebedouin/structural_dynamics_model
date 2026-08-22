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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: separation_of_powers_text__unitary_executive_reading
 *   human_readable: Unitary Executive Reading of Separation of Powers — Presidential Removal Power Over Independent Agencies
 *   domain: constitutional/administrative
 *
 * SUMMARY:
 *   The unitary executive reading asserts that Article II's Vesting Clause
 *   ('The executive Power shall be vested in a President') establishes an
 *   exclusive, undivided presidential authority over all executive branch
 *   action. Independent agencies — multi-member commissions with for-cause
 *   removal protections (FTC, NLRB, Fed, CFPB, SEC) — are structurally
 *   incompatible with this principle and therefore unconstitutional. This
 *   reading has migrated from a minority academic position (1980s) to the
 *   controlling framework of a Supreme Court majority (Seila Law 2020,
 *   Collins v. Yellen 2021, pending challenges to FTC/NLRB structure). The
 *   constraint is the legal-institutional arrangement that would result from
 *   full doctrinal adoption: plenary presidential removal power, elimination
 *   of independent agency autonomy, centralization of regulatory authority in
 *   the White House. This story instantiates ONLY the unitary executive
 *   reading; the formalist and functionalist readings are separate
 *   constraints (separation_of_powers_text__formalist_reading,
 *   separation_of_powers_text__functionalist_reading) linked via
 *   network.affects_constraints.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(separation_of_powers_text__unitary_executive_reading, 0.68).
domain_priors:suppression_score(separation_of_powers_text__unitary_executive_reading, 0.72).
domain_priors:theater_ratio(separation_of_powers_text__unitary_executive_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(separation_of_powers_text__unitary_executive_reading, tangled_rope).
narrative_ontology:human_readable(separation_of_powers_text__unitary_executive_reading, "Unitary Executive Reading of Separation of Powers — Presidential Removal Power Over Independent Agencies").
narrative_ontology:topic_domain(separation_of_powers_text__unitary_executive_reading, "constitutional/administrative").

domain_priors:requires_active_enforcement(separation_of_powers_text__unitary_executive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(separation_of_powers_text__unitary_executive_reading, '151ef3b5-10c2-4696-8f8a-55a5bcc75995').
narrative_ontology:cs_kernel_codification('151ef3b5-10c2-4696-8f8a-55a5bcc75995', fixed_text).
narrative_ontology:cs_authority_grounding('151ef3b5-10c2-4696-8f8a-55a5bcc75995', lineage).
narrative_ontology:cs_interpretation_layer_present('151ef3b5-10c2-4696-8f8a-55a5bcc75995').
narrative_ontology:cs_reading_relation('151ef3b5-10c2-4696-8f8a-55a5bcc75995', separation_of_powers_text__formalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('151ef3b5-10c2-4696-8f8a-55a5bcc75995', separation_of_powers_text__functionalist_reading, coexists_with).
narrative_ontology:cs_axiom('151ef3b5-10c2-4696-8f8a-55a5bcc75995', foundational, unitary_executive_originalist).
narrative_ontology:cs_axiom_status(unitary_executive_originalist, holdable).
narrative_ontology:cs_axiom_grounding('151ef3b5-10c2-4696-8f8a-55a5bcc75995', unitary_executive_originalist, empirically_contingent).
narrative_ontology:cs_axiom('151ef3b5-10c2-4696-8f8a-55a5bcc75995', foundational, presidential_removal_power_absolute).
narrative_ontology:cs_axiom_status(presidential_removal_power_absolute, holdable).
narrative_ontology:cs_axiom_grounding('151ef3b5-10c2-4696-8f8a-55a5bcc75995', presidential_removal_power_absolute, deontological).
narrative_ontology:cs_axiom('151ef3b5-10c2-4696-8f8a-55a5bcc75995', secondary, independent_agencies_unconstitutional).
narrative_ontology:cs_axiom_status(independent_agencies_unconstitutional, holdable).
narrative_ontology:cs_axiom_grounding('151ef3b5-10c2-4696-8f8a-55a5bcc75995', independent_agencies_unconstitutional, empirically_contingent).
narrative_ontology:cs_reference_frame('151ef3b5-10c2-4696-8f8a-55a5bcc75995', founding_era_unitary_executive).
narrative_ontology:cs_drift_state('151ef3b5-10c2-4696-8f8a-55a5bcc75995', contemporary_originalist_majority, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('151ef3b5-10c2-4696-8f8a-55a5bcc75995', '').
narrative_ontology:cs_kernel_id(separation_of_powers_text__unitary_executive_reading, separation_of_powers_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(separation_of_powers_text__unitary_executive_reading, executive_branch).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__unitary_executive_reading, presidential_office).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__unitary_executive_reading, unitary_executive_advocates).
narrative_ontology:constraint_victim(separation_of_powers_text__unitary_executive_reading, independent_agencies).
narrative_ontology:constraint_victim(separation_of_powers_text__unitary_executive_reading, ftc).
narrative_ontology:constraint_victim(separation_of_powers_text__unitary_executive_reading, nlrb).
narrative_ontology:constraint_victim(separation_of_powers_text__unitary_executive_reading, federal_reserve).
narrative_ontology:constraint_victim(separation_of_powers_text__unitary_executive_reading, consumer_financial_protection_bureau).
narrative_ontology:constraint_victim(separation_of_powers_text__unitary_executive_reading, securities_exchange_commission).
narrative_ontology:constraint_victim(separation_of_powers_text__unitary_executive_reading, judiciary_oversight_capacity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(separation_of_powers_text__unitary_executive_reading, executive_branch).
narrative_ontology:constraint_vindicates(separation_of_powers_text__unitary_executive_reading, unitary_executive_principle).
narrative_ontology:constraint_vindicates(separation_of_powers_text__unitary_executive_reading, presidential_removal_power_absolute).
narrative_ontology:constraint_vindicates(separation_of_powers_text__unitary_executive_reading, article_ii_vesting_clause_exclusivity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Asserts plenary removal authority over all executive officers including independent agency heads; directs litigation strategy to eliminate tenure protections; appoints ideologically aligned nominees to independent agency leadership to render constraints moot from within. Collects expanded appointment and direction power as the direct beneficiary of the constraint's operation.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, presidential_office, agenda_setter,
    institutional, biographical, arbitrage, national).

% Gains centralized control over regulatory agenda previously dispersed across independent commissions; absorbs coordination costs of managing formerly autonomous agencies (political accountability for technical decisions, staffing friction). Pays in legitimacy and institutional capacity when political direction replaces expertise-driven process.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, executive_branch, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(separation_of_powers_text__unitary_executive_reading, executive_branch, payer).

% Legal scholars, Federalist Society network, and OLC attorneys who advance the unitary executive theory through judicial appointments, amicus briefs, and executive branch legal opinions. Benefit intellectually and professionally from the theory's ascendance; their exit is mobile (they can shift doctrinal focus) but identity-invested in the reading's success.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, unitary_executive_advocates, beneficiary,
    organized, generational, mobile, national).

% FTC, NLRB, Fed, CFPB, SEC and similar bodies structured with for-cause removal protections to insulate technical expertise from political cycles. Bear the full extraction: loss of statutory independence, forced policy alignment with presidential priorities, erosion of long-term institutional memory. No meaningful exit — their statutory charters are the constraint's target.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, independent_agencies, payer,
    organized, generational, trapped, national).

% Competition and consumer protection mandate requires insulation from the industries it regulates and the political branches that oversee them. For-cause removal protection is the structural guarantor; the unitary executive reading eliminates it, converting the commission into a sub-cabinet office. Bears concentrated extraction with no alternative institutional form.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, ftc, payer,
    organized, generational, trapped, national).

% Labor relations adjudication depends on perceived neutrality between unions and employers. Presidential control over the General Counsel and Board members transforms the NLRB into a partisan instrument. Extraction is the loss of legitimacy as a neutral forum — the asset the agency's statutory scheme was built to protect.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, nlrb, payer,
    organized, generational, trapped, national).

% Monetary policy independence rests on the Chair's 14-year term and for-cause removal protection. The unitary executive reading would subject Fed leadership to presidential direction, collapsing the credibility of inflation-fighting commitments. Exit is constrained — the Fed could resist through institutional norms and market signaling, but statutory independence is the structural bulwark.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, federal_reserve, payer,
    powerful, generational, constrained, national).

% Single-director structure with for-cause removal was specifically designed to survive political cycles. Seila Law (2020) already narrowed this; the unitary executive reading would complete the conversion to a standard executive office. Bears extraction as the test case for the theory's reach.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, consumer_financial_protection_bureau, payer,
    organized, generational, trapped, national).

% Multi-member commission with staggered terms and for-cause removal protects securities market integrity from political interference. Extraction is the conversion of expert-driven enforcement into politically directed enforcement — market participants lose confidence in the neutrality of the regulator.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, securities_exchange_commission, payer,
    organized, generational, trapped, national).

% Courts lose the structural basis for reviewing agency action when agencies become mere presidential subordinates. The non-delegation doctrine, major questions doctrine, and arbitrary-and-capricious review all presuppose agencies with some independent judgment. Extraction is the hollowing out of administrative law as a constraint on executive power.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, judiciary_oversight_capacity, payer,
    institutional, civilizational, constrained, national).

% Congress designs independent agencies with tenure protections precisely to create expertise repositories insulated from political cycles. The unitary executive reading declares those design choices unconstitutional, stripping Congress of its Article I authority to structure the executive branch. Would object vigorously but is excluded from the adjudication — the theory is litigated in courts, not negotiated in legislatures.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, congress_legislative_design, excluded,
    institutional, generational, constrained, national).

% Produces the doctrinal architecture for all three readings; tracks the unitary executive theory's migration from fringe to majority position on the Supreme Court. Neither collects nor pays directly but shapes the interpretive landscape through which the constraint operates.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, constitutional_law_academy, observer,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of executive branch unity: ensures the President can direct all executive officers to implement a coherent policy agenda without internal resistance from insulated bureaucracies.
% TRANSFER_FUNCTION: Moves decision-making authority over regulatory enforcement, rulemaking, and adjudication from independent agency heads to the White House; moves accountability for regulatory outcomes from expert bodies to the elected President; moves institutional independence from statutory design to presidential grace.
% ABSENT_VOICES: The regulated public (consumers, workers, investors) who rely on independent agency expertise and neutrality; future Congresses deprived of the structural tool of independent agency design; state attorneys general who coordinate with independent agencies on enforcement. They are not in the room when the Supreme Court adjudicates removal power.
% DISAPPEARANCE_RATIONALE: If the unitary executive reading vanished overnight, independent agencies would regain statutory removal protections; regulatory decision-making would revert to multi-member commissions with staggered terms; Congress would reclaim authority to design executive branch structure; the administrative state would re-fragment into expertise-driven rather than politically directed bodies.
% FOUNDING_PROBLEM: The Founding generation debated whether the executive power should be unitary or plural; the unitary executive reading was built to solve the problem of presidential accountability — ensuring the President could be held responsible for all executive action by giving him control over all executive officers.
% FOUNDING_PROBLEM_CORROBORATION: Unitary executive advocates (Calabresi, Yoo, Federalist Society) attest the founding problem is live — presidential accountability requires removal power. Formalist and functionalist scholars (Manning, Vermeule, Kagan, Strauss) plus historical practice from the First Congress through Humphrey's Executor and Morrison attest the founding problem was resolved differently: the First Congress created offices with tenure protections, and the Court has twice upheld for-cause removal. The corroboration comes from outside the beneficiary set — the historical record and the Court's own precedent contradict the unitary executive's origin claim.
narrative_ontology:disappearance_verdict(separation_of_powers_text__unitary_executive_reading, world_rearranges).
narrative_ontology:founding_problem_status(separation_of_powers_text__unitary_executive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(separation_of_powers_text__unitary_executive_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(separation_of_powers_text__unitary_executive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(separation_of_powers_text__unitary_executive_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(separation_of_powers_text__unitary_executive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(separation_of_powers_text__unitary_executive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(separation_of_powers_text__unitary_executive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.68) is high because the constraint transfers massive decision-making authority from expert-insulated bodies to a single political office — the entire regulatory state's independence is the extracted asset. Suppression (0.72) is high because the constraint's operation requires active judicial enforcement (overturning Humphrey's Executor, Morrison) and political enforcement (appointing ideologically committed judges, using OLC opinions to direct agency action). Theater (0.31) is moderate: the 'accountability' justification is genuine but increasingly performs as cover for policy control. Accessibility collapse (0.45) and resistance (0.58) reflect that alternative institutional designs (formalist, functionalist) remain legally and politically live — the constraint has not collapsed the field. The measurement series on a shared 11-point grid from 1789 to 2026 shows extraction and suppression rising together as the theory captures the Court; theater rises more slowly, tracking the gap between the theory's accountability rationale and its policy-control operation.
 *
 * PERSPECTIVAL GAP:
 *   From the presidential_office seat, the constraint is a coordination mechanism (rope-like): it solves the problem of a fragmented executive branch. From the independent_agencies seats, it is pure extraction (snare-like): their statutory independence is taken without compensation. From the judiciary_oversight_capacity seat, it is extraction of the courts' own structural role. The engine computes these divergent per-seat types from the power/exit/role data; the single claimed_type (tangled_rope) captures the hybrid structure — genuine coordination function (executive unity) with asymmetric extraction (independent agencies pay).
 *
 * DIRECTIONALITY LOGIC:
 *   presidential_office and unitary_executive_advocates are beneficiaries (d near 0.0): they collect expanded authority and doctrinal victory. executive_branch is a dual-positioned beneficiary/payer: gains control but pays in legitimacy and capacity. All independent agency seats (ftc, nlrb, federal_reserve, cfpb, sec) and judiciary_oversight_capacity are payers (d near 1.0): they bear the full extraction with trapped or constrained exit. congress_legislative_design is excluded (not a beneficiary or payer in the engine's sense — its design authority is negated, not extracted). The derivation chain produces these d values from role + power + exit; no overrides needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (presidential accountability) was live in 1789. By 1935 (Humphrey's Executor) the Court recognized that accountability and expertise-insulation can coexist. The unitary executive reading revives the founding problem as if 1935-2020 never happened — the mandate (accountability) has been met by other means (elections, oversight, functionalist structure), but the reading treats the mandate as permanently unsatisfied. This is mandatrophy: the constraint persists by declaring its founding problem eternally live, extracting independence from agencies that the founding problem never required eliminating.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    unitary_executive_historical_fact,
    'Does the historical record of the First Congress''s creation of offices with tenure protections (e.g., Comptroller of the Treasury, 1789) foreclose the unitary executive reading''s claim about original meaning, or is that record compatible with a unitary executive that permits Congress to create such offices?',
    'Originalist historical methodology applied to the 1789-1791 congressional debates and statutes; the Court''s treatment of this evidence in Seila Law and subsequent opinions.',
    'If the historical record forecloses the unitary reading''s origin claim, the reading''s foundational axiom (unitary_executive_originalist) becomes empirically_contingent and vulnerable to axiom_overriding drift. If compatible, the reading''s origin claim remains holdable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(unitary_executive_historical_fact, conceptual, 'Whether the unitary executive reading''s historical origin claim survives the First Congress''s actual practice.').

omega_variable(
    coordination_extraction_separability,
    'Can the coordination function (executive branch unity) be achieved without the extraction (eliminating independent agency independence)? Is there an institutional design that solves the unity problem while preserving expertise insulation?',
    'Comparative constitutional analysis (parliamentary systems with independent central banks, independent regulatory commissions in other presidential systems); political science literature on executive control vs. bureaucratic autonomy.',
    'If separable, the extraction is not necessary for the coordination — the tangled rope''s extraction component is gratuitous, strengthening a snare classification for the extraction sub-constraint. If inseparable, the extraction is the price of coordination, supporting the tangled_rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_separability, empirical, 'Whether the constraint''s coordination and extraction components are structurally separable.').

omega_variable(
    kernel_framing_underdetermination,
    'Does the kernel ''separation_of_powers_text'' admit only these three readings, or is there a fourth framing (e.g., a democratic accountability reading that centers legislative supremacy over executive structure) that would produce a different constraint family?',
    'Survey of constitutional theory literature for alternative framings of the separation of powers that are not reducible to formalist/functionalist/unitary; assessment of whether any such framing has institutional uptake.',
    'If a fourth framing exists and is live, the current three-constraint family is incomplete; the network.affects_constraints edges would need revision. The cs_structure.reading_relations would change if a new sibling reading forecloses or influences the unitary reading differently.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the declared kernel admits readings beyond the three currently modeled.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(separation_of_powers_text__unitary_executive_reading, 1789, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sepa_tr_t1789, separation_of_powers_text__unitary_executive_reading, theater_ratio, 1789, 0.05).
narrative_ontology:measurement(sepa_tr_t1801, separation_of_powers_text__unitary_executive_reading, theater_ratio, 1801, 0.08).
narrative_ontology:measurement(sepa_tr_t1865, separation_of_powers_text__unitary_executive_reading, theater_ratio, 1865, 0.12).
narrative_ontology:measurement(sepa_tr_t1913, separation_of_powers_text__unitary_executive_reading, theater_ratio, 1913, 0.15).
narrative_ontology:measurement(sepa_tr_t1935, separation_of_powers_text__unitary_executive_reading, theater_ratio, 1935, 0.18).
narrative_ontology:measurement(sepa_tr_t1976, separation_of_powers_text__unitary_executive_reading, theater_ratio, 1976, 0.22).
narrative_ontology:measurement(sepa_tr_t1988, separation_of_powers_text__unitary_executive_reading, theater_ratio, 1988, 0.25).
narrative_ontology:measurement(sepa_tr_t2000, separation_of_powers_text__unitary_executive_reading, theater_ratio, 2000, 0.27).
narrative_ontology:measurement(sepa_tr_t2010, separation_of_powers_text__unitary_executive_reading, theater_ratio, 2010, 0.28).
narrative_ontology:measurement(sepa_tr_t2020, separation_of_powers_text__unitary_executive_reading, theater_ratio, 2020, 0.3).
narrative_ontology:measurement(sepa_tr_t2026, separation_of_powers_text__unitary_executive_reading, theater_ratio, 2026, 0.31).

% Extraction over time
narrative_ontology:measurement(sepa_be_t1789, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 1789, 0.15).
narrative_ontology:measurement(sepa_be_t1801, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 1801, 0.22).
narrative_ontology:measurement(sepa_be_t1865, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 1865, 0.28).
narrative_ontology:measurement(sepa_be_t1913, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 1913, 0.35).
narrative_ontology:measurement(sepa_be_t1935, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 1935, 0.42).
narrative_ontology:measurement(sepa_be_t1976, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 1976, 0.48).
narrative_ontology:measurement(sepa_be_t1988, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 1988, 0.52).
narrative_ontology:measurement(sepa_be_t2000, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement(sepa_be_t2010, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 2010, 0.58).
narrative_ontology:measurement(sepa_be_t2020, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 2020, 0.64).
narrative_ontology:measurement(sepa_be_t2026, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 2026, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(sepa_su_t1789, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 1789, 0.1).
narrative_ontology:measurement(sepa_su_t1801, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 1801, 0.15).
narrative_ontology:measurement(sepa_su_t1865, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 1865, 0.2).
narrative_ontology:measurement(sepa_su_t1913, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 1913, 0.3).
narrative_ontology:measurement(sepa_su_t1935, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 1935, 0.45).
narrative_ontology:measurement(sepa_su_t1976, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 1976, 0.55).
narrative_ontology:measurement(sepa_su_t1988, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 1988, 0.6).
narrative_ontology:measurement(sepa_su_t2000, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 2000, 0.63).
narrative_ontology:measurement(sepa_su_t2010, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 2010, 0.66).
narrative_ontology:measurement(sepa_su_t2020, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 2020, 0.7).
narrative_ontology:measurement(sepa_su_t2026, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 2026, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(separation_of_powers_text__unitary_executive_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(separation_of_powers_text__unitary_executive_reading, 0.12).
narrative_ontology:affects_constraint(separation_of_powers_text__unitary_executive_reading, separation_of_powers_text__formalist_reading).
narrative_ontology:affects_constraint(separation_of_powers_text__unitary_executive_reading, separation_of_powers_text__functionalist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the separation_of_powers_text constraint family (three readings of the same constitutional text). The unitary_executive_reading extracts from independent agencies to benefit the presidential office. The formalist_reading extracts from congressional delegation to benefit judicial review. The functionalist_reading extracts from neither — it permits the existing structure. The ε values differ substantially: unitary (0.68) > formalist (~0.45) > functionalist (~0.15). The family is linked via affects_constraints in all three stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
