% ============================================================================
% CONSTRAINT STORY: statutory_debt_ceiling__extraction_snare_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_statutory_debt_ceiling__extraction_snare_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: statutory_debt_ceiling__extraction_snare_reading
 *   human_readable: Statutory Debt Ceiling as Weaponized Extraction Mechanism
 *   domain: constitutional/political_economy/fiscal_governance
 *
 * SUMMARY:
 *   The statutory debt ceiling, originally a coordination scaffold for
 *   Treasury borrowing authority, has mutated into a weaponized boundary that
 *   enables a cohesive legislative minority to extract policy concessions
 *   under threat of sovereign default. This reading instantiates the
 *   constraint as a high-extractiveness snare: the coordination function is
 *   dead (founding_problem_status=dead, corroborated by Treasury/CRS/BPC),
 *   the suppression requirement has intensified from 0.15 to 0.88 as
 *   enforcement machinery (extraordinary measures, credit rating
 *   surveillance, market discipline) matured, and the extraction accrues to
 *   identifiable beneficiaries (legislative minority faction, ideological
 *   donor networks) while costs are distributed across trapped and
 *   constrained payers (federal employees, Social Security recipients,
 *   bondholders, states, global financial system). The constraint persists
 *   not because it solves a coordination problem but because the minority
 *   faction and its donor network have institutionalized the hostage-taking
 *   mechanism and would lose structural leverage if it were abolished.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statutory_debt_ceiling__extraction_snare_reading, 0.82).
domain_priors:suppression_score(statutory_debt_ceiling__extraction_snare_reading, 0.88).
domain_priors:theater_ratio(statutory_debt_ceiling__extraction_snare_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statutory_debt_ceiling__extraction_snare_reading, snare).
narrative_ontology:human_readable(statutory_debt_ceiling__extraction_snare_reading, "Statutory Debt Ceiling as Weaponized Extraction Mechanism").
narrative_ontology:topic_domain(statutory_debt_ceiling__extraction_snare_reading, "constitutional/political_economy/fiscal_governance").

domain_priors:requires_active_enforcement(statutory_debt_ceiling__extraction_snare_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statutory_debt_ceiling__extraction_snare_reading, 'db7f7113-fda2-48d4-b3c4-e85cde12fd5e').
narrative_ontology:cs_kernel_codification('db7f7113-fda2-48d4-b3c4-e85cde12fd5e', formalized).
narrative_ontology:cs_authority_grounding('db7f7113-fda2-48d4-b3c4-e85cde12fd5e', extraction).
narrative_ontology:cs_interpretation_layer_present('db7f7113-fda2-48d4-b3c4-e85cde12fd5e').
narrative_ontology:cs_reading_relation('db7f7113-fda2-48d4-b3c4-e85cde12fd5e', statutory_debt_ceiling__coordination_scaffold_reading, forecloses).
narrative_ontology:cs_reading_relation('db7f7113-fda2-48d4-b3c4-e85cde12fd5e', statutory_debt_ceiling__constitutional_nullity_reading, influences).
narrative_ontology:cs_axiom('db7f7113-fda2-48d4-b3c4-e85cde12fd5e', foundational, debt_ceiling_is_weaponized_hostage_mechanism).
narrative_ontology:cs_axiom_status(debt_ceiling_is_weaponized_hostage_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('db7f7113-fda2-48d4-b3c4-e85cde12fd5e', debt_ceiling_is_weaponized_hostage_mechanism, empirically_contingent).
narrative_ontology:cs_axiom('db7f7113-fda2-48d4-b3c4-e85cde12fd5e', foundational, minority_veto_extracts_policy_concessions).
narrative_ontology:cs_axiom_status(minority_veto_extracts_policy_concessions, holdable).
narrative_ontology:cs_axiom_grounding('db7f7113-fda2-48d4-b3c4-e85cde12fd5e', minority_veto_extracts_policy_concessions, empirically_contingent).
narrative_ontology:cs_axiom('db7f7113-fda2-48d4-b3c4-e85cde12fd5e', secondary, founding_coordination_function_is_obsolete).
narrative_ontology:cs_axiom_status(founding_coordination_function_is_obsolete, holdable).
narrative_ontology:cs_axiom_grounding('db7f7113-fda2-48d4-b3c4-e85cde12fd5e', founding_coordination_function_is_obsolete, empirically_contingent).
narrative_ontology:cs_reference_frame('db7f7113-fda2-48d4-b3c4-e85cde12fd5e', original_1917_1939_coordination_scaffold).
narrative_ontology:cs_drift_state('db7f7113-fda2-48d4-b3c4-e85cde12fd5e', post_2011_brinkmanship_normalization, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('db7f7113-fda2-48d4-b3c4-e85cde12fd5e', '').
narrative_ontology:cs_kernel_id(statutory_debt_ceiling__extraction_snare_reading, statutory_debt_ceiling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__extraction_snare_reading, legislative_minority_faction).
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__extraction_snare_reading, ideological_donor_networks).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, federal_employees).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, social_security_recipients).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, treasury_bondholders).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, state_local_governments).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, global_financial_system_participants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, treasury_secretary).
narrative_ontology:constraint_vindicates(statutory_debt_ceiling__extraction_snare_reading, legislative_power_of_the_purse).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A cohesive minority caucus in one chamber uses the debt ceiling vote as a choke point to extract policy concessions impossible through normal legislative process. They face primary challenges if they don't use the weapon, and lose leverage if the ceiling is abolished or suspended permanently. Their structural position depends on maintaining the credibility of the default threat.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, legislative_minority_faction, beneficiary,
    organized, biographical, constrained, national).

% Fund and coordinate primary challenges against incumbents who vote for clean debt ceiling increases. They extract ideological compliance from legislators and policy concessions from the executive. Their exit is arbitrage-grade: they can shift funding to other pressure points (judicial appointments, state-level races) if the debt ceiling mechanism degrades.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, ideological_donor_networks, beneficiary,
    powerful, generational, arbitrage, national).

% Face furloughs, delayed paychecks, and operational chaos during standoffs. Cannot exit the employment relationship without losing pension accrual, security clearance, and specialized career capital. The constraint extracts labor stability and financial security from them with zero compensatory mechanism.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, federal_employees, payer,
    powerless, immediate, trapped, national).

% Depend on timely benefit payments for basic survival. Have no alternative income source, no political leverage, and no ability to 'wait out' a default. The constraint extracts existential security from the most vulnerable population as collateral in a political fight they cannot influence.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, social_security_recipients, payer,
    powerless, immediate, trapped, national).

% Hold the 'risk-free' asset that becomes risky during standoffs. Credit rating downgrades (S&P 2011, Fitch 2023) impose permanent risk premium increases. Can diversify but cannot exit the dollar system; the constraint extracts a structural risk premium from the global reserve asset's credibility.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, treasury_bondholders, payer,
    organized, biographical, constrained, global).

% Depend on federal grants and reimbursements that halt during standoffs. Must maintain balanced budgets while federal partners default on obligations. No exit from the federal fiscal partnership; the constraint extracts fiscal stability from sub-sovereign units that did not choose the fight.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, state_local_governments, payer,
    moderate, biographical, constrained, national).

% Central banks, sovereign wealth funds, and commercial banks worldwide hold Treasuries as collateral and reserves. A U.S. default would cascade through repo markets, derivative clearing, and currency pegs. Cannot exit the dollar system without decades of transition; the constraint extracts systemic stability from the entire global financial architecture.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, global_financial_system_participants, payer,
    organized, generational, constrained, global).

% Must execute 'extraordinary measures' (suspending pension fund investments, cash management gymnastics) to delay default. Personally bears reputational and legal risk if measures fail. Administers the constraint's operational mechanics but cannot change the statutory trigger; the constraint extracts administrative credibility and personal liability from the office.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, treasury_secretary, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(statutory_debt_ceiling__extraction_snare_reading, treasury_secretary, payer).

% Analyze the 14th Amendment Section 4 argument that the debt ceiling is unconstitutional. Provide the intellectual infrastructure for the constitutional_nullity_reading. Their structural position is analytical: they see the full constraint family but hold no operational lever.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The debt ceiling was originally enacted (1917, 1939) to give Treasury blanket borrowing authority up to a limit, replacing the need for Congress to approve each bond issuance individually — a genuine coordination function for wartime and depression-era finance.
% TRANSFER_FUNCTION: Moves policy concessions, legislative priorities, and executive branch compliance from the majority/governing coalition to a cohesive minority faction, using the threat of sovereign default as the extraction lever. The transfer is not monetary but political: the minority receives policy outcomes it could not win through elections or normal legislation; the majority pays in governance capacity, institutional credibility, and economic stability.
% ABSENT_VOICES: Future generations who inherit the degraded credibility of U.S. sovereign debt and the normalized hostage-taking precedent. Foreign sovereign holders of Treasuries (China, Japan, Gulf states) who have no vote in U.S. politics but bear the systemic risk. The 'full faith and credit' itself — a constitutional commitment that becomes a bargaining chip.
% DISAPPEARANCE_RATIONALE: If the statutory debt ceiling vanished overnight, Treasury would revert to managing debt issuance under general borrowing authority (as most sovereigns do). The minority faction would lose its primary leverage point. Governing majorities would pass budgets without a separate debt vote. Credit rating agencies would remove the 'political brinkmanship' risk factor. The global reserve asset would trade on economic fundamentals alone. The arrangement's disappearance would rearrange legislative-executive power dynamics, fiscal credibility, and global financial architecture.
% FOUNDING_PROBLEM: The original founding problem (1917 Liberty Bond Acts, 1939 Public Debt Act) was congressional micromanagement of every bond issuance — Congress needed to delegate blanket borrowing authority to Treasury for wartime and crisis speed while retaining a statutory check.
% FOUNDING_PROBLEM_CORROBORATION: Treasury's own historical analysis (Office of Debt Management), the Congressional Research Service, and the Bipartisan Policy Center all document that the original coordination problem — congressional approval of each issuance — has been obsolete for decades. Modern budget process (Congressional Budget Act 1974) already controls spending; the debt ceiling votes on paying for spending already enacted. No credible analyst outside the benefiting minority faction argues the ceiling serves its original coordination function.
narrative_ontology:disappearance_verdict(statutory_debt_ceiling__extraction_snare_reading, world_rearranges).
narrative_ontology:founding_problem_status(statutory_debt_ceiling__extraction_snare_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statutory_debt_ceiling__extraction_snare_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(statutory_debt_ceiling__extraction_snare_reading, 'none', 1).
narrative_ontology:epsilon_provenance(statutory_debt_ceiling__extraction_snare_reading, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(statutory_debt_ceiling__extraction_snare_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(statutory_debt_ceiling__extraction_snare_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(statutory_debt_ceiling__extraction_snare_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.82) is high because the constraint's operation transfers political power and policy outcomes from the governing majority to a minority faction, while imposing systemic economic risk on populations with no voice in the standoff. Suppression (0.88) is near-maximum because the constraint's persistence depends on actively maintaining the credibility of the default threat — any credible exit (14th Amendment invocation, platinum coin seigniorage, clean suspension) is suppressed by the minority faction's veto power and the donor network's primary enforcement. Theater ratio (0.42) reflects that the 'fiscal responsibility' framing is increasingly performative: the same minority faction routinely votes for deficit-increasing tax cuts and spending when in the majority. Accessibility collapse (0.35) is moderate because alternatives exist (abolition, 14th Amendment, automatic suspension) but are politically inaccessible due to the minority's structural veto. Resistance (0.68) is substantial: the executive branch has developed extraordinary measures, credit rating agencies have downgraded, and constitutional scholars have advanced the nullity argument — but resistance has not translated into structural change because the minority's veto point holds.
 *
 * PERSPECTIVAL GAP:
 *   From the minority faction's seat, the constraint is a legitimate exercise of the power of the purse — a constitutional check on executive spending. From the trapped payers' seats, it is a hostage mechanism that converts their existential dependence into political leverage for a faction they cannot vote against. From the analytical seat, the constraint is a dead coordination scaffold that has been colonized by an extraction logic the original framers did not anticipate. The engine computes these divergent per-seat classifications from the structural data; the authored claim (snare) reflects the analytical seat's reading.
 *
 * DIRECTIONALITY LOGIC:
 *   The legislative minority faction and its donor network are structural beneficiaries (d near 0.0): they collect policy concessions and ideological compliance without bearing the costs of default. Federal employees and Social Security recipients are full targets (d near 1.0): trapped, identity-locked to their benefits/employment, bearing existential risk with zero compensatory mechanism. Treasury bondholders and state/local governments are constrained targets (d ~0.7-0.8): organized but cannot exit the dollar/federal system. The Treasury Secretary is a dual-positioned agenda_setter/payer: administers the constraint's mechanics but bears personal liability and reputational risk. Constitutional scholars are analytical observers (d=0.5): they see the structure but hold no lever.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (congressional micromanagement of bond issuance) is dead — corroborated by Treasury, CRS, and BPC. The arrangement persists as a zombie constraint: its original justification is gone, but the institutional inertia of the statutory text and the minority faction's captured veto point prevent abolition. The mandatrophy is not resolved (mandatrophy_resolved=false) because the benefiting faction actively defends the constraint's current form against all reform attempts. This is not a piton (which would have no concentrated beneficiary) — the minority faction and donor network are concentrated beneficiaries who profit from the constraint's persistence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fourteenth_amendment_justiciability,
    'Is the 14th Amendment Section 4 argument (debt ceiling unconstitutional) justiciable, or does the political question doctrine permanently bar judicial resolution?',
    'A test case where Treasury invokes the 14th Amendment to issue debt past the ceiling, forcing standing and merits review. The Supreme Court''s composition (shaped by the same donor network that benefits from the snare) determines the outcome.',
    'If justiciable and upheld, the debt ceiling is void — the snare dissolves. If non-justiciable or rejected, the snare''s legal architecture is ratified and the minority''s veto point is constitutionally entrenched.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fourteenth_amendment_justiciability, empirical, 'Whether the constitutional escape hatch is legally operable or structurally sealed.').

omega_variable(
    minority_faction_cohesion_under_majority_rule,
    'Would the legislative minority faction maintain cohesion and extraction capacity if the debt ceiling were abolished and replaced with majority-rule governance?',
    'Counterfactual analysis of the faction''s behavior in contexts where they lack a veto point (e.g., budget reconciliation, judicial nominations under nuclear option). Track whether they develop alternative extraction mechanisms.',
    'If the faction would develop alternative hostage mechanisms (government shutdowns, appropriations riders), the snare''s extraction logic is portable — abolition alone doesn''t solve the structural problem. If the faction collapses without the choke point, the debt ceiling is the linchpin of their power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_faction_cohesion_under_majority_rule, conceptual, 'Whether the extraction logic is specific to the debt ceiling mechanism or portable to any available veto point.').

omega_variable(
    global_reserve_currency_exit_threshold,
    'At what level of repeated brinkmanship does the dollar''s reserve currency status incur irreversible fragmentation?',
    'Monitor central bank reserve allocation shifts, bilateral currency swap expansion, and alternative clearing system development (mBridge, BRICS Pay) as functions of debt ceiling crisis frequency/severity.',
    'If the threshold is near, the snare''s extraction is self-limiting: the minority faction destroys the asset (global dollar hegemony) that makes the hostage valuable. If the threshold is distant, the snare can persist indefinitely with escalating systemic cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(global_reserve_currency_exit_threshold, empirical, 'Whether the snare''s extraction has a structural ceiling imposed by the global system''s tolerance.').

omega_variable(
    kernel_reading_relations_framing,
    'Does the extraction_snare_reading foreclose, coexist with, or influence the constitutional_nullity_reading and coordination_scaffold_reading?',
    'Structural analysis: if the snare reading''s core premise (the constraint is a weaponized hostage mechanism) is true, does that logically eliminate the scaffold reading''s premise (the constraint coordinates Treasury operations) within any single framework? Does it create downstream pressure on the nullity reading by entrenching the judicial appointments that would decide it?',
    'Determines the reading_relations edges in cs_structure. A forecloses relation would mean the snare and scaffold cannot both be true in one framework. An influences relation would mean the snare''s entrenchment of minority veto power over judicial appointments structurally pressures the nullity reading''s viability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_relations_framing, conceptual, 'Structural relationship between this reading and its sibling readings in the kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statutory_debt_ceiling__extraction_snare_reading, 1917, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(statutory_debt_ceiling__extraction_snare_reading_tr_t1917, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 1917, 0.05).
narrative_ontology:measurement(statutory_debt_ceiling__extraction_snare_reading_tr_t1939, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 1939, 0.08).
narrative_ontology:measurement(statutory_debt_ceiling__extraction_snare_reading_tr_t1979, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 1979, 0.15).
narrative_ontology:measurement(statutory_debt_ceiling__extraction_snare_reading_tr_t1995, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 1995, 0.28).
narrative_ontology:measurement(statutory_debt_ceiling__extraction_snare_reading_tr_t2011, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 2011, 0.38).
narrative_ontology:measurement(statutory_debt_ceiling__extraction_snare_reading_tr_t2013, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 2013, 0.4).
narrative_ontology:measurement(statutory_debt_ceiling__extraction_snare_reading_tr_t2021, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 2021, 0.41).
narrative_ontology:measurement(statutory_debt_ceiling__extraction_snare_reading_tr_t2023, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 2023, 0.42).
narrative_ontology:measurement(statutory_debt_ceiling__extraction_snare_reading_tr_t2025, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 2025, 0.42).

% Extraction over time
narrative_ontology:measurement(statutory_debt_ceiling__extraction_snare_reading_be_t1917, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 1917, 0.08).
narrative_ontology:measurement(statutory_debt_ceiling__extraction_snare_reading_be_t1939, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 1939, 0.12).
narrative_ontology:measurement(statutory_debt_ceiling__extraction_snare_reading_be_t1979, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 1979, 0.18).
narrative_ontology:measurement(statutory_debt_ceiling__extraction_snare_reading_be_t1995, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 1995, 0.45).
narrative_ontology:measurement(statutory_debt_ceiling__extraction_snare_reading_be_t2011, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 2011, 0.71).
narrative_ontology:measurement(statutory_debt_ceiling__extraction_snare_reading_be_t2013, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 2013, 0.76).
narrative_ontology:measurement(statutory_debt_ceiling__extraction_snare_reading_be_t2021, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 2021, 0.79).
narrative_ontology:measurement(statutory_debt_ceiling__extraction_snare_reading_be_t2023, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 2023, 0.81).
narrative_ontology:measurement(statutory_debt_ceiling__extraction_snare_reading_be_t2025, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 2025, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(statutory_debt_ceiling__extraction_snare_reading_su_t1917, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 1917, 0.15).
narrative_ontology:measurement(statutory_debt_ceiling__extraction_snare_reading_su_t1939, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 1939, 0.2).
narrative_ontology:measurement(statutory_debt_ceiling__extraction_snare_reading_su_t1979, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 1979, 0.35).
narrative_ontology:measurement(statutory_debt_ceiling__extraction_snare_reading_su_t1995, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 1995, 0.65).
narrative_ontology:measurement(statutory_debt_ceiling__extraction_snare_reading_su_t2011, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 2011, 0.82).
narrative_ontology:measurement(statutory_debt_ceiling__extraction_snare_reading_su_t2013, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 2013, 0.85).
narrative_ontology:measurement(statutory_debt_ceiling__extraction_snare_reading_su_t2021, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 2021, 0.87).
narrative_ontology:measurement(statutory_debt_ceiling__extraction_snare_reading_su_t2023, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 2023, 0.88).
narrative_ontology:measurement(statutory_debt_ceiling__extraction_snare_reading_su_t2025, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 2025, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statutory_debt_ceiling__extraction_snare_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(statutory_debt_ceiling__extraction_snare_reading, statutory_debt_ceiling__coordination_scaffold_reading).
narrative_ontology:affects_constraint(statutory_debt_ceiling__extraction_snare_reading, statutory_debt_ceiling__constitutional_nullity_reading).
narrative_ontology:affects_constraint(statutory_debt_ceiling__extraction_snare_reading, appropriations_process__government_shutdown_snare).
narrative_ontology:affects_constraint(statutory_debt_ceiling__extraction_snare_reading, federal_reserve_independence__political_capture_risk).
narrative_ontology:affects_constraint(statutory_debt_ceiling__extraction_snare_reading, global_dollar_hegemony__reserve_currency_trust).

% DUAL FORMULATION NOTE:
% This reading (extraction_snare) and the coordination_scaffold_reading decompose the same statutory text into two constraints with divergent ε: the scaffold reading authors ε≈0.15 (coordination overhead), this reading authors ε≈0.82 (systemic extraction). The constitutional_nullity_reading authors ε≈0.0 for the ceiling itself but ε>0 for the constitutional crisis its invocation would trigger. All three share the kernel statutory_debt_ceiling and form a constraint family linked by affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(statutory_debt_ceiling__extraction_snare_reading, institutional, 0.35).
constraint_indexing:directionality_override(statutory_debt_ceiling__extraction_snare_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
