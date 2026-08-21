% ============================================================================
% CONSTRAINT STORY: statutory_debt_ceiling__coordination_scaffold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_statutory_debt_ceiling__coordination_scaffold_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: statutory_debt_ceiling__coordination_scaffold_reading
 *   human_readable: Statutory Debt Ceiling (Coordination Scaffold Reading)
 *   domain: constitutional_law/political_economy/fiscal_governance
 *
 * SUMMARY:
 *   This constraint story instantiates the 'coordination_scaffold_reading' of
 *   the 'statutory_debt_ceiling' kernel. This reading emphasizes the debt
 *   ceiling's role as a procedural mechanism for fiscal coordination,
 *   facilitating Treasury operations by providing an aggregate borrowing
 *   limit that Congress periodically adjusts. It is distinct from the
 *   'extraction_snare_reading' which views it as a tool for political
 *   leverage and hostage-taking, and the 'constitutional_nullity_reading'
 *   which questions its constitutional validity. In this reading, the
 *   constraint functions as a low-extraction scaffold, enabling orderly
 *   governance.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statutory_debt_ceiling__coordination_scaffold_reading, 0.15).
domain_priors:suppression_score(statutory_debt_ceiling__coordination_scaffold_reading, 0.1).
domain_priors:theater_ratio(statutory_debt_ceiling__coordination_scaffold_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__coordination_scaffold_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(statutory_debt_ceiling__coordination_scaffold_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__coordination_scaffold_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(statutory_debt_ceiling__coordination_scaffold_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statutory_debt_ceiling__coordination_scaffold_reading, scaffold).
narrative_ontology:human_readable(statutory_debt_ceiling__coordination_scaffold_reading, "Statutory Debt Ceiling (Coordination Scaffold Reading)").
narrative_ontology:topic_domain(statutory_debt_ceiling__coordination_scaffold_reading, "constitutional_law/political_economy/fiscal_governance").

domain_priors:requires_active_enforcement(statutory_debt_ceiling__coordination_scaffold_reading).
narrative_ontology:has_sunset_clause(statutory_debt_ceiling__coordination_scaffold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statutory_debt_ceiling__coordination_scaffold_reading, '851c68b7-c266-4dbc-a78c-d8bed6f3f76b').
narrative_ontology:cs_kernel_codification('851c68b7-c266-4dbc-a78c-d8bed6f3f76b', formalized).
narrative_ontology:cs_authority_grounding('851c68b7-c266-4dbc-a78c-d8bed6f3f76b', lineage).
narrative_ontology:cs_interpretation_layer_present('851c68b7-c266-4dbc-a78c-d8bed6f3f76b').
narrative_ontology:cs_reading_relation('851c68b7-c266-4dbc-a78c-d8bed6f3f76b', statutory_debt_ceiling__extraction_snare_reading, coexists_with).
narrative_ontology:cs_reading_relation('851c68b7-c266-4dbc-a78c-d8bed6f3f76b', statutory_debt_ceiling__constitutional_nullity_reading, coexists_with).
narrative_ontology:cs_axiom('851c68b7-c266-4dbc-a78c-d8bed6f3f76b', foundational, congressional_fiscal_prerogative).
narrative_ontology:cs_axiom_status(congressional_fiscal_prerogative, holdable).
narrative_ontology:cs_axiom_grounding('851c68b7-c266-4dbc-a78c-d8bed6f3f76b', congressional_fiscal_prerogative, conventional).
narrative_ontology:cs_axiom('851c68b7-c266-4dbc-a78c-d8bed6f3f76b', foundational, treasury_operational_autonomy).
narrative_ontology:cs_axiom_status(treasury_operational_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('851c68b7-c266-4dbc-a78c-d8bed6f3f76b', treasury_operational_autonomy, conventional).
narrative_ontology:cs_reference_frame('851c68b7-c266-4dbc-a78c-d8bed6f3f76b', orderly_fiscal_management).
narrative_ontology:cs_drift_state('851c68b7-c266-4dbc-a78c-d8bed6f3f76b', contemporary_procedural_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('851c68b7-c266-4dbc-a78c-d8bed6f3f76b', '').
narrative_ontology:cs_kernel_id(statutory_debt_ceiling__coordination_scaffold_reading, statutory_debt_ceiling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__coordination_scaffold_reading, us_treasury).
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__coordination_scaffold_reading, congressional_majority).
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__coordination_scaffold_reading, us_taxpayers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(statutory_debt_ceiling__coordination_scaffold_reading, congressional_minority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operates within the aggregate debt limit set by Congress, managing the day-to-day borrowing needs of the federal government. Benefits from the clarity and predictability of an overall limit, avoiding constant micromanagement by Congress.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__coordination_scaffold_reading, us_treasury, agenda_setter,
    institutional, immediate, constrained, national).

% Sets the aggregate fiscal policy by adjusting the debt ceiling, without needing to approve every individual debt issuance. Benefits from a streamlined process that allows focus on broader legislative priorities.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__coordination_scaffold_reading, congressional_majority, beneficiary,
    institutional, biographical, mobile, national).

% Participates in the process of adjusting the debt ceiling, which, in this reading, is a routine procedural vote. Bears the 'cost' of not being able to micromanage Treasury operations, but benefits from the overall fiscal discipline the limit represents.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__coordination_scaffold_reading, congressional_minority, payer,
    powerful, biographical, constrained, national).

% Reacts to the stability and predictability of U.S. fiscal management. Benefits from the orderly process of debt issuance and the avoidance of default risk, which the debt ceiling, in this reading, helps to ensure.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__coordination_scaffold_reading, financial_markets, observer,
    organized, immediate, mobile, global).

% Benefits from the orderly and predictable management of federal finances, which helps maintain economic stability and the government's ability to fund essential services. Bears the ultimate cost of federal debt, but this constraint aims to manage it responsibly.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__coordination_scaffold_reading, us_taxpayers, beneficiary,
    powerless, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(statutory_debt_ceiling__coordination_scaffold_reading, diffuse).
narrative_ontology:fixing_cost_class(statutory_debt_ceiling__coordination_scaffold_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To provide a procedural mechanism for Congress to retain ultimate control over the total amount of federal debt, while delegating the day-to-day management of borrowing to the Treasury, thereby facilitating efficient government operations and avoiding repeated legislative micromanagement of individual debt issuances.
% TRANSFER_FUNCTION: Facilitates the flow of funds necessary for government operations by providing a clear, albeit adjustable, aggregate borrowing authority. In this reading, it does not directly extract or transfer wealth, but rather coordinates fiscal management.
% ABSENT_VOICES: In this reading, which emphasizes the procedural and coordinative function, there are no significant absent voices, as the mechanism is generally accepted as a necessary tool for fiscal governance. Critiques of its potential for weaponization belong to other readings.
% DISAPPEARANCE_RATIONALE: If the debt ceiling vanished overnight, Congress would either need to approve every bond issuance individually (an impractical and inefficient process) or grant unlimited borrowing authority to the Treasury (a significant loss of legislative fiscal control). This would fundamentally alter the balance of power in fiscal governance and Treasury operations.
% FOUNDING_PROBLEM: The problem of balancing congressional oversight of federal borrowing with the need for the Treasury to efficiently manage day-to-day government finances without constant legislative intervention for every debt issuance.
% FOUNDING_PROBLEM_CORROBORATION: Fiscal policy experts, government accountability offices, and historical legislative records generally corroborate the ongoing need for a mechanism to manage aggregate federal debt and coordinate borrowing authority, even if the specific form of the debt ceiling is debated.
narrative_ontology:disappearance_verdict(statutory_debt_ceiling__coordination_scaffold_reading, world_rearranges).
narrative_ontology:founding_problem_status(statutory_debt_ceiling__coordination_scaffold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statutory_debt_ceiling__coordination_scaffold_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(statutory_debt_ceiling__coordination_scaffold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(statutory_debt_ceiling__coordination_scaffold_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(statutory_debt_ceiling__coordination_scaffold_reading_tests).
:- end_tests(statutory_debt_ceiling__coordination_scaffold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness (0.15) reflects that, in this reading, the debt ceiling primarily serves an administrative and coordinative function, with minimal direct costs beyond the procedural overhead of periodic adjustments. Suppression (0.10) is low because it's a generally accepted procedural tool, not actively coercing or preventing alternatives, but rather structuring them. The very low theater ratio (0.05) indicates that its function is overwhelmingly genuine, with little performative aspect. The measurements show a stable, low-impact trajectory over the period where this reading is most applicable, reflecting routine adjustments without significant political weaponization.
 *
 * PERSPECTIVAL GAP:
 *   The primary perspectival gap for the debt ceiling lies between this 'coordination scaffold' reading and the 'extraction snare' reading. While this reading sees a functional, low-impact procedural tool, the snare reading would see high extraction and suppression, driven by political opportunism. The engine's classification would highlight this divergence based on the differing metric profiles and stakeholder declarations across the readings.
 *
 * DIRECTIONALITY LOGIC:
 *   The US Treasury and the Congressional Majority are beneficiaries, as the debt ceiling, in this reading, streamlines their operations and provides a clear framework for fiscal policy. The Congressional Minority, while participating in the adjustment process, is a 'payer' in the sense that their ability to micromanage is constrained by the aggregate limit, though they also benefit from the overall fiscal order. US Taxpayers are diffuse beneficiaries of stable fiscal management. Financial markets are observers, reacting to the stability this mechanism provides.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    debt_ceiling_functional_ambiguity,
    'Is the statutory debt ceiling primarily a procedural coordination mechanism, or has its function drifted towards enabling political extraction and hostage-taking?',
    'Analysis of legislative history and political science data, specifically tracking instances of government shutdowns or near-defaults tied to debt ceiling debates, and the outcomes of such confrontations.',
    'If resolved as primarily coordination, this ''scaffold'' classification holds. If resolved as enabling extraction, the constraint would reclassify towards a ''snare'' or ''tangled_rope'' for periods of weaponization.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(debt_ceiling_functional_ambiguity, empirical, 'Ambiguity between coordination and extraction functions of the debt ceiling.').

omega_variable(
    constitutional_validity_ambiguity,
    'Is the statutory debt ceiling a legitimate exercise of legislative power, or is it superseded by the 14th Amendment''s Public Debt Clause, rendering it constitutionally void?',
    'Supreme Court ruling on the constitutionality of the debt ceiling, or a sustained period of executive action asserting the 14th Amendment''s supremacy in a debt crisis.',
    'If resolved as constitutionally void, the constraint would effectively cease to exist as a binding legal mechanism, reclassifying as a ''mountain'' (of constitutional law) that forecloses the statutory constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_validity_ambiguity, conceptual, 'Ambiguity regarding the constitutional validity of the debt ceiling.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statutory_debt_ceiling__coordination_scaffold_reading, 1970, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1970, statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 1970, 0.03).
narrative_ontology:measurement(stat_tr_t1980, statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 1980, 0.04).
narrative_ontology:measurement(stat_tr_t1990, statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 1990, 0.04).
narrative_ontology:measurement(stat_tr_t2000, statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 2000, 0.05).
narrative_ontology:measurement(stat_tr_t2010, statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 2010, 0.05).
narrative_ontology:measurement(stat_tr_t2020, statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 2020, 0.05).

% Extraction over time
narrative_ontology:measurement(stat_be_t1970, statutory_debt_ceiling__coordination_scaffold_reading, base_extractiveness, 1970, 0.12).
narrative_ontology:measurement(stat_be_t1980, statutory_debt_ceiling__coordination_scaffold_reading, base_extractiveness, 1980, 0.13).
narrative_ontology:measurement(stat_be_t1990, statutory_debt_ceiling__coordination_scaffold_reading, base_extractiveness, 1990, 0.14).
narrative_ontology:measurement(stat_be_t2000, statutory_debt_ceiling__coordination_scaffold_reading, base_extractiveness, 2000, 0.14).
narrative_ontology:measurement(stat_be_t2010, statutory_debt_ceiling__coordination_scaffold_reading, base_extractiveness, 2010, 0.15).
narrative_ontology:measurement(stat_be_t2020, statutory_debt_ceiling__coordination_scaffold_reading, base_extractiveness, 2020, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1970, statutory_debt_ceiling__coordination_scaffold_reading, suppression_requirement, 1970, 0.08).
narrative_ontology:measurement(stat_su_t1980, statutory_debt_ceiling__coordination_scaffold_reading, suppression_requirement, 1980, 0.09).
narrative_ontology:measurement(stat_su_t1990, statutory_debt_ceiling__coordination_scaffold_reading, suppression_requirement, 1990, 0.09).
narrative_ontology:measurement(stat_su_t2000, statutory_debt_ceiling__coordination_scaffold_reading, suppression_requirement, 2000, 0.1).
narrative_ontology:measurement(stat_su_t2010, statutory_debt_ceiling__coordination_scaffold_reading, suppression_requirement, 2010, 0.1).
narrative_ontology:measurement(stat_su_t2020, statutory_debt_ceiling__coordination_scaffold_reading, suppression_requirement, 2020, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statutory_debt_ceiling__coordination_scaffold_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'statutory_debt_ceiling' kernel, alongside 'extraction_snare_reading' and 'constitutional_nullity_reading'. Each reading presents a distinct structural and functional interpretation of the same underlying legal mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
