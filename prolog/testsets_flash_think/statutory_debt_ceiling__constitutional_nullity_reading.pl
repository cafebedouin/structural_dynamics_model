% ============================================================================
% CONSTRAINT STORY: statutory_debt_ceiling__constitutional_nullity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: statutory_debt_ceiling__constitutional_nullity_reading
 *   human_readable: Statutory Debt Ceiling (Constitutional Nullity Reading)
 *   domain: constitutional_law/political_economy/fiscal_governance
 *
 * SUMMARY:
 *   This constraint story analyzes the statutory debt ceiling from the
 *   perspective of the 'constitutional nullity' reading, which posits that
 *   the 14th Amendment, Section 4, renders the debt ceiling legally
 *   inoperative as a limit on the Executive Branch's duty to pay legally
 *   incurred debts. Under this reading, the debt ceiling is a Piton: its
 *   original function has atrophied, and its persistence is due to
 *   institutional inertia and political theater, rather than genuine legal
 *   force. The metrics reflect its status as a largely performative
 *   constraint with minimal actual extractive or suppressive power, but
 *   significant theatricality.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statutory_debt_ceiling__constitutional_nullity_reading, 0.15).
domain_priors:suppression_score(statutory_debt_ceiling__constitutional_nullity_reading, 0.1).
domain_priors:theater_ratio(statutory_debt_ceiling__constitutional_nullity_reading, 0.85).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 0.85).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statutory_debt_ceiling__constitutional_nullity_reading, piton).
narrative_ontology:human_readable(statutory_debt_ceiling__constitutional_nullity_reading, "Statutory Debt Ceiling (Constitutional Nullity Reading)").
narrative_ontology:topic_domain(statutory_debt_ceiling__constitutional_nullity_reading, "constitutional_law/political_economy/fiscal_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statutory_debt_ceiling__constitutional_nullity_reading, '062fd3d5-3faa-4128-aafe-48eb54fd0212').
narrative_ontology:cs_kernel_codification('062fd3d5-3faa-4128-aafe-48eb54fd0212', fixed_text).
narrative_ontology:cs_authority_grounding('062fd3d5-3faa-4128-aafe-48eb54fd0212', lineage).
narrative_ontology:cs_interpretation_layer_present('062fd3d5-3faa-4128-aafe-48eb54fd0212').
narrative_ontology:cs_reading_relation('062fd3d5-3faa-4128-aafe-48eb54fd0212', statutory_debt_ceiling__coordination_scaffold_reading, forecloses).
narrative_ontology:cs_reading_relation('062fd3d5-3faa-4128-aafe-48eb54fd0212', statutory_debt_ceiling__extraction_snare_reading, forecloses).
narrative_ontology:cs_axiom('062fd3d5-3faa-4128-aafe-48eb54fd0212', foundational, debt_validity_unquestionable).
narrative_ontology:cs_axiom_status(debt_validity_unquestionable, holdable).
narrative_ontology:cs_axiom_grounding('062fd3d5-3faa-4128-aafe-48eb54fd0212', debt_validity_unquestionable, deontological).
narrative_ontology:cs_axiom('062fd3d5-3faa-4128-aafe-48eb54fd0212', foundational, executive_duty_to_pay).
narrative_ontology:cs_axiom_status(executive_duty_to_pay, holdable).
narrative_ontology:cs_axiom_grounding('062fd3d5-3faa-4128-aafe-48eb54fd0212', executive_duty_to_pay, deontological).
narrative_ontology:cs_reference_frame('062fd3d5-3faa-4128-aafe-48eb54fd0212', constitutional_supremacy_framework).
narrative_ontology:cs_drift_state('062fd3d5-3faa-4128-aafe-48eb54fd0212', contemporary_political_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('062fd3d5-3faa-4128-aafe-48eb54fd0212', '').
narrative_ontology:cs_kernel_id(statutory_debt_ceiling__constitutional_nullity_reading, statutory_debt_ceiling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__constitutional_nullity_reading, congress_minority_party).
narrative_ontology:constraint_victim(statutory_debt_ceiling__constitutional_nullity_reading, us_public).
narrative_ontology:constraint_victim(statutory_debt_ceiling__constitutional_nullity_reading, congress_majority_party).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Under this reading, the Treasury has a constitutional duty to pay all legally incurred debts, regardless of the statutory debt ceiling. Its actions are guided by appropriations, not the ceiling. It faces political pressure but is legally bound to avoid default.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, us_treasury, agenda_setter,
    institutional, immediate, constrained, national).

% Asserts the constitutional nullity of the debt ceiling under the 14th Amendment, Section 4, and directs the Treasury to continue borrowing to meet obligations. Bears political costs for this stance but avoids economic catastrophe.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, president_executive_branch, agenda_setter,
    institutional, biographical, constrained, national).

% Bears the political cost of the debt ceiling debate, as it is responsible for appropriations that necessitate borrowing. Under this reading, it is legally compelled to allow borrowing but faces political attacks for 'raising the debt ceiling'.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, congress_majority_party, payer,
    organized, biographical, constrained, national).

% Benefits from the political leverage created by the debt ceiling debate, using it to extract concessions or criticize the majority party's fiscal policy. Under this reading, they cannot legally force a default, but the political theater serves their agenda.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, congress_minority_party, beneficiary,
    organized, biographical, mobile, national).

% The ultimate arbiter of constitutional questions, but has historically avoided ruling directly on the debt ceiling's constitutionality, preferring to let political branches resolve it. Its potential intervention looms over the debate.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, us_supreme_court, observer,
    institutional, generational, analytical, national).

% Bears the uncertainty and potential economic instability caused by the political brinkmanship around the debt ceiling, even if a default is constitutionally impossible under this reading. Their economic well-being is held hostage by political theater.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, us_public, payer,
    powerless, immediate, trapped, national).

% Observe the political debate and react to perceived default risk, but generally assume the US will honor its debts due to the 14th Amendment. Their confidence is crucial for US borrowing costs, but they are not directly constrained by the ceiling itself under this reading.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, bond_markets_investors, observer,
    powerful, immediate, arbitrage, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Under this reading, the statutory debt ceiling performs no legitimate coordination function, as it is constitutionally void. The actual coordination of fiscal policy occurs through appropriations and the Treasury's duty to pay.
% TRANSFER_FUNCTION: It transfers political leverage and media attention to the congressional minority party by creating a high-stakes political crisis, but does not legitimately transfer fiscal resources.
% ABSENT_VOICES: Constitutional scholars and legal experts who consistently argue for the 14th Amendment's nullifying effect are often marginalized in the political debate, which focuses on the statutory limit as if it were binding.
% DISAPPEARANCE_RATIONALE: If the statutory debt ceiling and its associated political theater vanished overnight, the political landscape would significantly rearrange. The minority party would lose a key leverage point, forcing new modes of fiscal negotiation. The Executive Branch would no longer face this particular political challenge, and the US public would be spared the uncertainty of default threats.
% FOUNDING_PROBLEM: The debt ceiling was originally created in 1917 to simplify congressional authorization of borrowing, allowing the Treasury to issue bonds as needed without specific approval for each issuance, within an aggregate limit. It was not intended as a tool to limit spending already authorized by Congress.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and constitutional scholars widely corroborate the original intent of the debt ceiling as a procedural mechanism, not a spending limit. They also attest to the 14th Amendment's clear language regarding the inviolability of the public debt, which, under this reading, renders the debt ceiling constitutionally inoperative as a limit on payment.
narrative_ontology:disappearance_verdict(statutory_debt_ceiling__constitutional_nullity_reading, world_rearranges).
narrative_ontology:founding_problem_status(statutory_debt_ceiling__constitutional_nullity_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statutory_debt_ceiling__constitutional_nullity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(statutory_debt_ceiling__constitutional_nullity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(statutory_debt_ceiling__constitutional_nullity_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(statutory_debt_ceiling__constitutional_nullity_reading_tests).
:- end_tests(statutory_debt_ceiling__constitutional_nullity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness (0.15) and suppression (0.10) reflect the core premise that the debt ceiling is constitutionally void and cannot legally compel or extract. The high and rising theater ratio (0.85 at end) captures the increasing performative nature of the debt ceiling debate, where political leverage is sought through brinkmanship rather than actual legislative power over debt payment. Accessibility collapse is low (0.20) because the legal alternative (Treasury continuing to borrow) is available, though politically fraught. Resistance is moderate (0.30) from those asserting the 14th Amendment, but not from a broad public resisting the 'constraint' itself.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Executive Branch and constitutional scholars, the debt ceiling is a legal nullity, and the debate around it is a political distraction. From the perspective of the congressional minority, it is a valuable tool for political leverage. The engine's classification as Piton captures the structural reality of a constraint whose legal function has atrophied but whose political performance persists.
 *
 * DIRECTIONALITY LOGIC:
 *   The US Treasury and President/Executive Branch are agenda-setters, legally bound to pay debts and asserting the 14th Amendment's supremacy. The US public and the congressional majority party are payers, bearing the costs of uncertainty and political gridlock. The congressional minority party is a beneficiary of the political theater, gaining leverage without the ability to legally force default. The Supreme Court and bond markets are observers, reacting to the political environment but not directly constrained by the ceiling's legal force under this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutional_validity_ambiguity,
    'Is the statutory debt ceiling truly constitutionally void under the 14th Amendment, Section 4, or does it retain some legal force as a valid exercise of congressional power?',
    'A definitive ruling by the US Supreme Court on the constitutionality of the debt ceiling, or a constitutional amendment clarifying congressional fiscal powers.',
    'If ruled constitutionally valid, the constraint''s extractiveness and suppression would be significantly higher, reclassifying it towards a Snare or Tangled Rope. If definitively ruled void, its Piton classification would be solidified, and its theater ratio might eventually decline as political actors adapt.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutional_validity_ambiguity, conceptual, 'Ambiguity regarding the debt ceiling''s constitutional standing.').

omega_variable(
    political_theater_efficacy,
    'To what extent does the political theater surrounding the debt ceiling genuinely influence fiscal policy outcomes, beyond merely providing political leverage?',
    'Empirical analysis of legislative outcomes during and after debt ceiling debates, comparing policy shifts to periods without such crises.',
    'If the theater consistently leads to substantive policy changes (e.g., spending cuts), it suggests a higher, albeit indirect, form of extraction or suppression, potentially shifting the classification towards a Tangled Rope. If it primarily yields political posturing, the Piton classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_theater_efficacy, empirical, 'The actual impact of debt ceiling political crises on fiscal policy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statutory_debt_ceiling__constitutional_nullity_reading, 1917, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1917, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 1917, 0.1).
narrative_ontology:measurement(stat_tr_t1940, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 1940, 0.2).
narrative_ontology:measurement(stat_tr_t1970, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 1970, 0.4).
narrative_ontology:measurement(stat_tr_t1990, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 1990, 0.6).
narrative_ontology:measurement(stat_tr_t2010, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 2010, 0.75).
narrative_ontology:measurement(stat_tr_t2024, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 2024, 0.85).

% Extraction over time
narrative_ontology:measurement(stat_be_t1917, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 1917, 0.05).
narrative_ontology:measurement(stat_be_t1940, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 1940, 0.08).
narrative_ontology:measurement(stat_be_t1970, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 1970, 0.1).
narrative_ontology:measurement(stat_be_t1990, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 1990, 0.12).
narrative_ontology:measurement(stat_be_t2010, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 2010, 0.14).
narrative_ontology:measurement(stat_be_t2024, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1917, statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 1917, 0.05).
narrative_ontology:measurement(stat_su_t1940, statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 1940, 0.06).
narrative_ontology:measurement(stat_su_t1970, statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 1970, 0.07).
narrative_ontology:measurement(stat_su_t1990, statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 1990, 0.08).
narrative_ontology:measurement(stat_su_t2010, statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 2010, 0.09).
narrative_ontology:measurement(stat_su_t2024, statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 2024, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
