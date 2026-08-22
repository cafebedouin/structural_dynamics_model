% ============================================================================
% CONSTRAINT STORY: dollar_gold_convertibility__strict_convertibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dollar_gold_convertibility__strict_convertibility_reading, []).

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
 *   constraint_id: dollar_gold_convertibility__strict_convertibility_reading
 *   human_readable: Article IV Dollar-Gold Convertibility as Strict Legal Obligation
 *   domain: international_political_economy/monetary_history
 *
 * SUMMARY:
 *   This constraint instantiates the strict_convertibility_reading of the
 *   dollar_gold_convertibility kernel. Under this reading, Article IV of the
 *   IMF Articles imposes an unconditional, binding legal obligation on the
 *   United States to maintain dollar-gold convertibility at a fixed parity,
 *   regardless of domestic economic conditions. The U.S. monetary authorities
 *   enter the victim set as a constrained issuer, surplus creditor nations
 *   enter the beneficiary set with enforceable redemption claims, and the
 *   resulting extraction from U.S. domestic policy space is high. Sibling
 *   readings include policy_flexible_reading (conditional obligation
 *   subordinate to domestic stability) and triffin_structural_reading
 *   (inherently unsustainable design flaw requiring systemic revision).
 *
 * KEY AGENTS:
 *   - us_treasury_federal_reserve: Primary target (institutional/constrained) â bears the extraction of foregone monetary autonomy
 *   - surplus_creditor_nations: Primary beneficiary (organized/mobile) â holds enforceable gold-redemption claims and confidence benefits
 *   - imf_secretariat: Agenda setter (institutional/analytical) â administers the legal framework that binds the issuer
 *   - domestic_full_employment_advocates: Excluded voice (moderate/constrained) â structurally absent from the international monetary negotiations
 *   - international_monetary_scholars: Analytical observer (analytical/analytical) â documents the divergence between legal text and practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dollar_gold_convertibility__strict_convertibility_reading, 0.78).
domain_priors:suppression_score(dollar_gold_convertibility__strict_convertibility_reading, 0.68).
domain_priors:theater_ratio(dollar_gold_convertibility__strict_convertibility_reading, 0.33).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 0.33).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dollar_gold_convertibility__strict_convertibility_reading, tangled_rope).
narrative_ontology:human_readable(dollar_gold_convertibility__strict_convertibility_reading, "Article IV Dollar-Gold Convertibility as Strict Legal Obligation").
narrative_ontology:topic_domain(dollar_gold_convertibility__strict_convertibility_reading, "international_political_economy/monetary_history").

domain_priors:requires_active_enforcement(dollar_gold_convertibility__strict_convertibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dollar_gold_convertibility__strict_convertibility_reading, 'e29c6ecb-f3af-4cfd-9cca-dc78bcc0fabb').
narrative_ontology:cs_kernel_codification('e29c6ecb-f3af-4cfd-9cca-dc78bcc0fabb', formalized).
narrative_ontology:cs_authority_grounding('e29c6ecb-f3af-4cfd-9cca-dc78bcc0fabb', lineage).
narrative_ontology:cs_interpretation_layer_present('e29c6ecb-f3af-4cfd-9cca-dc78bcc0fabb').
narrative_ontology:cs_reading_relation('e29c6ecb-f3af-4cfd-9cca-dc78bcc0fabb', dollar_gold_convertibility__policy_flexible_reading, forecloses).
narrative_ontology:cs_reading_relation('e29c6ecb-f3af-4cfd-9cca-dc78bcc0fabb', dollar_gold_convertibility__triffin_structural_reading, coexists_with).
narrative_ontology:cs_axiom('e29c6ecb-f3af-4cfd-9cca-dc78bcc0fabb', foundational, article_iv_unconditional_obligation).
narrative_ontology:cs_axiom_status(article_iv_unconditional_obligation, holdable).
narrative_ontology:cs_axiom_grounding('e29c6ecb-f3af-4cfd-9cca-dc78bcc0fabb', article_iv_unconditional_obligation, conventional).
narrative_ontology:cs_axiom('e29c6ecb-f3af-4cfd-9cca-dc78bcc0fabb', secondary, domestic_policy_subordination).
narrative_ontology:cs_axiom_status(domestic_policy_subordination, holdable).
narrative_ontology:cs_axiom_grounding('e29c6ecb-f3af-4cfd-9cca-dc78bcc0fabb', domestic_policy_subordination, conventional).
narrative_ontology:cs_reference_frame('e29c6ecb-f3af-4cfd-9cca-dc78bcc0fabb', bretton_woods_legal_fixity).
narrative_ontology:cs_drift_state('e29c6ecb-f3af-4cfd-9cca-dc78bcc0fabb', post_1971_gold_window_closure, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('e29c6ecb-f3af-4cfd-9cca-dc78bcc0fabb', '').
narrative_ontology:cs_kernel_id(dollar_gold_convertibility__strict_convertibility_reading, dollar_gold_convertibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__strict_convertibility_reading, surplus_creditor_nations).
narrative_ontology:constraint_victim(dollar_gold_convertibility__strict_convertibility_reading, us_treasury_federal_reserve).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues the world's primary reserve currency and is legally bound to redeem dollars for gold at a fixed parity under Article IV. Must defend the parity through contractionary monetary policy and interest-rate hikes even when domestic conditions demand expansion, effectively subordinating full-employment and growth objectives to an external legal commitment.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, us_treasury_federal_reserve, payer,
    institutional, generational, constrained, global).

% Accumulate dollar reserves through trade surpluses and hold the legal right under the Articles to convert dollar balances into gold at the fixed parity. Benefit from the confidence and stability of the dollar anchor and from the discipline the constraint imposes on U.S. monetary policy, which protects the real value of their holdings and gives them enforceable redemption claims against the issuer.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, surplus_creditor_nations, beneficiary,
    organized, generational, mobile, global).

% Administers the Articles of Agreement, monitors exchange-rate compliance, and provides the institutional machinery through which convertibility obligations are defined and supervised. Maintains the legal framework that binds the issuer and empowers creditors, deriving institutional authority from the treaty's continuity.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, imf_secretariat, agenda_setter,
    institutional, generational, analytical, global).

% Argue for monetary expansion, lower interest rates, and fiscal stimulus to reduce domestic unemployment. Their policy preferences are structurally precluded by the external convertibility commitment, which is treated as a supra-legislative legal obligation immune to ordinary democratic fiscal preference. They are not seated in the international monetary negotiations where the constraint is enforced.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, domestic_full_employment_advocates, excluded,
    moderate, biographical, constrained, national).

% Analyze the legal and economic architecture of Bretton Woods. They document the divergence between the strict legal text and actual monetary practice, and assess the sustainability of maintaining convertibility against domestic policy needs.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, international_monetary_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a fixed exchange-rate anchor and gold-convertibility guarantee that facilitates international trade and investment by eliminating currency uncertainty and establishing a credible nominal anchor in the post-war period.
% TRANSFER_FUNCTION: Moves monetary-policy autonomy from the U.S. issuer to the external constraint, transferring the costs of adjustmentârecessionary interest rates, deflationary pressure, and foregone domestic stimulusâto the U.S. while transferring confidence benefits, price stability, and enforceable gold-redemption rights to creditor nations holding dollar reserves.
% ABSENT_VOICES: Domestic full-employment advocates and U.S. Keynesian planners who would prioritize unemployment reduction and domestic growth over external balance; structurally excluded because the legal obligation is framed as supra-legislative and not subject to ordinary democratic fiscal preference.
% DISAPPEARANCE_RATIONALE: If the strict convertibility obligation vanished overnight, the U.S. could adjust interest rates and money supply purely to domestic conditions, the Bretton Woods fixed-rate architecture would collapse, creditor nations would lose gold-redemption rights, and international monetary arrangements would reorganize around floating or alternative anchors.
% FOUNDING_PROBLEM: Post-WWII monetary chaos and competitive devaluations of the interwar period; the need for a credible nominal anchor to restore trade and investment confidence.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians and international-relations scholars outside the benefiting creditor nations attest that the interwar monetary problem was substantially resolved by the early 1960s, while U.S. Treasury and Federal Reserve internal documents from the 1960s acknowledge the growing divergence between the legal commitment and sustainable domestic policy.
narrative_ontology:disappearance_verdict(dollar_gold_convertibility__strict_convertibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(dollar_gold_convertibility__strict_convertibility_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dollar_gold_convertibility__strict_convertibility_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dollar_gold_convertibility__strict_convertibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dollar_gold_convertibility__strict_convertibility_reading, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dollar_gold_convertibility__strict_convertibility_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dollar_gold_convertibility__strict_convertibility_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dollar_gold_convertibility__strict_convertibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78 at interval end) because the strict legal reading subordinates the entire range of U.S. domestic monetary toolsâinterest rates, money supply, lender-of-last-resort scaleâto the external parity commitment, extracting policy autonomy regardless of domestic unemployment or growth needs. Suppression is substantial (0.68) because the legal framework and creditor-enforcement threat actively suppress the alternative of floating or domestically targeted monetary policy. Theater is moderate (0.33): the commitment was initially functional, but by the late 1960s an increasing share of U.S. activity was performative maintenance of a parity everyone knew was under fundamental stress (gold-pool operations, confidence-building communiques). Accessibility collapse is high (0.72) because, once the Articles were accepted as binding international law, unilateral exit appeared legally and politically blocked until the 1971 rupture. Resistance is moderate (0.52): internal U.S. resistance grew across the interval, culminating in the Nixon shock, but was held in check for decades by legal and alliance considerations.
 *
 * PERSPECTIVAL GAP:
 *   The U.S. payer seat and the creditor beneficiary seat should compute to markedly different types: from the issuer's position the arrangement is externally imposed extraction that forces recessionary policy during downturns, while from the creditor position it is beneficial coordination that secures the value of reserves and imposes desirable discipline. The IMF agenda-setter seat sees legal maintenance. The engine computes this divergence from beneficiary/victim declarations and exit-option asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   The us_treasury_federal_reserve is a structural target: it bears the costs of the constraint, has constrained exit because unilateral abrogation would rupture the monetary system and alliance structure, and therefore derives a high directionality value leading to high effective extraction. Surplus_creditor_nations are structural beneficiaries: they collect confidence and redemption rights, enjoy mobile exit options (can diversify or demand gold), and derive a low directionality value that dampens or inverts effective extraction. The IMF secretariat sits near symmetric: it does not collect the extraction but administers the framework. Domestic advocates are excluded entirely, their absence part of the suppression structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint was built to solve genuine coordination failureâinterwar monetary chaosâand initially functioned as a scaffold or rope. Over the post-war decades the founding problem died (the chaos was solved), while the U.S. role as sole liquidity provider and the Triffin dilution of gold backing transformed the arrangement. The strict reading preserves the legal form after the coordination function has atrophied, producing mandatrophy: a dead founding problem sustained by legal inertia and creditor-country interest, with extraction rising as the functional rationale faded. The high theater_ratio near interval end signals this decay.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legal_obligation_enforceability,
    'Does Article IV convertibility under the IMF Articles create a judicially enforceable obligation against the U.S., or merely a framework standard subject to sovereign discretion?',
    'International tribunal ruling or definitive IMF General Counsel interpretation on the binding force of Article IV, Section 1 versus Section 4 provisions.',
    'If unenforceable, the constraint''s suppression and extraction are lower than authored, trending toward rope; if enforceable, the strict reading''s extraction profile holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legal_obligation_enforceability, conceptual, 'Ambiguity over whether Article IV is a hard legal obligation or soft coordination standard.').

omega_variable(
    adjustment_burden_distribution,
    'Is the extraction borne by the U.S. issuer structurally necessary for the system''s coordination function, or does it reflect an avoidable misallocation of adjustment costs?',
    'Comparative analysis of alternative reserve-currency architectures to determine if the same coordination could obtain with symmetric adjustment obligations.',
    'If necessary, the extraction is the price of coordination (tangled_rope sustained); if avoidable, the constraint approaches snare in its asymmetric phase.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adjustment_burden_distribution, empirical, 'Whether U.S. adjustment burden is structurally necessary for system stability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dollar_gold_convertibility__strict_convertibility_reading, 0, 27).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dgc_strict_tr_t0, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(dgc_strict_tr_t7, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 7, 0.13).
narrative_ontology:measurement(dgc_strict_tr_t14, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 14, 0.19).
narrative_ontology:measurement(dgc_strict_tr_t21, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 21, 0.26).
narrative_ontology:measurement(dgc_strict_tr_t27, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 27, 0.33).

% Extraction over time
narrative_ontology:measurement(dgc_strict_be_t0, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(dgc_strict_be_t7, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 7, 0.46).
narrative_ontology:measurement(dgc_strict_be_t14, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 14, 0.57).
narrative_ontology:measurement(dgc_strict_be_t21, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 21, 0.69).
narrative_ontology:measurement(dgc_strict_be_t27, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 27, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(dgc_strict_su_t0, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(dgc_strict_su_t7, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 7, 0.51).
narrative_ontology:measurement(dgc_strict_su_t14, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 14, 0.62).
narrative_ontology:measurement(dgc_strict_su_t21, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 21, 0.73).
narrative_ontology:measurement(dgc_strict_su_t27, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 27, 0.81).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dollar_gold_convertibility__strict_convertibility_reading, global_infrastructure).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
