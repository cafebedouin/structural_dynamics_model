% ============================================================================
% CONSTRAINT STORY: domestic_currency_vs_external_settlement_seam
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_domestic_currency_vs_external_settlement_seam, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: domestic_currency_vs_external_settlement_seam
 *   human_readable: Domestic Issuance / External Settlement Architectural Seam
 *   domain: constitutional political economy / monetary theory
 *
 * SUMMARY:
 *   The constitutional design at issue claims that domestic monetary
 *   sovereignty is preserved precisely by architecturally disclaiming any
 *   promise about the currency's external value — the Monetary Organ governs
 *   issuance quantity by internal deliberative criteria, while the State
 *   Organ separately manages external legitimacy, and no institution is
 *   instructed to defend an exchange rate with domestic monetary tools. This
 *   is one reading (issuance_as_deliberative_judgment) of the shared kernel
 *   question 'how does a proposed future acquire present purchasing power' —
 *   sibling readings (endogenous credit multiplication, physical backing,
 *   market-discovered confidence) would each classify this same architectural
 *   seam differently, and in particular the catallactic reading would treat
 *   the seam's claimed insulation as itself illusory, since on that reading
 *   the exchange rate signal IS the legitimating mechanism the domestic organ
 *   cannot actually escape. This story evaluates the seam as read from the
 *   deliberative-judgment kernel commitment: the question is whether, under
 *   real external stress, the architecturally-promised insulation holds, or
 *   whether the external seam forces convergence toward market-discovered
 *   confidence regardless of the domestic law's formal commitment.
 *
 * KEY AGENTS:
 *   - domestic_monetary_organ: primary agenda-setter and beneficiary of insulation, institutional/analytical exit
 *   - export_competitive_producers: beneficiary of float, organized/mobile
 *   - import_dependent_consumers: primary payer, powerless/trapped
 *   - foreign_currency_debtors: payer, moderate/constrained
 *   - external_creditors_holding_domestic_claims: excluded, powerful/mobile — structurally outside domestic process
 *   - state_organ_external_relations: secondary agenda-setter, where convergence pressure first registers
 *   - constitutional_drafters_and_courts: analytical observer assessing whether the seam holds under stress
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(domestic_currency_vs_external_settlement_seam, 0.28).
domain_priors:suppression_score(domestic_currency_vs_external_settlement_seam, 0.22).
domain_priors:theater_ratio(domestic_currency_vs_external_settlement_seam, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(domestic_currency_vs_external_settlement_seam, extractiveness, 0.28).
narrative_ontology:constraint_metric(domestic_currency_vs_external_settlement_seam, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(domestic_currency_vs_external_settlement_seam, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(domestic_currency_vs_external_settlement_seam, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(domestic_currency_vs_external_settlement_seam, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(domestic_currency_vs_external_settlement_seam, rope).
narrative_ontology:human_readable(domestic_currency_vs_external_settlement_seam, "Domestic Issuance / External Settlement Architectural Seam").
narrative_ontology:topic_domain(domestic_currency_vs_external_settlement_seam, "constitutional political economy / monetary theory").

domain_priors:requires_active_enforcement(domestic_currency_vs_external_settlement_seam).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(domestic_currency_vs_external_settlement_seam, 'fa03db43-3861-4bd8-b22b-e38a0e70cec2').
narrative_ontology:cs_kernel_codification('fa03db43-3861-4bd8-b22b-e38a0e70cec2', formalized).
narrative_ontology:cs_authority_grounding('fa03db43-3861-4bd8-b22b-e38a0e70cec2', distributed).
narrative_ontology:cs_reading_relation('fa03db43-3861-4bd8-b22b-e38a0e70cec2', domestic_currency_vs_external_settlement_seam__issuance_as_endogenous_credit_multiplication, coexists_with).
narrative_ontology:cs_reading_relation('fa03db43-3861-4bd8-b22b-e38a0e70cec2', domestic_currency_vs_external_settlement_seam__issuance_as_physical_backing, coexists_with).
narrative_ontology:cs_reading_relation('fa03db43-3861-4bd8-b22b-e38a0e70cec2', domestic_currency_vs_external_settlement_seam__issuance_as_market_discovered_confidence, forecloses).
narrative_ontology:cs_axiom('fa03db43-3861-4bd8-b22b-e38a0e70cec2', foundational, issuance_legitimacy_requires_constituted_visible_judgment).
narrative_ontology:cs_axiom_status(issuance_legitimacy_requires_constituted_visible_judgment, holdable).
narrative_ontology:cs_axiom_grounding('fa03db43-3861-4bd8-b22b-e38a0e70cec2', issuance_legitimacy_requires_constituted_visible_judgment, conventional).
narrative_ontology:cs_axiom('fa03db43-3861-4bd8-b22b-e38a0e70cec2', secondary, external_value_disclaimer_is_necessary_for_domestic_sovereignty).
narrative_ontology:cs_axiom_status(external_value_disclaimer_is_necessary_for_domestic_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('fa03db43-3861-4bd8-b22b-e38a0e70cec2', external_value_disclaimer_is_necessary_for_domestic_sovereignty, instrumental).
narrative_ontology:cs_reference_frame('fa03db43-3861-4bd8-b22b-e38a0e70cec2', constitutional_deliberative_insulation).
narrative_ontology:cs_drift_state('fa03db43-3861-4bd8-b22b-e38a0e70cec2', post_external_shock_stress_episode, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fa03db43-3861-4bd8-b22b-e38a0e70cec2', '').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(domestic_currency_vs_external_settlement_seam, domestic_monetary_organ).
narrative_ontology:constraint_beneficiary(domestic_currency_vs_external_settlement_seam, export_competitive_producers).
narrative_ontology:constraint_beneficiary(domestic_currency_vs_external_settlement_seam, sovereign_debt_holders_in_domestic_currency).
narrative_ontology:constraint_victim(domestic_currency_vs_external_settlement_seam, import_dependent_consumers).
narrative_ontology:constraint_victim(domestic_currency_vs_external_settlement_seam, foreign_currency_debtors).
narrative_ontology:constraint_victim(domestic_currency_vs_external_settlement_seam, external_creditors_holding_domestic_claims).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(domestic_currency_vs_external_settlement_seam, sovereign_debt_holders_in_domestic_currency).
narrative_ontology:constraint_vindicates(domestic_currency_vs_external_settlement_seam, monetary_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(domestic_currency_vs_external_settlement_seam, flexible_exchange_rate_insulation_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers domestic currency issuance and is constitutionally instructed to treat external market value as a separate, disclaimed variable. It sets policy rates and quantity decisions by reference to domestic deliberative judgment, and the seam is precisely the doctrine that shields its votes from an obligation to defend any particular exchange rate.
narrative_ontology:constraint_stakeholder(domestic_currency_vs_external_settlement_seam, domestic_monetary_organ, agenda_setter,
    institutional, generational, analytical, national).

% Benefit when currency depreciation is treated as a market signal rather than a policy failure requiring defense; the seam lets the currency float against external pressure, improving their relative competitiveness without requiring the domestic organ to tighten money to defend a peg.
narrative_ontology:constraint_stakeholder(domestic_currency_vs_external_settlement_seam, export_competitive_producers, beneficiary,
    organized, biographical, mobile, global).

% Hold claims denominated in the currency the organ controls; they benefit from the guarantee that domestic obligations will not be conscripted to defend external convertibility, but bear the cost when depreciation erodes real returns as the seam's mechanism absorbs external shocks through the exchange rate rather than through domestic tightening.
narrative_ontology:constraint_stakeholder(domestic_currency_vs_external_settlement_seam, sovereign_debt_holders_in_domestic_currency, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(domestic_currency_vs_external_settlement_seam, sovereign_debt_holders_in_domestic_currency, payer).

% Pay directly through imported-goods inflation whenever the currency depreciates under external pressure. They have no meaningful exit from the domestic currency and no voice in the organ's decision to let the exchange rate, rather than domestic policy, absorb the external shock.
narrative_ontology:constraint_stakeholder(domestic_currency_vs_external_settlement_seam, import_dependent_consumers, payer,
    powerless, immediate, trapped, national).

% Borrowed or owe in foreign currency (directly, or through firms exposed to foreign inputs) and are hurt precisely because the seam disclaims any promise to hold the exchange rate stable; a depreciation that the doctrine treats as a benign external adjustment is, from this seat, a balance-sheet shock with no institutional recourse.
narrative_ontology:constraint_stakeholder(domestic_currency_vs_external_settlement_seam, foreign_currency_debtors, payer,
    moderate, biographical, constrained, national).

% Hold claims on the domestic economy denominated in the domestic currency and would prefer the organ prioritize external confidence over domestic deliberative judgment. They are structurally outside the domestic constitutional process that governs the Monetary Organ's votes, and their preferred remedy — defend the peg, tighten to preserve external value — is exactly what the seam is built to refuse them standing to demand.
narrative_ontology:constraint_stakeholder(domestic_currency_vs_external_settlement_seam, external_creditors_holding_domestic_claims, excluded,
    powerful, biographical, mobile, global).

% Negotiates external legitimacy, trade terms, and reserve arrangements; it is the seam's other face, absorbing diplomatic and market pressure to converge toward externally-legible signals (yield spreads, exchange rate stability) while the Monetary Organ holds the domestic issuance line. Under acute stress, this seat is where convergence pressure first registers before it can reach the Monetary Organ.
narrative_ontology:constraint_stakeholder(domestic_currency_vs_external_settlement_seam, state_organ_external_relations, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(domestic_currency_vs_external_settlement_seam, state_organ_external_relations, observer).

% Interpret whether the architectural separation of issuance and settlement has actually been maintained under stress, or whether crisis-era emergency measures have quietly re-coupled the two layers, collapsing the seam's promised insulation.
narrative_ontology:constraint_stakeholder(domestic_currency_vs_external_settlement_seam, constitutional_drafters_and_courts, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(domestic_currency_vs_external_settlement_seam, diffuse).
narrative_ontology:fixing_cost_class(domestic_currency_vs_external_settlement_seam, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Separates two genuinely distinct problems — how much domestic currency to issue (a question of internal political judgment) and what that currency is worth to foreigners (a question no domestic institution can unilaterally answer) — so that stress on one does not automatically force capitulation on the other. This lets the Monetary Organ make issuance decisions on domestic criteria without being conscripted into defending an exchange rate with tools meant for domestic stabilization.
% TRANSFER_FUNCTION: Under normal operation, moves adjustment costs from the domestic issuance mechanism (which stays insulated) to the exchange rate and to agents holding foreign-currency exposure or import dependence — from those with mobile capital and diversified exposure to those trapped in the domestic currency, whenever external imbalance forces depreciation instead of defense.
% ABSENT_VOICES: Import-dependent consumers bear the seam's transfer cost most directly but have no seat in the Monetary Organ's deliberations, which are structured around domestic political representation, not exposure to external price pass-through. External creditors are formally excluded from domestic constitutional process even though the seam's refusal to defend the currency's value is a direct answer to their preferred remedy.
% DISAPPEARANCE_RATIONALE: If the seam were abolished and issuance were formally re-coupled to an external peg or convertibility promise, the Monetary Organ's votes would become subordinate to defending reserves and interest-rate parity — a genuine architectural rearrangement for domestic policymaking. But market participants dispute whether the seam meaningfully insulates issuance in practice at all: during acute external stress, some observers hold that the same defensive tightening reappears under a different name (interest-rate response to depreciation, capital controls), meaning the seam's disappearance would formalize what already happens informally rather than change outcomes.
% FOUNDING_PROBLEM: Historically, fixed exchange-rate and gold/reserve-convertibility commitments repeatedly forced domestic monetary authorities to tighten credit and induce recessions purely to defend an external price, even when domestic conditions called for the opposite — the seam was built so a republic could set its own monetary quantity without an external commitment vetoing that judgment.
% FOUNDING_PROBLEM_CORROBORATION: Central bank officials and monetary-sovereignty scholars attest the founding problem remains live, citing episodes where fixed-rate regimes forced pro-cyclical tightening. Foreign-currency debtors, IMF program economists, and some external creditors attest that under sufficient stress the seam does not hold — that capital-flight dynamics and balance-sheet effects re-import the very convergence pressure the architecture was meant to exclude, so the disclaimed control is reasserted informally through crisis-response tightening rather than being genuinely absent.
narrative_ontology:disappearance_verdict(domestic_currency_vs_external_settlement_seam, contested).
narrative_ontology:founding_problem_status(domestic_currency_vs_external_settlement_seam, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(domestic_currency_vs_external_settlement_seam, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-08',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(domestic_currency_vs_external_settlement_seam, 'none', 1).
narrative_ontology:epsilon_provenance(domestic_currency_vs_external_settlement_seam, 0.28, 'claude-sonnet-5', 'c2_monetary_architecture_2026_20260808_170220', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(domestic_currency_vs_external_settlement_seam_tests).
:- end_tests(domestic_currency_vs_external_settlement_seam_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-low at baseline (0.18) because under ordinary conditions the seam functions as advertised — a genuine coordination mechanism letting domestic policy proceed on domestic criteria. It rises under stress (peaking near 0.29-0.31 around t=24, modeling an acute external-shock episode) because the theoretical insulation is imperfect: import-dependent consumers and foreign-currency debtors absorb concentrated costs precisely when the seam is tested, and the state organ faces real pressure to re-couple issuance to external defense. Theater ratio also spikes at the same point (0.40) — during crisis, institutions perform 'the seam is holding, this is just healthy market adjustment' even as informal convergence toward external-confidence criteria (capital controls, emergency rate hikes explicitly justified by currency stability) creeps in. Suppression tracks the same crisis dynamic: it is low in calm periods (formal insulation requires little active enforcement) and rises when the organ must actively resist domestic and external pressure to defend the currency using tools reserved for domestic policy. All three metrics share one time grid across six points.
 *
 * PERSPECTIVAL GAP:
 *   From the Monetary Organ's seat, the seam is a working constitutional achievement — proof that domestic judgment need not answer to external market discipline. From the import-dependent consumer's seat, the same seam is the mechanism by which external shocks are converted into domestic price shocks with no compensating voice in the decision. The engine should compute these as structurally different experiences of one architecture, not as disagreement about a single shared fact.
 *
 * DIRECTIONALITY LOGIC:
 *   The Monetary Organ and export-competitive producers sit near the beneficiary end: the former retains policy autonomy, the latter gains competitiveness whenever the currency floats down under pressure. Import-dependent consumers and foreign-currency debtors sit near the target end: trapped or constrained exit, no representation in the issuance decision, and the ones who absorb the adjustment the seam declines to distribute elsewhere. External creditors are excluded rather than coordinated — their preferred remedy (peg defense) is exactly what the seam denies them standing to demand, which is why they sit at high power but with no formal channel into the domestic process.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — fixed-rate regimes forcing pro-cyclical domestic tightening — remains partly live (many economies still face this dynamic) but the corroboration is genuinely split: central-bank voices treat the seam as still solving a real problem, while crisis-economics literature documents that under sufficient external stress the same defensive tightening reappears informally. This is not a simple mandatrophy case (the function has not simply died while the form persists) — it is a live, contested question about whether the architecture's insulation claim survives contact with acute stress, which is exactly the observable this story is built to track rather than resolve by fiat.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    seam_insulation_under_stress,
    'Does the architectural separation between domestic issuance and external settlement actually hold when a persistent current-account imbalance or acute external shock hits, or does convergence pressure force the domestic organ toward market-discovered-confidence criteria regardless of the formal doctrine?',
    'Case-study comparison of monetary-organ decision records during acute balance-of-payments crises: track whether policy rate changes are justified by domestic criteria (inflation, output gap) or explicitly by exchange-rate defense language, and whether emergency capital controls are invoked.',
    'If the seam holds under stress, the constraint functions closer to a genuine rope — real coordination benefit with low extraction. If it systematically collapses under stress into de facto peg defense, the constraint is better read as a tangled_rope or even a scaffold whose insulation promise is largely performative under the conditions it was designed for.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(seam_insulation_under_stress, empirical, 'Whether the domestic/external architectural separation survives real external stress or collapses into informal re-coupling.').

omega_variable(
    kernel_reading_indeterminacy,
    'Is the seam''s claimed insulation a genuine structural achievement under the deliberative-judgment reading, or is it a doctrinal artifact that the market-discovered-confidence reading would show was never operative — i.e., is the exchange-rate signal always the real legitimating test, with the domestic organ''s formal disclaimer merely obscuring that fact?',
    'Compare episodes where the Monetary Organ''s vote diverged sharply from what exchange-rate-implied policy would prescribe, and track whether those divergences persisted or were reversed under market pressure within a defined window.',
    'Persistent divergence without reversal supports the deliberative reading''s claim that the seam genuinely insulates; systematic reversal toward market-implied policy supports the catallactic reading''s claim that price discovery is the real, unavoidable legitimating mechanism and the seam''s insulation claim is cover.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Whether the deliberative-judgment reading''s core claim about the seam survives scrutiny from the sibling market-discovered-confidence reading.').

omega_variable(
    beneficiary_naturalness_ambiguity,
    'Is the seam''s benefit to export-competitive producers and domestic-currency debt holders an incidental byproduct of a genuinely necessary constitutional separation, or is the separation itself partly shaped by these beneficiaries'' influence over how the doctrine was constitutionally ratified?',
    'Examine the constitutional drafting history and lobbying record around the Monetary Organ''s founding charter for evidence of export-sector or debt-holder influence on the specific insulation language adopted.',
    'If the doctrine''s specific form was shaped by these beneficiaries, the rope claim is weaker and the constraint drifts toward tangled_rope even outside acute crisis; if the separation predates and is independent of their influence, the rope reading is better supported.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_naturalness_ambiguity, empirical, 'Whether beneficiary influence shaped the doctrine''s adoption, bearing on the rope-vs-tangled-rope claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(domestic_currency_vs_external_settlement_seam, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dome_tr_t0, domestic_currency_vs_external_settlement_seam, theater_ratio, 0, 0.15).
narrative_ontology:measurement(dome_tr_t8, domestic_currency_vs_external_settlement_seam, theater_ratio, 8, 0.18).
narrative_ontology:measurement(dome_tr_t16, domestic_currency_vs_external_settlement_seam, theater_ratio, 16, 0.22).
narrative_ontology:measurement(dome_tr_t24, domestic_currency_vs_external_settlement_seam, theater_ratio, 24, 0.4).
narrative_ontology:measurement(dome_tr_t32, domestic_currency_vs_external_settlement_seam, theater_ratio, 32, 0.34).
narrative_ontology:measurement(dome_tr_t40, domestic_currency_vs_external_settlement_seam, theater_ratio, 40, 0.3).

% Extraction over time
narrative_ontology:measurement(dome_be_t0, domestic_currency_vs_external_settlement_seam, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(dome_be_t8, domestic_currency_vs_external_settlement_seam, base_extractiveness, 8, 0.2).
narrative_ontology:measurement(dome_be_t16, domestic_currency_vs_external_settlement_seam, base_extractiveness, 16, 0.24).
narrative_ontology:measurement(dome_be_t24, domestic_currency_vs_external_settlement_seam, base_extractiveness, 24, 0.29).
narrative_ontology:measurement(dome_be_t32, domestic_currency_vs_external_settlement_seam, base_extractiveness, 32, 0.26).
narrative_ontology:measurement(dome_be_t40, domestic_currency_vs_external_settlement_seam, base_extractiveness, 40, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(dome_su_t0, domestic_currency_vs_external_settlement_seam, suppression_requirement, 0, 0.12).
narrative_ontology:measurement(dome_su_t8, domestic_currency_vs_external_settlement_seam, suppression_requirement, 8, 0.14).
narrative_ontology:measurement(dome_su_t16, domestic_currency_vs_external_settlement_seam, suppression_requirement, 16, 0.17).
narrative_ontology:measurement(dome_su_t24, domestic_currency_vs_external_settlement_seam, suppression_requirement, 24, 0.31).
narrative_ontology:measurement(dome_su_t32, domestic_currency_vs_external_settlement_seam, suppression_requirement, 32, 0.25).
narrative_ontology:measurement(dome_su_t40, domestic_currency_vs_external_settlement_seam, suppression_requirement, 40, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(domestic_currency_vs_external_settlement_seam, enforcement_mechanism).
narrative_ontology:affects_constraint(domestic_currency_vs_external_settlement_seam, future_claims_present_resources_issuance_as_deliberative_judgment).
narrative_ontology:affects_constraint(domestic_currency_vs_external_settlement_seam, future_claims_present_resources_issuance_as_market_discovered_confidence).

% DUAL FORMULATION NOTE:
% This constraint is downstream of the kernel reading issuance_as_deliberative_judgment: it is the specific external-settlement architecture that reading's commitment requires. It stands in direct tension with issuance_as_market_discovered_confidence, which would treat the seam's claimed insulation as a distortion of (rather than a legitimate alternative to) continuous price discovery. Both sibling kernel-reading constraints, if separately authored, should link back here to document how this seam's stress-behavior functions as empirical evidence in the dispute between the two readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
