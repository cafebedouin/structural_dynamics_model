% ============================================================================
% CONSTRAINT STORY: dollar_gold_convertibility__strict_convertibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
 *   human_readable: Article IV Convertibility as Binding Legal Obligation Constraining U.S. Monetary Policy
 *   domain: international_political_economy/monetary_history/international_law
 *
 * SUMMARY:
 *   The strict convertibility reading treats Article IV of the IMF Articles
 *   of Agreement as a binding legal obligation requiring the United States to
 *   maintain gold convertibility at $35/ounce, constraining Federal Reserve
 *   monetary policy and Treasury fiscal operations to defend the parity.
 *   Creditor nations (France, Germany, Italy, Belgium, Netherlands,
 *   Switzerland) hold enforceable claims on U.S. gold reserves and use IMF
 *   surveillance to discipline U.S. policy. The U.S. enters the victim set as
 *   the constrained issuer: its domestic policy space is extracted to
 *   maintain the international monetary order. The coordination function — a
 *   rule-based system preventing competitive devaluation and providing stable
 *   reserves — is real but asymmetrically costly. The constraint's
 *   enforcement machinery (gold pool, IMF conditionality, swap networks,
 *   market intervention) intensifies over 1945–1971 as U.S. gold coverage
 *   declines. The legal form is not merely declaratory: the 1968 two-tier
 *   gold market and 1971 closure were treated as systemic ruptures, not
 *   routine policy adjustments.
 *
 * KEY AGENTS:
 *   - us_treasury_federal_reserve: Primary victim (institutional/powerful/trapped) — bears extraction of policy autonomy, gold reserves, and fiscal space
 *   - creditor_nation_central_banks: Primary beneficiary (institutional/powerful/arbitrage) — hold enforceable gold claims, gain reserve stability and export competitiveness
 *   - gold_bloc_governments: Secondary beneficiary (institutional/organized/constrained) — coordinate to enforce convertibility, benefit from fixed exchange rates
 *   - international_monetary_fund_surveillance: Agenda setter (institutional/organized/analytical) — administers the legal framework, authorizes parity changes, conditions access
 *   - us_domestic_labor_markets: Secondary victim (class/powerless/trapped) — bear unemployment and wage suppression when monetary policy tightens to defend gold
 *   - us_fiscal_authority: Secondary victim (institutional/powerful/constrained) — fiscal space constrained by balance-of-payments discipline
 *   - academic_monetary_economists: Observer (analytical/analytical/analytical) — debate the constraint's classification, later produce Triffin reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dollar_gold_convertibility__strict_convertibility_reading, 0.78).
domain_priors:suppression_score(dollar_gold_convertibility__strict_convertibility_reading, 0.85).
domain_priors:theater_ratio(dollar_gold_convertibility__strict_convertibility_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dollar_gold_convertibility__strict_convertibility_reading, tangled_rope).
narrative_ontology:human_readable(dollar_gold_convertibility__strict_convertibility_reading, "Article IV Convertibility as Binding Legal Obligation Constraining U.S. Monetary Policy").
narrative_ontology:topic_domain(dollar_gold_convertibility__strict_convertibility_reading, "international_political_economy/monetary_history/international_law").

domain_priors:requires_active_enforcement(dollar_gold_convertibility__strict_convertibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dollar_gold_convertibility__strict_convertibility_reading, 'a0c5a954-2a4a-4f50-8d7d-4a71389cd0bb').
narrative_ontology:cs_kernel_codification('a0c5a954-2a4a-4f50-8d7d-4a71389cd0bb', formalized).
narrative_ontology:cs_authority_grounding('a0c5a954-2a4a-4f50-8d7d-4a71389cd0bb', extraction).
narrative_ontology:cs_interpretation_layer_present('a0c5a954-2a4a-4f50-8d7d-4a71389cd0bb').
narrative_ontology:cs_reading_relation('a0c5a954-2a4a-4f50-8d7d-4a71389cd0bb', dollar_gold_convertibility__policy_flexible_reading, influences).
narrative_ontology:cs_reading_relation('a0c5a954-2a4a-4f50-8d7d-4a71389cd0bb', dollar_gold_convertibility__triffin_structural_reading, coexists_with).
narrative_ontology:cs_axiom('a0c5a954-2a4a-4f50-8d7d-4a71389cd0bb', foundational, article_iv_creates_binding_legal_obligation).
narrative_ontology:cs_axiom_status(article_iv_creates_binding_legal_obligation, holdable).
narrative_ontology:cs_axiom_grounding('a0c5a954-2a4a-4f50-8d7d-4a71389cd0bb', article_iv_creates_binding_legal_obligation, conventional).
narrative_ontology:cs_axiom('a0c5a954-2a4a-4f50-8d7d-4a71389cd0bb', foundational, us_as_constrained_issuer_bears_adjustment_burden).
narrative_ontology:cs_axiom_status(us_as_constrained_issuer_bears_adjustment_burden, holdable).
narrative_ontology:cs_axiom_grounding('a0c5a954-2a4a-4f50-8d7d-4a71389cd0bb', us_as_constrained_issuer_bears_adjustment_burden, empirically_contingent).
narrative_ontology:cs_reference_frame('a0c5a954-2a4a-4f50-8d7d-4a71389cd0bb', bretton_woods_legal_order_1945).
narrative_ontology:cs_drift_state('a0c5a954-2a4a-4f50-8d7d-4a71389cd0bb', nixon_shock_1971, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('a0c5a954-2a4a-4f50-8d7d-4a71389cd0bb', '2026-08-03T14:22:00Z').
narrative_ontology:cs_kernel_id(dollar_gold_convertibility__strict_convertibility_reading, dollar_gold_convertibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__strict_convertibility_reading, creditor_nation_central_banks).
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__strict_convertibility_reading, gold_bloc_governments).
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__strict_convertibility_reading, international_monetary_fund_surveillance).
narrative_ontology:constraint_victim(dollar_gold_convertibility__strict_convertibility_reading, us_treasury_federal_reserve).
narrative_ontology:constraint_victim(dollar_gold_convertibility__strict_convertibility_reading, us_domestic_labor_markets).
narrative_ontology:constraint_victim(dollar_gold_convertibility__strict_convertibility_reading, us_fiscal_authority).
narrative_ontology:constraint_vindicates(dollar_gold_convertibility__strict_convertibility_reading, bretton_woods_legal_order).
narrative_ontology:constraint_vindicates(dollar_gold_convertibility__strict_convertibility_reading, gold_standard_automatic_adjustment).
narrative_ontology:constraint_vindicates(dollar_gold_convertibility__strict_convertibility_reading, creditor_rights_supremacy).
narrative_ontology:constraint_vindicates(dollar_gold_convertibility__strict_convertibility_reading, rule_based_monetary_system).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The U.S. monetary authority bears the primary adjustment burden: must contract money supply, raise interest rates, and limit fiscal deficits to stem gold outflows. The gold window is the only exit mechanism, and closing it (1971) ended the constraint but triggered systemic rupture. No alternative reserve currency existed; the Triffin dilemma made the position structurally inescapable while the system operated.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, us_treasury_federal_reserve, payer,
    powerful, biographical, trapped, global).

% European central banks (especially France, Germany, Italy) accumulated dollar reserves convertible to gold at $35/oz. They could arbitrage by converting dollars to gold (France did aggressively 1965–1968), gaining a risk-free asset while the U.S. bore the adjustment. Their exit option was the gold conversion right itself — a structural arbitrage the constraint granted them.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, creditor_nation_central_banks, beneficiary,
    powerful, biographical, arbitrage, global).

% The European gold bloc coordinated through the Bank for International Settlements and the gold pool to enforce convertibility discipline on the U.S. They benefited from stable exchange rates for their export-led recovery but were constrained by their own dependence on the dollar system — they could not easily exit without devaluing their own reserves.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, gold_bloc_governments, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(dollar_gold_convertibility__strict_convertibility_reading, gold_bloc_governments, agenda_setter).

% The IMF administers Article IV: approves/disapproves parity changes, conducts Article IV consultations, conditions access to Fund resources on policy adjustment. It is the institutional enforcer of the strict reading. Its analytical seat sees the full structure but its operational role is to maintain the legal framework — making it both agenda-setter and observer.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, international_monetary_fund_surveillance, agenda_setter,
    institutional, generational, analytical, global).

% When the Fed tightens to defend gold (1957, 1959, 1966, 1969), unemployment rises and wage growth slows. Workers have no exit from the monetary regime — they cannot convert wages to gold, cannot move to a different currency zone. The extraction from their employment prospects is diffuse but real, and they have no voice in the Article IV governance.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, us_domestic_labor_markets, payer,
    powerless, biographical, trapped, national).

% Congress and the Executive face balance-of-payments constraints on fiscal expansion (Vietnam War + Great Society spending drove 1960s deficits that accelerated gold loss). They have some policy space (tax surcharges, expenditure cuts) but the external constraint narrows domestic choices. Exit would mean abandoning the dollar's reserve role — a geopolitical cost they would not pay voluntarily.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, us_fiscal_authority, payer,
    powerful, biographical, constrained, national).

% Economists (Triffin, Mundell, Johnson, Kindleberger) analyzed the constraint from outside the operational seats. Triffin's 1960 testimony to Congress produced the structural reading that the system was inherently flawed. Their analytical seat has full exit and no stake in the extraction — they classify, they do not bear.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, academic_monetary_economists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a rule-based international monetary system preventing competitive devaluation, supplying stable reserve assets (dollars convertible to gold), and enabling predictable trade and capital flows — the Bretton Woods solution to the interwar chaos.
% TRANSFER_FUNCTION: Moves policy autonomy and gold reserves from the U.S. (issuer) to creditor nations (reserve holders): the U.S. supplies global liquidity by running deficits, creditor nations accumulate convertible claims, and the adjustment burden falls asymmetrically on the issuer via gold outflows and contractionary policy.
% ABSENT_VOICES: Developing nations (Global South) were excluded from the Article IV governance structure — they held few gold reserves, had no voice in IMF quota allocation, and bore the contractionary spillover when the U.S. tightened. The G-77 would later demand SDR allocation and reform, but in 1945–1971 they were structurally absent. Also absent: U.S. Congress (ceded monetary authority to Fed/Treaty), U.S. labor (no representation in international monetary governance).
% DISAPPEARANCE_RATIONALE: If Article IV convertibility vanished overnight (as it effectively did in August 1971), the world monetary system rearranged completely: floating exchange rates, end of gold convertibility, shift to dollar-standard without anchor, eventual petrodollar recycling, and a decade of inflation and volatility before the Volcker disinflation. The constraint's disappearance was the Nixon Shock — a systemic rupture, not a marginal adjustment.
% FOUNDING_PROBLEM: The interwar period (1919–1939) demonstrated that uncoordinated national monetary policies produced competitive devaluation, trade collapse, gold standard rigidity, and contributed to the Great Depression and political extremism. The founders at Bretton Woods (1944) sought a rule-based system with adjustable pegs, IMF surveillance, and gold convertibility to prevent recurrence.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (interwar monetary chaos) is dead — no serious analyst argues the 1970s floating-rate system recreated 1930s chaos. The corroboration comes from outside the beneficiary set: Triffin (1960), Kindleberger (1973), Eichengreen (1992), and the IMF's own historical assessments all attest the original problem was solved but the strict convertibility obligation persisted as extraction. The U.S. Treasury (beneficiary of the flexible reading) and creditor nations (beneficiaries of strict reading) both had incentives to maintain the system past its founding justification.
narrative_ontology:disappearance_verdict(dollar_gold_convertibility__strict_convertibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(dollar_gold_convertibility__strict_convertibility_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dollar_gold_convertibility__strict_convertibility_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(dollar_gold_convertibility__strict_convertibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dollar_gold_convertibility__strict_convertibility_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.78) is high because the constraint extracts U.S. domestic policy autonomy — the Fed cannot pursue full employment independently when gold outflows demand contraction. The Triffin dynamic makes this extraction structural: supplying global reserves requires U.S. deficits, which undermine the very convertibility the constraint demands. Suppression (0.85) is very high: the gold pool (1961–1968), swap lines, IMF Article IV consultations, and market discipline create a multi-layered enforcement apparatus. Theater ratio (0.22) is low-moderate: the coordination function (stable reserves, prevented competitive devaluation) is genuine but erodes over time as the system's own success creates the Triffin contradiction. The measurement series on a shared grid (1945, 1950, 1955, 1958, 1960, 1965, 1968, 1971) show extractiveness rising from 0.35 to 0.78, suppression from 0.45 to 0.85, and theater ratio from 0.10 to 0.22 — a coordination system becoming progressively more extractive.
 *
 * PERSPECTIVAL GAP:
 *   From the creditor-nation seat (France 1965), the constraint is a Rope: genuine coordination preventing U.S. inflationary finance from devaluing their reserves. From the U.S. seat (Treasury 1968), it is a Snare: extraction of policy autonomy for a coordination function that increasingly benefits others. From the IMF staff seat, it is a Tangled Rope: coordination with asymmetric extraction requiring active enforcement. The engine computes this divergence from the structural data — the strict reading's legal formalism makes the asymmetry structurally visible.
 *
 * DIRECTIONALITY LOGIC:
 *   The U.S. Treasury/Fed is the structural victim (d near 1.0): constrained issuer, trapped by the gold standard's mechanics, bears the adjustment burden. Creditor nation central banks are structural beneficiaries (d near 0.0): hold enforceable gold claims, gain stable reserves for their export sectors, and can arbitrage via the gold pool. The IMF surveillance function sits at d ≈ 0.5 (symmetric): it administers the system but its legitimacy depends on even-handed enforcement — which the historical record shows was creditor-biased. U.S. domestic labor markets are identity-locked victims (d ≈ 0.9): no exit from the monetary regime, bear costs through unemployment. U.S. fiscal authority is constrained (d ≈ 0.7): some policy space but balance-of-payments discipline binds.
 *
 * MANDATROPHY ANALYSIS:
 *   The Bretton Woods system was founded to solve the interwar coordination failure (competitive devaluation, trade collapse, gold standard rigidity). By 1960, the founding problem (interwar chaos) was dead — but the arrangement persisted and intensified extraction. The mandatrophy is unresolved: the system's legal form (Article IV) became a vehicle for creditor-nation rent extraction from the issuer. The strict reading captures this by naming the U.S. as victim and creditor nations as beneficiaries. The policy_flexible_reading is the mandatrophy-resolution attempt (subordinate convertibility to domestic stability); the Triffin reading is the structural diagnosis of why the strict reading was unsustainable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint one reading of the dollar_gold_convertibility kernel, and does the strict_convertibility_reading structurally differ from policy_flexible_reading and triffin_structural_reading?',
    'Trace the constraint family: each reading must instantiate a distinct constraint with its own ε, beneficiaries, victims, and classification. The strict reading places the U.S. in the victim set with high extractiveness from domestic policy space; the flexible reading subordinates convertibility to domestic stability; the Triffin reading treats convertibility as a structural flaw. Confirm by cross-referencing sibling constraint_ids in network.affects_constraints.',
    'If the readings are not structurally distinct, the ε-invariance principle is violated — one kernel label would cover multiple constraints. This omega documents the committer-frame structure that must not leak into standard fields.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commitment kernel decomposition: strict vs. flexible vs. Triffin readings as distinct constraints').

omega_variable(
    legal_obligation_vs_policy_space,
    'Does Article IV convertibility operate as a binding legal obligation that extracts from U.S. domestic policy space, or is the legal form a coordination cover for a system that already privileged creditor nations?',
    'Compare the legal text of IMF Articles of Agreement (Article IV, Sections 1–4) against the operational record of U.S. monetary policy 1945–1971: frequency of policy deviations justified by domestic objectives, instances of IMF enforcement or waiver, and whether the gold window closure (1971) was treated as legal breach or systemic necessity.',
    'If binding legal obligation: high extractiveness, U.S. as victim, Tangled Rope classification holds. If legal form is cover: the constraint is a Snare from the U.S. seat, and the coordination function is retrospective justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legal_obligation_vs_policy_space, empirical, 'Whether the legal obligation is the operative constraint or a legitimating frame').

omega_variable(
    triffin_dilemma_as_omega,
    'Is the Triffin dilemma (the issuer must run deficits to supply reserves, undermining convertibility) an exogenous structural fact or an endogenous consequence of the strict reading''s enforcement?',
    'Model the reserve accumulation dynamics under strict convertibility: if creditor nations'' reserve demand forces U.S. deficits, the Triffin pressure is structural. If U.S. policy choices (e.g., Vietnam War spending, Great Society) independently drove deficits, the dilemma is partly endogenous. Examine the 1958–1971 gold outflow trajectory against fiscal/monetary policy shifts.',
    'If exogenous: the strict reading''s coordination function is inherently self-undermining — a Scaffold with a structural sunset. If endogenous: the extraction is policy-contingent, and the strict reading could have been sustained with different U.S. choices — making the Tangled Rope classification more robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(triffin_dilemma_as_omega, empirical, 'Triffin dilemma as exogenous structural limit vs. endogenous policy consequence').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (IMF conditionality, gold pool coordination, market discipline) or internalized (U.S. policymakers adopting creditor-nation preferences as their own)?',
    'Post-exit suppression trajectory: after 1971, did U.S. monetary policy immediately pursue domestic objectives without external constraint, or did the ''gold standard mentality'' persist in Volcker-era policy? If suppression persists after the extractive mechanism is removed, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit. This would elevate the computed extraction for the U.S. seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in the U.S. policy seat').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dollar_gold_convertibility__strict_convertibility_reading, 1945, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dgc_strict_tr_t1945, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 1945, 0.1).
narrative_ontology:measurement(dgc_strict_tr_t1950, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 1950, 0.12).
narrative_ontology:measurement(dgc_strict_tr_t1955, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 1955, 0.15).
narrative_ontology:measurement(dgc_strict_tr_t1958, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 1958, 0.18).
narrative_ontology:measurement(dgc_strict_tr_t1960, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 1960, 0.2).
narrative_ontology:measurement(dgc_strict_tr_t1965, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 1965, 0.21).
narrative_ontology:measurement(dgc_strict_tr_t1968, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 1968, 0.22).
narrative_ontology:measurement(dgc_strict_tr_t1971, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 1971, 0.22).

% Extraction over time
narrative_ontology:measurement(dgc_strict_be_t1945, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 1945, 0.35).
narrative_ontology:measurement(dgc_strict_be_t1950, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 1950, 0.42).
narrative_ontology:measurement(dgc_strict_be_t1955, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 1955, 0.51).
narrative_ontology:measurement(dgc_strict_be_t1958, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 1958, 0.58).
narrative_ontology:measurement(dgc_strict_be_t1960, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 1960, 0.65).
narrative_ontology:measurement(dgc_strict_be_t1965, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 1965, 0.72).
narrative_ontology:measurement(dgc_strict_be_t1968, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 1968, 0.76).
narrative_ontology:measurement(dgc_strict_be_t1971, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 1971, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(dgc_strict_su_t1945, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 1945, 0.45).
narrative_ontology:measurement(dgc_strict_su_t1950, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 1950, 0.55).
narrative_ontology:measurement(dgc_strict_su_t1955, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 1955, 0.62).
narrative_ontology:measurement(dgc_strict_su_t1958, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 1958, 0.7).
narrative_ontology:measurement(dgc_strict_su_t1960, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 1960, 0.75).
narrative_ontology:measurement(dgc_strict_su_t1965, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 1965, 0.8).
narrative_ontology:measurement(dgc_strict_su_t1968, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 1968, 0.83).
narrative_ontology:measurement(dgc_strict_su_t1971, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 1971, 0.85).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1945, tn=1971
narrative_ontology:measurement(dgc_strict_grid_01, dollar_gold_convertibility__strict_convertibility_reading, accessibility_collapse(class), 1945, 0.3).
narrative_ontology:measurement(dgc_strict_grid_02, dollar_gold_convertibility__strict_convertibility_reading, accessibility_collapse(class), 1971, 0.65).
narrative_ontology:measurement(dgc_strict_grid_03, dollar_gold_convertibility__strict_convertibility_reading, accessibility_collapse(individual), 1945, 0.25).
narrative_ontology:measurement(dgc_strict_grid_04, dollar_gold_convertibility__strict_convertibility_reading, accessibility_collapse(individual), 1971, 0.55).
narrative_ontology:measurement(dgc_strict_grid_05, dollar_gold_convertibility__strict_convertibility_reading, accessibility_collapse(organizational), 1945, 0.35).
narrative_ontology:measurement(dgc_strict_grid_06, dollar_gold_convertibility__strict_convertibility_reading, accessibility_collapse(organizational), 1971, 0.78).
narrative_ontology:measurement(dgc_strict_grid_07, dollar_gold_convertibility__strict_convertibility_reading, accessibility_collapse(structural), 1945, 0.4).
narrative_ontology:measurement(dgc_strict_grid_08, dollar_gold_convertibility__strict_convertibility_reading, accessibility_collapse(structural), 1971, 0.85).
narrative_ontology:measurement(dgc_strict_grid_09, dollar_gold_convertibility__strict_convertibility_reading, resistance(class), 1945, 0.25).
narrative_ontology:measurement(dgc_strict_grid_10, dollar_gold_convertibility__strict_convertibility_reading, resistance(class), 1971, 0.6).
narrative_ontology:measurement(dgc_strict_grid_11, dollar_gold_convertibility__strict_convertibility_reading, resistance(individual), 1945, 0.2).
narrative_ontology:measurement(dgc_strict_grid_12, dollar_gold_convertibility__strict_convertibility_reading, resistance(individual), 1971, 0.52).
narrative_ontology:measurement(dgc_strict_grid_13, dollar_gold_convertibility__strict_convertibility_reading, resistance(organizational), 1945, 0.3).
narrative_ontology:measurement(dgc_strict_grid_14, dollar_gold_convertibility__strict_convertibility_reading, resistance(organizational), 1971, 0.68).
narrative_ontology:measurement(dgc_strict_grid_15, dollar_gold_convertibility__strict_convertibility_reading, resistance(structural), 1945, 0.35).
narrative_ontology:measurement(dgc_strict_grid_16, dollar_gold_convertibility__strict_convertibility_reading, resistance(structural), 1971, 0.72).
narrative_ontology:measurement(dgc_strict_grid_17, dollar_gold_convertibility__strict_convertibility_reading, stakes_inflation(class), 1945, 0.22).
narrative_ontology:measurement(dgc_strict_grid_18, dollar_gold_convertibility__strict_convertibility_reading, stakes_inflation(class), 1971, 0.72).
narrative_ontology:measurement(dgc_strict_grid_19, dollar_gold_convertibility__strict_convertibility_reading, stakes_inflation(individual), 1945, 0.18).
narrative_ontology:measurement(dgc_strict_grid_20, dollar_gold_convertibility__strict_convertibility_reading, stakes_inflation(individual), 1971, 0.6).
narrative_ontology:measurement(dgc_strict_grid_21, dollar_gold_convertibility__strict_convertibility_reading, stakes_inflation(organizational), 1945, 0.28).
narrative_ontology:measurement(dgc_strict_grid_22, dollar_gold_convertibility__strict_convertibility_reading, stakes_inflation(organizational), 1971, 0.82).
narrative_ontology:measurement(dgc_strict_grid_23, dollar_gold_convertibility__strict_convertibility_reading, stakes_inflation(structural), 1945, 0.3).
narrative_ontology:measurement(dgc_strict_grid_24, dollar_gold_convertibility__strict_convertibility_reading, stakes_inflation(structural), 1971, 0.88).
narrative_ontology:measurement(dgc_strict_grid_25, dollar_gold_convertibility__strict_convertibility_reading, suppression(class), 1945, 0.35).
narrative_ontology:measurement(dgc_strict_grid_26, dollar_gold_convertibility__strict_convertibility_reading, suppression(class), 1971, 0.75).
narrative_ontology:measurement(dgc_strict_grid_27, dollar_gold_convertibility__strict_convertibility_reading, suppression(individual), 1945, 0.28).
narrative_ontology:measurement(dgc_strict_grid_28, dollar_gold_convertibility__strict_convertibility_reading, suppression(individual), 1971, 0.65).
narrative_ontology:measurement(dgc_strict_grid_29, dollar_gold_convertibility__strict_convertibility_reading, suppression(organizational), 1945, 0.4).
narrative_ontology:measurement(dgc_strict_grid_30, dollar_gold_convertibility__strict_convertibility_reading, suppression(organizational), 1971, 0.8).
narrative_ontology:measurement(dgc_strict_grid_31, dollar_gold_convertibility__strict_convertibility_reading, suppression(structural), 1945, 0.45).
narrative_ontology:measurement(dgc_strict_grid_32, dollar_gold_convertibility__strict_convertibility_reading, suppression(structural), 1971, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dollar_gold_convertibility__strict_convertibility_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(dollar_gold_convertibility__strict_convertibility_reading, 0.12).
narrative_ontology:affects_constraint(dollar_gold_convertibility__strict_convertibility_reading, dollar_gold_convertibility__policy_flexible_reading).
narrative_ontology:affects_constraint(dollar_gold_convertibility__strict_convertibility_reading, dollar_gold_convertibility__triffin_structural_reading).
narrative_ontology:affects_constraint(dollar_gold_convertibility__strict_convertibility_reading, bretton_woods_adjustment_mechanism).
narrative_ontology:affects_constraint(dollar_gold_convertibility__strict_convertibility_reading, gold_pool_coordination).
narrative_ontology:affects_constraint(dollar_gold_convertibility__strict_convertibility_reading, imf_conditionality_evolution).
narrative_ontology:affects_constraint(dollar_gold_convertibility__strict_convertibility_reading, triffin_dilemma_diagnosis).

% DUAL FORMULATION NOTE:
% The dollar_gold_convertibility kernel decomposes into three structurally distinct constraints: (1) strict_convertibility_reading — Article IV as binding legal obligation extracting from U.S. policy space (Tangled Rope, ε=0.78); (2) policy_flexible_reading — convertibility as conditional, subordinate to domestic stability (Rope or Scaffold, lower ε); (3) triffin_structural_reading — convertibility as self-undermining design flaw (Piton or Scaffold with structural sunset). The ε values differ because the referent (the standing arrangement under contest) is assessed by each reading's own lights: the strict reading sees binding law; the flexible reading sees managed flexibility; the Triffin reading sees structural contradiction. This decomposition follows the BGS pattern: spectral universality (Mountain) vs. eigenvector thermalization (Tangled Rope) — same label, different constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dollar_gold_convertibility__strict_convertibility_reading, institutional, 0.15).
constraint_indexing:directionality_override(dollar_gold_convertibility__strict_convertibility_reading, powerful, 0.85).
constraint_indexing:directionality_override(dollar_gold_convertibility__strict_convertibility_reading, powerless, 0.92).
constraint_indexing:directionality_override(dollar_gold_convertibility__strict_convertibility_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
