% ============================================================================
% CONSTRAINT STORY: dollar_gold_convertibility__strict_convertibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: dollar_gold_convertibility__strict_convertibility_reading
 *   human_readable: Article IV Convertibility as Binding Legal Obligation on U.S. Monetary Policy
 *   domain: international_political_economy/monetary_history/international_law
 *
 * SUMMARY:
 *   Under the strict convertibility reading of Article IV of the Bretton
 *   Woods Agreement, the U.S. commitment to redeem dollars in gold at 35
 *   dollars per troy ounce is a binding legal obligation that constrains
 *   domestic monetary policy. The strict reading treats convertibility not as
 *   a policy tool or conditional mechanism, but as an enforceable constraint:
 *   creditor nations hold claims on U.S. gold, and the U.S. cannot
 *   unilaterally reweight policy objectives toward full employment or growth
 *   without breaching the legal commitment. As gold reserves fell (from
 *   ~21,000 tonnes in 1949 to ~8,134 tonnes by 1971), the constraint
 *   tightened: the Fed faced explicit pressure to contract money supply,
 *   raise interest rates, and subordinate domestic prosperity to external
 *   gold discipline. By 1970–71, the constraint was openly extractive — the
 *   U.S. could not pursue independent fiscal stimulus without triggering
 *   capital flight and gold runs. This reading emerged as the dominant frame
 *   in the 1960s as structural economists argued that the Bretton Woods
 *   system had become mathematically impossible to sustain: Triffin showed
 *   that the U.S. could not simultaneously maintain fixed gold parity, run a
 *   reserve-currency monetary system, and pursue full employment. The strict
 *   reading insists on the binding nature of the legal obligation even as the
 *   system approached collapse.
 *
 * KEY AGENTS:
 *   - United States Federal Reserve: operationally responsible for maintaining the gold peg; faces the policy contradiction between dual mandate (price stability + full employment) and gold discipline
 *   - U.S. government (Treasury, President): nominally sovereign but trapped by the legal commitment; exit requires either unilateral treaty violation or negotiated reform
 *   - Creditor nations (West Germany, Japan, France): accumulate dollar reserves and hold the threat of gold redemption; benefit from enforcement leverage
 *   - International Monetary Fund: interprets and mediates Article IV obligations; structurally positioned to validate the strict reading
 *   - Domestic constituencies (labor, business, political): trapped inside U.S. economy; interests constrained by external-balance discipline
 *   - Excluded voices (Triffin economists, Global South): argue the system is unsustainable but are locked out of policy choice under the strict reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dollar_gold_convertibility__strict_convertibility_reading, 0.78).
domain_priors:suppression_score(dollar_gold_convertibility__strict_convertibility_reading, 0.61).
domain_priors:theater_ratio(dollar_gold_convertibility__strict_convertibility_reading, 0.29).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 0.29).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, resistance, 0.69).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dollar_gold_convertibility__strict_convertibility_reading, tangled_rope).
narrative_ontology:human_readable(dollar_gold_convertibility__strict_convertibility_reading, "Article IV Convertibility as Binding Legal Obligation on U.S. Monetary Policy").
narrative_ontology:topic_domain(dollar_gold_convertibility__strict_convertibility_reading, "international_political_economy/monetary_history/international_law").

domain_priors:requires_active_enforcement(dollar_gold_convertibility__strict_convertibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dollar_gold_convertibility__strict_convertibility_reading, '000ac743-a8b4-43f7-9d9f-12631f0d8b35').
narrative_ontology:cs_kernel_codification('000ac743-a8b4-43f7-9d9f-12631f0d8b35', formalized).
narrative_ontology:cs_authority_grounding('000ac743-a8b4-43f7-9d9f-12631f0d8b35', lineage).
narrative_ontology:cs_interpretation_layer_present('000ac743-a8b4-43f7-9d9f-12631f0d8b35').
narrative_ontology:cs_reading_relation('000ac743-a8b4-43f7-9d9f-12631f0d8b35', dollar_gold_convertibility__policy_flexible_reading, forecloses).
narrative_ontology:cs_reading_relation('000ac743-a8b4-43f7-9d9f-12631f0d8b35', dollar_gold_convertibility__triffin_structural_reading, coexists_with).
narrative_ontology:cs_axiom('000ac743-a8b4-43f7-9d9f-12631f0d8b35', foundational, article_iv_binding_legal_obligation).
narrative_ontology:cs_axiom_status(article_iv_binding_legal_obligation, holdable).
narrative_ontology:cs_axiom_grounding('000ac743-a8b4-43f7-9d9f-12631f0d8b35', article_iv_binding_legal_obligation, conventional).
narrative_ontology:cs_axiom('000ac743-a8b4-43f7-9d9f-12631f0d8b35', foundational, gold_parity_non_negotiable_under_law).
narrative_ontology:cs_axiom_status(gold_parity_non_negotiable_under_law, overridden).
narrative_ontology:cs_axiom_grounding('000ac743-a8b4-43f7-9d9f-12631f0d8b35', gold_parity_non_negotiable_under_law, conventional).
narrative_ontology:cs_reference_frame('000ac743-a8b4-43f7-9d9f-12631f0d8b35', binding_legal_convertibility_obligation).
narrative_ontology:cs_drift_state('000ac743-a8b4-43f7-9d9f-12631f0d8b35', post_1971_breakdown, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('000ac743-a8b4-43f7-9d9f-12631f0d8b35', '').
narrative_ontology:cs_kernel_id(dollar_gold_convertibility__strict_convertibility_reading, dollar_gold_convertibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__strict_convertibility_reading, creditor_nations_dollar_holders).
narrative_ontology:constraint_victim(dollar_gold_convertibility__strict_convertibility_reading, united_states_as_issuer).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__strict_convertibility_reading, international_creditor_bloc).
narrative_ontology:constraint_victim(dollar_gold_convertibility__strict_convertibility_reading, federal_reserve).
narrative_ontology:constraint_victim(dollar_gold_convertibility__strict_convertibility_reading, domestic_policy_constituencies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the issuer of the reserve currency, the U.S. commits to redeem dollars in gold at fixed parity (35 dollars per troy ounce). Under the strict reading, this convertibility obligation constrains domestic monetary policy: the Federal Reserve cannot expand money supply beyond what gold reserves support without triggering capital flight and depleting gold stocks. The constraint locks U.S. economic sovereignty into a gold-denominated straightjacket — full employment and growth objectives must yield to balance-of-payments discipline. Exit looks like unilateral suspension of convertibility, which carries diplomatic and legitimacy costs (appears as default on a binding legal commitment) and triggers immediate retaliation from creditor nations.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, united_states_as_issuer, payer,
    institutional, generational, identity_locked, global).

% Hold substantial dollar reserves and possess enforceable claims to gold redemption at fixed parity. They benefit from a stable, gold-backed reserve currency that anchors international trade and finance. They also hold the enforcement mechanism: if the U.S. cannot credibly maintain convertibility, capital flight and gold redemption demands force policy compliance. Creditor nations (especially those running trade surpluses: West Germany, Japan) accumulate dollars and can exercise the threat of redemption to constrain U.S. fiscal and monetary choices.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, creditor_nations_dollar_holders, beneficiary,
    organized, generational, constrained, global).

% Operates under the strict reading of Article IV: maintains the gold peg and manages domestic policy to preserve convertibility. The Fed is responsible for both price stability and full employment (dual mandate), but under the strict convertibility regime, gold discipline forces it to prioritize external balance (gold reserves) over domestic objectives. The Fed absorbs the policy contradiction: it is formally empowered to conduct monetary policy but operationally subordinated to the convertibility requirement. Suspension or abandonment of convertibility is technically within the Fed's operational scope but politically and legally contested.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, federal_reserve, agenda_setter,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(dollar_gold_convertibility__strict_convertibility_reading, federal_reserve, payer).

% Governments and central banks of Western Europe, Japan, and other dollar-reserve holders who have accumulated claims on U.S. gold. They collectively hold the threat of triggering gold redemption runs. Under the strict reading, their threat is credible and enforceable — convertibility is a binding legal obligation, not a policy choice. They can coordinate to demand redemption and force policy discipline on the U.S., or they can hold reserves passively and reap the seigniorage benefit of a stable dollar system. Their power derives from the U.S. commitment to legal convertibility.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, international_creditor_bloc, beneficiary,
    organized, generational, arbitrage, global).

% U.S. labor unions, businesses, and political constituencies that depend on full employment and growth. Under the strict convertibility reading, their policy demands for expansionary monetary and fiscal stimulus are subordinated to gold discipline. When the Fed must choose between domestic full employment and external gold discipline, the strict reading forces external balance to win. Labor faces underemployment; businesses face credit constraints; political coalitions lose electoral leverage. Exit is not available — their interests are trapped inside the U.S. economy.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, domestic_policy_constituencies, payer,
    organized, biographical, trapped, national).

% Economists who argue (following Robert Triffin) that the strict convertibility regime is mathematically and structurally unsustainable — the U.S. cannot simultaneously serve as monetary authority for a global system, maintain a fixed gold peg, and pursue full employment. They call for systemic redesign (floating rates, special drawing rights, delinked reserve arrangements). Under the strict reading, their structural critique is excluded from policy space: convertibility is treated as a legal obligation, not a design flaw. Their voice enters only as pressure for the reading itself to be abandoned.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, academic_economists_triffin_camp, excluded,
    analytical, generational, analytical, global).

% Manages the Bretton Woods system and interprets Article IV obligations. Under the strict reading, the IMF enforces convertibility discipline on all members, including the U.S., and mediates dispute resolution when members (especially creditors) demand enforcement of the gold-convertibility obligation. The Fund is formally neutral but structurally positioned to validate the creditor interpretation and delegitimize U.S. attempts to subordinate convertibility to domestic policy.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, international_monetary_fund_imf, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dollar_gold_convertibility__strict_convertibility_reading, creditor_nations_dollar_holders).
narrative_ontology:fixing_cost_class(dollar_gold_convertibility__strict_convertibility_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Anchors the post-World War II international monetary system on a gold-backed reserve currency: provides a stable numeraire for international trade, a discipline mechanism enforcing balanced external accounts, and a confidence mechanism that dollar reserves maintain purchasing power. Solves the collective-action problem of choosing a common medium of international exchange without requiring explicit central coordination.
% TRANSFER_FUNCTION: Extracts U.S. domestic monetary policy space and subordinates it to external gold discipline: the U.S. Fed must constrain money creation to preserve gold reserves, which constrains fiscal flexibility and full-employment policy. The extraction flows to creditor nations as the power to trigger gold runs and force U.S. compliance with external-balance constraints. Creditor nations receive the benefit of a stable, gold-backed reserve currency while holding enforcement leverage over the U.S.
% ABSENT_VOICES: Domestic labor, unemployed workers, and politically marginalized groups who would benefit from expansionary policy but are excluded from the monetary-policy conversation. Also excluded: economists and policymakers arguing that the strict convertibility regime is unsustainable (they are present in advisory roles but structurally locked out of policy choices under the strict reading). Countries in the Global South and non-aligned bloc are excluded from the dollar-holder beneficiary set and bear the external-stability constraint as discipline (the Bretton Woods system enforces austerity on deficit countries regardless of development needs).
% DISAPPEARANCE_RATIONALE: If the strict convertibility obligation disappeared (unilateral U.S. suspension, or formal treaty amendment), the international monetary system would reorganize: creditor nations would lose enforcement leverage, U.S. monetary policy would regain autonomy, fixed exchange rates would become difficult to maintain without a gold anchor, and the system would drift toward floating rates or a multilateral reserve arrangement (SDR-based). Trade patterns would shift as exchange-rate flexibility opened. The constraint's disappearance would be catastrophic for the current system's architecture.
% FOUNDING_PROBLEM: Post-WWII international monetary disorder: with the gold standard in collapse, the U.S. had a strategic interest in providing a stable medium for international exchange to rebuild war-shattered economies and create a stable trading system. By pegging the dollar to gold at a fixed rate and offering redemption, the U.S. provided a public good — a credible, gold-backed numeraire that reduced transaction uncertainty and enabled normalized trade. The founding problem was real: credible, stable money for international commerce.
% FOUNDING_PROBLEM_CORROBORATION: U.S. and Allied negotiators at Bretton Woods attested the founding problem: gold-standard collapse created monetary chaos that hindered recovery. Independent economic historians (e.g., Steil, Eichengreen) corroborate that the post-WWI monetary disorder incentivized a gold-backed reserve system. HOWEVER: by the 1960s, creditor nations and structural economists (Triffin, Kindleberger) attested that the founding problem had been SOLVED — stable trade had resumed, capital flows had normalized, and the gold-standard discipline was no longer the binding constraint. The U.S. government attested (privately; publicly maintained the opposite) that the founding problem was solved and the constraint had become a policy straitjacket. The contest is whether convertibility persists as solution to a live problem (strict reading) or as constraining machinery on a solved problem (Triffin reading).
narrative_ontology:disappearance_verdict(dollar_gold_convertibility__strict_convertibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(dollar_gold_convertibility__strict_convertibility_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dollar_gold_convertibility__strict_convertibility_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dollar_gold_convertibility__strict_convertibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dollar_gold_convertibility__strict_convertibility_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness rises from 0.35 (1944, early post-war, large U.S. gold reserves, constraint not yet binding) to 0.78 (1971, gold reserves depleted, constraint maximally restrictive). The trend reflects the structural mechanism of the constraint: in the early years, the U.S. had sufficient gold to absorb policy flexibility; by the 1960s, gold stocks fell and the constraint became binding and extractive. Suppression is moderate (0.61 at interval end) because the constraint operates through legal obligation, not overt coercion — the suppression comes from the threat of gold redemption runs and the reputational cost of breach, not armed force. Theater rises from 0.08 to 0.29 as the system deteriorates: the Fed increasingly engages in managed rituals (the London Gold Pool, coordination meetings, diplomatic theater) to maintain the appearance of convertibility while secretly acknowledging its unsustainability. The constraint's function shifts from genuine coordination (1944–55) to enforced extraction (1965–71), showing lifecycle drift toward theater and mandatrophy.
 *
 * PERSPECTIVAL GAP:
 *   From the U.S. seat: the constraint is increasingly extractive and policy-hostile — it subordinates domestic full employment to external discipline and is backed by a threat (gold runs) that grows more credible as reserves deplete. From the creditor-nation seat: the constraint provides valuable enforcement leverage and a stable reserve currency system. From the IMF seat: the constraint is a neutral legal obligation applying to all members equally (formally); but its application is asymmetric — the U.S. is the issuer and bears the main burden, while creditors enjoy enforcement power. The engine should compute substantially different effective extractiveness across these seats.
 *
 * DIRECTIONALITY LOGIC:
 *   U.S. as issuer: directionality near 1.0 (full target). The constraint is authored as binding legal obligation, leaving no policy autonomy. Exit options are identity_locked — the U.S. cannot abandon its reserve-currency role without reputational and diplomatic catastrophe; suspension of convertibility is treated as default on a legal commitment. Beneficiaries (creditor nations): directionality near 0.0 (full beneficiary). They hold enforceable claims and can trigger gold redemption; they benefit from the stable dollar system and from the leverage the constraint provides. The IMF and international observers: directionality near 0.5 (symmetric) — they have an interest in system stability but also in symmetrical application of obligations. Domestic constituencies: trapped inside — no exit options at all. The extreme directionality gradient (issuer as 1.0 target vs. creditors as 0.0 beneficiary) is the hallmark of extractive asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The strict reading invokes a legal mandate (Article IV convertibility obligation) to justify the constraint, but by 1970–71, the mandate had become mandatrophy: the founding problem (providing stable money for post-war recovery) had been solved; the constraint persisted and became extractive. The real problem the U.S. faced was NOT convertibility breakdown, but structural deficiency — the system was mathematically designed to become impossible (Triffin dilemma). The strict reading cannot acknowledge this; instead, it maintains that convertibility is a binding legal obligation regardless of economic circumstance. This is mandatrophy: the constraint persists as legal theater even as its underlying function has atrophied. By 1971, the system was held together by managed fictions (London Gold Pool, Swiss arrangements, forward contracts) — pure theater. The classification should flag this: a tangled_rope that is drifting toward piton status as the coordination function atrophies and only the extraction/theater remains.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legal_binding_vs_policy_flexibility,
    'Is Article IV convertibility a binding legal obligation that constrains U.S. policy choice, or a conditional commitment subordinate to domestic economic stability?',
    'Historical record of U.S. government statements, IMF interpretations, and creditor-nation pressure campaigns. Does the U.S. government treat convertibility as non-negotiable, or as reweightable against domestic objectives? When does the U.S. invoke legal language vs. policy language?',
    'If binding: the strict reading is correct and the constraint is maximally extractive from the U.S. seat. If conditional: the constraint is subordinate to domestic policy and less extractive; the policy_flexible_reading is correct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legal_binding_vs_policy_flexibility, conceptual, 'Whether Article IV convertibility is binding law or conditional policy.').

omega_variable(
    structural_unsustainability_vs_design_feature,
    'Is the gold-peg system a design feature that coordinates international exchange (and is therefore binding on all parties including the U.S.), or is it structurally unsustainable by Triffin''s argument and therefore irrational to maintain?',
    'Technical analysis of the mathematics of simultaneous fixed rates, a monetary authority, and full employment (the impossible trinity). Empirical trace: does the constraint become progressively more extractive as reserves deplete, as Triffin predicts?',
    'If unsustainable by design: the binding legal reading is irrational and mandatrophy is the correct diagnosis. If sustainable: the constraint can remain enforceable. The measurements show rising extractiveness, which supports the unsustainability diagnosis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(structural_unsustainability_vs_design_feature, empirical, 'Whether the dollar-gold peg is a sustainable design or a mathematical impossibility.').

omega_variable(
    enforcement_mechanism_credibility,
    'Can creditor nations credibly trigger gold redemption runs to enforce convertibility, or is the threat empty?',
    'Empirical: (1) Do gold stocks fall as the threat is tested (they do, sharply 1960–71)? (2) Do creditor nations actually redeem gold when threatened (France does, gradually and strategically)? (3) Is the U.S. forced to accommodate redemption demands (yes, repeatedly)?',
    'If credible: the enforcement mechanism is real and the constraint is extractive from the U.S. seat. If empty: the constraint is theater and suppression is overstated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_mechanism_credibility, empirical, 'Whether creditor nations can enforce the convertibility obligation through gold redemption.').

omega_variable(
    identity_lock_mechanism_u_s,
    'Why does the U.S. not simply suspend convertibility unilaterally? What identity or institutional constraint locks the U.S. into the commitment?',
    'Analysis of U.S. stated rationale for maintaining convertibility despite deteriorating gold stocks. Factors: (1) fear of financial crisis and capital flight, (2) commitment to postwar liberal order, (3) ideological attachment to gold standard as legitimacy mechanism, (4) fear of Soviet advantage if the dollar system fails.',
    'If the lock is purely institutional/reputational (fear of breach consequences), exit_options is constrained, not identity_locked. If the lock includes ideological or institutional identity fusion (the U.S. sees itself as the guarantor of Western financial order), exit_options is identity_locked and directionality is higher.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_u_s, conceptual, 'The mechanism that locks U.S. into convertibility despite policy costs.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dollar_gold_convertibility__strict_convertibility_reading, 1944, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doll_tr_t1944, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 1944, 0.08).
narrative_ontology:measurement(doll_tr_t1950, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 1950, 0.12).
narrative_ontology:measurement(doll_tr_t1960, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 1960, 0.19).
narrative_ontology:measurement(doll_tr_t1965, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 1965, 0.24).
narrative_ontology:measurement(doll_tr_t1970, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 1970, 0.27).
narrative_ontology:measurement(doll_tr_t1971, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 1971, 0.29).

% Extraction over time
narrative_ontology:measurement(doll_be_t1944, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 1944, 0.35).
narrative_ontology:measurement(doll_be_t1950, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 1950, 0.42).
narrative_ontology:measurement(doll_be_t1960, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 1960, 0.65).
narrative_ontology:measurement(doll_be_t1965, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 1965, 0.72).
narrative_ontology:measurement(doll_be_t1970, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 1970, 0.76).
narrative_ontology:measurement(doll_be_t1971, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 1971, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(doll_su_t1944, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 1944, 0.25).
narrative_ontology:measurement(doll_su_t1950, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 1950, 0.35).
narrative_ontology:measurement(doll_su_t1960, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 1960, 0.48).
narrative_ontology:measurement(doll_su_t1965, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 1965, 0.56).
narrative_ontology:measurement(doll_su_t1970, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 1970, 0.59).
narrative_ontology:measurement(doll_su_t1971, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 1971, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dollar_gold_convertibility__strict_convertibility_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(dollar_gold_convertibility__strict_convertibility_reading, 0.25).
narrative_ontology:affects_constraint(dollar_gold_convertibility__strict_convertibility_reading, dollar_gold_convertibility__policy_flexible_reading).
narrative_ontology:affects_constraint(dollar_gold_convertibility__strict_convertibility_reading, dollar_gold_convertibility__triffin_structural_reading).
narrative_ontology:affects_constraint(dollar_gold_convertibility__strict_convertibility_reading, bretton_woods_fixed_rate_regime).
narrative_ontology:affects_constraint(dollar_gold_convertibility__strict_convertibility_reading, international_monetary_fund_surveillance).
narrative_ontology:affects_constraint(dollar_gold_convertibility__strict_convertibility_reading, dollar_hegemony_post_wwii).

% DUAL FORMULATION NOTE:
% The dollar-gold convertibility kernel admits three structurally distinct constraint readings. This story (strict_convertibility_reading) treats the commitment as binding law that constrains U.S. policy; the policy_flexible_reading treats it as conditional and reweightable; the triffin_structural_reading treats it as mathematically unsustainable design flaw. Each reading has a different ε, beneficiary structure, and classification. The strict reading makes the U.S. a victim (constrained issuer); the flexible reading returns autonomy to the U.S. (lower extractiveness); the Triffin reading treats the whole system as mandatrophic. All three are live positions in 1960s policy discourse. Link the three stories through network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dollar_gold_convertibility__strict_convertibility_reading, institutional, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
