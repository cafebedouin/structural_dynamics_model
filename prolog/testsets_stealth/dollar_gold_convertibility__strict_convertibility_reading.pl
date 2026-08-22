% ============================================================================
% CONSTRAINT STORY: dollar_gold_convertibility__strict_convertibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-07
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
 *   human_readable: Article IV Dollar-Gold Convertibility as Binding Legal Obligation Constraining U.S. Monetary Policy (Strict Reading)
 *   domain: economic/political/legal
 *
 * SUMMARY:
 *   The Bretton Woods Articles of Agreement (negotiated 1944, operational
 *   from 1946) committed the United States to convert foreign official dollar
 *   holdings into gold at thirty-five dollars per ounce. This story
 *   instantiates the STRICT reading of that undertaking: a binding legal
 *   obligation that constrained U.S. monetary and fiscal policy, since
 *   over-issue invited presentation of dollars for metal and forced domestic
 *   tightening. Creditor nations, France foremost, exercised the claim as
 *   deliberate leverage from 1965; the U.S. defended parity through the
 *   London Gold Pool, the Interest Equalization Tax, voluntary
 *   capital-control programs, and moral suasion, until suspending conversion
 *   on 15 August 1971. This file is ONE reading of a contested kernel: the
 *   policy_flexible_reading and the triffin_structural_reading are separate
 *   constraints (separate files), linked here via
 *   network.affects_constraints. Claim/metric independence is preserved: the
 *   constraint is CLAIMED as tangled_rope (genuine coordination function plus
 *   asymmetric, enforced extraction through the same structure) while the
 *   authored metrics describe substantially extractive operation assessed
 *   from the strict reading's own lights, with the standing arrangement under
 *   contest — the convertibility obligation as it bound U.S. policy — as the
 *   epsilon referent.
 *
 * KEY AGENTS:
 *   - us_monetary_policymakers: Primary target (institutional/identity_locked) — bears the conversion obligation and simultaneously administers the gold window; dual-positioned payer/agenda_setter
 *   - creditor_nation_central_banks: Primary beneficiary (organized/arbitrage) — hold enforceable gold claims; the conversion decision is the system's enforcement mechanism
 *   - surplus_exporting_member_states: Secondary beneficiary (powerful/mobile) — accumulate dollar claims, gain stable export parities, finance the U.S. deficit by forbearance
 *   - fixed_rate_trading_partners: Diffuse beneficiary (moderate/constrained) — trade under stable parities, absorb transmitted tightening
 *   - us_fiscal_expansion_administrations: Co-target (powerful/constrained) — war and welfare spending collide with a finite gold stock
 *   - deficit_developing_members: Excluded voice (powerless/trapped) — short of reserves, subject to conditionality, largely absent from the 1944 design
 *   - imf_par_value_supervisors: Analytical observer (institutional/analytical) — supervise par values and interpret 'fundamental disequilibrium'; cannot compel the U.S.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dollar_gold_convertibility__strict_convertibility_reading, 0.7).
domain_priors:suppression_score(dollar_gold_convertibility__strict_convertibility_reading, 0.6).
domain_priors:theater_ratio(dollar_gold_convertibility__strict_convertibility_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dollar_gold_convertibility__strict_convertibility_reading, tangled_rope).
narrative_ontology:human_readable(dollar_gold_convertibility__strict_convertibility_reading, "Article IV Dollar-Gold Convertibility as Binding Legal Obligation Constraining U.S. Monetary Policy (Strict Reading)").
narrative_ontology:topic_domain(dollar_gold_convertibility__strict_convertibility_reading, "economic/political/legal").

domain_priors:requires_active_enforcement(dollar_gold_convertibility__strict_convertibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dollar_gold_convertibility__strict_convertibility_reading, 'a35ce909-02cd-46a0-a601-e3c61847b969').
narrative_ontology:cs_kernel_codification('a35ce909-02cd-46a0-a601-e3c61847b969', formalized).
narrative_ontology:cs_authority_grounding('a35ce909-02cd-46a0-a601-e3c61847b969', lineage).
narrative_ontology:cs_interpretation_layer_present('a35ce909-02cd-46a0-a601-e3c61847b969').
narrative_ontology:cs_reading_relation('a35ce909-02cd-46a0-a601-e3c61847b969', dollar_gold_convertibility__policy_flexible_reading, forecloses).
narrative_ontology:cs_reading_relation('a35ce909-02cd-46a0-a601-e3c61847b969', dollar_gold_convertibility__triffin_structural_reading, coexists_with).
narrative_ontology:cs_axiom('a35ce909-02cd-46a0-a601-e3c61847b969', foundational, article_iv_binding_notwithstanding_domestic_conditions).
narrative_ontology:cs_axiom_status(article_iv_binding_notwithstanding_domestic_conditions, holdable).
narrative_ontology:cs_axiom_grounding('a35ce909-02cd-46a0-a601-e3c61847b969', article_iv_binding_notwithstanding_domestic_conditions, conventional).
narrative_ontology:cs_axiom('a35ce909-02cd-46a0-a601-e3c61847b969', secondary, creditor_redemption_claim_justiciable).
narrative_ontology:cs_axiom_status(creditor_redemption_claim_justiciable, holdable).
narrative_ontology:cs_axiom_grounding('a35ce909-02cd-46a0-a601-e3c61847b969', creditor_redemption_claim_justiciable, conventional).
narrative_ontology:cs_reference_frame('a35ce909-02cd-46a0-a601-e3c61847b969', treaty_bound_gold_parity_order).
narrative_ontology:cs_drift_state('a35ce909-02cd-46a0-a601-e3c61847b969', late_dollar_glut_1968_1971, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a35ce909-02cd-46a0-a601-e3c61847b969', '').
narrative_ontology:cs_kernel_id(dollar_gold_convertibility__strict_convertibility_reading, dollar_gold_convertibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__strict_convertibility_reading, creditor_nation_central_banks).
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__strict_convertibility_reading, surplus_exporting_member_states).
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__strict_convertibility_reading, fixed_rate_trading_partners).
narrative_ontology:constraint_victim(dollar_gold_convertibility__strict_convertibility_reading, us_monetary_policymakers).
narrative_ontology:constraint_victim(dollar_gold_convertibility__strict_convertibility_reading, us_fiscal_expansion_administrations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold large balances of dollars accumulated from trade surpluses. Under the Articles they may present those dollars to the U.S. Treasury for gold at thirty-five dollars per ounce. When they doubt the dollar's worth they present them; France did so on a large scale from 1965, flying bullion home. Their recurring choice — hold more dollars or take the metal — is the main thing U.S. officials watch when setting policy, and exercising it costs them nothing but the relationship.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, creditor_nation_central_banks, beneficiary,
    organized, generational, arbitrage, global).

% Run persistent trade surpluses — West Germany, Japan, Italy through the 1960s — and accumulate dollar claims. They gain stable exchange rates for exports and a ready settlement asset. Most choose to hold dollars rather than convert, supporting the parity out of export interest and alliance politics; their accumulation is what finances the U.S. deficit, and they can shift between holding and converting as their interests move.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, surplus_exporting_member_states, beneficiary,
    powerful, generational, mobile, global).

% The broad membership that pegs currencies to the dollar and trades under stable parities. They receive predictable prices for cross-border commerce and access to a working settlements system. They carry indirect costs when U.S. tightening transmits through the peg, and they cannot revalue without breaking the peg they rely on.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, fixed_rate_trading_partners, beneficiary,
    moderate, biographical, constrained, global).

% Set U.S. interest rates and money growth while administering the gold window at the Treasury. Every expansionary move enlarges the stock of foreign dollar claims that can be presented for metal; every defense of parity pulls domestic policy tighter than domestic conditions call for. They designed the system, run the window, and answer for it; walking away means announcing that the order they lead has failed, which is why successive administrations improvise workarounds — the Interest Equalization Tax, voluntary restraint programs, the Gold Pool — rather than leave.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, us_monetary_policymakers, payer,
    institutional, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(dollar_gold_convertibility__strict_convertibility_reading, us_monetary_policymakers, agenda_setter).

% Finance Vietnam escalation and Great Society programs through the late 1960s. Each deficit widens foreign dollar holdings and brings redemption pressure closer, while the electoral horizon is short and the gold stock finite. They press the Fed for easier money even as the window drains, and they inherit whatever credibility remains at the next crisis.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, us_fiscal_expansion_administrations, payer,
    powerful, immediate, constrained, national).

% Newly independent and developing members short of reserves. The par-value system offers them stability but little liquidity, and Fund assistance arrives with conditionality attached. They had little voice in the 1944 design and would argue for automatic liquidity creation rather than creditor-disciplined scarcity; their objections surface in Fund annual meetings rather than in the Articles.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, deficit_developing_members, excluded,
    powerless, biographical, trapped, regional).

% The Fund's Board and staff supervise par values, consult with members on fundamentals, and can declare a member ineligible to draw if it misuses resources. They interpret phrases like 'fundamental disequilibrium' and mediate between creditor demands and issuer convenience. They observe and advise; they cannot compel the United States to defend any particular parity.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, imf_par_value_supervisors, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dollar_gold_convertibility__strict_convertibility_reading, creditor_nation_central_banks).
narrative_ontology:fixing_cost_class(dollar_gold_convertibility__strict_convertibility_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Anchors the postwar exchange-rate system: a single external anchor at thirty-five dollars per ounce makes all member parities mutually consistent, supplies a universally acceptable settlement asset, and disciplines member monetary policy against competitive devaluation — solving once, centrally, the problem each country otherwise faces separately of defending its own currency's external value.
% TRANSFER_FUNCTION: Moves adjustment burden and discipline from the collective fixed-rate order to the U.S. issuer: transfers policy autonomy from Washington to the system's rules, moves physical gold from Fort Knox to creditor central banks whenever the U.S. runs external deficits, and moves deflationary pressure onto U.S. domestic policy whenever redemption threatens.
% ABSENT_VOICES: Deficit developing countries would object that the design privileges creditors and starves deficit members of liquidity — they were largely absent from the 1944 negotiation and speak only through Fund conditionality. U.S. domestic full-employment advocates would object that external discipline overrides the Employment Act mandate. Both stand outside the drafting coalition that wrote the creditor claims into the Articles.
% DISAPPEARANCE_RATIONALE: If the obligation vanished overnight, the par-value system loses its anchor: parities drift apart, the dollar's premium as settlement asset erodes, creditor gold claims are extinguished, and the U.S. recovers full monetary autonomy while the trading order reorganizes around either floating rates or a newly negotiated anchor — approximately the sequence the post-1971 world actually executed.
% FOUNDING_PROBLEM: Restore multilateral trade and payments after the interwar breakdown: competitive devaluations, exchange controls, discriminatory clearing blocs, and the Depression-era collapse of the classical gold standard had fragmented world commerce. The Articles sought stable parities under a credible anchor, with gold-dollar convertibility supplying the anchor's credibility.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: monetary historians of the interwar period (Kindleberger, Eichengreen, Bordo) document the dysfunction the founders targeted; the 1969 creation of the SDR and the post-1971 persistence of managed exchange rates show participants in every camp still treating orderly liquidity provision as an open problem; debtor-member statements in Fund annual meetings attest the problem from outside the creditor set. The creditor nations also attest it, but they are beneficiaries, so the load-bearing corroboration is the historical and debtor-side record.
narrative_ontology:disappearance_verdict(dollar_gold_convertibility__strict_convertibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(dollar_gold_convertibility__strict_convertibility_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dollar_gold_convertibility__strict_convertibility_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dollar_gold_convertibility__strict_convertibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dollar_gold_convertibility__strict_convertibility_reading, 0.7, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is high (0.70 at interval end) because the obligation decoupled U.S. domestic policy preferences from feasible policy: the Interest Equalization Tax (1963), voluntary foreign credit restraint programs, and Operation Twist are documented instances of U.S. policy bending to parity defense rather than domestic conditions. Suppression (0.60) reflects foreclosure of alternatives — floating rates and devaluation were understood and openly debated (Friedman's advocacy gained traction through the 1960s) but ruled out by alliance politics and by the U.S. leadership identity; accessibility_collapse (0.48) records that exits were visible, not collapsed. Resistance (0.58) captures active pushback from both sides: U.S. workarounds and eventual repudiation, and French conversion campaigns against the dollar's reserve role. Theater_ratio (0.42) rises across the interval: after the March 1968 gold crisis ended the Gold Pool, the two-tier market maintained the official price as an admitted fiction — performative maintenance of a parity the market no longer confirmed. The measurement series share one time grid (points 0, 5, 10, 15, 20, 25 mapping 1946-1971) with every tracked metric authored at every point; trajectories are ratchet-shaped rather than cyclical, each crisis episode (1960, 1965, 1968, 1971) leaving enforcement machinery heavier. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled, by directionality and the global scope's verification difficulty.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical treaty text. From the creditor seat the arrangement is an enforceable contract and a matter of elementary justice — Jacques Rueff's complaint was precisely that the reserve-currency country ran 'deficits without tears.' From the U.S. policymaker seat the same clauses are a straitjacket on democratically mandated macro policy, defended at the cost of capital controls that distorted private flows. From the deficit developing-country seat the system is largely beside the point or hostile — stability without liquidity, assistance with conditionality. From the Fund supervisor seat it is an interpretive exercise in 'fundamental disequilibrium.' The engine derives these divergent per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. Creditor nation central banks sit nearest the beneficiary pole (declared beneficiaries with arbitrage-grade exit: they convert or hold at will, and the option itself is their leverage). Surplus exporting member states are beneficiaries with mobile exit — low d, damped effective extraction. Fixed rate trading partners are beneficiaries with constrained exit — nearer symmetric, since stability benefits arrive alongside transmitted tightening. U.S. monetary policymakers are declared victims with identity_locked exit (the system's architect cannot repudiate its own cornerstone without dissolving the leadership identity constituted by it), placing them near the full-target pole; fiscal expansion administrations are victims with constrained exit and immediate horizons, also high d. Deficit developing members are excluded voices — neither beneficiaries nor victims of this constraint as read, contributing no directionality. No directionality overrides are needed: the derivation from declarations plus exit options reproduces the intended structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — interwar monetary chaos, competitive devaluation, discriminatory clearing — remained live across the entire interval, and the arrangement persisted because the problem persisted: the live-status x world-rearranges pairing is coherent, so no zombie/capture mismatch is expected. The tangled_rope classification prevents mislabeling in both directions: a pure-snare reading would ignore that the system delivered twenty-five years of trade expansion and a working settlements asset; a pure-rope reading would ignore that the U.S. bore asymmetric, actively enforced costs through the same structure that coordinated everyone else. The constraint's death in 1971 came by repudiation (deliberate suspension), not by atrophy — categorically distinct from piton decay, and the rising theater_ratio traces the performative phase that preceded the repudiation rather than a long inertial afterlife.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint instantiates the strict_convertibility_reading of the dollar_gold_convertibility kernel; do the sibling readings (policy_flexible_reading, triffin_structural_reading) instantiate structurally different constraints with materially different epsilon on U.S. policy space?',
    'Author and compare the sibling stories: the flexible reading should author lower epsilon (self-judging, domestically subordinate obligation) and likely a rope-leaning profile; the triffin reading should relocate the defect to systemic design rather than to any seat. Divergence across the family confirms per-reading classification.',
    'If the siblings converge on this reading''s profile, the kernel is less contested than the committer frame assumes; if they diverge as expected, aggregating across readings is invalid and each reading keeps its own epsilon, beneficiaries, and victims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer-frame uncertainty: this is one of three readings of the convertibility kernel.').

omega_variable(
    legal_bindingness_status,
    'Was the U.S. convertibility undertaking a justiciable legal obligation or a politically self-judging commitment?',
    'Drafting history of the Articles of Agreement, contemporaneous Treasury and Federal Reserve legal opinions, state practice on par-value maintenance, and scholarly treatment of the conversion undertaking''s enforceability.',
    'If self-judging, epsilon drops sharply and the arrangement trends toward cooperative rope (voluntary discipline); if justiciable and binding, the tangled_rope profile with the U.S. as target stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legal_bindingness_status, conceptual, 'Whether the convertibility commitment carried legal or merely political force.').

omega_variable(
    creditor_forbearance_dependence,
    'How much of the constraint''s binding force depended on voluntary creditor restraint (Germany and Japan holding dollars rather than converting) rather than on the obligation itself?',
    'Counterfactual conversion analysis using the historical record: the 1965-1968 French conversion campaign and the 1971 run model gold-stock depletion under fuller exercise of creditor claims.',
    'If bindingness rested mostly on forbearance, effective suppression and extraction fall and the arrangement looks cooperative; if the claims alone sufficed to discipline, the enforced-extraction profile stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(creditor_forbearance_dependence, empirical, 'Dependence of the obligation''s bite on creditor cooperation.').

omega_variable(
    adjustment_burden_distribution,
    'Did the arrangement''s adjustment burden fall primarily on the U.S. issuer, as this reading asserts, or disproportionately on deficit developing countries under Fund conditionality?',
    'Comparative balance-of-payments adjustment histories 1950-1971: isolate U.S. policy changes attributable to gold loss versus conditionality-imposed austerity in deficit members.',
    'If the burden was broadly shared or fell hardest on deficit members, the U.S.-as-target framing weakens and the beneficiary/victim sets need revision, shifting the profile toward rope with diffuse targets.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adjustment_burden_distribution, empirical, 'Who actually bore the system''s adjustment costs.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dollar_gold_convertibility__strict_convertibility_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doll_tr_t0, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(doll_tr_t5, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 5, 0.16).
narrative_ontology:measurement(doll_tr_t10, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(doll_tr_t15, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 15, 0.3).
narrative_ontology:measurement(doll_tr_t20, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 20, 0.37).
narrative_ontology:measurement(doll_tr_t25, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 25, 0.42).

% Extraction over time
narrative_ontology:measurement(doll_be_t0, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(doll_be_t5, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(doll_be_t10, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(doll_be_t15, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 15, 0.64).
narrative_ontology:measurement(doll_be_t20, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(doll_be_t25, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 25, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(doll_su_t0, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 0, 0.34).
narrative_ontology:measurement(doll_su_t5, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 5, 0.4).
narrative_ontology:measurement(doll_su_t10, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(doll_su_t15, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 15, 0.55).
narrative_ontology:measurement(doll_su_t20, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(doll_su_t25, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 25, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dollar_gold_convertibility__strict_convertibility_reading, resource_allocation).
narrative_ontology:affects_constraint(dollar_gold_convertibility__strict_convertibility_reading, policy_flexible_reading).
narrative_ontology:affects_constraint(dollar_gold_convertibility__strict_convertibility_reading, triffin_structural_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Bretton Woods convertibility' covers three structurally distinct claims instantiated as three readings of the dollar_gold_convertibility kernel. This story (strict reading) authors epsilon ~0.70 against U.S. domestic policy space, with the U.S. in the victim set and creditor nations as beneficiaries with enforceable claims. The policy_flexible_reading authors a lower epsilon (the obligation yields to domestic stability) and would classify differently; the triffin_structural_reading treats the arrangement as an unsustainable design and locates the pathology in the architecture rather than in any seat's extraction. Each file carries its own epsilon, stakeholders, and classification; they are linked here because the strict reading's operation (actual redemptions draining the gold stock) supplies the empirical pressure to which the triffin reading responds, and the flexible reading is the interpretive stance the U.S. increasingly acted on before formalizing it in 1971.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
