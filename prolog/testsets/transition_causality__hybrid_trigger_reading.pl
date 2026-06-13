% ============================================================================
% CONSTRAINT STORY: transition_causality__hybrid_trigger_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_transition_causality__hybrid_trigger_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: transition_causality__hybrid_trigger_reading
 *   human_readable: Bretton Woods Bretton Woods Stability Mechanism (Hybrid Trigger Reading)
 *   domain: economic/political
 *
 * SUMMARY:
 *   The Bretton Woods monetary system (1944–1971) created a stable
 *   exchange-rate mechanism with the US dollar pegged to gold at $35 per
 *   ounce and all other currencies pegged to the dollar. This reading frames
 *   the system's collapse as the outcome of hybrid causality: the Triffin
 *   Dilemma (structural contradiction that the dollar cannot simultaneously
 *   supply unlimited liquidity AND remain convertible to gold at fixed
 *   parity) accumulated inexorably, BUT the specific timing and form of
 *   collapse depended on contingent trigger events—the Vietnam War's fiscal
 *   shock (1965+) and the French gold runs (de Gaulle's 1965 call for return
 *   to gold standard)—without which the system might have persisted longer or
 *   transitioned differently. The constraint is CLAIMED as tangled_rope
 *   (genuine coordination function + asymmetric extraction + active
 *   enforcement) while measuring substantial extractiveness (0.68) and
 *   moderate suppression (0.42); the measurement series shows monotonic rise
 *   in extraction and theater from 1944 to 1971 on a shared grid.
 *
 * KEY AGENTS:
 *   - US monetary authority: sets the gold parity, reaps seigniorage, runs deficits
 *   - Dollar-pegged exporters (Germany, Japan, UK): benefit from coordination and trade stability, subordinated to US monetary policy
 *   - Gold-reserve holders (central banks): locked into dollar peg by convention and confidence, cannot easily liquidate reserves
 *   - France: excluded from constraint-setting, explicitly contests dollar dominance and demands gold convertibility
 *   - Economists (Triffin, Kindleberger): identify the structural contradiction but debate its fatality
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(transition_causality__hybrid_trigger_reading, 0.68).
domain_priors:suppression_score(transition_causality__hybrid_trigger_reading, 0.42).
domain_priors:theater_ratio(transition_causality__hybrid_trigger_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(transition_causality__hybrid_trigger_reading, tangled_rope).
narrative_ontology:human_readable(transition_causality__hybrid_trigger_reading, "Bretton Woods Bretton Woods Stability Mechanism (Hybrid Trigger Reading)").
narrative_ontology:topic_domain(transition_causality__hybrid_trigger_reading, "economic/political").

domain_priors:requires_active_enforcement(transition_causality__hybrid_trigger_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(transition_causality__hybrid_trigger_reading, '4252281c-d8fc-470b-878d-4ef12ca2a0d1').
narrative_ontology:cs_kernel_codification('4252281c-d8fc-470b-878d-4ef12ca2a0d1', fixed_text).
narrative_ontology:cs_authority_grounding('4252281c-d8fc-470b-878d-4ef12ca2a0d1', lineage).
narrative_ontology:cs_interpretation_layer_present('4252281c-d8fc-470b-878d-4ef12ca2a0d1').
narrative_ontology:cs_reading_relation('4252281c-d8fc-470b-878d-4ef12ca2a0d1', transition_causality__contingent_choice_reading, coexists_with).
narrative_ontology:cs_reading_relation('4252281c-d8fc-470b-878d-4ef12ca2a0d1', transition_causality__overdetermined_collapse_reading, influences).
narrative_ontology:cs_axiom('4252281c-d8fc-470b-878d-4ef12ca2a0d1', foundational, structural_contradiction_with_contingent_triggers).
narrative_ontology:cs_axiom_status(structural_contradiction_with_contingent_triggers, holdable).
narrative_ontology:cs_axiom_grounding('4252281c-d8fc-470b-878d-4ef12ca2a0d1', structural_contradiction_with_contingent_triggers, empirically_contingent).
narrative_ontology:cs_axiom('4252281c-d8fc-470b-878d-4ef12ca2a0d1', foundational, triffin_dilemma_as_binding_constraint).
narrative_ontology:cs_axiom_status(triffin_dilemma_as_binding_constraint, holdable).
narrative_ontology:cs_axiom_grounding('4252281c-d8fc-470b-878d-4ef12ca2a0d1', triffin_dilemma_as_binding_constraint, empirically_contingent).
narrative_ontology:cs_reference_frame('4252281c-d8fc-470b-878d-4ef12ca2a0d1', dollar_peg_bretton_woods_commitment).
narrative_ontology:cs_drift_state('4252281c-d8fc-470b-878d-4ef12ca2a0d1', vietnam_war_era_1965_1971, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('4252281c-d8fc-470b-878d-4ef12ca2a0d1', '').
narrative_ontology:cs_kernel_id(transition_causality__hybrid_trigger_reading, transition_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(transition_causality__hybrid_trigger_reading, us_monetary_authority).
narrative_ontology:constraint_beneficiary(transition_causality__hybrid_trigger_reading, dollar_pegged_exporters).
narrative_ontology:constraint_victim(transition_causality__hybrid_trigger_reading, gold_reserve_holders).
narrative_ontology:constraint_victim(transition_causality__hybrid_trigger_reading, deficit_nations).
narrative_ontology:constraint_victim(transition_causality__hybrid_trigger_reading, subordinate_central_banks).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(transition_causality__hybrid_trigger_reading, deficit_fiscal_authority).
narrative_ontology:constraint_victim(transition_causality__hybrid_trigger_reading, dollar_pegged_exporters).
narrative_ontology:constraint_victim(transition_causality__hybrid_trigger_reading, deficit_fiscal_authority).
narrative_ontology:constraint_vindicates(transition_causality__hybrid_trigger_reading, hegemonic_stability_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Federal Reserve and US Treasury set the dollar parity at $35/oz and commit to gold redemption on demand. They administer the system, manage sterilization operations to manage gold outflows, coordinate with other central banks through the BIS and bilateral agreements, and run large fiscal deficits (especially after 1965) that increase dollar liabilities beyond gold backing. They benefit from seigniorage (the ability to print dollars and use them to buy real goods) and from US hegemonic status in security and trade. They suppress information about gold reserve adequacy and deflect French and other central banks' demands for reform.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, us_monetary_authority, agenda_setter,
    institutional, generational, arbitrage, global).

% Western European (Germany, UK, France) and Japanese exporters gain from the stable dollar peg: they can price goods in dollars, access US capital markets, and rely on predictable exchange rates for trade planning. They also benefit from the US security umbrella (NATO, bilateral treaties) in exchange for monetary subordination. But they import US inflation (especially post-1965), and their central banks must hold depreciating dollar reserves as part of the system. Germany and Japan especially face the dilemma: leaving the dollar system would destroy their export markets, but staying requires absorbing US monetary policy.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, dollar_pegged_exporters, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(transition_causality__hybrid_trigger_reading, dollar_pegged_exporters, payer).

% Central banks and treasuries worldwide hold gold reserves theoretically to back their own currencies and provide confidence. But under Bretton Woods, the gold is structurally locked: they cannot easily demand redemption without triggering a bank-run panic, cannot revalue it without admitting the dollar is overvalued, and cannot substitute it with other assets without losing the 'safe' identity that makes them reserves. They bear the cost of the system: as US deficits increase dollar supply, the gold backing ratio declines, and their reserve assets lose real purchasing power. Their exit is identity-locked: the whole concept of 'gold reserves' implies restraint and confidence, so exercising the redemption right appears as panic-selling and destroys the identity.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, gold_reserve_holders, payer,
    moderate, biographical, identity_locked, global).

% Central banks of countries pegged to the dollar (Canada, Belgium, Netherlands, Denmark) are constrained by their export markets and balance-of-payments positions. They must hold dollars as reserves, peg their currencies to the dollar, and import US monetary policy. When the US inflates, they inflate; when the US runs deficits, the deficits appear as capital inflows that they must sterilize or absorb. They cannot unilaterally devalue or unpeg without destroying trade relationships and invoking capital flight. Their exit is constrained by the fundamental dependency of their export markets on the dollar system.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, subordinate_central_banks, payer,
    moderate, biographical, constrained, global).

% The US fiscal authority (Congress, Executive) benefits from the ability to run large deficits and finance them at low rates because foreign central banks are forced to hold dollars. The Vietnam War (1965+) and Great Society programs are financed through deficit spending, which increases dollar liabilities and pressures the gold reserve. The fiscal authority treats the constraint's extraction as a cost worth bearing for the geopolitical benefits (military reach, aid allocation, domestic spending). But fiscal deficits increase monetary pressure on the system, directly causing the triggers that actualize the structural contradiction.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, deficit_fiscal_authority, beneficiary,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(transition_causality__hybrid_trigger_reading, deficit_fiscal_authority, payer).

% France under de Gaulle (from 1960 onward, explicit challenge in 1965) contests the dollar's privileged position and calls for return to a true gold standard. De Gaulle sees the dollar system as a form of US imperialism disguised as coordination. France can threaten to redeem dollars for gold, and has enough reserves and independence (withdrawal from NATO command, alliance with USSR on some issues) to make the threat credible. France is excluded from the constraint-setting process because it is a NATO ally nominally committed to the system, but its voice would demand fundamental reform if heard. The French gold demands become a trigger event that crystallizes the underlying structural contradiction.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, france, excluded,
    powerful, biographical, mobile, global).

% USSR and developing nations are excluded from Bretton Woods (or included only nominally). They have no vote on system rules and cannot call gold redemption—they are not part of the dollar-reserve system. Their exclusion from decision-making means their interests in commodity prices, colonial trade arrangements, and monetary autonomy are not represented. They would object most strongly to the system if they had formal voice, but are trapped outside it.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, soviet_union_and_non_aligned_nations, excluded,
    organized, generational, trapped, global).

% Economists like Robert Triffin (1960), Charles Kindleberger, and John Williamson identify and publicize the structural contradiction in the Bretton Woods system. They point out that the US cannot simultaneously maintain a fixed gold parity AND supply unlimited dollars for global liquidity growth. They debate whether the system is doomed (structural necessity) or salvageable (policy choice), and whether reform should move toward a multilateral gold-standard, a composite reserve currency, or floating rates. Their analysis circulates among policymakers but does not change the fundamental constraint or its enforcement.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, economists_and_policy_analysts, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified global exchange-rate system pegged to the US dollar, which is fixed to gold. Solves the multi-party coordination problem of determining relative currency values without the instability of negotiated bilateral rates or the deflationary pressure of a pure gold standard. Enables growth in international trade and capital flows by providing a stable, trusted numeraire for pricing and settlement. Supplies global liquidity (dollar creation) to finance trade growth without requiring economically destructive gold mining or deflation.
% TRANSFER_FUNCTION: Transfers seigniorage from dollar creation to the US (the ability to print dollars and use them to purchase real goods and services without equivalent production). Transfers inflation costs to gold-reserve holders and subordinate central-bank nations (as US deficits increase dollar supply, the purchasing power of dollar reserves declines, and the dollar loses backing in gold). Transfers the fiscal costs of US foreign policy and domestic spending (Vietnam War, Great Society) to the rest of the world by forcing them to hold depreciating dollar reserves or lose trade access.
% ABSENT_VOICES: France (excluded from constraint-setting despite being a member; formally committed to the system but contests it from within). USSR and non-aligned nations (excluded entirely; not party to Bretton Woods and cannot call gold redemption or contest the system's rules). Developing nations and commodity exporters (excluded or weakly represented; their interests in commodity-price stability and monetary autonomy are not represented). Workers and taxpayers in deficit nations (diffusely affected by inflation but not represented in central-bank or Treasury decision-making).
% DISAPPEARANCE_RATIONALE: If the constraint (dollar peg to gold at $35/oz with convertibility guarantee) vanished overnight, the coordination problem does not disappear, but the specific institutional solution collapses. Exchange rates would re-equilibrate rapidly; trade pricing would shift to either a new numeraire (gold, SDR, currency basket, floating rates) or bilateral negotiation. Capital flows would reorient away from dollar-denominated assets; international trade would require higher transaction costs or multiple settlement currencies until a new system emerged. All stakeholders would reorganize: the US loses seigniorage; reserve-holding nations gain the ability to revalue and diversify; exporters face higher exchange-rate uncertainty. The world does not collapse without Bretton Woods (trade and finance existed before it), but the specific arrangement's disappearance triggers large-scale rearrangement.
% FOUNDING_PROBLEM: Post-WWII international monetary chaos: the gold standard had collapsed in the 1930s; most currencies were inconvertible; exchange rates were unstable, often set administratively rather than by markets; trade was restricted by bilateral agreements and capital controls. Economically devastated Europe and Japan needed a reliable medium of exchange (the dollar) and a stable reference point (the gold parity) to reconstruct international trade and finance without the deflationary pressures of a pure gold standard or the uncertainty of floating rates.
% FOUNDING_PROBLEM_CORROBORATION: US officials, British Keynesians (Maynard Keynes, John Williamson), and American economic historians (Eckes, Steil) attest the founding problem was acute in 1944–1950 and justify ongoing Bretton Woods operation on those grounds through the 1950s and early 1960s. By 1965–1971, economists (Triffin, Kindleberger, international economic historians), European officials (especially France, Germany), and British Treasury assessments attest the founding problem was substantially solved—post-war reconstruction was complete, European and Japanese exports boomed, and trade had returned to near pre-Depression levels—and the constraint now operates as US privilege extraction. Developing-nation economists and French officials explicitly corroborate the shifted-function reading from outside the US beneficiary set, arguing that the system has become a mechanism for US monetary hegemony disguised as coordination.
narrative_ontology:disappearance_verdict(transition_causality__hybrid_trigger_reading, world_rearranges).
narrative_ontology:founding_problem_status(transition_causality__hybrid_trigger_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(transition_causality__hybrid_trigger_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(transition_causality__hybrid_trigger_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(transition_causality__hybrid_trigger_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(transition_causality__hybrid_trigger_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(transition_causality__hybrid_trigger_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.15 at founding (genuine coordination need, low abuse of privilege) to 0.68 by 1971 (system running on US fiscal deficit and seigniorage, not on genuine shared benefit). Theater rises slowly (0.05→0.28) because the system maintains its coordination story (stable rates, trade growth) throughout, but an increasing share of enforcement energy is devoted to defending US monetary privilege rather than to the coordination function itself. Suppression is moderate and stable (0.15→0.42) because the constraint persists through institutional hierarchy and currency interdependence, not through overt coercion—subordinate central banks are trapped by their own export markets and balance-of-payments dynamics, not by gunboat diplomacy. The measurement grid aligns at year-boundaries; every metric is authored at every sampled year. The hybrid-trigger reading locates the inflection at the Vietnam War (1965) and French challenge (1965), where the structural contradiction becomes acute, but the system hangs on through institutional inertia and confidence mechanisms until the final break in 1971.
 *
 * PERSPECTIVAL GAP:
 *   From the US seat, the system is genuine coordination it provides and maintains—the constraint is rope with modest efficiency cost. From the reserve-holder seat, the system is extraction dressed as coordination—the constraint is tangled_rope approaching snare. From the French seat, the constraint is pure US privilege extraction using coordination language as cover. The engine computes this divergence from the power atoms and exit options: US institutional + arbitrage exit → low d (beneficiary seat); subordinate central banks moderate + constrained → high d (target seat); France powerful + mobile → intermediate but contested d. This perspectival gap IS the engine's measurement; it is not resolved by the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   US monetary authority: institutional power, arbitrage exit (can always print dollars, set policy unilaterally) → d near 0.1 (full beneficiary). Dollar-pegged exporters: organized power, constrained exit (tied to US markets and finance) → d near 0.4 (moderate asymmetry; they benefit from coordination but lose from inflation import). Gold-reserve holders: moderate power, identity-locked exit (convention that gold is 'safe' prevents liquidation without triggering panic) → d near 0.75 (high target). Subordinate central banks: moderate power, constrained exit (bound by trade and capital flows) → d near 0.7 (target). France: powerful, mobile exit (can demand gold, threaten withdrawal) → d near 0.5–0.6 (contested; powerful enough to contest, but still pegged to dollar markets). The beneficiary set is {us_monetary_authority, dollar_pegged_exporters} and the victim set is {gold_reserve_holders, subordinate_central_banks, deficit_nations} — with France as the excluded agent that would most strongly contest if inside.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid-trigger reading resists mandatrophy collapse by declaring a genuine coordination function that solves the post-WWII exchange-rate problem, AND declaring asymmetric extraction that grows over time as US fiscal deficits (Vietnam War, Great Society) burden the rest of the world. The system is NOT pure coordination (rope) because extraction is substantial and growing; it is NOT pure extraction (snare) because the coordination function is real and valued by beneficiaries; it is tangled_rope because both functions coexist. The theater_ratio stays low-to-moderate (0.05→0.28) because enforcement activity does track real coordination needs (managing exchange volatility, enabling trade) throughout the interval; the rise is steady but not explosive, indicating the constraint is not mostly performative. The reading avoids the mandatrophy trap by explicitly coupling the classified type (tangled_rope) to the measured trajectory (rising extraction, modest theater): the claim and the metrics are independent and in TENSION, which is exactly what the classification system exists to detect.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    structural_vs_contingent_causality,
    'Was the Bretton Woods collapse caused by the Triffin Dilemma as a structural contradiction (inevitable once dollar liabilities exceeded gold reserves), or did the collapse depend essentially on contingent events (Vietnam War shock, French gold demands) that could have been averted or delayed?',
    'Counterfactual analysis: model the system''s trajectory under alternative fiscal policies (no Vietnam escalation) or alternative monetary policies (Fed sterilization, Roosa bonds, SDR expansion) with empirical parameters from the historical record. If plausible trajectories exist where the system persists past 1971, contingency is vindicated; if all pathways converge to collapse within a 5-year window, necessity is vindicated.',
    'If the collapse was CONTINGENT on trigger timing, the hybrid-trigger reading is correct and policy alternatives existed; the system was not doomed but vulnerable to shocks. If the collapse was NECESSARY (structural), the overdetermined-collapse reading is correct and trigger events merely timing. Classification consequence: the reading''s core claim pins the classification of all three sibling readings—if structural necessity is vindicated, contingent_choice_reading recalibrates toward false consciousness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_vs_contingent_causality, empirical, 'Whether Bretton Woods collapse was structurally determined or contingent-trigger dependent.').

omega_variable(
    triffin_dilemma_sharpness,
    'How sharp was the mathematical contradiction in the Triffin Dilemma? Did the dollar''s liabilities exceed gold reserves by a critical threshold that made collapse inevitable once breached, or was the constraint slack enough that policy adjustment (reduced deficits, gold-price adjustment, or reserve-asset alternatives) could have sustained the system indefinitely?',
    'Historical accounting: reconstruct the Fed''s gold reserve, dollar liabilities, and reserve adequacy ratios year by year. Establish thresholds at which central banks'' confidence models predict bank-run dynamics. Compare threshold-breach timing to trigger events (Vietnam escalation, French demands) to determine if triggers preceded or followed the critical threshold.',
    'If triggers preceded threshold breach, the constraint was structural but not yet critical; contingency is vindicated. If triggers followed breach, the constraint was already critical; necessity is vindicated. If the two coincided, causality is ambiguous and the hybrid reading holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(triffin_dilemma_sharpness, empirical, 'Quantitative sharpness of the Triffin Dilemma''s binding constraint.').

omega_variable(
    policy_alternatives_counterfactual,
    'What policy alternatives existed at 1965–1968 (the trigger window) to manage the gold-outflow crisis without Bretton Woods'' collapse? Could fiscal consolidation, monetary tightening, gold revaluation, or a modified gold-exchange standard have sustained the system?',
    'Detailed historical reconstruction of policy options considered (Roosa bonds, SDR creation, Fort Knox gold sales proposals) and why they were rejected or failed. Interviews with policymakers and economic historians on counterfactuals: would Bretton Woods have persisted if Vietnam was not escalated, or if de Gaulle''s calls for gold convertibility were accommodated earlier?',
    'If plausible alternatives existed and were rejected for political reasons (US unwilling to accept fiscal discipline, allies unwilling to coordinate), contingency and policy choice are vindicated. If alternatives were analyzed and rejected for economic reasons (insufficient scale, insufficient credibility), necessity dominates. The hybrid reading requires that alternatives existed but were not taken because of the contingent conjunction of fiscal shock and French challenge.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_alternatives_counterfactual, conceptual, 'Whether credible policy alternatives could have sustained Bretton Woods past 1971.').

omega_variable(
    identity_lock_on_gold_reserve_holders,
    'Why did gold-reserve-holding central banks not demand gold conversion en masse once the Triffin Dilemma became widely understood (post-1960)? Was it structural (no alternative coordination mechanism existed), institutional (central banks internalized restraint as part of their role), or psychological (the ''safer'' identity of gold holders prevented them from acting on their own interests)?',
    'Archival study of central bank policy debates and correspondence 1960–1971; behavioral analysis of why known-problematic constraints persist; comparison with the French exception (where de Gaulle broke the restraint identity). If France was structurally unique (political independence from US security umbrella), identity-lock was the mechanism; if other central banks faced identical structural constraints but chose restraint, psychology/internalization dominated.',
    'If identity-lock was the binding mechanism, it is a suppression mechanism endemic to the constraint''s design—benefiting the US and harming reserve holders by preventing exit. If structural dependence was the mechanism, it is not specific to Bretton Woods but inherent to any hegemonic system. Reclassifying which mechanism dominated changes the attribution of extraction: it may be less about US privilege and more about the structure of international finance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_on_gold_reserve_holders, empirical, 'Why gold-reserve holders did not exploit their redemption rights during the Triffin crisis.').

omega_variable(
    france_as_exogenous_vs_endogenous_shock,
    'Was France''s gold demand (de Gaulle, 1965) an exogenous political shock to the system, or an endogenous response to structural pressure that would have materialized from some other actor if France had not acted?',
    'Counterfactual: model the gold-outflow dynamics without France''s demands. Did US gold reserves reach critical levels solely from subordinate central banks'' ordinary reserve diversification? Or did France''s explicit challenge accelerate an already-incipient process? Examine statements from other potential challengers (Germany, Belgium) to assess whether they would have demanded gold conversion absent French leadership.',
    'If France was exogenous (idiosyncratic political choice), then the hybrid-trigger reading is correct: the Dilemma was structural but France''s contingent demand made it acute. If France was endogenous (first-mover among rational actors), then the trigger was structural and France merely named the inevitable; the overdetermined-collapse reading is correct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(france_as_exogenous_vs_endogenous_shock, empirical, 'Whether France''s gold demands were contingent political choice or inevitable economic response.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(transition_causality__hybrid_trigger_reading, 1944, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tran_tr_t1944, transition_causality__hybrid_trigger_reading, theater_ratio, 1944, 0.05).
narrative_ontology:measurement_basis(tran_tr_t1944, observed).
narrative_ontology:measurement(tran_tr_t1950, transition_causality__hybrid_trigger_reading, theater_ratio, 1950, 0.08).
narrative_ontology:measurement_basis(tran_tr_t1950, observed).
narrative_ontology:measurement(tran_tr_t1958, transition_causality__hybrid_trigger_reading, theater_ratio, 1958, 0.12).
narrative_ontology:measurement_basis(tran_tr_t1958, observed).
narrative_ontology:measurement(tran_tr_t1963, transition_causality__hybrid_trigger_reading, theater_ratio, 1963, 0.18).
narrative_ontology:measurement_basis(tran_tr_t1963, observed).
narrative_ontology:measurement(tran_tr_t1967, transition_causality__hybrid_trigger_reading, theater_ratio, 1967, 0.24).
narrative_ontology:measurement_basis(tran_tr_t1967, observed).
narrative_ontology:measurement(tran_tr_t1971, transition_causality__hybrid_trigger_reading, theater_ratio, 1971, 0.28).
narrative_ontology:measurement_basis(tran_tr_t1971, observed).

% Extraction over time
narrative_ontology:measurement(tran_be_t1944, transition_causality__hybrid_trigger_reading, base_extractiveness, 1944, 0.15).
narrative_ontology:measurement_basis(tran_be_t1944, observed).
narrative_ontology:measurement(tran_be_t1950, transition_causality__hybrid_trigger_reading, base_extractiveness, 1950, 0.22).
narrative_ontology:measurement_basis(tran_be_t1950, observed).
narrative_ontology:measurement(tran_be_t1958, transition_causality__hybrid_trigger_reading, base_extractiveness, 1958, 0.35).
narrative_ontology:measurement_basis(tran_be_t1958, observed).
narrative_ontology:measurement(tran_be_t1963, transition_causality__hybrid_trigger_reading, base_extractiveness, 1963, 0.48).
narrative_ontology:measurement_basis(tran_be_t1963, observed).
narrative_ontology:measurement(tran_be_t1967, transition_causality__hybrid_trigger_reading, base_extractiveness, 1967, 0.62).
narrative_ontology:measurement_basis(tran_be_t1967, observed).
narrative_ontology:measurement(tran_be_t1971, transition_causality__hybrid_trigger_reading, base_extractiveness, 1971, 0.68).
narrative_ontology:measurement_basis(tran_be_t1971, observed).

% Suppression requirement over time
narrative_ontology:measurement(tran_su_t1944, transition_causality__hybrid_trigger_reading, suppression_requirement, 1944, 0.15).
narrative_ontology:measurement_basis(tran_su_t1944, observed).
narrative_ontology:measurement(tran_su_t1950, transition_causality__hybrid_trigger_reading, suppression_requirement, 1950, 0.22).
narrative_ontology:measurement_basis(tran_su_t1950, observed).
narrative_ontology:measurement(tran_su_t1958, transition_causality__hybrid_trigger_reading, suppression_requirement, 1958, 0.3).
narrative_ontology:measurement_basis(tran_su_t1958, observed).
narrative_ontology:measurement(tran_su_t1963, transition_causality__hybrid_trigger_reading, suppression_requirement, 1963, 0.36).
narrative_ontology:measurement_basis(tran_su_t1963, observed).
narrative_ontology:measurement(tran_su_t1967, transition_causality__hybrid_trigger_reading, suppression_requirement, 1967, 0.41).
narrative_ontology:measurement_basis(tran_su_t1967, observed).
narrative_ontology:measurement(tran_su_t1971, transition_causality__hybrid_trigger_reading, suppression_requirement, 1971, 0.42).
narrative_ontology:measurement_basis(tran_su_t1971, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(transition_causality__hybrid_trigger_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(transition_causality__hybrid_trigger_reading, 0.22).
narrative_ontology:affects_constraint(transition_causality__hybrid_trigger_reading, transition_causality__contingent_choice_reading).
narrative_ontology:affects_constraint(transition_causality__hybrid_trigger_reading, transition_causality__overdetermined_collapse_reading).
narrative_ontology:affects_constraint(transition_causality__hybrid_trigger_reading, triffin_dilemma_structural_contradiction).
narrative_ontology:affects_constraint(transition_causality__hybrid_trigger_reading, us_monetary_hegemony).

% DUAL FORMULATION NOTE:
% This is one reading of a three-way kernel contest over Bretton Woods' collapse causality. The hybrid-trigger reading claims structural contradiction (Triffin Dilemma) as NECESSARY but NOT SUFFICIENT; contingent triggers (Vietnam War, French gold demands) are SUFFICIENT to actualize collapse but not necessary (alternative transitions might have occurred without them). Siblings: contingent_choice_reading (all causality is policy choice; no structural necessity); overdetermined_collapse_reading (multiple reinforcing contradictions make collapse inevitable regardless of trigger timing). The three readings partition the logical space and affect each other through counterfactual dependencies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(transition_causality__hybrid_trigger_reading, powerful, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
