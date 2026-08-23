% ============================================================================
% CONSTRAINT STORY: dollar_gold_convertibility__policy_flexible_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dollar_gold_convertibility__policy_flexible_reading, []).

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
 *   constraint_id: dollar_gold_convertibility__policy_flexible_reading
 *   human_readable: Article IV Convertibility as Conditional Obligation Subordinate to Domestic Stability
 *   domain: international_political_economy/monetary_history/international_law
 *
 * SUMMARY:
 *   The IMF Article IV convertibility obligation (dollar-gold parity at
 *   $35/oz) is read by U.S. authorities as conditional on domestic economic
 *   stability — the 'fundamental disequilibrium' clause permits suspension or
 *   adjustment when full employment and output goals conflict with gold
 *   discipline. This reading emerges from the Bretton Woods compromise:
 *   White's dollar-centric design gave the U.S. the privilege of defining
 *   disequilibrium. Over 1944-1971, the conditional reading expands from rare
 *   safeguard to routine policy tool, enabling U.S. expansionary finance
 *   (Great Society, Vietnam) while foreign central banks accumulate dollars
 *   they cannot convert without triggering systemic collapse. The constraint
 *   is a tangled rope: it coordinates global trade through stable exchange
 *   rates (genuine coordination function) while extracting seigniorage and
 *   policy autonomy from dollar holders (asymmetric extraction). Active
 *   enforcement is required — the London Gold Pool (1961-68), swap lines, and
 *   moral suasion on foreign central banks all sustain the conditional
 *   reading against conversion pressure.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dollar_gold_convertibility__policy_flexible_reading, 0.68).
domain_priors:suppression_score(dollar_gold_convertibility__policy_flexible_reading, 0.55).
domain_priors:theater_ratio(dollar_gold_convertibility__policy_flexible_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dollar_gold_convertibility__policy_flexible_reading, tangled_rope).
narrative_ontology:human_readable(dollar_gold_convertibility__policy_flexible_reading, "Article IV Convertibility as Conditional Obligation Subordinate to Domestic Stability").
narrative_ontology:topic_domain(dollar_gold_convertibility__policy_flexible_reading, "international_political_economy/monetary_history/international_law").

domain_priors:requires_active_enforcement(dollar_gold_convertibility__policy_flexible_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dollar_gold_convertibility__policy_flexible_reading, '945deea5-ecc5-40c6-96b0-8db78fa092ff').
narrative_ontology:cs_kernel_codification('945deea5-ecc5-40c6-96b0-8db78fa092ff', formalized).
narrative_ontology:cs_authority_grounding('945deea5-ecc5-40c6-96b0-8db78fa092ff', lineage).
narrative_ontology:cs_interpretation_layer_present('945deea5-ecc5-40c6-96b0-8db78fa092ff').
narrative_ontology:cs_reading_relation('945deea5-ecc5-40c6-96b0-8db78fa092ff', dollar_gold_convertibility__strict_convertibility_reading, coexists_with).
narrative_ontology:cs_reading_relation('945deea5-ecc5-40c6-96b0-8db78fa092ff', dollar_gold_convertibility__triffin_structural_reading, influences).
narrative_ontology:cs_axiom('945deea5-ecc5-40c6-96b0-8db78fa092ff', foundational, domestic_stability_supremacy_over_convertibility).
narrative_ontology:cs_axiom_status(domestic_stability_supremacy_over_convertibility, holdable).
narrative_ontology:cs_axiom_grounding('945deea5-ecc5-40c6-96b0-8db78fa092ff', domestic_stability_supremacy_over_convertibility, instrumental).
narrative_ontology:cs_axiom('945deea5-ecc5-40c6-96b0-8db78fa092ff', foundational, fundamental_disequilibrium_as_self_judging).
narrative_ontology:cs_axiom_status(fundamental_disequilibrium_as_self_judging, holdable).
narrative_ontology:cs_axiom_grounding('945deea5-ecc5-40c6-96b0-8db78fa092ff', fundamental_disequilibrium_as_self_judging, conventional).
narrative_ontology:cs_reference_frame('945deea5-ecc5-40c6-96b0-8db78fa092ff', bretton_woods_original_compromise).
narrative_ontology:cs_drift_state('945deea5-ecc5-40c6-96b0-8db78fa092ff', pre_nixon_shock_1971, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('945deea5-ecc5-40c6-96b0-8db78fa092ff', '').
narrative_ontology:cs_kernel_id(dollar_gold_convertibility__policy_flexible_reading, dollar_gold_convertibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__policy_flexible_reading, us_treasury_fed).
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__policy_flexible_reading, us_domestic_economy).
narrative_ontology:constraint_victim(dollar_gold_convertibility__policy_flexible_reading, foreign_central_banks).
narrative_ontology:constraint_victim(dollar_gold_convertibility__policy_flexible_reading, foreign_governments_holding_dollars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__policy_flexible_reading, international_traders_firms).
narrative_ontology:constraint_victim(dollar_gold_convertibility__policy_flexible_reading, international_traders_firms).
narrative_ontology:constraint_vindicates(dollar_gold_convertibility__policy_flexible_reading, domestic_employment_priority_doctrine).
narrative_ontology:constraint_vindicates(dollar_gold_convertibility__policy_flexible_reading, fundamental_disequilibrium_clause).
narrative_ontology:constraint_vindicates(dollar_gold_convertibility__policy_flexible_reading, monetary_sovereignty_retention).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the dollar-gold convertibility obligation under IMF Article IV. Interprets 'fundamental disequilibrium' clause to justify suspending or adjusting convertibility when domestic employment and output goals require expansionary policy. Sets global monetary conditions through unilateral action (e.g., 1971 Nixon Shock). Collects seigniorage and policy autonomy; bears reputational cost when credibility erodes.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, us_treasury_fed, agenda_setter,
    institutional, generational, arbitrage, global).

% Hold dollar reserves as primary international liquidity under Bretton Woods. Forced to absorb U.S. inflationary policy transmitted through fixed exchange rates; cannot easily exit dollar system without disrupting their own trade and reserve adequacy. Bear devaluation risk when U.S. exploits conditional obligation. Some (e.g., Bundesbank, Bank of France) resist through gold conversion demands, but collective action is fragmented.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, foreign_central_banks, payer,
    powerful, biographical, constrained, global).

% Sovereigns accumulating dollar reserves for trade financing and intervention. Experience direct wealth transfer when dollar devalues relative to gold. Politically constrained from diversifying by alliance structures (NATO, Cold War alignment) and lack of credible alternative reserve asset. Their complaint is structural: the conditional reading makes their reserves a call option on U.S. domestic policy.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, foreign_governments_holding_dollars, payer,
    organized, biographical, constrained, global).

% Gains monetary policy autonomy to pursue full employment and output targets without gold-convertibility discipline. The flexible reading legitimizes expansionary policy (Great Society, Vietnam War financing) that would otherwise trigger gold outflows. Benefits are diffuse across workers, firms, and fiscal authorities; costs of eventual inflation are delayed and partly exported.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, us_domestic_economy, beneficiary,
    organized, biographical, mobile, national).

% Monitors compliance with Article IV obligations. Formally the guardian of the convertibility rule, but in practice accommodates U.S. interpretation of 'fundamental disequilibrium' because the Fund's legitimacy depends on U.S. participation. Produces Article IV consultations that document but rarely constrain U.S. policy. Its authority is derivative: it interprets the kernel but does not set it.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, imf_surveillance, observer,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(dollar_gold_convertibility__policy_flexible_reading, imf_surveillance, agenda_setter).

% Benefit from stable exchange rates and dollar-centered invoicing that lowers transaction costs. Pay indirectly through imported inflation when U.S. policy exploits the conditional obligation. Can hedge currency risk but cannot escape the system's structural asymmetry: they price in dollars, so U.S. policy shocks transmit directly to their margins.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, international_traders_firms, beneficiary,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(dollar_gold_convertibility__policy_flexible_reading, international_traders_firms, payer).

% Argue that convertibility must be strict and automatic; any conditionality destroys the discipline that makes the system credible. Their voices are marginalized in official IMF discourse after 1960s; the policy_flexible_reading becomes orthodoxy among Western policymakers. They re-emerge only after the system collapses (1971-73) as critics of the fiat regime.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, gold_standard_advocates, excluded,
    moderate, generational, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable nominal anchor for the post-war international monetary system: fixed dollar-gold parity and fixed exchange rates enable trade expansion, capital flow predictability, and confidence in dollar as reserve asset. The conditional obligation is meant to handle genuine 'fundamental disequilibrium' without systemic collapse.
% TRANSFER_FUNCTION: Moves monetary policy autonomy from the gold constraint to the U.S. authorities, and moves devaluation risk and inflationary consequences from the U.S. domestic economy to foreign dollar holders. The transfer is asymmetric: U.S. gains seigniorage and countercyclical capacity; foreign central banks accumulate depreciating claims.
% ABSENT_VOICES: Gold-standard disciplinarians (excluded stakeholders) who would insist on automatic convertibility; developing countries outside the dollar bloc who had no voice in Bretton Woods design but were subjected to its asymmetries; future generations who inherit the fiat system's inflationary bias. The excluded are not in the room because the kernel's authority structure (IMF Articles) only recognizes signatory states, and the U.S. veto power shapes interpretation.
% DISAPPEARANCE_RATIONALE: If the conditional reading vanished and convertibility became strict/automatic (strict_convertibility_reading), U.S. monetary policy would be bound by gold outflows, ending the Great Society/Vietnam financing mix and likely triggering earlier recession. Foreign dollar holders would gain gold-convertibility certainty but lose the liquidity expansion that funded global trade growth. The Triffin dilemma would force a different systemic resolution (SDR expansion, gold revaluation, or managed float) — the world rearranges around a different stability/autonomy tradeoff.
% FOUNDING_PROBLEM: Post-war reconstruction required a monetary system that combined exchange rate stability (for trade) with sufficient liquidity expansion (for growth). The gold standard's pre-war rigidity caused deflationary spirals; pure fiat was politically unacceptable. The conditional convertibility design — dollar pegged to gold, other currencies pegged to dollar, with 'fundamental disequilibrium' escape clause — was the negotiated compromise at Bretton Woods (1944).
% FOUNDING_PROBLEM_CORROBORATION: Bretton Woods negotiators (Keynes, White) documented the intent: Keynes wanted an automatic adjustment mechanism (ICU), White insisted on dollar centrality with limited flexibility. The 'fundamental disequilibrium' clause was deliberately ambiguous — U.S. delegates (White) understood it as preserving U.S. policy space; European delegates understood it as a rare safeguard. Triffin (1960) testified the design flaw was structural, not a temporary disequilibrium. No single party's account is definitive; the founding problem's status is contested by design.
narrative_ontology:disappearance_verdict(dollar_gold_convertibility__policy_flexible_reading, world_rearranges).
narrative_ontology:founding_problem_status(dollar_gold_convertibility__policy_flexible_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dollar_gold_convertibility__policy_flexible_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dollar_gold_convertibility__policy_flexible_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dollar_gold_convertibility__policy_flexible_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dollar_gold_convertibility__policy_flexible_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dollar_gold_convertibility__policy_flexible_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dollar_gold_convertibility__policy_flexible_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.15 to 0.68 as U.S. policy increasingly exploits the conditional reading: early years (1944-58) see genuine gold discipline; post-1958, U.S. deficits become structural and the conditional reading becomes the mechanism for exporting inflation. Suppression (0.55) reflects the active machinery needed to prevent conversion runs — Gold Pool, swap network, political pressure on allies — not passive acceptance. Theater ratio (0.25) is moderate: the coordination function (stable rates, trade growth) is real but declining; by 1971 the enforcement apparatus mainly defends the extraction. Accessibility collapse (0.45) is partial: alternatives (SDRs, gold revaluation, floating) exist but are politically blocked. Resistance (0.60) is high: foreign central banks (France, Germany) repeatedly demand conversion, challenge the reading, and ultimately force the 1971 closure.
 *
 * PERSPECTIVAL GAP:
 *   From the U.S. agenda-setter seat, the conditional reading is legitimate coordination: the 'fundamental disequilibrium' clause was designed for exactly this purpose — preventing domestic depression. From foreign central bank payer seats, the same structure is extractive: the clause is invoked not for rare crises but as permanent cover for inflationary finance. The IMF observer seat experiences tension: its mandate requires defending convertibility, but its funding and legitimacy require U.S. acquiescence. The engine computes these divergences from the structural data; the claimed_type (tangled_rope) captures the dual nature without adjudicating which seat is 'right.'
 *
 * DIRECTIONALITY LOGIC:
 *   U.S. Treasury/Fed is structural beneficiary (d ~ 0.15): collects seigniorage, sets global monetary conditions, exits gold constraint at will. Foreign central banks and governments are structural victims (d ~ 0.85): trapped in dollar system by reserve adequacy needs and alliance politics, bear devaluation risk without consent. IMF Surveillance sits near symmetric (d ~ 0.5): formally guardian of the rule, practically accommodator of U.S. interpretation — its authority derives from the kernel it cannot control. U.S. domestic economy is beneficiary (d ~ 0.2): gains policy space but inherits delayed inflation. International traders are near symmetric: gain transaction-cost reduction, pay imported inflation. The conditional reading shifts extractiveness from domestic policy (gold discipline) to external creditors (dollar holders) — exactly the structural delta described.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-war stability + liquidity) was live in 1944. By 1960, Triffin demonstrated the design flaw: the system cannot simultaneously provide liquidity and maintain convertibility. The conditional reading resolves the tension by sacrificing convertibility credibility for liquidity — but this was not the founding bargain. The mandatrophy is unresolved: the arrangement persists past its founding justification (status=contested) because the U.S. benefits and foreign holders are trapped. The conditional reading becomes a piton-in-the-making: the coordination function atrophies (gold discipline evaporates) but the constraint (dollar centrality) persists through 1971 and beyond into the fiat era.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the policy_flexible_reading a legitimate interpretation of Article IV or a post-hoc rationalization for U.S. policy drift?',
    'Compare White''s negotiating record (1944) with subsequent U.S. policy statements (1958-71). If White intended the clause as U.S. policy space, the reading is authentic; if White understood it as rare safeguard, the reading is a rationalization.',
    'If authentic, the conditional obligation is a designed feature (coordination with escape valve); if rationalization, the extraction is opportunistic — the constraint was a mountain (binding law) that became a snare through reinterpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the conditional reading reflects original intent or opportunistic drift.').

omega_variable(
    extraction_allocation,
    'How much of the measured extractiveness (0.68 at 1971) is attributable to the conditional reading itself versus exogenous factors (Vietnam War, Great Society, productivity slowdown)?',
    'Counterfactual simulation: model U.S. balance of payments under strict convertibility (gold standard discipline) vs. actual policy. The difference in foreign reserve accumulation and inflation export isolates the reading''s extraction contribution.',
    'If most extraction is exogenous, the conditional reading is a minor enabler; if the reading itself generates the bulk, it is the primary extraction mechanism — a tangled_rope where the coordination function is a thin veneer.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_allocation, empirical, 'Attribution of extraction to the reading vs. exogenous policy choices.').

omega_variable(
    foreign_holder_agency,
    'Did foreign central banks have credible exit options (gold conversion, SDR diversification, bilateral swap networks) that would reduce their victim status?',
    'Analyze actual conversion demands (France 1965, Germany 1968-70) and their outcomes. If conversion was technically possible but politically suppressed, exit_options = constrained; if technically blocked (Gold Pool), exit_options = trapped.',
    'If trapped, the extraction is more snare-like; if constrained, the tangled_rope classification holds — victims have agency but face high coordination costs for collective exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foreign_holder_agency, empirical, 'Whether dollar holders'' victim status reflects structural entrapment or constrained choice.').

omega_variable(
    triffin_foreclosure,
    'Does the triffin_structural_reading foreclose the policy_flexible_reading, or do they coexist as diagnostic vs. prescriptive frames?',
    'Examine whether Triffin''s testimony (1960) was treated as a prediction of systemic collapse (coexists_with) or as a logical proof that conditional convertibility is impossible (forecloses). Policy response: if policymakers adopted Triffin''s SDR proposal as patch, they accepted coexistence; if they dismissed it, they treated it as foreclosed.',
    'If forecloses, the kernel has a logical contradiction at its core; if coexists_with, the policy_flexible_reading persists as a pragmatic patch until collapse.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(triffin_foreclosure, conceptual, 'Structural relationship between the flexible reading and the Triffin critique.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dollar_gold_convertibility__policy_flexible_reading, 1944, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dgc_pfr_tr_t1944, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 1944, 0.05).
narrative_ontology:measurement(dgc_pfr_tr_t1950, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 1950, 0.08).
narrative_ontology:measurement(dgc_pfr_tr_t1955, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 1955, 0.1).
narrative_ontology:measurement(dgc_pfr_tr_t1960, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 1960, 0.15).
narrative_ontology:measurement(dgc_pfr_tr_t1965, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 1965, 0.2).
narrative_ontology:measurement(dgc_pfr_tr_t1968, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 1968, 0.23).
narrative_ontology:measurement(dgc_pfr_tr_t1971, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 1971, 0.25).

% Extraction over time
narrative_ontology:measurement(dgc_pfr_be_t1944, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1944, 0.15).
narrative_ontology:measurement(dgc_pfr_be_t1950, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1950, 0.2).
narrative_ontology:measurement(dgc_pfr_be_t1955, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1955, 0.25).
narrative_ontology:measurement(dgc_pfr_be_t1960, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1960, 0.4).
narrative_ontology:measurement(dgc_pfr_be_t1965, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1965, 0.55).
narrative_ontology:measurement(dgc_pfr_be_t1968, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1968, 0.62).
narrative_ontology:measurement(dgc_pfr_be_t1971, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1971, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(dgc_pfr_su_t1944, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1944, 0.2).
narrative_ontology:measurement(dgc_pfr_su_t1950, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1950, 0.25).
narrative_ontology:measurement(dgc_pfr_su_t1955, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1955, 0.3).
narrative_ontology:measurement(dgc_pfr_su_t1960, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1960, 0.4).
narrative_ontology:measurement(dgc_pfr_su_t1965, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1965, 0.5).
narrative_ontology:measurement(dgc_pfr_su_t1968, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1968, 0.53).
narrative_ontology:measurement(dgc_pfr_su_t1971, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1971, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dollar_gold_convertibility__policy_flexible_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(dollar_gold_convertibility__policy_flexible_reading, 0.18).
narrative_ontology:affects_constraint(dollar_gold_convertibility__policy_flexible_reading, dollar_gold_convertibility__strict_convertibility_reading).
narrative_ontology:affects_constraint(dollar_gold_convertibility__policy_flexible_reading, dollar_gold_convertibility__triffin_structural_reading).
narrative_ontology:affects_constraint(dollar_gold_convertibility__policy_flexible_reading, bretton_woods_adjustment_mechanism).
narrative_ontology:affects_constraint(dollar_gold_convertibility__policy_flexible_reading, sdrs_creation_1969).
narrative_ontology:affects_constraint(dollar_gold_convertibility__policy_flexible_reading, nixon_shock_1971).
narrative_ontology:affects_constraint(dollar_gold_convertibility__policy_flexible_reading, petrodollar_recycling_1974).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the dollar_gold_convertibility kernel. The strict_convertibility_reading treats Article IV as binding law (Mountain-claim, low extraction); the triffin_structural_reading treats the design as inherently unstable (Tangled Rope at system level, high extraction). This reading (policy_flexible) is the operational interpretation that governed 1944-1971: conditional obligation enabling U.S. policy autonomy. The three stories form a constraint family linked by affects_constraints; their ε values diverge because they measure different structural arrangements (law-as-written, law-as-operated, system-as-designed).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dollar_gold_convertibility__policy_flexible_reading, institutional, 0.15).
constraint_indexing:directionality_override(dollar_gold_convertibility__policy_flexible_reading, powerful, 0.85).
constraint_indexing:directionality_override(dollar_gold_convertibility__policy_flexible_reading, organized, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
