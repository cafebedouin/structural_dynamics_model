% ============================================================================
% CONSTRAINT STORY: monetary_anchor_principle__punctuated_swap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_monetary_anchor_principle__punctuated_swap_reading, []).

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
 *   constraint_id: monetary_anchor_principle__punctuated_swap_reading
 *   human_readable: Nixon Shock — Punctuated Swap Reading
 *   domain: monetary_economics/political_economy/international_finance
 *
 * SUMMARY:
 *   This story models the Nixon Shock as a punctuated institutional choice —
 *   a discrete swap from the Bretton Woods gold-exchange standard to a
 *   floating fiat regime, executed by executive authority on August 15, 1971.
 *   The reading treats the transition as a coordination failure (unilateral
 *   defection) rather than structural inevitability. The constraint is the
 *   post-1971 monetary anchor principle: the dollar's value is anchored by
 *   U.S. policy discretion, not gold convertibility. This is one of three
 *   readings of the monetary_anchor_principle kernel; the other two
 *   (overdetermined_composite_reading, triffin_inevitability_reading) frame
 *   the same transition as structurally overdetermined or Triffin-inevitable.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monetary_anchor_principle__punctuated_swap_reading, 0.45).
domain_priors:suppression_score(monetary_anchor_principle__punctuated_swap_reading, 0.35).
domain_priors:theater_ratio(monetary_anchor_principle__punctuated_swap_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monetary_anchor_principle__punctuated_swap_reading, rope).
narrative_ontology:human_readable(monetary_anchor_principle__punctuated_swap_reading, "Nixon Shock — Punctuated Swap Reading").
narrative_ontology:topic_domain(monetary_anchor_principle__punctuated_swap_reading, "monetary_economics/political_economy/international_finance").

domain_priors:requires_active_enforcement(monetary_anchor_principle__punctuated_swap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monetary_anchor_principle__punctuated_swap_reading, '46cf253c-029b-4130-bfb4-b3bd38a68da5').
narrative_ontology:cs_kernel_codification('46cf253c-029b-4130-bfb4-b3bd38a68da5', fixed_text).
narrative_ontology:cs_authority_grounding('46cf253c-029b-4130-bfb4-b3bd38a68da5', lineage).
narrative_ontology:cs_interpretation_layer_present('46cf253c-029b-4130-bfb4-b3bd38a68da5').
narrative_ontology:cs_reading_relation('46cf253c-029b-4130-bfb4-b3bd38a68da5', monetary_anchor_principle__overdetermined_composite_reading, coexists_with).
narrative_ontology:cs_reading_relation('46cf253c-029b-4130-bfb4-b3bd38a68da5', monetary_anchor_principle__triffin_inevitability_reading, influences).
narrative_ontology:cs_axiom('46cf253c-029b-4130-bfb4-b3bd38a68da5', foundational, august_15_1971_discrete_executive_choice).
narrative_ontology:cs_axiom_status(august_15_1971_discrete_executive_choice, holdable).
narrative_ontology:cs_axiom_grounding('46cf253c-029b-4130-bfb4-b3bd38a68da5', august_15_1971_discrete_executive_choice, empirically_contingent).
narrative_ontology:cs_axiom('46cf253c-029b-4130-bfb4-b3bd38a68da5', secondary, floating_regime_restores_policy_autonomy).
narrative_ontology:cs_axiom_status(floating_regime_restores_policy_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('46cf253c-029b-4130-bfb4-b3bd38a68da5', floating_regime_restores_policy_autonomy, instrumental).
narrative_ontology:cs_reference_frame('46cf253c-029b-4130-bfb4-b3bd38a68da5', bretton_woods_gold_exchange_standard).
narrative_ontology:cs_drift_state('46cf253c-029b-4130-bfb4-b3bd38a68da5', august_15_1971_suspension, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('46cf253c-029b-4130-bfb4-b3bd38a68da5', '').
narrative_ontology:cs_kernel_id(monetary_anchor_principle__punctuated_swap_reading, monetary_anchor_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__punctuated_swap_reading, us_fiscal_authority).
narrative_ontology:constraint_victim(monetary_anchor_principle__punctuated_swap_reading, foreign_dollar_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__punctuated_swap_reading, us_domestic_economy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exercises unilateral authority to suspend gold convertibility on August 15, 1971. Gains immediate fiscal autonomy to run deficits without gold reserve constraint and to devalue the dollar against foreign currencies. Controls the rule change and collects the seigniorage benefit of the new regime.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, us_fiscal_authority, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(monetary_anchor_principle__punctuated_swap_reading, us_fiscal_authority, beneficiary).

% Central banks and governments holding dollar reserves as gold-backed claims. On August 15, 1971, their assets are devalued overnight with no compensation. Exit options are constrained: they can sell dollars (accelerating devaluation), negotiate Smithsonian Agreement adjustments, or accept the new floating regime. No individual holder can enforce convertibility.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, foreign_dollar_holders, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(monetary_anchor_principle__punctuated_swap_reading, foreign_dollar_holders, payer).

% Benefits from the policy space opened by ending gold convertibility: expansionary monetary policy, competitive devaluation boosting exports, freedom from external discipline. The arrangement subsidizes domestic employment and output targets at the expense of foreign holders.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, us_domestic_economy, beneficiary,
    powerful, biographical, mobile, national).

% IMF and World Bank — their founding charter is built on the adjustable peg system. The swap renders their surveillance and lending framework incoherent. They administer the transition to floating rates (Smithsonian Agreement, Jamaica Agreement) but did not author the initial defection.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, bretton_woods_institutions, observer,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(monetary_anchor_principle__punctuated_swap_reading, bretton_woods_institutions, agenda_setter).

% Economists and policymakers who argue gold convertibility is the only anchor preventing inflationary bias. Their structural position is excluded from the decision — the swap is executed by executive order without legislative debate or international consultation. They bear the epistemic cost of being proven wrong or right only in retrospect.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, gold_standard_advocates, excluded,
    moderate, civilizational, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solved the international liquidity problem: the U.S. supplied dollars to a growing world economy, and foreign holders accepted dollars as good-as-gold reserves. The peg coordinated expectations about exchange rates and price stability across the Bretton Woods system.
% TRANSFER_FUNCTION: Moves the exorbitant privilege of seigniorage and fiscal autonomy from a shared gold-disciplined system to the U.S. fiscal authority alone. Foreign dollar holders absorb the devaluation loss; the U.S. gains the policy space to finance Vietnam War deficits and Great Society spending without gold outflow constraint.
% ABSENT_VOICES: Foreign dollar holders (especially European central banks) were not consulted on the August 15 decision — they learned of the suspension by public announcement. Gold standard advocates in Congress and academia were excluded from the executive decision. The decision was made by a small circle in the Nixon administration.
% DISAPPEARANCE_RATIONALE: If the August 15 suspension and its floating-rate aftermath vanished overnight, the world would revert to a gold-convertible dollar standard — requiring the U.S. to either contract its money supply dramatically or deplete remaining gold reserves. Global trade and finance would reorganize around the restored peg. The arrangement is not a natural fact; it is a regime that structures the monetary world.
% FOUNDING_PROBLEM: The Bretton Woods gold-exchange standard required the U.S. to supply global liquidity by running deficits, but those deficits eroded the gold backing that made dollars acceptable as reserves. The founding problem was the contradiction between supplying the world's money and maintaining its gold convertibility.
% FOUNDING_PROBLEM_CORROBORATION: Triffin (1960) identified the dilemma before the swap. The overdetermined_composite_reading and triffin_inevitability_reading both corroborate that the structural contradiction was recognized by independent analysts before 1971. The U.S. fiscal authority's own contemporaneous internal memos (Volcker, Burns) acknowledge the gold drain was unsustainable. The founding problem is dead — the contradiction was resolved by abandoning convertibility, not by fixing it.
narrative_ontology:disappearance_verdict(monetary_anchor_principle__punctuated_swap_reading, world_rearranges).
narrative_ontology:founding_problem_status(monetary_anchor_principle__punctuated_swap_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monetary_anchor_principle__punctuated_swap_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(monetary_anchor_principle__punctuated_swap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(monetary_anchor_principle__punctuated_swap_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(monetary_anchor_principle__punctuated_swap_reading_tests).
:- end_tests(monetary_anchor_principle__punctuated_swap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45 at the swap point) because the constraint extracts from foreign holders via devaluation but also provides genuine coordination (floating rates absorb shocks that would break a peg). Suppression is moderate (0.35) — the constraint is enforced by U.S. monetary sovereignty and the lack of any alternative reserve asset in 1971, not by active repression of alternatives. Theater is low (0.15) — the swap is presented as a temporary measure but the functional shift is real. The metrics peak at the 1971 decision point and moderate afterward as the floating regime stabilizes.
 *
 * PERSPECTIVAL GAP:
 *   The U.S. fiscal authority experiences the swap as liberation from an untenable constraint (rope: coordination problem solved by regime change). Foreign dollar holders experience it as extraction (snare: their assets devalued by executive fiat). The engine computes this seat divergence from the structural data — the same constraint is rope from the setter's seat, extractive from the holder's seat.
 *
 * DIRECTIONALITY LOGIC:
 *   U.S. fiscal authority is the agenda-setter and primary beneficiary (d ~ 0.15): it controls the rule change and gains fiscal autonomy. Foreign dollar holders are the primary payers/victims (d ~ 0.85): they hold the devalued claims with constrained exit. U.S. domestic economy is a secondary beneficiary (d ~ 0.3): gains policy space but bears inflation risk. Bretton Woods institutions are observers/administrators of the transition (d ~ 0.5). Gold standard advocates are excluded (d ~ 0.9): their structural position is foreclosed by the swap.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Triffin dilemma) was real and recognized. The swap resolved it by abandoning the gold anchor — the mandatrophy is resolved (the constraint no longer serves its founding coordination function under gold discipline). The post-1971 anchor principle is a NEW constraint, not the old one persisting without its function. The theater rise after 1971 (0.15→0.22) reflects the performative maintenance of 'strong dollar' rhetoric over a floating regime.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    punctuated_vs_overdetermined_boundary,
    'Is the August 15, 1971 swap a genuine punctuated choice (the U.S. could have chosen otherwise, e.g. devaluation with gold convertibility maintained) or was the choice itself forced by the structural pressures the overdetermined_composite_reading identifies?',
    'Counterfactual analysis of the Nixon administration''s decision space in summer 1971: were Smithsonian Agreement adjustments (December 1971) a viable alternative path that would have preserved convertibility at a new parity?',
    'If the choice was genuinely discretionary, the constraint is rope (coordination problem solved by agency). If forced, it is mountain-like (structural inevitability misread as choice) or tangled_rope (coordination + extraction where the coordination story covers structural compulsion).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(punctuated_vs_overdetermined_boundary, conceptual, 'Whether the punctuated swap reading''s core premise (discrete choice) is structurally distinct from the overdetermined composite reading.').

omega_variable(
    extraction_reversibility,
    'Is the extraction from foreign dollar holders reversible in principle (the U.S. could re-peg or compensate), or is the expropriation structurally locked in by the floating regime''s path dependence?',
    'Historical test: the Smithsonian Agreement (1971) and Jamaica Agreement (1976) both failed to restore gold convertibility. The floating regime persisted. Reversibility would require a voluntary return to gold discipline — which no major economy has done since 1971.',
    'If reversible, epsilon is moderate and the constraint is rope/scaffold. If locked in, epsilon is higher and the constraint drifts toward tangled_rope (the coordination function of floating rates becomes the cover for permanent extraction).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_reversibility, empirical, 'Whether the moderate extraction at the swap point accumulates or reverses over the floating regime''s lifespan.').

omega_variable(
    kernel_reading_location,
    'Where exactly do the three readings of monetary_anchor_principle disagree structurally — on the extractiveness of the post-1971 regime, on the naturalness of the transition, on the identity of the constraint itself?',
    'Comparative epsilon assessment: each reading authors epsilon for the SAME referent (the post-1971 standing arrangement). The punctuated_swap_reading authors moderate epsilon (institutional choice). The triffin_inevitability_reading likely authors higher epsilon (inevitable extraction). The overdetermined_composite_reading authors epsilon reflecting structural compulsion. The disagreement is on epsilon''s value for a shared referent.',
    'If readings author different epsilon for the same referent, the kernel is a genuine contested classification site. If they author epsilon for different referents (e.g. one for the transition event, one for the post-1971 regime), the kernel label conflates distinct constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Structural location of the disagreement among the three kernel readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monetary_anchor_principle__punctuated_swap_reading, 1960, 1976).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mapi_psr_tr_t1960, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 1960, 0.05).
narrative_ontology:measurement(mapi_psr_tr_t1965, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 1965, 0.08).
narrative_ontology:measurement(mapi_psr_tr_t1968, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 1968, 0.12).
narrative_ontology:measurement(mapi_psr_tr_t1971, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 1971, 0.15).
narrative_ontology:measurement(mapi_psr_tr_t1973, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 1973, 0.18).
narrative_ontology:measurement(mapi_psr_tr_t1976, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 1976, 0.22).

% Extraction over time
narrative_ontology:measurement(mapi_psr_be_t1960, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 1960, 0.15).
narrative_ontology:measurement(mapi_psr_be_t1965, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 1965, 0.22).
narrative_ontology:measurement(mapi_psr_be_t1968, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 1968, 0.35).
narrative_ontology:measurement(mapi_psr_be_t1971, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 1971, 0.45).
narrative_ontology:measurement(mapi_psr_be_t1973, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 1973, 0.42).
narrative_ontology:measurement(mapi_psr_be_t1976, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 1976, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(mapi_psr_su_t1960, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 1960, 0.1).
narrative_ontology:measurement(mapi_psr_su_t1965, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 1965, 0.18).
narrative_ontology:measurement(mapi_psr_su_t1968, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 1968, 0.28).
narrative_ontology:measurement(mapi_psr_su_t1971, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 1971, 0.35).
narrative_ontology:measurement(mapi_psr_su_t1973, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 1973, 0.3).
narrative_ontology:measurement(mapi_psr_su_t1976, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 1976, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monetary_anchor_principle__punctuated_swap_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(monetary_anchor_principle__punctuated_swap_reading, 0.12).
narrative_ontology:affects_constraint(monetary_anchor_principle__punctuated_swap_reading, monetary_anchor_principle__overdetermined_composite_reading).
narrative_ontology:affects_constraint(monetary_anchor_principle__punctuated_swap_reading, monetary_anchor_principle__triffin_inevitability_reading).
narrative_ontology:affects_constraint(monetary_anchor_principle__punctuated_swap_reading, floating_rate_regime_stability).
narrative_ontology:affects_constraint(monetary_anchor_principle__punctuated_swap_reading, dollar_reserve_currency_status).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the monetary_anchor_principle kernel. The overdetermined_composite_reading and triffin_inevitability_reading are sibling constraints sharing the same kernel_id but with different structural deltas (epsilon, beneficiary/victim structure, claimed_type). All three form a constraint family linked by network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(monetary_anchor_principle__punctuated_swap_reading, institutional, 0.15).
constraint_indexing:directionality_override(monetary_anchor_principle__punctuated_swap_reading, organized, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
