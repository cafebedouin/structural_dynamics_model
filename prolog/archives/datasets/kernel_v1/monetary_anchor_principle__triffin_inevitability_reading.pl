% ============================================================================
% CONSTRAINT STORY: monetary_anchor_principle__triffin_inevitability_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_monetary_anchor_principle__triffin_inevitability_reading, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: monetary_anchor_principle__triffin_inevitability_reading
 *   human_readable: Triffin Dilemma: Mathematical Inevitability of Gold Standard Collapse
 *   domain: monetary_economics/international_finance
 *
 * SUMMARY:
 *   The Triffin dilemma, articulated by economist Robert Triffin in 1960,
 *   identifies a structural contradiction at the core of the Bretton Woods
 *   monetary system: a reserve currency issuer committed to redeeming its
 *   currency in gold at a fixed price must simultaneously allow that currency
 *   to become the global medium of international exchange. To supply adequate
 *   global liquidity for growing international trade, the reserve-currency
 *   issuer must run balance-of-payments deficits — printing currency in
 *   excess of export revenue to finance global credit. These deficits
 *   accumulate as foreign holdings of the reserve currency. Foreign agents
 *   holding that currency have the right (and eventually the incentive) to
 *   redeem it for gold at the pegged price. As deficits accumulate, foreign
 *   claims on gold reserves grow. Eventually, accumulated foreign claims
 *   exceed the issuer's gold reserves. The peg becomes unsustainable. On
 *   August 15, 1971, President Richard Nixon suspended convertibility, ending
 *   the Bretton Woods system. From the Triffin inevitability perspective,
 *   this was not a choice but a deduction: the mathematical contradiction
 *   between reserve-currency role and gold peg left no other outcome
 *   available. This reading treats the collapse as a mountain constraint — a
 *   structural impossibility that no policy choice could have indefinitely
 *   deferred. It stands in contrast to the punctuated-swap reading (which
 *   sees August 15, 1971 as a discrete institutional choice) and the
 *   overdetermined-composite reading (which emphasizes that multiple causal
 *   factors — Vietnam deficits, capital mobility, policy consensus shifts —
 *   made collapse inevitable by the late 1960s).
 *
 * KEY AGENTS:
 *   - The United States Federal Reserve and Treasury: Reserve-currency issuer (institutional/arbitrage) — trapped by the structural contradiction; no policy choice resolves the dilemma
 *   - The Bretton Woods Institutional Framework: Victim (institutional/trapped) — the framework itself is the locus of the contradiction; collapse is its structural destiny
 *   - Foreign Central Banks (France, Germany, UK): Secondary holders of claims (institutional/constrained) — accumulate dollar reserves and face eventual non-redemption
 *   - The International Monetary System: Victim (powerless/trapped) — the post-WWII monetary order cannot survive the mathematical impossibility at its core
 *   - Analytical Observer: Civilian (analytical/analytical) — recognizes the logical necessity; understands that blame for the collapse is misplaced (it was not a policy failure but a structural inevitability)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monetary_anchor_principle__triffin_inevitability_reading, 0.12).
domain_priors:suppression_score(monetary_anchor_principle__triffin_inevitability_reading, 0.02).
domain_priors:theater_ratio(monetary_anchor_principle__triffin_inevitability_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monetary_anchor_principle__triffin_inevitability_reading, mountain).
narrative_ontology:human_readable(monetary_anchor_principle__triffin_inevitability_reading, "Triffin Dilemma: Mathematical Inevitability of Gold Standard Collapse").
narrative_ontology:topic_domain(monetary_anchor_principle__triffin_inevitability_reading, "monetary_economics/international_finance").

domain_priors:emerges_naturally(monetary_anchor_principle__triffin_inevitability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monetary_anchor_principle__triffin_inevitability_reading, '7c70e2eb-1bc9-4f42-aba0-5a39ec604237').
narrative_ontology:cs_kernel_codification('7c70e2eb-1bc9-4f42-aba0-5a39ec604237', fixed_text).
narrative_ontology:cs_authority_grounding('7c70e2eb-1bc9-4f42-aba0-5a39ec604237', expertise).
narrative_ontology:cs_interpretation_layer_present('7c70e2eb-1bc9-4f42-aba0-5a39ec604237').
narrative_ontology:cs_reading_relation('7c70e2eb-1bc9-4f42-aba0-5a39ec604237', monetary_anchor_principle__punctuated_swap_reading, influences).
narrative_ontology:cs_reading_relation('7c70e2eb-1bc9-4f42-aba0-5a39ec604237', monetary_anchor_principle__overdetermined_composite_reading, coexists_with).
narrative_ontology:cs_axiom('7c70e2eb-1bc9-4f42-aba0-5a39ec604237', foundational, gold_scarcity_binds_reserve_supply).
narrative_ontology:cs_axiom_status(gold_scarcity_binds_reserve_supply, holdable).
narrative_ontology:cs_axiom_grounding('7c70e2eb-1bc9-4f42-aba0-5a39ec604237', gold_scarcity_binds_reserve_supply, empirically_contingent).
narrative_ontology:cs_axiom('7c70e2eb-1bc9-4f42-aba0-5a39ec604237', foundational, reserve_currency_role_incompatible_with_commodity_peg).
narrative_ontology:cs_axiom_status(reserve_currency_role_incompatible_with_commodity_peg, holdable).
narrative_ontology:cs_axiom_grounding('7c70e2eb-1bc9-4f42-aba0-5a39ec604237', reserve_currency_role_incompatible_with_commodity_peg, deontological).
narrative_ontology:cs_reference_frame('7c70e2eb-1bc9-4f42-aba0-5a39ec604237', gold_standard_viability_axiom).
narrative_ontology:cs_drift_state('7c70e2eb-1bc9-4f42-aba0-5a39ec604237', contemporary_post_bretton_woods, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('7c70e2eb-1bc9-4f42-aba0-5a39ec604237', '').
narrative_ontology:cs_kernel_id(monetary_anchor_principle__triffin_inevitability_reading, monetary_anchor_principle).

% --- Structural relationships ---
narrative_ontology:constraint_victim(monetary_anchor_principle__triffin_inevitability_reading, bretton_woods_institutional_framework).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRAPPED RESERVE CURRENCY ISSUER (MOUNTAIN) — The structural logic is inescapable: to supply global liquidity under gold standard, must run deficits. Deficits deplete gold reserves. Reserve depletion triggers collapse. No policy choice can resolve the contradiction. The agent is trapped by mathematical necessity, not policy failure. Accessible_collapse = 0.92: once the dilemma's structure is clear, the issuer's inability to escape is complete.
constraint_indexing:constraint_classification(monetary_anchor_principle__triffin_inevitability_reading, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: BRETTON WOODS INSTITUTIONAL FRAMEWORK (MOUNTAIN) — The framework itself cannot survive the mathematical contradiction. Even if policymakers possessed perfect foresight and infinite gold reserves, the structural tension (reserve-currency role + gold peg + global demand for liquidity) admits no institutional fix. The framework was always self-undermining. Resistance = 0.08: minimal institutional alternatives could have prevented the collapse; the collapse was not a failure of will or competence but a structural inevitability encoded in the rules themselves.
constraint_indexing:constraint_classification(monetary_anchor_principle__triffin_inevitability_reading, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (MOUNTAIN) — From the perspective of logical necessity, the Triffin dilemma is a mathematical impossibility claim: you cannot simultaneously (a) peg a currency to gold at a fixed price, (b) make that currency the global reserve medium, (c) allow global demand for reserves to exceed the underlying gold stock, and (d) keep the peg. One of these constraints must give. The collapse of the gold peg is not a contingent historical event but a deduction from the axioms. Extractiveness ≤ 0.12 and suppression ≤ 0.02 reflect that this is not an extractive mechanism but a structural limit.
constraint_indexing:constraint_classification(monetary_anchor_principle__triffin_inevitability_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(monetary_anchor_principle__triffin_inevitability_reading_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(monetary_anchor_principle__triffin_inevitability_reading, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(monetary_anchor_principle__triffin_inevitability_reading, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, ExtMetricName, E),
    domain_priors:suppression_score(monetary_anchor_principle__triffin_inevitability_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(monetary_anchor_principle__triffin_inevitability_reading),
    narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(monetary_anchor_principle__triffin_inevitability_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Very low. This reading treats the dilemma as a mathematical limit, not an extraction mechanism. The 'extraction' is not intentional or beneficial to any agent — it is the byproduct of an impossible structural position. The reserve-currency issuer gains seigniorage benefits from the dollar's reserve role, but once the contradiction becomes binding, these benefits evaporate. No agent intentionally extracts value; rather, the system generates a contradiction that no allocation of benefits and costs can resolve. The low value reflects that this is not a distributional problem amenable to negotiation or policy adjustment. Suppression (0.02): Minimal. This reading rejects the idea that the collapse was suppressed by coercion or alternatives-denial. The constraint is natural law, not institutional coercion. No alternatives were suppressed because the alternatives were logically impossible — not merely undiscussed. Theater ratio (0.08): Minimal. The Triffin mechanism is transparent. Once Triffin articulated it in 1960, the mechanism was public knowledge. The fact that the system continued for 11 more years does not reflect performative maintenance but rather genuine delay driven by temporary gold discoveries, capital controls, and coordination agreements (swap lines, Special Drawing Rights) that slowed but could not indefinitely prevent reserve depletion. Measurements show base_extractiveness rising from 0.02 (1945, system newly established) to 0.12 (1971, collapse), reflecting the accumulation of foreign dollar claims as the dilemma's structural pressure intensifies. Theater ratio remains low throughout, consistent with the transparency of the mechanism.
 *
 * PERSPECTIVAL GAP:
 *   The three perspectives all classify as mountain — the Triffin reading predicts uniform classification across all observation positions because the constraint is a logical necessity, not a distributional or institutional problem. The trapped issuer sees mathematical impossibility; the framework sees its own structural flaw; the analytical observer sees logical necessity. The gap is not between perspectives (all agree on the type) but between the logical necessity claim and competing readings that see the collapse as contingent on policy choice (overdetermined reading) or as a discrete institutional decision (punctuated-swap reading). Those alternative readings would produce snare, tangled_rope, or scaffold perspectives from the same agent positions — the classification disagreement is about whether the Triffin dilemma was necessary or merely significant among multiple causes.
 *
 * DIRECTIONALITY LOGIC:
 *   The Triffin reading assigns all agents to d ≈ 1.0 (victim position) because none can escape the constraint: the issuer is trapped by the requirement to maintain the peg while supplying liquidity; foreign holders are trapped by accumulating claims they eventually cannot redeem; the framework itself is trapped by the logical contradiction it encodes. This differs sharply from distributional constraints where agents have differential exit options. Here, all agents face the same structural impossibility, so directionality is uniform toward victimhood. The reserve-currency issuer might appear to be a beneficiary (seigniorage gains), but from the Triffin perspective, this benefit is illusory — it depends on the peg holding, which the dilemma guarantees will not hold indefinitely. Once the contradiction becomes binding, the 'beneficiary' position evaporates.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does not require mandatrophy resolution because extractiveness < 0.25 and classification is uniform across perspectives. No perspectival contradiction arises — all observers agree the constraint is a mountain. The mandatrophy would arise if a sibling reading (e.g., overdetermined_composite) claimed that the same structural data should classify as tangled_rope (mixed coordination and extraction). That disagreement would instantiate the mandatrophy: is the Bretton Woods system a natural law (mountain), a coordination problem with policy solutions (tangled_rope or rope), or a temporary institutional choice (scaffold)? The Triffin reading settles the mandatrophy in favor of mountain by denying that policy alternatives could have indefinitely sustained the system.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    triffin_dilemma_necessity_vs_contingency,
    'Was the gold standard''s collapse a logical necessity (Triffin reading) or a contingent outcome of specific policy choices and historical events (overdetermined or punctuated reading)?',
    'Counterfactual analysis: could different deficit management, different international coordination, or different capital control regimes have extended the gold standard indefinitely? Were there policies available in principle (not in practice, but in principle) that could have resolved the contradiction?',
    'If necessary: mountain classification confirmed. If contingent on policy: reclassify as tangled_rope (coordination problem with available solutions) or snare (systemic extraction via monetary policy). If contingent on discrete choice: reclassify as scaffold (temporary institutional arrangement with a chosen sunset).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(triffin_dilemma_necessity_vs_contingency, conceptual, 'Whether collapse was logically necessary or contingent on policy choices').

omega_variable(
    gold_supply_elasticity_bounds,
    'Were gold discovery rates and mining capacity in principle sufficient to meet global liquidity demand under Bretton Woods, or was scarcity inherent to the constraint?',
    'Historical mining data + counterfactual reserve availability analysis. If gold supply could have grown at 5%+ annually (matching broad money growth), the scarcity was not absolute. If gold supply was capped around 2-3% annually, scarcity was structural.',
    'If elastic supply possible: the constraint was policy choice (capital controls, reserve requirements, deficit spending), not mathematical impossibility. Reclassify as tangled_rope. If inelastic: supports mountain reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gold_supply_elasticity_bounds, empirical, 'Whether gold supply could have satisfied global liquidity demand').

omega_variable(
    international_coordination_alternative_paths,
    'Could have a coordinated shift to a non-commodity-backed reserve currency (SDR, basket peg, or fully fiduciary global money) have been negotiated ex ante, preventing the Triffin crisis?',
    'Historical record of monetary negotiations 1960-1971; analysis of why Keynes''s bancor proposal and subsequent SDR experiments failed to gain traction; game-theoretic analysis of commitment incentives for reserve-currency aspirants.',
    'If coordination was structurally possible: the transition was driven by coordination failure, not logical necessity. Reclassify as rope (pure coordination) or tangled_rope. If coordination was structurally impossible: supports mountain reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_coordination_alternative_paths, empirical, 'Whether coordinated alternative monetary arrangement could have prevented collapse').

omega_variable(
    reading_kernel_distinction,
    'This reading holds that the gold standard collapse was NECESSARY (a logical deduction from the Triffin dilemma). Do the sibling readings (overdetermined_composite and punctuated_swap) logically foreclose this reading, coexist with it, or influence it?',
    'Examine whether the sibling readings accept the Triffin dilemma as a true structural constraint. If yes, they coexist (both can be true; one emphasizes necessity, others emphasize additional causes or institutional discretion). If no, they foreclose (deny the dilemma''s force entirely).',
    'If forecloses: the readings are mutually incompatible; only one can be true. If coexists: multiple readings can be simultaneously held by different parties or communities. If influences: this reading constrains but does not rule out the siblings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_distinction, conceptual, 'Relationship between triffin_inevitability and sibling readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monetary_anchor_principle__triffin_inevitability_reading, 1945, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(triffin_tr_t1945, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 1945, 0.05).
narrative_ontology:measurement(triffin_tr_t1971, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 1971, 0.08).

% Extraction over time
narrative_ontology:measurement(triffin_be_t1945, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 1945, 0.02).
narrative_ontology:measurement(triffin_be_t1958, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 1958, 0.05).
narrative_ontology:measurement(triffin_be_t1965, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 1965, 0.1).
narrative_ontology:measurement(triffin_be_t1971, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 1971, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monetary_anchor_principle__triffin_inevitability_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(monetary_anchor_principle__triffin_inevitability_reading, 0.1).
narrative_ontology:affects_constraint(monetary_anchor_principle__triffin_inevitability_reading, monetary_anchor_principle__punctuated_swap_reading).
narrative_ontology:affects_constraint(monetary_anchor_principle__triffin_inevitability_reading, monetary_anchor_principle__overdetermined_composite_reading).
narrative_ontology:affects_constraint(monetary_anchor_principle__triffin_inevitability_reading, international_monetary_reserve_fragility).
narrative_ontology:affects_constraint(monetary_anchor_principle__triffin_inevitability_reading, fiat_currency_exorbitant_privilege).

% DUAL FORMULATION NOTE:
% The Triffin inevitability reading models the gold standard collapse as a logical necessity. Sibling readings in the same kernel model the collapse as contingent on policy (overdetermined) or as a discrete choice (punctuated). These are three readings of one contested kernel, not three separate constraints. The network links show dependencies: the inevitability reading provides a structural floor; the sibling readings add contingent causes or institutional discretion on top of that floor.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
