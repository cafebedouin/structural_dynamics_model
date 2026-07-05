% ============================================================================
% CONSTRAINT STORY: price_formation_kernel__naturalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_price_formation_kernel__naturalist_reading, []).

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
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: price_formation_kernel__naturalist_reading
 *   human_readable: Price Formation as Natural Equilibrium (Naturalist Reading)
 *   domain: political_economy/housing_markets/institutional_analysis
 *
 * SUMMARY:
 *   The naturalist reading of the price-formation kernel holds that the price
 *   of housing (or any good) is the emergent output of decentralized,
 *   voluntary exchange among agents with subjective preferences confronting
 *   objectively scarce resources — a discovery process, not a construction.
 *   On this reading, an observed price is data about underlying scarcity and
 *   preference intensity, not a policy artifact; attempts to fix, cap, or
 *   steer it (rent control, price floors, discretionary allocation) do not
 *   redistribute value neutrally but destroy it, producing shortages,
 *   misallocation, and quality degradation (deadweight loss). This is one of
 *   four structurally distinct readings of a single contested kernel: the
 *   naturalist reading treats price formation as a Mountain (no
 *   beneficiaries, no victims, negligible extraction, near-total
 *   accessibility collapse to the equilibrium outcome). The institutional
 *   reading (constraint_id: institutional_reading, not this file) treats the
 *   SAME observable — the market-clearing price — as the output of zoning,
 *   lending standards, tax treatment, and platform intermediation, which does
 *   name beneficiaries and victims and computes as tangled_rope or worse. The
 *   georgist reading decomposes price into rent (unearned, site-value) and
 *   improvement (earned) components. The financialization reading attributes
 *   price movement to credit expansion and asset-feedback dynamics rather
 *   than to real scarcity/preference. These are not four measurements of one
 *   constraint; per the epsilon-invariance principle they are four separate
 *   constraints, linked here only by shared kernel identity, each with its
 *   own epsilon, its own metrics, its own file.
 *
 * KEY AGENTS:
 *   - Market participants (buyers/sellers/renters): observers within the mountain frame, price-takers whose revealed preferences and budget constraints are inputs to, not victims of, the equilibrium
 *   - Economic analysts (naturalist school): treat the observed price series as evidence of scarcity/preference dynamics, not as a policy lever
 *   - Policy interveners (rent-control boards, price-cap legislators): from the naturalist seat, their interventions are read as exogenous shocks that create deadweight loss rather than as legitimate re-allocations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(price_formation_kernel__naturalist_reading, 0.03).
domain_priors:suppression_score(price_formation_kernel__naturalist_reading, 0.02).
domain_priors:theater_ratio(price_formation_kernel__naturalist_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, extractiveness, 0.03).
narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(price_formation_kernel__naturalist_reading, mountain).
narrative_ontology:human_readable(price_formation_kernel__naturalist_reading, "Price Formation as Natural Equilibrium (Naturalist Reading)").
narrative_ontology:topic_domain(price_formation_kernel__naturalist_reading, "political_economy/housing_markets/institutional_analysis").

domain_priors:emerges_naturally(price_formation_kernel__naturalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(price_formation_kernel__naturalist_reading, '2a7e6ace-7788-4fcc-a6bb-6ba596163759').
narrative_ontology:cs_kernel_codification('2a7e6ace-7788-4fcc-a6bb-6ba596163759', distributed).
narrative_ontology:cs_authority_grounding('2a7e6ace-7788-4fcc-a6bb-6ba596163759', distributed).
narrative_ontology:cs_reading_relation('2a7e6ace-7788-4fcc-a6bb-6ba596163759', price_formation_kernel__institutional_reading, coexists_with).
narrative_ontology:cs_reading_relation('2a7e6ace-7788-4fcc-a6bb-6ba596163759', price_formation_kernel__georgist_reading, coexists_with).
narrative_ontology:cs_reading_relation('2a7e6ace-7788-4fcc-a6bb-6ba596163759', price_formation_kernel__financialization_reading, coexists_with).
narrative_ontology:cs_axiom('2a7e6ace-7788-4fcc-a6bb-6ba596163759', foundational, price_is_discovered_not_constructed).
narrative_ontology:cs_axiom_status(price_is_discovered_not_constructed, holdable).
narrative_ontology:cs_axiom_grounding('2a7e6ace-7788-4fcc-a6bb-6ba596163759', price_is_discovered_not_constructed, empirically_contingent).
narrative_ontology:cs_axiom('2a7e6ace-7788-4fcc-a6bb-6ba596163759', secondary, voluntary_exchange_yields_pareto_efficient_allocation).
narrative_ontology:cs_axiom_status(voluntary_exchange_yields_pareto_efficient_allocation, holdable).
narrative_ontology:cs_axiom_grounding('2a7e6ace-7788-4fcc-a6bb-6ba596163759', voluntary_exchange_yields_pareto_efficient_allocation, empirically_contingent).
narrative_ontology:cs_reference_frame('2a7e6ace-7788-4fcc-a6bb-6ba596163759', classical_market_clearing_equilibrium).
narrative_ontology:cs_drift_state('2a7e6ace-7788-4fcc-a6bb-6ba596163759', post_2008_financialized_housing_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2a7e6ace-7788-4fcc-a6bb-6ba596163759', '').
narrative_ontology:cs_kernel_id(price_formation_kernel__naturalist_reading, price_formation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_vindicates(price_formation_kernel__naturalist_reading, supply_demand_equilibrium_theorem).
narrative_ontology:constraint_vindicates(price_formation_kernel__naturalist_reading, subjective_value_theory).
narrative_ontology:constraint_vindicates(price_formation_kernel__naturalist_reading, deadweight_loss_of_price_controls).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None in the extractive sense — the naturalist reading holds that decentralized price signals coordinate the allocation of scarce housing stock among agents with heterogeneous preferences and budgets more efficiently than any centralized alternative, by aggregating dispersed information no single party possesses.
% TRANSFER_FUNCTION: None. On this reading nothing is systematically moved from an identifiable payer to an identifiable collector through the price mechanism itself; voluntary exchange is by construction mutually beneficial at the moment of trade (both parties revealed a preference for the trade over the status quo).
% ABSENT_VOICES: Renters and would-be buyers priced out of a market are not structurally 'excluded' on this reading in the sense of a suppressed alternative — the reading treats their exclusion as the direct expression of scarcity meeting preference intensity (others outbid them), not as a voice being silenced. The institutional_reading, georgist_reading, and financialization_reading siblings are where those same excluded parties' structural claims (that zoning, credit terms, or rent-seeking — not scarcity alone — priced them out) are actually adjudicated.
% DISAPPEARANCE_RATIONALE: The naturalist reading claims that if all policy interventions vanished, prices would not disappear or fundamentally rearrange the world — they would simply reflect scarcity and preference more purely and directly, which is presented as the baseline state, not a rearrangement. This is consistent with mountain status: removing this constraint does not reorganize anyone's arrangements because, on this reading, nothing artificial was ever holding the arrangement up.
% FOUNDING_PROBLEM: The naturalist reading is not a policy built to solve a problem — it is presented as a positive description of how decentralized exchange under scarcity necessarily behaves, analogous to a physical regularity rather than an institutional design choice.
% FOUNDING_PROBLEM_CORROBORATION: Mainstream neoclassical economists and many market participants attest the naturalist account describes real housing markets adequately outside of severely distorted cases. Institutional economists, land-value theorists (Georgist tradition), and post-Keynesian/financialization scholars — working from outside any naturalist-reading beneficiary group, since the naturalist reading itself names no beneficiaries — dispute that scarcity and preference alone explain observed housing-price dynamics, citing zoning-constrained supply, credit-driven demand feedback, and land-rent capture as first-order determinants. No corroborating source has been identified who defends the naturalist reading as a complete account of ACTUAL housing markets rather than as an idealized baseline case.
narrative_ontology:disappearance_verdict(price_formation_kernel__naturalist_reading, world_unchanged).
narrative_ontology:founding_problem_status(price_formation_kernel__naturalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(price_formation_kernel__naturalist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(price_formation_kernel__naturalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(price_formation_kernel__naturalist_reading, 0.03, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(price_formation_kernel__naturalist_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, ExtMetricName, E),
    domain_priors:suppression_score(price_formation_kernel__naturalist_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(price_formation_kernel__naturalist_reading),
    narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(price_formation_kernel__naturalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored near-zero (0.03) because the naturalist reading structurally denies that price formation itself transfers value from an identifiable payer to an identifiable collector — the price IS the coordination signal, and no party's position is asymmetric relative to it. Suppression is near-zero (0.02): no coercive apparatus is required to sustain a price arrived at through voluntary exchange; the 'enforcement' visible in real markets (contract law, property rights) is treated by this reading as a precondition for exchange, not as constraint-specific coercion aimed at extracting from a target. Accessibility collapse is high (0.88): once agents understand relative scarcity and their own preferences, alternative price levels are not viable equilibria — arbitrage collapses them. Resistance is low (0.12): those who resist are, on this reading, resisting scarcity itself (via price controls) rather than resisting an extractive structure, and that resistance is precisely what the reading interprets as the source of deadweight loss rather than as evidence of victimhood. Theater ratio is low (0.05): there is minimal performative activity in a genuinely emergent process — what theater exists is confined to the discourse defending the naturalist frame against interventionist critique.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiaries or victims are declared, consistent with the mountain claim: on this reading price formation has no structural target and no structural collector. This is a deliberate authoring choice reflecting the reading's own premises, not an oversight — the sibling institutional_reading is precisely the file where beneficiaries (landowners, incumbent zoning beneficiaries, lending intermediaries) and victims (excluded renters, would-be developers) are named for the SAME observable price outcome. The decomposition is required by the epsilon-invariance principle: this file's zero-beneficiary structure and the institutional file's populated beneficiary/victim arrays are not in tension: they are different constraints sharing a kernel.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy does not apply straightforwardly to a mountain, but the omega below documents where a mandatrophy-style question migrates for this reading: the founding claim ('price reflects scarcity and preference') could itself be defended past the point where it corresponds to reality (e.g., in markets with severe informational asymmetry, credit-driven feedback, or supply constrained by non-market rules) — at which point continued naturalist framing would function as ideological cover for the more extractive readings rather than as an accurate account of price discovery.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naturalist_frame_construction_ambiguity,
    'Is price formation genuinely a scarcity/preference discovery process (mountain), or does the naturalist frame itself function as ideological cover that benefits parties who profit from the constructed features (zoning scarcity, credit expansion, land-rent capture) being misread as natural?',
    'Compare price behavior across housing markets with materially different institutional structures (supply elasticity, zoning regimes, credit availability) holding underlying population/preference parameters roughly constant; if price divergence tracks institutional variables more than scarcity/preference variables, the naturalist frame is under-specified for that market.',
    'If institutional variables dominate, the naturalist reading is not wrong as a claim about SOME idealized market, but is mischaracterizing the actual observed housing-price mechanism, and the institutional_reading file becomes the operative constraint for policy purposes rather than this one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalist_frame_construction_ambiguity, conceptual, 'Whether the natural-law framing of price formation is genuine or ideologically load-bearing for beneficiaries of the constructed alternative readings.').

omega_variable(
    kernel_disagreement_location,
    'Where exactly do the four sibling readings of the price_formation_kernel disagree — is it about the EXISTENCE of scarcity/preference inputs (none of the readings deny these exist), or about whether scarcity and preference are SUFFICIENT to explain the observed price, versus requiring institutional, rent-theoretic, or credit-theoretic supplementation?',
    'Formal decomposition of price variance into scarcity/preference components versus institutional/rent/credit components using structural econometric models across multiple housing markets and time periods.',
    'If scarcity/preference explains the overwhelming majority of price variance, the naturalist reading''s mountain classification is well-supported as the dominant structural account; if institutional/rent/credit components dominate, the naturalist reading becomes a special-case idealization rather than a general account, and the sibling readings'' extractive classifications carry more of the real-world explanatory weight.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_disagreement_location, empirical, 'Locating the precise structural disagreement among the four kernel readings rather than treating them as competing labels for the same phenomenon.').

omega_variable(
    policy_intervention_deadweight_universality,
    'Does EVERY policy intervention in price formation (rent control, zoning relaxation, land value tax, credit regulation) produce deadweight loss as the naturalist reading implies, or only interventions that fight against genuine scarcity signals while leaving interventions that correct for genuinely constructed distortions (the institutional/georgist/financialization mechanisms) net-positive?',
    'Case-by-case welfare analysis distinguishing interventions that suppress a real scarcity signal from interventions that remove a rent-extraction or credit-distortion mechanism identified by the sibling readings.',
    'If some interventions are welfare-improving because they correct for the sibling readings'' extractive mechanisms rather than override scarcity, the naturalist reading''s blanket deadweight-loss claim requires the sibling readings'' distinctions to be applied case-by-case, undermining the naturalist reading''s claim to universal applicability without abandoning its validity for the pure-scarcity case.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_intervention_deadweight_universality, conceptual, 'Whether deadweight-loss claims under the naturalist reading generalize across all interventions or only those targeting genuine scarcity signals.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(price_formation_kernel__naturalist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pric_tr_t0, price_formation_kernel__naturalist_reading, theater_ratio, 0, 0.04).
narrative_ontology:measurement(pric_tr_t8, price_formation_kernel__naturalist_reading, theater_ratio, 8, 0.04).
narrative_ontology:measurement(pric_tr_t16, price_formation_kernel__naturalist_reading, theater_ratio, 16, 0.05).
narrative_ontology:measurement(pric_tr_t24, price_formation_kernel__naturalist_reading, theater_ratio, 24, 0.05).
narrative_ontology:measurement(pric_tr_t32, price_formation_kernel__naturalist_reading, theater_ratio, 32, 0.05).
narrative_ontology:measurement(pric_tr_t40, price_formation_kernel__naturalist_reading, theater_ratio, 40, 0.05).

% Extraction over time
narrative_ontology:measurement(pric_be_t0, price_formation_kernel__naturalist_reading, base_extractiveness, 0, 0.02).
narrative_ontology:measurement(pric_be_t8, price_formation_kernel__naturalist_reading, base_extractiveness, 8, 0.02).
narrative_ontology:measurement(pric_be_t16, price_formation_kernel__naturalist_reading, base_extractiveness, 16, 0.03).
narrative_ontology:measurement(pric_be_t24, price_formation_kernel__naturalist_reading, base_extractiveness, 24, 0.03).
narrative_ontology:measurement(pric_be_t32, price_formation_kernel__naturalist_reading, base_extractiveness, 32, 0.03).
narrative_ontology:measurement(pric_be_t40, price_formation_kernel__naturalist_reading, base_extractiveness, 40, 0.03).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(price_formation_kernel__naturalist_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(price_formation_kernel__naturalist_reading, institutional_reading).
narrative_ontology:affects_constraint(price_formation_kernel__naturalist_reading, georgist_reading).
narrative_ontology:affects_constraint(price_formation_kernel__naturalist_reading, financialization_reading).

% DUAL FORMULATION NOTE:
% This file is one of four sibling constraints sharing the price_formation_kernel. The naturalist_reading is the unique mountain-classified member of the family (no beneficiaries, no victims, negligible extraction). institutional_reading names zoning boards, lending institutions, and platform intermediaries as beneficiaries and excluded renters/developers as victims for the SAME observable (housing price), computing as tangled_rope. georgist_reading decomposes the same price into a rent component (extractive, unearned) and an improvement component (legitimate, earned). financialization_reading attributes price dynamics to credit expansion and asset-feedback loops, with mortgage originators and asset holders as likely beneficiaries and first-time buyers as likely victims. Per the epsilon-invariance principle, these are four separate constraints, not one constraint measured four ways — each carries its own epsilon and its own stakeholder structure, linked only by shared kernel identity and by the empirical fact that they compete to explain the same observed price series.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
