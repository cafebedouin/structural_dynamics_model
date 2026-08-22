% ============================================================================
% CONSTRAINT STORY: price_formation_kernel__naturalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   domain: economic/political_economy/housing_markets
 *
 * SUMMARY:
 *   This story instantiates the naturalist reading of the
 *   price_formation_kernel: the claim that housing (and market) prices emerge
 *   from an equilibrium process driven by objective scarcity and subjective
 *   preference, with the price mechanism functioning as a discovery procedure
 *   rather than a constructed arrangement. Under this reading, policy
 *   interventions (rent control, restrictive zoning enforced against market
 *   clearing, transfer taxes) are read as distortions imposed on top of an
 *   otherwise natural process, producing deadweight loss. This is a
 *   Mountain-type claim: no beneficiaries collect from the price mechanism's
 *   operation, no victims are extracted from by it, and alternatives
 *   (administered pricing, rationing) are understood as inferior on
 *   efficiency grounds rather than as suppressed competitors. Three sibling
 *   readings of the same kernel are NOT part of this constraint — they are
 *   separate files: institutional_reading treats zoning/lending/tax/platform
 *   structures as constitutive of price rather than background;
 *   georgist_reading separates land rent from improvement value and treats
 *   rent capture as a distinct extractive component; financialization_reading
 *   treats credit expansion and asset-price feedback as the dominant driver,
 *   particularly in housing. This file's ε, beneficiary/victim structure, and
 *   type are authored solely for the naturalist reading's own internal logic
 *   and must not be averaged or reconciled with the siblings.
 *
 * KEY AGENTS:
 *   - market_participants (buyers and sellers): Analytical/mobile — engage in voluntary exchange, revealing preferences through bids and asks; under this reading, no party is targeted or subsidized by the price mechanism itself
 *   - economic_analysts_naturalist_school: Analytical observer — articulates and defends the equilibrium-discovery framing
 *   - policymakers_considering_intervention: Institutional/moderate exit — face the mountain's resistance when proposing price controls, interpreted under this reading as fighting arithmetic rather than power
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
narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(price_formation_kernel__naturalist_reading, mountain).
narrative_ontology:human_readable(price_formation_kernel__naturalist_reading, "Price Formation as Natural Equilibrium (Naturalist Reading)").
narrative_ontology:topic_domain(price_formation_kernel__naturalist_reading, "economic/political_economy/housing_markets").

domain_priors:emerges_naturally(price_formation_kernel__naturalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(price_formation_kernel__naturalist_reading, '2a85182a-b347-464e-b86c-fc0841c9fca1').
narrative_ontology:cs_kernel_codification('2a85182a-b347-464e-b86c-fc0841c9fca1', distributed).
narrative_ontology:cs_authority_grounding('2a85182a-b347-464e-b86c-fc0841c9fca1', distributed).
narrative_ontology:cs_reading_relation('2a85182a-b347-464e-b86c-fc0841c9fca1', price_formation_kernel__institutional_reading, coexists_with).
narrative_ontology:cs_reading_relation('2a85182a-b347-464e-b86c-fc0841c9fca1', price_formation_kernel__georgist_reading, coexists_with).
narrative_ontology:cs_reading_relation('2a85182a-b347-464e-b86c-fc0841c9fca1', price_formation_kernel__financialization_reading, influences).
narrative_ontology:cs_axiom('2a85182a-b347-464e-b86c-fc0841c9fca1', foundational, price_is_discovered_not_constructed).
narrative_ontology:cs_axiom_status(price_is_discovered_not_constructed, holdable).
narrative_ontology:cs_axiom_grounding('2a85182a-b347-464e-b86c-fc0841c9fca1', price_is_discovered_not_constructed, empirically_contingent).
narrative_ontology:cs_axiom('2a85182a-b347-464e-b86c-fc0841c9fca1', foundational, intervention_produces_deadweight_loss).
narrative_ontology:cs_axiom_status(intervention_produces_deadweight_loss, holdable).
narrative_ontology:cs_axiom_grounding('2a85182a-b347-464e-b86c-fc0841c9fca1', intervention_produces_deadweight_loss, instrumental).
narrative_ontology:cs_reference_frame('2a85182a-b347-464e-b86c-fc0841c9fca1', market_clearing_equilibrium_baseline).
narrative_ontology:cs_drift_state('2a85182a-b347-464e-b86c-fc0841c9fca1', post_2008_housing_financialization_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2a85182a-b347-464e-b86c-fc0841c9fca1', '').
narrative_ontology:cs_kernel_id(price_formation_kernel__naturalist_reading, price_formation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_vindicates(price_formation_kernel__naturalist_reading, supply_demand_equilibrium_doctrine).
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
% COORDINATION_FUNCTION: Under this reading, the price mechanism coordinates dispersed information about scarcity and preference that no central authority could otherwise aggregate, allowing resources to flow toward their highest-valued uses without requiring anyone to know the whole system's state.
% TRANSFER_FUNCTION: Under the naturalist reading, nothing is transferred by the price mechanism itself between structurally distinct parties — voluntary exchanges reflect mutual gains from trade given prior endowments; any redistribution traces to those prior endowments and preferences, not to price formation as such.
% ABSENT_VOICES: The institutional_reading, georgist_reading, and financialization_reading would each object that this reading treats historically contingent, actively maintained arrangements (zoning law, credit issuance rules, land tenure systems) as background scenery rather than as constitutive forces — they are not absent from the world, only absent from this particular reading's account of what counts as 'the' price mechanism.
% DISAPPEARANCE_RATIONALE: Under the naturalist reading's own lights, the mountain claim asserts that even if any particular set of market institutions vanished, some price-discovery process would re-emerge because it follows from the logical/physical fact of scarce goods meeting finite willingness to pay — the world does not fundamentally rearrange because the underlying equilibrium-seeking behavior is not itself an institution that could be abolished, only channeled differently. (Contrast: the institutional_reading's disappearance verdict for zoning/lending architecture would very plausibly be world_rearranges — that asymmetry is exactly what distinguishes the two readings.)
% FOUNDING_PROBLEM: There is no founding problem in the institutional sense under this reading — price formation is not presented as an arrangement built to solve a problem at a point in time, but as a standing feature of exchange under conditions of scarcity, present wherever trade occurs.
% FOUNDING_PROBLEM_CORROBORATION: Mainstream neoclassical and Austrian economic traditions (largely overlapping with the reading's own proponents) attest that scarcity-and-preference-driven price discovery is a persistent, non-obsolescing feature of exchange. Outside corroboration is harder to locate precisely because the claim denies having a historical founding moment to corroborate — this is itself the load-bearing gap the omega variable 'naturalist_reading_as_committer_position' flags: a genealogy claim that resists genealogical interrogation by design is exactly the pattern that warrants scrutiny, and no source outside economic traditions sympathetic to the naturalist framing is offered here.
narrative_ontology:disappearance_verdict(price_formation_kernel__naturalist_reading, world_unchanged).
narrative_ontology:founding_problem_status(price_formation_kernel__naturalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(price_formation_kernel__naturalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored near zero (0.03) because, under the naturalist reading's own lights, no party captures rents from the price-discovery process itself — profits and losses reflect information and preference revelation, not extraction. Suppression is near zero (0.02): the mountain classification requires that the constraint would persist with or without enforcement, and the naturalist reading holds that prices would clear even absent any single enforcer, following from the physical/logical fact of scarce goods meeting finite willingness to pay. Theater ratio is low (0.05): there is minimal performative overhead in this reading's own account of how prices form. Accessibility collapse is high (0.88): the naturalist reading holds that once the equilibrium logic is understood, there is no coherent alternative price-discovery mechanism that isn't itself a degraded or delayed version of market clearing (rationing, queues, black markets all reintroduce implicit pricing). Resistance is low-moderate (0.15): the mountain meets resistance mainly from those who dispute its naturalness (the sibling readings) rather than from participants within its own frame.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiaries or victims are declared for this reading, consistent with a genuine mountain claim — under the naturalist reading's own account, the price mechanism does not select winners and losers structurally; distributional outcomes trace to prior endowments and preferences, not to the pricing mechanism as such. This is why no stakeholders array is required or authored: a genuine mountain with no beneficiaries/victims has no parties to name.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy question does not apply cleanly here because the naturalist reading denies the pricing mechanism was ever a 'mandate' with a founding problem to outlive — it presents itself as a standing feature of exchange under scarcity, not an institution that could become obsolete. This is itself a load-bearing structural claim: the omega variable 'naturalist_reading_as_committer_position' exists precisely to test whether this denial of institutional history is warranted or whether it is doing the ideological work of foreclosing the institutional_reading's claim that the 'mandate' (zoning, lending, tax architecture) is very much a historically contingent, changeable arrangement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naturalist_reading_as_committer_position,
    'Is the naturalist reading of price formation a genuine description of an unconstructed equilibrium process, or is it itself one committed reading among four of a contested kernel (price_formation_kernel), whose apparent naturalness depends on treating the institutional scaffolding around markets (zoning, credit issuance, tax treatment, platform intermediation) as background rather than as constitutive of the price itself?',
    'Comparative institutional analysis: hold preference and physical scarcity constant across two housing markets with materially different zoning/lending/tax regimes and observe whether resulting prices converge (supporting naturalist reading) or diverge in ways traceable to the regime differences (supporting institutional_reading).',
    'If prices diverge systematically with regime rather than with underlying scarcity/preference, the mountain classification is a false summit — the naturalist reading would be describing a constructed arrangement using natural-law vocabulary, and the beneficiaries of that framing (asset holders, incumbent institutions) would need to be named.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalist_reading_as_committer_position, conceptual, 'Whether naturalist framing correctly identifies an unconstructed process or performs the ideological work of naturalizing an institutionally constructed one.').

omega_variable(
    kernel_disagreement_location,
    'Where exactly do the four price_formation_kernel readings disagree — is it about which forces exist (all four could agree scarcity, institutions, land rent, and credit all operate) or about which force is CAUSALLY DOMINANT and therefore deserves to be called ''the'' price formation process?',
    'Decompose observed price into components attributable to marginal scarcity, land rent capture, credit/leverage effects, and regulatory constraint; measure relative variance contribution across markets and time periods.',
    'If the disagreement is about causal weighting rather than existence, the four readings are not strictly incompatible — this affects whether reading_relations should be forecloses or coexists_with/influences.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_disagreement_location, conceptual, 'Locates the structural disagreement among sibling readings as a weighting dispute rather than an existence dispute.').

omega_variable(
    deadweight_loss_measurement_dependency,
    'Does the claimed deadweight loss from policy intervention (rent control, zoning relaxation mandates, transfer taxes) hold as an intrinsic mountain-property, or does its magnitude depend on which institutional baseline (the counterfactual ''undistorted'' market) is assumed — a baseline itself contested by the institutional_reading?',
    'Meta-analysis of deadweight loss estimates across studies using different counterfactual baselines; test sensitivity of the estimated loss to baseline specification.',
    'High sensitivity to baseline choice would suggest the deadweight-loss claim smuggles in institutional assumptions the naturalist reading claims to avoid, weakening the mountain classification''s independence from the institutional_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deadweight_loss_measurement_dependency, empirical, 'Tests whether the deadweight-loss claim is baseline-independent (mountain-consistent) or baseline-dependent (institutionally entangled).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(price_formation_kernel__naturalist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(price_formation_kernel__naturalist_reading, price_formation_kernel__institutional_reading).
narrative_ontology:affects_constraint(price_formation_kernel__naturalist_reading, price_formation_kernel__georgist_reading).
narrative_ontology:affects_constraint(price_formation_kernel__naturalist_reading, price_formation_kernel__financialization_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling stories decomposing the natural-language label 'price formation' per the ε-invariance principle: naturalist_reading (this file, Mountain, ε≈0.03), institutional_reading (constructed by zoning/lending/tax/platforms — expected Tangled Rope or Snare, higher ε), georgist_reading (separates unearned land rent from earned improvement value — expected Tangled Rope with land-rent-capturing beneficiaries), and financialization_reading (credit expansion and asset-price feedback — expected Snare or Tangled Rope with high ε driven by leverage dynamics). Each reading is independently authored with its own ε and its own beneficiary/victim structure; none is derived from or averaged with the others. The naturalist reading's low ε does not contradict the other readings' higher ε values — they describe structurally different claims about the same colloquial phenomenon, exactly as the BGS spectral-universality/eigenvector-thermalization decomposition models.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
