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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   The naturalist reading of price formation holds that prices emerge from
 *   the interaction of scarcity and preference through decentralized
 *   exchange, forming an equilibrium that coordinates supply and demand
 *   without central authority. Under this reading, prices DISCOVER the value
 *   of things, not CONSTRUCT it; they are properties of any exchange system
 *   facing constraint, not artifacts of particular institutional choices. The
 *   reading asserts zero extractiveness: no party benefits from price
 *   formation itself; prices are a consequence, not a rent source. This
 *   reading meets the structural requirements for a mountain claim—high
 *   accessibility collapse (once prices exist in an exchange system, the
 *   alternative (barter, central planning, rationing) becomes unthinkable
 *   without rebuilding the entire coordination mechanism) and minimal
 *   resistance (the mechanism is neutral, not defended by identifiable
 *   beneficiaries).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(price_formation_kernel__naturalist_reading, 0.0).
domain_priors:suppression_score(price_formation_kernel__naturalist_reading, 0.0).
domain_priors:theater_ratio(price_formation_kernel__naturalist_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, extractiveness, 0.0).
narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(price_formation_kernel__naturalist_reading, mountain).
narrative_ontology:human_readable(price_formation_kernel__naturalist_reading, "Price Formation as Natural Equilibrium (Naturalist Reading)").
narrative_ontology:topic_domain(price_formation_kernel__naturalist_reading, "political_economy/housing_markets/institutional_analysis").

domain_priors:emerges_naturally(price_formation_kernel__naturalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(price_formation_kernel__naturalist_reading, 'd718cfe2-82bc-418f-83ee-053c5bc1328e').
narrative_ontology:cs_kernel_codification('d718cfe2-82bc-418f-83ee-053c5bc1328e', distributed).
narrative_ontology:cs_authority_grounding('d718cfe2-82bc-418f-83ee-053c5bc1328e', expertise).
narrative_ontology:cs_reading_relation('d718cfe2-82bc-418f-83ee-053c5bc1328e', price_formation_kernel__institutional_reading, coexists_with).
narrative_ontology:cs_reading_relation('d718cfe2-82bc-418f-83ee-053c5bc1328e', price_formation_kernel__georgist_reading, coexists_with).
narrative_ontology:cs_reading_relation('d718cfe2-82bc-418f-83ee-053c5bc1328e', price_formation_kernel__financialization_reading, coexists_with).
narrative_ontology:cs_axiom('d718cfe2-82bc-418f-83ee-053c5bc1328e', foundational, prices_discover_not_construct).
narrative_ontology:cs_axiom_status(prices_discover_not_construct, holdable).
narrative_ontology:cs_axiom_grounding('d718cfe2-82bc-418f-83ee-053c5bc1328e', prices_discover_not_construct, deontological).
narrative_ontology:cs_axiom('d718cfe2-82bc-418f-83ee-053c5bc1328e', foundational, scarcity_and_preference_determine_equilibrium).
narrative_ontology:cs_axiom_status(scarcity_and_preference_determine_equilibrium, holdable).
narrative_ontology:cs_axiom_grounding('d718cfe2-82bc-418f-83ee-053c5bc1328e', scarcity_and_preference_determine_equilibrium, empirically_contingent).
narrative_ontology:cs_reference_frame('d718cfe2-82bc-418f-83ee-053c5bc1328e', market_equilibrium_with_perfect_information).
narrative_ontology:cs_drift_state('d718cfe2-82bc-418f-83ee-053c5bc1328e', contemporary_real_world, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d718cfe2-82bc-418f-83ee-053c5bc1328e', '').
narrative_ontology:cs_kernel_id(price_formation_kernel__naturalist_reading, price_formation_kernel).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Stakeholders authored EMPTY (Pattern-5: an explicit assertion that no
% entity's arrangements depend on this constraint — paired with the
% world_unchanged verdict below, enforced by the schema).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Price signals aggregate scattered information about scarcity, preference, and opportunity cost into a single metric that coordinates resource allocation without central authority.
% TRANSFER_FUNCTION: No persistent transfer; prices equilibrate supply and demand in real time, with no party capturing systematic rents.
% ABSENT_VOICES: Parties claiming price formation is constructed (institutional, georgist, financialization readings) are absent from this framework's internal logic—they would object that the naturalist reading misidentifies what is discovered vs. constructed.
% DISAPPEARANCE_RATIONALE: Price formation as an equilibrium process is a consequence of scarcity and preference, not a rule that could be removed. Markets would continue to form prices; the naturalist reading asserts this is not a contingent institutional choice but a brute fact of resource allocation under constraint.
% FOUNDING_PROBLEM: How does decentralized coordination occur when no single actor knows all supply, demand, or opportunity costs? Price formation is the discovered mechanism by which this coordination emerges without planning.
% FOUNDING_PROBLEM_CORROBORATION: Economists from multiple traditions (Austrian, neoclassical, complexity) acknowledge that price signals solve a real information-coordination problem. This corroboration is internal to economic theory itself; it comes from the structural logic of the problem, not from external benefiting parties.
narrative_ontology:disappearance_verdict(price_formation_kernel__naturalist_reading, world_unchanged).
narrative_ontology:founding_problem_status(price_formation_kernel__naturalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(price_formation_kernel__naturalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(price_formation_kernel__naturalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(price_formation_kernel__naturalist_reading, 0.0, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   A mountain constraint has extractiveness near zero, suppression near zero, theater near zero, accessibility collapse near or above 0.85, and resistance near zero. The naturalist reading's authored metrics reflect this profile: prices are not suppressive (they respond to preference, not coercion), are not theatrical (they serve no agenda beyond coordination), and face minimal organized resistance because they appear to be laws of nature, not rules anyone chose. The accessibility collapse (0.92) reflects the fact that once price formation is understood as an equilibrium mechanism solving a real coordination problem, rejecting it requires rejecting the entire premise that scarcity exists and preferences vary—an untenable alternative. The resistance (0.08) accounts for heterodox economists, institutional theorists, and activists who reject the framing, but who remain far from the mainstream and do not appear to dent the naturalist reading's authority in policy discourse. The claim/metric independence is critical here: the authored claim (mountain) and the authored metrics (near-zero extraction/suppression, high collapse, low resistance) are NOT tuned to each other—they follow from the reading's own premises about what prices are. The engine will compute the classification from these metrics and structural data; where it diverges from the claimed mountain, that divergence is exactly the measurement the corpus takes.
 *
 * PERSPECTIVAL GAP:
 *   A constraint with zero stakeholders should have no perspectival gap—there is no payer seat diverging from an agenda-setter seat because the reading asserts that no seat is extracting from any other. The price mechanism belongs to no one; it is discovered by all. This gap-absence is itself the structural claim of the mountain: if price formation were contested, if some parties benefited from it while others bore costs, if it required enforcement, then stakeholders would exist and the gap would open. The absence of stakeholders IS the data that supports the mountain claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Not applicable. Mountains with zero beneficiaries and victims derive zero directionality vectors. The price mechanism is not directed at anyone; it is a property of exchange systems themselves.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to coordinate without central planning) remains live in this reading's framing: price formation is still solving that problem today. The disappearance verdict (world unchanged) asserts that price formation is not contingent on any policy choice—markets would form prices even if no government enforced them. No mandatrophy arises unless the policy supporting the market (property rights, contract enforcement, currency) were removed; but those are upstream of price formation itself, not targets of this constraint's persistence. The constraint (price formation) is not maintained by anyone; it EMERGES from scarcity and preference.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_vs_constructed_distinction,
    'Is price formation a property of any exchange system with scarcity and preference, or is it constructed by specific institutional architectures (lending standards, zoning, tax treatment, platforms)?',
    'Cross-institutional comparison: do radically different institutional contexts (planned economies, barter networks, modern markets, historical systems) all produce price-like signals? Do systems that claim to remove pricing still require implicit price mechanisms? Do price patterns persist despite policy attempts to suppress them?',
    'If price formation is universal across institutional contexts, the naturalist reading is strengthened (mountain status). If price signals vary substantially with institutional design and can be suppressed by policy, the reading retreats to ''prices exist under market institutions'' (not a mountain) and the institutional reading advances.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_vs_constructed_distinction, conceptual, 'Whether price formation is a natural law or an institutional artifact.').

omega_variable(
    equilibrium_vs_power_dynamics,
    'Do observed prices reflect equilibrium between supply and demand, or do they reflect the power of actors to impose prices (monopsony, monopoly, regulatory capture, financing control)?',
    'Empirical: do price changes correspond to supply/demand shifts in predicted directions, or do they correlate with changes in seller/buyer market power independent of scarcity? Counterfactual: in markets where power is more concentrated, do prices deviate systematically from competitive benchmarks? Do deviations persist or dissipate?',
    'Persistent price deviations from competitive benchmarks that correlate with market power (not preference or scarcity shifts) would support the institutional/financialization readings. Equilibration around competitive levels despite power shifts would support the naturalist reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(equilibrium_vs_power_dynamics, empirical, 'Whether observed prices are equilibria or coercive impositions.').

omega_variable(
    information_aggregation_sufficiency,
    'Does price formation in housing markets actually aggregate the information available to market participants, or does it reflect information asymmetries, framing effects, and behavioral distortions that prevent true preference revelation?',
    'Behavioral economics evidence: do homebuyers price in objectively available information (school quality, crime, transit, environmental risk) consistently? Do they exhibit systematic overweighting or underweighting of certain attributes? Do prices respond immediately to new information or slowly? Do repeated markets (resale, refinance) show convergence to rational expectations?',
    'If prices consistently misprice available information, the aggregation function breaks down and the naturalist reading loses its central mechanism. If prices aggregate available information within predictable error bounds, the reading is supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(information_aggregation_sufficiency, empirical, 'Whether price formation faithfully aggregates preference and scarcity information.').

omega_variable(
    kernel_contest_framing,
    'Does this reading''s core premise—that price reflects natural scarcity and preference—foreclose any of the sibling readings within a single framework, or do they coexist as different accounts of overlapping mechanisms?',
    'Logical analysis: can a single analytical framework simultaneously hold (1) prices reflect natural equilibrium, (2) prices are constructed by institutions, and (3) prices separate natural rents from artificial ones? If yes, the readings coexist at different explanatory levels. If no, identify the incompatibility.',
    'If the readings logically foreclose each other, one will eventually dominate empirically. If they coexist, they describe different aspects of a multi-layered system (prices discover equilibria within institutional constraints). The coexistence case is more probable given the evidence, implying this reading is not a mountain but a valid account of one layer.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_contest_framing, conceptual, 'Whether the naturalist reading forecloses or coexists with institutional/rent-separation framings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(price_formation_kernel__naturalist_reading, 0, 1).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(price_formation_kernel__naturalist_reading, information_standard).
narrative_ontology:boltzmann_floor_override(price_formation_kernel__naturalist_reading, 0.0).
narrative_ontology:affects_constraint(price_formation_kernel__naturalist_reading, price_formation_kernel__institutional_reading).
narrative_ontology:affects_constraint(price_formation_kernel__naturalist_reading, price_formation_kernel__georgist_reading).
narrative_ontology:affects_constraint(price_formation_kernel__naturalist_reading, price_formation_kernel__financialization_reading).

% DUAL FORMULATION NOTE:
% Price formation is a contested kernel with four sibling readings instantiated as separate constraint stories. The naturalist reading (this story) claims price formation is a natural equilibrium; the institutional reading decomposes it into constructed institutional elements; the georgist reading separates unearned land rent from earned improvement value; the financialization reading attributes price movement to credit cycles and asset speculation. Each reading carries its own epsilon, beneficiary/victim structure, and type classification. Constraints are linked bidirectionally via network.affects_constraints: each reading influences the others' empirical standing by providing alternative causal accounts that must be evidentially distinguished.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
