% ============================================================================
% CONSTRAINT STORY: us_tariffs_2025
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_us_tariffs_2025, []).

:- use_module(library(plunit)).
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
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: us_tariffs_2025
 *   human_readable: 2025 United States Tariff Policy on Imported Goods
 *   domain: economic/political
 *
 * SUMMARY:
 *   A series of broad tariffs imposed by the United States government on
 *   a wide range of imported goods, effective in 2025. The stated goal is
 *   to protect domestic industries and encourage reshoring of manufacturing.
 *   However, economic analysis indicates the costs are primarily passed
 *   through to domestic consumers and importing businesses via higher prices.
 *
 * KEY AGENTS (by structural relationship):
 *   - US Consumers: Primary target (powerless/trapped) — bear extraction via higher prices.
 *   - Domestic Importing Firms: Secondary target (organized/constrained) — bear extraction via higher input costs.
 *   - Domestic Protected Producers: Primary beneficiary (organized/mobile) — benefit from reduced foreign competition.
 *   - US Government: Architect/Beneficiary (institutional/arbitrage) — benefits from tariff revenue and policy goals.
 *   - Foreign Governments/Exporters: Inter-institutional target (institutional/constrained) — lose market access.
 *   - Economic Analysts: Analytical observer — sees both the coordination claim and the extractive reality.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Base extraction (ε) is high, representing the significant portion of the tariff
% cost passed on to consumers, minus administrative overhead and partial absorption
% by foreign producers.
domain_priors:base_extractiveness(us_tariffs_2025, 0.48).

% Suppression is high because the core mechanism of a tariff is to make
% foreign alternatives non-competitive, thereby suppressing choice.
domain_priors:suppression_score(us_tariffs_2025, 0.75).

% Theater is moderate. There is significant political messaging ("protecting jobs"),
% but the underlying economic mechanism is real and functional, not purely performative.
domain_priors:theater_ratio(us_tariffs_2025, 0.20).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_tariffs_2025, extractiveness, 0.48).
narrative_ontology:constraint_metric(us_tariffs_2025, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(us_tariffs_2025, theater_ratio, 0.20).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(us_tariffs_2025, tangled_rope).
narrative_ontology:human_readable(us_tariffs_2025, "2025 United States Tariff Policy on Imported Goods").

% --- Binary flags ---
% Tariffs require active enforcement by customs agencies at ports of entry.
% This is a mandatory flag for a Tangled Rope classification.
domain_priors:requires_active_enforcement(us_tariffs_2025).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(us_tariffs_2025, domestic_protected_producers).
narrative_ontology:constraint_beneficiary(us_tariffs_2025, us_government_revenue).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(us_tariffs_2025, us_consumers).
narrative_ontology:constraint_victim(us_tariffs_2025, domestic_importing_firms).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE)
% US Consumers are trapped; they must purchase goods and have limited ability
% to avoid the price increases. They experience the tariff as pure cost.
% Engine derives d from victim status + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42.
% χ = 0.48 * 1.42 * 1.0 (national) ≈ 0.68. This is ≥ 0.66, thus a Snare.
constraint_indexing:constraint_classification(us_tariffs_2025, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY / ARCHITECT (ROPE)
% The US Government imposes the tariff. It benefits from revenue and sees the
% policy as a coordination tool. Its 'arbitrage' exit is the ability to repeal the law.
% Engine derives d from beneficiary status + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12.
% χ is negative, classifying as a Rope.
constraint_indexing:constraint_classification(us_tariffs_2025, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% An economic analyst sees both the stated coordination goal (benefiting some
% domestic producers) and the clear, asymmetric extraction from consumers.
% Engine's canonical d for analytical is ~0.73.
% χ = 0.48 * f(0.73) * 1.2 (global) = 0.48 * 1.15 * 1.2 ≈ 0.66.
% This is on the boundary of Snare/Tangled Rope, perfectly capturing the analytical
% tension. The presence of a coordination function pushes it to Tangled Rope.
constraint_indexing:constraint_classification(us_tariffs_2025, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVES ---

% PERSPECTIVE 4A: DOMESTIC IMPORTING FIRMS (TANGLED ROPE)
% These firms are victims (higher costs) but are organized and have some
% ability to adapt supply chains (constrained, not trapped). They perceive
% the policy's logic but bear its costs directly.
% Engine derives a victim d, but constrained exit lowers it from the 'trapped' value.
% Assume d ≈ 0.65. χ = 0.48 * f(0.65) * 1.0 = 0.48 * 1.0 * 1.0 ≈ 0.48. This is a Tangled Rope.
constraint_indexing:constraint_classification(us_tariffs_2025, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4B: TARGETED FOREIGN GOVERNMENTS (TANGLED ROPE)
% A foreign government whose exports are targeted is an institutional victim.
% Their exit is constrained; they cannot ignore the policy and must engage through
% diplomacy or retaliation. They see the stated US goal but experience it as extraction.
% Victim + institutional + constrained exit -> d > 0.5. Assume d ≈ 0.6.
% χ = 0.48 * f(0.6) * 1.1 (continental) = 0.48 * 0.84 * 1.1 ≈ 0.44. Tangled Rope.
constraint_indexing:constraint_classification(us_tariffs_2025, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_tariffs_2025_tests).

test(perspectival_gap_target_vs_beneficiary, [nondet]) :-
    constraint_indexing:constraint_classification(us_tariffs_2025, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(us_tariffs_2025, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(tangled_rope_structural_requirements, [nondet]) :-
    % Verify the core metrics and flags for Tangled Rope are present.
    narrative_ontology:constraint_claim(us_tariffs_2025, tangled_rope),
    domain_priors:base_extractiveness(us_tariffs_2025, E), E >= 0.30,
    domain_priors:suppression_score(us_tariffs_2025, S), S >= 0.40,
    domain_priors:requires_active_enforcement(us_tariffs_2025),
    narrative_ontology:constraint_beneficiary(us_tariffs_2025, _),
    narrative_ontology:constraint_victim(us_tariffs_2025, _).

test(inter_institutional_gap, [nondet]) :-
    % Verify the architect (US Gov) and target (Foreign Gov) see it differently.
    constraint_indexing:constraint_classification(us_tariffs_2025, TypeArchitect, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    constraint_indexing:constraint_classification(us_tariffs_2025, TypeTarget, context(agent_power(institutional), _, exit_options(constrained), _)),
    assertion(TypeArchitect == rope),
    assertion(TypeTarget == tangled_rope),
    TypeArchitect \= TypeTarget.

:- end_tests(us_tariffs_2025_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.48): This value reflects that a substantial portion of the
 *     tariff cost is passed directly to downstream agents (importers, consumers),
 *     representing a clear extractive transfer. It is not 1.0 because some costs
 *     are absorbed by foreign producers or offset by currency fluctuations.
 *   - Suppression (0.75): A tariff's primary function is to suppress the choice of
 *     purchasing cheaper foreign goods by artificially inflating their price. The
 *     score is high because this is the intended mechanism.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. The US Government (architect) views the tariff as a 'Rope'—a
 *   coordination tool to achieve national economic goals with revenue as a side
 *   benefit. For them, χ is negative. In contrast, a US Consumer (target)
 *   experiences it as a 'Snare'—a non-consensual cost increase on necessary
 *   goods with no visible benefit or alternative. Their high derived directionality
 *   (d≈0.95) results in a high χ (≈0.68), capturing the pure extraction they feel.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from the explicit beneficiary/victim declarations.
 *   - Beneficiaries: 'domestic_protected_producers' and 'us_government_revenue'.
 *     Agents in these groups (like the institutional architect) get low 'd' values.
 *   - Victims: 'us_consumers' and 'domestic_importing_firms'. Agents in these
 *     groups get high 'd' values.
 *   The combination of these structural roles with exit options (trapped vs. arbitrage
 *   vs. constrained) generates the wide range of perceived classifications.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   This story highlights a key inter-institutional conflict. Both the US Government
 *   and a targeted foreign government are 'institutional' actors. However, their
 *   different structural relationships and exit options create a perspectival gap.
 *   The US Govt has 'arbitrage' exit (it can repeal its own law), sees itself as a
 *   beneficiary, and classifies the constraint as a Rope. The foreign government has
 *   'constrained' exit (it cannot simply ignore the policy), is a victim, and
 *   classifies it as a Tangled Rope—acknowledging the stated policy goal but
 *   experiencing it as coercive extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This is a canonical example of a Tangled Rope that could be mis-sold as a pure
 *   Rope. The political narrative focuses entirely on the coordination function
 *   ("protecting jobs"). The Deferential Realism framework, by indexing to the
 *   'powerless' and 'trapped' consumer, correctly identifies the Snare component.
 *   The final 'Tangled Rope' classification from the analytical view prevents the
 *   coordination claim from obscuring the highly extractive reality for its targets.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_us_tariffs_2025,
    'What is the true long-term elasticity of domestic production in response to the tariffs?',
    'Longitudinal studies (10+ years) measuring manufacturing output, employment, and capital investment in protected vs. unprotected sectors.',
    'If elasticity is high, the "coordination" function is stronger, justifying the Tangled Rope view. If elasticity is low, the policy functions primarily as extraction (Snare) with little coordination benefit, making the powerless perspective more accurate overall.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(us_tariffs_2025, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data models the tendency for tariffs to become entrenched, with their
% extractive function solidifying over time as supply chains adjust and
% political justifications become more performative.
% Required because base_extractiveness (0.48) > 0.46.

% Theater ratio over time:
narrative_ontology:measurement(us_tariffs_2025_tr_t0, us_tariffs_2025, theater_ratio, 0, 0.10).
narrative_ontology:measurement(us_tariffs_2025_tr_t5, us_tariffs_2025, theater_ratio, 5, 0.15).
narrative_ontology:measurement(us_tariffs_2025_tr_t10, us_tariffs_2025, theater_ratio, 10, 0.20).

% Extraction over time:
narrative_ontology:measurement(us_tariffs_2025_ex_t0, us_tariffs_2025, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(us_tariffs_2025_ex_t5, us_tariffs_2025, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(us_tariffs_2025_ex_t10, us_tariffs_2025, base_extractiveness, 10, 0.48).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: A tariff is a mechanism for reallocating resources (consumer
% spending) from foreign producers to domestic ones.
narrative_ontology:coordination_type(us_tariffs_2025, resource_allocation).

% Network relationships: Tariffs directly influence inflation and supply chain structures.
narrative_ontology:affects_constraint(us_tariffs_2025, consumer_price_inflation).
narrative_ontology:affects_constraint(us_tariffs_2025, global_supply_chain_resilience).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this story. The automatic derivation chain,
% using the declared beneficiary/victim groups and agent exit options,
% accurately computes the directionality (d) for each perspective.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */