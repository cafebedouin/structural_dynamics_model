% ============================================================================
% CONSTRAINT STORY: naturalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_naturalist_reading, []).

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
 *   constraint_id: naturalist_reading
 *   human_readable: Price Formation as Natural Equilibrium (Naturalist Reading)
 *   domain: political_economy/price_theory/market_equilibrium
 *
 * SUMMARY:
 *   The naturalist reading of price formation asserts that prices emerge from
 *   the objective interaction of supply and demand—that is, from scarcity of
 *   resources and heterogeneity of preferences—rather than from institutional
 *   construction or power asymmetries. Under this reading, price signals are
 *   discovered through competitive market processes, not set or manipulated
 *   by political will. Scarcity is an inescapable fact; preferences are
 *   revealed through trade; equilibrium is the natural outcome. Policy
 *   interventions that attempt to suppress or control prices—whether price
 *   ceilings on housing, wage floors, or financial transaction
 *   taxes—necessarily create deadweight loss and cannot escape the
 *   fundamental constraint that prices contain essential information about
 *   relative scarcity. This reading treats price formation as a natural law
 *   of economics, as immutable as thermodynamic limits. It instantiates the
 *   Mountain type: the constraint cannot be negotiated, circumvented, or
 *   overcome through institutional reform. All market participants and
 *   policymakers face the same immutable logic, regardless of their power,
 *   position, or preferences.
 *
 * KEY AGENTS:
 *   - Neoclassical Economists: Primary intellectual beneficiaries (analytical/arbitrage) — their disciplinary authority rests on the naturalness of price theory; the reading immunizes price theory from institutional critique
 *   - Market Efficiency Advocates: Secondary beneficiaries (powerful/arbitrage) — individuals and organizations that benefit from minimal price regulation gain intellectual armor for their position
 *   - Institutional Critics: Implicit victims (moderate/constrained) — alternative readings (institutional, Georgist, financialization) are foreclosed or marginalized when naturalism is accepted as settled truth
 *   - Policymakers: Secondary targets (institutional/constrained) — experience price theory as a constraint on feasible policy, reducing their apparent discretion
 *   - Price Theory Community: Authority grounding (organized/arbitrage) — economists who specialize in price theory maintain disciplinary boundaries and interpretive authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(naturalist_reading, 0.0).
domain_priors:suppression_score(naturalist_reading, 0.0).
domain_priors:theater_ratio(naturalist_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(naturalist_reading, extractiveness, 0.0).
narrative_ontology:constraint_metric(naturalist_reading, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(naturalist_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(naturalist_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(naturalist_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(naturalist_reading, mountain).
narrative_ontology:human_readable(naturalist_reading, "Price Formation as Natural Equilibrium (Naturalist Reading)").
narrative_ontology:topic_domain(naturalist_reading, "political_economy/price_theory/market_equilibrium").

domain_priors:emerges_naturally(naturalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(naturalist_reading, 'd04bf4e9-2693-46d1-80ce-2b1c2513b195').
narrative_ontology:cs_kernel_codification('d04bf4e9-2693-46d1-80ce-2b1c2513b195', formalized).
narrative_ontology:cs_authority_grounding('d04bf4e9-2693-46d1-80ce-2b1c2513b195', expertise).
narrative_ontology:cs_interpretation_layer_present('d04bf4e9-2693-46d1-80ce-2b1c2513b195').
narrative_ontology:cs_reading_relation('d04bf4e9-2693-46d1-80ce-2b1c2513b195', naturalist_reading__institutional_reading, coexists_with).
narrative_ontology:cs_reading_relation('d04bf4e9-2693-46d1-80ce-2b1c2513b195', naturalist_reading__georgist_reading, coexists_with).
narrative_ontology:cs_reading_relation('d04bf4e9-2693-46d1-80ce-2b1c2513b195', naturalist_reading__financialization_reading, coexists_with).
narrative_ontology:cs_axiom('d04bf4e9-2693-46d1-80ce-2b1c2513b195', foundational, prices_discovered_from_objective_scarcity).
narrative_ontology:cs_axiom_status(prices_discovered_from_objective_scarcity, holdable).
narrative_ontology:cs_axiom_grounding('d04bf4e9-2693-46d1-80ce-2b1c2513b195', prices_discovered_from_objective_scarcity, empirically_contingent).
narrative_ontology:cs_axiom('d04bf4e9-2693-46d1-80ce-2b1c2513b195', foundational, equilibrium_mechanism_natural_immutable).
narrative_ontology:cs_axiom_status(equilibrium_mechanism_natural_immutable, holdable).
narrative_ontology:cs_axiom_grounding('d04bf4e9-2693-46d1-80ce-2b1c2513b195', equilibrium_mechanism_natural_immutable, empirically_contingent).
narrative_ontology:cs_reference_frame('d04bf4e9-2693-46d1-80ce-2b1c2513b195', competitive_equilibrium_frame).
narrative_ontology:cs_drift_state('d04bf4e9-2693-46d1-80ce-2b1c2513b195', contemporary_financial_capitalism, gap(stable, minor, true)).
narrative_ontology:cs_created_at('d04bf4e9-2693-46d1-80ce-2b1c2513b195', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(naturalist_reading, price_formation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(naturalist_reading, neoclassical_economists).
narrative_ontology:constraint_beneficiary(naturalist_reading, market_efficiency_advocates).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NATURALIST READING (MOUNTAIN) — Price emerges from supply and demand as an equilibrium process reflecting objective constraints: scarcity of resources, heterogeneity of preferences, and marginal productivity. Prices are discovered through market mechanisms, not constructed by institutional power. From this perspective, the constraint is immutable: no society can eliminate the fundamental scarcity problem or the information-aggregation function that prices perform. Policy interventions that suppress price signals create deadweight loss and cannot escape this iron law. Classification is Mountain because the claim asserts that price formation obeys laws of economic nature as binding as physical laws.
constraint_indexing:constraint_classification(naturalist_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: MARKET PARTICIPANT (MOUNTAIN) — A powerful agent (developer, landlord, investor) experiences prices as naturally set constraints they must navigate. They cannot will prices lower or higher — the market is the arbiter. From this position, price controls appear as unnatural impositions that distort the discovery mechanism. The constraint is immutable in their lived experience: competitive markets force adaptation to equilibrium signals.
constraint_indexing:constraint_classification(naturalist_reading, mountain,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: REGULATORY ECONOMIST (MOUNTAIN) — A regulator trained in neoclassical economics experiences price theory as a scientific law constraining what policies are feasible. They cannot repeal the law of supply and demand any more than they can repeal gravity. Attempts to set prices below equilibrium produce shortage; above equilibrium produce surplus. This naturalist reading classifies as Mountain from all positions because it asserts universal immutability.
constraint_indexing:constraint_classification(naturalist_reading, mountain,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: TENANT/BUYER (MOUNTAIN) — A moderate agent with constrained exit experiences prices as forces of nature they cannot resist. Whether they understand the theory or not, high prices feel inevitable — the market is vast and impersonal. The constraint appears immutable from this position because individual market power is negligible. Rising prices feel like a law of nature, not a policy choice.
constraint_indexing:constraint_classification(naturalist_reading, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(naturalist_reading_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(naturalist_reading, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(naturalist_reading, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(naturalist_reading, ExtMetricName, E),
    domain_priors:suppression_score(naturalist_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(naturalist_reading),
    narrative_ontology:constraint_metric(naturalist_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(naturalist_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(naturalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.0): The naturalist reading claims zero extraction because prices are discovered, not extracted. No agent collects rents from the market mechanism itself; the mechanism is self-operating. Suppression (0.0): No coercion is required because the constraint is natural — participants conform to market signals voluntarily, as they would conform to gravity. Theater ratio (0.0): No performative element — price signals are functionally informative; they carry no symbolic or ritual content. These metrics reflect the reading's core claim: that price formation is a law of nature, not an institutional arrangement. Accessibility collapse (0.95): Once the naturalist reading is accepted, alternatives collapse nearly completely. The logic of supply and demand becomes intuitive and nearly universal in professional economics; competing framings appear unscientific or ideologically motivated. Resistance (0.08): Minimal active resistance within mainstream economics. The naturalist reading is the dominant paradigm; it meets no organized scientific opposition. (The resistance score would be much higher if measured as opposition from heterodox economists or institutional critics, but from within the naturalist frame, such opposition is treated as non-scientific.)
 *
 * PERSPECTIVAL GAP:
 *   The naturalist reading produces Mountain classification from all perspectives listed because the claim is that the constraint is universally immutable. All agents—powerful or powerless, institutional or individual—face the same inexorable price mechanism. However, this apparent unanimity masks the committer-axis contestation: the institutional reading, the Georgist reading, and the financialization reading each decompose price formation differently, assigning agency and beneficiary structure where the naturalist reading sees only impersonal mechanism. The perspectival gap is not within this reading but between this reading and its siblings. Within this reading, the gap is minimal—all observers see the same Mountain.
 *
 * DIRECTIONALITY LOGIC:
 *   The naturalist reading declares beneficiaries (neoclassical economists and market efficiency advocates) not because the constraint extracts from them—it does not—but because the reading's acceptance as universal truth serves their intellectual and political interests. This is FSM authoring: a constraint claimed as Mountain but with identifiable beneficiaries. The beneficiary declarations trigger the false_summit_mountain signature in the engine, which will evaluate whether the naturalist reading's mountain status is genuine or a naturalization of an institutional arrangement. If the institutional reading is empirically defensible, the engine will reclassify this constraint from Mountain to Tangled Rope or Snare, revealing the false summit.
 *
 * MANDATROPHY ANALYSIS:
 *   READING-SPECIFIC MANDATROPHY: The naturalist reading's mandate is to defend price theory as a universal law of economics. The reading maintains this mandate successfully within professional economics and policy circles. No mandate-function decay is apparent—price theory remains central to economic training and policy analysis. However, the omega variables (kernel contestation, beneficiary structure, institutional invariance) document irreducible uncertainties about whether the mandate itself rests on natural law or ideological closure. The mandatrophy would be resolved not by showing the reading is wrong but by establishing (a) whether price formation is genuinely natural or constructed, and (b) whether the naturalist reading's dominance reflects empirical success or institutional power.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naturalist_vs_constructed_kernel,
    'Is price formation a natural law of economics (objective scarcity → equilibrium) or a constructed institutional arrangement (property rights + capital markets + regulatory regime → price signals)?',
    'Historical-comparative analysis: do different institutional arrangements (gift economies, feudal allocation, planned economies, market economies) produce genuinely different price-discovery mechanisms or do they all converge on the same equilibrium logic? Archeological/anthropological evidence on pre-market societies.',
    'If prices are natural: Mountain classification holds. Price theory is science, policy interventions are futile or harmful. If prices are constructed: False summit — naturalist reading is FSM candidate. Classification shifts to Tangled Rope or Snare depending on who benefits from the institutional arrangement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(naturalist_vs_constructed_kernel, conceptual, 'Whether price formation is natural law or institutional construction').

omega_variable(
    scarcity_definition_ambiguity,
    'Is ''scarcity'' an objective physical fact (finite resources, irreversible time) or a socially constructed category (what counts as a resource, what claims are honored)?',
    'Formal comparison: enumerate scarcities across different institutional frames (feudal land scarcity vs. modern regulatory scarcity vs. attention scarcity in digital markets). Test whether scarcity is intrinsic or frame-dependent.',
    'If scarcity is objective: naturalist reading stands. If scarcity is constructed: the ''natural'' equilibrium is actually conditioned on institutional choices (property regime, contract law, what can be owned). Price theory becomes a theory of how institutions define scarcity, not a discovery of objective scarcity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scarcity_definition_ambiguity, conceptual, 'Whether scarcity is objective or socially constructed').

omega_variable(
    beneficiary_false_summit,
    'Who benefits from the naturalist reading''s acceptance as universal truth? Does naturalism serve as ideological cover for distributions that are actually contingent on property and power structures?',
    'Beneficiary analysis: track which groups gain policy immunity, rent-protection, or bargaining advantage from the ''prices are natural'' framing. Compare policy outcomes under naturalist vs. institutional readings.',
    'If substantial beneficiaries exist: FSM trigger fires. The constraint is not Mountain but Tangled Rope at minimum, or Snare if beneficiaries actively suppress the institutional reading. Accessibility collapse remains high (natural law framing is genuinely persuasive) but emerges_naturally becomes contested.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(beneficiary_false_summit, empirical, 'Beneficiary structure underlying naturalist reading').

omega_variable(
    measurement_commensurability,
    'Are all prices (housing, labor, rents, wages, financial assets) produced by the same equilibrium mechanism, or do they obey different structural logics?',
    'Empirical comparison of price-discovery mechanisms across asset classes: housing, labor, equities, commodities, cryptocurrencies. Identify whether they satisfy equilibrium properties (price clears markets, no excess supply/demand in equilibrium).',
    'If unified mechanism: naturalist reading applies universally. If heterogeneous mechanisms: some prices may be discoveries (commodity prices in large markets) while others are constructed (housing in regulated markets, wages in hierarchical firms). The constraint must decompose into separate stories per asset class.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(measurement_commensurability, empirical, 'Whether all prices obey the same equilibrium mechanism').

omega_variable(
    institutional_invariance,
    'Do prices reach the same equilibrium values across different institutional arrangements (if institutional detail truly did not matter), or do institutional choices produce structurally different price paths?',
    'Comparative institutional analysis: study the same market under different regulatory regimes (e.g., housing prices under rent control vs. deregulation, labor prices under union vs. non-union regimes). Test whether prices converge or diverge structurally.',
    'If institutional detail is irrelevant: prices are discovered (mountain). If institutional choices alter equilibrium: prices are co-determined by institutions and preferences (tangled_rope or institutional_reading). This is the crux omega for the kernel decomposition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_invariance, empirical, 'Whether institutional arrangements affect equilibrium prices structurally').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(naturalist_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nat_tr_t0, naturalist_reading, theater_ratio, 0, 0.0).
narrative_ontology:measurement(nat_tr_t50, naturalist_reading, theater_ratio, 50, 0.0).
narrative_ontology:measurement(nat_tr_t100, naturalist_reading, theater_ratio, 100, 0.0).

% Extraction over time
narrative_ontology:measurement(nat_be_t0, naturalist_reading, base_extractiveness, 0, 0.0).
narrative_ontology:measurement(nat_be_t50, naturalist_reading, base_extractiveness, 50, 0.0).
narrative_ontology:measurement(nat_be_t100, naturalist_reading, base_extractiveness, 100, 0.0).

% Suppression requirement over time
narrative_ontology:measurement(nat_su_t0, naturalist_reading, suppression_requirement, 0, 0.0).
narrative_ontology:measurement(nat_su_t50, naturalist_reading, suppression_requirement, 50, 0.0).
narrative_ontology:measurement(nat_su_t100, naturalist_reading, suppression_requirement, 100, 0.0).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(naturalist_reading, resource_allocation).
narrative_ontology:affects_constraint(naturalist_reading, institutional_reading).
narrative_ontology:affects_constraint(naturalist_reading, georgist_reading).
narrative_ontology:affects_constraint(naturalist_reading, financialization_reading).
narrative_ontology:affects_constraint(naturalist_reading, housing_scarcity_narrative).
narrative_ontology:affects_constraint(naturalist_reading, efficient_market_hypothesis).

% DUAL FORMULATION NOTE:
% Price formation is a contested kernel decomposing into four structurally distinct readings. The naturalist reading claims Mountain (price is discovered). The institutional reading claims Tangled Rope (price is co-constructed by institutions and preferences). The Georgist reading claims Snare (land monopoly extracts rents that appear as price). The financialization reading claims Tangled Rope (financial engineering de-couples prices from underlying scarcity). Each reading has different ε, different beneficiary/victim structure, and different policy implications. They share the empirical domain (prices in modern capitalist economies) but instantiate incompatible causal mechanisms. Sibling readings are linked via network.affects_constraints from each story to the others, forming a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
