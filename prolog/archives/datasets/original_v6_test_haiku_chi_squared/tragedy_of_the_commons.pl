% ============================================================================
% CONSTRAINT STORY: tragedy_of_the_commons
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tragedy_of_the_commons, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: tragedy_of_the_commons
 *   human_readable: The Tragedy of the Commons
 *   domain: economic/social
 *
 * SUMMARY:
 *   The tragedy of the commons describes a structural dynamic where the
 *   rational pursuit of self-interest by autonomous agents collectively
 *   produces outcomes harmful to all — a classic coordination failure. The
 *   constraint operates across fisheries, forests, aquifers, grazing lands,
 *   fisheries, and atmospheric carbon. What makes it 'tragic' is that
 *   individuals benefit from extraction (reducing extraction hurts them
 *   individually) but suffer from collective depletion (everyone benefits
 *   from restraint). The structural tension creates pressures for
 *   institutional intervention, which introduces new extraction forms:
 *   regulatory rents, property-rights regimes, and tradeable permits. The
 *   constraint manifests differently depending on the observer's structural
 *   position: from the resource's perspective it is pure snare (extraction
 *   with no exit); from the subsistence user's perspective it is snare
 *   (survival necessity); from the extraction industry it is tangled rope
 *   (both benefits from low-cost access and pays enforcement costs if
 *   regulated); from the state/market coordinator it is tangled rope (profit
 *   from new regulations); from self-governing communities it is rope
 *   (symmetric information sharing); from transitional sustainability regimes
 *   it is scaffold (sunset as alternatives mature); from the Hardin narrative
 *   framework it is piton (once-live theory now inert through institutional
 *   dominance); from the civilizational analytical view it appears as
 *   mountain (immutable human nature) but base properties reveal this as
 *   false summit.
 *
 * KEY AGENTS:
 *   - Resource Ecosystem: Primary victim (powerless/trapped) — fisheries, forests, aquifers, atmosphere bear depletion costs with no exit or agency
 *   - Subsistence User: Primary victim (powerless/trapped) — small-scale fishers and pastoralists depend on commons for survival; rational extraction to meet need appears selfish when aggregated
 *   - Extraction Industry: Secondary beneficiary (organized/constrained) — commercial fishing, logging, mining corporations benefit from low-access-cost commons but constrained by capital lock-in and eventual enforcement
 *   - Institutional Coordinator (State/Market): Secondary beneficiary (powerful/arbitrage) — governments and property-rights regimes profit from new regulatory/permitting systems while solving coordination
 *   - Community Self-Governance Coalition: Tertiary actor (moderate/mobile) — user groups establish local commons management through norms and reciprocal monitoring; benefits/costs symmetric
 *   - Sustainable Transition Coalition: Organized actor (organized/constrained) — environmental regimes and impact investment create temporary scaffolds bridging to private stewardship or restoration
 *   - Hardin Narrative Framework: Institutional constraint (institutional/arbitrage) — conceptual frame persisting through textbook dominance despite empirical commons success; piton degradation
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional failures as immutable human nature
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tragedy_of_the_commons, 0.58).
domain_priors:suppression_score(tragedy_of_the_commons, 0.65).
domain_priors:theater_ratio(tragedy_of_the_commons, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tragedy_of_the_commons, extractiveness, 0.58).
narrative_ontology:constraint_metric(tragedy_of_the_commons, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(tragedy_of_the_commons, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tragedy_of_the_commons, tangled_rope).
narrative_ontology:human_readable(tragedy_of_the_commons, "The Tragedy of the Commons").
narrative_ontology:topic_domain(tragedy_of_the_commons, "economic/social").

domain_priors:requires_active_enforcement(tragedy_of_the_commons).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tragedy_of_the_commons, short_term_individual_extractors).
narrative_ontology:constraint_victim(tragedy_of_the_commons, long_term_resource_stewards).
narrative_ontology:constraint_victim(tragedy_of_the_commons, future_generations).
narrative_ontology:constraint_victim(tragedy_of_the_commons, collective_resource_base).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE RESOURCE ECOSYSTEM (SNARE) — Fisheries, forests, aquifers, and atmospheric commons cannot exit or organize. They bear full extraction cost of rational individual overuse. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.97. Pure extraction from the perspective of what is extracted.
constraint_indexing:constraint_classification(tragedy_of_the_commons, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE SUBSISTENCE USER (SNARE) — Small-scale fishers, pastoralists, and subsistence farmers depend on the commons for survival. Rational use of remaining resource to meet immediate need appears selfish when aggregated, but exit to alternative livelihood is blocked. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.80. Trapped by survival necessity.
constraint_indexing:constraint_classification(tragedy_of_the_commons, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: THE EXTRACTION INDUSTRY (TANGLED ROPE) — Commercial fishing, logging, and mining corporations benefit from open-access commons (low input costs) while bearing real enforcement costs if commons are to be managed. They have some exit options (investing in replacement resources, shifting sectors) but are constrained by capital lock-in. Benefits from coordination (stable supply chains) but extracts asymmetrically (externalize depletion costs). d≈0.55, f(d)≈0.75, σ=1.1 → χ≈0.48. Active enforcement creates the coordination function.
constraint_indexing:constraint_classification(tragedy_of_the_commons, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: THE INSTITUTIONAL COORDINATOR (STATE/MARKET) (TANGLED ROPE) — Governments and property rights regimes that impose commons management (quotas, permits, privatization) solve the coordination problem but extract through rent-seeking. They benefit from new regulatory systems (fines, licenses, taxes) while also bearing enforcement costs. Powerful institutions with arbitrage options (can shift governance models). d≈0.35, f(d)≈0.20, σ=1.2 → χ≈0.14. Effective extraction is low because they have alternatives; they coordinate because they profit from coordination.
constraint_indexing:constraint_classification(tragedy_of_the_commons, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: THE COMMUNITY SELF-GOVERNANCE COALITION (ROPE) — User groups that establish local commons management (traditional fishing rights, pastoral councils, water user associations) solve collective action through information sharing, social enforcement, and reciprocal monitoring. Benefits and costs are symmetric when norms are internalized. Mobile: members can exit to alternative commons if rules become unfair. d≈0.50, f(d)≈0.65, σ=0.9 → χ≈0.30. Pure coordination with low extraction because enforcement is horizontal.
constraint_indexing:constraint_classification(tragedy_of_the_commons, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 6: THE SUSTAINABLE TRANSITION COALITION (SCAFFOLD) — Environmental regimes (CITES, fisheries quotas, carbon markets) and impact investment frameworks are temporary coordination structures designed to bridge from open-access overexploitation to either private stewardship or commons restoration. They extract some coordination rents but have explicit sunset: as renewable energy replaces fossil fuels, carbon constraints ease; as aquaculture matures, wild fishery pressure declines. Has enforcement but declining theater as alternatives mature. d≈0.45, f(d)≈0.50, σ=1.1 → χ≈0.29. Constrained exit: requires alternative livelihoods to materialize.
constraint_indexing:constraint_classification(tragedy_of_the_commons, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: THE HARDIN NARRATIVE FRAMEWORK (PITON) — The tragedy-of-the-commons framing (from Garrett Hardin's 1968 essay) is a piton: it was once a live theoretical claim (rope/snare classification debate of 1960s-1980s). Over 50 years, empirical commons management has shown that community self-governance and property-rights solutions work widely, degrading the Hardin narrative's explanatory power. Yet the frame persists in policy and economics textbooks due to institutional inertia. theater_ratio≈0.80 (narrative dominance exceeds empirical support). d≈0.10, f(d)≈-0.05, σ=1.2 → χ≈-0.05. Negative effective extraction; the framework constrains productive commons solutions through false naturalization.
constraint_indexing:constraint_classification(tragedy_of_the_commons, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: THE ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational view, the fundamental tension between individual incentives and collective welfare is axiomatic in any resource-sharing system. No governance model eliminates this tension; it merely relocates it. This perspective sees the tragedy as an immutable structural feature of any commons, regardless of institutional form. However, base_properties reveal χ=0.58, requiring active enforcement and producing beneficiaries/victims — contradicting the mountain gate. The engine will classify this as a false summit.
constraint_indexing:constraint_classification(tragedy_of_the_commons, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tragedy_of_the_commons_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(tragedy_of_the_commons, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(tragedy_of_the_commons, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(tragedy_of_the_commons, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(tragedy_of_the_commons, TR),
    TR >= 0.70.

:- end_tests(tragedy_of_the_commons_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts significantly from resource ecosystems and subsistence users through depletion, but extraction is not total (some commons are sustainably managed at scale; Elinor Ostrom's research documents 1000+ stable commons). The value reflects that unmanaged commons experience severe extraction while managed commons show much lower rates. The interval trajectory (0.35→0.58) shows extractiveness increasing as commons are increasingly exposed to market pressures and population growth. Suppression (0.65): Moderate-high. Significant barriers exist to self-governance and exit: knowledge asymmetries (difficult to estimate sustainable yield), monitoring costs (enforcement against cheating), and institutional lock-in (existing property regimes resist change). But suppression is not total — community management has succeeded in thousands of cases. Suppression increases toward 0.70+ in highly degraded commons (fish stocks, tropical forests) and decreases toward 0.40-0.50 in actively managed commons with strong institutions. Theater ratio (0.35): Low-moderate. The constraint is primarily functional (actual depletion is real, not performative), but some theater exists in regulatory systems (permits and quotas create illusion of control that may exceed actual enforcement). Theater is lower than in verification or academic systems because the material consequences of overextraction are concrete.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates remarkable perspectival diversity from a single set of base properties. The resource ecosystem and subsistence user both see snare (powerless victims of extraction logic). The extraction industry sees tangled rope (benefits from open access but constrained by regulation). The state sees tangled rope (coordinates extraction while profiting from new rents). Communities see rope (symmetric coordination through norms). Sustainable transitions see scaffold (temporary coordination with explicit sunset). The Hardin narrative sees nothing — it has become piton, inert institutional framing. The analytical observer risks seeing mountain (immutable human nature) but the structural data contradicts this: beneficiary/victim asymmetry, active enforcement requirements, and high theater ratios all reveal the constraint as institutional, not natural law. The perspectival gap is extreme because the constraint is fundamentally one of institutional design rather than resource physics.
 *
 * DIRECTIONALITY LOGIC:
 *   Short-term individual extractors: Beneficiary + arbitrage (high-exit option through capital mobility) → d≈0.15, f(d)≈0.00. Net beneficiary; effectively no directional extraction from this actor's perspective because they can exit. Long-term resource stewards: Victim + trapped (subsistence dependence) → d≈0.95, f(d)≈1.42. Maximum directionality extraction. Future generations: Victim + trapped (no contemporaneous participation) → d≈1.0, f(d)≈1.42. Extraction maximum. Collective resource base: Victim + trapped (no agency) → d≈0.95, f(d)≈1.42. Extraction maximum. Extraction industry (constrained by regulation): Victim + constrained (capital lock-in) → d≈0.55, f(d)≈0.75. Moderate extraction when regulated. State/market (arbitrage options in governance models): Beneficiary + arbitrage → d≈0.35, f(d)≈0.20. Low extraction; multiple alternatives. Community self-governance (mobile, can switch commons) → d≈0.50, f(d)≈0.65. Symmetric (coordination benefit ≈ participation cost).
 *
 * MANDATROPHY ANALYSIS:
 *   The tragedy of the commons resolves the mandatrophy by revealing that the 'inevitability' claim is contingent on institutional assumptions. The Hardin original framing (1968) treated the tragedy as inevitable from rational-actor logic — a natural law waiting to be discovered. Fifty years of commons research by Ostrom, Dolšak, Agrawal, and others shows that the tragedy is NOT inevitable: communities can and do manage commons sustainably through property rights, monitoring, graduated sanctions, and conflict resolution. This means: (1) The mountain classification (natural law) is a false summit — base properties contradict it. (2) The constraint is primarily institutional failure, not human nature. (3) Successful commons management reduces extractiveness from 0.58 toward 0.30-0.40 (rope territory), showing the constraint is not intrinsic. (4) The Hardin frame persists as piton (theater_ratio 0.80) because it serves institutional interests (justifies state takeover or privatization) despite empirical refutation. Mandatrophy is resolved: the constraint is tangled_rope (not inevitably snare), and alternative institutions (rope/scaffold) exist at scale.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    commons_vs_coordination_boundary,
    'Is the tragedy of the commons a constraint on coordination or a constraint on resource depletion physics itself?',
    'Empirical analysis of commons systems that have achieved stable management (Swiss Alpine meadows, Balinese subak water systems, Pacific Island fisheries). Do they eliminate depletion incentives or simply enforce sustainable extraction rates?',
    'If depletion incentives are fundamental: tangled_rope classifications hold across all management regimes. If incentives are contingent on institutional failure: rope/scaffold classifications dominate when management succeeds, revealing the constraint as institutional (not natural law).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(commons_vs_coordination_boundary, empirical, 'Whether the tragedy is inherent to commons or contingent on institutional form').

omega_variable(
    extraction_vs_coordination_trade_off,
    'Do all solutions to the tragedy of the commons necessarily involve extractive rents for the coordinating institution, or can coordination be achieved with symmetric cost-sharing?',
    'Comparative study of commons governance: quota systems with rent extraction (fishing licenses, carbon permits), community self-governance with reciprocal monitoring (no external rent), and private stewardship with consumption-based extraction (ownership rents). Measure actual extraction asymmetry in each model.',
    'If all solutions extract: tangled_rope is inevitable (coordination + extraction linked). If rope solutions exist at scale: the constraint is a coordination problem (rope) in some regimes and extraction (snare/tangled_rope) only when bad institutions govern.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_vs_coordination_trade_off, empirical, 'Whether coordinating rents are inherent to commons solutions').

omega_variable(
    rational_actor_assumption_validity,
    'Do commons users actually behave as rational individual extractors, or are norms of sustainability internalized sufficiently that extraction is constrained by social preference rather than external enforcement?',
    'Behavioral experiments (common-pool resource games with and without communication). Ethnographic study of actual commons users'' decision-making. Compare stated extraction intentions with actual harvesting behavior in communities with established norms vs new-access regimes.',
    'If rational extraction dominates: snare/tangled_rope classifications correct; tragedy is inevitable without enforcement. If norms constrain voluntarily: rope classifications dominate; the tragedy may be largely a problem of institutional design, not human nature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rational_actor_assumption_validity, empirical, 'Whether commons users behave as rational extractors or norm-constrained stewards').

omega_variable(
    substitution_elasticity_across_commons,
    'Are different commons substitutable from the user''s perspective (can a fisher move to a different fishery if one is depleted), or are some commons irreplaceable?',
    'Economic analysis of exit options for commons users. Empirical measurement of switching costs and availability of alternatives for fishers, pastoralists, water users across different regimes.',
    'High substitution: exit_options are more mobile than ''trapped'' suggests; classifications shift toward rope/scaffold. Low substitution: trapped is accurate; victims'' d values and snare classifications hold.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(substitution_elasticity_across_commons, empirical, 'Whether commons are substitutable or users face genuine trap').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tragedy_of_the_commons, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(toc_tr_t0, tragedy_of_the_commons, theater_ratio, 0, 0.25).
narrative_ontology:measurement(toc_tr_t25, tragedy_of_the_commons, theater_ratio, 25, 0.3).
narrative_ontology:measurement(toc_tr_t50, tragedy_of_the_commons, theater_ratio, 50, 0.35).

% Extraction over time
narrative_ontology:measurement(toc_be_t0, tragedy_of_the_commons, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(toc_be_t25, tragedy_of_the_commons, base_extractiveness, 25, 0.5).
narrative_ontology:measurement(toc_be_t50, tragedy_of_the_commons, base_extractiveness, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tragedy_of_the_commons, resource_allocation).
narrative_ontology:affects_constraint(tragedy_of_the_commons, overfishing_sustainability_window).
narrative_ontology:affects_constraint(tragedy_of_the_commons, deforestation_tipping_point).
narrative_ontology:affects_constraint(tragedy_of_the_commons, groundwater_aquifer_depletion).
narrative_ontology:affects_constraint(tragedy_of_the_commons, atmospheric_carbon_commons).

% DUAL FORMULATION NOTE:
% The tragedy of the commons is a meta-constraint affecting specific resource systems (fisheries, forests, aquifers, atmosphere). Each specific resource has its own constraint story with domain-specific ε values, but all are downstream of this institutional constraint structure. The generic tragedy is upstream; specific resource tragedies are instances of institutional failure in commons governance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(tragedy_of_the_commons, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
