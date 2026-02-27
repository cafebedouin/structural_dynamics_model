% ============================================================================
% CONSTRAINT STORY: meta_nuclear_power_agreement
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_meta_nuclear_power_agreement, []).

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
 *   constraint_id: meta_nuclear_power_agreement
 *   human_readable: Meta's direct investment and offtake agreements for advanced nuclear power
 *   domain: technological/economic
 *
 * SUMMARY:
 *   Meta's direct investment and long-term offtake agreements with advanced
 *   nuclear power developers (particularly Small Modular Reactors) represent
 *   a structural shift in energy markets where hyperscale data center
 *   operators bypass traditional utility procurement and competitive energy
 *   markets to secure dedicated low-carbon power. This constraint exhibits
 *   characteristics of both pure coordination (solving the financing problem
 *   for novel reactor designs) and extractive rent-seeking (using market
 *   power to lock out competitors and circumvent regulatory oversight). The
 *   same structural arrangement appears as enabling coordination to some
 *   observers (decarbonization advocates, nuclear developers), as extraction
 *   to others (competitive energy market, incumbent utilities, public
 *   interest), and as a degraded institutional form (regulated utility model)
 *   to still others. The constraint's rising extractiveness over the
 *   measurement interval (0.28→0.52) reflects increasing market power
 *   consolidation as other hyperscale operators and industrial users follow
 *   Meta's precedent, creating barriers to entry for smaller competitors.
 *   Theater ratio remains moderate (0.48) because the agreements function
 *   substantially as real power-purchase mechanisms rather than performative
 *   rituals, though regulatory displacement (bypassing public commission
 *   oversight) introduces some theatrical element.
 *
 * KEY AGENTS:
 *   - Meta Corporation: Primary beneficiary (powerful/mobile) — secures long-term fixed-price power, influences reactor design, builds strategic infrastructure moat
 *   - Advanced Nuclear Developers (e.g., Commonwealth Fusion Systems, Terrapower, NuScale): Primary beneficiary (institutional/arbitrage) — obtain de-risked financing and committed offtake guarantee enabling project viability
 *   - Competitive Energy Market: Primary victim (powerless/trapped) — locked out of supply; cannot negotiate equivalent terms; face coordination failure in power procurement
 *   - Incumbent Utilities and Grid Operators: Secondary victim (moderate/constrained) — lose load to private agreements; retain regulatory obligation to serve remaining customers; face cost recovery challenges
 *   - Other Hyperscale Operators (Google, Amazon, Microsoft): Competitive victims (powerful/mobile) — excluded from earliest offtake agreements; forced to follow Meta's negotiation playbook or remain dependent on wholesale markets
 *   - Regulatory Agencies (FERC, state utility commissions): Constrained beneficiaries (organized/constrained) — benefit from decarbonization outcomes but lose jurisdiction over power procurement and pricing
 *   - Decarbonization Coalition (environmental NGOs, grid modernization advocates): Beneficiaries with reservations (organized/constrained) — support advanced nuclear deployment but concerned about competitive market erosion
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(meta_nuclear_power_agreement, 0.52).
domain_priors:suppression_score(meta_nuclear_power_agreement, 0.58).
domain_priors:theater_ratio(meta_nuclear_power_agreement, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(meta_nuclear_power_agreement, extractiveness, 0.52).
narrative_ontology:constraint_metric(meta_nuclear_power_agreement, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(meta_nuclear_power_agreement, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(meta_nuclear_power_agreement, tangled_rope).
narrative_ontology:human_readable(meta_nuclear_power_agreement, "Meta's direct investment and offtake agreements for advanced nuclear power").
narrative_ontology:topic_domain(meta_nuclear_power_agreement, "technological/economic").

domain_priors:requires_active_enforcement(meta_nuclear_power_agreement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(meta_nuclear_power_agreement, advanced_nuclear_developers).
narrative_ontology:constraint_beneficiary(meta_nuclear_power_agreement, meta_corporation).
narrative_ontology:constraint_victim(meta_nuclear_power_agreement, competitive_energy_market).
narrative_ontology:constraint_victim(meta_nuclear_power_agreement, incumbent_utilities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COMPETITIVE ENERGY MARKET (SNARE) — Cannot exit the vertically integrated power-purchase agreement; bears full cost of Meta's preferential access to novel nuclear capacity. Smaller competitors and distributed energy providers face locked-out supply while Meta secures long-term fixed pricing. d≈0.92, f(d)≈1.40, σ=1.0 → χ≈0.73.
constraint_indexing:constraint_classification(meta_nuclear_power_agreement, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INCUMBENT UTILITIES (TANGLED ROPE) — Constrained by regulatory obligations to serve all load and maintain grid stability, but also benefit from Meta's capital reducing their own decarbonization costs. Direct private power agreements extract monopoly rents while coordination through bulk power markets enables efficient dispatch. d≈0.70, f(d)≈1.08, σ=0.9 → χ≈0.54.
constraint_indexing:constraint_classification(meta_nuclear_power_agreement, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ADVANCED NUCLEAR DEVELOPERS (ROPE) — Benefit from Meta's offtake guarantee and capital investment; solves coordination problem of bringing novel SMR technology to market by de-risking financing. Meta's long-term demand commitment enables construction financing. Experience constraint as enablement. d≈0.08, f(d)≈-0.11, σ=1.1 → χ≈-0.06.
constraint_indexing:constraint_classification(meta_nuclear_power_agreement, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: META CORPORATION (TANGLED ROPE) — Benefits from long-term fixed-price power access, preferential routing, and technology development influence. Simultaneously enforces constraint through market power: ability to sign exclusive offtakes that competitors cannot match. Exit options are mobile (could negotiate with other developers, use traditional grid); chooses direct agreements for strategic advantage. d≈0.35, f(d)≈0.35, σ=1.1 → χ≈0.20.
constraint_indexing:constraint_classification(meta_nuclear_power_agreement, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: DECARBONIZATION COALITION (SCAFFOLD) — Organized agents (environmental NGOs, grid modernization advocates, state renewable energy portfolios) see Meta's nuclear offtakes as temporary coordination that accelerates decarbonization, but extract a cost: lock-in of private power deals that bypass competitive procurement. The sunset logic is ambiguous: advanced nuclear developers eventually reach cost parity and market maturity, but the exclusive offtake structure may persist beyond its coordination necessity. d≈0.45, f(d)≈0.44, σ=1.0 → χ≈0.21.
constraint_indexing:constraint_classification(meta_nuclear_power_agreement, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: REGULATORY AGENCIES / PUBLIC INTEREST (TANGLED ROPE) — Constrained by statutory mandate to ensure public benefit and competitive markets; also benefit from Meta's acceleration of advanced nuclear deployment and grid decarbonization. Direct private offtakes extract cost: circumvent public procurement, reduce regulatory oversight of pricing, and create precedent for monopoly power in energy markets. Suppression is high because utility commissions lack statutory tools to mandate competitive bidding for private power buyers. d≈0.65, f(d)≈1.00, σ=1.0 → χ≈0.52.
constraint_indexing:constraint_classification(meta_nuclear_power_agreement, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: REGULATED UTILITY BUSINESS MODEL (PITON) — The exclusive service territory model (one utility per region, rate-of-return regulation) is largely inert, maintained through regulatory grandfathering and incumbent lobbying despite obsolescence in competitive wholesale markets. Direct private power deals (Meta → SMR developer) bypass this entirely, revealing the model as theatrical: the illusion of universal service obligation persists while actual power flows follow bilateral contracts. theater_ratio=0.58 indicates moderate theatrical component; the utility retains some coordination function (grid stability, emergency dispatch) but the primary value-add (stable power supply) is captured by private agreements.
constraint_indexing:constraint_classification(meta_nuclear_power_agreement, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, large energy consumers purchasing power via bilateral contracts is a fundamental principle of competitive markets: agents with large, predictable demand can negotiate better terms than retail consumers. This appears immutable until you notice the constraint structures it: Meta's market power, nuclear developers' financing constraints, and utilities' regulatory handcuffs are all contingent institutional arrangements, not laws of physics. The engine's false summit detector will flag this.
constraint_indexing:constraint_classification(meta_nuclear_power_agreement, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(meta_nuclear_power_agreement_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(meta_nuclear_power_agreement, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(meta_nuclear_power_agreement, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(meta_nuclear_power_agreement, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(meta_nuclear_power_agreement, TR),
    TR >= 0.70.

:- end_tests(meta_nuclear_power_agreement_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, rising. The initial value (0.28) reflects that Meta's first offtake agreements were solving genuine coordination problems for developers with insufficient capital to finance risky SMR projects. As deployment scales and other operators follow Meta's model, extractiveness rises (0.40→0.52) because the constraint now locks out competitors from maturing SMR supply while maintaining exclusive pricing advantages. The extraction is not as severe as pure snare (0.66+) because the agreements do deliver coordination value: they accelerate decarbonization and reduce energy-related emissions. However, extractiveness exceeds rope threshold (0.45) because of market power asymmetry and regulatory circumvention. Suppression (0.58): Moderate-high. Significant barriers prevent competitive responses: SMR developers have limited capacity and prefer assured offtakes over speculative wholesale markets; competitors cannot replicate Meta's scale and financing capacity; regulatory agencies lack statutory tools to mandate competitive bidding for private power; incumbent utilities face regulatory constraint to serve all load while losing profitable customers. Barriers are not absolute (utilities could compete through private agreements, regulators could change statutes) but friction is high. Theater ratio (0.48): Moderate. The agreements function substantially as real power purchases (not performative) but include theatrical elements: regulatory filing requirements present agreements as beneficial to the public interest while bypassing public comment; claims of 'enabling decarbonization' obscure market power dynamics; developer narratives emphasize technological breakthrough while downplaying financing dependency on Meta's capital.
 *
 * PERSPECTIVAL GAP:
 *   Meta's offtake agreements appear as pure coordination (Rope) from advanced nuclear developers' perspective — the agreements solve their capital and offtake risk, enabling technology deployment that would not happen otherwise. Simultaneously, from the competitive energy market's perspective, the same agreements appear as pure extraction (Snare) — competitors cannot negotiate equivalent terms, supply is locked out, pricing advantages persist. From the regulatory perspective, the agreements are simultaneously beneficial (accelerating decarbonization) and harmful (undermining competitive markets and regulatory jurisdiction). The perspectival gap is extreme: the same contract has χ ≈ -0.06 (net subsidy) from the developer's view and χ ≈ 0.73 (net extraction) from the competitive market's view. The large gap indicates that Meta's market power is the decisive variable: the developer needs Meta's capital and would accept unfavorable terms; competitors cannot replicate Meta's terms even if willing to pay more. This asymmetry is the hallmark of tangled_rope: genuine coordination function (developer financing) overlaid with asymmetric extraction (market power over competitors).
 *
 * DIRECTIONALITY LOGIC:
 *   Competitive energy market: Victim + trapped → d≈0.92, f(d)≈1.40. Cannot exit or negotiate; full extraction target. Incumbent utilities: Victim + constrained → d≈0.70, f(d)≈1.08. Significant extraction (loss of profitable customers, load reduction) but not maximum (retain baseline service obligation, can compete in some markets). Advanced nuclear developers: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11. Strong beneficiary; have arbitrage options (alternative financing, utility partnerships) but strongly prefer Meta's terms. Meta: Beneficiary + mobile → d≈0.35, f(d)≈0.35. Primary beneficiary; high exit options (could use grid, negotiate other offtakes) but chooses direct agreements for competitive advantage. Regulatory agencies: Mixed (organized/constrained) → d≈0.65, f(d)≈1.00. Bear costs of market fragmentation and jurisdictional erosion; benefit from decarbonization outcomes. Decarbonization coalition: Beneficiary + constrained → d≈0.45, f(d)≈0.44. Net beneficiary (support advanced nuclear) but concerned about competitive market erosion and precedent.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves the mandatrophy by demonstrating that it is genuinely a tangled_rope, not a false dichotomy of pure coordination vs. pure extraction. The coordination function is real and necessary: advanced nuclear developers cannot finance SMR projects without offtake commitments; Meta's capital reduces financing risk and enables deployment. The extraction is also real: Meta's market power locks out competitors, creates rent-seeking opportunities through exclusive pricing, and circumvents regulatory oversight. Both functions coexist in the same contractual structure. The mandatrophy question 'Is this coordination or extraction?' has answer 'both, from different perspectives.' The beneficiary (Meta) experiences it as low-cost, stable coordination. The victim (competitive market) experiences it as monopolistic extraction. The analytical observer sees the hybrid. The rising extractiveness trajectory (0.28→0.52) indicates that as the technology matures and supply increases, the coordination justification weakens while the extraction component strengthens. A well-designed sunset clause would address this: offtake agreements could include price escalation, capacity release, or conversion to market-based pricing as SMR deployment scales. Current agreements do not include such provisions, suggesting extraction will persist beyond the coordination necessity window.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    offtake_pricing_extraction_threshold,
    'At what price discount relative to competitive market rates does Meta''s offtake agreement constitute extractive rent-seeking vs. competitive advantage?',
    'Comparison of Meta''s negotiated rates against levelized cost of electricity (LCOE) for equivalent SMR capacity; benchmarking against independent power producer (IPP) contract terms; regulatory rate review proceedings',
    'If discount < 10% of LCOE: market-competitive, tangled_rope classification confirmed. If discount > 25% of LCOE: extractive, snare classification from competitive market perspective strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(offtake_pricing_extraction_threshold, empirical, 'Pricing threshold distinguishing competitive advantage from extraction').

omega_variable(
    exclusive_vs_committed_capacity,
    'Does Meta''s offtake agreement lock out all competitors from SMR capacity, or does it commit Meta to a portion while allowing other offtakers?',
    'Contract analysis of exclusivity clauses; review of SMR developer''s stated capacity allocation and commitment to distributed offtakes; interviews with competing data center operators attempting to negotiate',
    'If fully exclusive: snare for competitors strengthens, entrenching market power. If partial commitment with open offtakes: tangled_rope confirms, market remains contestable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exclusive_vs_committed_capacity, empirical, 'Whether Meta''s agreement is exclusive or allows competitor access').

omega_variable(
    nuclear_developer_financing_dependency,
    'To what degree is the advanced nuclear developer dependent on Meta''s capital and offtake commitment for project viability? Would alternative financing (venture capital, government subsidy, utility partnerships) permit equivalent deployment?',
    'Capital structure analysis of SMR developers; counterfactual financing scenarios; comparison of deployment timelines with vs. without Meta partnerships; interviews with developers on financing constraints',
    'If high dependency (>80% of capital): Meta''s coordination function is genuine and necessary, rope/tangled_rope confirmed. If low dependency (<30%): Meta is capturing already-viable projects, snare interpretation strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nuclear_developer_financing_dependency, empirical, 'Degree of SMR developer dependency on Meta''s capital').

omega_variable(
    regulatory_approval_acceleration,
    'Does Meta''s financial backing accelerate regulatory approval of novel SMR designs? If so, is this acceleration purely enabling (removing financing delays) or extractive (creating precedent for private power buyers to override public procurement)?',
    'Timeline analysis of NRC licensing for Meta-backed vs. utility-backed SMR projects; regulatory comment review for precedent-setting arguments; interviews with NRC staff on influence patterns',
    'If purely enabling: scaffold/rope logic confirmed. If precedent-setting: establishes regulatory capture risk, strengthens snare classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_approval_acceleration, conceptual, 'Whether Meta backing accelerates or distorts nuclear regulation').

omega_variable(
    grid_stability_coordination_necessity,
    'Do Meta''s advanced nuclear offtakes provide genuine coordination services to the grid (predictable baseload, voltage support, reactive power), or do they extract grid services without compensation while pursuing independent power agreements?',
    'Grid modeling of Meta''s load profile; comparison of actual service contributions against wholesale market rates for equivalent services; utility cost-benefit analysis',
    'If genuine coordination: rope/tangled_rope classification confirmed. If extraction of services: snare from utility perspective strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(grid_stability_coordination_necessity, empirical, 'Whether Meta''s load provides genuine grid services or extracts them').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(meta_nuclear_power_agreement, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(meta_nuc_tr_t0, meta_nuclear_power_agreement, theater_ratio, 0, 0.35).
narrative_ontology:measurement(meta_nuc_tr_t3, meta_nuclear_power_agreement, theater_ratio, 3, 0.42).
narrative_ontology:measurement(meta_nuc_tr_t6, meta_nuclear_power_agreement, theater_ratio, 6, 0.48).

% Extraction over time
narrative_ontology:measurement(meta_nuc_be_t0, meta_nuclear_power_agreement, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(meta_nuc_be_t3, meta_nuclear_power_agreement, base_extractiveness, 3, 0.4).
narrative_ontology:measurement(meta_nuc_be_t6, meta_nuclear_power_agreement, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(meta_nuclear_power_agreement, resource_allocation).
narrative_ontology:affects_constraint(meta_nuclear_power_agreement, utility_cost_recovery_crisis).
narrative_ontology:affects_constraint(meta_nuclear_power_agreement, small_modular_reactor_financing_gap).
narrative_ontology:affects_constraint(meta_nuclear_power_agreement, competitive_energy_market_fragmentation).

% DUAL FORMULATION NOTE:
% Meta's nuclear offtake agreements represent a constraint family decomposing into three structurally distinct claims: (1) the financing coordination problem for SMR developers (ε≈0.15, Rope) — solved by Meta's capital commitment; (2) the market power extraction over competitors (ε≈0.65, Snare) — enabled by Meta's scale and exclusive agreements; (3) the regulatory capture of utility commission authority (ε≈0.45, Tangled Rope) — where private agreements bypass public procurement. These three should be tracked as separate constraints linked through network effects. The unified story (claimed_type: tangled_rope, ε=0.52) represents the composite, but decomposition enables tracking the upstream coordination function separately from the downstream extraction effects.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(meta_nuclear_power_agreement, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
