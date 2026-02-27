% ============================================================================
% CONSTRAINT STORY: legibility_trap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legibility_trap, []).

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
 *   constraint_id: legibility_trap
 *   human_readable: The Grid-Map Displacement
 *   domain: political/social/economic
 *
 * SUMMARY:
 *   The legibility trap arises when a state or institution imposes
 *   simplified, standardized metrics (cadastral surveys, property
 *   classifications, labor codes, health bureaucracies) onto a complex,
 *   organic social system to make it 'governable.' This mechanism appears as
 *   a Rope for administrative coordination: standardized land tenure enables
 *   tax collection, contract enforcement, and resource planning. But it
 *   functions simultaneously as a Snare for those whose livelihoods, safety
 *   nets, and cultural institutions depend on high-resolution local
 *   knowledge—commons systems, informal credit networks, indigenous land
 *   management practices, gift economies, and dispute resolution through
 *   kinship and reputation. The grid-map's abstraction liquidates local
 *   knowledge and concentrates coercive power in those who control the formal
 *   system. Over the 25-year interval studied, extractiveness rises from 0.22
 *   (early legibility—still co-existing with informal systems) to 0.58
 *   (mature legibility—informal systems liquidated, populations fully
 *   dependent on formal categories). Theater ratio rises from 0.35 to 0.68,
 *   indicating that the cadastral system's performative content increases as
 *   its coordination function is completed: verification rituals persist long
 *   after the problem they solved has been solved.
 *
 * KEY AGENTS:
 *   - Local communities with commons-based livelihoods: Primary victims (powerless/trapped) — bear costs of classification rigidity and loss of informal networks; cannot exit without losing cultural continuity
 *   - Indigenous land management systems: Primary victims (powerless/trapped) — eradicated by grid classification; no formal representation in property law
 *   - Informal safety nets and kinship insurance: Victim category (powerless/trapped) — replaced by formal welfare that requires legibility but provides less personalized coverage
 *   - State administrative apparatus: Primary beneficiary (institutional/arbitrage) — gains ability to tax, conscript, and govern through standardized metrics
 *   - Formal sector entrepreneurs and banks: Secondary beneficiary (moderate/constrained) — benefit from legible property rights and credit markets; constrained by standardization requirements
 *   - Development institutions (World Bank, IMF, land-reform consultants): Organized beneficiary (organized/constrained) — promote legibility as modernization; capture rents through consulting and conditional lending
 *   - Cadastral systems and property registries: Institutional actor (institutional/constrained) — persist through bureaucratic inertia even as coordination function is completed; theater ratio indicates performative maintenance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legibility_trap, 0.58).
domain_priors:suppression_score(legibility_trap, 0.72).
domain_priors:theater_ratio(legibility_trap, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legibility_trap, extractiveness, 0.58).
narrative_ontology:constraint_metric(legibility_trap, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(legibility_trap, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legibility_trap, tangled_rope).
narrative_ontology:human_readable(legibility_trap, "The Grid-Map Displacement").
narrative_ontology:topic_domain(legibility_trap, "political/social/economic").

domain_priors:requires_active_enforcement(legibility_trap).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legibility_trap, state_administrative_apparatus).
narrative_ontology:constraint_beneficiary(legibility_trap, centralized_planning_bureaucracy).
narrative_ontology:constraint_victim(legibility_trap, local_informal_safety_nets).
narrative_ontology:constraint_victim(legibility_trap, indigenous_land_management_systems).
narrative_ontology:constraint_victim(legibility_trap, organic_community_institutions).
narrative_ontology:constraint_victim(legibility_trap, low_income_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISPLACED LOCAL COMMUNITY (SNARE) — Trapped by relocation, standardized land tenure, loss of informal reciprocity networks, and legal classification systems that do not recognize commons or gift economies. Exit requires abandoning generational knowledge and social position. d≈0.92, f(d)≈1.38, σ=0.9 → χ≈0.72.
constraint_indexing:constraint_classification(legibility_trap, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: LIQUIDATED COMMONS (SNARE) — Shared forests, water rights, grazing lands, and informal dispute-resolution mechanisms cannot survive the grid's property classification. No exit option: the commons ceases to exist once privatized or cadastrally registered. d≈0.95, f(d)≈1.42, σ=0.8 → χ≈0.65.
constraint_indexing:constraint_classification(legibility_trap, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: FORMAL SECTOR ENTREPRENEUR (TANGLED ROPE) — Benefits from legible property rights, contract enforcement, and access to credit secured against registered land. Constrained by rigid classification that eliminates informal business models. Experiences both coordination (registration enables contracts) and extraction (complying with standardization regulations imposes compliance costs). d≈0.55, f(d)≈0.75, σ=0.9 → χ≈0.39.
constraint_indexing:constraint_classification(legibility_trap, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: STATE ADMINISTRATIVE APPARATUS (ROPE) — Solves the coordination problem of tax collection, resource allocation, and law enforcement by making the social landscape legible. Cadastral surveys, standardized classifications, and formalized tenure enable the apparatus to see and govern the population. d≈0.08, f(d)≈-0.08, σ=1.0 → χ≈-0.004. Net beneficiary; the constraint is a genuine coordination mechanism from this perspective.
constraint_indexing:constraint_classification(legibility_trap, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: DEVELOPMENT SPECIALIST / INTERNATIONAL LENDER (TANGLED ROPE) — Organized actors (World Bank, IMF, land-reform consultants) promote grid legibility as essential modernization. Coordination function: enabling land markets, contract enforcement, and credit access. Extraction: imposing one model globally despite local knowledge, capturing rents through consulting fees and loan conditions. d≈0.45, f(d)≈0.45, σ=1.1 → χ≈0.29.
constraint_indexing:constraint_classification(legibility_trap, tangled_rope,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: CADASTRAL SYSTEM (PITON) — The grid has become institutionalized: surveyors, registrars, property lawyers, and judges depend on its maintenance. Theater ratio high because much of the work is performative (formal verification of boundaries that nobody disputes) rather than resolving actual property conflicts. Persists through bureaucratic inertia even as its core coordination function has been accomplished. theater_ratio=0.68 suggests significant but not dominant performativity. d≈0.30, f(d)≈0.20, σ=1.0 → χ≈0.012.
constraint_indexing:constraint_classification(legibility_trap, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (ATTEMPTED MOUNTAIN, FAILS) — Naturalized narrative: states require legible populations for taxation and defense; this is an immutable law of statecraft. Accessibility_collapse would need to be ≥0.85, but the structural data (ε=0.58, suppression=0.72, theater=0.68, requires_active_enforcement=true) contradicts this. The false summit detector fires: legibility is contingent, not a law of nature. The constraint persists through institutional enforcement and ideology, not because alternatives are impossible.
constraint_indexing:constraint_classification(legibility_trap, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legibility_trap_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(legibility_trap, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(legibility_trap, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(legibility_trap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(legibility_trap, TR),
    TR >= 0.70.

:- end_tests(legibility_trap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The grid-map extraction increases over time as informal systems are liquidated. Early legibility allows coexistence (ε=0.22): communities can comply with formal classification while maintaining informal networks. Mature legibility forces dependence (ε=0.58): the formal system is the only recognized pathway for property rights, credit access, and dispute resolution. The intermediate value reflects that while state apparatus genuinely solves coordination problems (taxation, contract enforcement), it does so by concentrating power and eliminating alternatives. Suppression (0.72): High. The grid suppresses alternatives through legal prohibition of commons, informal credit, and non-standard dispute resolution; through displacement of populations who resist classification; and through cultural delegitimation of non-legible knowledge. But suppression is not absolute because informal systems persist in practice, evade registration, and maintain parallel legitimacy. Theater ratio (0.68): Moderate-high. Cadastral work begins with genuine coordination content (surveying, establishing first-time property boundaries). As the system matures, much work becomes performative: verifying boundaries nobody contests, maintaining registries against claims already settled, executing formalities for transactions that informal networks had already negotiated. The theater ratio increases over time as the coordination problem is solved but the administrative machinery persists.
 *
 * PERSPECTIVAL GAP:
 *   Extreme divergence. The state apparatus sees pure coordination (Rope): creating legible categories enables taxation, law enforcement, and planning. The displaced local community sees pure extraction (Snare): loss of commons, informal insurance, and cultural legitimacy, with no exit option. The formal sector entrepreneur sees mixed coordination-extraction (Tangled Rope): legible property rights enable credit access, but standardization eliminates informal business models. The development specialist sees beneficial modernization (Rope toward Scaffold): legibility is temporary friction enabling long-term development, though the sunset clause is weak. The analytical observer is tempted to naturalize the constraint (Mountain): 'states require legible populations'—but the structural data reveals this as a false summit. The grid persists through enforcement and ideological work, not because alternatives are impossible (non-legible medieval cities coordinated complex economies; contemporary underground and diaspora networks coordinate significant production). The constraint is a choice that acquires the appearance of necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   State administrative apparatus: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08. Net beneficiary. Creates legibility, captures taxation and coercive power, experiences the constraint as pure coordination. Displaced local community: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction. Cannot exit without abandoning generational position, cultural knowledge, and kinship networks. The grid is imposed through legal force and displacement, not consent. Liquidated commons: Victim + trapped → d≈0.95, f(d)≈1.42. Maximum extraction and destruction. Commons cease to exist once property-classified; no exit option exists because the exit is the constraint itself. Formal sector entrepreneur: Victim + constrained → d≈0.60, f(d)≈0.85, mixed with beneficiary status. Experiences both coordination (legible property enables contracts and credit) and extraction (standardization requirements eliminate informal business models). d≈0.55 reflects the mixed position. Development specialist: Organized beneficiary + constrained → d≈0.45, f(d)≈0.45. Promotes legibility but constrained by political pushback and implementation friction. Genuine coordination function but also capture of rents through conditional lending and consulting. Cadastral system: Institutional + constrained → d≈0.30, f(d)≈0.20. Piton classification from theater ratio, not from high chi. The system persists through bureaucratic inertia; d is low because the institution benefits from the constraint but is not targeting anyone specifically.
 *
 * MANDATROPHY ANALYSIS:
 *   CRITICAL RESOLUTION: This constraint resolves the mandatrophy by revealing that legibility functions as BOTH coordination and extraction, but from structurally different positions. From the state apparatus, it is pure coordination—the mechanism by which a large-scale political economy becomes governable. From the displaced community, it is pure extraction—the mechanism by which local power and informal institutions are liquidated in service to centralized administration. This is not a measurement error or observer bias. The perspectival gap is STRUCTURAL. The state apparatus genuinely needs legibility to provide public goods (defense, infrastructure, rule of law). The displaced community genuinely loses capabilities and options when forced into legible categories. Both claims are true. The mandatrophy does not dissolve—it SHARPENS the diagnosis: legibility is a transfer of coordination capacity from local to central levels. Informal systems (kinship networks, reputation-based dispute resolution, commons management) coordinate production, exchange, and insurance at the local level with lower overhead than formal bureaucracy. The grid-map transfers this coordination to the state, where economies of scale and standardization provide benefits (credit market access, tax-funded infrastructure, uniform law) at the cost of losing high-resolution local knowledge and informal resilience. The mandatrophy asks: do the state-level benefits outweigh the loss of local-level coordination capacity? The constraint's trajectory over 25 years suggests diminishing returns: early legibility (ε rises from 0.22 to 0.45) provides coordination gains; mature legibility (ε=0.58) extracts rents through a bureaucratic system that has solved the original coordination problem but persists through institutional inertia (theater ratio 0.68). The omega variables identify the empirical cruxes that would determine whether this is a necessary tradeoff or an extractive power grab dressed in modernization rhetoric.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    organic_coordination_viability,
    'Can complex societies of >100,000 people coordinate production, exchange, and security without centralized legible metrics?',
    'Historical case studies of pre-grid urban societies (medieval city-states, Islamic waqf networks, merchant guild systems); contemporary examples of non-legible coordination (underground economies, diaspora remittance networks, open-source software production)',
    'If yes: legibility is coordination choice, not structural necessity. Constraint shifts more toward Snare. If no: legibility is unavoidable for scale. Constraint remains Tangled Rope but with weaker mandatrophy tension.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(organic_coordination_viability, empirical, 'Whether organic coordination can scale without legible metrics').

omega_variable(
    informal_safety_net_substitutability,
    'Do formal welfare systems with legible beneficiary targeting replace informal mutual aid and kinship insurance at equivalent functional levels?',
    'Comparison of risk-pooling effectiveness: household income volatility before/after formalization; cross-sectional data on coping strategies in legible vs non-legible economies; longitudinal tracking of household resilience metrics',
    'If formal systems are superior: victims bear short-term losses for long-term stability. Constraint becomes Scaffold with sunset. If formal systems fail equivalence: legibility trap extracts resilience. Constraint remains Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(informal_safety_net_substitutability, empirical, 'Whether formal welfare replaces informal safety nets functionally').

omega_variable(
    legibility_threshold_function,
    'What is the minimal legibility required for a state''s core functions (taxation, defense, dispute resolution) versus the level imposed in contemporary legibility regimes?',
    'Functional decomposition of state capacity requirements; measurement of actual legibility burden relative to minimal effective levels; case studies of legibility reduction experiments',
    'If minimal legibility << actual imposed: extraction is choice, not necessity. Constraint shows high mandatrophy (can be unwound). If minimal legibility ≈ actual: constraint is tighter Rope than Tangled Rope. Mandatrophy weakens.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legibility_threshold_function, empirical, 'Minimal legibility required versus actual imposed levels').

omega_variable(
    cultural_epistemic_collapse,
    'Is loss of local knowledge after grid imposition permanent or recoverable through post-grid revitalization?',
    'Documentation of knowledge recovery in communities where grid is relaxed (e.g., land-back movements, commons restoration); intergenerational transmission analysis in hybrid systems; ethnographic assessment of whether local categories can be reconstructed after legible classification removes them',
    'If permanent: extraction is irreversible. Victims cannot revert even if legibility is lifted. If recoverable: extraction is extractive rather than destructive. Affects long-term classification as Snare vs Tangled Rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cultural_epistemic_collapse, empirical, 'Whether local knowledge is permanently or reversibly lost under legibility').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legibility_trap, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legib_tr_t0, legibility_trap, theater_ratio, 0, 0.35).
narrative_ontology:measurement(legib_tr_t10, legibility_trap, theater_ratio, 10, 0.52).
narrative_ontology:measurement(legib_tr_t25, legibility_trap, theater_ratio, 25, 0.68).

% Extraction over time
narrative_ontology:measurement(legib_be_t0, legibility_trap, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(legib_be_t10, legibility_trap, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(legib_be_t25, legibility_trap, base_extractiveness, 25, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legibility_trap, enforcement_mechanism).
narrative_ontology:affects_constraint(legibility_trap, indigenous_knowledge_erasure).
narrative_ontology:affects_constraint(legibility_trap, bureaucratic_proliferation).
narrative_ontology:affects_constraint(legibility_trap, formal_credit_gatekeeping).
narrative_ontology:affects_constraint(legibility_trap, commons_tragedy_manufactured).

% DUAL FORMULATION NOTE:
% Legibility trap decomposes into four structurally related constraints: (1) indigenous_knowledge_erasure (ε≈0.70, Snare) — the irreversible liquidation of local ecological knowledge; (2) bureaucratic_proliferation (ε≈0.35, Piton) — the institutional inertia of cadastral systems maintaining theater after coordination is complete; (3) formal_credit_gatekeeping (ε≈0.52, Tangled Rope) — the transition mechanism forcing borrowers into formal banking; (4) commons_tragedy_manufactured (ε≈0.65, Snare) — the deliberate conversion of commons to private property. These are not observable-dependent variants but structurally distinct mechanisms sharing a common upstream cause (state legibility agenda). Link them via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legibility_trap, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
