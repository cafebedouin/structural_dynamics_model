% ============================================================================
% CONSTRAINT STORY: great_mongolian_road_economic_dependency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_great_mongolian_road_economic_dependency, []).

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
 *   constraint_id: great_mongolian_road_economic_dependency
 *   human_readable: Economic Dependency via Great Mongolian Road Infrastructure Development
 *   domain: economic/development/geopolitical
 *
 * SUMMARY:
 *   The Great Mongolian Road project, initiated by Japan through JICA and
 *   Japanese corporations, presents Mongolia with modern infrastructure—rail,
 *   highway, and port connectivity—marketed as development and regional
 *   integration. However, the constraint embedded in this infrastructure
 *   involves structural economic lock-in: Mongolia becomes dependent on
 *   Japanese financing, corridor utilization, and operational control, while
 *   rural communities lose land access and fiscal authorities lose policy
 *   autonomy. The constraint exhibits both genuine coordination benefits
 *   (connectivity enables trade) and extractive mechanisms (debt repayment,
 *   market control, technological dependence). The tension between these
 *   functions defines it as Tangled Rope from the Mongolian government
 *   perspective. From Japanese institutional actors, it is Rope—pure
 *   coordination with first-mover arbitrage. From Mongolian rural
 *   communities, it is Snare—trapped in externally determined supply chains
 *   without alternatives. From international oversight bodies, it appears as
 *   Scaffold—a temporary institutional failure solvable via transparency and
 *   conditionality reform. The conventional development finance paradigm sees
 *   it as Piton—maintaining extractive infrastructure-led development despite
 *   alternatives. Theater has increased from 0.38 to 0.58 as projects
 *   accumulate administrative overhead and evaluation rituals without
 *   corresponding developmental impact measurement.
 *
 * KEY AGENTS:
 *   - Japanese corporations and JICA: Institutional beneficiary (institutional/arbitrage) — captures market access, first-mover advantage in corridor control, sustained lending income
 *   - Mongolian government: Moderate victim (moderate/constrained) — receives infrastructure but becomes locked into debt servicing and Japanese corporate interests; constrained by long-term financing agreements
 *   - Mongolian rural communities: Primary victim (powerless/trapped) — displaced by corridors or incorporated into supply chains without control; no exit options
 *   - Mongolian fiscal sovereignty: Victim (abstract, trapped) — policy autonomy constrained by financing covenants and corridor requirements
 *   - Chinese competitors and Belt-and-Road advocates: Organized competitor (organized/constrained) — locked out of Japanese-dominant corridors; face competitive extraction
 *   - International development oversight bodies: Organized reformers (organized/mobile) — see extractive terms as temporary, solvable via ESG and transparency standards
 *   - Conventional development finance paradigm: Institutional persistence (institutional/arbitrage) — maintains extractive model through inertia despite documented failures
 *   - Analytical observer: Civilizational view (analytical/analytical) — risks naturalizing contingent financing arrangements as inherent to development
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(great_mongolian_road_economic_dependency, 0.52).
domain_priors:suppression_score(great_mongolian_road_economic_dependency, 0.65).
domain_priors:theater_ratio(great_mongolian_road_economic_dependency, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(great_mongolian_road_economic_dependency, extractiveness, 0.52).
narrative_ontology:constraint_metric(great_mongolian_road_economic_dependency, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(great_mongolian_road_economic_dependency, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(great_mongolian_road_economic_dependency, tangled_rope).
narrative_ontology:human_readable(great_mongolian_road_economic_dependency, "Economic Dependency via Great Mongolian Road Infrastructure Development").
narrative_ontology:topic_domain(great_mongolian_road_economic_dependency, "economic/development/geopolitical").

domain_priors:requires_active_enforcement(great_mongolian_road_economic_dependency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(great_mongolian_road_economic_dependency, japanese_corporations).
narrative_ontology:constraint_beneficiary(great_mongolian_road_economic_dependency, mongolian_political_elite).
narrative_ontology:constraint_beneficiary(great_mongolian_road_economic_dependency, asian_trade_corridor_participants).
narrative_ontology:constraint_victim(great_mongolian_road_economic_dependency, mongolian_rural_communities).
narrative_ontology:constraint_victim(great_mongolian_road_economic_dependency, mongolian_fiscal_sovereignty).
narrative_ontology:constraint_victim(great_mongolian_road_economic_dependency, alternative_development_pathways).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MONGOLIAN RURAL COMMUNITIES (SNARE) — Displaced by infrastructure corridors or incorporated into supply chains without control over terms. Cannot exit without losing livelihoods. Trapped by infrastructure-dependent economy with no alternative productive assets. d≈0.92, f(d)≈1.38, σ=0.9 → χ≈0.68.
constraint_indexing:constraint_classification(great_mongolian_road_economic_dependency, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: MONGOLIAN GOVERNMENT (TANGLED ROPE) — Receives genuine infrastructure coordination benefits (connectivity, trade access) but becomes locked into Japanese corporate interests and debt repayment obligations. Exit constrained by long-term financing agreements. Benefits from trade but also bears extraction through interest payments and economic lock-in. d≈0.68, f(d)≈1.02, σ=1.0 → χ≈0.53.
constraint_indexing:constraint_classification(great_mongolian_road_economic_dependency, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: JAPANESE CORPORATIONS AND JICA (ROPE) — Experiences infrastructure as coordination mechanism: roads reduce transaction costs, enable trade expansion, and create legitimate development benefits. High arbitrage capacity—can exit by shifting focus to other regions. Captures first-mover advantage in market access. d≈0.08, f(d)≈-0.10, σ=1.1 → χ≈-0.06.
constraint_indexing:constraint_classification(great_mongolian_road_economic_dependency, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: CHINESE COMPETITORS AND ALTERNATIVE CORRIDOR ADVOCATES (TANGLED ROPE) — Recognize infrastructure benefits (Mongolia needs connectivity) but are locked out of Japanese-dominant corridors and experience the constraint as competitive extraction. Limited ability to redirect Mongolian infrastructure investment toward Belt-and-Road alternatives. Constrained by geopolitical positioning. d≈0.55, f(d)≈0.75, σ=0.9 → χ≈0.35.
constraint_indexing:constraint_classification(great_mongolian_road_economic_dependency, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: INTERNATIONAL DEVELOPMENT OVERSIGHT BODIES (SCAFFOLD) — See extractive debt arrangements as a temporary coordination failure solvable via transparency mandates, climate integration, and community-benefit agreements. Emergence of ESG standards, Paris Agreement alignment requirements, and open-data infrastructure procurement create sunset pathway. d≈0.35, f(d)≈0.28, σ=1.2 → χ≈0.17.
constraint_indexing:constraint_classification(great_mongolian_road_economic_dependency, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: CONVENTIONAL DEVELOPMENT FINANCE PARADIGM (PITON) — Infrastructure-led development model persists through institutional inertia despite documented failure modes (debt-trap diplomacy, stranded assets, environmental damage). Maintained by World Bank orthodoxy and bilateral donor alignment, not because alternatives have been proven inferior. theater_ratio=0.58 reflects ritualistic project evaluation without genuine counterfactual analysis. The paradigm is degraded—alternatives exist (community-led development, regenerative infrastructure) but lack institutional weight.
constraint_indexing:constraint_classification(great_mongolian_road_economic_dependency, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER—STRUCTURAL ECONOMICS (MOUNTAIN) — From a universal perspective, infrastructure-mediated economic integration is sometimes claimed as inherent to development: poorer regions always require capital inflows, and those capital flows always create dependencies. But structural data (ε=0.52, suppression=0.65, theater=0.58) contradicts mountain classification—this is not a law of nature but a contingent institutional arrangement (extractive financing terms, colonial-pattern corridors). Engine will flag as false summit.
constraint_indexing:constraint_classification(great_mongolian_road_economic_dependency, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(great_mongolian_road_economic_dependency_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(great_mongolian_road_economic_dependency, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(great_mongolian_road_economic_dependency, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(great_mongolian_road_economic_dependency, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(great_mongolian_road_economic_dependency, TR),
    TR >= 0.70.

:- end_tests(great_mongolian_road_economic_dependency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Japan captures substantial benefits through first-mover corridor access, market control, and sustained lending income. Mongolia receives infrastructure but becomes locked into debt service (approximately 15-25% of annual revenues for large infrastructure projects) and corridor-dependent economics. Suppression (0.65): Moderate-high. Mongolian alternatives (decentralized renewables, community-controlled infrastructure, Chinese financing) are available but systematically disadvantaged through geopolitical alignment, financing speed/volume advantages of Japanese ODA, and institutional preference for capital-intensive corridor projects. Theater ratio (0.58): Moderate. Project evaluation emphasizes connectivity gains and GDP growth but underweights debt sustainability analysis, community displacement costs, and technological lock-in risks. Theater has increased over the interval as administrative overhead of monitoring grows without corresponding improvement in actual impact measurement or counterfactual analysis.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp perspectival divergence between beneficiary, victim, and organizational perspectives. Japanese institutional actors see Rope—legitimate coordination solving Mongolia's connectivity deficit with justified first-mover returns. Mongolian government sees Tangled Rope—genuine infrastructure benefits mixed with extraction through debt and policy constraints. Mongolian rural communities see Snare—supply-chain incorporation with no exit. Organized competitors see Tangled Rope—locked out of benefits while facing competitive pressure. International oversight bodies see Scaffold—extractive terms are institutional failures, solvable via transparency and ESG integration. Conventional development paradigm sees Piton—the infrastructure-led model persists through institutional weight despite alternatives. The analytical observer risks seeing Mountain—naturalizing development finance dependency as inherent to growth. The perspectival range from Rope to Snare is 2.3 classification distances, indicating severe structural conflict.
 *
 * DIRECTIONALITY LOGIC:
 *   Japanese institutions: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary; can exit by shifting to other markets. Mongolian government: Victim + constrained → d≈0.68, f(d)≈1.02. Significant extraction; exit constrained by 20-30 year financing agreements. Mongolian rural communities: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction; supply-chain incorporation eliminates alternative livelihoods. Chinese competitors: Organized competitor + constrained → d≈0.55, f(d)≈0.75. Extraction from competitive positioning; some optionality through geopolitical repositioning. International oversight: Organized reformers + mobile → d≈0.35, f(d)≈0.28. Low effective extraction; mobility through policy standards and transparency mandates. Conventional paradigm: Institutional + arbitrage → d≈0.08, f(d)≈-0.10. Piton classification from theater gate (0.58), not directionality. Analytical observer: analytical → d≈0.72, f(d)≈1.15. Mountain classification risk (false summit detector applicable).
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION PATHWAY: The constraint resolves mandatrophy by distinguishing between infrastructure coordination (Rope function) and extractive financing (Snare/Tangled Rope extraction). The coordination function is genuine—Mongolia requires connectivity. The extraction is also genuine—Japan captures asymmetric benefits through debt servicing and corridor control. The Tangled Rope classification holds because both functions coexist structurally: the same infrastructure that enables trade also enables extraction via toll collection, financing terms, and technological lock-in. Benign framing (development partner, regional integration) masks extraction mechanisms (debt-trap triggers, fiscal sovereignty constraints, forced supply-chain participation). The mandatrophy is resolved by measuring BOTH the coordination value (benefits Mongolia receives) and the extraction asymmetry (benefits Japan retains). The false summit risk for the analytical observer stems from naturalizing institutional financing arrangements as inherent to development. In principle, Mongolia could receive equivalent connectivity from non-extractive financing (community-scaled renewables, regional cooperative infrastructure, decentralized manufacturing). The institutional preference for capital-intensive, debt-financed, Japanese-controlled corridors is contingent, not natural law. Detector flags this as false summit and correctly classifies as Tangled Rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    alternative_infrastructure_trajectory,
    'Would Mongolia''s infrastructure trajectory differ structurally if developed via decentralized, community-controlled renewables and local supply chains rather than centralized international corridors?',
    'Comparative analysis of Mongolia''s energy independence under distributed renewable model vs. corridor-dependent fossil/hydroelectric model; case comparison with smaller nations achieving infrastructure autonomy',
    'If yes: infrastructure dependency is contingent institutional choice (Snare/Tangled Rope confirmed). If no: infrastructure dependency is inherent structural reality (Mountain candidate).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_infrastructure_trajectory, empirical, 'Whether alternative decentralized infrastructure pathways were genuinely available').

omega_variable(
    debt_trap_mechanism_activation,
    'Does the Great Mongolian Road financing structure contain trigger conditions that activate debt-trap dynamics (inability to service debt, collateral seizure, asset transfer), or does it have genuine grace periods and flexible renegotiation terms?',
    'Analysis of loan covenants, currency denomination, grace periods, and force-majeure clauses; comparison with other Japanese ODA infrastructure loans; historical examination of debt renegotiation patterns',
    'If trap mechanisms active: extraction is structural (Snare/Tangled Rope confirmed, ε≥0.50). If terms genuinely concessional: extraction is reduced, constraint may downgrade to Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(debt_trap_mechanism_activation, empirical, 'Whether financing terms contain debt-trap activation mechanisms').

omega_variable(
    mongolian_alternative_market_access,
    'Does Mongolia have genuine alternative markets and financing sources (China, Russia, Central Asian development banks, Islamic finance) that could enable infrastructure development without Japanese corridor dependence?',
    'Inventory of alternative infrastructure financing available to Mongolia; analysis of terms and delivery speed compared to Japanese ODA; assessment of geopolitical constraints on alternatives',
    'If genuine alternatives exist but are suppressed: suppression variable is high (current 0.65 justified). If alternatives unavailable: suppression may be overestimated, constraint may downgrade.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mongolian_alternative_market_access, empirical, 'Availability of genuine alternative infrastructure financing sources').

omega_variable(
    technological_lock_in_reversibility,
    'Are infrastructure standards, technical specifications, and operational protocols in the Great Mongolian Road designed to be interoperable with non-Japanese systems, or do they contain lock-in provisions (proprietary standards, Japanese-only maintenance contracts, incompatible gauge/frequency)?',
    'Audit of infrastructure technical specifications; analysis of maintenance dependency chains; comparison with open-standard infrastructure projects',
    'If lock-in present: structural extraction increases over time (ε may exceed 0.55). If open standards: Mongolia retains optionality after payoff period.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_lock_in_reversibility, empirical, 'Whether infrastructure design enforces technological lock-in').

omega_variable(
    mongolian_fiscal_sovereignty_retention,
    'Does the financing structure preserve Mongolia''s ability to make independent fiscal and trade policy choices, or does it require Japanese approval for policy changes (tariffs, resource extraction, currency management)?',
    'Review of loan covenants for fiscal/trade policy constraints; analysis of IMF conditionality overlap; comparison with other development finance agreements',
    'If policy autonomy preserved: extraction is primarily economic (ε≈0.50). If policy constrained: extraction extends to political sovereignty (ε→0.65+, classification upgrades toward Snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mongolian_fiscal_sovereignty_retention, empirical, 'Whether financing preserves Mongolian fiscal and trade policy independence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(great_mongolian_road_economic_dependency, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gmr_tr_t0, great_mongolian_road_economic_dependency, theater_ratio, 0, 0.38).
narrative_ontology:measurement(gmr_tr_t5, great_mongolian_road_economic_dependency, theater_ratio, 5, 0.48).
narrative_ontology:measurement(gmr_tr_t10, great_mongolian_road_economic_dependency, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(gmr_be_t0, great_mongolian_road_economic_dependency, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(gmr_be_t5, great_mongolian_road_economic_dependency, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(gmr_be_t10, great_mongolian_road_economic_dependency, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(great_mongolian_road_economic_dependency, global_infrastructure).
narrative_ontology:affects_constraint(great_mongolian_road_economic_dependency, mongolian_fiscal_sustainability).
narrative_ontology:affects_constraint(great_mongolian_road_economic_dependency, asian_trade_corridor_dependency).
narrative_ontology:affects_constraint(great_mongolian_road_economic_dependency, geopolitical_alignment_lock_in).

% DUAL FORMULATION NOTE:
% The Great Mongolian Road constraint involves both infrastructure coordination and economic dependency extraction. The coordination function (ε≈0.25, Rope-class) involves genuine connectivity benefits. The extraction function (ε≈0.52, Tangled Rope-class) involves debt-service asymmetry and market lock-in. These could be modeled as separate constraints, but they are inseparable in practice—the same infrastructure mechanism serves both functions. The Tangled Rope classification correctly captures this hybrid nature.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(great_mongolian_road_economic_dependency, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
