% ============================================================================
% CONSTRAINT STORY: resource_curse_narrative
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_resource_curse_narrative, []).

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
 *   constraint_id: resource_curse_narrative
 *   human_readable: Resource Curse Narrative in Petro-State Governance
 *   domain: political_economy/development
 *
 * SUMMARY:
 *   The resource curse narrative describes how resource wealth in
 *   petro-states systematically undermines democratic governance, economic
 *   diversification, and institutional development. The constraint operates
 *   through multiple mechanisms: elite capture of resource revenues through
 *   patronage networks, suppression of political opposition via security
 *   apparatus funded by resource wealth, coordination of international
 *   extraction through multinational corporations and trade agreements, and
 *   identity-fusion of the state apparatus around resource nationalism and
 *   extraction maximization. This constraint exhibits Tangled Rope
 *   characteristics: it contains genuine coordination functions (resource
 *   exploration and extraction require technical cooperation, revenue-sharing
 *   mechanisms enable local input, transparency initiatives provide
 *   governance frameworks) alongside asymmetric extraction (wealth
 *   concentration in elite networks, environmental externalities borne by
 *   affected populations, institutional capacity diverted from service
 *   delivery). The extractiveness value (0.58) reflects moderate extraction
 *   that has accumulated over time as state capacity for diversification
 *   atrophied. The suppression value (0.62) reflects security apparatus
 *   infrastructure, publication restrictions on resource contracts, and
 *   political opposition barriers. Theater ratio (0.65) indicates substantial
 *   performative development bureaucracy that signals developmental intent
 *   without delivering institutional diversification.
 *
 * KEY AGENTS:
 *   - Broad Population: Primary victim (powerless/trapped/generational) — geographically immobile, dependent on state services, unable to exit jurisdiction; bears cost of foregone public goods and environmental externalities
 *   - Extractive Corporations: Primary beneficiary (institutional/arbitrage/immediate) — captures commodity profits; experiences constraint as pure coordination; can exit jurisdiction or relocate operations
 *   - State Apparatus: Institutional beneficiary-victim (institutional/identity_locked/generational) — benefits from resource revenue; trapped by identity fusion with extraction maximization; enforces constraint through security apparatus
 *   - Civil Society Organizations: Organized actors (organized/constrained/generational) — benefit from coordination frameworks; face extraction through co-optation and suppression; constrained by career interdependence
 *   - International Governance Coalition: Organized temporary support (organized/constrained/civilizational) — introduce scaffold mechanisms (transparency, anti-corruption standards) with embedded sunset logic; constrained by limited sovereignty
 *   - Development Bureaucracy: Institutional-performative (institutional/constrained/biographical) — maintains theater of development through policy frameworks; functionally degraded; persists through bureaucratic inertia
 *   - Analytical Observer: Civilizational perspective (analytical/analytical/civilizational) — risks naturalizing political-institutional choices as inevitable consequences of hydrocarbon geology
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(resource_curse_narrative, 0.58).
domain_priors:suppression_score(resource_curse_narrative, 0.62).
domain_priors:theater_ratio(resource_curse_narrative, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(resource_curse_narrative, extractiveness, 0.58).
narrative_ontology:constraint_metric(resource_curse_narrative, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(resource_curse_narrative, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(resource_curse_narrative, tangled_rope).
narrative_ontology:human_readable(resource_curse_narrative, "Resource Curse Narrative in Petro-State Governance").
narrative_ontology:topic_domain(resource_curse_narrative, "political_economy/development").

domain_priors:requires_active_enforcement(resource_curse_narrative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(resource_curse_narrative, extractive_corporations).
narrative_ontology:constraint_beneficiary(resource_curse_narrative, elite_capture_networks).
narrative_ontology:constraint_victim(resource_curse_narrative, broad_population).
narrative_ontology:constraint_victim(resource_curse_narrative, future_generations).
narrative_ontology:constraint_victim(resource_curse_narrative, institutional_development).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: BROAD POPULATION (SNARE) — Structurally trapped by geographic immobility and lack of economic alternatives. Revenue dependency creates suppression: state apparatus redirects resource wealth toward elite military, security, and patronage networks rather than public goods. Population cannot exit the jurisdiction without extreme cost and cannot organize effective political opposition due to security-state infrastructure. Experiences pure extraction disguised as resource nationalism or developmentalism.
constraint_indexing:constraint_classification(resource_curse_narrative, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CIVIL SOCIETY ORGANIZATIONS (TANGLED ROPE) — Organized actors (NGOs, local councils, activist networks) benefit from coordination: revenue-sharing mechanisms, environmental monitoring agreements, and transparency reporting create frameworks for collective action on resource governance. Simultaneously, they face extraction: security state suppression, co-optation through funding capture, and the resource wealth's concentration in elite networks that bypass their advocacy. Exit is constrained by career interdependence with government and corporations.
constraint_indexing:constraint_classification(resource_curse_narrative, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MULTINATIONAL EXTRACTIVE CORPORATIONS (ROPE) — Primary beneficiaries with full arbitrage options: can exit the jurisdiction, relocate operations, or shift portfolios to new resource frontiers. Experiences the constraint as pure coordination: navigating licensing, security arrangements, and revenue-sharing treaties with the state. The extraction narrative (resource curse) flows toward these actors — they gain commodity profits while externalizing environmental and social costs. No experienced extraction from their perspective; the constraint is a coordination mechanism they dominate.
constraint_indexing:constraint_classification(resource_curse_narrative, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: STATE APPARATUS (TANGLED ROPE with IDENTITY LOCK) — The state is simultaneously beneficiary (resource revenue funds the state's budget and security apparatus) and victim (resource dependency locks governance identity into extraction maximization, preventing institutional development toward tax-based, service-delivery systems). Exit is identity-locked: the state's institutional self-conception has become fused with resource control and rentier patronage. Reforming toward diversified revenue would require abandoning the identity the state has constructed around resource nationalism. Active enforcement through security apparatus maintains the constraint.
constraint_indexing:constraint_classification(resource_curse_narrative, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 5: INTERNATIONAL GOVERNANCE COALITION (SCAFFOLD) — Organized actors (IMF, World Bank, EITI, revenue transparency initiatives, climate finance mechanisms) introduce temporary coordination mechanisms with explicit sunset logic: anti-corruption governance standards, revenue transparency requirements, and mandatory sovereign wealth fund management are designed to be absorbed into domestic institutions over time. Extraction is moderate because international actors have constraints on enforcement (limited sovereignty) and exit (reputational cost of withdrawal). The scaffold has sunset clauses embedded: as domestic institutions mature, international oversight declines.
constraint_indexing:constraint_classification(resource_curse_narrative, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: DEVELOPMENT BUREAUCRACY (PITON) — Agencies created to manage resource revenues (sovereign wealth funds, development banks, industrial policy ministries) are substantially performative. These institutions produce reports, strategic plans, and governance frameworks that signal developmental intent while actual resource allocation remains captured by political elites. Theater ratio high: the machinery of development persists through bureaucratic inertia, not effective function. Functionally degraded because the institutions' stated purpose (equitable development, economic diversification, intergenerational wealth transfer) conflicts with the actual purpose (patronage distribution, elite enrichment). Piton classification reflects the theater gate.
constraint_indexing:constraint_classification(resource_curse_narrative, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilization-scale perspective, resource curse appears as an immutable constraint: hydrocarbon geology determines extraction patterns; commodity markets determine prices; global capital flows determine investment directions; political economy appears determined by these material factors. This perspective risks naturalizing what are actually contingent institutional choices — elite political economy, corruption incentive structures, and governance failure — as inevitable consequences of resource endowment. The engine flags this as a false summit, revealing the naturalization of political choice as natural law.
constraint_indexing:constraint_classification(resource_curse_narrative, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(resource_curse_narrative_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(resource_curse_narrative, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(resource_curse_narrative, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(resource_curse_narrative, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(resource_curse_narrative, TR),
    TR >= 0.70.

:- end_tests(resource_curse_narrative_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-to-high, accumulated over time. Initial extraction at state formation (0.35) reflects early resource nationalism and international agreements that appeared balanced. Over 20 years, extractiveness increased (0.58) as elite capture mechanisms matured, fiscal institutions atrophied, and international commodity prices volatility forced state dependence. The extraction is not as severe as a pure Snare (0.66+) because some coordination benefits exist — resource revenue does fund some public goods, international governance does introduce accountability mechanisms, and some diversification initiatives exist. But the trajectory shows accumulation, not decline. Suppression (0.62): Moderate-high. Security apparatus is substantial (funded by resource revenues), political opposition faces harassment and media restrictions, and popular grievances are channeled through controlled outlets rather than genuine political voice. However, suppression is not total — civil society organizations operate (though constrained), some media freedom exists, and international monitoring creates pressure for restraint. Theater ratio (0.65): Moderate-high. Development bureaucracy (sovereign wealth funds, industrial policy ministries, anti-corruption agencies) produces substantial reportage and strategic planning but actual institutional diversification remains limited. Theater increased as development institutions multiplied without corresponding functional output.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural phenomenon generates six distinct classifications. The multinational corporation sees coordination (Rope). The state sees nationalist protection of resources but is identity-locked preventing reform. Civil society sees mixed coordination and extraction (Tangled Rope). The broad population sees pure extraction (Snare). Development institutions see their own degraded ritual (Piton). The international coalition sees a solvable temporary problem (Scaffold). The civilizational observer risks seeing natural law (false Mountain) when the constraint is actually political institutional choices. The perspectival gaps are driven by: (1) different power levels (powerless vs institutional vs organized), (2) different exit options (trapped vs arbitrage vs identity_locked), and (3) different time horizons (immediate benefit vs generational cost accumulation). The gap between the state's self-perception (nationalist beneficiary) and the analytical observer's reconstruction (identity-locked actor preventing its own reform) is diagnostically crucial — it reveals how identity fusion functions as suppression.
 *
 * DIRECTIONALITY LOGIC:
 *   The extraction narrative flows from powerless populations and degraded institutions toward multinational corporations and elite networks. Directionality is determined by structural position: those who can exit (corporations with arbitrage) experience negative effective extraction; those who cannot (population with trapped status) experience positive extraction; those whose exit is cognitive (state with identity lock) occupy an intermediate position where they could theoretically reform but cannot perceive the pathway. The chi formula χ = ε × f(d) × σ(S) scales the base extraction (ε=0.58) by the sigmoid function of directionality f(d) and the scope modifier σ(S=national, 1.0). For the broad population (d≈0.95), f(d)≈1.42, producing χ≈0.82. For the corporation (d≈0.05), f(d)≈-0.12, producing χ≈-0.07 (net coordination benefit). The state's identity lock produces a d≈0.40 that would yield χ≈0.35 through the formula, but the cognitive lock prevents the state from seeing this as modifiable extraction. The directionality logic explains why different actors genuinely experience different levels of extraction from the same constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The resource curse resolves mandatrophy by disambiguating between coordination and extraction. The constraint could be misread as pure Snare (extraction enforced through security apparatus) or pure Rope (coordination of resource development). The tangled rope classification captures the true structure: genuine coordination mechanisms (international agreements, technical expertise, revenue frameworks) are layered with asymmetric extraction (wealth concentration, elite patronage, suppression). The mandate crisis would arise if the constraint were labeled purely as either type — labeling it pure Snare would obscure that some coordination benefits genuinely exist; labeling it pure Rope would obscure that extraction dominates. The tangled rope classification prevents false adjudication by naming both functions explicitly. The theater ratio (0.65) indicates that some of the constraint's mechanisms (development bureaucracy, transparency initiatives) are performative rather than functionally extractive, but not so performative as to degrade to Piton. The mandatrophy is resolved by the perspectival multiplicity — each actor's experience (Snare for powerless, Rope for corporations, Tangled Rope for organized, Scaffold for international) is accurate from their structural position. The unified constraint type (Tangled Rope) captures the institutional reality: coordination and extraction are genuinely colocated in the same mechanisms.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    elite_capture_vs_structural_incentive,
    'Is the resource curse driven by elite capture (institutional failure contingent on leadership choices) or by structural incentives inherent to hydrocarbon economies?',
    'Comparative analysis: petro-states with strong institutions (Norway, Botswana) vs weak institutions (Nigeria, Venezuela); identification of causal lever — corruption prosecution rates, institutional independence, fiscal rule adherence',
    'If elite capture: constraint is Snare with potential reform pathway (prosecute elites, strengthen institutions, diversify revenues). If structural: constraint appears closer to Mountain (institutional forms cannot overcome resource-driven incentives). Classification shifts from Snare toward Scaffold or even Mountain if structural determination is dominant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elite_capture_vs_structural_incentive, conceptual, 'Whether resource curse is elite capture or structural incentive').

omega_variable(
    diversification_feasibility,
    'Can petro-states realistically diversify away from hydrocarbon dependency, or does resource wealth create institutional lock-in that prevents economic restructuring?',
    'Longitudinal tracking of diversification attempts: correlation between resource revenue volatility, sovereign wealth fund performance, and non-hydrocarbon sector growth; case studies of successful vs failed diversification (UAE, Saudi Arabia, Russia)',
    'If feasible: scaffold sunset logic is realistic — international governance can support transition to diversified economy. If locked-in: constraint approaches Snare permanence — diversification attempts fail repeatedly, locking state into extraction maximization indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diversification_feasibility, empirical, 'Whether economic diversification from hydrocarbon dependency is achievable').

omega_variable(
    international_finance_enablement,
    'Do international finance mechanisms (sovereign wealth fund management, development banks, technical assistance) genuinely enable institutional development or do they serve as cover stories for extractive capture?',
    'Outcome measurement: countries with vs without international governance frameworks; tracking whether revenue transparency requirements correlate with reduced corruption, improved public goods delivery, or institutional diversification',
    'If enabling: scaffold perspective is accurate — international actors are building exit pathways for resource-dependent economies. If enabling-as-cover: international mechanisms are performative (piton), legitimizing elite extraction through technical governance language.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(international_finance_enablement, empirical, 'Whether international finance genuinely enables or legitimizes extraction').

omega_variable(
    identity_lock_reversibility,
    'Can a state''s identity-fused commitment to resource nationalism be undone, or does the identity lock persist even after institutional reform?',
    'Post-reform trajectory analysis: states that have shifted away from extraction-centric governance; measurement of whether national identity narratives, political rhetoric, and policy innovation show evidence of identity frame-shift or persistence of extraction framing',
    'If reversible: state actor (institutional/identity_locked) can exit the constraint through identity frame change, making diversification possible. If persistent: identity lock survives institutional reform, creating persistent extraction maximization even when elites theoretically support diversification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_reversibility, conceptual, 'Whether state identity lock around resource nationalism is reversible').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(resource_curse_narrative, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rescurse_tr_t0, resource_curse_narrative, theater_ratio, 0, 0.42).
narrative_ontology:measurement(rescurse_tr_t10, resource_curse_narrative, theater_ratio, 10, 0.55).
narrative_ontology:measurement(rescurse_tr_t20, resource_curse_narrative, theater_ratio, 20, 0.65).

% Extraction over time
narrative_ontology:measurement(rescurse_be_t0, resource_curse_narrative, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(rescurse_be_t10, resource_curse_narrative, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(rescurse_be_t20, resource_curse_narrative, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(resource_curse_narrative, resource_allocation).
narrative_ontology:affects_constraint(resource_curse_narrative, sovereign_wealth_fund_governance).
narrative_ontology:affects_constraint(resource_curse_narrative, commodity_export_dependency).
narrative_ontology:affects_constraint(resource_curse_narrative, elite_patronage_networks).

% DUAL FORMULATION NOTE:
% The resource curse narrative decomposes into three structurally distinct constraints: (1) commodity export dependency (ε=0.45, Tangled Rope) — genuine coordination in resource extraction and export logistics; asymmetric extraction through terms-of-trade. (2) Sovereign wealth fund governance (ε=0.52, Tangled Rope) — coordination of intergenerational wealth transfer; extraction through political capture of fund allocation. (3) Elite patronage networks (ε=0.68, Snare) — minimal coordination; pure extraction through rent-seeking and corruption. The overall resource curse narrative has ε=0.58 as the family average, with the three sub-constraints varying by mechanism. The narrative story links to all three.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(resource_curse_narrative, institutional, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
