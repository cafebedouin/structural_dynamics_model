% ============================================================================
% CONSTRAINT STORY: artisanal_mining_displacement
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_artisanal_mining_displacement, []).

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
 *   constraint_id: artisanal_mining_displacement
 *   human_readable: Artisanal Mining Displacement by Large-Scale Industrial Operations
 *   domain: economic/environmental/labor
 *
 * SUMMARY:
 *   Artisanal mining displacement occurs when governments and industrial
 *   corporations use formalized permitting systems, environmental regulation,
 *   and capital-intensive infrastructure to exclude small-scale miners from
 *   ore bodies they have customarily accessed. This constraint exhibits the
 *   full range of DR classification types depending on the observer's
 *   structural position. The constraint itself is straightforward: industrial
 *   mining operations are more efficient at scale, generating government
 *   revenue and corporate profit. But the displacement of artisanal miners
 *   creates a snare — the constraint uses state enforcement mechanisms to
 *   concentrate extraction benefits and distribute displacement costs. The
 *   theater ratio (0.55) reflects that licensing and environmental review
 *   processes are partially performative: they provide legitimacy to
 *   industrial expansion without meaningfully constraining it. Extractiveness
 *   has increased from 0.45 to 0.68 over the measurement interval as
 *   industrial mining has scaled globally and formalization efforts have
 *   provided bureaucratic cover for displacement. The constraint exists in a
 *   network with specific commodity systems (rare earth mining, artisanal
 *   gold mining, conflict minerals), environmental degradation patterns, and
 *   labor market dynamics.
 *
 * KEY AGENTS:
 *   - Artisanal Miners: Primary victim (powerless/trapped) — lose access to customary mining grounds; face economic desperation and geographic immobility
 *   - Mining-Dependent Communities: Primary victim (powerless/constrained) — economies collapse when artisanal mining is displaced; cannot easily relocate or transition to alternatives
 *   - Industrial Mining Corporations: Primary beneficiary (institutional/arbitrage) — capture economic rents from ore extraction; benefit from regulatory frameworks that exclude competitors
 *   - Host Government Agencies: Secondary beneficiary (institutional/arbitrage) — collect mining taxes and royalties from industrial operations; revenue-dependent but less directly extracted from
 *   - Artisanal Mining Coalitions: Organized secondary actor (organized/constrained) — when formed, can negotiate compensation and transition support; reduce but do not eliminate extraction
 *   - International Regulatory Bodies: Institutional observer (institutional/arbitrage) — set standards for environmental assessment and due diligence that legitimize industrial expansion
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing displacement as inevitable consequence of development
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(artisanal_mining_displacement, 0.68).
domain_priors:suppression_score(artisanal_mining_displacement, 0.72).
domain_priors:theater_ratio(artisanal_mining_displacement, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(artisanal_mining_displacement, extractiveness, 0.68).
narrative_ontology:constraint_metric(artisanal_mining_displacement, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(artisanal_mining_displacement, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(artisanal_mining_displacement, snare).
narrative_ontology:human_readable(artisanal_mining_displacement, "Artisanal Mining Displacement by Large-Scale Industrial Operations").
narrative_ontology:topic_domain(artisanal_mining_displacement, "economic/environmental/labor").

domain_priors:requires_active_enforcement(artisanal_mining_displacement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(artisanal_mining_displacement, industrial_mining_corporations).
narrative_ontology:constraint_beneficiary(artisanal_mining_displacement, host_government_revenue_agencies).
narrative_ontology:constraint_victim(artisanal_mining_displacement, artisanal_miners).
narrative_ontology:constraint_victim(artisanal_mining_displacement, mining_dependent_communities).
narrative_ontology:constraint_victim(artisanal_mining_displacement, ecosystem_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ARTISANAL MINER (SNARE) — Trapped by economic dependency with no viable exit. Industrial operations destroy artisanal claims and subsistence income sources. No legal recourse, minimal compensation. Suppression is structural: geographic immobility, lack of capital, economic desperation, and state enforcement of industrial permits over customary rights. Maximum extraction — the constraint entirely restructures livelihood options.
constraint_indexing:constraint_classification(artisanal_mining_displacement, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: MINING-DEPENDENT COMMUNITY (SNARE) — Communities whose economies depend entirely on artisanal mining face generational collapse. Exit options are constrained but structurally difficult: relocation requires capital and social networks unavailable to poor communities; remaining requires accepting ecological destruction of local water and soil. The constraint forces a false binary: impoverishment through displacement or impoverishment through environmental degradation. Either way, extraction runs toward industrial operators and distant consumers.
constraint_indexing:constraint_classification(artisanal_mining_displacement, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INDUSTRIAL MINING CORPORATION (ROPE) — Experiences the constraint as coordination: state permitting systems, infrastructure development, supply chain integration. The corporation benefits from regulatory frameworks that formalize property rights and displace competing informal claims. From this position, the constraint is pure coordination — solving the problem of how to access ore bodies at scale. No experienced extraction; full experienced benefit.
constraint_indexing:constraint_classification(artisanal_mining_displacement, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ARTISANAL MINING COALITION (TANGLED ROPE) — When artisanal miners organize (through NGOs, labor unions, community councils), they can negotiate compensation and transition support. The constraint becomes mixed: genuine coordination on benefit-sharing, alongside asymmetric extraction (industrial operators still control the terms and timeline). Organized agents have agency but face structural power asymmetry. Exit is constrained but negotiable.
constraint_indexing:constraint_classification(artisanal_mining_displacement, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: TRANSITIONAL REGULATION REGIME (SCAFFOLD) — Governments implementing artisanal mining formalization programs (licensing, cooperatives, skill transition to industrial roles) see the displacement as a temporary coordination problem with a sunset. Low effective extraction because the regime has an exit strategy: formalize artisanal mining, transition workers to industrial employment or alternative livelihoods, manage the process over 10-20 years. Beneficiaries include both governments (regulatory coherence) and some artisanal miners (formal status). High suppression during transition (forced consolidation) but decreasing over the interval.
constraint_indexing:constraint_classification(artisanal_mining_displacement, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: INTERNATIONAL LICENSING REGIME (PITON) — The global apparatus of mining concessions, environmental impact assessments, and due diligence protocols is largely performative. Assessments are conducted by industry-hired firms; concessions are granted despite documented community opposition; 'consultation' is theater that precedes predetermined industrial expansion. The regime persists through institutional inertia: international finance, export standards, and certification schemes all reference formal permitting and environmental review, but the review process rarely blocks projects. Theater ratio (0.55 baseline) reflects that the licensing regime performs compliance while actual decisions are made by capital flows and political patronage.
constraint_indexing:constraint_classification(artisanal_mining_displacement, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the displacement may appear as a natural law of development: economies transition from artisanal to industrial production; this creates inevitable frictions and dislocations. The constraint appears immutable because 'development requires concentration of capital and technology.' However, this naturalizes what is actually a contingent choice: governments could regulate industrial mining with genuine benefit-sharing, could support artisanal formalization without displacement, or could restrict large-scale extraction in favor of distributed artisanal production. The mountain classification is a false summit — it mistakes political choice for physical law.
constraint_indexing:constraint_classification(artisanal_mining_displacement, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(artisanal_mining_displacement_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(artisanal_mining_displacement, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(artisanal_mining_displacement, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(artisanal_mining_displacement, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(artisanal_mining_displacement, TR),
    TR >= 0.70.

:- end_tests(artisanal_mining_displacement_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and increasing. The constraint explicitly redistributes access rights from artisanal miners to industrial operators. The initial extractiveness (0.45) reflects the early stages of displacement when some artisanal mining persisted alongside industrial operations. Current extractiveness (0.68) reflects near-complete displacement in major mining regions — industrial operations now have exclusive or nearly exclusive access to high-value ore bodies. Suppression (0.72): High. Barriers to continued artisanal mining are both structural (lack of capital, technology, legal standing) and coercive (police enforcement of industrial claims, criminalization of artisanal mining on industrial concessions). Government enforcement of mining permits over customary rights is active, not residual. Theater ratio (0.55): Moderate. Environmental impact assessments and community consultation processes provide legitimacy to industrial expansion but do not materially constrain it. The licensing regime is substantially performative — pre-determined industrial approvals use the formal review process as theater rather than genuine gate.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates asymmetric classification across the observer spectrum. The artisanal miner sees a snare (pure extraction, no escape). The organized coalition sees tangled rope (mixed coordination on benefit-sharing, but asymmetric terms). The industrial corporation sees rope (coordination of property rights and supply chains). The transitional regulator sees scaffold (temporary friction being solved through formalization). The international licensing regime appears as piton (performative review legitimizing predetermined expansion). The civilizational analyst risks seeing mountain (development as natural law). This perspectival cascade reveals the constraint's true structure: what appears to one position as coordination appears to another as pure extraction, depending on who benefits and who can exit.
 *
 * DIRECTIONALITY LOGIC:
 *   Artisanal miners occupy the maximum-extraction position: they are victims with trapped exit (no alternatives, no capital for relocation, geographic immobility, economic desperation). The sigmoid f(d) applied to d=0.95 (trapped victim) produces f(d)≈1.42, amplifying their experienced extractiveness. Industrial corporations occupy the minimum-extraction position: they are beneficiaries with arbitrage exit (can move to other jurisdictions, operate profitably anywhere). The sigmoid f(d) applied to d=0.05 (beneficiary with arbitrage) produces f(d)≈-0.12, producing negative experienced extractiveness — they experience the constraint as pure benefit. When organized, artisanal miners achieve d≈0.75 (victim but with organized exit capacity), producing f(d)≈1.15, reducing experienced extraction through collective agency. The government's d≈0.40 (secondary beneficiary, constrained by revenue dependency) produces f(d)≈0.40, moderate extraction — government experiences mixed coordination and benefit.
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE VS TANGLED ROPE AMBIGUITY: The primary mandatrophy tension is whether this constraint contains a genuine coordination function. Governments might argue that formalization of artisanal mining (issuing licenses, organizing cooperatives, providing training for industrial employment) is coordination — solving the problem of how to integrate artisanal miners into formal economies. But this framing obscures the asymmetry: the coordination is mandatory for miners (no option to continue artisanal mining) while optional for corporations (who would proceed with extraction regardless of formalization schemes). The requirement for active enforcement distinguishes snare from rope. Enforcement is active and asymmetric: police remove artisanal miners from industrial concessions while corporations are never similarly constrained. The constraint remains snare because the 'coordination' function is unilaterally structured by the beneficiary. When miners are actually organized (forming coalitions with legal representation), the constraint escalates to tangled rope because the coordination becomes genuine — two parties with leverage negotiating terms. The snare classification reflects the pre-organization reality in most mining regions, where individual artisanal miners face industrial operations with no collective voice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    redistribution_vs_displacement_tradeoff,
    'Is displacement inevitable to achieve economies of scale in mining, or is it a choice made by governments and corporations to concentrate rents?',
    'Comparative analysis of countries with different regulatory regimes: those protecting artisanal mining rights vs those prioritizing large-scale extraction; measurement of ore recovery efficiency and profitability under mixed-scale models',
    'If inevitable: constraint shifts toward mountain classification and development necessity. If a choice: constraint remains snare — displacement is extraction mechanism, not natural law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(redistribution_vs_displacement_tradeoff, empirical, 'Whether displacement is economically necessary or a regulatory choice').

omega_variable(
    community_exit_feasibility,
    'Are alternative livelihoods (agricultural intensification, manufacturing, service sector) genuinely available to displaced mining communities, or are they structurally constrained?',
    'Longitudinal tracking of displaced miners'' income pathways post-displacement; availability of capital, training, and market access for alternative sectors; comparison with communities that retained artisanal mining access',
    'If alternatives accessible: exit is ''constrained'' not ''trapped'' — could escalate some perspectives to tangled_rope. If alternatives blocked: exit remains trapped — snare classification strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(community_exit_feasibility, empirical, 'Feasibility of alternative livelihoods post-displacement').

omega_variable(
    environmental_restoration_timeline,
    'Do industrial mining sites achieve genuine environmental remediation within a reasonable timeframe, or is environmental damage permanent?',
    'Post-mining site environmental audits; measurement of soil restoration, water quality recovery, ecosystem function restoration; comparison between planned remediation and actual outcomes',
    'If genuine restoration possible: scaffold perspective (sunset of mining, return to baseline) is credible. If restoration fails: environmental cost becomes generational, escalating victim status and shifting perspectives toward permanent snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(environmental_restoration_timeline, empirical, 'Whether environmental damage from industrial mining is reversible').

omega_variable(
    corporate_benefit_sharing_enforcement,
    'When benefit-sharing agreements are negotiated (mining revenue percentages, community development funds, employment quotas), are they enforced or circumvented through transfer pricing and corporate restructuring?',
    'Audit of corporate tax payments and transfer pricing practices; tracking of community development funds actually received vs agreed percentages; comparison of enforcement mechanisms across jurisdictions',
    'If genuinely enforced: tangled_rope perspectives are accurate and negotiations can achieve mixed outcomes. If circumvented: apparent agreements are theater — classify as piton. Victims remain powerless despite formal agreements.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(corporate_benefit_sharing_enforcement, empirical, 'Enforcement of benefit-sharing agreements in mining contracts').

omega_variable(
    suppression_mechanism_structural_vs_cognitive,
    'Is the measured suppression (0.72) primarily structural (lack of capital, geographic barriers, legal exclusion) or partly internalized (miners'' belief that they have no alternative, that industrial expansion is inevitable)?',
    'Pre- vs post-displacement analysis: if suppression persists after legal displacement occurs (miners unable to organize alternatives despite structural barriers removed), this indicates internalized suppression. Comparison with communities that successfully maintained artisanal access.',
    'If structural: exit remains trapped; snare classification stable. If internalized: effective suppression is higher than measured (cognitive capture adds to structural barriers); community may be unable to organize even if material barriers removed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_cognitive, empirical, 'Whether suppression is structural or internalized').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(artisanal_mining_displacement, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(amd_tr_t0, artisanal_mining_displacement, theater_ratio, 0, 0.35).
narrative_ontology:measurement(amd_tr_t5, artisanal_mining_displacement, theater_ratio, 5, 0.45).
narrative_ontology:measurement(amd_tr_t10, artisanal_mining_displacement, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(amd_be_t0, artisanal_mining_displacement, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(amd_be_t5, artisanal_mining_displacement, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(amd_be_t10, artisanal_mining_displacement, base_extractiveness, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(artisanal_mining_displacement, resource_allocation).
narrative_ontology:affects_constraint(artisanal_mining_displacement, artisanal_gold_mining_mercury_exposure).
narrative_ontology:affects_constraint(artisanal_mining_displacement, rare_earth_mining_environmental_externality).
narrative_ontology:affects_constraint(artisanal_mining_displacement, conflict_minerals_supply_chain).

% DUAL FORMULATION NOTE:
% Artisanal mining displacement is upstream of commodity-specific constraints: gold mining exposes workers to mercury; rare earth mining creates localized environmental damage; conflict minerals create geopolitical capture. Each commodity system has its own structural dynamics, but all downstream constraints are shaped by the underlying displacement mechanism that concentrates extraction rights in industrial operators.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(artisanal_mining_displacement, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
