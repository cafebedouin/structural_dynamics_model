% ============================================================================
% CONSTRAINT STORY: cumbria_mine_rejection
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cumbria_mine_rejection, []).

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
 *   constraint_id: cumbria_mine_rejection
 *   human_readable: UK government rejection of the Woodhouse Colliery coal mine
 *   domain: economic/political
 *
 * SUMMARY:
 *   The UK government's rejection of the Woodhouse Colliery coal mine in
 *   Cumbria (October 2023) represents a critical constraint at the
 *   intersection of climate policy, regional economic interests, and
 *   regulatory authority. The constraint embodies the structural tension
 *   between enforcing net-zero climate commitments at the national level and
 *   the concentrated economic costs borne by specific regions dependent on
 *   coal extraction. The decision emerged from a complex institutional
 *   process: the local planning authority (Cumbria County Council) initially
 *   approved the application (2020) on grounds of local economic benefit and
 *   regulatory compliance; the national government (then under Boris Johnson)
 *   appeared to support the project; but subsequent policy shifts,
 *   international climate commitments, and environmental pressure led the new
 *   Prime Minister (Liz Truss, later Rishi Sunak) to overturn the approval on
 *   grounds of climate impact incompatibility with net-zero targets. The
 *   rejection illustrates how the same regulatory framework functions as
 *   coordination mechanism (enforcing climate commitments), extraction
 *   apparatus (concentrating transition costs on mining communities),
 *   degraded local authority (centralized override of local deliberation),
 *   temporary transition framework (climate deadline creating sunset logic),
 *   and natural law enforcement (physics of carbon budgets). Theater has
 *   increased over the decision interval: the initial local planning inquiry
 *   (2017-2020) was substantive deliberation over mine design, environmental
 *   impact, and regional economic need; the centralized override converted
 *   that deliberative process into performative theater — the local decision
 *   was subordinated to predetermined national climate framework.
 *   Extractiveness has accumulated as the constraint has hardened: initial
 *   soft resistance (2017-2020, coordination framing) has transformed into
 *   structural lock-out (2023+, extraction framing). The theater ratio
 *   reflects that central government justified the decision via climate
 *   necessity while simultaneously managing headlines about 'regional
 *   levelling-up' and 'supporting workers' — the rhetoric of transition
 *   support masked the reality of centralized cost displacement.
 *
 * KEY AGENTS:
 *   - Cumbrian Mining Community: Primary victim (powerless/trapped) — faces permanent job loss, asset devaluation, skill obsolescence with no regional alternatives
 *   - Regional Coal Industry (Woodhouse Colliery operator): Organized victim (organized/constrained) — loses capital investment, stranded assets, market access
 *   - UK Central Government (Department for Levelling Up, BEIS): Decision-maker (institutional/constrained) — experiences mixed coordination (climate credibility) and extraction (regional pain management)
 *   - Climate Regulatory Framework: Beneficiary (institutional/arbitrage) — net-zero targets, carbon budgets, international climate commitments all benefit from capacity constraint
 *   - Environmental Constituencies: Organized beneficiary (organized/mobile) — climate movement, conservation groups benefit from precedent-setting rejection
 *   - Local Planning Authority (Cumbria County Council): Degraded institutional actor (institutional/constrained) — initial authority subordinated to central override
 *   - Global Coking Coal Markets: Structural beneficiary (powerful/arbitrage) — reduced UK supply narrows global competition, supports alternative suppliers
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing policy choice as physical law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cumbria_mine_rejection, 0.38).
domain_priors:suppression_score(cumbria_mine_rejection, 0.62).
domain_priors:theater_ratio(cumbria_mine_rejection, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cumbria_mine_rejection, extractiveness, 0.38).
narrative_ontology:constraint_metric(cumbria_mine_rejection, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(cumbria_mine_rejection, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cumbria_mine_rejection, tangled_rope).
narrative_ontology:human_readable(cumbria_mine_rejection, "UK government rejection of the Woodhouse Colliery coal mine").
narrative_ontology:topic_domain(cumbria_mine_rejection, "economic/political").

domain_priors:requires_active_enforcement(cumbria_mine_rejection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cumbria_mine_rejection, climate_regulatory_framework).
narrative_ontology:constraint_beneficiary(cumbria_mine_rejection, environmental_constituencies).
narrative_ontology:constraint_victim(cumbria_mine_rejection, regional_coal_industry).
narrative_ontology:constraint_victim(cumbria_mine_rejection, local_employment).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CUMBRIAN MINING COMMUNITY (SNARE) — Faces extractive rejection with no meaningful alternative employment in the region. Historical skill sets become worthless; geographic and social capital locked into mining. d≈0.92, f(d)≈1.40, σ=0.9 → χ≈0.49.
constraint_indexing:constraint_classification(cumbria_mine_rejection, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: REGIONAL MINING OPERATORS (TANGLED ROPE) — Constrained by capital expenditure, regulatory uncertainty, and market volatility. Extraction visible through access denial and stranded assets. Coordination exists through industry associations and regional economic forums. d≈0.68, f(d)≈1.02, σ=0.9 → χ≈0.35.
constraint_indexing:constraint_classification(cumbria_mine_rejection, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: GLOBAL ENERGY MARKETS (ROPE) — Net beneficiary through coal price support from capacity constraints. UK rejection narrows supply competition. Can arbitrage to alternative suppliers. d≈0.15, f(d)≈-0.01, σ=1.0 → χ≈-0.004. Effectively benefits from the constraint through reduced domestic competition.
constraint_indexing:constraint_classification(cumbria_mine_rejection, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CLIMATE TRANSITION FRAMEWORK (SCAFFOLD) — Central government coordination mechanism with sunset: net-zero commitments and carbon budgets create binding endpoints for coal extraction. The rejection enforces a temporary but time-limited extraction pathway. Has beneficiary (climate goals) and coordination function (transition planning). d≈0.35, f(d)≈0.35, σ=1.0 → χ≈0.13.
constraint_indexing:constraint_classification(cumbria_mine_rejection, scaffold,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: UK GOVERNMENT DECISION-MAKER (TANGLED ROPE) — Central government apparatus experiences the constraint as mixed: coordination function (enforcing climate commitments, maintaining international credibility) and extraction mechanism (denying regional economic opportunity, concentrating transition pain). Constrained by election cycles, climate pledges, and fiscal responsibility. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.25.
constraint_indexing:constraint_classification(cumbria_mine_rejection, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ENVIRONMENTAL CONSTITUENCIES (TANGLED ROPE) — Organized actors (climate movements, conservation groups) experience coordination benefit (norm-setting for climate action) and extraction mechanism (displacement of climate costs to poorer regions, outsourcing of mining to countries with weaker regulation). Mobile through both advocacy and investment shift. d≈0.48, f(d)≈0.59, σ=1.2 → χ≈0.27.
constraint_indexing:constraint_classification(cumbria_mine_rejection, tangled_rope,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: LOCAL PLANNING AUTHORITY (PITON) — Initially approved the mine application (2020); central government override devalued the local process to performative theater. Theater ratio high (0.58+) because planning inquiry showed sophistication of local deliberation contradicted by centralized reversal. Central government decision was predetermined by national climate framework, making local process a ritual. d≈0.58, f(d)≈0.75, σ=0.9 → χ≈0.31.
constraint_indexing:constraint_classification(cumbria_mine_rejection, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational scale, carbon physics is immutable: continued coal combustion violates net-zero constraints. The rejection appears as enforcement of physical law. However, base properties (ε=0.38, suppression=0.62, theater=0.58) contradict mountain classification. Engine will compute false summit: the 'natural law' framing masks policy choices about transition speed, distributional fairness, and technology deployment.
constraint_indexing:constraint_classification(cumbria_mine_rejection, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cumbria_mine_rejection_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cumbria_mine_rejection, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cumbria_mine_rejection, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(cumbria_mine_rejection, TR),
    TR >= 0.70.

:- end_tests(cumbria_mine_rejection_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The rejection channels real climate necessity (carbon physics) but distributes costs in ways that exceed necessity — the same climate goal could be achieved with faster transition support, technology deployment, or alternative coking coal sourcing. The moderate value reflects that genuine coordination (climate enforcement) is entangled with extraction (cost displacement to specific region). Initial ε=0.18 reflects belief in coordination (2017-2020, local deliberation framing); middle ε=0.28 reflects initial conflict (2020-2023, planning inquiry vs government pivot); final ε=0.38 reflects extraction visibility after rejection (2023+, stranded assets, job losses). Suppression (0.62): Moderate-high. Significant barriers to alternative outcomes include global energy market structure, net-zero legal commitments, carbon budget irreversibility, and political economy of climate transition. However, suppression is not total — regional transition support, technology substitution, and global market shifts could reduce costs. Theater ratio (0.58): Moderate-high. The decision process involved substantive local planning deliberation (low theater, 2017-2020) but was ultimately predetermined by national climate framework, converting local process into ritual. Central government framing emphasized climate necessity while managing 'levelling up' narrative — gap between substantive justification and strategic rhetoric indicates theater. Trajectory shows rising theater as the central override subordinated local deliberation.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates multiple perspectives collapsing into single institutional decision. Mining community sees pure extraction (Snare): rejection with no transition support or alternative livelihood pathway. Mining operators see extraction with some coordination (Tangled Rope): loss of capital investment but also participation in industry associations and market mechanisms. Global energy markets see net coordination benefit (Rope): reduced UK supply competition benefits existing suppliers. Climate framework sees temporary transition (Scaffold): net-zero deadline creates sunset for coal extraction with structured wind-down pathway. Central government sees mixed extraction and coordination (Tangled Rope): enforcement of climate commitments (coordination) with concentrated regional costs (extraction). Environmental constituencies see coordination victory with externalized costs (Tangled Rope): climate action achieved but extraction of costs from poorer regions not visible. Local planning authority sees degraded authority (Piton): decision-making process converted to performative theater by centralized override. Analytical observer risks seeing immutable natural law (Mountain): carbon physics appears to dictate outcome — but the rejection's extractive character (cost distribution, timing, transition support) reveals contingent policy choice, not physical necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   Cumbrian mining community: Victim + trapped → d≈0.92, f(d)≈1.40. Maximum extraction. Mining operators: Victim + constrained → d≈0.68, f(d)≈1.02. High extraction. Global energy markets: Beneficiary + arbitrage → d≈0.15, f(d)≈-0.01. Net beneficiary. Climate framework: Beneficiary + mobile → d≈0.35, f(d)≈0.35. Low-moderate extraction (framework has agency through policy adjustment). Central government: Beneficiary (of climate credibility) and victim (of regional pain) simultaneously; constrained by electoral cycles and international commitments → d≈0.50, f(d)≈0.65. Symmetric exposure. Environmental constituencies: Beneficiary (climate goal achieved) + mobile (can shift investment, advocacy) → d≈0.48, f(d)≈0.59. Moderate extraction (beneficiary but mobile, so not extracting at maximum). Local authority: Beneficiary in theory (enforcing climate law) but victim of authority erosion (constrained) → d≈0.58, f(d)≈0.75. Extraction visible through degradation. Analytical observer: Structural beneficiary of 'natural law' framing (naturalizes policy choice) → d≈0.72, f(d)≈1.15. Mountain classification is false summit.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that this constraint exhibits genuine coordination function (enforcing net-zero commitments, setting climate precedent, managing international reputation) AND asymmetric extraction (concentrating transition costs on specific region, displacing mining without equivalent transition support, subordinating local decision-making). The tangled rope classification reflects this duality. The extraction is not incidental to the coordination function — it is built into the structure: climate goals are achieved faster when costs are externalized to politically weak regions than when transition support is front-loaded. The regional mining community cannot articulate the extraction as coordinated because they lack power (Snare perspective). The environmental constituencies cannot see the extraction because they benefit from the coordination (Rope-like perspective). The analytical observer risks naturalizing the extraction as physical necessity (false summit Mountain). The mandatrophy resolves when we recognize that all perspectives are legitimate: the constraint IS tangled rope. It coordinates climate action AND extracts regionally. No single perspective captures the full structure. The constraint's purpose (climate transition) is real; its extractive distribution is also real. The resolution mechanism would be transition support and local agency restoration — moving the constraint from Tangled Rope toward Scaffold by adding time-bound regional investment and community voice in implementation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transition_speed_extraction_boundary,
    'Does the rejection represent fair climate policy or accelerated extraction from the mining region?',
    'Comparative analysis of transition support: government investment in regional retraining, renewable infrastructure, and economic diversification in Cumbria vs total climate benefit of coal rejection. Measure regional decline metrics (unemployment, out-migration, asset devaluation) against marginal carbon savings from UK rejection.',
    'If transition support ≥ 60% of extraction cost: classification shifts toward Scaffold (temporary support with real structural exit). If transition support < 20% of extraction cost: classification strengthens toward Snare (pure extraction justified by climate framing).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transition_speed_extraction_boundary, empirical, 'Boundary between fair climate transition and extractive concentration of adjustment costs').

omega_variable(
    leakage_externality_reality,
    'Does UK coal rejection reduce global emissions or simply displace mining to countries with weaker environmental regulation?',
    'Lifecycle analysis of UK coal rejection: comparison of UK coal supply cost vs global coking coal alternatives; measurement of actual carbon displacement (reduced global combustion) vs carbon leakage (same coal extracted elsewhere). Historical precedent from UK coal phase-out (1970s-1990s).',
    'If leakage > 70%: rejection is performative extraction by wealthy countries (coal still burned, but blame externalized). If leakage < 30%: rejection has real climate function and Tangled Rope classification holds. If leakage 30-70%: mixed function, both coordination and strategic externality.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(leakage_externality_reality, empirical, 'Whether coal rejection reduces global emissions or displaces extraction').

omega_variable(
    regulatory_capture_direction,
    'Is the rejection driven by climate science and public mandate, or by regulatory capture by environmental constituencies?',
    'Timeline analysis: when did climate science consensus emerge vs when did environmental organizations shift from opposition to centrality in decision-making? Funding sources for decision-makers; revolving door between Environment Agency, BEIS, and environmental NGOs vs industry-regulator connections.',
    'If capture by environmental constituencies: beneficiary extraction mechanism is more visible, higher d for government actor. If pure climate science: coordination mechanism predominates, lower d. Direction of capture determines whether constraint is balanced tangled rope or asymmetric snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_direction, empirical, 'Whether rejection is science-driven or environmentally-captured regulation').

omega_variable(
    localism_suppression_mechanism,
    'Does central government override suppress local agency by design (democratic erosion) or by necessity (climate emergency)?',
    'Institutional comparison: analyze other sectors (nuclear waste, renewable energy, infrastructure) where central/local authority balance is similar. Examine decision documentation for explicit local voice incorporation vs pre-determination language. Compare to other climate-adjacent rejections with local support (fossil fuel infrastructure).',
    'If by design: suppression is structural and intentional, supporting Snare/Tangled Rope classification. If by necessity: suppression is contingent on climate timescale, supporting Scaffold classification with sunset clause clarity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(localism_suppression_mechanism, conceptual, 'Whether centralized rejection suppresses local agency by necessity or design').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cumbria_mine_rejection, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cumbria_tr_t0, cumbria_mine_rejection, theater_ratio, 0, 0.35).
narrative_ontology:measurement(cumbria_tr_t5, cumbria_mine_rejection, theater_ratio, 5, 0.48).
narrative_ontology:measurement(cumbria_tr_t10, cumbria_mine_rejection, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(cumbria_be_t0, cumbria_mine_rejection, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(cumbria_be_t5, cumbria_mine_rejection, base_extractiveness, 5, 0.28).
narrative_ontology:measurement(cumbria_be_t10, cumbria_mine_rejection, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cumbria_mine_rejection, enforcement_mechanism).
narrative_ontology:affects_constraint(cumbria_mine_rejection, net_zero_carbon_budget_regime).
narrative_ontology:affects_constraint(cumbria_mine_rejection, regional_development_inequality).
narrative_ontology:affects_constraint(cumbria_mine_rejection, energy_transition_stranded_assets).

% DUAL FORMULATION NOTE:
% Cumbria Mine Rejection (ε=0.38, Tangled Rope) is downstream of Net-Zero Carbon Budget Regime (higher confidence, ε≈0.08-0.15, Mountain/Rope) and affects Regional Development Inequality (lower confidence, higher extraction). The rejection illustrates how a well-established coordination mechanism (carbon budgets) can drive asymmetric extraction when implementation lacks transition support.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cumbria_mine_rejection, institutional, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
