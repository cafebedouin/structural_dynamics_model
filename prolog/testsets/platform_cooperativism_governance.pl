% ============================================================================
% CONSTRAINT STORY: platform_cooperativism_governance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_platform_cooperativism_governance, []).

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
 *   constraint_id: platform_cooperativism_governance
 *   human_readable: Democratic Worker Governance in Platform Cooperativism
 *   domain: economic/social/technological
 *
 * SUMMARY:
 *   Platform Cooperativism emerged in the mid-2010s as an alternative to
 *   venture-backed platform capitalism. The model proposes that workers
 *   should own and democratically govern the digital platforms they produce
 *   value through. This constraint examines the structural tension between
 *   the governance ideal (workers collectively deciding platform policy) and
 *   the material conditions that constrain that ideal (technical complexity,
 *   capital barriers, algorithmic opacity, unpaid labor burden, global
 *   distribution). The constraint exhibits properties of both pure
 *   coordination (workers solving collective action problems) and pure
 *   extraction (concentration of technical/financial power, unpaid governance
 *   labor, capital dependence). The core paradox: granting workers democratic
 *   governance can simultaneously empower them (Tangled Rope, Rope
 *   perspectives) and burden them with unpaid labor (Snare perspective),
 *   depending on whether governance participation is genuinely valued and
 *   compensated or performatively demanded.
 *
 * KEY AGENTS:
 *   - Individual Platform Workers: Primary victims (powerless/trapped) — face income dependence, algorithmic control, and unpaid governance labor burden
 *   - Worker Collective Leadership: Primary beneficiary (organized/constrained) — capture ownership equity, strategic influence, and dignity; but constrained by technical complexity and capital limits
 *   - Platform Cooperative Network: Secondary beneficiary (institutional/arbitrage) — leverage scale and knowledge transfer without hierarchy; benefit from network externalities
 *   - Cooperativism Transition Initiative: Institutional support actor (organized/constrained) — provide temporary governance training and capital bridging with explicit sunset clause
 *   - Venture Capital Venture-Backed Platforms: Competitive analog (institutional/arbitrage) — maintain performative worker consultation while retaining hierarchical control; persist through institutional inertia
 *   - Technical/Financial Staff Core: Institutional power center (organized/arbitrage) — retain control over algorithms and capital decisions even when formal governance is democratic
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(platform_cooperativism_governance, 0.52).
domain_priors:suppression_score(platform_cooperativism_governance, 0.58).
domain_priors:theater_ratio(platform_cooperativism_governance, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(platform_cooperativism_governance, extractiveness, 0.52).
narrative_ontology:constraint_metric(platform_cooperativism_governance, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(platform_cooperativism_governance, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(platform_cooperativism_governance, tangled_rope).
narrative_ontology:human_readable(platform_cooperativism_governance, "Democratic Worker Governance in Platform Cooperativism").
narrative_ontology:topic_domain(platform_cooperativism_governance, "economic/social/technological").

domain_priors:requires_active_enforcement(platform_cooperativism_governance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(platform_cooperativism_governance, worker_collectives).
narrative_ontology:constraint_beneficiary(platform_cooperativism_governance, platform_members).
narrative_ontology:constraint_victim(platform_cooperativism_governance, governance_capacity).
narrative_ontology:constraint_victim(platform_cooperativism_governance, capital_formation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL PLATFORM WORKER (SNARE) — Trapped in dual extraction: algorithmic management from the platform, plus distributed decision-making burden where workers must participate in governance without compensation or training. Democratic participation becomes unpaid labor. Exit costs are high (loss of income). No alternative platforms offering comparable income. Experiences maximum extraction through both wage suppression and governance coercion.
constraint_indexing:constraint_classification(platform_cooperativism_governance, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: WORKER COLLECTIVE LEADERSHIP (TANGLED ROPE) — Organized agents with genuine collective voice (board seats, governance committees), but constrained by technical complexity (platform architecture, financial modeling), capital constraints (limited ability to raise funding vs. venture-backed competitors), and decision paralysis in large distributed communities. Benefits from ownership equity and strategic influence; bears costs of governance labor and fiduciary liability. Mixed coordination function (democratic decision-making) and extraction (concentrated power in leadership core).
constraint_indexing:constraint_classification(platform_cooperativism_governance, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: PLATFORM COOPERATIVE NETWORK (ROPE) — Network organizations (Fairbnb, Stocksy, Savvy Cooperative) experience this constraint as pure coordination: linking independent platforms for shared standards, knowledge transfer, and collective bargaining leverage. Exit costs are low (can always operate independently). Benefits from scale without hierarchy. Sees governance democracy as a coordination strength, not an extraction problem.
constraint_indexing:constraint_classification(platform_cooperativism_governance, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: COOPERATIVISM TRANSITION INITIATIVE (SCAFFOLD) — Organizations (Cooperative Economics Alliance, Platform Cooperative Consortium, tech worker unions) providing temporary governance infrastructure, training, and capital bridging. They explicitly frame their role as sunset: building capacity until platforms can self-govern. Theater ratio reflects that much 'support' is aspirational networking rather than binding governance. Exit clause is clear: support phase out as platforms mature.
constraint_indexing:constraint_classification(platform_cooperativism_governance, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: VENTURE CAPITAL VENTURE-BACKED PLATFORM MODEL (PITON) — Conventional VC-backed platforms maintain performative worker consultation rituals (listening sessions, worker advisory boards) while retaining algorithmic control and unilateral decision-making. These rituals are largely theatrical — they produce the appearance of worker voice without structural power. But the model persists due to institutional inertia: the VC template is well-understood by investors, and genuine cooperative governance is seen as high-risk. Theater ratio high (0.64+) because the consultation apparatus exists but doesn't influence core decisions.
constraint_indexing:constraint_classification(platform_cooperativism_governance, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (STRUCTURAL VIEW) (TANGLED ROPE) — From a civilizational perspective, platform cooperativism presents a genuine hybrid: it enables coordination (worker collectives solving collective action problems around algorithmic governance, income stability, and work standards) while simultaneously embedding asymmetric extraction (governance labor is unpaid; technical/financial complexity concentrates power; capital constraints force compromises). The constraint cannot be reduced to either pure coordination or pure extraction. It is structurally a Tangled Rope: the coordination function is real but so is the extraction.
constraint_indexing:constraint_classification(platform_cooperativism_governance, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(platform_cooperativism_governance_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(platform_cooperativism_governance, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(platform_cooperativism_governance, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(platform_cooperativism_governance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(platform_cooperativism_governance, TR),
    TR >= 0.70.

:- end_tests(platform_cooperativism_governance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Platform cooperatives show genuine benefits (equity ownership, dignity, strategic voice) but also real costs (unpaid governance labor, technical complexity that excludes non-experts, capital constraints that force compromises, algorithmic opacity). The extractiveness is not as high as pure extraction (0.66+) because the coordination function is real — workers genuinely solve collective problems through democratic governance. But it exceeds pure coordination (0.35) because significant asymmetric extraction exists: governance labor is unpaid, technical decisions concentrate power, and capital dependence forces reintroduction of hierarchical funding relationships. Suppression (0.58): Moderate-high. Significant barriers include technical complexity (platform architecture, algorithmic systems, financial modeling require specialized expertise), capital constraints (lack of access to large-scale investment capital vs. VC-backed competitors), geographic distribution (consensus-building across global distributed workers is costly), and information asymmetry (workers often lack visibility into platform finances and technical constraints). Suppression is not total (workers can exit, though at significant cost) and is declining over time as tools improve. Theater ratio (0.64): Moderate-high. Governance processes often include performative elements: consultation meetings where key decisions are already made, voting on options that technical staff pre-filtered, committee work that doesn't influence deployment. Theater is not majority (0.64 < 0.70) because some governance decisions do genuinely affect platform operations. But theater has increased over the 12-year interval (0.48 → 0.64) as cooperatives have grown and decision-making has become more complex.
 *
 * PERSPECTIVAL GAP:
 *   Sharp perspectival divergence between individual workers and leadership. Individual workers experience this as Snare: they are coerced into governance participation (failure to participate risks social ostracism in tight-knit cooperatives) and receive no compensation for governance labor, while still facing algorithmic management and income volatility. Collective leadership experiences Tangled Rope: they genuinely govern strategy and capture ownership benefits, but are constrained by complexity and capital limits. The cooperative network experiences Rope: they coordinate without being subject to the constraints that bind individual platforms. The transition initiative experiences Scaffold: they see governance democracy as a temporary problem being solved through training and capital infrastructure. The analytical observer experiences Tangled Rope: the constraint is genuinely hybrid, combining real coordination function with real extraction through uncompensated labor and technical concentration of power. The VC-backed model's Piton classification reveals that conventional platforms simulate the cooperative governance form (consultation rituals) without the substance (decision power), maintaining the appearance while retaining hierarchical control.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) vary sharply across perspectives. For individual workers (powerless/trapped exit), d ≈ 0.95 (nearly full target) — they bear the costs of unpaid governance labor and income dependence. For collective leadership (organized/constrained exit), d ≈ 0.45 (mixed) — they benefit from ownership and influence but are constrained by complexity and capital limits. For the cooperative network (institutional/arbitrage exit), d ≈ 0.10 (near beneficiary) — they can exit easily and benefit from scale effects. For the transition initiative (organized/constrained with explicit sunset), d ≈ 0.35 (moderate beneficiary) — they provide temporary support and are incentivized to succeed. The analytical perspective (analytical/analytical exit) computes d ≈ 0.65 (moderate victim) — the structural constraint that creates the mismatch between governance aspiration and execution capacity affects the entire system's legitimacy.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in platform cooperativism is: 'Is worker democratic governance primarily a solution to platform extraction (Rope/Scaffold perspective) or primarily a new form of extraction masked in democratic language (Snare perspective)?' The analysis resolves this by showing that BOTH are structurally true simultaneously. The governance system IS a solution to unilateral algorithmic control (workers genuinely collectively decide platform policy in ways VC-backed platforms do not permit). But the governance system IS ALSO a mechanism that extracts unpaid labor from workers and concentrates technical/financial power in a core. The constraint is Tangled Rope: it must be both beneficial and extractive to be classified correctly. The mandatrophy is resolved not by choosing one reading but by accepting that the constraint's classification is genuinely mixed — it is a hybrid that solves real problems while creating new problems. Snare for individual rank-and-file; Tangled Rope for leadership and analytical observers; Rope for network participants; Scaffold for support institutions. The presheaf of perspectives IS the complete answer. False summits to avoid: treating it as pure Rope (ignores unpaid labor extraction), treating it as pure Snare (ignores genuine governance power), treating it as pure Scaffold (ignores that some cooperatives are mature, not temporary).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    governance_complexity_threshold,
    'What level of technical and financial complexity exceeds the decision-making capacity of distributed worker collectives, converting participation into rubber-stamp theater?',
    'Longitudinal study of cooperative platform governance decisions: correlation between decision complexity (quantified by blockchain votes, options analyzed, stakeholder consultation rounds) and actual implementation fidelity. Do workers'' governance choices actually change platform behavior, or are complex decisions delegated to technical staff regardless of collective votes?',
    'If threshold is low (complexity quickly exceeds worker capacity): most platform cooperatives are Pitons (performative democracy). If threshold is high (workers can handle significant complexity): genuine Tangled Ropes with real mixed function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(governance_complexity_threshold, empirical, 'Technical complexity threshold for worker governance capacity').

omega_variable(
    capital_formation_viability,
    'Can platform cooperatives raise sufficient capital to compete with VC-backed platforms, or is capital constraint a structural feature that forces compromises with venture capital (diluting ownership) or limits functionality?',
    'Comparative financial analysis: growth rates, feature parity, market share for capital-constrained cooperatives vs. VC-backed platforms in same domains. Identify whether capital constraint is overcome through alternative financing (equity crowdfunding, institutional impact investing, worker investment) or whether it forces cooperatives into venture partnerships that reintroduce hierarchy.',
    'If capital constraint can be overcome: cooperativism is a viable Tangled Rope with genuine coordination and modest extraction. If capital constraint is structural: cooperativism remains dependent on external capital sources, which reintroduce the extraction it claims to avoid. Snare classification becomes more defensible.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(capital_formation_viability, empirical, 'Whether platform cooperatives can achieve capital parity with VC-backed platforms').

omega_variable(
    governance_participation_fatigue,
    'Does the unpaid labor of democratic governance (attending meetings, voting, committee work) create participation fatigue or withdrawal, particularly among lower-income workers who cannot afford unpaid labor time?',
    'Participation audits across cooperatives: measure voting turnout, meeting attendance, and representativeness of active participants. Correlate with worker income, tenure, and platform seniority. Track whether governance participation becomes concentrated among a small leadership core or remains distributed.',
    'If participation fatigue is severe and stratified: governance becomes oligarchic despite democratic form. Snare classification for rank-and-file workers becomes dominant. If participation remains distributed and equitable: genuine Tangled Rope with real democratic function, though uncompensated labor remains extractive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(governance_participation_fatigue, empirical, 'Whether unpaid governance labor produces participation fatigue and oligarchy').

omega_variable(
    algorithmic_governance_authority,
    'Who retains control over algorithmic decision-making (task allocation, rating systems, payment formulas)? If workers govern ''strategy'' but technical staff control algorithms unilaterally, is the governance system a performance?',
    'Governance authority mapping: for each major platform feature (task allocation, rating, payment, deactivation), identify who has final decision power. Compare stated governance structures with actual deployment authority. Identify whether algorithmic changes are decided democratically or implemented by technical core without worker approval.',
    'If algorithms are outside worker governance: workers vote on strategy while algorithms execute extraction. Pure Snare from worker perspective. If algorithms are subject to democratic oversight: genuine Tangled Rope with worker control over both strategy and execution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithmic_governance_authority, empirical, 'Whether algorithmic governance is subject to worker democratic control').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(platform_cooperativism_governance, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(platcoop_tr_t0, platform_cooperativism_governance, theater_ratio, 0, 0.48).
narrative_ontology:measurement(platcoop_tr_t6, platform_cooperativism_governance, theater_ratio, 6, 0.58).
narrative_ontology:measurement(platcoop_tr_t12, platform_cooperativism_governance, theater_ratio, 12, 0.64).

% Extraction over time
narrative_ontology:measurement(platcoop_be_t0, platform_cooperativism_governance, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(platcoop_be_t6, platform_cooperativism_governance, base_extractiveness, 6, 0.46).
narrative_ontology:measurement(platcoop_be_t12, platform_cooperativism_governance, base_extractiveness, 12, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(platform_cooperativism_governance, resource_allocation).
narrative_ontology:affects_constraint(platform_cooperativism_governance, algorithmic_management_opacity).
narrative_ontology:affects_constraint(platform_cooperativism_governance, platform_worker_income_volatility).
narrative_ontology:affects_constraint(platform_cooperativism_governance, digital_labour_unionization).

% DUAL FORMULATION NOTE:
% Platform Cooperativism as a governance model (this story) is structurally distinct from the underlying economic constraints it attempts to solve: algorithmic management, income volatility, and labor power extraction. The cooperativism governance constraint has its own extractiveness (0.52) reflecting the structural cost of implementing democracy in technical systems, independent of whether the underlying platform economics are cooperative or capitalist. A cooperatively-owned platform with extractive algorithms is still subject to this constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(platform_cooperativism_governance, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
