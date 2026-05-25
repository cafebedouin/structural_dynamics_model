% ============================================================================
% CONSTRAINT STORY: iron_law_of_oligarchy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_iron_law_of_oligarchy, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: iron_law_of_oligarchy
 *   human_readable: The Iron Law of Oligarchy
 *   domain: political/social
 *
 * SUMMARY:
 *   Robert Michels' Iron Law of Oligarchy proposes that all complex
 *   organizations, regardless of their initial democratic aspirations or
 *   constitutional design, inevitably develop into oligarchies where power
 *   concentrates in the hands of a small leadership core. The constraint
 *   operates through a combination of coordination necessity, information
 *   asymmetry, and structural incentive misalignment. Members initially
 *   participate expecting democratic voice and collective benefit;
 *   organizations initially form with egalitarian intent. Yet within
 *   organizational lifespans (measured in years to decades), decision-making
 *   authority concentrates upward, participatory mechanisms become
 *   performative, and a self-perpetuating leadership class emerges. This
 *   constraint exhibits all six classification types from different
 *   structural perspectives, making it a diagnostic exemplar for
 *   understanding how a single organizational phenomenon can be
 *   simultaneously natural law (inherent to organization at scale),
 *   coordination mechanism (leadership function), extraction regime (power
 *   asymmetry), temporary problem (alternative governance models offering
 *   sunset paths), degraded ritual (leadership apparatus maintained by
 *   inertia), and Tangled Rope hybrid (mixing genuine coordination benefits
 *   with systematic power concentration). The theater ratio trajectory (0.35
 *   → 0.68) reflects the degradation of participatory mechanisms from
 *   functional (early democratic procedures with real member voice) to
 *   performative (mature oligarchic systems where democratic forms persist
 *   but substantive power is concentrated).
 *
 * KEY AGENTS:
 *   - Rank-and-File Members: Primary victims (powerless/trapped) — bear organizational labor and costs while decision authority concentrates; exit barriers create sustained entrapment
 *   - Organizational Leadership: Primary beneficiary (institutional/arbitrage) — captures authority, status, resource control, and information privileges; experiences oligarchy as functional coordination
 *   - Administrative Core: Secondary beneficiary (powerful/constrained) — manages organizational machinery; benefits from hierarchy but constrained by leadership oversight
 *   - Democratic Reformers: Secondary victims (moderate/constrained) — seek distributed authority within existing organizational framework; experience mixed benefits and extraction
 *   - Structural Reform Coalition: Organized agents (organized/constrained) — develop alternative governance architectures (blockchain governance, liquid democracy, federated models); view oligarchy as solvable problem with sunset trajectory
 *   - Institutional Legacy Apparatus: Institutional actor (institutional/arbitrage) — maintains hierarchical bureaucratic structures through inertia; sees own mechanisms as degraded
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks treating contingent institutional arrangements as inherent organizational laws
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(iron_law_of_oligarchy, 0.58).
domain_priors:suppression_score(iron_law_of_oligarchy, 0.65).
domain_priors:theater_ratio(iron_law_of_oligarchy, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(iron_law_of_oligarchy, extractiveness, 0.58).
narrative_ontology:constraint_metric(iron_law_of_oligarchy, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(iron_law_of_oligarchy, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(iron_law_of_oligarchy, tangled_rope).
narrative_ontology:human_readable(iron_law_of_oligarchy, "The Iron Law of Oligarchy").
narrative_ontology:topic_domain(iron_law_of_oligarchy, "political/social").

domain_priors:requires_active_enforcement(iron_law_of_oligarchy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(iron_law_of_oligarchy, organizational_leadership).
narrative_ontology:constraint_beneficiary(iron_law_of_oligarchy, administrative_core).
narrative_ontology:constraint_victim(iron_law_of_oligarchy, rank_and_file_members).
narrative_ontology:constraint_victim(iron_law_of_oligarchy, participatory_democracy_principle).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RANK-AND-FILE MEMBER (SNARE) — Entrapped within organizational structures that promise democratic voice but systematically concentrate power upward. Members bear organizational costs (dues, participation, labor) while decision-making authority flows exclusively to leadership. Exit is costly (loss of community, collective benefits, organizational identity). Maximum experienced extraction with no meaningful alternatives.
constraint_indexing:constraint_classification(iron_law_of_oligarchy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INTERNAL REFORMER (TANGLED ROPE) — Benefits from organizational coordination (collective resources, scale advantages, institutional stability) but also experiences extraction through systematic exclusion from power. Some agency exists (committees, dissent, internal campaigns) but constrained by structural incentives that favor leadership consolidation. Mixed experience: genuine coordination gains from the organization, genuine extraction through governance asymmetry.
constraint_indexing:constraint_classification(iron_law_of_oligarchy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ORGANIZATIONAL LEADERSHIP (ROPE) — Experiences the oligarchic structure as coordination mechanism. Leadership consolidates and stabilizes organizational function, communicates direction, manages complexity. From leadership position, oligarchy is functional solution to coordination problems — seen as enabling rather than constraining.
constraint_indexing:constraint_classification(iron_law_of_oligarchy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: STRUCTURAL REFORM COALITION (SCAFFOLD) — Organized agents (transparency advocates, participatory democracy movements, decentralized governance experiments) view oligarchic consolidation as a solvable coordination failure with a sunset trajectory. Blockchain-based governance, liquid democracy, federated structures, and distributed authority models offer alternative architectures. Extraction appears high only before sunset mechanisms activate — as alternative governance technologies mature, the traditional oligarchic lever loses leverage.
constraint_indexing:constraint_classification(iron_law_of_oligarchy, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: INSTITUTIONAL LEGACY APPARATUS (PITON) — Traditional hierarchical organizational structures maintain oligarchic patterns through institutional inertia despite degraded functional necessity. The bureaucratic apparatus persists because nothing has fully replaced it, not because it optimally solves modern coordination problems. High theater ratio: performative committees, facade democratic procedures, symbolic consultation without power transfer. The machinery is theatrically maintained.
constraint_indexing:constraint_classification(iron_law_of_oligarchy, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational perspective, some degree of leadership differentiation and authority concentration is an invariant feature of complex organization. Perfectly distributed power creates coordination collapse. The 'iron law' frames oligarchy as inherent mathematical/organizational necessity — the price of scale and complexity. However, this risks naturalizing what may be contingent institutional arrangements (funding models, information asymmetries, deliberative capacity constraints) as immutable laws.
constraint_indexing:constraint_classification(iron_law_of_oligarchy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(iron_law_of_oligarchy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(iron_law_of_oligarchy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(iron_law_of_oligarchy, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(iron_law_of_oligarchy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(iron_law_of_oligarchy, TR),
    TR >= 0.70.

:- end_tests(iron_law_of_oligarchy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Leadership consolidates decision authority, information access, and control of organizational resources over time. Members bear coordination costs (dues, participation, labor) without proportional influence. However, not maximal (0.70+) because organizations also provide genuine collective benefits — pooled resources, institutional stability, scale advantages — that distribute to members. The extraction is real but superimposed on coordination functions. Suppression (0.65): Moderate-high. Barriers to meaningful member power include: structural incentives for leadership consolidation, information asymmetries in organizational operations, deliberative capacity constraints (large organizations cannot make all decisions democratically), sunk costs in hierarchy maintenance, psychological factors (legitimacy narratives for authority), and exit costs (loss of community, collective benefits). But suppression is not total — member voice mechanisms exist (albeit degraded), dissent is possible, and some organizations maintain higher participation than others. Theater ratio (0.68): Moderately high. Participatory democracy procedures (member votes, committees, forums) become increasingly performative as organizations mature. Democratic forms persist as legitimacy theater while substantive decision authority remains concentrated. The trajectory from 0.35 to 0.68 reflects Goodhart drift: early organizational stage retains genuine democratic function; mature stage maintains democratic appearance while function atrophies.
 *
 * PERSPECTIVAL GAP:
 *   The perceptual rift between organizational levels is fundamental. Leadership genuinely solves coordination problems through hierarchical direction-setting — from their perspective, oligarchy enables function. Members experience the same hierarchy as a power extraction mechanism — from their perspective, leadership consolidation contradicts stated democratic intent. Internal reformers experience both simultaneously: organizational scale benefits them (collective resources, institutional reach) while power asymmetry constrains them (excluded from strategy, governance controlled by leadership). The structural reform coalition sees oligarchy as a solvable problem (technology can restore distributed decision-making); the institutional legacy apparatus sees oligarchy as inevitable (hierarchy is the only model that has worked). The analytical observer risks naturalizing the institutional arrangement as an immutable law — treating organizational scale constraints as if they make oligarchy logically necessary rather than contingently institutional. The mandatrophy question is whether the Iron Law describes an invariant property of complex systems or a specific institutional design pattern that could be replaced.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values emerge from structural position within the power hierarchy. Rank-and-file members with trapped exit options and no power in decision-making experience high d (0.90+) — maximum experienced extraction. Leadership with arbitrage options (can move between organizations, access alternative institutional positions) experiences low d (0.10-0.20) — minimal effective extraction despite formal authority concentration. This is the fundamental asymmetry: the same institutional structure produces radically different experienced extraction depending on structural position. The reformer occupies intermediate position (d ≈ 0.55-0.65) — benefits from organizational resources but bears costs of excluded authority. The organized reform coalition has agency and exit alternatives (d ≈ 0.40-0.50) — constrained but not trapped. The analytical observer's d approaches a measurement artifact (0.72) because their exit is analytical rather than structural — they observe the system but don't inhabit it.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy surfaces the deepest structural question: Is the Iron Law an immutable property of complex organization, or a contingent institutional arrangement enabled by specific technological/structural limitations? The natural law perspective (Mountain classification from analytical context) argues that coordination at scale necessarily requires information concentration and authority delegation — democracy requires participation, participation becomes infeasible above some group size, hierarchy emerges as a solvable necessity. The extraction perspective (Snare classification from member context) argues that modern information technology, distributed decision platforms, and transparent accountability systems make oligarchy a choice rather than a necessity — leadership consolidates power because existing institutional structures enable and incentivize it, not because democracy is mathematically impossible at scale. The scaffold perspective (organized/constrained) argues that alternative governance architectures (liquid democracy, blockchain-based decisions, federated authority) offer genuine sunset paths to oligarchic consolidation — as these technologies mature and prove viable, the traditional oligarchic lever becomes optional. The Tangled Rope classification (moderate/constrained) captures the genuine hybrid: organizations do provide coordination benefits that members value (collective resources, institutional stability, scale advantages) AND power asymmetries that members bear (excluded decision authority, concentrated information, leadership privilege). The mandatrophy is resolved by recognizing that the constraint is structural but not inevitable — the observable data (organizational power concentration over time) is real, but its classification depends on whether you measure it as a necessary feature of organization (Mountain) or as a contingent institutional arrangement (Snare/Tangled Rope/Scaffold). The decomposition into separate constraint stories reveals the answer: organizations with genuinely distributed authority (federated governance, cooperative structures, liquid democracy pilots) do exist and function at scale, indicating that oligarchy is not mathematically necessary. The Iron Law as traditionally stated (all organizations inevitably oligarch) is therefore false — it describes a widespread institutional pattern that emerges from specific organizational design choices, not an immutable law of nature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    necessity_vs_contingency,
    'Is oligarchic consolidation an inherent feature of organization at scale, or a contingent institutional arrangement enabled by specific information/deliberative constraints?',
    'Empirical analysis of organizations with genuinely distributed authority (network-based governance, liquid democracy platforms, federated structures); comparison of power concentration metrics across governance architectures; technological capability assessment for transparent, auditable distributed decision-making',
    'If inherent (Mountain): oligarchy is unavoidable constraint on democratization. If contingent (Snare/Tangled Rope): oligarchy is extractive arrangement sustained by specific institutional choices that could be redesigned.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessity_vs_contingency, empirical, 'Whether oligarchic concentration is inherent or contingent').

omega_variable(
    member_exit_capacity,
    'What proportion of organizational members could exit without catastrophic loss? How does this vary by organization type and member socioeconomic dependency?',
    'Survey of exit barriers by membership type; analysis of member churn under varying oligarchic severity; comparison of exit rates in organizations with distributed vs concentrated authority',
    'High exit capacity → trap severity decreases, classification shifts from Snare toward Tangled Rope/Scaffold. Low exit capacity → true entrapment, Snare confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(member_exit_capacity, empirical, 'Measurement of member exit capacity and barriers').

omega_variable(
    scale_threshold_for_oligarchy,
    'Below what organization size does meaningful distributed authority remain viable? Where does the critical scale threshold lie?',
    'Comparative analysis of governance structures in organizations of different sizes (10 vs 100 vs 1000 vs 10000 members); identification of size cohorts where democratic participation remains materially feasible vs where consolidation becomes dominant',
    'If threshold is < 100 members: most organizations remain above it and are susceptible to oligarchy (Mountain view supported). If threshold is > 1000 members: many organizations operate below it with viable distributed authority (Mountain view undermined, contingency supported).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scale_threshold_for_oligarchy, empirical, 'Organization size threshold for oligarchic consolidation').

omega_variable(
    information_asymmetry_role,
    'How much of oligarchic consolidation is driven by information asymmetries (leaders have superior knowledge/communication capacity) vs structural incentive misalignment (leadership benefits from power consolidation)?',
    'Controlled experiments with organizations given radical transparency tools (all decision data, deliberation records, performance metrics publicly auditable); measurement of power redistribution when information asymmetry is eliminated',
    'If asymmetry-driven: transparency and distributed information systems could reverse oligarchy (Scaffold view valid). If incentive-driven: information alone insufficient, structural incentive redesign required (deeper extraction mechanism).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(information_asymmetry_role, empirical, 'Role of information asymmetry versus structural incentives in oligarchy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(iron_law_of_oligarchy, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(iron_tr_t0, iron_law_of_oligarchy, theater_ratio, 0, 0.35).
narrative_ontology:measurement(iron_tr_t15, iron_law_of_oligarchy, theater_ratio, 15, 0.55).
narrative_ontology:measurement(iron_tr_t30, iron_law_of_oligarchy, theater_ratio, 30, 0.68).

% Extraction over time
narrative_ontology:measurement(iron_be_t0, iron_law_of_oligarchy, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(iron_be_t15, iron_law_of_oligarchy, base_extractiveness, 15, 0.45).
narrative_ontology:measurement(iron_be_t30, iron_law_of_oligarchy, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(iron_law_of_oligarchy, enforcement_mechanism).
narrative_ontology:affects_constraint(iron_law_of_oligarchy, regulatory_capture).
narrative_ontology:affects_constraint(iron_law_of_oligarchy, union_corruption).
narrative_ontology:affects_constraint(iron_law_of_oligarchy, nonprofit_mission_drift).

% DUAL FORMULATION NOTE:
% The Iron Law of Oligarchy decomposes into at least two structurally distinct claims: (1) Organizational complexity requires some degree of leadership differentiation and decision authority concentration (true, low ε ≈ 0.15, Mountain); (2) Power concentration in complex organizations is inevitable and irreversible (disputed, ε varies 0.40-0.70 depending on governance architecture). This story addresses the composite claim including both dimensions. Downstream constraints (regulatory capture, union corruption, nonprofit drift) instantiate the Iron Law in specific institutional domains where oligarchic consolidation has extracted private benefits from public function.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
