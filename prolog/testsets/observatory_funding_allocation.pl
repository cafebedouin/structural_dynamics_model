% ============================================================================
% CONSTRAINT STORY: observatory_funding_allocation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_observatory_funding_allocation, []).

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
 *   constraint_id: observatory_funding_allocation
 *   human_readable: Observatory Funding Allocation Constraint
 *   domain: scientific_resource_allocation/astronomy
 *
 * SUMMARY:
 *   Observatory funding allocation creates a structural constraint that
 *   couples resource scarcity with institutional prestige hierarchies. Large
 *   observatories require enormous capital investment and operational costs,
 *   making centralization economically rational. However, the allocation
 *   process for observing time systematizes preferences for established
 *   institutions, creating barriers for early-career astronomers and
 *   non-elite research groups. This constraint exhibits the tangled rope
 *   structure: genuine coordination functions (shared instrumentation,
 *   coordinated observation campaigns, multi-institutional collaboration)
 *   coexist with asymmetric extraction (preferential access, funding bias,
 *   career gatekeeping). The theater ratio has risen over the interval as
 *   peer review committees increasingly recognize that allocation decisions
 *   reflect prestige more than merit, yet the ritual persists through
 *   institutional momentum. Meanwhile, open-access observatory movements are
 *   building alternative models (distributed networks, tiered access,
 *   crowdfunded observations) with explicit sunset clauses, creating a
 *   scaffold perspective on the same structural data.
 *
 * KEY AGENTS:
 *   - Early-Career Astronomers: Primary victims (powerless/trapped) — require established affiliation to access major observatories; cannot exit without career abandonment
 *   - Small and Regional Observatories: Secondary victims (moderate/constrained) — face funding competition bias; benefit from coordinated networks but constrained by prestige hierarchies
 *   - Large Research Institutions: Primary beneficiaries (institutional/arbitrage) — capture preferential access; can leverage prestige for additional resources; exit options through international partnerships
 *   - Allocation Review Committees: Institutional enforcement (institutional/arbitrage) — recognize degraded merit assessment but maintain ritual through inertia; see performative review as necessary evil
 *   - Open-Access Observatory Coalition: Organized agents (organized/constrained) — developing alternative funding models and distributed access networks with realistic sunset timeline
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing economic scarcity as justification for prestige-based allocation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(observatory_funding_allocation, 0.58).
domain_priors:suppression_score(observatory_funding_allocation, 0.62).
domain_priors:theater_ratio(observatory_funding_allocation, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(observatory_funding_allocation, extractiveness, 0.58).
narrative_ontology:constraint_metric(observatory_funding_allocation, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(observatory_funding_allocation, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(observatory_funding_allocation, tangled_rope).
narrative_ontology:human_readable(observatory_funding_allocation, "Observatory Funding Allocation Constraint").
narrative_ontology:topic_domain(observatory_funding_allocation, "scientific_resource_allocation/astronomy").

domain_priors:requires_active_enforcement(observatory_funding_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(observatory_funding_allocation, large_research_institutions).
narrative_ontology:constraint_beneficiary(observatory_funding_allocation, established_observatories).
narrative_ontology:constraint_victim(observatory_funding_allocation, early_career_astronomers).
narrative_ontology:constraint_victim(observatory_funding_allocation, small_observatories).
narrative_ontology:constraint_victim(observatory_funding_allocation, emerging_research_programs).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EARLY-CAREER ASTRONOMER (SNARE) — Trapped by funding constraints that require affiliation with well-established institutions to access major observatories. Cannot build independent research program or exit the constraint without career abandonment. Bears full cost of allocation system asymmetry.
constraint_indexing:constraint_classification(observatory_funding_allocation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: REGIONAL OBSERVATORY DIRECTOR (TANGLED ROPE) — Experiences both coordination (shared instrumentation, collaborative access protocols) and extraction (funding bias toward large institutions reduces their facility's competitiveness). High cost to exit through independent funding; some benefit from coordinated research networks.
constraint_indexing:constraint_classification(observatory_funding_allocation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LARGE RESEARCH INSTITUTION (ROPE) — Experiences the constraint as coordination mechanism. Established infrastructure enables efficient allocation; institutional prestige and track record generate preferential access. Net beneficiary with exit options through alternative funding and international partnerships.
constraint_indexing:constraint_classification(observatory_funding_allocation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN-ACCESS OBSERVATORY MOVEMENT (SCAFFOLD) — Organized coalition building alternative funding models (crowdfunding, tiered access, distributed observation networks) with explicit sunset clause. Low effective extraction because organized agents see genuine exit pathway. Constraint classification reflects temporary nature of centralized funding bottleneck.
constraint_indexing:constraint_classification(observatory_funding_allocation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: PEER REVIEW ALLOCATION COMMITTEE (PITON) — Review process for allocating observatory time is substantially performative. Committee members recognize that allocation decisions are driven by institutional prestige and track record rather than scientific merit alone. The review ritual persists through institutional inertia despite degraded functional verification capacity.
constraint_indexing:constraint_classification(observatory_funding_allocation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — Civilizational perspective frames observatory funding allocation as an immutable consequence of resource scarcity and institutional efficiency: large observatories require economies of scale, and concentrating resources maximizes scientific output. However, this naturalization masks contingent institutional arrangements that preserve existing power structures.
constraint_indexing:constraint_classification(observatory_funding_allocation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(observatory_funding_allocation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(observatory_funding_allocation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(observatory_funding_allocation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(observatory_funding_allocation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(observatory_funding_allocation, TR),
    TR >= 0.70.

:- end_tests(observatory_funding_allocation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The allocation system extracts preferential access for established institutions, creating career barriers for early-career researchers and smaller observatories. The extraction is not total because some mechanisms for alternative access exist (proposal competitions, director's discretionary time, international partnerships), and the system does coordinate legitimate multi-institutional research. The rising trend (0.42 → 0.58 over 20 years) reflects increasing concentration of observing time at flagship facilities as funding pressure grows. Suppression (0.62): High. Barriers include: requirement for institutional affiliation, lack of alternative large-observatory funding, publication bias favoring research from elite institutions, career risk of pursuing independent funding, and information asymmetry about allocation criteria. These barriers are substantial but not total — some early-career researchers do secure access through persistence, collaboration, or innovative funding. Theater ratio (0.68): Moderate-high. Peer review committees must assess scientific merit, but allocation decisions are visibly influenced by institutional prestige, track record, and reviewer familiarity with proposing institutions. The performative content has increased as committees recognize that blind review alone cannot overcome prestige bias, yet maintain review rituals as legitimating theater.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap reflects power asymmetries across the observation site. Large institutions and review committees see the constraint as legitimate coordination (Rope) — efficiently allocating scarce resources and enabling collaborative science. Early-career astronomers see pure extraction (Snare) — being locked out of career advancement without institutional backing. Regional observatories see mixed coordination and extraction (Tangled Rope) — their equipment and expertise support collaborative research while their institutions lose competitive advantage. The open-access coalition sees a temporary problem (Scaffold) — existing funding models are being replaced by distributed alternatives. The review committees see their own process as degraded (Piton) — they recognize the merit-assessment ritual is performative but maintain it for legitimacy. The civilizational analytical observer risks seeing immutable scarcity (Mountain) — observatories are expensive and rare, so concentration is inevitable — but this naturalizes what is partly a contingent institutional choice about how to allocate fixed resources.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality d derives from each agent's structural position within the extraction flow. Large institutions occupy beneficiary positions with arbitrage exits (they can secure observing time through multiple channels), generating low d values and negative f(d) — they experience the constraint as coordination benefit. Early-career astronomers are trapped victims with no exit alternatives, generating high d values and maximum f(d) — they experience the constraint as pure extraction (snare perspective). Regional observatories are constrained victims with some exit options (alternative telescopes, distributed collaboration), generating moderate d values. The open-access coalition occupies an organized position with constrained exits but with visibility of upcoming pathways (sunset clause logic), moderating their experienced extraction. The review committee, though institutional, has some capture by the system they administer, affecting their directionality compared to a fully independent institutional observer.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint simultaneously coordinates legitimate scientific collaboration and extracts preferential access through institutional prestige. The coordination function is real: shared observatories enable research impossible for individual institutions, and centralized review prevents duplication and optimizes observation schedules. The extraction is also real: the same processes that enable coordination create asymmetric access. The classification (Tangled Rope) reflects both mechanisms. Attempting to classify as pure coordination (Rope) ignores career gatekeeping effects; attempting to classify as pure extraction (Snare) ignores genuine scientific coordination benefits. The mandatrophy is resolved not by choosing one type but by recognizing the perspectival structure: beneficiaries perceive coordination; victims perceive extraction; observers at intermediate positions perceive both.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    merit_versus_prestige_circularity,
    'Are allocation decisions based on scientific merit or on institutional prestige that reinforces itself through preferential access?',
    'Analysis of approved proposals grouped by institution type; tracking citation impact and significance of research from elite vs non-elite institutions; blind review trials comparing allocation outcomes',
    'If merit-driven: constraint is legitimate coordination (Rope from all perspectives). If prestige-driven: constraint is extractive asymmetry (Snare from early-career perspective). If circular: institution''s past success generates preferential access that ensures future success (reinforcement loop).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(merit_versus_prestige_circularity, empirical, 'Whether allocation reflects scientific merit or self-reinforcing institutional prestige').

omega_variable(
    alternative_funding_sufficiency,
    'Do alternative funding models (crowdfunding, distributed networks, citizen science) provide adequate resolution, or is large observatory access functionally irreplaceable for certain research domains?',
    'Comparative analysis of research domains: identification of questions that require large observatory access vs those solvable through alternatives; cost-effectiveness metrics for alternative approaches',
    'If alternatives sufficient: scaffold sunset is realistic and constraint is temporary. If irreplaceable: alternatives address only subset of research, maintaining extraction for excluded domains.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_funding_sufficiency, empirical, 'Whether alternative funding models can replace large observatory access').

omega_variable(
    suppression_mechanism_structural_versus_cognitive,
    'Is suppression of alternative access primarily structural (no competing funding sources) or partially internalized (researchers accept prestige hierarchy as natural)?',
    'Post-allocation interviews; tracking of funding applications to non-elite institutions; analysis of researcher narratives about resource constraints vs hierarchical acceptance',
    'If structural: high-cost exit (constrained) but mechanistically clear. If internalized: lower nominal suppression but higher effective binding because agents carry hierarchy internalization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_versus_cognitive, empirical, 'Whether suppression is structural or partly internalized').

omega_variable(
    geographic_versus_institutional_bottleneck,
    'Is the primary bottleneck geographic (few large observatories exist) or institutional (allocation processes favor certain institutions)?',
    'Comparison of access patterns at geographically distributed observatories of equal capability; analysis of whether institutional prestige or location determines allocation',
    'If geographic: constraint is closer to Mountain (physical limit). If institutional: constraint is closer to Snare (engineered extraction). Affects long-term classification stability.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(geographic_versus_institutional_bottleneck, empirical, 'Whether bottleneck is geographic scarcity or institutional allocation bias').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(observatory_funding_allocation, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(obsfund_tr_t0, observatory_funding_allocation, theater_ratio, 0, 0.55).
narrative_ontology:measurement(obsfund_tr_t10, observatory_funding_allocation, theater_ratio, 10, 0.62).
narrative_ontology:measurement(obsfund_tr_t20, observatory_funding_allocation, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(obsfund_be_t0, observatory_funding_allocation, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(obsfund_be_t10, observatory_funding_allocation, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(obsfund_be_t20, observatory_funding_allocation, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(observatory_funding_allocation, resource_allocation).
narrative_ontology:affects_constraint(observatory_funding_allocation, peer_review_prestige_bias).
narrative_ontology:affects_constraint(observatory_funding_allocation, scientific_career_stratification).

% DUAL FORMULATION NOTE:
% Observatory funding allocation is upstream of specific proposal decisions but represents a distinct structural constraint. Downstream constraints (peer review bias in allocation, career stratification in astronomy) inherit extractiveness patterns from this parent constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
