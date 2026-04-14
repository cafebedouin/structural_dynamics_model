% ============================================================================
% CONSTRAINT STORY: dead_sea_effect
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dead_sea_effect, []).

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
 *   constraint_id: dead_sea_effect
 *   human_readable: The Dead Sea Effect (Talent Evaporation)
 *   domain: social/economic
 *
 * SUMMARY:
 *   The Dead Sea Effect occurs in organizations when the most talented and
 *   mobile individuals leave because they have superior exit options, while
 *   less talented or trapped individuals remain. This creates a vicious
 *   cycle: as talent departs, the organization's quality declines, making
 *   remaining talent more likely to leave, accelerating the evaporation
 *   process. The constraint exhibits the full spectrum of DR classifications
 *   depending on the observer's structural position. For trapped workers, it
 *   is pure extraction (Snare): they are forced to remain in an increasingly
 *   dysfunctional environment. For moderately talented but constrained
 *   workers, it is mixed extraction and coordination (Tangled Rope): they
 *   benefit from organizational infrastructure but are extracted from via
 *   career stagnation as promotion pathways clog. For highly talented mobile
 *   workers, it appears as pure coordination (Rope): the organization is a
 *   training ground and network hub with no extraction cost. For HR functions
 *   implementing retention policies, it appears as a temporary coordination
 *   problem with a sunset (Scaffold): reskilling and career development can
 *   reverse the dynamic. For legacy organizational hierarchies, it appears as
 *   degraded performance maintained by inertia (Piton): the tenure-protected
 *   management structure no longer serves mentoring functions but persists
 *   bureaucratically. From a natural law perspective, it appears inevitable
 *   (Mountain): in any system with heterogeneous mobility, the most mobile
 *   depart first. The constraint's extractiveness increases over time (0.28 →
 *   0.52) as talent loss accelerates, while theater increases modestly (0.18
 *   → 0.35) as organizations compensate with performative retention
 *   mechanisms that lack real career advancement function.
 *
 * KEY AGENTS:
 *   - High-Talent, High-Mobility Expert: Primary beneficiary (organized/arbitrage) — captures training and network value; experiences constraint as pure coordination enabling their exit
 *   - Organizational Management: Primary beneficiary (institutional/constrained) — maintains hierarchical structure and promotes chosen successors; extracts rent through delayed advancement of threatening talent
 *   - Low-Talent, Low-Mobility Worker: Primary victim (powerless/trapped) — trapped by lack of alternatives; faces degrading environment and loss of mentorship; bears full extraction cost
 *   - Mid-Tier Talent: Secondary victim (moderate/constrained) — constrained by risk and obligations; experiences mixed extraction (stagnation) and coordination (infrastructure) benefits
 *   - Human Resources Function: Tertiary actor (institutional/constrained) — implements retention policies with implicit sunset; sees the effect as solvable through career development
 *   - External Labor Market: Institutional beneficiary (powerful/mobile) — absorbs trained talent; benefits from information signal of organizational departure
 *   - Organizational Mission/Identity: Structural victim (powerless/analytical) — no exit option; bears cost of talent-dependent functions (innovation, mentorship, quality) degrading over time
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dead_sea_effect, 0.52).
domain_priors:suppression_score(dead_sea_effect, 0.48).
domain_priors:theater_ratio(dead_sea_effect, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dead_sea_effect, extractiveness, 0.52).
narrative_ontology:constraint_metric(dead_sea_effect, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(dead_sea_effect, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dead_sea_effect, tangled_rope).
narrative_ontology:human_readable(dead_sea_effect, "The Dead Sea Effect (Talent Evaporation)").
narrative_ontology:topic_domain(dead_sea_effect, "social/economic").

domain_priors:requires_active_enforcement(dead_sea_effect).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dead_sea_effect, organizational_management).
narrative_ontology:constraint_beneficiary(dead_sea_effect, tenure_protected_cohort).
narrative_ontology:constraint_victim(dead_sea_effect, high_talent_pool).
narrative_ontology:constraint_victim(dead_sea_effect, organizational_capability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-TALENT, LOW-MOBILITY WORKER (SNARE) — Trapped in the organization by lack of alternative opportunities. Faces a degrading organizational environment as talent evaporates. Cannot exit; bears full extraction cost: declining mentorship, reduced innovation, diminishing career capital. Experiences pure extraction with minimal coordination benefit.
constraint_indexing:constraint_classification(dead_sea_effect, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: MID-TIER TALENT (TANGLED ROPE) — Moderately mobile; has some exit options but constrained by risk aversion, family obligations, or regional labor market thinness. Benefits from the organization's infrastructure and mentorship from senior talent (while it lasts). Extracted from via career stagnation as promotion pathways clog. Mixed experience: genuine coordination benefit plus significant extraction.
constraint_indexing:constraint_classification(dead_sea_effect, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: HIGH-TALENT, HIGH-MOBILITY EXPERT (ROPE) — Highly mobile with abundant exit options (multiple job offers, consulting, startup founding, geographic arbitrage). Experiences the organization as pure coordination: learns skills, builds networks, accumulates credentials. The constraint is a mechanism that enables this agent's mobility — the organization functions as a training ground. Extraction runs toward the organization (in the form of lost talent); this agent experiences only coordination benefit.
constraint_indexing:constraint_classification(dead_sea_effect, rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: HUMAN RESOURCES FUNCTION (SCAFFOLD) — Institutional actors implementing retention policies, reskilling programs, and organizational restructuring with an implicit sunset: if labor market tightens or organizational culture improves, the need for aggressive retention mechanisms declines. HR sees the effect as a temporary coordination problem solvable through mentorship culture, clear promotion pathways, and transparent advancement rules. Suppression is moderate because the mechanisms (career development, competitive pay) have genuine coordination functions. Theater is lower than in snare perspectives because actual career mobility is a real mechanism, not purely performative.
constraint_indexing:constraint_classification(dead_sea_effect, scaffold,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LEGACY ORGANIZATIONAL STRUCTURE (PITON) — The tenure-protected management hierarchy persists despite functional atrophy. Senior leaders remain in place by institutional inertia (vested benefits, social integration, path dependence of organizational knowledge). The structure no longer serves its original coordination function (mentoring junior talent, maintaining institutional memory) because talented juniors leave before absorbing that knowledge. The hierarchy is maintained performatively through titles, committees, and ceremonial advancement. Theater ratio high because organizational charts and succession planning are largely theatrical when the underlying talent pool is evaporating.
constraint_indexing:constraint_classification(dead_sea_effect, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: EXTERNAL LABOR MARKET (TANGLED ROPE) — Powerful actor (global talent markets, venture capital, competing firms) with high mobility. Benefits from the Dead Sea Effect by acquiring trained, proven talent at market rates. Also constrained by information asymmetry: the decision to leave an organization signals quality, but the organization's hiring process extracts information rent from that signal. The talent flight creates a coordination problem for firms recruiting from degraded organizations (how to distinguish signal quality). Mixed extraction and coordination from the market perspective.
constraint_indexing:constraint_classification(dead_sea_effect, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational timescale, the Dead Sea Effect is an inevitable consequence of differential mobility under heterogeneous talent. Talent, by definition, has more exit options. Organizations with exit barriers (vesting cliffs, geographic monoculture, skill specificity) experience differential loss. This perspective sees the effect as a structural law: in any system with heterogeneous mobility, the most mobile members leave first — a consequence of basic selection mechanics, not contingent policy. However, this natural law reading obscures the institutional contingencies (vesting schedules, promotion politics, knowledge hoarding by senior staff) that determine the effect's severity.
constraint_indexing:constraint_classification(dead_sea_effect, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dead_sea_effect_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(dead_sea_effect, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(dead_sea_effect, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(dead_sea_effect, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(dead_sea_effect, TR),
    TR >= 0.70.

:- end_tests(dead_sea_effect_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, rising over the interval. The constraint extracts from trapped and constrained workers via organizational decay, while benefiting mobile workers and management through talent concentration and selection. The extractiveness is not maximal (≥0.70 snare-level) because high-talent workers have genuine exit options that provide real agency, and the coordination benefits to HR (mentoring infrastructure, knowledge transfer systems) are partially real. Suppression (0.48): Moderate. Vesting schedules, geographic concentration, skill specificity, and career path opacity create barriers to mobility, but these are not absolute — talented workers regularly overcome them. Career risk of lateral moves and information asymmetry in outside job searches add suppression. Theater (0.35): Low-moderate, rising modestly. Retention programs, career development initiatives, and succession planning have real mechanisms (promotions, raises, training budgets) that are not purely theatrical, but their actual effectiveness is limited because the underlying talent exodus is driven by external opportunity, not internal dissatisfaction with specific procedures. As the effect progresses, organizational announcements of 'renewed commitment to talent development' become increasingly theatrical relative to actual outcomes.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. The trapped low-talent worker perceives a Snare (pure extraction, no exit, maximum cost). The external labor market perceives a Rope (pure coordination, they gain trained talent and network effects). The organizational management perceives a Rope or minor constraint (they maintain control, promote chosen successors, benefit from talent selection). The mid-tier constrained talent perceives a Tangled Rope (genuine career development infrastructure offset by promotion stagnation and extraction of effort). The HR function perceives a Scaffold (a temporary problem solvable through policy). The legacy organizational hierarchy perceives a Piton (their own functions are degraded but persists through inertia). The analytical observer risks a Mountain (inevitable selection dynamics). The perspectival gap arises entirely from differential exit options and structural positioning relative to the extraction flow — the same organizational phenomenon is coordination for those who can leave and extraction for those who cannot.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) derive from each agent's structural relationship to the extraction flow. Trapped workers with no exit options have d ≈ 0.95 (maximum target): they absorb all extraction costs and cannot exit. Moderately mobile constrained workers have d ≈ 0.55: their career stagnation imposes costs, but organizational benefits (infrastructure, networks) provide partial offset. Highly mobile experts have d ≈ 0.10 (near beneficiary): the organization benefits them via training/networks while they extract institutional knowledge on departure. Institutional management has d ≈ 0.15 (beneficiary): they benefit from talent concentration, hierarchy maintenance, and selective advancement of chosen successors. External labor markets have d ≈ 0.25: they benefit from the signal quality of departing talent and absorb training value created by the sending organization. The engine's sigmoid f(d) amplifies the experienced extractiveness for trapped workers and dampens it for mobile beneficiaries, producing the perspectival gaps observed above.
 *
 * MANDATROPHY ANALYSIS:
 *   The Dead Sea Effect resolves the mandatrophy by showing that classification depends entirely on the agent's exit options, not on the organization's objective characteristics. No agent is mistaken about what they observe — each perception is structurally accurate from their position. The organization is simultaneously a training ground (Rope for talented exiters), a trap (Snare for trapped workers), a mixed opportunity-and-stagnation system (Tangled Rope for constrained workers), a temporary coordination problem (Scaffold for HR), and a degraded bureaucracy (Piton for legacy management). The mandatrophy is resolved by accepting that a single organizational constraint can be all six types simultaneously, each legitimately observed from a different structural vantage point. The false natural law claim (Mountain) — that talent evaporation is inevitable — obscures the institutional contingencies (vesting schedules, promotion politics, knowledge hoarding) that determine severity. Organizations that implement transparent advancement, knowledge transfer systems, and flexible mobility options shift more perspectives toward Rope/Scaffold and away from Snare/Piton, confirming that the effect is contingent on policy, not a law of nature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    organizational_skill_complementarity,
    'How much of the Dead Sea Effect is driven by genuine complementarity between senior and junior talent versus organizational hoarding of tacit knowledge by entrenched staff?',
    'Comparative analysis of organizations with explicit knowledge transfer systems (documentation, structured mentoring, apprenticeships) versus those relying on osmotic absorption. Measure talent retention rates and time-to-productivity for junior hires.',
    'If complementarity dominant: the effect is a coordination problem (Scaffold perspective confirmed). If hoarding dominant: the effect is extractive (Snare/Tangled Rope confirmed). Affects policy response and organizational structure redesign.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(organizational_skill_complementarity, empirical, 'Skill complementarity versus knowledge hoarding as cause of talent loss').

omega_variable(
    external_market_selection,
    'To what extent does the external labor market''s willingness to absorb departing talent represent genuine opportunity creation versus a parasitic extraction of training investment?',
    'Longitudinal tracking of departure cohorts: career outcomes, income trajectory, skill utilization for those who leave versus those who stay. Attribution of human capital gains to sending organization versus receiver organization.',
    'If opportunity creation: the external market perspective is genuine coordination (Rope). If parasitic: the external market is extracting training value created by the organization. Affects interpretation of whether talent flight is healthy churn or organizational degradation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(external_market_selection, empirical, 'Whether external market opportunities are creation or extraction of organizational training value').

omega_variable(
    vesting_schedule_optimality,
    'Are standard vesting schedules (4-year cliffs, annual vesting) structurally extractive barriers to mobility, or do they serve a genuine coordination function (preventing free-riding on training investment)?',
    'Experimental variation: compare organizations with cliff vesting versus continuous vesting versus no vesting. Measure talent retention, organizational stability, and quality of junior advancement. Survey departing talent on vesting impact in exit decision.',
    'If genuine coordination: vesting is justified (Rope perspective). If extractive barrier: vesting is suppression mechanism that exacerbates the effect by trapping low-talent workers and creating resentment that accelerates high-talent exit (Snare perspective). Policy implication: change vesting structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(vesting_schedule_optimality, empirical, 'Whether vesting schedules are extractive barriers or justified coordination mechanisms').

omega_variable(
    organizational_identity_preservation,
    'When organizational identity is explicitly tied to retaining a specific cohort (founding team, generation of leaders), does the Dead Sea Effect represent organizational renewal or identity death?',
    'Case study comparison: organizations that successfully rebuilt around new talent versus those that experienced declining identity and mission drift. Interviews with organizational historians and members across departure cohorts.',
    'If renewal: the effect is a healthy adaptation (Scaffold with genuine sunset). If identity death: the organization is experiencing core mission extraction (Snare from the organizational mission perspective). Affects interpretation of whether to fight the effect or accept it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(organizational_identity_preservation, conceptual, 'Whether talent evaporation represents organizational renewal or identity degradation').

omega_variable(
    selection_sorting_equilibrium,
    'Is there a stable equilibrium where organizations accept gradual talent loss and specialize in lower-skill work, or do all organizations face extinction-level Dead Sea dynamics?',
    'Longitudinal organizational studies: track firms/institutions over 20+ years. Identify survivors that stabilized with lower talent tiers versus those in terminal decline. Characterize the niche positions of survivors.',
    'If stable niches exist: the effect is a Scaffold (transition to new equilibrium possible). If extinction universal: the effect is a Snare (no escape). Affects whether the constraint is avoidable or inevitable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(selection_sorting_equilibrium, empirical, 'Existence of stable organizational niches after talent evaporation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dead_sea_effect, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dse_tr_t0, dead_sea_effect, theater_ratio, 0, 0.18).
narrative_ontology:measurement(dse_tr_t5, dead_sea_effect, theater_ratio, 5, 0.27).
narrative_ontology:measurement(dse_tr_t10, dead_sea_effect, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(dse_be_t0, dead_sea_effect, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(dse_be_t5, dead_sea_effect, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(dse_be_t10, dead_sea_effect, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dead_sea_effect, resource_allocation).
narrative_ontology:affects_constraint(dead_sea_effect, organizational_knowledge_hoarding).
narrative_ontology:affects_constraint(dead_sea_effect, vesting_cliff_trap).
narrative_ontology:affects_constraint(dead_sea_effect, tenure_protection_lock).

% DUAL FORMULATION NOTE:
% The Dead Sea Effect is downstream of several specific institutional constraints: vesting schedules (which create suppression), tenure protection (which creates promotion bottlenecks), and knowledge hoarding (which reduces training value). This story models the emergent phenomenon; the upstream stories model the specific mechanisms. Organizations can reduce Dead Sea severity by addressing upstream constraints (restructuring vesting, transparent advancement, explicit knowledge transfer) while the Dead Sea phenomenon itself persists at the selection dynamics level.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dead_sea_effect, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
