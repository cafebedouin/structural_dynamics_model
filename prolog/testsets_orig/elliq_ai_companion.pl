% ============================================================================
% CONSTRAINT STORY: elliq_ai_companion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_elliq_ai_companion, []).

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
 *   constraint_id: elliq_ai_companion
 *   human_readable: State-Funded AI Companionship for Seniors
 *   domain: technological/social_policy
 *
 * SUMMARY:
 *   State-funded deployment of AI companions (exemplified by ElliQ) to combat
 *   elderly loneliness represents a policy solution that exhibits genuine
 *   coordination benefits alongside significant extractive mechanisms. The
 *   program addresses a real problem — seniors experiencing severe social
 *   isolation with high documented health costs — yet does so in a way that
 *   substitutes technological provision for the structural investment in
 *   intergenerational care, community infrastructure, and family caregiving
 *   redesign. The constraint's extractiveness has increased over the
 *   program's deployment period (0.28 → 0.52) as initial hopes that robots
 *   would supplement community care have given way to evidence that they
 *   substitute for it, displacing volunteer programs and family visits while
 *   creating vendor lock-in. The theater ratio (0.64) reflects the program's
 *   performative dimension: it generates visible political credit for
 *   'addressing elderly loneliness' while leaving underlying causes (work
 *   structures that prevent intergenerational co-residence, underfunded
 *   community infrastructure, family geographic fragmentation) unaddressed.
 *   The constraint demonstrates why tangled rope classifications are
 *   necessary: the program genuinely helps isolated seniors AND extracts from
 *   intergenerational care labor, family agency, and community
 *   infrastructure. A snare-only or rope-only classification would miss the
 *   true structure.
 *
 * KEY AGENTS:
 *   - Isolated Seniors: Primary victims (powerless/trapped) — experience genuine loneliness relief but at cost of normalizing outsourced companionship and reducing human connection opportunities
 *   - Adult Children: Secondary agents (moderate/constrained) — benefit from reduced caregiving burden and guilt but trapped in moral hazard where program reduces pressure for family investment
 *   - Technology Vendor (ElliQ Producer): Primary beneficiary (institutional/arbitrage) — receives stable state contracts and market access; experiences constraint as favorable coordination
 *   - Senior Community Organizations: Tertiary agents (organized/constrained) — displaced by robotics programs despite complementary role; suppressed through political attention shifting to tech solutions
 *   - State Health Policy Apparatus: Institutional actor (institutional/arbitrage) — benefits from political visibility of addressing elderly loneliness without fiscal commitment to systemic care redesign
 *   - Intergenerational Care Practice: Abstract victim (powerless/trapped) — social fabric of reciprocal elder care is eroded as technology substitution becomes normalized; no mechanism for exit or defense
 *   - Analytical Observer: Universal perspective (analytical/analytical) — risks naturalizing policy choice as technological necessity (false mountain)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(elliq_ai_companion, 0.52).
domain_priors:suppression_score(elliq_ai_companion, 0.68).
domain_priors:theater_ratio(elliq_ai_companion, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(elliq_ai_companion, extractiveness, 0.52).
narrative_ontology:constraint_metric(elliq_ai_companion, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(elliq_ai_companion, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(elliq_ai_companion, tangled_rope).
narrative_ontology:human_readable(elliq_ai_companion, "State-Funded AI Companionship for Seniors").
narrative_ontology:topic_domain(elliq_ai_companion, "technological/social_policy").

domain_priors:requires_active_enforcement(elliq_ai_companion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(elliq_ai_companion, technology_vendor).
narrative_ontology:constraint_beneficiary(elliq_ai_companion, state_health_bureaucracy).
narrative_ontology:constraint_beneficiary(elliq_ai_companion, adult_children_of_beneficiaries).
narrative_ontology:constraint_victim(elliq_ai_companion, senior_social_autonomy).
narrative_ontology:constraint_victim(elliq_ai_companion, intergenerational_care_labor).
narrative_ontology:constraint_victim(elliq_ai_companion, elder_community_infrastructure).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE ISOLATED SENIOR (SNARE) — Trapped by mobility limitations, death of peers, family geographic dispersion, and economic constraints on care alternatives. The AI companion fills a genuine void but at the cost of normalizing human connection as outsourced commodity. No exit option: cannot afford private care, cannot rebuild social networks from isolation, cannot refuse the program without admitting the depth of abandonment. Suppression is total — the senior experiences the constraint as care provision but cannot exit the dependency it creates.
constraint_indexing:constraint_classification(elliq_ai_companion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ADULT CHILDREN / CAREGIVING PROXY (TANGLED ROPE) — Moderate power; constrained by geography, work obligations, and the emotional weight of their parent's isolation. The AI companion solves their coordination problem (monitoring parent, ensuring engagement without guilt) AND extracts from them by substituting their labor. They benefit from reduced caregiving burden and real-time monitoring, but are also trapped in the moral hazard: the program reduces pressure on families to visit, restructure work, or rebuild community care. Active enforcement maintains this split — if the program were optional, families would reveal their true preference (human care when affordable, but not when outsourced care is available for free).
constraint_indexing:constraint_classification(elliq_ai_companion, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: TECHNOLOGY VENDOR (ROPE) — Experiences the state program as pure coordination: the vendor solves the state's legitimacy problem (demonstrating action on elderly loneliness) in exchange for stable contract revenue. From the vendor's perspective, the constraint is the successful alignment of state interest (appearing to address elder isolation) with market capability (deploying existing robots at scale). This is textbook rope — low extraction experience because both parties get what they want.
constraint_indexing:constraint_classification(elliq_ai_companion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SENIOR COMMUNITY ORGANIZATIONS (TANGLED ROPE) — Organized agents (senior centers, meal programs, volunteer visitor networks) are displaced by the program. They see both coordination (the robot supplements their work, reaches isolated seniors they cannot) and extraction (funding, volunteer recruitment, and political attention shift to the tech solution). They are constrained in exit options — they cannot refuse the program without being seen as opposing elder welfare, but they also cannot compete with free state-subsidized robots. Their suppression is high: the program redefines their mission from community-building to logistics, reducing their agency to referral agents.
constraint_indexing:constraint_classification(elliq_ai_companion, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: STATE HEALTH POLICY APPARATUS (PITON) — The state sees the AI companion program as solving the elder isolation problem while maintaining fiscal containment. But the theater ratio is high (0.64): the program is largely performative. It satisfies political demand to 'do something about elderly loneliness' without addressing root causes (family geographic fragmentation, work structures that prevent intergenerational care, community infrastructure decay). The policy persists through institutional inertia and the low cost relative to actual care systems (home health aides, community centers, housing that co-locates ages). Once deployed, the robot program is difficult to terminate politically even as its effectiveness degrades — termination signals admission of failure. Theater ratio increases over time as the program matures and the perception gap (robot solves loneliness vs. robot substitutes for human care) widens.
constraint_indexing:constraint_classification(elliq_ai_companion, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: INTERGENERATIONAL CARE AS SOCIAL PRACTICE (SNARE) — The abstract social good of intergenerational reciprocity and peer-based elder community is degraded by the program. Once the robot normalizes outsourced companionship, rebuilding human-based care systems becomes politically difficult — the state's investment in robots creates path dependency. Younger generations who grow up in a world where elder companionship is technological learn to expect automation rather than participate in community care. This abstract victim (the social fabric itself) cannot exit, organize, or negotiate. The constraint extracts from human social practice in favor of technological provision.
constraint_indexing:constraint_classification(elliq_ai_companion, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal analytical perspective, there may be an immutable constraint: human connection cannot be fully substituted by technology; AI companions can reduce acute loneliness but cannot satisfy the deep structural human need for mutual care and recognition. This perspective risks false-summit naturalization — what appears as a law of human nature (loneliness is ineradicable) may instead be a contingent institutional failure (we have chosen not to structure work and community to enable intergenerational care). The mountain claim requires verification that the constraint is truly inherent rather than policy-contingent.
constraint_indexing:constraint_classification(elliq_ai_companion, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(elliq_ai_companion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(elliq_ai_companion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(elliq_ai_companion, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(elliq_ai_companion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(elliq_ai_companion, TR),
    TR >= 0.70.

:- end_tests(elliq_ai_companion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): High-moderate. The program extracts from multiple sources: (1) intergenerational care labor is outsourced to the state-vendor partnership, reducing adult children's participation and family reciprocity; (2) community infrastructure is displaced by individual technological provision, extracting from senior organizations and volunteer networks; (3) the state's fiscal commitment to robotics creates path dependency that locks out investment in community alternatives. Initial extractiveness (0.28) was low because early deployment was genuinely supplementary — robots filled gaps where human care was unavailable. Current extractiveness (0.52) reflects crowding out and substitution mechanisms revealed by operational data. Suppression (0.68): High. Seniors cannot exit without admitting abandonment; families cannot refuse without appearing to neglect parents; vendors cannot be dislodged once deployed; community organizations cannot openly oppose without being labeled anti-elderly-welfare. The program suppresses alternatives through political lock-in (appearing to solve the problem removes pressure for systemic solutions) and technical lock-in (switching costs for both seniors and states). Theater ratio (0.64): Moderate-high. The program generates significant political credit for action on elderly isolation while the mechanism (technological companionship) is only weakly effective at addressing root causes. Theater has increased as gap between program visibility and outcome effectiveness has widened. Initial theater (0.35) reflected genuine uncertainty about effectiveness; current theater (0.64) reflects clearer evidence that robots supplement rather than replace human care.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary and victim perspectives on this constraint diverge maximally. The technology vendor (rope) experiences alignment between their interests and the state's stated goals — they provide what the state says it wants. The isolated senior (snare) experiences both genuine benefit (companionship fills a void) and extraction (normalization of outsourced care creates dependency and prevents rebuilding human networks). Adult children (tangled rope) experience coordination gain (peace of mind, monitoring, reduced guilt) alongside extraction (reduced family obligation and moral pressure to restructure work for caregiving). Community organizations (tangled rope) experience both — the robot supplements their reach AND displaces their funding and mission. The state (piton) sees this as successfully addressing a problem while actually maintaining the institutional status quo that created the problem. The intergenerational care practice (snare) has no advocate in the policy process. The perspectival gap is structural: the program benefits those with institutional power or market position while extracting from the abstract goods (social care practice, family reciprocity, community infrastructure) that lack political voice.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim declarations and exit options. The technology vendor (institutional/arbitrage) derives d ≈ 0.05 → f(d) ≈ -0.12 (beneficiary with escape options experiences negative/zero extraction). The isolated senior (powerless/trapped) derives d ≈ 0.95 → f(d) ≈ 1.42 (maximum extraction for those with no exit). Adult children (moderate/constrained) derive d ≈ 0.60 → f(d) ≈ 0.85 (significant extraction but with some agency). Community organizations (organized/constrained) derive d ≈ 0.58 → f(d) ≈ 0.80 (similar to families; constrained by political context). The state apparatus (institutional/arbitrage) derives d ≈ 0.15 → f(d) ≈ -0.01 (net beneficiary). Intergenerational care (powerless/trapped) derives d ≈ 1.0 → f(d) ≈ 1.42 (maximum, as it has no representation). The directionality pipeline correctly surfaces that what appears as a win-win for technology vendor and state is actually a compound extraction from multiple victim categories with high suppression preventing exit or renegotiation.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled rope classification resolves the potential mandatrophy by identifying that the program has BOTH genuine coordination function AND asymmetric extraction. Coordination: the robot genuinely helps isolated seniors and solves the state's visibility problem. Extraction: the program substitutes for systemic investment in community care, extracts from family labor, displaces community organizations, and creates vendor lock-in. Without the tangled rope category, an analyst might classify this as rope (emphasizing coordination benefit) or snare (emphasizing extraction), missing that both are structurally true. The piton perspective is important: the state's review apparatus sees the program as increasingly performative (theater ratio 0.64) rather than functionally adequate, yet political lock-in prevents termination. The false mountain perspective (technology substitution is a natural limit) is explicit and marked as a risk — the constraint is policy-contingent, not inherent. The mandatrophy is resolved by the perspectival architecture: each observer sees a legitimate classification from their structural position, and the system surface the true structure (tangled coordination-extraction hybrid with high suppression) rather than collapsing to a single type.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    emotional_substitution_capacity,
    'Can an AI companion provide sufficient psychological benefit to reduce measurable loneliness and depression in seniors, or does it provide only performative comfort that decays as seniors recognize its non-reciprocal nature?',
    'Longitudinal clinical outcome studies (PHQ-9, UCLA Loneliness Scale) comparing seniors with AI companions to matched controls; qualitative interviews tracking perception of relationship authenticity over 12-24 months',
    'If genuine benefit: program may be justified despite theater (snare perspective weakens). If performative only: program is pure extraction masked as care, and suppression is justified (snare perspective strengthens).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emotional_substitution_capacity, empirical, 'Whether AI companions provide genuine emotional benefit or performative comfort').

omega_variable(
    family_substitution_rate,
    'Does the availability of free AI companions reduce adult children''s visit frequency, time investment, and care participation compared to counterfactual (no program)?',
    'Pre-post comparison of visit frequency and care-related communication in treatment vs. control groups; propensity-matched analysis controlling for initial isolation severity',
    'If high substitution (visits drop >30%): the program extracts from intergenerational care labor and creates moral hazard (tangled rope perspective confirmed). If low substitution (visits stable): program supplements rather than replaces human care (rope perspective strengthens).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(family_substitution_rate, empirical, 'Whether AI companions reduce human family visits and care').

omega_variable(
    community_infrastructure_decay,
    'Does state investment in individual AI companions crowd out funding for community-based alternatives (senior centers, volunteer visiting programs, age-integrated housing) that could address isolation at lower theater cost?',
    'Budget allocation analysis; comparison of spending trajectory on robotics vs. community infrastructure over 10-year period; assessment of political feasibility of scaling community programs if robotics budget were reallocated',
    'If crowding out occurs: program is extractive from community infrastructure and intergenerational practice (snare on social practice perspective confirmed). If complementary: program fills gaps that community infrastructure cannot reach (rope perspective stronger).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(community_infrastructure_decay, empirical, 'Whether robot programs crowd out community-based elder care alternatives').

omega_variable(
    technology_vendor_dependence,
    'Once seniors become habituated to AI companions, is the state locked into ongoing vendor contracts due to switching costs and political infeasibility of transition, creating rent-seeking extraction by the vendor?',
    'Contract analysis of termination clauses, switching costs, and vendor pricing power over contract renewals; political economy assessment of vendor lobbying and program defense',
    'If locked in: the ''rope'' experience for the vendor becomes rope + opportunistic extraction as vendor price-discriminates and introduces behavioral lock-in (tangled rope for vendor, snare for state budget). If competitive alternatives exist: rope perspective holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_vendor_dependence, empirical, 'Whether vendor dependence creates lock-in and rent-seeking behavior').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(elliq_ai_companion, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(elliq_tr_t0, elliq_ai_companion, theater_ratio, 0, 0.35).
narrative_ontology:measurement(elliq_tr_t3, elliq_ai_companion, theater_ratio, 3, 0.5).
narrative_ontology:measurement(elliq_tr_t6, elliq_ai_companion, theater_ratio, 6, 0.64).

% Extraction over time
narrative_ontology:measurement(elliq_be_t0, elliq_ai_companion, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(elliq_be_t3, elliq_ai_companion, base_extractiveness, 3, 0.38).
narrative_ontology:measurement(elliq_be_t6, elliq_ai_companion, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(elliq_ai_companion, resource_allocation).
narrative_ontology:affects_constraint(elliq_ai_companion, family_caregiving_outsourcing).
narrative_ontology:affects_constraint(elliq_ai_companion, community_elder_infrastructure_decay).
narrative_ontology:affects_constraint(elliq_ai_companion, senior_tech_dependence).

% DUAL FORMULATION NOTE:
% The AI companion constraint is downstream of broader policy decisions around family structure and work-life integration. It is upstream of vendor lock-in dynamics. The ε-invariance principle: if measured as 'does the robot improve senior wellbeing?', extractiveness ≈ 0.20 (mountain-like). If measured as 'does the program substitute for systemic care investment?', extractiveness ≈ 0.60 (snare-like). These are distinct constraints. This story measures the POLICY MECHANISM (substitution and extraction), not the TECHNICAL OUTCOME (whether robots help). Related constraint stories should decompose vendor lock-in (ε ≈ 0.45) and family caregiving displacement (ε ≈ 0.55) separately.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(elliq_ai_companion, powerless, 0.95).
constraint_indexing:directionality_override(elliq_ai_companion, moderate, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
