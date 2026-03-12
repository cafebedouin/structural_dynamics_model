% ============================================================================
% CONSTRAINT STORY: character_revelation_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_character_revelation_asymmetry, []).

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
 *   constraint_id: character_revelation_asymmetry
 *   human_readable: Character Revelation Asymmetry in Social Assessment
 *   domain: moral_philosophy/social_psychology/virtue_ethics
 *
 * SUMMARY:
 *   Character revelation asymmetry describes the structural gap between the
 *   multi-context observation required for reliable character assessment and
 *   the single-context sampling most observers can access. Character is
 *   revealed differentially across contexts: crisis reveals courage or
 *   cowardice, power reveals integrity or corruption, time reveals
 *   consistency or volatility, intimacy reveals empathy or narcissism,
 *   scarcity reveals generosity or selfishness. But observers typically
 *   sample from only one or two contexts — the professional setting, the
 *   social performance, the curated public presentation. This creates an
 *   information asymmetry that benefits those who can maintain durable masks
 *   (context-specific personas that diverge from cross-context character) and
 *   extracts from those who must make assessments from incomplete data. The
 *   constraint has a genuine coordination function — social norms around
 *   character assessment enable trust formation and relationship initiation,
 *   solving the collective action problem of how to form relationships under
 *   uncertainty. But it also has an extraction function — the asymmetry
 *   enables sustained deception, misplaced trust, and resource transfer to
 *   those who control context presentation. The constraint is downstream of
 *   proximity-affinity conflation (the tendency to mistake proximity for
 *   affinity enables single-context assessment to feel sufficient) and
 *   affects relationship formation across all domains.
 *
 * KEY AGENTS:
 *   - Premature Assessors: Primary victims (powerless/trapped) — limited observational access, social pressure to judge quickly, bear full cost of misassessment with no structural capacity to demand multi-context observation
 *   - Relationship Investors: Secondary victims (moderate/constrained) — have resources to extend observation across some contexts but face real costs for extended vetting; benefit from coordination function but bear extraction from premature commitment
 *   - Impression Managers: Primary beneficiaries (institutional/arbitrage) — control context presentation, can curate which settings observers access, exit before multi-context revelation
 *   - Context Controllers: Secondary beneficiaries (powerful/mobile) — those with power to limit which contexts are observable (employers controlling work-life boundaries, public figures controlling media access, abusers isolating partners)
 *   - Distributed Reputation Networks: Organized agents (organized/mobile) — building multi-context aggregation mechanisms (professional references, social media history, community reputation tracking) with sunset logic
 *   - Formal Reference Systems: Institutional actors (institutional/arbitrage) — maintain theatrical vetting processes (reference letters, character witnesses) that persist through inertia despite low functional verification
 *   - Analytical Observer: Civilizational view (analytical/analytical) — recognizes both coordination function and asymmetric extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(character_revelation_asymmetry, 0.48).
domain_priors:suppression_score(character_revelation_asymmetry, 0.52).
domain_priors:theater_ratio(character_revelation_asymmetry, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(character_revelation_asymmetry, extractiveness, 0.48).
narrative_ontology:constraint_metric(character_revelation_asymmetry, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(character_revelation_asymmetry, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(character_revelation_asymmetry, tangled_rope).
narrative_ontology:human_readable(character_revelation_asymmetry, "Character Revelation Asymmetry in Social Assessment").
narrative_ontology:topic_domain(character_revelation_asymmetry, "moral_philosophy/social_psychology/virtue_ethics").

domain_priors:requires_active_enforcement(character_revelation_asymmetry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(character_revelation_asymmetry, those_with_durable_masks).
narrative_ontology:constraint_beneficiary(character_revelation_asymmetry, impression_managers).
narrative_ontology:constraint_beneficiary(character_revelation_asymmetry, context_controllers).
narrative_ontology:constraint_victim(character_revelation_asymmetry, those_making_premature_assessments).
narrative_ontology:constraint_victim(character_revelation_asymmetry, single_context_observers).
narrative_ontology:constraint_victim(character_revelation_asymmetry, relationship_investors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PREMATURE ASSESSOR (SNARE) — Trapped by limited observational access and social pressure to form judgments quickly. Bears full cost of misassessment (failed relationships, misplaced trust, resource loss) with no structural capacity to demand multi-context observation. The constraint extracts through asymmetric information access — those being assessed control context presentation while assessors face social costs for withholding judgment.
constraint_indexing:constraint_classification(character_revelation_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: RELATIONSHIP INVESTOR (TANGLED ROPE) — Has resources to extend observation across some contexts (can observe professional and social settings, can wait through some time horizons) but faces real costs for extended vetting (opportunity costs, social pressure to commit, risk of seeming untrusting). Benefits from the coordination function (social norms around character assessment enable relationship formation) but also bears extraction (premature commitment based on incomplete data). Constrained exit — can demand more contexts but at significant social cost.
constraint_indexing:constraint_classification(character_revelation_asymmetry, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: IMPRESSION MANAGER (ROPE) — Controls context presentation and benefits from single-context sampling. Can curate which settings observers access (professional polish, social charm, crisis avoidance) and exit relationships before multi-context revelation occurs. Experiences the constraint as coordination — social norms around character assessment are the mechanism through which they signal desirable traits. Net beneficiary with arbitrage exit — can move between social contexts and relationships before full character revelation.
constraint_indexing:constraint_classification(character_revelation_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: DISTRIBUTED REPUTATION NETWORK (SCAFFOLD) — Organized systems (professional reference checks, social media history, community reputation tracking, background verification services) are building multi-context aggregation mechanisms that reduce the asymmetry. These networks see the single-context assessment norm as a temporary coordination failure with a sunset — digital permanence and networked information sharing are creating persistent multi-context character records. Mobile exit because the network can choose which relationships to vet and which to skip. Estimated sunset: 15-25 years as digital reputation systems mature and social norms around privacy/transparency stabilize.
constraint_indexing:constraint_classification(character_revelation_asymmetry, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: FORMAL CHARACTER REFERENCE SYSTEM (PITON) — Traditional reference letters, character witnesses, and formal vetting processes are largely theatrical. References are selected by the subject (sampling bias), social norms prevent honest negative assessments (politeness suppression), and format constraints prevent context-specific detail. The ritual persists through institutional inertia (hiring processes, academic admissions, legal proceedings still require references) despite low functional verification. High theater ratio — the process signals diligence without producing multi-context revelation.
constraint_indexing:constraint_classification(character_revelation_asymmetry, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — Recognizes both the genuine coordination function (social norms around character assessment enable trust formation and relationship initiation, solving a real collective action problem) and the asymmetric extraction (those who control context presentation extract from those who must assess from limited samples). The asymmetry is not a natural law — it is a contingent feature of information access, social norms around privacy and judgment speed, and power differentials in relationship formation. The constraint coordinates (enables social trust) and extracts (enables durable deception).
constraint_indexing:constraint_classification(character_revelation_asymmetry, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(character_revelation_asymmetry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(character_revelation_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(character_revelation_asymmetry, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(character_revelation_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(character_revelation_asymmetry, TR),
    TR >= 0.70.

:- end_tests(character_revelation_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The asymmetry enables significant extraction — those who can maintain durable masks across contexts capture trust, resources, and relationship investment from those who assess from limited samples. But extraction is not maximal because some observers can and do demand multi-context observation, and some contexts (crisis, time) are difficult to fully control. The value reflects that the career, romantic, and social advantages of controlled presentation are substantial but not total. Suppression (0.52): Moderate-high. Significant barriers to multi-context observation include social norms against 'slow trust' (seeming paranoid or untrusting), opportunity costs of extended vetting (relationship market competition), privacy norms that limit context access, and power differentials that enable context controllers to restrict observation. But suppression is not total — some relationships do form through extended multi-context observation (long courtships, professional apprenticeships, community integration). Theater ratio (0.58): Moderate-high. Formal character reference systems are substantially performative — references are selected by the subject, social norms prevent honest negative assessments, format constraints prevent context-specific detail. The theater has increased over the interval as legal liability concerns and politeness norms have made honest negative references rarer. Distributed reputation networks (digital footprints, social media history) have lower theater but face their own manipulation (curated online personas, selective disclosure).
 *
 * PERSPECTIVAL GAP:
 *   The impression manager sees coordination (Rope) — social norms around character assessment are the mechanism through which they signal desirable traits and form relationships. The distributed reputation network sees a temporary problem with a sunset (Scaffold) — digital permanence and networked information are building multi-context aggregation that will reduce the asymmetry. The formal reference system sees its own degraded ritual (Piton) — the process persists through inertia despite low functional verification. The relationship investor sees mixed coordination and extraction (Tangled Rope) — the system both enables relationship formation and extracts through premature commitment. The premature assessor sees pure extraction (Snare) — limited observational access and social pressure to judge quickly create a trap with no exit. The analytical observer sees tangled rope — genuine coordination function (trust formation under uncertainty) intertwined with asymmetric extraction (durable deception enabled by single-context sampling). The gap reveals that 'character assessment norms' are experienced as natural and necessary by beneficiaries, as a solvable coordination problem by organized agents, as a degraded ritual by institutional actors, and as a trap by those with limited observational access.
 *
 * DIRECTIONALITY LOGIC:
 *   Premature assessors are victims with trapped exit — they face structural barriers to multi-context observation (limited access, social costs, time constraints) and bear the full cost of misassessment. High d, high f(d), high chi. Relationship investors are victims with constrained exit — they have resources to extend observation across some contexts but face real costs for doing so. Moderate-high d, moderate-high f(d), moderate-high chi. Impression managers are beneficiaries with arbitrage exit — they control context presentation and can exit relationships before multi-context revelation. Low d, low/negative f(d), low/negative chi. Context controllers are beneficiaries with mobile exit — they have power to limit which contexts are observable. Low-moderate d, low-moderate f(d), low-moderate chi. Distributed reputation networks are organized agents with mobile exit — they can choose which relationships to vet and have access to multi-context data. Moderate d (they benefit from coordination but also work to reduce extraction), moderate f(d), moderate chi. Formal reference systems are institutional actors with arbitrage exit — they maintain the theatrical process but are not bound by it. Low d, low f(d), low chi. The analytical observer recognizes both the coordination function (enabling trust formation) and the extraction function (enabling durable deception).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that character revelation asymmetry has both a genuine coordination function and an extraction function, and that the balance depends on the observer's structural position. The coordination function is real — social norms around character assessment enable trust formation and relationship initiation under uncertainty, solving a collective action problem. Without some mechanism for forming judgments from limited data, relationship formation would be prohibitively slow. But the extraction function is also real — the asymmetry between multi-context revelation and single-context sampling enables those who control context presentation to extract trust, resources, and relationship investment from those who must assess from incomplete data. The tangled rope classification captures this duality: the constraint coordinates (enables social trust) and extracts (enables durable deception). The perspectival gap shows that beneficiaries experience the coordination function (impression managers see social norms as enabling relationship formation) while victims experience the extraction function (premature assessors bear the cost of misassessment). The analytical observer recognizes both functions and sees that the asymmetry is not a natural law but a contingent feature of information access, social norms, and power differentials.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    context_sufficiency_threshold,
    'How many distinct contexts (crisis, power, time, intimacy, scarcity) are required for reliable character assessment?',
    'Longitudinal studies correlating number of observed contexts with prediction accuracy of future behavior; comparison of single-context vs multi-context assessment validity across relationship types',
    'If threshold is low (2-3 contexts): extraction is minimal, most assessors can achieve it. If threshold is high (6+ contexts): extraction is severe, asymmetry is structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(context_sufficiency_threshold, empirical, 'Number of contexts required for reliable character assessment').

omega_variable(
    mask_durability_distribution,
    'What proportion of the population can maintain durable masks (context-specific personas that diverge from cross-context character) across multiple contexts and time horizons?',
    'Psychological studies of impression management capacity, self-monitoring scales, longitudinal tracking of behavioral consistency across contexts; clinical data on personality disorders involving sustained deception',
    'If rare (< 5%): constraint is primarily coordination with edge-case extraction. If common (> 20%): constraint is widespread extraction mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mask_durability_distribution, empirical, 'Prevalence of durable mask capacity in population').

omega_variable(
    digital_permanence_effect,
    'Does digital permanence (social media history, searchable records, persistent reputation data) actually reduce character revelation asymmetry or merely shift it to those who control their digital presentation?',
    'Comparison of assessment accuracy in high-digital-footprint vs low-digital-footprint populations; analysis of whether digital records provide multi-context sampling or just more single-context data; measurement of digital impression management sophistication',
    'If reduces asymmetry: scaffold perspective confirmed, sunset is real. If shifts asymmetry: extraction mechanism adapts, beneficiaries change but asymmetry persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(digital_permanence_effect, empirical, 'Whether digital permanence reduces or shifts the asymmetry').

omega_variable(
    social_cost_of_extended_vetting,
    'What are the actual social and opportunity costs of demanding multi-context observation before relationship commitment?',
    'Ethnographic studies of relationship formation norms across cultures; measurement of social penalties for ''slow trust'' vs ''fast trust'' strategies; economic analysis of opportunity costs in professional and romantic relationship markets',
    'If costs are low: suppression is overstated, more observers could exit. If costs are high: suppression is structural, trapped classification is accurate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(social_cost_of_extended_vetting, empirical, 'Social and opportunity costs of extended vetting').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(character_revelation_asymmetry, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(char_rev_tr_t0, character_revelation_asymmetry, theater_ratio, 0, 0.4).
narrative_ontology:measurement(char_rev_tr_t10, character_revelation_asymmetry, theater_ratio, 10, 0.5).
narrative_ontology:measurement(char_rev_tr_t20, character_revelation_asymmetry, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(char_rev_be_t0, character_revelation_asymmetry, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(char_rev_be_t10, character_revelation_asymmetry, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(char_rev_be_t20, character_revelation_asymmetry, base_extractiveness, 20, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(character_revelation_asymmetry, identity_coordination).

% DUAL FORMULATION NOTE:
% Character revelation asymmetry is downstream of proximity_affinity_conflation (the tendency to mistake proximity for affinity makes single-context assessment feel sufficient, reducing demand for multi-context observation). The upstream constraint has its own extractiveness reflecting the cognitive bias; this constraint has its own extractiveness reflecting the information asymmetry and social norms around assessment speed.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
