% ============================================================================
% CONSTRAINT STORY: suanne_coup_of_peace
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_suanne_coup_of_peace, []).

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
 *   constraint_id: suanne_coup_of_peace
 *   human_readable: The Hostile Social Environment at the Lead Basketball Game
 *   domain: social/cultural
 *
 * SUMMARY:
 *   The Lead, South Dakota basketball game represents a structural constraint
 *   where a rural white community uses a sporting event as an instrument of
 *   racial dominance and humiliation against a visiting Lakota team from Pine
 *   Ridge. The hostile social environment — manifested in racial slurs,
 *   taunts, and systematic psychological harassment during the game —
 *   functions as an extraction mechanism that removes dignity, focus, and
 *   competitive advantage from the visiting team while reinforcing white
 *   community identity and status. This constraint exhibits the full range of
 *   DR classifications: the visiting team experiences pure extraction
 *   (Snare), the host community experiences coordination of social hierarchy
 *   (Rope), the state athletic association experiences mixed enforcement
 *   (Tangled Rope), advocacy groups see a solvable problem with legal and
 *   norm-shift remedies (Scaffold), and a civilizational observer might
 *   mistake the mechanism for an immutable feature of human tribal behavior
 *   (false Mountain). The extractiveness value (0.58) reflects that the
 *   hostile environment produces measurable performance degradation through
 *   psychological interference, not through formal rules or explicit coercion
 *   — the mechanism operates through unmanaged community behavior and
 *   selective enforcement of anti-discrimination policies. The suppression
 *   value (0.72) reflects high barriers to exit and limited alternatives: the
 *   team must complete the game, cannot effectively defend themselves against
 *   slurs, and lack institutional backing in a hostile jurisdiction.
 *
 * KEY AGENTS:
 *   - Pine Ridge Basketball Team: Primary victim (powerless/trapped) — faces maximum extraction through racial harassment with no exit option
 *   - Pine Ridge Community Supporters: Secondary victim (moderate/constrained) — travel to game, face direct insult, cannot leave without abandoning their team
 *   - Lead High School / Community Authority: Primary beneficiary (institutional/arbitrage) — benefits from status assertion and community cohesion; maintains hostile environment through selective non-enforcement
 *   - Lead White Community: Institutional beneficiary — uses the game as a venue for asserting racial dominance
 *   - South Dakota High School Athletic Association: Institutional actor (organized/constrained) — has anti-discrimination rules but weak enforcement in rural areas
 *   - Advocacy and Civil Rights Groups: Organized agents (organized/mobile) — see the constraint as solvable through policy enforcement and norm change
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the hostile environment as inevitable human tribal behavior
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(suanne_coup_of_peace, 0.58).
domain_priors:suppression_score(suanne_coup_of_peace, 0.72).
domain_priors:theater_ratio(suanne_coup_of_peace, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(suanne_coup_of_peace, extractiveness, 0.58).
narrative_ontology:constraint_metric(suanne_coup_of_peace, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(suanne_coup_of_peace, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(suanne_coup_of_peace, snare).
narrative_ontology:human_readable(suanne_coup_of_peace, "The Hostile Social Environment at the Lead Basketball Game").
narrative_ontology:topic_domain(suanne_coup_of_peace, "social/cultural").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(suanne_coup_of_peace, lead_community_white_residents).
narrative_ontology:constraint_victim(suanne_coup_of_peace, pine_ridge_basketball_team).
narrative_ontology:constraint_victim(suanne_coup_of_peace, lakota_dignity_and_agency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PINE RIDGE BASKETBALL TEAM (SNARE) — The visiting team is trapped: they must play the game (contractual/conference requirement), cannot leave the hostile environment without forfeiting, and experience maximum coercion through racial taunts, slurs, and psychological harassment. The constraint extracts focus, composure, and dignity. No alternative exit path exists — the game must be completed in the hostile venue.
constraint_indexing:constraint_classification(suanne_coup_of_peace, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: PINE RIDGE COMMUNITY SUPPORTERS (SNARE) — Family members and community supporters who travel to watch the game experience racial hostility while physically present in the lead arena. Exit options are constrained: leaving early abandons their team; staying means enduring harassment. The constraint extracts emotional labor and exposes them to direct insult. Suppression is high — they are outnumbered and lack institutional backing in a hostile jurisdiction.
constraint_indexing:constraint_classification(suanne_coup_of_peace, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 3: LEAD HIGH SCHOOL / COMMUNITY AUTHORITY (ROPE) — The host institution experiences the constraint as coordination of social hierarchy maintenance. The racial taunting reinforces community status boundaries. From the perspective of those managing the game, the hostile environment 'works' as intended: it signals dominance, maintains tribal (racial) cohesion among the Lead community, and establishes their arena as hostile territory. Enforcement is minimal — the environment self-perpetuates through social pressure and cultural norms. Exit options are arbitrage: they can suspend the game or police the crowd, but choose not to, because the extraction mechanism (humiliation of visitors) serves their social interests.
constraint_indexing:constraint_classification(suanne_coup_of_peace, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 4: SDHSAA / REGIONAL ATHLETIC BODY (TANGLED ROPE) — The state athletic association has explicit anti-discrimination rules but weak enforcement mechanisms in rural areas. They benefit from maintaining sports programs (coordination function) but extract compliance asymmetrically: enforcement is strict for schools with organized advocacy (urban, well-funded programs) and lax for isolated rural schools. The constraint represents a mixed coordination-extraction hybrid: legitimate sports administration (coordination) overlaid with selective enforcement (extraction).
constraint_indexing:constraint_classification(suanne_coup_of_peace, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: ADVOCACY AND LEGAL GROUPS (SCAFFOLD) — Civil rights organizations, tribal legal counsel, and sports equity advocates see the hostile environment as a temporary structural problem with a clear sunset: explicit anti-discrimination policies, monitoring protocols, potential legal remedies (Title VI, civil rights act violations), and shifting regional norms can dismantle the mechanism. Low theater — the hostile environment is viscerally real, not performative. Sunset logic is genuine: as regional racial attitudes shift and legal accountability strengthens, the economic and social cost of hosting hostile games increases, creating pressure for change.
constraint_indexing:constraint_classification(suanne_coup_of_peace, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FALSE NATURAL LAW VIEW (MOUNTAIN) — From a civilizational distance, one might frame the hostile environment as inevitable: human communities have always marked boundaries; outsiders have always faced resistance in unfamiliar territory; tribal identity is inherent. This perspective risks naturalizing the constraint as an immutable feature of social psychology. However, the structural data reveals this as a false summit: the hostile environment is contingent on specific institutional choices (selective law enforcement, lack of venue security against slurs, community social pressure), not on laws of nature or inevitable human behavior.
constraint_indexing:constraint_classification(suanne_coup_of_peace, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(suanne_coup_of_peace_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(suanne_coup_of_peace, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(suanne_coup_of_peace, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(suanne_coup_of_peace, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(suanne_coup_of_peace_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.58): High. The hostile environment extracts measurable value from the victim team: psychological disruption, reduced focus, humiliation, and reputation damage. The extraction is not through explicit rules (which would be enforceable) but through unmanaged community behavior — taunts and slurs during the game itself. The extractiveness has increased slightly over the interval (0.48 → 0.58) as the reputation of Lead's hostility has spread, creating anticipatory anxiety in the visiting team before arrival. Suppression (0.72): Very high. The visiting team faces severe barriers to resistance or exit: (1) game must be completed to avoid forfeiture; (2) racial slurs and taunts are not subject to formal rules in the same way fouls are; (3) the visiting team is outnumbered and lacks backing from the host jurisdiction; (4) reporting mechanisms (SDHSAA) are distant and have weak enforcement in rural areas. Theater ratio (0.45): Moderate-low. The hostile environment is not primarily performative — the actual hostility and its psychological effects are genuine. However, some portion (approximately 45%) may be theatrical: the repetition and intensity of slurs serves a performative function (demonstrating tribal solidarity to the home crowd), even though the underlying hostility is real. The theater has increased slightly over the interval as Lead's reputation for hostile games has preceded the teams, creating anticipatory performativity.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits a stark perspectival gap between the team experiencing extraction (Snare) and the community enabling extraction (Rope). From the Pine Ridge team's position, this is pure coercion with no coordination benefit — the hostile environment removes competitive focus and dignity. From the Lead community's position, the hostility serves a coordination function: it asserts community boundaries, maintains tribal (racial) identity, and signals status. The SDHSAA occupies a middle position (Tangled Rope) — they have legitimate coordination functions (managing sports programs) but enforce anti-discrimination rules asymmetrically, extracting compliance from scrutinized programs while permitting rural hosts to evade accountability. Advocacy groups see the Scaffold perspective: the constraint has a sunset clause because legal remedies, policy enforcement, and shifting regional norms can dismantle the mechanism. The false Mountain perspective naturalizes what is actually a contingent institutional failure — selective enforcement of existing rules.
 *
 * DIRECTIONALITY LOGIC:
 *   The Pine Ridge team's directionality (d ≈ 0.95) is derived from: victim status (they bear the costs of hostility) + trapped exit options (they must complete the game). This produces maximum f(d) ≈ 1.42, amplifying the experienced extractiveness chi. The Lead community's directionality (d ≈ 0.10) is derived from: beneficiary status (they extract status and dominance) + arbitrage exit options (they can choose to police the crowd or suspend the game but choose not to). This produces negative or low f(d), creating negative effective extraction — they experience the constraint as beneficial coordination, not as a burden. The SDHSAA's directionality (d ≈ 0.55) is derived from: mixed victim/beneficiary status (they are supposed to enforce rules but benefit from not enforcing in rural areas) + constrained exit options (they can enforce but face political pressure). This produces moderate f(d), making their experience a mixed hybrid. Supporters' directionality (d ≈ 0.75) is derived from: victim status + constrained exit options (they can leave early but this abandons their team).
 *
 * MANDATROPHY ANALYSIS:
 *   CRITICAL DISTINCTION: This constraint resolves the mandatrophy by showing how Snare and Rope can coexist from the same structural mechanism viewed from opposite positions. The Pine Ridge team experiences Snare (pure extraction, no coordination benefit) because they have no exit and no voice. The Lead community experiences Rope (coordination of social identity) because the hostile environment serves their community cohesion goals. This is NOT a case of 'which type is correct?' but 'the constraint's function depends on your structural position.' The mandatrophy is resolved by noting that mandatrophy only occurs when a SINGLE agent or observer perceives BOTH coordination and extraction from the SAME constraint. Here, different agents perceive different functions because they occupy opposite structural positions. The constraint is precisely a Snare for the victims and precisely a Rope for the beneficiaries — no contradiction, because their perspectives are fundamentally different. The false Mountain perspective (naturalizing as human tribal behavior) is unmasked by the structural data: the hostile environment is contingent on specific institutional choices (selective enforcement, lack of venue security protocols, community social pressure). If SDHSAA enforced rules uniformly and provided security against harassment, the constraint would dissolve. Therefore it is not a natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intent_vs_structure,
    'Is the hostile environment a product of explicit institutional policy (discriminatory intent) or emergent from unmanaged community behavior?',
    'Documentary evidence of school/athletic association decisions; interviews with lead community authority; records of reported incidents and response protocols',
    'If explicit policy: constraint is more easily actionable through institutional accountability. If emergent: requires community-level intervention and norm change (longer timeline, different remedies).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intent_vs_structure, empirical, 'Whether hostile environment reflects institutional policy or emergent community behavior').

omega_variable(
    enforcement_capability,
    'Does SDHSAA have genuine enforcement capacity to sanction hosts who permit racial harassment, or is enforcement capacity constrained by rural political influence?',
    'Historical case review of SDHSAA sanctions; correlation between violations reported and penalties imposed; interviews with athletic directors about enforcement pressure',
    'If capacity exists: scaffold sunset is realistic (rules can be enforced). If constrained: constraint persists because institutional enforcement is captured.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_capability, empirical, 'SDHSAA enforcement capacity against host discrimination').

omega_variable(
    team_coping_mechanisms,
    'Does the Pine Ridge team develop psychological resilience techniques (team cohesion, focusing rituals) that partially neutralize the extractive force, or does exposure persist without adaptation?',
    'Longitudinal tracking of team psychological state; performance comparison (home vs away games in hostile venues); player interviews about coping strategies',
    'If resilience develops: experienced extraction χ decreases over repeated exposures (Snare becomes less severe). If no adaptation: extraction remains constant or increases (trauma accumulation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(team_coping_mechanisms, empirical, 'Whether repeated exposure produces psychological resilience or trauma accumulation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(suanne_coup_of_peace, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(suan_tr_t0, suanne_coup_of_peace, theater_ratio, 0, 0.35).
narrative_ontology:measurement(suan_tr_t5, suanne_coup_of_peace, theater_ratio, 5, 0.42).
narrative_ontology:measurement(suan_tr_t10, suanne_coup_of_peace, theater_ratio, 10, 0.45).

% Extraction over time
narrative_ontology:measurement(suan_be_t0, suanne_coup_of_peace, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(suan_be_t5, suanne_coup_of_peace, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(suan_be_t10, suanne_coup_of_peace, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(suanne_coup_of_peace, enforcement_mechanism).
narrative_ontology:affects_constraint(suanne_coup_of_peace, lakota_education_access).
narrative_ontology:affects_constraint(suanne_coup_of_peace, rural_institutional_capture).

% DUAL FORMULATION NOTE:
% The hostile game environment is a specific manifestation of broader rural racial hierarchies and selective enforcement of civil rights protections. Decomposition: (1) base constraint is rural selective enforcement of anti-discrimination law (institutional capture); (2) specific manifestation is the Lead game hostile environment (Snare at local scope). Both share the same ε values and beneficiary/victim structure but differ in scope and temporal horizon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(suanne_coup_of_peace, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
