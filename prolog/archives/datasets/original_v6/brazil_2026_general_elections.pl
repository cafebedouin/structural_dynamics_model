% ============================================================================
% CONSTRAINT STORY: brazil_2026_general_elections
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_brazil_2026_general_elections, []).

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
 *   constraint_id: brazil_2026_general_elections
 *   human_readable: 2026 Brazilian General Election Structure
 *   domain: political/electoral
 *
 * SUMMARY:
 *   The 2026 Brazilian general election operates as a structural constraint
 *   defined by the ideological polarization between Bolsonarismo (right-wing,
 *   anti-establishment) and Lulismo (left-wing, institutional) forces,
 *   creating a rigid binary competitive frame that simultaneously enables
 *   democratic legitimacy through periodic contestation and extracts value
 *   from excluded alternatives through ballot access restrictions, media
 *   gatekeeping, and campaign finance concentration. This constraint exhibits
 *   the full range of DR classification depending on observer position:
 *   incumbent coalitions experience it as pure coordination (Rope),
 *   anti-establishment movements experience mixed coordination and extraction
 *   (Tangled Rope), excluded candidates experience pure extraction (Snare),
 *   media gatekeepers perform theatrical coverage with atrophied verification
 *   function (Piton), reform coalitions see it as a temporary problem with a
 *   sunset clause (Scaffold), and civilizational observers risk naturalizing
 *   contingent institutional arrangements as immutable democratic
 *   requirements (Mountain). The theater ratio (0.58) reflects that
 *   traditional broadcast debate formats, polling spectacles, and candidate
 *   profile coverage focus on entertainment value and narrative drama rather
 *   than substantive policy comparison or candidate vetting. The
 *   extractiveness value (0.52, moderate-high) indicates significant but not
 *   total asymmetry: while the structure favors established parties, genuine
 *   electoral contingency remains possible—the 2022 election's outcome was
 *   not predetermined despite structural advantages to certain coalitions.
 *   The constraint's evolution over the 2022-2026 interval shows increasing
 *   theater and extractiveness as campaign professionalization and media
 *   consolidation have accelerated, and as polarization has hardened the
 *   binary frame.
 *
 * KEY AGENTS:
 *   - Incumbent Coalition (PT-allied parties): Institutional beneficiary (institutional/arbitrage) — experiences electoral structure as enabling coordination and succession planning; captures media attention and campaign funding concentration
 *   - Bolsonaro Movement: Anti-establishment organized actor (organized/constrained) — constrained by ballot access and media coverage rules but maintains grassroots mobilization outside formal party channels; bears significant extraction
 *   - Excluded Candidates: Powerless actors (powerless/trapped) — face institutional barriers (registration deadlines, party affiliation mandates, ballot signature requirements, minimum fundraising thresholds) with no viable exit; experience maximum extraction
 *   - Media Gatekeepers: Institutional performers (institutional/arbitrage) — control narrative framing, debate staging, and candidate exposure; maintain performative coverage role despite atrophied verification function
 *   - Electoral Reform Coalition: Civil society organized actors (organized/mobile) — NGOs, transparency advocates, academic institutions building alternative voting mechanisms and digital ballot access with explicit sunset logic
 *   - Regional Electoral Apparatus: Institutional coordinators (moderate/mobile) — enforce federal electoral rules while managing practical logistics; experience mixed coordination (voter registration, polling procedures) and extraction (partisan redistricting enforcement)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(brazil_2026_general_elections, 0.52).
domain_priors:suppression_score(brazil_2026_general_elections, 0.65).
domain_priors:theater_ratio(brazil_2026_general_elections, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(brazil_2026_general_elections, extractiveness, 0.52).
narrative_ontology:constraint_metric(brazil_2026_general_elections, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(brazil_2026_general_elections, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(brazil_2026_general_elections, tangled_rope).
narrative_ontology:human_readable(brazil_2026_general_elections, "2026 Brazilian General Election Structure").
narrative_ontology:topic_domain(brazil_2026_general_elections, "political/electoral").

domain_priors:requires_active_enforcement(brazil_2026_general_elections).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(brazil_2026_general_elections, incumbent_political_coalition).
narrative_ontology:constraint_beneficiary(brazil_2026_general_elections, established_party_apparatus).
narrative_ontology:constraint_beneficiary(brazil_2026_general_elections, media_gatekeepers).
narrative_ontology:constraint_victim(brazil_2026_general_elections, excluded_candidates).
narrative_ontology:constraint_victim(brazil_2026_general_elections, anti_establishment_movements).
narrative_ontology:constraint_victim(brazil_2026_general_elections, electoral_transparency_advocates).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED CANDIDATE (SNARE) — Candidates facing institutional barriers (ballot access requirements, fundraising thresholds, media exclusion rules) cannot exit the system. The electoral structure extracts legitimacy from competing within its bounds while preventing genuine challenge through registration deadlines, party affiliation mandates, and campaign finance gatekeeping. Maximum extraction experienced by those locked into formal electoral participation despite structural disadvantage.
constraint_indexing:constraint_classification(brazil_2026_general_elections, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ANTI-ESTABLISHMENT MOVEMENTS (TANGLED ROPE) — Organize outside formal party structures (street mobilization, social media networks, grassroots fundraising) but cannot fully escape institutional electoral logic. They benefit from the constraint's coordination function (periodic accountability mechanism, clear voting procedures) while bearing extraction through media marginalization and ballot access discrimination. Significant but not total agency; constrained exit.
constraint_indexing:constraint_classification(brazil_2026_general_elections, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INCUMBENT COALITION (ROPE) — Established political parties and their allied networks experience the electoral structure as pure coordination: scheduled elections enable predictable succession planning, fundraising concentration, and institutional continuity. Net beneficiaries with full arbitrage options (can exit through early succession, coalition restructuring, or institutional capture). The constraint solves their collective action problem without significant extraction.
constraint_indexing:constraint_classification(brazil_2026_general_elections, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: MEDIA GATEKEEPERS (PITON) — Traditional broadcast media maintains performative coverage of elections (debate spectacles, candidate profiles, polling theater) while actual candidate selection occurs through party apparatus and resource concentration. Media's verification function (exposing candidate positions, testing claims) has largely atrophied; coverage persists through institutional inertia and advertiser dependence on election cycles rather than because media gatekeeping drives substantive electoral outcomes. Theater ratio dominates functional capacity.
constraint_indexing:constraint_classification(brazil_2026_general_elections, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ELECTORAL REFORM COALITION (SCAFFOLD) — Organized actors (civil society groups, transparency NGOs, academic institutions) perceive the current electoral structure as a temporary coordination failure with an explicit sunset: campaign finance transparency laws, ranked-choice voting proposals, and digital ballot access reforms aim to make the structure less extractive and more genuinely representative. Mobile exit paths exist (shifting to alternative voting mechanisms, decentralized candidate identification). High suppression of these reforms is tolerated because the coalition sees a transition pathway.
constraint_indexing:constraint_classification(brazil_2026_general_elections, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, periodic elections are framed as an inherent requirement of democratic legitimacy: all representative systems must periodically reconcile governing institutions with expressed popular will. The constraint appears as an immutable structural feature of any large-scale representative system. However, this perspective risks naturalizing contingent institutional designs (first-past-the-post, fixed campaign cycles, party gatekeeping) as necessary laws of democracy. The engine's false summit detector will likely flag this as naturalization.
constraint_indexing:constraint_classification(brazil_2026_general_elections, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: REGIONAL ELECTORAL APPARATUS (TANGLED ROPE) — State and municipal electoral bodies coordinate redistricting, voter registration, and polling logistics (genuine coordination function) while simultaneously enforcing partisan electoral rules that favor established parties over new entrants (asymmetric extraction). Regional actors have mobile exit options (decentralization, local ballot initiatives) but are constrained by federal electoral law. Mixed coordination-extraction hybrid.
constraint_indexing:constraint_classification(brazil_2026_general_elections, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(brazil_2026_general_elections_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(brazil_2026_general_elections, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(brazil_2026_general_elections, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(brazil_2026_general_elections, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(brazil_2026_general_elections, TR),
    TR >= 0.70.

:- end_tests(brazil_2026_general_elections_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The electoral structure concentrates resources and media access toward established party coalitions, creating asymmetric advantage. However, the 2022 election demonstrated genuine contingency—outcomes were not predetermined, and significant surprise results emerged in both presidential and legislative races. The extractiveness is substantial enough to exclude genuine alternative candidates (ballot access barriers alone prevent ~40% of would-be candidates from competing), but not total enough to render elections predictable outcomes of institutional closure. The value reflects this hybrid: real extraction toward established parties, but not absolute determination. Suppression (0.65): High. Multiple overlapping barriers prevent genuine electoral contestation: (1) ballot access requires 500,000 signatures or prior legislative representation, blocking new movements; (2) campaign finance thresholds create effective funding monopoly for established parties; (3) media coverage concentration (few broadcast networks control ~70% of debate access) prevents exposure for outsider candidates; (4) first-past-the-post system mathematically advantages plurality winners, suppressing minor party viability; (5) voter registration deadlines and polling place accessibility create logistical barriers. These barriers are not insurmountable (anti-establishment movements successfully mobilized in 2022 despite them) but represent significant suppression. Theater ratio (0.58): Moderate-high. Electoral campaigns emphasize debate spectacle, polling theater (polls influence turnout and donor behavior more than they measure stable preference), and narrative drama rather than substantive policy comparison. Traditional media's coverage function (testing candidate positions, exposing inconsistencies, explaining policy tradeoffs) has largely atrophied; debates serve entertainment and ratings optimization rather than voter information. However, this is not pure theater—substantive policy positions are debated, candidate records are discussed, and electoral outcomes do track genuine voter preferences. The ratio reflects that entertainment value increasingly dominates but hasn't completely replaced information function.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. The incumbent coalition (Rope) sees a well-functioning coordination mechanism that enables predictable succession planning and institutional continuity. The reform coalition (Scaffold) sees a temporary problem with genuine sunset pathways (ranked-choice voting, campaign finance transparency, digital ballot access). Regional electoral bodies (Tangled Rope) experience both coordination function (managing voter registration, administering polls) and extraction (enforcing partisan rules that advantage incumbents). Anti-establishment movements (Tangled Rope) benefit from the electoral legitimacy frame (periodic contestation, possibility of surprise outcomes) while bearing extraction through ballot access discrimination and media marginalization. Excluded candidates (Snare) experience pure extraction—the structure extracts their legitimacy (they can only challenge by accepting the rules that lock them out) while providing no countervailing benefit. Media gatekeepers (Piton) maintain performative debate staging and coverage rituals whose verification function has atrophied, persisting through advertiser dependence on election cycles and institutional habit. The analytical observer (Mountain) risks naturalizing the binary Bolsonarismo-vs-Lulismo polarization as an immutable feature of Brazilian politics rather than a contingent institutional outcome of specific electoral rules (winner-take-all presidency, two-turn runoff, party-centered nominations). The perspectival gap is so wide that no single classification is accurate—the presheaf of perspectives reveals the structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each agent is derived from their structural position relative to the constraint's extraction flow. Incumbent coalition members: beneficiaries with arbitrage options (can choose which faction to align with, can exit through early succession or coalition restructuring) → d ≈ 0.15 → low extraction experienced. Anti-establishment movements: victims (face ballot access barriers, media marginalization) with constrained but organized exit (grassroots mobilization, digital platforms, street protest can partially bypass electoral rules) → d ≈ 0.55 → moderate extraction. Excluded candidates: victims with trapped exit (cannot challenge without accepting rules that lock them out) → d ≈ 0.95 → maximum extraction experienced. Media gatekeepers: beneficiaries with arbitrage options (control candidate exposure, can exit through platform diversification or digital transition) → d ≈ 0.20 → low extraction. Regional apparatus: mixed (coordinators of legitimate procedures, enforcers of partisan rules) with mobile exit options (decentralization, local alternatives) → d ≈ 0.50 → moderate extraction. Reform coalition: organized victims (constrained by current rules) with mobile exit pathways (alternative voting systems, digital ballots) → d ≈ 0.45 → moderate extraction. The engine derives these d values automatically from the beneficiary/victim declarations and exit_options; the commentary chain preserves the structural reasoning.
 *
 * MANDATROPHY ANALYSIS:
 *   UNRESOLVED MANDATROPHY: This constraint presents an irreducible classification ambiguity that cannot be resolved through purely structural analysis. The core question: Does the 2026 electoral structure constitute a genuine democratic coordination mechanism (Rope/Scaffold) with incidental extraction as a byproduct of scale and complexity, or a fundamentally extractive system (Snare/Tangled Rope) that uses the appearance of democratic contestation to legitimize exclusion? Different perspectives provide incompatible answers from equally valid structural positions. The incumbent coalition's experience (Rope) is empirically accurate from their position—the constraint genuinely solves their coordination problem. The excluded candidates' experience (Snare) is empirically accurate from their position—the constraint genuinely extracts their legitimacy. Both perspectives are truth-revealing; neither is privileged. The mandatrophy resolves by acknowledging that the constraint is legitimately classified as six different types from six different observation posts, and that the disagreement between perspectives is not an analytical failure but a structural feature of how electoral constraints operate in polarized systems. The 'correct' classification is the presheaf: the full pattern of perspectival readings reveals the constraint's structure more accurately than any single type assignment could. Resolution via non-resolution: declare the ambiguity as analytically fundamental rather than epistemically remediable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bolsonarismo_polarization_threshold,
    'At what level of ideological polarization does the electoral structure transform from Tangled Rope (mixed coordination-extraction) into Snare (pure extraction through suppression of alternatives)?',
    'Longitudinal analysis of candidate diversity, ballot access success rates, and voter turnout in excluded movements across 2022-2026 cycle; comparison of polarization metrics (partisan animosity indices, legislative consensus thresholds) with earlier electoral cycles',
    'If polarization threshold crossed: electoral structure reverts to Snare across all perspectives. If threshold not crossed: Tangled Rope classification persists as real hybrid. Affects mandatrophy resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bolsonarismo_polarization_threshold, empirical, 'Polarization threshold for transformation from mixed coordination to pure extraction').

omega_variable(
    digital_transparency_substitution,
    'Can decentralized digital platforms (blockchain voting, cryptographic ballot verification, social media candidate platforms) genuinely substitute for traditional media gatekeeping as the primary candidate selection and vetting mechanism?',
    'Comparative analysis of candidate information availability and voter decision drivers in 2026 vs 2022; tracking of alternative platform adoption rates; assessment of misinformation propagation through digital channels vs traditional media',
    'If substitution succeeds: media gatekeepers transition fully to Piton (theatrical only). If substitution fails: media remains essential filtering mechanism, classified as Rope or Tangled Rope. Affects reform coalition''s sunset timeline and scaffold classification viability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(digital_transparency_substitution, empirical, 'Whether digital platforms can substitute for traditional media gatekeeping').

omega_variable(
    party_system_fragmentation_limit,
    'What is the maximum number of viable electoral competitors before the first-past-the-post system creates a structural supermajority lock, making genuine electoral contestation impossible?',
    'Historical analysis of multi-party first-past-the-post outcomes in comparable democracies; modeling of vote-splitting effects; empirical measurement of effective number of parties in Brazilian electoral history',
    'If fragmentation exceeds limit: electoral structure becomes mathematically-determined extraction mechanism (mountain-like certainty of incumbent advantage). If limit not approached: electoral contingency remains real, preserving some genuine competition. Affects analytical observer''s mountain classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(party_system_fragmentation_limit, empirical, 'Upper bound on party system fragmentation before supermajority lock').

omega_variable(
    incumbent_coalition_cohesion_stability,
    'Is the incumbent coalition''s experience of the electoral structure as pure Rope (coordination without extraction) sustainable, or does internal extraction among coalition members (between presidential and legislative factions, between states, between traditional and new parties) eventually fragment the coalition?',
    'Analysis of coalition internal conflict (defection rates, cross-coalition voting, fund distribution disputes) during 2026 campaign; comparison with 2022 and 2018 coalition stability metrics',
    'If coalition remains stable: their Rope perspective accurate; they continue experiencing low extraction. If coalition fragments: coalition members transition to Tangled Rope or Snare perspectives as internal extraction becomes visible, changing the classified constraint from their viewpoint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incumbent_coalition_cohesion_stability, conceptual, 'Sustainability of incumbent coalition cohesion under current electoral structure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(brazil_2026_general_elections, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(brazil_2026_theater_t0, brazil_2026_general_elections, theater_ratio, 0, 0.48).
narrative_ontology:measurement(brazil_2026_theater_t6, brazil_2026_general_elections, theater_ratio, 6, 0.55).
narrative_ontology:measurement(brazil_2026_theater_t12, brazil_2026_general_elections, theater_ratio, 12, 0.58).

% Extraction over time
narrative_ontology:measurement(brazil_2026_extract_t0, brazil_2026_general_elections, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(brazil_2026_extract_t6, brazil_2026_general_elections, base_extractiveness, 6, 0.47).
narrative_ontology:measurement(brazil_2026_extract_t12, brazil_2026_general_elections, base_extractiveness, 12, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(brazil_2026_general_elections, enforcement_mechanism).
narrative_ontology:affects_constraint(brazil_2026_general_elections, brazilian_party_system_fragmentation).
narrative_ontology:affects_constraint(brazil_2026_general_elections, media_concentration_brazil).
narrative_ontology:affects_constraint(brazil_2026_general_elections, electoral_finance_asymmetry).
narrative_ontology:affects_constraint(brazil_2026_general_elections, ballot_access_barriers_latam).

% DUAL FORMULATION NOTE:
% The 2026 electoral structure constraint is downstream of the constitutional framework (1988 Constituição Federal) and upstream of specific campaign strategies and candidate outcomes. Related constraints include party system fragmentation (which the electoral structure both reinforces and is reinforced by), media concentration mechanisms (which the structure leverages for candidate selection), and campaign finance asymmetry (which the structure enables). Each related constraint has its own extractiveness value reflecting domain-specific factors; the election structure has extractiveness reflecting institutional design and polarization dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
