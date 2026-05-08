% ============================================================================
% CONSTRAINT STORY: irony_absorption_mechanism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_irony_absorption_mechanism, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: irony_absorption_mechanism
 *   human_readable: Irony Absorption Mechanism in Subversive Cultural Performance
 *   domain: cultural_sociology/political_economy/performance_studies
 *
 * SUMMARY:
 *   The irony absorption mechanism describes the systematic process by which
 *   subversive cultural performances are stripped of critical content and
 *   repackaged as profitable commodities. SantaCon provides the canonical
 *   trajectory: originating in 1994 as a Cacophony Society action mocking
 *   consumerism and conformity, it evolved by 2014 into a commercialized bar
 *   crawl generating millions in hospitality revenue while retaining only the
 *   aesthetic shell of transgression. The mechanism operates through a
 *   multi-stage process: (1) subversive artists create critique-laden
 *   performances, (2) the performances gain visibility through media
 *   amplification, (3) commercial actors recognize profit potential, (4) the
 *   performance is recontextualized as entertainment rather than critique,
 *   (5) the original critical function is evacuated while the aesthetic form
 *   persists, (6) the absorbed form becomes the dominant public
 *   understanding, erasing the original intent. The constraint exhibits high
 *   theater ratio (0.81) because the absorbed performances maintain the
 *   appearance of transgression — participants dress as Santa, engage in
 *   public spectacle, feel they are doing something subversive — while the
 *   actual function has inverted: what began as mockery of consumer culture
 *   becomes a driver of consumer spending. The mechanism's extractiveness
 *   (0.68) reflects that it converts artists' critical labor into profit
 *   while destroying the work's intended function, but some genuine
 *   coordination occurs (artists do reach audiences, participants do
 *   experience community), preventing classification as pure extraction.
 *
 * KEY AGENTS:
 *   - Subversive Art Movements: Primary victim (powerless/identity_locked) — create critique-laden work that gets systematically absorbed and inverted; identity-locked because exit requires abandoning the artist identity entirely
 *   - Originating Artists: Secondary victim (powerless/identity_locked) — watch their specific creations become the opposite of their intent; Cacophony Society members who disavowed SantaCon as it commercialized
 *   - Cultural Critique Capacity: Abstract victim (powerless/trapped) — the collective capacity for culture to critique power; trapped because successful critique guarantees absorption
 *   - Bar/Hospitality Industry: Primary beneficiary (institutional/arbitrage) — captures predictable revenue from absorbed performances without creating the original content
 *   - Event Promotion Companies: Secondary beneficiary (institutional/arbitrage) — monetize the absorbed aesthetic through ticketing, branding, sponsorship
 *   - Media Platforms: Secondary beneficiary (institutional/arbitrage) — viral subversive content generates engagement and ad revenue
 *   - Participating Ironists: Mixed position (moderate/constrained) — experience genuine community and fun but subsidize commercial extraction through participation
 *   - Post-Ironic Collective: Organized resistance (organized/mobile) — building alternative performance modes designed to resist absorption
 *   - Legacy Alternative Media: Degraded institution (institutional/constrained) — publications like Village Voice that perform 'edgy coverage' while functioning as event promotion
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(irony_absorption_mechanism, 0.68).
domain_priors:suppression_score(irony_absorption_mechanism, 0.72).
domain_priors:theater_ratio(irony_absorption_mechanism, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(irony_absorption_mechanism, extractiveness, 0.68).
narrative_ontology:constraint_metric(irony_absorption_mechanism, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(irony_absorption_mechanism, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(irony_absorption_mechanism, snare).
narrative_ontology:human_readable(irony_absorption_mechanism, "Irony Absorption Mechanism in Subversive Cultural Performance").
narrative_ontology:topic_domain(irony_absorption_mechanism, "cultural_sociology/political_economy/performance_studies").

domain_priors:requires_active_enforcement(irony_absorption_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(irony_absorption_mechanism, bar_hospitality_industry).
narrative_ontology:constraint_beneficiary(irony_absorption_mechanism, event_promotion_companies).
narrative_ontology:constraint_beneficiary(irony_absorption_mechanism, media_platforms).
narrative_ontology:constraint_victim(irony_absorption_mechanism, subversive_art_movements).
narrative_ontology:constraint_victim(irony_absorption_mechanism, cultural_critique_capacity).
narrative_ontology:constraint_victim(irony_absorption_mechanism, originating_artists).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBVERSIVE ARTIST (SNARE) — Identity-locked rather than materially trapped: the artist's professional identity and self-concept are constituted through creating critique-laden work, but the absorption mechanism ensures that successful dissemination requires entering channels that strip the critique. Exit would mean abandoning the identity of 'subversive artist' entirely. The artist watches their work become the opposite of its intent — the performance meant to mock consumer culture becomes a consumer product. Maximum extraction: the mechanism converts the artist's labor into profit while destroying the work's critical function.
constraint_indexing:constraint_classification(irony_absorption_mechanism, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 2: CULTURAL CRITIQUE CAPACITY (SNARE) — The abstract collective capacity for culture to critique power. Trapped with no exit: when irony absorption becomes systematic, critique itself becomes a product category. The mechanism creates a double bind: successful critique (wide reach) guarantees absorption; unsuccessful critique (no reach) has no impact. The commons of cultural resistance bears full extraction with no advocate and no escape.
constraint_indexing:constraint_classification(irony_absorption_mechanism, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: PARTICIPATING IRONIST (TANGLED ROPE) — The individual who joins SantaCon or similar events. Constrained by social network effects and FOMO, but also genuinely experiences coordination: the event provides community, spectacle, and shared experience. Extraction is real (the participant's ironic performance subsidizes bar revenue and media content) but not total — some authentic fun occurs. The participant knows the event has been captured but participates anyway because the alternative is isolation from peer culture.
constraint_indexing:constraint_classification(irony_absorption_mechanism, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: BAR/HOSPITALITY INDUSTRY (ROPE) — Primary beneficiary. Experiences the mechanism as pure coordination: subversive performances deliver predictable crowds to commercial venues. The industry did not create the performances but learned to capture them. Arbitrage exit: can switch to other event types if this one stops being profitable. Net beneficiary — extraction flows toward this agent.
constraint_indexing:constraint_classification(irony_absorption_mechanism, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: MEDIA PLATFORMS (ROPE) — Secondary beneficiary. Viral subversive content generates engagement and ad revenue. The platform provides infrastructure (coordination function) but captures asymmetric value: the artist gets exposure, the platform gets monetizable attention. Experiences the mechanism as coordination because the platform genuinely connects artists to audiences — the extraction is a side effect from this perspective.
constraint_indexing:constraint_classification(irony_absorption_mechanism, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: POST-IRONIC COLLECTIVE (SCAFFOLD) — Organized artists and theorists building alternative performance modes that resist absorption: metamodernism, new sincerity, relational aesthetics, participatory art that cannot be commodified because it exists only in unreproducible moments. See irony absorption as a temporary problem with a sunset: as the mechanism becomes visible and theorized, artists develop immune responses. The scaffold logic: once you see the trap, you can design around it.
constraint_indexing:constraint_classification(irony_absorption_mechanism, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: LEGACY ALTERNATIVE MEDIA (PITON) — Publications like Village Voice that once amplified subversive culture now perform the ritual of 'edgy coverage' while functioning as event promotion. The editorial mission (cultural critique) has atrophied into theater: covering SantaCon as transgressive spectacle while the coverage itself drives attendance and bar revenue. The institution sees its own degradation — maintained through inertia and advertiser relationships, not through critical function.
constraint_indexing:constraint_classification(irony_absorption_mechanism, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, irony absorption is both a genuine coordination problem (how to disseminate critique in a market economy) and an extraction mechanism (capital systematically neutralizes threats by commodifying them). The mechanism has a real function (connecting artists to audiences, providing venues for performance) but the function is inseparable from the extraction (stripping critique, converting resistance into profit). Tangled Rope: irreducible hybrid of coordination and extraction.
constraint_indexing:constraint_classification(irony_absorption_mechanism, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(irony_absorption_mechanism_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(irony_absorption_mechanism, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(irony_absorption_mechanism, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(irony_absorption_mechanism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(irony_absorption_mechanism, TR),
    TR >= 0.70.

:- end_tests(irony_absorption_mechanism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The mechanism converts artists' critical labor into profit while destroying the work's critical function. The artist creates a performance mocking consumerism; the performance becomes a consumer product. The extraction is not total (0.68 rather than 0.85+) because some genuine coordination occurs: artists do reach audiences, participants do experience community, and the absorbed form sometimes retains fragments of critique that can be recovered by attentive observers. But the primary function has inverted. Suppression (0.72): High. Multiple mechanisms suppress alternatives: (1) mass reach requires entering commercial channels (media, venues, platforms) that impose absorption as the price of visibility, (2) non-absorbed critique remains marginal and invisible, creating selection pressure toward absorption, (3) once absorption becomes systematic, audiences expect the absorbed aesthetic and reject non-absorbed alternatives as 'preachy' or 'not fun', (4) artists who resist absorption face career marginalization and economic precarity. The suppression is partly internalized (artists self-censor to maintain market access) and partly structural (genuine alternative channels have limited reach). Theater ratio (0.81): Very high. The absorbed performances maintain the aesthetic shell of transgression while the critical function has been evacuated. SantaCon participants dress as Santa, engage in public spectacle, and feel they are doing something subversive, but the actual function is driving bar revenue. The theater has increased over the interval as the gap between aesthetic form (transgressive costume, public disruption) and actual function (commercial consumption) has widened. Early SantaCon (1994-1999) had lower theater because the performances still carried some critical charge; late SantaCon (2009-2014) is almost pure theater.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates extreme perspectival divergence. The bar/hospitality industry sees pure coordination (Rope): subversive performances deliver predictable crowds to commercial venues, solving the industry's customer acquisition problem. Media platforms see coordination (Rope): they provide infrastructure connecting artists to audiences, and the monetization is a side effect. The participating ironist sees mixed coordination and extraction (Tangled Rope): genuine fun and community occur, but the participant knows they are subsidizing commercial capture. The subversive artist sees pure extraction (Snare): their critical labor is converted into profit while the work's intended function is destroyed, and they cannot exit without abandoning their identity. Cultural critique capacity sees pure extraction (Snare): the mechanism creates a double bind where successful critique guarantees absorption. The post-ironic collective sees a temporary problem with a sunset (Scaffold): once the mechanism becomes visible, artists can design around it. Legacy alternative media sees degraded ritual (Piton): the institution performs 'edgy coverage' while functioning as event promotion. The analytical observer sees irreducible hybrid (Tangled Rope): the mechanism has a genuine coordination function (disseminating performances, connecting artists to audiences) that is inseparable from the extraction (stripping critique, converting resistance into profit). The gap reveals that 'irony absorption' is not a single phenomenon but a structural position within a multi-agent system where each position experiences a different constraint type.
 *
 * DIRECTIONALITY LOGIC:
 *   The subversive artist is identity-locked rather than materially trapped: the binding mechanism is that the artist's professional identity and self-concept are constituted through creating critique-laden work, but successful dissemination requires entering channels that strip the critique. Exit would mean abandoning the identity of 'subversive artist' entirely — becoming a commercial entertainer, a fine artist in the gallery system, or leaving art altogether. The artist is a victim (bears extraction) with identity_locked exit, producing high directionality. Cultural critique capacity is an abstract collective with no advocate and no exit option — trapped, producing maximum directionality. Participating ironists are victims (subsidize commercial extraction) but also beneficiaries (experience genuine community), with constrained exit (can stop participating but face social costs), producing moderate directionality. The bar/hospitality industry is a pure beneficiary with arbitrage exit (can switch to other event types), producing low/negative directionality. Media platforms are beneficiaries with arbitrage exit, producing low/negative directionality. The post-ironic collective is organized with mobile exit (can create alternative performance modes), producing moderate directionality even though they are working against the mechanism. Legacy alternative media is constrained (advertiser dependencies, institutional inertia) and experiences the mechanism as degradation of their own function, producing moderate-high directionality despite institutional power.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by demonstrating that the same mechanism can be simultaneously coordination (from the beneficiary's perspective), extraction (from the victim's perspective), and hybrid (from the analytical perspective) without contradiction. The bar industry genuinely experiences coordination: they did not create SantaCon, but it solves their customer acquisition problem. The subversive artist genuinely experiences extraction: their critical labor is converted into profit while the work's intended function is destroyed. Both are true. The mandatrophy error would be to claim that because the industry experiences coordination, the artist's extraction is not real, or that because the artist experiences extraction, the industry's coordination is false consciousness. The indexical classification system resolves this by showing that coordination and extraction are not mutually exclusive properties of a constraint but perspectival readings from different structural positions. The mechanism IS a coordination device (from institutional/arbitrage positions) AND an extraction device (from powerless/identity_locked positions). The analytical observer's Tangled Rope classification captures this irreducible duality: the coordination function and the extraction function are structurally inseparable because the same process (mass dissemination of subversive performances) that enables artists to reach audiences also enables commercial actors to capture and invert the critique.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    absorption_inevitability,
    'Is irony absorption an inevitable feature of mass-mediated culture under capitalism, or a contingent institutional arrangement that could be restructured?',
    'Historical analysis of pre-capitalist and non-capitalist cultural critique mechanisms; examination of whether decentralized/non-monetized platforms (early internet, zines, direct action) exhibit lower absorption rates',
    'If inevitable: mountain from all perspectives — a natural law of information economies. If contingent: the current institutional arrangement (advertising-funded media, venue-based performance, IP law) is the extractive mechanism, not culture itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(absorption_inevitability, conceptual, 'Whether absorption is structural inevitability or contingent institutional design').

omega_variable(
    artist_complicity_threshold,
    'At what point does the artist''s participation in absorption channels constitute complicity rather than victimization?',
    'Ethical framework for distinguishing structural coercion (no alternative dissemination channels exist) from strategic choice (artist chooses reach over purity). Analysis of artist statements, alternative platform usage, and exit attempts.',
    'If low threshold (artist is complicit once aware): victim classification inappropriate, artist becomes beneficiary. If high threshold (structural coercion dominates): victim classification stands, artist is identity-locked.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(artist_complicity_threshold, preference, 'Threshold for artist complicity vs. structural coercion').

omega_variable(
    post_ironic_immunity,
    'Do post-ironic and metamodern performance modes actually resist absorption, or do they represent the next iteration of the same mechanism?',
    'Longitudinal tracking of new sincerity and metamodern art movements: do they maintain critical function as they scale, or do they get absorbed into ''authentic experience'' marketing? Comparison of early-stage vs. mature-stage commodification.',
    'If immune: scaffold perspective confirmed — the sunset is real. If absorbed: the mechanism is more fundamental than any particular aesthetic strategy, and the scaffold is aspirational rather than structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_ironic_immunity, empirical, 'Whether post-ironic modes resist or replicate absorption').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.72) primarily structural (lack of alternative channels) or internalized (artists self-censor to maintain market access)?',
    'Analysis of artist behavior when alternative channels exist: do artists use non-commodified platforms when available, or do they self-select into absorption channels even when alternatives exist? Comparison of artist statements about constraints vs. revealed preferences.',
    'If structural: suppression is external barrier, removal of commercial gatekeepers would reduce extraction. If internalized: suppression persists even after structural barriers fall, artists carry the constraint with them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Structural vs. internalized suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(irony_absorption_mechanism, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(irony_absorb_theater_1994, irony_absorption_mechanism, theater_ratio, 0, 0.35).
narrative_ontology:measurement(irony_absorb_theater_1999, irony_absorption_mechanism, theater_ratio, 5, 0.52).
narrative_ontology:measurement(irony_absorb_theater_2004, irony_absorption_mechanism, theater_ratio, 10, 0.68).
narrative_ontology:measurement(irony_absorb_theater_2009, irony_absorption_mechanism, theater_ratio, 15, 0.77).
narrative_ontology:measurement(irony_absorb_theater_2014, irony_absorption_mechanism, theater_ratio, 20, 0.81).

% Extraction over time
narrative_ontology:measurement(irony_absorb_extract_1994, irony_absorption_mechanism, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(irony_absorb_extract_1999, irony_absorption_mechanism, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(irony_absorb_extract_2004, irony_absorption_mechanism, base_extractiveness, 10, 0.51).
narrative_ontology:measurement(irony_absorb_extract_2009, irony_absorption_mechanism, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(irony_absorb_extract_2014, irony_absorption_mechanism, base_extractiveness, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(irony_absorption_mechanism, identity_coordination).
narrative_ontology:boltzmann_floor_override(irony_absorption_mechanism, 0.08).
narrative_ontology:affects_constraint(irony_absorption_mechanism, platform_algorithmic_curation).
narrative_ontology:affects_constraint(irony_absorption_mechanism, attention_economy_dynamics).

% DUAL FORMULATION NOTE:
% Irony absorption is downstream of two upstream constraints: erasure_before_celebration (the mountain-level pattern where marginalized cultures are commodified only after their threatening elements are removed) provides the template, and commodified_permission_structure (the tangled_rope mechanism where transgression is permitted only in commercially profitable forms) provides the enforcement mechanism. Irony absorption is the specific instantiation of these patterns in the domain of subversive cultural performance. The three constraints form a family: erasure_before_celebration establishes the general pattern, commodified_permission_structure describes the institutional enforcement, and irony_absorption_mechanism describes the cultural-performance-specific dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(irony_absorption_mechanism, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
