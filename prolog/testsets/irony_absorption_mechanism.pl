% ============================================================================
% CONSTRAINT STORY: irony_absorption_mechanism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
 *   repackaged as profitable commodities. SantaCon exemplifies the
 *   trajectory: originating in 1994 as a Cacophony Society action mocking
 *   consumerism and conformity through mass Santa impersonation in public
 *   spaces, it evolved by 2014 into a commercialized bar crawl generating
 *   millions in hospitality revenue while retaining only the aesthetic shell
 *   of transgression. The mechanism is not a simple market failure or one-way
 *   extraction — it exhibits genuine coordination (assembling performers and
 *   audiences, creating venues, managing logistics) alongside asymmetric
 *   extraction (critical message dilution, financial benefit concentration,
 *   suppression of non-absorbed alternatives). The constraint demonstrates
 *   how absorption occurs at the performative level rather than the material
 *   level: the appearance of subversion (costume, irony, public gathering)
 *   persists and even intensifies, while the critical content becomes
 *   decorative. This makes it exceptionally difficult to resist — refusing
 *   participation looks like refusing fun, community, and creativity rather
 *   than like opposing a clear extractive mechanism.
 *
 * KEY AGENTS:
 *   - Subversive Artistic Intent / Original Creator Collective: Primary victim (powerless/trapped) — the critical impulse that initiates performance; cannot exit the absorption cycle once visibility is achieved
 *   - Participating Artists and Performers: Secondary victim with agency (moderate/constrained) — benefit from platform access and income but face pressure to dilute critical content for commercial viability
 *   - Commercial Interests (Bars, Event Promoters, Media Platforms): Primary beneficiary (institutional/arbitrage) — extract revenue while providing genuine coordination services; experience the constraint as pure cooperation
 *   - Audiences and Participants: Complex mixed position (moderate/constrained) — may experience genuine pleasure and community while unknowingly participating in aesthetic colonization
 *   - Counter-Commodification Movements: Organized resistance (organized/constrained) — developing non-commodifiable performance forms and alternative distribution; see the problem as temporary and solvable
 *   - Institutional Critique Industry: Piton actors (institutional/arbitrage) — maintain the language of 'subversion' and 'transgression' as empty rhetoric supporting the very systems being critiqued
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the mechanism as inevitable capitalism rather than as an engineered coordination problem
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(irony_absorption_mechanism, 0.58).
domain_priors:suppression_score(irony_absorption_mechanism, 0.52).
domain_priors:theater_ratio(irony_absorption_mechanism, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(irony_absorption_mechanism, extractiveness, 0.58).
narrative_ontology:constraint_metric(irony_absorption_mechanism, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(irony_absorption_mechanism, theater_ratio, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(irony_absorption_mechanism, tangled_rope).
narrative_ontology:human_readable(irony_absorption_mechanism, "Irony Absorption Mechanism in Subversive Cultural Performance").
narrative_ontology:topic_domain(irony_absorption_mechanism, "cultural_sociology/political_economy/performance_studies").

domain_priors:requires_active_enforcement(irony_absorption_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(irony_absorption_mechanism, commercial_interests).
narrative_ontology:constraint_beneficiary(irony_absorption_mechanism, cultural_gatekeepers).
narrative_ontology:constraint_victim(irony_absorption_mechanism, subversive_artistic_intent).
narrative_ontology:constraint_victim(irony_absorption_mechanism, cultural_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBVERSIVE ARTISTIC INTENT (SNARE) — The critical impulse that initiates performance cannot exit the absorption cycle. Once a transgressive aesthetic gains visibility, market forces immediately colonize it. The artist cannot reclaim the critical content once it has been stripped and commodified. Experiences maximum extraction with zero exit options — trapped within the very success that markets the work.
constraint_indexing:constraint_classification(irony_absorption_mechanism, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PARTICIPATING ARTIST-PERFORMER (TANGLED ROPE) — Constrained by material dependency on performance income and platform access, but also benefits from the visibility and participation opportunities the absorbed mechanism provides. Experiences genuine coordination (audience assembly, collaborative creativity) alongside extraction (critical message dilution, wage asymmetry). Has agency but at significant cost.
constraint_indexing:constraint_classification(irony_absorption_mechanism, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: COMMERCIAL INTERESTS (ROPE) — Bars, event promoters, media platforms experience the absorbed performance as pure coordination: assembling audiences, managing logistics, distributing entertainment. Extracts significant revenue while providing genuine service (venue, amplification, audience connection). Net beneficiary with full arbitrage options — can shift promotional themes or abandon performances when profitability declines.
constraint_indexing:constraint_classification(irony_absorption_mechanism, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: COUNTER-COMMODIFICATION MOVEMENTS (SCAFFOLD) — Organized activist groups (culture jamming networks, open-source aesthetics, community art collectives) see the absorption mechanism as a temporary structural problem with a sunset clause. Creating genuinely non-commodifiable performance forms (ephemeral, non-reproducible, explicitly anti-capitalist) and distributing performance codes openly bypasses the absorption pathway. These movements have agency and perceive an exit route through aesthetic innovation.
constraint_indexing:constraint_classification(irony_absorption_mechanism, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: CRITIQUE OF CAPITALISM AS INSTITUTIONAL RITUAL (PITON) — The language of 'subversion' and 'transgression' persists in marketing and cultural commentary as institutional performance despite having lost meaningful critical function. Brands invoke subversion rhetoric ('question authority,' 'disrupt the status quo') while selling conformity. The critical vocabulary becomes pure theater — the critique ritual maintains itself through inertia and marketing value, not functional transgression. Theater ratio approaches 1.0 when the critique vocabulary has been completely absorbed into the system it claims to oppose.
constraint_indexing:constraint_classification(irony_absorption_mechanism, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / STRUCTURAL INEVITABILITY (MOUNTAIN) — From a civilizational view, the absorption mechanism appears as an immutable property of market capitalism: all aesthetic resistance is eventually colonized and commodified. The 'cooptation cycle' is presented as natural law rather than contingent institutional arrangement. However, this perspective naturalizes what is actually an engineered coordination problem between artists, markets, and audiences — the engine will detect this as a false summit.
constraint_indexing:constraint_classification(irony_absorption_mechanism, mountain,
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
 *   Extractiveness (0.58): The constraint exhibits moderate-high extraction that increases over time. Initial extractiveness is low (0.22) because early subversive performances operate outside market logic entirely — the critical content is authentic and compensation is social/political rather than financial. As the performance becomes visible and commercially valuable, extractiveness rises. By year 12, the mechanism is well-established: critical content has been stripped, commercial benefit concentrated, and suppression of alternatives has solidified (0.58). The trajectory shows extraction accumulation — a characteristic signature of constraint degradation. Suppression (0.52): Moderate-high and rising. Initial suppression is low because subversive performances operate in explicitly counter-institutional spaces with high visibility (they want to be seen protesting conformity). Over time, suppression mechanisms strengthen: non-absorbed alternatives become harder to fund, distribute, or access; audiences internalize the absorbed version as 'the real thing'; performing subversive work outside the commercial frame becomes identity-threatening (artists fear losing platform access). Theater ratio (0.78): High and rising. The mechanism's distinguishing feature is that performative transgression intensifies while critical content disappears. Theater rises from 0.35 (genuine subversion, low performance content) to 0.78 (pure aesthetic performance, zero critical function). This is the opposite of piton decay (where function disappears but ritual persists) — here, the performance becomes MORE theatrical as the content becomes LESS critical. The irony inversion marks the absorption completion: 'subversion' is now purely ornamental.
 *
 * PERSPECTIVAL GAP:
 *   The snare/rope boundary separates the trapped artist (who cannot exit the visibility trap) from the commercial beneficiary (who can arbitrage away). The tangled rope classification for moderate participants reflects their dual position: they benefit from platform access and income but bear the cost of critical dilution. The scaffold perspective's sunset clause is testable: as alternative non-commodifiable performance forms mature (ephemeral art, open-source aesthetics, autonomous zone performances), the absorption mechanism's effectiveness should decline. The piton classification reveals that the critique of capitalism has been absorbed into the system as institutional theater — the language persists but the function has atrophied. The mountain classification risks presenting the absorption as inevitable when it is actually contingent on market structures, funding concentration, and audience awareness. The perspectival disagreement is not about different interpretations of shared facts but about experiencing structurally different constraints from different positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from structural position and exit options. The subversive impulse (powerless/trapped) experiences maximum extraction: d ≈ 0.95, giving f(d) ≈ 1.42. Participating artists (moderate/constrained) experience d ≈ 0.70, giving f(d) ≈ 1.05 — significant extraction but with some agency. Commercial interests (institutional/arbitrage) experience d ≈ 0.05, giving f(d) ≈ -0.12 — negative effective extraction because they are beneficiaries with exit options. The scaffold perspective's organized agents (organized/constrained) have d ≈ 0.50, giving f(d) ≈ 0.65 — symmetric position with both costs and benefits. The piton perspective's institutional ritual actors experience d ≈ 0.20, giving f(d) ≈ 0.02 — minimal effective extraction because their function is purely performative. These varying d values explain why the same base extractiveness (0.58) produces different classifications: chi = 0.58 × f(d) × σ(S), so institutions with arbitrage exit experience chi ≈ -0.07 (rope), while trapped artists experience chi ≈ 0.82 (snare). The perspectival gap is a direct consequence of differing directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY STRUCTURE: The constraint exhibits the core mandatrophy pattern — a mechanism that provides genuine coordination (assembling performers, creating venues, managing logistics) while extracting asymmetrically (concentrating financial benefit, diluting critical content, suppressing alternatives). The trap is that refusing participation looks like refusing legitimate services and community. Mandatrophy resolution requires identifying the specific structural feature that enables coordination while extracting: in this case, the market structure that monetizes visibility itself. Alternative institutional arrangements (cooperative ownership of venues, artist-controlled distribution, commons-based funding) would preserve the coordination function while eliminating the extraction mechanism. The constraint does NOT resolve into pure snare because commercial interests genuinely coordinate (they create real venues, assemble audiences, manage logistics). It does NOT resolve into pure rope because the financial benefit is asymmetrically distributed and critical intent is actively suppressed. It remains tangled rope: both functions are genuinely present, and both are structurally necessary. The mandatrophy is resolved by recognizing that the absorption mechanism is not inevitable but engineered — changing the institutional structure (ownership, distribution, funding) changes which content gets absorbed and which persists. SantaCon's commercial evolution was not spontaneous market discovery but active colonization: bars recognized the revenue opportunity and actively marketed the event, thereby actively suppressing its original counter-consumerist intent. The mechanism's continuation depends on sustained institutional work, not on some natural law of capitalism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intentional_ambiguity_as_strategy,
    'Is the ironic performance genuinely subversive, or does it deliberately exploit ambiguity to achieve commercial success while claiming critical intent?',
    'Artist interviews documenting intent; comparison of explicit statements vs market behavior; historical analysis of whether original creators resisted commodification or embraced it strategically',
    'If performers deliberately use ironic ambiguity as a cover story: the constraint is not absorption of subversion but performative resistance that benefits both artist and market (mutual extraction, not pure snare). If genuine subversion: the snare classification from the artist perspective is accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intentional_ambiguity_as_strategy, empirical, 'Whether ironic subversion is genuine or strategically ambiguous').

omega_variable(
    audience_complicity_and_agency,
    'Do audiences participating in absorbed performances experience the mechanism as extractive (being sold false transgression) or as genuine pleasure in performance, irony, and community?',
    'Ethnographic study of SantaCon participants and other absorbed performances; interviews about motivations, awareness of commercialization, felt experience of transgression',
    'If audiences feel genuine transgression: the mechanism is coordination with partial theater, not pure extraction. If audiences are aware of the commodification and participate anyway: the mechanism is consensual commodification, not snare. If audiences are unaware: the snare classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(audience_complicity_and_agency, empirical, 'Whether audiences experience absorbed performance as extraction or pleasure').

omega_variable(
    counterculture_recycling_periodicity,
    'Is there a measurable time constant for how long subversive aesthetics retain critical force before market absorption becomes visible?',
    'Historical timeline analysis across multiple performance genres (punk, hip-hop, rave, meme culture); measurement of lag between emergence and first commercial imitation; correlation with media cycle acceleration',
    'If time constant is shrinking (faster absorption in recent decades): the mechanism is accelerating and extraction is increasing. If constant or stable: the mechanism is structural but not intensifying. Short time constant (<2 years) suggests intentional market strategy; long constant (>10 years) suggests organic market discovery.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterculture_recycling_periodicity, empirical, 'Time lag between subversive emergence and market absorption').

omega_variable(
    suppression_mechanism_structure,
    'Is suppression of non-absorbed subversive alternatives structural (market exclusion, capital requirements) or normalized (performers and audiences have internalized the absorbed version as ''the real thing'')?',
    'Comparison of access barriers: cost to mount non-commercial performance vs absorbed version; visibility analysis (media coverage, social media amplification); interviews about whether performers perceive non-absorbed alternatives as viable',
    'If suppression is primarily structural: different policies (funding allocation, platform algorithm) could reduce extraction without changing artist behavior. If primarily normalized: the mechanism is identity-locked (artists and audiences have internalized the absorbed version as inevitable), requiring cognitive reframing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structure, empirical, 'Whether suppression of non-absorbed alternatives is structural or internalized').

omega_variable(
    false_summit_natural_law_claim,
    'Is the absorption mechanism a natural law of capitalism, or a contingent institutional arrangement that depends on specific market structures and can be redesigned?',
    'Comparative analysis of non-capitalist cultural production systems; historical periods where subversive performance persisted without absorption; examination of open-source, community-owned, and cooperative cultural production models',
    'If truly natural law: all subversive aesthetics will eventually be absorbed regardless of context. If contingent: alternative institutional structures (cooperative ownership, commons-based performance, autonomous zones) could maintain critical force. This omega directly tests whether the mountain perspective is a genuine natural law or a false summit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_law_claim, conceptual, 'Whether absorption is natural law or contingent institutional arrangement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(irony_absorption_mechanism, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(irony_tr_t0, irony_absorption_mechanism, theater_ratio, 0, 0.35).
narrative_ontology:measurement(irony_tr_t3, irony_absorption_mechanism, theater_ratio, 3, 0.52).
narrative_ontology:measurement(irony_tr_t7, irony_absorption_mechanism, theater_ratio, 7, 0.68).
narrative_ontology:measurement(irony_tr_t12, irony_absorption_mechanism, theater_ratio, 12, 0.78).

% Extraction over time
narrative_ontology:measurement(irony_be_t0, irony_absorption_mechanism, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(irony_be_t3, irony_absorption_mechanism, base_extractiveness, 3, 0.38).
narrative_ontology:measurement(irony_be_t7, irony_absorption_mechanism, base_extractiveness, 7, 0.51).
narrative_ontology:measurement(irony_be_t12, irony_absorption_mechanism, base_extractiveness, 12, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(irony_su_t0, irony_absorption_mechanism, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(irony_su_t7, irony_absorption_mechanism, suppression_requirement, 7, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(irony_absorption_mechanism, identity_coordination).
narrative_ontology:affects_constraint(irony_absorption_mechanism, commodification_of_dissent).
narrative_ontology:affects_constraint(irony_absorption_mechanism, cultural_commons_erosion).

% DUAL FORMULATION NOTE:
% The irony absorption mechanism is one reading of a larger constraint family around cultural commodification. Separate stories track: (1) the absorption mechanism itself (this story) — focusing on the performative/critical dynamics; (2) commodification of dissent — focusing on the political economic structures enabling market colonization; (3) cultural commons erosion — focusing on the loss of non-commodifiable aesthetic spaces. These are linked by network edges but represent structurally distinct constraints with different ε values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(irony_absorption_mechanism, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
