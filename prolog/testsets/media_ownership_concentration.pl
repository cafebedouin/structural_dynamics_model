% ============================================================================
% CONSTRAINT STORY: media_ownership_concentration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_media_ownership_concentration, []).

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
 *   constraint_id: media_ownership_concentration
 *   human_readable: Media Ownership Concentration and Editorial Control
 *   domain: media/political_economy
 *
 * SUMMARY:
 *   Media ownership concentration represents a structural constraint where
 *   consolidation of news production under corporate ownership creates
 *   asymmetric extraction between owners (who capture editorial control and
 *   political influence) and workers/public (who lose editorial autonomy and
 *   information access). The constraint exhibits simultaneous coordination
 *   functions (consolidated outlets enable economies of scale, unified
 *   editorial strategy, professional standards infrastructure) and extractive
 *   mechanisms (suppression of inconvenient stories, political alignment with
 *   corporate interests, elimination of independent outlets that would
 *   compete). The base extractiveness value (0.58) reflects that
 *   consolidation genuinely does solve some coordination problems for media
 *   corporations while simultaneously extracting from journalists,
 *   communities, and the information commons. The suppression value (0.65)
 *   captures both structural barriers (capital requirements for launching
 *   independent outlets) and behavioral suppression (editorial directives,
 *   career consequences for publishing disfavored stories). Theater ratio
 *   (0.48) reflects that professional journalism standards (fact-checking,
 *   editorial processes) continue but increasingly serve to legitimize
 *   corporate narratives rather than provide genuine independence—a moderate
 *   level of performativity, not yet the high theater of purely degraded
 *   institutions. This constraint decomposes into at least two structurally
 *   distinct stories: (1) the market consolidation itself (economic
 *   extraction through competitive elimination) and (2) the editorial
 *   suppression mechanism (information extraction through narrative control).
 *   The narrative focuses on editorial suppression; market consolidation
 *   would be a separate story with potentially higher extractiveness.
 *
 * KEY AGENTS:
 *   - Large Media Corporations: Primary beneficiary (institutional/arbitrage) — capture editorial control, political access, and market rents through consolidated ownership
 *   - Independent Journalists: Primary victim (powerless/trapped) — face elimination of employment pathways, editorial censorship, career suppression
 *   - Local Communities: Primary victim (powerless/trapped) — loss of local accountability reporting, information deserts, no exit capacity
 *   - Mid-Level Editorial Staff: Secondary victim (moderate/constrained) — benefit from professional infrastructure while constrained by ownership directives; can exit at cost
 *   - Political Elite with Media Access: Moderate beneficiary (powerful/mobile) — benefit from narrative amplification while maintaining exit options through alternative channels
 *   - Journalism Profession: Institutional actor (institutional/arbitrage) — maintains professional norms theatrically while structural basis has degraded
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(media_ownership_concentration, 0.58).
domain_priors:suppression_score(media_ownership_concentration, 0.65).
domain_priors:theater_ratio(media_ownership_concentration, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(media_ownership_concentration, extractiveness, 0.58).
narrative_ontology:constraint_metric(media_ownership_concentration, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(media_ownership_concentration, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(media_ownership_concentration, tangled_rope).
narrative_ontology:human_readable(media_ownership_concentration, "Media Ownership Concentration and Editorial Control").
narrative_ontology:topic_domain(media_ownership_concentration, "media/political_economy").

domain_priors:requires_active_enforcement(media_ownership_concentration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(media_ownership_concentration, large_media_corporations).
narrative_ontology:constraint_beneficiary(media_ownership_concentration, political_elite_with_media_access).
narrative_ontology:constraint_victim(media_ownership_concentration, independent_journalists).
narrative_ontology:constraint_victim(media_ownership_concentration, public_information_commons).
narrative_ontology:constraint_victim(media_ownership_concentration, local_news_infrastructure).
narrative_ontology:constraint_victim(media_ownership_concentration, editorial_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDEPENDENT JOURNALIST (SNARE) — Faces near-total career barriers: consolidation has eliminated independent outlets and closed editorial positions. Must either accept corporate employment under editorial constraints or abandon journalism entirely. No viable alternative pathways. Maximum extraction: cannot negotiate terms, cannot exit the constraint, cannot organize sufficient collective power. The journalist's labor is extracted through wage suppression and editorial censorship masked as market forces.
constraint_indexing:constraint_classification(media_ownership_concentration, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LOCAL COMMUNITY (SNARE) — Concentration has eliminated local news coverage; communities cannot organize alternative information systems quickly enough. Trapped in information deserts. No exit without decades of collective effort. Extracted information value (local accountability reporting) has been removed from the constraint system. Pure extraction with no coordination benefit.
constraint_indexing:constraint_classification(media_ownership_concentration, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: MID-LEVEL EDITORIAL STAFF (TANGLED ROPE) — Experiences both coordination and extraction. Benefits from professional infrastructure (fact-checking resources, editorial standards, distribution networks) that consolidation enables. Simultaneously constrained by ownership directives, editorial suppression of stories that conflict with corporate interests, and career advancement tied to corporate preference. Exit is costly (reputation damage, industry blacklisting) but not impossible for highly skilled journalists. Moderate effective extraction masks genuine coordination function.
constraint_indexing:constraint_classification(media_ownership_concentration, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: LARGE MEDIA CORPORATION (ROPE) — Consolidation solves the coordination problem of managing multiple editorial voices under unified business strategy. The corporation experiences the constraint as pure coordination: centralizing ownership reduces editorial conflict, enables economies of scale in newsgathering, and allows strategic narrative alignment. Net beneficiary with complete exit capacity (can divest, can exit markets). No suppression experienced at this level — constraints operate downward, not upward.
constraint_indexing:constraint_classification(media_ownership_concentration, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: POLITICAL ELITE WITH MEDIA ACCESS (TANGLED ROPE) — Experiences mixed coordination and extraction. Benefits from consolidated media's ability to amplify preferred narratives (coordination function: organized message distribution). Simultaneously faces extraction when media ownership aligns with opposing political factions. Mobile exit option (can cultivate alternative media channels, social media platforms, direct communication). Experienced extraction is moderate because powerful agents can negotiate and exit.
constraint_indexing:constraint_classification(media_ownership_concentration, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: JOURNALISM PROFESSION AS INSTITUTION (PITON) — The professional norms of journalism (objectivity, editorial independence, public interest) persist despite concentration eroding the structural basis for these norms. Theater ratio 0.48 reflects that professional rituals (editorial standards, fact-checking protocols) continue but increasingly serve to legitimize corporate narratives rather than provide genuine editorial independence. The profession maintains theatrical adherence to independence ideals while material conditions of work have fundamentally changed. Inertia keeps the professional frame intact despite functional degradation.
constraint_indexing:constraint_classification(media_ownership_concentration, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (FALSE MOUNTAIN) — From a civilizational/global perspective, media consolidation might appear as an inevitable consequence of capital accumulation and technology (network effects favor large platforms, economies of scale favor large newsrooms, etc.). This perspective risks naturalizing what is actually a contingent institutional arrangement dependent on regulatory choices (antitrust law, licensing regimes, ownership caps). The engine will identify this as a false summit: consolidation is structurally contingent, not a law of nature or technology.
constraint_indexing:constraint_classification(media_ownership_concentration, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(media_ownership_concentration_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(media_ownership_concentration, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(media_ownership_concentration, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(media_ownership_concentration, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(media_ownership_concentration, TR),
    TR >= 0.70.

:- end_tests(media_ownership_concentration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts through dual mechanisms: (1) elimination of competitive alternatives (powerless journalists have no exit), and (2) editorial control (consolidated outlets suppress narratives that contradict corporate interests). The value reflects that consolidation solves some genuine coordination problems (economies of scale, professional standards) for the beneficiary while simultaneously creating extraction mechanisms. The measurement trajectory (0.30→0.58 over 15 time periods) shows increasing extractiveness as consolidation matured and professional norms were incrementally compromised. Suppression (0.65): High. Significant structural barriers include: capital requirements for launching independent outlets (billions required for national reach), advertising market consolidation favoring large outlets, journalistic labor market concentration (fewer independent employers), career consequences for publishing disfavored stories (blacklisting, editorial suppression, compensation reduction). Some exit exists (freelance work, nonprofit outlets, digital platforms) but at substantial cost. Theater ratio (0.48): Moderate. Professional journalism maintains genuine editorial standards (fact-checking, source verification) that serve legitimate verification functions. However, these same standards increasingly serve to legitimize corporate-preferred narratives rather than provide genuine editorial independence. The theater is neither minimal (standards are materially enforced) nor maximal (some investigative capacity persists), positioning the constraint as mid-stage degradation rather than pure piton. The measurement trajectory (0.32→0.48) shows theater increasing as professional norms became decoupled from their original function.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp perspectival divergence from structural position. The beneficiary (large media corporation) sees pure coordination (Rope) — consolidation solves the engineering problem of managing multiple editorial voices and distributing content profitably. The trapped agent (independent journalist) sees pure extraction (Snare) — no career alternatives, editorial censorship, professional suppression. The constrained agent (mid-level staff) sees mixed coordination and extraction (Tangled Rope) — benefits from professional infrastructure while oppressed by ownership directives. The organized agent (open media advocacy movements) sees a structural problem with potential policy solutions (Scaffold with sunset) — antitrust intervention could break consolidation. The profession maintains theatrical commitment to independence (Piton) — professional norms persist despite material conditions for independence have eroded. The analytical observer risks false naturalization (Mountain) — mistaking a contingent institutional arrangement for an inevitable consequence of technology or capital. The perspectival gap reflects real structural differences: powerful agents with exit options experience weak extraction; powerless agents with no exit experience maximum extraction. The constraint is not Mountain (not inevitable), not Rope (coordination is present but not primary function), not pure Snare (beneficiaries genuinely gain from coordination, not just from extraction), and not pure Scaffold (the sunset requires active policy intervention, not inevitable technological replacement). Tangled Rope is the structural truth: consolidation coordinates corporate operations while extracting from journalists, communities, and editorial autonomy.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from (1) agent power level, (2) exit options within this specific constraint, and (3) beneficiary/victim status. Large media corporations: institutional power + arbitrage exit + beneficiary status → d ≈ 0.05 → low/negative χ. Independent journalists: powerless + trapped exit + victim status → d ≈ 0.95 → maximum χ. Mid-level editors: moderate power + constrained exit + victim status (constrained by suppression) → d ≈ 0.70 → moderate-high χ. Political elite with media access: powerful power + mobile exit + partial beneficiary status → d ≈ 0.30-0.40 → low-moderate χ. The derivation chain prioritizes structural data (beneficiary/victim declarations, exit options) over power atom alone. A powerful agent with trapped exit experiences higher extraction than their power level suggests; a powerless agent with arbitrage exit experiences lower extraction than their power level suggests.
 *
 * MANDATROPHY ANALYSIS:
 *   Tangled Rope classification resolves potential mandatrophy by recognizing that consolidation creates BOTH a genuine coordination function AND asymmetric extraction. The false choice between 'pure coordination problem (Rope)' and 'pure extraction mechanism (Snare)' is dissolved by the Tangled Rope category, which requires: (1) genuine coordination function (consolidated outlets do solve editorial coordination and distribution problems), (2) asymmetric extraction (these same consolidations eliminate competitive alternatives and suppress editorial independence), and (3) active enforcement of asymmetry (ownership structures must be actively maintained, not spontaneous). The Piton perspective notes that professional norms (objectivity, editorial independence) persist theatrically despite material conditions supporting those norms have degraded — this is the degradation signature. The false Mountain perspective (consolidation is inevitable technology effect) is prevented by recognizing the constraint as regulatory/institutional contingent, not technological necessity. The Scaffold perspective (antitrust intervention creates sunset) is legitimate but secondary to the Tangled Rope structural reality — policy intervention might break the constraint but the constraint currently exists as Tangled Rope, not as an incipient Scaffold.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    editorial_suppression_mechanism,
    'Is observed editorial convergence driven by ownership control or by genuine coordination of professional standards?',
    'Cross-ownership comparison of editorial positions on politically contentious stories; analysis of editorial variance within consolidated outlets vs across independent outlets; journalist testimony regarding editorial pressures.',
    'If ownership-driven: suppression value should increase to 0.75+, classification shifts toward pure Snare for powerless agents. If standards-driven: suppression value should decrease to 0.40-0.45, constraint reclassifies as weaker Tangled Rope or Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(editorial_suppression_mechanism, empirical, 'Whether editorial suppression is structurally required or contingent on ownership').

omega_variable(
    alternative_media_viability,
    'Do digital platforms and independent online media constitute genuine alternatives to consolidated outlets, or are they structurally unable to replace local news function?',
    'Longitudinal tracking of audience migration to alternative outlets; analysis of whether alternative outlets generate investigative journalism or only aggregate/comment; measurement of local accountability reporting volume before/after consolidation.',
    'If alternatives viable: exit_options for journalists should shift from ''trapped'' to ''constrained'', reducing effective extraction. If alternatives insufficient: confirms snare classification and justifies higher suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_media_viability, empirical, 'Whether alternative media can replace consolidated outlets'' social function').

omega_variable(
    regulatory_intervention_feasibility,
    'Can antitrust action or ownership caps reversibly break the consolidation pattern, or has it reached structural irreversibility?',
    'Historical case analysis of post-antitrust divestiture outcomes; modeling of market viability for independent local news under different regulatory regimes; journalist labor market analysis.',
    'If reversible: constraint is structural but contingent (Tangled Rope confirmed, potential for Scaffold with sunset). If irreversible: constraint approaches Mountain-like immutability through accumulated capital barriers.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_intervention_feasibility, empirical, 'Whether media consolidation is reversible through policy intervention').

omega_variable(
    suppression_internalization,
    'To what degree have journalists internalized editorial suppression as professional norm rather than experienced it as external coercion?',
    'Qualitative analysis of journalist self-narratives; comparison of suppression experienced by hired journalists vs independent/freelance journalists; tracking of self-censorship patterns.',
    'High internalization shifts the binding mechanism from structural (career consequences) toward identity-locked (professional identity fused with corporate expectations). Increases effective suppression beyond measured value because the journalist carries constraints internally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Degree of internalization of editorial suppression as professional norm').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(media_ownership_concentration, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mediacon_tr_t0, media_ownership_concentration, theater_ratio, 0, 0.32).
narrative_ontology:measurement(mediacon_tr_t5, media_ownership_concentration, theater_ratio, 5, 0.38).
narrative_ontology:measurement(mediacon_tr_t10, media_ownership_concentration, theater_ratio, 10, 0.45).
narrative_ontology:measurement(mediacon_tr_t15, media_ownership_concentration, theater_ratio, 15, 0.48).

% Extraction over time
narrative_ontology:measurement(mediacon_be_t0, media_ownership_concentration, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(mediacon_be_t5, media_ownership_concentration, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(mediacon_be_t10, media_ownership_concentration, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(mediacon_be_t15, media_ownership_concentration, base_extractiveness, 15, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(media_ownership_concentration, resource_allocation).
narrative_ontology:affects_constraint(media_ownership_concentration, editorial_suppression_of_political_narratives).
narrative_ontology:affects_constraint(media_ownership_concentration, local_news_market_elimination).
narrative_ontology:affects_constraint(media_ownership_concentration, journalistic_labor_precarity).

% DUAL FORMULATION NOTE:
% Media ownership concentration decomposes into at least three distinct constraints: (1) editorial_suppression_of_political_narratives (ε ≈ 0.65, Snare/Tangled Rope at editorial level), (2) local_news_market_elimination (ε ≈ 0.72, Snare at community level, higher extractiveness), and (3) journalistic_labor_precarity (ε ≈ 0.60, Snare/Tangled Rope at labor market level). This story focuses on the editorial suppression mechanism (ε=0.58, Tangled Rope). The upstream constraint is market concentration itself (economic extraction through competitive elimination); downstream constraints involve specific manifestations of suppressed narratives and labor precarity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(media_ownership_concentration, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
