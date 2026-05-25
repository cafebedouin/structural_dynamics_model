% ============================================================================
% CONSTRAINT STORY: taste_formation_monopoly
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_taste_formation_monopoly, []).

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
 *   constraint_id: taste_formation_monopoly
 *   human_readable: Taste Formation Monopoly: Cultural Gatekeeping and Preference Construction
 *   domain: cultural_political_economy
 *
 * SUMMARY:
 *   Taste formation monopoly describes the structural control exercised by
 *   cultural gatekeepers — critics, curators, algorithm designers, award
 *   bodies, prestigious institutions — over what is recognized as 'good
 *   taste' and therefore what gains visibility, value, and legitimacy. This
 *   constraint exhibits the distinctive signature of a tangled rope: genuine
 *   coordination function (establishing shared aesthetic standards that
 *   enable coherent cultural discourse) combined with asymmetric extraction
 *   (gatekeepers accumulate cultural capital and economic benefit while
 *   emerging creators and non-canonical preferences bear the cost of
 *   exclusion). The constraint has become increasingly visible as digital
 *   platforms and decentralized systems have made it possible to imagine
 *   alternatives. The theater ratio (0.64) reflects that much critical and
 *   curatorial activity is now performative legitimation — establishment
 *   critics maintain ceremonial gatekeeping (awards, reviews, canonical
 *   lists) with diminishing actual influence over what audiences consume.
 *   Meanwhile, emerging creators are trapped in a system where visibility and
 *   credibility depend on gatekeeper validation, but validation is scarce and
 *   preferentially directed toward work that conforms to established
 *   aesthetic categories. Niche communities with non-canonical preferences
 *   experience the constraint as both coordinating (their shared taste builds
 *   community identity) and extractive (their aesthetic innovations are
 *   harvested for trend data without credit). Decentralized alternatives
 *   (social media algorithms, creator cooperatives, blockchain reputation
 *   systems) are building scaffolds that could enable taste formation without
 *   gatekeepers, but these are still nascent and have not yet achieved the
 *   legitimacy or coordination capacity of traditional institutions.
 *
 * KEY AGENTS:
 *   - Emerging Creators: Primary victims (powerless/trapped) — need gatekeeper validation for visibility and credibility; face circular dependency (work gains value by being recognized as good taste, but only gatekeepers determine what counts as good taste)
 *   - Niche Communities: Secondary victims (moderate/constrained) — coordinate through shared taste but experience extraction as gatekeepers mine their preferences for trend data and appropriate aesthetic innovations
 *   - Taste Arbiter Institutions: Primary beneficiaries (institutional/arbitrage) — museums, awards bodies, publications, algorithms that distribute cultural authority; experience constraint as coordination mechanism
 *   - Decentralized Creator Networks: Organized alternative (organized/constrained) — social media, artist collectives, blockchain-based credentialing building exit pathways from gatekeeper monopoly
 *   - Critical Establishment: Institutional performer (institutional/arbitrage) — maintains performative gatekeeping with declining actual influence; increasingly piton-like (theater-driven maintenance of authority)
 *   - Consumer Autonomy: Diffuse victim — abstract capacity for independent taste formation is subordinated to gatekeeper-curated preference discovery
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(taste_formation_monopoly, 0.58).
domain_priors:suppression_score(taste_formation_monopoly, 0.68).
domain_priors:theater_ratio(taste_formation_monopoly, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(taste_formation_monopoly, extractiveness, 0.58).
narrative_ontology:constraint_metric(taste_formation_monopoly, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(taste_formation_monopoly, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(taste_formation_monopoly, tangled_rope).
narrative_ontology:human_readable(taste_formation_monopoly, "Taste Formation Monopoly: Cultural Gatekeeping and Preference Construction").
narrative_ontology:topic_domain(taste_formation_monopoly, "cultural_political_economy").

domain_priors:requires_active_enforcement(taste_formation_monopoly).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(taste_formation_monopoly, cultural_gatekeepers).
narrative_ontology:constraint_beneficiary(taste_formation_monopoly, incumbent_aesthetic_arbiters).
narrative_ontology:constraint_victim(taste_formation_monopoly, emerging_creators).
narrative_ontology:constraint_victim(taste_formation_monopoly, non_canonical_preferences).
narrative_ontology:constraint_victim(taste_formation_monopoly, consumer_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ASPIRING CREATOR (SNARE) — Faces structural barriers to having work recognized as having 'good taste'. The gatekeepers (critics, curators, influencers, algorithms) control access to visibility. No legitimate alternative pathway exists to establish credibility outside the gatekeeper system. The creator is trapped by resource dependency (need distribution platform), reputation dependency (need institutional validation), and the circular logic that work gains value by being recognized as good taste, but only gatekeepers determine what counts as good taste.
constraint_indexing:constraint_classification(taste_formation_monopoly, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: NICHE COMMUNITY (TANGLED ROPE) — Communities with non-canonical preferences coordinate through shared taste, which is genuine coordination. But they also experience extraction: their preferences are mined for trend data, their aesthetic innovations are absorbed into mainstream without credit or compensation, and their cultural capital is appropriated by gatekeepers. Exit is costly (requires finding alternative communities, building new distribution infrastructure) but possible. The constraint provides coordination services (community identity, taste coherence) alongside asymmetric extraction of cultural innovation.
constraint_indexing:constraint_classification(taste_formation_monopoly, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: TASTE ARBITER INSTITUTION (ROPE) — Museums, awards bodies, prestigious publications, algorithm designers, and influential critics experience the constraint as pure coordination: establishing shared aesthetic standards enables coherent cultural discourse and provides trusted signals to consumers. They have arbitrage options (can redirect their validation authority to different creators or aesthetic movements if incentives shift). The constraint appears as coordination mechanism, not extraction.
constraint_indexing:constraint_classification(taste_formation_monopoly, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DECENTRALIZED CREATOR NETWORK (SCAFFOLD) — Social media platforms, artist collectives, blockchain-based credentialing systems (NFTs, DAO governance), and algorithmic recommendation diversity are building alternative taste-formation mechanisms that bypass traditional gatekeepers. These represent temporary scaffolds with a real sunset clause: as decentralized reputation systems mature and network effects shift, the gatekeeper monopoly loses enforcement power. Low effective extraction because organized agents have visible exit pathways and declining cost to use them.
constraint_indexing:constraint_classification(taste_formation_monopoly, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: CRITICAL ESTABLISHMENT (PITON) — The institution of professional criticism, formalized taste adjudication, and canon maintenance persists largely through inertia and theater. Critics perform expertise and cultural authority at a scale that no longer corresponds to their actual influence (many consumers ignore critics; algorithms now drive preference discovery). The establishment maintains performative gatekeeping — reviews, awards, curatorial selections — but these are increasingly decorative rather than determinative. Theater ratio high (0.64) because much critical activity is performative legitimation rather than functional taste formation.
constraint_indexing:constraint_classification(taste_formation_monopoly, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, all taste formation involves some gatekeeping: coherent cultural discourse requires shared standards, which means some preferences are elevated and others subordinated. This perspective sees taste monopoly as an immutable feature of any cultural system — you cannot have taste without gatekeepers, and gatekeepers always extract. However, this risks naturalizing contingent institutional arrangements (the specific gatekeepers, the specific extraction mechanisms, the specific barriers to alternative taste systems) as inherent to culture itself. The engine will likely flag this as a false summit.
constraint_indexing:constraint_classification(taste_formation_monopoly, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(taste_formation_monopoly_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(taste_formation_monopoly, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(taste_formation_monopoly, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(taste_formation_monopoly, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(taste_formation_monopoly, TR),
    TR >= 0.70.

:- end_tests(taste_formation_monopoly_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The gatekeepers extract significant value by controlling visibility and credibility. Emerging creators cannot reach audiences without validation; niche communities cannot monetize innovations they have created; cultural capital accumulates to institutional arbiters. But this is not maximal extraction — alternative systems (social media, peer recommendation) do exist and are growing; consumers increasingly ignore gatekeepers; and gatekeepers themselves are capturing less total cultural authority than they did 20 years ago. The value has risen from 0.42 to 0.58 over the interval because gatekeepers have become more concentrated (algorithm consolidation) and more actively enforced (editorial curation, platform policy). Suppression (0.68): High. Significant barriers prevent emerging creators from building credibility outside the gatekeeper system. These include: resource barriers (need funding for professional production, distribution infrastructure), reputation barriers (credibility is defined as gatekeeper recognition), structural barriers (algorithm design privileges institutional content), and normative barriers (prevailing expectation that legitimacy comes from canonical institutions). But suppression is not total — alternative pathways have grown; internet infrastructure enables direct-to-audience work; niche communities have built self-sustaining reputation systems. Theater ratio (0.64): Moderate-high. Much gatekeeper activity is performative: critical reviews for audiences who don't read reviews; awards ceremonies for prestige; curatorial selections that are aesthetically conservative yet ceremonially innovative. The gap between performed authority and actual influence has widened as algorithms and social media have displaced traditional gatekeeping. The theater ratio has increased from 0.48 to 0.64 because gatekeeping institutions have increasingly maintained their authority through performative legitimation (awards, critical acclaim) rather than functional influence (determining what audiences actually consume).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural phenomenon appears as immutable law (mountain), pure coordination (rope), mixed coordination-extraction (tangled rope), pure extraction (snare), theatrical maintenance (piton), and scaffolded exit (scaffold) depending on the observer's structural position. The gatekeeper sees coordination (rope) — establishing aesthetic standards is a legitimate social function. The decentralized network sees a temporary problem with a sunset (scaffold) — alternative taste systems are becoming viable. The critical establishment sees its own degraded authority (piton) — much of what it does is ceremonial. Niche communities see both benefits (coordination of shared taste) and costs (extraction of innovation). Emerging creators see pure extraction (snare) — they cannot escape the gatekeeper dependency. The civilizational analytical observer risks seeing naturalized necessity (mountain) — 'all cultures have gatekeepers' — but the structural data reveals this as a false summit: the specific gatekeepers, the specific mechanisms of extraction, and the specific barriers to alternatives are contingent institutions, not laws of nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from agent power, exit options, and beneficiary/victim status. Aspiring creators (powerless/trapped) experience maximum d ≈ 0.95, producing high f(d) and experiencing χ as snare. Niche communities (moderate/constrained) have moderate d ≈ 0.60, experiencing mixed coordination and extraction. Taste arbiters (institutional/arbitrage) have low d ≈ 0.10, experiencing negative effective extraction (they benefit). Decentralized networks (organized/constrained) have d ≈ 0.45, experiencing moderate extraction with visible exit pathways. Critical establishment (institutional/arbitrage) has low d but high theater_ratio, producing piton classification. The analytical observer uses canonical d ≈ 0.72 for the universal perspective, but the false summit detector should flag the mountain classification as naturalized.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that taste formation monopoly is a hybrid constraint with genuine coordination function (establishing shared aesthetic standards that enable cultural coherence) combined with asymmetric extraction (gatekeepers accumulate benefits while emerging creators and non-canonical preferences bear costs). The constraint is not pure coordination (rope) because gatekeepers actively maintain barriers to alternative taste systems and extract cultural capital. The constraint is not pure extraction (snare) because the gatekeeper system does perform a real coordination function — without some shared aesthetic standards, cultural discourse would fragment into incoherent babble. The constraint is tangled rope because both the coordination function and the extraction are structural: you cannot have the benefits of shared aesthetic standards without some gatekeeping, but the current institutional arrangement extracts more than the minimum necessary coordination cost. The tangled rope classification also explains why the constraint persists despite obvious unfairness: it does solve a real problem (coordination of cultural discourse), which makes it seem justified (the gatekeeper narrative). But the solution is not the only possible solution — decentralized systems could provide coordination without the extraction. The mandatrophy dissolves when we recognize that the constraint coordinates (tangled rope) but could be restructured to extract less (scaffold with sunset, or pure rope if alternatives mature).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    gatekeeping_necessity,
    'Is taste gatekeeping a structural necessity (immutable coordination requirement) or a contingent institutional arrangement?',
    'Comparative analysis of taste-formation mechanisms across cultures and historical periods; assessment of decentralized systems (algorithmic recommendation, peer rating, blockchain reputation) for their capacity to coordinate coherent aesthetics without centralized gatekeepers',
    'If necessary (mountain): taste monopoly cannot be eliminated, only managed for fairness. If contingent (snare/tangled rope): alternative systems are possible and the current gatekeeping is extractive rather than inherent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gatekeeping_necessity, conceptual, 'Whether taste gatekeeping is structural necessity or contingent arrangement').

omega_variable(
    decentralization_efficacy,
    'Can decentralized taste-formation systems (social media algorithms, peer rating, blockchain credentialing) actually sustain coherent aesthetics and cultural coordination, or do they fragment into isolated preference bubbles?',
    'Empirical observation of decentralized systems'' ability to generate shared aesthetic standards, cross-bubble communication, and cultural coherence over 10+ year horizons; measurement of taste diversity vs fragmentation',
    'If coherent: scaffold sunset is real and gatekeeper monopoly has a genuine extinction timeline. If fragmentation: decentralized systems fail at coordination function, and gatekeepers retain essential role (constraint reverts to pure coordination/rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decentralization_efficacy, empirical, 'Whether decentralized taste systems can sustain cultural coherence').

omega_variable(
    extraction_mechanism_identity,
    'Is the primary extraction mechanism resource scarcity (limited validation slots, finite institutional attention) or active suppression of alternatives (gatekeepers actively preventing decentralized systems)?',
    'Process tracing of gatekeeper behavior; analysis of institutional investment in traditional systems vs opposition to alternative systems; measurement of suppression intensity (regulatory barriers, platform policy enforcement, normative delegitimation)',
    'If scarcity: extraction is largely passive/structural (harder to change). If active suppression: gatekeepers are enforcing the monopoly and extraction is more easily disrupted by removing enforcement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_mechanism_identity, empirical, 'Whether extraction is passive scarcity or active suppression').

omega_variable(
    appropriation_without_attribution,
    'Do niche communities actually experience sustained extraction of cultural innovation without credit or compensation, or is the ''appropriation'' diffusion and evolution that is inherent to cultural processes?',
    'Longitudinal tracing of aesthetic innovations from niche origin to mainstream adoption; quantification of creator attribution, economic compensation, and credit flows; comparison with prior eras of cultural evolution',
    'If sustained extraction: victims classification is accurate and constraint is asymmetric extraction. If natural diffusion: ''appropriation'' framing may anthropomorphize collective cultural evolution and misidentify victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(appropriation_without_attribution, empirical, 'Whether cultural innovation appropriation is extraction or natural diffusion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(taste_formation_monopoly, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(taste_tr_t0, taste_formation_monopoly, theater_ratio, 0, 0.48).
narrative_ontology:measurement(taste_tr_t10, taste_formation_monopoly, theater_ratio, 10, 0.56).
narrative_ontology:measurement(taste_tr_t20, taste_formation_monopoly, theater_ratio, 20, 0.64).

% Extraction over time
narrative_ontology:measurement(taste_be_t0, taste_formation_monopoly, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(taste_be_t10, taste_formation_monopoly, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(taste_be_t20, taste_formation_monopoly, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(taste_formation_monopoly, identity_coordination).
narrative_ontology:boltzmann_floor_override(taste_formation_monopoly, 0.12).
narrative_ontology:affects_constraint(taste_formation_monopoly, algorithmic_curation_bias).
narrative_ontology:affects_constraint(taste_formation_monopoly, cultural_appropriation_asymmetry).
narrative_ontology:affects_constraint(taste_formation_monopoly, creator_economic_dependency).

% DUAL FORMULATION NOTE:
% Taste formation monopoly is upstream of several more specific constraints: algorithmic curation bias (the mechanism by which taste monopoly enforces visibility hierarchies), cultural appropriation asymmetry (the mechanism by which gatekeepers extract innovation without attribution), and creator economic dependency (the mechanism by which emerging creators become trapped). Each downstream constraint has its own epsilon and can be analyzed separately, but they all depend on the underlying taste monopoly constraint for their structural coherence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(taste_formation_monopoly, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
