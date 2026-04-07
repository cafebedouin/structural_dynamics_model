% ============================================================================
% CONSTRAINT STORY: allusive_density_as_exclusion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_allusive_density_as_exclusion, []).

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
 *   constraint_id: allusive_density_as_exclusion
 *   human_readable: Allusive Density as Cultural Capital Barrier in Modernist Poetry
 *   domain: literary_criticism/modernist_poetry/cultural_theory
 *
 * SUMMARY:
 *   The Waste Land's allusive density — spanning 7+ languages, 30+ source
 *   texts from classical antiquity through contemporary popular culture,
 *   requiring extensive scholarly apparatus for comprehension — functions as
 *   a cultural capital barrier that structures access to modernist literary
 *   culture. Published in 1922, the poem's interpretive difficulty was
 *   immediately recognized as both aesthetic achievement and exclusionary
 *   mechanism. The constraint exhibits extraction that has intensified over
 *   the century as the cultural references have become more obscure and the
 *   scholarly apparatus has proliferated. What began as a coordination
 *   mechanism (synthesizing fragmented modern experience through literary
 *   tradition) has accumulated extractive overhead as the apparatus
 *   requirement has grown. The theater_ratio reflects the degree to which
 *   scholarly production about the poem has become self-referential: much
 *   contemporary criticism addresses other criticism rather than the poem
 *   itself, and the apparatus has become a credentialing ritual rather than
 *   an interpretive aid. Digital humanities projects and open-access
 *   annotations represent an alternative pathway, but educational
 *   prerequisites persist — the tools cannot substitute for the cultural
 *   formation that elite institutions provide.
 *
 * KEY AGENTS:
 *   - Non-Specialist Readers: Primary victims (powerless/trapped) — excluded from interpretive participation by educational barriers and apparatus requirements; cultural capital flows away from this position
 *   - Autodidact Readers: Secondary victims (moderate/constrained) — can acquire access through self-directed study but face high costs in time and resources without institutional support
 *   - Graduate Students: Mixed position (moderate/constrained) — experience both coordination (genuine interpretive scaffolding) and extraction (years of credential acquisition required for participation)
 *   - Credentialed Scholars: Primary beneficiaries (institutional/arbitrage) — the allusive density validates existing cultural capital and generates endless material for scholarly production
 *   - Elite Educational Institutions: Institutional beneficiaries (institutional/arbitrage) — the constraint reproduces cultural capital hierarchies and justifies specialized curricula
 *   - Open Access Movement: Organized agents (organized/mobile) — building alternative pathways through digital annotations but facing persistent educational prerequisites
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent authorial choices as inherent properties of literary complexity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(allusive_density_as_exclusion, 0.68).
domain_priors:suppression_score(allusive_density_as_exclusion, 0.72).
domain_priors:theater_ratio(allusive_density_as_exclusion, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(allusive_density_as_exclusion, extractiveness, 0.68).
narrative_ontology:constraint_metric(allusive_density_as_exclusion, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(allusive_density_as_exclusion, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(allusive_density_as_exclusion, snare).
narrative_ontology:human_readable(allusive_density_as_exclusion, "Allusive Density as Cultural Capital Barrier in Modernist Poetry").
narrative_ontology:topic_domain(allusive_density_as_exclusion, "literary_criticism/modernist_poetry/cultural_theory").

domain_priors:requires_active_enforcement(allusive_density_as_exclusion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(allusive_density_as_exclusion, academically_credentialed_readers).
narrative_ontology:constraint_beneficiary(allusive_density_as_exclusion, scholarly_apparatus_producers).
narrative_ontology:constraint_beneficiary(allusive_density_as_exclusion, elite_educational_institutions).
narrative_ontology:constraint_victim(allusive_density_as_exclusion, non_specialist_readers).
narrative_ontology:constraint_victim(allusive_density_as_exclusion, autodidact_readers).
narrative_ontology:constraint_victim(allusive_density_as_exclusion, non_anglophone_readers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-SPECIALIST READER (SNARE) — Trapped by educational barriers and lack of access to scholarly apparatus. The poem's allusive density creates an insurmountable interpretive barrier without specialized training. Cannot exit the exclusion without years of credential acquisition. Experiences maximum extraction: cultural capital flows away from this position toward credentialed gatekeepers.
constraint_indexing:constraint_classification(allusive_density_as_exclusion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: AUTODIDACT READER (SNARE) — Has some exit capacity through self-directed study but faces high costs: time investment in learning multiple languages, acquiring reference materials, navigating scholarly discourse without institutional support. The constraint extracts heavily even from motivated agents with moderate resources. Suppression operates through credential requirements for legitimate participation in interpretive discourse.
constraint_indexing:constraint_classification(allusive_density_as_exclusion, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: GRADUATE STUDENT (TANGLED ROPE) — Experiences both coordination (the allusive network provides genuine interpretive scaffolding and connects to broader literary tradition) and extraction (must invest years acquiring cultural capital to participate; career advancement depends on demonstrating mastery of the apparatus). Constrained by institutional requirements but also benefits from the scholarly ecosystem. Mixed experience: the density both enables sophisticated reading and enforces hierarchy.
constraint_indexing:constraint_classification(allusive_density_as_exclusion, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CREDENTIALED SCHOLAR (ROPE) — Primary beneficiary. The allusive density functions as coordination: it enables sophisticated intertextual analysis, connects disparate literary traditions, and provides endless material for scholarly production. Experiences minimal extraction because the constraint validates existing cultural capital and generates career opportunities. Can arbitrage between interpretive communities and institutional contexts.
constraint_indexing:constraint_classification(allusive_density_as_exclusion, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ELITE EDUCATIONAL INSTITUTION (ROPE) — Benefits from the constraint's reproduction of cultural capital hierarchies. The allusive density justifies specialized curricula, validates credential systems, and maintains institutional prestige. Experiences the constraint as pure coordination: it organizes literary study, establishes standards for expertise, and creates demand for institutional mediation.
constraint_indexing:constraint_classification(allusive_density_as_exclusion, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: OPEN ACCESS MOVEMENT (TANGLED ROPE) — Organized agents (digital humanities projects, open-access annotations, Wikipedia, public domain translations) see both coordination function (the allusive network genuinely enriches interpretation) and extraction mechanism (the apparatus requirement excludes non-credentialed readers). Building alternative pathways through freely available annotations and hyperlinked editions, but the constraint's suppression persists through educational prerequisites.
constraint_indexing:constraint_classification(allusive_density_as_exclusion, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / COMPLEXITY VIEW (MOUNTAIN) — From a civilizational perspective, some interpretive difficulty is inherent to complex literary art: ambitious poetry always requires cultural knowledge, and the gap between immediate comprehension and deep understanding is a structural feature of how meaning works. This perspective sees allusive density as an immutable property of sophisticated literature. However, structural data contradicts this naturalization — the specific pattern of 7+ languages and 30+ source texts is a contingent authorial choice, not a law of literary complexity.
constraint_indexing:constraint_classification(allusive_density_as_exclusion, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(allusive_density_as_exclusion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(allusive_density_as_exclusion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(allusive_density_as_exclusion, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(allusive_density_as_exclusion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(allusive_density_as_exclusion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The allusive density creates severe interpretive barriers that concentrate cultural capital among credentialed readers. The extraction is not total (some readers can access the poem through self-directed study or digital tools) but is substantial and persistent. The value reflects that the apparatus requirement excludes the majority of potential readers and that the exclusion is structural rather than incidental. Suppression (0.72): High. Barriers include: years of specialized education required to acquire linguistic and cultural knowledge; credential requirements for legitimate participation in interpretive discourse; institutional gatekeeping of scholarly apparatus; economic barriers to accessing reference materials and critical editions. The suppression operates through both material barriers (cost of education, access to libraries) and symbolic barriers (credential requirements for interpretive authority). Theater ratio (0.58): Moderate-high. Much scholarly production about the poem has become self-referential, addressing other criticism rather than the poem itself. The apparatus has proliferated beyond interpretive necessity into credentialing ritual. However, some genuine coordination function persists — the scholarly tradition does provide real interpretive insights and connects the poem to broader literary history. The theater has increased over the interval as the cultural references have become more obscure and the apparatus has grown, but digital tools have recently reduced some performative overhead.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates extreme perspectival divergence. Non-specialist readers experience pure extraction (snare) — the allusive density excludes them from interpretive participation with no coordination benefit. Credentialed scholars experience pure coordination (rope) — the density enables sophisticated analysis and validates their expertise. Graduate students experience mixed coordination and extraction (tangled rope) — the density both enriches interpretation and enforces hierarchy. The open access movement sees the same mixed pattern but with organized agency to build alternatives. The analytical observer risks seeing an immutable property of literary art (mountain) — complex poetry always requires cultural knowledge — but the structural data reveals this as naturalization: the specific pattern of 7+ languages and 30+ source texts is a contingent authorial choice, not a law of aesthetic complexity. The perspectival gap is not 'which type is correct?' but 'which structural position are you measuring from?' The powerless reader's snare and the credentialed scholar's rope are both legitimate readings of the same constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Non-specialist readers are trapped victims experiencing maximum extraction — cultural capital flows away from them toward credentialed gatekeepers, and they have no exit option without years of educational investment. Autodidact readers are constrained victims with some exit capacity but facing high costs. Graduate students occupy a mixed position: they are victims of the credential requirement (must invest years acquiring apparatus mastery) but also beneficiaries of the scholarly ecosystem (the density provides material for career advancement). Credentialed scholars are primary beneficiaries with arbitrage options — the constraint validates their existing cultural capital and generates professional opportunities. Elite institutions are institutional beneficiaries — the constraint reproduces hierarchies that justify their existence. The open access movement sees both coordination and extraction but has organized agency to build alternatives. The analytical observer risks naturalizing the contingent authorial choice (7+ languages, 30+ source texts) as an inherent property of literary complexity.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY UNRESOLVED: The constraint requires further analysis to distinguish coordination function from extractive overhead. The allusive density genuinely does provide interpretive scaffolding and connect the poem to broader literary tradition (coordination), but it also functions as a cultural capital barrier that excludes non-credentialed readers (extraction). The mandatrophy question is: what proportion of the allusive density is necessary for the poem's aesthetic function vs what proportion serves primarily to enforce educational hierarchy? Omega variable 'apparatus_necessity_threshold' addresses this directly. If < 30% of allusions require apparatus for basic comprehension, coordination dominates. If > 70% require apparatus, extraction dominates. Current evidence suggests the threshold is high (most allusions are opaque without scholarly mediation), but digital tools may be shifting the balance. The constraint's classification as snare from powerless perspectives and rope from institutional perspectives reflects this unresolved tension. Resolution requires empirical reader response studies across educational backgrounds with controlled apparatus access.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    authorial_intent_vs_structural_effect,
    'Was the allusive density intended as democratic enrichment (making high culture available to all through synthesis) or as exclusionary gatekeeping?',
    'Historical analysis of authorial statements, correspondence, and contemporary reception; comparison with other modernist strategies for managing cultural capital',
    'If democratic intent: extraction is unintended side effect of coordination mechanism. If exclusionary intent: extraction is primary function disguised as aesthetic necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authorial_intent_vs_structural_effect, conceptual, 'Whether allusive density was intended as enrichment or exclusion').

omega_variable(
    apparatus_necessity_threshold,
    'What proportion of allusions must be comprehensible without scholarly apparatus for the poem to function as accessible art rather than credentialed puzzle?',
    'Reader response studies across educational backgrounds; identification of minimum viable interpretive access; correlation between apparatus dependency and reader exclusion',
    'If threshold < 30%: most readers can engage meaningfully without apparatus (coordination dominates). If threshold > 70%: apparatus is mandatory for basic comprehension (extraction dominates).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(apparatus_necessity_threshold, empirical, 'Proportion of allusions requiring apparatus for basic comprehension').

omega_variable(
    digital_annotation_sufficiency,
    'Do freely available digital annotations and hyperlinked editions reduce the constraint''s extraction to coordination-level overhead, or does educational prerequisite persist?',
    'Comparison of interpretive access between readers using open digital tools vs traditional scholarly apparatus; measurement of comprehension and engagement across educational backgrounds with equivalent tool access',
    'If sufficient: constraint is degrading toward rope (tools democratize access). If insufficient: extraction persists because apparatus cannot substitute for educational formation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(digital_annotation_sufficiency, empirical, 'Whether digital tools reduce extraction to coordination overhead').

omega_variable(
    multilingual_access_asymmetry,
    'Does the 7+ language requirement constitute a distinct extraction mechanism beyond general cultural capital, particularly for non-Anglophone readers?',
    'Analysis of interpretive access patterns by linguistic background; identification of which allusions are language-specific vs culturally-specific; measurement of comprehension gaps attributable to linguistic vs educational barriers',
    'If distinct: multilingual requirement is separate snare requiring decomposition. If integrated: linguistic barrier is component of general cultural capital extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(multilingual_access_asymmetry, empirical, 'Whether multilingual requirement is distinct extraction mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(allusive_density_as_exclusion, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(allusive_theater_1922, allusive_density_as_exclusion, theater_ratio, 0, 0.35).
narrative_ontology:measurement(allusive_theater_1947, allusive_density_as_exclusion, theater_ratio, 25, 0.48).
narrative_ontology:measurement(allusive_theater_1972, allusive_density_as_exclusion, theater_ratio, 50, 0.58).
narrative_ontology:measurement(allusive_theater_1997, allusive_density_as_exclusion, theater_ratio, 75, 0.62).
narrative_ontology:measurement(allusive_theater_2022, allusive_density_as_exclusion, theater_ratio, 100, 0.55).

% Extraction over time
narrative_ontology:measurement(allusive_extract_1922, allusive_density_as_exclusion, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(allusive_extract_1947, allusive_density_as_exclusion, base_extractiveness, 25, 0.61).
narrative_ontology:measurement(allusive_extract_1972, allusive_density_as_exclusion, base_extractiveness, 50, 0.68).
narrative_ontology:measurement(allusive_extract_1997, allusive_density_as_exclusion, base_extractiveness, 75, 0.7).
narrative_ontology:measurement(allusive_extract_2022, allusive_density_as_exclusion, base_extractiveness, 100, 0.66).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(allusive_density_as_exclusion, identity_coordination).
narrative_ontology:boltzmann_floor_override(allusive_density_as_exclusion, 0.12).

% DUAL FORMULATION NOTE:
% This constraint is downstream of 'mythic_scaffolding_vs_formal_fragmentation' (the upstream tangled rope about whether The Waste Land's mythic method provides genuine structural unity or is itself a critical apparatus imposition). The upstream constraint addresses whether the poem has coherent structure; this constraint addresses whether the allusive density that supposedly provides that structure functions as coordination or extraction. The two constraints have different epsilon values because they measure different structural properties: the upstream constraint measures the tension between claimed unity and formal fragmentation (ε ≈ 0.45, tangled rope), while this constraint measures the cultural capital barrier created by allusive density (ε = 0.68, snare from powerless perspectives). Both are legitimate structural features of the same literary object, requiring separate stories per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(allusive_density_as_exclusion, moderate, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
