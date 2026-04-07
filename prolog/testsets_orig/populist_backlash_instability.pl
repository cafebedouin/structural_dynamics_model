% ============================================================================
% CONSTRAINT STORY: populist_backlash_instability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_populist_backlash_instability, []).

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
 *   constraint_id: populist_backlash_instability
 *   human_readable: Populist Backlash Instability: Elite Legitimacy and Mass Resentment
 *   domain: political_economy/institutional_legitimacy
 *
 * SUMMARY:
 *   Populist backlash instability represents a structural tension between
 *   capital mobility (which allows extraction to flow upward toward
 *   institutional elites and away from national jurisdictions) and democratic
 *   legitimacy (which requires elites to maintain responsiveness to mass
 *   constituents). As capital mobility increases and worker bargaining power
 *   declines, the extraction intensifies while institutional elites attempt
 *   to maintain legitimacy through democratic theater and periodic symbolic
 *   concessions. Displaced workers, lacking exit options, experience the
 *   constraint as a snare: their grievances are channeled into movements that
 *   mobilize their anger without fundamentally restructuring the extraction
 *   mechanisms. The populist movements themselves occupy a tangled rope
 *   position — they provide genuine coordination (voice amplification,
 *   community mobilization, political visibility) while extracting through
 *   emotional manipulation, false scapegoating, and identity fusion that
 *   diverts attention from institutional extraction. The constraint's
 *   instability derives from the fact that populist backlash is both symptoms
 *   of underlying extraction AND a mechanism that elites use to manage
 *   legitimacy crises — suppression (police action, media marginalization,
 *   deplatforming) is high because the backlash threatens established
 *   arrangements, but suppression cannot eliminate the underlying economic
 *   displacement that fuels it. Theater has increased substantially over the
 *   20-year measurement interval, reflecting the growing disjunction between
 *   democratic institutions' formal functions (representing constituent
 *   interests) and their actual responsiveness (prioritizing capital and
 *   institutional preservation).
 *
 * KEY AGENTS:
 *   - Economically Displaced Workers: Primary victims (powerless/trapped) — bear extraction through wage decline, credential obsolescence, reduced purchasing power, eroded political voice. Trapped by lack of geographic/economic mobility and credential barriers to alternative employment.
 *   - Institutional Elites: Primary beneficiaries (institutional/arbitrage) — capture gains from capital mobility, regulatory capture, financial services concentration. Experience constraint as coordination problem requiring periodic legitimacy maintenance.
 *   - Populist Political Movements: Secondary beneficiaries (powerful/mobile) — extract from displaced workers through emotional mobilization and identity fusion while providing genuine coordination of voice and visibility. Powerful enough to shift electoral dynamics but not to restructure underlying extraction.
 *   - Labor and Redistribution Organizations: Organized alternative builders (organized/constrained) — attempting to construct alternative coordination mechanisms (worker voice, sectoral adjustment, local manufacturing investment). Constrained by corporate political power and austerity constraints.
 *   - Middle Class / Aspiring Bourgeoisie: Mixed agents (moderate/constrained) — experience both benefits (institutional stability, status maintenance) and costs (credential devaluation, precarity, political marginalization). Serve as swing actors in electoral competition.
 *   - Democratic Institutions: Piton actors (institutional/arbitrage) — maintain performative functions (elections, representation, public discourse) while actual responsiveness declines. Preserve legitimacy through ritual rather than delivery.
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangements (capital mobility + democratic deficit + credential inflation) as immutable laws of politics.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(populist_backlash_instability, 0.58).
domain_priors:suppression_score(populist_backlash_instability, 0.68).
domain_priors:theater_ratio(populist_backlash_instability, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(populist_backlash_instability, extractiveness, 0.58).
narrative_ontology:constraint_metric(populist_backlash_instability, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(populist_backlash_instability, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(populist_backlash_instability, tangled_rope).
narrative_ontology:human_readable(populist_backlash_instability, "Populist Backlash Instability: Elite Legitimacy and Mass Resentment").
narrative_ontology:topic_domain(populist_backlash_instability, "political_economy/institutional_legitimacy").

domain_priors:requires_active_enforcement(populist_backlash_instability).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(populist_backlash_instability, institutional_elites).
narrative_ontology:constraint_beneficiary(populist_backlash_instability, regulatory_gatekeepers).
narrative_ontology:constraint_victim(populist_backlash_instability, economically_displaced_workers).
narrative_ontology:constraint_victim(populist_backlash_instability, political_representation_quality).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISPLACED WORKER (SNARE) — Trapped by economic dependency, geographic immobility, and credential obsolescence. Limited exit options (retrain at high cost, relocate away from social networks, accept lower-wage work). Bears full extraction: lost purchasing power, eroded social status, political voice channeled into protest with minimal concrete gain. Maximum suppression — actual alternatives are few, and perceived alternatives are systematically discredited by institutional messaging.
constraint_indexing:constraint_classification(populist_backlash_instability, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INSTITUTIONAL ELITE (ROPE) — Benefits from existing coordination mechanisms (capital mobility, regulatory capture, institutional access). Experiences the constraint as coordination: populist pressure forces periodic legitimacy maintenance (rhetoric, selective redistribution, symbolic concessions). Net beneficiary — able to arbitrage between domestic pressure and global capital flows. Suppression directed outward at populist actors, not inward.
constraint_indexing:constraint_classification(populist_backlash_instability, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: POPULIST POLITICAL MOVEMENT (TANGLED ROPE) — Powerful agents (party apparatus, media platforms, charismatic leadership) with structural mobility. Extract from displaced workers through emotional mobilization, false scapegoating, and identity fusion while providing genuine coordination (voice amplification, community mobilization, counter-institutional visibility). Both coordination and extraction present: mobilize workers' grievances while channeling them into movements that do not fundamentally restructure extraction mechanisms.
constraint_indexing:constraint_classification(populist_backlash_instability, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: LABOR-BASED REDISTRIBUTION COALITION (SCAFFOLD) — Organized agents (unions, worker advocacy groups, regional development initiatives) attempting to build alternative coordination mechanisms with explicit sunset: stronger labor protections, sectoral adjustment funds, skills training, local manufacturing investment. Constrained by corporate political power and austerity constraints. Theater is moderate (< 0.70) because these groups have genuine delivery mechanisms, though implementation is incomplete. Sunset logic: if sectoral coordination and worker voice are rebuilt, the populist backlash should decline.
constraint_indexing:constraint_classification(populist_backlash_instability, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: MIDDLE CLASS / ASPIRING BOURGEOISIE (TANGLED ROPE) — Moderate agents (small business owners, semi-skilled managers, suburban professionals) who both benefit and lose from the existing structure. Experience coordination through institutional stability and status maintenance, but also experience extraction through credential devaluation, precarious employment, political marginalization. Constrained exit: cannot fully escape the institutional system, but can shift political allegiance. Genuine coordination exists (institutions maintain order, property rights), but extraction is real (risk externalization onto workers, political voice disproportionately allocated to capital). Suppression is moderate — these agents have some media channels and political influence.
constraint_indexing:constraint_classification(populist_backlash_instability, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: DEMOCRATIC LEGITIMACY THEATER (PITON) — The formal apparatus of democratic representation (elections, legislative processes, public consultation) has become substantially performative. Theater ratio is high (0.64) because the mechanisms exist (voting, representation, public discourse) but their functional coordination impact has declined as capital mobility increases and institutional gatekeepers respond primarily to financial rather than electoral pressure. Maintains legitimacy through ritual and messaging rather than structural responsiveness. Piton classification reflects degradation of function over time — the democratic apparatus persists through institutional inertia despite declining ability to redistribute or coordinate meaningful policy change.
constraint_indexing:constraint_classification(populist_backlash_instability, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN CANDIDATE) — From a civilizational analytical perspective, populist backlash could appear as an immutable feature of late-stage industrialized political economy: declining material security generates mass resentment, which creates political instability as a permanent fixture. However, structural data contradicts the mountain classification. The base extractiveness (0.58) and suppression (0.68) are high but not immutable; the theater ratio (0.64) reflects degraded but still-functional institutions. This perspective risks naturalizing what is actually a contingent institutional arrangement (capital mobility + democratic deficit + credential inflation) as an immutable law of politics.
constraint_indexing:constraint_classification(populist_backlash_instability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(populist_backlash_instability_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(populist_backlash_instability, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(populist_backlash_instability, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(populist_backlash_instability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(populist_backlash_instability, TR),
    TR >= 0.70.

:- end_tests(populist_backlash_instability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts from displaced workers through wage decline, credential obsolescence, and political marginalization, but the extraction is not absolute (some redistributive policy persists, some institutional responsiveness remains). The 20-year trajectory shows extraction accumulating from 0.35 to 0.58, reflecting increasing capital mobility and declining worker bargaining power. Suppression (0.68): High. Significant barriers to exit include: economic dependency (limited alternative employment), geographic immobility (social networks, housing stock), credential barriers (retraining costs), and institutional suppression (police action against backlash movements, media marginalization, deplatforming of alternative narratives). Theater ratio (0.64): Moderate-high. Democratic institutions perform their legitimacy functions (elections occur, representation exists, public discourse happens) but with declining functional responsiveness. The increase from 0.45 to 0.64 reflects growing disjunction between formal democratic functions and actual policy output. Claimed type: Tangled Rope. The constraint exhibits genuine coordination (populist movements coordinate displaced workers' voice, institutions do provide some mechanism for grievance expression and political choice) alongside asymmetric extraction (elites capture disproportionate gains, movements extract through identity manipulation, workers bear the suppression costs). Requires active enforcement (suppression of backlash movements, media gatekeeping, institutional legitimacy maintenance narratives).
 *
 * PERSPECTIVAL GAP:
 *   The core gap is between the snare (displaced workers) and rope (elites) perspectives. Workers see the same extraction mechanism that benefits elites: immobility of capital-displaced workers into geographical regions abandoned by investment, while capital flows globally; political institutions that cannot constraint capital flows because capital has credible exit threats. From the worker's perspective, the constraint is a snare: they are trapped, have no exit options, and experience pure extraction (loss of livelihood, status, political voice). From the elite's perspective, the same constraint is a rope: capital mobility is a coordination mechanism that allocates resources to high-return uses globally, and domestic political institutions are an arena where they coordinate with competitors on rules of extraction. The populist movement's tangled rope perspective reveals that movements both solve a coordination problem (aggregating dispersed worker grievances into visible political power) and extract from workers (through emotional manipulation, scapegoating, identity fusion that redirects anger from capital toward outgroups). The middle class's tangled rope perspective reveals they experience genuine coordination benefits (property rights, consumer access, institutional stability) alongside extraction (credential devaluation, precarity, political voice dilution). The labor coalition's scaffold perspective introduces an alternative hypothesis: if worker voice, sectoral coordination, and local investment are rebuilt, extraction could convert to genuine coordination. But this depends on whether elites will accept the concessions required (sufficient redistribution, strong labor protections, meaningful democratic responsiveness). The piton perspective reveals that democratic institutions are performing legitimacy through ritual (elections, representation, public discourse) while their functional responsiveness declines. The analytical false summit perspective warns that naturalizing backlash as inevitable late-capitalism dynamics obscures the contingent institutional arrangements (capital mobility rules + democratic deficit + credential inflation) that generate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declaration: Institutional elites and regulatory gatekeepers benefit from the mobility-legitimacy gap because it allows extraction to flow upward (capital mobility) without structural accountability (democratic deficit). Elites arbitrage between domestic political pressure and global capital flows. Victim declaration: Economically displaced workers and political representation quality bear the extraction costs. Workers experience material loss, credential obsolescence, and political marginalization. Representation quality declines because institutional responsiveness shifts from electoral constituents to financial stakeholders. The constraint's extractiveness chi is computed from base extractiveness (0.58) × f(d) × σ(national scope = 1.0). For powerless/trapped agents, d ≈ 0.95, yielding f(d) ≈ 1.42, and chi ≈ 0.82 — maximum experienced extraction. For institutional/arbitrage beneficiaries, d ≈ 0.05, yielding f(d) ≈ -0.12, and chi ≈ -0.07 — they experience the constraint as providing net benefit (negative extraction). For moderate/constrained middle class, d ≈ 0.50, yielding f(d) ≈ 0.65, and chi ≈ 0.38 — moderate experienced extraction due to mixed cost-benefit. Suppression (0.68) is a raw structural property, unscaled by position: all agents experience suppression through institutional gatekeeping, police action, and media marginalization, but the suppression has different effects depending on exit options (trapped agents cannot escape it; mobile agents can arbitrage past it).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that populist backlash instability IS a genuine tangled rope with false summit risk: it genuinely coordinates worker grievances (rope function) while extracting through manipulation and identity-fusion (snare mechanism). The constraint is not a pure snare that could be resolved by worker education or better information; it is not a pure rope that represents optimal coordination; and it is not a natural law of politics inevitable to late capitalism. The tangled rope classification holds: there is real coordination (movements mobilize dispersed workers into visible political power), real extraction (elite maintains asymmetric benefit through capital mobility + democratic deficit), and real active enforcement (suppression of backlash movements, media gatekeeping, institutional legitimacy narratives). The false summit risk at the analytical level warns that the 'inevitable backlash' framing naturalizes what is actually a contingent institutional arrangement. If capital mobility were constrained (capital controls, place-based investment requirements), if democratic institutions were responsive (campaign finance reform, worker board representation, stakeholder governance), and if credential inflation were reversed (credential value restoration, alternative certification pathways), then the extraction mechanism would convert to genuine coordination or rope (pure coordination). The constraint's instability (high theater ratio, increasing extractiveness over time) signals that it is not a stable equilibrium: either institutional reform will restructure the constraint (into rope or scaffold), or backlash will intensify into open class conflict, or suppression will deepen (piton degradation into snare from institutional perspective). The mandatrophy resolution shows that the six-type classification system correctly identifies this as an unstable mixed constraint: tangled rope with elements of snare for victims and piton performance for institutions, carrying false summit risk if analytically naturalized.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    material_vs_status_resentment,
    'Is populist backlash primarily driven by material economic loss or by status anxiety and identity threat?',
    'Econometric analysis of backlash intensity vs local unemployment/wage decline; comparison of support across workers with objectively similar economic vulnerability but different social identity/status positioning; analysis of backlash persistence after local economic recovery',
    'If material-driven: extraction classification holds; redistributive policy would defuse backlash. If status-driven: extraction mechanism is primarily identity-based; material redistribution alone insufficient. If both: extraction operates through identity amplification of material loss.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(material_vs_status_resentment, empirical, 'Material loss vs status anxiety as driver of populist backlash').

omega_variable(
    scapegoating_necessity,
    'Does populist backlash require scapegoating of outgroups (immigrants, minorities, elites) to sustain, or can it function as pure class-based coalition?',
    'Analysis of populist movement rhetoric across contexts; comparison of movements with vs without explicit scapegoating; study of whether scapegoating intensity correlates with durable coalition formation vs short-term mobilization',
    'If necessary: the tangled rope classification is correct — movements extract through identity manipulation. If contingent: scapegoating is a specific strategy choice, not structural necessity; different populist variants could coordinate without extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scapegoating_necessity, conceptual, 'Whether scapegoating is structurally necessary for populist mobilization').

omega_variable(
    redistributive_sufficiency_threshold,
    'What level of material redistribution and democratic responsiveness would convert the snare classification (displaced workers) to rope (genuine coordination)?',
    'Historical comparison: periods of high redistribution + institutional responsiveness vs low redistribution + institutional gatekeeping; econometric analysis of support for populist backlash against redistribution intensity and policy responsiveness metrics; pilot programs with enhanced regional redistribution and worker voice mechanisms',
    'If low threshold (< 10% redistribution): scaffold perspective is optimistic but requires only modest policy change. If high threshold (> 30% redistribution): structural constraint is deep; populist instability persists even with substantial reform. If no threshold exists: backlash is driven by non-redistributive factors (identity, status); redistribution alone cannot resolve.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(redistributive_sufficiency_threshold, empirical, 'Redistribution level required to convert snare to rope for displaced workers').

omega_variable(
    institutional_elite_adaption_capacity,
    'Can institutional elites maintain arbitrage and extraction while accommodating populist pressure (perpetual legitimacy maintenance), or does sustained populist pressure force structural concessions?',
    'Long-term analysis of elite strategy: periods of concession vs periods of repression; comparison of outcomes where populist pressure was accommodated vs suppressed; measurement of whether accommodations are reversible or create structural lock-in',
    'If indefinite accommodation: constraint persists as tangled rope forever (elites make periodic concessions, workers remain trapped). If forced concessions: sustained pressure drives institutional reform; populist backlash becomes a ratchet mechanism toward redistribution. If elite adaptation succeeds through alternative channels (nationalism, cultural concessions, military investment): extraction persists despite populist pressure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_elite_adaption_capacity, preference, 'Whether elite can indefinitely accommodate populist pressure or concessions force structural change').

omega_variable(
    democratic_legitimacy_reconstruction,
    'Can democratic institutions be rebuilt to genuine responsiveness (converting piton back to rope), or is the performance deficit permanent?',
    'Comparison of institutional reform efforts and their outcomes; analysis of whether changes to electoral systems, campaign finance, legislative procedures, or worker participation mechanisms increase policy responsiveness to non-capital interests; long-term tracking of whether restored responsiveness reduces populist backlash',
    'If reconstructible: scaffold sunset is real; institutional reforms can convert theater into function. If not: piton classification is stable; democratic theater persists indefinitely as cover for capital-responsive extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(democratic_legitimacy_reconstruction, preference, 'Whether democratic legitimacy theater can be converted to genuine responsiveness').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(populist_backlash_instability, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(popul_tr_t0, populist_backlash_instability, theater_ratio, 0, 0.45).
narrative_ontology:measurement(popul_tr_t10, populist_backlash_instability, theater_ratio, 10, 0.55).
narrative_ontology:measurement(popul_tr_t20, populist_backlash_instability, theater_ratio, 20, 0.64).

% Extraction over time
narrative_ontology:measurement(popul_be_t0, populist_backlash_instability, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(popul_be_t10, populist_backlash_instability, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(popul_be_t20, populist_backlash_instability, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(populist_backlash_instability, enforcement_mechanism).
narrative_ontology:affects_constraint(populist_backlash_instability, capital_mobility_regulatory_capture).
narrative_ontology:affects_constraint(populist_backlash_instability, credential_inflation_trap).
narrative_ontology:affects_constraint(populist_backlash_instability, democratic_deficit_institutional).

% DUAL FORMULATION NOTE:
% Populist backlash instability is downstream of capital mobility (which enables elite extraction) and institutional gatekeeping (which prevents democratic responsiveness to worker interests). It represents a distinct constraint with its own extractiveness value (0.58) and perspectival structure. Separate constraint stories model capital mobility (ε ≈ 0.45, rope with capital beneficiaries), credential inflation (ε ≈ 0.52, tangled rope with worker victims), and democratic deficit (ε ≈ 0.50, piton with legitimacy victims). Populist backlash is the political manifestation where these three upstream constraints compress into visible movement, making backlash instability a network hub constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(populist_backlash_instability, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
