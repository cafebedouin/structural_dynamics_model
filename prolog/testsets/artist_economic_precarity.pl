% ============================================================================
% CONSTRAINT STORY: artist_economic_precarity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_artist_economic_precarity, []).

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
 *   constraint_id: artist_economic_precarity
 *   human_readable: Artist Economic Precarity and Cultural Production Extraction
 *   domain: cultural_economics/labor
 *
 * SUMMARY:
 *   Artist economic precarity represents a structural extraction mechanism
 *   where cultural intermediaries (galleries, record labels, publishing
 *   houses, streaming platforms) concentrate gatekeeping power over market
 *   access while offloading income uncertainty onto individual creators. The
 *   constraint produces a tangled coordination-extraction hybrid: genuine
 *   curation and audience-finding services (coordination function) coexist
 *   with systematic underpayment and exposure-based compensation (extraction
 *   mechanism). The precarity is sustained through multiple enforcement
 *   layers: identity fusion (artists cannot imagine themselves outside
 *   artistic practice), suppression of alternatives (limited capital
 *   accumulation, high barriers to formal employment), and theatrical
 *   legitimation (romantic artist mythology justifying low compensation as
 *   cultural authenticity). The constraint has intensified over the 20-year
 *   measurement interval as platform intermediaries have captured
 *   distribution power while pushing economic risk from institutional
 *   structures onto individual creators.
 *
 * KEY AGENTS:
 *   - Independent Artists: Primary victim (powerless/trapped) — structurally immobilized by capital scarcity and identity fusion; bear uncompensated labor and income volatility
 *   - Artist Collectives and Unions: Secondary victim/organized response (moderate/constrained) — benefit from mutual support but also absorbed into exposure-based labor markets
 *   - Cultural Intermediaries: Primary beneficiary (institutional/arbitrage) — galleries, record labels, publishers, platforms capture distribution power and gatekeeping rent; can exit or diversify at low cost
 *   - Institutional Gatekeepers: Secondary beneficiary (institutional/arbitrage) — museums, funding bodies, awards maintain prestige-based legitimation that justifies low market compensation
 *   - Artist Advocacy Coalition: Organized reform agent (organized/constrained) — pushes policy interventions (UBI, public funding, copyright reform) as sunset mechanism
 *   - Cultural Diversity: Structural victim (analytical/trapped) — non-dominant artistic traditions extracted more severely; face higher gatekeeping barriers and lower compensation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(artist_economic_precarity, 0.58).
domain_priors:suppression_score(artist_economic_precarity, 0.68).
domain_priors:theater_ratio(artist_economic_precarity, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(artist_economic_precarity, extractiveness, 0.58).
narrative_ontology:constraint_metric(artist_economic_precarity, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(artist_economic_precarity, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(artist_economic_precarity, tangled_rope).
narrative_ontology:human_readable(artist_economic_precarity, "Artist Economic Precarity and Cultural Production Extraction").
narrative_ontology:topic_domain(artist_economic_precarity, "cultural_economics/labor").

domain_priors:requires_active_enforcement(artist_economic_precarity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(artist_economic_precarity, cultural_intermediaries).
narrative_ontology:constraint_beneficiary(artist_economic_precarity, platform_operators).
narrative_ontology:constraint_beneficiary(artist_economic_precarity, institutional_gatekeepers).
narrative_ontology:constraint_victim(artist_economic_precarity, independent_artists).
narrative_ontology:constraint_victim(artist_economic_precarity, cultural_diversity).
narrative_ontology:constraint_victim(artist_economic_precarity, artistic_experimentation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRECARIOUS ARTIST (SNARE) — Structurally trapped by lack of capital accumulation, no alternative income sources, and identity fusion with artistic practice. Bears full extraction: uncompensated labor, exposure work, unpaid internships, subsistence costs absorbed individually. No meaningful exit without abandoning artistic identity.
constraint_indexing:constraint_classification(artist_economic_precarity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EMERGING ARTIST COLLECTIVE (TANGLED ROPE) — Constrained by resource scarcity and market power concentration, but also benefits from collective action, mutual support networks, and cultural validation through peer recognition. Coordination function exists (resource sharing, knowledge transfer, community building) alongside asymmetric extraction through exposure-based compensation and unpaid festival labor.
constraint_indexing:constraint_classification(artist_economic_precarity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CULTURAL INTERMEDIARY (ROPE) — Institutional beneficiary with arbitrage options. Experiences the constraint as pure coordination: connecting artists to audiences, filtering cultural signals, managing attention scarcity. Net extraction runs toward this agent, but also provides genuine curation and distribution services. Can exit at low cost and enter alternative markets.
constraint_indexing:constraint_classification(artist_economic_precarity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ARTIST ADVOCACY COALITION (SCAFFOLD) — Organized agents (arts unions, advocacy groups, public funding bodies) recognize precarity as a solvable coordination failure. Sunset mechanism: universal basic income, public arts funding, copyright reform, and artist residency programs represent temporary scaffolding toward a stabilized cultural economy. Extraction declines as policy interventions mature.
constraint_indexing:constraint_classification(artist_economic_precarity, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ROMANTIC ARTIST NARRATIVE (PITON) — The institutional mythology that 'true artists must suffer for their craft' persists as degraded theater. Once a genuine marker of authentic commitment, the narrative now functions primarily to justify low compensation and cultural prestige as replacement wages. Theater ratio: 0.64 reflects that artistic legitimacy rituals (gallery credentials, critical reviews, portfolio performance) increasingly substitute for economic viability.
constraint_indexing:constraint_classification(artist_economic_precarity, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN-FALSE SUMMIT) — From a civilizational perspective, attention scarcity appears as an immutable constraint: there is always more art produced than can be consumed, and sorting requires gatekeeping. However, the structural data contradicts the mountain classification. Attention scarcity is real, but the extraction mechanism—concentration of gatekeeping power in institutional intermediaries—is contingent. The false summit naturalizes what is actually regulatory capture of cultural distribution.
constraint_indexing:constraint_classification(artist_economic_precarity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(artist_economic_precarity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(artist_economic_precarity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(artist_economic_precarity, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(artist_economic_precarity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(artist_economic_precarity, TR),
    TR >= 0.70.

:- end_tests(artist_economic_precarity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, reflecting the sustained income asymmetry between creators and intermediaries, the systematic underpayment through exposure compensation, and the inability of most artists to accumulate capital. The value is not extreme (0.70+) because genuine cultural curation services exist and some artist-intermediary relationships are genuinely cooperative. Suppression (0.68): High. Multiple barriers prevent exit: (1) psychological—identity fusion makes leaving artistic practice feel like self-annihilation; (2) economic—capital scarcity prevents skill-switching or business ownership; (3) institutional—credential systems lock artists into gatekeeping dependencies; (4) social—peer recognition and community validation operate through artistic networks, making exit costly socially. Theater ratio (0.64): Moderately high. Romantic artist mythology (the suffering artist as authentic, poverty as credential) has intensified over the interval as economic precarity has deepened. Artistic legitimacy increasingly operates through performative markers (gallery credentials, critical reviews, awards, portfolio prestige) rather than market compensation. The ratio reflects that cultural gatekeepers now explicitly substitute prestige for wages: 'exposure,' 'portfolio building,' and 'artistic credibility' serve as compensation narratives.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is explained by the differential directionality: trapped artists experience d ≈ 0.90 and see snare; intermediaries experience d ≈ 0.15 and see rope. Both are reading the same constraint accurately from their structural position. The gap is not epistemic failure but faithful representation of asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from structural position and exit options. Artists with trapped or identity_locked exit options experience high d (0.85-0.95), producing maximum f(d) and high experienced extraction χ. Cultural intermediaries with arbitrage options experience low d (0.10-0.20), producing negative f(d) and negative χ—they benefit from the constraint. Artist collectives with constrained exit experience moderate d (0.50-0.65), experiencing moderate extraction. The pipeline computes χ = ε × f(d) × σ(S), so d directly determines how much of the base extraction is experienced by each agent. Global scope (σ=1.2) amplifies the experienced extraction for powerless agents, making their situation appear more severe from within their local context than aggregate statistics suggest.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that 'tangled rope' is the canonical classification because (1) genuine coordination function exists (curation, signal-finding, audience connection), (2) asymmetric extraction is structurally embedded (concentration of gatekeeping power, unequal income distribution), and (3) active enforcement is required (cultural legitimacy systems, credential gatekeeping, mythology maintenance). The false mountain perspective (attention scarcity as immutable law) is diagnostic—it reveals where naturalization is occurring. The snare perspective from powerless artists is not wrong but perspectival: it correctly identifies their experienced extraction while missing the coordination function that actually runs through the constraint. The scaffold perspective identifies the real reform vector: breaking the gatekeeping concentration through policy intervention (UBI, public funding, copyright reform, platform regulation) that transforms the constraint from extraction to genuine coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    subsistence_income_threshold,
    'Below what threshold of sustainable annual income does an artist-focused career become structurally untenable independent of talent or cultural output?',
    'Cross-country longitudinal data on artist income, career continuation rates, and exit rates by income quartile; correlation with cost-of-living variations',
    'If threshold is low (<median wage): artist economic precarity is structural feature; reform requires policy intervention. If threshold is high: precarity is selection mechanism filtering for economic privilege.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subsistence_income_threshold, empirical, 'Income threshold for sustainable artistic career').

omega_variable(
    exposure_wage_effectiveness,
    'Does compensation through ''exposure'' (audience reach, visibility, portfolio building) actually convert to future economic returns at rates exceeding direct wage replacement?',
    'Tracking study: artists offered exposure-only opportunities vs. directly compensated artists; measurement of subsequent income, opportunity access, and market positioning over 3-5 year horizon',
    'If effective: exposure is rational exchange; extraction is moderate. If ineffective: exposure is pure extraction mechanism with cultural justification; snare classification is robust.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(exposure_wage_effectiveness, empirical, 'Whether exposure compensation creates future economic returns').

omega_variable(
    identity_lock_mechanism,
    'To what extent is artist economic precarity sustained by identity fusion (inability to leave artistic practice) vs. structural barriers (absence of alternative income)?',
    'Comparative analysis of career exit rates among identity-locked (refused alternative employment due to artistic commitment) vs. structurally trapped (sought alternatives but lacked options) cohorts; temporal patterns of exit during income crises',
    'If primarily identity-locked: psychological intervention and identity frame-shifting could increase exit options and bargaining power. If primarily structural: policy must address income barriers, not psychology.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, conceptual, 'Identity lock vs. structural barriers in artist precarity').

omega_variable(
    cultural_diversity_extraction,
    'Does the concentration of gatekeeping power in cultural intermediaries systematically extract non-dominant artistic traditions (e.g., indigenous, diaspora, working-class cultural forms) more severely than dominant traditions?',
    'Comparative analysis of compensation, visibility, and institutional support for culturally marginalized vs. dominant art forms; measurement of gatekeeping barrier heights by cultural origin',
    'If yes: constraint has racialized/class extraction component; precarity is not distributed equally; policy requires equity-specific remedies. If no: precarity is universal constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cultural_diversity_extraction, empirical, 'Differential extraction of non-dominant cultural traditions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(artist_economic_precarity, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aep_tr_t0, artist_economic_precarity, theater_ratio, 0, 0.45).
narrative_ontology:measurement(aep_tr_t10, artist_economic_precarity, theater_ratio, 10, 0.58).
narrative_ontology:measurement(aep_tr_t20, artist_economic_precarity, theater_ratio, 20, 0.64).

% Extraction over time
narrative_ontology:measurement(aep_be_t0, artist_economic_precarity, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(aep_be_t10, artist_economic_precarity, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(aep_be_t20, artist_economic_precarity, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(artist_economic_precarity, attachment_coordination).
narrative_ontology:affects_constraint(artist_economic_precarity, cultural_prestige_wage_substitution).
narrative_ontology:affects_constraint(artist_economic_precarity, attention_scarcity_gatekeeping).
narrative_ontology:affects_constraint(artist_economic_precarity, artist_identity_lock).

% DUAL FORMULATION NOTE:
% Artist economic precarity decomposes into three structurally distinct constraints: (1) cultural prestige as wage substitution mechanism (ε≈0.55, theater-driven), (2) attention scarcity as gatekeeping justification (ε≈0.25, coordination problem), and (3) identity lock as exit barrier (ε≈0.45, psychological enforcement). This story models the aggregate effect; decomposition enables independent policy targeting.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(artist_economic_precarity, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
