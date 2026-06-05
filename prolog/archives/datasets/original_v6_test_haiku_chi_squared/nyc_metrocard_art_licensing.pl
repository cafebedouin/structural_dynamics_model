% ============================================================================
% CONSTRAINT STORY: nyc_metrocard_art_licensing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nyc_metrocard_art_licensing, []).

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
 *   constraint_id: nyc_metrocard_art_licensing
 *   human_readable: NYC MetroCard Art Licensing Agreement
 *   domain: economic
 *
 * SUMMARY:
 *   The NYC MetroCard art licensing agreement represents a structural
 *   constraint between a large institutional buyer (MTA) and individual
 *   creative workers (visual artists). The MTA benefits from below-market
 *   artwork acquisition that enhances passenger experience and reduces design
 *   costs, while artists receive fixed, low compensation in exchange for
 *   public visibility. The constraint operates as a hybrid
 *   coordination-extraction mechanism: there is genuine coordination
 *   (providing artists with a platform and predictable income, providing
 *   transit passengers with visual interest), but the coordination is paired
 *   with asymmetric extraction (the MTA captures all secondary revenue,
 *   controls usage scope and duration, and uses informational advantage to
 *   set compensation unilaterally). Over the 30-year interval, extractiveness
 *   has increased as MTA profitability has grown while artist compensation
 *   has remained stagnant, and theater ratio has increased as the
 *   'promotional platform' narrative has become decoupled from actual market
 *   value. The constraint exhibits all six classification types depending on
 *   observer position: the individual artist sees extraction (Snare), the MTA
 *   sees coordination (Rope), the organized artist collective sees mixed
 *   coordination and enforcement (Tangled Rope), reform advocates see a
 *   removable layer (Scaffold), institutional licensing practices see inertia
 *   (Piton), and the analytical observer risks naturalizing institutional
 *   power asymmetry as market law (false Mountain).
 *
 * KEY AGENTS:
 *   - Visual Artists: Primary victims (powerless/trapped) — individual creators with minimal negotiation power, no exit from agreement without forfeiting visibility
 *   - MTA Transit Authority: Primary beneficiary (institutional/arbitrage) — acquires artwork below market rate, captures all secondary revenue, maintains unilateral control
 *   - Artist Collective/Labor Organizations: Secondary actor (organized/constrained) — can organize resistance and negotiate collectively but constrained by need for platform visibility
 *   - Artist Rights Reform Advocates: Secondary actor (powerful/arbitrage) — institutional reformers proposing revenue-sharing and renegotiation windows
 *   - Transit Passengers: Incidental beneficiary (moderate/mobile) — benefit from visual interest but not directly party to constraint
 *   - Analytical Observer: Neutral observer (analytical/analytical) — risks naturalizing power asymmetry as immutable market mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nyc_metrocard_art_licensing, 0.52).
domain_priors:suppression_score(nyc_metrocard_art_licensing, 0.58).
domain_priors:theater_ratio(nyc_metrocard_art_licensing, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nyc_metrocard_art_licensing, extractiveness, 0.52).
narrative_ontology:constraint_metric(nyc_metrocard_art_licensing, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(nyc_metrocard_art_licensing, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nyc_metrocard_art_licensing, tangled_rope).
narrative_ontology:human_readable(nyc_metrocard_art_licensing, "NYC MetroCard Art Licensing Agreement").
narrative_ontology:topic_domain(nyc_metrocard_art_licensing, "economic").

domain_priors:requires_active_enforcement(nyc_metrocard_art_licensing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nyc_metrocard_art_licensing, mta_transit_authority).
narrative_ontology:constraint_victim(nyc_metrocard_art_licensing, visual_artists).
narrative_ontology:constraint_victim(nyc_metrocard_art_licensing, artist_compensation_equity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VISUAL ARTIST (SNARE) — Individual artists have minimal negotiating power against the MTA, a large institutional buyer. Once artwork is licensed, the artist cannot control its use, distribution scope, or secondary revenue streams. Career visibility may create a false sense of benefit while actual compensation remains fixed and low. Exit from the agreement requires forfeiting visibility or legal action neither artist can afford. d≈0.92, f(d)≈1.39, σ=0.8 → χ≈0.58.
constraint_indexing:constraint_classification(nyc_metrocard_art_licensing, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: MTA TRANSIT AUTHORITY (ROPE) — The MTA experiences this as pure coordination: acquire visual interest for MetroCards to enhance ridership psychology and brand perception, pay artists a flat fee that is below market rate but creates legitimacy through artist participation. Exit options abundant: can switch artists, cancel the program, or develop in-house design. d≈0.08, f(d)≈-0.11, σ=1.0 → χ≈-0.06. Net beneficiary through arbitrage of artist desperation against public good framing.
constraint_indexing:constraint_classification(nyc_metrocard_art_licensing, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: ARTIST COLLECTIVE / LABOR ORGANIZING (TANGLED ROPE) — When organized, artists see both coordination function (shared marketing platform for visual work) and asymmetric extraction (MTA captures secondary revenue, controls licensing scope, sets compensation unilaterally). Organized artists have some leverage (withhold participation, public campaigns) but constrained by need for visibility and cultural prestige. Enforcement is active — MTA uses contract terms to prevent collective renegotiation. d≈0.65, f(d)≈1.00, σ=1.0 → χ≈0.52.
constraint_indexing:constraint_classification(nyc_metrocard_art_licensing, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ARTIST RIGHTS REFORM MOVEMENT (SCAFFOLD) — Policy advocates and reformed-thinking institutional actors see this as a temporary extraction mechanism that can be restructured through sunset legislation: percentage-of-revenue sharing, artist approval rights, secondary licensing caps, and renegotiation windows. The coordination function (platform for visual work) is real; the extraction layer (flat-fee expropriation) is removable. d≈0.30, f(d)≈0.20, σ=1.0 → χ≈0.10. Low effective extraction because reform movement has institutional allies and legislative pathways.
constraint_indexing:constraint_classification(nyc_metrocard_art_licensing, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: LEGACY LICENSING FRAMEWORK (PITON) — The agreement persists through institutional inertia: MTA contracts officers follow precedent, artists accept low compensation because alternatives (self-promotion, gallery sales) are more precarious. The original coordination logic (cost-effective visual interest) has been superseded by digital alternatives and social media, yet the licensing framework remains. theater_ratio=0.45 is below piton threshold (0.70), but the framework is inertial: maintained not because it works well but because changing it requires effort and creates friction with incumbent vendors. Treating as piton because the functional coordination has been hollowed by decades of digitalization.
constraint_indexing:constraint_classification(nyc_metrocard_art_licensing, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / MARKET-LAW VIEW (MOUNTAIN) — From a civilizational perspective, this looks like a natural economic law: labor-abundant visual artists must discount their work to reach audiences; transit authorities capture value through scale; information asymmetry between individuals and institutions is immutable. However, structural data (ε=0.52, suppression=0.58, theater=0.45) contradicts mountain classification. The engine identifies this as a false summit: the constraint is contingent institutional practice (contract terms, licensing scope, compensation formulas), not a law of markets or nature.
constraint_indexing:constraint_classification(nyc_metrocard_art_licensing, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nyc_metrocard_art_licensing_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(nyc_metrocard_art_licensing, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nyc_metrocard_art_licensing, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(nyc_metrocard_art_licensing, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(nyc_metrocard_art_licensing, TR),
    TR >= 0.70.

:- end_tests(nyc_metrocard_art_licensing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The MTA captures below-market compensation differential, all secondary revenue, and controls licensing scope unilaterally. Individual artists typically receive flat fees ($500-$3000 per design) while the MTA prints millions of cards generating revenue through transit operations. However, the extraction is not maximal (not ≥0.66 snare-level) because: (1) artists voluntarily participate seeking visibility, (2) the visibility may generate some spillover value through portfolio effects, and (3) the coordination function is real (the MTA does provide a platform that artists could not access individually). The trajectory from 0.35 to 0.52 reflects increasing MTA profitability while artist compensation remained flat. Suppression (0.58): Moderate-high. Significant barriers to artist resistance include: (1) individual artists have no negotiating power against large institutional buyer, (2) alternatives for public visibility are less accessible or costlier, (3) participation is presented as an honor/opportunity rather than employment (suppressing demands for market-rate compensation), (4) contract terms are non-negotiable (take-it-or-leave-it). But suppression is not maximal (not ≥0.60 snare-level) because: artists can refuse participation without legal penalty, and some collective organizing power exists. Theater ratio (0.45): Moderate. The 'artistic platform' narrative provides some performative legitimacy, but the core function is cost-effective acquisition of visual content. Theater increased from 0.30 to 0.45 as digital alternatives have made the 'promotional value' claim weaker — the MTA now emphasizes 'cultural partnership' and 'artist support' more heavily as the material justification (visibility leads to commissions) has eroded.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows maximal perspectival divergence: the individual artist sees pure extraction (Snare: trapped, powerless, biographical), while the MTA sees pure coordination (Rope: institutional, immediate, arbitrage). The organized artist collective occupies the middle ground (Tangled Rope: organized, generational, constrained) where they can identify both the coordination function and the extraction layer. Reform advocates see a structural design flaw that can be corrected (Scaffold: powerful, generational, arbitrage) — the sunset is legislative revision of contract terms to include revenue-sharing. The legacy licensing framework appears as degraded institutional practice (Piton) — maintained through inertia rather than function. The analytical observer risks committing the naturalization fallacy (false Mountain) by framing power asymmetry as inevitable market law rather than contingent institutional design. The perspectival gap reveals that 'fair platform for artists' (MTA framing) and 'exploitative expropriation' (artist framing) are both structurally correct — the question is which observer position you occupy.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual Artist: Victim + trapped + powerless → d≈0.92, f(d)≈1.39. Maximum extraction in personal temporal horizon. Cannot exit without career cost; compensation is below market; MTA controls all downstream revenue. Artist Collective: Victim + constrained + organized → d≈0.65, f(d)≈1.00. Significant but not maximal extraction. Organized artists have some leverage (withhold, collective action) and constrained (not trapped) because they can walk away as a group. MTA: Beneficiary + arbitrage + institutional → d≈0.08, f(d)≈-0.11. Net beneficiary. Has abundant exit options, sets compensation unilaterally, captures all secondary value. Reform Advocates: Observer + arbitrage + powerful → d≈0.30, f(d)≈0.20. Moderate position with institutional authority to propose sunset (legislative change). Can exit current system through policy, not trapped. The directionality spread (0.08 to 0.92) across agent perspectives reveals the constraint's core asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE CLASSIFICATION: This constraint resolves the mandatrophy by declaring THREE structural requirements simultaneously: (1) coordination function — the MTA does provide a platform that artists could not access individually, and passengers do benefit from visual interest; (2) asymmetric extraction — the MTA captures below-market compensation and all secondary revenue using informational advantage; (3) active enforcement — the MTA uses contract terms (non-negotiable offer, no artist approval rights, usage control) to prevent collective renegotiation and lock in the extraction mechanism. All three are necessary and present. Without the coordination function, this would be pure Snare. Without the extraction layer, this would be pure Rope. Without the enforcement, artists could exit and renegotiate. The mandatrophy is resolved: this is legitimately a hybrid, not a mislabeling of pure extraction as coordination or vice versa. The fact that the individual artist (Snare perspective) disagrees is perspectival — they lack the information and power to see the coordination function that the organized collective can access.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    secondary_revenue_attribution,
    'Does the MetroCard licensing agreement include revenue-sharing for collectible card sales, secondary markets, or commemorative editions featuring the licensed artwork?',
    'Audit of MTA licensing contracts; comparison with artists'' royalty statements; analysis of MetroCard sales data and artist compensation records',
    'If revenue-sharing exists: compensation may be higher than currently perceived (reduces extraction). If absent: artists receive no compensation from appreciating collectible value, increasing extraction to ε≈0.65.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(secondary_revenue_attribution, empirical, 'Whether secondary revenue from collectible MetroCards is shared with artists').

omega_variable(
    artist_portfolio_value_impact,
    'Does MetroCard licensing meaningfully increase an artist''s market value (gallery sales, commissions, employment) or is the visibility benefit illusory?',
    'Longitudinal tracking of artists'' career outcomes post-licensing; comparison of gallery sales, commission frequency, and earning growth for licensed vs unlicensed artists; qualitative interviews about perceived career impact',
    'If meaningful: licensing functions as partial compensation in visibility and career advancement (reduces net extraction). If illusory: visibility is non-monetizable and operates as extraction mechanism (increases ε).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(artist_portfolio_value_impact, empirical, 'Whether MetroCard licensing provides measurable career or financial benefit').

omega_variable(
    contract_negotiation_asymmetry,
    'Are MTA licensing terms fixed-offer-only, or do artists have genuine negotiation authority over compensation, licensing scope, and usage restrictions?',
    'Analysis of contract variation across artists and time; interviews with artists about negotiation opportunities; comparison with industry-standard licensing terms for transit or public-sector visual licensing',
    'If fixed-offer-only: suppression is high (≥0.65), snare classification confirmed for individual artists. If genuine negotiation exists: suppression drops to ≤0.45, enabling rope classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(contract_negotiation_asymmetry, empirical, 'Degree of artist negotiation power over licensing terms').

omega_variable(
    alternative_visibility_pathways,
    'How accessible are alternative platforms for visual artists to reach transit audiences at comparable cost-benefit ratios (Instagram, TikTok, digital ads, independent print)?',
    'Cost-benefit analysis of MetroCard licensing vs digital marketing; reach metrics comparison; artist survey on alternative options perceived as available',
    'If alternatives are accessible and comparable: artists have genuine exit options (reduces d for individual perspective). If MetroCard is quasi-monopoly visibility channel: exit options are trapped (increases d).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_visibility_pathways, empirical, 'Availability of comparable alternative visibility platforms for artists').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nyc_metrocard_art_licensing, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mta_art_theater_t0, nyc_metrocard_art_licensing, theater_ratio, 0, 0.3).
narrative_ontology:measurement(mta_art_theater_t15, nyc_metrocard_art_licensing, theater_ratio, 15, 0.4).
narrative_ontology:measurement(mta_art_theater_t30, nyc_metrocard_art_licensing, theater_ratio, 30, 0.45).

% Extraction over time
narrative_ontology:measurement(mta_art_extract_t0, nyc_metrocard_art_licensing, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(mta_art_extract_t15, nyc_metrocard_art_licensing, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(mta_art_extract_t30, nyc_metrocard_art_licensing, base_extractiveness, 30, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nyc_metrocard_art_licensing, resource_allocation).
narrative_ontology:affects_constraint(nyc_metrocard_art_licensing, platform_artist_compensation).
narrative_ontology:affects_constraint(nyc_metrocard_art_licensing, cultural_institution_extraction).

% DUAL FORMULATION NOTE:
% The MetroCard licensing agreement is a specific instantiation of a broader constraint family around platform-mediated artist labor. The platform (MTA transit system) uses informational advantage and scale asymmetry to extract below-market compensation. This story decomposes from the broader 'platform_artist_compensation' constraint, which operates similarly across digital platforms (Spotify, Instagram, TikTok), commercial galleries, and public institutions. The ε=0.52 reflects the specific MTA agreement; other platforms may score higher (e.g., Spotify at ε≈0.68) or lower (e.g., government art grants at ε≈0.25). Each story captures the institution-specific extraction mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nyc_metrocard_art_licensing, organized, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
