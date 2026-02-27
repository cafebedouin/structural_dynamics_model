% ============================================================================
% CONSTRAINT STORY: nyc_metrocard_art_licensing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
 *   domain: economic/cultural_policy
 *
 * SUMMARY:
 *   The NYC MetroCard art licensing agreement structures a cultural-economic
 *   constraint where the MTA acquires intellectual property rights to
 *   artists' work for transit card production while artists receive limited
 *   upfront compensation and minimal ongoing royalties. The constraint
 *   exhibits hybrid characteristics: genuine coordination (art enhances the
 *   transit experience, provides portfolio exposure to emerging artists,
 *   generates collector interest that subsidizes MTA operations) coupled with
 *   systematic extraction (MTA captures perpetual licensing rights, controls
 *   distribution to millions of daily transit users, accesses market data on
 *   collector demand, retains secondary market premiums). The agreement is
 *   maintained through suppression mechanisms including information asymmetry
 *   (artists lack access to actual sales data and collector premium metrics),
 *   power asymmetry (individual artists negotiate against institutional MTA),
 *   and limited alternatives for comparable mass-market exposure. The
 *   constraint's theater_ratio (0.58) reflects that the licensing arrangement
 *   performs copyright formality (contracts are signed, rights are formally
 *   assigned) while enforcement against secondary market exploitations and
 *   unauthorized uses remains minimal. Extractiveness has risen from 0.35 to
 *   0.52 over the 30-year interval as collector markets matured and MTA
 *   discovered it could monetize card variants without proportional artist
 *   compensation increases.
 *
 * KEY AGENTS:
 *   - Independent Artists: Primary victim (powerless/trapped) — surrender copyright control, receive one-time or capped royalty payments, bear reputational association risk without corresponding control
 *   - Artist Community Collective: Secondary victim/moderate agent (moderate/constrained) — organized artists have some bargaining capacity but constrained by information asymmetry, career risk, and limited alternatives for mass-market circulation
 *   - MTA Transit Authority: Primary beneficiary (institutional/arbitrage) — captures perpetual licensing rights, controls distribution to millions of users, accesses market data, has strong exit options, genuine coordination function
 *   - MetroCard Collector Market: Secondary beneficiary (powerful/arbitrage) — benefits from aesthetic variety and secondary market premiums; high exit options; pure coordination experience
 *   - Copyright Enforcement System: Institutional actor (institutional/constrained) — maintains formality of licensing (signed contracts) while actual enforcement against secondary market uses is weak; piton classification
 *   - Analytical Observer: Observes tangled_rope structure — hybrid coordination/extraction with suppression maintained through information and power asymmetries
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nyc_metrocard_art_licensing, 0.52).
domain_priors:suppression_score(nyc_metrocard_art_licensing, 0.65).
domain_priors:theater_ratio(nyc_metrocard_art_licensing, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nyc_metrocard_art_licensing, extractiveness, 0.52).
narrative_ontology:constraint_metric(nyc_metrocard_art_licensing, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(nyc_metrocard_art_licensing, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nyc_metrocard_art_licensing, tangled_rope).
narrative_ontology:human_readable(nyc_metrocard_art_licensing, "NYC MetroCard Art Licensing Agreement").
narrative_ontology:topic_domain(nyc_metrocard_art_licensing, "economic/cultural_policy").

domain_priors:requires_active_enforcement(nyc_metrocard_art_licensing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nyc_metrocard_art_licensing, mta_transit_authority).
narrative_ontology:constraint_beneficiary(nyc_metrocard_art_licensing, metrocard_collectors).
narrative_ontology:constraint_victim(nyc_metrocard_art_licensing, independent_artists).
narrative_ontology:constraint_victim(nyc_metrocard_art_licensing, artist_copyright_control).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDEPENDENT ARTIST (SNARE) — Artist has minimal bargaining power against institutional MTA. The agreement structures one-time or minimal royalty payments while MTA retains perpetual licensing rights and controls distribution through millions of cards. Artist cannot exit without abandoning potential exposure and compensation. Copyright control is surrendered; artist bears reputational risk (association with MTA decisions about card designs, distribution, discontinuation) without corresponding control.
constraint_indexing:constraint_classification(nyc_metrocard_art_licensing, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: ARTIST COMMUNITY COLLECTIVE (TANGLED ROPE) — Organized artists (through advocacy groups, creative unions) have some negotiating capacity and benefit from exposure/portfolio value of MetroCard licensing, but constrained by asymmetric information (MTA controls actual sales data, collector demand metrics), career risk (refusing to license may signal difficult collaboration), and limited alternatives for mass-market circulation in transit systems. Mixed: coordination benefit (exposure, legitimacy) + asymmetric extraction (royalty cap, perpetual rights, data access asymmetry).
constraint_indexing:constraint_classification(nyc_metrocard_art_licensing, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: MTA TRANSIT AUTHORITY (ROPE) — Experiences the agreement as coordination mechanism: licensing art creates collector interest, reduces vandalism via cultural legitimacy, attracts ridership through aesthetic appeal, and generates secondary revenue (collector premiums, merchandise). MTA has strong exit options (can license other artists, commission original works, default to standard cards) and benefits substantially. Net beneficiary with genuine coordination function (art adds value to transit system).
constraint_indexing:constraint_classification(nyc_metrocard_art_licensing, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: METROCARD COLLECTOR MARKET (ROPE) — Collectors benefit from art-licensed cards (increased variety, aesthetic value, secondary market pricing). Market has strong exit options (can collect other transit cards, other memorabilia, or abandon collecting). The licensing mechanism coordinates supply (limited edition runs) with demand (collector interest). Collectors experience this as pure coordination with no meaningful extraction.
constraint_indexing:constraint_classification(nyc_metrocard_art_licensing, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: COPYRIGHT ENFORCEMENT FICTION (PITON) — The licensing agreement performs copyright compliance theater: MTA obtains artist signatures and licensing language, giving the arrangement a formal legal appearance. But enforcement is weak — secondary market reselling, unauthorized reproductions, and derivative uses by collectors proceed with minimal monitoring or artist compensation. The copyright structure persists through institutional inertia (contracts are signed because that's standard) rather than because enforcement mechanisms actually work. Theater ratio inflated by the gap between contracted rights and actual enforcement.
constraint_indexing:constraint_classification(nyc_metrocard_art_licensing, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — The constraint exhibits genuine coordination (art enhances transit experience, exposure benefits emerging artists) AND asymmetric extraction (MTA captures perpetual licensing revenue, controls distribution, accesses market data, faces minimal reversion of rights). The hybrid structure is maintained by suppression: artists lack bargaining power, face information asymmetry about actual card sales and collector premiums, and have limited alternatives for mass-market art circulation. Active enforcement of licensing terms against artists is rare; enforcement against unauthorized uses is theatrical.
constraint_indexing:constraint_classification(nyc_metrocard_art_licensing, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nyc_metrocard_art_licensing_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(nyc_metrocard_art_licensing, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nyc_metrocard_art_licensing, TypeOther, context(agent_power(moderate), _, _, _)),
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
 *   Extractiveness (0.52): Above the median. MTA captures perpetual rights to artwork, controls distribution to 5+ million daily transit users, monetizes collector interest through limited edition variants, and retains secondary market premiums. Artists receive one-time payments (typically $500-$2,500 based on public reports) while MTA benefits from decades of collector demand. The extraction is not absolute (artists do gain legitimate exposure and portfolio value, and coordination function is real) but measurable and asymmetric. Suppression (0.65): Moderately high. Information asymmetry (artists don't know actual card sales volumes or collector premium percentages), power asymmetry (institutional MTA vs individual artists), limited alternatives for comparable mass-market distribution, and weak enforcement mechanisms against unauthorized secondary market uses all suppress alternatives and reduce artist leverage. Theater ratio (0.58): Moderate-high. The licensing apparatus performs copyright formality (standard contracts are executed) while enforcement is weak — secondary market reselling, unauthorized reproductions by collectors, and derivative uses proceed with minimal artist compensation or MTA monitoring. The increase over time (0.42 to 0.58) reflects that licensing language became more elaborate and formal as collector value was recognized, while actual enforcement and artist compensation did not scale proportionally.
 *
 * PERSPECTIVAL GAP:
 *   Three distinct structural positions produce three distinct classifications from identical base properties. The independent artist sees a snare because they are powerless, trapped, and bear full extraction. The MTA sees a rope because it is institutional, has arbitrage options, and experiences genuine coordination benefits. The organized artist collective sees a tangled rope because they have some power and some benefits, but constrained by information asymmetry and limited alternatives. This perspectival gap reveals the constraint's true structure: it is a hybrid that extracts from powerless agents while coordinating benefits for powerful actors. The disagreement is not subjective or contextual — it follows mathematically from the indexical tuple and the structural facts about who benefits and who bears costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Independent artists: victim status + powerless + trapped → derived d ≈ 0.95 → f(d) ≈ 1.42, producing high experienced extractiveness (snare). MTA: beneficiary status + institutional + arbitrage → derived d ≈ 0.05 → f(d) ≈ -0.12, producing negative experienced extractiveness (rope). Artist collective: mixed (some coordination benefit from exposure) + moderate + constrained → derived d ≈ 0.60 → f(d) ≈ 0.85, producing moderate-high extractiveness (tangled_rope). Collectors: pure beneficiary + powerful + arbitrage → derived d ≈ 0.10 → f(d) ≈ -0.05, producing negative extractiveness (rope). The scope modifier σ(S) for regional scope is 0.9, dampening χ slightly compared to national scope. The effective extraction formula χ = ε × f(d) × σ(S) produces: for artists, χ ≈ 0.52 × 1.42 × 0.9 ≈ 0.67 (snare); for MTA, χ ≈ 0.52 × (-0.12) × 0.9 ≈ -0.06 (rope); for collective, χ ≈ 0.52 × 0.85 × 0.9 ≈ 0.40 (tangled_rope).
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED: The constraint avoids mandatrophy (false coordination disguised as extraction) because both coordination and extraction are present in the structure. The MTA genuinely benefits from art licensing (coordination is real). Independent artists genuinely bear extraction (asymmetric rights transfer is real). The constraint is tangled_rope from the analytical perspective precisely because both aspects coexist. The mandatrophy would arise if a beneficiary (MTA) falsely claimed that the licensing agreement was 'pure coordination' with 'equal benefit to all parties' — this framing would be mandathropous because it erases the asymmetric extraction and misrepresents trapped artists as equal participants. Conversely, a snare framing that ignores the genuine exposure and portfolio value artists receive would also be mandathropous. The tangled_rope classification captures the true structure: coordination (art enhances transit and collections) + asymmetric extraction (perpetual rights, minimal compensation, information gaps) + suppression (power imbalance, limited alternatives). Resolution is empirical: omega variables about actual artist compensation percentages and copyright reversion terms would strengthen or weaken the tangled_rope claim by revealing whether extraction dominates coordination (→snare) or coordination dominates extraction (→rope).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    actual_artist_compensation_opacity,
    'What percentage of MetroCard collector premium revenue actually flows to featured artists versus retained by MTA and retailers?',
    'Audit of MTA licensing contract terms and royalty accounting; comparison with collector secondary market prices; artist survey of actual payments received',
    'If artist share < 5%: snare classification confirmed even from moderate perspective. If artist share > 25%: rope classification more defensible. Current opacity enables extraction extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(actual_artist_compensation_opacity, empirical, 'Percentage of MetroCard collector premium revenue flowing to artists').

omega_variable(
    copyright_reversion_terms,
    'Do licensing agreements include automatic copyright reversion to artists after a fixed term, or do rights remain perpetual to MTA?',
    'Analysis of standard MTA licensing contracts; historical examples of rights reverted or retained; comparison with industry standards for limited-edition merchandise licensing',
    'If perpetual with no reversion: extraction mechanism confirmed (artist surrenders control permanently). If reversion included: more balanced rope structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(copyright_reversion_terms, empirical, 'Copyright reversion terms in MTA licensing agreements').

omega_variable(
    alternative_artist_circulation_pathways,
    'Are there viable alternative mechanisms for independent artists to achieve comparable mass-market distribution and cultural legitimacy without MTA licensing?',
    'Analysis of artist portfolio value contributions from transit card licensing vs social media, gallery representation, merchandise platforms; artist career outcome tracking',
    'If alternatives exist: artists have true exit options (mobile/arbitrage), reducing suppression. If MetroCard is a near-monopoly pathway: suppression and extraction confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_artist_circulation_pathways, empirical, 'Viability of alternative mass-market distribution pathways for artists').

omega_variable(
    collector_secondary_market_governance,
    'Who controls and profits from the secondary market resale of limited-edition MetroCards — artists, MTA, retailers, or collectors themselves?',
    'Examination of collector trading platforms (eBay, specialty card dealers); artist royalty structures for secondary sales; MTA licensing terms regarding resale rights',
    'If artists receive secondary sales royalties: extraction reduced. If collectors/retailers capture all resale premium with no artist benefit: extraction mechanism strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collector_secondary_market_governance, empirical, 'Secondary market governance and profit distribution for MetroCard resales').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nyc_metrocard_art_licensing, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(metrocard_tr_t0, nyc_metrocard_art_licensing, theater_ratio, 0, 0.42).
narrative_ontology:measurement(metrocard_tr_t15, nyc_metrocard_art_licensing, theater_ratio, 15, 0.52).
narrative_ontology:measurement(metrocard_tr_t30, nyc_metrocard_art_licensing, theater_ratio, 30, 0.58).

% Extraction over time
narrative_ontology:measurement(metrocard_be_t0, nyc_metrocard_art_licensing, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(metrocard_be_t15, nyc_metrocard_art_licensing, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(metrocard_be_t30, nyc_metrocard_art_licensing, base_extractiveness, 30, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nyc_metrocard_art_licensing, resource_allocation).
narrative_ontology:affects_constraint(nyc_metrocard_art_licensing, cultural_labor_extraction).
narrative_ontology:affects_constraint(nyc_metrocard_art_licensing, intellectual_property_asymmetry).

% DUAL FORMULATION NOTE:
% The MetroCard licensing agreement represents a specific instantiation of broader constraints on cultural labor extraction and intellectual property asymmetry in transit systems. This story decomposes the coordination mechanism (genuine transit system value from art licensing) from the extraction mechanism (asymmetric copyright transfer and compensation). The upstream constraint (cultural_labor_extraction) has ε ≈ 0.68 (snare); this story's hybrid nature (ε ≈ 0.52) reflects that MTA coordination function is real. The downstream constraint (intellectual_property_asymmetry) inherits the perpetual rights structure described here.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nyc_metrocard_art_licensing, organized, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
