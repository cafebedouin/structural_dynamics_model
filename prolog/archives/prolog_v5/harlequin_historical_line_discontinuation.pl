% ============================================================================
% CONSTRAINT STORY: harlequin_historical_line_discontinuation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_harlequin_historical_line_discontinuation, []).

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
 *   constraint_id: harlequin_historical_line_discontinuation
 *   human_readable: Harlequin's Discontinuation of its Dedicated Historical Romance Imprint
 *   domain: economic/publishing
 *
 * SUMMARY:
 *   Harlequin's 2025 discontinuation of its dedicated Historical Romance
 *   imprint represents a structural constraint operating at the intersection
 *   of corporate portfolio optimization, reader preference fragmentation, and
 *   author career dependency on single-publisher institutional support. The
 *   constraint exhibits simultaneous extraction (from authors and readers
 *   trapped by network effects and brand dependency) and coordination
 *   (resource reallocation within Harlequin's contemporary romance strategy).
 *   The core tension: Harlequin presents the discontinuation as responding to
 *   market demand, but the discontinuation itself may be driving the demand
 *   signal it claims to observe, by redirecting editorial resources,
 *   marketing budget, and retail shelf space toward contemporary lines. This
 *   creates a snare for trapped historical romance authors and readers, while
 *   simultaneously benefiting organized actors (contemporary imprints, indie
 *   publishers) positioned to arbitrage the transition. The theater ratio
 *   (0.48) reflects moderate performative content: the discontinuation is
 *   justified through market-language ('declining sales,' 'reader
 *   preference'), but these metrics are themselves products of corporate
 *   categorization and resource allocation decisions that are not fully
 *   disclosed.
 *
 * KEY AGENTS:
 *   - Harlequin Historical Authors (mid-career, under contract): Primary victims (powerless/trapped) — face series termination, backlist removal, brand identity collapse, limited alternative distribution
 *   - Historical Romance Reader Community: Primary victims (powerless/trapped) — lose reliable supply source, forced to search for alternatives, experience network disruption
 *   - HarperCollins / Harlequin Corporate Leadership: Primary beneficiary (institutional/arbitrage) — reallocate resources to higher-margin contemporary lines, simplify portfolio management, execute strategic shift
 *   - Contemporary Romance Imprints: Secondary beneficiary (organized/constrained) — gain editorial resources and marketing budget freed by Historical discontinuation; benefit from reduced internal competition; constrained by company-level viability
 *   - Independent and Small Press Publishers: Tertiary beneficiary (organized/mobile) — see market opening, can acquire displaced author backlist, access displaced readers; have exit options and can enter market at reduced cost
 *   - Romance Reader Market Structure: Institutional observer (institutional/arbitrage) — reflects institutional category preferences and resource allocation decisions rather than inherent reader demand; benefits from simplified corporate portfolio
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(harlequin_historical_line_discontinuation, 0.58).
domain_priors:suppression_score(harlequin_historical_line_discontinuation, 0.62).
domain_priors:theater_ratio(harlequin_historical_line_discontinuation, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(harlequin_historical_line_discontinuation, extractiveness, 0.58).
narrative_ontology:constraint_metric(harlequin_historical_line_discontinuation, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(harlequin_historical_line_discontinuation, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(harlequin_historical_line_discontinuation, snare).
narrative_ontology:human_readable(harlequin_historical_line_discontinuation, "Harlequin's Discontinuation of its Dedicated Historical Romance Imprint").
narrative_ontology:topic_domain(harlequin_historical_line_discontinuation, "economic/publishing").

domain_priors:requires_active_enforcement(harlequin_historical_line_discontinuation).
% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(harlequin_historical_line_discontinuation, harlequin_parent_company).
narrative_ontology:constraint_beneficiary(harlequin_historical_line_discontinuation, contemporary_romance_imprints).
narrative_ontology:constraint_victim(harlequin_historical_line_discontinuation, historical_romance_authors).
narrative_ontology:constraint_victim(harlequin_historical_line_discontinuation, historical_romance_readers).
narrative_ontology:constraint_victim(harlequin_historical_line_discontinuation, subgenre_ecosystem).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HISTORICAL ROMANCE AUTHORS (SNARE) — Mid-career authors with existing Harlequin Historical contracts face extraction through discontinuation: their series are terminated, backlist goes out of print, brand identity collapses. Trapped by contract terms, by lack of alternative distribution channels at comparable scale, and by reader bases tied to the Harlequin brand. Career damage and lost future revenue. No meaningful exit option within the romance market's institutional structure.
constraint_indexing:constraint_classification(harlequin_historical_line_discontinuation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: HISTORICAL ROMANCE READERS (SNARE) — Readers habituated to Harlequin Historical as a reliable source for a specific product (historical romance with consistent heat/length/tropes) face extraction through supply collapse. Alternative sources exist (indie publishing, small presses) but require active search and offer no equivalent quality-control guarantee. Network effects favor series reading; losing Harlequin Historical disrupts established reading habits. Trapped by preference formation and by the fragmentation of the subgenre.
constraint_indexing:constraint_classification(harlequin_historical_line_discontinuation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: CONTEMPORARY ROMANCE IMPRINTS (TANGLED ROPE) — Harlequin's contemporary lines (Desire, Intrigue, Presents) benefit from the reallocation of editorial resources, marketing budget, and shelf space freed by Historical's discontinuation. Also benefit from reduced competition for reader attention within the Harlequin ecosystem. But this is a hybrid: they also depend on Harlequin's continued viability and on romance reader market size overall. Discontinuation may signal reader decline, affecting contemporary lines downstream. Constrained by company-level strategic decisions.
constraint_indexing:constraint_classification(harlequin_historical_line_discontinuation, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: HARLEQUIN CORPORATE LEADERSHIP (ROPE) — Faces the constraint as a portfolio optimization problem: reallocating resources from a lower-margin historical line to contemporary lines with better market metrics and digital adaptation potential. The discontinuation is the solution to a coordination problem: matching product portfolio to market demand. Benefits from margin improvement and operational simplification. Exit options include digital-only legacy models, licensing backlist to smaller presses, or other arbitrage strategies. Experiences the constraint as functional coordination.
constraint_indexing:constraint_classification(harlequin_historical_line_discontinuation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INDIE/SMALL PRESS PUBLISHERS (SCAFFOLD) — See this as a temporary disruption creating a market opening. Author backlist becomes available, displaced readers seek alternative sources, and there is a defined sunset: the transition period (12-24 months of warehouse clear-out and final publication runs) after which the market stabilizes at a new equilibrium. Small publishers have mobile exit options and can enter the historical romance market at lower cost now that Harlequin has de-emphasized it. Theater is low because the business logic is straightforward.
constraint_indexing:constraint_classification(harlequin_historical_line_discontinuation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ROMANCE READER MARKET STRUCTURE (PITON) — From a civilizational view, this constraint reflects the atrophy of historical romance as a cultural category. Harlequin Historical was an institutional bulwark of the subgenre; its discontinuation is performative: the category is being abandoned because it has already atrophied within corporate publishing logic, not because the subgenre is genuinely extinct (indie historical romance thrives). The constraint persists due to institutional inertia in how corporate publishers define categories and allocate resources. The theater is high because the 'market demand' justification obscures publisher category preferences.
constraint_indexing:constraint_classification(harlequin_historical_line_discontinuation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN RISK) — Risks classifying this as a natural law: 'Publishing markets naturally select for contemporary over historical; Harlequin is responding to inevitable demand signals.' However, this naturalizes a contingent institutional decision. Market selection is real, but the selection mechanism is itself structured (corporate profit margins, genre categorization schemas, retail shelf space allocation, marketing budgets). The appearance of inevitability derives from these institutional structures, not from immutable properties of reader preference.
constraint_indexing:constraint_classification(harlequin_historical_line_discontinuation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(harlequin_historical_line_discontinuation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(harlequin_historical_line_discontinuation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(harlequin_historical_line_discontinuation, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(harlequin_historical_line_discontinuation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(harlequin_historical_line_discontinuation, TR),
    TR >= 0.70.

:- end_tests(harlequin_historical_line_discontinuation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately-high. The discontinuation extracts from authors through contract termination, backlist removal, and career disruption. It extracts from readers through supply collapse and search costs. But extraction is not maximal (0.66+) because alternative sources exist (indie publishing, small presses), and the extraction is time-bounded — the market will restabilize after the transition period (12-24 months). The extractiveness value reflects the real but not total trapping of both author and reader populations. Suppression (0.62): High. Harlequin Historical authors have limited alternatives within the traditional publishing ecosystem; readers face search and quality-verification costs; market entry barriers for small presses seeking to replace Harlequin's distribution network are substantial. But suppression is not total — indie publishing and small presses do provide alternatives, reducing the suppression below 0.70. Theater ratio (0.48): Moderate-low. The discontinuation is justified through market-language and financial metrics, but the actual business logic is straightforward: portfolio optimization driven by corporate margin targets. There is some performative element (the 'market demand' framing obscures category preferences), but the underlying extraction mechanism is transparent — this is not a false compliance ritual like peer review theater. The theatrical content derives from information asymmetry about corporate financial data, not from functional degradation.
 *
 * PERSPECTIVAL GAP:
 *   The constraint generates a five-way perspectival gap. Authors and readers (powerless/trapped) experience maximal extraction and see a snare. Contemporary romance imprints (organized/constrained) see a tangled rope — they benefit from resource reallocation but depend on Harlequin's viability and romance market health overall. Corporate leadership (institutional/arbitrage) sees a rope — solving a portfolio coordination problem. Indie publishers (organized/mobile) see a scaffold — a temporary disruption creating an opening. The market structure (civilizational) risks being classified as a mountain ('publishing naturally selects contemporary over historical') but is actually a piton — the appearance of inevitability derives from institutional inertia in how corporate publishers categorize and allocate resources, not from immutable properties of reader demand.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from structural position relative to the extraction mechanism. Authors with active contracts and reader bases tied to the Harlequin Historical brand occupy the victim end of the spectrum (high d, approaching 1.0) — they cannot exit without career damage. Readers with reading habits synchronized to Harlequin Historical releases occupy similar positions. HarperCollins corporate leadership occupies the beneficiary end (low d, approaching 0.0) — they gain margin improvement and portfolio simplification. Indie publishers occupy a middle position (d ≈ 0.4-0.5) — they benefit from market opening but face execution risk in building author/reader relationships. The contemporary romance imprints occupy a constrained middle (d ≈ 0.5-0.6) — they benefit from resource reallocation but depend on company strategy. The constraint's directionality structure is asymmetric by design: extraction flows from trapped populations toward institutional beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy resolves through perspectival decomposition. The snare classification (authors and readers perspective) is structurally accurate: extraction is real, suppression is high, and trapped agents experience maximal chi. The tangled rope classification (contemporary imprints perspective) correctly identifies that the constraint has both a coordination function (resource reallocation solving a portfolio problem) and asymmetric extraction (contemporary lines gain at historical's expense, creating a mixed experience). The scaffold classification (indie publishers perspective) identifies the temporal structure: there is a real sunset (transition period after which market restabilizes). The mountain risk (market structure perspective) is a false summit: the appearance that 'publishing naturally selects contemporary over historical' naturalizes what is actually a contingent institutional decision about category prioritization and resource allocation. The corporation could choose to maintain Historical as a niche imprint; it chooses not to, and frames that choice as market response. No single classification is 'correct' — the presheaf over the observation site includes all six.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    actual_market_demand_vs_publisher_preference,
    'Does the historical romance discontinuation reflect genuine reader demand decline or publisher preference for contemporary romance''s margins and digital adaptation profiles?',
    'Comparative analysis: sales trends for historical romance vs contemporary romance lines (2015-2025); reader survey data on demand suppression vs genuine preference shift; indie publishing growth in historical romance during same period; margin analysis for historical vs contemporary titles',
    'If genuine demand decline: snare classification is accurate — market is actually selecting. If publisher preference: snare is amplified — extraction is masquerading as market response. If demand suppressed by marketing reallocation: snare is reversed — publisher choices created the appearance of declining demand.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(actual_market_demand_vs_publisher_preference, empirical, 'Market demand decline vs publisher portfolio preference').

omega_variable(
    author_exit_capacity_post_discontinuation,
    'Can displaced Harlequin Historical authors successfully transition to indie publishing, small presses, or other platforms without catastrophic career damage?',
    'Longitudinal tracking of 20+ Harlequin Historical authors post-discontinuation: royalty income, reader base retention, backlist availability, contract terms with new publishers, career satisfaction surveys',
    'If high exit capacity: snare classification weakens (authors are constrained, not trapped). If low capacity: snare is validated (contractual and reputational barriers are real extraction mechanisms). If highly variable: perspectival gap widens — organized authors with large fan bases escape, powerless mid-list authors remain trapped.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(author_exit_capacity_post_discontinuation, empirical, 'Whether authors can transition to alternative publishing platforms').

omega_variable(
    reader_defection_permanence,
    'Do historical romance readers who switch to indie or small-press sources during the Harlequin Historical transition permanently leave the Harlequin ecosystem, or do they return to contemporary lines?',
    'Reader cohort tracking: survey former Harlequin Historical readers at 6, 12, 24 months post-discontinuation; measure adoption of alternative sources; track purchases of contemporary Harlequin lines by historical romance defectors',
    'If defection is permanent: contemporary romance lines lose potential future market (extraction backfires). If readers return: extraction is real but limited in duration (confirms tangled rope for contemporary lines). If readers fragment: ecosystem fragmentation becomes permanent (snare for subgenre structure itself).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reader_defection_permanence, empirical, 'Whether reader defection to alternatives is permanent').

omega_variable(
    corporate_margin_calculation_transparency,
    'Are Harlequin''s published justifications for Historical discontinuation (declining sales, reader preference) supported by disclosed margin and market data, or are margins and portfolio strategy opaque?',
    'FOIA/investor disclosure requests for HarperCollins financial data on Harlequin imprint profitability; cross-reference with industry analyst reports; compare Harlequin Historical margin trajectories (2015-2025) to indie historical romance profitability metrics',
    'If transparent data supports discontinuation: snare classification assumes corporate logic is accurate. If data is opaque or contradictory: suppression (information asymmetry) is a primary extraction mechanism, elevating classification to high-extraction snare. If data shows historical was more profitable than claimed: discontinuation is pure rent-seeking (snare confirmed).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(corporate_margin_calculation_transparency, empirical, 'Transparency of financial justifications for discontinuation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(harlequin_historical_line_discontinuation, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hh_hist_tr_t0, harlequin_historical_line_discontinuation, theater_ratio, 0, 0.38).
narrative_ontology:measurement(hh_hist_tr_t6, harlequin_historical_line_discontinuation, theater_ratio, 6, 0.43).
narrative_ontology:measurement(hh_hist_tr_t12, harlequin_historical_line_discontinuation, theater_ratio, 12, 0.48).

% Extraction over time
narrative_ontology:measurement(hh_hist_be_t0, harlequin_historical_line_discontinuation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(hh_hist_be_t6, harlequin_historical_line_discontinuation, base_extractiveness, 6, 0.5).
narrative_ontology:measurement(hh_hist_be_t12, harlequin_historical_line_discontinuation, base_extractiveness, 12, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(harlequin_historical_line_discontinuation, resource_allocation).
narrative_ontology:affects_constraint(harlequin_historical_line_discontinuation, romance_subgenre_fragmentation).
narrative_ontology:affects_constraint(harlequin_historical_line_discontinuation, indie_publishing_market_growth).

% DUAL FORMULATION NOTE:
% The discontinuation is a downstream consequence of corporate publishing's genre categorization system and margin-driven portfolio management. It affects broader constraints on romance subgenre viability and indie publishing economics. Separate constraint stories for those upstream/downstream relationships would decompose the ecosystem-level effects.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(harlequin_historical_line_discontinuation, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
