% ============================================================================
% CONSTRAINT STORY: harlequin_historical_line_discontinuation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-02-24
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
 *   Harlequin's 2025 discontinuation of its dedicated Historical romance
 *   imprint represents a consolidation extraction that masks economic
 *   efficiency rhetoric with structural harm to genre diversity, author
 *   pathways, and reader ecosystem resilience. The discontinuation affects
 *   mid-list authors who built careers within Harlequin's historical
 *   infrastructure, readers who rely on the imprint's curation and
 *   distribution, and the broader publishing ecosystem's ability to sustain
 *   specialist genres. The constraint exhibits snare characteristics: high
 *   suppression (concentration of reader acquisition and distribution
 *   networks), high extractiveness (authors and readers have limited
 *   alternatives), and increasing theater (justification framing
 *   consolidation as 'market rationality'). The corporate beneficiary
 *   (HarperCollins/Harlequin leadership) experiences the discontinuation as
 *   rational portfolio optimization; the displaced authors and readers
 *   experience it as extraction with no recourse. Independent publishers
 *   benefit modestly from reduced competition but lack the distribution scale
 *   to fully absorb displaced demand.
 *
 * KEY AGENTS:
 *   - Mid-list Historical Romance Authors: Primary victim (powerless/trapped) — built careers within Harlequin infrastructure; face barrier to establishing presence at other imprints; dependent on platform distribution
 *   - Historical Romance Readers: Primary victim (powerless/constrained) — face reduced curation and discovery friction; may exit romance category if alternative sources are insufficient
 *   - Harlequin/HarperCollins Corporate Leadership: Primary beneficiary (institutional/arbitrage) — reduce operational overhead and consolidate marketing spend; capture margin from portfolio simplification
 *   - Contemporary Romance Division: Secondary beneficiary (institutional/arbitrage) — recapture reader attention and author bandwidth previously devoted to historical subgenre
 *   - Independent Historical Romance Publishers: Secondary actor (moderate/constrained) — benefit from reduced competition and displaced author supply but lack distribution scale of major publisher
 *   - Publishing Diversity Ecosystem: Analytical victim — abstract collective good; sustained by multiple imprints competing for genre niches; loses resilience with consolidation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(harlequin_historical_line_discontinuation, 0.58).
domain_priors:suppression_score(harlequin_historical_line_discontinuation, 0.65).
domain_priors:theater_ratio(harlequin_historical_line_discontinuation, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(harlequin_historical_line_discontinuation, extractiveness, 0.58).
narrative_ontology:constraint_metric(harlequin_historical_line_discontinuation, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(harlequin_historical_line_discontinuation, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(harlequin_historical_line_discontinuation, snare).
narrative_ontology:human_readable(harlequin_historical_line_discontinuation, "Harlequin's Discontinuation of its Dedicated Historical Romance Imprint").
narrative_ontology:topic_domain(harlequin_historical_line_discontinuation, "economic/publishing").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(harlequin_historical_line_discontinuation, harlequin_corporate_parent).
narrative_ontology:constraint_beneficiary(harlequin_historical_line_discontinuation, contemporary_romance_division).
narrative_ontology:constraint_victim(harlequin_historical_line_discontinuation, historical_romance_authors).
narrative_ontology:constraint_victim(harlequin_historical_line_discontinuation, historical_romance_readers).
narrative_ontology:constraint_victim(harlequin_historical_line_discontinuation, publishing_diversity_ecosystem).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISCONTINED HISTORICAL ROMANCE AUTHORS (SNARE) — Mid-list authors dependent on Harlequin's historical line have no equivalent publisher ecosystem for historical romance at scale. Trapped by publishing infrastructure concentration and reader distribution networks. d≈0.92, f(d)≈1.40, σ=1.2 → χ≈0.96. Maximum extraction.
constraint_indexing:constraint_classification(harlequin_historical_line_discontinuation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: HISTORICAL ROMANCE READERS (SNARE) — Loss of dedicated imprint signals narrowing of genre diversity; readers face higher friction finding historical romance in consolidated market. Cannot exit existing author relationships easily. d≈0.88, f(d)≈1.35, σ=1.2 → χ≈0.92.
constraint_indexing:constraint_classification(harlequin_historical_line_discontinuation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: HARLEQUIN CORPORATE LEADERSHIP (ROPE) — Discontinuation is justified as rational portfolio consolidation. Publishers benefit from simplified operational overhead and concentrated marketing spend on higher-margin contemporary romance. Experiences constraint as efficient resource allocation. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.07. Net beneficiary.
constraint_indexing:constraint_classification(harlequin_historical_line_discontinuation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INDEPENDENT / SMALL-PRESS HISTORICAL ROMANCE PUBLISHERS (TANGLED ROPE) — Benefit from reduced competition for reader attention and displaced author pipelines; also face spillover from Harlequin's network effects and distribution advantages. Constrained by limited marketing budgets. d≈0.50, f(d)≈0.65, σ=0.9 → χ≈0.34.
constraint_indexing:constraint_classification(harlequin_historical_line_discontinuation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CONSOLIDATED PUBLISHING NORM (PITON) — Industry rationalization (fewer, larger imprints; concentration of reader acquisition spend) is performatively justified as 'market efficiency' but masks extraction of margin from specialist genres. theater_ratio=0.48 indicates moderate performative overlay. The consolidation narrative naturalizes extraction as inevitable industry evolution.
constraint_indexing:constraint_classification(harlequin_historical_line_discontinuation, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER — GENRE DIVERSITY COST (SNARE) — The discontinuation extracts from the public good of genre diversity and reader/author ecosystem resilience. Over time, imprint consolidation reduces the diversity of publishing pathways available to authors and readers. d≈0.90, f(d)≈1.38, σ=1.2 → χ≈0.94.
constraint_indexing:constraint_classification(harlequin_historical_line_discontinuation, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(harlequin_historical_line_discontinuation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(harlequin_historical_line_discontinuation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(harlequin_historical_line_discontinuation, TypeOther, context(agent_power(institutional), _, _, _)),
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
 *   Extractiveness (0.58): High-moderate. The discontinuation extracts real economic value from authors (reduced royalty opportunities, reputational cost of imprint closure, transition friction) and readers (reduced genre diversity, higher search costs). However, the extraction is not maximal (0.70+) because some displaced authors and readers can migrate to independent publishers or self-publishing, and the consolidation rationale (lower operational overhead) reflects genuine cost pressure rather than pure rent-seeking. The trajectory from 0.35 to 0.58 reflects increasing extraction over 4 years as corporate strategic clarity solidifies and authors face harder choices. Suppression (0.65): High. The consolidation of publishing infrastructure concentrates reader discovery and author placement in fewer hands. Barriers to alternative publishing include: (1) diminished visibility for independent publishers (no bookstore shelf space equivalent to Harlequin's retail footprint), (2) tacit knowledge sunk in Harlequin systems (cover design, marketing templates, reader expectations), (3) career risk for mid-list authors of pivoting to smaller presses with unproven sales channels. Theater ratio (0.48): Moderate. The discontinuation is partially justified through legitimate efficiency arguments (operational consolidation, portfolio rationalization) but also masks extractive motives (consolidate reader attention, simplify marketing). The theater has increased from 0.32 to 0.48 as corporate communications shifted from neutral portfolio management to efficiency framing.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary perspective (Harlequin leadership) sees rational consolidation — eliminating a lower-margin imprint allows concentrated investment in high-performing contemporary romance lines. The victim perspectives (authors, readers) see extraction — loss of viable publishing pathway and reader access to preferred genre. Independent publishers occupy a middle position: they benefit from reduced competition but cannot fully absorb displaced demand due to distribution disadvantages. The analytical observer perspective captures the long-term systemic cost: each imprint discontinuation reduces the diversity of publishing pathways, concentrating author and reader choice in fewer mega-publishers. The snare classification reflects that authors and readers cannot easily exit the constraint — the publishing infrastructure is not a free market but a concentrated oligopoly where Harlequin's scale and distribution network are difficult to replicate.
 *
 * DIRECTIONALITY LOGIC:
 *   Harlequin/HarperCollins leadership: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary through portfolio consolidation and margin capture. Mid-list historical authors: Victim + trapped → d≈0.92, f(d)≈1.40. Maximum extraction — authors lack alternative infrastructure at scale and face reputational cost of imprint discontinuation. Historical romance readers: Victim + constrained → d≈0.88, f(d)≈1.35. High extraction — readers can migrate to other publishers but face friction and potential genre abandonment if quality alternatives are insufficient. Independent publishers: Mixed beneficiary/victim → d≈0.50, f(d)≈0.65. Benefit from reduced competition but constrained by limited distribution reach. Publishing diversity ecosystem: Victim + trapped → d≈0.90, f(d)≈1.38. Abstract collective good that cannot organize or exit; each imprint closure reduces ecosystem resilience.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the snare is institutional rather than natural. The corporate justification ('market efficiency') risks naturalizing what is a contingent choice to consolidate rather than invest in underperforming imprints. A true mountain would be an immutable law of economics; the snare is a structure of incentives shaped by capital concentration and corporate ownership. The beneficiary perspective (efficiency) is legitimate within corporate accounting logic; the victim perspectives (loss of publishing diversity) are legitimate within ecosystem and author-resilience logic. The constraint's extractiveness arises not from physics or mathematics but from market structure — concentrated publishing firms can enforce extraction (discontinuation) because alternative pathways are scarce. Resolution would require either: (1) sustained investment in historical romance despite lower margins (cultural/mission-driven choice), or (2) ecosystem restructuring that enables independent publishers to compete for author/reader attention. The snare persists because neither condition currently obtains.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    author_displacement_magnitude,
    'How many mid-list Harlequin Historical authors lack viable alternative publishing pathways post-discontinuation?',
    'Author career tracking; comparison of publishing output and income for authors before/after 2025 discontinuation announcement; survey of affected authors regarding alternative imprint acceptance rates',
    'If >60% of affected authors fail to place subsequent works: extraction is severe and sustained. If <20% face significant displacement: extraction is moderate and partially absorbed by industry reallocation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(author_displacement_magnitude, empirical, 'Magnitude of author displacement from Harlequin Historical discontinuation').

omega_variable(
    reader_retention_to_contemporary,
    'Do historical romance readers transition to contemporary romance imprints, or do they exit the Harlequin ecosystem entirely?',
    'Tracking of reader purchasing behavior post-discontinuation; Goodreads/online community analysis of reader sentiment; comparison of contemporary romance sales uplift to historical romance sales loss',
    'If readers transition to contemporary: corporate extraction is real but reader harm is partial. If readers exit Harlequin entirely: extraction is severe and creates gaps in reader satisfaction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reader_retention_to_contemporary, empirical, 'Reader retention pattern post-discontinuation').

omega_variable(
    competitive_ecosystem_absorption,
    'Do small-press historical romance publishers have sufficient capital and distribution reach to absorb the displaced author/reader demand, or does consolidation reduce overall market diversity?',
    'Market share analysis of small-press historical romance publishers pre/post 2025; entry/exit rates for historical romance indies; reader accessibility metrics (discoverability, pricing, availability)',
    'If small-press absorption is >80% effective: snare extracts market dominance but diversity persists. If <40% effective: snare extracts author/reader choice and reinforces publishing consolidation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(competitive_ecosystem_absorption, empirical, 'Capacity of alternative publishers to absorb displaced historical romance ecosystem').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(harlequin_historical_line_discontinuation, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(harlequin_hist_tr_t0, harlequin_historical_line_discontinuation, theater_ratio, 0, 0.32).
narrative_ontology:measurement(harlequin_hist_tr_t2, harlequin_historical_line_discontinuation, theater_ratio, 2, 0.4).
narrative_ontology:measurement(harlequin_hist_tr_t4, harlequin_historical_line_discontinuation, theater_ratio, 4, 0.48).

% Extraction over time
narrative_ontology:measurement(harlequin_hist_be_t0, harlequin_historical_line_discontinuation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(harlequin_hist_be_t2, harlequin_historical_line_discontinuation, base_extractiveness, 2, 0.52).
narrative_ontology:measurement(harlequin_hist_be_t4, harlequin_historical_line_discontinuation, base_extractiveness, 4, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(harlequin_historical_line_discontinuation, resource_allocation).
narrative_ontology:affects_constraint(harlequin_historical_line_discontinuation, publishing_market_consolidation).
narrative_ontology:affects_constraint(harlequin_historical_line_discontinuation, genre_diversity_erosion).
narrative_ontology:affects_constraint(harlequin_historical_line_discontinuation, author_career_fragility).

% DUAL FORMULATION NOTE:
% Harlequin Historical discontinuation is downstream of broader publishing consolidation (HarperCollins acquiring Penguin Random House competing imprints, Amazon's distribution control). The specific constraint has ε=0.58 reflecting the extractive impact on historical romance ecosystem; upstream consolidation constraints have different ε values reflecting their broader market scope.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
