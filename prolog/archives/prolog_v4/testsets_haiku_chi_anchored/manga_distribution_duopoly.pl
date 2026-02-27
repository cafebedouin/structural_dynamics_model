% ============================================================================
% CONSTRAINT STORY: manga_distribution_duopoly
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_manga_distribution_duopoly, []).

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
 *   constraint_id: manga_distribution_duopoly
 *   human_readable: Manga Distribution Duopoly in North America
 *   domain: economic/publishing
 *
 * SUMMARY:
 *   The North American manga distribution market exhibits duopolistic control
 *   by Viz Media and Yen Press, which dominate retail placement through
 *   exclusive licensing agreements with major Japanese publishers and
 *   long-standing relationships with bookstores and comic shops. This
 *   constraint operates at the interface between Japanese intellectual
 *   property, North American logistics/retail infrastructure, and reader
 *   demand. The duopoly extracts economic rent from independent publishers
 *   and creators who lack alternative distribution channels, while also
 *   extracting from Japanese rights holders through unfavorable licensing
 *   terms (typically 40-50% of retail proceeds to distributor, 10-15% to
 *   original licensee). Extractiveness has increased over the past 20 years
 *   as the duopolists have consolidated market share and consolidated retail
 *   relationships. However, a scaffold dynamic is emerging: webcomic
 *   platforms, direct digital distribution, and Kickstarter-funded projects
 *   represent growing exit routes for creators, particularly younger cohorts.
 *   The theater ratio reflects that duopolistic gatekeeping involves both
 *   functional coordination (retail standardization, reliable distribution
 *   logistics) and performative control (shelf placement decisions driven by
 *   distributor preference rather than reader demand).
 *
 * KEY AGENTS:
 *   - Viz Media: Primary beneficiary (institutional/arbitrage) — dominant market position, exclusive licensing control, retail channel dominance
 *   - Yen Press: Primary beneficiary (institutional/arbitrage) — secondary but significant market position, exclusive licensing control, retail channel presence
 *   - Independent Manga Publishers: Primary victim (powerless/trapped) — no viable distribution alternatives; must negotiate unfavorable terms or abandon print distribution
 *   - Small Press Creators: Primary victim (moderate/constrained) — cannot reach retail independently; forced into contract terms (10-15% royalties) or self-publishing with minimal income
 *   - North American Readers: Secondary victim (moderate/mobile) — limited title selection; benefits from retail availability but constrained by gatekeeping
 *   - Japanese Publishers / Rights Holders: Secondary victim (organized/constrained) — locked into exclusive long-term contracts; limited direct access to North American market
 *   - Print Retail Infrastructure: Institutional channel (institutional/arbitrage) — reinforces duopolistic control through dependence on distributor relationships; piton status reflects degradation as digital alternatives erode retail relevance
 *   - Digital Distribution Platforms: Emerging alternative (organized/mobile) — Webtoon, Tapas, Patreon, Kickstarter represent structural sunset for print duopoly
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(manga_distribution_duopoly, 0.58).
domain_priors:suppression_score(manga_distribution_duopoly, 0.68).
domain_priors:theater_ratio(manga_distribution_duopoly, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(manga_distribution_duopoly, extractiveness, 0.58).
narrative_ontology:constraint_metric(manga_distribution_duopoly, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(manga_distribution_duopoly, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(manga_distribution_duopoly, snare).
narrative_ontology:human_readable(manga_distribution_duopoly, "Manga Distribution Duopoly in North America").
narrative_ontology:topic_domain(manga_distribution_duopoly, "economic/publishing").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(manga_distribution_duopoly, viz_media).
narrative_ontology:constraint_beneficiary(manga_distribution_duopoly, yen_press).
narrative_ontology:constraint_victim(manga_distribution_duopoly, independent_manga_publishers).
narrative_ontology:constraint_victim(manga_distribution_duopoly, small_press_creators).
narrative_ontology:constraint_victim(manga_distribution_duopoly, north_american_readers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDEPENDENT MANGA PUBLISHER (SNARE) — Trapped by duopolistic control of retail distribution channels (bookstores, comic shops, Amazon). No alternative distribution pathway exists at scale. Career and survival depend on negotiating with one of two gatekeepers. d≈0.92, f(d)≈1.40, σ=0.9 → χ≈0.73.
constraint_indexing:constraint_classification(manga_distribution_duopoly, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: SMALL PRESS CREATOR (SNARE) — Cannot distribute directly to retail; forced into either Viz/Yen contract terms (unfavorable royalties ~10-15% of cover price after retailer discounts) or self-publishing (no retail presence). Webcomic + self-publishing path exists but has negligible income. d≈0.85, f(d)≈1.18, σ=0.9 → χ≈0.71.
constraint_indexing:constraint_classification(manga_distribution_duopoly, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: NORTH AMERICAN READER (TANGLED ROPE) — Benefits from concentrated distribution (reliable availability, consistent formatting standards, retail shelf space). Also constrained by limited title selection — duopoly has gatekeeping incentives to narrow catalog to proven sellers. Can exit to Japanese imports or webcomics but with friction. d≈0.60, f(d)≈0.75, σ=0.9 → χ≈0.39.
constraint_indexing:constraint_classification(manga_distribution_duopoly, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 4: VIZ MEDIA / YEN PRESS (ROPE) — Experiences the constraint as coordination: duopoly enables standardization of formats, retail relations, and logistics. Both firms benefit from divided market (reduced price competition, defined territory). Can exit by vertically integrating upstream (licensing) or downstream (direct sales). d≈0.10, f(d)≈0.05, σ=0.9 → χ≈0.03.
constraint_indexing:constraint_classification(manga_distribution_duopoly, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: JAPANESE PUBLISHERS / RIGHTS HOLDERS (SNARE) — Constrained by geographic distance and contract lock-in. Licensing deals with Viz/Yen are long-term and exclusive; lack direct access to North American retail. Arbitrage-seeking by Japanese firms (direct e-publishing) is blocked by existing exclusive contracts. d≈0.80, f(d)≈1.12, σ=1.1 → χ≈0.65.
constraint_indexing:constraint_classification(manga_distribution_duopoly, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: PRINT RETAIL INFRASTRUCTURE (PITON) — Bookstores and comic shops function as channels for duopolistic extraction but are themselves degraded. Theater_ratio=0.45 reflects that retail shelving decisions are partly performative gatekeeping by distributor preference, not consumer demand. Infrastructure persists through inertia (no viable alternative for physical retail); both duopolists reinforce retail dependence despite erosion from digital/webcomic platforms.
constraint_indexing:constraint_classification(manga_distribution_duopoly, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 7: DIGITAL ALTERNATIVE PATHWAYS (SCAFFOLD) — Web-based distribution (Webtoon, Tapas, Patreon, direct Kickstarter) represents a growing sunset for print duopoly. Creators increasingly bypass traditional distribution entirely. Sunset timeline: 10-15 years as digital-native cohorts replace print-dependent readers. d≈0.35, f(d)≈0.33, σ=1.2 → χ≈0.17.
constraint_indexing:constraint_classification(manga_distribution_duopoly, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (FALSE SUMMIT RISK) — May naturalize duopoly as inevitable (geographic distance, language barrier, scale economics of print distribution). However, structural data (ε=0.58, suppression=0.68) contradicts mountain classification — this is an extractive institutional arrangement, not a natural law.
constraint_indexing:constraint_classification(manga_distribution_duopoly, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(manga_distribution_duopoly_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(manga_distribution_duopoly, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(manga_distribution_duopoly, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(manga_distribution_duopoly, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(manga_distribution_duopoly, TR),
    TR >= 0.70.

:- end_tests(manga_distribution_duopoly_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The duopolists capture 40-50% of retail cover price as distributor fees, independent publishers and creators are locked into unfavorable contracts (10-15% royalties after retailer discount), and Japanese publishers face exclusive licensing restrictions that prevent them from developing direct North American distribution. The extraction is not as severe as a pure monopoly (which would be 0.70+) because (1) some creators achieve viability through self-publishing and digital platforms, (2) readers benefit from reliable retail availability, and (3) the duopolists maintain competitive pressure on each other (preventing even more extraction). Suppression (0.68): High. Multiple barriers prevent exit: (1) exclusive licensing contracts with major Japanese publishers, (2) retail relationships built over decades that strongly favor the duopolists, (3) logistical barriers to independent warehousing and distribution, (4) publisher risk aversion toward unproven small presses. However, suppression is not absolute (0.85+) because digital distribution is eroding print dependence. Theater ratio (0.45): Moderate. The duopolists maintain retail gatekeeping through shelf placement decisions and retailer relationships, which are partly performative (reflect distributor preference, not demand) but also functional (they do solve the coordination problem of getting diverse titles to diverse stores). The ratio has increased over time as market concentration has increased, but remains below 0.70 (piton threshold) because the underlying distribution logistics are genuinely functional.
 *
 * PERSPECTIVAL GAP:
 *   Independent publishers and creators perceive pure extraction (Snare) — they have no viable exit and depend entirely on duopolist terms. Readers perceive mixed coordination and extraction (Tangled Rope) — they benefit from retail availability but are constrained by limited selection. The duopolists perceive coordination (Rope) — market division and standardized logistics benefit both firms. Japanese rights holders perceive extraction (Snare) — locked into long-term exclusive contracts with limited visibility into North American sales data. The print retail infrastructure persists through inertia (Piton) — it solves a coordination problem but increasingly inefficiently as digital alternatives grow. Digital platforms represent an emerging exit (Scaffold) — webcomic distribution and direct sales are building alternative pathways with eventual sunset of print dependence. The false summit risk is naturalizing the duopoly as inevitable due to 'economics of scale' and 'geographic distance' — the structural data reveals this as an institutional arrangement (snare extraction + rope gatekeeping), not a natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Independent publishers and creators: Victims + trapped → d≈0.90, f(d)≈1.38. Maximum extraction; no exit options within the print distribution system. Readers: Victims + mobile (constrained but with digital alternatives) → d≈0.60, f(d)≈0.75. Significant extraction but not maximum; readers can exit to webcomics, Japanese imports, or digital platforms. Duopolists (Viz/Yen): Beneficiaries + arbitrage → d≈0.10, f(d)≈0.05. Net beneficiaries; can exit if they choose or adapt to digital competition. Japanese publishers: Victims + constrained (locked in by exclusive contracts but with long-term alternative strategies) → d≈0.80, f(d)≈1.12. High extraction; trapped in current licensing regime but not permanently. Retail infrastructure: Institutional + arbitrage → d≈0.10, f(d)≈0.05. Piton classification comes from theater gate, not from high chi; retailers benefit from duopolist relationships but are themselves degraded by shifting demand.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is resolved by recognizing that the constraint exhibits genuine snare characteristics for small/independent actors (high extraction, high suppression, high d-values for victims) while simultaneously containing rope/scaffold components for duopolists and alternative platform providers. The constraint is NOT purely extractive (that would be ε≥0.70, χ≥0.66 for all perspectives, suppression≥0.60) — it also solves a coordination problem (retail standardization, format consistency) that benefits readers and firms. The theater ratio (0.45) is moderate because the duopolistic gatekeeping, while performative, rests on genuinely functional distribution infrastructure. The snare classification is justified because victims (independent creators) have effectively zero exit options within the print system, suppression is high (exclusive licensing, retail barriers), and the extraction mechanism (40-50% distributor fee + unfavorable royalty terms) is substantial. The scaffold perspective is credible because digital platforms are already capturing significant market share from younger cohorts and projections suggest print will decline to <30% of manga revenue within 15 years. This resolves the mandatrophy: the duopoly is a snare for small actors and creators, a rope for the duopolists themselves, a scaffold for digital alternatives, and a piton for print retail — all simultaneously, from different structural positions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vertical_integration_feasibility,
    'Can Japanese publishers or new entrants vertically integrate directly into North American retail without the duopoly''s infrastructure, and what are the true economics?',
    'Case studies of direct Japanese retail attempts (Kadokawa''s attempts, manga publishers'' e-commerce platforms); cost analysis of warehousing and shipping vs distributor fees',
    'If feasible and economical: snare classification weakens—alternative pathways exist. If infeasible: snare reinforced—structural barrier (logistics cost) justifies duopoly.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(vertical_integration_feasibility, empirical, 'Whether vertical integration can break the duopoly').

omega_variable(
    digital_transition_speed,
    'How quickly will digital/webcomic distribution eclipse print manga distribution in North America, and will duopolists adapt or be displaced?',
    'Market share tracking: print vs digital revenue for major manga titles; creator migration rates to webcomic platforms; generational reading habit surveys',
    'If transition < 10 years: scaffold sunset is real and near. If transition > 20 years: print duopoly persists; snare classification stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(digital_transition_speed, empirical, 'Speed of digital transition relative to print distribution').

omega_variable(
    licensing_contract_exclusivity_duration,
    'What are the typical exclusivity durations in Japanese publisher licensing agreements with Viz/Yen, and are they shortening?',
    'Contract data from Japanese publishers (Shueisha, Kodansha, Kadokawa); analysis of renewal timelines; frequency of non-exclusive or shorter-term deals',
    'If durations > 15 years and rigid: snare extraction entrenched. If durations < 5 years and declining: alternative licensees can enter; snare extraction weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(licensing_contract_exclusivity_duration, empirical, 'Exclusivity duration and trend in manga licensing').

omega_variable(
    independent_publisher_survival_rates,
    'What percentage of independent manga publishers and small-press creators achieve economic viability outside the duopoly, and is this rate increasing?',
    'Longitudinal tracking of Kickstarter manga projects, self-published creators, and independent press survival rates; income distribution analysis for independent vs duopoly-licensed creators',
    'If survival rate < 10%: duopoly trap is severe (snare). If > 30% and rising: viable escape routes exist (tangled rope, not snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(independent_publisher_survival_rates, empirical, 'Viable pathways for independent manga distribution').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(manga_distribution_duopoly, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(manga_tr_t0, manga_distribution_duopoly, theater_ratio, 0, 0.35).
narrative_ontology:measurement(manga_tr_t10, manga_distribution_duopoly, theater_ratio, 10, 0.4).
narrative_ontology:measurement(manga_tr_t20, manga_distribution_duopoly, theater_ratio, 20, 0.45).

% Extraction over time
narrative_ontology:measurement(manga_be_t0, manga_distribution_duopoly, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(manga_be_t10, manga_distribution_duopoly, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(manga_be_t20, manga_distribution_duopoly, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(manga_distribution_duopoly, resource_allocation).
narrative_ontology:affects_constraint(manga_distribution_duopoly, japanese_publishing_market_concentration).
narrative_ontology:affects_constraint(manga_distribution_duopoly, digital_manga_platform_ecosystem).

% DUAL FORMULATION NOTE:
% This constraint is downstream of Japanese publishing industry concentration (which limits available titles for North American licensing) and upstream of digital manga platform ecosystems (which represent structural alternatives to print distribution). The duopoly's extractiveness is amplified by upstream concentration but weakened by downstream digital alternatives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(manga_distribution_duopoly, moderate, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
