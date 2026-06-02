% ============================================================================
% CONSTRAINT STORY: professional_table_tennis_labor_markets
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_professional_table_tennis_labor_markets, []).

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
 *   constraint_id: professional_table_tennis_labor_markets
 *   human_readable: Professional Table Tennis Labor Market Extraction
 *   domain: sports_economics/labor
 *
 * SUMMARY:
 *   The professional table tennis labor market exhibits structural extraction
 *   embedded within genuine coordination mechanisms. National federations and
 *   the International Table Tennis Federation (ITTF) organize global
 *   competition infrastructure, standardize training curricula, and allocate
 *   tournament opportunities — coordination functions that enable talent
 *   discovery and elite performance. Simultaneously, these same institutions
 *   exercise monopsony control over player labor, enforce geographic mobility
 *   restrictions through federation assignment systems, capture tournament
 *   revenue asymmetrically, and suppress player bargaining power through
 *   early-career commitment mechanisms. The constraint demonstrates tangled
 *   rope dynamics: players experience both the coordination benefit (access
 *   to international tournaments, standardized competition, career pathways)
 *   and the extraction cost (controlled wages, restricted exit options,
 *   geographic dependency). The extractiveness trajectory shows an increasing
 *   trend (0.35 → 0.58 over 20 years), reflecting the consolidation of
 *   federation power and professionalization of the circuit. Theater ratio
 *   increase (0.40 → 0.55) reflects growing performative elements in player
 *   development and tournament formats designed for media consumption rather
 *   than fair competition.
 *
 * KEY AGENTS:
 *   - Professional Players (Powerless/Trapped): Primary victims — early specialization creates path dependence; contractual obligations to national federations restrict labor mobility
 *   - Mid-Tier Players (Moderate/Constrained): Mixed experience — structurally constrained but also benefit from tournament infrastructure and sponsorship networks
 *   - National Federations (Institutional/Arbitrage): Primary beneficiaries — control player pipelines, capture federation fees, set training standards
 *   - ITTF and Tournament Organizers (Powerful/Arbitrage): Institutional beneficiaries — set ranking systems, allocate tournament slots, determine revenue sharing formulas
 *   - Developing Nation Athletes (Organized/Constrained): Organized collective at disadvantage — visa barriers and currency arbitrage create extraction within coordination opportunities
 *   - Grassroots Coaches (Moderate/Constrained): Degraded institutional role (Piton) — maintain training infrastructure through inertia; suppressed by federation qualification mandates
 *   - Equipment Manufacturers (Powerful/Arbitrage): Secondary beneficiaries — standardization creates controlled market; captured demand from player contracts
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(professional_table_tennis_labor_markets, 0.58).
domain_priors:suppression_score(professional_table_tennis_labor_markets, 0.65).
domain_priors:theater_ratio(professional_table_tennis_labor_markets, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(professional_table_tennis_labor_markets, extractiveness, 0.58).
narrative_ontology:constraint_metric(professional_table_tennis_labor_markets, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(professional_table_tennis_labor_markets, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(professional_table_tennis_labor_markets, tangled_rope).
narrative_ontology:human_readable(professional_table_tennis_labor_markets, "Professional Table Tennis Labor Market Extraction").
narrative_ontology:topic_domain(professional_table_tennis_labor_markets, "sports_economics/labor").

domain_priors:requires_active_enforcement(professional_table_tennis_labor_markets).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(professional_table_tennis_labor_markets, national_federations).
narrative_ontology:constraint_beneficiary(professional_table_tennis_labor_markets, tournament_organizers).
narrative_ontology:constraint_beneficiary(professional_table_tennis_labor_markets, equipment_manufacturers).
narrative_ontology:constraint_victim(professional_table_tennis_labor_markets, professional_players).
narrative_ontology:constraint_victim(professional_table_tennis_labor_markets, developing_nation_athletes).
narrative_ontology:constraint_victim(professional_table_tennis_labor_markets, grassroots_coaches).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PROFESSIONAL PLAYER (SNARE) — Trapped in the labor market by career path dependence. Early specialization (age 6-8) makes exit costlier than continuation. High suppression: contract obligations to national federations, tournament circuit dependency for income, no portable skills to alternative careers. No coordination benefit perceived — the player experiences pure extraction through contractual obligations, tournament fee structures, and geographic mobility restrictions imposed by federation assignments.
constraint_indexing:constraint_classification(professional_table_tennis_labor_markets, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MID-TIER PLAYER (TANGLED ROPE) — Structurally constrained by tournament circuit economics and federation governance, but also benefits from tournament infrastructure, coaching networks, and standardized competition rules. Extraction is significant but not absolute: some tournament revenue sharing, some sponsorship autonomy, possibility of switching federations at high but surmountable cost. Mixed experience of coordination (tournaments enable competition) and extraction (skewed revenue distribution).
constraint_indexing:constraint_classification(professional_table_tennis_labor_markets, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: NATIONAL FEDERATION (ROPE) — Experiences the constraint as coordination. Federation manages player pipelines, standardizes training curricula, organizes domestic tournaments, and negotiates international circuit access. Revenue flow toward federation through player contracts and tournament sanctioning fees. Primary beneficiary with arbitrage options — can shift talent development models, redirect players between disciplines, modify federation structures with limited cost to their institutional capacity.
constraint_indexing:constraint_classification(professional_table_tennis_labor_markets, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ITTF AND TOURNAMENT ORGANIZERS (ROPE) — International federation and major tournament hosts experience the labor market as a coordination mechanism. They set rules, allocate tournament slots, enforce ranking systems, and structure revenue sharing. Powerful position with full arbitrage: can restructure tournament formats, modify player compensation models, adjust circuit geography. Net beneficiary position — extraction flows toward this institutional level.
constraint_indexing:constraint_classification(professional_table_tennis_labor_markets, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: DEVELOPING NATION ATHLETE (TANGLED ROPE) — Organized collective (though informal). Faces high suppression through visa restrictions, limited tournament access in home regions, language barriers, and lack of equipment supply chains. But also genuinely benefits from international competition infrastructure, sponsorship opportunities unavailable domestically, and access to high-level training. Extraction is embedded in coordination benefits — international circuit enables upward mobility but extracts through visa dependence and currency arbitrage.
constraint_indexing:constraint_classification(professional_table_tennis_labor_markets, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: GRASSROOTS COACH (PITON) — Local coaches maintain training infrastructure through institutional inertia. Theater ratio high (0.55+) — much coaching activity is performative credential maintenance rather than functional talent development. Coaches face suppression through federation qualification requirements, standardized curriculum mandates, and economic dependency on player fees. But also constrained by exit costs (local reputational capital, limited alternative income sources). Classification reflects degradation: coaching infrastructure persists because no alternative has replaced it, not because the system optimally develops talent.
constraint_indexing:constraint_classification(professional_table_tennis_labor_markets, piton,
    context(agent_power(moderate),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational distance, the professional table tennis labor market exhibits genuine coordination (standardized tournaments, training infrastructure, career pathways) alongside systematic extraction. Coordination function: enables global competition, talent discovery, and skill development at scale. Extraction mechanisms: skewed revenue distribution, geographic mobility controls, monopsony power of national federations, suppression of player mobility between nations. The constraint is real tangled rope — both functions exist, neither can be eliminated without collapsing the other.
constraint_indexing:constraint_classification(professional_table_tennis_labor_markets, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(professional_table_tennis_labor_markets_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(professional_table_tennis_labor_markets, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(professional_table_tennis_labor_markets, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(professional_table_tennis_labor_markets, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(professional_table_tennis_labor_markets, TR),
    TR >= 0.70.

:- end_tests(professional_table_tennis_labor_markets_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The professional table tennis labor market shows extraction well above the pure coordination threshold. Revenue concentration: ~70% of tournament prize pools go to top 50 players globally, while ~95% of players earn below median income. Wage suppression through monopsony: federations control player assignments to tournaments, preventing wage competition. Geographic mobility restriction: federation assignment determines tournament access; players cannot freely shop between federations without losing circuit status. However, extractiveness is not at maximum snare levels (0.75+) because genuine coordination benefits exist: players can pursue international careers at all, training infrastructure enables talent identification, tournament standardization creates predictable competition. The value (0.58) reflects mixed extraction and coordination. Suppression (0.65): High. Multiple mechanisms: (1) Career path dependence — early specialization (age 6-8) makes exit costly; (2) Contractual lock-in — federation agreements restrict player movement; (3) Visa and geographic barriers — developing nation athletes face structural mobility costs; (4) Information asymmetry — tournament revenue formulas and federation accounting are opaque; (5) Economic dependency — limited alternative income sources for specialized athletes. Theater ratio (0.55): Moderate-high. Growing component of performative activity in professional circuits: exhibition matches designed for media spectacle, ranking system optimization for viewership, player brand management and content creation obligations. Theater has increased over the interval as the circuit professionalized and commercial broadcasting became dominant revenue source. Theater is not dominant (would approach 0.70+ for piton) but significant enough to indicate degradation of pure competition function.
 *
 * PERSPECTIVAL GAP:
 *   The professional table tennis labor market demonstrates dramatic perspectival divergence. The trapped professional player sees pure snare extraction: contractual obligations, restricted tournament access, wage suppression, no meaningful exit option. The national federation sees coordination (rope): solving the problem of organizing global competition and player development. The developing nation athlete sees tangled rope: genuine benefits from international circuit access alongside extraction through visa barriers and currency arbitrage. The grassroots coach sees a degraded system (piton): infrastructure persists through inertia despite poor talent development outcomes. The analytical observer sees clear tangled rope: both functions exist and are inseparable. The perspectival gap reveals why labor organizing in professional table tennis is difficult — different athlete cohorts (trapped vs. constrained vs. mobile) perceive the constraint differently and thus have divergent preferences about reform.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values range from 0.05 (federation beneficiary with arbitrage) to 0.95 (trapped player). National federations derive d ≈ 0.10 (institutional + beneficiary + arbitrage) → f(d) ≈ -0.10, producing negative effective extraction (net benefit). Professional players derive d ≈ 0.92 (powerless + victim + trapped) → f(d) ≈ 1.38, producing maximum effective extraction. Mid-tier players derive d ≈ 0.65 (moderate + mixed victim/beneficiary + constrained) → f(d) ≈ 1.00, producing symmetric effective extraction. Developing nation athletes derive d ≈ 0.78 (organized + victim + constrained) → f(d) ≈ 1.18, producing high effective extraction. Scope modifier σ(S) scales extractiveness: local (0.8), regional (0.9), national (1.0), global (1.2). From a powerless player's trapped perspective at global scope: χ = 0.58 × 1.38 × 1.2 ≈ 0.96 (near-maximal perceived extraction). From institutional federation's arbitrage perspective at global scope: χ = 0.58 × (-0.10) × 1.2 ≈ -0.07 (perceived benefit). The large perspectival gap in derived χ (0.96 vs. -0.07) explains why labor disputes in professional table tennis revolve around fundamentally incompatible experienced realities.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED: The professional table tennis labor market resolves the mandatrophy by showing tangled rope as the stable analytical classification. The constraint cannot be reduced to pure coordination (Rope) because extraction is systematic: players do not exit en masse only because viable alternatives don't exist, not because coordination benefits are balanced with extraction. The constraint cannot be classified as pure extraction (Snare) because genuine coordination mechanisms exist and players do receive access to competitive opportunities that would be unavailable without federation infrastructure. The extractiveness value (0.58) sits firmly in the tangled rope window (0.40 ≤ χ ≤ 0.90 at analytical context). Both functions are real: federations genuinely solve coordination problems (standardizing rules, organizing tournaments, developing talent pipelines); simultaneously, they genuinely extract (controlling wages, restricting mobility, capturing revenue asymmetrically). Mandatrophy is avoided by acknowledging both functions as structural, not pretending extraction is coordination cost or coordination is rhetorical cover for extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    federation_monopsony_vs_coordination,
    'Is national federation control over player labor primarily an exercise of monopsony power or a necessary coordination mechanism?',
    'Comparative analysis: labor market outcomes in contexts with federation control vs. looser governance models (private club systems, player unions). Measurement of wage dispersion, career duration, transition rates, and player satisfaction.',
    'If primarily monopsony: reclassify toward Snare from more perspectives. If primarily coordination: reclassify toward Rope. The extractiveness value depends critically on this distinction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federation_monopsony_vs_coordination, empirical, 'Whether federation control is monopsony extraction or necessary coordination').

omega_variable(
    developing_nation_athlete_identity_lock,
    'To what extent are developing nation athletes trapped by structural barriers vs. identity-locked into career paths by cultural expectations and invested identity?',
    'Post-exit analysis: study athletes who leave professional circuits. Do they experience barriers (visa, funding, opportunity scarcity) or identity shifts (no longer identifying as ''athlete'', social status loss, family rejection)?',
    'If primarily structural barriers: use trapped/constrained exit options. If primarily identity-fused: use identity_locked. Affects directionality derivation and may shift some perspectives from Tangled Rope to Snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(developing_nation_athlete_identity_lock, empirical, 'Structural vs. identity-based entrapment of developing nation athletes').

omega_variable(
    equipment_manufacturer_dependency,
    'Does equipment manufacturer control over standardization create a separate extractive constraint or is it structurally inseparable from labor market extraction?',
    'Decomposition analysis: compare labor market outcomes in equipment-monopoly regimes (single approved manufacturer) vs. open equipment markets (multiple approved brands). Measure player equipment costs as percentage of career earnings.',
    'If separable: write a distinct constraint story (equipment_supply_standardization) with its own extractiveness value and network link. If inseparable: keep as part of labor market story. This is an ε-invariance decision.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(equipment_manufacturer_dependency, empirical, 'Whether equipment standardization is a separate extractive constraint').

omega_variable(
    tournament_revenue_sharing_opacity,
    'Are tournament revenue allocation formulas genuinely obscure or is opacity itself an extraction mechanism (making verification of unfairness infeasible)?',
    'Access to federation accounting records and contract documentation. Analysis of whether opacity persists due to genuine complexity or deliberate withholding. Comparison with sports leagues that practice full transparency.',
    'If genuinely complex: suppression value justified (0.65) as coordination cost. If deliberately opaque: suppress value should be higher (0.75+) as extraction mechanism. Affects tangled_rope vs. snare boundary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(tournament_revenue_sharing_opacity, empirical, 'Whether revenue allocation opacity is intentional extraction mechanism').

omega_variable(
    early_specialization_causation,
    'Does federation emphasis on early specialization (age 6-8) reflect genuine talent development science or institutional path dependence that now extracts through career lock-in?',
    'Comparative pedagogy: peak performance age analysis across cohorts trained with early vs. delayed specialization models. Long-term career duration and post-athletic transition outcomes.',
    'If scientifically justified: early specialization is coordination (necessary for elite development). If path-dependent: it is extraction mechanism (locks players into path despite better alternative models). Affects whether young players experience ''trapped'' or ''constrained'' exit, and fed into classification from younger cohorts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(early_specialization_causation, empirical, 'Whether early specialization reflects science or institutional path dependence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(professional_table_tennis_labor_markets, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pttlm_tr_t0, professional_table_tennis_labor_markets, theater_ratio, 0, 0.4).
narrative_ontology:measurement(pttlm_tr_t10, professional_table_tennis_labor_markets, theater_ratio, 10, 0.48).
narrative_ontology:measurement(pttlm_tr_t20, professional_table_tennis_labor_markets, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(pttlm_be_t0, professional_table_tennis_labor_markets, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(pttlm_be_t10, professional_table_tennis_labor_markets, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(pttlm_be_t20, professional_table_tennis_labor_markets, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(professional_table_tennis_labor_markets, resource_allocation).
narrative_ontology:affects_constraint(professional_table_tennis_labor_markets, elite_sports_visa_restrictions).
narrative_ontology:affects_constraint(professional_table_tennis_labor_markets, athletic_equipment_standardization).

% DUAL FORMULATION NOTE:
% Professional table tennis labor markets decompose along domain lines: (1) labor_extraction_component (ε ≈ 0.58, primary story), (2) equipment_supply_standardization (separate story if equipment monopoly control has independent ε > 0.40), (3) visa_system_coupling (separate story if athletic visa restrictions exhibit their own structural dynamic). All three are linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(professional_table_tennis_labor_markets, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
