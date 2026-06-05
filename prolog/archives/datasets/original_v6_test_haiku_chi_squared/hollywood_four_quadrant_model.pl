% ============================================================================
% CONSTRAINT STORY: hollywood_four_quadrant_model
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hollywood_four_quadrant_model, []).

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
 *   constraint_id: hollywood_four_quadrant_model
 *   human_readable: The Four-Quadrant Blockbuster Model
 *   domain: economic/entertainment/cultural
 *
 * SUMMARY:
 *   The four-quadrant blockbuster model is a Hollywood decision-making
 *   heuristic that emerged in the 1980s-1990s as studios consolidated and
 *   theatrical distribution became the primary revenue stream. The model
 *   mandates that greenlighted films appeal to both males and females, and
 *   both audiences under 25 and over 25, creating a demographic filter that
 *   favors broad-appeal franchises and superhero narratives while suppressing
 *   niche, experimental, or culturally specific storytelling. The constraint
 *   exhibits hybrid coordination-extraction structure: it genuinely solved a
 *   real coordination problem (how to market a $200M film to justify
 *   theatrical risk), but it simultaneously functions as an extraction
 *   mechanism that locks out non-mainstream filmmakers and audiences. The
 *   model's extractiveness (0.52) and suppression (0.65) reflect that the
 *   constraint creates significant barriers to entry and cultural voice, but
 *   these are enforced through capital allocation rather than explicit
 *   coercion. The rising theater_ratio (0.35→0.58) indicates that the model's
 *   primary coordination function (predicting theatrical success) has
 *   atrophied as streaming and international presales have diversified studio
 *   revenue sources, yet the institutional invocation of quadrant logic
 *   persists, revealing institutional inertia and Goodhart drift.
 *
 * KEY AGENTS:
 *   - Major Studios (Disney, Warner Bros., Paramount): Institutional beneficiaries (institutional/arbitrage) — use the model to justify blockbuster capital allocation and coordinate marketing across global theatrical markets
 *   - Niche Filmmakers: Primary victims (powerless/trapped) — locked out of studio funding and major theatrical distribution unless they conform to quadrant logic; alternative pathways (streaming, festivals) offer limited capital and reach
 *   - Specialized Audiences: Secondary victims (moderate/constrained) — face restricted theatrical offerings; specialized narratives (adult women dramas, LGBTQ+ stories, regional cinema) are underproduced; constrained exit via streaming/festivals
 *   - Emerging Creator Coalition: Organized agents (organized/constrained) — independent producers, streaming platforms, international co-productions building alternative distribution; constrained by legacy theatrical infrastructure and capital disadvantage
 *   - Theater Owners: Institutional actors (institutional/constrained) — depend on studio supply of blockbusters; trapped in symbiotic relationship with quadrant model; limited exit if theatrical declines
 *   - Prestige/Awards Industry: Institutional observers (institutional/analytical) — Oscar and festival circuits increasingly showcase non-quadrant films, creating institutional counterweight to studio dominance; medium exit options
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hollywood_four_quadrant_model, 0.52).
domain_priors:suppression_score(hollywood_four_quadrant_model, 0.65).
domain_priors:theater_ratio(hollywood_four_quadrant_model, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hollywood_four_quadrant_model, extractiveness, 0.52).
narrative_ontology:constraint_metric(hollywood_four_quadrant_model, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(hollywood_four_quadrant_model, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hollywood_four_quadrant_model, tangled_rope).
narrative_ontology:human_readable(hollywood_four_quadrant_model, "The Four-Quadrant Blockbuster Model").
narrative_ontology:topic_domain(hollywood_four_quadrant_model, "economic/entertainment/cultural").

domain_priors:requires_active_enforcement(hollywood_four_quadrant_model).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hollywood_four_quadrant_model, major_studios).
narrative_ontology:constraint_beneficiary(hollywood_four_quadrant_model, mass_market_franchises).
narrative_ontology:constraint_victim(hollywood_four_quadrant_model, niche_filmmakers).
narrative_ontology:constraint_victim(hollywood_four_quadrant_model, specialized_audiences).
narrative_ontology:constraint_victim(hollywood_four_quadrant_model, artistic_risk_taking).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NICHE FILMMAKER (SNARE) — Powerless filmmakers targeting specific demographics (e.g., adult women over 25, LGBTQ+ audiences, regional/ethnic communities) cannot access major studio funding or distribution. The four-quadrant gate locks them out. d≈0.92, f(d)≈1.40, σ=1.2 → χ≈0.73.
constraint_indexing:constraint_classification(hollywood_four_quadrant_model, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SPECIALIZED AUDIENCE (SNARE) — Audiences seeking stories outside quadrant-optimized formulas (character-driven dramas, genre experiments, culturally specific narratives) face restricted theatrical offerings. Exit options are constrained (streaming, limited releases, festival circuits offer poor distribution). d≈0.80, f(d)≈1.25, σ=1.0 → χ≈0.65.
constraint_indexing:constraint_classification(hollywood_four_quadrant_model, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MAJOR STUDIO FRANCHISE DIVISION (ROPE) — Institutional beneficiary. The four-quadrant model is a genuine coordination mechanism for maximizing theatrical return on blockbuster capital. It solves a real coordination problem: how to market a $200M film to achieve global box office minimums. The model enables franchises (Marvel, Fast & Furious, Dune) to function as revenue-generating utilities. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.06.
constraint_indexing:constraint_classification(hollywood_four_quadrant_model, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: EMERGING CREATOR COALITION (TANGLED ROPE) — Independent filmmakers, streaming platforms, and prestige-drama producers are building alternative distribution pathways. They experience the four-quadrant constraint as both coordination (it defined the theatrical market they must compete in) and extraction (it constricts their access to capital and marquee venues). They have constrained exit (some can migrate to streaming, some cannot). d≈0.55, f(d)≈0.75, σ=0.9 → χ≈0.36.
constraint_indexing:constraint_classification(hollywood_four_quadrant_model, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LEGACY BOX OFFICE RITUAL (PITON) — The four-quadrant model emerged in the 1980s-90s as a rational response to theatrical economics: broad appeal maximized box office in an era of finite screens. But its primary function (predicting theatrical success) has atrophied as streaming fragmented audiences and theatrical windows compressed. Major studios still invoke quadrant optimization, but the actual strategic logic has shifted to franchise sequelization and international presales. theater_ratio=0.58 reflects that quadrant talk persists in greenlight meetings despite reduced functional relevance. The model lingers through institutional inertia.
constraint_indexing:constraint_classification(hollywood_four_quadrant_model, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, the four-quadrant model exhibits genuine coordination function (it scaled blockbuster production capital) and genuine extraction (it suppressed cultural diversity and artistic risk). The model is not a natural law of filmmaking — it is a contingent institutional arrangement optimized for a specific era (theatrical scarcity, demographics-based marketing, studio monopolies). ε=0.52, suppression=0.65 indicate hybrid structure: meaningful extraction but not pure coercion. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.72.
constraint_indexing:constraint_classification(hollywood_four_quadrant_model, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hollywood_four_quadrant_model_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hollywood_four_quadrant_model, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hollywood_four_quadrant_model, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(hollywood_four_quadrant_model, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(hollywood_four_quadrant_model, TR),
    TR >= 0.70.

:- end_tests(hollywood_four_quadrant_model_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The model creates substantial barriers to capital and distribution for non-mainstream work, but these barriers are not absolute — alternative funding exists, albeit more precarious. The measurement trajectory (0.38→0.52) reflects increasing extraction as theatrical fragmentation makes quadrant logic less functionally necessary yet studios enforce it more rigidly to maintain studio control. Suppression (0.65): Significant. Barriers include capital concentration (studios control most theatrical budgets), demographic targeting logic (marketed as scientific but excludes narrative types), and exhibition bottlenecks (limited theatrical screens favor blockbusters). But suppression is not total — niche work can reach audiences via streaming, festivals, and word-of-mouth. Theater ratio (0.58): Moderate-high. The four-quadrant language persists in greenlight meetings, marketing strategies, and studio strategy documents, but its predictive validity has declined as box office correlates now with franchise pedigree, IP recognition, and international presales rather than demographic appeal. Quadrant-optimized original films have increasingly underperformed since 2015 (e.g., *Terminator Genisys* appealed to all four quadrants but flopped; *Everything Everywhere All at Once* violated quadrant logic and succeeded massively). The theater_ratio rise indicates studios invoke quadrant optimization for legitimacy reasons (framing capital concentration as systematic rather than arbitrary) while actually allocating based on other signals.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how institutional beneficiaries experience coordination while victims experience extraction. The major studio sees the four-quadrant model as a genuine coordination achievement — it enabled the modern blockbuster industrial complex and justifies massive capital allocation by providing a rational framework for risk reduction. The niche filmmaker sees it as a pure barrier: the model functions as a gating mechanism that makes their stories ineligible for studio funding regardless of artistic merit or market demand. The emerging coalition sees it as a fading but still-powerful extraction mechanism — they must work around it (streaming, international co-productions) because direct confrontation (trying to secure studio backing for non-quadrant films) remains nearly impossible. The theater owner is trapped in institutional symbiosis: they depend on blockbuster supply (studios' quadrant-optimized output) yet have no exit option if theatrical continues declining. The analytical observer recognizes the model as a contingent artifact of 1980s-1990s theatrical economics that persists despite technological and market changes that have rendered its coordination function obsolete.
 *
 * DIRECTIONALITY LOGIC:
 *   Major studios: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary; low effective extraction relative to their power. Niche filmmakers: Victim + trapped → d≈0.92, f(d)≈1.40. Maximum extraction; no legitimate alternative pathway within studio system. Specialized audiences: Victim + constrained → d≈0.80, f(d)≈1.25. High extraction; streaming/festivals provide some exit but with vastly lower theatrical reach and production values. Emerging coalition: Victim + constrained → d≈0.55, f(d)≈0.75. Moderate extraction; coalition has agency and is building alternatives but remains capital-disadvantaged relative to studios. Theater owners: Institutional + constrained → d≈0.50, f(d)≈0.65. Symmetric extraction; studios extract margin from theatrical while theater owners extract operational margin, but neither has meaningful exit option. Analytical observer: analytical → d≈0.72, f(d)≈1.15. High observed extraction because the model now functions primarily as cultural gatekeeping rather than rational risk management.
 *
 * MANDATROPHY ANALYSIS:
 *   The four-quadrant model resolves mandatrophy by exhibiting genuine but declining coordination function coupled with persistent extraction. It is not a mountain (no natural law signature — theater_ratio too high, suppression too substantial). It is not a pure rope (suppression and extraction too high — this is not frictionless coordination). It is not a pure snare (victims have some exit options, and the beneficiary provides genuine coordination services). It is a tangled rope: genuine coordination (managing blockbuster capital allocation and global marketing) and genuine asymmetric extraction (locking out niche narratives and filmmakers). The rising theater_ratio indicates mandatrophy drift: as the coordination function atrophies (streaming reduces need for theatrical demographic targeting), the extraction function persists through institutional inertia and legitimacy theater (studios continue invoking quadrant logic to justify blockbuster budgets). The classification holds at Tangled Rope from both the primary beneficiary and analytical perspectives because the model simultaneously enables studio coordination and suppresses cultural diversity. From the niche filmmaker perspective, it is Snare — pure extraction with no coordination benefit to their position.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theatrical_obsolescence_threshold,
    'At what point does theatrical market fragmentation render the four-quadrant model functionally irrelevant to studio greenlight decisions?',
    'Historical analysis of studio greenlight documentation pre- vs post-streaming market dominance; correlation between quadrant compliance and theatrical success 2020-2026 vs 2010-2015',
    'If threshold already crossed (pre-2020): constraint classifies as Piton (theatrical obliga­tions lingers despite reduced function). If threshold 2030+: constraint retains Tangled Rope/Snare classification (still functionally extractive). If never crossed: structural coordination lock persists indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theatrical_obsolescence_threshold, empirical, 'Whether theatrical fragmentation has rendered quadrant logic obsolete').

omega_variable(
    alternative_funding_sufficiency,
    'Do non-studio funding sources (streaming platforms, foreign presales, private equity, international co-productions) now provide sufficient capital for quality films outside quadrant constraints?',
    'Comparative budget analysis: films greenlit by non-studio sources vs studio-greenlit films; quality/critical metrics (Oscars, major festival selection) by funding source; box office performance of non-quadrant films on alternate distribution platforms',
    'If sufficient: Snare classification weakens (victims have genuine exit via streaming/festivals). If insufficient: Snare persists (alternative funding is precarious or low-budget). If mixed by genre: constraint is actually multiple constraints (decompose into stories).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_funding_sufficiency, empirical, 'Whether alternative funding sources provide viable paths for non-quadrant films').

omega_variable(
    demographic_marketing_prediction_validity,
    'Does four-quadrant demographic targeting actually predict theatrical box office success, or has it become decoupled from audience behavior?',
    'Regression analysis of quadrant compliance vs box office outcome (2015-2026); identification of films that violated quadrant logic and succeeded; comparison with pre-2008 data when the model was strongest',
    'If still predictive: the model retains coordination function and institutional justification. If decoupled: studios cling to the model for legitimacy reasons (theater_ratio rises further), and it becomes pure extraction vehicle.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(demographic_marketing_prediction_validity, empirical, 'Whether quadrant targeting predicts box office success').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hollywood_four_quadrant_model, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fqm_tr_t0, hollywood_four_quadrant_model, theater_ratio, 0, 0.35).
narrative_ontology:measurement(fqm_tr_t15, hollywood_four_quadrant_model, theater_ratio, 15, 0.5).
narrative_ontology:measurement(fqm_tr_t30, hollywood_four_quadrant_model, theater_ratio, 30, 0.58).

% Extraction over time
narrative_ontology:measurement(fqm_be_t0, hollywood_four_quadrant_model, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(fqm_be_t15, hollywood_four_quadrant_model, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(fqm_be_t30, hollywood_four_quadrant_model, base_extractiveness, 30, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hollywood_four_quadrant_model, resource_allocation).
narrative_ontology:affects_constraint(hollywood_four_quadrant_model, theatrical_presales_dependency).
narrative_ontology:affects_constraint(hollywood_four_quadrant_model, franchise_sequel_lock_in).
narrative_ontology:affects_constraint(hollywood_four_quadrant_model, streaming_market_fragmentation).

% DUAL FORMULATION NOTE:
% The four-quadrant model is downstream of theatrical economics (limited screens, exhibition bottlenecks) and upstream of contemporary studio strategy (international presales, franchise sequelization). The model represents a specific institutional implementation of capital concentration within theatrical markets; the broader constraints of theatrical scarcity and franchise dependence operate independently and should be decomposed into separate stories if detailed analysis of their ε values is needed.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hollywood_four_quadrant_model, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
