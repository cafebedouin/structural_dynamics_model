% ============================================================================
% CONSTRAINT STORY: independent_criticism_patronage
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_independent_criticism_patronage, []).

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
 *   constraint_id: independent_criticism_patronage
 *   human_readable: The Patronage Model for Independent Cultural Criticism
 *   domain: economic/technological
 *
 * SUMMARY:
 *   The patronage model for independent cultural criticism represents a
 *   hybrid coordination-extraction system that emerged as a response to the
 *   collapse of traditional institutional funding for specialized critical
 *   voices. Platforms like Patreon and Substack enabled direct financial
 *   relationships between critics and audiences, bypassing editorial
 *   gatekeepers and enabling long-form work on niche topics (experimental
 *   cinema, electronic music composition, literary microtrends) that
 *   mainstream publications could no longer support. This constraint exhibits
 *   the full perspectival range across the classification system: for
 *   emerging critics, it functions as a snare (bootstrapping trap); for
 *   established critics, as tangled rope (coordination with embedded audience
 *   constraints); for patrons, as rope (pure coordination); for legacy
 *   institutions, as extraction and fragmentation; for the public-goods
 *   movement, as temporary scaffolding; and for market economists, as an
 *   immutable natural law reflecting technological cost structure. The
 *   extractiveness has risen from 0.15 to 0.38 over 16 years as the patronage
 *   model matured, suggesting increasing rent-capture through platform fees,
 *   creator lock-in through network effects, and audience gatekeeping through
 *   paywall fragmentation.
 *
 * KEY AGENTS:
 *   - Independent Critics: Primary beneficiary (institutional/arbitrage) — gain editorial freedom and sustained income from niche work; network effects create switching costs
 *   - Emerging Critics Without Audience: Primary victim (powerless/trapped) — face bootstrapping barrier to entry; cannot sustain criticism financially without existing audience
 *   - Mass Audience: Secondary victim (powerless/constrained) — face paywall fragmentation and access barriers to quality long-form criticism previously available freely
 *   - Patronage Platforms (Patreon/Substack): Institutional actors (institutional/arbitrage) — coordinate matching between critics and patrons; capture 8-10% of transaction value; benefit from network effects and data
 *   - Patron Communities: Secondary beneficiary (moderate/mobile) — gain voice and community around shared critical interests; coordinate to sustain critics they value
 *   - Legacy Cultural Institutions: Powerful actors (powerful/arbitrage) — reduce costs by outsourcing specialized criticism but lose institutional influence over critical discourse
 *   - Public-Goods Coalition: Organized actors (organized/constrained) — work to re-establish public funding for criticism as alternative to patronage gatekeeping
 *   - Academic Literary Criticism: Institutional apparatus (institutional/arbitrage) — operates in parallel to patronage with minimal interaction; maintains through inertia despite reduced cultural influence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(independent_criticism_patronage, 0.38).
domain_priors:suppression_score(independent_criticism_patronage, 0.48).
domain_priors:theater_ratio(independent_criticism_patronage, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(independent_criticism_patronage, extractiveness, 0.38).
narrative_ontology:constraint_metric(independent_criticism_patronage, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(independent_criticism_patronage, theater_ratio, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(independent_criticism_patronage, tangled_rope).
narrative_ontology:human_readable(independent_criticism_patronage, "The Patronage Model for Independent Cultural Criticism").
narrative_ontology:topic_domain(independent_criticism_patronage, "economic/technological").

domain_priors:requires_active_enforcement(independent_criticism_patronage).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(independent_criticism_patronage, independent_critics).
narrative_ontology:constraint_beneficiary(independent_criticism_patronage, patron_platforms).
narrative_ontology:constraint_victim(independent_criticism_patronage, mass_audience_access).
narrative_ontology:constraint_victim(independent_criticism_patronage, emerging_critics).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EMERGING CRITIC WITHOUT AUDIENCE (SNARE) — Trapped by the bootstrapping problem: cannot sustain criticism financially without existing audience, but cannot build audience without sustained critical output. Must either maintain other income sources (reducing criticism quality) or accept precarity. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.63.
constraint_indexing:constraint_classification(independent_criticism_patronage, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MASS AUDIENCE SEEKING FREE CRITICISM (SNARE) — Faces paywall fragmentation and patronage gatekeeping. Quality long-form criticism increasingly locked behind subscription paywalls. Constrained by economic access barriers; trapped by the shift from public-good journalism to patron-funded niche work. d≈0.88, f(d)≈1.32, σ=1.2 → χ≈0.60.
constraint_indexing:constraint_classification(independent_criticism_patronage, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: ESTABLISHED INDEPENDENT CRITIC (TANGLED ROPE) — Benefits from direct patronage enabling focus on critical work without editorial constraint; coordinating with audience on what gets produced. Also trapped by patron preference lock-in: must maintain patron satisfaction or lose income; cannot sharply pivot critical positions without audience loss. Mobile enough to migrate platforms but constrained by network effects. d≈0.52, f(d)≈0.68, σ=1.2 → χ≈0.31.
constraint_indexing:constraint_classification(independent_criticism_patronage, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: PATRONAGE PLATFORM (ROPE) — Pure coordination mechanism between critics and patrons; enables direct financial relationship that bypasses traditional gatekeepers. Platform benefits from fee structure (8-10% of transactions) and data/network effects, but primary function is coordination. d≈0.10, f(d)≈-0.05, σ=1.2 → χ≈-0.02. Net beneficiary through coordination.
constraint_indexing:constraint_classification(independent_criticism_patronage, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: PATRON COMMUNITY (ROPE) — Coordinates to sustain critics they value; gets direct voice in critical agenda through commenting, poll participation, and community discussion. d≈0.08, f(d)≈-0.08, σ=1.2 → χ≈-0.03. Net beneficiary through coordination and voice.
constraint_indexing:constraint_classification(independent_criticism_patronage, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: LEGACY CULTURAL INSTITUTIONS (TANGLED ROPE) — Benefit from patronage system by outsourcing specialized criticism (niche voices on experimental film, electronic music, literary micro-movements) that they would not fund internally; reduces their coordination costs. But extracted from: critical ecosystem fragments into non-overlapping patron communities, reducing institutional influence over critical discourse. d≈0.45, f(d)≈0.53, σ=1.0 → χ≈0.20.
constraint_indexing:constraint_classification(independent_criticism_patronage, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: PUBLIC-GOODS COALITION (SCAFFOLD) — Movement to fund criticism as public intellectual good (arts grants, library funding, public broadcasting, academic criticism) views patronage as transitional: necessary to sustain independent voices during institutional defunding, but sunset when public funding for criticism is re-established. d≈0.35, f(d)≈0.30, σ=1.2 → χ≈0.11. Low extraction because the coalition sees a path forward through institutional redesign.
constraint_indexing:constraint_classification(independent_criticism_patronage, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ACADEMIC LITERARY CRITICISM (PITON) — Traditional academic criticism (journals, university presses, tenure-based scholarship) persists as a credential system despite diminished public engagement or influence. Patronage has not displaced academic criticism; instead, they operate in parallel with low interaction. Academic apparatus maintains itself through institutional inertia (tenure, citations, program accreditation) but sees theater_ratio ≈0.75 (performative credentialing, minimal cultural impact). d≈0.08, f(d)≈-0.07, σ=1.0 → χ≈-0.02.
constraint_indexing:constraint_classification(independent_criticism_patronage, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / MARKET ECONOMICS VIEW (MOUNTAIN) — From a civilizational perspective, patronage-based funding reflects an inevitable consequence of technological cost structure: digital distribution eliminates the economic rent that newspapers and magazines once captured. Direct patronage is the natural equilibrium outcome of zero marginal cost information goods. ε=0.08, suppression=0.08, emerges_naturally=true. However, this perspective risks naturalizing what is actually a policy choice: public funding regimes (library systems, public radio, arts grants) are equally compatible with technology and have different distributional outcomes.
constraint_indexing:constraint_classification(independent_criticism_patronage, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(independent_criticism_patronage_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(independent_criticism_patronage, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(independent_criticism_patronage, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(independent_criticism_patronage, TR),
    TR >= 0.70.

:- end_tests(independent_criticism_patronage_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The patronage model genuinely coordinates critics with interested audiences, reducing extraction compared to traditional publishing gatekeeping. However, platform fees (8-10%), network effects that lock in established critics, and the requirement that critics maintain patron satisfaction create meaningful extraction layers. The value reflects that this is hybrid coordination-extraction, not pure extraction. Suppression (0.48): Moderate. Barriers to entry include bootstrapping requirement and network discovery friction, but suppression is not total — critics with niche expertise and minimal audience can build slowly. The rise in theater_ratio from 0.35 to 0.52 over 16 years reflects increasing emphasis on community management and audience-pleasing content, suggesting growing performative overhead. Theater ratio (0.52): Moderate-high and rising. As the patronage model matured, critics increasingly devoted energy to engagement metrics (reply time, community discussion, bonus content) that may not directly improve critical work but maintain patron loyalty. This drift suggests the system is acquiring theatrical overhead as it scales.
 *
 * PERSPECTIVAL GAP:
 *   This constraint displays a fundamental conflict between the institutional and powerless agent perspectives. For established independent critics, patronage represents liberation from editorial constraint and coordination with their intended audience (Rope from their view; Tangled Rope recognizing the patron lock-in). For emerging critics and mass audiences, patronage represents a gatekeeping mechanism worse than traditional publishing because it requires pre-existing network advantage (Snare from their view). The gap reflects that patronage is structurally asymmetric: it enables criticism about the right topics for the right audience, but those topics and audiences are determined by who can survive the bootstrapping phase. The public-goods coalition sees this as temporary (Scaffold) — scaffolding while public funding regimes are rebuilt — but this requires a political shift that has not yet materialized. The academic apparatus persists as piton (degraded ritual) because universities still credential literary critics even as their cultural influence declines and their work reaches minimal audiences.
 *
 * DIRECTIONALITY LOGIC:
 *   Emerging critics: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction — cannot exit without abandoning criticism. Mass audience: Victim + constrained → d≈0.88, f(d)≈1.32. High extraction — can consume some free criticism but faces paywall fragmentation. Established critics: Mixed beneficiary and victim + mobile → d≈0.52, f(d)≈0.68. Moderate extraction because they have built network effects but remain constrained by patron preferences. Platforms: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.05. Net beneficiary through coordination. Patrons: Beneficiary + mobile → d≈0.08, f(d)≈-0.08. Net beneficiary. Legacy institutions: Beneficiary and victim + arbitrage → d≈0.45, f(d)≈0.53. Moderate extraction reflecting fragmentation cost. Public-goods coalition: Organized victim + constrained → d≈0.35, f(d)≈0.30. Low extraction because coalition has agency and sees exit path. Academic apparatus: Institutional beneficiary + arbitrage → d≈0.08, f(d)≈-0.07. Piton classification from theater gate, not directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint is genuinely hybrid — it is neither pure coordination (Rope) nor pure extraction (Snare) but a tangled system that solves one coordination problem (matching critics to audiences) while creating new extraction problems (bootstrapping barrier, platform lock-in, paywall fragmentation). The Tangled Rope classification is stable across perspectives that see both the coordination function and the asymmetric costs. The snare perspectives (emerging critics, mass audience) see the extraction without the coordination benefit. The scaffold perspective does not deny the extraction but frames it as transitional — temporary until public funding regimes are restored. The piton perspective (academic apparatus) is not alternative but parallel — academia and patronage now operate in different networks with minimal interaction. The false mountain (market economics naturalizing patronage as inevitable) is caught by noting that public-funding models are equally compatible with technology and have different extraction profiles. The constraint resolves mandatrophy by being genuinely mixed, not by collapsing to a single type.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bootstrapping_threshold,
    'What patron-funding threshold is required to sustain a critic working full-time on niche topics, and how many critics can reach this threshold given the finite patronage market?',
    'Empirical analysis of Patreon/Substack earnings distributions; calculation of sustainability thresholds by topic domain; longitudinal tracking of critic attrition rates by income level',
    'If threshold is reachable by >20% of active critics: patronage functions as a viable independent income source (Rope from more perspectives). If threshold is reachable by <5%: patronage is selection mechanism for pre-funded or privileged critics (Snare for emerging critics confirmed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(bootstrapping_threshold, empirical, 'Minimum patron funding for full-time criticism sustainability').

omega_variable(
    patron_preference_lock_in,
    'Does the patronage model create structural lock-in where critics cannot change critical positions or topic focus without significant income loss?',
    'Qualitative interviews with established critics about topic/position changes; correlation analysis between content changes and supporter churn; comparison with traditional media critics'' flexibility',
    'If strong lock-in: patronage creates hidden editorial constraint (Tangled Rope confirmed, Snare tendency for established critics). If weak lock-in: critics genuinely have editorial freedom (Rope from established critic perspective).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(patron_preference_lock_in, empirical, 'Existence and strength of patron-preference lock-in on critical positions').

omega_variable(
    public_good_substitution,
    'Are patronage-funded critics genuinely producing public intellectual goods, or are they producing niche entertainment that would not be funded under a public-good model?',
    'Citation analysis of patronage critics vs academic critics; measurement of critical influence on institutional decisions (museum exhibitions, festival programming, curriculum); welfare analysis of access distribution',
    'If patronage critics produce comparable public goods: scaffold view is viable (public funding could replace patronage). If patronage critics produce niche goods: criticism has bifurcated into public-intellectual and entertainment streams, and patronage is not transitional.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_good_substitution, conceptual, 'Whether patronage-funded criticism produces public intellectual goods or niche entertainment').

omega_variable(
    extraction_vs_coordination_balance,
    'What proportion of the constraint''s structure reflects genuine coordination (matching critics to audience) versus extraction (platform rent-capture, critic rent-seeking, audience gatekeeping)?',
    'Comparative analysis of patronage model costs (platform fees, payment friction, discovery burden) against counterfactual models (public funding, institutional support, traditional publishing); welfare incidence analysis by agent type',
    'If coordination dominates: Tangled Rope classification confirmed. If extraction dominates: classification shifts toward Snare for multiple perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_coordination_balance, empirical, 'Balance between coordination function and extraction mechanisms in the patronage model').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(independent_criticism_patronage, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(icp_tr_t0, independent_criticism_patronage, theater_ratio, 0, 0.35).
narrative_ontology:measurement(icp_tr_t8, independent_criticism_patronage, theater_ratio, 8, 0.43).
narrative_ontology:measurement(icp_tr_t16, independent_criticism_patronage, theater_ratio, 16, 0.52).

% Extraction over time
narrative_ontology:measurement(icp_be_t0, independent_criticism_patronage, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(icp_be_t8, independent_criticism_patronage, base_extractiveness, 8, 0.27).
narrative_ontology:measurement(icp_be_t16, independent_criticism_patronage, base_extractiveness, 16, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(independent_criticism_patronage, information_standard).
narrative_ontology:affects_constraint(independent_criticism_patronage, cultural_criticism_gatekeeping).
narrative_ontology:affects_constraint(independent_criticism_patronage, public_intellectual_defunding).
narrative_ontology:affects_constraint(independent_criticism_patronage, platform_algorithmic_curation).

% DUAL FORMULATION NOTE:
% The patronage model is structurally downstream of the institutional defunding of cultural criticism in newspapers, magazines, and university presses (constraint: public_intellectual_defunding, ε=0.50+). Patronage emerges as a solution to that constraint but creates its own extraction layers. The three network nodes represent: (1) the original gatekeeping it displaced; (2) the institutional defunding it responds to; (3) the algorithmic discovery systems it depends on. Each is a distinct constraint with its own ε value and perspective set.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(independent_criticism_patronage, moderate, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
