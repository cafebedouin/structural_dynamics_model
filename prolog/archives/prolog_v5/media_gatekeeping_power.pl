% ============================================================================
% CONSTRAINT STORY: media_gatekeeping_power
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_media_gatekeeping_power, []).

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
 *   constraint_id: media_gatekeeping_power
 *   human_readable: Media Gatekeeping Power in Information Distribution
 *   domain: communications/political_economy
 *
 * SUMMARY:
 *   Media gatekeeping power refers to the structural control exercised by
 *   dominant media corporations and allied institutions over which voices,
 *   narratives, and information reach mass audiences. This constraint
 *   exhibits eight distinct perspectival readings from the same structural
 *   base, making it a diagnostic exemplar for institutional extraction
 *   mechanisms. The gatekeeper solves a genuine coordination problem—curating
 *   information in a world of information overabundance—while simultaneously
 *   extracting power through monopolistic control of distribution channels.
 *   Extractiveness has risen from 0.42 to 0.58 over the measurement interval,
 *   driven by concentration of ownership and algorithmic prioritization that
 *   reinforces dominant voices. Theater ratio has remained moderate
 *   (0.48→0.55), reflecting that editorial legitimacy claims have modest
 *   performative content—editors do perform meaningful curation, but the
 *   function no longer justifies the monopolistic positioning. The constraint
 *   is hybrid: genuine coordination function coexists with asymmetric
 *   extraction targeting powerless and moderate voices.
 *
 * KEY AGENTS:
 *   - Dominant Media Corporations: Primary beneficiary (institutional/arbitrage) — control distribution channels, set narrative frames, capture advertising revenue, maintain monopolistic positioning
 *   - Marginalized Voices: Primary victim (powerless/trapped) — cannot reach audiences without gatekeeper approval; trapped by distribution infrastructure requirements and capital barriers
 *   - Independent Publishers: Secondary victim (moderate/constrained) — depend on gatekeeping platforms, face algorithmic suppression, constrained by network effects and survival economics
 *   - Political Establishment: Secondary beneficiary (institutional/constrained) — benefits from media relationships and guaranteed access; constrained by need to maintain legitimacy through gatekeepers
 *   - Social Media Platforms: Rival gatekeeper (institutional/constrained) — replicate gatekeeping through algorithms; constrained by regulatory pressure and advertiser demands
 *   - Open Information Movement: Organized challenger (organized/constrained) — building decentralized alternatives with sunset logic; constrained by network effects and coordination challenges
 *   - Legacy Editorial Standards: Degraded institutional practice (institutional/arbitrage) — once legitimate coordination mechanism; now maintained through inertia (piton perspective)
 *   - Analytical Observer: Civilizational context (analytical/analytical) — sees genuine tangled rope: real coordination function coexisting with asymmetric extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(media_gatekeeping_power, 0.58).
domain_priors:suppression_score(media_gatekeeping_power, 0.65).
domain_priors:theater_ratio(media_gatekeeping_power, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(media_gatekeeping_power, extractiveness, 0.58).
narrative_ontology:constraint_metric(media_gatekeeping_power, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(media_gatekeeping_power, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(media_gatekeeping_power, tangled_rope).
narrative_ontology:human_readable(media_gatekeeping_power, "Media Gatekeeping Power in Information Distribution").
narrative_ontology:topic_domain(media_gatekeeping_power, "communications/political_economy").

domain_priors:requires_active_enforcement(media_gatekeeping_power).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(media_gatekeeping_power, dominant_media_corporations).
narrative_ontology:constraint_beneficiary(media_gatekeeping_power, established_political_actors).
narrative_ontology:constraint_victim(media_gatekeeping_power, marginalized_voices).
narrative_ontology:constraint_victim(media_gatekeeping_power, information_access_equity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED VOICE (SNARE) — Marginalized speakers cannot access dominant media channels without intermediation by gatekeepers. No exit from the constraint without gaining institutional backing or capital. Suppression is structural: distribution platforms require editorial approval, advertising budgets, or institutional credibility that powerless agents lack. Full extraction experienced.
constraint_indexing:constraint_classification(media_gatekeeping_power, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INDEPENDENT PUBLISHER (TANGLED ROPE) — Medium-sized outlets or independent journalists depend on distribution networks controlled by gatekeepers. They benefit from the coordination function: editorial standards, fact-checking infrastructure, audience aggregation. But they also bear extraction costs: algorithmic prioritization favors dominant outlets, distribution requires proprietary platforms, revenue concentration limits sustainability. Constrained exit due to network effects.
constraint_indexing:constraint_classification(media_gatekeeping_power, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DOMINANT MEDIA CORPORATION (ROPE) — Experiences the gatekeeping constraint as pure coordination: setting editorial standards, aggregating audiences, establishing credibility signals. Net beneficiary through monopolistic positioning. Can arbitrage between content creation, distribution, and advertising. The constraint appears as a necessary mechanism for managing information quality and reducing audience search costs.
constraint_indexing:constraint_classification(media_gatekeeping_power, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: POLITICAL ESTABLISHMENT (TANGLED ROPE) — Established political parties and leaders benefit from gatekeeping: media relationships provide guaranteed access, framing power, and protection from insurgent challengers. They also rely on media coordination for legitimacy and narrative control. Constrained by need to maintain media relationships; exit would require building parallel media infrastructure. Active enforcement of access norms benefits this actor.
constraint_indexing:constraint_classification(media_gatekeeping_power, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LEGACY EDITORIAL STANDARDS (PITON) — The institutional practice of editorial gatekeeping (fact-checking, source verification, narrative coherence) was designed to reduce information chaos. But the primary function has degraded: editorial standards are now selectively enforced, theater-ratio content dominates, and the legitimacy function persists through inertia rather than effectiveness. Digital platforms have made these standards partially obsolete while maintaining their performative role.
constraint_indexing:constraint_classification(media_gatekeeping_power, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: OPEN INFORMATION MOVEMENT (SCAFFOLD) — Organized coalitions (open-source journalism, decentralized media platforms, blockchain-based distribution, fact-checking networks) are building temporary coordination mechanisms that bypass traditional gatekeeping. These scaffolds have sunset logic: as distributed verification becomes mature and algorithm transparency improves, the traditional gatekeeping power diminishes. Medium extractiveness because organized actors have agency and visible exit paths.
constraint_indexing:constraint_classification(media_gatekeeping_power, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: SOCIAL MEDIA PLATFORMS (TANGLED ROPE) — Digital platforms both replicate and disrupt traditional gatekeeping. They benefit from controlling algorithmic distribution (gatekeeping 2.0) but are constrained by regulatory pressure, advertiser demands, and coordination requirements with legacy media. They exert extraction through algorithmic prioritization while claiming to provide coordination through network effects and audience aggregation. Constrained by regulatory capture and advertiser dependency.
constraint_indexing:constraint_classification(media_gatekeeping_power, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational perspective, gatekeeping coordinates editorial standards and reduces information chaos while extracting through monopoly positioning and narrative control. The constraint simultaneously solves a genuine coordination problem (too much information requires curation) and creates asymmetric power over narrative. This is genuine tangled rope: both functions are real and inseparable.
constraint_indexing:constraint_classification(media_gatekeeping_power, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(media_gatekeeping_power_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(media_gatekeeping_power, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(media_gatekeeping_power, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(media_gatekeeping_power, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(media_gatekeeping_power, TR),
    TR >= 0.70.

:- end_tests(media_gatekeeping_power_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Gatekeeping corporations extract through monopoly control of distribution channels, algorithmic prioritization of in-house content, and narrative framing advantages. The extraction has increased from 0.42 to 0.58 because digital platforms have amplified concentration effects: traditional media consolidation plus algorithmic amplification creates dual gatekeeping. But extractiveness is below 0.70 because legitimate coordination functions (editing, fact-checking, audience aggregation) remain partially real, and alternative platforms (social media, podcasting, direct distribution) provide partial exits. Suppression (0.65): High. Barriers to accessing dominant media include capital requirements (production and distribution costs), institutional credibility requirements (track records, advertising budgets), technical infrastructure (server capacity, algorithmic optimization), and gatekeeping decisions themselves (editorial discretion to reject). Suppression is not total—some groups find workarounds (social media organic growth, podcast networks, newsletter platforms)—but it is substantial and persistent. Theater ratio (0.55): Moderate. Editorial gatekeeping involves genuine quality maintenance (fact-checking, source verification, narrative coherence) but also significant performative elements: prestige signaling, institutional legitimacy claims, and manufactured debate formats that serve ratings rather than information quality. The theater ratio increased from 0.48 to 0.55 because digital platforms have reduced the functional necessity of editorial curation while maintaining its performative role as a legitimacy mechanism.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is maximal between powerless/trapped outsiders (Snare perception) and institutional/arbitrage beneficiaries (Rope perception). The beneficiary experiences the constraint as natural coordination—what the system does is organize information, and their prominent position is earned through quality and institutional legitimacy. The powerless outsider experiences the constraint as pure extraction—the same mechanisms that coordinate for beneficiaries suppress access for them. The moderately positioned independent publisher experiences both: real benefits from editorial infrastructure and audience aggregation, real costs from algorithmic suppression and resource barriers. This perspectival gap is not a difference in perspective alone—it is a difference in structural extraction. The same constraint distributes costs and benefits asymmetrically, producing genuine disagreement about what the constraint IS. The beneficiary calls it coordination; the victim calls it extraction; both are empirically correct from their positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for each perspective derives from structural position relative to distribution extraction. Powerless/trapped agents (marginalized voices) experience d ≈ 0.95: maximum distance from beneficiary position, no arbitrage options, high f(d) → experienced extraction χ is maximum. Institutional/arbitrage agents (dominant media) experience d ≈ 0.05: beneficiary position, extraction flows toward them, low f(d) → negative or near-zero χ. Moderate/constrained agents (independent publishers) experience d ≈ 0.55-0.65: partially benefiting from coordination, partially extracted from through algorithmic suppression, mixed f(d) → moderate χ. Institutional/constrained agents (political establishment, platforms) experience d ≈ 0.40-0.50: benefiting from gatekeeping access while constrained by regulatory and relationship requirements, moderate f(d) → low-to-moderate χ. Organized/constrained agents (open information movement) experience d ≈ 0.45: organized enough to articulate alternatives, but constrained by network effects and coordination challenges, moderate f(d) → low χ. Analytical/analytical agents experience d ≈ 0.72: outsider position permits full observation of extraction mechanisms; high f(d) reflects that analytical position itself is constrained by the constraint's invisibility to insiders.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: Media gatekeeping is a genuine tangled rope, not a snare masquerading as rope or a rope hiding snare extraction. The analytical perspective confirms that coordination and extraction are inseparable in this constraint. (1) Genuine coordination function: editorial standards reduce information chaos, fact-checking surfaces verification, audience aggregation solves search costs. These are real problems that gatekeeping solves. (2) Asymmetric extraction: beneficiaries capture disproportionate benefits (revenue, narrative control, agenda-setting power), while victims bear suppression costs (access barriers, voice exclusion, narrative marginalization). The extraction is not hidden or rhetorical—it is structural and measurable. (3) Requires active enforcement: the constraint persists through conscious decisions by gatekeeping institutions (editorial review, algorithmic prioritization, advertising allocation). Without active enforcement, alternative platforms would capture audiences and narrative diversity would increase. (4) No single perspective dominates: the powerless agent correctly identifies extraction; the beneficiary correctly identifies coordination; neither is lying or mistaken. The constraint genuinely coordinates and extracts simultaneously. This is the defining signature of tangled rope. Alternative classification: Could this be a snare disguised as rope? Only from a powerless perspective with no awareness of coordination benefits. The moderate and institutional perspectives reveal the genuine coordination function, disconfirming snare classification. Could this be rope hiding snare? Only if beneficiaries are deceived about their own role, which contradicts the analytical perspective confirming active enforcement. Tangled rope is the structurally accurate classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    gatekeeper_legitimacy_foundation,
    'Is media gatekeeping legitimate because it reduces information chaos and maintains quality, or is quality maintenance a rationalization for monopoly control?',
    'Comparative analysis of fact-checking accuracy, retraction rates, and narrative diversity across gatekept vs non-gatekept information sources over 10+ year longitudinal study',
    'If quality benefits are genuine and substantial: classification shifts toward Rope (more coordination, less extraction). If quality outcomes are indistinguishable or worse: classification confirms Snare for powerless agents (pure extraction dressed as quality maintenance).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gatekeeper_legitimacy_foundation, empirical, 'Whether gatekeeping legitimacy rests on genuine quality benefits').

omega_variable(
    alternative_curation_scalability,
    'Can decentralized curation (community fact-checking, algorithmic transparency, distributed reputation systems) perform the information-reduction function at scale without concentrating power?',
    'Network analysis of distributed platforms (Bluesky, Mastodon, decentralized journalism networks); measurement of false-positive rates, coordinated misinformation detection, and audience search costs',
    'If alternatives scale effectively: scaffold perspective confirmed — transition path exists and sunset is realistic. If alternatives fail at scale: gatekeeping extraction is unavoidable (tangled rope confirmed as stable equilibrium).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_curation_scalability, empirical, 'Scalability of decentralized curation mechanisms').

omega_variable(
    algorithmic_gatekeeping_equivalence,
    'Do social media algorithms replicate traditional gatekeeping extraction or create new extraction mechanisms with different structural properties?',
    'Comparative power analysis: measurement of visibility concentration (top-10% accounts as share of total impressions) across traditional media, social media, and decentralized platforms',
    'If algorithms replicate traditional gatekeeping: constraint is resilient to platform shifts (gatekeeping power relocates to algorithms). If algorithms create distinct extraction: multiple tangled-rope constraints exist with different beneficiaries and victims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithmic_gatekeeping_equivalence, empirical, 'Whether algorithmic distribution replicates traditional gatekeeping power').

omega_variable(
    suppression_mechanism_internalization,
    'Is suppression primarily structural (economic barriers to distribution, technical requirements, institutional access) or internalized (marginalized actors self-censor due to anticipated rejection)?',
    'Survey and interview data on decision to speak vs. remain silent; measurement of suppression coefficient pre- and post-platform access removal',
    'If primarily structural: suppression reverses when barriers are removed. If internalized: suppression persists (internalized gatekeeper) even after structural barriers fall.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Structural vs. internalized suppression mechanisms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(media_gatekeeping_power, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(media_gate_tr_t0, media_gatekeeping_power, theater_ratio, 0, 0.48).
narrative_ontology:measurement(media_gate_tr_t5, media_gatekeeping_power, theater_ratio, 5, 0.52).
narrative_ontology:measurement(media_gate_tr_t10, media_gatekeeping_power, theater_ratio, 10, 0.55).
narrative_ontology:measurement(media_gate_tr_t15, media_gatekeeping_power, theater_ratio, 15, 0.58).

% Extraction over time
narrative_ontology:measurement(media_gate_be_t0, media_gatekeeping_power, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(media_gate_be_t5, media_gatekeeping_power, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(media_gate_be_t10, media_gatekeeping_power, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(media_gate_be_t15, media_gatekeeping_power, base_extractiveness, 15, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(media_gatekeeping_power, information_standard).
narrative_ontology:affects_constraint(media_gatekeeping_power, algorithmic_recommendation_bias).
narrative_ontology:affects_constraint(media_gatekeeping_power, attention_economy_extraction).
narrative_ontology:affects_constraint(media_gatekeeping_power, narrative_framing_power).

% DUAL FORMULATION NOTE:
% Media gatekeeping has decomposed into three constraint stories reflecting distinct mechanisms: information standards (traditional editorial gatekeeping, ε=0.58, tangled rope), algorithmic recommendation (digital platform gatekeeping, ε=0.65, snare), and narrative framing (power over story selection and interpretation, ε=0.52, tangled rope). All three are linked through network.affects_constraints and share institutional beneficiaries and marginalized victims, but have distinct ε values reflecting different measurement observables and structural mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(media_gatekeeping_power, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
