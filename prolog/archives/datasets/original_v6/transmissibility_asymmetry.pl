% ============================================================================
% CONSTRAINT STORY: transmissibility_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_transmissibility_asymmetry, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: transmissibility_asymmetry
 *   human_readable: Transmissibility Asymmetry Between Simple and Complex Claims
 *   domain: political_philosophy/rhetoric/epistemology
 *
 * SUMMARY:
 *   The transmissibility asymmetry describes the empirical observation that
 *   simple, emotionally valent, ideologically clear claims spread faster and
 *   reach larger audiences than nuanced, conditional, institutionally
 *   detailed analysis, independent of truth value. This pattern appears
 *   across communication substrates (oral tradition, print, broadcast,
 *   digital) and persists despite institutional efforts to promote nuance
 *   (media literacy education, fact-checking infrastructure, academic
 *   prestige systems). The constraint is classified as mountain from all
 *   perspectives because the asymmetry appears to derive from
 *   information-theoretic and cognitive limits rather than from removable
 *   institutional barriers. However, the presence of identifiable
 *   beneficiaries (simplifiers, ideological entrepreneurs, attention
 *   merchants) triggers the false summit detector, requiring omega variables
 *   to resolve whether the asymmetry is genuinely natural or a naturalized
 *   extraction mechanism.
 *
 * KEY AGENTS:
 *   - Information Consumer: Powerless agent (powerless/trapped) — experiences asymmetry as unchangeable feature of information environment; finite cognitive bandwidth
 *   - Platform Algorithm Designer: Institutional agent (institutional/arbitrage) — can modulate but not reverse transmission gradient; algorithms follow user behavior which follows cognitive architecture
 *   - Institutional Analyst: Powerful agent (powerful/mobile) — produces complex analysis; experiences asymmetry as career constraint but recognizes structural origin
 *   - Media Literacy Coalition: Organized agent (organized/constrained) — teaches critical evaluation; sees asymmetry as permanent feature requiring continuous compensatory effort
 *   - Simplifiers/Ideological Entrepreneurs/Attention Merchants: Declared beneficiaries (institutional/arbitrage) — occupy transmission advantage niche; omega variable addresses whether they extract rents or passively benefit
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees asymmetry as universal across substrates and epochs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(transmissibility_asymmetry, 0.08).
domain_priors:suppression_score(transmissibility_asymmetry, 0.03).
domain_priors:theater_ratio(transmissibility_asymmetry, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(transmissibility_asymmetry, extractiveness, 0.08).
narrative_ontology:constraint_metric(transmissibility_asymmetry, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(transmissibility_asymmetry, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(transmissibility_asymmetry, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(transmissibility_asymmetry, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(transmissibility_asymmetry, mountain).
narrative_ontology:human_readable(transmissibility_asymmetry, "Transmissibility Asymmetry Between Simple and Complex Claims").
narrative_ontology:topic_domain(transmissibility_asymmetry, "political_philosophy/rhetoric/epistemology").

domain_priors:emerges_naturally(transmissibility_asymmetry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(transmissibility_asymmetry, simplifiers).
narrative_ontology:constraint_beneficiary(transmissibility_asymmetry, ideological_entrepreneurs).
narrative_ontology:constraint_beneficiary(transmissibility_asymmetry, attention_merchants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INFORMATION CONSUMER (MOUNTAIN) — Experiences the asymmetry as an unchangeable feature of information environments. Cannot individually alter the transmission dynamics; simple claims reach them regardless of effort to seek nuance. Cognitive bandwidth and attention are finite resources governed by information-theoretic limits.
constraint_indexing:constraint_classification(transmissibility_asymmetry, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PLATFORM ALGORITHM DESIGNER (MOUNTAIN) — Even with full control over recommendation systems, cannot eliminate the asymmetry. Engagement metrics reflect genuine cognitive processing constraints: simple claims require less working memory, parse faster, trigger emotional responses more reliably. Algorithmic amplification follows user behavior, which follows cognitive architecture. The designer can modulate but not reverse the gradient.
constraint_indexing:constraint_classification(transmissibility_asymmetry, mountain,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (MOUNTAIN) — The transmissibility asymmetry derives from information-theoretic and cognitive constraints that appear universal across communication media. Shannon entropy, working memory capacity (Miller's 7±2), emotional valence as heuristic shortcut, and the computational complexity of evaluating conditional claims all create structural advantages for simple messages. Observable across oral tradition, print media, broadcast, and digital platforms. The asymmetry persists independent of institutional arrangement or technological substrate.
constraint_indexing:constraint_classification(transmissibility_asymmetry, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: MEDIA LITERACY COALITION (MOUNTAIN) — Organized efforts to teach critical evaluation, source verification, and nuance recognition face the same asymmetry. Educational interventions can shift individual behavior at the margin but cannot eliminate the transmission gradient. Even highly trained analysts default to heuristic processing under time pressure or cognitive load. The coalition sees the constraint as a permanent feature requiring continuous compensatory effort, not a solvable problem.
constraint_indexing:constraint_classification(transmissibility_asymmetry, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: INSTITUTIONAL ANALYST (MOUNTAIN) — Analysts producing nuanced institutional analysis experience the asymmetry as a career constraint but recognize it as structural rather than contingent. Complex analysis reaches smaller audiences regardless of quality or institutional backing. The analyst can choose simpler framings (exit to simplification) but cannot make complex claims transmit at simple-claim rates without losing the complexity that constitutes the analysis. The asymmetry is a selection pressure, not a removable barrier.
constraint_indexing:constraint_classification(transmissibility_asymmetry, mountain,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(transmissibility_asymmetry_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(transmissibility_asymmetry, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(transmissibility_asymmetry, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(transmissibility_asymmetry, ExtMetricName, E),
    domain_priors:suppression_score(transmissibility_asymmetry, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(transmissibility_asymmetry),
    narrative_ontology:constraint_metric(transmissibility_asymmetry, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(transmissibility_asymmetry, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(transmissibility_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Very low. The asymmetry creates differential reach for simple vs complex claims, but this appears to be a structural feature of information transmission rather than an extraction mechanism. The slight extractiveness reflects that simplifiers may capture attention and resources that would otherwise flow to nuanced analysis, but the magnitude is minimal because the asymmetry does not suppress alternatives — complex analysis remains available to those who seek it. The value is above zero (not pure mountain at 0.00-0.05) because identifiable agents do benefit from the gradient. Suppression (0.03): Very low. The asymmetry does not prevent complex claims from being produced or accessed; it affects transmission rates, not availability. Nuanced analysis exists in academic journals, longform journalism, books, and specialist forums. The constraint is a selection pressure, not a barrier. Theater ratio (0.15): Very low. The asymmetry is not performative — it reflects genuine cognitive and information-theoretic dynamics. Engagement metrics (shares, retweets, upvotes) are noisy but not theatrical; they measure real transmission events. The slight theater component reflects that some platform metrics are gameable and some viral spread is bot-driven, but the core asymmetry persists in organic human behavior. Accessibility collapse (0.92): Very high. The transmission gradient is extremely difficult to reverse. Institutional interventions (media literacy, algorithmic adjustments, prestige systems favoring nuance) produce marginal effects but do not eliminate the asymmetry. Resistance (0.08): Very low. Attempts to equalize transmission rates face strong headwinds from cognitive architecture and information theory. The asymmetry reasserts itself across substrates and epochs.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits minimal perspectival gap — all five perspectives classify as mountain. The uniformity reflects that the asymmetry appears to derive from constraints (information theory, cognitive architecture) that bind all agents regardless of power, exit options, or time horizon. The powerless consumer, the institutional designer, the organized coalition, the powerful analyst, and the analytical observer all experience the same transmission gradient. The lack of gap is itself diagnostic: genuine natural laws produce perspectival invariance. The false summit detector fires not because of perspectival disagreement but because beneficiaries exist — agents who systematically benefit from the asymmetry. The omega variables resolve whether these beneficiaries are extracting rents (making the mountain classification a naturalization of contingent arrangements) or simply occupying the niche that information-theoretic limits create (confirming the mountain classification).
 *
 * DIRECTIONALITY LOGIC:
 *   All perspectives classify as mountain because the asymmetry appears invariant to the observer's structural position. The information consumer cannot individually escape the gradient. The platform designer cannot reverse it algorithmically. The institutional analyst cannot make complex claims transmit at simple-claim rates without sacrificing complexity. The media literacy coalition cannot eliminate the asymmetry through education. The analytical observer sees the pattern as universal. Beneficiaries are declared (simplifiers, ideological entrepreneurs, attention merchants) because identifiable agents do occupy the transmission advantage niche, but the omega variables address whether this constitutes extraction (active suppression of nuance, rent-seeking) or passive niche occupation (benefiting from a natural gradient without creating or maintaining it). If omega resolution reveals active suppression, the constraint reclassifies as false summit (tangled_rope or snare). If resolution confirms passive niche occupation, mountain classification holds.
 *
 * MANDATROPHY ANALYSIS:
 *   The transmissibility asymmetry resolves mandatrophy by demonstrating that very low extractiveness (0.08) combined with declared beneficiaries can still produce mountain classification if the beneficiaries occupy natural niches rather than extract rents. The mandatrophy question is: 'Can a constraint be both a natural law AND benefit identifiable agents?' The answer is yes, if the agents benefit from the law's existence without creating or maintaining it. Gravity benefits those who sell fall protection equipment, but this does not make gravity a snare. The asymmetry benefits simplifiers, but if the asymmetry derives from Shannon entropy and working memory limits, the benefit is niche occupation, not extraction. The omega variables formalize this distinction: if beneficiaries actively suppress nuance (lobbying against media literacy, gaming algorithms to bury complex content, creating institutional barriers to longform analysis), the constraint reclassifies as false summit. If beneficiaries passively benefit (producing simple content that transmits well because of natural dynamics), mountain classification holds. The false summit detector ensures the question is asked; the omega variables provide the resolution mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    beneficiary_extraction_mechanism,
    'Do the declared beneficiaries (simplifiers, ideological entrepreneurs, attention merchants) extract rents from the asymmetry, or do they simply occupy the niche the asymmetry creates?',
    'Counterfactual analysis: if the asymmetry were eliminated (e.g., via cognitive enhancement technology that equalized processing costs), would these agents lose structural advantage or merely face different competition? Longitudinal tracking of whether simplifiers actively suppress nuance or passively benefit from transmission dynamics.',
    'If active suppression exists: reclassify as false summit (tangled_rope or snare). If passive niche occupation: mountain classification confirmed. The distinction determines whether the constraint is a natural law or a naturalized institutional arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_extraction_mechanism, empirical, 'Whether beneficiaries extract rents or occupy natural niches').

omega_variable(
    substrate_invariance,
    'Is the asymmetry truly substrate-invariant (appears in oral, print, broadcast, digital) or does it vary with medium in ways that suggest institutional rather than natural origins?',
    'Cross-medium comparative analysis: measure transmission gradients in oral tradition (folklore), print (pamphlets vs treatises), broadcast (soundbites vs documentaries), and digital (tweets vs longform). If gradients vary significantly, the asymmetry may be medium-specific rather than universal.',
    'If substrate-variant: mountain classification weakens; constraint may be a family of medium-specific tangled ropes. If substrate-invariant: mountain classification strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(substrate_invariance, empirical, 'Whether transmission asymmetry is invariant across communication media').

omega_variable(
    cognitive_enhancement_counterfactual,
    'Would cognitive enhancement technologies (working memory expansion, processing speed increases, attention augmentation) eliminate the asymmetry or merely shift the complexity threshold?',
    'Theoretical modeling of enhanced cognitive architectures; empirical testing with cognitive aids (external memory, visualization tools, AI summarization). If asymmetry persists at higher complexity levels, it reflects fundamental information-theoretic limits. If it disappears, it reflects contingent biological constraints.',
    'If enhancement eliminates asymmetry: constraint is biological mountain (contingent on human cognitive architecture) rather than information-theoretic mountain (universal). If asymmetry persists: information-theoretic mountain confirmed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cognitive_enhancement_counterfactual, conceptual, 'Whether cognitive enhancement would eliminate or merely shift the asymmetry').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(transmissibility_asymmetry, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trans_asym_tr_t0, transmissibility_asymmetry, theater_ratio, 0, 0.12).
narrative_ontology:measurement(trans_asym_tr_t5, transmissibility_asymmetry, theater_ratio, 5, 0.14).
narrative_ontology:measurement(trans_asym_tr_t10, transmissibility_asymmetry, theater_ratio, 10, 0.15).

% Extraction over time
narrative_ontology:measurement(trans_asym_be_t0, transmissibility_asymmetry, base_extractiveness, 0, 0.06).
narrative_ontology:measurement(trans_asym_be_t5, transmissibility_asymmetry, base_extractiveness, 5, 0.07).
narrative_ontology:measurement(trans_asym_be_t10, transmissibility_asymmetry, base_extractiveness, 10, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(transmissibility_asymmetry, information_standard).

% DUAL FORMULATION NOTE:
% The transmissibility asymmetry is a candidate natural law rather than a decomposed constraint family. If substrate-invariance omega resolves negatively (asymmetry varies significantly by medium), decompose into medium-specific stories: oral_transmission_asymmetry, print_transmission_asymmetry, broadcast_transmission_asymmetry, digital_transmission_asymmetry, each with its own epsilon reflecting medium-specific extraction mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
