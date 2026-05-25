% ============================================================================
% CONSTRAINT STORY: sm_addictive_design
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sm_addictive_design, []).

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
 *   constraint_id: sm_addictive_design
 *   human_readable: Social Media Addictive Design Features
 *   domain: technological/behavioral_economics
 *
 * SUMMARY:
 *   Social media addictive design features represent a structural constraint
 *   where platform operators have systematized engagement optimization
 *   through behavioral engineering. The constraint encompasses algorithmic
 *   ranking (feeds optimized for engagement over recency), variable reward
 *   schedules (notifications timed to sustain attention), infinite content
 *   availability (scroll without friction), and social proof mechanisms
 *   (likes, shares, counts visible in real-time). This creates extraction of
 *   user attention, behavioral data, and cognitive capacity in exchange for
 *   coordination benefits (social connection, content discovery, network
 *   effects). The constraint exhibits all six DR types from different
 *   perspectives, making it a diagnostic exemplar for how indexical
 *   classification reveals the gap between designer intent (coordination) and
 *   user experience (extraction). Base extractiveness has increased from 0.35
 *   to 0.58 over the measurement interval as platforms have progressively
 *   layered addictive mechanisms on top of core social functions. Theater
 *   ratio remains relatively low (0.38) because the engagement is not purely
 *   performative — platforms do create genuine social value — but the theater
 *   has increased as the design intent has shifted from enabling connection
 *   to maximizing metrics. The suppression mechanisms (behavioral
 *   vulnerability, social costs of exit, informational asymmetry about
 *   addictive power) are high (0.72) and largely non-degrading, indicating
 *   the constraint will persist absent external intervention.
 *
 * KEY AGENTS:
 *   - End Users: Primary victims (powerless/trapped) — bear full extraction of attention, behavioral data, and cognitive capacity; exit blocked by social costs and network lock-in
 *   - Adolescent Mental Health Commons: Secondary victims (powerless/trapped) — collective harm from designs targeting developing brains; bearing costs of addiction, anxiety, depression, sleep disruption
 *   - Platform Operators: Primary beneficiaries (institutional/arbitrage) — capture value through engagement metrics, advertising revenue, and user data; experience constraint as beneficial coordination
 *   - Advertisers and Data Brokers: Secondary beneficiaries (organized/arbitrage) — benefit from high-value targeting enabled by addictive engagement and behavioral data collection
 *   - Content Creators and Influencers: Mixed position (moderate/mobile) — benefit from algorithmic amplification but also trapped by platform dependence and algorithm changes
 *   - Regulatory Coalition: Organized agents (organized/constrained) — EU/US regulators, child safety advocates building alternative requirements through legislation; viewing constraint as temporary market failure with sunset mechanism
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent profit-optimization choices as inherent features of digital markets
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sm_addictive_design, 0.58).
domain_priors:suppression_score(sm_addictive_design, 0.72).
domain_priors:theater_ratio(sm_addictive_design, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sm_addictive_design, extractiveness, 0.58).
narrative_ontology:constraint_metric(sm_addictive_design, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(sm_addictive_design, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sm_addictive_design, snare).
narrative_ontology:human_readable(sm_addictive_design, "Social Media Addictive Design Features").
narrative_ontology:topic_domain(sm_addictive_design, "technological/behavioral_economics").

domain_priors:requires_active_enforcement(sm_addictive_design).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sm_addictive_design, platform_operators).
narrative_ontology:constraint_beneficiary(sm_addictive_design, advertisers).
narrative_ontology:constraint_victim(sm_addictive_design, end_users).
narrative_ontology:constraint_victim(sm_addictive_design, adolescent_mental_health).
narrative_ontology:constraint_victim(sm_addictive_design, attention_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL USER (SNARE) — Powerless to resist algorithmic optimization designed by teams of behavioral engineers. Exit is theoretically possible but practically blocked by social costs (isolation from peer networks, missing job/relationship information, exclusion from community groups). Trapped exit option; bears full behavioral extraction through time-on-platform optimization. Maximum experienced extraction.
constraint_indexing:constraint_classification(sm_addictive_design, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ADOLESCENT MENTAL HEALTH (SNARE) — Collective harm from designs targeting developing brains during critical neuroplasticity windows. Victims cannot organize; no exit mechanism; bearing costs of addiction, anxiety, depression, sleep disruption, body image pathology, comparison effects. Suppression mechanism is informational asymmetry (teens underestimate addictive power) and developmental vulnerability (prefrontal cortex maturaton incomplete). Pure extraction with no coordination benefit.
constraint_indexing:constraint_classification(sm_addictive_design, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: PLATFORM OPERATORS (ROPE) — Perceive addictive design as coordination mechanism: maximizing engagement solves matching problem (connecting advertisers to audiences) and creates network effects that require user participation. Beneficiary with arbitrage options; experiences the constraint as beneficial coordination that solves a real problem (matching). Net positive revenue capture; no experienced extraction.
constraint_indexing:constraint_classification(sm_addictive_design, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ADVERTISERS (ROPE) — Organized institutional actors with arbitrage options. Perceive addictive design as enabling coordination: higher engagement = higher conversion rates, better ROI on ad spend. Design features that keep users trapped enable high-value targeting. Net beneficiaries; see the system as solving their matching problem with minimal coercive overhead.
constraint_indexing:constraint_classification(sm_addictive_design, rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: CONTENT CREATORS (TANGLED ROPE) — Mixed position: benefit from algorithmic amplification and audience reach (coordination function), but also trapped by platform dynamics. Must optimize content for algorithm-driven engagement; algorithm changes can destroy livelihoods. Mobile exit technically possible (migrate to competing platforms) but costly due to audience lock-in. Experience both coordination (reach/income) and extraction (algorithmic capture, platform dependence). Effective extraction moderate but real.
constraint_indexing:constraint_classification(sm_addictive_design, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: REGULATORY COALITION (SCAFFOLD) — Organized agents (EU Digital Services Act, US proposed DETOUR Act, child safety advocates) viewing addictive design as a temporary extraction mechanism with built-in sunset. Design patterns (infinite scroll, variable rewards, notification timing, algorithmic promotion) can be regulated away. Suppression mechanism (addictive design) is contingent on regulatory gaps, not technical necessity. Sees the constraint as a temporary market failure being solved through legislation and platform redesign requirements. Sunset clause: 5-10 years as regulatory frameworks mature and platforms implement consent-based non-addictive UX options.
constraint_indexing:constraint_classification(sm_addictive_design, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ATTENTION ECONOMY (PITON) — Civilizational/global view that addictive design is a natural feature of attention markets: scarcity of human attention creates extraction incentives, and optimization for engagement is the inevitable result of advertising-driven business models. This perspective naturalizes the design as an immutable feature of how digital media markets work. However, the structural evidence contradicts the piton classification — the degradation has occurred because the coordination function (user engagement) is real, but the extraction mechanism (behavioral capture for profit) is increasingly theatrical. Theater ratio (0.38) shows that much of the engagement is performative rather than genuine value creation.
constraint_indexing:constraint_classification(sm_addictive_design, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, some version of engagement optimization is inherent to any attention-mediated system: systems that capture and retain attention outcompete systems that do not, regardless of designer intent. This is a natural selection dynamic at the level of technology itself. However, THIS perspective misses the distinction between addictive design (behavioral manipulation for profit) and engagement design (optimizing for genuine value creation). The mountain classification is a false summit: it naturalizes what is actually a contingent choice between coordination (user benefit) and extraction (platform profit). The constraint is not an immutable law but a design choice enabled by regulatory gaps and informational asymmetries.
constraint_indexing:constraint_classification(sm_addictive_design, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sm_addictive_design_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sm_addictive_design, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sm_addictive_design, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sm_addictive_design, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sm_addictive_design, TR),
    TR >= 0.70.

:- end_tests(sm_addictive_design_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Platform operators extract attention, behavioral data, and cognitive capacity from users through systematic optimization of engagement. The extraction is significant but not maximal (0.66+) because users do receive genuine coordination benefits (social connection, content discovery, network effects). The increase over the interval (0.35 → 0.58) reflects progressive layering of addictive mechanisms as competition for engagement has intensified. Suppression (0.72): High and non-degrading. Exit suppression is multi-layered: (1) behavioral — reward schedules and variable reinforcement create dopamine-based habituation; (2) social — network effects and FOMO create social costs to exit; (3) informational — users systematically underestimate addictive power and behavioral capture. Suppression is increasing because design sophistication has improved and network effects have deepened. Theater ratio (0.38): Moderate, indicating the constraint retains genuine functional value (connection, discovery) alongside performative metrics optimization. The low-to-moderate theater reflects that engagement is not purely theatrical — users do form real social bonds and discover genuine content — but the design choices consistently optimize for metrics over user welfare, introducing theater into what could be a pure coordination mechanism. Mandatrophy resolved: yes. The constraint is analytically distinct from pure coordination (Rope) or pure extraction (Snare at maximized form) precisely because it exhibits both functions. Users receive genuine connection value (coordination) while bearing systematic extraction of attention and behavioral data (extraction). The beneficiary/victim declarations and institutional enforcement confirm this is Tangled Rope at the primary level, with Snare at the user level and Rope at the platform/advertiser level.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. Platform operators and advertisers perceive a beneficial coordination mechanism (Rope) — addictive design solves the attention matching problem. End users perceive pure extraction (Snare) — they are trapped by behavioral capture and social costs. Content creators perceive mixed extraction with coordination benefits (Tangled Rope) — they benefit from reach but are dependent on algorithmic preference changes. The regulatory coalition perceives a temporary coordination failure being solved through legislation (Scaffold) — addictive design is a market failure that will degrade as regulatory frameworks mature and alternative non-addictive designs emerge. The attention-economy view (Piton) mistakes architectural inertia for natural law, seeing addictive design as an inevitable feature of attention markets rather than a contingent choice enabled by regulatory gaps. The analytical observer risks naturalizing (Mountain) what is actually a profit-optimization choice by treating engagement extraction as immutable behavioral physics. The gap between platform perspective (Rope) and user perspective (Snare) is the largest, revealing the core mandatrophy: the same design features that platforms experience as beneficial coordination are experienced by users as behavioral manipulation and extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from beneficiary/victim status and exit options. Platform operators perceive low d (0.05-0.15) because they are beneficiaries with arbitrage options — they can choose to operate addictive or non-addictive designs, and they choose the former because it maximizes profit. This produces negative or minimal f(d), representing experienced benefit rather than extraction. End users perceive high d (0.90+) because they are victims with trapped exit options — FOMO, social costs, and behavioral habituation prevent exit despite experiencing harms. This produces maximum f(d) ≈ 1.42, representing maximum extraction. Content creators occupy middle ground (d ≈ 0.55-0.65) because they benefit from algorithmic reach but are also trapped by platform dependence; their exit options are mobile but costly, producing moderate experienced extraction. The regulatory coalition perceives moderate d (d ≈ 0.40-0.55) because they represent victim interests but have agency and exit pathways (legislation, alternative platforms, design mandates), producing moderate f(d). These differential directionalities explain why the constraint classifies differently from each perspective: beneficiaries see Rope; trapped victims see Snare; moderate actors see Tangled Rope; organized reformers see Scaffold.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: The mandatrophy between 'coordination mechanism' and 'extraction system' is resolved by acknowledging that addictive design is genuinely both. The coordination function is real: platforms do solve the matching problem between content creators and audiences; they do enable social connection; they do provide discovery value. The extraction function is equally real: platforms systematically optimize engagement metrics over user welfare; they capture behavioral data; they design for addiction. The constraint is Tangled Rope precisely because it exhibits both functions, and the classification reveals what was invisible in the platform operator perspective (Rope): that the coordination benefits are real but asymmetrically distributed (platforms and advertisers gain most), while extraction costs are borne primarily by users and adolescent development. The mandatrophy resolves by mapping the constraint across perspectives and measuring chi from each. Platform perspective: χ ≈ low/negative (net beneficiary, arbitrage options) → Rope. User perspective: χ ≈ high (victim, trapped) → Snare. Creator perspective: χ ≈ moderate (mixed) → Tangled Rope. The system-level classification (Snare as base, escalating to Tangled Rope with institutional awareness) reflects that the primary function is increasingly extraction with coordination as the cover mechanism, not vice versa. This is evidenced by the theater ratio (0.38) being higher than would be expected for pure coordination (Rope theater ≤ 0.30) and the extractiveness (0.58) being in the high-moderate range that defines Tangled Rope (0.40-0.90).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_vs_manipulated_engagement,
    'Is user engagement with social media features a genuine expression of preference or predominantly the result of behavioral manipulation?',
    'Neuroscience studies of reward responses (dopamine patterns, fMRI activation) during platform use; comparative analysis of engagement patterns with and without addictive design features (A/B testing with non-addictive variants); user preference revelation when given alternative platforms with different UX designs',
    'If genuine: engagement is coordination (Rope all perspectives). If predominantly manipulated: engagement is extraction (Snare/Tangled Rope dominant). This determines whether the constraint fundamentally solves a matching problem or primarily extracts attention for profit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_vs_manipulated_engagement, empirical, 'Whether engagement is genuine preference or behavioral manipulation').

omega_variable(
    addictive_design_necessity,
    'Are addictive design features (variable rewards, infinite scroll, notification timing, algorithmic ranking) necessary to achieve platform scale and network effects, or contingent optimizations for profit maximization?',
    'Historical analysis of platform growth before and after introduction of specific addictive features; comparison of user retention curves on platforms with non-addictive UX variants (Discord, BeReal, etc.); controlled redesign experiments removing features from existing platforms and measuring user retention decay',
    'If necessary: addictive design is a coordination requirement (Rope/Scaffold perspectives reinforced). If contingent: addictive design is pure optimization for extraction (Snare perspectives reinforced). Determines whether the constraint is a feature of platform economics or a choice enabled by regulatory gaps.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(addictive_design_necessity, empirical, 'Whether addictive design is technically necessary or profit-optimization choice').

omega_variable(
    adolescent_vulnerability_window,
    'Do adolescents exposed to addictive design during neuroplasticity windows experience persistent changes in reward processing, impulse control, or attention capacity, or do effects reverse after platform exit?',
    'Longitudinal neuroscience studies tracking dopamine receptor density, prefrontal connectivity, and reward sensitivity in adolescents with varying social media exposure histories; follow-up studies of users who quit platforms and measure recovery of attention/impulse control metrics; population-level analysis of psychiatric symptom trends correlating with platform adoption cohorts',
    'If persistent: addictive design creates irreversible developmental harm (classification shifts toward pure extraction/Snare for all perspectives viewing long-term costs). If reversible: short-term extraction with recovery possible (Scaffold sunset mechanism more credible). Determines whether the constraint damages human capital irreversibly or temporarily.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(adolescent_vulnerability_window, empirical, 'Whether adolescent dopamine/attention changes from addictive design are persistent').

omega_variable(
    regulatory_capture_depth,
    'Can regulatory frameworks (Digital Services Act, age-verification, design transparency requirements) meaningfully constrain addictive design, or do platforms retain sufficient regulatory capture/innovation speed to evade compliance through feature redesign?',
    'Post-regulation comparative analysis of addictive feature prevalence on EU vs. non-EU versions of major platforms; measurement of behavioral engagement metrics before/after regulatory compliance deadlines; analysis of platform innovation velocity: time from feature ban to functional equivalent workaround',
    'If regulations effective: Scaffold sunset is real, constraint will degrade to lower extractiveness over 5-10 years. If platforms evade successfully: Scaffold perspective is aspirational, constraint will persist or morph into new designs (classification remains Snare). Determines credibility of the regulatory coalition''s sunset strategy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_depth, empirical, 'Whether regulatory frameworks can effectively constrain addictive design features').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sm_addictive_design, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(smad_tr_t0, sm_addictive_design, theater_ratio, 0, 0.22).
narrative_ontology:measurement(smad_tr_t5, sm_addictive_design, theater_ratio, 5, 0.3).
narrative_ontology:measurement(smad_tr_t10, sm_addictive_design, theater_ratio, 10, 0.38).

% Extraction over time
narrative_ontology:measurement(smad_be_t0, sm_addictive_design, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(smad_be_t5, sm_addictive_design, base_extractiveness, 5, 0.47).
narrative_ontology:measurement(smad_be_t10, sm_addictive_design, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sm_addictive_design, information_standard).
narrative_ontology:affects_constraint(sm_addictive_design, attention_scarcity_rent_seeking).
narrative_ontology:affects_constraint(sm_addictive_design, behavioral_data_surveillance).
narrative_ontology:affects_constraint(sm_addictive_design, adolescent_neural_development_harm).

% DUAL FORMULATION NOTE:
% Addictive design is a coordination mechanism (matching content to users) that has been progressively layered with extraction mechanisms (behavioral capture for profit). The constraint family includes: (1) core social matching (low ε, Rope), (2) engagement-driven ranking systems (medium ε, Tangled Rope), (3) notification/reward manipulation (high ε, Snare). These are structurally linked — the second builds on the first, the third builds on the second — but represent distinct ε values and classifications. The addictive design constraint itself (this story) occupies the Tangled Rope position, representing the current institutional arrangement where coordination and extraction are inseparably entangled.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
