% ============================================================================
% CONSTRAINT STORY: adolescent_neural_development_harm
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_adolescent_neural_development_harm, []).

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
 *   constraint_id: adolescent_neural_development_harm
 *   human_readable: Adolescent Neural Development Harm from Digital Technology Exposure
 *   domain: neurodevelopmental/public_health/technology_policy
 *
 * SUMMARY:
 *   Adolescent neural development occurs during a critical period of
 *   heightened neuroplasticity (ages 12-25), characterized by structural
 *   reorganization of the prefrontal cortex, recalibration of dopamine
 *   systems, and social-reward learning. This developmental window creates
 *   both developmental necessity (adolescents must integrate into peer groups
 *   and practice social coordination) and vulnerability (immature
 *   decision-making systems and reward-sensitivity hypersensitivity make them
 *   susceptible to reinforcement-based manipulation). Digital platforms have
 *   captured this vulnerability through designs explicitly engineered to
 *   maximize engagement: variable reward schedules (notifications), infinite
 *   scroll interfaces, algorithmic recommendation systems that amplify
 *   emotionally triggering content, and social validation feedback loops. The
 *   constraint operates at multiple scales: individual (attention
 *   fragmentation, sleep disruption, dopamine dysregulation), relational
 *   (parent-child conflict, peer pressure intensification), institutional
 *   (degraded research visibility, regulatory capture by industry), and
 *   generational (cohort-level increases in anxiety, depression, and
 *   attention disorders correlating with platform adoption). The
 *   extractiveness value (0.62) reflects that platforms capture adolescent
 *   attention and neurological development as a deliberate profit mechanism
 *   (engagement metrics drive advertising revenue), while simultaneously
 *   obscuring this extraction through framing the system as peer-coordination
 *   infrastructure. The suppression value (0.68) reflects high barriers to
 *   exit: neurological maturation cannot be paused, peer belonging cannot be
 *   obtained outside platforms, and parents lack both information and
 *   leverage to limit exposure effectively. The theater ratio (0.55) reflects
 *   that platforms present their design as user-benefit optimization (UX
 *   research, creator support, community building) while internal documents
 *   reveal deliberate engagement-maximization and harm minimization as
 *   competing objectives.
 *
 * KEY AGENTS:
 *   - Adolescents (ages 12-25): Primary victims (powerless/trapped) — developing brains cannot exit exposure during critical neurological window; social belonging requires platform access; prefrontal cortex immaturity prevents strategic limitation of use
 *   - Parents and Guardians: Secondary victims (moderate/constrained) — lose oversight capacity as algorithmic curation obscures content, face social costs if restricting child access, experience own attention fragmentation limiting supervision
 *   - Technology Platform Operators: Primary beneficiaries (institutional/arbitrage) — extract attention as monetizable commodity; engagement metrics drive advertising revenue; can exit/pivot business model without bearing developmental costs
 *   - Advertising Industrial Complex: Secondary beneficiary (powerful/arbitrage) — captures attention targeting and behavioral prediction data; funds platform development through ad spending; maintains extraction mechanism through business model dependency
 *   - Developmental Neuroscience Research Community: Institutional actor (powerful/mobile) — generates evidence of harm but observes own mechanism as degraded (piton); industry-funded counter-research, publication bias, and researcher reputational risk limit coordination around protective norms
 *   - Regulatory and Advocacy Coalition: Organized actor (organized/constrained) — legislators, pediatric associations, child advocacy groups building alternative policy frameworks; perceive regulatory solutions as viable but suppression is high (industry lobbying, regulatory capture); see sunset in emerging age-verification and platform duty-of-care legislation
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing adolescent vulnerability to neural harm as immutable developmental feature rather than contingent institutional design choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(adolescent_neural_development_harm, 0.62).
domain_priors:suppression_score(adolescent_neural_development_harm, 0.68).
domain_priors:theater_ratio(adolescent_neural_development_harm, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(adolescent_neural_development_harm, extractiveness, 0.62).
narrative_ontology:constraint_metric(adolescent_neural_development_harm, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(adolescent_neural_development_harm, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(adolescent_neural_development_harm, snare).
narrative_ontology:human_readable(adolescent_neural_development_harm, "Adolescent Neural Development Harm from Digital Technology Exposure").
narrative_ontology:topic_domain(adolescent_neural_development_harm, "neurodevelopmental/public_health/technology_policy").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(adolescent_neural_development_harm, technology_platform_operators).
narrative_ontology:constraint_beneficiary(adolescent_neural_development_harm, advertising_industrial_complex).
narrative_ontology:constraint_victim(adolescent_neural_development_harm, adolescents_developing_brains).
narrative_ontology:constraint_victim(adolescent_neural_development_harm, parental_oversight_capacity).
narrative_ontology:constraint_victim(adolescent_neural_development_harm, developmental_neuroscience_field).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEVELOPING ADOLESCENT (SNARE) — Neurologically immature decision-making systems (prefrontal cortex still developing until age 25) cannot exit exposure to platforms engineered for addictive engagement. Trapped by developmental stage, peer pressure (social belonging drives platform use), and lack of alternatives for social coordination. Bears extraction through attention harvesting, dopamine dysregulation, sleep disruption, and attention fragmentation. Maximum suppression: the biological window is closing; cannot wait for alternatives to mature.
constraint_indexing:constraint_classification(adolescent_neural_development_harm, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PARENT/GUARDIAN (TANGLED ROPE) — Structurally constrained by peer effects (adolescent's social participation requires platform access), information asymmetry (constant feature changes), and own attention fragmentation. Experiences both coordination (platforms enable family communication, peer connection) and extraction (lost oversight capacity, algorithmic steering of adolescent behavior). Moderate power; exit incurs social costs for the adolescent without eliminating harm.
constraint_indexing:constraint_classification(adolescent_neural_development_harm, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PLATFORM OPERATOR (ROPE) — Net beneficiary experiencing the constraint as a pure coordination mechanism: connecting users, enabling communication, and capturing engagement metrics for ad targeting. Perceives the system as solving a genuine coordination problem (social connection) with minimal coercive overhead from the platform's perspective. Arbitrage exit options (can pivot business model, exit markets, shift revenue sources) mean they experience low effective extraction relative to the harm they generate.
constraint_indexing:constraint_classification(adolescent_neural_development_harm, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DEVELOPMENTAL NEUROSCIENCE FIELD (PITON) — Institutional capacity to study adolescent neural development exists but is performatively obscured by industry-funded counter-research, selective publication of null results, and reputational risks to researchers who publish findings that implicate platforms. The field sees its own mechanism as degraded (theater_ratio 0.70+): research exists, but its influence on policy is theatrical. Powerful agents have mobile exit options but institutional inertia prevents coordination around harm reduction.
constraint_indexing:constraint_classification(adolescent_neural_development_harm, piton,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY/ADVOCACY COALITION (SCAFFOLD) — Organized state and civil-society actors (legislators, pediatric associations, child advocacy groups) are building regulatory alternatives: digital limits laws, age-verification requirements, attention-protection norms, platform duty of care standards. See this as a temporary coordination failure being solved through generational policy shifts. Suppression is high (industry captures regulators), but organized agents perceive a sunset: once age-restricted design becomes legally mandated, the extraction mechanism's force declines. Scaffold derives from visible sunset clauses in emerging legislation.
constraint_indexing:constraint_classification(adolescent_neural_development_harm, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (NATURALIZED VIEW) — From a civilizational perspective, adolescent vulnerability to neural harm could appear as an immutable feature of human development: adolescents have always been risk-prone, novelty-seeking, and susceptible to peer influence because these are developmentally adaptive strategies. This perspective risks naturalizing the constraint as inherent to biology rather than contingent institutional design. The engine will flag this as a false summit: the underlying vulnerability is real, but the extraction mechanism (algorithmic amplification of addictive engagement, attention harvesting) is not a law of nature.
constraint_indexing:constraint_classification(adolescent_neural_development_harm, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(adolescent_neural_development_harm_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(adolescent_neural_development_harm, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(adolescent_neural_development_harm, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(adolescent_neural_development_harm, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(adolescent_neural_development_harm, TR),
    TR >= 0.70.

:- end_tests(adolescent_neural_development_harm_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): High. Platform operators deliberately engineer addictive engagement, generating time-on-app metrics that drive advertising revenue. The extraction is not incidental to coordination — internal documents reveal engagement maximization explicitly optimizes for metrics that produce behavioral harm (sleep disruption, attention fragmentation, anxiety amplification). The value reflects that this is not pure extraction (some genuine coordination of peer connection occurs) but the coordination is subordinated to extraction. The measurement trajectory (0.35 → 0.62 over 20 years) reflects increasing optimization: early platforms were less refined; modern platforms (2015-2026) have incorporated multiple engagement-maximization mechanisms discovered through years of behavioral experimentation. Suppression (0.68): High. Adolescents cannot exit the developmental window; peer belonging requires platform access as norms have shifted; parents lack both information (constant algorithmic changes) and leverage (social costs of restriction, inability to provide equivalent peer-coordination alternatives). The suppression is not total (some adolescents do limit use, some families resist), but barriers are severe. Measurement trajectory reflects increasing suppression as platforms have become more socially essential and algorithmic opacity has increased. Theater ratio (0.55): Moderate. Platforms present themselves as user-benefit-maximizing (UX research, creator monetization, community building, mental health features), and some genuine user benefit exists. However, internal evidence reveals this framing as partially theatrical: engagement-maximization explicitly trades off against user well-being in design decisions, and harm-reduction features are added only when regulatory pressure emerges. The theater ratio of 0.55 reflects that the system combines real coordination function with significant performative legitimacy claims.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence from identical base properties. The developing adolescent perceives snare: no exit, maximum suppression, pure extraction of attention and neurological development. Parents perceive tangled_rope: genuine peer-coordination benefit alongside harmful extraction of oversight capacity. Platform operators perceive rope: coordination mechanism solving peer-connection problem with minimal overhead from their structural position. Developmental neuroscience perceives piton: research exists and reveals harm, but institutional mechanisms are theatrical (industry-funded counter-research, publication bias, career risk for findings that implicate platforms). Regulatory coalition perceives scaffold: temporary problem with a visible sunset as legislation mandates platform duty of care. Analytical observer risks perceiving mountain: adolescent vulnerability to neural harm could naturalize as inherent developmental feature. The perspectival gap reveals that the constraint's classification is not observer-independent but rather depends entirely on structural position: who benefits, who bears costs, what exit options exist, and what time horizon is assumed.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from agents' structural positions relative to the extraction flow. Adolescents occupy d ≈ 0.95 (trapped victims with no exit): developing brains during critical window, social belonging necessity, peer-pressure amplification. Parents occupy d ≈ 0.70 (constrained victims with partial benefits): lose oversight but gain connection tools, face social costs of limitation. Platform operators occupy d ≈ 0.10 (institutional beneficiaries with arbitrage options): extract attention as profit without bearing developmental costs, can pivot business model. The field of developmental neuroscience occupies d ≈ 0.55 (powerful moderate agents constrained by industry influence): has knowledge and research capacity but sees own mechanism as degraded (piton). Regulatory coalition occupies d ≈ 0.60 (organized agents with constrained but visible exit path): see alternatives building through regulation, suppression is high but sunset is visible. The perspectival gaps reflect these directionality differences: powerless adolescents see pure extraction (snare), moderately-powerful parents see mixed coordination/extraction (tangled_rope), institutional beneficiaries see pure coordination (rope), degraded research institutions see their own theatrical ritual (piton), organized regulatory actors see a sunset-based problem (scaffold), and civilizational observers risk naturalizing the constraint (false summit mountain).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: This constraint resolves the mandatrophy by demonstrating that all six types are structurally coherent readings of the same base properties from different observer positions. The mandatrophy question — 'Is adolescent neural development harm extraction or coordination or natural necessity?' — has no single answer because the answer depends on who you are in the constraint. For the adolescent: snare (trapped, no exit, maximum extraction). For the beneficiary: rope (pure coordination of peer connection). For the regulatory coalition: scaffold (temporary problem with sunset in emerging regulation). For the degraded research institution: piton (mechanism exists but is theatrical). For the parent: tangled_rope (mixed coordination and extraction). For the civilizational view: false summit mountain (naturalization of contingent institutional design). The resolution is not that one type is correct and others are wrong, but rather that the presheaf of perspectives over observer positions IS the correct description. The constraint's classification at each perspective is entailed by the base properties plus the observer's structural relationship (power, exit, time horizon, scope). Mandatrophy resolves when the analyst accepts that multi-perspectival classification is not incoherence but structural insight.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causality_vs_correlation,
    'Do observed neural changes (reduced gray matter, altered dopamine signaling, sleep disruption) in heavy platform users result from causal exposure or selection (pre-existing vulnerability driving platform use)?',
    'Longitudinal neuroimaging studies with randomized reduction in exposure; comparison of neural trajectories across matched groups with controlled platform access; prospective identification of pre-exposure vulnerability markers',
    'If causal: extractiveness remains high (0.62+), suppression justified by biological window closure. If mostly selection: extractiveness drops to ~0.35 (coordination mechanism with pre-existing vulnerability), snare classification collapses to rope/tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causality_vs_correlation, empirical, 'Causal harm vs. selection bias in neural outcomes').

omega_variable(
    mechanism_sufficiency,
    'Is intentional platform design for addictive engagement (variable rewards, infinite scroll, algorithmic recommendation) a necessary cause of neural harm, or would exposure to any high-bandwidth social medium produce similar developmental disruption?',
    'Historical comparison: pre-smartphone adolescent peer interaction effects on sleep, attention, anxiety; neuroimaging studies of less-optimized digital environments (plain social networks without engagement-maximization); intervention studies with algorithmic demotion of engagement-maximizing features',
    'If design-specific: extraction mechanism is clear (intentional optimization for harm), snare classification robust. If any-social-medium: extraction is obscured by unavoidable developmental effects of peer coordination; reclassify to tangled_rope with reduced suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mechanism_sufficiency, empirical, 'Whether extraction mechanism requires intentional harmful design').

omega_variable(
    alternative_coordination_maturity,
    'Can alternative digital platforms designed for adolescent well-being (no engagement maximization, transparent algorithms, developer-friendly regulatory compliance) sustain the peer-coordination function at lower cost?',
    'Pilot deployment of well-designed alternatives; measurement of adoption rates, retention, functional equivalence for peer connection; cost-benefit analysis of regulatory compliance vs. user experience',
    'If viable alternatives exist: scaffold perspective confirmed, sunset is real, suppression may decline. If alternatives fail: coordination problem is genuine, extraction mechanism includes structural necessity, reclassify snare as more robust tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_coordination_maturity, empirical, 'Whether harm-reducing alternatives can sustain peer coordination').

omega_variable(
    developmental_window_reversibility,
    'Are neural changes during adolescent heavy platform use reversible after exposure ceases, or do they produce permanent organizational alterations that persist into adulthood?',
    'Longitudinal neuroimaging through transition to adulthood; measurement of functional outcomes (attention, reward sensitivity, sleep quality) in individuals who reduce/cease platform use; comparison with those who maintain heavy use',
    'If reversible: suppression is lower than assessed (0.68 → 0.45), extraction timing window is critical but recovery is possible. If permanent: suppression justified, snare classification robust, extraction mechanism''s temporal force is maximized.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(developmental_window_reversibility, empirical, 'Reversibility of neural changes after exposure cessation').

omega_variable(
    social_belonging_necessity,
    'Is platform-mediated peer connection functionally necessary for adolescent social-emotional development, or do non-digital peer coordination mechanisms (school, community, family) provide sufficient belonging without extraction costs?',
    'Cross-cultural comparison of adolescent outcomes in low-platform-access populations; longitudinal tracking of social development outcomes in platform-limited cohorts; measurement of peer-connection adequacy from non-digital sources',
    'If platforms are functionally necessary: trapped classification is robust, suppression reflects genuine biological urgency, snare is unavoidable. If non-digital coordination suffices: exit option shifts to constrained (not trapped), classification downgrades to tangled_rope, regulation becomes structurally viable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(social_belonging_necessity, empirical, 'Functional necessity of platform-mediated peer connection').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(adolescent_neural_development_harm, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(adol_neuro_tr_t0, adolescent_neural_development_harm, theater_ratio, 0, 0.3).
narrative_ontology:measurement(adol_neuro_tr_t10, adolescent_neural_development_harm, theater_ratio, 10, 0.48).
narrative_ontology:measurement(adol_neuro_tr_t20, adolescent_neural_development_harm, theater_ratio, 20, 0.55).
narrative_ontology:measurement(adol_neuro_tr_t5, adolescent_neural_development_harm, theater_ratio, 5, 0.38).

% Extraction over time
narrative_ontology:measurement(adol_neuro_be_t0, adolescent_neural_development_harm, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(adol_neuro_be_t10, adolescent_neural_development_harm, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(adol_neuro_be_t20, adolescent_neural_development_harm, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(adol_neuro_be_t5, adolescent_neural_development_harm, base_extractiveness, 5, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(adolescent_neural_development_harm, attachment_coordination).
narrative_ontology:affects_constraint(adolescent_neural_development_harm, adolescent_mental_health_epidemic).
narrative_ontology:affects_constraint(adolescent_neural_development_harm, sleep_deprivation_normalization).
narrative_ontology:affects_constraint(adolescent_neural_development_harm, attention_fragmentation_institutional).
narrative_ontology:affects_constraint(adolescent_neural_development_harm, parental_oversight_technological_gap).

% DUAL FORMULATION NOTE:
% Adolescent neural development harm decomposes into multiple structurally distinct constraints. This story captures the primary extraction mechanism (platform engagement-maximization). The upstream story would be platform business model dependency on engagement metrics (affects this constraint). The downstream stories are specific harm manifestations: mental health epidemic (anxiety, depression outcomes), sleep disruption (circadian rhythm dysregulation), attention fragmentation (divided attention as normalized cognitive state), parental oversight gap (information asymmetry and leverage loss). Each downstream constraint has its own ε value reflecting specific harm mechanisms. Together they form a constraint family: the platform business model constraint enables this neural development constraint, which produces the mental health, sleep, and attention manifestations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(adolescent_neural_development_harm, powerful, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
