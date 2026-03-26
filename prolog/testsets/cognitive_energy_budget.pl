% ============================================================================
% CONSTRAINT STORY: cognitive_energy_budget
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cognitive_energy_budget, []).

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
 *   constraint_id: cognitive_energy_budget
 *   human_readable: The Attention Exhaustion Trap
 *   domain: cognitive/technological/social
 *
 * SUMMARY:
 *   The cognitive energy budget constraint represents the finite biological
 *   limit of individual daily cognitive capacity, now operated as an
 *   extraction mechanism by attention-hungry employment systems, engagement
 *   platforms, and advertising models. Every human has a fixed-ish pool of
 *   executive function resources (glucose, dopamine, sustained attention
 *   capacity) that depletes through the day. This immutable biological fact
 *   has been colonized by institutional arrangements that compete for
 *   attention and cognition: email responsiveness norms in knowledge work,
 *   algorithmic feeds engineered for engagement, streaming platforms
 *   optimized for binge-watching, and social media designed to maximize daily
 *   active users. The constraint's extractiveness has increased over the
 *   interval (0.32 to 0.58) as devices have become ubiquitous, notification
 *   systems more sophisticated, and always-on work culture more dominant. The
 *   theater_ratio has also increased (0.25 to 0.48) because the institutional
 *   response to flagging attention (productivity software, wellness
 *   workshops, notification management) is increasingly performative:
 *   organizations install focus-time tools while maintaining email escalation
 *   norms that undermine the tools. The constraint exhibits six distinct
 *   perspectival readings, ranging from a genuine neurobiological mountain
 *   (civilizational view) to a pure extraction snare (powerless worker view)
 *   to an emerging scaffold with sunset properties (digital wellness
 *   movements). The mandate-atrophy tension is resolved by recognizing that
 *   the constraint conflates two structurally different claims: (1) humans
 *   have finite cognitive energy (mountain), and (2) modern institutional
 *   arrangements competitively extract that energy (snare/tangled rope).
 *   These are not the same constraint viewed from different angles—they have
 *   different ε values and different resolution mechanisms.
 *
 * KEY AGENTS:
 *   - Knowledge Workers: Primary victim (powerless/trapped) — bear maximum extraction from employment responsiveness norms and personal attention platform use; no exit available without career cost
 *   - Students: Primary victim (powerless/trapped) — trapped in educational extraction (coursework demands) and peer social extraction (social media pressure); no institutional support for attention recovery
 *   - Attention Extraction Platforms (social media, streaming, news): Primary beneficiary (institutional/arbitrage) — benefit from engagement optimization, behavioral data access, advertising premium pricing; claim to be solving coordination problems (content discovery, social connection)
 *   - Advertisers and Data Brokers: Secondary beneficiary (powerful/arbitrage) — extract value from behavioral targeting, high-frequency bidding, audience data; constrained by platform algorithm changes and privacy regulation
 *   - Educational Institutions: Institutional actor (institutional/constrained) — maintain lecture-based attention-dependent curricula as performative ritual; degraded from core function but persist through inertia; constrained by student attention fragmentation
 *   - Digital Wellness Movement: Organized agent (organized/constrained) — building alternative pathways (focus protocols, device-free time blocks, school phone bans); represent scaffold with sunset clause as adoption increases
 *   - Neuroscientists and Cognitive Science: Analytical observer (analytical/analytical) — characterize cognitive energy budget as neurobiological law; risk naturalizing contingent institutional extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cognitive_energy_budget, 0.58).
domain_priors:suppression_score(cognitive_energy_budget, 0.65).
domain_priors:theater_ratio(cognitive_energy_budget, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cognitive_energy_budget, extractiveness, 0.58).
narrative_ontology:constraint_metric(cognitive_energy_budget, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(cognitive_energy_budget, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cognitive_energy_budget, snare).
narrative_ontology:human_readable(cognitive_energy_budget, "The Attention Exhaustion Trap").
narrative_ontology:topic_domain(cognitive_energy_budget, "cognitive/technological/social").

domain_priors:requires_active_enforcement(cognitive_energy_budget).
% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cognitive_energy_budget, attention_extraction_platforms).
narrative_ontology:constraint_beneficiary(cognitive_energy_budget, advertisers).
narrative_ontology:constraint_beneficiary(cognitive_energy_budget, content_creators).
narrative_ontology:constraint_victim(cognitive_energy_budget, knowledge_workers).
narrative_ontology:constraint_victim(cognitive_energy_budget, students).
narrative_ontology:constraint_victim(cognitive_energy_budget, sustained_attention_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXHAUSTED KNOWLEDGE WORKER (SNARE) — Individual worker with finite daily cognitive budget cannot exit the extraction. Employment demands synchronous responsiveness (email, messaging, meetings) that fragment attention. Attention platforms (social media, news feeds, streaming) layer additional extraction on top. Worker is trapped between employment extraction and leisure-time extraction. Zero degrees of freedom once employment is secured — even offline hours are colonized by asynchronous notifications and FOMO mechanisms. Maximum experienced extraction.
constraint_indexing:constraint_classification(cognitive_energy_budget, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: STUDENT UNDER COGNITIVE LOAD (SNARE) — Academic and social demands fragment attention across courses, assignments, social media, and part-time work. Cognitive energy directed toward sustained learning competes with engagement platform incentives. Student cannot exit education to preserve cognitive energy (career requirement); cannot opt out of attention platforms (social cohesion requirement). Trapped in both dimensions. High suppression: no institutional support for attention recovery; productivity culture frames rest as laziness.
constraint_indexing:constraint_classification(cognitive_energy_budget, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: ATTENTION EXTRACTION PLATFORM (ROPE) — Platform sees attention allocation as coordination problem: users want to share content, discover information, maintain social bonds. Platform solves coordination (connecting peers, enabling broadcast, archiving conversations) while extracting attention residual (user engagement time, behavioral data, attention data for advertising models). Platform experiences this as pure coordination with extraction as byproduct of solving user problems. Low suppression from platform perspective — users benefit from network effects and information access; extraction feels optional. Arbitrage exit available: platform can exit user relationship by deprioritizing user (algorithmic suppression, shadowban).
constraint_indexing:constraint_classification(cognitive_energy_budget, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ADVERTISER AND DATA BROKER (TANGLED ROPE) — Benefits from attention extraction infrastructure (access to attentional data, behavioral targeting capacity). Also constrained by platform algorithm changes, competitive bidding for attention slots, and regulatory restrictions on data use. Experiences both benefits (coordination of ad delivery, efficiency of targeting) and extraction (paying premium for shrinking ad impression effectiveness, losing data access in privacy regulations). Active enforcement required: constant investment in new targeting methods, audience building, compliance overhead. Moderate extraction with genuine coordination benefits.
constraint_indexing:constraint_classification(cognitive_energy_budget, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: EDUCATIONAL INSTITUTION (PITON) — Schools and universities claim to develop sustained attention and deep learning while operating within attention economy constraints. Traditional lecture-based education (theater_ratio ~0.60) has been degraded by student attention fragmentation and competition from online media. Institutions maintain attention-based curricula through inertia: exam-based assessment, lecture attendance policies, assigned reading lists. But the functional capacity for sustained attention in student populations has declined. Institutions see their own mechanisms as increasingly performative (students physically present but cognitively elsewhere), yet persist with similar formats. Sunset clause absent — no clear path to alternative pedagogical structure.
constraint_indexing:constraint_classification(cognitive_energy_budget, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: DIGITAL WELLNESS MOVEMENT (SCAFFOLD) — Organized groups (mindfulness programs, digital minimalism advocates, school-based attention recovery curricula, workplace focus policies) see cognitive energy budget management as a temporary coordination failure with emerging exit pathways. Sunset clause: as attention recovery practices diffuse (focus blocks, notification management, notification silence protocols, school phone bans), the extraction mechanism's effectiveness declines. Workers and students who implement recovery protocols experience lower effective extraction. Movement is constrained by economic incentives (platforms benefit from high engagement) but has genuine agency and visible momentum. Theater_ratio moderate because genuine functional value (sustained attention recovery) is available.
constraint_indexing:constraint_classification(cognitive_energy_budget, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NEUROSCIENCE FRAME (MOUNTAIN) — From a neurobiological perspective, cognitive energy depletion is an immutable property of how human attention works: executive function draws on a finite pool of metabolic resources; context-switching has irreducible switching costs; sustained focus has upper time boundaries (ultradian rhythms). No organizational or technological change can eliminate these constraints — only respect them. This perspective sees the cognitive energy budget as a natural law of neurobiology. However, the structural data contradicts the mountain classification: the base extractiveness value (0.58) reflects institutional arrangements (email responsiveness norms, engagement metrics optimization, always-on culture) that are contingent and changeable, not neurobiological constants. The engine's false summit detector will identify this as naturalization of a contingent institutional arrangement.
constraint_indexing:constraint_classification(cognitive_energy_budget, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cognitive_energy_budget_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cognitive_energy_budget, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cognitive_energy_budget, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cognitive_energy_budget, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(cognitive_energy_budget, TR),
    TR >= 0.70.

:- end_tests(cognitive_energy_budget_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts attention and cognitive energy from trapped agents across multiple channels simultaneously (employment synchronous demands, platform engagement, social media FOMO, streaming autoplay). The extractiveness is not at maximum (0.70+) because some agents retain partial agency (digital wellness practices, some employers respect focus time, some individuals successfully implement offline boundaries), and because recovery is possible if extraction is reduced. The increase from 0.32 to 0.58 over the interval reflects the accumulation of extraction mechanisms: email became standard (2000s), smartphones became ubiquitous (2010s), algorithmic feeds optimized for engagement (2015+), and remote work normalized synchronous responsiveness (2020+). Suppression (0.65): High. Substantial barriers to escaping extraction include: employment norms that treat offline hours as shirking, educational systems that assume constant availability, social media platforms with algorithmic suppression of users who reduce engagement (shadow-banning, feed deprioritization), and cultural pressure that treats always-on responsiveness as professional competence. Suppression via platform algorithm is particularly severe: users cannot fully exit attention platforms without losing social connection and information access. Theater_ratio (0.48): Moderate, rising over interval. Institutional responses to attention crisis (wellness workshops, mindfulness programs, focus-time applications, notification management tools) are partly theatrical—they are implemented alongside systems that undermine them (email escalation norms, slack-as-always-on, productivity metrics based on daily activity). However, some genuine functional value exists: focus blocks and offline time do recover cognitive capacity when implemented consistently. The rising theater ratio reflects increased performative interventions without fundamental system change.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates radical perspectival divergence. The trapped knowledge worker sees pure snare: extraction across employment and leisure with no exit. The student sees snare: academic and social demands with no institutional support for cognitive recovery. The platform sees rope: solving genuine coordination problems (content discovery, social connection) while extracting attention as secondary effect. The advertiser sees tangled rope: benefits from targeting infrastructure but constrained by platform algorithm changes and regulatory pressure. The educational institution sees piton: its traditional attention-based pedagogy has become degraded by student cognitive fragmentation but persists through inertia. The digital wellness movement sees scaffold: a temporary coordination failure being solved through behavior change and protocol adoption, with visible sunset clause as practices diffuse. The neuroscientist sees mountain: cognitive energy budget is an immutable property of human neurobiology. The perspectival gap is maximal between powerless agents (snare) and institutional/organized agents (rope, tangled rope, scaffold). The gap reflects genuine structural differences in exit options and extraction direction—not merely different interpretations of identical facts.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality d is derived from agent power, exit options, and beneficiary/victim status. Trapped powerless agents (knowledge workers, students) occupy victim positions with no arbitrage or mobile exits → d ≈ 0.95 → f(d) ≈ 1.42 (maximum experienced extractiveness). Institutional beneficiaries (platforms) occupy beneficiary positions with arbitrage exits → d ≈ 0.05-0.15 → f(d) ≈ -0.12 to -0.01 (negative or near-zero effective extraction from their perspective). Powerful advertisers occupy beneficiary positions with arbitrage exits but face constraints from platform dependence → d ≈ 0.35 → f(d) ≈ 0.35 (moderate experienced extraction). Organized wellness agents occupy constrained positions with exit-seeking behavior → d ≈ 0.50-0.55 → f(d) ≈ 0.65-0.75 (moderate effective extraction, decreasing over interval as protocols mature). The charter's scope is global (σ=1.2), which amplifies effective extractiveness via the scope modifier—the attention extraction mechanism operates across all time zones and cultures. Effective extraction χ = ε × f(d) × σ(S) ranges from 0.1 (platforms, d≈0.05, f≈-0.12, σ=1.2 → χ≈-0.007) to 0.98 (trapped workers, d≈0.95, f≈1.42, σ=1.2 → χ≈0.98).
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION MECHANISM: The constraint's extractiveness (0.58) exceeds the snare threshold (0.46), triggering mandatrophy analysis. The resolution recognizes that 'the attention exhaustion trap' conflates two distinct structural claims with different ε values. CLAIM A (Mountain): Humans have finite daily cognitive energy, depleting through ultradian cycles. This is a neurobiological constraint with ε ≈ 0.08 (empirically robust for 50+ years across all human populations). Classification: Mountain. No beneficiaries or victims — the budget is a limit on all. CLAIM B (Snare): Modern institutional arrangements competitively extract cognitive energy through employment responsiveness norms, platform engagement optimization, and notification systems, leaving workers cognitively depleted. This is a contingent institutional arrangement with ε ≈ 0.58 (recent emergence; varies by sector and geography; reversible through norm change). Classification: Snare (powerless agents) / Rope (beneficiary platforms) / Scaffold (organized wellness movements). Clear beneficiaries (platforms, employers) and victims (workers, students). The mandatrophy is resolved by decomposing: Claim A (neurobiological) is a legitimate mountain underlying the structural analysis. Claim B (institutional extraction) is a genuine snare/tangled rope/scaffold depending on perspective. They interact: the mountain creates vulnerability that the snare mechanism exploits, but the snare is not inevitable from the mountain alone. Institutional change can reduce extraction without violating the neurobiology. The high mandatrophy_resolved flag (true) indicates that the system has disambiguated the conflated claims and assigned each to appropriate types. The interaction is documented in network.affects_constraints to show that cognitive_energy_budget (the snare, extractive claim) is downstream of and enabled by neuroscience facts about attention fatigue.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cognitive_reserve_heterogeneity,
    'Does cognitive energy budget vary substantially across individuals (age, neurotype, training, genetic factors), or is the budget a population-universal constraint?',
    'Longitudinal measurement of sustained attention capacity, cognitive fatigue onset timing, and recovery rates across diverse populations; comparison of individual differences in ultradian rhythm amplitude and context-switching costs',
    'If highly heterogeneous: constraint classification should vary by population subgroup (some agents not trapped). If universal: mountain classification more defensible. Current analysis assumes moderate heterogeneity with common lower bound.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cognitive_reserve_heterogeneity, empirical, 'Degree of individual variation in cognitive energy budget').

omega_variable(
    extraction_mechanism_intentionality,
    'Is the attention extraction by platforms and employers a deliberate mechanism or an emergent byproduct of incentive structures?',
    'Analysis of platform design choices (notification defaults, autoplay, algorithmic feed optimization, infinite scroll); comparison of intentional engagement optimization vs structural incentives (ad revenue models, user growth metrics); examination of internal platform documents and design decisions',
    'If deliberate: snare classification confirmed across all perspectives. If emergent incentive byproduct: tangled rope classification more appropriate (platforms also constrained by business model competition). Changes mandate classification for platforms and advertisers.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_mechanism_intentionality, conceptual, 'Whether attention extraction is intentional design or emergent from incentives').

omega_variable(
    attention_recovery_protocol_efficacy,
    'Do focus-time protocols (notification silence, pomodoro intervals, attention-blind work blocks, internet blocking) actually restore cognitive energy capacity or merely redistribute exhaustion across the daily timeline?',
    'Randomized trials comparing attention recovery protocols vs control; measurement of sustained focus duration, decision quality, and cognitive fatigue across intervention conditions; longitudinal tracking of knowledge workers implementing protocols',
    'If protocols effective: scaffold perspective is real (sunset through behavior change). If ineffective: mountain perspective gains credibility (exhaustion is inevitable). If partially effective: tangled rope dominates (protocols constrained by external extraction demands).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(attention_recovery_protocol_efficacy, empirical, 'Whether attention recovery protocols restore cognitive capacity').

omega_variable(
    always_on_work_necessity,
    'Is synchronous email and messaging responsiveness an actual job requirement or a norm enforced through informal social pressure?',
    'Organizational analysis comparing explicit role requirements vs informal expectations; measurement of consequences (career impact, review ratings) for workers with strict offline boundaries vs always-on workers; comparison across industries and cultures',
    'If genuine requirement: extraction is locked in place (snare persists). If norm-based: high-suppression exit exists (some workers can successfully establish boundaries). Changes experienced extraction d values substantially.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(always_on_work_necessity, empirical, 'Whether always-on responsiveness is requirement or norm').

omega_variable(
    neuroplasticity_attention_training,
    'Can sustained attention capacity be increased through training (meditation, focus practice, cognitive enrichment), or is the ultradian rhythm budget truly fixed?',
    'Longitudinal meditation and focus training studies measuring sustained attention duration, switching costs, and cognitive fatigue thresholds before/after training; comparison with neuroimaging markers of prefrontal capacity',
    'If trainable: cognitive budget is less of a universal mountain and more of a shaped resource (rope or scaffold). If fixed: mountain classification strengthened. Current analysis assumes modest trainability (15-20% capacity increase ceiling).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(neuroplasticity_attention_training, empirical, 'Whether cognitive attention capacity can be trained to increase').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cognitive_energy_budget, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cogn_tr_t0, cognitive_energy_budget, theater_ratio, 0, 0.25).
narrative_ontology:measurement(cogn_tr_t5, cognitive_energy_budget, theater_ratio, 5, 0.38).
narrative_ontology:measurement(cogn_tr_t10, cognitive_energy_budget, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(cogn_be_t0, cognitive_energy_budget, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(cogn_be_t5, cognitive_energy_budget, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(cogn_be_t10, cognitive_energy_budget, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cognitive_energy_budget, resource_allocation).
narrative_ontology:affects_constraint(cognitive_energy_budget, sleep_deprivation_cycle).
narrative_ontology:affects_constraint(cognitive_energy_budget, burnout_accumulation).
narrative_ontology:affects_constraint(cognitive_energy_budget, learning_capacity_decline).

% DUAL FORMULATION NOTE:
% The attention exhaustion trap decomposes into a neurobiological mountain (finite cognitive energy, universal constraint ε≈0.08) and an institutional snare (competitive extraction of that finite resource through platform engagement and employment responsiveness norms, ε≈0.58). The snare is downstream of the mountain in that it exploits the neurobiological vulnerability, but the snare is not inevitable from the mountain. The network reflects institutional dependents: sleep deprivation cycles deepen extraction; burnout accumulation reflects long-term snare exposure; learning capacity decline is the terminal damage mode of chronic cognitive extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cognitive_energy_budget, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
