% ============================================================================
% CONSTRAINT STORY: social_media_addiction_loop
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_social_media_addiction_loop, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: social_media_addiction_loop
 *   human_readable: Social Media Addiction Loop
 *   domain: digital_psychology/behavioral_extraction
 *
 * SUMMARY:
 *   The social media addiction loop represents a hybrid constraint combining
 *   intermittent reinforcement (variable reward scheduling), identity fusion
 *   (self-concept constituted through platform presence), and structural
 *   suppression (algorithmic design that concentrates attention extraction).
 *   Users experience this constraint through identity-lock rather than
 *   material barriers — they are structurally mobile (could stop) but
 *   psychologically bound through internalized habit loops, social identity
 *   fusion, and fear of exclusion (FOMO). The constraint exhibits signature
 *   Snare characteristics: high extractiveness (0.62), high suppression
 *   (0.68), minimal coordination benefit for the targeted user, and reliance
 *   on suppressing exit awareness. The theater ratio (0.55) reflects that
 *   platform self-regulation (well-being features, screen-time tools, content
 *   moderation) is partially performative — addressing symptoms while
 *   preserving the core engagement-optimization mechanism. The constraint
 *   family includes distinct stories at different timescales: immediate
 *   addiction cycles (intermittent reinforcement), biographical identity
 *   development (identity-lock formation), and generational regulatory
 *   intervention (scaffold sunset). The extractiveness trajectory shows
 *   accumulation: early-stage adoption (0.35) -> habit formation with
 *   increased algorithmic optimization (0.48) -> mature addiction with full
 *   identity integration (0.62). Theater ratio growth indicates increasing
 *   performativity: initial genuine features (chronological feed curation)
 *   give way to well-being theater (screen-time warnings,
 *   distraction-reduction tools) that operate at the margin of the core
 *   mechanism.
 *
 * KEY AGENTS:
 *   - Individual Users: Primary victims (powerless/identity_locked) — structurally mobile but identity-locked through habit loops, social status fusion, and internalized suppression
 *   - Platform Operators: Primary beneficiaries (institutional/arbitrage) — capture user attention, monetize through advertising, enjoy full flexibility in business model and implementation
 *   - Advertising Networks: Secondary beneficiaries (institutional/arbitrage) — benefit from concentrated user attention and behavioral data harvesting
 *   - Content Creators: Secondary victims (moderate/constrained) — depend on platform for audience but constrained by algorithmic suppression and optimization demands
 *   - Regulatory Coalitions: Organized agents (organized/constrained) — digital rights organizations, child protection advocates, governmental bodies working to establish sunset frameworks
 *   - Platform Self-Regulation Functions: Institutional actors (institutional/arbitrage) — community standards, content moderation, well-being features operating as performative theater
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(social_media_addiction_loop, 0.62).
domain_priors:suppression_score(social_media_addiction_loop, 0.68).
domain_priors:theater_ratio(social_media_addiction_loop, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(social_media_addiction_loop, extractiveness, 0.62).
narrative_ontology:constraint_metric(social_media_addiction_loop, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(social_media_addiction_loop, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(social_media_addiction_loop, snare).
narrative_ontology:human_readable(social_media_addiction_loop, "Social Media Addiction Loop").
narrative_ontology:topic_domain(social_media_addiction_loop, "digital_psychology/behavioral_extraction").

domain_priors:requires_active_enforcement(social_media_addiction_loop).
% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(social_media_addiction_loop, platform_operators).
narrative_ontology:constraint_beneficiary(social_media_addiction_loop, advertising_networks).
narrative_ontology:constraint_victim(social_media_addiction_loop, user_attention).
narrative_ontology:constraint_victim(social_media_addiction_loop, user_autonomy).
narrative_ontology:constraint_victim(social_media_addiction_loop, user_time_allocation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE ADDICTED USER (SNARE) — Structurally mobile (could physically stop using) but identity-locked through internalized habit loops, social identity fusion, and cognitive capture. User's self-concept, social status, and daily rhythm are constituted through the platform. Exit is perceivable as 'dying' socially rather than as a rational cost-benefit decision. Suppression operates through both structural design (infinite scroll, notification scheduling) and internalized patterns (FOMO, dopamine habituation). Maximum experienced extraction — the user is the target of the attention economy.
constraint_indexing:constraint_classification(social_media_addiction_loop, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 2: THE CONTENT CREATOR (TANGLED ROPE) — Constrained by platform dependency for audience and income. Genuine coordination function exists (platform enables creator-audience connection) alongside asymmetric extraction (algorithm suppresses reach unless engagement metrics climb). Moderate extraction — creators benefit from audience access but pay through constant content optimization and algorithmic uncertainty.
constraint_indexing:constraint_classification(social_media_addiction_loop, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: PLATFORM OPERATORS (ROPE) — Experiences the constraint as pure coordination of social connection. Manages supply (creators) and demand (users) through algorithmic mediation. High flexibility and arbitrage options — can shift business models, jurisdictions, or technical implementations. Extraction flows toward this agent, but operators frame it as solving coordination problems: matching creators with audiences, reducing information overload through curation. Net beneficiary position.
constraint_indexing:constraint_classification(social_media_addiction_loop, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY COALITIONS (SCAFFOLD) — Digital rights organizations, child protection advocates, and digital-wellness coalitions see the addiction loop as a temporary problem with regulatory sunset. Digital services acts, attention economy taxation, algorithmic transparency mandates, and duty-of-care frameworks represent structured exit pathways with declining suppression. Organized agents (governments, advocacy groups, parent organizations) perceive the constraint as having bounded duration — new regulatory norms are creating alternative coordination pathways.
constraint_indexing:constraint_classification(social_media_addiction_loop, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: PLATFORM SELF-REGULATION (PITON) — Community standards, content moderation policies, well-being initiatives, and screen-time controls are largely performative. Platforms introduce these features to manage regulatory pressure and public perception while maintaining core business logic (engagement-based optimization). Theater ratio (0.55) reflects that these interventions operate at the margin of the actual addiction mechanism. Real function has atrophied — self-regulation persists through institutional inertia and reputation management, not because it addresses the underlying attention extraction.
constraint_indexing:constraint_classification(social_media_addiction_loop, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER WITH IDENTITY LOCK (SNARE) — Even the analyst studying this constraint likely uses these platforms for professional reach, information distribution, and intellectual community. The analytical position is itself captured by the addiction mechanism — the framework cannot see structure that would require rejecting the platforms that distribute the framework itself. This perspective instantiates the oracle gap (Theorem 4): the analytical observer's native instruments cannot detect capture structures that the observer is embedded in. The binding mechanism is cognitive — the analyst's professional identity and epistemic community are constituted through the platforms being analyzed.
constraint_indexing:constraint_classification(social_media_addiction_loop, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(identity_locked),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(social_media_addiction_loop_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(social_media_addiction_loop, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(social_media_addiction_loop, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(social_media_addiction_loop, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(social_media_addiction_loop, TR),
    TR >= 0.70.

:- end_tests(social_media_addiction_loop_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): High and accumulating. The constraint extracts user attention (the scarce resource in attention economy), time allocation, behavioral data, and cognitive capacity. The extraction increases over the user's tenure as algorithmic optimization refines targeting and as identity fusion deepens. This is not maximal (0.72+) because users do receive genuine value — social connection, information access, entertainment — though this value is increasingly overwhelmed by extraction as the user becomes further locked in. Suppression (0.68): High. Multiple suppression mechanisms operate: (a) structural design (infinite scroll, notification scheduling, algorithmic feed control prevent easy exit), (b) social suppression (FOMO, fear of missing community, social status tied to platform presence), (c) internalized suppression (identity fusion makes exit psychologically unthinkable). Theater ratio (0.55): Moderate. Platform self-regulation features (screen-time controls, well-being settings, content moderation) are genuine interventions operating at the margin of the actual addiction mechanism. They address symptoms (reduce immediate screen time) but preserve the core engagement-optimization logic. Growth of theater over the interval reflects increasing performativity as platforms respond to regulatory pressure with well-being theater while maintaining extraction.
 *
 * PERSPECTIVAL GAP:
 *   The gap reveals that 'social media addiction' is not a single constraint but a presheaf over different observation sites. From the platform's position it is coordination (Rope). From the user's position it is extraction (Snare). From the regulatory view it is a temporary problem (Scaffold). From the performative theater view it is a degraded ritual (Piton). The analytical observer must avoid naturalizing the platform's frame as coordinate while also avoiding the reductionism of treating all platform design as pure extraction. The mandate is to map the presheaf: identify which agents genuinely experience coordination benefit and which experience pure extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are determined by structural position relative to extraction flow. Platform operators experience low d (0.05-0.15) — they are beneficiaries with arbitrage options; extraction flows toward them; f(d) returns negative or near-zero χ. Users experience high d (0.85-0.95) — they are victims with identity-lock preventing exit perception; f(d) returns high χ (1.15+). Content creators experience moderate d (0.50-0.65) — they both benefit (audience access) and bear costs (algorithmic optimization demands); f(d) returns moderate χ. Regulatory coalitions experience moderate d (0.45-0.55) with constrained exit reflecting they can influence but not unilaterally determine outcome. Scope modifier σ(S) amplifies χ at global scope (σ=1.2): the constraint operates across jurisdictions, making verification and regulatory intervention harder, thus amplifying effective extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: This constraint resolves the coordination-extraction ambiguity through perspectival decomposition. Is platform mediation 'just coordination' (beneficiary frame) or 'extraction with coordination cover' (victim frame)? The answer is both, measured from different structural positions. Platform operators genuinely experience coordination — algorithmic mediation does solve the legitimate problem of matching creators to audiences at scale. Users genuinely experience extraction — the addictive mechanism preys on attention scarcity and identity fusion. Neither frame is false; both are structural truths from their respective positions. The mandatrophy is resolved by showing that the classification presheaf is consistent: beneficiary position → Rope, victim position → Snare, organized agent with exit route → Scaffold, performative institutional position → Piton. No single type captures the full constraint; the presheaf captures it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_lock_vs_behavioral_addiction,
    'Is the user''s binding mechanism primarily identity fusion or behavioral conditionality?',
    'Longitudinal study of post-exit trajectory: if suppression persists after platform removal (FOMO, identity void, social anxiety), binding is identity-locked. If suppression drops rapidly, binding is behavioral-conditioned (trapped model).',
    'If identity-locked: users require identity reframing work post-exit; exit costs are internalized. If behavioral: exit is structurally possible with barrier removal; suppression is external. Classification shifts from identity-locked to trapped under this outcome.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_behavioral_addiction, empirical, 'Whether user binding is identity fusion or behavioral conditioning').

omega_variable(
    algorithmic_inevitability,
    'Is the addiction loop an inevitable consequence of algorithmic optimization for engagement or a contingent design choice?',
    'Comparative analysis of platforms with different ranking algorithms (algorithmic vs. chronological feed); A/B testing of engagement-neutral vs engagement-optimizing curation; theoretical analysis of whether engagement-maximization and well-being can be jointly optimized.',
    'If inevitable: suppression is structural (f=1.0) — the constraint cannot be decomposed without abandoning the platform model. If contingent: extractiveness can be reduced through design change; suppression is 0.4-0.6 rather than 0.8+.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithmic_inevitability, conceptual, 'Whether addiction is inherent to algorithmic engagement optimization').

omega_variable(
    regulatory_sunset_credibility,
    'Will regulatory frameworks (DSA, online safety bills, duty of care) actually constrain platform addiction mechanisms or will enforcement gap persist?',
    'Post-implementation analysis of regulatory compliance effectiveness; comparison of platforms in high-regulation vs. low-regulation jurisdictions; measurement of engagement metrics and attention-extraction rates before/after regulation.',
    'If effective: scaffold perspective confirmed — suppression will decline over 5-10 years. If ineffective: scaffold is aspirational; suppression persists at high levels. Classification downgrade from Scaffold to Piton or Snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_sunset_credibility, empirical, 'Whether regulatory frameworks will effectively constrain platform addiction mechanisms').

omega_variable(
    attention_scarcity_fundamental,
    'Is the scarcity of human attention a fundamental resource limit (Mountain) or an artifact of platform design choices that concentrate attention extraction?',
    'Historical comparison: attention allocation patterns in pre-digital and low-digital-penetration populations; analysis of whether attention scarcity is universal or specific to platforms with engagement-optimization algorithms.',
    'If fundamental: addiction loop is an immutable structural feature of any digital communication system. If artifact: addiction is contingent on design choices and regulatory regime. Reclassification from Snare toward Rope with regulatory intervention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(attention_scarcity_fundamental, conceptual, 'Whether attention scarcity is fundamental or design-artifact').

omega_variable(
    suppression_internalization_timeline,
    'What is the typical timeline for internalization of suppression — when does external design coercion become internalized cognitive pattern?',
    'Longitudinal cognitive psychology studies tracking habituation, neural adaptation to reward, and identity fusion rates across user cohorts; measurement of FOMO, anxiety, and restlessness as functions of platform use duration.',
    'If <6 months: young users rapidly identity-lock; suppression becomes internalized quickly. If >2 years: suppression primarily remains structural (external design). Timeline affects treatment models and intervention design — early intervention may prevent identity-lock; late intervention must address both structural and internalized suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_timeline, empirical, 'Timeline for suppression internalization from design to cognitive pattern').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(social_media_addiction_loop, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(smad_tr_t0, social_media_addiction_loop, theater_ratio, 0, 0.25).
narrative_ontology:measurement(smad_tr_t5, social_media_addiction_loop, theater_ratio, 5, 0.4).
narrative_ontology:measurement(smad_tr_t10, social_media_addiction_loop, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(smad_be_t0, social_media_addiction_loop, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(smad_be_t5, social_media_addiction_loop, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(smad_be_t10, social_media_addiction_loop, base_extractiveness, 10, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(social_media_addiction_loop, attachment_coordination).
narrative_ontology:boltzmann_floor_override(social_media_addiction_loop, 0.12).
narrative_ontology:affects_constraint(social_media_addiction_loop, algorithmic_engagement_optimization).
narrative_ontology:affects_constraint(social_media_addiction_loop, behavioral_data_extraction).
narrative_ontology:affects_constraint(social_media_addiction_loop, attention_economy_scarcity).

% DUAL FORMULATION NOTE:
% The addiction loop constrains three distinct problems with different ε values: (1) algorithmic_engagement_optimization (ε≈0.40, how platforms rank content) affects all users but allows some agency through platform-switching; (2) behavioral_data_extraction (ε≈0.55, how user behavior is harvested) affects all users with high suppression and identity-lock; (3) attention_economy_scarcity (ε≈0.35, finite human attention), the most upstream constraint, provides the structural justification for engagement optimization. Each story should be analyzed separately per the ε-invariance principle. This story (social_media_addiction_loop) focuses on the user-experienced extraction loop combining all three.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(social_media_addiction_loop, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
