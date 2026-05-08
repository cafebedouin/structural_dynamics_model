% ============================================================================
% CONSTRAINT STORY: locus_of_harm_prevention
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_locus_of_harm_prevention, []).

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
 *   constraint_id: locus_of_harm_prevention
 *   human_readable: Locus of Harm Prevention in Platform Governance
 *   domain: platform_governance/content_moderation/community_norms
 *
 * SUMMARY:
 *   The locus of harm prevention in platform governance addresses a
 *   fundamental structural question: who bears responsibility for preventing
 *   harmful content exposure — the consumer (through filtering and avoidance)
 *   or the producer (through self-censorship and removal)? This constraint is
 *   presented as a natural law of online community management: some mechanism
 *   must exist to prevent harm, and the only question is which mechanism.
 *   However, the presence of identifiable beneficiaries (platform operators
 *   who gain liability shields and advertiser safety, regulatory bodies who
 *   gain enforcement leverage) triggers false summit evaluation. The
 *   constraint exhibits mountain characteristics from all perspectives (low
 *   extractiveness, low suppression, high accessibility collapse, natural
 *   emergence), but the structural data reveals that specific institutional
 *   actors benefit from the framing of harm prevention as a non-negotiable
 *   necessity requiring centralized enforcement. The epsilon value (0.08)
 *   reflects minimal extraction in the base case — the genuine coordination
 *   cost of preventing severe harms (CSAM, credible threats, spam) — but the
 *   beneficiary declarations flag that this 'natural law' may naturalize a
 *   contingent institutional arrangement.
 *
 * KEY AGENTS:
 *   - Content Consumers: Powerless/trapped — experience harm prevention as immutable feature of online spaces; no exit from the need for some mechanism
 *   - Content Creators: Moderate/constrained — face structural reality that some content causes harm; choice is mechanism, not escape from constraint
 *   - Platform Operators: Institutional/arbitrage — PRIMARY BENEFICIARY: gain liability shields (Section 230, DSA safe harbor), advertiser safety guarantees, and justification for moderation infrastructure investment
 *   - Advertisers: Institutional/arbitrage — SECONDARY BENEFICIARY: benefit from brand safety guarantees that producer-side removal provides; consumer-side filtering alone would not protect ad placement
 *   - Regulatory Bodies: Organized/mobile — TERTIARY BENEFICIARY: gain enforcement leverage through platform liability; producer-side removal creates a compliance chokepoint that consumer-side filtering would not provide
 *   - Analytical Observer: Analytical/analytical — risks naturalizing contingent institutional arrangements (centralized moderation, platform liability, advertiser safety requirements) as inherent features of online communication
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(locus_of_harm_prevention, 0.08).
domain_priors:suppression_score(locus_of_harm_prevention, 0.03).
domain_priors:theater_ratio(locus_of_harm_prevention, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(locus_of_harm_prevention, extractiveness, 0.08).
narrative_ontology:constraint_metric(locus_of_harm_prevention, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(locus_of_harm_prevention, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(locus_of_harm_prevention, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(locus_of_harm_prevention, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(locus_of_harm_prevention, mountain).
narrative_ontology:human_readable(locus_of_harm_prevention, "Locus of Harm Prevention in Platform Governance").
narrative_ontology:topic_domain(locus_of_harm_prevention, "platform_governance/content_moderation/community_norms").

domain_priors:emerges_naturally(locus_of_harm_prevention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(locus_of_harm_prevention, platform_operators).
narrative_ontology:constraint_beneficiary(locus_of_harm_prevention, advertisers).
narrative_ontology:constraint_beneficiary(locus_of_harm_prevention, regulatory_bodies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONTENT CONSUMER (MOUNTAIN) — Experiences the structural necessity of harm prevention as immutable. Whether harm is prevented by filtering (consumer responsibility) or removal (producer responsibility), the consumer perceives the constraint as a natural feature of online spaces. No exit from the need for some harm prevention mechanism.
constraint_indexing:constraint_classification(locus_of_harm_prevention, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CONTENT CREATOR (MOUNTAIN) — Faces the structural reality that some content will cause harm to some audiences. The choice between self-censorship and post-hoc removal is a choice of mechanism, not a choice to escape the underlying constraint. High exit costs (platform switching, audience loss) but the constraint itself appears immutable.
constraint_indexing:constraint_classification(locus_of_harm_prevention, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PLATFORM OPERATOR (MOUNTAIN) — Benefits from the structural necessity of harm prevention (justifies moderation infrastructure, regulatory compliance, advertiser safety). Experiences the constraint as a natural law of online community management: some mechanism must exist to prevent harm, whether consumer-side filtering or producer-side removal. The platform's choice of mechanism is strategic, but the necessity of choosing is perceived as immutable.
constraint_indexing:constraint_classification(locus_of_harm_prevention, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY COALITION (MOUNTAIN) — Organized actors (EU Digital Services Act, Section 230 reform advocates, child safety organizations) see harm prevention as a non-negotiable requirement. The debate is over mechanism (who bears responsibility), not over whether harm prevention is necessary. The constraint appears as a natural law of digital governance.
constraint_indexing:constraint_classification(locus_of_harm_prevention, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational perspective, the necessity of harm prevention in communication systems is a structural feature of human interaction at scale. Every communication medium in history has developed some mechanism to prevent harm (libel law, editorial review, community moderation). The locus of responsibility (consumer vs producer) is a policy variable, but the existence of the constraint is a natural law of social coordination. However, the presence of identifiable beneficiaries (platform operators, advertisers, regulatory bodies) triggers FSM evaluation — is this genuinely a natural law, or a contingent institutional arrangement that benefits specific actors?
constraint_indexing:constraint_classification(locus_of_harm_prevention, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(locus_of_harm_prevention_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(locus_of_harm_prevention, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(locus_of_harm_prevention, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(locus_of_harm_prevention, ExtMetricName, E),
    domain_priors:suppression_score(locus_of_harm_prevention, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(locus_of_harm_prevention),
    narrative_ontology:constraint_metric(locus_of_harm_prevention, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(locus_of_harm_prevention, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(locus_of_harm_prevention_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Very low. The base extraction reflects the genuine coordination cost of preventing severe, universally-recognized harms (CSAM, credible violence threats, large-scale spam). Most agents agree these harms require prevention, and the cost of prevention mechanisms (whether filtering or removal) is relatively low. The value is above zero because there is some asymmetry: platform operators and advertisers capture benefits (liability protection, brand safety) that consumers and creators do not receive, but the asymmetry is small in the base case. The omega variables address whether this low extraction is stable or whether it masks a larger constructed constraint. Suppression (0.03): Very low. Minimal coercion in the base case — agents have high degrees of freedom to choose platforms with different harm prevention models, and the constraint does not rely on suppressing alternatives. However, network effects and regulatory pressure may increase suppression over time (not yet reflected in base metrics). Theater ratio (0.15): Low. Harm prevention mechanisms (both filtering and removal) are largely functional rather than performative in the base case. Some theater exists (over-broad removal to satisfy advertisers, performative content warnings), but the core function (preventing exposure to severe harms) is real. Accessibility collapse (0.92): Very high. The necessity of some harm prevention mechanism is nearly universal across all communication systems — agents cannot easily imagine a large-scale online platform with zero harm prevention. Resistance (0.08): Very low. Minimal resistance to the claim that harm prevention is necessary; debate focuses on mechanism (locus of responsibility), not on whether prevention is required. Emerges naturally (true): Harm prevention mechanisms emerge in every online community without top-down imposition — even anarchic spaces like early Usenet developed killfiles and moderation norms.
 *
 * PERSPECTIVAL GAP:
 *   All five perspectives classify as mountain, which is unusual and diagnostic. The uniformity reflects that the constraint is presented as a natural law — harm prevention is necessary, and the only question is mechanism. However, the presence of beneficiaries (platform operators, advertisers, regulatory bodies) creates a structural gap that the perspectival classifications do not capture. The gap is not between perspectives (all see mountain) but between the claimed natural law status and the beneficiary structure. This is the signature of a false summit: a constraint that appears immutable from all perspectives but benefits identifiable actors. The FSM detector will flag this for reclassification. If the omega variables resolve toward 'institutional arrangement' rather than 'natural law', the analytical perspective should reclassify to tangled_rope: the constraint coordinates genuine harm prevention (low base extraction) but also extracts asymmetrically by concentrating liability protection and enforcement leverage at the platform/regulatory level while imposing costs (chilling effects, reduced autonomy) on consumers and creators.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform operators are declared as beneficiaries because they gain liability protection (Section 230 safe harbor, DSA exemptions) and advertiser safety guarantees from the existence of harm prevention infrastructure. The structural relationship is: the constraint (necessity of harm prevention) justifies the platform's moderation apparatus, which in turn shields the platform from legal liability and enables advertiser revenue. Advertisers are beneficiaries because producer-side removal provides brand safety guarantees that consumer-side filtering alone would not — ads are not placed next to harmful content because harmful content is removed, not merely hidden from sensitive users. Regulatory bodies are beneficiaries because platform liability creates a compliance chokepoint — if harm prevention were purely consumer-side (user-controlled filtering), regulators would have no enforcement leverage over platforms. The producer-side removal model concentrates responsibility at the platform level, which regulators can target. No victims are declared in the base case because the constraint, as formulated, does not extract from any agent beyond the minimal coordination cost. However, the omega variables address whether this framing is accurate — if harm prevention 'necessity' is a constructed constraint rather than a natural law, then both consumers (reduced autonomy, paternalistic filtering) and creators (chilling effects, arbitrary removal) are victims of a constraint that benefits platforms and regulators. The FSM evaluation will determine whether the mountain classification holds or whether the constraint reclassifies to tangled_rope when the beneficiary structure is accounted for.
 *
 * MANDATROPHY ANALYSIS:
 *   FALSE SUMMIT CANDIDATE: This constraint resolves the mandatrophy by demonstrating that a mountain classification can coexist with identifiable beneficiaries if the beneficiaries gain from the constraint's existence rather than from its extractive function. The platform operators, advertisers, and regulatory bodies benefit from the framing of harm prevention as a non-negotiable necessity, but they do not benefit from the extraction itself (which is minimal — epsilon 0.08). The mandatrophy question is: does the presence of beneficiaries disqualify the mountain classification, or does it reveal that the mountain is a false summit (a constructed constraint naturalized as a law)? The FSM evaluation answers this: if the constraint is genuinely a natural law (harm prevention is structurally necessary regardless of institutional arrangements), the beneficiaries are incidental — they benefit from complying with a real constraint, not from constructing a fake one. If the constraint is an institutional arrangement (harm prevention could be achieved through consumer-side filtering, but platforms choose producer-side removal to gain liability shields), the beneficiaries are constitutive — the constraint exists because it benefits them, and the mountain classification is a naturalization of their interests. The omega variables provide the resolution mechanism: empirical testing of consumer-side filtering sufficiency, historical analysis of pre-platform harm prevention models, and cross-jurisdictional comparison of harm definitions. Until resolved, the constraint is classified as mountain with FSM flag.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_institutional_arrangement,
    'Is the necessity of harm prevention a genuine natural law of communication systems, or a contingent institutional arrangement that benefits platform operators and regulatory bodies?',
    'Historical analysis of pre-platform communication systems (Usenet, IRC, early forums) that operated with minimal harm prevention infrastructure. Comparison of harm rates and community sustainability across different governance models. Examination of whether platforms with user-controlled filtering (no removal) exhibit higher harm rates than platforms with producer-side removal.',
    'If natural law: mountain classification confirmed across all perspectives. If institutional arrangement: reclassify to tangled_rope from analytical perspective — the ''necessity'' of centralized harm prevention is a constructed constraint that benefits platform operators (liability shield, advertiser safety) and regulatory bodies (enforcement leverage) while extracting from both consumers (reduced autonomy) and producers (chilling effects).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_institutional_arrangement, empirical, 'Whether harm prevention necessity is natural law or constructed constraint').

omega_variable(
    harm_definition_stability,
    'Is the definition of ''harm'' stable enough across contexts to support a universal constraint, or does definitional variance reveal that the constraint is observer-dependent?',
    'Cross-cultural and cross-temporal analysis of what content is classified as harmful. Measurement of definitional drift in platform TOS over time. Comparison of harm definitions across jurisdictions (EU vs US vs China).',
    'If stable: supports mountain classification — harm prevention is a universal coordination problem. If unstable: the constraint decomposes into multiple jurisdiction-specific or culture-specific constraints with different epsilon values, violating the epsilon-invariance principle for a single ''locus of harm prevention'' story.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(harm_definition_stability, conceptual, 'Whether harm definition is stable enough for universal constraint').

omega_variable(
    filtering_technology_sufficiency,
    'Are consumer-side filtering technologies (content warnings, keyword filters, algorithmic personalization) sufficient to prevent harm without producer-side removal, or is removal structurally necessary?',
    'Empirical testing of platforms that rely exclusively on user-controlled filtering (Mastodon instances with no removal policy, early Twitter with mute/block only). Measurement of harm exposure rates and user satisfaction. Analysis of whether filtering can scale to handle adversarial content (spam, harassment, CSAM).',
    'If sufficient: the locus of harm prevention is a policy choice (tangled_rope), not a natural law — platforms choose removal to benefit advertisers and reduce liability, not because filtering is structurally inadequate. If insufficient: mountain classification confirmed — some producer-side responsibility is structurally necessary, and the only question is degree.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(filtering_technology_sufficiency, empirical, 'Whether consumer-side filtering is structurally sufficient').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(locus_of_harm_prevention, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(locus_harm_tr_t0, locus_of_harm_prevention, theater_ratio, 0, 0.1).
narrative_ontology:measurement(locus_harm_tr_t5, locus_of_harm_prevention, theater_ratio, 5, 0.12).
narrative_ontology:measurement(locus_harm_tr_t10, locus_of_harm_prevention, theater_ratio, 10, 0.15).

% Extraction over time
narrative_ontology:measurement(locus_harm_be_t0, locus_of_harm_prevention, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(locus_harm_be_t5, locus_of_harm_prevention, base_extractiveness, 5, 0.07).
narrative_ontology:measurement(locus_harm_be_t10, locus_of_harm_prevention, base_extractiveness, 10, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(locus_of_harm_prevention, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is formulated as a single structural question (locus of harm prevention responsibility) rather than decomposed by mechanism (filtering vs removal) because the epsilon value is stable across both mechanisms in the base case — both involve minimal extraction (0.08) when limited to severe, universally-recognized harms. If the scope expands to contested harms (misinformation, offensive speech, political content), the epsilon values diverge and decomposition is required: consumer-side filtering for contested content has epsilon ~0.05 (pure coordination), while producer-side removal for contested content has epsilon ~0.35 (tangled rope — coordinates community norms but extracts through chilling effects and arbitrary enforcement). Future work: decompose into locus_of_harm_prevention_severe (this story) and locus_of_harm_prevention_contested (new story, higher epsilon, tangled_rope classification).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
