% ============================================================================
% CONSTRAINT STORY: dark_patterns_manipulation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dark_patterns_manipulation, []).

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
 *   constraint_id: dark_patterns_manipulation
 *   human_readable: Dark Patterns (Interface Coercion)
 *   domain: technological/economic
 *
 * SUMMARY:
 *   Dark patterns represent a structural extraction mechanism in which
 *   platform operators exploit user cognitive biases and interface psychology
 *   to steer behavior toward outcomes that benefit the platform at user
 *   expense. The constraint operates across digital ecosystems — social
 *   media, e-commerce, attention-harvesting applications — wherever user
 *   engagement or behavioral data is monetizable. The extractiveness has
 *   increased over the measurement interval (0.35 → 0.58) as competitive
 *   pressure drives more aggressive interface design, regulatory awareness
 *   increases theater_ratio (performative compliance frameworks, design
 *   ethics statements), and the constraint becomes more central to platform
 *   business models. Dark patterns are not inevitable features of digital
 *   systems; they are deliberate architectural choices justified by
 *   behavioral economics research on cognitive biases (loss aversion, default
 *   effect, choice overload) and incentivized by the advertising-driven
 *   business model. The suppression is exceptionally high (0.72) because
 *   users lack technical capacity to escape interfaces designed by teams of
 *   UX engineers and behavioral psychologists, regulatory frameworks are
 *   territorially bound while platforms operate globally, and alternative
 *   platforms have network effects that lock users into extractive systems.
 *
 * KEY AGENTS:
 *   - End Users: Primary victims (powerless/trapped) — exploited via cognitive biases; no meaningful exit from global platform ecosystem
 *   - User Autonomy (Abstraction): Victim — treated as resource to extract; degraded by interface coercion
 *   - Platform Operators: Primary beneficiary (institutional/arbitrage) — capture attention, data, behavioral steering; experience constraint as coordination solution
 *   - Regulatory Bodies (GDPR, FTC, Regional): Organized actors (organized/constrained) — enforce standards but constrained by territorial jurisdiction and enforcement gaps
 *   - Researchers and Consumer Advocates: Moderate/moderate — document and campaign against dark patterns; operate in degraded institutional structures (piton perspective)
 *   - Open Standards and Alternative Platforms: Organized/mobile — provide exit pathways with sunset logic (scaffold perspective)
 *   - Analytical Observer: Civilizational perspective — sees pure extraction justified by behavioral economics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dark_patterns_manipulation, 0.58).
domain_priors:suppression_score(dark_patterns_manipulation, 0.72).
domain_priors:theater_ratio(dark_patterns_manipulation, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dark_patterns_manipulation, extractiveness, 0.58).
narrative_ontology:constraint_metric(dark_patterns_manipulation, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(dark_patterns_manipulation, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dark_patterns_manipulation, snare).
narrative_ontology:human_readable(dark_patterns_manipulation, "Dark Patterns (Interface Coercion)").
narrative_ontology:topic_domain(dark_patterns_manipulation, "technological/economic").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dark_patterns_manipulation, platform_operators).
narrative_ontology:constraint_victim(dark_patterns_manipulation, end_users).
narrative_ontology:constraint_victim(dark_patterns_manipulation, user_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: END USER (SNARE) — Trapped within the interface with no meaningful exit. Cognitive biases are exploited precisely because users cannot escape them through interface design alone. Users experience maximum extraction: their attention, data, and behavioral choices are harvested against their interests. No alternative platform offers genuine escape from dark patterns — the entire ecosystem has converged on coercive design.
constraint_indexing:constraint_classification(dark_patterns_manipulation, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: USER AUTONOMY ABSTRACTION (SNARE) — Treated as a resource to be extracted. Dark patterns systematically undermine informed consent and deliberative choice. Users experience extraction of agency itself — the constraint is not merely extractive but corrosive to the capacity for autonomous decision-making. Suppression is total: no technical or regulatory mechanism prevents cognitive hijacking through interface design.
constraint_indexing:constraint_classification(dark_patterns_manipulation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: PLATFORM OPERATORS (ROPE) — Experience the constraint as coordination: dark patterns are a solution to the collective action problem of user attention scarcity. All platforms face the same pressure to monetize engagement; dark patterns solve this problem by aligning user behavior with platform incentives. For operators, the constraint enables rather than constrains. Net beneficiary perspective — extraction runs toward this agent.
constraint_indexing:constraint_classification(dark_patterns_manipulation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY BODIES (TANGLED ROPE) — Possess organizational capacity to enforce standards but face structural constraints: regulatory authority is territorial (national/regional scope), while platforms operate globally. Regulation must balance user protection against innovation incentives and competitive disadvantage for domestic platforms. Mixed coordination-extraction: regulations like GDPR create genuine user-protective mechanisms (consent gates, transparency requirements) but also extract compliance costs that smaller platforms cannot absorb, creating de facto market consolidation. Requires active enforcement.
constraint_indexing:constraint_classification(dark_patterns_manipulation, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: RESEARCHERS AND CONSUMER ADVOCATES (PITON) — Possess technical and epistemic capacity to identify dark patterns but operate within degraded institutional structures. Academic research documents dark patterns; consumer advocacy organizations campaign against them; design ethics frameworks propose alternatives. Yet the constraint persists because the institutional mechanisms for enforcement lack power and urgency. Theater ratio is high: awareness campaigns, design guidelines, and ethics boards create performative resistance while the underlying incentive structure remains unchanged. The advocacy ecosystem maintains visibility without reducing extraction.
constraint_indexing:constraint_classification(dark_patterns_manipulation, piton,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 6: OPEN STANDARDS AND ALTERNATIVE PLATFORMS (SCAFFOLD) — Organized initiatives (ActivityPub, open web standards, ethical design frameworks) represent a temporary support structure for reducing dark pattern dependence. These alternatives have sunset logic: as interoperability standards mature and decentralized platforms accumulate user base, dark pattern monetization becomes less necessary — platforms competing on user trust and autonomy gain competitive advantage. Low effective extraction because organized agents have exit pathways and can migrate to alternatives.
constraint_indexing:constraint_classification(dark_patterns_manipulation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE) — From civilizational/global scope, dark patterns represent a fundamental extraction mechanism where platform operators capture asymmetric information advantage and exploit cognitive vulnerabilities. The constraint is not a temporary coordination problem or an inevitable feature of digital systems — it is a deliberate architectural choice to maximize extraction. Cognitive science (loss aversion, default bias, choice architecture) provides the weaponization tooling. This perspective sees dark patterns as pure institutional extraction justified by behavioral economics findings.
constraint_indexing:constraint_classification(dark_patterns_manipulation, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dark_patterns_manipulation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(dark_patterns_manipulation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(dark_patterns_manipulation, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(dark_patterns_manipulation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(dark_patterns_manipulation, TR),
    TR >= 0.70.

:- end_tests(dark_patterns_manipulation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High but not extreme. The extraction is real and systematic — platform operators deliberately design interfaces to maximize attention and behavioral steering against user interests. The value reflects the intermediate position: platforms cannot extract 100% of user value (users retain choice to exit, though with friction), and some platforms compete on user trust (reducing exploitation). The upward trend (0.35 → 0.58 over interval) reflects competitive intensification and normalization of dark patterns. Suppression (0.72): Very high. Users have extremely limited capacity to escape dark patterns: cognitive biases operate below conscious awareness, alternatives have network effect lock-in, regulatory enforcement is weak, and technical countermeasures trigger platform counter-adaptation. Suppression does not mean users cannot exit platforms entirely, but rather that they cannot exit dark pattern influence without abandoning the platform ecosystem. Theater ratio (0.61): High and rising. Design ethics frameworks, FTC consent guidelines, and regulatory compliance statements (GDPR, privacy policies) create performative resistance while dark pattern intensity increases. Researchers publish findings; advocates campaign; regulators fine platforms — all create theater while extraction continues. Claimed type: Snare. User perspective + platform beneficiary + no meaningful exit = pure extraction mechanism with suppression.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits stark perspectival disagreement. Platform operators see coordination (Rope) — dark patterns solve the collective action problem of attention scarcity in a free-attention market. Users see extraction (Snare) — cognitive hijacking with no exit. Regulatory bodies see mixed coordination and extraction (Tangled Rope) — they enforce genuine protective mechanisms (consent gates) but also extract compliance costs and consolidate markets. Advocates see degraded institutional resistance (Piton) — awareness campaigns and ethics frameworks maintain visibility without reducing extraction. Alternative platforms see a temporary problem (Scaffold) — open standards and decentralized architectures have sunset logic. The analytical observer sees pure institutional extraction (Snare) — justified by behavioral economics and enabled by asymmetric information.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) flow from structural position. Platform operators as beneficiaries with arbitrage options (exit to profitable alternative business models remains available, though unmotivated) derive low d from the beneficiary + arbitrage path. End users as victims with trapped exit derive maximum d from victim + trapped: they cannot leave digital systems (they need social coordination, commerce, information access) and they cannot escape dark patterns through technical means. Regulatory bodies have organized power but constrained exit (must regulate without destroying innovation capacity), producing moderate-to-high d. The divergence creates a perspectival gap: from platform perspective χ is negative or near-zero (they experience coordination benefit); from user perspective χ is maximally high (they experience extraction with no recourse).
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE CLASSIFICATION CONFIRMED: This constraint avoids mandatrophy between Snare and Rope by recognizing that the coordination function exists only from the platform operator perspective. From the user perspective, there is no genuine coordination — dark patterns are pure extraction mechanisms disguised as UX improvements. The snare classification captures this: suppression (0.72) is exceptionally high because users lack agency, extraction (0.58) is substantial, and no counter-extraction mechanism (victim organization, regulatory enforcement, viable alternatives) significantly constrains operators. The scaffold and piton perspectives exist as secondary structures: alternatives provide theoretical sunset paths; advocacy creates theater that simulates enforcement. But the core structural relationship is snare. Mandatrophy would arise if we claimed both that dark patterns are necessary coordination mechanisms (Rope) and that they are pure extraction (Snare) — they are not. They are pure extraction mechanisms implemented as coordination solutions from the platform perspective, enabling the perspectival gap.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cognitive_bias_threshold,
    'At what point does exploiting a cognitive bias cross from legitimate choice architecture (nudge) into coercive dark pattern?',
    'Cognitive load testing, user intention alignment metrics, comparison of user stated preferences against revealed preferences under dark pattern conditions',
    'If threshold favors platforms: most design patterns remain defensible. If threshold favors users: requires proactive design to highlight user interests, eliminating much of current platform monetization.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cognitive_bias_threshold, conceptual, 'Threshold distinguishing nudge from coercive dark pattern').

omega_variable(
    alternative_monetization_viability,
    'Can platforms remain competitive and funded without dark patterns (via subscription, micropayment, ethical ads, cooperative models)?',
    'Historical analysis of viable platform business models; comparison of user growth and profitability across platforms with varying dark pattern intensity',
    'If viable alternatives exist: dark patterns are choice, not necessity — extraction classification is confirmed. If alternatives are economically impossible: scaffold perspective (temporary) vs snare perspective hinges on whether the underlying business model is itself coercive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_monetization_viability, empirical, 'Whether alternative platform monetization models are economically viable').

omega_variable(
    regulatory_effectiveness_ceiling,
    'Can territorial regulation (GDPR, FTC, regional laws) actually constrain global platforms'' dark pattern design, or does regulatory capture and enforcement gap enable ongoing extraction?',
    'Measurement of dark pattern prevalence before/after regulatory intervention; analysis of platform compliance as function of enforcement capacity and fine magnitude relative to platform revenue',
    'If regulation effective: tangled rope perspective confirmed — genuine user-protective mechanism with enforcement. If regulation ineffective: regulatory perspective degrades to piton (performative), and constraint remains pure snare from user perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_effectiveness_ceiling, empirical, 'Whether territorial regulation can constrain global platforms').

omega_variable(
    user_agency_recovery_mechanisms,
    'Do technical tools (browser extensions, privacy dashboards, user attention dashboards, consent management platforms) actually restore user agency against dark patterns, or do they generate a new arms race where platforms counter-adapt?',
    'User autonomy measurement before/after tool deployment; analysis of platform interface evolution in response to user protection tools; user-reported sense of control metrics',
    'If tools effective: user agency can be restored through technical means — reduces snare classification severity. If arms race dynamic: dark patterns persist via platform counter-adaptation, and snare classification is confirmed for users lacking technical sophistication.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(user_agency_recovery_mechanisms, empirical, 'Whether user protection tools can sustain agency against platform counter-adaptation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dark_patterns_manipulation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(darkpat_tr_t0, dark_patterns_manipulation, theater_ratio, 0, 0.42).
narrative_ontology:measurement(darkpat_tr_t5, dark_patterns_manipulation, theater_ratio, 5, 0.52).
narrative_ontology:measurement(darkpat_tr_t10, dark_patterns_manipulation, theater_ratio, 10, 0.61).

% Extraction over time
narrative_ontology:measurement(darkpat_be_t0, dark_patterns_manipulation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(darkpat_be_t5, dark_patterns_manipulation, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(darkpat_be_t10, dark_patterns_manipulation, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dark_patterns_manipulation, resource_allocation).
narrative_ontology:affects_constraint(dark_patterns_manipulation, attention_extraction_business_model).
narrative_ontology:affects_constraint(dark_patterns_manipulation, regulatory_capture_tech_platforms).
narrative_ontology:affects_constraint(dark_patterns_manipulation, user_data_commercialization).

% DUAL FORMULATION NOTE:
% Dark patterns themselves are one constraint (this story); the upstream constraints are the attention-based business model (ε higher, more fundamental) and user data monetization (ε overlapping, shared extraction mechanism). Dark patterns are downstream tactics enabling these higher-level extraction regimes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dark_patterns_manipulation, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
