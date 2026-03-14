% ============================================================================
% CONSTRAINT STORY: platform_algorithmic_capture
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_platform_algorithmic_capture, []).

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
 *   constraint_id: platform_algorithmic_capture
 *   human_readable: Platform Algorithmic Capture
 *   domain: digital_economics/platform_governance
 *
 * SUMMARY:
 *   Platform algorithmic capture describes the structural entrapment of
 *   content creators and end users within opaque algorithmic systems that
 *   systematically extract value and behavioral data while presenting
 *   themselves as neutral coordination mechanisms. The constraint operates at
 *   the intersection of technical infrastructure, economic incentives, and
 *   information asymmetry. Platforms experience algorithms as essential
 *   coordination tools enabling scale; creators and users experience them as
 *   traps from which exit is economically or technically impossible. The
 *   constraint exhibits genuine coordination function (ranking billions of
 *   items requires algorithmic filtering) coupled with systematic extraction
 *   (behavioral manipulation, market consolidation, suppression of
 *   alternatives). Suppression mechanisms include: network effects that trap
 *   users, economic dependency that traps creators, algorithmic opacity that
 *   prevents accountability, and regulatory capture that prevents
 *   intervention. The theater ratio reflects the pervasive performance of
 *   algorithmic neutrality — the claim that algorithms are objective,
 *   unbiased, and merely technical — which obscures the fundamentally
 *   political character of ranking choices and serves as cover for
 *   rent-seeking and behavioral manipulation.
 *
 * KEY AGENTS:
 *   - Content Creators: Primary victim (powerless/trapped) — economically dependent on platform visibility; cannot understand algorithmic ranking; face livelihoods threatened by opaque algorithm changes
 *   - End Users: Primary victim (powerless/trapped) — trapped within filter bubbles and attention manipulation; cannot inspect or revoke algorithmic data collection; no alternative social infrastructure at comparable scale
 *   - Platform Operators: Primary beneficiary (institutional/arbitrage) — benefits from algorithmic lock-in of users and creators; controls algorithmic ranking decisions; has arbitrage options for algorithmic systems but maintains opacity for competitive advantage
 *   - Content Moderation Workforce: Secondary victim (moderate/constrained) — coordinates genuine governance function but experiences low pay, high trauma, precarity; constrained by labor market options and NDAs
 *   - Regulatory Authorities: Constrained powerful agent (powerful/mobile) — can theoretically impose algorithmic transparency and interoperability standards but face lobbying, regulatory capture, and jurisdictional limits; see genuine coordination but also systematic asymmetry
 *   - Algorithmic Accountability Community: Analytical observer — researchers, auditors, civil society tracking algorithmic harms; positioned to see full structure but constrained by platforms' information withholding and legal threats
 *   - Analytical Observer: Civilizational context (analytical/analytical) — sees integrated structure of coordination + extraction + suppression + theater; resolves mandatrophy by showing coordination and extraction are coupled through design choices
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(platform_algorithmic_capture, 0.58).
domain_priors:suppression_score(platform_algorithmic_capture, 0.68).
domain_priors:theater_ratio(platform_algorithmic_capture, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(platform_algorithmic_capture, extractiveness, 0.58).
narrative_ontology:constraint_metric(platform_algorithmic_capture, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(platform_algorithmic_capture, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(platform_algorithmic_capture, tangled_rope).
narrative_ontology:human_readable(platform_algorithmic_capture, "Platform Algorithmic Capture").
narrative_ontology:topic_domain(platform_algorithmic_capture, "digital_economics/platform_governance").

domain_priors:requires_active_enforcement(platform_algorithmic_capture).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(platform_algorithmic_capture, platform_operators).
narrative_ontology:constraint_beneficiary(platform_algorithmic_capture, algorithmic_service_providers).
narrative_ontology:constraint_victim(platform_algorithmic_capture, content_creators).
narrative_ontology:constraint_victim(platform_algorithmic_capture, end_users).
narrative_ontology:constraint_victim(platform_algorithmic_capture, algorithmic_accountability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Content creators depend on platform distribution for income and audience. Algorithm visibility is non-transparent; creators cannot understand why content reaches or fails to reach audiences. Trapped by economic dependency and algorithmic opacity — no alternative distribution channels at comparable scale. Maximum extraction experienced: algorithmic changes can eliminate livelihood overnight with no recourse.
constraint_indexing:constraint_classification(platform_algorithmic_capture, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% End users cannot exit the algorithmic filtering without losing platform access entirely. Trapped within filter bubbles, algorithmic polarization, and attention manipulation. Algorithm operates on behavioral data the user cannot inspect or revoke without discontinuing service. No alternative social infrastructure at comparable network scale. Experiences maximum suppression through lock-in and network effects.
constraint_indexing:constraint_classification(platform_algorithmic_capture, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% Moderators depend on platform contracts for income but face high burnout, trauma, and precarity. Genuinely coordinate content governance with platform (essential function). Also experience extraction: low pay, poor working conditions, algorithmic task assignment without transparency. Mixed: the moderation function is real and necessary; the exploitation is also real. Constrained by labor market options and nondisclosure agreements.
constraint_indexing:constraint_classification(platform_algorithmic_capture, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Platform experiences algorithmic governance as coordination mechanism — algorithms enable serving billions of users, matching content to attention, mediating interactions at scale. This is genuinely functional coordination. Platform has arbitrage options: can adjust algorithms, switch vendors, or invest in alternative ranking systems. Benefits from first-mover network effects and algorithmic lock-in of users. Experiences the constraint as enabling rather than extractive.
constraint_indexing:constraint_classification(platform_algorithmic_capture, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% Regulators see genuine coordination (algorithmic ranking is necessary to manage scale) but also see systematic asymmetry: platforms use algorithmic opacity to resist accountability, manipulate user behavior, and consolidate market power. Regulators are mobile in theory (can impose standards, interoperability, transparency requirements) but constrained in practice by lobbying, regulatory capture, and jurisdictional limits. Medium power with some exit options but facing significant structural resistance.
constraint_indexing:constraint_classification(platform_algorithmic_capture, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% The performance of algorithmic objectivity ('the algorithm is neutral,' 'we don't editorialize') has become purely theatrical. Algorithmic choices are value-laden (ranking by engagement rather than veracity, optimizing for watch time). The ritual of algorithmic neutrality persists through institutional inertia — platforms claim algorithms are objective, researchers cite algorithmic fairy tales, regulators seek algorithmic transparency that would be expensive and disruptive. The constraint persists not because the theater works functionally but because dismantling it would expose the fundamentally political character of ranking choices. Theater ratio is high because the entire accountability structure rests on a performance that everyone partially disbelieves.
constraint_indexing:constraint_classification(platform_algorithmic_capture, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% From a civilizational analytical perspective, algorithmic capture involves both genuine coordination (ranking at scale) and systematic extraction (behavioral manipulation, market power consolidation, regulatory capture). The constraint exhibits all hallmarks of tangled rope: significant coordination function without which scale is impossible; significant extraction asymmetry that benefits platform operators at expense of users and creators; active enforcement through algorithmic opacity and legal/contractual barriers; and suppression mechanisms that prevent alternatives from scaling. The analytical frame resolves the mandatrophy by showing the coordination and extraction are intrinsically coupled — cannot have algorithmic scale without opacity, cannot have algorithmic governance without behavioral capture.
constraint_indexing:constraint_classification(platform_algorithmic_capture, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(platform_algorithmic_capture_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(platform_algorithmic_capture, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(platform_algorithmic_capture, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(platform_algorithmic_capture, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(platform_algorithmic_capture, TR),
    TR >= 0.70.

:- end_tests(platform_algorithmic_capture_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Platforms extract through: (1) behavioral data harvesting (used for targeting advertisers), (2) attention extraction (optimizing engagement over veracity), (3) market consolidation (algorithmic opacity prevents competitive alternatives), (4) labor extraction (creator monetization skewed toward platform). The 0.58 value reflects that extraction is significant but not absolute — creators and users do receive real value (distribution, community, income), and platforms do solve genuine coordination problems. Extraction is coupled to coordination. Suppression (0.68): High. Multiple barriers to exit: (1) network effects (switching users away from dominant platform is economically irrational), (2) economic lock-in (creators have no alternative income source at comparable scale), (3) informational opacity (users cannot understand or object to algorithmic choices), (4) regulatory capture (prevents intervention). Notably, suppression includes prevention of collective action — platforms can identify and neutralize creator organizing through algorithmic demotion or policy enforcement. Theater ratio (0.64): Moderate-high. The performance of algorithmic neutrality ('we don't editorialize, the algorithm is objective') has become institutionalized. Platforms use algorithmic language to deflect responsibility ('the algorithm decided'), researchers cite algorithmic explanations while ignoring value choices in ranking systems, regulators seek algorithmic transparency that would require expensive redesign. The theater increases as platforms face accountability pressure — they invest more heavily in algorithmic legitimacy claims rather than addressing underlying extraction.
 *
 * PERSPECTIVAL GAP:
 *   The constraint shows maximum perspectival divergence between platform operators and trapped victims. Platforms see Rope (pure coordination) — algorithms enable matching content to attention, reducing friction. Trapped creators see Snare (pure extraction) — algorithmic invisibility eliminates income. Trapped users see Snare (pure extraction) — algorithmic manipulation and filter bubbles. Moderators see Tangled Rope (mixed coordination and exploitation) — they coordinate content governance while being exploited. Regulators see Tangled Rope with power asymmetry (powerful but constrained). Analytical observer sees Tangled Rope with clear mandatrophy resolution — the coordination and extraction are intrinsically coupled through design choices (opacity enables both scale and lock-in, behavioral targeting enables both personalization and manipulation). The gap between platform perspective (Rope) and user perspective (Snare) is the diagnostic signal that the constraint is not genuine coordination but extraction masked by coordination language. If platforms genuinely experienced only coordination with no extraction benefit, they would not resist transparency and interoperability so vigorously.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from structural position within the extraction flow. Content creators appear as victims with trapped exit (d ≈ 0.95 → high f(d)) — they bear maximum extraction. End users similarly trapped and victimized (d ≈ 0.95). Platform operators are beneficiaries with arbitrage exit (d ≈ 0.15 → low/negative f(d)) — they capture extraction. Moderators are both victims and coordinate function (d ≈ 0.70 → high f(d)). Regulators see the asymmetry but have organizational mobility (d ≈ 0.55 → moderate f(d)). The analytical observer sees the full structure including the coupling between genuine coordination and systematic extraction (d ≈ 0.72). The directionality computation reveals that platforms' arbitrage options (they can change algorithms, invest in alternatives, migrate away from behavioral targeting) are decoupled from creators' and users' exit options (no viable alternatives at comparable scale). This structural asymmetry is the core of the extraction mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: This constraint resolves the coordination/extraction dilemma by showing that the coupling is not accidental but designed. Platforms could reduce extractiveness by: (1) providing algorithmic transparency, (2) enabling creator and user data portability, (3) supporting algorithmic interoperability. That they resist all three reveals that the apparent coordination function is intertwined with extraction mechanisms. The suppression (network effects + regulatory capture) exists precisely to prevent the decoupling that would allow pure coordination. The theater (algorithmic neutrality performance) justifies maintaining opacity. The mandatrophy is resolved not by choosing between 'algorithms are coordination' and 'algorithms are extraction' but by recognizing that *the current implementation* couples coordination with extraction through design. A redesigned system (interoperable algorithms, transparent ranking, user data rights) could reduce extractiveness to 0.25-0.35 range (Rope or Scaffold). The current system maintains high extractiveness because platforms benefit from the coupling. This is the key insight: the constraint is a tangled_rope by design, not accident. Unwinding it requires both technical change (algorithmic transparency, data portability) and institutional change (regulatory standards, interoperability mandates, antitrust enforcement).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    algorithmic_opacity_necessity,
    'Is algorithmic opacity a necessary consequence of scale or a chosen mechanism of control?',
    'Comparative analysis: platforms that invested in algorithmic explainability vs those that did not; measure user manipulation effectiveness across transparency levels; test whether optimization performance degrades with transparency',
    'If necessary: extractiveness is partially justified as coordination cost (ε → 0.42). If chosen: extractiveness is primarily rent-seeking via information asymmetry (ε → 0.68). Changes classification from tangled_rope to snare depending on power of agent analyzing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_opacity_necessity, empirical, 'Whether algorithmic opacity is structurally necessary or strategically chosen').

omega_variable(
    network_effect_inevitability,
    'Are network effects that lock users into platform algorithms inevitable or created by design choices (interoperability prevention, proprietary data, algorithmic lock-in)?',
    'Historical analysis of interoperability standards in digital platforms; comparative study of jurisdictions with interoperability mandates; measurement of switching costs with vs without algorithmic portability',
    'If inevitable: suppression is structural (physical network dynamics). If designed: suppression is institutional extraction (could be reduced through technical/regulatory choices). Changes suppression interpretation from 0.68 to potentially 0.35-0.50 if redesigned.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(network_effect_inevitability, empirical, 'Whether network lock-in is inevitable or deliberately constructed').

omega_variable(
    content_creator_coalition_threshold,
    'At what scale of creator departure or collective action would platform algorithms lose function? Can creators organize exit?',
    'Network analysis of creator dependency: measure minimum viable creator population for platform viability; identify critical creator segments; track coordination attempts and counter-measures',
    'If critical mass is achievable: powerless creators could become organized (coalition power upgrade, classification changes to rope or scaffold from some perspectives). If prevented: suppression includes coordination prevention (raises suppression from 0.68 to 0.78+). Reveals whether trap is inherent or maintained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(content_creator_coalition_threshold, empirical, 'Whether content creators can organize coalition power to renegotiate algorithmic terms').

omega_variable(
    user_behavioral_lock_in_malleability,
    'How much of user lock-in is due to genuine network benefits vs algorithmic manipulation? Can users perceive or escape behavioral capture?',
    'Controlled intervention: provide users with transparency about algorithmic ranking decisions; measure if awareness changes engagement; comparative study of platforms with high vs low algorithmic manipulation; identity_locked persistence testing',
    'If primarily genuine network effects: exit_options could be upgraded from trapped to constrained for end users. If primarily manipulation: users are identity_locked (captured identity fused with platform community) rather than trapped. This distinction is critical for understanding whether users could exercise agency if constraints changed. Changes interpretation of suppression mechanism from structural to behavioral.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(user_behavioral_lock_in_malleability, empirical, 'Degree to which user lock-in reflects genuine network effects vs algorithmic behavioral capture').

omega_variable(
    regulatory_capture_circularity,
    'Is regulatory capture of algorithmic policy self-reinforcing? Do platforms use algorithmic opacity to prevent accountability, which prevents effective regulation, which allows platforms to continue opacity?',
    'Trace regulatory action: identify cases where algorithmic transparency was mandated; measure compliance costs vs evasion costs; identify regulatory agencies captured by platform funding/employment pipelines; test whether algorithmic auditing can survive platform resistance',
    'If self-reinforcing: the constraint may be strengthening over time (theater_ratio and extractiveness both increasing). If breakable: regulatory action could reduce extractiveness to 0.40-0.45 range (Rope or weak Tangled Rope). Affects trajectory predictions and intervention effectiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_circularity, empirical, 'Whether algorithmic capture prevents its own regulation (self-reinforcing cycle)').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(platform_algorithmic_capture, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(plac_tr_t0, platform_algorithmic_capture, theater_ratio, 0, 0.42).
narrative_ontology:measurement(plac_tr_t4, platform_algorithmic_capture, theater_ratio, 4, 0.54).
narrative_ontology:measurement(plac_tr_t8, platform_algorithmic_capture, theater_ratio, 8, 0.64).

% Extraction over time
narrative_ontology:measurement(plac_be_t0, platform_algorithmic_capture, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(plac_be_t4, platform_algorithmic_capture, base_extractiveness, 4, 0.47).
narrative_ontology:measurement(plac_be_t8, platform_algorithmic_capture, base_extractiveness, 8, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(platform_algorithmic_capture, resource_allocation).
narrative_ontology:affects_constraint(platform_algorithmic_capture, attention_economy_extraction).
narrative_ontology:affects_constraint(platform_algorithmic_capture, algorithmic_polarization_feedback).
narrative_ontology:affects_constraint(platform_algorithmic_capture, platform_regulatory_capture).

% DUAL FORMULATION NOTE:
% Algorithmic capture decomposes into three structurally distinct constraints with different ε values: (1) algorithmic_resource_allocation (ε≈0.30, Rope) — ranking algorithm coordinating attention/content matching; (2) algorithmic_behavioral_manipulation (ε≈0.62, Snare) — behavioral targeting and filter bubbles with pure extraction function; (3) algorithmic_opacity_enforcement (ε≈0.55, Tangled Rope) — maintaining opacity as active mechanism coupling coordination benefits with extraction lock-in. Platform_algorithmic_capture is the umbrella story integrating all three; the decomposition reveals which aspects have genuine coordination function and which are pure extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(platform_algorithmic_capture, institutional, 0.18).
constraint_indexing:directionality_override(platform_algorithmic_capture, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
