% ============================================================================
% CONSTRAINT STORY: algorithmic_salience_curation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_algorithmic_salience_curation, []).

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
 *   constraint_id: algorithmic_salience_curation
 *   human_readable: Algorithmic Salience Curation in Digital Platforms
 *   domain: digital_platforms/information_systems
 *
 * SUMMARY:
 *   Algorithmic salience curation in digital platforms creates a structural
 *   tension between the coordination problem (connecting billions of users
 *   with millions of creators) and the extraction mechanism (optimizing
 *   engagement and advertising revenue through asymmetric visibility
 *   allocation). The constraint exhibits a five-perspective perspectival
 *   spread: from the trapped powerless (attention commons with no exit),
 *   through constrained moderates (marginalized information seekers), to
 *   institutional beneficiaries (platform operators and high-engagement
 *   creators) who experience the algorithm as neutral coordination, to
 *   organized reformers (regulatory coalitions) who see a sunset on the
 *   extraction mechanism, to the piton performance of algorithmic neutrality.
 *   The increasing theater_ratio over the measurement interval (0.45 → 0.75)
 *   reflects the growing gap between the platforms' legitimacy claims
 *   (objective, neutral, user-preference-driven curation) and the documented
 *   reality (intentional ranking for engagement and advertising, systematic
 *   suppression of marginal voices, feedback loops that create rather than
 *   respond to preferences). The extractiveness trajectory (0.32 → 0.62)
 *   shows how the extraction mechanism has intensified as engagement
 *   optimization became the dominant platform business model. At time 0
 *   (early recommendation systems, 2010-2014), algorithms were genuinely
 *   closer to coordination — they solved real information overload problems
 *   with modest engagement optimization. By time 6 (2020-2022),
 *   extractiveness had reached tangled_rope thresholds as engagement became
 *   the primary objective. The analytical observer risks seeing salience
 *   curation as an immutable law of information flow, but the structural data
 *   reveals it as a contingent institutional design.
 *
 * KEY AGENTS:
 *   - Platform Operators: Primary beneficiary (institutional/arbitrage) — capture advertising value and user lock-in through engagement optimization; frame as coordination
 *   - Attention Commons: Primary victim (powerless/trapped) — shared informational environment distorted by extraction; no exit option or voice
 *   - Low-Engagement Creators: Secondary victim (powerless/constrained) — algorithmically suppressed reach; high cost to exit through platform switching
 *   - Marginalized Information Seekers: Secondary victim (moderate/constrained) — information relevant to their communities suppressed by engagement optimization; trapped by network effects
 *   - High-Engagement Content Creators: Beneficiary (powerful/arbitrage) — amplified by algorithm; experience as coordination mechanism rewarding their participation
 *   - Regulatory Reformers: Organized agents (organized/constrained) — DSA, OSB, transparency mandates building alternative regulatory pathway with sunset logic
 *   - Algorithmic Legitimacy Ritual: Institutional actor (institutional/arbitrage) — maintains performance of neutrality through opacity reports, fairness audits; persists through inertia
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent design as law of information physics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(algorithmic_salience_curation, 0.58).
domain_priors:suppression_score(algorithmic_salience_curation, 0.62).
domain_priors:theater_ratio(algorithmic_salience_curation, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(algorithmic_salience_curation, extractiveness, 0.58).
narrative_ontology:constraint_metric(algorithmic_salience_curation, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(algorithmic_salience_curation, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(algorithmic_salience_curation, tangled_rope).
narrative_ontology:human_readable(algorithmic_salience_curation, "Algorithmic Salience Curation in Digital Platforms").
narrative_ontology:topic_domain(algorithmic_salience_curation, "digital_platforms/information_systems").

domain_priors:requires_active_enforcement(algorithmic_salience_curation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(algorithmic_salience_curation, platform_operators).
narrative_ontology:constraint_beneficiary(algorithmic_salience_curation, high_engagement_content_creators).
narrative_ontology:constraint_victim(algorithmic_salience_curation, attention_commons).
narrative_ontology:constraint_victim(algorithmic_salience_curation, marginalized_information_seekers).
narrative_ontology:constraint_victim(algorithmic_salience_curation, low_engagement_creators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE ATTENTION COMMONS (SNARE) — The shared informational environment has no advocate and no exit option. Cannot reorganize its own salience structure. Bears full cost of algorithmic extraction: distorted information distribution, amplified polarization, suppressed marginal voices. Maximum experienced extraction from a structural position with zero agency.
constraint_indexing:constraint_classification(algorithmic_salience_curation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: LOW-ENGAGEMENT CREATORS (SNARE) — Creators whose content is algorithmically deprioritized face suppressed reach despite identical utility to high-engagement alternatives. Exit options exist but require platform switching with network effects penalty — high cost, not impossible. Experience asymmetric visibility distribution as extraction: algorithmic suppression of reach.
constraint_indexing:constraint_classification(algorithmic_salience_curation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: MARGINALIZED INFO SEEKERS (TANGLED ROPE) — Face algorithmic suppression of information relevant to their communities: medical information in non-dominant languages, local news in peripheral regions, niche expertise that lacks mass engagement signals. Simultaneously benefit from algorithmic curation reducing information overload. Moderate agency — can use search, follow specific creators, but discovery mechanisms extract value from the marginalized.
constraint_indexing:constraint_classification(algorithmic_salience_curation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: PLATFORM OPERATORS (ROPE) — Experience the algorithm as a coordination mechanism solving the aggregate problem: matching billions of users with relevant content from millions of creators. Extractive benefits flow toward platforms through engagement optimization and advertising rate maximization, but the platforms frame and experience this as system coordination: 'helping people find what they want.' Net beneficiary with structural mobility.
constraint_indexing:constraint_classification(algorithmic_salience_curation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: HIGH-ENGAGEMENT CREATORS (ROPE) — Benefit from algorithmic amplification; their content reaches audiences at scale through positive engagement feedback. Experience the algorithm as coordination: transparent rules (maximize engagement, satisfy viewer retention) that reward their participation. High agency — can adjust content to optimize, experiment with formats, arbitrage between platforms.
constraint_indexing:constraint_classification(algorithmic_salience_curation, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: REGULATORY REFORMERS (SCAFFOLD) — Digital Services Act, Online Safety Bill, algorithmic transparency mandates, and decentralized alternative platforms represent organized agents viewing the bottleneck as a temporary institutional failure with a sunset. Low effective extraction from this perspective because the coalition has agency and a visible exit path: regulatory override of proprietary algorithms, interoperability requirements, data portability. Sunset mechanism: once regulation matures, algorithmic suppression becomes legally costly.
constraint_indexing:constraint_classification(algorithmic_salience_curation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: LEGITIMACY RITUAL (PITON) — 'The algorithm is neutral and objective' is a sustained performative claim despite documented bias and intentional steering. Platforms publish opacity reports and launch fairness audits as institutional theater: maintaining the appearance of rational curation while suppressing visibility of intentional ranking. Theater persists through institutional inertia (investor expectations, regulatory evasion) rather than functional necessity. Algorithm design could be transparent; the secrecy is enforced.
constraint_indexing:constraint_classification(algorithmic_salience_curation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / FALSE SUMMIT VIEW (MOUNTAIN) — From a sufficiently abstract perspective, some salience curation is inherent to information systems: perfect equality of visibility is impossible; scarcity of human attention forces allocation. This perspective risks naturalizing what are contingent design choices (engagement optimization, advertiser maximization) as unavoidable laws of information flow. Engine will detect this as a false summit — the structural data contradicts mountain classification, revealing naturalization of institutional arrangement.
constraint_indexing:constraint_classification(algorithmic_salience_curation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(algorithmic_salience_curation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(algorithmic_salience_curation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(algorithmic_salience_curation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(algorithmic_salience_curation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(algorithmic_salience_curation, TR),
    TR >= 0.70.

:- end_tests(algorithmic_salience_curation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Platform operators capture significant value through engagement optimization (higher advertising rates, increased user lock-in) at the cost of distorted information distribution. The extraction is not as severe as a pure snare (0.72+) because platforms do solve a real coordination problem — matching users to content at scale — and high-engagement creators genuinely benefit from the mechanism. But the primary objective function (engagement, not information quality or user welfare) drives asymmetric extraction from low-engagement creators and information-seekers. The extractiveness trajectory rising from 0.32 to 0.62 reflects the intensification of engagement optimization as platforms moved from information-matching to attention-capture business models. Suppression (0.62): High. Multiple suppression mechanisms: (1) algorithmic ranking against marginal content, (2) language and region bias in engagement signals, (3) information asymmetry about ranking criteria, (4) network effects creating switching costs for exit, (5) opacity reports performing neutrality while suppressing algorithm details. Suppression is not total (users can still search, follow creators directly) but substantial. Theater ratio (0.68): High. The gap between legitimacy claims and mechanism is large: platforms present algorithms as objective, neutral, user-preference-responsive while systematically optimizing for engagement and advertising. Opacity reports and fairness audits are institutional theater — they maintain the performance of accountability without constraining the extraction mechanism. Theater has increased over time as extraction intensified, creating need for stronger legitimacy performance.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates stark perspectival divergence: platform operators genuinely experience coordination (Rope) — they are solving the matching problem with apparent elegance. High-engagement creators experience coordination (Rope) — the algorithm rewards them transparently. But trapped low-engagement creators and the powerless attention commons experience pure extraction (Snare) — the algorithm suppresses them with no exit. Marginalized information seekers experience mixed extraction and coordination (Tangled Rope) — they benefit from information filtering but are systematically deprived of relevant information about their own communities. The regulatory coalition sees a temporary problem with a sunset (Scaffold) — regulatory override will make algorithmic suppression costly. The piton perspective reveals algorithmic legitimacy as theater persisting through inertia. The analytical observer risks seeing salience curation as inevitable, which would naturalize the design choice as law and shield extraction from scrutiny. The perspectival gap reveals that the same algorithmic mechanism is experienced as coordination by beneficiaries and extraction by victims — the structural data confirms extraction is happening (high suppression, asymmetric visibility allocation, increasing theater), not that it's an artifact of perspective.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective derives from the agent's power level, exit options, and position in the extraction flow. Platform operators with institutional power and arbitrage options have low d (~0.15) — they are net beneficiaries and experience the constraint as coordination. High-engagement creators with powerful status and arbitrage options also have low d (~0.25) — they benefit from amplification. Low-engagement creators and marginalized information seekers have high d (0.75-0.90) — they are trapped or heavily constrained by network effects and algorithmic suppression. The powerless agents (attention commons, trapped creators) have maximum d (0.95) — they bear extraction without exit or benefit. The regulatory reformers with organized power and constrained exit have moderate d (0.55-0.65) — they see extraction clearly and have built coalition power to address it, but are not trapped. The analytical observer at civilizational scope has canonical d (0.72) — risks naturalizing contingent design as inevitable law.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE CONFIRMED: The constraint possesses both genuine coordination (matching users to content solves real information problem) and asymmetric extraction (engagement optimization suppresses marginal voices and extracts attention value toward advertising). Active enforcement is required — platforms maintain algorithmic opacity and actively suppress alternative curation mechanisms. The beneficiaries (platforms, high-engagement creators) benefit from the coordination function AND the extraction asymmetry. The victims (low-engagement creators, marginalized seekers, attention commons) bear both coordination costs AND extraction. The mandatrophy is resolved by recognizing that platforms have intentionally designed their coordination mechanism to maximize extraction: engagement optimization is not a neutral response to user preferences but a deliberate choice to optimize platform revenue. The choice could be different (chronological feeds, user-controlled ranking, diverse-exposure mandates) without losing coordination benefits. This is why the piton perspective (algorithmic neutrality ritual) is correct — platforms perform neutrality to justify what are actually extractive design choices. Regulatory reform (scaffold) targets exactly this point: mandate transparency and interoperability, and the same coordination benefits can be achieved with reduced extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    engagement_metric_causality,
    'Does algorithmic amplification of high-engagement content serve user preferences, or does it create and reinforce engagement patterns through feedback loops?',
    'Longitudinal A/B testing: randomized chronological vs algorithmic feeds over months; measurement of content distribution divergence; user preference elicitation independent of algorithmic exposure',
    'If user-preference-driven: engagement optimization is primarily coordination (Rope classification stable). If feedback-loop-driven: engagement is extraction mechanism (Snare/Tangled Rope from marginalized perspectives). Determines whether extraction vs coordination framing is defensible.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(engagement_metric_causality, empirical, 'Whether algorithmic amplification follows or creates user engagement preferences').

omega_variable(
    marginalization_intentionality,
    'Is algorithmic suppression of marginal-language and peripheral-region content a side effect of engagement optimization or an intentional design feature?',
    'Analysis of algorithm design documentation; comparison of engagement-equivalent content in dominant vs marginal languages; deliberate reweighting tests with language/region coefficients removed',
    'If side effect: suppression is unintended extraction (high theater, structural fix possible). If intentional: extraction is designed and defended (high suppression, political choice). Changes framing of victim status.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marginalization_intentionality, empirical, 'Whether algorithmic marginalization is intentional design or unintended side effect').

omega_variable(
    interoperability_feasibility,
    'Can distributed or interoperable algorithm architectures maintain coordination benefits (matching users to content) while reducing extraction (suppression of marginalized voices)?',
    'Technical feasibility studies of federation, algorithmic portability, and decentralized recommendation systems; comparison of coordination function between centralized and distributed systems in testbeds',
    'If feasible: scaffold perspective is structural (regulatory sunset is real, alternative architecture works). If infeasible: scaffold is aspirational (no real exit path), and platform concentration remains inevitable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interoperability_feasibility, empirical, 'Whether interoperable algorithm architectures can maintain coordination benefits').

omega_variable(
    transparency_paradox,
    'Does algorithmic transparency (revealing ranking criteria to users and creators) increase user agency and reduce extraction, or does it enable new forms of gaming and manipulation?',
    'Comparison of user and creator behavior before/after transparency interventions; measurement of SEO-gaming intensity in transparent vs opaque recommendation systems; user satisfaction and content diversity metrics',
    'If transparency increases agency: opacity is extractive mechanism (suppression gate driven by information asymmetry). If transparency enables gaming: transparency-as-solution is false (extraction persists through different mechanism). Affects regulatory reform viability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transparency_paradox, empirical, 'Whether algorithmic transparency reduces extraction or enables new gaming').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(algorithmic_salience_curation, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(alg_sal_tr_t0, algorithmic_salience_curation, theater_ratio, 0, 0.45).
narrative_ontology:measurement(alg_sal_tr_t3, algorithmic_salience_curation, theater_ratio, 3, 0.58).
narrative_ontology:measurement(alg_sal_tr_t6, algorithmic_salience_curation, theater_ratio, 6, 0.68).
narrative_ontology:measurement(alg_sal_tr_t9, algorithmic_salience_curation, theater_ratio, 9, 0.75).

% Extraction over time
narrative_ontology:measurement(alg_sal_be_t0, algorithmic_salience_curation, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(alg_sal_be_t3, algorithmic_salience_curation, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(alg_sal_be_t6, algorithmic_salience_curation, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(alg_sal_be_t9, algorithmic_salience_curation, base_extractiveness, 9, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(algorithmic_salience_curation, information_standard).
narrative_ontology:affects_constraint(algorithmic_salience_curation, attention_extraction_business_model).
narrative_ontology:affects_constraint(algorithmic_salience_curation, information_asymmetry_platform_design).
narrative_ontology:affects_constraint(algorithmic_salience_curation, engagement_feedback_loops).

% DUAL FORMULATION NOTE:
% Algorithmic salience curation should be decomposed into three distinct constraints per ε-invariance principle: (1) information matching (pure coordination, ε~0.15, Rope), (2) engagement optimization (extraction mechanism, ε~0.65, Snare), (3) algorithmic opacity theater (performative legitimacy, ε~0.50, Piton). This story represents the aggregate institutional practice, but empirical analysis should evaluate each mechanism separately.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(algorithmic_salience_curation, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
