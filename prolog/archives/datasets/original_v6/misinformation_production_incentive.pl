% ============================================================================
% CONSTRAINT STORY: misinformation_production_incentive
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_misinformation_production_incentive, []).

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
 *   constraint_id: misinformation_production_incentive
 *   human_readable: Misinformation Production Incentive Structure
 *   domain: information/media/politics
 *
 * SUMMARY:
 *   The misinformation production incentive is a structural constraint
 *   arising from the alignment of platform engagement-maximization
 *   algorithms, individual creator incentives for attention, and information
 *   consumer cognitive biases. The constraint operates across social media,
 *   cable news, and algorithmic content recommendation systems at scale. From
 *   the perspective of information consumers, it functions as a pure
 *   extraction mechanism: cognitive effort is extracted (time spent
 *   evaluating false claims, confusion from competing narratives) with
 *   minimal coordination benefit. From the perspective of platforms and
 *   content creators, it functions as a coordination solution: the algorithm
 *   reliably signals which content succeeds, enabling creators to optimize.
 *   The constraint exhibits high extractiveness (0.68) because the
 *   beneficiaries (platforms, creators) capture enormous value from
 *   engagement while victims (the consuming public, epistemic commons) bear
 *   costs they do not choose and cannot easily escape. Suppression is high
 *   (0.72) because the mechanisms preventing exit are multiple and
 *   overlapping: algorithmic amplification makes alternative information
 *   pathways less visible, identity-fusion prevents certain populations from
 *   questioning narratives, and the sheer volume of competing claims makes
 *   individual verification prohibitively costly. Theater ratio is elevated
 *   (0.65) because much of the visible activity — fact-checking posts,
 *   counter-narratives, verification attempts — is largely performative given
 *   the structural incentives that continue to reward engagement regardless
 *   of accuracy.
 *
 * KEY AGENTS:
 *   - Information Consuming Public: Primary victims (powerless/trapped) — face extraction of cognitive trust and epistemic confidence; trapped by algorithmic environment designed to maximize engagement over accuracy
 *   - Epistemically Vulnerable Populations: Secondary victims (powerless/identity_locked) — identity-fused into misinformation narratives; exit would require identity transformation, not just belief change
 *   - Independent Journalists: Secondary actors (moderate/constrained) — coordinate information discovery while constrained by platform gatekeeping and sensationalism incentives
 *   - Platform Companies: Primary beneficiaries (institutional/arbitrage) — capture engagement value while externalizing epistemic costs; experience the constraint as advantageous coordination
 *   - Professional Misinformation Producers: Secondary beneficiaries (organized/mobile) — specialize in high-engagement narratives; experience the constraint as pure coordination with mobile exit options
 *   - Epistemic Commons (Abstract): Victim collective (powerless/trapped) — shared reality-testing capacity degrades as confidence in shared information sources erodes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(misinformation_production_incentive, 0.68).
domain_priors:suppression_score(misinformation_production_incentive, 0.72).
domain_priors:theater_ratio(misinformation_production_incentive, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(misinformation_production_incentive, extractiveness, 0.68).
narrative_ontology:constraint_metric(misinformation_production_incentive, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(misinformation_production_incentive, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(misinformation_production_incentive, snare).
narrative_ontology:human_readable(misinformation_production_incentive, "Misinformation Production Incentive Structure").
narrative_ontology:topic_domain(misinformation_production_incentive, "information/media/politics").

domain_priors:requires_active_enforcement(misinformation_production_incentive).
% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(misinformation_production_incentive, content_creators_optimizing_engagement).
narrative_ontology:constraint_beneficiary(misinformation_production_incentive, platforms_maximizing_time_on_site).
narrative_ontology:constraint_beneficiary(misinformation_production_incentive, political_actors_manipulating_narratives).
narrative_ontology:constraint_victim(misinformation_production_incentive, information_consuming_public).
narrative_ontology:constraint_victim(misinformation_production_incentive, epistemic_commons).
narrative_ontology:constraint_victim(misinformation_production_incentive, democratic_deliberation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MISINFORMED CONSUMER (SNARE) — Citizens attempting to form accurate beliefs face an information environment engineered to maximize engagement over accuracy. Algorithmic amplification, algorithmic filter bubbles, and platform incentives create asymmetric extraction: cognitive effort is extracted (attention spent on false claims) with minimal coordination benefit. The trap is structural — exit requires developing individual media literacy while the entire ecosystem actively undermines it.
constraint_indexing:constraint_classification(misinformation_production_incentive, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EPISTEMICALLY VULNERABLE POPULATION (SNARE) — Agents whose information-forming capacity is constrained by education, cognitive load, or identity fusion into communities with specific misinformation narratives. Identity-locked exit: the agent cannot question the belief system without breaking their community identity. Biographical timeline shows mountain (unchangeable), but generational timeline shows snare — institutional inertia perpetuates the lock across populations.
constraint_indexing:constraint_classification(misinformation_production_incentive, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 3: INDEPENDENT JOURNALIST (TANGLED ROPE) — Coordinates information discovery and verification (coordination function) while being constrained by platform gatekeeping and algorithmic suppression of low-sensationalism reporting. Benefits from the attention economy when pursuing compelling stories; harmed by the same incentive structure when pursuing accurate but less sensational reporting. Mixed extraction and coordination.
constraint_indexing:constraint_classification(misinformation_production_incentive, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: PLATFORM COMPANY (ROPE) — Solves the coordination problem of connecting billions of information producers and consumers. From the platform's structural position, misinformation is not a bug but a feature of the engagement-maximization algorithm — it is a pure coordination benefit (engagement) that comes with externalized costs (epistemic damage to society). The platform experiences the constraint as advantageous coordination without extraction experienced by the platform itself.
constraint_indexing:constraint_classification(misinformation_production_incentive, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: PROFESSIONAL MISINFORMATION PRODUCER (ROPE) — Specialized content creators optimize for engagement-maximizing narratives (true or false is irrelevant). From this actor's perspective, the constraint is pure coordination: algorithms reliably reward high-engagement content, and the producer coordinates with platform incentives. The producer has mobile exit options (switch platforms, switch content genres) and experiences high benefit with minimal suppression from their own perspective.
constraint_indexing:constraint_classification(misinformation_production_incentive, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — From a civilizational perspective analyzing information ecosystem health, the misinformation production incentive is a pure extraction mechanism. The constraint extracts cognitive trust (citizens must expend energy distinguishing truth from falsehood), epistemic commons reliability (shared reality becomes contested), and deliberative capacity (polarization reduces productive dialogue). The beneficiaries are concentrated; the victims are diffuse and largely powerless.
constraint_indexing:constraint_classification(misinformation_production_incentive, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(misinformation_production_incentive_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(misinformation_production_incentive, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(misinformation_production_incentive, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(misinformation_production_incentive, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(misinformation_production_incentive_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness: 0.68. The constraint extracts significant value in the form of engagement metrics (which translate to advertising revenue, user retention, and capital valuation for platforms) while imposing costs on information consumers in the form of confusion, wasted cognitive effort, and reduced epistemic confidence. The value asymmetry is severe because platforms capture concentrated benefits while costs are diffused across billions of users. The initial measurement (0.35) reflects a period when social media incentive structures were less systematically optimized for engagement; the final measurement (0.68) reflects current algorithmic sophistication in maximizing engagement regardless of accuracy. Suppression: 0.72. Multiple overlapping mechanisms prevent exit: algorithmic amplification makes low-engagement truthful content less visible; platforms provide no easy mechanism for users to opt out of engagement-maximizing feeds; identity-fusion prevents epistemically vulnerable populations from questioning foundational narratives; and the high cost of individual fact-verification (time-intensive, requires specialized knowledge) creates de facto barriers. Suppression is high but not total — some users do switch platforms, develop media literacy, or seek alternative information sources, though at significant cost. Theater ratio: 0.65. Much visible activity in response to misinformation (fact-checking posts, counter-narratives, media literacy campaigns) is structurally performative given that engagement-maximization algorithms continue to reward virality regardless of accuracy. The theater has increased over the interval as awareness of misinformation problems has grown but algorithmic incentives have remained unchanged, creating a gap between performative response and structural change.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates stark disagreement across structural positions. The platform company's perspective (Rope) sees coordination: algorithms enable content discovery at scale, creators benefit from clear feedback signals, and consumers benefit from access to diverse information. The consuming public's perspective (Snare) sees extraction: they are trapped in an algorithmic environment that extracts their attention and erodes their epistemic confidence. The professional misinformation producer's perspective (Rope) sees coordination: engagement signals clearly reward high-sentiment narratives, and exit options (switch platforms, switch niches) are available if incentives change. The epistemically vulnerable population's perspective (Snare, identity_locked) sees a trap that operates through identity: they cannot question the misinformation without breaking community bonds or abandoning foundational self-concept. The analytical observer's perspective (Snare) sees pure extraction from the civilizational viewpoint: the constraint concentrates benefits among a small number of platforms and creators while diffusing costs across billions of information consumers, degrading shared reality and deliberative capacity. The perspectival gap reflects fundamentally different structural positions: beneficiaries with mobile exit options experience the constraint as coordination; trapped victims experience it as extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for each agent is derived from their structural relationship to the extraction flow. The information consuming public are victims with trapped exit options: high d → high f(d) → high experienced extractiveness. Epistemically vulnerable populations are victims with identity_locked exit: even higher d because cognitive barriers compound material ones. Independent journalists have ambiguous directionality: they are both beneficiaries (when pursuing sensational stories that drive engagement) and victims (when pursuing accurate-but-unsexy reporting). Platform companies are beneficiaries with arbitrage exit options: low d → low/negative f(d) → they experience low or negative extraction. Professional misinformation producers are beneficiaries with mobile exit options: low d despite being beneficiaries, because their exit capacity is high. The deriving chain prioritizes structural data (beneficiary/victim + exit options) over nominal power level. A platform company's institutional power is high, but its beneficiary status and arbitrage exit option both push d downward; the platform experiences the constraint as advantageous. A powerless individual's trapped exit status pushes d upward regardless of other factors; they experience high extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by distinguishing agent perspectives. The constraint is NOT universally a Snare: it is a Snare from the victim perspectives (consuming public, epistemically vulnerable populations) and Rope from the beneficiary perspectives (platforms, creators). The mandatrophy is resolved by recognizing that an extractive constraint can appear as coordination to those who benefit from it. The false summit is the claim that misinformation production is inevitable (mountain) — the civilization-level analytical observer risks naturalizing what is actually a contingent institutional arrangement (engagement-maximization incentive). The institutional reality is that platforms and creators are not forced by physics or logic to maximize engagement over accuracy; they choose to because the business model rewards it. The constraint is a Snare because the choice is not symmetric: consumers have no vote in the algorithmic rules, while platforms have complete control. Mandatrophy resolution requires explicitly stating: this is extractive (Snare) from the perspective of those who bear costs and have no exit; it is coordinative (Rope) from the perspective of those who capture benefits and have options; the asymmetry defines it as a Snare overall.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    engagement_metric_causality,
    'Does high engagement necessarily correlate with misinformation production, or is engagement maximization decoupled from truth-accuracy alignment?',
    'Longitudinal analysis of platform engagement metrics correlated with fact-check veracity ratings; counterfactual testing whether reduced engagement amplification decreases misinformation spread without decreasing overall engagement',
    'If decoupled: misinformation production is not inherent to engagement optimization — platform incentive redesign could reduce suppression without coordination loss (Rope instead of Snare). If tightly coupled: engagement algorithms structurally produce misinformation (Snare confirmed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(engagement_metric_causality, empirical, 'Whether engagement maximization inherently produces misinformation or causality is contingent').

omega_variable(
    identity_lock_reversibility,
    'For agents who are identity-locked into misinformation narratives, what fraction of identity-lock is reversible through counter-evidence vs. irreversible through identity constitution?',
    'Intervention studies examining belief change post-exposure to high-quality counter-evidence; measurement of identity-fusion strength via multiple scales before and after exposure',
    'If highly reversible: identity-locked exit is actually constrained exit with lower barriers than assumed — classification shifts from mountain (biographical) to rope (biographical). If largely irreversible: identity-lock is a genuine structural trap requiring identity-transformation intervention rather than information intervention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_reversibility, empirical, 'Whether identity-lock into misinformation is reversible through evidence or irreversible through identity fusion').

omega_variable(
    platform_incentive_inevitability,
    'Are engagement-maximization incentives inherent to platform business models, or are they contingent institutional choices that alternative models could escape?',
    'Historical analysis of platform business model alternatives (subscription, public utility, cooperative); measurement of misinformation production under different incentive structures; economic modeling of platform sustainability under non-engagement-maximizing metrics',
    'If inevitable: misinformation production is a structural feature of digital platforms (Snare appears from all perspectives). If contingent: misinformation is institutional choice, not constraint — redesigned platforms could produce Rope or Mountain if operating under different incentives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(platform_incentive_inevitability, conceptual, 'Whether engagement-maximization is inherent to platform economics or contingent institutional choice').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (citizens accepting low-confidence beliefs without verification) structural (external barriers make verification impossible) or internalized (citizens have learned to distrust their own epistemic capacity)?',
    'Measurement of suppression persistence after removal of structural barriers (e.g., providing citizens with full access to expert fact-checking); comparison of citizens with high vs low epistemic self-efficacy holding constant information access',
    'If structural: removing algorithmic distortion and providing access to fact-checking lowers suppression. If internalized: suppression persists even with improved access — the damage from prior exposure has calcified into reduced epistemic agency. Affects long-term constraint severity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is structural information barriers or internalized epistemic helplessness').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(misinformation_production_incentive, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(misinf_tr_t0, misinformation_production_incentive, theater_ratio, 0, 0.42).
narrative_ontology:measurement(misinf_tr_t5, misinformation_production_incentive, theater_ratio, 5, 0.53).
narrative_ontology:measurement(misinf_tr_t10, misinformation_production_incentive, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(misinf_be_t0, misinformation_production_incentive, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(misinf_be_t5, misinformation_production_incentive, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(misinf_be_t10, misinformation_production_incentive, base_extractiveness, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(misinformation_production_incentive, information_standard).
narrative_ontology:affects_constraint(misinformation_production_incentive, algorithmic_filter_bubble).
narrative_ontology:affects_constraint(misinformation_production_incentive, attention_economy_rent_extraction).
narrative_ontology:affects_constraint(misinformation_production_incentive, epistemic_polarization).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(misinformation_production_incentive, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
