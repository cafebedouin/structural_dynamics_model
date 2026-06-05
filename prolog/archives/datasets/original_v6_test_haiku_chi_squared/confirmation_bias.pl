% ============================================================================
% CONSTRAINT STORY: confirmation_bias
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_confirmation_bias, []).

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
 *   constraint_id: confirmation_bias
 *   human_readable: Confirmation Bias (Socially Amplified)
 *   domain: social/cognitive/technological
 *
 * SUMMARY:
 *   Confirmation bias as a socially amplified constraint exhibits a dual
 *   nature: it appears to be a cognitive feature (immutable pattern of human
 *   perception) while functioning as an extraction mechanism (selective
 *   filtering of reality-correcting information in networked systems). The
 *   constraint's severity has increased dramatically with digital media and
 *   algorithmic amplification — the base extractiveness grew from ~0.30
 *   (pre-internet conditions where social bubbles were geographic and
 *   family-based) to 0.58 (current platform-mediated environment with
 *   algorithmic personalization). The theater ratio rose from 0.42 to 0.68,
 *   indicating that institutions nominally designed to counter confirmation
 *   bias (peer review, fact-checking, journalism) have become increasingly
 *   performative as confirmation bias has invaded their own structures. The
 *   constraint demonstrates how individual cognitive biases become extraction
 *   mechanisms when embedded in systems with (a) attention economics, (b)
 *   homophily networks, (c) algorithmic feedback loops, and (d) institutional
 *   capture. Belief holders experience confirmation bias as a beneficial
 *   coordination mechanism — reducing cognitive load, enabling social
 *   belonging — while simultaneously being extracted from (prevented from
 *   accessing reality-correcting information). Information intermediaries
 *   experience it as pure coordination (matching user priors to content
 *   optimizes engagement). The epistemic commons is a trapped victim with no
 *   exit mechanism.
 *
 * KEY AGENTS:
 *   - Individual Belief Holders: Primary beneficiaries (powerful/mobile) — experience reduced cognitive load and social cohesion benefits; also bear extraction costs through false belief persistence
 *   - Information Intermediaries (Platforms/Algorithms): Primary extractors (institutional/arbitrage) — capture attention surplus through belief-confirmation matching; coordinate user retention with advertiser demand
 *   - Reality-Correcting Information: Primary victim (powerless/trapped) — facts contradicting prior beliefs are systematically suppressed, delayed, and reinterpreted; no mechanism for exit or advocacy
 *   - Epistemic Commons (Aggregate Truth-Tracking): Secondary victim (powerless/trapped) — contaminated by persistent false beliefs; collective ability to track reality degrades as confirmation bias spreads
 *   - Minority Perspectives: Secondary victim (moderate/constrained) — face systematic search, interpretation, and publication disadvantages; constrained by homophily and algorithmic suppression
 *   - Academic/Fact-Checking Institutions: Institutional gatekeepers (institutional/arbitrage) — nominally designed to counter confirmation bias but increasingly captured; their peer review and fact-checking processes are themselves contaminated by confirmation bias
 *   - Reality-Correction Coalition: Organized agents (organized/mobile) — fact-checkers, science communicators, open-science advocates; experience mixed coordination (shared truth-seeking standards) and extraction (forced to work against ambient social dynamics)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(confirmation_bias, 0.58).
domain_priors:suppression_score(confirmation_bias, 0.65).
domain_priors:theater_ratio(confirmation_bias, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(confirmation_bias, extractiveness, 0.58).
narrative_ontology:constraint_metric(confirmation_bias, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(confirmation_bias, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(confirmation_bias, tangled_rope).
narrative_ontology:human_readable(confirmation_bias, "Confirmation Bias (Socially Amplified)").
narrative_ontology:topic_domain(confirmation_bias, "social/cognitive/technological").

domain_priors:requires_active_enforcement(confirmation_bias).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(confirmation_bias, belief_holders).
narrative_ontology:constraint_beneficiary(confirmation_bias, attention_extractors).
narrative_ontology:constraint_beneficiary(confirmation_bias, ideological_entrepreneurs).
narrative_ontology:constraint_victim(confirmation_bias, epistemic_commons).
narrative_ontology:constraint_victim(confirmation_bias, marginal_perspectives).
narrative_ontology:constraint_victim(confirmation_bias, reality_correction_mechanisms).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FACTUAL REALITY / TRAPPED OBSERVER (SNARE) — Events and facts that contradict a belief holder's prior conviction have no advocate and no exit mechanism from suppression. Reality-correcting information is systematically filtered, delayed, or reinterpreted. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.98. Maximum extraction from the epistemic commons.
constraint_indexing:constraint_classification(confirmation_bias, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MINORITY VIEWPOINT / DISSENTING INFORMATION (SNARE) — Perspectives that contradict the dominant belief face systematic search, interpretation, and recall disadvantages. Constrained by publication bias, algorithmic suppression, and social sanctions against belief contradiction. d≈0.85, f(d)≈1.15, σ=1.0 → χ≈0.67.
constraint_indexing:constraint_classification(confirmation_bias, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INDIVIDUAL BELIEF HOLDER (TANGLED ROPE) — Benefits from confirmation bias as a cognitive coordination mechanism (reduces cognitive load, enables social cohesion with in-group). Also bears extraction costs through reduced truth-tracking and vulnerability to false information. Mobile exit (can update beliefs) but psychologically costly. d≈0.50, f(d)≈0.65, σ=0.9 → χ≈0.34.
constraint_indexing:constraint_classification(confirmation_bias, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 4: INFORMATION INTERMEDIARY (ROPE) — Social media platforms, news aggregators, recommendation algorithms. Experience confirmation bias as a pure coordination mechanism: matching user beliefs to content reduces friction, increases engagement, optimizes network effects. d≈0.10, f(d)≈0.02, σ=1.2 → χ≈0.01. Net beneficiary through arbitrage between user attention and advertiser demand.
constraint_indexing:constraint_classification(confirmation_bias, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ACADEMIC EPISTEMIC SYSTEM (PITON) — Peer review, citation networks, and journal gatekeeping were designed as safeguards against confirmation bias. Now substantially degraded and performative: peer reviewers often confirm their own priors, citation patterns amplify trendy claims, replication failures are buried. Theater_ratio=0.68 reflects that the institutional process of 'checking' has become largely theatrical. Maintained through prestige inertia rather than epistemic function. d≈0.08, f(d)≈-0.07, σ=1.0 → χ≈-0.04.
constraint_indexing:constraint_classification(confirmation_bias, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: REALITY-CORRECTION COALITION (TANGLED ROPE) — Fact-checkers, science communicators, open-science advocates. Organized agents with genuine exit (can build alternative epistemic pathways). Experience the constraint as mixed coordination (truth-seeking networks benefit from shared standards) and extraction (confirmation bias forces them to work against ambient social dynamics). d≈0.55, f(d)≈0.75, σ=1.2 → χ≈0.52.
constraint_indexing:constraint_classification(confirmation_bias, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: COGNITIVE NEUROSCIENCE OBSERVER (MOUNTAIN) — From a neuroscientific perspective, confirmation bias is an immutable feature of how human pattern-recognition systems work: predictive processing requires priors, and priors constrain perception. But the structural data (ε=0.58, suppression=0.65, theater=0.68) contradicts mountain classification — the base extraction is far too high. This perspective risks naturalizing the contingent social amplification (algorithms, media incentives, homophily) as inherent neurobiology.
constraint_indexing:constraint_classification(confirmation_bias, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(confirmation_bias_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(confirmation_bias, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(confirmation_bias, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(confirmation_bias, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(confirmation_bias, TR),
    TR >= 0.70.

:- end_tests(confirmation_bias_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts by preventing access to reality-correcting information. The extraction is not maximal (snare level >0.66) because belief holders retain some ability to update when evidence becomes sufficiently overwhelming and social costs drop. But the extraction is substantial — most people accept new information at rates far below what rational belief revision would predict. The rise from 0.30 (pre-internet) to 0.58 (platform era) reflects algorithmic amplification: personalized feeds have increased the selection pressure for belief-confirming content. Suppression (0.65): High. Multiple layers suppress reality-correction: (1) cognitive (confirmation bias itself), (2) social (homophily in networks), (3) algorithmic (recommendation systems optimize for engagement, which correlates with belief confirmation), (4) institutional (peer review captures priors, fact-checkers lack reach). Theater ratio (0.68): High. Peer review, fact-checking, and journalism were designed as institutional corrections for confirmation bias. They are now substantially performative: peer reviewers confirm their own priors, fact-checkers reach only small fractions of false-belief holders, journalism fragments into tribal outlets that confirm tribal priors. The theater increased from 0.42 to 0.68 as platforms replaced gatekeepers — the check-and-balance institutions lost structural power while maintaining prestige appearances.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits a sharp perspectival gap between benefit and harm. The individual belief holder experiences confirmation bias as net beneficial (Tangled Rope) — genuine cognitive coordination benefits with extraction costs. The information intermediary experiences it as pure coordination (Rope) — no costs, only attention optimization. The epistemic commons and reality-correcting information experience it as pure extraction (Snare) — maximum suppression, no exit, no benefit. The minority perspective experiences it as extraction with constrained mobility (Snare). The academic/fact-checking system sees its own degradation (Piton) — the institutional process is maintained through inertia despite low epistemic function. The reality-correction coalition sees mixed coordination and extraction (Tangled Rope) — they work within truth-seeking norms (coordination) but against ambient confirmation bias (extraction). The cognitive neuroscience observer risks seeing an immutable natural law (Mountain) — but the structural data reveals this as a false summit. The rise in extractiveness from 0.30 to 0.58 shows that confirmation bias is NOT immutable; it is amplified by social systems.
 *
 * DIRECTIONALITY LOGIC:
 *   Belief holders: Mixed (moderate/mobile) → d≈0.50, f(d)≈0.65. They are both beneficiaries (cognitive load reduction, social cohesion) and victims (prevented from reality-tracking). Information intermediaries: Beneficiary (institutional/arbitrage) → d≈0.10, f(d)≈0.02. Net extractors. Reality-correcting information: Victim (powerless/trapped) → d≈0.95, f(d)≈1.42. Maximum extraction. Minority perspectives: Victim (moderate/constrained) → d≈0.85, f(d)≈1.15. Significant extraction. Academic institutions: Nominal safeguard (institutional/arbitrage) → d≈0.08, f(d)≈-0.07. But piton classification (theater_ratio=0.68) indicates the safeguard is degraded. Reality-correction coalition: Organized agents with mixed role (organized/mobile) → d≈0.55, f(d)≈0.75. Experience both the coordination benefits of truth-seeking networks and the extraction pressure of confirmation-biased populations.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: Confirmation bias is neither pure coordination (Rope) nor pure extraction (Snare). It is a hybrid where the same mechanism benefits belief holders (reduced cognitive load = genuine coordination) while harming the epistemic commons (suppressed reality-correction = extraction). The mandatrophy is resolved by recognizing that the constraint operates at different scales: at the individual level, it provides coordination benefits (load reduction, group cohesion); at the population level, it creates extraction (suppressed truth-tracking, contaminated epistemic commons). The rise in base_extractiveness from 0.30 to 0.58 indicates that social amplification has pushed the constraint toward the Snare end of the spectrum — institutional mechanisms (platforms, algorithms, media) have weaponized individual confirmation bias into a coordinated extraction mechanism. The theater_ratio rise (0.42→0.68) shows that counter-institutions (peer review, fact-checking) have become performative rather than functional, unable to correct confirmation bias at scale. The constraint is Tangled Rope (not Snare) because: (1) genuine coordination functions remain (truth-seeking communities, scientific norms), (2) belief holders retain some exit capacity (costly but possible belief updating), and (3) institutional corrections partially function (some false beliefs are corrected, just slowly). The theater does not yet exceed 0.70, and suppression is not total (0.65, not 0.95).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cognitive_vs_social_amplification,
    'Is confirmation bias primarily an irreducible cognitive constraint or a socially amplified extraction mechanism? Does the individual bias exist in isolation, or only in systems with feedback loops?',
    'Longitudinal studies of belief revision in isolated vs socially-connected agents; comparison of confirmation bias severity in pre-internet vs platform-mediated communication; experimental manipulation of social amplification while holding individual bias constant',
    'If primarily cognitive: classification approaches Mountain from most perspectives. If primarily social-amplified: classification approaches Snare across institutional perspectives. The epsilon value (0.58) assumes social amplification; if purely cognitive, ε would drop to ~0.20.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cognitive_vs_social_amplification, empirical, 'Cognitive constraint vs. socially amplified extraction').

omega_variable(
    algorithm_amplification_vs_user_preference,
    'Do recommendation algorithms amplify confirmation bias because they must optimize for user retention (natural consequence of economics) or because engineers deliberately select for belief-confirming content (intentional extraction)?',
    'Algorithmic audits measuring content diversity in randomized vs optimized feeds; interviews with platform engineers on design priorities; comparison of recommendation diversity across platforms with different business models',
    'If economic optimization: constraint is Rope (coordination between user attention and platform incentives). If deliberate extraction: constraint is Snare (deliberate suppression of reality-correcting information). Current intermediary perspective assumes Rope; if evidence supports deliberate extraction, classification upgrades to Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithm_amplification_vs_user_preference, empirical, 'Whether algorithmic amplification is optimization or extraction').

omega_variable(
    belief_updating_cost_threshold,
    'What psychological and social cost threshold determines when belief updating becomes irrational (extraction) vs rational (coordination)? When is confirmation bias adaptive vs extractive?',
    'Studies of belief revision costs across domains; correlation between updating costs and accuracy improvements; analysis of which belief domains show net benefit from confirmation vs net harm',
    'If most domains show net harm from confirmation bias: Snare classification dominates. If significant domains show adaptive value: Tangled Rope classification is more robust. The current assessment assumes net harm (ε=0.58); if evidence supports significant adaptive value, ε drops to ~0.30.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(belief_updating_cost_threshold, empirical, 'Psychological cost threshold for rational belief updating').

omega_variable(
    reality_correction_institutional_capacity,
    'Can institutions (fact-checkers, science communication, open science) actually correct for confirmation bias at population scale, or is the institutional correction mechanism itself captured by confirmation bias?',
    'Longitudinal studies of belief change following fact-checking; measurement of fact-checker reach and persuasiveness by prior belief; analysis of whether fact-checking institutions have their own confirmation bias against certain perspectives',
    'If institutional correction succeeds: Scaffold perspective is viable (sunset as correction mechanisms mature). If correction fails: constraint is Snare with no exit (correction mechanisms are illusory). This determines whether theater_ratio decline is possible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reality_correction_institutional_capacity, empirical, 'Whether institutional correction mechanisms can succeed at scale').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(confirmation_bias, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(confbias_tr_t0, confirmation_bias, theater_ratio, 0, 0.42).
narrative_ontology:measurement(confbias_tr_t5, confirmation_bias, theater_ratio, 5, 0.56).
narrative_ontology:measurement(confbias_tr_t10, confirmation_bias, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(confbias_be_t0, confirmation_bias, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(confbias_be_t5, confirmation_bias, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(confbias_be_t10, confirmation_bias, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(confirmation_bias, information_standard).
narrative_ontology:affects_constraint(confirmation_bias, epistemic_fragmentation).
narrative_ontology:affects_constraint(confirmation_bias, tribal_belief_polarization).
narrative_ontology:affects_constraint(confirmation_bias, institutional_capture).

% DUAL FORMULATION NOTE:
% Confirmation bias decomposes into two distinct constraints: (1) Cognitive Confirmation Bias (ε≈0.20, Mountain) — the irreducible pattern of how human pattern-recognition systems process information; and (2) Socially Amplified Confirmation Bias (ε=0.58, Tangled Rope) — the extraction mechanism created when individual bias is embedded in platforms with attention economics and algorithmic feedback. This story addresses the socially amplified form. The cognitive form is a downstream constraint modeling the neuroscience perspective.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(confirmation_bias, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
