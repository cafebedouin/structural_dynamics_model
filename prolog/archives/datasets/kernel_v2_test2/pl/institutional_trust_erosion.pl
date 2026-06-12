% ============================================================================
% CONSTRAINT STORY: institutional_trust_erosion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_institutional_trust_erosion, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: institutional_trust_erosion
 *   human_readable: Institutional Trust Erosion in Democratic Systems
 *   domain: political_science/public_opinion/democratic_theory
 *
 * SUMMARY:
 *   Institutional trust erosion in democratic systems presents as a mountain
 *   — an apparently inevitable consequence of scale, complexity, and
 *   modernization — but exhibits structural features that suggest false
 *   summit dynamics. Survey data shows declining confidence across multiple
 *   dimensions: 44% express little/no confidence in the political system, 39%
 *   believe voting doesn't affect government, 53% say most people can't be
 *   trusted, and 75% believe elites don't understand their challenges. These
 *   trends appear independent of partisan affiliation or specific policy
 *   preferences, suggesting a structural rather than contingent phenomenon.
 *   However, the presence of identifiable beneficiaries (populist movements,
 *   alternative media platforms) who gain political opportunity from trust
 *   decline, combined with viable institutional alternatives (deliberative
 *   democracy, participatory mechanisms) that show trust-restoration
 *   potential, indicates the 'natural law' framing may naturalize contingent
 *   institutional failures. The constraint's low but rising extractiveness
 *   (0.08 → 0.15 over 40 years) and theater ratio (0.20 → 0.35) reflect
 *   gradual accumulation of rent-seeking behavior layered onto genuine
 *   institutional distance. The high accessibility collapse (0.88) reflects
 *   that citizens perceive few alternatives to declining trust — exit from
 *   the political system is not viable for most. The low resistance (0.12)
 *   reflects normalization — trust erosion has become an accepted feature of
 *   democratic life rather than something actively contested.
 *
 * KEY AGENTS:
 *   - Disengaged Citizen: Primary victim (powerless/trapped) — experiences trust erosion as immutable; no exit from political system; biographical time horizon makes alternatives invisible
 *   - Marginalized Community: Secondary victim (powerless/trapped) — trust erosion compounds existing exclusion; weaponized to justify further marginalization
 *   - Democratic Legitimacy: Abstract victim (powerless/trapped) — collective good that cannot organize or exit; bears full cost of institutional breakdown
 *   - Populist Entrepreneur: Primary beneficiary (institutional/arbitrage) — captures political opportunity from trust crisis; mobilizes anti-establishment sentiment
 *   - Alternative Media Platforms: Secondary beneficiary (institutional/arbitrage) — benefits from distrust of mainstream institutions; validates counter-narratives
 *   - Reform Advocate: Mixed position (moderate/constrained) — civic organizations see both genuine institutional failures and extractive crisis narratives
 *   - Democratic Innovation Network: Organized agents (organized/mobile) — building alternative legitimacy pathways with sunset logic
 *   - Legacy Political Party: Institutional actor (institutional/constrained) — maintains trust-restoration theater that has lost functional effectiveness
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements as inevitable modernization effects
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(institutional_trust_erosion, 0.15).
domain_priors:suppression_score(institutional_trust_erosion, 0.2).
domain_priors:theater_ratio(institutional_trust_erosion, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(institutional_trust_erosion, extractiveness, 0.15).
narrative_ontology:constraint_metric(institutional_trust_erosion, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(institutional_trust_erosion, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(institutional_trust_erosion, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(institutional_trust_erosion, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(institutional_trust_erosion, mountain).
narrative_ontology:human_readable(institutional_trust_erosion, "Institutional Trust Erosion in Democratic Systems").
narrative_ontology:topic_domain(institutional_trust_erosion, "political_science/public_opinion/democratic_theory").

domain_priors:emerges_naturally(institutional_trust_erosion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(institutional_trust_erosion, anti_establishment_movements).
narrative_ontology:constraint_beneficiary(institutional_trust_erosion, populist_political_entrepreneurs).
narrative_ontology:constraint_beneficiary(institutional_trust_erosion, alternative_media_platforms).
narrative_ontology:constraint_victim(institutional_trust_erosion, democratic_legitimacy).
narrative_ontology:constraint_victim(institutional_trust_erosion, policy_implementation_capacity).
narrative_ontology:constraint_victim(institutional_trust_erosion, civic_engagement_infrastructure).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(institutional_trust_erosion, populist_entrepreneurs).
narrative_ontology:constraint_beneficiary(institutional_trust_erosion, reform_advocates).
narrative_ontology:constraint_victim(institutional_trust_erosion, disengaged_citizens).
narrative_ontology:constraint_victim(institutional_trust_erosion, marginalized_communities).
narrative_ontology:constraint_victim(institutional_trust_erosion, reform_advocates).
narrative_ontology:constraint_vindicates(institutional_trust_erosion, elite_disconnect_thesis).
narrative_ontology:constraint_vindicates(institutional_trust_erosion, democratic_crisis_narrative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Experience declining institutional trust as an immutable feature of modern democracy. Cannot exit the political system. See no alternatives to institutional distance at biographical time horizons. Bear the costs of reduced civic efficacy and political alienation without benefiting from crisis narratives.
narrative_ontology:constraint_stakeholder(institutional_trust_erosion, disengaged_citizens, payer,
    powerless, biographical, trapped, national).

% Trust erosion compounds existing exclusion from political power. Declining institutional confidence weaponized to justify further marginalization ('they don't participate anyway'). No voice in reform processes, no exit options, and trust crisis used to delegitimize their claims.
narrative_ontology:constraint_stakeholder(institutional_trust_erosion, marginalized_communities, payer,
    powerless, immediate, trapped, local).

% Gain political opportunity from institutional distrust. Trust erosion validates anti-establishment critique and coordinates mobilization strategy. Can enter and exit political arenas strategically, leveraging crisis narratives for electoral advantage.
narrative_ontology:constraint_stakeholder(institutional_trust_erosion, populist_entrepreneurs, beneficiary,
    institutional, immediate, arbitrage, national).

% Benefit from distrust of mainstream institutions. Trust erosion drives audience to alternative information sources. Validate counter-narratives and conspiracy theories. Can pivot business models and content strategies based on trust dynamics.
narrative_ontology:constraint_stakeholder(institutional_trust_erosion, alternative_media_platforms, beneficiary,
    institutional, biographical, arbitrage, global).

% Civic organizations and reform movements experience trust erosion as both problem and opportunity. Genuine institutional failures create demand for reform advocacy, but crisis narratives also benefit political entrepreneurs who resist structural change. Constrained by resource limitations and institutional inertia.
narrative_ontology:constraint_stakeholder(institutional_trust_erosion, reform_advocates, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(institutional_trust_erosion, reform_advocates, beneficiary).

% Deliberative democracy practitioners, participatory budgeting advocates, and civic tech developers building alternative legitimacy pathways. See trust erosion as temporary coordination failure with sunset logic. Can move resources and attention across jurisdictions based on reform opportunities.
narrative_ontology:constraint_stakeholder(institutional_trust_erosion, democratic_innovation_network, agenda_setter,
    organized, generational, mobile, global).

% Maintain trust-building rituals (town halls, constituent services, party conventions) that have lost functional effectiveness. Performance persists through institutional inertia. Lack alternative legitimacy mechanisms but cannot easily exit traditional party structures.
narrative_ontology:constraint_stakeholder(institutional_trust_erosion, legacy_political_parties, agenda_setter,
    institutional, civilizational, constrained, national).

% Abstract collective good representing the legitimacy foundation of democratic governance. Cannot organize, cannot exit, bears full cost of institutional breakdown. Listed for narrative completeness as the ultimate victim of trust erosion, but excluded from beneficiary/victim derivation as a non-agent entity.
narrative_ontology:constraint_stakeholder(institutional_trust_erosion, democratic_legitimacy, payer,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(institutional_trust_erosion, democratic_legitimacy).

% Views trust erosion from civilizational/universal perspective. Risks naturalizing contingent institutional arrangements as inevitable consequences of scale and complexity. Neither collects from nor pays into the constraint directly, but analytical framing shapes how trust erosion is understood and addressed.
narrative_ontology:constraint_stakeholder(institutional_trust_erosion, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(institutional_trust_erosion, diffuse).
narrative_ontology:fixing_cost_class(institutional_trust_erosion, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: At the claimed level: none — trust erosion is presented as an inevitable structural feature of modern democracy, not a coordination mechanism. At the revealed level (if false summit confirmed): coordinates anti-establishment mobilization and validates alternative media business models by creating shared crisis narrative.
% TRANSFER_FUNCTION: Political opportunity and audience attention flow from mainstream institutions to populist entrepreneurs and alternative media platforms. Civic efficacy and democratic legitimacy flow away from disengaged citizens and marginalized communities. The transfer is diffuse rather than concentrated — no single actor captures all the gains, but the pattern systematically advantages anti-establishment actors.
% ABSENT_VOICES: Citizens who have exited political participation entirely (non-voters, non-respondents to surveys) are absent from the conversation about trust erosion. Their absence is both cause and consequence — they don't participate because they don't trust, and their non-participation is used to justify claims that trust erosion is universal and inevitable. Also absent: future generations who will inherit degraded democratic institutions but have no voice in current reform debates.
% DISAPPEARANCE_RATIONALE: If trust erosion disappeared overnight (institutional confidence restored to 1960s levels), the world would rearrange substantially: populist movements would lose mobilization fuel, alternative media platforms would lose audience, reform advocates would shift focus, and civic engagement patterns would change. However, the analytical observer's mountain view holds that trust erosion is an inevitable consequence of scale and complexity, suggesting the world would quickly revert to low-trust equilibrium. The verdict is contested because it depends on whether trust erosion is structural inevitability (mountain) or contingent institutional failure (false summit).
% FOUNDING_PROBLEM: Not applicable — trust erosion is not an arrangement built to solve a problem. It is presented as an emergent consequence of democratic modernization: increasing scale, complexity, and institutional distance between citizens and decision-makers. The 'founding problem' framing assumes intentional design, but trust erosion is claimed to arise naturally from structural features of large-scale representative democracy.
% FOUNDING_PROBLEM_CORROBORATION: The structural inevitability thesis is corroborated by political scientists studying democratic scale effects (Dahl's 'problem of scale', Putnam's social capital decline) and sociologists documenting modernization's atomizing effects (Putnam, Fukuyama). However, comparative political scientists studying institutional variation (Lijphart's consensus democracies, participatory budgeting outcomes) and deliberative democracy researchers (Fishkin, Ackerman & Fishkin) provide counter-evidence that trust levels are contingent on institutional design rather than inevitable. The corroboration is contested across disciplinary and theoretical lines.
narrative_ontology:disappearance_verdict(institutional_trust_erosion, contested).
narrative_ontology:founding_problem_status(institutional_trust_erosion, contested).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISENGAGED CITIZEN (MOUNTAIN) — Experiences trust erosion as an immutable feature of modern democracy. Cannot exit the political system; sees no alternatives to declining institutional confidence. Perceives the constraint as a natural law of contemporary governance — 'this is just how things are now.'
constraint_indexing:constraint_classification(institutional_trust_erosion, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REFORM ADVOCATE (TANGLED ROPE) — Civic organizations and reform movements experience trust erosion as both a coordination problem (genuine institutional failures need addressing) and an extraction mechanism (crisis narratives benefit political entrepreneurs). Constrained by resource limitations and institutional inertia, but sees pathways to reform.
constraint_indexing:constraint_classification(institutional_trust_erosion, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: POPULIST ENTREPRENEUR (ROPE) — Anti-establishment movements and populist political actors benefit from trust erosion. The constraint coordinates their messaging strategy and validates their critique of elites. Net beneficiary — trust erosion creates political opportunity and mobilization potential.
constraint_indexing:constraint_classification(institutional_trust_erosion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: DEMOCRATIC INNOVATION NETWORK (SCAFFOLD) — Deliberative democracy practitioners, participatory budgeting advocates, and civic tech developers see trust erosion as a temporary coordination failure with a sunset. New institutional forms (citizens' assemblies, sortition, digital participation platforms) are building alternative legitimacy pathways. Estimated sunset: 15-25 years for new democratic practices to mature and restore trust through direct participation.
constraint_indexing:constraint_classification(institutional_trust_erosion, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: MARGINALIZED COMMUNITY (SNARE) — Communities already excluded from political power experience trust erosion as pure extraction. Declining institutional confidence compounds existing barriers to representation. No exit options, no voice in reform processes, and trust erosion is weaponized to justify further exclusion ('they don't participate anyway').
constraint_indexing:constraint_classification(institutional_trust_erosion, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 6: LEGACY POLITICAL PARTY (PITON) — Traditional party organizations maintain trust-building rituals (town halls, constituent services, party conventions) that have lost functional effectiveness. The performance persists through institutional inertia — parties continue trust-restoration theater because they lack alternative legitimacy mechanisms, not because the rituals work.
constraint_indexing:constraint_classification(institutional_trust_erosion, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / STRUCTURAL INEVITABILITY VIEW (MOUNTAIN) — From a civilizational/universal perspective, trust erosion appears as an inevitable consequence of modernization, complexity, and scale. Large-scale representative democracy inherently generates distance between citizens and decision-makers; information asymmetry and institutional opacity are structural features of complex governance. This perspective naturalizes trust erosion as an immutable property of modern political systems. However, the presence of identifiable beneficiaries (populist entrepreneurs, alternative media) and the scaffold perspective's viable alternatives suggest this is a false summit — what appears as natural law may be a contingent institutional arrangement that benefits specific actors.
constraint_indexing:constraint_classification(institutional_trust_erosion, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(institutional_trust_erosion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(institutional_trust_erosion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(institutional_trust_erosion, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(institutional_trust_erosion, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(institutional_trust_erosion, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(institutional_trust_erosion, ExtMetricName, E),
    domain_priors:suppression_score(institutional_trust_erosion, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(institutional_trust_erosion),
    narrative_ontology:constraint_metric(institutional_trust_erosion, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(institutional_trust_erosion, resistance, R),
    AC >= 0.85,
    R =< 0.15.

test(piton_threshold) :-
    domain_priors:theater_ratio(institutional_trust_erosion, TR),
    TR >= 0.70.

:- end_tests(institutional_trust_erosion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.15): Low but non-negligible. Trust erosion creates political opportunity for anti-establishment actors and validates alternative media platforms, but the extraction is modest compared to more overtly extractive constraints. Much of what appears as extraction may be legitimate response to genuine institutional failures. The gradual increase (0.08 → 0.15) reflects accumulation of rent-seeking behavior over time. Suppression (0.20): Low-moderate. Citizens face barriers to institutional reform (complexity, coordination costs, incumbent resistance) but are not actively prevented from organizing or advocating change. The modest increase (0.15 → 0.20) reflects growing difficulty of reform as trust erosion becomes normalized. Theater ratio (0.35): Moderate. Traditional trust-building mechanisms (town halls, constituent services, transparency initiatives) increasingly performative as institutional distance grows, but not yet fully theatrical. The increase (0.20 → 0.35) tracks the gap between trust-restoration rituals and actual legitimacy recovery. Accessibility collapse (0.88): Very high. Citizens perceive few alternatives to declining institutional trust — exit from the political system is not viable, and the constraint appears as natural law of modern democracy. Resistance (0.12): Very low. Trust erosion has been normalized rather than actively contested; declining resistance (0.15 → 0.12) reflects acceptance of institutional distance as inevitable.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural phenomenon — declining institutional trust — appears as different constraint types depending on the observer's position. Disengaged citizens see an immutable natural law (Mountain) — trust erosion is just how modern democracy works, with no alternatives visible at biographical time horizons. Populist entrepreneurs see coordination (Rope) — trust erosion validates their critique and coordinates their mobilization strategy. The democratic innovation network sees a temporary problem with a sunset (Scaffold) — new participatory mechanisms are building alternative legitimacy pathways. Reform advocates see mixed coordination and extraction (Tangled Rope) — genuine institutional failures coexist with extractive crisis narratives. Marginalized communities see pure extraction (Snare) — trust erosion compounds their exclusion and is weaponized against them. Legacy parties see degraded ritual (Piton) — trust-building mechanisms persist through inertia despite losing functional effectiveness. The analytical observer risks seeing structural inevitability (Mountain) — trust erosion as an immutable consequence of scale and complexity — but the presence of beneficiaries and viable alternatives suggests this is a false summit. The perspectival gap is not 'which type is correct?' but 'which structural position are you measuring from?' The constraint's classification is indexical to observer position.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from structural position relative to trust erosion. Populist entrepreneurs and alternative media platforms are beneficiaries — they gain political opportunity, audience, and legitimacy from institutional distrust. Their arbitrage exit options and institutional power produce low directionality values (d ≈ 0.1-0.2), resulting in negative or near-zero effective extraction (they benefit from the constraint). Disengaged citizens and marginalized communities are victims — they bear the costs of institutional breakdown without benefiting from crisis narratives. Their trapped exit options and powerless position produce high directionality values (d ≈ 0.8-0.9), resulting in maximum effective extraction. Reform advocates occupy a mixed position — they experience both genuine coordination problems (institutional failures need addressing) and extraction (crisis narratives benefit political entrepreneurs), producing moderate directionality (d ≈ 0.4-0.5). The democratic innovation network's organized power and mobile exit options produce low directionality (d ≈ 0.2-0.3) — they see pathways to reform and are building alternatives. Legacy political parties' constrained position produces moderate directionality (d ≈ 0.5) — they maintain performative trust-building rituals but lack effective alternatives. The analytical observer's mountain classification is perspectival — the false summit detector will identify this as naturalization of contingent institutional arrangements.
 *
 * MANDATROPHY ANALYSIS:
 *   FALSE SUMMIT CANDIDATE: This constraint resolves the mandatrophy by revealing that the 'natural law' framing naturalizes contingent institutional arrangements. The analytical observer's mountain classification appears plausible — trust erosion seems like an inevitable consequence of democratic scale, complexity, and modernization. High accessibility collapse (0.88) and low resistance (0.12) support the mountain hypothesis. However, three structural features contradict the natural law claim: (1) Identifiable beneficiaries exist — populist entrepreneurs and alternative media platforms gain political opportunity from trust decline, suggesting the constraint is not a neutral feature of reality but an arrangement that benefits specific actors. (2) Viable alternatives exist — deliberative democracy innovations, participatory budgeting, and citizens' assemblies show trust-restoration potential, indicating trust erosion is not structurally inevitable. (3) Extraction is accumulating — the gradual increase in extractiveness (0.08 → 0.15) and theater ratio (0.20 → 0.35) reflects rent-seeking behavior layered onto genuine institutional distance. The false summit detector should flag this constraint: a claimed mountain with declared beneficiaries, rising extraction over time, and a scaffold perspective showing viable alternatives. The 'structural inevitability' framing serves the interests of actors who benefit from institutional distrust while obscuring the contingent institutional failures that could be addressed through reform. The mandatrophy is resolved not by choosing between mountain and snare, but by recognizing that the mountain classification is itself a perspectival artifact — what appears as natural law from the trapped citizen's position is revealed as extractive institutional arrangement from the analytical cross-position view.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    structural_vs_contingent_erosion,
    'Is trust erosion an inevitable consequence of democratic scale and complexity (mountain), or a contingent outcome of specific institutional failures that could be addressed through reform (tangled_rope/scaffold)?',
    'Cross-national comparison of trust trajectories in democracies with different institutional designs; longitudinal analysis of trust recovery following institutional reforms (e.g., New Zealand''s electoral reform, participatory budgeting in Porto Alegre); experimental evidence from deliberative democracy interventions',
    'If structural/inevitable: mountain classification confirmed across all perspectives. If contingent: false summit detected — the ''natural law'' framing naturalizes extractive institutional arrangements that benefit anti-establishment actors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_vs_contingent_erosion, empirical, 'Whether trust erosion is structural inevitability or contingent institutional failure').

omega_variable(
    beneficiary_causation_direction,
    'Do populist entrepreneurs and alternative media platforms benefit from pre-existing trust erosion (opportunistic), or do they actively cause/accelerate erosion to create political opportunity (extractive)?',
    'Causal inference analysis: timing of trust decline relative to populist mobilization; content analysis of populist messaging for trust-undermining vs trust-reflecting rhetoric; experimental studies of exposure to anti-institutional messaging on trust levels',
    'If opportunistic: beneficiaries are responding to genuine structural change (mountain more plausible). If causal: beneficiaries are extracting by manufacturing crisis (snare/tangled_rope more plausible, false summit confirmed).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_causation_direction, empirical, 'Direction of causation between beneficiaries and trust erosion').

omega_variable(
    democratic_innovation_effectiveness,
    'Do participatory democracy innovations (citizens'' assemblies, deliberative polling, participatory budgeting) actually restore institutional trust at scale, or do they work only in small-scale/self-selected contexts?',
    'Meta-analysis of trust outcomes from deliberative democracy experiments; longitudinal tracking of trust levels in jurisdictions that have institutionalized participatory mechanisms; comparison of trust trajectories between early adopters and non-adopters of democratic innovations',
    'If effective at scale: scaffold perspective confirmed — sunset is real and trust erosion is temporary. If effective only in limited contexts: scaffold is aspirational rather than structural, and trust erosion may be more persistent (mountain or piton more plausible).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_innovation_effectiveness, empirical, 'Whether democratic innovations restore trust at scale').

omega_variable(
    trust_measurement_stability,
    'Do survey measures of institutional trust capture a stable underlying construct, or are they sensitive to question framing, recent events, and partisan identity in ways that confound structural trends?',
    'Psychometric analysis of trust measures across survey instruments and time periods; experimental manipulation of question framing and context; comparison of explicit trust measures with behavioral indicators (voting turnout, civic participation, compliance with government directives)',
    'If stable construct: observed erosion reflects genuine structural change. If measurement artifact: apparent erosion may partly reflect survey methodology changes, partisan polarization effects, or negativity bias in self-reporting rather than actual institutional breakdown.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(trust_measurement_stability, empirical, 'Stability and validity of trust measurement instruments').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(institutional_trust_erosion, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trust_erosion_tr_t0, institutional_trust_erosion, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(trust_erosion_tr_t0, observed).
narrative_ontology:measurement(trust_erosion_tr_t10, institutional_trust_erosion, theater_ratio, 10, 0.24).
narrative_ontology:measurement_basis(trust_erosion_tr_t10, observed).
narrative_ontology:measurement(trust_erosion_tr_t20, institutional_trust_erosion, theater_ratio, 20, 0.28).
narrative_ontology:measurement_basis(trust_erosion_tr_t20, observed).
narrative_ontology:measurement(trust_erosion_tr_t30, institutional_trust_erosion, theater_ratio, 30, 0.32).
narrative_ontology:measurement_basis(trust_erosion_tr_t30, observed).
narrative_ontology:measurement(trust_erosion_tr_t40, institutional_trust_erosion, theater_ratio, 40, 0.35).
narrative_ontology:measurement_basis(trust_erosion_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(trust_erosion_be_t0, institutional_trust_erosion, base_extractiveness, 0, 0.08).
narrative_ontology:measurement_basis(trust_erosion_be_t0, observed).
narrative_ontology:measurement(trust_erosion_be_t10, institutional_trust_erosion, base_extractiveness, 10, 0.1).
narrative_ontology:measurement_basis(trust_erosion_be_t10, observed).
narrative_ontology:measurement(trust_erosion_be_t20, institutional_trust_erosion, base_extractiveness, 20, 0.12).
narrative_ontology:measurement_basis(trust_erosion_be_t20, observed).
narrative_ontology:measurement(trust_erosion_be_t30, institutional_trust_erosion, base_extractiveness, 30, 0.14).
narrative_ontology:measurement_basis(trust_erosion_be_t30, observed).
narrative_ontology:measurement(trust_erosion_be_t40, institutional_trust_erosion, base_extractiveness, 40, 0.15).
narrative_ontology:measurement_basis(trust_erosion_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(trust_erosion_su_t0, institutional_trust_erosion, suppression_requirement, 0, 0.15).
narrative_ontology:measurement_basis(trust_erosion_su_t0, observed).
narrative_ontology:measurement(trust_erosion_su_t20, institutional_trust_erosion, suppression_requirement, 20, 0.18).
narrative_ontology:measurement_basis(trust_erosion_su_t20, observed).
narrative_ontology:measurement(trust_erosion_su_t40, institutional_trust_erosion, suppression_requirement, 40, 0.2).
narrative_ontology:measurement_basis(trust_erosion_su_t40, observed).

% Leveled coercion grid (OQ-93): 12/32 authored points at t0=0, tn=40
narrative_ontology:measurement(trust_erosion_grid_01, institutional_trust_erosion, accessibility_collapse(organizational), 0, 0.65).
narrative_ontology:measurement_basis(trust_erosion_grid_01, observed).
narrative_ontology:measurement(trust_erosion_grid_02, institutional_trust_erosion, accessibility_collapse(organizational), 40, 0.72).
narrative_ontology:measurement_basis(trust_erosion_grid_02, observed).
narrative_ontology:measurement(trust_erosion_grid_03, institutional_trust_erosion, accessibility_collapse(structural), 0, 0.82).
narrative_ontology:measurement_basis(trust_erosion_grid_03, observed).
narrative_ontology:measurement(trust_erosion_grid_04, institutional_trust_erosion, accessibility_collapse(structural), 40, 0.88).
narrative_ontology:measurement_basis(trust_erosion_grid_04, observed).
narrative_ontology:measurement(trust_erosion_grid_05, institutional_trust_erosion, resistance(organizational), 0, 0.28).
narrative_ontology:measurement_basis(trust_erosion_grid_05, observed).
narrative_ontology:measurement(trust_erosion_grid_06, institutional_trust_erosion, resistance(organizational), 40, 0.22).
narrative_ontology:measurement_basis(trust_erosion_grid_06, observed).
narrative_ontology:measurement(trust_erosion_grid_07, institutional_trust_erosion, resistance(structural), 0, 0.15).
narrative_ontology:measurement_basis(trust_erosion_grid_07, observed).
narrative_ontology:measurement(trust_erosion_grid_08, institutional_trust_erosion, resistance(structural), 40, 0.12).
narrative_ontology:measurement_basis(trust_erosion_grid_08, observed).
narrative_ontology:measurement(trust_erosion_grid_09, institutional_trust_erosion, suppression(class), 0, 0.25).
narrative_ontology:measurement_basis(trust_erosion_grid_09, observed).
narrative_ontology:measurement(trust_erosion_grid_10, institutional_trust_erosion, suppression(class), 40, 0.3).
narrative_ontology:measurement_basis(trust_erosion_grid_10, observed).
narrative_ontology:measurement(trust_erosion_grid_11, institutional_trust_erosion, suppression(structural), 0, 0.18).
narrative_ontology:measurement_basis(trust_erosion_grid_11, observed).
narrative_ontology:measurement(trust_erosion_grid_12, institutional_trust_erosion, suppression(structural), 40, 0.22).
narrative_ontology:measurement_basis(trust_erosion_grid_12, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(institutional_trust_erosion, identity_coordination).
narrative_ontology:affects_constraint(institutional_trust_erosion, electoral_participation_decline).
narrative_ontology:affects_constraint(institutional_trust_erosion, civic_engagement_atrophy).
narrative_ontology:affects_constraint(institutional_trust_erosion, policy_implementation_capacity_erosion).

% DUAL FORMULATION NOTE:
% Institutional trust erosion is upstream of multiple downstream constraints in democratic systems. Electoral participation decline, civic engagement atrophy, and policy implementation capacity erosion are all affected by trust levels, but each has its own extractiveness value reflecting distinct structural dynamics. Trust erosion represents the perceptual/attitudinal constraint; the downstream constraints represent behavioral and institutional consequences.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(institutional_trust_erosion, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
