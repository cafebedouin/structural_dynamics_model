% ============================================================================
% CONSTRAINT STORY: institutional_trust_erosion_c0
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_institutional_trust_erosion_c0, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: institutional_trust_erosion_c0
 *   human_readable: Institutional Trust Erosion in Democratic Systems
 *   domain: political_science/public_opinion/democratic_theory
 *
 * SUMMARY:
 *   Institutional trust erosion in democratic systems is claimed as a
 *   mountain — an inevitable consequence of scale, complexity, and
 *   modernization in advanced democracies. Survey data shows systematic
 *   decline: 44% express little/no confidence in political systems, 39%
 *   believe voting doesn't affect government, 53% say most people can't be
 *   trusted, 75% believe elites don't understand their challenges. These
 *   patterns appear independent of partisan affiliation or specific policy
 *   outcomes, suggesting structural rather than contingent causation.
 *   However, the presence of identifiable beneficiaries (populist movements,
 *   anti-establishment media, alternative authority structures) who gain
 *   power and resources directly from trust erosion, combined with rising
 *   extractiveness over time and cross-national variation unexplained by
 *   structural factors, suggests false summit dynamics. The claim/metric
 *   independence is deliberate: the constraint is CLAIMED as mountain (the
 *   dominant framing in political science and public discourse) while the
 *   metrics describe accumulating extraction and beneficiary concentration —
 *   the engine measures that divergence.
 *
 * KEY AGENTS:
 *   - median_voters: Primary targets (moderate/identity_locked) — bear costs of institutional dysfunction while remaining psychologically bound to democratic participation
 *   - established_democratic_institutions: Dual-positioned (institutional/constrained) — administer the system while experiencing legitimacy erosion that undermines their capacity
 *   - populist_movements: Primary beneficiaries (organized/mobile) — gain mobilization capacity and political power directly from trust decline
 *   - anti_establishment_media: Secondary beneficiaries (organized/mobile) — capture audience share as institutional credibility erodes
 *   - alternative_authority_structures: Secondary beneficiaries (organized/mobile) — fill legitimacy vacuum with competing frameworks
 *   - marginalized_communities: Excluded voices (powerless/trapped) — experience rational distrust from historical exclusion but are absent from erosion discourse
 *   - social_capital_researchers: Analytical observers — document patterns without direct stake in outcomes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(institutional_trust_erosion_c0, 0.68).
domain_priors:suppression_score(institutional_trust_erosion_c0, 0.52).
domain_priors:theater_ratio(institutional_trust_erosion_c0, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(institutional_trust_erosion_c0, extractiveness, 0.68).
narrative_ontology:constraint_metric(institutional_trust_erosion_c0, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(institutional_trust_erosion_c0, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(institutional_trust_erosion_c0, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(institutional_trust_erosion_c0, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(institutional_trust_erosion_c0, mountain).
narrative_ontology:human_readable(institutional_trust_erosion_c0, "Institutional Trust Erosion in Democratic Systems").
narrative_ontology:topic_domain(institutional_trust_erosion_c0, "political_science/public_opinion/democratic_theory").

domain_priors:emerges_naturally(institutional_trust_erosion_c0).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(institutional_trust_erosion_c0, populist_movements).
narrative_ontology:constraint_beneficiary(institutional_trust_erosion_c0, anti_establishment_media).
narrative_ontology:constraint_beneficiary(institutional_trust_erosion_c0, alternative_authority_structures).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(institutional_trust_erosion_c0, median_voters).
narrative_ontology:constraint_victim(institutional_trust_erosion_c0, established_democratic_institutions).
narrative_ontology:constraint_vindicates(institutional_trust_erosion_c0, elite_disconnect_thesis).
narrative_ontology:constraint_vindicates(institutional_trust_erosion_c0, scale_complexity_inevitability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Experience declining confidence in democratic institutions (44% express little/no confidence in political system, 39% believe voting doesn't affect government). Their civic identity is fused with democratic participation, making exit psychologically costly even as efficacy beliefs erode. They bear the costs of institutional dysfunction through policy failures and representation gaps while remaining structurally bound to the system.
narrative_ontology:constraint_stakeholder(institutional_trust_erosion_c0, median_voters, payer,
    moderate, biographical, identity_locked, national).

% Administer democratic processes while experiencing legitimacy erosion. They set procedural rules and maintain formal authority structures, but their capacity to govern effectively diminishes as trust declines. The erosion creates a self-reinforcing cycle: reduced trust leads to reduced compliance, which leads to reduced effectiveness, which further reduces trust.
narrative_ontology:constraint_stakeholder(institutional_trust_erosion_c0, established_democratic_institutions, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(institutional_trust_erosion_c0, established_democratic_institutions, agenda_setter).

% Gain political power and mobilization capacity directly from institutional trust erosion. They frame the erosion as validation of their critique of elite disconnect (75% believe elites don't understand their challenges). Their electoral success and fundraising capacity scale with trust metrics decline. They benefit whether the erosion is natural or constructed, creating incentives to amplify rather than repair trust gaps.
narrative_ontology:constraint_stakeholder(institutional_trust_erosion_c0, populist_movements, beneficiary,
    organized, biographical, mobile, national).

% Capture audience share and revenue as trust in mainstream institutions declines. Their business model depends on framing institutional dysfunction as evidence of systemic corruption or elite conspiracy. They amplify trust-eroding narratives regardless of empirical basis, because audience engagement tracks with institutional delegitimization.
narrative_ontology:constraint_stakeholder(institutional_trust_erosion_c0, anti_establishment_media, beneficiary,
    organized, biographical, mobile, national).

% Fill the legitimacy vacuum created by institutional trust erosion. These include religious organizations, ethnic solidarity networks, conspiracy theory communities, and ideological movements that offer alternative frameworks for understanding social reality. They gain adherents, resources, and social influence as democratic institutions lose credibility.
narrative_ontology:constraint_stakeholder(institutional_trust_erosion_c0, alternative_authority_structures, beneficiary,
    organized, generational, mobile, regional).

% Study trust erosion patterns across democracies, measuring trends in social capital, institutional confidence, and civic engagement. They document the phenomenon without direct stake in its persistence or resolution, though their findings are weaponized by various actors to support competing narratives about causation.
narrative_ontology:constraint_stakeholder(institutional_trust_erosion_c0, social_capital_researchers, observer,
    analytical, generational, analytical, global).

% Experience institutional distrust as rational response to historical exclusion and ongoing discrimination, but their voices are absent from mainstream trust-erosion discourse. The general trust decline is framed as a new crisis affecting everyone equally, erasing the fact that some communities never had institutional trust to lose. Their structural exclusion from the conversation allows the erosion narrative to be captured by actors who benefit from it.
narrative_ontology:constraint_stakeholder(institutional_trust_erosion_c0, marginalized_communities, excluded,
    powerless, biographical, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(institutional_trust_erosion_c0, populist_movements).
narrative_ontology:fixing_cost_class(institutional_trust_erosion_c0, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Democratic institutions coordinate collective decision-making, peaceful power transitions, and rule-of-law enforcement across large-scale diverse populations. Trust is the coordination mechanism that makes compliance voluntary rather than coerced.
% TRANSFER_FUNCTION: Transfers political legitimacy and mobilization capacity from established democratic institutions to populist movements and alternative authority structures. Transfers audience attention and revenue from mainstream to anti-establishment media. Transfers the psychological costs of institutional dysfunction and representation failure onto median voters who remain identity-locked to democratic participation.
% ABSENT_VOICES: Marginalized communities whose institutional distrust predates the current erosion and stems from structural exclusion rather than recent dysfunction. Their absence allows the erosion narrative to be framed as a universal crisis rather than a selective loss of trust among previously-trusting populations, which obscures who benefits from the shift.
% DISAPPEARANCE_RATIONALE: If trust erosion disappeared overnight and institutional confidence returned to 1960s levels, populist movements would lose their primary mobilization narrative, anti-establishment media would lose audience share, and alternative authority structures would lose adherents. However, defenders of the mountain claim argue the erosion would simply re-emerge from the same structural forces (scale, complexity, elite-mass distance) that produced it initially, making any restoration temporary.
% FOUNDING_PROBLEM: Large-scale democratic governance in complex modern societies creates inevitable information asymmetries, principal-agent problems, and elite-mass distance that erode trust over time regardless of institutional performance.
% FOUNDING_PROBLEM_CORROBORATION: The mountain framing is attested primarily by actors who benefit from trust erosion (populist movements citing it as validation, media outlets whose business models depend on it) and by some political scientists who treat it as structural inevitability. However, comparative democratic research from scholars outside these beneficiary groups shows substantial cross-national variation in trust trajectories that is not explained by scale or complexity alone, suggesting the erosion is not purely structural. Historical analysis shows periods of trust restoration following institutional reform, contradicting inevitability claims.
narrative_ontology:disappearance_verdict(institutional_trust_erosion_c0, contested).
narrative_ontology:founding_problem_status(institutional_trust_erosion_c0, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(institutional_trust_erosion_c0, '046e0a40c34cddf4fff29b8c15f632dbdef31b7a',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-12',
    'cohort_zero_regen', 'agent/example_platform_commission.json',
    'claude-sonnet-4-5-20250929', 'temperature=0.2').
narrative_ontology:story_seed(institutional_trust_erosion_c0, 'institutional_trust_erosion', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(institutional_trust_erosion_c0_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(institutional_trust_erosion_c0, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(institutional_trust_erosion_c0, ExtMetricName, E),
    domain_priors:suppression_score(institutional_trust_erosion_c0, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(institutional_trust_erosion_c0),
    narrative_ontology:constraint_metric(institutional_trust_erosion_c0, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(institutional_trust_erosion_c0, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(institutional_trust_erosion_c0_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is substantial (0.68) and rising because identifiable actors capture political power, media revenue, and social influence from the erosion while median voters bear representation failures and institutional dysfunction. The extraction accumulation over the 60-year interval (0.38 to 0.68) indicates layered rent-seeking rather than stable coordination cost. Suppression is moderate (0.52) because exit from democratic participation is psychologically costly for identity-locked citizens even as efficacy beliefs collapse, and because institutional authority persists through formal mechanisms despite legitimacy erosion. Theater ratio is moderate-high (0.41) because growing shares of democratic ritual (voting, deliberation, representation claims) operate as performance rather than genuine power-sharing as trust declines. Accessibility collapse is high (0.71) because once citizens internalize the elite-disconnect narrative, alternative framings become cognitively inaccessible — the erosion becomes self-validating. Resistance is substantial (0.58) because reform movements, civic renewal efforts, and institutional defenders actively contest the inevitability narrative, distinguishing this from a genuine natural law.
 *
 * PERSPECTIVAL GAP:
 *   From the median voter seat, the constraint operates as enforced extraction: declining efficacy with identity-locked participation creates a trap where democratic engagement becomes increasingly costly and futile. From the populist movement seat, the same structure operates as vindication and opportunity: trust erosion validates their critique and expands their mobilization base. From the institutional seat, it operates as degradation: legitimacy loss undermines governance capacity in a self-reinforcing cycle. From the analytical seat, it appears as a measurable social phenomenon with contested structural vs. contingent causation. The engine computes these divergences from the structural data; the mountain claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Median voters are structural targets: they experience declining institutional efficacy while remaining identity-locked to democratic participation (d near 0.85 — high target position). Established institutions are partly targets of legitimacy loss but also agenda-setters maintaining formal authority (d around 0.55 — mixed position). Populist movements, anti-establishment media, and alternative authority structures are beneficiaries: they collect power, revenue, and adherents from the erosion (d near 0.15 — strong beneficiary position). Marginalized communities are excluded rather than coordinated — their structural position is outside the trust-erosion frame entirely. The analytical seat observes without direct extraction or subsidy.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy question is whether trust erosion is genuine structural inevitability (mountain) or constructed extraction benefiting identifiable actors (false summit). The resolution mechanism distinguishes: if the erosion is purely structural, it should appear uniformly across democracies of similar scale and complexity, beneficiaries should be incidental rather than causal, and restoration efforts should fail regardless of design. If it is false summit, cross-national variation should track with beneficiary mobilization rather than structural factors, extraction should accumulate over time as beneficiaries consolidate, and targeted interventions addressing beneficiary incentives should show restoration potential. The rising extractiveness trajectory, beneficiary concentration, and unexplained cross-national variation support false summit classification, but the mountain framing persists because it serves beneficiary interests and because the scale-complexity narrative is intuitively compelling.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    structural_vs_constructed_erosion,
    'Is institutional trust erosion an inevitable consequence of democratic scale and complexity (mountain), or a constructed phenomenon amplified by actors who benefit from institutional delegitimization (false summit)?',
    'Cross-national comparative analysis controlling for scale and complexity: if erosion tracks with beneficiary mobilization (populist movement strength, anti-establishment media penetration) rather than structural variables, the constructed hypothesis is supported. Natural experiments from democracies that implemented trust-restoration reforms: if targeted interventions addressing beneficiary incentives show restoration effects, inevitability is refuted.',
    'If structural, the erosion is a mountain and adaptation rather than restoration is the appropriate response. If constructed, it is a false summit and interventions targeting beneficiary extraction mechanisms (media regulation, campaign finance reform, institutional transparency) could restore trust. The classification determines whether declining trust is a fact to accept or a constraint to contest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_vs_constructed_erosion, empirical, 'Whether trust erosion is structural inevitability or constructed extraction with identifiable beneficiaries.').

omega_variable(
    identity_lock_mechanism,
    'Is the identity-locked exit condition for median voters structural (democratic citizenship is constitutive of modern identity) or internalized (citizens have been socialized to view exit as unthinkable even when participation becomes futile)?',
    'Longitudinal analysis of civic identity formation and exit-consideration patterns across cohorts. If younger cohorts show weaker identity fusion with democratic participation and higher exit consideration despite similar socialization, the internalized hypothesis is supported. Cross-cultural comparison with democracies showing different civic identity patterns.',
    'If structural, the identity lock is a genuine constraint on exit and suppression is inherent to the democratic form. If internalized, the lock is a product of specific socialization practices and could be altered through civic education reform or alternative participation frameworks, reducing effective suppression.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism, conceptual, 'Whether identity-locked democratic participation is structural or internalized suppression.').

omega_variable(
    beneficiary_causation_vs_opportunism,
    'Do populist movements and anti-establishment media cause trust erosion through active delegitimization campaigns, or do they merely benefit opportunistically from erosion caused by genuine institutional failures?',
    'Time-series analysis of trust metrics, beneficiary mobilization, and institutional performance: if trust decline precedes beneficiary mobilization and tracks with performance failures, opportunism is supported. If beneficiary messaging intensity predicts trust decline controlling for performance, causation is supported. Experimental studies of delegitimization messaging effects on institutional confidence.',
    'If opportunistic, beneficiaries are incidental to a genuine structural problem and their extraction is a side effect. If causal, beneficiaries are actively producing the erosion they profit from, which supports interventions targeting their amplification mechanisms and reclassifies the constraint from mountain to tangled rope or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_causation_vs_opportunism, empirical, 'Whether beneficiaries cause trust erosion or merely exploit it opportunistically.').

omega_variable(
    marginalized_community_exclusion_impact,
    'Does the exclusion of marginalized communities from trust-erosion discourse materially affect the constraint''s classification, or is their absence merely a narrative gap?',
    'Comparative analysis of trust trajectories in democracies with different inclusion patterns for historically marginalized voices. If inclusion correlates with different erosion patterns or beneficiary structures, exclusion is materially relevant. Counterfactual analysis: if marginalized communities were centered in the discourse, would the erosion be framed differently and would different interventions be prioritized?',
    'If exclusion is material, the constraint''s beneficiary structure is incomplete and the mountain framing is partly an artifact of whose trust is being measured. If exclusion is merely narrative, the structural analysis holds regardless of whose voices are centered. This affects whether the constraint is a universal democratic phenomenon or a selective loss of trust among previously-privileged populations.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(marginalized_community_exclusion_impact, conceptual, 'Whether marginalized community exclusion from erosion discourse affects structural classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(institutional_trust_erosion_c0, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inst_tr_t0, institutional_trust_erosion_c0, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(inst_tr_t0, observed).
narrative_ontology:measurement(inst_tr_t10, institutional_trust_erosion_c0, theater_ratio, 10, 0.26).
narrative_ontology:measurement_basis(inst_tr_t10, observed).
narrative_ontology:measurement(inst_tr_t20, institutional_trust_erosion_c0, theater_ratio, 20, 0.3).
narrative_ontology:measurement_basis(inst_tr_t20, observed).
narrative_ontology:measurement(inst_tr_t30, institutional_trust_erosion_c0, theater_ratio, 30, 0.34).
narrative_ontology:measurement_basis(inst_tr_t30, observed).
narrative_ontology:measurement(inst_tr_t40, institutional_trust_erosion_c0, theater_ratio, 40, 0.37).
narrative_ontology:measurement_basis(inst_tr_t40, observed).
narrative_ontology:measurement(inst_tr_t50, institutional_trust_erosion_c0, theater_ratio, 50, 0.39).
narrative_ontology:measurement_basis(inst_tr_t50, observed).
narrative_ontology:measurement(inst_tr_t60, institutional_trust_erosion_c0, theater_ratio, 60, 0.41).
narrative_ontology:measurement_basis(inst_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(inst_be_t0, institutional_trust_erosion_c0, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(inst_be_t0, observed).
narrative_ontology:measurement(inst_be_t10, institutional_trust_erosion_c0, base_extractiveness, 10, 0.45).
narrative_ontology:measurement_basis(inst_be_t10, observed).
narrative_ontology:measurement(inst_be_t20, institutional_trust_erosion_c0, base_extractiveness, 20, 0.52).
narrative_ontology:measurement_basis(inst_be_t20, observed).
narrative_ontology:measurement(inst_be_t30, institutional_trust_erosion_c0, base_extractiveness, 30, 0.58).
narrative_ontology:measurement_basis(inst_be_t30, observed).
narrative_ontology:measurement(inst_be_t40, institutional_trust_erosion_c0, base_extractiveness, 40, 0.63).
narrative_ontology:measurement_basis(inst_be_t40, observed).
narrative_ontology:measurement(inst_be_t50, institutional_trust_erosion_c0, base_extractiveness, 50, 0.66).
narrative_ontology:measurement_basis(inst_be_t50, observed).
narrative_ontology:measurement(inst_be_t60, institutional_trust_erosion_c0, base_extractiveness, 60, 0.68).
narrative_ontology:measurement_basis(inst_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(inst_su_t0, institutional_trust_erosion_c0, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(inst_su_t0, observed).
narrative_ontology:measurement(inst_su_t10, institutional_trust_erosion_c0, suppression_requirement, 10, 0.38).
narrative_ontology:measurement_basis(inst_su_t10, observed).
narrative_ontology:measurement(inst_su_t20, institutional_trust_erosion_c0, suppression_requirement, 20, 0.42).
narrative_ontology:measurement_basis(inst_su_t20, observed).
narrative_ontology:measurement(inst_su_t30, institutional_trust_erosion_c0, suppression_requirement, 30, 0.45).
narrative_ontology:measurement_basis(inst_su_t30, observed).
narrative_ontology:measurement(inst_su_t40, institutional_trust_erosion_c0, suppression_requirement, 40, 0.48).
narrative_ontology:measurement_basis(inst_su_t40, observed).
narrative_ontology:measurement(inst_su_t50, institutional_trust_erosion_c0, suppression_requirement, 50, 0.5).
narrative_ontology:measurement_basis(inst_su_t50, observed).
narrative_ontology:measurement(inst_su_t60, institutional_trust_erosion_c0, suppression_requirement, 60, 0.52).
narrative_ontology:measurement_basis(inst_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(institutional_trust_erosion_c0, identity_coordination).
narrative_ontology:boltzmann_floor_override(institutional_trust_erosion_c0, 0.08).
narrative_ontology:affects_constraint(institutional_trust_erosion_c0, populist_electoral_success).
narrative_ontology:affects_constraint(institutional_trust_erosion_c0, media_polarization_dynamics).
narrative_ontology:affects_constraint(institutional_trust_erosion_c0, civic_engagement_decline).

% DUAL FORMULATION NOTE:
% This constraint is part of a democratic governance constraint family. Related constraints include populist_electoral_success (downstream: gains mobilization capacity from this erosion), media_polarization_dynamics (coupled: both cause and effect of trust decline), and civic_engagement_decline (downstream: reduced trust leads to reduced participation). Each has distinct ε values and beneficiary structures but they form a mutually-reinforcing network.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(institutional_trust_erosion_c0, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
