% ============================================================================
% CONSTRAINT STORY: ai_risk_prioritization__near_term_harms_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_risk_prioritization__near_term_harms_reading, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ai_risk_prioritization__near_term_harms_reading
 *   human_readable: AI Risk Prioritization: Near-Term Harms Reading
 *   domain: AI_safety/technology_governance/algorithmic_justice
 *
 * SUMMARY:
 *   The near-term harms reading of AI risk prioritization frames the problem
 *   as urgent, measurable, and addressable through justice interventions
 *   targeting algorithmic discrimination, worker displacement, and
 *   surveillance systems deployed now. This reading contests a sibling kernel
 *   reading centered on existential risk from advanced AI systems. The
 *   near-term harms reading mobilizes a constituency of affected populations
 *   (marginalized communities subject to algorithmic systems),
 *   fairness/accountability researchers, and regulatory advocates. It
 *   suppresses alternative framings by treating existential risk as
 *   speculative distraction from present harms that demand immediate
 *   intervention. The constraint exhibits tangled-rope structure: it
 *   coordinates algorithmic justice work (genuine coordination problem: how
 *   to audit systems, regulate deployment, protect vulnerable populations)
 *   while extracting from the existential risk research community (suppresses
 *   funding, institutional legitimacy, and policy attention for long-horizon
 *   AI alignment work). Suppression is moderate-high because the near-term
 *   framing has institutional legitimacy through civil rights narratives and
 *   demonstrates measurable harms, making the existential risk framing appear
 *   speculative by comparison.
 *
 * KEY AGENTS:
 *   - Surveilled and Discriminated Populations: Primary victim (powerless/trapped) — subject to algorithmic harm in credit, hiring, criminal justice, welfare systems with no exit option
 *   - Affected Workers and Communities: Secondary victim (moderate/constrained) — experience job displacement and surveillance with some constrained remediation through advocacy and retraining
 *   - Algorithmic Justice Researchers: Primary beneficiary (institutional/arbitrage) — funding, legitimacy, and career advancement flow to fairness/accountability research when near-term harms are centered
 *   - Regulatory and Advocacy Coalition: Organized beneficiary (organized/constrained) — civil rights groups, algorithmic justice advocates, regulatory bodies mobilized around near-term harm framing
 *   - AI Industry and Deployment Infrastructure: Secondary actor (institutional/arbitrage) — bears compliance burden (audits, transparency, bias testing) experienced as performative ritual
 *   - Existential Risk Research Community: Suppressed actor (powerless/trapped) — resources, institutional attention, and policy legitimacy diverted from long-horizon alignment research
 *   - Analytical Observer: Perspective (analytical/analytical) — risks naturalizing the near-term/existential dichotomy as an immutable feature of AI governance rather than a political choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_prioritization__near_term_harms_reading, 0.58).
domain_priors:suppression_score(ai_risk_prioritization__near_term_harms_reading, 0.65).
domain_priors:theater_ratio(ai_risk_prioritization__near_term_harms_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_prioritization__near_term_harms_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_prioritization__near_term_harms_reading, "AI Risk Prioritization: Near-Term Harms Reading").
narrative_ontology:topic_domain(ai_risk_prioritization__near_term_harms_reading, "AI_safety/technology_governance/algorithmic_justice").

domain_priors:requires_active_enforcement(ai_risk_prioritization__near_term_harms_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_prioritization__near_term_harms_reading, '7e703fc6-4baa-4e41-aca0-7470ad33a133').
narrative_ontology:cs_kernel_codification('7e703fc6-4baa-4e41-aca0-7470ad33a133', distributed).
narrative_ontology:cs_authority_grounding('7e703fc6-4baa-4e41-aca0-7470ad33a133', extraction).
narrative_ontology:cs_interpretation_layer_present('7e703fc6-4baa-4e41-aca0-7470ad33a133').
narrative_ontology:cs_reading_relation('7e703fc6-4baa-4e41-aca0-7470ad33a133', ai_risk_prioritization__existential_risk_reading, coexists_with).
narrative_ontology:cs_axiom('7e703fc6-4baa-4e41-aca0-7470ad33a133', foundational, present_algorithmic_harms_require_immediate_intervention).
narrative_ontology:cs_axiom_status(present_algorithmic_harms_require_immediate_intervention, holdable).
narrative_ontology:cs_axiom_grounding('7e703fc6-4baa-4e41-aca0-7470ad33a133', present_algorithmic_harms_require_immediate_intervention, empirically_contingent).
narrative_ontology:cs_axiom('7e703fc6-4baa-4e41-aca0-7470ad33a133', foundational, existential_risk_is_speculative_distraction_from_present_justice).
narrative_ontology:cs_axiom_status(existential_risk_is_speculative_distraction_from_present_justice, holdable).
narrative_ontology:cs_axiom_grounding('7e703fc6-4baa-4e41-aca0-7470ad33a133', existential_risk_is_speculative_distraction_from_present_justice, deontological).
narrative_ontology:cs_reference_frame('7e703fc6-4baa-4e41-aca0-7470ad33a133', algorithmic_discrimination_as_civil_rights_issue).
narrative_ontology:cs_drift_state('7e703fc6-4baa-4e41-aca0-7470ad33a133', contemporary_post_audit_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('7e703fc6-4baa-4e41-aca0-7470ad33a133', '').
narrative_ontology:cs_kernel_id(ai_risk_prioritization__near_term_harms_reading, ai_risk_prioritization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__near_term_harms_reading, marginalized_communities).
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__near_term_harms_reading, algorithmic_justice_researchers).
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__near_term_harms_reading, regulatory_advocates).
narrative_ontology:constraint_victim(ai_risk_prioritization__near_term_harms_reading, present_populations_subject_to_algorithmic_discrimination).
narrative_ontology:constraint_victim(ai_risk_prioritization__near_term_harms_reading, workers_displaced_by_automation).
narrative_ontology:constraint_victim(ai_risk_prioritization__near_term_harms_reading, surveilled_populations).
narrative_ontology:constraint_victim(ai_risk_prioritization__near_term_harms_reading, existential_risk_research_funding).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SURVEILLED AND DISCRIMINATED POPULATIONS (SNARE) — Subject to algorithmic harm now: facial recognition targeting, lending discrimination, hiring screening, predictive policing. No effective exit from algorithmic systems deployed at scale. Maximum extraction: systems optimize against this population's interests while suppressing alternatives (regulatory, technical fixes). Biographical time frame is the relevant one — harms accumulate across a person's life.
constraint_indexing:constraint_classification(ai_risk_prioritization__near_term_harms_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: AFFECTED WORKERS AND COMMUNITIES (TANGLED ROPE) — Experience both extraction (job displacement, surveillance intensification) and coordination benefit (some communities receive algorithmic bias audits, fairness interventions, policy advocacy resources targeted at near-term harms). Constrained exit: retraining and advocacy are costly but possible. Effective extraction is substantial but not maximal — some agency exists through community organizing and legal challenge.
constraint_indexing:constraint_classification(ai_risk_prioritization__near_term_harms_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ALGORITHMIC JUSTICE RESEARCH COMMUNITY (ROPE) — Primary beneficiary of the near-term harms reading. Research funding, institutional legitimacy, and career advancement flow to fairness/accountability researchers when near-term harms are centered. The reading frames their research as urgent and necessary. Arbitrage available: researchers can exit by adopting existential risk framing, but doing so reduces the reading's resource allocation to their work. Experiences the constraint as coordination: the reading mobilizes resources for urgent algorithmic justice research.
constraint_indexing:constraint_classification(ai_risk_prioritization__near_term_harms_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: AI INDUSTRY AND DEPLOYMENT INFRASTRUCTURE (PITON) — Theater-heavy compliance. The near-term harms reading generates algorithmic audit requirements, bias testing, fairness frameworks, and transparency reports. Industry compliance is largely performative: bias audits often fail to catch harms in deployment, transparency reports obscure real decision-making, fairness metrics optimize for measurable proxies while missing material discrimination. Theater ratio (0.48 at baseline) reflects that some genuine work happens (bias audits, testing) alongside substantial performative compliance. The industry sees this constraint as inertial — compliance burden maintained by regulatory pressure and reputational risk, not by demonstrable efficacy of audits in preventing harms.
constraint_indexing:constraint_classification(ai_risk_prioritization__near_term_harms_reading, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY AND ADVOCACY COALITION (SCAFFOLD) — Organized agents (civil rights groups, regulatory bodies, algorithmic justice advocates) see near-term harms as a solvable problem with explicit policy endpoints: algorithmic impact assessments, bias auditing requirements, transparency mandates, worker retraining programs. These are temporary coordination mechanisms with sunset logic — as regulations mature and industry compliance deepens, the extraordinary focus on near-term harms can shift. Constrained exit: coalitions depend on continued near-term harm framing for political salience; existential risk framing reduces pressure for immediate intervention.
constraint_indexing:constraint_classification(ai_risk_prioritization__near_term_harms_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational frame, one might argue that algorithmic systems inevitably encode the biases of their training data and deployment context — a natural law of machine learning. Discrimination is inherent to scale, unmeasurable harms are inevitable, and the near-term/far-term distinction is artificial. This perspective risks naturalizing what is actually a choice: which harms to measure, which populations to monitor, which timescale to prioritize. The engine's false summit detector will flag this as a contested framing, not a genuine natural law.
constraint_indexing:constraint_classification(ai_risk_prioritization__near_term_harms_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: EXISTENTIAL RISK RESEARCH (OPPORTUNITY COST PERSPECTIVE) (SNARE) — This perspective treats existential risk research as trapped in a resource allocation snare: the near-term harms reading channels funding, policy attention, and institutional legitimacy toward near-term interventions, leaving existential-risk-focused researchers (longtermism) without resources for long-horizon research. From this view, the near-term reading extracts from the existential research community by suppressing its alternative framing. The powerless status reflects that x-risk researchers lack the immediate constituency of affected populations, making their resource claims less politically compelling.
constraint_indexing:constraint_classification(ai_risk_prioritization__near_term_harms_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_risk_prioritization__near_term_harms_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ai_risk_prioritization__near_term_harms_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ai_risk_prioritization__near_term_harms_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_risk_prioritization__near_term_harms_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ai_risk_prioritization__near_term_harms_reading, TR),
    TR >= 0.70.

:- end_tests(ai_risk_prioritization__near_term_harms_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The near-term harms reading concentrates institutional attention, research funding, and policy resources on measurable algorithmic harm (discrimination, displacement, surveillance). This extraction runs toward algorithmic justice researchers and advocacy coalitions and away from existential risk research. The extractiveness is not severe because near-term work is genuinely addressing real harms, and the extraction is a side effect of resource allocation choices rather than the primary purpose. The trajectory from 0.42 to 0.58 over 6 years reflects the growing institutional entrenchment of the near-term framing — as the reading gains legitimacy through regulatory adoption (GDPR, algorithmic auditing requirements, impact assessments), it accumulates more extractive force relative to alternative framings. Suppression (0.65): Moderate-high. The near-term framing suppresses existential risk research by treating it as speculative, distracting, or insufficient for urgent present harms. This suppression is structural (finite resources create forced prioritization) but also extractive (the reading actively dismisses x-risk framings as misguided or harmful to marginalized communities). Theater ratio (0.48): Moderate-low. The near-term harms reading involves some theatrical compliance (algorithmic audits that are performative, transparency reports that obscure, bias metrics that optimize for measurable proxies while missing material discrimination) but also genuine work (worker protections, regulatory frameworks, fairness research). The theater ratio rises from 0.35 to 0.48 as industry compliance becomes more ritualized and regulators rely increasingly on auditing ceremonies rather than material outcomes.
 *
 * PERSPECTIVAL GAP:
 *   The near-term harms reading produces a wide perspectival gap. Surveilled populations experience pure extraction (snare) — algorithmic systems harm them with no exit. Affected workers experience mixed extraction and coordination (tangled rope) — some fairness work helps, but harms continue. Algorithmic justice researchers experience pure coordination (rope) — the reading mobilizes resources for their work. Regulatory coalitions experience temporary coordination with a sunset (scaffold) — near-term interventions aim to solve the problem. The AI industry experiences performative compliance (piton) — audit requirements are ritual rather than functional. Existential risk researchers experience pure suppression (snare) — their work is deprioritized by the reading's framing. The analytical observer risks naturalizing the dichotomy (mountain) — treating the near-term/existential split as an immutable feature of AI risk rather than a contingent framing choice. The gap reveals that the reading is not simply addressing one problem fairly — it is actively suppressing an alternative framing through legitimate resource scarcity.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) is derived from the agent's structural position relative to this specific constraint. Surveilled populations are victims + trapped, yielding high d (0.95) and maximum experienced extraction. Affected workers are victims + constrained, yielding moderate d (0.70). Algorithmic justice researchers are beneficiaries + arbitrage, yielding low d (0.15). Regulatory coalitions are beneficiaries + constrained, yielding low-moderate d (0.35). The AI industry is a secondary beneficiary (compliance work is work) + arbitrage, yielding low d (0.20). Existential risk researchers are victims (of resource suppression) + trapped (cannot exit the near-term framing without losing research legitimacy), yielding high d (0.90). The analytical observer is analytical/analytical, yielding d ≈ 0.73. The suppression mechanism — treating existential risk as speculative — is a form of constraint enforcement: it makes the alternative framing costly to advocate in academic and policy forums.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves mandatrophy by acknowledging the genuine coordination function (measuring and mitigating algorithmic harm) alongside the extraction mechanism (suppressing existential risk research through resource reallocation). The mandatrophy was: 'Is AI risk prioritization primarily a coordination problem or an extraction problem?' The answer is 'both, relative to different agents.' The near-term reading coordinates fairness work and addresses present harms (rope/tangled rope from those perspectives) while extracting from existential risk research (snare from that perspective). The reading is not misclassified — it is correctly identified as tangled rope because it has BOTH coordination (genuine algorithmic justice work) and extraction (suppression of alternative framing). Mandatrophy is resolved by the perspectival structure: the reading is not a single type, but a family of types across the observation lattice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is AI risk fundamentally a near-term algorithmic harm problem or a long-horizon existential risk problem, or is this a false dichotomy?',
    'Longitudinal tracking of which risks materialize: if measurable algorithmic harms (discrimination, displacement, surveillance) dominate realized harms over 10-20 years, near-term reading confirmed. If civilization-level risks (misaligned AGI, loss of human agency) materialize despite near-term mitigation efforts, existential reading vindicated. If both materialize independently, the dichotomy itself is the artifact.',
    'If near-term reading is vindicated: algorithmic justice work is the correct priority; x-risk is distraction. If existential reading is vindicated: near-term work is necessary but insufficient; long-horizon alignment is paramount. If both: resources must be allocated simultaneously, making the kernel contest itself a false dichotomy generated by scarcity frames.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, empirical, 'Which set of AI risks materialize as dominant threats').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of existential risk research within this reading structural (legitimate resource scarcity forcing prioritization) or extractive (near-term researchers actively suppressing alternative framings)?',
    'Funding audit: compare funding flows to near-term vs long-horizon AI research before and after near-term harm reading gained institutional legitimacy. Citation analysis: measure whether near-term researchers cite and engage with x-risk literature or actively dismiss it. Institutional analysis: do institutions hosting near-term research explicitly reject x-risk, or do they simply allocate resources based on different priorities?',
    'If structural: the reading exhibits legitimate constraint triage — resources are finite, and near-term harms are measurable and urgent. If extractive: the reading is a snare that weaponizes moral urgency to suppress competing framings. Affects classification of the entire constraint at the existential risk perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Whether suppression of x-risk research is structural necessity or extractive mechanism').

omega_variable(
    algorithmic_transparency_efficacy,
    'Do transparency mandates, bias audits, and algorithmic impact assessments actually reduce algorithmic harms, or are they primarily performative compliance mechanisms?',
    'Comparative measurement: discrimination rates before and after audit/transparency requirements in the same jurisdictions. Deployment analysis: whether systems flagged by audits are modified, shelved, or re-deployed without substantive change. Harm tracking: longitudinal measurement of discrimination complaints, disparate impact litigation, and displacement outcomes in jurisdictions with vs without algorithmic governance frameworks.',
    'If efficacious: near-term harm mitigation via regulation is viable; theater ratio should be lower. If performative: theater ratio is higher; the constraint is piton-shifted (maintenance through ritual rather than function). Affects whether the regulatory coalition perspective (scaffold) is structural or aspirational.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_transparency_efficacy, empirical, 'Whether algorithmic governance reduces harms or provides ritual compliance').

omega_variable(
    timescale_commensurability,
    'Are near-term harms (5-year window) and existential risks (50+ year window) genuinely incommensurable problems requiring competing resources, or are they coupled — near-term errors producing x-risk conditions?',
    'Causal pathway analysis: trace how decisions made under near-term harm prioritization affect long-horizon outcomes. Scenario modeling: run forward 30-year models from near-term-optimized decision points to identify whether they create or mitigate existential conditions. Institutional learning: examine whether early algorithmic justice interventions improve or degrade robustness of AI governance at scale.',
    'If decoupled: true resource scarcity — must choose between near-term and long-horizon focus. If coupled (near-term errors → x-risk): simultaneous optimization is possible; false dichotomy generated by short planning horizons. If coupled (near-term focus prevents x-risk prep): existential reading is correct about the stakes. Determines whether the kernel is genuinely contested or whether one reading is instrumentally upstream of the other.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(timescale_commensurability, empirical, 'Whether near-term and existential AI risks are coupled or independent').

omega_variable(
    marginalized_community_resource_distribution,
    'Do resources channeled toward algorithmic justice actually flow to and benefit marginalized communities, or do they primarily fund researcher/advocate/practitioner work that leaves material conditions unchanged?',
    'Funding flow analysis: track algorithmic justice research and advocacy funding; measure how much reaches affected communities as direct benefit vs institutional overhead. Outcome measurement: do communities subject to algorithmic discrimination experience measurable improvement in discrimination rates, surveillance intensity, or material outcomes following algorithmic justice interventions?',
    'If resources flow to affected communities: beneficiary declaration is accurate; the reading is genuinely serving marginalized populations. If resources fund researchers/advocates primarily: the reading may be extractive toward affected populations (their harm provides legitimacy currency for academic/advocacy work) while offering minimal material improvement. Affects whether marginalized communities are genuine beneficiaries or secondary victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marginalized_community_resource_distribution, empirical, 'Whether algorithmic justice resources reach marginalized communities or fund intermediate institutions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_prioritization__near_term_harms_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(airisk_nt_tr_t0, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(airisk_nt_tr_t3, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 3, 0.42).
narrative_ontology:measurement(airisk_nt_tr_t6, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 6, 0.48).

% Extraction over time
narrative_ontology:measurement(airisk_nt_be_t0, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(airisk_nt_be_t3, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 3, 0.5).
narrative_ontology:measurement(airisk_nt_be_t6, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 6, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(airisk_nt_su_t0, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(airisk_nt_su_t3, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 3, 0.62).
narrative_ontology:measurement(airisk_nt_su_t6, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 6, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_prioritization__near_term_harms_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ai_risk_prioritization__near_term_harms_reading, ai_risk_prioritization__existential_risk_reading).

% DUAL FORMULATION NOTE:
% The AI risk prioritization kernel contains at least two structurally distinct constraint stories: the near-term harms reading (this file) and the existential risk reading (separate constraint). These are not measurements of the same constraint via different observables — they are different constraints because they have different victim sets, different beneficiary structures, different timescales, and different ε values. The near-term reading focuses on present algorithmic discrimination with ε ≈ 0.58; the existential reading focuses on long-horizon alignment failures with ε derived from a different empirical basis. They compete for institutional legitimacy and resources. Link them via network.affects_constraints to model their structural coupling.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_risk_prioritization__near_term_harms_reading, powerless, 0.95).
constraint_indexing:directionality_override(ai_risk_prioritization__near_term_harms_reading, analytical, 0.73).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
