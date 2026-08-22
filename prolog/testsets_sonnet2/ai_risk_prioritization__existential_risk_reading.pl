% ============================================================================
% CONSTRAINT STORY: ai_risk_prioritization__existential_risk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_risk_prioritization__existential_risk_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ai_risk_prioritization__existential_risk_reading
 *   human_readable: Existential-Risk Framing of AI Risk Prioritization
 *   domain: technology_governance/risk_assessment
 *
 * SUMMARY:
 *   This constraint is the existential-risk reading of a contested kernel
 *   about how AI risk attention and resources should be prioritized. On this
 *   reading, misaligned artificial general intelligence poses an
 *   extinction-level threat to humanity, and alignment research and
 *   capability controls are the paramount, resource-deserving priority. The
 *   reading has consolidated real institutional power: dedicated research
 *   institutes, longtermist philanthropic infrastructure, and safety teams
 *   inside frontier labs, whose funding and prestige depend on this framing
 *   continuing to dominate the field. It coexists in an ongoing contest with
 *   a near-term-harms reading of the same underlying kernel (deployed AI
 *   systems causing measurable discrimination, displacement, and surveillance
 *   now), which is authored as a separate, sibling constraint
 *   (near_term_harms_reading) with its own ε and stakeholder structure — not
 *   folded into this one. The ε authored here (0.58) is for the standing
 *   arrangement as this reading's own advocates and critics jointly observe
 *   it operating: a real coordination function (pooling scarce talent and
 *   capital around a genuinely hard technical problem) running alongside a
 *   documented pattern of crowding out near-term harms funding and framing
 *   that work as a distraction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_prioritization__existential_risk_reading, 0.58).
domain_priors:suppression_score(ai_risk_prioritization__existential_risk_reading, 0.52).
domain_priors:theater_ratio(ai_risk_prioritization__existential_risk_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_prioritization__existential_risk_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_prioritization__existential_risk_reading, "Existential-Risk Framing of AI Risk Prioritization").
narrative_ontology:topic_domain(ai_risk_prioritization__existential_risk_reading, "technology_governance/risk_assessment").

domain_priors:requires_active_enforcement(ai_risk_prioritization__existential_risk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_prioritization__existential_risk_reading, '8440c1ed-8cec-40f3-adce-d6ed7cf63a56').
narrative_ontology:cs_kernel_codification('8440c1ed-8cec-40f3-adce-d6ed7cf63a56', distributed).
narrative_ontology:cs_authority_grounding('8440c1ed-8cec-40f3-adce-d6ed7cf63a56', distributed).
narrative_ontology:cs_reading_relation('8440c1ed-8cec-40f3-adce-d6ed7cf63a56', ai_risk_prioritization__near_term_harms_reading, coexists_with).
narrative_ontology:cs_axiom('8440c1ed-8cec-40f3-adce-d6ed7cf63a56', foundational, extinction_level_outcomes_dominate_expected_value_calculus).
narrative_ontology:cs_axiom_status(extinction_level_outcomes_dominate_expected_value_calculus, holdable).
narrative_ontology:cs_axiom_grounding('8440c1ed-8cec-40f3-adce-d6ed7cf63a56', extinction_level_outcomes_dominate_expected_value_calculus, instrumental).
narrative_ontology:cs_axiom('8440c1ed-8cec-40f3-adce-d6ed7cf63a56', foundational, future_persons_carry_comparable_moral_weight_to_present_persons).
narrative_ontology:cs_axiom_status(future_persons_carry_comparable_moral_weight_to_present_persons, holdable).
narrative_ontology:cs_axiom_grounding('8440c1ed-8cec-40f3-adce-d6ed7cf63a56', future_persons_carry_comparable_moral_weight_to_present_persons, deontological).
narrative_ontology:cs_reference_frame('8440c1ed-8cec-40f3-adce-d6ed7cf63a56', precautionary_extinction_avoidance_priority).
narrative_ontology:cs_drift_state('8440c1ed-8cec-40f3-adce-d6ed7cf63a56', post_frontier_lab_commercialization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8440c1ed-8cec-40f3-adce-d6ed7cf63a56', '').
narrative_ontology:cs_kernel_id(ai_risk_prioritization__existential_risk_reading, ai_risk_prioritization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__existential_risk_reading, xrisk_research_institutes).
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__existential_risk_reading, longtermist_funders).
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__existential_risk_reading, frontier_lab_safety_teams).
narrative_ontology:constraint_victim(ai_risk_prioritization__existential_risk_reading, algorithmically_harmed_communities).
narrative_ontology:constraint_victim(ai_risk_prioritization__existential_risk_reading, near_term_ai_justice_researchers).
narrative_ontology:constraint_victim(ai_risk_prioritization__existential_risk_reading, future_humanity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__existential_risk_reading, future_humanity).
narrative_ontology:constraint_vindicates(ai_risk_prioritization__existential_risk_reading, extinction_level_ai_threat_is_the_dominant_risk_category).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set research agendas, publish threat models, and advise policymakers on the premise that misalignment risk dominates. Control a large and growing share of philanthropic and government funding earmarked for AI safety, and shape which risks count as 'safety' in grant calls, conference programs, and legislative testimony.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, xrisk_research_institutes, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_prioritization__existential_risk_reading, xrisk_research_institutes, beneficiary).

% Direct philanthropic capital toward alignment research and capability-control advocacy, justified by expected-value calculations over astronomical future populations. Their giving strategy is validated by the existential framing continuing to dominate the field's prestige hierarchy and funding priorities.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, longtermist_funders, beneficiary,
    institutional, civilizational, arbitrage, global).

% Staff internal alignment teams at major AI developers; their organizational relevance and funding depend on the existential frame remaining the field's central legitimating narrative, even as their employers ship deployed systems with measurable present-day harms.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, frontier_lab_safety_teams, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_prioritization__existential_risk_reading, frontier_lab_safety_teams, agenda_setter).

% Experience discriminatory lending, wrongful arrest from facial recognition, algorithmic wage suppression, and automated benefits denial today. Have little say in an agenda that treats their harms as lower priority than speculative future extinction, and cannot easily exit systems already embedded in housing, employment, and policing.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, algorithmically_harmed_communities, payer,
    powerless, immediate, trapped, national).

% Study deployed-system harms — bias, surveillance, labor displacement — and compete for funding, journal space, and institutional attention against an existential-risk framing that is often explicitly counterposed to their work as a 'distraction.' Career progression narrows when funders and prestigious venues privilege x-risk framing.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, near_term_ai_justice_researchers, payer,
    moderate, biographical, constrained, national).

% Nonexistent or not-yet-born persons whose interests the existential framing claims to represent and protect through alignment research; simultaneously bear the risk if resources are misallocated away from correcting present harms that compound into future structural inequities, and have no voice of their own in the allocation debate.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, future_humanity, beneficiary,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_secondary_role(ai_risk_prioritization__existential_risk_reading, future_humanity, payer).

% Draft AI governance legislation under intense lobbying pressure from both x-risk-framed and harms-framed advocacy coalitions; often lack independent technical capacity to adjudicate between the two framings and default to whichever coalition has more established access and credibility in a given legislative session.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, policymakers_and_regulators, excluded,
    institutional, biographical, constrained, national).

% Push for compute governance, model weight restrictions, and international coordination mechanisms premised on catastrophic misalignment risk; these policy asks compete directly with near-term interventions (audits, disclosure mandates, liability regimes) for the same finite legislative and regulatory bandwidth.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, capability_control_advocates, agenda_setter,
    organized, generational, arbitrage, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_risk_prioritization__existential_risk_reading, xrisk_research_institutes).
narrative_ontology:fixing_cost_class(ai_risk_prioritization__existential_risk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates scarce technical talent, philanthropic capital, and policy attention around a single dominant threat model (catastrophic misalignment), enabling large, well-resourced, long-horizon research programs (interpretability, alignment theory, compute governance) that would be difficult to sustain under fragmented near-term-harms funding models.
% TRANSFER_FUNCTION: Moves research funding, policy attention, media coverage, and regulatory bandwidth away from documented present-day algorithmic harms and toward speculative long-horizon alignment and capability-control research; also moves reputational capital and institutional legitimacy toward the x-risk research community.
% ABSENT_VOICES: Communities currently harmed by deployed algorithmic systems — over-policed neighborhoods, gig workers subject to algorithmic management, applicants denied credit or housing by opaque scoring systems — are rarely present in the elite forums (AI safety summits, alignment funding boards) where existential-risk prioritization is set. Future persons, invoked as the primary beneficiaries, cannot speak for themselves and are represented only by the framing's own advocates.
% DISAPPEARANCE_RATIONALE: If the existential-risk framing vanished overnight, x-risk institutes would lose funding legitimacy and reallocate toward near-term harms work (a real rearrangement for that sector), but researchers on both sides dispute whether the underlying catastrophic risk itself would remain unaddressed — existential-risk proponents argue the world would become dramatically more dangerous without alignment research continuing regardless of framing; near-term-harms proponents argue little of substance would change since deployed-harm mitigation would simply regain proportionate attention.
% FOUNDING_PROBLEM: Early AI safety researchers (2000s-2010s) identified that sufficiently capable, goal-directed AI systems could pursue objectives misaligned with human values in ways that are difficult to correct once capability thresholds are crossed, potentially causing irreversible or extinction-level outcomes with no opportunity for course-correction.
% FOUNDING_PROBLEM_CORROBORATION: Some AI capability researchers outside the x-risk funding ecosystem (in academic ML and some national AI safety institutes) corroborate that misalignment at scale is a genuine open technical problem. However, a substantial body of near-term-harms researchers, science-and-technology-studies scholars, and some AI ethics practitioners — largely outside the beneficiary set — contend the extinction framing is empirically underdetermined relative to documented present harms and functions partly to secure funding and prestige for a specific research program; no consensus corroboration exists outside parties with a stake in one framing or the other.
narrative_ontology:disappearance_verdict(ai_risk_prioritization__existential_risk_reading, contested).
narrative_ontology:founding_problem_status(ai_risk_prioritization__existential_risk_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_prioritization__existential_risk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_risk_prioritization__existential_risk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_risk_prioritization__existential_risk_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_risk_prioritization__existential_risk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_risk_prioritization__existential_risk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_risk_prioritization__existential_risk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.58 — moderate-high, not extreme — because the coordination function is genuine (catastrophic misalignment is a real open technical question with credentialed researchers across institutions, not merely constructed to extract). But resources, media attention, and regulatory bandwidth are finite, and the existential framing's dominance measurably displaces near-term harms work, which is the asymmetric extraction component. Suppression (0.52) reflects the documented rhetorical move of framing near-term algorithmic justice concerns as a 'distraction' from the paramount existential concern — this delegitimizes competing claims on the same scarce attention rather than physically barring them. Theater ratio (0.42) is elevated because a portion of alignment-adjacent activity (conference circuits, high-profile open letters, symbolic governance commitments) functions more as field-legitimating performance than as tractable technical progress, though a substantial core of real interpretability and control research remains functional. Accessibility collapse (0.48) is moderate: near-term harms researchers and advocates are not fully locked out of funding or policy access, but the existential frame's dominance narrows the practical field of workable alternatives inside elite AI governance venues. Resistance (0.61) is comparatively high because near-term-harms researchers, AI ethics scholars, and affected communities actively contest the framing in academic literature, congressional testimony, and public advocacy.
 *
 * DIRECTIONALITY LOGIC:
 *   X-risk research institutes, longtermist funders, and frontier lab safety teams sit near the beneficiary end: they set or heavily influence the agenda, and their institutional standing and funding flows are validated by the framing's continued dominance. Algorithmically harmed communities and near-term-harms researchers sit near the target end: they bear the real cost of displaced attention and resources, and their exit options are constrained — communities cannot easily escape systems already embedded in housing, credit, and policing, and researchers face narrowed career paths when prestige and funding concentrate elsewhere. Future humanity is deliberately dual-coded: nominally the primary beneficiary the framing exists to protect, but also a payer if resource misallocation (away from correcting compounding present-day harms) produces worse structural outcomes by the time that future arrives — and future persons have no seat to advocate for themselves, which is exactly the gap the framing's critics point to.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents two mislabeling errors. First, it avoids treating this purely as extraction cover: the misalignment research agenda addresses a genuine, non-trivial technical uncertainty, and dismissing it entirely as a snare would mischaracterize a body of real, falsifiable research. Second, it avoids treating this purely as neutral coordination: the framing's dominance is not merely an efficient division of intellectual labor but is actively maintained by rhetorical suppression of competing claims on the same resource pool, and by institutional structures (dedicated funding streams, elite conference circuits) whose continued legitimacy depends on the extinction narrative retaining priority status. The founding problem (irreversible catastrophic misalignment) is genuinely contested as live-vs-dead-vs-displaced — some technical researchers regard it as a serious unresolved question, others regard the resourcing pattern as having outrun the evidentiary basis and functioning now mainly to sustain the institutions built around it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extinction_risk_empirical_tractability,
    'Is catastrophic AI misalignment a well-specified, empirically tractable risk comparable to other extinction-level threats (e.g., asteroid impact, engineered pandemics), or is it a speculative extrapolation whose probability and timeline estimates are not meaningfully falsifiable with current evidence?',
    'Longitudinal tracking of concrete alignment failure incidents in deployed frontier systems, expert elicitation calibration studies, and post-hoc evaluation of past x-risk timeline predictions against actual capability trajectories.',
    'If tractable and well-calibrated, the resource allocation toward alignment research is substantially justified as genuine coordination against a real threat. If largely speculative and poorly calibrated, the framing''s dominance looks more like extraction of prestige and funding using an unfalsifiable threat model, shifting the classification toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extinction_risk_empirical_tractability, empirical, 'Whether extinction-level misalignment risk is empirically well-grounded or largely speculative.').

omega_variable(
    committer_kernel_reading_split,
    'This constraint is one reading (existential_risk_reading) of the ai_risk_prioritization kernel; the sibling reading (near_term_harms_reading) allocates victim status to presently-harmed communities and beneficiary status to near-term-harms researchers and advocates instead. Where exactly does the disagreement between readings live structurally?',
    'The disagreement is located in the choice of time horizon (10-100 years vs. immediate-to-biographical) and in the composition of the victim set (nonexistent future persons vs. presently identifiable harmed communities) — not in a factual dispute about whether AI causes harm, which both readings affirm. Resolving the kernel dispute would require settling how to weigh probabilistic future catastrophe against certain present harm, which is a normative allocation question, not a purely empirical one.',
    'If policymakers or funders adopt the sibling reading''s time-horizon and victim-set framing, resource flows reverse: near-term harms research becomes the paramount priority and this reading''s beneficiaries lose funding legitimacy. The two readings cannot both be fully resourced from the same finite attention and funding pool, which is why they function as genuinely rival, not merely complementary, claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_kernel_reading_split, conceptual, 'Where the existential-risk and near-term-harms readings of the shared kernel structurally diverge.').

omega_variable(
    future_persons_representation_legitimacy,
    'Can any existing institution legitimately claim to represent the interests of future, not-yet-existing persons in a resource allocation debate, or does invoking ''future humanity'' as a beneficiary class function mainly to insulate the framing''s current advocates from being challenged by presently-existing stakeholders?',
    'Comparative institutional analysis of other domains that formally represent future-generation interests (e.g., long-term environmental trusteeship models, intergenerational equity clauses in constitutional law) and whether comparable accountability mechanisms exist or could exist for AI x-risk governance.',
    'If no legitimate representation mechanism can exist, the beneficiary claim on behalf of future humanity is unfalsifiable and effectively transfers decision authority entirely to current x-risk institutions, strengthening the case for suppression of competing (verifiable, present-day) harm claims. If representation mechanisms are plausible, the framing''s beneficiary claim gains more structural legitimacy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(future_persons_representation_legitimacy, conceptual, 'Whether current institutions can legitimately represent nonexistent future persons'' interests.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_prioritization__existential_risk_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_r_tr_t0, ai_risk_prioritization__existential_risk_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(ai_r_tr_t4, ai_risk_prioritization__existential_risk_reading, theater_ratio, 4, 0.28).
narrative_ontology:measurement(ai_r_tr_t8, ai_risk_prioritization__existential_risk_reading, theater_ratio, 8, 0.33).
narrative_ontology:measurement(ai_r_tr_t12, ai_risk_prioritization__existential_risk_reading, theater_ratio, 12, 0.37).
narrative_ontology:measurement(ai_r_tr_t16, ai_risk_prioritization__existential_risk_reading, theater_ratio, 16, 0.4).
narrative_ontology:measurement(ai_r_tr_t20, ai_risk_prioritization__existential_risk_reading, theater_ratio, 20, 0.42).

% Extraction over time
narrative_ontology:measurement(ai_r_be_t0, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ai_r_be_t4, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 4, 0.42).
narrative_ontology:measurement(ai_r_be_t8, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 8, 0.49).
narrative_ontology:measurement(ai_r_be_t12, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 12, 0.53).
narrative_ontology:measurement(ai_r_be_t16, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 16, 0.56).
narrative_ontology:measurement(ai_r_be_t20, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(ai_r_su_t0, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(ai_r_su_t4, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 4, 0.36).
narrative_ontology:measurement(ai_r_su_t8, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 8, 0.42).
narrative_ontology:measurement(ai_r_su_t12, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 12, 0.46).
narrative_ontology:measurement(ai_r_su_t16, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 16, 0.49).
narrative_ontology:measurement(ai_r_su_t20, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_prioritization__existential_risk_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(ai_risk_prioritization__existential_risk_reading, 0.12).
narrative_ontology:affects_constraint(ai_risk_prioritization__existential_risk_reading, near_term_harms_reading).

% DUAL FORMULATION NOTE:
% This constraint and near_term_harms_reading are sibling readings of the shared ai_risk_prioritization kernel, per the ε-invariance decomposition principle. They share the underlying natural-language label ('AI risk prioritization') but diverge in victim set, timescale, and beneficiary composition, and are authored as two separate constraint stories with independent ε values rather than one story with an averaged or measurement-dependent ε. This story's authored ε (0.58) reflects the existential-risk reading's own standing arrangement; the sibling story authors its own ε independently. Both stories should be read together for the full kernel contest, but neither substitutes for the other.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
