% ============================================================================
% CONSTRAINT STORY: ai_risk_governance_priority__existential_risk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_risk_governance_priority__existential_risk_reading, []).

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
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   constraint_id: ai_risk_governance_priority__existential_risk_reading
 *   human_readable: Existential-Risk-Priority Reading of AI Risk Governance
 *   domain: AI governance / technology ethics / risk assessment
 *
 * SUMMARY:
 *   This story instantiates the existential-risk-priority reading of the
 *   contested kernel 'AI risk governance must prioritize X.' Under this
 *   reading, the governance imperative is to prevent superintelligence
 *   scenarios capable of annihilating or permanently curtailing humanity's
 *   potential, and resources — research funding, legislative bandwidth,
 *   international coordination capacity — should flow toward
 *   alignment-as-control, adversarial capability testing, and AGI-scenario
 *   governance frameworks. This is NOT a story about AI risk governance in
 *   general; it is a story about the specific claim that x-risk deserves
 *   priority over present, documented harms. The sibling readings
 *   (near_term_harms_reading, bridge_reading) are separate constraints with
 *   their own ε and stakeholder structures — this file does not average over
 *   them, hedge between them, or describe their contest internally.
 *
 * KEY AGENTS:
 *   - xrisk_research_institutes: agenda-setter and beneficiary — defines what counts as governance-worthy risk
 *   - frontier_ai_labs_claiming_safety_leadership: beneficiary — captures reputational and regulatory cover
 *   - algorithmic_bias_affected_communities, gig_and_displaced_workers, surveilled_populations, global_south_ai_deployment_subjects: payers — bear present harms deprioritized by the resource diversion
 *   - future_humanity: named beneficiary/notional victim with no seat or corroboration capacity
 *   - policymakers_and_regulators: agenda-setter/observer under heavy influence from xrisk-aligned advisory input
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_governance_priority__existential_risk_reading, 0.58).
domain_priors:suppression_score(ai_risk_governance_priority__existential_risk_reading, 0.42).
domain_priors:theater_ratio(ai_risk_governance_priority__existential_risk_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_governance_priority__existential_risk_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_governance_priority__existential_risk_reading, "Existential-Risk-Priority Reading of AI Risk Governance").
narrative_ontology:topic_domain(ai_risk_governance_priority__existential_risk_reading, "AI governance / technology ethics / risk assessment").

domain_priors:requires_active_enforcement(ai_risk_governance_priority__existential_risk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_governance_priority__existential_risk_reading, 'c034445f-3c01-444f-804e-4fd8dff06971').
narrative_ontology:cs_kernel_codification('c034445f-3c01-444f-804e-4fd8dff06971', distributed).
narrative_ontology:cs_authority_grounding('c034445f-3c01-444f-804e-4fd8dff06971', extraction).
narrative_ontology:cs_interpretation_layer_present('c034445f-3c01-444f-804e-4fd8dff06971').
narrative_ontology:cs_reading_relation('c034445f-3c01-444f-804e-4fd8dff06971', ai_risk_governance_priority__near_term_harms_reading, coexists_with).
narrative_ontology:cs_reading_relation('c034445f-3c01-444f-804e-4fd8dff06971', ai_risk_governance_priority__bridge_reading, influences).
narrative_ontology:cs_axiom('c034445f-3c01-444f-804e-4fd8dff06971', foundational, irreversible_catastrophic_tail_risk_dominates_expected_value).
narrative_ontology:cs_axiom_status(irreversible_catastrophic_tail_risk_dominates_expected_value, holdable).
narrative_ontology:cs_axiom_grounding('c034445f-3c01-444f-804e-4fd8dff06971', irreversible_catastrophic_tail_risk_dominates_expected_value, instrumental).
narrative_ontology:cs_axiom('c034445f-3c01-444f-804e-4fd8dff06971', secondary, capability_discontinuity_justifies_advance_resource_commitment).
narrative_ontology:cs_axiom_status(capability_discontinuity_justifies_advance_resource_commitment, holdable).
narrative_ontology:cs_axiom_grounding('c034445f-3c01-444f-804e-4fd8dff06971', capability_discontinuity_justifies_advance_resource_commitment, empirically_contingent).
narrative_ontology:cs_reference_frame('c034445f-3c01-444f-804e-4fd8dff06971', precautionary_longtermist_priority_ordering).
narrative_ontology:cs_drift_state('c034445f-3c01-444f-804e-4fd8dff06971', post_2023_frontier_model_governance_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c034445f-3c01-444f-804e-4fd8dff06971', '').
narrative_ontology:cs_kernel_id(ai_risk_governance_priority__existential_risk_reading, ai_risk_governance_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__existential_risk_reading, xrisk_research_institutes).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__existential_risk_reading, frontier_ai_labs_claiming_safety_leadership).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__existential_risk_reading, ai_safety_credentialed_researchers).
narrative_ontology:constraint_victim(ai_risk_governance_priority__existential_risk_reading, algorithmic_bias_affected_communities).
narrative_ontology:constraint_victim(ai_risk_governance_priority__existential_risk_reading, gig_and_displaced_workers).
narrative_ontology:constraint_victim(ai_risk_governance_priority__existential_risk_reading, surveilled_populations).
narrative_ontology:constraint_victim(ai_risk_governance_priority__existential_risk_reading, global_south_ai_deployment_subjects).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__existential_risk_reading, future_humanity).
narrative_ontology:constraint_victim(ai_risk_governance_priority__existential_risk_reading, future_humanity).
narrative_ontology:constraint_vindicates(ai_risk_governance_priority__existential_risk_reading, longtermist_expected_value_calculus).
narrative_ontology:constraint_vindicates(ai_risk_governance_priority__existential_risk_reading, agi_discontinuity_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the research agenda, funding priorities, and public framing for what counts as 'AI risk' — writes the reports governments cite, staffs the advisory panels, and defines which harms count as governance-worthy. Draws grant funding, philanthropic capital, and policy access specifically premised on the superintelligence-annihilation framing; has no exposure to the near-term harms it deprioritizes.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, xrisk_research_institutes, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_governance_priority__existential_risk_reading, xrisk_research_institutes, beneficiary).

% Fund and co-produce existential-risk discourse while continuing to ship the very systems whose present-day harms (bias, labor displacement, surveillance capability) the same discourse deprioritizes. Safety-leadership branding buys regulatory goodwill and slows near-term accountability measures that would be costlier to comply with than speculative long-horizon commitments.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, frontier_ai_labs_claiming_safety_leadership, beneficiary,
    institutional, generational, arbitrage, global).

% Career paths, publication venues, and prestige now attach to alignment-as-control research (adversarial testing, interpretability aimed at AGI scenarios). Exit to near-term-harms work is possible but costs status and funding access built around the existential framing.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, ai_safety_credentialed_researchers, beneficiary,
    organized, biographical, mobile, global).

% Experience discriminatory scoring, denial of services, and misclassification from deployed systems today. Governance resources, hearing time, and regulatory attention are diverted toward speculative superintelligence scenarios; they have no capacity to redirect the agenda and no exit from the systems already governing their lives.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, algorithmic_bias_affected_communities, payer,
    powerless, immediate, trapped, national).

% Bear labor displacement and algorithmic management harms from current-generation deployment. The governance apparatus, when it prioritizes AGI containment, produces frameworks (compute thresholds, model registries for frontier systems) that do not touch the mid-size deployment causing their present harm.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, gig_and_displaced_workers, payer,
    powerless, biographical, constrained, national).

% Subject to present-day AI-enabled surveillance and predictive policing; the existential framing does not address deployed surveillance infrastructure because it is not a 'superintelligence' scenario, leaving no governance lever pointed at their harm.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, surveilled_populations, payer,
    powerless, immediate, trapped, national).

% Receive exported AI systems (content moderation, credit scoring, agricultural automation) built and tested primarily against harms salient to wealthy-country deployers. Global governance capacity is spent on frontier-lab containment rather than on deployment harms concentrated in their jurisdictions.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, global_south_ai_deployment_subjects, payer,
    powerless, biographical, trapped, global).

% Named as the ultimate beneficiary of existential-risk prevention (a world not annihilated or permanently curtailed by misaligned superintelligence) but has no vote, no seat, and no way to corroborate whether the resources spent in its name are well-targeted; also the notional victim class if the risk is real and unaddressed.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, future_humanity, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_secondary_role(ai_risk_governance_priority__existential_risk_reading, future_humanity, payer).

% Draft binding frameworks under advice largely supplied by xrisk-aligned institutions and labs; increasingly asked to weigh compute-threshold regulation for frontier models against near-term harm statutes, with the existential framing crowding legislative bandwidth and staff attention.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, policymakers_and_regulators, observer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(ai_risk_governance_priority__existential_risk_reading, policymakers_and_regulators, agenda_setter).

% Civil-society and academic groups documenting present algorithmic harms are present in the discourse but structurally underweighted in the specific governance bodies and funding streams this reading controls — they would argue the existential framing is a resource-diversion mechanism, but their objection routes through a separate governance track, not this one.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, near_term_harms_advocates, excluded,
    organized, immediate, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_risk_governance_priority__existential_risk_reading, xrisk_research_institutes).
narrative_ontology:fixing_cost_class(ai_risk_governance_priority__existential_risk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates scarce technical-safety talent, compute-governance frameworks, and international attention around a specific catastrophic tail risk — misaligned superintelligent systems — that no single actor can unilaterally prevent and that plausibly requires advance coordination before capabilities arrive.
% TRANSFER_FUNCTION: Moves research funding, regulatory attention, legislative bandwidth, and philanthropic capital toward frontier-capability containment and away from remediation of present-day algorithmic harms; the beneficiary institutions capture funding, prestige, and policy access, while the cost is borne by populations experiencing deployed-system harms now who receive proportionally less governance capacity.
% ABSENT_VOICES: Communities harmed by present algorithmic bias, surveillance, and labor displacement are present in the broader AI-ethics discourse but structurally underweighted within the governance bodies, funding panels, and legislative priorities this reading sets; they would argue the existential framing functions as a resource-diversion and reputation-laundering mechanism for labs shipping present-harm systems.
% DISAPPEARANCE_RATIONALE: If existential-risk-priority governance vanished overnight, xrisk institutes and their funding streams would lose their primary justification and largely dissolve or repurpose; frontier labs would lose a reputational shield but continue operating under whatever governance replaced it. Whether the world 'rearranges' or 'stays the same' is exactly the contested question between this reading and the near-term-harms reading: existential-risk advocates hold that real catastrophic exposure would increase; near-term-harms advocates hold that governance capacity would simply become available for present-harm remediation, i.e. the world improves for those currently paying the cost.
% FOUNDING_PROBLEM: The founding problem, as this reading states it: transformative AI capabilities may arrive with a discontinuous jump in capability that current governance, safety research, and international coordination are unprepared for, and a misaligned system at that capability level could cause irreversible civilizational harm with no opportunity for correction after the fact.
% FOUNDING_PROBLEM_CORROBORATION: Some independent AI researchers outside the xrisk-institute funding network (in academic ML safety and some national AI-safety institutes) corroborate that discontinuous capability jumps and alignment failure are live technical concerns worth resourcing. However, critics outside the beneficiary set — labor economists, algorithmic-fairness researchers, and civil-society auditors — dispute that the probability-weighted urgency justifies the current resource allocation relative to documented, non-speculative present harms; no corroborating source independent of the beneficiary institutions has established the relative prioritization itself as correct, only that the underlying technical risk is non-zero.
narrative_ontology:disappearance_verdict(ai_risk_governance_priority__existential_risk_reading, contested).
narrative_ontology:founding_problem_status(ai_risk_governance_priority__existential_risk_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_governance_priority__existential_risk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_risk_governance_priority__existential_risk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_risk_governance_priority__existential_risk_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_risk_governance_priority__existential_risk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_risk_governance_priority__existential_risk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_risk_governance_priority__existential_risk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects a genuine coordination function (advance preparation for a plausible catastrophic tail risk) layered with a real resource-diversion effect measurable in comparative funding and legislative attention data — hence tangled_rope rather than pure rope or pure snare. Suppression (0.42) is moderate: this reading does not forcibly silence near-term-harms advocacy, but it structurally captures governance bandwidth, funding panels, and legislative drafting priority such that alternative framings compete from a starting disadvantage. Theater ratio (0.40) and its upward trajectory reflect a growing share of 'safety' activity (voluntary commitments, safety-summit communiques, published risk frameworks) that functions more as reputational signaling by frontier labs than as binding capability constraint. Accessibility collapse is comparatively low (0.35) — near-term-harms governance tracks still exist and are not eliminated, only under-resourced relative to this reading's priority claim. Resistance (0.55) is substantial: near-term-harms advocates, labor economists, and algorithmic-fairness researchers actively contest the prioritization, which is exactly the resistance you would expect from a genuinely contested kernel reading rather than settled consensus.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (xrisk institutes, safety-branded labs), this reading reads as urgent, underfunded coordination against a unique irreversible risk. From the payer seats (bias-affected communities, displaced workers, surveilled populations), the same structure reads as a resource-diversion mechanism that reliably wins governance attention against harms that are documented, present, and disproportionately borne by populations with the least capacity to redirect the agenda. The engine computes both seat-classifications from the same structural data; this divergence is the finding, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   xrisk research institutes and safety-branded frontier labs sit near the full-beneficiary end: they set the agenda, capture funding and reputational capital, and bear none of the deprioritized present-day harms. Populations experiencing algorithmic bias, surveillance, and labor displacement sit near the full-target end: trapped or constrained exit, no seat in the resource-allocation decision, and their harms are the ones this reading's resource diversion leaves under-addressed. 'Future humanity' is deliberately given an analytical power atom with no real exit options — it is invoked as the ultimate stakeholder but structurally cannot corroborate, contest, or benefit from the allocation in any observable way within this interval, which is itself part of what makes the beneficiary/victim asymmetry durable: the named beneficiary of the whole framing cannot show up to complain if the allocation is wrong.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (discontinuous capability jump, irreversible misalignment) may well remain live at the technical level — this story does not claim x-risk is fabricated. What the six-questions R5 interview surfaces is narrower: the founding-problem STATUS is contested specifically on the question of relative prioritization, and corroboration for the current resource allocation (as opposed to the underlying risk) comes almost entirely from within the beneficiary set. That is the specific pattern this classification exists to flag — not 'x-risk is fake' but 'the priority claim built on top of a real risk has outrun independent corroboration of its resource-allocation implications,' which is the tangled_rope signature: genuine coordination function, real asymmetric extraction, same structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_risk_vs_priority_capture,
    'Is the underlying existential risk this reading names sufficiently probable and severe to justify the CURRENT relative allocation of governance resources against it, or has a real but low/uncertain-probability risk been used to justify a resource allocation disproportionate to its expected disvalue relative to present, certain harms?',
    'Comparative cost-effectiveness analysis conducted by parties with no funding or reputational stake in either framing, examining marginal harm-reduction per dollar/staff-hour across x-risk research versus present-harm remediation; independent audit of xrisk-institute funding sources and disclosure of conflicts with frontier-lab safety-branding.',
    'If the allocation is well-calibrated to genuinely comparable expected-value reasoning, this reading is closer to a rope with real coordination value; if the allocation reflects capture by well-resourced beneficiary institutions setting their own priority terms, the tangled_rope classification understates the extraction and it drifts toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(genuine_risk_vs_priority_capture, conceptual, 'Whether existential-risk prioritization reflects calibrated expected-value reasoning or beneficiary-institution capture of the governance agenda.').

omega_variable(
    future_humanity_representation_gap,
    'Can ''future humanity'' function as a coherent beneficiary/victim class for governance purposes when it has no mechanism to corroborate, contest, or correct the allocation made in its name?',
    'There is no empirical resolution mechanism in principle — this is a conceptual question about whether unrepresentable future stakeholders can ground present resource-allocation claims without a proxy-representation mechanism accountable to some observable check.',
    'If no coherent representation mechanism exists, claims made on behalf of future humanity function structurally as claims made by whoever currently holds the microphone (the agenda-setting institutions), which strengthens the case that the beneficiary/victim asymmetry is durable and self-reinforcing rather than a resolvable empirical question.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(future_humanity_representation_gap, conceptual, 'Whether unrepresented future beneficiaries can ground present governance priority claims without a corroboration mechanism.').

omega_variable(
    cs_framing_alternative_agenda_layer,
    'Should the kernel-adjudicating authority be read as the xrisk research institutes themselves (the obvious framing — they administer funding panels and advisory bodies), or as the deeper legitimacy claim that AGI-scenario risk is the correct lens for civilizational-scale governance at all (a framing layered above the institutions, closer to a paradigm than an administrator)?',
    'Track whether contestation, when it occurs, targets specific institutional funding decisions (supports the administrator framing) or targets the underlying longtermist/discontinuity paradigm itself (supports the paradigm framing) — examine legislative hearing transcripts and academic rebuttal literature for which target predominates.',
    'Under the administrator framing, cs_structure.authority_grounding = extraction (institutions extract benefit from kernel stability) is clearly correct. Under the paradigm framing, authority_grounding might instead resemble diffuse_epistemic (a loosely held expected-value framework rather than a specific institutional hierarchy), which would change how interpretation_layer_present should be read. This story adopts the administrator framing because the named beneficiaries (institutes, labs) are concrete, funded, and identifiable — but the alternative framing would classify authority differently.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_alternative_agenda_layer, conceptual, 'Whether the kernel''s adjudicating authority is the concrete beneficiary institutions or the underlying longtermist paradigm layered above them.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_governance_priority__existential_risk_reading, 2015, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_r_tr_t2015, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 2015, 0.15).
narrative_ontology:measurement(ai_r_tr_t2017, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 2017, 0.2).
narrative_ontology:measurement(ai_r_tr_t2019, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 2019, 0.26).
narrative_ontology:measurement(ai_r_tr_t2021, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 2021, 0.32).
narrative_ontology:measurement(ai_r_tr_t2023, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 2023, 0.37).
narrative_ontology:measurement(ai_r_tr_t2026, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 2026, 0.4).

% Extraction over time
narrative_ontology:measurement(ai_r_be_t2015, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 2015, 0.3).
narrative_ontology:measurement(ai_r_be_t2017, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 2017, 0.36).
narrative_ontology:measurement(ai_r_be_t2019, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 2019, 0.42).
narrative_ontology:measurement(ai_r_be_t2021, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 2021, 0.48).
narrative_ontology:measurement(ai_r_be_t2023, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 2023, 0.54).
narrative_ontology:measurement(ai_r_be_t2026, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 2026, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(ai_r_su_t2015, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 2015, 0.2).
narrative_ontology:measurement(ai_r_su_t2017, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 2017, 0.24).
narrative_ontology:measurement(ai_r_su_t2019, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 2019, 0.29).
narrative_ontology:measurement(ai_r_su_t2021, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 2021, 0.34).
narrative_ontology:measurement(ai_r_su_t2023, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 2023, 0.39).
narrative_ontology:measurement(ai_r_su_t2026, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 2026, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_governance_priority__existential_risk_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ai_risk_governance_priority__existential_risk_reading, 0.12).
narrative_ontology:affects_constraint(ai_risk_governance_priority__existential_risk_reading, ai_risk_governance_priority__near_term_harms_reading).
narrative_ontology:affects_constraint(ai_risk_governance_priority__existential_risk_reading, ai_risk_governance_priority__bridge_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraints decomposing the natural-language claim 'AI risk governance must prioritize X' per the ε-invariance principle. Each reading (existential_risk_reading, near_term_harms_reading, bridge_reading) has its own ε, beneficiary/victim structure, and classification — they are not the same constraint viewed from different angles. This reading shows higher ε on speculative-capability governance and lower ε on present algorithmic-harm governance; the near_term_harms_reading sibling shows the inverse ε profile with a different victim/beneficiary set (present-harm-affected populations as victims of neglect become the coordination beneficiaries under that reading's frame); the bridge_reading sibling attempts to hold both without the resource-diversion structure this reading identifies, and should show correspondingly lower ε and different (or absent) tangled_rope gating.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_risk_governance_priority__existential_risk_reading, analytical, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
