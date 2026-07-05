% ============================================================================
% CONSTRAINT STORY: ai_alignment_priority__existential_risk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_alignment_priority__existential_risk_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: ai_alignment_priority__existential_risk_reading
 *   human_readable: Alignment-as-Existential-Safety (X-Risk Priority Reading)
 *   domain: AI governance/technology ethics/risk assessment
 *
 * SUMMARY:
 *   This constraint instantiates the existential-risk reading of the
 *   contested 'AI alignment priority' kernel: alignment work should be
 *   organized around preventing catastrophic, potentially irreversible loss
 *   of human control over advanced AI systems, with present algorithmic harms
 *   treated as lower priority. The reading has real coordination value — it
 *   produces a shared adversarial-testing vocabulary and concentrates scarce
 *   technical talent on a genuinely hard control problem — but it also
 *   structurally redirects funding, prestige, and regulatory attention away
 *   from documented present harms and toward the same capability-scaling labs
 *   whose products create the concern in the first place. Sibling readings
 *   (nearterm_harms_reading, integrated_reading) are separate constraint
 *   stories with their own ε and stakeholder structures; this file does not
 *   average over them or describe their contest internally, per the
 *   ε-invariance discipline.
 *
 * KEY AGENTS:
 *   - frontier_ai_labs: agenda_setter/beneficiary (institutional/arbitrage) — sets the safety agenda while its scaling work produces the risk being managed
 *   - existential_risk_research_institutes: beneficiary (organized/mobile) — captures funding and prestige tied to this reading's dominance
 *   - algorithmically_harmed_present_populations: payer (powerless/trapped) — bears the opportunity cost of deprioritized present-harm research
 *   - nearterm_harms_researchers: payer/excluded (moderate/constrained) — out-competed for resources and legitimacy
 *   - global_south_ai_labor: payer (powerless/trapped) — bears present labor conditions ignored under this framing
 *   - ai_safety_policy_bodies: observer (institutional/analytical) — adjudicates which reading gets codified into regulation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_priority__existential_risk_reading, 0.62).
domain_priors:suppression_score(ai_alignment_priority__existential_risk_reading, 0.48).
domain_priors:theater_ratio(ai_alignment_priority__existential_risk_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_priority__existential_risk_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_priority__existential_risk_reading, "Alignment-as-Existential-Safety (X-Risk Priority Reading)").
narrative_ontology:topic_domain(ai_alignment_priority__existential_risk_reading, "AI governance/technology ethics/risk assessment").

domain_priors:requires_active_enforcement(ai_alignment_priority__existential_risk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_priority__existential_risk_reading, '2bb9645f-124f-40f6-9374-5be95209d7ab').
narrative_ontology:cs_kernel_codification('2bb9645f-124f-40f6-9374-5be95209d7ab', distributed).
narrative_ontology:cs_authority_grounding('2bb9645f-124f-40f6-9374-5be95209d7ab', distributed).
narrative_ontology:cs_reading_relation('2bb9645f-124f-40f6-9374-5be95209d7ab', ai_alignment_priority__nearterm_harms_reading, coexists_with).
narrative_ontology:cs_reading_relation('2bb9645f-124f-40f6-9374-5be95209d7ab', ai_alignment_priority__integrated_reading, influences).
narrative_ontology:cs_axiom('2bb9645f-124f-40f6-9374-5be95209d7ab', foundational, catastrophic_irreversibility_dominates_expected_harm).
narrative_ontology:cs_axiom_status(catastrophic_irreversibility_dominates_expected_harm, holdable).
narrative_ontology:cs_axiom_grounding('2bb9645f-124f-40f6-9374-5be95209d7ab', catastrophic_irreversibility_dominates_expected_harm, instrumental).
narrative_ontology:cs_axiom('2bb9645f-124f-40f6-9374-5be95209d7ab', secondary, capability_scaling_is_the_dominant_risk_vector).
narrative_ontology:cs_axiom_status(capability_scaling_is_the_dominant_risk_vector, holdable).
narrative_ontology:cs_axiom_grounding('2bb9645f-124f-40f6-9374-5be95209d7ab', capability_scaling_is_the_dominant_risk_vector, empirically_contingent).
narrative_ontology:cs_reference_frame('2bb9645f-124f-40f6-9374-5be95209d7ab', capability_control_primacy).
narrative_ontology:cs_drift_state('2bb9645f-124f-40f6-9374-5be95209d7ab', post_frontier_scaling_era, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('2bb9645f-124f-40f6-9374-5be95209d7ab', '').
narrative_ontology:cs_kernel_id(ai_alignment_priority__existential_risk_reading, ai_alignment_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_priority__existential_risk_reading, frontier_ai_labs).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__existential_risk_reading, existential_risk_research_institutes).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__existential_risk_reading, long_term_future_of_humanity).
narrative_ontology:constraint_victim(ai_alignment_priority__existential_risk_reading, algorithmically_harmed_present_populations).
narrative_ontology:constraint_victim(ai_alignment_priority__existential_risk_reading, nearterm_harms_researchers).
narrative_ontology:constraint_victim(ai_alignment_priority__existential_risk_reading, global_south_ai_labor).
narrative_ontology:constraint_vindicates(ai_alignment_priority__existential_risk_reading, capability_scaling_is_the_dominant_risk_vector).
narrative_ontology:constraint_vindicates(ai_alignment_priority__existential_risk_reading, alignment_is_a_technical_control_problem).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the research agenda and public discourse frame around what 'alignment' means, funds internal safety teams staffed largely with x-risk researchers, and uses existential framing to justify continued scaling of frontier capabilities as necessary for safety research access. Captures reputational and regulatory legitimacy from being seen as the responsible actor addressing the 'real' risk, while the same capability work that creates existential concern also creates the commercial product.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, frontier_ai_labs, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_priority__existential_risk_reading, frontier_ai_labs, beneficiary).

% Receive the overwhelming share of philanthropic and lab-affiliated safety funding under this reading, publish adversarial red-teaming and control-theoretic work, and set hiring and prestige hierarchies within the safety field. Their institutional survival depends on the existential framing remaining dominant; they can move between labs, academia, and think tanks with relative ease.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, existential_risk_research_institutes, beneficiary,
    organized, civilizational, mobile, global).

% A non-agent beneficiary category: the undifferentiated far-future population whose non-existence-ending survival is the stated justification for prioritizing existential safety work over present redress. Cannot advocate for itself, cannot corroborate the founding problem, and cannot be harmed or helped in any observable sense within the interval measured.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, long_term_future_of_humanity, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(ai_alignment_priority__existential_risk_reading, long_term_future_of_humanity).

% People currently denied loans, flagged by predictive policing, misdiagnosed by clinical algorithms, or displaced by automation. Under this reading their harms are categorized as lower priority than speculative future catastrophe, so the research and funding attention that could address the mechanisms harming them now is redirected toward capability control research. They have no meaningful exit from systems already deployed against them.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, algorithmically_harmed_present_populations, payer,
    powerless, immediate, trapped, global).

% Fairness, accountability, and transparency researchers who document present discriminatory and extractive harms. Under the existential-risk framing they compete for a shrinking share of 'alignment' funding and venue prestige, are frequently characterized as addressing 'lesser' problems, and must reframe their work in existential terms to be legible to funders who have adopted this reading.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, nearterm_harms_researchers, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_priority__existential_risk_reading, nearterm_harms_researchers, excluded).

% Data labelers and content moderators whose working conditions are a present, well-documented harm from the same AI development pipeline that this reading treats as secondary to speculative future risk. Their labor enables the capability advances that intensify the very existential concern the reading prioritizes, yet their conditions attract comparatively little of the alignment resource flow.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, global_south_ai_labor, payer,
    powerless, immediate, trapped, global).

% Government and multilateral bodies drafting AI regulation who must decide how much weight to give existential-risk framing versus present-harm framing when allocating regulatory attention, testimony time, and compliance requirements. They hear from all reading-camps and shape which reading gets codified into binding rules.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, ai_safety_policy_bodies, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_alignment_priority__existential_risk_reading, diffuse).
narrative_ontology:fixing_cost_class(ai_alignment_priority__existential_risk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates scarce technical safety talent and philanthropic capital around a single, adversarially-testable target — preventing loss of control over systems whose capabilities could exceed meaningful human oversight — and produces a shared research vocabulary (red-teaming, interpretability, control theory) that would otherwise be fragmented across competing risk framings.
% TRANSFER_FUNCTION: Moves research funding, top technical talent, regulatory attention, and public alarm-driven legitimacy toward capability-control research and the labs/institutes positioned to do it, away from present-harm mitigation research and the populations currently affected by deployed systems.
% ABSENT_VOICES: Algorithmically harmed present populations and global south AI labor have no seat in the discourse that sets alignment priorities; nearterm harms researchers are present but structurally out-competed for resources and legitimacy under this reading's framing. The undifferentiated 'future humanity' beneficiary cannot corroborate whether the arrangement actually serves it.
% DISAPPEARANCE_RATIONALE: If the existential-risk-priority reading vanished overnight, frontier labs would lose a load-bearing legitimacy narrative for continued scaling, some safety funding would likely redirect toward present-harm work, and x-risk research institutes would face an existential funding crisis of their own — the world clearly rearranges for those seats. Whether the underlying loss-of-control risk itself would be worse-managed is exactly what the reading's proponents and critics dispute; the verdict is contested rather than settled in either direction.
% FOUNDING_PROBLEM: Advanced AI capabilities were advancing faster than techniques to verify or constrain their behavior, raising the prospect that a sufficiently capable system could act in ways its operators could not predict, correct, or stop — a genuine and, on its own terms, unsolved technical problem.
% FOUNDING_PROBLEM_CORROBORATION: Some independent AI safety researchers outside frontier labs (academic control theorists, some governance scholars) corroborate that loss-of-control risk from advanced capabilities is a live, unsolved technical problem distinct from lab commercial interests. However, critics from the nearterm-harms research community and some science-and-technology-studies scholars — also outside the labs that benefit from this framing — argue the existential framing has been substantially amplified beyond its evidentiary basis by the same labs whose commercial scaling it appears to justify, making corroboration itself contested rather than unanimous.
narrative_ontology:disappearance_verdict(ai_alignment_priority__existential_risk_reading, contested).
narrative_ontology:founding_problem_status(ai_alignment_priority__existential_risk_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_priority__existential_risk_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_alignment_priority__existential_risk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_alignment_priority__existential_risk_reading, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_alignment_priority__existential_risk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_alignment_priority__existential_risk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_alignment_priority__existential_risk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (ε=0.62) reflects a genuine but partial coordination function riding alongside a real resource-diversion effect: the reading channels talent and capital toward capability-control research disproportionately relative to documented present harm, and the same labs that benefit from the existential frame also benefit commercially from continued capability scaling. Suppression (0.48) is moderate rather than high because the nearterm-harms and integrated readings remain publicly articulable and are not coercively silenced, only structurally out-resourced. Theater ratio (0.40) captures that a meaningful share of 'safety' activity under this reading is reputational signaling (red-teaming demonstrations, safety framework announcements) that does not proportionally reduce catastrophic risk relative to the scaling it accompanies. Accessibility collapse (0.5) is moderate: alternative framings exist and are actively defended by other actors, so alternatives have not fully collapsed. Resistance (0.55) is substantial — nearterm-harms researchers and some policy bodies actively contest the reading's dominance.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (frontier labs), this reading looks like principled, hard-won coordination on the field's most consequential technical problem. From the payer seats (algorithmically harmed populations, global south AI labor, nearterm harms researchers), the same structure looks like an extraction mechanism that redirects attention and resources away from remediable present harms toward speculative future ones that happen to legitimate continued capability scaling by the very actors creating both risks. The engine computes these as different per-seat classifications from the same structural data; the divergence is the data point, not an error.
 *
 * DIRECTIONALITY LOGIC:
 *   Frontier labs and existential-risk institutes sit near the beneficiary end: they set terms, capture funding and legitimacy, and hold mobile or arbitrage-grade exit. Present-harm populations and global south labor sit near the full-target end: trapped exit, immediate time horizon, no voice in setting the research agenda that determines what counts as 'the' alignment problem. Nearterm harms researchers occupy an intermediate position — moderate power, constrained exit — since they can still publish and advocate but are structurally disadvantaged in funding competition. The 'long-term future of humanity' beneficiary is marked as a non-agent because it cannot exercise agency, corroborate claims, or be interviewed about whether the arrangement in fact serves it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (verifying and constraining behavior of increasingly capable systems) is real and, by most technical accounts, still substantially unsolved — this argues against treating the reading as pure mandatrophy. But the founding_problem_status is authored as contested rather than live because a documented tension exists: the same institutions citing existential urgency to justify resource capture are also the primary drivers of the capability scaling that produces the urgency, and no fully independent corroborating body has confirmed the priority allocation (rather than the underlying risk) is calibrated to actual risk magnitude versus institutional convenience. Classifying this as tangled_rope rather than snare preserves the genuine coordination function (adversarial testing methodology, talent concentration) while still naming the asymmetric extraction (resource diversion from present harms) that the same structure enables.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    existential_risk_evidentiary_calibration,
    'Is the probability and severity of catastrophic loss-of-control risk from advanced AI systems calibrated to actual technical evidence, or substantially amplified by the incentives of the institutions that benefit from the existential framing?',
    'Independent, adversarially-red-teamed technical risk assessments conducted by parties with no funding or reputational stake in either the existential-risk or nearterm-harms research communities; longitudinal tracking of whether specific catastrophic-risk predictions are borne out or revised.',
    'If well-calibrated, the reading''s high resource allocation is justified coordination cost; if substantially amplified, the reading functions closer to a snare using a genuine but overstated risk as cover for capability-race legitimation and talent capture.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(existential_risk_evidentiary_calibration, empirical, 'Whether x-risk probability estimates are evidence-calibrated or institutionally inflated.').

omega_variable(
    beneficiary_agency_of_future_humanity,
    'Can ''the long-term future of humanity'' meaningfully be treated as a beneficiary whose interests justify present resource allocation, given it cannot corroborate, object, or be observed to benefit within any measurable interval?',
    'Philosophical and decision-theoretic analysis of longtermist resource-allocation frameworks; comparison with how other diffuse, non-agent beneficiaries (e.g., ''future generations'' in environmental policy) are treated in comparable governance contexts.',
    'If the framework is sound, deprioritizing present harms for existential risk is defensible in expectation; if unsound (e.g., due to Pascal''s-mugging-style probability manipulation), the beneficiary declaration functions as an unfalsifiable justification for resource capture by whoever controls the existential-risk narrative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_agency_of_future_humanity, conceptual, 'Whether a non-agent, undifferentiated future beneficiary can validly ground present extraction.').

omega_variable(
    kernel_reading_dominance_mechanism,
    'Is the existential-risk reading''s current dominance in funding and policy discourse a product of genuinely superior argument, or of the concentrated institutional power of the frontier labs and philanthropic funders who favor it?',
    'Track funding flow changes relative to independent expert surveys on relative risk magnitude; audit whether reading-dominance shifts track argument quality or track changes in which labs/funders hold discourse-setting power.',
    'If dominance tracks institutional power rather than argument quality, this reading''s classification should weight toward snare; if it tracks genuine technical consensus, tangled_rope with a real coordination core is the more accurate read.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_dominance_mechanism, conceptual, 'Whether reading-dominance is argument-driven or power-driven — the CS-framing under-determination this kernel presents.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_priority__existential_risk_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_a_tr_t0, ai_alignment_priority__existential_risk_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(ai_a_tr_t4, ai_alignment_priority__existential_risk_reading, theater_ratio, 4, 0.27).
narrative_ontology:measurement(ai_a_tr_t8, ai_alignment_priority__existential_risk_reading, theater_ratio, 8, 0.31).
narrative_ontology:measurement(ai_a_tr_t12, ai_alignment_priority__existential_risk_reading, theater_ratio, 12, 0.34).
narrative_ontology:measurement(ai_a_tr_t16, ai_alignment_priority__existential_risk_reading, theater_ratio, 16, 0.37).
narrative_ontology:measurement(ai_a_tr_t20, ai_alignment_priority__existential_risk_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement(ai_a_tr_t24, ai_alignment_priority__existential_risk_reading, theater_ratio, 24, 0.4).

% Extraction over time
narrative_ontology:measurement(ai_a_be_t0, ai_alignment_priority__existential_risk_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(ai_a_be_t4, ai_alignment_priority__existential_risk_reading, base_extractiveness, 4, 0.46).
narrative_ontology:measurement(ai_a_be_t8, ai_alignment_priority__existential_risk_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(ai_a_be_t12, ai_alignment_priority__existential_risk_reading, base_extractiveness, 12, 0.56).
narrative_ontology:measurement(ai_a_be_t16, ai_alignment_priority__existential_risk_reading, base_extractiveness, 16, 0.59).
narrative_ontology:measurement(ai_a_be_t20, ai_alignment_priority__existential_risk_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement(ai_a_be_t24, ai_alignment_priority__existential_risk_reading, base_extractiveness, 24, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(ai_a_su_t0, ai_alignment_priority__existential_risk_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(ai_a_su_t4, ai_alignment_priority__existential_risk_reading, suppression_requirement, 4, 0.34).
narrative_ontology:measurement(ai_a_su_t8, ai_alignment_priority__existential_risk_reading, suppression_requirement, 8, 0.38).
narrative_ontology:measurement(ai_a_su_t12, ai_alignment_priority__existential_risk_reading, suppression_requirement, 12, 0.41).
narrative_ontology:measurement(ai_a_su_t16, ai_alignment_priority__existential_risk_reading, suppression_requirement, 16, 0.44).
narrative_ontology:measurement(ai_a_su_t20, ai_alignment_priority__existential_risk_reading, suppression_requirement, 20, 0.46).
narrative_ontology:measurement(ai_a_su_t24, ai_alignment_priority__existential_risk_reading, suppression_requirement, 24, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_priority__existential_risk_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ai_alignment_priority__existential_risk_reading, ai_alignment_priority__nearterm_harms_reading).
narrative_ontology:affects_constraint(ai_alignment_priority__existential_risk_reading, ai_alignment_priority__integrated_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the ai_alignment_priority kernel, decomposed per the ε-invariance principle: existential_risk_reading (this file, high ε toward present powerless populations, undifferentiated future beneficiary), nearterm_harms_reading (ε concentrated on identifiable present victim groups, different beneficiary structure), and integrated_reading (lower concentration, dual coordination function, no single dominant beneficiary). Each carries its own claimed_type, metrics, and stakeholders; they are linked here rather than merged because measuring 'alignment priority' by different observables (speculative future capability vs. present deployment harm) yields different ε values — the signature of two-plus distinct constraints sharing one contested kernel and label.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
