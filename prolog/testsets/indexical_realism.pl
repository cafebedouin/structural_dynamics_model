% ============================================================================
% CONSTRAINT STORY: indexical_realism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_indexical_realism, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: indexical_realism
 *   human_readable: Indexical Realism: Visual Evidence Authority via Detection Infrastructure
 *   domain: technology_governance/information_epistemology/digital_forensics
 *
 * SUMMARY:
 *   This constraint instantiates the indexical realism reading of visual
 *   evidentiary authority: the claim that detection methods can keep pace
 *   with synthesis, that expert analysis remains authoritative, and that
 *   institutional verification infrastructure can adapt to maintain epistemic
 *   ground truth. The constraint coordinates genuine verification capacity
 *   while extracting asymmetrically from those denied access to detection
 *   tools. The claim/metric gap is deliberate: claimed as tangled_rope
 *   (coordination + extraction) while metrics show substantial and rising
 *   extraction as the arms race intensifies and access costs increase.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(indexical_realism, 0.68).
domain_priors:suppression_score(indexical_realism, 0.72).
domain_priors:theater_ratio(indexical_realism, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(indexical_realism, extractiveness, 0.68).
narrative_ontology:constraint_metric(indexical_realism, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(indexical_realism, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(indexical_realism, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(indexical_realism, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(indexical_realism, tangled_rope).
narrative_ontology:human_readable(indexical_realism, "Indexical Realism: Visual Evidence Authority via Detection Infrastructure").
narrative_ontology:topic_domain(indexical_realism, "technology_governance/information_epistemology/digital_forensics").

domain_priors:requires_active_enforcement(indexical_realism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(indexical_realism, '5cc5155d-ebb8-4576-a4af-30b8b82b265f').
narrative_ontology:cs_kernel_codification('5cc5155d-ebb8-4576-a4af-30b8b82b265f', distributed).
narrative_ontology:cs_authority_grounding('5cc5155d-ebb8-4576-a4af-30b8b82b265f', expertise).
narrative_ontology:cs_interpretation_layer_present('5cc5155d-ebb8-4576-a4af-30b8b82b265f').
narrative_ontology:cs_reading_relation('5cc5155d-ebb8-4576-a4af-30b8b82b265f', visual_evidentiary_authority__epistemic_collapse, coexists_with).
narrative_ontology:cs_reading_relation('5cc5155d-ebb8-4576-a4af-30b8b82b265f', visual_evidentiary_authority__distributed_verification, influences).
narrative_ontology:cs_reading_relation('5cc5155d-ebb8-4576-a4af-30b8b82b265f', visual_evidentiary_authority__post_evidentiary, coexists_with).
narrative_ontology:cs_axiom('5cc5155d-ebb8-4576-a4af-30b8b82b265f', foundational, indexical_traces_recoverable).
narrative_ontology:cs_axiom_status(indexical_traces_recoverable, holdable).
narrative_ontology:cs_axiom_grounding('5cc5155d-ebb8-4576-a4af-30b8b82b265f', indexical_traces_recoverable, empirically_contingent).
narrative_ontology:cs_axiom('5cc5155d-ebb8-4576-a4af-30b8b82b265f', foundational, expert_analysis_authoritative).
narrative_ontology:cs_axiom_status(expert_analysis_authoritative, holdable).
narrative_ontology:cs_axiom_grounding('5cc5155d-ebb8-4576-a4af-30b8b82b265f', expert_analysis_authoritative, conventional).
narrative_ontology:cs_reference_frame('5cc5155d-ebb8-4576-a4af-30b8b82b265f', pre_generative_indexical_authority).
narrative_ontology:cs_drift_state('5cc5155d-ebb8-4576-a4af-30b8b82b265f', post_diffusion_model_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('5cc5155d-ebb8-4576-a4af-30b8b82b265f', '').
narrative_ontology:cs_kernel_id(indexical_realism, visual_evidentiary_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(indexical_realism, forensic_verification_institutions).
narrative_ontology:constraint_beneficiary(indexical_realism, credentialed_detection_experts).
narrative_ontology:constraint_beneficiary(indexical_realism, platform_authentication_services).
narrative_ontology:constraint_victim(indexical_realism, resource_constrained_journalists).
narrative_ontology:constraint_victim(indexical_realism, independent_fact_checkers).
narrative_ontology:constraint_victim(indexical_realism, global_south_verification_seekers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate the detection infrastructure and set standards for what counts as verified. They maintain proprietary detection models, credential expert analysts, and provide verification-as-a-service to courts, newsrooms, and platforms. Their authority rests on the claim that indexical traces remain recoverable through expert analysis, which justifies the infrastructure investment and gatekeeping.
narrative_ontology:constraint_stakeholder(indexical_realism, forensic_verification_institutions, agenda_setter,
    institutional, generational, arbitrage, global).

% Hold specialized training in forensic media analysis and are certified by the institutions. They are hired as expert witnesses, platform consultants, and newsroom advisors. Their professional identity and income depend on detection remaining a specialized skill rather than a commodity tool.
narrative_ontology:constraint_stakeholder(indexical_realism, credentialed_detection_experts, beneficiary,
    powerful, biographical, mobile, global).

% Integrate institutional verification APIs into content moderation pipelines. They benefit from outsourcing epistemic authority to credentialed third parties, which shields them from direct accountability for verification failures while maintaining the appearance of rigorous fact-checking.
narrative_ontology:constraint_stakeholder(indexical_realism, platform_authentication_services, beneficiary,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(indexical_realism, platform_authentication_services, agenda_setter).

% Need to verify visual evidence for reporting but cannot afford institutional verification services or proprietary detection tools. They must either trust platform labels, defer to credentialed experts they cannot hire, or publish unverified material and risk credibility loss. The cost structure makes verification a luxury good.
narrative_ontology:constraint_stakeholder(indexical_realism, resource_constrained_journalists, payer,
    moderate, biographical, constrained, national).

% Operate outside institutional newsrooms with minimal budgets. They face the same verification cost barrier as resource-constrained journalists but with even less institutional backing. Open-source detection tools exist but lag proprietary models by months or years, and lack the evidentiary weight of institutional certification.
narrative_ontology:constraint_stakeholder(indexical_realism, independent_fact_checkers, payer,
    moderate, biographical, constrained, regional).

% Encounter synthetic or manipulated media in local information ecosystems but have no access to verification infrastructure. Institutional services are priced for Western markets and optimized for Western media formats. They bear the epistemic costs of unverifiable information flows without the tools to restore ground truth.
narrative_ontology:constraint_stakeholder(indexical_realism, global_south_verification_seekers, payer,
    powerless, immediate, trapped, regional).

% Argue that verification tools should be open-source, decentralized, and accessible at marginal cost. They build alternative detection models and publish adversarial examples, but their work is dismissed as insufficiently rigorous or is co-opted into proprietary systems. The institutional framing treats democratized verification as a threat to epistemic order.
narrative_ontology:constraint_stakeholder(indexical_realism, open_verification_advocates, excluded,
    organized, generational, constrained, global).

% Study the arms race between synthesis and detection, document the accessibility gap, and measure verification accuracy across institutional and open-source tools. They see the full structure: genuine coordination function (detection does work), asymmetric extraction (access is gatekept), and the unresolved question of whether the arms race is fundamentally winnable.
narrative_ontology:constraint_stakeholder(indexical_realism, computational_media_researchers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism to distinguish authentic indexical traces from synthetic or manipulated media, restoring epistemic ground truth in contexts where visual evidence is contested. Detection infrastructure solves the collective problem of verification at scale.
% TRANSFER_FUNCTION: Moves verification authority and associated revenue from distributed, low-cost analysis to centralized, credentialed institutions. Resource-constrained actors pay in foregone verification capacity; institutions collect in service fees, expert witness income, and epistemic gatekeeping power.
% ABSENT_VOICES: Open verification advocates and global south verification seekers are structurally excluded from standard-setting. Their proposals for democratized detection tools are treated as epistemically suspect or technically infeasible, even when they demonstrate comparable accuracy to proprietary systems.
% DISAPPEARANCE_RATIONALE: If institutional verification infrastructure vanished overnight, newsrooms and courts would scramble for alternative authentication methods, open-source detection tools would proliferate rapidly, and the epistemic authority of visual evidence would fragment across competing verification communities. The information economy would reorganize around distributed trust networks rather than centralized certification.
% FOUNDING_PROBLEM: Early generative models produced detectable artifacts; verification was straightforward. As synthesis quality improved, the founding problem was: how do we maintain evidentiary authority for visual media when generation becomes indistinguishable from capture?
% FOUNDING_PROBLEM_CORROBORATION: Forensic institutions attest the problem is live and solvable through expert analysis and infrastructure investment. Computational media researchers and open verification advocates attest the problem is partially solved but the solution is artificially gatekept; independent academic studies show open-source detection models achieve comparable accuracy to proprietary systems when given equivalent training data, suggesting the access barrier is economic rather than technical.
narrative_ontology:disappearance_verdict(indexical_realism, world_rearranges).
narrative_ontology:founding_problem_status(indexical_realism, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(indexical_realism, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-16',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-4-5-20250929', 'unspecified').
narrative_ontology:story_seed(indexical_realism, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(indexical_realism_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(indexical_realism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(indexical_realism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is substantial (0.68 at interval end) because verification access is gatekept by cost and credentialing, not by technical necessity—open-source models demonstrate comparable accuracy when resourced. Suppression is high (0.72) because the constraint's persistence depends on actively delegitimizing distributed verification and maintaining proprietary detection as the gold standard. Theater ratio is moderate (0.41): detection infrastructure does work, but a growing share of institutional activity defends the gatekeeping apparatus rather than improving verification accuracy. Accessibility collapse is moderate (0.58): alternatives exist but are systematically discredited. Resistance is substantial (0.64): open verification advocates and resource-constrained actors actively contest the institutional monopoly.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats should compute as coordination-dominant (detection solves a real problem, infrastructure investment is justified). The payer seats should compute as extraction-dominant (the access barrier is artificial, the cost structure is decoupled from marginal service provision). The observer seat sees both: genuine coordination function layered with asymmetric extraction sustained by suppressing open alternatives.
 *
 * DIRECTIONALITY LOGIC:
 *   Forensic institutions and credentialed experts are structural beneficiaries (collect verification revenue, control standards—d near beneficiary end). Resource-constrained journalists, independent fact-checkers, and global south verification seekers are targets (pay in foregone capacity or direct service fees, constrained or trapped exit—d near target end). Platform authentication services are mixed beneficiaries (outsource epistemic risk but remain dependent on institutional providers). Open verification advocates are excluded rather than coordinated.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (maintain evidentiary authority for visual media) remains live, but the institutional solution (centralized, credentialed verification) increasingly serves to extract rents from the verification function rather than to democratize epistemic access. The founding problem (distinguishing authentic from synthetic media) is real and ongoing, but the institutional framing treats open verification as a threat rather than a complement, which reveals the extraction component.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    arms_race_winnability,
    'Is the detection-synthesis arms race fundamentally winnable, or does it asymptotically approach parity where detection lags synthesis by a constant but non-zero interval?',
    'Longitudinal measurement of detection accuracy vs. synthesis quality over multiple model generations, controlling for resource investment. If detection accuracy plateaus below synthesis quality despite increasing investment, the arms race is unwinnable.',
    'If unwinnable, the indexical realism reading collapses and authority migrates to the epistemic_collapse or post_evidentiary readings. If winnable, the institutional infrastructure is justified as coordination rather than extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(arms_race_winnability, empirical, 'Whether detection can keep pace with synthesis at the technological frontier.').

omega_variable(
    access_barrier_necessity,
    'Is the cost and credentialing barrier to verification tools technically necessary, or is it an artificial scarcity maintained to preserve institutional authority?',
    'Controlled comparison of proprietary vs. open-source detection accuracy on identical test sets. If open-source models achieve parity when given equivalent training data, the barrier is artificial.',
    'If artificial, the extraction component dominates and the constraint reclassifies toward snare. If necessary, the coordination component dominates and the constraint remains tangled_rope or shifts toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(access_barrier_necessity, empirical, 'Whether verification access costs track technical necessity or institutional gatekeeping.').

omega_variable(
    distributed_verification_viability,
    'Can distributed, peer-based verification networks achieve comparable epistemic authority to centralized institutional certification, or does verification authority require hierarchical credentialing?',
    'Natural experiment from contexts where institutional verification is unavailable: measure epistemic outcomes (misinformation spread, contested evidence resolution) in distributed vs. centralized verification regimes.',
    'If distributed networks achieve parity, the institutional monopoly is unjustified and the constraint reclassifies as extractive. If hierarchical credentialing is necessary, the institutional structure is coordination-justified.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(distributed_verification_viability, conceptual, 'Whether verification authority is structurally centralized or can be distributed.').

omega_variable(
    sibling_reading_displacement,
    'Under what empirical conditions would this reading (indexical_realism) be displaced by a sibling reading (epistemic_collapse, distributed_verification, or post_evidentiary)?',
    'Measurement of detection lag, institutional verification cost trajectory, and epistemic authority migration. If detection lag exceeds a threshold, epistemic_collapse becomes structurally true. If distributed networks capture verification authority, distributed_verification displaces this reading. If visual evidence loses legal/journalistic standing, post_evidentiary displaces it.',
    'Displacement would reclassify the constraint entirely—different beneficiaries, different victims, different coordination function. This omega documents the kernel-level uncertainty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_displacement, conceptual, 'Conditions under which this reading of the kernel would be superseded by a sibling.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(indexical_realism, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inde_tr_t0, indexical_realism, theater_ratio, 0, 0.18).
narrative_ontology:measurement(inde_tr_t6, indexical_realism, theater_ratio, 6, 0.22).
narrative_ontology:measurement(inde_tr_t12, indexical_realism, theater_ratio, 12, 0.27).
narrative_ontology:measurement(inde_tr_t18, indexical_realism, theater_ratio, 18, 0.32).
narrative_ontology:measurement(inde_tr_t24, indexical_realism, theater_ratio, 24, 0.36).
narrative_ontology:measurement(inde_tr_t30, indexical_realism, theater_ratio, 30, 0.39).
narrative_ontology:measurement(inde_tr_t36, indexical_realism, theater_ratio, 36, 0.41).

% Extraction over time
narrative_ontology:measurement(inde_be_t0, indexical_realism, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(inde_be_t6, indexical_realism, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(inde_be_t12, indexical_realism, base_extractiveness, 12, 0.54).
narrative_ontology:measurement(inde_be_t18, indexical_realism, base_extractiveness, 18, 0.59).
narrative_ontology:measurement(inde_be_t24, indexical_realism, base_extractiveness, 24, 0.63).
narrative_ontology:measurement(inde_be_t30, indexical_realism, base_extractiveness, 30, 0.66).
narrative_ontology:measurement(inde_be_t36, indexical_realism, base_extractiveness, 36, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(inde_su_t0, indexical_realism, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(inde_su_t6, indexical_realism, suppression_requirement, 6, 0.53).
narrative_ontology:measurement(inde_su_t12, indexical_realism, suppression_requirement, 12, 0.58).
narrative_ontology:measurement(inde_su_t18, indexical_realism, suppression_requirement, 18, 0.63).
narrative_ontology:measurement(inde_su_t24, indexical_realism, suppression_requirement, 24, 0.67).
narrative_ontology:measurement(inde_su_t30, indexical_realism, suppression_requirement, 30, 0.7).
narrative_ontology:measurement(inde_su_t36, indexical_realism, suppression_requirement, 36, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(indexical_realism, information_standard).
narrative_ontology:boltzmann_floor_override(indexical_realism, 0.08).
narrative_ontology:affects_constraint(indexical_realism, epistemic_collapse).
narrative_ontology:affects_constraint(indexical_realism, distributed_verification).
narrative_ontology:affects_constraint(indexical_realism, post_evidentiary).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the visual_evidentiary_authority kernel. The indexical_realism reading holds that detection can keep pace with synthesis and that institutional verification can maintain epistemic ground truth. Sibling readings model alternative structural outcomes: epistemic_collapse (detection is unwinnable), distributed_verification (authority fragments), post_evidentiary (visual media lose evidentiary status). The readings share a kernel but have different ε values, different beneficiary/victim structures, and different persistence mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
