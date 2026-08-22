% ============================================================================
% CONSTRAINT STORY: ai_alignment_commitment__ethics_justice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_alignment_commitment__ethics_justice_reading, []).

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
 *   constraint_id: ai_alignment_commitment__ethics_justice_reading
 *   human_readable: Alignment-as-Bias-and-Present-Harm-Prevention (Ethics/Justice Reading)
 *   domain: AI governance / technology ethics / risk assessment
 *
 * SUMMARY:
 *   This story instantiates the ethics/justice reading of the contested 'AI
 *   alignment' kernel: alignment is defined here as the prevention of
 *   reproduction of social bias and present-day harm against identifiable,
 *   currently affected populations. The reading genuinely coordinates ethics
 *   researchers, trust-and-safety practitioners, and civil-society advocates
 *   around measurable, documented harms — credit discrimination, biased
 *   hiring screens, moderation labor exploitation, predictive-policing bias.
 *   But the same coordination structure also extracts: it redirects the
 *   scarce institutional resource of the 'alignment' label and its associated
 *   funding away from long-horizon control research, and it relies on the
 *   continuing existence of documented present harms (and on
 *   moderation/labeling labor) as the basis for its own institutional
 *   standing. This is a tangled rope, not a rope: the coordination function
 *   is real and the extraction from long-term safety researchers and from
 *   moderation workers is asymmetric and requires active enforcement (grant
 *   committees, conference program committees, hiring criteria) to sustain
 *   the label's current allocation.
 *
 * KEY AGENTS:
 *   - ai_ethics_research_institutes: agenda-setting beneficiary that defines the operational metrics
 *   - trust_and_safety_teams: agenda-setter/payer implementing the mandate operationally
 *   - marginalized_communities_subject_to_biased_systems: beneficiary-in-name, payer-in-fact — trapped inside the very systems the reading exists to fix
 *   - gig_and_content_moderation_workers: payer bearing direct labor costs of the harm-detection pipeline
 *   - long_term_safety_researchers: payer whose research agenda loses resource priority under this reading
 *   - civil_society_advocacy_orgs: beneficiary gaining policy standing from championing this framing
 *   - frontier_ai_labs: excluded from this reading's own beneficiary/victim accounting despite funding both sides of the kernel dispute
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_commitment__ethics_justice_reading, 0.58).
domain_priors:suppression_score(ai_alignment_commitment__ethics_justice_reading, 0.42).
domain_priors:theater_ratio(ai_alignment_commitment__ethics_justice_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_commitment__ethics_justice_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(ai_alignment_commitment__ethics_justice_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_commitment__ethics_justice_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(ai_alignment_commitment__ethics_justice_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_commitment__ethics_justice_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_commitment__ethics_justice_reading, "Alignment-as-Bias-and-Present-Harm-Prevention (Ethics/Justice Reading)").
narrative_ontology:topic_domain(ai_alignment_commitment__ethics_justice_reading, "AI governance / technology ethics / risk assessment").

domain_priors:requires_active_enforcement(ai_alignment_commitment__ethics_justice_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_commitment__ethics_justice_reading, '132579b7-915e-4c12-94c5-d126628af071').
narrative_ontology:cs_kernel_codification('132579b7-915e-4c12-94c5-d126628af071', distributed).
narrative_ontology:cs_authority_grounding('132579b7-915e-4c12-94c5-d126628af071', distributed).
narrative_ontology:cs_reading_relation('132579b7-915e-4c12-94c5-d126628af071', ai_alignment_commitment__safety_control_reading, coexists_with).
narrative_ontology:cs_reading_relation('132579b7-915e-4c12-94c5-d126628af071', ai_alignment_commitment__integrated_reading, influences).
narrative_ontology:cs_axiom('132579b7-915e-4c12-94c5-d126628af071', foundational, present_documented_harm_has_moral_priority).
narrative_ontology:cs_axiom_status(present_documented_harm_has_moral_priority, holdable).
narrative_ontology:cs_axiom_grounding('132579b7-915e-4c12-94c5-d126628af071', present_documented_harm_has_moral_priority, deontological).
narrative_ontology:cs_axiom('132579b7-915e-4c12-94c5-d126628af071', secondary, speculative_future_risk_cannot_outweigh_certain_present_injury).
narrative_ontology:cs_axiom_status(speculative_future_risk_cannot_outweigh_certain_present_injury, holdable).
narrative_ontology:cs_axiom_grounding('132579b7-915e-4c12-94c5-d126628af071', speculative_future_risk_cannot_outweigh_certain_present_injury, instrumental).
narrative_ontology:cs_reference_frame('132579b7-915e-4c12-94c5-d126628af071', harm_reduction_primacy_framework).
narrative_ontology:cs_drift_state('132579b7-915e-4c12-94c5-d126628af071', post_generative_ai_scaling_era, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('132579b7-915e-4c12-94c5-d126628af071', '').
narrative_ontology:cs_kernel_id(ai_alignment_commitment__ethics_justice_reading, ai_alignment_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__ethics_justice_reading, ai_ethics_research_institutes).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__ethics_justice_reading, trust_and_safety_teams).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__ethics_justice_reading, civil_society_advocacy_orgs).
narrative_ontology:constraint_victim(ai_alignment_commitment__ethics_justice_reading, marginalized_communities_subject_to_biased_systems).
narrative_ontology:constraint_victim(ai_alignment_commitment__ethics_justice_reading, gig_and_content_moderation_workers).
narrative_ontology:constraint_victim(ai_alignment_commitment__ethics_justice_reading, long_term_safety_researchers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__ethics_justice_reading, marginalized_communities_subject_to_biased_systems).
narrative_ontology:constraint_victim(ai_alignment_commitment__ethics_justice_reading, trust_and_safety_teams).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines what counts as 'alignment work' inside major labs and conferences by setting bias-audit standards, fairness metrics, and red-team protocols. Draws funding, hiring pipelines, and institutional prestige from being the recognized authority on what present-day harm looks like and how to measure it.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, ai_ethics_research_institutes, agenda_setter,
    institutional, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_commitment__ethics_justice_reading, ai_ethics_research_institutes, beneficiary).

% Implements the bias-mitigation and content policy pipelines that operationalize this reading of alignment. Absorbs the operational cost of continuous re-tuning, faces internal pressure to ship, and is blamed by both sides when systems either censor too much or still produce biased outputs.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, trust_and_safety_teams, agenda_setter,
    organized, immediate, constrained, national).
narrative_ontology:stakeholder_secondary_role(ai_alignment_commitment__ethics_justice_reading, trust_and_safety_teams, payer).

% Currently experience discriminatory outcomes from credit scoring, hiring screens, content moderation, and predictive policing systems. This reading names them as the reason alignment work exists, directs resources toward documenting their harms, but leaves them dependent on the same institutions' good faith and without exit from the systems themselves — their harm is the constraint's justification and also its ongoing cost.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, marginalized_communities_subject_to_biased_systems, beneficiary,
    powerless, immediate, trapped, national).
narrative_ontology:stakeholder_secondary_role(ai_alignment_commitment__ethics_justice_reading, marginalized_communities_subject_to_biased_systems, payer).

% Perform the labeling, red-teaming, and moderation labor that generates the datasets and judgments used to detect and correct bias. Bear direct psychological and material costs (exposure to harmful content, precarious contract status) in service of a harm-reduction mandate they did not set and cannot bargain over.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, gig_and_content_moderation_workers, payer,
    powerless, immediate, trapped, global).

% Work on catastrophic-risk and control problems (interpretability, scalable oversight) and find funding, publication venues, and institutional attention redirected toward present-harm framing under this reading's ascendant definition of 'alignment.' Their research questions are not refuted, simply deprioritized in the allocation of the shared 'alignment' label and its resources.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, long_term_safety_researchers, payer,
    moderate, civilizational, constrained, global).

% Gain standing, funding, and a seat at policy tables by advancing this reading — testifying before legislatures, publishing audits, shaping procurement rules. Genuinely represent affected communities but also build durable institutional positions contingent on present-harm framing remaining the dominant lens.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, civil_society_advocacy_orgs, beneficiary,
    organized, generational, mobile, national).

% Fund and staff alignment work under whichever definition is politically and commercially advantageous at a given moment; not structurally excluded from the conversation but excluded from this reading's victim/beneficiary accounting because the reading is authored from the justice advocates' vantage, not the labs'.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, frontier_ai_labs, excluded,
    institutional, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_alignment_commitment__ethics_justice_reading, diffuse).
narrative_ontology:fixing_cost_class(ai_alignment_commitment__ethics_justice_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates researchers, advocates, and trust-and-safety teams around a shared operational definition of 'alignment' that directs audit resources, hiring, and policy attention toward measurable, present-tense harms to identifiable populations rather than toward speculative future risks.
% TRANSFER_FUNCTION: Moves institutional attention, research funding, and the 'alignment' label itself away from long-horizon control research and toward bias-audit and harm-mitigation work; moves labor costs onto moderation and labeling workers; moves reputational and policy capital toward the institutions and advocacy groups who champion this reading.
% ABSENT_VOICES: Long-term safety researchers whose catastrophic-risk framing is deprioritized rarely appear as named parties in this reading's own accounting — they are treated as a resource competitor, not a stakeholder with a legitimate present claim, even though the kernel dispute is partly about them. Affected communities themselves are frequently invoked but not directly consulted in setting the metrics used on their behalf.
% DISAPPEARANCE_RATIONALE: If this reading's institutional dominance vanished overnight, bias-audit teams would lose their primary justification for headcount and mandate, civil-society orgs would lose a key policy lever, and resource allocation inside labs would likely swing toward control/interpretability work; documented present-day harms to marginalized communities would not disappear but would lose their current organizational champions.
% FOUNDING_PROBLEM: Early deployed ML systems (credit, hiring, policing, content moderation) demonstrably reproduced and amplified existing social biases against marginalized groups, and no institutional mechanism inside AI development addressed this as a first-order problem.
% FOUNDING_PROBLEM_CORROBORATION: Independent audits (e.g. academic algorithmic-fairness studies, journalistic investigations of hiring and lending algorithms) attest the founding problem remains live and outside the control of the institutions that now administer this reading. However, the SCOPE and PRIORITY given to it relative to catastrophic-risk work is contested even among researchers who accept the founding problem's reality — that prioritization dispute is not corroborated from outside the advocacy and ethics-institute seats themselves.
narrative_ontology:disappearance_verdict(ai_alignment_commitment__ethics_justice_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_alignment_commitment__ethics_justice_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_commitment__ethics_justice_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_alignment_commitment__ethics_justice_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_alignment_commitment__ethics_justice_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_alignment_commitment__ethics_justice_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_alignment_commitment__ethics_justice_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_alignment_commitment__ethics_justice_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects genuine, substantial resource redirection away from long-horizon safety work, but not maximal extraction — the coordination function (real, documented harm reduction) is genuine and partially offsets the redirection cost. Suppression (0.42) is moderate: there is real institutional pressure (grant panels, hiring committees, conference gatekeeping) enforcing this definitional dominance, but dissenting researchers can and do publish, fund, and organize outside it — exit is constrained, not foreclosed. Theater ratio (0.4) is nontrivial because some bias-audit activity has become compliance-oriented box-checking (published fairness reports with limited downstream effect) rather than a driver of actual system change, and this share has grown over the interval as institutionalization matured. Accessibility collapse (0.35) is moderate-low: alternative framings of alignment remain visible and contested in the field, unlike a genuinely naturalized constraint. Resistance (0.62) is high because long-term safety researchers and their institutional allies actively contest the resource reallocation implicit in this reading, producing an ongoing structural fight over what 'alignment' means.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setting seat (ethics institutes, trust-and-safety teams), the arrangement reads as overdue correction of a real, structurally neglected problem — a rope. From the long-term safety researcher's seat, the same institutional machinery reads as extraction: a scarce label and its attached resources reallocated by advocacy pressure rather than by risk-weighted analysis. From the moderation worker's seat, it reads as uncompensated, high-cost labor performed in service of a mandate set entirely by others. The engine's per-seat computation should reflect these divergent experiences of one structural arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Ai_ethics_research_institutes and civil_society_advocacy_orgs are declared beneficiaries because they gain funding, standing, and agenda-setting power from this reading's ascendancy — low d. Marginalized_communities_subject_to_biased_systems occupy a genuinely dual position: nominal beneficiary (the reading exists for them) but structurally trapped payer (they cannot exit the biased systems and bear the ongoing cost of documented harm regardless of the institutional response) — this asymmetry is intentional and central to the tangled-rope classification. Gig_and_content_moderation_workers and long_term_safety_researchers are declared victims because resources, funding, and labor costs flow away from them toward the dominant institutional framing, with constrained or trapped exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (documented algorithmic bias against marginalized groups) remains demonstrably live, corroborated by sources outside the benefiting institutions (independent academic audits, journalism) — this is not a case of an arrangement persisting after its function died. What is contested, and what this reading's own accounting cannot self-certify, is whether the CURRENT SCALE of resource allocation toward this framing (relative to control-risk research) is proportionate to the founding problem's continuing severity, or whether institutional incumbency has outrun the marginal harm-reduction return. Classifying this as tangled_rope rather than snare preserves the genuine coordination function (harm documentation and mitigation are real and effective) while still registering the asymmetric extraction from the deprioritized research community — collapsing it to snare would deny the real present-harm-reduction achieved; collapsing it to rope would deny the resource-reallocation cost borne by long-term safety work.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    resource_allocation_proportionality,
    'Is the redirection of the ''alignment'' label and its associated funding toward present-harm work proportionate to the founding problem''s severity, or has institutional incumbency (grant committees, hiring pipelines, conference gatekeeping favoring this framing) outrun the marginal harm-reduction return relative to underfunded control research?',
    'Comparative analysis of funding flows against independently measured harm-reduction outcomes (bias-audit efficacy studies) versus independently assessed catastrophic-risk timelines and tractability estimates; would require adjudication outside either advocacy community.',
    'If proportionate, this reading functions closer to a rope with acceptable coordination overhead; if disproportionate, the tangled_rope classification understates extraction and the arrangement drifts toward snare with long-term safety researchers as clear victims.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(resource_allocation_proportionality, empirical, 'Whether present-harm resource capture is proportionate to documented need or has become self-perpetuating institutional capture.').

omega_variable(
    kernel_framing_indeterminacy,
    'Is ''alignment'' genuinely one contested concept with three competing readings, or are ''preventing present bias/harm'' and ''preventing catastrophic loss of control'' actually two structurally distinct problems that have been forced into a single semantic container by funding and institutional convenience?',
    'Track whether the two problem classes share causal mechanisms (e.g., interpretability research serving both bias auditing and control verification) versus requiring wholly disjoint technical and institutional approaches; convergent methods would support ''one kernel, competing readings,'' divergent methods would support ''two constraints wrongly sharing a label.''',
    'If the mechanisms are genuinely disjoint, this entire kernel-family (all three readings) may itself be a false unification analogous to the BGS labeling problem, and each reading should be evaluated as a fully independent constraint rather than as siblings of one kernel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_indeterminacy, conceptual, 'Whether the ai_alignment_commitment kernel is a genuine shared commitment under contest or a label conflating structurally separate problems.').

omega_variable(
    beneficiary_status_of_named_populations,
    'Are marginalized_communities_subject_to_biased_systems correctly modeled as both beneficiary and payer, or does the ''beneficiary'' framing itself serve primarily to legitimate the institutions that invoke them without producing commensurate material improvement in their outcomes?',
    'Longitudinal outcome tracking for the specific populations named in bias audits (loan approval rates, hiring outcomes, moderation appeal success) pre- and post- adoption of this reading''s mitigation frameworks, compared against institutional funding/prestige growth over the same period.',
    'If material outcomes for named communities lag institutional growth, the beneficiary designation is substantially rhetorical and the arrangement is more extractive (closer to snare) than the tangled_rope classification indicates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_status_of_named_populations, empirical, 'Whether the reading''s named beneficiary population receives commensurate material benefit or primarily legitimating function.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_commitment__ethics_justice_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_a_tr_t0, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ai_a_tr_t4, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 4, 0.24).
narrative_ontology:measurement(ai_a_tr_t8, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 8, 0.29).
narrative_ontology:measurement(ai_a_tr_t12, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 12, 0.33).
narrative_ontology:measurement(ai_a_tr_t16, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 16, 0.36).
narrative_ontology:measurement(ai_a_tr_t20, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(ai_a_tr_t24, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 24, 0.4).

% Extraction over time
narrative_ontology:measurement(ai_a_be_t0, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(ai_a_be_t4, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 4, 0.4).
narrative_ontology:measurement(ai_a_be_t8, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 8, 0.46).
narrative_ontology:measurement(ai_a_be_t12, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 12, 0.5).
narrative_ontology:measurement(ai_a_be_t16, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 16, 0.53).
narrative_ontology:measurement(ai_a_be_t20, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 20, 0.56).
narrative_ontology:measurement(ai_a_be_t24, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 24, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(ai_a_su_t0, ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(ai_a_su_t4, ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 4, 0.33).
narrative_ontology:measurement(ai_a_su_t8, ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 8, 0.36).
narrative_ontology:measurement(ai_a_su_t12, ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 12, 0.38).
narrative_ontology:measurement(ai_a_su_t16, ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 16, 0.4).
narrative_ontology:measurement(ai_a_su_t20, ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 20, 0.41).
narrative_ontology:measurement(ai_a_su_t24, ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 24, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_commitment__ethics_justice_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(ai_alignment_commitment__ethics_justice_reading, 0.08).
narrative_ontology:affects_constraint(ai_alignment_commitment__ethics_justice_reading, ai_alignment_commitment__safety_control_reading).
narrative_ontology:affects_constraint(ai_alignment_commitment__ethics_justice_reading, ai_alignment_commitment__integrated_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the ai_alignment_commitment kernel. The safety_control_reading authors a disjoint ε and victim set centered on catastrophic-risk populations and deprioritized control researchers; the integrated_reading authors a claim that both problem classes should be jointly resourced (a coexists_with resolution attempt) rather than competing for the same label. Each reading's ε is authored independently per the ε-invariance principle; the three do not average into one 'alignment' constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
