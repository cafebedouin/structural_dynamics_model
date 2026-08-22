% ============================================================================
% CONSTRAINT STORY: licensing_statute_mandate__public_safety_coordination
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_licensing_statute_mandate__public_safety_coordination, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: licensing_statute_mandate__public_safety_coordination
 *   human_readable: Statutory Credential Mandate for Public Safety Coordination
 *   domain: labor_economics/regulatory_policy
 *
 * SUMMARY:
 *   Statutory credential requirements (licenses) mandate that practitioners
 *   meet minimum training, experience, and examination standards before
 *   lawful practice. This reading treats the constraint as genuine
 *   coordination around consumer safety: incompetent or fraudulent
 *   practitioners are screened out, consumers receive a reliable quality
 *   signal, and the profession maintains reputation. The constraint is
 *   CLAIMED as rope (real coordination function, consumer benefit, shared
 *   threshold) while the authored metrics show moderate extractiveness and
 *   suppression rising over time—indicating that the coordination function
 *   coexists with rent-seeking behavior. This reading instantiates the
 *   public-safety-centered framing of a contested kernel; sibling readings
 *   ('rent_seeking_suppression', 'graduated_access_filter') frame the same
 *   statutory mechanism as labor restriction and class gatekeeping
 *   respectively. The measurement series show extractiveness stabilizing by
 *   interval end, suggesting the constraint reaches a steady state where the
 *   coordination benefit no longer expands but the protection remains.
 *
 * KEY AGENTS:
 *   - Consumer base: primary beneficiary; receives quality assurance and reduced assessment burden.
 *   - Established practitioners: agenda-setter and secondary beneficiary; sets standards, benefits from reduced market competition and reputational protection.
 *   - Credential-ineligible practitioners: primary payer; trapped, cannot practice lawfully regardless of competence.
 *   - Licensing board: administers and enforces; typically dominated by incumbent practitioners.
 *   - Regulatory authority: observer; measures fit-to-purpose and considers alternative safeguards.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(licensing_statute_mandate__public_safety_coordination, 0.42).
domain_priors:suppression_score(licensing_statute_mandate__public_safety_coordination, 0.38).
domain_priors:theater_ratio(licensing_statute_mandate__public_safety_coordination, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__public_safety_coordination, extractiveness, 0.42).
narrative_ontology:constraint_metric(licensing_statute_mandate__public_safety_coordination, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(licensing_statute_mandate__public_safety_coordination, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__public_safety_coordination, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(licensing_statute_mandate__public_safety_coordination, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(licensing_statute_mandate__public_safety_coordination, rope).
narrative_ontology:human_readable(licensing_statute_mandate__public_safety_coordination, "Statutory Credential Mandate for Public Safety Coordination").
narrative_ontology:topic_domain(licensing_statute_mandate__public_safety_coordination, "labor_economics/regulatory_policy").

domain_priors:requires_active_enforcement(licensing_statute_mandate__public_safety_coordination).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(licensing_statute_mandate__public_safety_coordination, 'd7393a50-bac5-40ff-979c-1e71c48297ed').
narrative_ontology:cs_kernel_codification('d7393a50-bac5-40ff-979c-1e71c48297ed', formalized).
narrative_ontology:cs_authority_grounding('d7393a50-bac5-40ff-979c-1e71c48297ed', lineage).
narrative_ontology:cs_interpretation_layer_present('d7393a50-bac5-40ff-979c-1e71c48297ed').
narrative_ontology:cs_reading_relation('d7393a50-bac5-40ff-979c-1e71c48297ed', licensing_statute_mandate__rent_seeking_suppression, coexists_with).
narrative_ontology:cs_reading_relation('d7393a50-bac5-40ff-979c-1e71c48297ed', licensing_statute_mandate__graduated_access_filter, influences).
narrative_ontology:cs_axiom('d7393a50-bac5-40ff-979c-1e71c48297ed', foundational, consumer_protection_through_minimum_competence).
narrative_ontology:cs_axiom_status(consumer_protection_through_minimum_competence, holdable).
narrative_ontology:cs_axiom_grounding('d7393a50-bac5-40ff-979c-1e71c48297ed', consumer_protection_through_minimum_competence, empirically_contingent).
narrative_ontology:cs_axiom('d7393a50-bac5-40ff-979c-1e71c48297ed', foundational, statutory_credential_is_necessary_safeguard).
narrative_ontology:cs_axiom_status(statutory_credential_is_necessary_safeguard, holdable).
narrative_ontology:cs_axiom_grounding('d7393a50-bac5-40ff-979c-1e71c48297ed', statutory_credential_is_necessary_safeguard, empirically_contingent).
narrative_ontology:cs_reference_frame('d7393a50-bac5-40ff-979c-1e71c48297ed', consumer_harm_prevention_via_competence_threshold).
narrative_ontology:cs_drift_state('d7393a50-bac5-40ff-979c-1e71c48297ed', contemporary_alternative_safeguard_availability, gap(authority_erosion, minor, false)).
narrative_ontology:cs_created_at('d7393a50-bac5-40ff-979c-1e71c48297ed', '').
narrative_ontology:cs_kernel_id(licensing_statute_mandate__public_safety_coordination, licensing_statute_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__public_safety_coordination, consumer_base).
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__public_safety_coordination, established_practitioners).
narrative_ontology:constraint_victim(licensing_statute_mandate__public_safety_coordination, credential_ineligible_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(licensing_statute_mandate__public_safety_coordination, incompetent_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receives services from vetted, minimum-competence practitioners. Can rely on credential as a quality floor and does not need to individually assess each provider's training or background. Implicit coordination: all consumers know the credential signals minimum safety threshold; all practitioners must meet it. Exit would mean engaging unvetted providers or foregoing services entirely.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__public_safety_coordination, consumer_base, beneficiary,
    organized, biographical, constrained, national).

% Already hold credentials and set the licensing standard through professional boards and legislative advocacy. Benefit from reduced competition as the credential becomes the market gatekeep and from reduced reputational damage when incompetent practitioners operate outside the statutory framework. Shape the requirement's stringency and enforcement.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__public_safety_coordination, established_practitioners, agenda_setter,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(licensing_statute_mandate__public_safety_coordination, established_practitioners, beneficiary).

% Cannot practice lawfully without meeting the credential requirement. This includes mid-career entrants without prior certification paths, practitioners trained outside formal credential pathways, and those unable to afford credential acquisition costs (exams, training programs, fees). They bear the cost of exclusion: lost market access, inability to generate income in the field, retraining burden.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__public_safety_coordination, credential_ineligible_practitioners, payer,
    powerless, immediate, trapped, national).

% Are screened out and prevented from practicing, regardless of consumer willingness to accept risk or knowledge of their status. The constraint prevents their market access as an intended protective mechanism. Structurally they have no recourse: they are excluded not as a side effect but as the core enforcement mechanism.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__public_safety_coordination, incompetent_practitioners, payer,
    powerless, immediate, trapped, national).

% Administers the credential requirement, sets examination standards, reviews applications, and maintains the register. Derives authority from statute and exercises discretion in enforcement. Typically dominated by incumbent practitioners.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__public_safety_coordination, professional_licensing_board, agenda_setter,
    institutional, generational, analytical, national).

% Not an agent but the outcome the constraint prevents: clients harmed by substandard service from uncredentialed or incompetent practitioners. Named for narrative completeness—the absent voice here is not a party but a potential harm that justifies the constraint's existence. If a consumer is injured, they have remedies through tort or regulatory complaint but cannot participate in the licensing decision ex ante.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__public_safety_coordination, consumer_injury_from_incompetence, excluded,
    powerless, immediate, trapped, national).
narrative_ontology:stakeholder_non_agent(licensing_statute_mandate__public_safety_coordination, consumer_injury_from_incompetence).

% Oversees credential standards, conducts cost-benefit review, investigates whether the requirement is fit-for-purpose, and can recommend statutory revision or alternative safeguards (e.g., disclosure requirements, tiered credentials, competency-based rather than time-based thresholds).
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__public_safety_coordination, regulatory_authority, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(licensing_statute_mandate__public_safety_coordination, established_practitioners).
narrative_ontology:fixing_cost_class(licensing_statute_mandate__public_safety_coordination, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a shared quality assurance problem: consumers need a reliable signal of practitioner competence without individually auditing credentials, and practitioners need a shared standard that all must meet so quality reputations are not destroyed by incompetent fringe actors. The statute codifies the threshold, centralizing the gate-keeping rather than forcing each consumer to duplicate the assessment.
% TRANSFER_FUNCTION: Moves market access and income from practitioners unable or unwilling to meet the credential requirement to those who hold it. Also moves compliance costs (exam fees, training time, ongoing education) from consumers to practitioners; consumers gain time savings and reduced assessment burden.
% ABSENT_VOICES: Mid-career entrants without traditional training pathways, practitioners trained by apprenticeship or other non-credentialed methods, low-income populations unable to afford credential acquisition, and jurisdictions or contexts where the credential is culturally foreign or economically infeasible. These actors would object to the requirement's stringency or cost if heard, but are structurally excluded from the licensing board composition and legislative process.
% DISAPPEARANCE_RATIONALE: If the statutory credential requirement vanished overnight, practitioners previously excluded would immediately enter the market. Consumer injury rates would likely rise short-term until reputation and tort mechanisms re-calibrated; alternative quality signals (certifications, reviews, bonding) would emerge. Established practitioners' market share and pricing power would compress. The market would reorganize around private certification or reputation rather than statutory gatekeeping.
% FOUNDING_PROBLEM: Early iterations of this profession had no standardized training, no verification of competence, and consumers experienced significant harm from incompetent or fraudulent practitioners. Public health or financial safety required a minimum standard.
% FOUNDING_PROBLEM_CORROBORATION: Regulatory authorities and consumer protection advocates attest the founding problem was real and credentials have improved average competence. Incumbent practitioners attest the problem persists and credentials remain necessary. Independent researchers dispute both: some data suggests harm rates have plateaued or that alternative screening (reputation, insurance, disclosure) now captures most of the protective benefit, making the credential's marginal contribution small. No single corroborating source outside the benefiting parties has reached consensus.
narrative_ontology:disappearance_verdict(licensing_statute_mandate__public_safety_coordination, world_rearranges).
narrative_ontology:founding_problem_status(licensing_statute_mandate__public_safety_coordination, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(licensing_statute_mandate__public_safety_coordination, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(licensing_statute_mandate__public_safety_coordination, 'none', 1).
narrative_ontology:epsilon_provenance(licensing_statute_mandate__public_safety_coordination, 0.42, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(licensing_statute_mandate__public_safety_coordination_tests).
:- end_tests(licensing_statute_mandate__public_safety_coordination_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42 at endpoint) because the coordination function is real—consumers genuinely benefit from the quality floor and practitioners genuinely benefit from reduced market chaos—but incumbent practitioners also use the requirement to suppress competition and maintain pricing power. The measurement trajectory shows extractiveness rising from 0.28 to 0.42 over 40 time units, then plateauing: the initial rise reflects credential-eligibility gaps widening (more practitioners want entry but cannot afford or obtain credentials) and incumbent gatekeeping strengthening. The plateau suggests a new equilibrium where the requirement is neither strengthening nor weakening. Theater is low (0.22): the licensing board genuinely reviews and maintains standards; however, as time progresses, more of the board's activity is defensive (preventing low-cost alternatives like competency testing without formal training) rather than protective (eliminating demonstrably incompetent practitioners). Suppression (0.38 at endpoint) captures the ongoing exclusion of credential-ineligible practitioners—it is active and intentional but not violent or coercive in the ordinary sense.
 *
 * PERSPECTIVAL GAP:
 *   From the consumer and licensing-board seats, the constraint is a functional coordination mechanism that prevents real harm. From the credential-ineligible practitioner's seat, it is an arbitrary barrier to market entry. From the established practitioner's seat, it is both: genuine coordination plus valuable market protection. The engine should compute these seats as divergent types—consumers and established practitioners perceiving rope; ineligible practitioners perceiving snare or scaffold-turned-permanent. This divergence is the analytical point: the constraint's structure enables simultaneous beneficial coordination AND extractive labor suppression.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary extraction formula: beneficiaries have low d because they receive the coordination benefit (quality assurance, market transparency) without bearing market-access costs. Established practitioners have medium d because they set the standard (high power) but benefit from it (beneficiary role), creating an intentional-bias effect—their d should reflect both their power to set terms AND their structural gain from the arrangement. Credential-ineligible practitioners have d near 1.0 because they cannot access the market AND cannot modify the constraint; their exit options are trapped. The directionality chain: trapped exit → high d; powerless power → high d; payer role → high d. By all three paths, ineligible practitioners sit at the full-target end.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (consumer harm from incompetence) is CONTESTED as to whether it remains live or has been substantially solved by market alternatives. If the founding problem is dead (alternative screening mechanisms now capture the protective benefit), and the constraint persists, this signals possible mandatrophy—the coordination justification has decayed but the extraction persists, transforming rope into piton or snare. However, evidence is insufficient to declare mandatrophy resolved: licensing boards do remove incompetent practitioners in real cases, and tort/regulatory remedies remain available for injury. The theater ratio (0.22) is consistent with some performative maintenance but not with full piton-level performance. Without stronger evidence that alternatives are superior, mandatrophy is 'contested', not 'resolved'.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_live_or_dead,
    'Has the founding problem (consumer harm from incompetent practitioners) been substantially solved by market mechanisms, regulatory alternatives, or tort remedies, such that the credential requirement''s marginal protective benefit is now small?',
    'Empirical comparison: measure consumer injury rates in jurisdictions with and without statutory credentials; measure the protective contribution of the credential net of reputation mechanisms, insurance requirements, and disclosure rules.',
    'If the founding problem is dead, the constraint transitions from rope to piton or snare—the coordination justification dissolves and the extraction (rent-seeking, labor suppression) becomes visible. If live, the rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_live_or_dead, empirical, 'Whether consumer harm from incompetence remains a live coordination problem or has been solved by alternatives.').

omega_variable(
    credential_stringency_optimal,
    'Is the current credential requirement stringent precisely enough to screen out demonstrably incompetent practitioners, or is it overshooting, screening out practitioners who would serve consumers adequately while imposing excess cost?',
    'Competency-based validation: compare outcome rates (injury, complaint, satisfaction) for practitioners just-barely passing the current credential against practitioners just-barely failing; adjust the threshold if no outcome difference appears.',
    'If overshooting, the suppression (0.38) is partly unnecessary; the constraint could maintain the coordination benefit with lower extractiveness. If appropriate, suppression tracks genuine risk and is justified.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(credential_stringency_optimal, empirical, 'Whether the credential requirement screens optimally or overscreens incompetence.').

omega_variable(
    alternative_safeguard_sufficiency,
    'Would a tiered-credential system (basic, advanced, specialty) or a competency-based test (independent of formal training) provide sufficient consumer protection while lowering the barrier to market entry for mid-career practitioners?',
    'Pilot jurisdiction natural experiments: implement graduated credentials and measure whether injury rates, consumer satisfaction, and practitioner diversity change meaningfully.',
    'If alternatives are sufficient, the current requirement''s extractiveness is unnecessary; the constraint could be redesigned. If not, the current requirement''s stringency is justified.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_safeguard_sufficiency, conceptual, 'Whether the statutory credential requirement is the minimally-extractive way to achieve consumer protection.').

omega_variable(
    reading_foreclosure_test,
    'Does the public_safety_coordination reading logically foreclose the rent_seeking_suppression reading, or can both framings coexist in the same institution?',
    'Structural analysis: if the credential MUST reduce labor supply to protect consumers (i.e., market entry must be constrained for safety), then suppression is necessary and both readings collapse into one. If the credential could protect consumers WITHOUT reducing labor supply (e.g., tiered credentials, competency tests), then the rent-seeking reading is a contingent choice, not forced by the safety logic.',
    'If foreclosure holds, the readings are contradictory and cannot both describe the same constraint. If coexistence holds, both readings are present in the same statutory structure; the contested kernel is genuinely underdetermined by the mechanism itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_test, conceptual, 'Whether the safety-coordination reading logically entails or merely enables the rent-seeking reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(licensing_statute_mandate__public_safety_coordination, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lice_tr_t0, licensing_statute_mandate__public_safety_coordination, theater_ratio, 0, 0.08).
narrative_ontology:measurement(lice_tr_t5, licensing_statute_mandate__public_safety_coordination, theater_ratio, 5, 0.11).
narrative_ontology:measurement(lice_tr_t10, licensing_statute_mandate__public_safety_coordination, theater_ratio, 10, 0.14).
narrative_ontology:measurement(lice_tr_t15, licensing_statute_mandate__public_safety_coordination, theater_ratio, 15, 0.17).
narrative_ontology:measurement(lice_tr_t25, licensing_statute_mandate__public_safety_coordination, theater_ratio, 25, 0.21).
narrative_ontology:measurement(lice_tr_t35, licensing_statute_mandate__public_safety_coordination, theater_ratio, 35, 0.22).
narrative_ontology:measurement(lice_tr_t40, licensing_statute_mandate__public_safety_coordination, theater_ratio, 40, 0.22).

% Extraction over time
narrative_ontology:measurement(lice_be_t0, licensing_statute_mandate__public_safety_coordination, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(lice_be_t5, licensing_statute_mandate__public_safety_coordination, base_extractiveness, 5, 0.32).
narrative_ontology:measurement(lice_be_t10, licensing_statute_mandate__public_safety_coordination, base_extractiveness, 10, 0.36).
narrative_ontology:measurement(lice_be_t15, licensing_statute_mandate__public_safety_coordination, base_extractiveness, 15, 0.39).
narrative_ontology:measurement(lice_be_t25, licensing_statute_mandate__public_safety_coordination, base_extractiveness, 25, 0.41).
narrative_ontology:measurement(lice_be_t35, licensing_statute_mandate__public_safety_coordination, base_extractiveness, 35, 0.42).
narrative_ontology:measurement(lice_be_t40, licensing_statute_mandate__public_safety_coordination, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(lice_su_t0, licensing_statute_mandate__public_safety_coordination, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(lice_su_t5, licensing_statute_mandate__public_safety_coordination, suppression_requirement, 5, 0.28).
narrative_ontology:measurement(lice_su_t10, licensing_statute_mandate__public_safety_coordination, suppression_requirement, 10, 0.31).
narrative_ontology:measurement(lice_su_t15, licensing_statute_mandate__public_safety_coordination, suppression_requirement, 15, 0.34).
narrative_ontology:measurement(lice_su_t25, licensing_statute_mandate__public_safety_coordination, suppression_requirement, 25, 0.37).
narrative_ontology:measurement(lice_su_t35, licensing_statute_mandate__public_safety_coordination, suppression_requirement, 35, 0.38).
narrative_ontology:measurement(lice_su_t40, licensing_statute_mandate__public_safety_coordination, suppression_requirement, 40, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(licensing_statute_mandate__public_safety_coordination, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(licensing_statute_mandate__public_safety_coordination, 0.12).
narrative_ontology:affects_constraint(licensing_statute_mandate__public_safety_coordination, licensing_statute_mandate__rent_seeking_suppression).
narrative_ontology:affects_constraint(licensing_statute_mandate__public_safety_coordination, licensing_statute_mandate__graduated_access_filter).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the contested kernel 'licensing_statute_mandate'. The three stories represent different interpretations of the same statutory mechanism: public_safety_coordination frames the statute as genuine consumer protection; rent_seeking_suppression frames it as labor restriction masquerading as safety; graduated_access_filter frames it as class-sorting gatekeeping. Each story has its own ε, beneficiary/victim structure, and classified type. Sibling stories are linked via network.affects_constraints. The constraint family models how a single kernel (a law, a regulatory practice, a professional norm) can instantiate structurally different constraints depending on which reading is adopted.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(licensing_statute_mandate__public_safety_coordination, organized, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
