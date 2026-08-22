% ============================================================================
% CONSTRAINT STORY: legitimate_health_intervention__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimate_health_intervention__bodily_autonomy_primary, []).

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
 *   constraint_id: legitimate_health_intervention__bodily_autonomy_primary
 *   human_readable: Bodily Autonomy Primary: Medical Intervention Legitimacy via Consent
 *   domain: public_health/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   This constraint instantiates the bodily-autonomy-primary reading of the
 *   contested kernel 'legitimate health intervention.' Under this reading,
 *   the legitimacy of state-mandated medical intervention rests entirely on
 *   informed consent; coercion via legal penalty, employment contingency, or
 *   access denial violates bodily integrity regardless of the measurable
 *   public health benefit. The state, in enforcing a mandate against
 *   individual refusal, transforms from coordinator into extractor — it
 *   collects compliance authority by force, paying for it with the bodily
 *   autonomy of those who refuse. The constraint operates as a snare: the
 *   coordination problem (achieving population immunity) is real, but the
 *   resolution mechanism (state mandate against bodily autonomy) persists via
 *   suppression (legal penalties, employment termination, service denial)
 *   rather than via participant preference. This reading coexists with two
 *   siblings: public_health_primary (legitimacy derives from population-level
 *   morbidity/mortality reduction, individual refusal is externality
 *   imposition) and proportionality_reading (legitimacy requires weighing
 *   both autonomy and public benefit by disease severity). The reading is NOT
 *   a natural law — it is a contestable normative claim about what makes
 *   state authority legitimate. The claim/metric gap is intentional: the
 *   constraint is CLAIMED as a snare (which this reading asserts is true)
 *   while the authored metrics describe extraction that accumulates and
 *   plateaus (t=24–72), consistent with mandate enforcement that hardens
 *   mid-crisis and then sustains without relaxation.
 *
 * KEY AGENTS:
 *   - mandate_coerced_individuals: primary targets, powerless, identity-locked exit
 *   - employment_gatekept_workers: institutional lever point, moderate power but constrained exit
 *   - public_health_authority: agenda_setter, collects enforcement authority
 *   - third_party_beneficiaries: incidental beneficiaries from herd immunity, organize to defend mandate
 *   - civil_liberties_advocates: observers with institutional power to challenge via litigation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_health_intervention__bodily_autonomy_primary, 0.68).
domain_priors:suppression_score(legitimate_health_intervention__bodily_autonomy_primary, 0.76).
domain_priors:theater_ratio(legitimate_health_intervention__bodily_autonomy_primary, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, extractiveness, 0.68).
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_health_intervention__bodily_autonomy_primary, snare).
narrative_ontology:human_readable(legitimate_health_intervention__bodily_autonomy_primary, "Bodily Autonomy Primary: Medical Intervention Legitimacy via Consent").
narrative_ontology:topic_domain(legitimate_health_intervention__bodily_autonomy_primary, "public_health/medical_ethics/constitutional_law").

domain_priors:requires_active_enforcement(legitimate_health_intervention__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_health_intervention__bodily_autonomy_primary, 'b7815ddd-5eda-4ee6-a4ac-a4efe910f78f').
narrative_ontology:cs_kernel_codification('b7815ddd-5eda-4ee6-a4ac-a4efe910f78f', fixed_text).
narrative_ontology:cs_authority_grounding('b7815ddd-5eda-4ee6-a4ac-a4efe910f78f', lineage).
narrative_ontology:cs_interpretation_layer_present('b7815ddd-5eda-4ee6-a4ac-a4efe910f78f').
narrative_ontology:cs_reading_relation('b7815ddd-5eda-4ee6-a4ac-a4efe910f78f', legitimate_health_intervention__public_health_primary, coexists_with).
narrative_ontology:cs_reading_relation('b7815ddd-5eda-4ee6-a4ac-a4efe910f78f', legitimate_health_intervention__proportionality_reading, influences).
narrative_ontology:cs_axiom('b7815ddd-5eda-4ee6-a4ac-a4efe910f78f', foundational, bodily_integrity_fundamental_right).
narrative_ontology:cs_axiom_status(bodily_integrity_fundamental_right, holdable).
narrative_ontology:cs_axiom_grounding('b7815ddd-5eda-4ee6-a4ac-a4efe910f78f', bodily_integrity_fundamental_right, deontological).
narrative_ontology:cs_axiom('b7815ddd-5eda-4ee6-a4ac-a4efe910f78f', foundational, consent_requirement_for_bodily_intervention).
narrative_ontology:cs_axiom_status(consent_requirement_for_bodily_intervention, holdable).
narrative_ontology:cs_axiom_grounding('b7815ddd-5eda-4ee6-a4ac-a4efe910f78f', consent_requirement_for_bodily_intervention, deontological).
narrative_ontology:cs_axiom('b7815ddd-5eda-4ee6-a4ac-a4efe910f78f', secondary, public_benefit_does_not_justify_bodily_coercion).
narrative_ontology:cs_axiom_status(public_benefit_does_not_justify_bodily_coercion, holdable).
narrative_ontology:cs_axiom_grounding('b7815ddd-5eda-4ee6-a4ac-a4efe910f78f', public_benefit_does_not_justify_bodily_coercion, deontological).
narrative_ontology:cs_reference_frame('b7815ddd-5eda-4ee6-a4ac-a4efe910f78f', autonomous_medical_decision_making).
narrative_ontology:cs_drift_state('b7815ddd-5eda-4ee6-a4ac-a4efe910f78f', contemporary_pandemic_mandate_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b7815ddd-5eda-4ee6-a4ac-a4efe910f78f', '').
narrative_ontology:cs_kernel_id(legitimate_health_intervention__bodily_autonomy_primary, legitimate_health_intervention).

% --- Structural relationships ---
narrative_ontology:constraint_victim(legitimate_health_intervention__bodily_autonomy_primary, mandate_coerced_individuals).
narrative_ontology:constraint_victim(legitimate_health_intervention__bodily_autonomy_primary, employment_gatekept_workers).
narrative_ontology:constraint_victim(legitimate_health_intervention__bodily_autonomy_primary, access_denied_patients).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__bodily_autonomy_primary, employment_gatekept_workers).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__bodily_autonomy_primary, third_party_beneficiaries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Face legal requirement to undergo medical intervention (vaccination, treatment, reporting status). Refusal results in employment termination, loss of public service access, or criminal penalty. Their exit options are: comply, migrate to jurisdiction without mandate, or litigate. The identity-lock arises from bodily autonomy being constitutive of self-determination — accepting the intervention feels like surrendering moral agency, not merely accepting a regulation. They bear the biological risk and the bodily integrity violation directly.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, mandate_coerced_individuals, payer,
    powerless, biographical, identity_locked, national).

% In occupations where the mandate is enforced as a condition of employment (healthcare workers, military, public sector). They face a choice between career continuity and refusal. Some benefit from reduced disease risk in their workplace; others bear the mandate despite assessing their own risk as low. The constrained exit arises because changing careers or leaving the workforce is high-cost for mid-career workers with family obligations.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, employment_gatekept_workers, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(legitimate_health_intervention__bodily_autonomy_primary, employment_gatekept_workers, beneficiary).

% Individuals denied healthcare access, school enrollment, or essential services because they refuse the mandated intervention. They cannot exit — the services are essential and they have no jurisdiction where the mandate does not apply. They bear both the bodily integrity violation and the deprivation of the access they sought.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, access_denied_patients, payer,
    powerless, immediate, trapped, local).

% Sets the mandate, determines enforcement mechanisms, and adjudicates exemptions. Justifies the mandate as necessary for population-level disease control. Bears no direct cost to compliance (does not undergo the intervention). Can shift enforcement severity, add exceptions, or repeal the mandate. Benefits from reduced disease burden measurable in population statistics.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, public_health_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Individuals who benefit from herd immunity or reduced healthcare burden without being subject to the mandate (children, elderly, immunocompromised individuals in the population). They receive disease risk reduction at no direct bodily cost. From their position, the mandate on others is a coordination mechanism that benefits them.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, third_party_beneficiaries, beneficiary,
    organized, generational, arbitrage, national).

% Would offer alternative treatments or risk-acceptance frameworks if permitted. They are structurally barred from providing services that would constitute refusal of the mandate. Their exclusion is maintained by penalties for unauthorized practice and by the mandate's foreclosure of legitimate alternatives.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, alternative_medicine_practitioners, excluded,
    moderate, biographical, trapped, regional).

% Monitor the constraint and challenge it through litigation, legislative advocacy, and public discourse. They hold no direct stake in the medical outcome but stake their institutional mission on defending bodily autonomy doctrine. They can exit by choosing other advocacy targets; the power to litigate and influence opinion gives them institutional leverage.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, civil_liberties_advocates, observer,
    powerful, generational, arbitrage, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimate_health_intervention__bodily_autonomy_primary, public_health_authority).
narrative_ontology:fixing_cost_class(legitimate_health_intervention__bodily_autonomy_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of achieving sufficient population immunity or disease reduction to prevent healthcare system collapse and reduce disease transmission below epidemic thresholds. In principle, voluntary coordination would suffice; the mandate exists because voluntary uptake fell below the threshold deemed necessary.
% TRANSFER_FUNCTION: Transfers bodily autonomy and control over medical decision-making from individuals to state authorities. The direction of flow: individuals transfer their right to refuse; the state receives enforcement authority and collects compliance. This reading frames the transfer as a one-way extraction (individuals lose; the state gains decision authority), not as mutual agreement to coordinate.
% ABSENT_VOICES: Individuals who assess their own risk as low and would prefer to refuse have been excluded from the decision-making process. Alternative medicine practitioners and jurisdictions with different mandate policies are excluded by the scope and enforcement reach of this constraint. Communities with religious or philosophical objections to medical mandates are structurally barred from influence.
% DISAPPEARANCE_RATIONALE: If the mandate and its enforcement disappeared, individuals would reassert control over medical decisions. Some would choose the same intervention voluntarily; others would refuse. Healthcare systems would face demand fluctuations and potential disease resurgence in unvaccinated populations. The constraint's removal would immediately restore the baseline arrangement it replaced: a world where medical decisions rest with individuals, not state enforcement.
% FOUNDING_PROBLEM: During the emergency phase of pandemic or epidemic, population-level immunity fell short of herd immunity thresholds due to low voluntary uptake. Voluntary coordination was insufficient to achieve epidemiologically necessary coverage rates fast enough to prevent healthcare system strain and excess mortality.
% FOUNDING_PROBLEM_CORROBORATION: Public health authorities attest the founding problem was live during acute emergency phases and remains live during resurgent disease variants. Medical researchers corroborate the epidemiological necessity of high coverage rates. Civil liberties advocates and mandate-coerced individuals contest whether the emergency ever rose to the threshold that would justify bodily-autonomy violation, and dispute that alternative coordination mechanisms (rapid treatment, targeted protection, voluntary campaigns) were exhausted before resorting to coercion. Legislative investigations and longitudinal studies document the founding problem's status shifting from acute emergency (live, 2020–2021) to endemic management (contested, 2022–present).
narrative_ontology:disappearance_verdict(legitimate_health_intervention__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_health_intervention__bodily_autonomy_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_health_intervention__bodily_autonomy_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(legitimate_health_intervention__bodily_autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_health_intervention__bodily_autonomy_primary, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_health_intervention__bodily_autonomy_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimate_health_intervention__bodily_autonomy_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimate_health_intervention__bodily_autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.38 (early, mandate presented as temporary emergency measure) to 0.68 (steady-state, enforcement sustains without declared end date or restoration of autonomy). The plateau at t=36+ indicates the constraint has hardened into a maintained extraction rather than remaining emergency-temporary. Suppression is high (0.76 at plateau) because compliance is sustained by job loss, healthcare access denial, and criminal penalties, not by voluntary agreement. Theater ratio rises from 0.22 to 0.44, indicating an increase in performative activity — public health messaging, scientific justification, and exceptions (hardship waivers, religious exemptions) that perform legitimacy without materially changing enforcement. Accessibility collapse is high (0.71) because once the mandate is understood, alternatives are nearly unavailable: refuse-and-lose-employment, refuse-and-lose-care, or migrate are the only exit paths. Resistance is moderate (0.58): some individuals and communities actively refuse and litigate; others comply under duress. The theater-ratio rise suggests authorities are investing in narrative justification as direct compliance enforcement faces resistance.
 *
 * PERSPECTIVAL GAP:
 *   From the public health authority's seat: the constraint is coordination mechanism (herd immunity is genuinely necessary, voluntary uptake is insufficient, mandate achieves coverage). From the mandate-coerced individual's seat: the constraint is bodily autonomy violation (the state has extracted the right to refuse, enforces compliance via force, and calls this public health when it is coercion). From the third-party beneficiary's seat: the constraint is protective (they receive disease risk reduction without bearing the mandate). The engine computes each seat's type from the structural data: high directionality-d for the coerced (d~0.9, high target position), low d for the authority (d~0.1, agenda-setter position), intermediate d for beneficiaries (d~0.4, benefit without direct cost). This divergence is the measurement the system exists to take — the disagreement is structural, not a classification error.
 *
 * DIRECTIONALITY LOGIC:
 *   Mandate-coerced individuals: they are primary targets (role=payer, power=powerless, exit=identity_locked). Their d-value is high (~0.9) because they bear the bodily intervention directly, their exit paths are blocked (cannot refuse without severe penalty), and their identity as self-determining agents is constitutively threatened. Employment-gatekept workers: d-value is intermediate-high (~0.75) because they face a constrained choice (lose career or comply) and bear the intervention, but they have more leverage (professional licensing, union representation, relocation options) than powerless individuals. Public health authority: d-value is low (~0.1) because they set the agenda, bear no direct cost, can shift enforcement, and benefit from reduced disease burden (they collect the extraction). Third-party beneficiaries: d-value is moderate (~0.3) because they benefit from herd immunity without bearing the mandate directly. This distribution is asymmetric: many targets (all mandate-coerced individuals) at high d, few beneficiaries (the authority, some beneficiaries) at low d. The constraint's persistence depends on this asymmetry — the benefits concentrate in the authority and organized beneficiaries; the costs disperse across powerless and constrained individuals.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (pandemic/epidemic emergency, voluntary uptake insufficient) had a clear temporal bound: emergency phase ended by t=24–36. The mandate, however, persists unchanged through t=72 with no declared sunset and no restoration of autonomy. The founding_problem_status is 'contested' because public health authorities claim the problem remains live (disease resurgence, new variants) while mandate opponents argue the founding emergency is resolved and the mandate has become entrenched rent-seeking on state authority. The disappearance_verdict is 'world_rearranges' because the constraint is not natural law — removing it would immediately restore individual medical decision-making. The theater_ratio rise (0.22→0.44) indicates increasing performative maintenance: authorities invest in justification and exceptions management while direct resistance persists. This is mandatrophy-adjacent but not yet classic piton (a piton would have theater_ratio > 0.6 and no concentrated beneficiary collecting from it; here the authority concentrates extraction benefits). The classification is snare (extraction via coercion, identifiable victims, active enforcement) rather than piton (degraded coordination maintained by inertia). If the constraint persists beyond t=100 with extractiveness plateaued and founding problem status remaining 'contested,' it would meet piton criteria: theater_ratio would rise further, beneficiary power would diffuse, and the constraint would be maintained more by institutional inertia than by any party's active capture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_vs_natural_right,
    'Is bodily autonomy a reading of a contestable legitimacy kernel, or a natural right that exists prior to state authority?',
    'Philosophical analysis of whether the autonomy claim grounds state legitimacy or pre-exists it; historical tracing of when the claim entered legal doctrine vs. when it was asserted as pre-legal.',
    'If a natural right (prior to state), the constraint is not a reading of a kernel but a boundary on legitimate state action itself — classification would shift from kernel-reading to mountain-adjacent. If a reading, the constraint competes with public_health_primary and proportionality_reading within a single contested kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_vs_natural_right, conceptual, 'Whether bodily autonomy is a reading within the kernel or a pre-state natural right.').

omega_variable(
    coercion_mechanism_structural_vs_internalized,
    'Is the suppression measured here structural (legal penalties, employment termination, access denial) or internalized (fear of social sanction, belief that refusal is selfish)?',
    'Post-coercion observational study: do individuals maintain resistance after the structural coercion is removed? Do they report changed beliefs about legitimacy or only changed compliance?',
    'If structural dominates, the constraint''s suppression reflects external force and is accurately measured at 0.76. If internalized dominates, the constraint carries hidden narrative capture — individuals believe their coercion is legitimate — which would raise effective suppression above the structural measure and suggest the reading itself has become identity-locked within the coerced population.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_mechanism_structural_vs_internalized, empirical, 'Suppression mechanism: structural barriers vs. internalized belief.').

omega_variable(
    public_benefit_asymmetry,
    'Does the measured public health benefit flow to the coerced population or primarily to others (herd immunity, externality reduction)?',
    'Epidemiological analysis of direct vs. indirect benefits by demographic; comparison of disease risk faced by coerced individuals vs. benefits they receive.',
    'If benefits flow primarily to others (herd immunity), the constraint operates as externality-forced internalization — a transfer from coerced individuals to the benefit-receiving population, raising extraction reading. If benefits concentrate on the coerced individuals themselves, the constraint''s extractiveness could be reframed as paternalism rather than pure extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(public_benefit_asymmetry, empirical, 'Direction of public health benefit relative to coercion site.').

omega_variable(
    employment_leverage_separability,
    'Is employment-contingent health mandate enforceability separable from the underlying medical intervention requirement?',
    'Regulatory decomposition: mandate medical intervention as a condition of receiving government services (welfare, healthcare access) but decouple from private-sector employment. Measure whether employment mandates persist separately or collapse without government leverage.',
    'If employment mandates are separable, the state''s use of employment as a coercion vector is an independent extraction mechanism layered onto the medical requirement — would raise effective extraction via directionality (employment-gatekept workers sit at higher d-value). If inseparable (employers independently require it for workplace safety), extraction is lower and reflects genuine coordination rather than state leverage.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(employment_leverage_separability, empirical, 'Whether employment leverage is intrinsic to the medical intervention or an independent state enforcement mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_health_intervention__bodily_autonomy_primary, 0, 72).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 0, 0.22).
narrative_ontology:measurement(legi_tr_t12, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 12, 0.28).
narrative_ontology:measurement(legi_tr_t24, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 24, 0.38).
narrative_ontology:measurement(legi_tr_t36, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 36, 0.44).
narrative_ontology:measurement(legi_tr_t48, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 48, 0.42).
narrative_ontology:measurement(legi_tr_t60, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 60, 0.42).
narrative_ontology:measurement(legi_tr_t72, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 72, 0.42).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(legi_be_t12, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 12, 0.52).
narrative_ontology:measurement(legi_be_t24, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 24, 0.64).
narrative_ontology:measurement(legi_be_t36, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 36, 0.68).
narrative_ontology:measurement(legi_be_t48, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 48, 0.67).
narrative_ontology:measurement(legi_be_t60, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 60, 0.68).
narrative_ontology:measurement(legi_be_t72, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 72, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(legi_su_t12, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 12, 0.68).
narrative_ontology:measurement(legi_su_t24, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 24, 0.78).
narrative_ontology:measurement(legi_su_t36, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 36, 0.8).
narrative_ontology:measurement(legi_su_t48, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 48, 0.78).
narrative_ontology:measurement(legi_su_t60, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 60, 0.76).
narrative_ontology:measurement(legi_su_t72, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 72, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_health_intervention__bodily_autonomy_primary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(legitimate_health_intervention__bodily_autonomy_primary, 0.18).
narrative_ontology:affects_constraint(legitimate_health_intervention__bodily_autonomy_primary, legitimate_health_intervention__public_health_primary).
narrative_ontology:affects_constraint(legitimate_health_intervention__bodily_autonomy_primary, legitimate_health_intervention__proportionality_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of the 'legitimate_health_intervention' kernel family. It instantiates the bodily-autonomy-primary reading, which coexists with public_health_primary and proportionality_reading readings. All three readings share the same referent (the standing state mandate for medical intervention during health emergency) but assess legitimacy by different criteria. The three stories are linked by network.affects_constraints to preserve the kernel structure: bodily_autonomy_primary and public_health_primary coexist as competing frameworks; proportionality_reading influences both by proposing a middle-ground framework. ε values differ across readings (reading-indexed assessments of the same standing arrangement) and are not reconcilable; each reading's ε is correct within its own normative framework. This is decomposition via ε-invariance: one natural-language concept ('medical mandate legitimacy'), multiple structurally distinct constraints (three readings with distinct ε values), linked by kernel identity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
