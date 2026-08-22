% ============================================================================
% CONSTRAINT STORY: coercion_legitimacy_boundary__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_coercion_legitimacy_boundary__bodily_autonomy_primary, []).

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
 *   constraint_id: coercion_legitimacy_boundary__bodily_autonomy_primary
 *   human_readable: Bodily Autonomy Primacy Reading of the Coercion-Legitimacy Boundary
 *   domain: Public Health Policy / Medical Ethics / Constitutional Law
 *
 * SUMMARY:
 *   This story instantiates the bodily-autonomy-primary reading of the
 *   coercion-legitimacy kernel: medical intervention without consent is
 *   categorically impermissible regardless of collective benefit. Under this
 *   reading, no aggregation of epidemiological harm can license forced
 *   vaccination or treatment. The reading is coherent and internally
 *   consistent, but it produces its own extraction structure — the
 *   categorical bar externalizes infectious risk onto people who cannot
 *   vaccinate themselves. As non-enforcement of collective protective
 *   measures persists, the population of immunocompromised and
 *   medically-ineligible people who bear elevated exposure grows relative to
 *   any accounting mechanism, which is the moderate and rising ε this story
 *   authors. Sibling readings (public_health_primary,
 *   proportionality_reading) are separate constraints, not modeled here.
 *
 * KEY AGENTS:
 *   - vaccine_refusing_individuals: primary declared beneficiary of the categorical rule (moderate/mobile)
 *   - mandate_enforcement_bodies: institutional agenda-setter that administers and benefits from the bright-line rule
 *   - immunocompromised_individuals: primary bearer of externalized risk (powerless/trapped)
 *   - unvaccinated_dependent_minors: risk-bearers whose autonomy is exercised by proxy
 *   - constitutional_courts: analytical observer adjudicating the boundary across cases
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.42).
domain_priors:suppression_score(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.28).
domain_priors:theater_ratio(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, extractiveness, 0.42).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coercion_legitimacy_boundary__bodily_autonomy_primary, tangled_rope).
narrative_ontology:human_readable(coercion_legitimacy_boundary__bodily_autonomy_primary, "Bodily Autonomy Primacy Reading of the Coercion-Legitimacy Boundary").
narrative_ontology:topic_domain(coercion_legitimacy_boundary__bodily_autonomy_primary, "Public Health Policy / Medical Ethics / Constitutional Law").

domain_priors:requires_active_enforcement(coercion_legitimacy_boundary__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(coercion_legitimacy_boundary__bodily_autonomy_primary, '3c6ad95c-e5eb-4d5f-89e1-552972b6955b').
narrative_ontology:cs_kernel_codification('3c6ad95c-e5eb-4d5f-89e1-552972b6955b', formalized).
narrative_ontology:cs_authority_grounding('3c6ad95c-e5eb-4d5f-89e1-552972b6955b', lineage).
narrative_ontology:cs_interpretation_layer_present('3c6ad95c-e5eb-4d5f-89e1-552972b6955b').
narrative_ontology:cs_reading_relation('3c6ad95c-e5eb-4d5f-89e1-552972b6955b', coercion_legitimacy_boundary__public_health_primary, forecloses).
narrative_ontology:cs_reading_relation('3c6ad95c-e5eb-4d5f-89e1-552972b6955b', coercion_legitimacy_boundary__proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('3c6ad95c-e5eb-4d5f-89e1-552972b6955b', foundational, bodily_integrity_categorically_inviolable).
narrative_ontology:cs_axiom_status(bodily_integrity_categorically_inviolable, holdable).
narrative_ontology:cs_axiom_grounding('3c6ad95c-e5eb-4d5f-89e1-552972b6955b', bodily_integrity_categorically_inviolable, deontological).
narrative_ontology:cs_axiom('3c6ad95c-e5eb-4d5f-89e1-552972b6955b', foundational, collective_benefit_never_licenses_nonconsensual_intervention).
narrative_ontology:cs_axiom_status(collective_benefit_never_licenses_nonconsensual_intervention, holdable).
narrative_ontology:cs_axiom_grounding('3c6ad95c-e5eb-4d5f-89e1-552972b6955b', collective_benefit_never_licenses_nonconsensual_intervention, deontological).
narrative_ontology:cs_reference_frame('3c6ad95c-e5eb-4d5f-89e1-552972b6955b', post_nuremberg_informed_consent_doctrine).
narrative_ontology:cs_drift_state('3c6ad95c-e5eb-4d5f-89e1-552972b6955b', contemporary_routine_vaccination_disputes, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3c6ad95c-e5eb-4d5f-89e1-552972b6955b', '').
narrative_ontology:cs_kernel_id(coercion_legitimacy_boundary__bodily_autonomy_primary, coercion_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__bodily_autonomy_primary, vaccine_refusing_individuals).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__bodily_autonomy_primary, mandate_enforcement_bodies).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__bodily_autonomy_primary, bodily_autonomy_advocacy_groups).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__bodily_autonomy_primary, immunocompromised_individuals).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__bodily_autonomy_primary, unvaccinated_dependent_minors).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__bodily_autonomy_primary, healthcare_workers_treating_outbreaks).
narrative_ontology:constraint_vindicates(coercion_legitimacy_boundary__bodily_autonomy_primary, informed_consent_doctrine).
narrative_ontology:constraint_vindicates(coercion_legitimacy_boundary__bodily_autonomy_primary, substantive_due_process_bodily_integrity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decline a medical intervention on grounds of bodily integrity, religious conviction, or risk assessment, and under this reading cannot be compelled regardless of the aggregate epidemiological benefit their compliance would produce. They retain access to work, school, and public space that a compulsion-tolerant regime would condition on compliance.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, vaccine_refusing_individuals, beneficiary,
    moderate, biographical, mobile, national).

% Public health agencies and courts that administer the legal boundary itself — they benefit under this reading by having a bright-line rule that shields them from having to conduct case-by-case coercion, and from constitutional liability for compelled bodily intrusion. They set the standard by litigating and codifying it.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, mandate_enforcement_bodies, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(coercion_legitimacy_boundary__bodily_autonomy_primary, mandate_enforcement_bodies, beneficiary).

% Litigation and advocacy organizations whose founding purpose is vindicated by this reading; they gain legal precedent, funding, and standing whenever the categorical rule is upheld or extended.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, bodily_autonomy_advocacy_groups, beneficiary,
    organized, generational, analytical, national).

% Cannot be vaccinated themselves or cannot mount an immune response, and depend entirely on community-level immunity for protection. Under this reading they bear elevated exposure risk from unvaccinated others with no legal recourse to compel protective behavior around them; their exit option is functionally self-isolation, which is not a real exit.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, immunocompromised_individuals, payer,
    powerless, immediate, trapped, local).

% Children whose caregivers exercise the refusal right on their behalf; the minor bears the disease risk and any downstream harm but holds none of the decisional autonomy the doctrine is nominally protecting — the right is exercised by a proxy who is not the one exposed.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, unvaccinated_dependent_minors, payer,
    powerless, biographical, trapped, local).

% Absorb the clinical surge from preventable outbreaks that occur in under-vaccinated pockets; their occupational exposure and workload rise with community transmission that a compulsion-tolerant regime could have suppressed, but they have no standing to compel third-party vaccination.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, healthcare_workers_treating_outbreaks, payer,
    moderate, biographical, constrained, regional).

% Adjudicate the boundary itself across cases, weighing precedent on bodily integrity against public-health justifications; they do not bear the costs or collect the benefits directly but determine which reading of the kernel currently governs.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, constitutional_courts, observer,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(coercion_legitimacy_boundary__bodily_autonomy_primary, diffuse).
narrative_ontology:fixing_cost_class(coercion_legitimacy_boundary__bodily_autonomy_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, predictable line that individuals, employers, courts, and health agencies can all rely on: no medical procedure may be forced on a competent person's body, full stop, which removes the need for case-by-case proportionality litigation over every disease and every intervention.
% TRANSFER_FUNCTION: Moves the burden of infectious risk from the individual who refuses intervention onto third parties who cannot avoid exposure — most acutely those who are medically unable to be vaccinated themselves — while relieving refusers and their advocates of any compulsion cost.
% ABSENT_VOICES: Immunocompromised people and the parents of children too young to be vaccinated who nonetheless share exposure spaces are rarely parties to the litigation that entrenches this rule; the rule is typically fought and won in cases about the refuser's rights, not the exposed bystander's.
% DISAPPEARANCE_RATIONALE: If this categorical bar disappeared overnight, public health authorities would gain legal room to compel intervention during outbreaks weighed against harm, refusers would lose an absolute shield and face conditional consequences, and immunocompromised populations would gain a lever they currently lack — mandate litigation, workplace policy, and school admission rules would all shift.
% FOUNDING_PROBLEM: Built to prevent state and institutional actors from forcibly medicating, sterilizing, or experimenting on individuals without consent — a response to historical abuses (forced sterilization programs, non-consensual experimentation) where 'collective benefit' rhetoric was used to justify serious bodily violations.
% FOUNDING_PROBLEM_CORROBORATION: Bioethicists and disability-rights historians outside the current vaccine-mandate debate attest the founding problem (coercive non-consensual intervention against vulnerable populations) remains partly live and corroborate the doctrine's historical necessity; public health epidemiologists and immunocompromised-patient advocacy organizations — outside the beneficiary set of refusers and enforcement bodies — attest that in the specific context of routine, low-risk, well-studied vaccination, the doctrine now functions to shield refusal from any collective-harm accounting rather than to prevent the abuses it was built against.
narrative_ontology:disappearance_verdict(coercion_legitimacy_boundary__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(coercion_legitimacy_boundary__bodily_autonomy_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(coercion_legitimacy_boundary__bodily_autonomy_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(coercion_legitimacy_boundary__bodily_autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coercion_legitimacy_boundary__bodily_autonomy_primary_tests).
:- end_tests(coercion_legitimacy_boundary__bodily_autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42 at interval end) rather than high because the doctrine genuinely coordinates a real value — protection against historical abuses of forced intervention — and the harm it produces is diffuse and probabilistic rather than direct expropriation. It is not zero, and it is authored as rising over the interval, because the categorical bar's non-enforcement externality compounds as community-level immunity erodes over time in the populations that rely on it. Suppression is comparatively low (0.28) because the doctrine restrains state action rather than actively suppressing dissent — its coercive force runs toward the state, not toward the exposed bystanders, who have no suppression mechanism working against them at all, which is itself part of the harm (they cannot even resist through the legal channel this doctrine offers only to refusers). Resistance is elevated (0.62) reflecting active contestation by public health authorities and immunocompromised advocacy groups against the categorical reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Refusers and mandate-enforcement bodies sit near the beneficiary end: refusers gain an absolute shield, and enforcement bodies gain a bright-line rule that reduces their litigation and liability burden. Immunocompromised individuals and dependent minors sit near the target end: trapped exit options (no way to opt out of shared air, shared schools, shared workplaces) and powerless structural position mean the categorical bar's costs land on them with no corresponding voice in how the rule is set.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing coercive non-consensual medical abuse of vulnerable populations — remains partly live in contexts far from routine vaccination (forced sterilization, non-consensual psychiatric treatment, experimentation on incarcerated or disabled people). Classifying this as tangled_rope rather than snare or mountain preserves that genuine coordination function while still registering that, specifically applied to community-transmissible disease prevention, the doctrine has drifted into shielding a cost-externalizing choice from any collective accounting. A pure mountain or rope classification would erase the victim set entirely; a pure snare classification would erase the doctrine's real historical justification and its ongoing salience in non-vaccination contexts.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    categorical_vs_proportional_framing_location,
    'Is the disagreement between this reading and the proportionality_reading located in a factual dispute (how severe is the disease, how effective is the intervention) or in a genuinely incommensurable normative premise (whether any degree of collective benefit can ever license non-consensual bodily intervention)?',
    'Track whether proponents of this reading change position when presented with extreme-severity hypotheticals (e.g., a disease with near-certain fatality and near-perfect vaccine efficacy). If position holds even at the extreme, the disagreement is normative/incommensurable; if it shifts, the apparent categorical reading was implicitly proportional all along.',
    'If the categorical claim is actually proportional reasoning at an extreme point, this reading and the proportionality_reading are not truly foreclosing readings but points on a shared continuum, which would change the reading_relations declared in cs_structure from coexists_with toward influences.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(categorical_vs_proportional_framing_location, conceptual, 'Whether the categorical bodily-autonomy premise is genuinely incommensurable with proportionality reasoning or is proportionality reasoning at a limit.').

omega_variable(
    proxy_consent_legitimacy,
    'When a parent or guardian exercises the refusal right on behalf of a dependent minor who bears the exposure risk, is that a legitimate exercise of the doctrine or a structural mismatch between who decides and who bears the cost?',
    'Compare outcomes and legal treatment in jurisdictions that allow mature-minor override of parental medical refusal against those that do not, controlling for disease severity.',
    'If proxy-exercised refusal is found to systematically misalign decision-maker and risk-bearer, the victim set for this reading should be understood as including a genuine internal contradiction, not merely an external cost — strengthening the tangled_rope reading over a pure rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proxy_consent_legitimacy, empirical, 'Whether proxy consent for minors is a legitimate extension of bodily autonomy or a structural mismatch between decider and risk-bearer.').

omega_variable(
    historical_abuse_baseline_drift,
    'Has the founding problem (forced sterilization, non-consensual experimentation) sufficiently receded in the routine-vaccination context specifically, even if it remains live in other medical-coercion contexts (psychiatric commitment, incarcerated populations, reproductive coercion)?',
    'Domain-specific historical and legal review distinguishing vaccination-specific coercion history from the broader medical-coercion history the doctrine was built to address; check whether courts treat vaccination mandates and, e.g., forced sterilization under the same doctrinal test or have already bifurcated them.',
    'If courts have already functionally bifurcated these domains, this reading''s claim to inherit the full weight of the founding problem in the vaccination context specifically is weaker than the doctrine''s rhetoric suggests, supporting the founding_problem_status of ''contested'' rather than ''live'' for this specific application.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(historical_abuse_baseline_drift, empirical, 'Whether the historical abuse baseline that justifies the categorical rule still applies with full force to routine vaccination specifically.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coercion_legitimacy_boundary__bodily_autonomy_primary, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coer_tr_t0, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 0, 0.08).
narrative_ontology:measurement(coer_tr_t4, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 4, 0.09).
narrative_ontology:measurement(coer_tr_t8, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 8, 0.1).
narrative_ontology:measurement(coer_tr_t12, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 12, 0.12).
narrative_ontology:measurement(coer_tr_t16, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 16, 0.13).
narrative_ontology:measurement(coer_tr_t20, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 20, 0.14).
narrative_ontology:measurement(coer_tr_t24, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 24, 0.15).

% Extraction over time
narrative_ontology:measurement(coer_be_t0, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(coer_be_t4, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 4, 0.27).
narrative_ontology:measurement(coer_be_t8, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 8, 0.31).
narrative_ontology:measurement(coer_be_t12, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 12, 0.35).
narrative_ontology:measurement(coer_be_t16, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 16, 0.38).
narrative_ontology:measurement(coer_be_t20, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(coer_be_t24, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 24, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(coer_su_t0, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(coer_su_t4, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 4, 0.19).
narrative_ontology:measurement(coer_su_t8, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 8, 0.21).
narrative_ontology:measurement(coer_su_t12, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 12, 0.23).
narrative_ontology:measurement(coer_su_t16, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 16, 0.25).
narrative_ontology:measurement(coer_su_t20, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 20, 0.27).
narrative_ontology:measurement(coer_su_t24, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 24, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coercion_legitimacy_boundary__bodily_autonomy_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__bodily_autonomy_primary, coercion_legitimacy_boundary__public_health_primary).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__bodily_autonomy_primary, coercion_legitimacy_boundary__proportionality_reading).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the coercion_legitimacy_boundary kernel. bodily_autonomy_primary forecloses public_health_primary within a single legal framework (a court cannot simultaneously hold that consent is categorically required and that collective benefit can override it) while coexisting with proportionality_reading as a live alternative position held by different courts and jurisdictions. Each reading carries its own ε, beneficiary/victim structure, and classification per the ε-invariance principle; they are not to be averaged or merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
