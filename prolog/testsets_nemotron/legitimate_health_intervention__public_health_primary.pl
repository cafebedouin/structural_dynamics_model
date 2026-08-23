% ============================================================================
% CONSTRAINT STORY: legitimate_health_intervention__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimate_health_intervention__public_health_primary, []).

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
 *   constraint_id: legitimate_health_intervention__public_health_primary
 *   human_readable: Public Health Primary Mandate for Health Interventions
 *   domain: public_health_policy/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   This constraint represents the public-health-primary reading of the
 *   legitimate_health_intervention kernel: legitimacy derives from measurable
 *   population-level morbidity/mortality reduction; individual refusal is
 *   treated as externality imposition rather than rights exercise. The
 *   constraint emerged from 19th/20th century infectious disease control
 *   (smallpox, polio) and was dramatically expanded during COVID-19. It
 *   coordinates genuine collective action (herd immunity) but extracts
 *   asymmetrically: the immunocompromised and elderly receive concentrated
 *   survival benefits while refusers bear employment termination, access
 *   exclusion, and social marginalization. Enforcement requires active state
 *   power (mandates, passports, employment rules) — suppression is high and
 *   rising. Theater ratio is low but increasing as mandates extend to
 *   lower-severity diseases where the coordination rationale thins.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_health_intervention__public_health_primary, 0.68).
domain_priors:suppression_score(legitimate_health_intervention__public_health_primary, 0.78).
domain_priors:theater_ratio(legitimate_health_intervention__public_health_primary, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, extractiveness, 0.68).
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_health_intervention__public_health_primary, tangled_rope).
narrative_ontology:human_readable(legitimate_health_intervention__public_health_primary, "Public Health Primary Mandate for Health Interventions").
narrative_ontology:topic_domain(legitimate_health_intervention__public_health_primary, "public_health_policy/medical_ethics/constitutional_law").

domain_priors:requires_active_enforcement(legitimate_health_intervention__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_health_intervention__public_health_primary, '56e93365-421b-458b-b900-6b82795fe3e6').
narrative_ontology:cs_kernel_codification('56e93365-421b-458b-b900-6b82795fe3e6', distributed).
narrative_ontology:cs_authority_grounding('56e93365-421b-458b-b900-6b82795fe3e6', practice).
narrative_ontology:cs_interpretation_layer_present('56e93365-421b-458b-b900-6b82795fe3e6').
narrative_ontology:cs_reading_relation('56e93365-421b-458b-b900-6b82795fe3e6', legitimate_health_intervention__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('56e93365-421b-458b-b900-6b82795fe3e6', legitimate_health_intervention__proportionality_reading, influences).
narrative_ontology:cs_axiom('56e93365-421b-458b-b900-6b82795fe3e6', foundational, population_outcome_justifies_coercion).
narrative_ontology:cs_axiom_status(population_outcome_justifies_coercion, holdable).
narrative_ontology:cs_axiom_grounding('56e93365-421b-458b-b900-6b82795fe3e6', population_outcome_justifies_coercion, instrumental).
narrative_ontology:cs_axiom('56e93365-421b-458b-b900-6b82795fe3e6', foundational, refusal_is_externality_not_right).
narrative_ontology:cs_axiom_status(refusal_is_externality_not_right, holdable).
narrative_ontology:cs_axiom_grounding('56e93365-421b-458b-b900-6b82795fe3e6', refusal_is_externality_not_right, conventional).
narrative_ontology:cs_reference_frame('56e93365-421b-458b-b900-6b82795fe3e6', jacobson_v_massachusetts_1905).
narrative_ontology:cs_drift_state('56e93365-421b-458b-b900-6b82795fe3e6', post_covid_mandate_expansion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('56e93365-421b-458b-b900-6b82795fe3e6', '').
narrative_ontology:cs_kernel_id(legitimate_health_intervention__public_health_primary, legitimate_health_intervention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__public_health_primary, immunocompromised_populations).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__public_health_primary, elderly_high_risk_groups).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__public_health_primary, healthcare_workers).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__public_health_primary, essential_workers_high_exposure).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__public_health_primary, public_health_authorities).
narrative_ontology:constraint_victim(legitimate_health_intervention__public_health_primary, unvaccinated_by_choice).
narrative_ontology:constraint_victim(legitimate_health_intervention__public_health_primary, vaccine_hesitant_populations).
narrative_ontology:constraint_victim(legitimate_health_intervention__public_health_primary, religious_objectors).
narrative_ontology:constraint_victim(legitimate_health_intervention__public_health_primary, medical_exemption_denied_individuals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(legitimate_health_intervention__public_health_primary, healthcare_workers).
narrative_ontology:constraint_victim(legitimate_health_intervention__public_health_primary, essential_workers_high_exposure).
narrative_ontology:constraint_vindicates(legitimate_health_intervention__public_health_primary, population_health_primacy_doctrine).
narrative_ontology:constraint_vindicates(legitimate_health_intervention__public_health_primary, externality_imposition_principle).
narrative_ontology:constraint_vindicates(legitimate_health_intervention__public_health_primary, collective_immunity_threshold).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issue mandates, define exemptions, enforce compliance through employment rules, school access, and facility entry requirements. Their legitimacy rests on measurable population health outcomes. They bear political cost for overreach but institutional inertia favors mandate maintenance.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, public_health_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Cannot mount adequate immune response to vaccination; depend on high community coverage for indirect protection. Have no exit from vulnerability — their survival literally depends on others' compliance. Bear no cost of mandates but receive concentrated survival benefit.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, immunocompromised_populations, beneficiary,
    powerless, biographical, trapped, national).

% Experience disproportionate morbidity/mortality from vaccine-preventable diseases. Benefit directly from community immunity. Limited exit: can self-isolate at severe quality-of-life cost. Political voice through voting blocs and advocacy organizations.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, elderly_high_risk_groups, beneficiary,
    moderate, biographical, constrained, national).

% Face occupational exposure mandates; benefit from reduced patient loads and workplace transmission. Bear compliance cost (mandatory vaccination as employment condition) but gain professional protection. Exit constrained by licensing, specialization, and vocational identity — leaving healthcare is a career destruction event.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, healthcare_workers, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(legitimate_health_intervention__public_health_primary, healthcare_workers, payer).

% Transit, food, logistics workers with high contact rates. Mandates protect them from workplace outbreaks but employment termination for non-compliance is a concentrated cost. Exit options limited by economic necessity — they cannot afford job loss.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, essential_workers_high_exposure, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(legitimate_health_intervention__public_health_primary, essential_workers_high_exposure, payer).

% Decline vaccination for personal/philosophical reasons. Bear concentrated costs: employment termination, exclusion from public venues, travel restrictions, social stigma. Exit is identity-locked — refusal has become fused with political/tribal identity; compliance would be experienced as betrayal of self-concept and community.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, unvaccinated_by_choice, payer,
    moderate, biographical, identity_locked, national).

% Uncertain or fearful rather than ideologically opposed. Bear similar costs to refuseniks but with less identity investment — potentially movable with trusted messengers, accommodation, or time. Exit constrained by information environment and access barriers rather than identity fusion.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, vaccine_hesitant_populations, payer,
    powerless, biographical, constrained, national).

% Claim sincere religious objection. Bear mandate costs plus legal battle costs. Exit is identity-locked through doctrinal commitment — compliance violates conscience and community standing. Organized through religious liberty litigation networks.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, religious_objectors, payer,
    organized, biographical, identity_locked, national).

% Have legitimate medical contraindications (allergy, immunodeficiency, prior adverse event) but fall outside narrow exemption criteria. Bear full mandate costs with zero medical benefit and real medical risk. No exit: cannot vaccinate safely, cannot access society without vaccination.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, medical_exemption_denied_individuals, payer,
    powerless, biographical, trapped, national).

% Analyze the proportionality, necessity, and least-restrictive-means structure of mandates. Provide the normative vocabulary courts and legislatures use. No material stake in outcomes.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, bioethics_scholars, observer,
    analytical, civilizational, analytical, universal).

% Adjudicate challenges to mandates under liberty, equality, and due process frameworks. Their rulings define the enforceable boundary of the constraint. No direct health or economic stake.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Achieves and maintains population immunity thresholds that interrupt transmission chains, protecting those who cannot protect themselves through vaccination. Solves the collective action problem where individual vaccination decisions create positive externalities (reduced transmission) that no single actor captures.
% TRANSFER_FUNCTION: Moves compliance burden (vaccination acceptance, employment risk, access restrictions) onto individuals who would otherwise free-ride on community immunity, and moves protection (reduced exposure, outbreak prevention) to vulnerable populations who cannot self-protect. The state extracts compliance from the hesitant/refusing to subsidize the immunocompromised and system stability.
% ABSENT_VOICES: Future generations who inherit the precedent of state-compelled medical intervention; children of objectors who bear mandate consequences without voice; global populations in vaccine-scarce regions whose access is unaffected by wealthy-nation mandates but who are rhetorically invoked in equity debates.
% DISAPPEARANCE_RATIONALE: If mandates vanished overnight, vaccination rates would drop below herd immunity thresholds for measles, pertussis, and COVID-19 within 1-2 years. Immunocompromised and elderly would face dramatically elevated mortality. Healthcare systems would face recurrent surge cycles. Employment and school systems would lose their primary infection control layer. The social contract around communal disease defense would fracture.
% FOUNDING_PROBLEM: Recurrent epidemics of vaccine-preventable diseases causing avoidable death and disability, particularly among those medically unable to vaccinate. The free-rider problem: individuals rationally decline vaccination (bearing small personal risk) while benefiting from others' uptake, driving coverage below elimination thresholds.
% FOUNDING_PROBLEM_CORROBORATION: Public health historians (outside beneficiary set) document pre-vaccine mortality burdens and post-mandate elimination achievements. Economists outside the mandate apparatus confirm the free-rider structure. However, the 'current' founding problem is contested: critics argue the original problem (measles, polio) is solved and mandates now target diseases with different transmission/severity profiles (COVID-19, influenza) where the externality calculus differs.
narrative_ontology:disappearance_verdict(legitimate_health_intervention__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_health_intervention__public_health_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_health_intervention__public_health_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(legitimate_health_intervention__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_health_intervention__public_health_primary, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_health_intervention__public_health_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimate_health_intervention__public_health_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimate_health_intervention__public_health_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects concentrated costs on refusers (employment loss, exclusion) versus diffuse benefits. Suppression (0.78) is high because the constraint's persistence depends on active enforcement — without mandates, coverage drops. Theater (0.22) is modest: the public health function is real, but mandate scope has expanded beyond elimination-threshold diseases. Accessibility collapse (0.65): alternatives (home schooling, remote work, medical exemption) exist but are practically inaccessible for most refusers. Resistance (0.58): significant legal, political, and cultural pushback but has not reversed mandates at scale. The tangled_rope claim reflects genuine coordination function + asymmetric extraction + active enforcement — all three structural gates present.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (public health authorities), the constraint is a rope: genuine coordination solving a real collective action problem with proportional enforcement. From the payer seats (unvaccinated, objectors, denied-exemption), it is a snare: enforcement machinery suppresses exit, alternatives are practically closed, and the coordination story feels like cover for ideological conformity. From the pure beneficiary seat (immunocompromised), it is a mountain: without it, they die — the constraint's necessity is existential, not negotiated. The engine computes these divergences from power/exit/role structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities (institutional, analytical exit) are structural beneficiaries — they gain legitimacy, budget, and authority from mandate administration. Immunocompromised/elderly (powerless/moderate, trapped/constrained exit) are pure beneficiaries — survival depends on the constraint. Healthcare/essential workers (organized/moderate, constrained exit) are dual-role: benefit from workplace protection but bear compliance-as-condition-of-employment. Unvaccinated/hesitant/objectors/denied-exemption (moderate/powerless, identity_locked/trapped/constrained exit) are payers — bear concentrated costs. Identity-locked exit for objectors is critical: refusal has fused with political identity, making compliance psychologically prohibitive regardless of material cost. Medical exemption denied are trapped — cannot vaccinate, cannot exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (eliminating vaccine-preventable death) is contested as 'live.' For measles/polio, the problem is largely solved in wealthy nations — mandates persist as maintenance against reimportation. For COVID/influenza, the problem is live but the externality calculus differs (non-sterilizing vaccines, evolving variants). The constraint shows mandatrophy signals: expanding mandate scope to diseases where individual protection ≠ transmission blocking, narrowing exemptions, rising theater. Yet the core coordination function (protecting the unpprotectable) remains live for immunocompromised populations. This is a tangled_rope with a live coordination core and an extractive fringe.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint one reading of a contested kernel (legitimate_health_intervention) rather than a free-standing constraint?',
    'Structural comparison with sibling readings (bodily_autonomy_primary, proportionality_reading): if they share the same referent (state health mandates) but instantiate different beneficiary/victim structures and different ε values, they are readings of one kernel.',
    'Confirms the committer frame: this story must be ε-invariant for its reading only; sibling readings are separate constraints linked via network.affects_constraints and cs_structure.reading_relations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commitment to the kernel/reading architecture for this constraint.').

omega_variable(
    bodily_autonomy_foreclosure,
    'Does the public_health_primary reading''s core premise (population health outcomes legitimize coercion) logically foreclose the bodily_autonomy_primary reading (bodily integrity is inviolable regardless of outcomes) within a single legal/ethical framework?',
    'Constitutional doctrine analysis: can a framework simultaneously hold that (a) measurable population benefit justifies compelled medical intervention AND (b) bodily integrity categorically prohibits compelled medical intervention? Courts have ruled these are mutually exclusive at the level of constitutional principle — a framework must choose.',
    'If forecloses, the relation is ''forecloses'' in cs_structure.reading_relations. If coexists_with, both remain live positions in different jurisdictions/coalitions. This determines whether the kernel has a structural fault line or a pluralistic equilibrium.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(bodily_autonomy_foreclosure, conceptual, 'Whether the two primary readings are logically incompatible in a single framework.').

omega_variable(
    proportionality_influence,
    'Does the public_health_primary reading''s expansion of mandate scope (to lower-severity diseases, non-sterilizing vaccines) create structural downstream pressure on the proportionality_reading by shifting the Overton window of what counts as ''proportional''?',
    'Track judicial citations and legislative drafting: when public_health_primary mandates normalize for Disease X, does the proportionality_reading''s threshold for Disease Y shift upward? Empirical study of precedent cascades.',
    'If influences, the relation is ''influences'' in cs_structure.reading_relations — this reading moves the structural conditions the sibling operates in without logically foreclosing it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_influence, empirical, 'Whether this reading''s scope expansion pressures the proportionality sibling.').

omega_variable(
    sterilizing_vs_nonsterilizing_externality,
    'Does the externality-imposition rationale hold for non-sterilizing vaccines (COVID-19 mRNA) where individual vaccination reduces severity but not transmission to the same degree as sterilizing vaccines (measles)?',
    'Epidemiological measurement of transmission reduction by vaccine type and variant. If transmission reduction falls below the threshold where individual refusal imposes measurable population harm, the externality claim weakens.',
    'If the externality rationale is vaccine-dependent, the constraint''s ε should be disaggregated by disease/vaccine type — this single story may cover multiple constraints with different ε (violating ε-invariance). Would require decomposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sterilizing_vs_nonsterilizing_externality, empirical, 'Whether the extraction/coordination balance differs by vaccine mechanism.').

omega_variable(
    exemption_narrowing_trajectory,
    'Is the narrowing of medical and religious exemptions a structural feature of this reading''s logic (no principled stopping point) or a contingent political choice?',
    'Comparative analysis of exemption frameworks across jurisdictions and time. If all public_health_primary regimes converge on narrow exemptions regardless of political culture, it''s structural. If variation persists, it''s contingent.',
    'If structural, the constraint has a built-in ratchet toward snare (extraction without coordination justification). If contingent, the tangent_rope classification is stable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(exemption_narrowing_trajectory, conceptual, 'Whether exemption narrowing is inherent to the reading or politically contingent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_health_intervention__public_health_primary, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lhiphp_tr_t0, legitimate_health_intervention__public_health_primary, theater_ratio, 0, 0.12).
narrative_ontology:measurement(lhiphp_tr_t6, legitimate_health_intervention__public_health_primary, theater_ratio, 6, 0.15).
narrative_ontology:measurement(lhiphp_tr_t12, legitimate_health_intervention__public_health_primary, theater_ratio, 12, 0.18).
narrative_ontology:measurement(lhiphp_tr_t18, legitimate_health_intervention__public_health_primary, theater_ratio, 18, 0.21).
narrative_ontology:measurement(lhiphp_tr_t24, legitimate_health_intervention__public_health_primary, theater_ratio, 24, 0.22).

% Extraction over time
narrative_ontology:measurement(lhiphp_be_t0, legitimate_health_intervention__public_health_primary, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(lhiphp_be_t6, legitimate_health_intervention__public_health_primary, base_extractiveness, 6, 0.51).
narrative_ontology:measurement(lhiphp_be_t12, legitimate_health_intervention__public_health_primary, base_extractiveness, 12, 0.59).
narrative_ontology:measurement(lhiphp_be_t18, legitimate_health_intervention__public_health_primary, base_extractiveness, 18, 0.64).
narrative_ontology:measurement(lhiphp_be_t24, legitimate_health_intervention__public_health_primary, base_extractiveness, 24, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(lhiphp_su_t0, legitimate_health_intervention__public_health_primary, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(lhiphp_su_t6, legitimate_health_intervention__public_health_primary, suppression_requirement, 6, 0.63).
narrative_ontology:measurement(lhiphp_su_t12, legitimate_health_intervention__public_health_primary, suppression_requirement, 12, 0.71).
narrative_ontology:measurement(lhiphp_su_t18, legitimate_health_intervention__public_health_primary, suppression_requirement, 18, 0.75).
narrative_ontology:measurement(lhiphp_su_t24, legitimate_health_intervention__public_health_primary, suppression_requirement, 24, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_health_intervention__public_health_primary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(legitimate_health_intervention__public_health_primary, 0.12).
narrative_ontology:affects_constraint(legitimate_health_intervention__public_health_primary, legitimate_health_intervention__bodily_autonomy_primary).
narrative_ontology:affects_constraint(legitimate_health_intervention__public_health_primary, legitimate_health_intervention__proportionality_reading).
narrative_ontology:affects_constraint(legitimate_health_intervention__public_health_primary, school_vaccination_mandates).
narrative_ontology:affects_constraint(legitimate_health_intervention__public_health_primary, healthcare_worker_mandates).
narrative_ontology:affects_constraint(legitimate_health_intervention__public_health_primary, travel_vaccine_requirements).

% DUAL FORMULATION NOTE:
% This constraint, legitimate_health_intervention__bodily_autonomy_primary, and legitimate_health_intervention__proportionality_reading form a constraint family decomposing the 'legitimate health intervention' kernel. They share the same enforcement machinery (mandates, passports, exemptions) but instantiate different ε values and beneficiary/victim structures. This reading has the highest ε (0.68) due to broad enforcement scope; bodily_autonomy_primary has ε ≈ 0.15 (minimal enforcement, protection of refusal); proportionality_reading has ε ≈ 0.35 (targeted, disease-calibrated enforcement).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legitimate_health_intervention__public_health_primary, organized, 0.35).
constraint_indexing:directionality_override(legitimate_health_intervention__public_health_primary, powerless, 0.85).
constraint_indexing:directionality_override(legitimate_health_intervention__public_health_primary, moderate, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
