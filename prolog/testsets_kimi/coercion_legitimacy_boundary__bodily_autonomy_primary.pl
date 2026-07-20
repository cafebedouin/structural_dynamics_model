% ============================================================================
% CONSTRAINT STORY: coercion_legitimacy_boundary__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   human_readable: Categorical Prohibition on Non-Consensual Medical Intervention
 *   domain: public health policy / medical ethics / constitutional law
 *
 * SUMMARY:
 *   The constraint is the legal and ethical principle that medical
 *   intervention without individual consent is categorically impermissible,
 *   regardless of collective benefit. This reading of the coercion legitimacy
 *   boundary kernel treats bodily autonomy as non-derogable. It coordinates
 *   society around an absolute veto but externalizes infection risk to
 *   immunocompromised individuals who cannot benefit from voluntary herd
 *   immunity alone. It is actively enforced by constitutional courts and
 *   medical ethics boards. The structural delta from sibling readings is that
 *   immunocompromised individuals are victims (exposed to unvaccinated
 *   populations when coercion is barred), while mandate enforcersâhere the
 *   judicial and legal institutions that enforce the autonomy
 *   prohibitionâare beneficiaries of the institutional authority the
 *   categorical rule vindicates.
 *
 * KEY AGENTS:
 *   - immunocompromised_individuals: Primary target (powerless/trapped) â bears the externalized risk of blocked public health mandates.
 *   - mandate_enforcers: Primary beneficiary/agenda_setter (institutional/analytical) â courts and legal guardians that enforce the categorical boundary and accumulate institutional legitimacy.
 *   - general_public: Secondary beneficiary (organized/mobile) â gains absolute protection against coercion.
 *   - proportionality_advocates: Excluded voice (moderate/constrained) â argues for context-sensitive coercion but is rendered irrelevant by the categorical rule.
 *   - public_health_agencies: Institutional payer (institutional/constrained) â bears narrowed operational capacity and political accountability for preventable outbreaks.
 *   - epidemiologists: Analytical observer (organized/analytical) â sees the population-level cost of the constraint but is overridden by the legal boundary.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.48).
domain_priors:suppression_score(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.55).
domain_priors:theater_ratio(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, extractiveness, 0.48).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coercion_legitimacy_boundary__bodily_autonomy_primary, tangled_rope).
narrative_ontology:human_readable(coercion_legitimacy_boundary__bodily_autonomy_primary, "Categorical Prohibition on Non-Consensual Medical Intervention").
narrative_ontology:topic_domain(coercion_legitimacy_boundary__bodily_autonomy_primary, "public health policy / medical ethics / constitutional law").

domain_priors:requires_active_enforcement(coercion_legitimacy_boundary__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(coercion_legitimacy_boundary__bodily_autonomy_primary, '60b8ed4e-d00e-482e-a9c7-283fbdff1125').
narrative_ontology:cs_kernel_codification('60b8ed4e-d00e-482e-a9c7-283fbdff1125', formalized).
narrative_ontology:cs_authority_grounding('60b8ed4e-d00e-482e-a9c7-283fbdff1125', lineage).
narrative_ontology:cs_interpretation_layer_present('60b8ed4e-d00e-482e-a9c7-283fbdff1125').
narrative_ontology:cs_reading_relation('60b8ed4e-d00e-482e-a9c7-283fbdff1125', coercion_legitimacy_boundary__public_health_primary, forecloses).
narrative_ontology:cs_reading_relation('60b8ed4e-d00e-482e-a9c7-283fbdff1125', coercion_legitimacy_boundary__proportionality_reading, forecloses).
narrative_ontology:cs_axiom('60b8ed4e-d00e-482e-a9c7-283fbdff1125', foundational, bodily_autonomy_categorical_imperative).
narrative_ontology:cs_axiom_status(bodily_autonomy_categorical_imperative, holdable).
narrative_ontology:cs_axiom_grounding('60b8ed4e-d00e-482e-a9c7-283fbdff1125', bodily_autonomy_categorical_imperative, deontological).
narrative_ontology:cs_axiom('60b8ed4e-d00e-482e-a9c7-283fbdff1125', foundational, collective_benefit_irrelevant_to_consent).
narrative_ontology:cs_axiom_status(collective_benefit_irrelevant_to_consent, holdable).
narrative_ontology:cs_axiom_grounding('60b8ed4e-d00e-482e-a9c7-283fbdff1125', collective_benefit_irrelevant_to_consent, deontological).
narrative_ontology:cs_reference_frame('60b8ed4e-d00e-482e-a9c7-283fbdff1125', inviolable_autonomy_reference).
narrative_ontology:cs_drift_state('60b8ed4e-d00e-482e-a9c7-283fbdff1125', post_pandemic_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('60b8ed4e-d00e-482e-a9c7-283fbdff1125', '').
narrative_ontology:cs_kernel_id(coercion_legitimacy_boundary__bodily_autonomy_primary, coercion_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__bodily_autonomy_primary, mandate_enforcers).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__bodily_autonomy_primary, general_public).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__bodily_autonomy_primary, immunocompromised_individuals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__bodily_autonomy_primary, public_health_agencies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Constitutional courts and legal institutions that interpret and enforce the categorical prohibition on non-consensual medical intervention. They strike down public health mandates, derive institutional authority from upholding the autonomy boundary, and administer the constraint through judicial review and liability rulings.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, mandate_enforcers, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(coercion_legitimacy_boundary__bodily_autonomy_primary, mandate_enforcers, beneficiary).

% Individuals who retain an absolute legal veto over bodily interventions regardless of externalities or collective benefit. They cannot be subjected to compulsory vaccination, treatment, or examination under the categorical rule.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, general_public, beneficiary,
    organized, biographical, mobile, national).

% People with impaired immune systems who cannot mount protective responses to certain vaccines and depend on high community uptake for herd immunity. They bear elevated infection morbidity and mortality when voluntary coverage is insufficient because coercive preventive measures are categorically barred.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, immunocompromised_individuals, payer,
    powerless, biographical, trapped, national).

% Government agencies whose mission is population-level disease prevention. Their toolkit is narrowed by a bright-line legal barrier that removes coercive options during outbreaks; they bear operational and political costs when preventable transmission occurs.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, public_health_agencies, payer,
    institutional, generational, constrained, national).

% Legal scholars and ethicists who argue that coercion legitimacy should scale with disease severity and transmission dynamics. Their framework is structurally excluded from adjudication because the categorical rule renders empirical context irrelevant.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, proportionality_advocates, excluded,
    moderate, biographical, constrained, national).

% Scientists who model outbreak dynamics and can quantify the population-level cost of non-coercion. Their findings are admissible in discourse but are trumped by the autonomy boundary in legal and policy outcomes.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, epidemiologists, observer,
    organized, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(coercion_legitimacy_boundary__bodily_autonomy_primary, diffuse).
narrative_ontology:fixing_cost_class(coercion_legitimacy_boundary__bodily_autonomy_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a bright-line legal and ethical boundary that prevents state or medical coercion, coordinating societal expectations around an absolute individual veto over bodily interventions regardless of external cost.
% TRANSFER_FUNCTION: Transfers the burden of infectious disease risk from the collective and public health institutions to immunocompromised and vulnerable individuals by categorically blocking coercive preventive measures that would otherwise increase herd immunity.
% ABSENT_VOICES: Proportionality advocates and public health officials who would condition coercion on disease severity and transmission dynamics are structurally excluded because the categorical rule renders empirical context irrelevant.
% DISAPPEARANCE_RATIONALE: If the categorical prohibition vanished, states would gain authority to compel medical interventions during emergencies, legal frameworks would shift to proportionality or public-health-primary tests, and the risk burden on immunocompromised individuals would reallocate as herd immunity could be pursued through coercion.
% FOUNDING_PROBLEM: State medical overreach and historical abuses such as forced sterilization, non-consensual experimentation, and paternalistic coercion created a need for an absolute, non-negotiable barrier against non-consensual bodily intervention.
% FOUNDING_PROBLEM_CORROBORATION: Medical historians and human rights organizations attest to the historical abuses. Public health economists and epidemiologists attest from outside the beneficiary set that the founding problem has shifted and the categorical solution now imposes asymmetric costs on vulnerable populations.
narrative_ontology:disappearance_verdict(coercion_legitimacy_boundary__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(coercion_legitimacy_boundary__bodily_autonomy_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(coercion_legitimacy_boundary__bodily_autonomy_primary, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(coercion_legitimacy_boundary__bodily_autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.48, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coercion_legitimacy_boundary__bodily_autonomy_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(coercion_legitimacy_boundary__bodily_autonomy_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(coercion_legitimacy_boundary__bodily_autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.48) because the categorical rule externalizes significant infectious disease risk to vulnerable populations, but its enforcement is inconsistentâcourts sometimes permit emergency mandates, moderating the net harm. Suppression (0.55) reflects the active judicial suppression of coercive public health alternatives. Theater ratio (0.30) captures the symbolic performance of autonomy values alongside substantive legal effect. Accessibility collapse is high (0.75) because once the categorical frame is accepted, proportionality alternatives collapse conceptually. Resistance (0.60) is substantial from public health experts and crisis responders who experience the constraint as an artificial barrier to protective action.
 *
 * PERSPECTIVAL GAP:
 *   The mandate enforcer seat experiences the constraint as a foundational coordination mechanism protecting human dignity and limiting state power. The immunocompromised seat experiences the same constraint as a structural transfer of risk that constrains collective protective action. The public health agency seat experiences narrowed operational capacity. The engine computes this divergence from identical structural data via directionality: beneficiaries (autonomy holders, enforcers) get low d; victims (immunocompromised) and constrained institutional actors get high d.
 *
 * DIRECTIONALITY LOGIC:
 *   Mandate enforcers and the general public are structural beneficiaries of the prohibition (d near the beneficiary end), receiving legal protection and institutional vindication. Immunocompromised individuals are structural targets (d near the full-target end) because the constraint specifically exposes them to harm by blocking the coercive measures that would most reduce their risk. Public health agencies sit at high d as institutional targets whose operational scope is compressed. Proportionality advocates sit as excluded observers with no directional flow because their preferred alternatives are conceptually barred.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled rope prevents mislabeling the constraint as pure extraction (snare) because the autonomy protection is a genuine coordination function with historical justification (preventing state medical abuse). It also prevents mislabeling it as pure coordination (rope) because the asymmetric risk transfer to immunocompromised individuals is a real, identifiable cost borne by a discrete population. The mandate is not obsoleteâthe founding problem of medical overreach remains contestableâso piton is excluded. The moderate Îµ reflects the genuine coordination cost of maintaining an absolute boundary, not cover for extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the bodily autonomy reading a genuine deontological constant or a constructed legal cover for avoiding collective public health responsibility?',
    'Comparative legal analysis across jurisdictions with different readings; observe whether autonomy-primary jurisdictions show systematically different health outcomes and political economy.',
    'If constructed to serve identifiable beneficiaries, reclassification toward snare or higher-extraction tangled rope is warranted; if a genuine deontological constant, the classification stabilizes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether this reading is a natural legal principle or a constructed constraint serving identifiable beneficiaries.').

omega_variable(
    enforcement_inconsistency_ambiguity,
    'Does the moderate extractiveness reflect consistent protection of autonomy with moderate externalized costs, or inconsistent judicial enforcement that sometimes blocks and sometimes permits coercion during emergencies?',
    'Track judicial outcomes across emergency declarations; measure whether extraction spikes during periods of strict autonomy enforcement and falls when courts defer to public health necessity.',
    'If inconsistent enforcement drives the metric, the constraint is a tangled rope with variable extraction tied to enforcement cycles; if consistent, the moderate Îµ is structurally stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_inconsistency_ambiguity, empirical, 'Whether moderate extractiveness derives from variable enforcement or from stable structural operation.').

omega_variable(
    structural_vs_internalized_suppression,
    'Is the suppression of coercive public health alternatives structural (courts actively striking down mandates) or internalized (public health officials self-censoring coercive proposals because the categorical norm is taken as unchallengeable)?',
    'Survey public health agency legal counsel and legislative drafting records to determine whether mandate proposals are abandoned pre-litigation due to anticipated judicial defeat or due to normative internalization.',
    'If internalized, effective suppression is higher than the structural measure suggests, and the constraint operates partly through cognitive capture of the administrative state.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(structural_vs_internalized_suppression, empirical, 'Structural judicial suppression versus internalized administrative self-censorship.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coercion_legitimacy_boundary__bodily_autonomy_primary, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coercion_legitimacy_bodily_autonomy_tr_t0, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 0, 0.2).
narrative_ontology:measurement(coercion_legitimacy_bodily_autonomy_tr_t8, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 8, 0.25).
narrative_ontology:measurement(coercion_legitimacy_bodily_autonomy_tr_t16, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 16, 0.3).
narrative_ontology:measurement(coercion_legitimacy_bodily_autonomy_tr_t24, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 24, 0.35).
narrative_ontology:measurement(coercion_legitimacy_bodily_autonomy_tr_t32, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 32, 0.32).
narrative_ontology:measurement(coercion_legitimacy_bodily_autonomy_tr_t40, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 40, 0.3).

% Extraction over time
narrative_ontology:measurement(coercion_legitimacy_bodily_autonomy_be_t0, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(coercion_legitimacy_bodily_autonomy_be_t8, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 8, 0.35).
narrative_ontology:measurement(coercion_legitimacy_bodily_autonomy_be_t16, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 16, 0.4).
narrative_ontology:measurement(coercion_legitimacy_bodily_autonomy_be_t24, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 24, 0.48).
narrative_ontology:measurement(coercion_legitimacy_bodily_autonomy_be_t32, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 32, 0.5).
narrative_ontology:measurement(coercion_legitimacy_bodily_autonomy_be_t40, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 40, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(coercion_legitimacy_bodily_autonomy_su_t0, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(coercion_legitimacy_bodily_autonomy_su_t8, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 8, 0.45).
narrative_ontology:measurement(coercion_legitimacy_bodily_autonomy_su_t16, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 16, 0.5).
narrative_ontology:measurement(coercion_legitimacy_bodily_autonomy_su_t24, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 24, 0.6).
narrative_ontology:measurement(coercion_legitimacy_bodily_autonomy_su_t32, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 32, 0.58).
narrative_ontology:measurement(coercion_legitimacy_bodily_autonomy_su_t40, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 40, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coercion_legitimacy_boundary__bodily_autonomy_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__bodily_autonomy_primary, coercion_legitimacy_boundary__public_health_primary).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__bodily_autonomy_primary, coercion_legitimacy_boundary__proportionality_reading).

% DUAL FORMULATION NOTE:
% This constraint and its siblings are decomposed from the colloquial label 'coercion legitimacy boundary' per the epsilon-invariance principle. Each reading instantiates a structurally distinct constraint with different beneficiary-victim structures, epsilon values, and foreclosure relations. They are linked as a constraint family through mutual network edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
