% ============================================================================
% CONSTRAINT STORY: legitimate_health_intervention__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimate_health_intervention__proportionality_reading, []).

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
 *   constraint_id: legitimate_health_intervention__proportionality_reading
 *   human_readable: Proportionality-Based Legitimacy for Health Interventions
 *   domain: public_health_policy/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   This constraint encodes the proportionality reading of legitimate health
 *   intervention: state coercion is legitimate only when intervention
 *   severity is proportional to the threat level, with both population harm
 *   and individual autonomy weighted by disease characteristics
 *   (transmissibility, case-fatality rate, morbidity profile). It is a
 *   conditional constraint — the victim set and extraction intensity vary
 *   with disease severity (e.g., measles mandates vs. flu recommendations).
 *   The constraint sits between public_health_primary (which minimizes
 *   individual autonomy) and bodily_autonomy_primary (which minimizes state
 *   power), claiming to balance both. Historically rooted in Jacobson v.
 *   Massachusetts (1905) and refined through subsequent jurisprudence and
 *   public health practice. The claimed type is tangled_rope: it coordinates
 *   collective protection against disease while extracting compliance from
 *   objectors, with enforcement actively maintained through legal mandates,
 *   school-entry requirements, and employment conditions.
 *
 * KEY AGENTS:
 *   - public_health_agencies: Primary agenda_setter (institutional/generational/arbitrage/global) — sets intervention thresholds, mandates, and enforcement mechanisms
 *   - vulnerable_populations: Primary beneficiary (organized/biographical/constrained/national) — gains protection from disease spread they cannot individually prevent
 *   - healthcare_systems: Beneficiary (institutional/generational/arbitrage/national) — avoids surge overload, preserves capacity
 *   - objectors_to_specific_interventions: Primary victim (moderate/biographical/constrained/national) — bears mandate compliance costs, faces penalties for refusal
 *   - minority_religious_communities: Victim (organized/biographical/identity_locked/national) — disproportionately burdened when mandates conflict with sincere beliefs
 *   - autonomy_prioritizing_individuals: Victim (moderate/biographical/constrained/national) — bears liberty restriction weighted against population benefit
 *   - courts: Observer (institutional/generational/analytical/national) — adjudicates proportionality challenges, sets precedent
 *   - legislatures: Agenda_setter (institutional/generational/arbitrage/national) — enacts statutory frameworks, defines exemption scopes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_health_intervention__proportionality_reading, 0.45).
domain_priors:suppression_score(legitimate_health_intervention__proportionality_reading, 0.38).
domain_priors:theater_ratio(legitimate_health_intervention__proportionality_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_health_intervention__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(legitimate_health_intervention__proportionality_reading, "Proportionality-Based Legitimacy for Health Interventions").
narrative_ontology:topic_domain(legitimate_health_intervention__proportionality_reading, "public_health_policy/medical_ethics/constitutional_law").

domain_priors:requires_active_enforcement(legitimate_health_intervention__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_health_intervention__proportionality_reading, '1d228e47-3595-4164-a72f-398c508fc98c').
narrative_ontology:cs_kernel_codification('1d228e47-3595-4164-a72f-398c508fc98c', formalized).
narrative_ontology:cs_authority_grounding('1d228e47-3595-4164-a72f-398c508fc98c', lineage).
narrative_ontology:cs_interpretation_layer_present('1d228e47-3595-4164-a72f-398c508fc98c').
narrative_ontology:cs_reading_relation('1d228e47-3595-4164-a72f-398c508fc98c', legitimate_health_intervention__public_health_primary, coexists_with).
narrative_ontology:cs_reading_relation('1d228e47-3595-4164-a72f-398c508fc98c', legitimate_health_intervention__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_axiom('1d228e47-3595-4164-a72f-398c508fc98c', foundational, intervention_severity_must_track_threat_level).
narrative_ontology:cs_axiom_status(intervention_severity_must_track_threat_level, holdable).
narrative_ontology:cs_axiom_grounding('1d228e47-3595-4164-a72f-398c508fc98c', intervention_severity_must_track_threat_level, conventional).
narrative_ontology:cs_axiom('1d228e47-3595-4164-a72f-398c508fc98c', foundational, individual_autonomy_and_population_harm_are_commensurable).
narrative_ontology:cs_axiom_status(individual_autonomy_and_population_harm_are_commensurable, holdable).
narrative_ontology:cs_axiom_grounding('1d228e47-3595-4164-a72f-398c508fc98c', individual_autonomy_and_population_harm_are_commensurable, instrumental).
narrative_ontology:cs_axiom('1d228e47-3595-4164-a72f-398c508fc98c', foundational, disease_characteristics_determine_weighting).
narrative_ontology:cs_axiom_status(disease_characteristics_determine_weighting, holdable).
narrative_ontology:cs_axiom_grounding('1d228e47-3595-4164-a72f-398c508fc98c', disease_characteristics_determine_weighting, empirically_contingent).
narrative_ontology:cs_reference_frame('1d228e47-3595-4164-a72f-398c508fc98c', jacobson_proportionality_framework).
narrative_ontology:cs_drift_state('1d228e47-3595-4164-a72f-398c508fc98c', contemporary_precision_public_health_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('1d228e47-3595-4164-a72f-398c508fc98c', '2026-08-20T14:30:00Z').
narrative_ontology:cs_kernel_id(legitimate_health_intervention__proportionality_reading, legitimate_health_intervention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__proportionality_reading, public_health_agencies).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__proportionality_reading, vulnerable_populations).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__proportionality_reading, healthcare_systems).
narrative_ontology:constraint_victim(legitimate_health_intervention__proportionality_reading, objectors_to_specific_interventions).
narrative_ontology:constraint_victim(legitimate_health_intervention__proportionality_reading, minority_religious_communities).
narrative_ontology:constraint_victim(legitimate_health_intervention__proportionality_reading, autonomy_prioritizing_individuals).
narrative_ontology:constraint_vindicates(legitimate_health_intervention__proportionality_reading, proportionality_principle).
narrative_ontology:constraint_vindicates(legitimate_health_intervention__proportionality_reading, least_restrictive_means_doctrine).
narrative_ontology:constraint_vindicates(legitimate_health_intervention__proportionality_reading, disease_characteristics_weighting).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set intervention thresholds, define mandate scopes, design enforcement mechanisms, and justify measures through epidemiological modeling. They hold regulatory authority, control public messaging, and can shift between voluntary and mandatory frameworks. Exit is arbitrage-grade: they can redefine the threat level, change the intervention, or delegate enforcement.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, public_health_agencies, agenda_setter,
    institutional, generational, arbitrage, global).

% Immunocompromised individuals, infants too young for vaccination, elderly with comorbidities — they gain indirect protection from reduced community transmission. They cannot individually prevent exposure and depend on population-level compliance. Exit is constrained: they can limit contacts but cannot secure safety without collective action.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, vulnerable_populations, beneficiary,
    organized, biographical, constrained, national).

% Hospitals and health systems avoid surge overload, preserve ICU capacity, and maintain elective care continuity when mandates reduce severe disease burden. They influence policy through professional associations and emergency declarations. Exit is arbitrage-grade: they can triage, transfer patients, or declare crisis standards.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, healthcare_systems, beneficiary,
    institutional, generational, arbitrage, national).

% Individuals who refuse specific interventions (e.g., COVID-19 vaccines, MMR) based on risk-benefit assessment, distrust, or preference. They face school exclusion, employment termination, travel restrictions, or fines. Exit is constrained: home schooling, job change, or relocation are costly and not universally available.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, objectors_to_specific_interventions, payer,
    moderate, biographical, constrained, national).

% Communities whose sincere religious beliefs conflict with mandated interventions (e.g., certain Christian Scientist, Amish, Orthodox Jewish groups). They face concentrated burden: exclusion from communal institutions (schools, workplaces) that are central to community life. Exit is identity_locked: leaving the community means abandoning religious identity and social world.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, minority_religious_communities, payer,
    organized, biographical, identity_locked, national).

% Individuals who prioritize bodily autonomy as a non-negotiable principle, regardless of disease characteristics. They experience mandates as fundamental liberty violations. Exit is constrained: same practical options as other objectors, but the principled stance makes compliance psychologically costly.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, autonomy_prioritizing_individuals, payer,
    moderate, biographical, constrained, national).

% Adjudicate constitutional challenges to mandates, apply proportionality balancing tests, set precedent for what interventions survive scrutiny. They do not bear compliance costs or gain protection directly; their role is structural interpretation.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, courts, observer,
    institutional, generational, analytical, national).

% Enact statutory mandate frameworks, define exemption categories, set penalty structures, and oversee agency rulemaking. They respond to political pressure from all seats. Exit is arbitrage-grade: they can amend statutes, create new exemptions, or defund enforcement.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, legislatures, agenda_setter,
    institutional, generational, arbitrage, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimate_health_intervention__proportionality_reading, public_health_agencies).
narrative_ontology:fixing_cost_class(legitimate_health_intervention__proportionality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective action problem of communicable disease control: individuals face strong incentives to free-ride on others' compliance, but universal compliance achieves herd protection that no individual can secure alone. Proportionality calibrates the mandate to the threat, avoiding over-reach that erodes legitimacy.
% TRANSFER_FUNCTION: Moves compliance burden (liberty restriction, medical risk, financial penalty) from the population at large onto objectors, in exchange for population-level disease reduction that benefits vulnerable groups and preserves healthcare capacity. The transfer is conditional: higher threat → more burden transferred to objectors.
% ABSENT_VOICES: Future generations (who inherit precedent and infrastructure), undocumented migrants (excluded from exemption processes and healthcare access), global south populations (affected by vaccine nationalism driven by domestic mandate priorities), and those with medical contraindications who cannot be vaccinated but are not always centered in proportionality analysis.
% DISAPPEARANCE_RATIONALE: If proportionality-based mandates vanished overnight, jurisdictions would revert to either unrestricted state power (public_health_primary) or absolute individual refusal rights (bodily_autonomy_primary). Disease control would become either more coercive or more voluntary, fundamentally reorganizing the legal and practical landscape of public health authority.
% FOUNDING_PROBLEM: Early 20th century urbanization created dense populations vulnerable to explosive epidemics (smallpox, diphtheria). The state needed authority to mandate vaccination but faced constitutional limits on police power. Jacobson v. Massachusetts (1905) established that liberty is not absolute and may be restricted for the common good — but only to the extent necessary.
% FOUNDING_PROBLEM_CORROBORATION: Epidemiologists and public health historians attest that communicable disease threats persist and evolve (antimicrobial resistance, zoonotic spillover, climate-driven range shifts). Legal scholars outside the public health establishment (e.g., Gostin, Jacobson critics) corroborate that the proportionality framework remains the dominant constitutional standard, though its application is contested. No major jurisdiction has abandoned the proportionality principle for health mandates.
narrative_ontology:disappearance_verdict(legitimate_health_intervention__proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_health_intervention__proportionality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_health_intervention__proportionality_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(legitimate_health_intervention__proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_health_intervention__proportionality_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_health_intervention__proportionality_reading_tests).
:- end_tests(legitimate_health_intervention__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) reflects that the constraint imposes compliance costs on objectors — fines, exclusion from school/work, loss of livelihood — while the coordination benefit (disease control) accrues to the population. Suppression (0.38) is moderate: alternatives (home schooling, religious exemptions in some jurisdictions, alternative employment) exist but are constrained. Theater ratio (0.22) is low-moderate: the proportionality analysis is genuinely operationalized in courts and agencies, but some mandates persist after threat diminishes (performance of caution). Accessibility collapse (0.48) reflects that once a mandate is understood, alternatives narrow but do not fully collapse (exemptions, relocation, non-compliance). Resistance (0.52) is substantial: legal challenges, political movements, and non-compliance are persistent. The conditional structure (victim set varies by disease) means ε is not constant — measles mandates extract more than flu recommendations — but the base_properties extractiveness is the reading's weighted assessment across the disease spectrum.
 *
 * PERSPECTIVAL GAP:
 *   From the public_health_agencies seat (agenda_setter, institutional, arbitrage exit), the constraint is coordination: it solves the collective action problem of disease control with calibrated tools. From the objectors seat (payer, moderate, constrained exit), it is extraction: they bear disproportionate costs for a benefit they may not value or accept. From vulnerable_populations (beneficiary, organized, constrained exit), it is essential protection they cannot secure individually. Courts (observer) see the proportionality balancing test as the legitimate structure. The engine computes per-seat classification from these structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health agencies and legislatures are beneficiaries/agenda_setters (d near 0.0) — they gain authority, legitimacy, and operational capacity. Vulnerable populations and healthcare systems are beneficiaries (d ~ 0.2-0.3) — they gain protection but do not control the constraint. Objectors, minority religious communities, and autonomy-prioritizing individuals are victims/payers (d ~ 0.7-0.9) — they bear compliance costs, liberty restrictions, and penalty risks with constrained exit. Minority religious communities are identity_locked (exit requires abandoning community/identity), pushing their d higher than moderate objectors with merely constrained exit. Courts are analytical (d = 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (controlling communicable disease in dense populations) remains live but transformed: vaccines/therapeutics changed the threat landscape, yet the mandate structure persists. The constraint avoids pure mandatrophy because proportionality requires continual re-calibration — mandates for eradicated diseases (smallpox) were lifted. However, some mandates persist at low threat levels (theater), and exemption structures have narrowed in some jurisdictions (extraction accumulation). The conditional structure (disease-dependent victim set) is the anti-mandatrophy mechanism: it forces periodic reassessment rather than automatic persistence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_threshold_ambiguity,
    'Where exactly is the proportionality threshold — what threat level justifies what intervention severity?',
    'Judicial precedent analysis across jurisdictions; systematic review of how courts weigh transmissibility, CFR, and intervention burden in proportionality tests.',
    'If threshold is vague, the constraint operates as a delegation of unguided discretion (higher effective extraction); if precise, it functions as a genuine coordination limit on state power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_threshold_ambiguity, conceptual, 'The operational boundary of the proportionality principle').

omega_variable(
    disease_characteristics_weighting_method,
    'How are disease characteristics (transmissibility, CFR, morbidity) formally weighted in the proportionality calculus?',
    'Analysis of public health agency frameworks (CDC ACIP, WHO SAGE), legislative statutes, and judicial opinions for explicit weighting methodologies.',
    'If weighting is ad hoc, extraction varies unpredictably (coordination function degrades); if systematic, the conditional structure is genuine coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(disease_characteristics_weighting_method, empirical, 'Whether the conditional ε-scaling follows a transparent methodology').

omega_variable(
    kernel_reading_identity,
    'Is this proportionality reading a stable structural position or a transitional compromise between the sibling readings?',
    'Longitudinal analysis of judicial doctrine and public health practice: does proportionality analysis produce distinct outcomes from either sibling, or does it track one sibling''s outcomes with rhetorical cover?',
    'If transitional, the constraint may be a scaffold (temporary) or piton (degraded); if stable, tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether proportionality is a distinct reading or a rhetorical bridge').

omega_variable(
    exemption_structure_as_extraction_modulator,
    'Do exemption structures (medical, religious, philosophical) genuinely modulate extraction, or do they function as theater masking a fixed mandate?',
    'Comparative analysis of exemption grant rates, judicial scrutiny of exemption claims, and mandate persistence after exemption narrowing.',
    'If exemptions are narrowly granted and highly scrutinized, effective extraction approaches the no-exemption baseline; if broadly available, extraction is genuinely modulated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exemption_structure_as_extraction_modulator, empirical, 'Whether exemptions are functional escape valves or performative').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_health_intervention__proportionality_reading, 1905, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lhi_proportionality_tr_t1905, legitimate_health_intervention__proportionality_reading, theater_ratio, 1905, 0.15).
narrative_ontology:measurement(lhi_proportionality_tr_t1950, legitimate_health_intervention__proportionality_reading, theater_ratio, 1950, 0.18).
narrative_ontology:measurement(lhi_proportionality_tr_t1976, legitimate_health_intervention__proportionality_reading, theater_ratio, 1976, 0.2).
narrative_ontology:measurement(lhi_proportionality_tr_t2005, legitimate_health_intervention__proportionality_reading, theater_ratio, 2005, 0.21).
narrative_ontology:measurement(lhi_proportionality_tr_t2020, legitimate_health_intervention__proportionality_reading, theater_ratio, 2020, 0.22).
narrative_ontology:measurement(lhi_proportionality_tr_t2025, legitimate_health_intervention__proportionality_reading, theater_ratio, 2025, 0.22).

% Extraction over time
narrative_ontology:measurement(lhi_proportionality_be_t1905, legitimate_health_intervention__proportionality_reading, base_extractiveness, 1905, 0.35).
narrative_ontology:measurement(lhi_proportionality_be_t1950, legitimate_health_intervention__proportionality_reading, base_extractiveness, 1950, 0.38).
narrative_ontology:measurement(lhi_proportionality_be_t1976, legitimate_health_intervention__proportionality_reading, base_extractiveness, 1976, 0.41).
narrative_ontology:measurement(lhi_proportionality_be_t2005, legitimate_health_intervention__proportionality_reading, base_extractiveness, 2005, 0.43).
narrative_ontology:measurement(lhi_proportionality_be_t2020, legitimate_health_intervention__proportionality_reading, base_extractiveness, 2020, 0.45).
narrative_ontology:measurement(lhi_proportionality_be_t2025, legitimate_health_intervention__proportionality_reading, base_extractiveness, 2025, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(lhi_proportionality_su_t1905, legitimate_health_intervention__proportionality_reading, suppression_requirement, 1905, 0.3).
narrative_ontology:measurement(lhi_proportionality_su_t1950, legitimate_health_intervention__proportionality_reading, suppression_requirement, 1950, 0.32).
narrative_ontology:measurement(lhi_proportionality_su_t1976, legitimate_health_intervention__proportionality_reading, suppression_requirement, 1976, 0.35).
narrative_ontology:measurement(lhi_proportionality_su_t2005, legitimate_health_intervention__proportionality_reading, suppression_requirement, 2005, 0.37).
narrative_ontology:measurement(lhi_proportionality_su_t2020, legitimate_health_intervention__proportionality_reading, suppression_requirement, 2020, 0.38).
narrative_ontology:measurement(lhi_proportionality_su_t2025, legitimate_health_intervention__proportionality_reading, suppression_requirement, 2025, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_health_intervention__proportionality_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(legitimate_health_intervention__proportionality_reading, 0.12).
narrative_ontology:affects_constraint(legitimate_health_intervention__proportionality_reading, legitimate_health_intervention__public_health_primary).
narrative_ontology:affects_constraint(legitimate_health_intervention__proportionality_reading, legitimate_health_intervention__bodily_autonomy_primary).
narrative_ontology:affects_constraint(legitimate_health_intervention__proportionality_reading, school_entry_vaccine_mandates).
narrative_ontology:affects_constraint(legitimate_health_intervention__proportionality_reading, healthcare_worker_vaccine_requirements).
narrative_ontology:affects_constraint(legitimate_health_intervention__proportionality_reading, emergency_health_powers_act).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the 'legitimate health intervention' kernel into three readings with distinct ε values, victim sets, and coordination/extraction balances. Proportionality_reading is the conditional middle; public_health_primary minimizes individual autonomy weight; bodily_autonomy_primary minimizes state power weight.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legitimate_health_intervention__proportionality_reading, organized, 0.25).
constraint_indexing:directionality_override(legitimate_health_intervention__proportionality_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
