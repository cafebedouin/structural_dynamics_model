% ============================================================================
% CONSTRAINT STORY: mandate_legitimacy_scope__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mandate_legitimacy_scope__public_health_primary, []).

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
 *   constraint_id: mandate_legitimacy_scope__public_health_primary
 *   human_readable: State Vaccine Mandate Legitimacy (Public Health Primary Reading)
 *   domain: public_health/constitutional_law/medical_autonomy
 *
 * SUMMARY:
 *   This constraint story represents the public_health_primary reading of the
 *   contested kernel 'mandate_legitimacy_scope'. It asserts that state
 *   authority to compel vaccination is legitimate when necessary to protect
 *   vulnerable populations from serious harm. The constraint is the vaccine
 *   mandate itself — a state-imposed requirement that extracts compliance
 *   from individuals (especially the unvaccinated) to produce a collective
 *   good (herd immunity) that disproportionately benefits the
 *   immunocompromised. The reading claims the constraint is a legitimate
 *   coordination mechanism (a rope), but the authored metrics reveal
 *   substantial extraction (ε=0.75) and active enforcement, structurally
 *   positioning it as a tangled rope. The sibling readings —
 *   bodily_autonomy_primary (absolute bodily integrity) and
 *   proportionality_reading (contextual balancing) — are separate constraints
 *   in the kernel family.
 *
 * KEY AGENTS:
 *   - state_public_health_establishment: agenda setter (institutional/analytical) — sets and enforces mandates
 *   - immunocompromised_populations: primary beneficiary (powerless/trapped) — depend on mandate for survival
 *   - unvaccinated_individuals: primary payer (moderate/constrained) — bear bodily intrusion and liberty loss
 *   - general_public: dual beneficiary/payer (organized/constrained) — gain protection, bear same mandate burden
 *   - constitutional_courts: observer (institutional/analytical) — adjudicate mandate boundaries
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mandate_legitimacy_scope__public_health_primary, 0.75).
domain_priors:suppression_score(mandate_legitimacy_scope__public_health_primary, 0.8).
domain_priors:theater_ratio(mandate_legitimacy_scope__public_health_primary, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, extractiveness, 0.75).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mandate_legitimacy_scope__public_health_primary, tangled_rope).
narrative_ontology:human_readable(mandate_legitimacy_scope__public_health_primary, "State Vaccine Mandate Legitimacy (Public Health Primary Reading)").
narrative_ontology:topic_domain(mandate_legitimacy_scope__public_health_primary, "public_health/constitutional_law/medical_autonomy").

domain_priors:requires_active_enforcement(mandate_legitimacy_scope__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(mandate_legitimacy_scope__public_health_primary, '039a4808-cb34-46d3-b3a5-f02d98c1f502').
narrative_ontology:cs_kernel_codification('039a4808-cb34-46d3-b3a5-f02d98c1f502', formalized).
narrative_ontology:cs_authority_grounding('039a4808-cb34-46d3-b3a5-f02d98c1f502', lineage).
narrative_ontology:cs_interpretation_layer_present('039a4808-cb34-46d3-b3a5-f02d98c1f502').
narrative_ontology:cs_reading_relation('039a4808-cb34-46d3-b3a5-f02d98c1f502', bodily_autonomy_primary__mandate_legitimacy_scope, forecloses).
narrative_ontology:cs_reading_relation('039a4808-cb34-46d3-b3a5-f02d98c1f502', proportionality_reading__mandate_legitimacy_scope, influences).
narrative_ontology:cs_axiom('039a4808-cb34-46d3-b3a5-f02d98c1f502', foundational, state_police_power_includes_compulsory_vaccination).
narrative_ontology:cs_axiom_status(state_police_power_includes_compulsory_vaccination, holdable).
narrative_ontology:cs_axiom_grounding('039a4808-cb34-46d3-b3a5-f02d98c1f502', state_police_power_includes_compulsory_vaccination, conventional).
narrative_ontology:cs_axiom('039a4808-cb34-46d3-b3a5-f02d98c1f502', foundational, vulnerable_population_protection_justifies_bodily_intrusion).
narrative_ontology:cs_axiom_status(vulnerable_population_protection_justifies_bodily_intrusion, holdable).
narrative_ontology:cs_axiom_grounding('039a4808-cb34-46d3-b3a5-f02d98c1f502', vulnerable_population_protection_justifies_bodily_intrusion, deontological).
narrative_ontology:cs_reference_frame('039a4808-cb34-46d3-b3a5-f02d98c1f502', jacobson_police_power_framework).
narrative_ontology:cs_drift_state('039a4808-cb34-46d3-b3a5-f02d98c1f502', contemporary_covid_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('039a4808-cb34-46d3-b3a5-f02d98c1f502', '').
narrative_ontology:cs_kernel_id(mandate_legitimacy_scope__public_health_primary, mandate_legitimacy_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__public_health_primary, immunocompromised_populations).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__public_health_primary, vulnerable_populations).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__public_health_primary, unvaccinated_individuals).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__public_health_primary, vaccine_refusers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__public_health_primary, general_public).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__public_health_primary, general_public).
narrative_ontology:constraint_vindicates(mandate_legitimacy_scope__public_health_primary, public_health_necessity_doctrine).
narrative_ontology:constraint_vindicates(mandate_legitimacy_scope__public_health_primary, state_police_power_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacts and enforces vaccine mandates through legislation and public health orders. Justifies compulsion as necessary to achieve herd immunity and protect those who cannot be vaccinated. Bears administrative and political costs of enforcement but controls the mandate's scope and exceptions.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, state_public_health_establishment, agenda_setter,
    institutional, generational, analytical, national).

% Cannot receive vaccines or mount adequate immune response; depend on high population immunity to avoid exposure to life-threatening diseases. Have no exit from the risk environment other than the mandate's effect on community transmission. Bear no direct cost of the mandate but face existential harm if it is removed.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, immunocompromised_populations, beneficiary,
    powerless, biographical, trapped, national).

% Subject to compulsory vaccination or face exclusion from schools, workplaces, and public spaces. Some refuse on religious, philosophical, or safety grounds. Bear the bodily intrusion, potential adverse effects, and liberty restriction. Exit options limited to relocation to jurisdictions without mandates or homeschooling/remote work, which are costly and not universally available.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, unvaccinated_individuals, payer,
    moderate, biographical, constrained, national).

% Benefits from reduced disease transmission and stable public health infrastructure. Also subject to the same mandate requirements; bears the same bodily intrusion and liberty restriction as unvaccinated individuals but generally accepts the trade-off for collective protection. Exit options similarly constrained.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, general_public, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(mandate_legitimacy_scope__public_health_primary, general_public, payer).

% Adjudicate challenges to mandates under constitutional frameworks (e.g., Jacobson v. Massachusetts, religious freedom, due process). Their rulings define the operational boundaries of the mandate's legitimacy, shaping the constraint's effective scope over time.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Achieves herd immunity and protects vulnerable populations who cannot be vaccinated or for whom vaccines are less effective, by compelling vaccination of the broader population.
% TRANSFER_FUNCTION: Moves the burden of disease risk from immunocompromised populations to unvaccinated individuals, who bear the intrusion of mandatory vaccination and potential adverse effects, in exchange for collective protection.
% ABSENT_VOICES: Individuals with medical contraindications to vaccination who are not immunocompromised (e.g., allergic) but are still subject to mandate; future generations who bear the precedent of bodily compulsion; populations in jurisdictions without mandate authority who free-ride on global herd immunity.
% DISAPPEARANCE_RATIONALE: Without the mandate, vaccination rates would drop below herd immunity thresholds for many diseases, leaving immunocompromised individuals dependent on imperfect cocooning strategies; the state would lose its primary tool for outbreak control, and the legal precedent for compulsory medical intervention would be weakened.
% FOUNDING_PROBLEM: Historically, infectious diseases caused massive mortality and morbidity, disproportionately affecting the vulnerable; voluntary vaccination failed to achieve sufficient coverage to interrupt transmission, necessitating state compulsion to protect those who cannot protect themselves.
% FOUNDING_PROBLEM_CORROBORATION: Public health historians and epidemiologists attest that mandates were historically necessary for disease eradication (e.g., smallpox). Critics from medical ethics and civil liberties organizations attest that the founding problem is overstated for many current diseases and that less restrictive alternatives exist.
narrative_ontology:disappearance_verdict(mandate_legitimacy_scope__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(mandate_legitimacy_scope__public_health_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(mandate_legitimacy_scope__public_health_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(mandate_legitimacy_scope__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(mandate_legitimacy_scope__public_health_primary, 0.75, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mandate_legitimacy_scope__public_health_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(mandate_legitimacy_scope__public_health_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(mandate_legitimacy_scope__public_health_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.75) because the mandate imposes a significant bodily intrusion and liberty restriction on a defined class (the unvaccinated) to benefit another class (the immunocompromised). Suppression is high (0.8) because the constraint's persistence depends on active state enforcement (school exclusion, employment requirements, fines) and the suppression of exit alternatives. Theater ratio is moderate (0.3) — the public health rationale is genuine, but the mandate apparatus also serves institutional interests (bureaucratic expansion, political signaling). Accessibility collapse is high (0.7) because once the mandate is in place, the unvaccinated have few viable alternatives; resistance is moderate (0.6) reflecting sustained legal and political challenges.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (state) experiences the constraint as coordination infrastructure it built and maintains; the payer seats (unvaccinated, general public) experience it as enforced extraction. The beneficiary seat (immunocompromised) experiences it as essential protection. The engine computes these divergent per-seat classifications from the structural data — the claimed_type does not adjudicate the divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The state_public_health_establishment is the structural beneficiary (collects compliance, sets rules, controls exceptions — d near 0.0). Immunocompromised_populations are pure beneficiaries (d ≈ 0.0). Unvaccinated_individuals are full targets (bear the mandate's burden, constrained exit — d near 1.0). General_public sits near symmetric (d ≈ 0.5) — they both benefit from herd immunity and bear the mandate's intrusion. Courts are analytical observers (d = 0.5 by definition).
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling by separating the genuine coordination function (herd immunity for vulnerable protection) from the asymmetric extraction (burden placed on unvaccinated). The mandate is not a pure snare because the coordination function is real and the beneficiaries are identifiable and vulnerable; it is not a pure rope because the extraction is substantial and enforcement is active. The tangled_rope classification captures the hybrid nature and forces explicit accounting of who pays and who benefits.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the public_health_primary reading a genuine resolution of the kernel, or does it merely assert one side of an irresolvable normative conflict?',
    'Track whether courts and legislatures adopt a pure public health necessity standard (foreclosing bodily autonomy) or a proportionality balancing test (coexisting with bodily autonomy concerns).',
    'If pure necessity standard prevails, this reading forecloses bodily_autonomy_primary; if proportionality prevails, the three readings coexist in a structured balancing framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether the kernel admits a single legitimate reading or requires pluralist coexistence.').

omega_variable(
    coordination_extraction_boundary,
    'Is the mandate''s coordination function (herd immunity) structurally necessary for protecting the immunocompromised, or could the same protection be achieved through less extractive means (e.g., targeted cocooning, improved treatments, voluntary high uptake)?',
    'Empirical comparison of disease outcomes in jurisdictions with and without mandates, controlling for vaccine uptake, healthcare access, and population density; ethical analysis of less restrictive alternatives.',
    'If less extractive alternatives exist and are effective, the mandate''s extraction is not structurally necessary and the constraint shifts toward snare; if mandates are uniquely effective, the tangled_rope classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, empirical, 'Whether the extraction is a necessary cost of coordination or an avoidable imposition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mandate_legitimacy_scope__public_health_primary, 0, 120).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mand_tr_t0, mandate_legitimacy_scope__public_health_primary, theater_ratio, 0, 0.25).
narrative_ontology:measurement(mand_tr_t20, mandate_legitimacy_scope__public_health_primary, theater_ratio, 20, 0.27).
narrative_ontology:measurement(mand_tr_t40, mandate_legitimacy_scope__public_health_primary, theater_ratio, 40, 0.28).
narrative_ontology:measurement(mand_tr_t60, mandate_legitimacy_scope__public_health_primary, theater_ratio, 60, 0.29).
narrative_ontology:measurement(mand_tr_t80, mandate_legitimacy_scope__public_health_primary, theater_ratio, 80, 0.3).
narrative_ontology:measurement(mand_tr_t100, mandate_legitimacy_scope__public_health_primary, theater_ratio, 100, 0.3).
narrative_ontology:measurement(mand_tr_t120, mandate_legitimacy_scope__public_health_primary, theater_ratio, 120, 0.3).

% Extraction over time
narrative_ontology:measurement(mand_be_t0, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(mand_be_t20, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(mand_be_t40, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 40, 0.65).
narrative_ontology:measurement(mand_be_t60, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 60, 0.7).
narrative_ontology:measurement(mand_be_t80, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 80, 0.72).
narrative_ontology:measurement(mand_be_t100, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 100, 0.74).
narrative_ontology:measurement(mand_be_t120, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 120, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(mand_su_t0, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(mand_su_t20, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 20, 0.77).
narrative_ontology:measurement(mand_su_t40, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 40, 0.78).
narrative_ontology:measurement(mand_su_t60, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 60, 0.79).
narrative_ontology:measurement(mand_su_t80, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 80, 0.8).
narrative_ontology:measurement(mand_su_t100, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 100, 0.8).
narrative_ontology:measurement(mand_su_t120, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 120, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mandate_legitimacy_scope__public_health_primary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(mandate_legitimacy_scope__public_health_primary, 0.1).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__public_health_primary, school_vaccination_requirements).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__public_health_primary, healthcare_worker_vaccination_mandates).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__public_health_primary, travel_vaccination_requirements).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__public_health_primary, bodily_autonomy_primary__mandate_legitimacy_scope).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__public_health_primary, proportionality_reading__mandate_legitimacy_scope).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the mandate_legitimacy_scope kernel. The other two are bodily_autonomy_primary and proportionality_reading. All three share the kernel but instantiate different constraints with different ε, beneficiary/victim structures, and claimed types. They are linked via affects_constraints to enable contamination propagation analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
