% ============================================================================
% CONSTRAINT STORY: mandate_legitimacy_scope__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mandate_legitimacy_scope__proportionality_reading, []).

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
 *   constraint_id: mandate_legitimacy_scope__proportionality_reading
 *   human_readable: Proportionality-Conditioned Vaccine Mandate Authority
 *   domain: public_health_ethics/constitutional_law/medical_autonomy
 *
 * SUMMARY:
 *   This constraint instantiates the proportionality reading of the
 *   mandate_legitimacy_scope kernel: state authority to mandate vaccination
 *   is legitimate only when disease severity is high, the vaccine is safe and
 *   effective, and no less restrictive alternative exists. Under this
 *   reading, measles mandates clear the proportionality bar while flu
 *   mandates fail it, producing a victim set that is conditional on pathogen
 *   parameters. The constraint is actively enforced by public health
 *   authorities and reviewed by constitutional courts, creating a structure
 *   that coordinates genuine public health protection while simultaneously
 *   extracting bodily autonomy when empirical thresholds are manipulated or
 *   misapplied.
 *
 * KEY AGENTS:
 *   - public_health_authorities: Primary agenda_setter (institutional/constrained) â designs and enforces mandate policy using the proportionality framework
 *   - constitutional_courts: Secondary agenda_setter (institutional/constrained) â reviews mandates for proportionality and sets legal precedent
 *   - vulnerable_populations: Primary beneficiary (powerless/trapped) â receives protection from community transmission when mandates are proportionate
 *   - mandate_subjects: Primary payer/target (moderate/constrained) â bears bodily intrusion and liberty costs; victim status conditional on pathogen severity
 *   - bodily_autonomy_advocates: Excluded voice (organized/mobile) â rejects the proportionality framework as illegitimate compromise
 *   - medical_ethicists: Analytical observer (analytical/analytical) â evaluates whether the framework tracks genuine empirical parameters
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mandate_legitimacy_scope__proportionality_reading, 0.55).
domain_priors:suppression_score(mandate_legitimacy_scope__proportionality_reading, 0.6).
domain_priors:theater_ratio(mandate_legitimacy_scope__proportionality_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mandate_legitimacy_scope__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(mandate_legitimacy_scope__proportionality_reading, "Proportionality-Conditioned Vaccine Mandate Authority").
narrative_ontology:topic_domain(mandate_legitimacy_scope__proportionality_reading, "public_health_ethics/constitutional_law/medical_autonomy").

domain_priors:requires_active_enforcement(mandate_legitimacy_scope__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(mandate_legitimacy_scope__proportionality_reading, '4dec2de1-b477-41b3-8be3-0f8b0cc328bf').
narrative_ontology:cs_kernel_codification('4dec2de1-b477-41b3-8be3-0f8b0cc328bf', formalized).
narrative_ontology:cs_authority_grounding('4dec2de1-b477-41b3-8be3-0f8b0cc328bf', lineage).
narrative_ontology:cs_interpretation_layer_present('4dec2de1-b477-41b3-8be3-0f8b0cc328bf').
narrative_ontology:cs_reading_relation('4dec2de1-b477-41b3-8be3-0f8b0cc328bf', mandate_legitimacy_scope__public_health_primary, coexists_with).
narrative_ontology:cs_reading_relation('4dec2de1-b477-41b3-8be3-0f8b0cc328bf', mandate_legitimacy_scope__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_axiom('4dec2de1-b477-41b3-8be3-0f8b0cc328bf', foundational, coercion_requires_pathogen_specific_proportionality).
narrative_ontology:cs_axiom_status(coercion_requires_pathogen_specific_proportionality, holdable).
narrative_ontology:cs_axiom_grounding('4dec2de1-b477-41b3-8be3-0f8b0cc328bf', coercion_requires_pathogen_specific_proportionality, empirically_contingent).
narrative_ontology:cs_axiom('4dec2de1-b477-41b3-8be3-0f8b0cc328bf', foundational, less_restrictive_alternative_preempts_compulsion).
narrative_ontology:cs_axiom_status(less_restrictive_alternative_preempts_compulsion, holdable).
narrative_ontology:cs_axiom_grounding('4dec2de1-b477-41b3-8be3-0f8b0cc328bf', less_restrictive_alternative_preempts_compulsion, instrumental).
narrative_ontology:cs_reference_frame('4dec2de1-b477-41b3-8be3-0f8b0cc328bf', constitutional_proportionality_tradition).
narrative_ontology:cs_drift_state('4dec2de1-b477-41b3-8be3-0f8b0cc328bf', post_covid_19_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4dec2de1-b477-41b3-8be3-0f8b0cc328bf', '').
narrative_ontology:cs_kernel_id(mandate_legitimacy_scope__proportionality_reading, mandate_legitimacy_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__proportionality_reading, vulnerable_populations).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__proportionality_reading, general_public).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__proportionality_reading, mandate_subjects).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and enforce vaccination mandates under a proportionality framework that weighs disease severity, vaccine safety and efficacy, and the availability of less restrictive alternatives. They set the evidentiary thresholds and select which pathogens trigger mandate regimes.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Review mandate legislation and executive orders for proportionality, applying a balancing test between public health necessity and individual rights. Their precedents define how severity and alternative availability are weighed in law.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, constitutional_courts, agenda_setter,
    institutional, generational, constrained, national).

% Depend on herd immunity and mandate-driven uptake for protection when contraindications prevent direct vaccination. They cannot exit their medical vulnerability and benefit from reduced community transmission.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, vulnerable_populations, beneficiary,
    powerless, biographical, trapped, national).

% Benefits from lower community transmission and maintained healthcare capacity when mandates are applied to severe pathogens, without necessarily bearing the direct cost of the intervention.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, general_public, beneficiary,
    organized, biographical, mobile, national).

% Bear the cost of bodily intrusion, potential adverse effects, and liberty restrictions when subjected to vaccination or penalty. For measles-like severity this is experienced as necessary coordination; for flu-like severity it is experienced as unjust extraction.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, mandate_subjects, payer,
    moderate, biographical, constrained, national).

% Argue that medical intervention without informed consent is categorically impermissible regardless of pathogen severity. They are structurally excluded from the proportionality balancing calculus because their position forecloses the entire framework.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, bodily_autonomy_advocates, excluded,
    organized, generational, mobile, national).

% Analyze whether proportionality tests track genuine empirical parameters or function as post-hoc rationalizations for politically predetermined coercion. They evaluate the framework independently of the institutional interests that apply it.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, medical_ethicists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a decision procedure for when state coercion over bodily autonomy is permissible to protect vulnerable populations and maintain healthcare system capacity, preventing both unchecked paternalism and libertarian paralysis during infectious disease emergencies.
% TRANSFER_FUNCTION: Transfers the burden of medical intervention and compliance costs from the state and the general public to targeted individuals when empirical conditions of severity, safety, and lack of alternatives are judged to justify it.
% ABSENT_VOICES: Bodily autonomy absolutists who reject all medical coercion regardless of pathogen profile are excluded from the balancing calculus because their position eliminates the proportionality framework entirely. Public health absolutists who would compel regardless of alternatives are also excluded when courts enforce the test.
% DISAPPEARANCE_RATIONALE: If the proportionality framework vanished overnight, mandate law would polarize toward either unrestricted public health authority or categorical bodily autonomy protection. Courts would lose the structured balancing test, and the legal landscape would reorganize around one of the sibling readings.
% FOUNDING_PROBLEM: How to legitimately exercise state police power over bodily integrity during infectious disease emergencies without collapsing into either unchecked paternalism or libertarian paralysis.
% FOUNDING_PROBLEM_CORROBORATION: Civil liberties organizations (excluded seat) attest that unchecked mandate power poses risks of authoritarian drift; constitutional courts (agenda_setter seat outside the public-health beneficiary set) independently attest that a balancing framework is necessary to legitimate coercion. Neither corroboration comes from parties that directly benefit from mandate enforcement.
narrative_ontology:disappearance_verdict(mandate_legitimacy_scope__proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(mandate_legitimacy_scope__proportionality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(mandate_legitimacy_scope__proportionality_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(mandate_legitimacy_scope__proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(mandate_legitimacy_scope__proportionality_reading, 0.55, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mandate_legitimacy_scope__proportionality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(mandate_legitimacy_scope__proportionality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(mandate_legitimacy_scope__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.55) because the constraint extracts bodily autonomy but only conditionally; the same structure is coordination for measles and extraction for flu. Suppression (0.60) reflects that mandates are enforced through legal penalty and professional exclusion, though judicial review provides a partial check. Theater ratio (0.25) is moderate-low because proportionality tests contain genuine analytical content, but the risk of performative balancing rises when political pressure outstrips evidence. Accessibility collapse (0.45) is moderate because absolutist alternatives (bodily autonomy primary, public health primary) remain visible and politically live. Resistance (0.55) is substantial because anti-mandate movements and civil liberties groups actively contest the framework. The temporal series show a spike at interval point 16 representing emergency-period deference, followed by partial normalization.
 *
 * PERSPECTIVAL GAP:
 *   The vulnerable population seat experiences this constraint as protective rope; the mandate subject seat experiences it as potentially extractive depending on the pathogen. The court seat sees a legal doctrine; the public health authority sees a policy tool. The divergence is structural and pathogen-dependent, not merely perspectival.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities and constitutional courts sit near the beneficiary/agenda-setter end because the proportionality framework empowers their institutional role and legitimizes their decisions. Vulnerable populations and the general public are structural beneficiaries of reduced transmission. Mandate subjects are the structural targets: their directionality moves toward the full-target end when the pathogen severity is low or the proportionality test is performative, and toward symmetric when severity is high and alternatives absent. The engine should compute strong divergence between the protected beneficiary seats and the constrained payer seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The proportionality framework prevents mislabeling by requiring empirical triggers. Without it, public_health_primary would mislabel broad coercion as pure coordination, and bodily_autonomy_primary would mislabel necessary public health measures as pure extraction. However, the proportionality reading carries its own mandatrophy risk: if the empirical prongs become theatrical â severity assessments manipulated, alternative availability defined narrowly by those who benefit from mandate enforcement â the coordination function atrophies while the constraint persists. The theater_ratio metric captures this risk.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    severity_assessment_manipulation,
    'Who controls the empirical assessment of disease severity and vaccine efficacy, and can these assessments be manipulated to expand mandate scope beyond what genuine proportionality permits?',
    'Independent meta-analysis of severity metrics and vaccine trial data used in mandate litigation, compared against the assessments advanced by enforcing authorities.',
    'If severity assessments are systematically inflated or efficacy data selectively interpreted, the constraint''s extraction rises toward snare-like operation for low-severity pathogens; if assessments are robust, the moderate Îµ remains stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(severity_assessment_manipulation, empirical, 'Whether empirical inputs to proportionality are manipulable').

omega_variable(
    alternative_definition_ambiguity,
    'What counts as a sufficiently effective ''less restrictive alternative'' â epidemiological equivalence, economic feasibility, or political acceptability â and who defines the threshold?',
    'Comparative case law analysis across jurisdictions with different alternative-availability thresholds, correlated with mandate breadth.',
    'A broad definition of ''less restrictive'' would constrain mandate scope and lower extraction; a narrow or politically contingent definition hollows out the proportionality test and increases extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_definition_ambiguity, conceptual, 'Ambiguity in the less-restrictive-alternative prong').

omega_variable(
    kernel_reading_boundary,
    'How would classification change if the mandate_legitimacy_scope kernel were read through bodily_autonomy_primary or public_health_primary rather than proportionality?',
    'Comparative analysis of sibling constraint stories in the same kernel family.',
    'Under bodily_autonomy_primary, victim set expands to all mandate subjects (Îµ rises, type shifts toward snare). Under public_health_primary, victim set contracts (Îµ falls, type shifts toward rope). The proportionality reading''s moderate Îµ is stable only while empirical parameters remain contestable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Sibling reading structural deltas for the proportionality reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mandate_legitimacy_scope__proportionality_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mand_tr_t0, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(mand_tr_t4, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 4, 0.18).
narrative_ontology:measurement(mand_tr_t8, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement(mand_tr_t12, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 12, 0.26).
narrative_ontology:measurement(mand_tr_t16, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 16, 0.38).
narrative_ontology:measurement(mand_tr_t20, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 20, 0.3).

% Extraction over time
narrative_ontology:measurement(mand_be_t0, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(mand_be_t4, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 4, 0.42).
narrative_ontology:measurement(mand_be_t8, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(mand_be_t12, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 12, 0.52).
narrative_ontology:measurement(mand_be_t16, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 16, 0.64).
narrative_ontology:measurement(mand_be_t20, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 20, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(mand_su_t0, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(mand_su_t4, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 4, 0.52).
narrative_ontology:measurement(mand_su_t8, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 8, 0.58).
narrative_ontology:measurement(mand_su_t12, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 12, 0.62).
narrative_ontology:measurement(mand_su_t16, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 16, 0.74).
narrative_ontology:measurement(mand_su_t20, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 20, 0.66).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mandate_legitimacy_scope__proportionality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__proportionality_reading, public_health_primary).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__proportionality_reading, bodily_autonomy_primary).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the mandate_legitimacy_scope kernel. Each reading produces a distinct beneficiary/victim structure and Îµ profile. The proportionality reading is the empirically contingent middle position.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
