% ============================================================================
% CONSTRAINT STORY: takings_clause_boundary__categorical_takings_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_takings_clause_boundary__categorical_takings_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: takings_clause_boundary__categorical_takings_reading
 *   human_readable: Categorical Takings Reading of the Fifth Amendment Takings Clause
 *   domain: constitutional_law/property_rights
 *
 * SUMMARY:
 *   The categorical takings reading of the Fifth Amendment holds that
 *   permanent physical occupations and total economic value eliminations are
 *   per se compensable takings, while all other regulations are evaluated
 *   under the Penn Central ad hoc balancing test. This reading instantiates
 *   one of three live interpretations of the Takings Clause kernel, sitting
 *   between a narrower physical-appropriation-only reading and a broader
 *   regulatory-takings reading. It is a commitment system constraint: a
 *   judicial doctrine grounded in constitutional text and precedent,
 *   administered by the Supreme Court, that coordinates the boundary between
 *   compensable and non-compensable government burdens on property. It has
 *   genuine coordination valueâbright-line rules stabilize expectations at
 *   the extremesâbut extracts from property owners in the vast middle
 *   ground where Penn Central almost never triggers compensation.
 *
 * KEY AGENTS:
 *   - Supreme Court (agenda_setter): Administers the categorical/Penn Central boundary through constitutional interpretation.
 *   - State and local governments (beneficiary): Retain regulatory flexibility in the middle ground without compensation liability.
 *   - Property owners with per se claims (beneficiary): Secure compensation for physical occupation or total wipeout under categorical rules.
 *   - Property owners under moderate regulation (payer): Bear uncompensated regulatory burdens falling in the Penn Central middle ground.
 *   - Constitutional scholars (observer): Analyze and critique the doctrinal framework from outside the litigation context.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(takings_clause_boundary__categorical_takings_reading, 0.65).
domain_priors:suppression_score(takings_clause_boundary__categorical_takings_reading, 0.55).
domain_priors:theater_ratio(takings_clause_boundary__categorical_takings_reading, 0.47).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, theater_ratio, 0.47).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(takings_clause_boundary__categorical_takings_reading, tangled_rope).
narrative_ontology:human_readable(takings_clause_boundary__categorical_takings_reading, "Categorical Takings Reading of the Fifth Amendment Takings Clause").
narrative_ontology:topic_domain(takings_clause_boundary__categorical_takings_reading, "constitutional_law/property_rights").

domain_priors:requires_active_enforcement(takings_clause_boundary__categorical_takings_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(takings_clause_boundary__categorical_takings_reading, 'd0fbcda3-b773-404d-965c-e4370d109dd1').
narrative_ontology:cs_kernel_codification('d0fbcda3-b773-404d-965c-e4370d109dd1', formalized).
narrative_ontology:cs_authority_grounding('d0fbcda3-b773-404d-965c-e4370d109dd1', lineage).
narrative_ontology:cs_interpretation_layer_present('d0fbcda3-b773-404d-965c-e4370d109dd1').
narrative_ontology:cs_reading_relation('d0fbcda3-b773-404d-965c-e4370d109dd1', takings_clause_boundary__physical_appropriation_reading, coexists_with).
narrative_ontology:cs_reading_relation('d0fbcda3-b773-404d-965c-e4370d109dd1', takings_clause_boundary__regulatory_takings_reading, coexists_with).
narrative_ontology:cs_axiom('d0fbcda3-b773-404d-965c-e4370d109dd1', foundational, categorical_physical_occupation_compensation).
narrative_ontology:cs_axiom_status(categorical_physical_occupation_compensation, holdable).
narrative_ontology:cs_axiom_grounding('d0fbcda3-b773-404d-965c-e4370d109dd1', categorical_physical_occupation_compensation, conventional).
narrative_ontology:cs_axiom('d0fbcda3-b773-404d-965c-e4370d109dd1', foundational, categorical_total_elimination_compensation).
narrative_ontology:cs_axiom_status(categorical_total_elimination_compensation, holdable).
narrative_ontology:cs_axiom_grounding('d0fbcda3-b773-404d-965c-e4370d109dd1', categorical_total_elimination_compensation, conventional).
narrative_ontology:cs_reference_frame('d0fbcda3-b773-404d-965c-e4370d109dd1', categorical_compensation_for_extreme_takings).
narrative_ontology:cs_drift_state('d0fbcda3-b773-404d-965c-e4370d109dd1', modern_administrative_state, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d0fbcda3-b773-404d-965c-e4370d109dd1', '').
narrative_ontology:cs_kernel_id(takings_clause_boundary__categorical_takings_reading, takings_clause_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(takings_clause_boundary__categorical_takings_reading, state_local_governments).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__categorical_takings_reading, property_owners_per_se_cases).
narrative_ontology:constraint_victim(takings_clause_boundary__categorical_takings_reading, property_owners_moderate_regulation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the boundary between per se takings and Penn Central balancing through constitutional interpretation, precedent, and case-by-case adjudication. Sets the doctrinal framework that determines when governments must compensate property owners.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, supreme_court, agenda_setter,
    institutional, generational, analytical, national).

% Retain broad regulatory flexibility under Penn Central balancing for land-use, environmental, and health-and-safety regulations that fall short of physical occupation or total value elimination. Compensation liability is avoided for the vast majority of regulatory burdens.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, state_local_governments, beneficiary,
    institutional, generational, constrained, national).

% Own property subjected to permanent physical occupation or total economic value elimination. Under the categorical rule, they are entitled to compensation without proving the full Penn Central balance, receiving clearer protection than owners in the middle ground.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, property_owners_per_se_cases, beneficiary,
    moderate, biographical, constrained, local).

% Bear uncompensated regulatory burdens on property use that diminish value but do not amount to total wipeout or physical invasion. Their claims for compensation are evaluated under Penn Central, which rarely finds a taking, leaving them to absorb the cost of regulation.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, property_owners_moderate_regulation, payer,
    moderate, biographical, constrained, local).

% Analyze, critique, and trace the doctrinal evolution of the categorical/Penn Central boundary from outside the litigation context, producing competing originalist, textualist, and policy-based assessments of the framework.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a judicial framework that distinguishes per se unconstitutional takings (permanent physical occupation and total economic value elimination) from ordinary regulations, enabling governments to regulate for public welfare while warning property owners which extreme intrusions trigger guaranteed compensation.
% TRANSFER_FUNCTION: Transfers the cost of middle-ground regulatory burdens from governments (which avoid compensation under Penn Central balancing) to property owners (who bear uncompensated losses); transfers certainty-value to property owners at the categorical poles by guaranteeing compensation for physical invasion and total wipeout.
% ABSENT_VOICES: Property owners experiencing moderate regulatory diminution whose Penn Central claims are routinely denied; advocates for a unified regulatory takings standard or pure physical-appropriation test who are marginalized by the Court's two-tier framework.
% DISAPPEARANCE_RATIONALE: Without the categorical/Penn Central boundary, takings jurisprudence would collapse to a single standardâeither broader compensation requirements (regulatory takings reading) or narrower ones (physical appropriation only)âfundamentally rearranging the risk allocation between government regulators and property owners across all land-use and environmental regulation.
% FOUNDING_PROBLEM: How to preserve government's police power to regulate for public health, safety, and welfare without extinguishing private property rights or imposing crippling compensation liability on every regulatory burden.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians debate whether the 1791 understanding supported categorical rules; progressive legal scholars argue the doctrine is a modern construct to accommodate the administrative state, while originalist scholars cite founding-era physical-invasion torts. Corroboration from academic legal historians outside the beneficiary seats (governments and pole-protected owners) is split, with no unanimous external attestation.
narrative_ontology:disappearance_verdict(takings_clause_boundary__categorical_takings_reading, world_rearranges).
narrative_ontology:founding_problem_status(takings_clause_boundary__categorical_takings_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(takings_clause_boundary__categorical_takings_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(takings_clause_boundary__categorical_takings_reading, 'none', 1).
narrative_ontology:epsilon_provenance(takings_clause_boundary__categorical_takings_reading, 0.65, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(takings_clause_boundary__categorical_takings_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(takings_clause_boundary__categorical_takings_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(takings_clause_boundary__categorical_takings_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) is substantial because the Penn Central balancing test is structurally permissive to regulators, leaving most property owners in the middle ground uncompensated. Suppression (0.55) is moderate: the constraint suppresses alternative compensation frameworks (e.g., pure regulatory takings or uniform physical-rule tests) through judicial precedent. Theater ratio (0.47) reflects the performative character of Penn Central's three-factor balancing, which appears neutral but reliably produces government-favorable outcomes. Accessibility collapse (0.60) captures the doctrinal lock-in: once a litigant is classified in the middle ground, alternative pathways to compensation collapse. Resistance (0.45) is moderate, driven by persistent property-rights advocacy and scholarly criticism. The metrics are authored independently of the claimed type: the constraint coordinates genuinely at the poles (supporting tangled_rope over snare) but the middle-ground extraction is too substantial for rope.
 *
 * PERSPECTIVAL GAP:
 *   The Supreme Court and government regulators experience this constraint as a necessary framework for managing regulatory liability; property owners at the poles experience it as protective; property owners in the middle ground experience it as a denial of compensation. The engine will compute low directionality (beneficiary-side) for governments and per se owners, and high directionality (target-side) for moderate-regulation owners.
 *
 * DIRECTIONALITY LOGIC:
 *   State and local governments benefit from regulatory flexibility (low d); property owners with per se claims benefit from guaranteed compensation rules (low-to-moderate d); property owners in the middle ground bear the cost of the regime through uncompensated losses (high d). The structural asymmetry is driven by the Penn Central test, which places the cost of ordinary regulation on property owners while exempting governments from compensation.
 *
 * MANDATROPHY ANALYSIS:
 *   The claimed type is tangled_rope rather than snare because the constraint possesses a genuine coordination function: without the categorical poles, governments would face crippling uncertainty about compensation liability for basic regulations, and property owners would lack clear protection against physical invasion. However, the asymmetric extraction in the middle ground prevents classification as pure rope. The classification resists both the government's framing (rope) and radical property-rights framing (snare) by acknowledging the dual structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'This constraint is the categorical_takings_reading of kernel takings_clause_boundary; would the physical_appropriation_reading or regulatory_takings_reading produce a different beneficiary/victim structure?',
    'Cross-reading comparison of which regulatory burdens trigger compensation and which seats bear the cost under each interpretive framework.',
    'If the physical appropriation reading were adopted, property_owners_per_se_cases experiencing total wipeout would lose their per se protection and shift to payer status; if the regulatory takings reading were adopted, state_local_governments would lose middle-ground flexibility and shift toward payer status.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Structural delta between sibling readings of the takings clause kernel').

omega_variable(
    original_meaning_vs_precedent,
    'Is the categorical distinction between physical/total-loss takings and regulatory takings grounded in the original understanding of 1791, or is it a twentieth-century judicial construct to manage the administrative state?',
    'Originalist historical linguistics of ''take'' and ''property'' at founding; reconstruction of early American regulatory practice and compensation norms.',
    'If the categorical rules are not originalist, the kernel''s authority rests on precedent rather than text, making the constraint more vulnerable to repudiation_pressure and reclassification toward conventional authority grounding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_meaning_vs_precedent, empirical, 'Original meaning versus constructed precedent grounding').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(takings_clause_boundary__categorical_takings_reading, 1978, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(takings_cat_tr_t1978, takings_clause_boundary__categorical_takings_reading, theater_ratio, 1978, 0.2).
narrative_ontology:measurement(takings_cat_tr_t1982, takings_clause_boundary__categorical_takings_reading, theater_ratio, 1982, 0.25).
narrative_ontology:measurement(takings_cat_tr_t1992, takings_clause_boundary__categorical_takings_reading, theater_ratio, 1992, 0.3).
narrative_ontology:measurement(takings_cat_tr_t2005, takings_clause_boundary__categorical_takings_reading, theater_ratio, 2005, 0.38).
narrative_ontology:measurement(takings_cat_tr_t2013, takings_clause_boundary__categorical_takings_reading, theater_ratio, 2013, 0.42).
narrative_ontology:measurement(takings_cat_tr_t2021, takings_clause_boundary__categorical_takings_reading, theater_ratio, 2021, 0.45).
narrative_ontology:measurement(takings_cat_tr_t2024, takings_clause_boundary__categorical_takings_reading, theater_ratio, 2024, 0.47).

% Extraction over time
narrative_ontology:measurement(takings_cat_be_t1978, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 1978, 0.42).
narrative_ontology:measurement(takings_cat_be_t1982, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 1982, 0.48).
narrative_ontology:measurement(takings_cat_be_t1992, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 1992, 0.55).
narrative_ontology:measurement(takings_cat_be_t2005, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 2005, 0.6).
narrative_ontology:measurement(takings_cat_be_t2013, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 2013, 0.62).
narrative_ontology:measurement(takings_cat_be_t2021, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 2021, 0.64).
narrative_ontology:measurement(takings_cat_be_t2024, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 2024, 0.65).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(takings_clause_boundary__categorical_takings_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(takings_clause_boundary__categorical_takings_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(takings_clause_boundary__categorical_takings_reading, physical_appropriation_reading).
narrative_ontology:affects_constraint(takings_clause_boundary__categorical_takings_reading, regulatory_takings_reading).

% DUAL FORMULATION NOTE:
% The natural-language concept of Takings Clause boundaries decomposes into three structurally distinct readings: categorical_takings_reading (physical + total wipeout per se, middle ground balanced), physical_appropriation_reading (only physical seizures trigger), and regulatory_takings_reading (any regulation going too far triggers). Each has distinct epsilon values, stakeholder structures, and compensation rules. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
