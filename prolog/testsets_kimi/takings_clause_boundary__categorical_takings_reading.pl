% ============================================================================
% CONSTRAINT STORY: takings_clause_boundary__categorical_takings_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   human_readable: Categorical Takings Doctrine (Physical Occupation + Total Wipeout Per Se; Penn Central Balancing for Remainder)
 *   domain: constitutional/law/property_rights
 *
 * SUMMARY:
 *   This constraint instantiates the categorical_takings_reading of the
 *   takings_clause_boundary kernel. The kernel is the Fifth Amendment Takings
 *   Clause ('nor shall private property be taken for public use, without just
 *   compensation'). This reading holds that permanent physical occupations
 *   and total value elimination trigger automatic (per se) compensation,
 *   while all other regulations are evaluated under the ad hoc Penn Central
 *   factors. Sibling readings include physical_appropriation_reading
 *   (compensation only for direct physical seizures) and
 *   regulatory_takings_reading (any regulation going 'too far' in diminishing
 *   value is compensable).
 *
 * KEY AGENTS:
 *   - Property owners (moderate/constrained): Primary beneficiaries who receive compensation protection and bright-line rules at the extremes.
 *   - Supreme Court (institutional/analytical): Agenda-setter that establishes, maintains, and enforces the doctrinal boundary.
 *   - Municipal governments (organized/constrained): Payers that must fund compensation for per se takings and face liability uncertainty in the Penn Central middle.
 *   - Regulatory agencies (institutional/constrained): Payers that lose regulatory flexibility and bear legal costs to avoid triggering per se rules.
 *   - Legal scholars (analytical/analytical): Observers who debate the doctrinal coherence and distributive consequences of the categorical approach.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(takings_clause_boundary__categorical_takings_reading, 0.62).
domain_priors:suppression_score(takings_clause_boundary__categorical_takings_reading, 0.58).
domain_priors:theater_ratio(takings_clause_boundary__categorical_takings_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(takings_clause_boundary__categorical_takings_reading, tangled_rope).
narrative_ontology:human_readable(takings_clause_boundary__categorical_takings_reading, "Categorical Takings Doctrine (Physical Occupation + Total Wipeout Per Se; Penn Central Balancing for Remainder)").
narrative_ontology:topic_domain(takings_clause_boundary__categorical_takings_reading, "constitutional/law/property_rights").

domain_priors:requires_active_enforcement(takings_clause_boundary__categorical_takings_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(takings_clause_boundary__categorical_takings_reading, '1c2ab934-6ad4-471f-adac-7ddd6c8ad79d').
narrative_ontology:cs_kernel_codification('1c2ab934-6ad4-471f-adac-7ddd6c8ad79d', fixed_text).
narrative_ontology:cs_authority_grounding('1c2ab934-6ad4-471f-adac-7ddd6c8ad79d', lineage).
narrative_ontology:cs_interpretation_layer_present('1c2ab934-6ad4-471f-adac-7ddd6c8ad79d').
narrative_ontology:cs_reading_relation('1c2ab934-6ad4-471f-adac-7ddd6c8ad79d', takings_clause_boundary__physical_appropriation_reading, influences).
narrative_ontology:cs_reading_relation('1c2ab934-6ad4-471f-adac-7ddd6c8ad79d', takings_clause_boundary__regulatory_takings_reading, coexists_with).
narrative_ontology:cs_axiom('1c2ab934-6ad4-471f-adac-7ddd6c8ad79d', foundational, brightline_physical_occupation_rule).
narrative_ontology:cs_axiom_status(brightline_physical_occupation_rule, holdable).
narrative_ontology:cs_axiom_grounding('1c2ab934-6ad4-471f-adac-7ddd6c8ad79d', brightline_physical_occupation_rule, conventional).
narrative_ontology:cs_axiom('1c2ab934-6ad4-471f-adac-7ddd6c8ad79d', foundational, total_elimination_per_se_compensation).
narrative_ontology:cs_axiom_status(total_elimination_per_se_compensation, holdable).
narrative_ontology:cs_axiom_grounding('1c2ab934-6ad4-471f-adac-7ddd6c8ad79d', total_elimination_per_se_compensation, conventional).
narrative_ontology:cs_reference_frame('1c2ab934-6ad4-471f-adac-7ddd6c8ad79d', constitutional_property_baseline_with_brightline_protections).
narrative_ontology:cs_drift_state('1c2ab934-6ad4-471f-adac-7ddd6c8ad79d', administrative_state_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1c2ab934-6ad4-471f-adac-7ddd6c8ad79d', '').
narrative_ontology:cs_kernel_id(takings_clause_boundary__categorical_takings_reading, takings_clause_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(takings_clause_boundary__categorical_takings_reading, property_owners).
narrative_ontology:constraint_victim(takings_clause_boundary__categorical_takings_reading, municipal_governments).
narrative_ontology:constraint_victim(takings_clause_boundary__categorical_takings_reading, regulatory_agencies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive constitutionally mandated compensation when government permanently occupies their property or eliminates all economically beneficial use. Enjoy bright-line protection at the extremes but face uncertain Penn Central balancing for partial regulations, which generates litigation risk in the middle ground.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, property_owners, beneficiary,
    moderate, biographical, constrained, national).

% Establishes and maintains the doctrinal boundary between per se takingsâpermanent physical occupations and total wipeoutsâand regulatory takings evaluated under the Penn Central ad hoc balancing test. Retains authority to expand or contract the categorical list through constitutional interpretation.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, supreme_court, agenda_setter,
    institutional, generational, analytical, national).

% Must pay just compensation when land use regulations or access requirements constitute a permanent physical occupation or eliminate all economically beneficial use. Bear fiscal liability and planning uncertainty for regulations that fall in the Penn Central middle ground.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, municipal_governments, payer,
    organized, biographical, constrained, national).

% Face direct liability risk when environmental, health, or safety regulations approach total value elimination or require physical access. Must dedicate legal and fiscal resources to defending takings claims and to crafting regulations that avoid triggering per se rules.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, regulatory_agencies, payer,
    institutional, biographical, constrained, national).

% Analyze and debate whether the categorical approach correctly implements the Takings Clause or instead creates a judicial subsidy for property owners at the expense of legitimate regulatory authority. Track doctrinal evolution from Penn Central through Cedar Point.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, legal_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(takings_clause_boundary__categorical_takings_reading, property_owners).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Stabilizes property owner expectations against arbitrary regulatory wipeout by providing bright-line notice: if the government physically occupies or destroys all value, compensation is due. This solves a credible commitment problem where regulators might otherwise be tempted to regulate around formal condemnation.
% TRANSFER_FUNCTION: Moves compensation obligations and regulatory flexibility from government entities to property owners at the extremes (per se categories), while moving uncertainty and litigation costs to both parties in the middle ground where Penn Central balancing applies.
% ABSENT_VOICES: Tenants and lessees with less-than-fee interests are structurally underrepresented in takings litigation because the doctrine centers on owner expectations. Environmental justice communities that benefit from uncompensated regulatory protections are rarely parties in compensation proceedings.
% DISAPPEARANCE_RATIONALE: If the categorical rule vanished overnight, governments would routinely impose physical occupations and total value wipes without compensation; property investment expectations would collapse, land use planning would shift dramatically, and regulatory behavior would expand at the extremes.
% FOUNDING_PROBLEM: Prevent government abuse of regulatory power to effectively seize property without formal condemnation and just compensation, substituting regulation for eminent domain to avoid paying.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and originalist scholars outside the contemporary property-rights bar attest the founding problem was real and motivated the Clause. Progressive constitutional scholars and land-use planners contest that the current categorical doctrine overshoots the founding problem and now functions as a wealth transfer to property owners.
narrative_ontology:disappearance_verdict(takings_clause_boundary__categorical_takings_reading, world_rearranges).
narrative_ontology:founding_problem_status(takings_clause_boundary__categorical_takings_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(takings_clause_boundary__categorical_takings_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(takings_clause_boundary__categorical_takings_reading, 'none', 1).
narrative_ontology:epsilon_provenance(takings_clause_boundary__categorical_takings_reading, 0.62, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is substantial (0.62) because the doctrine compels government compensation for broad categories of regulatory action, extracting fiscal and planning autonomy from the regulatory state. Suppression is moderate-high (0.58) because the constraint's persistence depends on judicial enforcement suppressing the alternative of uncompensated regulation. Theater is low-moderate (0.25): the doctrine is substantively functional but carries formalist performance in its rigid categorical posture. Accessibility collapse is moderate-high (0.65) because, once the doctrine is established, the alternative of uncompensated physical invasion or total wipeout effectively collapses for governments. Resistance (0.55) reflects persistent government litigation challenging compensation claims. The measurement series tracks doctrinal expansion from the Penn Central balancing era (1978) through Loretto (1982), Lucas (1992), and Cedar Point (2021), showing rising extraction as categorical rules expanded.
 *
 * PERSPECTIVAL GAP:
 *   The property owner seat experiences the constraint as protective coordination that stabilizes investment expectations, yielding a low directionality and low effective extraction. The municipal government and regulatory agency seats experience the same constraint as fiscal and operational extraction, yielding high directionality and high effective extraction. The Supreme Court seat sits near symmetric: it administers the constraint but neither pays compensation nor receives it directly. The engine computes this divergence from the structural beneficiary and victim declarations.
 *
 * DIRECTIONALITY LOGIC:
 *   Property owners are declared beneficiaries and are structurally protected by the constraint; their directionality sits near the beneficiary pole (low d), which damps effective extraction into a subsidy. Municipal governments and regulatory agencies are declared victims and bear the direct costs of compensation and foregone regulatory options; their directionality sits near the target pole (high d), amplifying effective extraction. No override is needed because the structural derivation matches the actual relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mandatrophy mislabeling by requiring both a genuine coordination function (stabilizing property expectations against regulatory expropriation) and identifiable victims (government entities that must pay). A pure snare would lack the coordination story; a pure rope would lack the asymmetric extraction. The active judicial enforcement requirement confirms that the constraint must be actively maintained against government resistance, distinguishing it from a self-enforcing mountain or a degraded piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary,
    'Does the categorical reading''s bright-line boundary genuinely reduce litigation costs and stabilize expectations, or does it simply shift strategic behavior to characterize regulations as falling just outside the per se categories?',
    'Empirical study of takings litigation rates, claim characterization, and settlement patterns before and after the expansion of categorical rules (Loretto, Lucas, Cedar Point).',
    'If litigation rates rose or claim characterization became more strategic, the boundary functions as a litigation magnet rather than stable coordination; if rates fell, the bright-line function is genuine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, empirical, 'Whether the categorical boundary functions as coordination or strategic litigation site.').

omega_variable(
    compensation_incidence,
    'Are compensation costs borne by the specific government entity that imposed the regulation, or are they diffused across general taxpayers?',
    'Fiscal tracing of takings judgments to specific municipal budgets versus state or federal general funds.',
    'Concentrated incidence would create direct fiscal feedback on regulators; diffuse incidence would mask the extraction and reduce political accountability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compensation_incidence, empirical, 'Who ultimately pays compensation judgments.').

omega_variable(
    regulatory_chill_magnitude,
    'Does the threat of categorical compensation liability materially chill beneficial public regulation (e.g., environmental, health, safety land use controls)?',
    'Comparative regulatory output studies across jurisdictions with varying takings doctrinal stringency.',
    'If chill is demonstrated, the coordination benefit for property owners is paired with substantial social cost; if not, the extraction from government is more fiscal than functional.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_chill_magnitude, empirical, 'Whether compensation liability reduces beneficial regulation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(takings_clause_boundary__categorical_takings_reading, 0, 46).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(takings_cat_tr_t0, takings_clause_boundary__categorical_takings_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(takings_cat_tr_t12, takings_clause_boundary__categorical_takings_reading, theater_ratio, 12, 0.15).
narrative_ontology:measurement(takings_cat_tr_t24, takings_clause_boundary__categorical_takings_reading, theater_ratio, 24, 0.18).
narrative_ontology:measurement(takings_cat_tr_t36, takings_clause_boundary__categorical_takings_reading, theater_ratio, 36, 0.22).
narrative_ontology:measurement(takings_cat_tr_t46, takings_clause_boundary__categorical_takings_reading, theater_ratio, 46, 0.25).

% Extraction over time
narrative_ontology:measurement(takings_cat_be_t0, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(takings_cat_be_t12, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 12, 0.48).
narrative_ontology:measurement(takings_cat_be_t24, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 24, 0.56).
narrative_ontology:measurement(takings_cat_be_t36, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 36, 0.6).
narrative_ontology:measurement(takings_cat_be_t46, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 46, 0.62).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(takings_clause_boundary__categorical_takings_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(takings_clause_boundary__categorical_takings_reading, takings_clause_boundary__physical_appropriation_reading).
narrative_ontology:affects_constraint(takings_clause_boundary__categorical_takings_reading, takings_clause_boundary__regulatory_takings_reading).

% DUAL FORMULATION NOTE:
% The takings_clause_boundary kernel decomposes into three structurally distinct constraints. categorical_takings_reading (this file) holds that physical occupations and total value elimination are per se takings with Penn Central balancing for the remainder. physical_appropriation_reading restricts compensation to direct physical seizures or occupations only. regulatory_takings_reading treats value-diminishing regulations as potentially compensable under an ad hoc balancing approach. These are distinct constraints because their epsilon values, beneficiary structures, and enforcement postures differ structurally; they are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
