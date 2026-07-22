% ============================================================================
% CONSTRAINT STORY: bureaucratic_drift_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bureaucratic_drift_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: bureaucratic_drift_reading
 *   human_readable: Farm Labor Survey Replacement as Uncoordinated Administrative Drift
 *   domain: administrative_law/agricultural_policy/labor_migration
 *
 * SUMMARY:
 *   This story instantiates the 'bureaucratic drift' reading of the
 *   AEWR/FLS-to-OEWS transition: USDA discontinued the Farm Labor Survey for
 *   its own budget and methodological reasons; DOL, facing a statutory need
 *   for wage data, reasonably substituted the largest wage survey the federal
 *   government runs; employer associations lobbied through ordinary
 *   discretionary channels available to any commenter. Under this reading,
 *   the resulting wage-floor effects on H-2A workers are incidental residue
 *   of routine, uncoordinated administrative process rather than the product
 *   of a hidden or coordinated scheme. This is deliberately NOT the capture
 *   reading, the hold-up reading, or the coverage-neutral reading — those are
 *   separate constraints (instrument_capture_reading,
 *   hold_up_efficiency_reading, coverage_neutral_reading) sharing the same
 *   underlying facts but attributing different mechanisms and normative
 *   weight to them.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bureaucratic_drift_reading, 0.28).
domain_priors:suppression_score(bureaucratic_drift_reading, 0.15).
domain_priors:theater_ratio(bureaucratic_drift_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bureaucratic_drift_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(bureaucratic_drift_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(bureaucratic_drift_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bureaucratic_drift_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(bureaucratic_drift_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bureaucratic_drift_reading, piton).
narrative_ontology:human_readable(bureaucratic_drift_reading, "Farm Labor Survey Replacement as Uncoordinated Administrative Drift").
narrative_ontology:topic_domain(bureaucratic_drift_reading, "administrative_law/agricultural_policy/labor_migration").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bureaucratic_drift_reading, '1f767d52-c9d0-48c3-8233-a6f86288afa6').
narrative_ontology:cs_kernel_codification('1f767d52-c9d0-48c3-8233-a6f86288afa6', distributed).
narrative_ontology:cs_authority_grounding('1f767d52-c9d0-48c3-8233-a6f86288afa6', distributed).
narrative_ontology:cs_reading_relation('1f767d52-c9d0-48c3-8233-a6f86288afa6', adverse_effect_measurability__instrument_capture_reading, coexists_with).
narrative_ontology:cs_reading_relation('1f767d52-c9d0-48c3-8233-a6f86288afa6', adverse_effect_measurability__hold_up_efficiency_reading, coexists_with).
narrative_ontology:cs_reading_relation('1f767d52-c9d0-48c3-8233-a6f86288afa6', adverse_effect_measurability__coverage_neutral_reading, influences).
narrative_ontology:cs_axiom('1f767d52-c9d0-48c3-8233-a6f86288afa6', foundational, administrative_action_requires_no_hidden_mechanism).
narrative_ontology:cs_axiom_status(administrative_action_requires_no_hidden_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('1f767d52-c9d0-48c3-8233-a6f86288afa6', administrative_action_requires_no_hidden_mechanism, empirically_contingent).
narrative_ontology:cs_axiom('1f767d52-c9d0-48c3-8233-a6f86288afa6', secondary, incidental_beneficiary_is_not_evidence_of_design).
narrative_ontology:cs_axiom_status(incidental_beneficiary_is_not_evidence_of_design, holdable).
narrative_ontology:cs_axiom_grounding('1f767d52-c9d0-48c3-8233-a6f86288afa6', incidental_beneficiary_is_not_evidence_of_design, conventional).
narrative_ontology:cs_reference_frame('1f767d52-c9d0-48c3-8233-a6f86288afa6', routine_agency_discretion_baseline).
narrative_ontology:cs_drift_state('1f767d52-c9d0-48c3-8233-a6f86288afa6', post_oews_adoption_contemporary, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('1f767d52-c9d0-48c3-8233-a6f86288afa6', '').
narrative_ontology:cs_kernel_id(bureaucratic_drift_reading, adverse_effect_measurability).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bureaucratic_drift_reading, employer_lobby_associations).
narrative_ontology:constraint_beneficiary(bureaucratic_drift_reading, usda_budget_office).
narrative_ontology:constraint_victim(bureaucratic_drift_reading, h2a_farmworkers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(bureaucratic_drift_reading, dol_eta).
narrative_ontology:constraint_vindicates(bureaucratic_drift_reading, routine_administrative_process_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Ran the Farm Labor Survey (FLS) that produced the Adverse Effect Wage Rate (AEWR) inputs. Faced genuine budget exposure and documented methodological problems (declining response rates, regional gaps) that made continuing FLS in its existing form difficult to justify internally. Discontinued FLS for reasons legible on their own terms, without reference to DOL's downstream wage-setting use.
narrative_ontology:constraint_stakeholder(bureaucratic_drift_reading, usda_nass, agenda_setter,
    institutional, biographical, constrained, national).

% Needed some replacement wage data source once FLS ended, on a statutory deadline, and reasonably reached for the Occupational Employment and Wage Statistics (OEWS) survey as the largest wage dataset the federal government runs, despite its coarser occupational categories and weaker fit to farm labor markets. Acted under time and resource pressure, not with intent to alter wage-floor outcomes.
narrative_ontology:constraint_stakeholder(bureaucratic_drift_reading, dol_eta, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(bureaucratic_drift_reading, dol_eta, payer).

% Lobbied both agencies through ordinary discretionary channels (public comment, congressional appropriations pressure, agency meetings) during the transition, seeking wage methodology outcomes favorable to labor cost. Obtained routine administrative relief available to any organized commenter; did not need to engineer the FLS discontinuation itself, which had independent causes.
narrative_ontology:constraint_stakeholder(bureaucratic_drift_reading, employer_lobby_associations, beneficiary,
    organized, biographical, mobile, national).

% Have wages set by the AEWR, now derived from OEWS rather than FLS. Bear whatever wage-floor drift results from the coarser survey instrument's occupational aggregation, without having participated in either agency's internal process or the lobbying that shaped the transition. Cannot exit the wage-setting mechanism; visa status ties them to it.
narrative_ontology:constraint_stakeholder(bureaucratic_drift_reading, h2a_farmworkers, payer,
    powerless, immediate, trapped, national).

% Realized real cost savings from discontinuing an expensive, methodologically strained survey. Its interest in the outcome was budgetary, internal to USDA, and not coordinated with DOL's wage-setting function or with employer lobbying.
narrative_ontology:constraint_stakeholder(bureaucratic_drift_reading, usda_budget_office, beneficiary,
    institutional, immediate, arbitrage, national).

% Would object that the OEWS substitution degrades wage-floor accuracy for farm labor specifically, but were not present in USDA's internal budget deliberations or DOL's data-source selection process — those were agency-internal administrative determinations, not open rulemakings where advocacy input was structurally invited.
narrative_ontology:constraint_stakeholder(bureaucratic_drift_reading, farmworker_advocacy_groups, excluded,
    moderate, biographical, constrained, national).

% Study the AEWR methodology transition and can trace each agency's independent rationale — USDA's budget and measurement problems, DOL's deadline-driven reach for the largest available survey, and employer lobbying's use of ordinary discretionary channels — without needing to posit coordination between them.
narrative_ontology:constraint_stakeholder(bureaucratic_drift_reading, labor_economists, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Each agency solved its own local problem: USDA reduced an unsustainable survey cost, DOL filled a statutory data gap with the most defensible available instrument, and employer commenters exercised ordinary participation rights in the resulting rulemaking process.
% TRANSFER_FUNCTION: No single deliberate transfer occurs. Whatever wage-floor effect follows from OEWS's coarser occupational categories is an incidental byproduct distributed across H-2A wage determinations, not a designed movement of value from workers to employers.
% ABSENT_VOICES: Farmworker advocacy groups were not parties to USDA's internal budget process or DOL's data-source selection, which were agency-internal administrative determinations rather than open notice-and-comment proceedings inviting their structural participation.
% DISAPPEARANCE_RATIONALE: If this reading of events (uncoordinated, routine administrative drift) were rejected in favor of a coordination reading, the same facts would be renarrated as capture or hold-up rather than accident — the world of administrative practice would not change, but its normative characterization would. Under this reading itself, nothing 'disappearing' is coherent: there is no single mechanism to remove, only independent agency choices that happened in sequence.
% FOUNDING_PROBLEM: FLS had become methodologically unreliable and budget-strained; DOL needed a lawful, defensible wage data source under statutory deadline; employers sought discretionary relief through channels open to any commenter.
% FOUNDING_PROBLEM_CORROBORATION: Independent methodological reviews of FLS response-rate decline (cited in USDA's own program reviews) corroborate the measurement-problem rationale from outside the beneficiary set; GAO-style administrative process audits corroborate that OEWS substitution followed ordinary interagency data-sourcing practice rather than a documented coordinated scheme. No source outside the three agencies' own contemporaneous records speaks to intent, which is the limit of this reading's corroboration.
narrative_ontology:disappearance_verdict(bureaucratic_drift_reading, contested).
narrative_ontology:founding_problem_status(bureaucratic_drift_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bureaucratic_drift_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-22',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(bureaucratic_drift_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bureaucratic_drift_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bureaucratic_drift_reading_tests).
:- end_tests(bureaucratic_drift_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low-moderate (0.28) because this reading holds that no party engineered the outcome for extractive gain — the beneficiary status of employer associations and USDA's budget office is incidental, not causal. Suppression is low (0.15): nothing prevents advocacy groups from participating in future rulemakings; their absence from THIS transition was structural to how administrative process works, not an act of exclusion. Theater ratio rises modestly (0.15→0.32) reflecting that as the OEWS substitution became a settled practice, agencies increasingly cited 'administrative regularity' as post hoc justification rather than revisiting the substantive fit question.
 *
 * PERSPECTIVAL GAP:
 *   From USDA's seat, this looks like ordinary program discontinuation for cost and quality reasons. From DOL's seat, it looks like reasonable adaptation to data scarcity under deadline. From the farmworker seat, it looks identical in its material effect to what the capture reading would predict — this is precisely why the kernel is contested: the same downstream facts are compatible with multiple different accounts of mechanism, and this reading is the one that declines to infer coordination from correlation.
 *
 * DIRECTIONALITY LOGIC:
 *   Employer associations and USDA's budget office are marked as beneficiaries because they gained from the outcome, but under this reading their gain is not the mechanism that produced the outcome — it is a byproduct of decisions each agency made for independent reasons. H-2A farmworkers are the payer because whatever wage-floor degradation follows from OEWS's coarser occupational granularity lands on them, but this reading holds that no one targeted them; the harm is diffuse and unintended rather than extracted.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists mandatrophy framing almost by construction: it holds that there was never a single coordinated mandate to have outlived its function, only three independent administrative rationales that happened to converge. If the drift reading is correct, there is no zombie mandate to retire — only ordinary bureaucratic churn whose effects should be evaluated and corrected through ordinary means (methodological review of OEWS's fit for farm labor), not through an accountability framework built for detecting capture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_coincidence,
    'Did USDA''s FLS discontinuation, DOL''s OEWS substitution, and employer lobbying occur as genuinely independent processes, or did informal coordination (e.g., employer associations lobbying USDA on FLS''s fate while simultaneously lobbying DOL on the replacement) link them in ways that would convert this reading into the instrument_capture_reading?',
    'FOIA-obtained interagency correspondence and lobbying disclosure records covering the FLS discontinuation period and the OEWS adoption rulemaking; timeline reconstruction cross-referencing lobbying contact logs against internal agency decision memos.',
    'If informal coordination is documented, this story''s core premise (uncoordinated, independently-motivated administrative action) collapses and the correct reading becomes instrument_capture_reading, with employer associations reclassified from incidental beneficiary to agenda-setting principal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_coincidence, empirical, 'Whether the three agency/lobby actions were structurally independent or informally coordinated.').

omega_variable(
    measurability_kernel_framing,
    'Is ''adverse effect measurability'' better framed as a single ambiguous kernel with four competing readings (as structured here), or does the FLS-to-OEWS transition actually decompose into two separable claims — a measurement-methodology claim (was FLS defensibly discontinued?) and a distinct wage-effect claim (does OEWS produce a lower AEWR than FLS would have)?',
    'Parallel statistical analysis reconstructing counterfactual AEWR values under an FLS-continuation scenario versus actual OEWS-derived values, holding methodology claims separate from outcome claims.',
    'If the two claims are genuinely separable with different epistemic status (methodology claim well-supported, wage-effect claim empirically live), the four-reading kernel structure may itself be a simplification papering over an ε-invariance violation — the methodology question and the wage-effect question could have different extraction profiles requiring their own separate constraint stories rather than sibling readings of one kernel.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(measurability_kernel_framing, conceptual, 'Whether the kernel itself should be further decomposed rather than read four ways.').

omega_variable(
    diffuse_harm_measurement,
    'How large is the actual OEWS-vs-FLS wage-floor gap for H-2A occupations, and is it large enough that ''diffuse, incidental'' is an accurate description rather than a euphemism for a measurable and substantial wage suppression?',
    'Comparative labor economics study matching OEWS occupational codes against historical FLS regional/crop-specific wage data where both existed concurrently, to quantify the AEWR gap directly attributable to instrument substitution.',
    'A large, consistent, one-directional gap would undermine this reading''s ''incidental byproduct'' characterization even without evidence of coordination — systematic one-directional error is harder to characterize as pure accident regardless of intent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diffuse_harm_measurement, empirical, 'Whether the magnitude and directionality of wage-floor drift is consistent with genuine incidental byproduct.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bureaucratic_drift_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bure_tr_t0, bureaucratic_drift_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(bure_tr_t4, bureaucratic_drift_reading, theater_ratio, 4, 0.2).
narrative_ontology:measurement(bure_tr_t8, bureaucratic_drift_reading, theater_ratio, 8, 0.25).
narrative_ontology:measurement(bure_tr_t12, bureaucratic_drift_reading, theater_ratio, 12, 0.28).
narrative_ontology:measurement(bure_tr_t16, bureaucratic_drift_reading, theater_ratio, 16, 0.3).
narrative_ontology:measurement(bure_tr_t20, bureaucratic_drift_reading, theater_ratio, 20, 0.32).

% Extraction over time
narrative_ontology:measurement(bure_be_t0, bureaucratic_drift_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(bure_be_t4, bureaucratic_drift_reading, base_extractiveness, 4, 0.2).
narrative_ontology:measurement(bure_be_t8, bureaucratic_drift_reading, base_extractiveness, 8, 0.24).
narrative_ontology:measurement(bure_be_t12, bureaucratic_drift_reading, base_extractiveness, 12, 0.26).
narrative_ontology:measurement(bure_be_t16, bureaucratic_drift_reading, base_extractiveness, 16, 0.27).
narrative_ontology:measurement(bure_be_t20, bureaucratic_drift_reading, base_extractiveness, 20, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(bureaucratic_drift_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bureaucratic_drift_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(bureaucratic_drift_reading, 0.15).
narrative_ontology:affects_constraint(bureaucratic_drift_reading, instrument_capture_reading).
narrative_ontology:affects_constraint(bureaucratic_drift_reading, hold_up_efficiency_reading).
narrative_ontology:affects_constraint(bureaucratic_drift_reading, coverage_neutral_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling readings of the adverse_effect_measurability kernel, all sharing the same underlying facts (FLS discontinuation, OEWS substitution, employer lobbying, H-2A wage effects) but attributing different mechanisms and normative weight. bureaucratic_drift_reading holds ε low (0.28) because it denies a coordinating mechanism; instrument_capture_reading would hold ε substantially higher for the identical fact pattern because it asserts employer-agency coordination; hold_up_efficiency_reading would treat the transition as efficient renegotiation following an exogenous shock; coverage_neutral_reading would hold that OEWS substitution has no directional wage effect at all. Each is authored as its own constraint file with its own ε, per the ε-invariance principle — this file does not average across them or hedge between them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
