% ============================================================================
% CONSTRAINT STORY: software_source_status__pragmatic_development_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_source_status__pragmatic_development_reading, []).

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
 *   constraint_id: software_source_status__pragmatic_development_reading
 *   human_readable: Pragmatic Open Source Development Methodology
 *   domain: software_engineering/political_economy
 *
 * SUMMARY:
 *   This constraint is one reading of the contested kernel
 *   software_source_status, which concerns the normative and practical status
 *   of software source code openness. The pragmatic_development_reading holds
 *   that open source is a superior development methodology because peer
 *   review, distributed bug detection, and innovation velocity produce
 *   higher-quality software than proprietary alternatives; software freedom
 *   is valued instrumentally rather than as an ethical imperative, and
 *   proprietary software is not treated as inherently illegitimate. This
 *   reading shapes behavior through social norms, licensing choices, and
 *   institutional funding preferences rather than through coercion.
 *
 * KEY AGENTS:
 *   - individual_developers (moderate/mobile): contribute labor in exchange for peer review and reputation; net beneficiaries of coordination
 *   - corporate_adopters (institutional/arbitrage): consume open infrastructure with minimal licensing friction; primary beneficiaries of cost avoidance
 *   - open_source_foundations (organized/mobile): maintain methodological norms and license definitions; agenda setters without rent extraction
 *   - proprietary_developers (powerful/mobile): compete with open alternatives; analytical observers in this reading's framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_source_status__pragmatic_development_reading, 0.35).
domain_priors:suppression_score(software_source_status__pragmatic_development_reading, 0.2).
domain_priors:theater_ratio(software_source_status__pragmatic_development_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_source_status__pragmatic_development_reading, rope).
narrative_ontology:human_readable(software_source_status__pragmatic_development_reading, "Pragmatic Open Source Development Methodology").
narrative_ontology:topic_domain(software_source_status__pragmatic_development_reading, "software_engineering/political_economy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_source_status__pragmatic_development_reading, 'c2ee440e-32c2-4880-83ae-75b200c55ea0').
narrative_ontology:cs_kernel_codification('c2ee440e-32c2-4880-83ae-75b200c55ea0', distributed).
narrative_ontology:cs_authority_grounding('c2ee440e-32c2-4880-83ae-75b200c55ea0', practice).
narrative_ontology:cs_interpretation_layer_present('c2ee440e-32c2-4880-83ae-75b200c55ea0').
narrative_ontology:cs_reading_relation('c2ee440e-32c2-4880-83ae-75b200c55ea0', software_source_status__freedom_imperative_reading, forecloses).
narrative_ontology:cs_reading_relation('c2ee440e-32c2-4880-83ae-75b200c55ea0', software_source_status__property_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('c2ee440e-32c2-4880-83ae-75b200c55ea0', software_source_status__utilitarian_hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('c2ee440e-32c2-4880-83ae-75b200c55ea0', foundational, freedom_instrumental_to_quality).
narrative_ontology:cs_axiom_status(freedom_instrumental_to_quality, holdable).
narrative_ontology:cs_axiom_grounding('c2ee440e-32c2-4880-83ae-75b200c55ea0', freedom_instrumental_to_quality, empirically_contingent).
narrative_ontology:cs_axiom('c2ee440e-32c2-4880-83ae-75b200c55ea0', foundational, proprietary_development_legitimate).
narrative_ontology:cs_axiom_status(proprietary_development_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('c2ee440e-32c2-4880-83ae-75b200c55ea0', proprietary_development_legitimate, conventional).
narrative_ontology:cs_reference_frame('c2ee440e-32c2-4880-83ae-75b200c55ea0', instrumental_open_collaboration).
narrative_ontology:cs_drift_state('c2ee440e-32c2-4880-83ae-75b200c55ea0', contemporary_platform_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c2ee440e-32c2-4880-83ae-75b200c55ea0', '').
narrative_ontology:cs_kernel_id(software_source_status__pragmatic_development_reading, software_source_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_source_status__pragmatic_development_reading, individual_developers).
narrative_ontology:constraint_beneficiary(software_source_status__pragmatic_development_reading, corporate_adopters).
narrative_ontology:constraint_beneficiary(software_source_status__pragmatic_development_reading, open_source_foundations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Writes and shares code under permissive licenses, receives peer review and bug reports from distributed collaborators, and builds public reputation. May alternatively pursue proprietary employment without moral stigma under this reading.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, individual_developers, beneficiary,
    moderate, biographical, mobile, global).

% Integrates open source components into commercial products and services, paying no licensing fees and gaining broad interoperability. Contribution back to upstream projects is variable and often minimal.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, corporate_adopters, beneficiary,
    institutional, biographical, arbitrage, global).

% Maintains definitional standards for open source licenses and promotes the methodology through advocacy and education. Survives on donations and membership dues rather than rents from the licensing framework.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, open_source_foundations, agenda_setter,
    organized, generational, mobile, global).

% Develops closed-source software for sale or internal use. Experiences market pressure from zero-cost open alternatives but retains full legitimacy under this reading's norms.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, proprietary_developers, observer,
    powerful, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinate distributed software development across organizational boundaries so that source code is inspectable, bugs are found by many eyes, and improvements accumulate in a shared commons rather than being duplicated in silos.
% TRANSFER_FUNCTION: Moves volunteer and salaried labor into publicly inspectable codebases, and moves working software, defect reports, and reputational credibility back to contributors and downstream users.
% ABSENT_VOICES: Maintainers experiencing burnout from unpaid labor are often absent from governance conversations; proprietary developers who believe closed funding models enable software sustainability are present in industry but excluded from open source norm-setting institutions.
% DISAPPEARANCE_RATIONALE: Software development would reorganize around proprietary R&D pipelines; the commons of inspectable infrastructure would fragment, and the peer-review velocity this reading depends on would collapse.
% FOUNDING_PROBLEM: Software developed in closed silos suffered from hidden defects, redundant reinvention of similar tools, and vendor lock-in that trapped users; coordinating distributed expertise without a central owner was difficult.
% FOUNDING_PROBLEM_CORROBORATION: Empirical software engineering research from academic institutions outside the open source beneficiary set corroborates that distributed review improves defect detection in many domains; proprietary vendors contest that the model scales to all software categories.
narrative_ontology:disappearance_verdict(software_source_status__pragmatic_development_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_source_status__pragmatic_development_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_source_status__pragmatic_development_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(software_source_status__pragmatic_development_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_source_status__pragmatic_development_reading, 0.35, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_source_status__pragmatic_development_reading_tests).
:- end_tests(software_source_status__pragmatic_development_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Metrics are authored descriptively: extractiveness is moderate-low (0.35) because corporate free-riding creates asymmetric benefit without corresponding contribution, but the coordination function (peer review, shared infrastructure) remains genuine. Suppression is low (0.20) because this reading explicitly permits proprietary alternatives and does not delegitimize closed development. Theater ratio is low-moderate (0.25) reflecting some performative open-washing. The claim of rope is structurally grounded in the absence of active enforcement, the presence of net beneficiaries, and the non-suppression of alternatives; the metrics are left independent so the engine can detect divergence if the coordination has degraded. Measurements share a single time grid.
 *
 * PERSPECTIVAL GAP:
 *   A freedom_imperative_reading of the same kernel would compute a different seat for proprietary developers (payer/victim) and would likely show high suppression due to its delegitimization of proprietary software. From the pragmatic reading's seat, the absence of coercion and the instrumental framing keep directionality near the beneficiary end for all participants except potentially uncompensated maintainers, whose burnout risk is captured in the omega variables rather than the base victim set.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual developers and corporate adopters are structural beneficiaries: the former receive quality improvements and reputational returns, the latter receive zero-cost infrastructure. Neither is a target of extraction in the authored structure; the cost of contribution is voluntarily borne for instrumental gain. Proprietary developers are not victims because the reading does not suppress their model. Open source foundations set the agenda without capturing rents.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâclosed development producing hidden bugs and redundant effortâremains live, corroborated by empirical software engineering. The arrangement has not outlived its function, so mandatrophy does not apply. However, the drift toward corporate capture (documented in measurements and omegas) represents a potential future mandatrophy if coordination becomes entirely theatrical.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pragmatic_reading_kernel_position,
    'This constraint instantiates the pragmatic_development_reading of kernel software_source_status, holding that freedom is instrumental and proprietary development is legitimate. How does this structural commitment differ from the freedom_imperative_reading''s claim that proprietary software is an injustice?',
    'Normative analysis of the sibling readings'' axioms: this reading''s empirically contingent axiom (freedom_instrumental_to_quality) vs. the freedom_imperative reading''s deontological axiom (software_freedom_categorical).',
    'If the pragmatic reading is adopted, proprietary developers are observers rather than victims, and the constraint lacks the suppression structure typical of the freedom_imperative reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(pragmatic_reading_kernel_position, conceptual, 'Structural position of this reading within the software source status kernel').

omega_variable(
    corporate_contribution_asymmetry,
    'Do corporate adopters of open source contribute labor and capital back to the commons in proportion to the value they extract, or does the arrangement enable systematic free-riding?',
    'Measure the ratio of corporate downstream revenue attributable to open source infrastructure against upstream contributions (code, maintainers, funding).',
    'Persistent asymmetry would reclassify the constraint from rope to tangled_rope by identifying contributors as payers despite the coordination benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(corporate_contribution_asymmetry, empirical, 'Corporate extraction versus contribution balance').

omega_variable(
    methodology_theatricality,
    'To what extent has corporate open source adoption become performativeâreleasing minor code or rebranding proprietary services as ''open''ârather than genuinely participating in the peer-review coordination this reading values?',
    'Audit of corporate open source releases for actual external contribution volume, issue responsiveness, and governance transparency.',
    'High theatricality would raise theater_ratio and signal piton or snare dynamics rather than genuine rope coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(methodology_theatricality, empirical, 'Performative versus genuine corporate open source engagement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_source_status__pragmatic_development_reading, 0, 26).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, software_source_status__pragmatic_development_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(soft_tr_t5, software_source_status__pragmatic_development_reading, theater_ratio, 5, 0.11).
narrative_ontology:measurement(soft_tr_t10, software_source_status__pragmatic_development_reading, theater_ratio, 10, 0.14).
narrative_ontology:measurement(soft_tr_t15, software_source_status__pragmatic_development_reading, theater_ratio, 15, 0.18).
narrative_ontology:measurement(soft_tr_t20, software_source_status__pragmatic_development_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(soft_tr_t26, software_source_status__pragmatic_development_reading, theater_ratio, 26, 0.25).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, software_source_status__pragmatic_development_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(soft_be_t5, software_source_status__pragmatic_development_reading, base_extractiveness, 5, 0.18).
narrative_ontology:measurement(soft_be_t10, software_source_status__pragmatic_development_reading, base_extractiveness, 10, 0.22).
narrative_ontology:measurement(soft_be_t15, software_source_status__pragmatic_development_reading, base_extractiveness, 15, 0.27).
narrative_ontology:measurement(soft_be_t20, software_source_status__pragmatic_development_reading, base_extractiveness, 20, 0.31).
narrative_ontology:measurement(soft_be_t26, software_source_status__pragmatic_development_reading, base_extractiveness, 26, 0.35).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(software_source_status__pragmatic_development_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(software_source_status__pragmatic_development_reading, freedom_imperative_reading).
narrative_ontology:affects_constraint(software_source_status__pragmatic_development_reading, property_rights_reading).
narrative_ontology:affects_constraint(software_source_status__pragmatic_development_reading, utilitarian_hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is the pragmatic_development_reading of the software_source_status kernel; siblings represent alternative readings of the same contested kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
