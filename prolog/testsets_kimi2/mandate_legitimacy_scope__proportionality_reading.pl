% ============================================================================
% CONSTRAINT STORY: mandate_legitimacy_scope__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: Proportionality-Limited Vaccine Mandate Legitimacy
 *   domain: public_health_ethics/constitutional_law/medical_autonomy
 *
 * SUMMARY:
 *   This constraint is one reading of the contested kernel
 *   mandate_legitimacy_scope. The proportionality reading holds that state
 *   coercion of medical intervention is legitimate only when the targeted
 *   disease is severe, the vaccine is safe and effective, and less
 *   restrictive alternatives are infeasible. Under this reading, a measles
 *   mandate clears the threshold while a seasonal flu mandate does not. The
 *   constraint therefore extracts bodily autonomy conditionallyâmoderate
 *   epsilon that varies by pathogenâwhile coordinating a boundary between
 *   necessary public health action and impermissible overreach. The claim is
 *   tangled_rope: the framework genuinely sorts legitimate from illegitimate
 *   mandates (coordination), but when it authorizes compulsion it extracts
 *   from mandate targets (asymmetric extraction), and its persistence depends
 *   on active legal and institutional enforcement.
 *
 * KEY AGENTS:
 *   - public_health_authorities: Agenda-setter (institutional/constrained) â defines proportionality criteria and enforces mandates
 *   - general_public: Primary beneficiary (organized/constrained) â receives population-level health protection
 *   - mandate_targets: Primary payer (moderate/constrained) â bear bodily autonomy cost of compelled intervention
 *   - judicial_review_bodies: Analytical observer (institutional/analytical) â adjudicate proportionality without bearing direct costs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mandate_legitimacy_scope__proportionality_reading, 0.48).
domain_priors:suppression_score(mandate_legitimacy_scope__proportionality_reading, 0.6).
domain_priors:theater_ratio(mandate_legitimacy_scope__proportionality_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mandate_legitimacy_scope__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(mandate_legitimacy_scope__proportionality_reading, "Proportionality-Limited Vaccine Mandate Legitimacy").
narrative_ontology:topic_domain(mandate_legitimacy_scope__proportionality_reading, "public_health_ethics/constitutional_law/medical_autonomy").

domain_priors:requires_active_enforcement(mandate_legitimacy_scope__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(mandate_legitimacy_scope__proportionality_reading, '9b1f5a1d-41fd-4d6c-aa88-a400b9ef52c3').
narrative_ontology:cs_kernel_codification('9b1f5a1d-41fd-4d6c-aa88-a400b9ef52c3', formalized).
narrative_ontology:cs_authority_grounding('9b1f5a1d-41fd-4d6c-aa88-a400b9ef52c3', lineage).
narrative_ontology:cs_interpretation_layer_present('9b1f5a1d-41fd-4d6c-aa88-a400b9ef52c3').
narrative_ontology:cs_reading_relation('9b1f5a1d-41fd-4d6c-aa88-a400b9ef52c3', mandate_legitimacy_scope__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('9b1f5a1d-41fd-4d6c-aa88-a400b9ef52c3', mandate_legitimacy_scope__public_health_primary, coexists_with).
narrative_ontology:cs_axiom('9b1f5a1d-41fd-4d6c-aa88-a400b9ef52c3', foundational, proportionality_governs_mandate_legitimacy).
narrative_ontology:cs_axiom_status(proportionality_governs_mandate_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('9b1f5a1d-41fd-4d6c-aa88-a400b9ef52c3', proportionality_governs_mandate_legitimacy, conventional).
narrative_ontology:cs_axiom('9b1f5a1d-41fd-4d6c-aa88-a400b9ef52c3', foundational, least_restrictive_means_required).
narrative_ontology:cs_axiom_status(least_restrictive_means_required, holdable).
narrative_ontology:cs_axiom_grounding('9b1f5a1d-41fd-4d6c-aa88-a400b9ef52c3', least_restrictive_means_required, conventional).
narrative_ontology:cs_reference_frame('9b1f5a1d-41fd-4d6c-aa88-a400b9ef52c3', proportionality_constrained_public_health_authority).
narrative_ontology:cs_drift_state('9b1f5a1d-41fd-4d6c-aa88-a400b9ef52c3', post_pandemic_legal_environment, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9b1f5a1d-41fd-4d6c-aa88-a400b9ef52c3', '').
narrative_ontology:cs_kernel_id(mandate_legitimacy_scope__proportionality_reading, mandate_legitimacy_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__proportionality_reading, general_public).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__proportionality_reading, mandate_targets).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define the epidemiological criteria for disease severity, evaluate vaccine safety and efficacy profiles, and apply proportionality tests to mandate policies. Enforce compliance through legal penalties when proportionality is satisfied. Justify compulsion by citing population-level risk reduction and the exhaustion of less restrictive alternatives.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Receives reduced disease transmission and herd-protection benefits when proportionate mandates are activated. Does not administer the constraint but benefits from its sorting function, which distinguishes legitimate public health coercion from overreach. Exit is constrained by residence, employment, and citizenship ties.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, general_public, beneficiary,
    organized, biographical, constrained, national).

% Bear the direct cost of compelled medical interventionâbodily autonomy infringement, potential adverse effects, and compliance burdenâwhenever a mandate clears proportionality review. May challenge proportionality in court, but face school exclusion, employment termination, or fines during the challenge. Exit options are limited by economic and geographic barriers.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, mandate_targets, payer,
    moderate, biographical, constrained, national).

% Adjudicate whether specific mandates meet proportionality requirements by reviewing scientific evidence on disease severity, vaccine profiles, and the availability of less restrictive measures. Their rulings determine whether extraction proceeds under the constraint's authorization. They neither collect health benefits nor bear the bodily costs of compulsion.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, judicial_review_bodies, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Sorts legitimate from illegitimate public health mandates by requiring proof of severe disease threat, safe and effective intervention, and exhaustion of less restrictive alternatives before state coercion is authorized.
% TRANSFER_FUNCTION: Moves bodily autonomy and compliance burden from mandate targets to the state-enforced public health system when proportionality conditions are met; moves disease-risk reduction to the general public.
% ABSENT_VOICES: Absolutist bodily-autonomy advocates who reject all compelled medical intervention regardless of proportionality, and public-health maximalists who reject proportionality limits on state emergency power, are both structurally excluded from the central proportionality discourse.
% DISAPPEARANCE_RATIONALE: If the proportionality requirement vanished overnight, states would lack a structured legitimacy test for mandatesâlikely collapsing into either unconstrained mandate authority or categorical prohibitionâradically rearranging the boundary between individual bodily integrity and collective health.
% FOUNDING_PROBLEM: How to authorize necessary public health coercion during epidemic crises without licensing unlimited state infringement of bodily integrity and personal autonomy.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional courts and international human rights tribunals attest to the need for proportionality review from outside the public health beneficiary seat; historical records of non-consensual medical experimentation and forced sterilization corroborate the founding problem from seats that do not benefit from mandate authority.
narrative_ontology:disappearance_verdict(mandate_legitimacy_scope__proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(mandate_legitimacy_scope__proportionality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(mandate_legitimacy_scope__proportionality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(mandate_legitimacy_scope__proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(mandate_legitimacy_scope__proportionality_reading, 0.48, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is authored at 0.48 (moderate) because the proportionality framework authorizes bodily compulsion only under limited conditions; it is not permanently on but is substantially extractive when activated. Suppression is 0.60 because the constraint's operation requires legal penalties and institutional enforcement to secure compliance. Theater_ratio at 0.35 reflects that a growing share of proportionality review is performativeâcourts deferring to public health declarations without rigorous least-restrictive-means analysis. Accessibility_collapse at 0.65 captures that, once proportionality is accepted as the governing framework, the alternative of absolute bodily autonomy becomes marginalized in legal discourse. Resistance at 0.50 reflects ongoing litigation and political contestation from both autonomy absolutists and public health maximalists.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (public health authorities) and the beneficiary seat (general public) experience this constraint as a legitimate boundary on state power that prevents overreach while protecting the vulnerable. The payer seat (mandate targets) experiences the same structure as conditional but real bodily compulsion: even when proportionality is satisfied, they bear the autonomy cost. The engine will compute these seats differently because the structural data maps low directionality to beneficiaries and high directionality to victims, while the agenda_setter sits near the beneficiary end due to its control over activation conditions.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declaration (general_public) drives low directionality toward the beneficiary end: the public receives the coordination good (disease risk reduction) without paying the autonomy cost. Victim declaration (mandate_targets) drives high directionality: they bear the compelled intervention cost and have only constrained exit. Public health authorities, as agenda_setter, control the proportionality assessment and enforcement trigger, placing them structurally near the beneficiary end despite not being named in beneficiaries. Judicial observers are analytically detached with neutral directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents mislabeling this constraint as a pure rope (which would ignore the bodily autonomy extraction that occurs whenever a mandate clears proportionality) or as a pure snare (which would ignore the genuine coordination function of blocking disproportionate mandates). The proportionality framework was built to solve a real founding problemâauthorizing necessary coercion without licensing unlimited state medical controlâand that problem remains live, so mandatrophy_resolved is not declared. If the framework were classified as a rope, the extraction of mandate targets would be hidden; if classified as a snare, the coordination benefit to public health boundaries would be denied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary,
    'Does the proportionality reading collapse into bodily_autonomy_primary when empirical evidence on vaccine safety or disease severity is unfavorable, or does it remain structurally distinct by preserving conditional state legitimacy?',
    'Comparative doctrinal analysis across jurisdictions with strong versus weak proportionality review to determine whether the framework stabilizes as an independent balancing test or reverts to autonomy absolutism under marginal empirical conditions.',
    'If proportionality consistently collapses to autonomy when evidence is weak, the reading functions as a transitional scaffold toward an autonomy-dominant regime; if it stabilizes independently, it is a durable tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Structural stability of proportionality as distinct from autonomy absolutism').

omega_variable(
    empirical_contingency_of_victim_set,
    'Is the victim set structurally determined by the proportionality framework itself, or by empirical misjudgments of disease severity and vaccine safety that the framework fails to filter?',
    'Epidemiological and legal case-study review comparing measles (high severity) and influenza (low severity) mandate histories to measure whether proportionality review reliably blocks extraction or merely redescribes it.',
    'If proportionality review consistently fails to prevent low-severity mandates, the constraint''s effective extraction is higher than its theory suggests; if it blocks them reliably, the conditional victim set is structurally real.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_contingency_of_victim_set, empirical, 'Whether proportionality review empirically prevents disproportionate extraction').

omega_variable(
    public_health_primary_pressure,
    'Does the proportionality reading create sufficient downstream pressure on public_health_primary to narrow mandate scope, or do the two readings functionally converge during declared emergencies?',
    'Analyze emergency mandate litigation to measure whether proportionality requirements are suspended, diluted, or enforced during declared public health emergencies.',
    'If proportionality is systematically suspended during emergencies, the constraint''s extraction spikes precisely when demand for public health coercion is highest, undermining its hybrid coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_health_primary_pressure, empirical, 'Downstream pressure of proportionality on public health maximalism under emergency conditions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mandate_legitimacy_scope__proportionality_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mand_tr_t0, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(mand_tr_t6, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 6, 0.22).
narrative_ontology:measurement(mand_tr_t12, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 12, 0.25).
narrative_ontology:measurement(mand_tr_t18, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 18, 0.3).
narrative_ontology:measurement(mand_tr_t24, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 24, 0.35).

% Extraction over time
narrative_ontology:measurement(mand_be_t0, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(mand_be_t6, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 6, 0.28).
narrative_ontology:measurement(mand_be_t12, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 12, 0.32).
narrative_ontology:measurement(mand_be_t18, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 18, 0.4).
narrative_ontology:measurement(mand_be_t24, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 24, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(mand_su_t0, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(mand_su_t6, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 6, 0.48).
narrative_ontology:measurement(mand_su_t12, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 12, 0.52).
narrative_ontology:measurement(mand_su_t18, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 18, 0.58).
narrative_ontology:measurement(mand_su_t24, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 24, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mandate_legitimacy_scope__proportionality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__proportionality_reading, public_health_primary).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__proportionality_reading, bodily_autonomy_primary).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the mandate_legitimacy_scope kernel, which decomposes into three structurally distinct constraints (proportionality_reading, public_health_primary, bodily_autonomy_primary) because the natural-language concept 'mandate legitimacy' conflates empirically conditional, absolutist-autonomy, and public-health-maximalist claims that have different epsilon profiles, victim sets, and classification outcomes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
