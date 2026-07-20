% ============================================================================
% CONSTRAINT STORY: licensing_statute_mandate__graduated_access_filter
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_licensing_statute_mandate__graduated_access_filter, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: licensing_statute_mandate__graduated_access_filter
 *   human_readable: Statutory Licensing as Graduated Class Access Filter
 *   domain: labor_economics/regulatory_policy/public_administration
 *
 * SUMMARY:
 *   This constraint is the graduated_access_filter reading of the
 *   licensing_statute_mandate kernel. It treats statutory occupational
 *   licensing not as a public safety mechanism nor as mere incumbent rent
 *   extraction, but as a structural sorting device that filters market access
 *   by pre-existing class position and access to credentialing resources. The
 *   credential requirement is formally neutralâpass the exam, pay the
 *   feeâbut differentially accessible by class, producing tiered labor
 *   markets that reproduce inequality.
 *
 * KEY AGENTS:
 *   - incumbent_credentialed_practitioners: Primary beneficiary (organized/constrained) â captures economic rents via restricted supply
 *   - marginalized_workers: Primary target (powerless/trapped) â structurally excluded from credential acquisition
 *   - resource_poor_aspirants: Primary target (powerless/trapped) â lacks capital for statutory pathway costs
 *   - immigrant_workers_foreign_credentials: Primary target (powerless/trapped) â prior competence rendered invisible by non-recognition
 *   - state_licensing_boards: Agenda setter (institutional/constrained) â administers the statutory exclusion regime
 *   - civil_rights_organizations: Analytical observer (organized/analytical) â documents disparate impact and advocates reform
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(licensing_statute_mandate__graduated_access_filter, 0.78).
domain_priors:suppression_score(licensing_statute_mandate__graduated_access_filter, 0.72).
domain_priors:theater_ratio(licensing_statute_mandate__graduated_access_filter, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, extractiveness, 0.78).
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(licensing_statute_mandate__graduated_access_filter, snare).
narrative_ontology:human_readable(licensing_statute_mandate__graduated_access_filter, "Statutory Licensing as Graduated Class Access Filter").
narrative_ontology:topic_domain(licensing_statute_mandate__graduated_access_filter, "labor_economics/regulatory_policy/public_administration").

domain_priors:requires_active_enforcement(licensing_statute_mandate__graduated_access_filter).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(licensing_statute_mandate__graduated_access_filter, '9c0b3f2d-b335-4c01-a3fb-c6b4bc15c24c').
narrative_ontology:cs_kernel_codification('9c0b3f2d-b335-4c01-a3fb-c6b4bc15c24c', formalized).
narrative_ontology:cs_authority_grounding('9c0b3f2d-b335-4c01-a3fb-c6b4bc15c24c', lineage).
narrative_ontology:cs_interpretation_layer_present('9c0b3f2d-b335-4c01-a3fb-c6b4bc15c24c').
narrative_ontology:cs_reading_relation('9c0b3f2d-b335-4c01-a3fb-c6b4bc15c24c', licensing_statute_mandate__public_safety_coordination, coexists_with).
narrative_ontology:cs_reading_relation('9c0b3f2d-b335-4c01-a3fb-c6b4bc15c24c', licensing_statute_mandate__rent_seeking_suppression, coexists_with).
narrative_ontology:cs_axiom('9c0b3f2d-b335-4c01-a3fb-c6b4bc15c24c', foundational, licensing_functions_as_class_filter).
narrative_ontology:cs_axiom_status(licensing_functions_as_class_filter, holdable).
narrative_ontology:cs_axiom_grounding('9c0b3f2d-b335-4c01-a3fb-c6b4bc15c24c', licensing_functions_as_class_filter, empirically_contingent).
narrative_ontology:cs_axiom('9c0b3f2d-b335-4c01-a3fb-c6b4bc15c24c', foundational, safety_rhetoric_obscures_structural_exclusion).
narrative_ontology:cs_axiom_status(safety_rhetoric_obscures_structural_exclusion, holdable).
narrative_ontology:cs_axiom_grounding('9c0b3f2d-b335-4c01-a3fb-c6b4bc15c24c', safety_rhetoric_obscures_structural_exclusion, empirically_contingent).
narrative_ontology:cs_reference_frame('9c0b3f2d-b335-4c01-a3fb-c6b4bc15c24c', statutory_competence_verification_framework).
narrative_ontology:cs_drift_state('9c0b3f2d-b335-4c01-a3fb-c6b4bc15c24c', contemporary_labor_market_inequality_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9c0b3f2d-b335-4c01-a3fb-c6b4bc15c24c', '').
narrative_ontology:cs_kernel_id(licensing_statute_mandate__graduated_access_filter, licensing_statute_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__graduated_access_filter, incumbent_credentialed_practitioners).
narrative_ontology:constraint_victim(licensing_statute_mandate__graduated_access_filter, marginalized_workers).
narrative_ontology:constraint_victim(licensing_statute_mandate__graduated_access_filter, resource_poor_aspirants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(licensing_statute_mandate__graduated_access_filter, immigrant_workers_foreign_credentials).
narrative_ontology:constraint_vindicates(licensing_statute_mandate__graduated_access_filter, meritocratic_sorting_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Have already invested in statutory credentials and benefit from restricted labor supply that sustains above-equilibrium wages and professional status. They support maintaining or raising barriers to entry through association lobbying and legislative testimony. Exit from the constraint means accepting wage compression and status dilution from deregulated entry.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, incumbent_credentialed_practitioners, beneficiary,
    organized, biographical, constrained, national).

% Established by statute to verify competence and issue licenses. They set education and examination requirements, evaluate applicants, and discipline violators. Their authority derives from the legislative text and administrative delegation; they are structurally insulated from excluded populations and responsive to incumbent professional associations.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, state_licensing_boards, agenda_setter,
    institutional, generational, constrained, national).

% Perform competent work in informal or adjacent sectors but are barred from formal market access by credential requirements they cannot afford in time, money, or social capital. They experience the constraint as a hard ceiling on occupational mobility with no legitimate bypass.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, marginalized_workers, payer,
    powerless, immediate, trapped, local).

% Attempt to enter licensed occupations but lack the capital for tuition, exam fees, application costs, and foregone wages during statutory training periods. The barrier is absolute at their resource level, filtering them out before competence can be demonstrated.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, resource_poor_aspirants, payer,
    powerless, biographical, trapped, national).

% Hold equivalent foreign credentials and documented experience but face non-recognition, redundant retraining mandates, and language-testing barriers. Their prior competence is structurally invisible to the licensing regime, forcing them into lower-wage unlicensed work.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, immigrant_workers_foreign_credentials, payer,
    powerless, biographical, trapped, national).

% Document disparate impact of licensing requirements by race, class, and national origin. They file litigation, publish impact studies, and advocate for alternative pathways. They neither pay the constraint's costs nor benefit from its operation.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, civil_rights_organizations, observer,
    organized, generational, analytical, national).

% Offer competency-based or apprenticeship training outside the statutory framework. They are legally barred from placing graduates into licensed practice and are excluded from rulemaking processes that determine allowable pathways.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, alternative_training_providers, excluded,
    moderate, biographical, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(licensing_statute_mandate__graduated_access_filter, incumbent_credentialed_practitioners).
narrative_ontology:fixing_cost_class(licensing_statute_mandate__graduated_access_filter, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Public safety and consumer protection through standardized minimum-competence verification, presented as solving information asymmetry between service providers and consumers. In this reading the function is cover for structural exclusion.
% TRANSFER_FUNCTION: Moves economic rents from excluded and marginalized workers to incumbent credentialed practitioners by restricting labor supply, and transfers regulatory control over market entry from workers to state licensing boards.
% ABSENT_VOICES: Unlicensed but competent practitioners, alternative training providers offering non-statutory pathways, and marginalized workers who would enter if barriers were lower are structurally excluded from policy formation; their absence is necessary for the constraint's persistence.
% DISAPPEARANCE_RATIONALE: If licensing requirements vanished, incumbent practitioners would face immediate wage compression from expanded supply, marginalized workers and immigrants would enter previously gated occupations, alternative training pathways would proliferate, and the class composition of licensed professions would reorganize substantially.
% FOUNDING_PROBLEM: Consumer protection against unqualified or dangerous practitioners in complex technical or health-related occupations.
% FOUNDING_PROBLEM_CORROBORATION: Labor economists and civil rights organizations outside the credentialed class attest that less restrictive alternatives achieve equivalent safety outcomes and that the constraint functions primarily as a class filter; licensing boards and professional associations attest the problem is live. Corroboration from outside the beneficiary set disputes the live status.
narrative_ontology:disappearance_verdict(licensing_statute_mandate__graduated_access_filter, world_rearranges).
narrative_ontology:founding_problem_status(licensing_statute_mandate__graduated_access_filter, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(licensing_statute_mandate__graduated_access_filter, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(licensing_statute_mandate__graduated_access_filter, 'none', 1).
narrative_ontology:epsilon_provenance(licensing_statute_mandate__graduated_access_filter, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(licensing_statute_mandate__graduated_access_filter_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(licensing_statute_mandate__graduated_access_filter, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(licensing_statute_mandate__graduated_access_filter_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the constraint restricts labor supply to benefit incumbents while imposing costs on excluded workers far exceeding any demonstrated safety benefit. Suppression (0.72) reflects active enforcementâstatutory bars, disciplinary machinery, and exclusion of alternative pathwaysâneeded to maintain the filter. Theater ratio (0.45) indicates that nearly half of regulatory activity is performative maintenance of a public safety rationale that does not track actual risk reduction. Accessibility collapse (0.68) is high because once the licensing framework is accepted, alternatives like apprenticeship or foreign credential recognition collapse politically. Resistance (0.55) is moderate: excluded groups and reform economists mount steady but institutionally outgunned opposition.
 *
 * PERSPECTIVAL GAP:
 *   The incumbent seat experiences the constraint as legitimate professional gatekeeping that protects investment and standards; the marginalized worker seat experiences the same statute as an absolute barrier to economic mobility. The engine computes this divergence from structural dataâbeneficiary declarations, trapped exit, and power asymmetryâwithout requiring the claim to adjudicate the dispute. The agenda-setter seat (licensing boards) is constrained by statute but insulated from the costs of exclusion, producing a third distinct computed classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent credentialed practitioners are declared beneficiaries with constrained exit (sunk credential investment), placing their directionality near the subsidy end. Marginalized workers and resource-poor aspirants are declared victims with trapped exit, placing their directionality near the full-target end. State licensing boards are agenda-setters outside the beneficiary/victim arrays; their directionality falls to the institutional canonical fallback, reflecting neutral administration rather than rent capture. The effective extraction is therefore heavily amplified for the powerless victim seats and damped or inverted for the organized beneficiary seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling this constraint as a rope or scaffold by requiring demonstrable coordination function with net beneficiary status for all parties. Here the coordination story (public safety) is cover: victims are structurally present, enforcement is active, and the constraint's persistence depends on suppressing alternatives rather than solving a live collective-action problem. The metrics and structural declarations are authored independently of the claim; any divergence between the authored snare claim and engine computation is the signal the corpus exists to capture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    safety_outcome_equivalence,
    'Do statutory licensing requirements produce measurably better consumer safety outcomes than less restrictive alternatives such as certification or registration?',
    'Comparative policy analysis across jurisdictions with differing regulatory intensity, controlling for income, education, and baseline health and safety infrastructure.',
    'If safety outcomes are equivalent, the public_safety_coordination reading loses empirical support and this snare reading strengthens; if licensing demonstrably reduces harm, the kernel may shift toward public_safety_coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(safety_outcome_equivalence, empirical, 'Empirical test of the public safety justification against less restrictive alternatives.').

omega_variable(
    class_sorting_intentionality,
    'Is the differential barrier effect by class and resource access an intentional design feature of licensing statutes or an emergent side effect of competence-testing logic?',
    'Historical legislative record analysis, sponsor testimony, and regulatory impact assessments tracing the evolution of credential requirements.',
    'If intentional, the constraint is a deliberate snare; if emergent, it may be a tangled_rope where coordination and extraction coexist without design. This affects classification and remediation strategy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(class_sorting_intentionality, conceptual, 'Whether class sorting is by design or emergent side effect.').

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading of the licensing_statute_mandate kernel; how do the graduated_access_filter, public_safety_coordination, and rent_seeking_suppression readings differ structurally?',
    'Empirical safety data, wage-premium econometrics, and legislative history reviewed against each reading''s axioms.',
    'Determines whether the kernel is a genuine ambiguity (three coexisting readings) or whether evidence forecloses one reading in favor of another.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Structural contest between sibling readings of the licensing kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(licensing_statute_mandate__graduated_access_filter, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(licensing_graduated_access_tr_t0, licensing_statute_mandate__graduated_access_filter, theater_ratio, 0, 0.22).
narrative_ontology:measurement(licensing_graduated_access_tr_t10, licensing_statute_mandate__graduated_access_filter, theater_ratio, 10, 0.3).
narrative_ontology:measurement(licensing_graduated_access_tr_t20, licensing_statute_mandate__graduated_access_filter, theater_ratio, 20, 0.36).
narrative_ontology:measurement(licensing_graduated_access_tr_t30, licensing_statute_mandate__graduated_access_filter, theater_ratio, 30, 0.4).
narrative_ontology:measurement(licensing_graduated_access_tr_t40, licensing_statute_mandate__graduated_access_filter, theater_ratio, 40, 0.43).
narrative_ontology:measurement(licensing_graduated_access_tr_t50, licensing_statute_mandate__graduated_access_filter, theater_ratio, 50, 0.45).

% Extraction over time
narrative_ontology:measurement(licensing_graduated_access_be_t0, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(licensing_graduated_access_be_t10, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 10, 0.51).
narrative_ontology:measurement(licensing_graduated_access_be_t20, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(licensing_graduated_access_be_t30, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(licensing_graduated_access_be_t40, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 40, 0.74).
narrative_ontology:measurement(licensing_graduated_access_be_t50, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 50, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(licensing_graduated_access_su_t0, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(licensing_graduated_access_su_t10, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(licensing_graduated_access_su_t20, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(licensing_graduated_access_su_t30, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 30, 0.67).
narrative_ontology:measurement(licensing_graduated_access_su_t40, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(licensing_graduated_access_su_t50, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 50, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(licensing_statute_mandate__graduated_access_filter, licensing_statute_mandate__public_safety_coordination).
narrative_ontology:affects_constraint(licensing_statute_mandate__graduated_access_filter, licensing_statute_mandate__rent_seeking_suppression).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the licensing_statute_mandate kernel. The three readings decompose the colloquial label 'occupational licensing' into structurally distinct claims: public_safety_coordination (coordination function), rent_seeking_suppression (incumbent rent extraction), and graduated_access_filter (class-based exclusion). Each reading has distinct epsilon, stakeholders, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
