% ============================================================================
% CONSTRAINT STORY: bureaucratic_accommodation_as_extraction_persistence
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bureaucratic_accommodation_as_extraction_persistence, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: bureaucratic_accommodation_as_extraction_persistence
 *   human_readable: Bureaucratic Accommodation as Extraction Persistence
 *   domain: organizational_dynamics/labor_relations/institutional_power
 *
 * SUMMARY:
 *   The bureaucratic accommodation framework emerges in response to
 *   collective action pressure (upstream:
 *   collective_action_as_leverage_conversion) demanding resolution of
 *   architectural barriers (upstream:
 *   architectural_constraint_as_dual_substrate). The institution responds by
 *   creating a procedural pathway for individual accommodations rather than
 *   modifying the architectural substrate. This policy modification appears
 *   to resolve the conflict — workers now have a formal mechanism to request
 *   access — while maintaining structural inequality through approval
 *   stratification, procedural costs, and unchanged physical barriers. The
 *   framework exhibits genuine coordination function for some workers (those
 *   with high approval rates, low application costs, and organizational
 *   protection) while extracting compliance costs from surface-exposed
 *   workers who bear documentation burdens, medical disclosure requirements,
 *   waiting periods, and approval uncertainty. The theater ratio (0.78)
 *   reflects that the accommodation review process is substantially
 *   performative: approval decisions are often predetermined by budget
 *   constraints and managerial preferences, with formal review serving
 *   primarily as legal documentation. The extractiveness has increased over
 *   the interval (0.48 → 0.68) as procedural requirements have accumulated
 *   and approval stratification has widened, revealing the framework's drift
 *   from coordination toward extraction persistence.
 *
 * KEY AGENTS:
 *   - Surface-Exposed Workers: Primary victim (powerless/trapped) — economically dependent on positions requiring physical presence; bear maximum procedural extraction while architectural barriers remain unchanged
 *   - Accommodation Applicants: Secondary victim (moderate/constrained) — face application costs and career risk but receive genuine coordination benefit when accommodations are granted; mixed experience of extraction and coordination
 *   - Institutional Authority: Primary beneficiary (institutional/arbitrage) — gains liability shield, conflict resolution mechanism, and discretion over approval decisions; experiences framework as coordination protecting institutional prerogatives
 *   - Protected Worker Class: Secondary beneficiary (powerful/mobile) — workers not requiring site presence or with pre-existing accommodations; benefit from institutional legitimacy without bearing costs
 *   - Disability Rights Coalition: Organized agents (organized/constrained) — see framework as temporary compromise with universal design sunset; tolerate procedural extraction as transitional mechanism
 *   - Compliance Office: Institutional actor (institutional/arbitrage) — maintains performative review process; sees own function as degraded (piton perspective)
 *   - Analytical Observer: Structural view (analytical/analytical) — identifies irreducible hybrid of coordination and extraction; tangled rope classification reflects genuine dual function
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bureaucratic_accommodation_as_extraction_persistence, 0.68).
domain_priors:suppression_score(bureaucratic_accommodation_as_extraction_persistence, 0.72).
domain_priors:theater_ratio(bureaucratic_accommodation_as_extraction_persistence, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bureaucratic_accommodation_as_extraction_persistence, extractiveness, 0.68).
narrative_ontology:constraint_metric(bureaucratic_accommodation_as_extraction_persistence, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(bureaucratic_accommodation_as_extraction_persistence, theater_ratio, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bureaucratic_accommodation_as_extraction_persistence, tangled_rope).
narrative_ontology:human_readable(bureaucratic_accommodation_as_extraction_persistence, "Bureaucratic Accommodation as Extraction Persistence").
narrative_ontology:topic_domain(bureaucratic_accommodation_as_extraction_persistence, "organizational_dynamics/labor_relations/institutional_power").

domain_priors:requires_active_enforcement(bureaucratic_accommodation_as_extraction_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bureaucratic_accommodation_as_extraction_persistence, institutional_authority).
narrative_ontology:constraint_beneficiary(bureaucratic_accommodation_as_extraction_persistence, protected_worker_class).
narrative_ontology:constraint_victim(bureaucratic_accommodation_as_extraction_persistence, surface_exposed_workers).
narrative_ontology:constraint_victim(bureaucratic_accommodation_as_extraction_persistence, accommodation_applicants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SURFACE-EXPOSED WORKER (SNARE) — Trapped by economic necessity in positions requiring physical presence despite architectural barriers. The accommodation process imposes documentation costs, medical disclosure requirements, waiting periods, and approval uncertainty while the underlying architectural constraint remains unchanged. Cannot exit without losing livelihood; bears maximum extraction through procedural friction layered onto structural inaccessibility.
constraint_indexing:constraint_classification(bureaucratic_accommodation_as_extraction_persistence, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: ACCOMMODATION APPLICANT (TANGLED ROPE) — Constrained by application costs and career risk of disclosure, but the process does provide genuine coordination function for some workers. Experiences both the extraction mechanism (procedural barriers, approval stratification, denial without recourse) and the coordination benefit (when accommodations are granted, work becomes possible). Mixed experience — significant extraction but not maximal.
constraint_indexing:constraint_classification(bureaucratic_accommodation_as_extraction_persistence, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INSTITUTIONAL AUTHORITY (ROPE) — Benefits from the accommodation framework as a liability shield and conflict resolution mechanism. Experiences the constraint as coordination: the process channels worker demands into manageable bureaucratic pathways, demonstrates compliance with legal requirements, and maintains discretion over approval decisions. Net beneficiary — the framework protects institutional prerogatives while appearing responsive.
constraint_indexing:constraint_classification(bureaucratic_accommodation_as_extraction_persistence, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PROTECTED WORKER CLASS (ROPE) — Workers in positions not requiring physical site presence or with pre-existing accommodations experience the framework as pure coordination. They benefit from institutional legitimacy of the accommodation process without bearing its costs. Mobile within the organization or to similar roles elsewhere; sees the system as functional conflict resolution.
constraint_indexing:constraint_classification(bureaucratic_accommodation_as_extraction_persistence, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 5: DISABILITY RIGHTS COALITION (SCAFFOLD) — Organized advocacy groups see the accommodation framework as a temporary compromise with a sunset logic: as universal design principles mature, architectural barriers will be eliminated at construction rather than managed through individual accommodations. The procedural extraction is tolerated as a transitional mechanism while building codes, accessibility standards, and inclusive design norms advance. Estimated sunset: 15-25 years for universal design to become default practice.
constraint_indexing:constraint_classification(bureaucratic_accommodation_as_extraction_persistence, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: COMPLIANCE OFFICE (PITON) — The accommodation review process is substantially theatrical: approval decisions are often predetermined by budget constraints and managerial preferences, with the formal review serving primarily as documentation for legal defense. The office sees its own process as degraded — maintained to demonstrate compliance rather than to genuinely evaluate accommodation feasibility. High theater ratio derives from the gap between procedural formality and actual decision-making.
constraint_indexing:constraint_classification(bureaucratic_accommodation_as_extraction_persistence, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a structural view, the accommodation framework exhibits both genuine coordination function (resolving individual access conflicts, providing legal framework for worker rights) and asymmetric extraction (procedural barriers stratified by worker category, approval discretion concentrated in institutional hands, architectural substrate unchanged). The framework coordinates access for some while extracting compliance costs from those least able to bear them. Tangled rope classification reflects the irreducible hybrid structure.
constraint_indexing:constraint_classification(bureaucratic_accommodation_as_extraction_persistence, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bureaucratic_accommodation_as_extraction_persistence_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(bureaucratic_accommodation_as_extraction_persistence, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bureaucratic_accommodation_as_extraction_persistence, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(bureaucratic_accommodation_as_extraction_persistence, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(bureaucratic_accommodation_as_extraction_persistence, TR),
    TR >= 0.70.

:- end_tests(bureaucratic_accommodation_as_extraction_persistence_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The accommodation framework imposes significant procedural costs (documentation requirements, medical disclosure, waiting periods, approval uncertainty) that are stratified by worker category. Surface-exposed workers in economically precarious positions bear maximum costs while protected workers face minimal barriers. The extraction is not total (some accommodations are genuinely granted, providing real access) but is substantial and asymmetrically distributed. The value reflects that much of the procedural burden serves institutional liability management rather than accommodation evaluation. Suppression (0.72): High. Workers face multiple barriers to challenging denials: economic dependency on current position, career risk of disclosure, information asymmetry about approval criteria, lack of recourse mechanisms, and the architectural substrate remaining unchanged regardless of accommodation outcome. The suppression is not absolute (organized advocacy exists, legal frameworks provide some protection) but is severe for individual workers. Theater ratio (0.78): High. The accommodation review process is substantially performative. Approval decisions are often predetermined by budget allocations and managerial preferences, with the formal review serving primarily to generate documentation for legal defense. The theater has increased over the interval as procedural requirements have accumulated while actual decision-making has become more concentrated and less transparent.
 *
 * PERSPECTIVAL GAP:
 *   The institutional authority sees coordination (Rope) — the framework solves the legitimate problem of managing individual access requests while protecting institutional resources and discretion. Protected workers also see coordination (Rope) — the system functions for them without imposing costs. The disability rights coalition sees a temporary compromise (Scaffold) — the framework is tolerated as a transitional mechanism while universal design norms advance. The compliance office sees degraded ritual (Piton) — the review process persists through inertia and legal necessity rather than genuine function. Accommodation applicants see mixed coordination and extraction (Tangled Rope) — the system both enables and constrains their access. Surface-exposed workers see pure extraction (Snare) — procedural barriers layered onto architectural barriers with no exit option. The analytical observer sees irreducible hybrid structure (Tangled Rope) — genuine coordination function for some coexists with asymmetric extraction from those least able to bear costs. The perspectival gap reveals how policy modification can simultaneously resolve conflict for some stakeholders while maintaining structural inequality for others.
 *
 * DIRECTIONALITY LOGIC:
 *   The institutional authority is the primary beneficiary — the accommodation framework channels worker demands into manageable bureaucratic pathways, demonstrates legal compliance, and maintains institutional discretion over approval decisions. The authority experiences low effective extraction (beneficiary + arbitrage exit → low d → low/negative chi). Surface-exposed workers are the primary victims — they bear procedural costs, face approval uncertainty, and remain trapped by economic necessity while the architectural substrate persists. They experience maximum extraction (victim + trapped exit → high d → high chi). Accommodation applicants occupy a middle position — they face significant costs but also receive genuine coordination benefit when accommodations are granted (victim + constrained exit → moderate d → moderate chi). Protected workers are secondary beneficiaries — they benefit from the framework's legitimacy without bearing its costs (beneficiary + mobile exit → low d → low chi). The compliance office sees its own process as degraded (piton classification from theater gate rather than high chi). The disability rights coalition sees a temporary problem with a sunset (scaffold classification from organized power + generational horizon). The analytical observer identifies the irreducible hybrid structure (tangled rope classification from structural analysis).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: This constraint demonstrates how bureaucratic accommodation frameworks exhibit genuine coordination function (resolving individual access conflicts, providing legal framework for worker rights, channeling demands into manageable pathways) while simultaneously serving as extraction mechanisms (procedural barriers stratified by worker category, approval discretion concentrated in institutional hands, architectural substrate unchanged). The tangled rope classification is not a failure to choose between coordination and extraction — it is the accurate structural description. The framework coordinates access for protected workers and some applicants while extracting compliance costs from surface-exposed workers. The coordination is real (some accommodations are genuinely granted, enabling work that would otherwise be impossible). The extraction is real (procedural costs are asymmetrically distributed, approval rates are stratified, architectural barriers persist). The mandatrophy is resolved by recognizing that the same structural mechanism can be coordination from one perspective and extraction from another, and that this perspectival gap is the constraint's defining feature rather than an analytical failure. The high extractiveness (0.68) and high suppression (0.72) combined with genuine coordination function for some agents is exactly what tangled rope classification captures.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    approval_stratification_mechanism,
    'Is approval rate stratification by worker category driven by legitimate accommodation feasibility differences or by institutional preference for protecting high-value workers?',
    'Regression analysis controlling for accommodation type, cost, and architectural constraints; comparison of denial justifications across worker categories for equivalent requests',
    'If feasibility-driven: coordination function is genuine, extraction is side effect. If preference-driven: extraction is primary mechanism, coordination is cover story.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(approval_stratification_mechanism, empirical, 'Whether approval stratification reflects feasibility or institutional preference').

omega_variable(
    procedural_cost_threshold,
    'At what procedural cost level does the accommodation framework transition from coordination mechanism to extraction barrier?',
    'Survey data on application abandonment rates by cost burden; correlation between procedural requirements and approval rates; comparison of accommodation uptake across organizations with different procedural complexity',
    'If threshold is low: most accommodation frameworks are extractive. If threshold is high: only the most burdensome frameworks cross into extraction territory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(procedural_cost_threshold, empirical, 'Procedural cost threshold distinguishing coordination from extraction').

omega_variable(
    universal_design_timeline,
    'Will universal design principles actually eliminate the need for individual accommodations within a generational timeframe, or is the scaffold perspective aspirational?',
    'Longitudinal tracking of building code adoption rates; cost-benefit analysis of universal design vs retrofit accommodations; political economy of construction industry resistance',
    'If universal design sunset is real: scaffold classification confirmed, extraction is temporary. If sunset is indefinitely deferred: scaffold perspective is false hope, extraction persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_design_timeline, empirical, 'Whether universal design sunset is structurally achievable').

omega_variable(
    architectural_substrate_mutability,
    'Is the architectural constraint genuinely immutable (mountain) or contingently maintained through institutional choice?',
    'Cost analysis of architectural modification vs accommodation process overhead; examination of retrofit decisions in comparable organizations; identification of budget allocation priorities',
    'If immutable: accommodation framework is necessary coordination. If contingent: framework is extraction mechanism that preserves institutional discretion by avoiding substrate modification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(architectural_substrate_mutability, conceptual, 'Whether architectural barriers are structurally necessary or institutionally maintained').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bureaucratic_accommodation_as_extraction_persistence, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bureau_accom_tr_t0, bureaucratic_accommodation_as_extraction_persistence, theater_ratio, 0, 0.55).
narrative_ontology:measurement(bureau_accom_tr_t3, bureaucratic_accommodation_as_extraction_persistence, theater_ratio, 3, 0.64).
narrative_ontology:measurement(bureau_accom_tr_t6, bureaucratic_accommodation_as_extraction_persistence, theater_ratio, 6, 0.71).
narrative_ontology:measurement(bureau_accom_tr_t10, bureaucratic_accommodation_as_extraction_persistence, theater_ratio, 10, 0.78).

% Extraction over time
narrative_ontology:measurement(bureau_accom_be_t0, bureaucratic_accommodation_as_extraction_persistence, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(bureau_accom_be_t3, bureaucratic_accommodation_as_extraction_persistence, base_extractiveness, 3, 0.56).
narrative_ontology:measurement(bureau_accom_be_t6, bureaucratic_accommodation_as_extraction_persistence, base_extractiveness, 6, 0.62).
narrative_ontology:measurement(bureau_accom_be_t10, bureaucratic_accommodation_as_extraction_persistence, base_extractiveness, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bureaucratic_accommodation_as_extraction_persistence, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is downstream of architectural_constraint_as_dual_substrate (the physical barriers that accommodations address) and collective_action_as_leverage_conversion (the organizing pressure that forced institutional response). The accommodation framework represents the institution's strategic response: creating a procedural pathway that appears to resolve the conflict while maintaining structural inequality. The framework's extractiveness is distinct from the architectural constraint's extractiveness — the architectural barrier may be a mountain (genuinely immutable given current technology and resources) while the accommodation framework is a tangled rope (hybrid coordination-extraction mechanism). The network relationship captures that the accommodation framework exists because of the architectural constraint but has its own structural properties and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
