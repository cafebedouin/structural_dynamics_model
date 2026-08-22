% ============================================================================
% CONSTRAINT STORY: gpl_derivative_work_trigger__interface_boundary_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_derivative_work_trigger__interface_boundary_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: gpl_derivative_work_trigger__interface_boundary_reading
 *   human_readable: GPL Derivative Work Trigger â Interface Boundary Reading
 *   domain: software_licensing/copyright_law/open_source_governance
 *
 * SUMMARY:
 *   This constraint instantiates the interface_boundary_reading of the
 *   gpl_derivative_work_trigger kernel. It treats technically clean API
 *   boundaries as legal firewalls that prevent the formation of a derivative
 *   work under GPL, even when modules are tightly coupled in production. This
 *   enables mixed-licensing software ecosystems but leaves users without
 *   source for proprietary components that exploit the boundary. The
 *   constraint is claimed as scaffold because it functions as transitional
 *   support for modular architecture pending broader license-compatibility
 *   solutions, not as a permanent steady-state arrangement.
 *
 * KEY AGENTS:
 *   - ecosystem_integrators: Primary beneficiary (organized/constrained) â gain modular licensing freedom to combine proprietary and GPL components
 *   - users_expecting_fullstack_source: Primary target (moderate/constrained) â lose full-stack source guarantees when proprietary modules sit behind APIs
 *   - gpl_rightholders: Agenda setter (organized/arbitrage) â administer the license boundary and are partially constrained by the reading itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_derivative_work_trigger__interface_boundary_reading, 0.48).
domain_priors:suppression_score(gpl_derivative_work_trigger__interface_boundary_reading, 0.5).
domain_priors:theater_ratio(gpl_derivative_work_trigger__interface_boundary_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__interface_boundary_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__interface_boundary_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__interface_boundary_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__interface_boundary_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_derivative_work_trigger__interface_boundary_reading, scaffold).
narrative_ontology:human_readable(gpl_derivative_work_trigger__interface_boundary_reading, "GPL Derivative Work Trigger â Interface Boundary Reading").
narrative_ontology:topic_domain(gpl_derivative_work_trigger__interface_boundary_reading, "software_licensing/copyright_law/open_source_governance").

domain_priors:requires_active_enforcement(gpl_derivative_work_trigger__interface_boundary_reading).
narrative_ontology:has_sunset_clause(gpl_derivative_work_trigger__interface_boundary_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_derivative_work_trigger__interface_boundary_reading, '74206765-ca85-44ed-ace8-76b4d0c34280').
narrative_ontology:cs_kernel_codification('74206765-ca85-44ed-ace8-76b4d0c34280', fixed_text).
narrative_ontology:cs_authority_grounding('74206765-ca85-44ed-ace8-76b4d0c34280', lineage).
narrative_ontology:cs_interpretation_layer_present('74206765-ca85-44ed-ace8-76b4d0c34280').
narrative_ontology:cs_reading_relation('74206765-ca85-44ed-ace8-76b4d0c34280', gpl_derivative_work_trigger__broad_copyleft_reading, coexists_with).
narrative_ontology:cs_reading_relation('74206765-ca85-44ed-ace8-76b4d0c34280', gpl_derivative_work_trigger__narrow_linking_permissive_reading, influences).
narrative_ontology:cs_axiom('74206765-ca85-44ed-ace8-76b4d0c34280', foundational, clean_api_boundary_blocks_derivative_status).
narrative_ontology:cs_axiom_status(clean_api_boundary_blocks_derivative_status, holdable).
narrative_ontology:cs_axiom_grounding('74206765-ca85-44ed-ace8-76b4d0c34280', clean_api_boundary_blocks_derivative_status, conventional).
narrative_ontology:cs_axiom('74206765-ca85-44ed-ace8-76b4d0c34280', secondary, modular_composition_preserves_license_autonomy).
narrative_ontology:cs_axiom_status(modular_composition_preserves_license_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('74206765-ca85-44ed-ace8-76b4d0c34280', modular_composition_preserves_license_autonomy, instrumental).
narrative_ontology:cs_reference_frame('74206765-ca85-44ed-ace8-76b4d0c34280', technical_modularity_framework).
narrative_ontology:cs_drift_state('74206765-ca85-44ed-ace8-76b4d0c34280', contemporary_enforcement_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('74206765-ca85-44ed-ace8-76b4d0c34280', '').
narrative_ontology:cs_kernel_id(gpl_derivative_work_trigger__interface_boundary_reading, gpl_derivative_work_trigger).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__interface_boundary_reading, ecosystem_integrators).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__interface_boundary_reading, users_expecting_fullstack_source).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build and distribute systems that combine GPL-licensed components with proprietary modules separated by API boundaries. They rely on the legal interpretation that clean APIs block derivative-work status to avoid disclosing source code for their proprietary integrations. Their exit is constrained by the risk of litigation if a court rejects the boundary reading.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, ecosystem_integrators, beneficiary,
    organized, biographical, constrained, global).

% Deploy or use integrated software systems believing that copyleft obligations ensure they can obtain and modify the complete source code. Under this reading, they receive source for the GPL core but not for proprietary modules that sit behind clean APIs, reducing their actual control over the full stack. Their exit is constrained by dependency on integrated systems and lack of legal standing to force disclosure.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, users_expecting_fullstack_source, payer,
    moderate, biographical, constrained, global).

% Hold copyright in GPL-licensed code and enforce the license terms. Under this reading, their ability to claim derivative-work status over tightly coupled but API-separated modules is narrowed, limiting their enforcement portfolio to direct modifications and non-API-encapsulated integrations. They can still enforce on direct violators and can choose enforcement targets, giving them arbitrage options, but the reading reduces the scope of what they can enforce.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, gpl_rightholders, agenda_setter,
    organized, generational, arbitrage, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_derivative_work_trigger__interface_boundary_reading, ecosystem_integrators).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables modular software architecture by allowing distinct components interacting through clean API boundaries to retain separate licensing, solving the collective-action problem of combining open and proprietary codebases without triggering copyleft infection across the entire system.
% TRANSFER_FUNCTION: Moves the obligation to disclose source code away from API-boundary-crossing integrators and onto direct modifiers of GPL code, while users lose the expectation of receiving full-stack source for systems that include proprietary modules.
% ABSENT_VOICES: Proponents of the broad_copyleft_reading argue that tight coupling through any mechanism including APIs creates a derivative work; their objections are documented in amicus briefs and copyleft enforcement complaints but are structurally disadvantaged in venues where this reading dominates.
% DISAPPEARANCE_RATIONALE: If the interface boundary reading vanished, integrators would face broad copyleft obligations across API boundaries, proprietary kernel modules and microservice boundaries would be reclassified as derivative works, and the mixed-license software ecosystem would contract dramatically as integrators reorganized around monolithic open-source stacks or proprietary alternatives.
% FOUNDING_PROBLEM: The need to preserve software modularity and composability while maintaining copyleft for direct modifications, preventing a single copyleft component from automatically claiming source-disclosure rights over an entire integrated system.
% FOUNDING_PROBLEM_CORROBORATION: Ecosystem integrators and the Linux Foundation technical publications attest that modular kernel architecture requires license boundaries at APIs; the Free Software Conservancy and Software Freedom Law Center dispute that this architectural need overrides the GPL's derivative-work scope, corroborating that the problem is contested rather than settled from outside the beneficiary set.
narrative_ontology:disappearance_verdict(gpl_derivative_work_trigger__interface_boundary_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_derivative_work_trigger__interface_boundary_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_derivative_work_trigger__interface_boundary_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gpl_derivative_work_trigger__interface_boundary_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_derivative_work_trigger__interface_boundary_reading, 0.48, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_derivative_work_trigger__interface_boundary_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gpl_derivative_work_trigger__interface_boundary_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gpl_derivative_work_trigger__interface_boundary_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.48) because the reading transfers a genuine source-code entitlement from users to integrators without compensation, but it is not total â direct modifications remain copyleft. Suppression is moderate (0.50) because the constraint depends on courts and legal institutions actively enforcing the API-boundary test against broad copyleft claims. Theater is moderate (0.35) because arguments about 'cleanliness' of APIs often exceed technical precision and serve to ritualize a desired legal outcome. Accessibility collapse is moderate (0.60) because alternative licensing arrangements exist but are constrained by the dominance of GPL-family licenses in systems software. Resistance is moderate (0.55) because broad copyleft advocates actively contest the reading in enforcement actions and public advocacy.
 *
 * PERSPECTIVAL GAP:
 *   Ecosystem integrators experience the constraint as necessary architectural freedom that preserves innovation and investment in proprietary components. Users expecting full-stack source experience the same rule as a loophole that fragments copyleft guarantees across a system boundary. The engine computes this divergence from the structural role declarations and exit options; the scaffold claim does not resolve the dispute.
 *
 * DIRECTIONALITY LOGIC:
 *   ecosystem_integrators are beneficiaries (low d) because the constraint subsidizes their ability to combine proprietary and GPL code without disclosure obligation. users_expecting_fullstack_source are payers (high d) because the constraint extracts source-code rights from them by exempting API-separated modules from copyleft. gpl_rightholders are agenda setters whose structural relationship to this specific reading is ambiguous: the reading narrows their enforcement rights, but they retain authority over the kernel itself; with arbitrage exit options they sit near the symmetric middle.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as scaffold prevents mislabeling it as pure extraction (snare) because it solves a genuine coordination problem â modular architecture would be impossible if every API crossing triggered copyleft infection. It also prevents mislabeling it as pure rope because identifiable users bear the cost of lost source access. The scaffold type captures the transitional intent: the reading is justified as a bridge toward a mature modular licensing ecosystem, not as a permanent equilibrium. If the transitional justification proves false and the reading ossifies, the temporal measurements showing rising theater and stable extraction would support reclassification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is this constraint a stable legal doctrine or one contested reading of the GPL derivative-work kernel among several live alternatives?',
    'Supreme Court or transnational appellate rulings that explicitly adopt or reject the clean-API test in copyleft contexts.',
    'If adopted as settled doctrine, the reading stabilizes as a rope or enforcement mechanism; if rejected or persistently contested, it remains a scaffold or tangled rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committing frame uncertainty for interface_boundary_reading').

omega_variable(
    api_boundary_naturalness,
    'Do clean API boundaries reflect an intrinsic technical property that naturally blocks derivative status, or are they a legally constructed fiction performatively maintained to enable proprietary integration?',
    'Comparative legal-technical analysis of how courts distinguish ''clean'' from ''unclean'' APIs in copyright cases, independent of licensing context.',
    'If the boundary is constructed, the constraint''s coordination function is weaker and its extraction (lost source expectations) is higher; if intrinsic, the boundary has independent technical legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(api_boundary_naturalness, empirical, 'Whether API boundaries are natural technical facts or legal constructs').

omega_variable(
    scaffold_transience,
    'Is the interface boundary reading genuinely transitional toward a comprehensive license-compatibility framework, or has it become a permanent structural accommodation?',
    'Observation of whether proponents actively advance a sunset or replacement mechanism, or if the reading has ossified into standard practice without transitional intent.',
    'If permanent, reclassification from scaffold to rope or tangled rope; if transitional and actively moving toward replacement, scaffold classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scaffold_transience, empirical, 'Whether the scaffold''s transitional intent is genuine or cover').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_derivative_work_trigger__interface_boundary_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t0, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(gpl__tr_t5, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 5, 0.25).
narrative_ontology:measurement(gpl__tr_t10, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(gpl__tr_t15, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 15, 0.32).
narrative_ontology:measurement(gpl__tr_t20, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(gpl__tr_t25, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 25, 0.38).

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(gpl__be_t5, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(gpl__be_t10, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(gpl__be_t15, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 15, 0.44).
narrative_ontology:measurement(gpl__be_t20, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 20, 0.47).
narrative_ontology:measurement(gpl__be_t25, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 25, 0.48).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(gpl_derivative_work_trigger__interface_boundary_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_derivative_work_trigger__interface_boundary_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
