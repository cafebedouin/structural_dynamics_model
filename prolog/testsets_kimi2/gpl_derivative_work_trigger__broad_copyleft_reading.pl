% ============================================================================
% CONSTRAINT STORY: gpl_derivative_work_trigger__broad_copyleft_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_derivative_work_trigger__broad_copyleft_reading, []).

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
 *   constraint_id: gpl_derivative_work_trigger__broad_copyleft_reading
 *   human_readable: GPL Broad Copyleft Reading: Linking as Derivative Work Trigger
 *   domain: software_licensing/copyright_law/open_source_governance
 *
 * SUMMARY:
 *   The GNU General Public License (GPL) contains provisions requiring
 *   distribution of source code for derivative works. The Free Software
 *   Foundation and allied stewards have long maintained that linking a
 *   program to a GPL libraryâeven via dynamic linkingâcreates a
 *   derivative work, thereby triggering the source disclosure obligation for
 *   the combined work. This interpretation, contested by proprietary software
 *   vendors and alternative licensing advocates, functions as a legal
 *   constraint on software architecture and commercial strategy. It is
 *   authored here as the broad copyleft reading of the
 *   gpl_derivative_work_trigger kernel, distinct from narrower
 *   interpretations that limit the derivative work trigger to direct
 *   modification or respect clean interface boundaries.
 *
 * KEY AGENTS:
 *   - fsf_steward: Agenda-setter and interpretive authority â propagates the broad reading through license text, FAQ, and legal support.
 *   - gpl_copyright_holders: Enforcement seat â holds standing to sue and benefits from compliance through commons expansion.
 *   - proprietary_vendors: Primary payer â powerful commercial actors forced to disclose source or architecturally avoid GPL libraries.
 *   - commercial_integrators: Secondary payer â moderate-power developers bearing direct compliance analysis and redesign costs.
 *   - downstream_developers: Beneficiary â receives source code access compelled from proprietary distributors.
 *   - legal_practitioners: Analytical observer â navigates the ambiguity without direct stake in the outcome.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_derivative_work_trigger__broad_copyleft_reading, 0.74).
domain_priors:suppression_score(gpl_derivative_work_trigger__broad_copyleft_reading, 0.68).
domain_priors:theater_ratio(gpl_derivative_work_trigger__broad_copyleft_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, extractiveness, 0.74).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_derivative_work_trigger__broad_copyleft_reading, tangled_rope).
narrative_ontology:human_readable(gpl_derivative_work_trigger__broad_copyleft_reading, "GPL Broad Copyleft Reading: Linking as Derivative Work Trigger").
narrative_ontology:topic_domain(gpl_derivative_work_trigger__broad_copyleft_reading, "software_licensing/copyright_law/open_source_governance").

domain_priors:requires_active_enforcement(gpl_derivative_work_trigger__broad_copyleft_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_derivative_work_trigger__broad_copyleft_reading, '2e577b9b-caef-4368-9b0f-8bbfee8e2879').
narrative_ontology:cs_kernel_codification('2e577b9b-caef-4368-9b0f-8bbfee8e2879', fixed_text).
narrative_ontology:cs_authority_grounding('2e577b9b-caef-4368-9b0f-8bbfee8e2879', lineage).
narrative_ontology:cs_interpretation_layer_present('2e577b9b-caef-4368-9b0f-8bbfee8e2879').
narrative_ontology:cs_reading_relation('2e577b9b-caef-4368-9b0f-8bbfee8e2879', gpl_derivative_work_trigger__narrow_linking_permissive_reading, coexists_with).
narrative_ontology:cs_reading_relation('2e577b9b-caef-4368-9b0f-8bbfee8e2879', gpl_derivative_work_trigger__interface_boundary_reading, influences).
narrative_ontology:cs_axiom('2e577b9b-caef-4368-9b0f-8bbfee8e2879', foundational, linking_constitutes_derivation).
narrative_ontology:cs_axiom_status(linking_constitutes_derivation, holdable).
narrative_ontology:cs_axiom_grounding('2e577b9b-caef-4368-9b0f-8bbfee8e2879', linking_constitutes_derivation, conventional).
narrative_ontology:cs_axiom('2e577b9b-caef-4368-9b0f-8bbfee8e2879', foundational, license_boundaries_follow_code_dependencies).
narrative_ontology:cs_axiom_status(license_boundaries_follow_code_dependencies, holdable).
narrative_ontology:cs_axiom_grounding('2e577b9b-caef-4368-9b0f-8bbfee8e2879', license_boundaries_follow_code_dependencies, conventional).
narrative_ontology:cs_reference_frame('2e577b9b-caef-4368-9b0f-8bbfee8e2879', strong_copyleft_contagion).
narrative_ontology:cs_drift_state('2e577b9b-caef-4368-9b0f-8bbfee8e2879', contemporary_open_source_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2e577b9b-caef-4368-9b0f-8bbfee8e2879', '').
narrative_ontology:cs_kernel_id(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_derivative_work_trigger).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__broad_copyleft_reading, fsf_steward).
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__broad_copyleft_reading, downstream_developers).
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_copyright_holders).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__broad_copyleft_reading, proprietary_vendors).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__broad_copyleft_reading, commercial_integrators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the GPL license text and the official interpretation that linking creates derivative works. Provides legal guidance, supports enforcement litigation, and advocates for the broad copyleft position in public discourse and policy forums. Its authority derives from continuity with the founding free-software movement.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, fsf_steward, agenda_setter,
    institutional, generational, analytical, global).

% Hold copyright in GPL-licensed code and possess legal standing to sue for license violations. They benefit from downstream compliance through expansion of the modifiable commons and prevention of proprietary appropriation of their work.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_copyright_holders, agenda_setter,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_copyright_holders, beneficiary).

% Receive complete corresponding source code for derivative works distributed by others, enabling study, modification, and further distribution. Their ability to improve and audit software depends on the disclosure obligation triggered by the broad linking interpretation.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, downstream_developers, beneficiary,
    moderate, biographical, mobile, global).

% Develop or distribute proprietary software that links to GPL libraries. Under the broad reading, they must release their own source code or cease linking. They face legal risk, compliance engineering costs, and strategic constraints on product architecture.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, proprietary_vendors, payer,
    powerful, biographical, constrained, global).

% Individual developers and mid-size firms integrating third-party GPL components into commercial products. They bear the direct burden of license compliance analysis, code audits, and architectural redesign to avoid triggering the derivative work clause.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, commercial_integrators, payer,
    moderate, immediate, constrained, national).

% Advise clients on GPL compliance risk. They operate in an environment of legal ambiguity where the broad reading is the most conservative and risk-averse position, generating billable hours but also genuine uncertainty about client exposure.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, legal_practitioners, observer,
    organized, biographical, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures that software built upon or linked to GPL-licensed code remains available in source form to downstream users, preventing free-riding on communal development efforts and maintaining a growing commons of modifiable software.
% TRANSFER_FUNCTION: Transfers source code disclosure obligations and compliance costs from GPL copyright holders to downstream distributors and proprietary integrators; transfers source code access and modification rights to downstream developers and users.
% ABSENT_VOICES: Proprietary software users who benefit from GPL-derived products but do not participate in the licensing discourse; academic computer scientists who view linking as a technical mechanism unrelated to copyright derivation; small developers who cannot afford compliance analysis and simply avoid GPL code rather than voicing opposition.
% DISAPPEARANCE_RATIONALE: If the broad copyleft reading vanished overnight, proprietary vendors would rapidly adopt GPL libraries without source disclosure, the commons would cease to receive compelled contributions from linked proprietary works, and the current equilibrium of open-source proliferation would shift toward proprietary enclosure of GPL-adjacent code.
% FOUNDING_PROBLEM: Free software authors faced proprietary appropriation: companies would incorporate freely available code into closed products without contributing improvements back, leading to asymmetrical exploitation of communal labor and enclosure of the digital commons.
% FOUNDING_PROBLEM_CORROBORATION: FSF and GPL authors attest the problem remains live, citing ongoing proprietary use of GPL code without compliance. Proprietary vendors and some independent developers attest the problem is substantially addressed by the existing body of open-source alternatives and that the broad reading now functions primarily as a mechanism of competitive exclusion; legal scholarship and empirical studies of license compliance provide external assessment.
narrative_ontology:disappearance_verdict(gpl_derivative_work_trigger__broad_copyleft_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_derivative_work_trigger__broad_copyleft_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_derivative_work_trigger__broad_copyleft_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gpl_derivative_work_trigger__broad_copyleft_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_derivative_work_trigger__broad_copyleft_reading, 0.74, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_derivative_work_trigger__broad_copyleft_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gpl_derivative_work_trigger__broad_copyleft_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gpl_derivative_work_trigger__broad_copyleft_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.74) is high because the broad reading compels a costly action (source disclosure of potentially entire proprietary codebases) that proprietary actors would not choose voluntarily; suppression (0.68) reflects the active enforcement through copyright infringement threats and litigation that makes avoidance the rational default for risk-averse firms. Theater ratio (0.25) is relatively low because most GPL enforcement serves a genuine compliance function rather than pure performance, though public license campaigns carry some theatrical element. Accessibility collapse (0.45) is moderate: alternatives exist (proprietary libraries, MIT/BSD licensing, SaaS architectures that avoid distribution) but they impose real architectural and opportunity costs. Resistance (0.55) captures active legal and lobbying pushback from proprietary vendors, as well as the widespread industry practice of license avoidance that constitutes passive resistance. The temporal series show extractiveness and suppression rising from 1989 to the present as enforcement infrastructure matured and the FSF clarified its maximalist position.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (FSF, copyright holders) experiences this constraint as coordination protecting a digital commons; the engine should compute a lower effective extraction for that seat because they are structural beneficiaries with analytical or generational time horizons. The payer seats (proprietary vendors, commercial integrators) experience it as extractive imposition; their constrained exit and biographical horizons amplify effective extraction. The downstream developer seat sits betweenâgenuine benefit from source access, but no control over the enforcement mechanism. The analytical seat (legal practitioners) sees the ambiguity that the other seats resolve differently.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are declared as fsf_steward, downstream_developers, and gpl_copyright_holders. The FSF benefits through philosophical vindication and institutional relevance; downstream developers benefit through compelled source access; copyright holders benefit through license compliance and commons expansion. Victims are proprietary_vendors and commercial_integrators, who bear the compliance cost and strategic constraint. The structural derivation places beneficiaries at low d (subsidy/near-beneficiary) and victims at high d (target). No override is needed: the FSF's analytical exit and institutional power place it near the beneficiary end, while proprietary vendors' constrained exit and powerful-but-trapped status place them at moderate-high d.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy guard prevents misreading this constraint as a pure snare (coercion without coordination) or a pure rope (coordination without extraction). The genuine coordination functionâcommons growth, downstream source access, prevention of free-ridingâis real and measurable; without it, the constraint would be a snare. The asymmetric extractionâproprietary vendors compelled to disclose or avoidâis equally real; without it, the constraint would be a rope. The Tangled Rope classification captures that both are present and operate through the same mechanism. Temporal measurements show extraction rising over time, which the engine can flag for accumulation review, but the founding problem (proprietary appropriation) remains contested rather than dead, preventing automatic piton conversion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    linking_derivative_ambiguity,
    'Does dynamic linking, as a technical implementation mechanism, constitute copyright derivation under statutory law independent of license contractual terms?',
    'Supreme Court or authoritative appellate decision directly addressing whether dynamic linking meets the derivative work standard under 17 U.S.C. Â§ 101, or equivalent authoritative ruling in relevant jurisdictions.',
    'If linking is not derivative as a matter of copyright law, the broad copyleft reading collapses into a contractual claim with different enforceability and scope; if it is derivative, the reading gains statutory reinforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(linking_derivative_ambiguity, conceptual, 'Statutory copyright status of linking under derivative work doctrine').

omega_variable(
    enforcement_selectivity,
    'Is the broad reading enforced consistently against all linking violations, or selectively against commercially viable targets?',
    'Empirical audit of GPL enforcement actions categorizing target type, violation mode, and settlement terms across the enforcement history.',
    'Consistent enforcement supports the coordination framing; selective enforcement indicates extraction concentrated on specific victims, shifting classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_selectivity, empirical, 'Whether enforcement is uniform or selective across violators').

omega_variable(
    compliance_cost_benefit_ratio,
    'Does the volume and quality of source code compelled into the commons by the broad reading exceed the aggregate compliance and avoidance costs imposed on proprietary developers?',
    'Economic estimation of compelled disclosure value versus compliance engineering, legal counsel, and architectural avoidance costs across the proprietary software industry.',
    'If compelled disclosure value exceeds costs, the coordination function dominates; if costs exceed disclosure value, the extraction function dominates.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(compliance_cost_benefit_ratio, empirical, 'Balance between commons benefit and industry compliance burden').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_derivative_work_trigger__broad_copyleft_reading, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t0, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(gpl__tr_t6, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 6, 0.13).
narrative_ontology:measurement(gpl__tr_t12, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 12, 0.16).
narrative_ontology:measurement(gpl__tr_t18, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 18, 0.2).
narrative_ontology:measurement(gpl__tr_t24, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 24, 0.23).
narrative_ontology:measurement(gpl__tr_t30, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 30, 0.24).
narrative_ontology:measurement(gpl__tr_t36, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 36, 0.25).

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(gpl__be_t6, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 6, 0.51).
narrative_ontology:measurement(gpl__be_t12, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 12, 0.59).
narrative_ontology:measurement(gpl__be_t18, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 18, 0.65).
narrative_ontology:measurement(gpl__be_t24, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 24, 0.7).
narrative_ontology:measurement(gpl__be_t30, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 30, 0.72).
narrative_ontology:measurement(gpl__be_t36, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 36, 0.74).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t0, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(gpl__su_t6, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 6, 0.44).
narrative_ontology:measurement(gpl__su_t12, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 12, 0.52).
narrative_ontology:measurement(gpl__su_t18, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 18, 0.59).
narrative_ontology:measurement(gpl__su_t24, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 24, 0.64).
narrative_ontology:measurement(gpl__su_t30, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 30, 0.67).
narrative_ontology:measurement(gpl__su_t36, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 36, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_derivative_work_trigger__broad_copyleft_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__broad_copyleft_reading, narrow_linking_permissive_reading).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__broad_copyleft_reading, interface_boundary_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the gpl_derivative_work_trigger kernel. The broad copyleft reading interprets linking as creating derivative works; sibling readings interpret the same kernel text to reach narrower or boundary-dependent conclusions. They form a constraint family linked by contested kernel identity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
