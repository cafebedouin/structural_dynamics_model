% ============================================================================
% CONSTRAINT STORY: gpl_copyleft_scope__narrow_scope_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_copyleft_scope__narrow_scope_reading, []).

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
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: gpl_copyleft_scope__narrow_scope_reading
 *   human_readable: GPL Section 2(b) Narrow Copyleft Scope Reading
 *   domain: software/intellectual_property/open_source
 *
 * SUMMARY:
 *   GPL Section 2(b) establishes a copyleft obligation: recipients who
 *   distribute modified versions must make source code available under GPL.
 *   This constraint instantiates ONE reading of the contested kernel
 *   gpl_copyleft_scope — the narrow_scope_reading. Under this reading, the
 *   derivative-work boundary follows traditional copyright doctrine: direct
 *   modifications to GPL code trigger the obligation, but aggregation
 *   (bundling separate programs), plugin architectures (code boundaries
 *   defined by interface contracts), and certain dynamic linking forms
 *   (runtime symbol resolution without statically-linked symbols) do NOT
 *   constitute derivation and therefore do NOT trigger copyleft. This reading
 *   permits commercial firms to integrate GPL components into proprietary
 *   software stacks without releasing proprietary code. The narrow boundary
 *   is the coordinate. The sibling readings — strong_copyleft_reading (all
 *   coupling = derivation, copyleft applies universally) and
 *   enforcement_vacuum_reading (no judicial precedent settles the boundary,
 *   leaving both interpretations live in practice) — are DIFFERENT
 *   constraints with DIFFERENT epsilon values and DIFFERENT stakeholder
 *   structures. This file documents ONLY the narrow_scope_reading.
 *
 * KEY AGENTS:
 *   - commercial_software_firms: Beneficiary of the narrow boundary; can integrate GPL code without copyleft cascading
 *   - gpl_copyleft_advocates: Payer; expected universal code-sharing but the narrow boundary weakens that expectation
 *   - foss_developer_community: Mixed; benefits from some firm contributions, but cannot enforce universal obligation
 *   - fsf_and_enforcement_actors: Agenda-setter; interprets and enforces Section 2(b), but enforcement authority limited by the narrow boundary
 *   - judicial_system: Observer; has not definitively settled the derivative boundary, allowing ambiguity to persist
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_copyleft_scope__narrow_scope_reading, 0.35).
domain_priors:suppression_score(gpl_copyleft_scope__narrow_scope_reading, 0.28).
domain_priors:theater_ratio(gpl_copyleft_scope__narrow_scope_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_copyleft_scope__narrow_scope_reading, rope).
narrative_ontology:human_readable(gpl_copyleft_scope__narrow_scope_reading, "GPL Section 2(b) Narrow Copyleft Scope Reading").
narrative_ontology:topic_domain(gpl_copyleft_scope__narrow_scope_reading, "software/intellectual_property/open_source").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_copyleft_scope__narrow_scope_reading, '2723b046-6f62-48f0-a625-50a20b6fb505').
narrative_ontology:cs_kernel_codification('2723b046-6f62-48f0-a625-50a20b6fb505', fixed_text).
narrative_ontology:cs_authority_grounding('2723b046-6f62-48f0-a625-50a20b6fb505', extraction).
narrative_ontology:cs_interpretation_layer_present('2723b046-6f62-48f0-a625-50a20b6fb505').
narrative_ontology:cs_reading_relation('2723b046-6f62-48f0-a625-50a20b6fb505', gpl_copyleft_scope__strong_copyleft_reading, forecloses).
narrative_ontology:cs_reading_relation('2723b046-6f62-48f0-a625-50a20b6fb505', gpl_copyleft_scope__enforcement_vacuum_reading, coexists_with).
narrative_ontology:cs_axiom('2723b046-6f62-48f0-a625-50a20b6fb505', foundational, copyright_doctrine_delimits_copyleft).
narrative_ontology:cs_axiom_status(copyright_doctrine_delimits_copyleft, holdable).
narrative_ontology:cs_axiom_grounding('2723b046-6f62-48f0-a625-50a20b6fb505', copyright_doctrine_delimits_copyleft, deontological).
narrative_ontology:cs_axiom('2723b046-6f62-48f0-a625-50a20b6fb505', secondary, functional_coupling_not_derivative).
narrative_ontology:cs_axiom_status(functional_coupling_not_derivative, holdable).
narrative_ontology:cs_axiom_grounding('2723b046-6f62-48f0-a625-50a20b6fb505', functional_coupling_not_derivative, empirically_contingent).
narrative_ontology:cs_reference_frame('2723b046-6f62-48f0-a625-50a20b6fb505', copyright_doctrine_derivative_boundary).
narrative_ontology:cs_drift_state('2723b046-6f62-48f0-a625-50a20b6fb505', contemporary_software_coupling_expansion, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2723b046-6f62-48f0-a625-50a20b6fb505', '').
narrative_ontology:cs_kernel_id(gpl_copyleft_scope__narrow_scope_reading, gpl_copyleft_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__narrow_scope_reading, commercial_software_firms).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__narrow_scope_reading, proprietary_layer_developers).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_copyleft_scope__narrow_scope_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(gpl_copyleft_scope__narrow_scope_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_copyleft_scope__narrow_scope_reading_tests).
:- end_tests(gpl_copyleft_scope__narrow_scope_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-low (0.35) because the constraint permits broad access to GPL code without demanding reciprocal release from all users — commercial firms gain a benefit (reusable code) without symmetrical cost in the narrow-boundary interpretation. Suppression is low (0.28) because the constraint is a BOUNDARY RULE, not an enforcement mechanism; firms can choose to respect or contest the boundary, and judicial precedent is absent. Theater ratio is modest (0.22) because GPL enforcement activities are mostly functional (actual copyright litigation, licensing review) but increasingly include interpretive theater (debates about what the boundary should be, positioning papers, policy statements). Resistance is moderate-high (0.62) because strong-copyleft advocates actively contest the narrowness and argue for broader interpretations; the enforcement vacuum allows that contestation to persist. The time series shows extractiveness rising slightly in the early interval (0–16) as commercial integration patterns demonstrate the practical value of the narrow boundary, then stabilizing as adoption reaches equilibrium. Theater rises slightly as the GPL/proprietary boundary becomes more contested in the industry, then stabilizes at a modest level once the interpretive status quo is established. Measurements are shared on a single grid so every metric is authored at every time point.
 *
 * PERSPECTIVAL GAP:
 *   The narrow-reading interpretation creates a systematic perspectival gap between institutional power holders and the copyleft vision's authors. Commercial firms (powerful, institutional) perceive the constraint as enabling integration and mixed-license ecosystems (coordination + flexibility). GPL authors and advocates (organized but less powerful institutionally) perceive it as a limitation on copyleft force (extraction of code-sharing ambition). The FSF occupies a hybrid seat: they authored the GPL, but they lack unilateral enforcement authority to impose the strong-copyleft reading; their interpretive guidance is influential but not binding. Judicial absence amplifies this gap — no appellate precedent resolves the boundary, so interpretive communities can live in parallel (firms acting under narrow boundary, advocates arguing for strong boundary) without direct collision until litigation forces adjudication.
 *
 * DIRECTIONALITY LOGIC:
 *   Commercial firms (powerful, arbitrage-capable) are structural beneficiaries: they gain access to GPL code at low cost (no obligation to release proprietary layers). Copyleft advocates (organized, generational horizon, constrained exit) are structural payers: they invested in a vision of universal code-sharing that this reading does not deliver. The FOSS community occupies a dual position: they maintain GPL and benefit from contributed improvements, but they cannot enforce wider copyleft and lose potential recapture of proprietary extensions. The FSF/enforcement actors occupy an agenda-setter role: their interpretive authority narrows the copyleft scope relative to their stated goals (universal sharing), which is why they are not listed as simple beneficiaries. The copyright-law tradition and the judicial system are observers (do not collect from the constraint). From the commercial firm's seat, the constraint is pure coordination (access to code, clear legal boundaries). From the copyleft advocate's seat, it is partial extraction (loss of expected universal obligation). The engine computes this divergence from the structural data.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a LIVING COORDINATION MECHANISM with an active mandate. The narrow-boundary interpretation solves a real coordination problem: enabling commercial adoption and mixed-license ecosystems while preserving some copyleft force. The founding problem (commercial firms need GPL code without full copyleft cascade) is live and actively being managed. The constraint has not degraded into theater or inertia; it is actively enforced by licensing negotiations, legal review, and interpretive decisions. However, mandatrophy risk is present in the contested status: if strong-copyleft advocates gain institutional power and enforce a wider boundary, the narrow reading's coordination function disappears and it becomes a Snare (firms trapped by unexpected obligation). Conversely, if the narrow boundary continues to narrow (e.g., through court rulings that plugin boundaries do not count as derivative at all), the constraint might attenuate into theater. The measurement stability (extractiveness flat after interval 16) suggests the constraint is in equilibrium, not degrading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    derivative_work_boundary_ambiguity,
    'Does GPL Section 2(b)''s reference to ''derivative works'' adopt the traditional copyright-law boundary (narrow interpretation), extend it to all forms of functional coupling (strong interpretation), or remain radically ambiguous pending judicial clarification (enforcement vacuum)?',
    'Appellate-level copyright litigation establishing binding precedent on whether dynamic linking, plugin interfaces, and aggregation constitute derivative works under GPL Section 2(b). Alternatively, legislative GPL revision that explicitly defines the boundary (GPLv3 attempted this with linking clarifications, but ambiguity persists in v2 and for new linking patterns).',
    'A judicial ruling favoring narrow interpretation would certify this reading and weaken strong-copyleft expectations; a ruling favoring strong interpretation would invert the constraint (making this a Snare from the firm perspective and the strong reading a Rope). Continued ambiguity perpetuates the enforcement vacuum reading as the actual operative constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(derivative_work_boundary_ambiguity, empirical, 'The constitutional boundary for GPL copyleft scope.').

omega_variable(
    linux_kernel_module_precedent_status,
    'Do Linux kernel modules loaded at runtime (not statically linked) constitute GPL-derived works, and if so, are proprietary modules GPL violations under Section 2(b)?',
    'Linux Foundation policy clarification, FSF enforcement decision, or court ruling in a GPL v. proprietary-module-vendor case. The Sco v. IBM litigation attempted to address this but did not establish binding precedent; Linux kernel maintainers have taken inconsistent positions (some accepting proprietary modules, some not).',
    'If modules are deemed derivative, the narrow boundary''s practical utility collapses and proprietary-module ecosystems (Nvidia drivers, etc.) become technically GPL-violating. This would shift the constraint toward strong copyleft. If modules are not deemed derivative, the narrow reading is reinforced and commercial integration patterns solidify.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(linux_kernel_module_precedent_status, empirical, 'The specific case that tests whether runtime coupling counts as derivation.').

omega_variable(
    fsf_enforcement_authority_legitimacy,
    'Does the FSF have the legal or moral authority to enforce a wide-copyleft interpretation against firms that the plain copyright doctrine does not reach? Or is the FSF''s authority limited to enforcing the narrow copyright-doctrine boundary?',
    'FSF public licensing policy statements, enforcement litigation outcomes, and community consensus about FSF''s legitimate scope of authority. This is partly empirical (what does FSF do in practice) and partly normative (what should FSF do).',
    'If FSF authority is recognized as limited to the narrow boundary, strong-copyleft advocates lose their primary enforcement mechanism and must pursue legislative or technological alternatives. If FSF authority is seen as legitimately extending to wide-copyleft, this reading''s coordination function erodes and FSF decisions could shift the operative constraint toward strong copyleft.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fsf_enforcement_authority_legitimacy, conceptual, 'The legitimacy boundary for FSF''s enforcement role.').

omega_variable(
    commercial_integration_equilibrium_stability,
    'Is the current stable equilibrium (firms integrating GPL code via narrow-boundary techniques, community contributing improvements) robust to long-term shifts in market concentration or enforcement priorities? Or does it depend on enforcement restraint that could evaporate?',
    'Long-term observation of enforcement patterns; shifts in commercial market concentration or competitive dynamics; changes in FSF leadership and enforcement philosophy; measurement of actual code-flow asymmetry (do proprietary extensions actually outnumber GPL improvements over time).',
    'If the equilibrium is fragile (depends on enforcement restraint or on continued commercial competition), the narrow reading is vulnerable to collapse into either Snare (if strong-copyleft enforcement increases) or Piton (if enforcement atrophies). If it is robust, the rope classification is stable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(commercial_integration_equilibrium_stability, empirical, 'The durability of the narrow-boundary equilibrium.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_copyleft_scope__narrow_scope_reading, 0, 32).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t0, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(gpl__tr_t4, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 4, 0.19).
narrative_ontology:measurement(gpl__tr_t8, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement(gpl__tr_t12, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 12, 0.22).
narrative_ontology:measurement(gpl__tr_t16, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 16, 0.23).
narrative_ontology:measurement(gpl__tr_t20, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(gpl__tr_t24, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 24, 0.22).
narrative_ontology:measurement(gpl__tr_t28, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 28, 0.22).
narrative_ontology:measurement(gpl__tr_t32, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 32, 0.22).

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(gpl__be_t4, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 4, 0.33).
narrative_ontology:measurement(gpl__be_t8, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 8, 0.34).
narrative_ontology:measurement(gpl__be_t12, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 12, 0.35).
narrative_ontology:measurement(gpl__be_t16, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 16, 0.36).
narrative_ontology:measurement(gpl__be_t20, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 20, 0.35).
narrative_ontology:measurement(gpl__be_t24, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 24, 0.35).
narrative_ontology:measurement(gpl__be_t28, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 28, 0.35).
narrative_ontology:measurement(gpl__be_t32, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 32, 0.35).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(gpl_copyleft_scope__narrow_scope_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_copyleft_scope__narrow_scope_reading, information_standard).
narrative_ontology:affects_constraint(gpl_copyleft_scope__narrow_scope_reading, gpl_copyleft_scope__strong_copyleft_reading).
narrative_ontology:affects_constraint(gpl_copyleft_scope__narrow_scope_reading, gpl_copyleft_scope__enforcement_vacuum_reading).

% DUAL FORMULATION NOTE:
% The gpl_copyleft_scope kernel decomposes into three structurally distinct constraints, each instantiating a different reading of GPL Section 2(b)'s derivative-work boundary. narrow_scope_reading (this file) treats the boundary as constrained BY traditional copyright doctrine; strong_copyleft_reading extends the boundary to all functional coupling; enforcement_vacuum_reading models the actual operative constraint as an ambiguity exploited by different interpretive communities. The three readings have different epsilon values (moderate-rope vs. high-snare vs. low-rope) because they reflect fundamentally different derivative-work criteria. Sibling readings are linked via network.affects_constraints to enable contamination analysis (e.g., if judicial precedent forecloses the narrow reading, how does the strong reading's extraction profile shift).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gpl_copyleft_scope__narrow_scope_reading, organized, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
