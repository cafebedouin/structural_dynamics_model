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
 *   constraint_id: gpl_copyleft_scope__narrow_scope_reading
 *   human_readable: GPL Section 2(b) Narrow Derivative Work Scope Reading
 *   domain: legal/intellectual_property/software_governance
 *
 * SUMMARY:
 *   This constraint story captures the narrow_scope_reading of the
 *   gpl_copyleft_scope kernel: the interpretive position that GPL Section
 *   2(b) triggers copyleft obligations only for direct derivative works under
 *   traditional copyright doctrine, excluding mere aggregation, plugin
 *   architectures, and many dynamic linking patterns. Under this reading,
 *   commercial firms retain substantial flexibility to integrate GPL
 *   components into proprietary systems, and enforcement against dynamic
 *   linking is rare. The constraint functions as coordination infrastructure
 *   for mixed codebases, but structurally weakens copyleft advocates'
 *   expectations of universal code-sharing. It is claimed as a rope â a
 *   coordination mechanism â with moderate epsilon reflecting the genuine
 *   legal uncertainty and flexibility transfer, not tuned to match engine
 *   predictions.
 *
 * KEY AGENTS:
 *   - commercial_integrators: Primary beneficiary (powerful/mobile) â retains flexibility to build proprietary layers atop GPL components
 *   - downstream_users: Diffuse beneficiary (organized/constrained) â benefits from broader mixed-codebase software availability
 *   - copyleft_advocates: Excluded party (organized/constrained) â expectations of universal code-sharing structurally weakened
 *   - judicial_interpreters: Analytical observer (institutional/analytical) â applies traditional copyright doctrine without direct material stake
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_copyleft_scope__narrow_scope_reading, 0.32).
domain_priors:suppression_score(gpl_copyleft_scope__narrow_scope_reading, 0.25).
domain_priors:theater_ratio(gpl_copyleft_scope__narrow_scope_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_copyleft_scope__narrow_scope_reading, rope).
narrative_ontology:human_readable(gpl_copyleft_scope__narrow_scope_reading, "GPL Section 2(b) Narrow Derivative Work Scope Reading").
narrative_ontology:topic_domain(gpl_copyleft_scope__narrow_scope_reading, "legal/intellectual_property/software_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_copyleft_scope__narrow_scope_reading, '15d02b4d-eb29-4687-a7d9-1cbd6af788d3').
narrative_ontology:cs_kernel_codification('15d02b4d-eb29-4687-a7d9-1cbd6af788d3', formalized).
narrative_ontology:cs_authority_grounding('15d02b4d-eb29-4687-a7d9-1cbd6af788d3', lineage).
narrative_ontology:cs_interpretation_layer_present('15d02b4d-eb29-4687-a7d9-1cbd6af788d3').
narrative_ontology:cs_reading_relation('15d02b4d-eb29-4687-a7d9-1cbd6af788d3', gpl_copyleft_scope__strong_copyleft_reading, coexists_with).
narrative_ontology:cs_reading_relation('15d02b4d-eb29-4687-a7d9-1cbd6af788d3', gpl_copyleft_scope__enforcement_vacuum_reading, influences).
narrative_ontology:cs_axiom('15d02b4d-eb29-4687-a7d9-1cbd6af788d3', foundational, traditional_derivative_work_boundary).
narrative_ontology:cs_axiom_status(traditional_derivative_work_boundary, holdable).
narrative_ontology:cs_axiom_grounding('15d02b4d-eb29-4687-a7d9-1cbd6af788d3', traditional_derivative_work_boundary, conventional).
narrative_ontology:cs_axiom('15d02b4d-eb29-4687-a7d9-1cbd6af788d3', foundational, proprietary_code_separability).
narrative_ontology:cs_axiom_status(proprietary_code_separability, holdable).
narrative_ontology:cs_axiom_grounding('15d02b4d-eb29-4687-a7d9-1cbd6af788d3', proprietary_code_separability, conventional).
narrative_ontology:cs_reference_frame('15d02b4d-eb29-4687-a7d9-1cbd6af788d3', traditional_copyright_doctrine).
narrative_ontology:cs_drift_state('15d02b4d-eb29-4687-a7d9-1cbd6af788d3', contemporary_software_ecosystem, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('15d02b4d-eb29-4687-a7d9-1cbd6af788d3', '').
narrative_ontology:cs_kernel_id(gpl_copyleft_scope__narrow_scope_reading, gpl_copyleft_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__narrow_scope_reading, commercial_integrators).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__narrow_scope_reading, downstream_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Firms that combine GPL-licensed components with proprietary software layers. Under this narrow reading, they can use dynamic linking, plugin architectures, and mere aggregation without triggering copyleft obligations, provided the integration does not create a direct derivative work under traditional copyright doctrine. This enables hybrid business models and reduces legal uncertainty.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, commercial_integrators, beneficiary,
    powerful, biographical, mobile, global).

% End users and organizations that benefit from a larger ecosystem of software combining open-source and proprietary functionality. They receive more integrated products but have limited influence over the licensing architecture and cannot easily opt out of the mixed-codebase market.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, downstream_users, beneficiary,
    organized, biographical, constrained, global).

% Advocates for strong copyleft and universal code-sharing expectations. This reading structurally weakens their position by permitting proprietary layers adjacent to GPL code. They are largely excluded from the commercial interpretive communities that operationalize the narrow scope rule and lack decisive enforcement leverage in industry-dominated ecosystems.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, copyleft_advocates, excluded,
    organized, generational, constrained, global).

% Courts, legal scholars, and practitioners who apply traditional copyright doctrine to determine derivative work boundaries in software. They interpret the license without direct material stake in the commercial outcome, assessing whether code integration meets the legal standard for derivation.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, judicial_interpreters, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legally predictable boundary that allows GPL-licensed code to coexist with proprietary layers in mixed software systems, enabling commercial development models that incorporate open-source components without requiring full source disclosure of the proprietary stack.
% TRANSFER_FUNCTION: Transfers legal certainty and development flexibility from a maximalist copyleft position to commercial integrators and downstream users, narrowing the scope of reciprocal code-sharing obligations to direct derivative works under traditional copyright doctrine.
% ABSENT_VOICES: Strong copyleft advocates and Free Software Foundation-aligned enforcement entities are structurally excluded from the commercial and judicial interpretive communities that operationalize this narrow reading. They would argue for broader derivative work boundaries but lack decisive enforcement leverage in industry-dominated ecosystems.
% DISAPPEARANCE_RATIONALE: If the narrow scope reading vanished and strong copyleft were uniformly enforced, commercial firms would need to rearchitect software stacks, cease dynamic linking to GPL components, or release proprietary source code. The mixed-codebase ecosystem would contract significantly as hybrid business models became untenable.
% FOUNDING_PROBLEM: The legal indeterminacy of whether combining software modules under different licensing regimes triggers copyleft obligations, and whether traditional copyright's derivative work doctrine provides a limiting principle for software integration.
% FOUNDING_PROBLEM_CORROBORATION: Commercial legal counsel and industry associations attest the problem is solved by narrow interpretation permitting mixed codebases. The Free Software Foundation and Software Freedom Conservancy attest the problem remains unsolved and requires broader enforcement. Academic copyright scholars offer mixed corroboration depending on doctrinal commitment to statutory text versus license intent.
narrative_ontology:disappearance_verdict(gpl_copyleft_scope__narrow_scope_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_copyleft_scope__narrow_scope_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_copyleft_scope__narrow_scope_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gpl_copyleft_scope__narrow_scope_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_copyleft_scope__narrow_scope_reading, 0.32, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is moderate (0.32) because the narrow scope reading transfers genuine value â legal certainty and competitive flexibility â to commercial integrators, but it does so without creating a defined victim class subject to coercive extraction. Suppression is low (0.25) because alternative licensing strategies (strong copyleft, proprietary, permissive) remain fully available and are not suppressed. Theater ratio is low (0.15) as there is minimal performative maintenance; the reading is operationalized through sustained commercial practice and judicial deference, not theatrical compliance. Accessibility collapse is moderate (0.40) because while the narrow reading dominates commercial practice, alternative legal interpretations remain cognitively and legally available. Resistance is low (0.20) because the dominant technology industry has adopted this reading with minimal friction; only organized copyleft advocates mount substantive resistance, and they lack enforcement leverage in this interpretive regime.
 *
 * PERSPECTIVAL GAP:
 *   The commercial integrator seat experiences this constraint as a rope â a coordination mechanism that enables hybrid business models and reduces legal uncertainty. The copyleft advocate seat would experience the same legal boundary as a leaky coordination that permits free-riding on communal labor, though they are structurally excluded from the operative interpretive community. The judicial interpreter seat occupies an analytical position with directionality near symmetric, assessing the boundary without direct material stake. The engine computes this divergence from structural data rather than authored classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Commercial integrators are structural beneficiaries (low d): the narrow scope subsidizes their ability to build proprietary layers atop GPL components without reciprocal source disclosure. Downstream users are diffuse beneficiaries (low d). Copyleft advocates are excluded from the operative interpretive community; if seated as payers they would bear the cost of weakened copyleft (high d), but their exclusion means they do not directly feed the extraction calculation. Judicial interpreters are symmetric (d near 0.5) â they adjudicate without collecting or paying.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading avoids mandatrophy because its founding problem â the legal indeterminacy of software derivative works â remains live and contested. The narrow scope reading offers one coordination solution among several, and its persistence is justified by ongoing commercial need rather than institutional inertia. The continued vitality of strong_copyleft_reading as a sibling interpretation prevents this constraint from degrading into a piton: there is active contest over the correct scope, which keeps the coordination function from becoming purely theatrical.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    gpl_copyleft_kernel_reading,
    'This constraint instantiates the narrow_scope_reading of the gpl_copyleft_scope kernel; would adopting the strong_copyleft_reading or enforcement_vacuum_reading as the operative constraint change the beneficiary/victim structure and epsilon value?',
    'Cross-reference classification with sibling constraint stories in the same kernel family; compare structural data and computed seat types across readings.',
    'If strong_copyleft_reading is the true operative constraint, this reading''s rope classification is false and the constraint is actually a tangled rope or snare with higher extraction on commercial integrators. If enforcement_vacuum_reading is operative, the constraint dissolves into context-dependent capacity rather than a stable coordination mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gpl_copyleft_kernel_reading, conceptual, 'Kernel reading uncertainty for GPL copyleft scope').

omega_variable(
    derivative_work_technological_drift,
    'Does the traditional copyright doctrine of derivative works provide a stable boundary for software architectures involving dynamic linking, plugins, and aggregation, or does technological evolution systematically erode the narrow scope boundary?',
    'Longitudinal judicial precedent analysis tracking how courts treat new software integration mechanisms; empirical study of enforcement patterns against dynamic linking over time.',
    'If the boundary is unstable, the narrow scope reading may be a transient coordination state rather than a stable rope, potentially shifting toward strong copyleft (if courts expand derivative works) or total vacuum (if enforcement disappears against novel architectures).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(derivative_work_technological_drift, empirical, 'Technological drift in derivative work doctrine applicability').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_copyleft_scope__narrow_scope_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t0, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(gpl__tr_t6, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 6, 0.12).
narrative_ontology:measurement(gpl__tr_t12, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 12, 0.13).
narrative_ontology:measurement(gpl__tr_t18, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 18, 0.14).
narrative_ontology:measurement(gpl__tr_t24, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 24, 0.15).

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(gpl__be_t6, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 6, 0.31).
narrative_ontology:measurement(gpl__be_t12, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 12, 0.32).
narrative_ontology:measurement(gpl__be_t18, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 18, 0.33).
narrative_ontology:measurement(gpl__be_t24, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 24, 0.32).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t0, gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(gpl__su_t6, gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 6, 0.22).
narrative_ontology:measurement(gpl__su_t12, gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 12, 0.21).
narrative_ontology:measurement(gpl__su_t18, gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 18, 0.2).
narrative_ontology:measurement(gpl__su_t24, gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 24, 0.19).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_copyleft_scope__narrow_scope_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(gpl_copyleft_scope__narrow_scope_reading, strong_copyleft_reading).
narrative_ontology:affects_constraint(gpl_copyleft_scope__narrow_scope_reading, enforcement_vacuum_reading).

% DUAL FORMULATION NOTE:
% This story is one member of the gpl_copyleft_scope constraint family, decomposed per the epsilon-invariance principle because the natural-language label 'GPL copyleft scope' conflates structurally distinct claims: narrow_scope_reading (moderate-epsilon rope), strong_copyleft_reading (high-extraction tangled rope or snare), and enforcement_vacuum_reading (contested absence of settled constraint).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
