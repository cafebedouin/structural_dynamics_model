% ============================================================================
% CONSTRAINT STORY: gpl_derivative_work_trigger__interface_boundary_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: gpl_derivative_work_trigger__interface_boundary_reading
 *   human_readable: GPL Derivative Work Trigger â Interface Boundary Reading
 *   domain: software_licensing/copyright_law/open_source_governance
 *
 * SUMMARY:
 *   This constraint instantiates the interface_boundary_reading of the
 *   gpl_derivative_work_trigger kernel. The kernel is the contested GPL
 *   derivative-work trigger: under what conditions does combining code with a
 *   GPL-licensed work create a derivative work that must be licensed under
 *   the GPL? The broad_copyleft_reading holds that linking (even dynamic)
 *   creates derivation; the narrow_linking_permissive_reading holds that all
 *   linking is aggregation. This reading occupies the middle: clean API
 *   boundaries constitute non-derivative aggregation even when the
 *   integration is technically tight. It enables a mixed-licensing ecosystem
 *   where proprietary modules interface with GPL cores. The source material
 *   hypothesized a scaffold, but the constraint lacks a declared sunset and
 *   exhibits sustained asymmetric extraction, so the structural analysis
 *   treats it as a tangled_rope while preserving the manifest hypothesis in
 *   uke_scope.
 *
 * KEY AGENTS:
 *   - ecosystem_integrators: Primary beneficiary (powerful/mobile) â capture value from proprietary modules linking to GPL code without source disclosure.
 *   - users_expecting_full_stack_source: Primary target (powerless/constrained) â bear the cost of incomplete source availability for integrated stacks.
 *   - judicial_interpreters: Agenda setter (institutional/analytical) â adjudicate whether API boundaries block derivative work status.
 *   - fsf_copyleft_defenders: Observer (organized/constrained) â resist the reading ideologically and legally but do not capture or pay into the arrangement.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_derivative_work_trigger__interface_boundary_reading, 0.58).
domain_priors:suppression_score(gpl_derivative_work_trigger__interface_boundary_reading, 0.62).
domain_priors:theater_ratio(gpl_derivative_work_trigger__interface_boundary_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__interface_boundary_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__interface_boundary_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__interface_boundary_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__interface_boundary_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_derivative_work_trigger__interface_boundary_reading, tangled_rope).
narrative_ontology:human_readable(gpl_derivative_work_trigger__interface_boundary_reading, "GPL Derivative Work Trigger â Interface Boundary Reading").
narrative_ontology:topic_domain(gpl_derivative_work_trigger__interface_boundary_reading, "software_licensing/copyright_law/open_source_governance").

domain_priors:requires_active_enforcement(gpl_derivative_work_trigger__interface_boundary_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_derivative_work_trigger__interface_boundary_reading, 'c016ea76-2d84-4722-a2cc-ac85c411ca94').
narrative_ontology:cs_kernel_codification('c016ea76-2d84-4722-a2cc-ac85c411ca94', fixed_text).
narrative_ontology:cs_authority_grounding('c016ea76-2d84-4722-a2cc-ac85c411ca94', lineage).
narrative_ontology:cs_interpretation_layer_present('c016ea76-2d84-4722-a2cc-ac85c411ca94').
narrative_ontology:cs_reading_relation('c016ea76-2d84-4722-a2cc-ac85c411ca94', gpl_derivative_work_trigger__broad_copyleft_reading, influences).
narrative_ontology:cs_reading_relation('c016ea76-2d84-4722-a2cc-ac85c411ca94', gpl_derivative_work_trigger__narrow_linking_permissive_reading, coexists_with).
narrative_ontology:cs_axiom('c016ea76-2d84-4722-a2cc-ac85c411ca94', foundational, api_boundary_blocks_derivation).
narrative_ontology:cs_axiom_status(api_boundary_blocks_derivation, holdable).
narrative_ontology:cs_axiom_grounding('c016ea76-2d84-4722-a2cc-ac85c411ca94', api_boundary_blocks_derivation, conventional).
narrative_ontology:cs_axiom('c016ea76-2d84-4722-a2cc-ac85c411ca94', foundational, modularity_preserves_license_autonomy).
narrative_ontology:cs_axiom_status(modularity_preserves_license_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('c016ea76-2d84-4722-a2cc-ac85c411ca94', modularity_preserves_license_autonomy, conventional).
narrative_ontology:cs_reference_frame('c016ea76-2d84-4722-a2cc-ac85c411ca94', gpl_text_copyleft_intent).
narrative_ontology:cs_drift_state('c016ea76-2d84-4722-a2cc-ac85c411ca94', post_oracle_v_google_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c016ea76-2d84-4722-a2cc-ac85c411ca94', '').
narrative_ontology:cs_kernel_id(gpl_derivative_work_trigger__interface_boundary_reading, gpl_derivative_work_trigger).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__interface_boundary_reading, ecosystem_integrators).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__interface_boundary_reading, users_expecting_full_stack_source).
narrative_ontology:constraint_vindicates(gpl_derivative_work_trigger__interface_boundary_reading, modular_architecture_legitimacy).
narrative_ontology:constraint_vindicates(gpl_derivative_work_trigger__interface_boundary_reading, api_copyright_firewall_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop and distribute proprietary software modules that link against GPL-licensed libraries via clean API boundaries. They gain access to GPL ecosystem network effects and functionality without disclosing source code for their proprietary components. They invest in legal defense, compliance tooling, and industry consortia to maintain confidence in this integration model.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, ecosystem_integrators, beneficiary,
    powerful, biographical, mobile, global).

% Deploy and rely on software stacks that combine GPL and proprietary components integrated through APIs. They receive corresponding source for the GPL-licensed portions but not for the proprietary modules, which limits their ability to audit the full stack, modify integrated behavior, or self-host without vendor dependency.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, users_expecting_full_stack_source, payer,
    powerless, biographical, constrained, global).

% Courts and legal scholars who determine whether clean API boundaries prevent proprietary code from being classified as a derivative work of GPL-licensed code. Their rulings and scholarship create the legal certainty that allows the mixed-licensing integration model to persist.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, judicial_interpreters, agenda_setter,
    institutional, generational, analytical, national).

% Advocates and legal representatives of the Free Software Foundation and aligned organizations who argue that linking across any boundary creates a derivative work under the GPL. They file amicus briefs, publish interpretive guidance, and support enforcement actions that challenge the interface boundary reading in legal and policy forums.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, fsf_copyleft_defenders, observer,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_derivative_work_trigger__interface_boundary_reading, ecosystem_integrators).
narrative_ontology:fixing_cost_class(gpl_derivative_work_trigger__interface_boundary_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables modular software architecture where GPL-licensed and proprietary components can be combined in a single application or system without triggering copyleft source-disclosure obligations for the proprietary components, provided integration occurs through clean API boundaries.
% TRANSFER_FUNCTION: Transfers the obligation to disclose corresponding source from the integrated proprietary module to the GPL component only, allowing proprietary vendors to capture the value of their modules while benefiting from GPL ecosystem network effects; transfers the cost of opaque binaries to end users who lose full-stack auditability and modifiability.
% ABSENT_VOICES: End users who expected complete corresponding source under a broad copyleft regime are structurally underrepresented in license-drafting bodies and litigation; the Free Software Foundation's position is audible in public discourse but increasingly sidelined in commercial open-source governance forums where the interface boundary reading is treated as the practical default.
% DISAPPEARANCE_RATIONALE: If the interface boundary reading vanished and was replaced by broad copyleft, proprietary modules currently integrated via clean APIs would need to be re-licensed under GPL or replaced, ecosystem integrators would lose a primary legal shield for mixed stacks, and the software industry would reorganize around dual-licensing, full permissiveness, or vertical integration.
% FOUNDING_PROBLEM: Early open-source licenses were ambiguous about whether dynamic linking, modular integration, or API-based communication created a derivative work, producing legal uncertainty that deterred commercial investment and adoption in mixed-ecosystem software.
% FOUNDING_PROBLEM_CORROBORATION: Ecosystem integrators and proprietary vendors attest the problem is live and that the interface boundary reading resolves it. The Free Software Foundation attests that the GPL text already resolved the ambiguity and that the interface boundary reading manufactures a loophole inconsistent with the license's intent; no copyright office or international tribunal has formally endorsed the interface boundary reading as the definitive resolution.
narrative_ontology:disappearance_verdict(gpl_derivative_work_trigger__interface_boundary_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_derivative_work_trigger__interface_boundary_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_derivative_work_trigger__interface_boundary_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gpl_derivative_work_trigger__interface_boundary_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_derivative_work_trigger__interface_boundary_reading, 0.58, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.58) reflects that users systematically lose source access they would possess under a broad copyleft regime. Suppression (0.62) reflects that the broad copyleft alternative is suppressed through legal reinterpretation, market pressure, and the marginalization of FSF advocacy. Theater ratio (0.28) is moderate: compliance programs perform boundary cleanliness, but the API distinction carries substantive technical and legal content. Accessibility collapse (0.45) is moderate because full copyleft and full permissiveness remain available alternatives, though network effects and business models constrain adoption. Resistance (0.55) reflects persistent copyleft-community opposition and ongoing enforcement debates.
 *
 * PERSPECTIVAL GAP:
 *   The ecosystem integrator seat experiences this constraint as coordination: it resolves legal uncertainty and enables investment in modular architecture. The user seat experiences the same structure as extraction: they receive an incomplete source corpus and lose the self-hosting and audit rights promised by broad copyleft. The judicial seat experiences it as an interpretive framework with generational stability. The engine computes divergent per-seat classifications from this structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Ecosystem integrators are declared beneficiaries with mobile exit; the engine will derive a low directionality value, treating the constraint as a subsidy to their business model. Users expecting full-stack source are declared victims with constrained exit; the engine will derive a high directionality value, amplifying effective extraction for this seat. Judicial interpreters and FSF defenders are neither beneficiaries nor victims; their directionalities will fall toward the analytical midpoint or be modulated by their exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   Without the beneficiary-victim asymmetry, this constraint might be misclassified as a rope: it genuinely coordinates a mixed-licensing ecosystem that broad copyleft would fragment. Without the coordination function, it might be misclassified as a snare: it systematically deprives users of source rights. The tangled_rope classification captures the hybrid realityâgenuine coordination for integrators, asymmetric extraction for usersâpreventing both errors. The reading is not a scaffold because it carries no sunset clause and its justification is the steady-state mixed ecosystem, not a transition to a specified endpoint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    api_boundary_vs_derivation_test,
    'Does a clean API boundary function as a legal firewall against derivative work status under copyright law, or is it merely a technical fact that courts ignore when applying the derivative work test?',
    'Authoritative appellate or international tribunal ruling specifically addressing whether API boundaries block derivative work status in the context of a strong copyleft license.',
    'If API boundaries are held legally irrelevant to derivative work analysis, this constraint loses its enforcement foundation and collapses toward piton or snare; if they are dispositive, it stabilizes as tangled_rope or rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(api_boundary_vs_derivation_test, conceptual, 'Whether API boundaries are legally dispositive or merely descriptive in derivative work analysis.').

omega_variable(
    tight_coupling_ambiguity,
    'The reading permits tight coupling across the boundary. At what threshold of technical integration does the boundary become legally illusory, and has that threshold been defined?',
    'Case law or regulatory guidance establishing quantitative or qualitative thresholds for tight coupling that nonetheless respect the non-derivative boundary.',
    'If tight coupling consistently vitiates the boundary, the reading has no limiting principle and effective extractiveness rises substantially; if tight coupling is permitted without limit, the reading enables extensive proprietary enclosure of GPL ecosystems.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tight_coupling_ambiguity, empirical, 'Threshold ambiguity in permitted tight coupling across API boundaries.').

omega_variable(
    reading_stability_in_kernel_family,
    'Is the interface boundary reading a stable middle position between broad copyleft and narrow permissive readings, or an unstable compromise that will collapse toward one sibling as case law matures?',
    'Longitudinal analysis of judicial outcomes and enforcement practice in GPL cases involving API boundaries over the next decade.',
    'If unstable toward broad copyleft, this constraint was effectively a temporary scaffold; if unstable toward narrow permissive, it converges to rope; if stable, it persists as tangled_rope with ongoing hybrid coordination and extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_stability_in_kernel_family, conceptual, 'Stability of the interface boundary reading within the GPL derivative work kernel family.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_derivative_work_trigger__interface_boundary_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl_iface_bound_tr_t0, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(gpl_iface_bound_tr_t5, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement(gpl_iface_bound_tr_t10, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(gpl_iface_bound_tr_t15, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 15, 0.23).
narrative_ontology:measurement(gpl_iface_bound_tr_t20, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(gpl_iface_bound_tr_t25, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 25, 0.28).

% Extraction over time
narrative_ontology:measurement(gpl_iface_bound_be_t0, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(gpl_iface_bound_be_t5, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(gpl_iface_bound_be_t10, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(gpl_iface_bound_be_t15, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 15, 0.52).
narrative_ontology:measurement(gpl_iface_bound_be_t20, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(gpl_iface_bound_be_t25, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 25, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(gpl_iface_bound_su_t0, gpl_derivative_work_trigger__interface_boundary_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(gpl_iface_bound_su_t5, gpl_derivative_work_trigger__interface_boundary_reading, suppression_requirement, 5, 0.45).
narrative_ontology:measurement(gpl_iface_bound_su_t10, gpl_derivative_work_trigger__interface_boundary_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement(gpl_iface_bound_su_t15, gpl_derivative_work_trigger__interface_boundary_reading, suppression_requirement, 15, 0.58).
narrative_ontology:measurement(gpl_iface_bound_su_t20, gpl_derivative_work_trigger__interface_boundary_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(gpl_iface_bound_su_t25, gpl_derivative_work_trigger__interface_boundary_reading, suppression_requirement, 25, 0.62).


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
