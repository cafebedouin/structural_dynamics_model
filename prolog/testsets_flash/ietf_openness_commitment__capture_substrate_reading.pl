% ============================================================================
% CONSTRAINT STORY: ietf_openness_commitment__capture_substrate_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ietf_openness_commitment__capture_substrate_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ietf_openness_commitment__capture_substrate_reading
 *   human_readable: IETF Openness Commitment (Capture Substrate Reading)
 *   domain: technology_governance/internet_standards/institutional_economics
 *
 * SUMMARY:
 *   This constraint describes the IETF's commitment to open standards, viewed
 *   through the lens of 'capture substrate.' While ostensibly promoting
 *   interoperability and a level playing field, the process, due to its
 *   reliance on 'running code' and resource-intensive participation,
 *   inadvertently creates a substrate where large platform operators and
 *   incumbent vendors can encode their proprietary advantages into 'open'
 *   standards. This leads to moderate extraction from smaller implementers
 *   and end-users, who face de facto lock-in to dominant platforms that can
 *   afford to shape the standards and implement them first.
 *
 * KEY AGENTS:
 *   - large_platform_operators: Primary beneficiary (institutional/arbitrage) — shapes standards to its advantage
 *   - incumbent_vendors: Secondary beneficiary (organized/mobile) — leverages existing market position to influence standards
 *   - small_implementers: Primary victim (moderate/constrained) — forced to adopt standards that favor incumbents
 *   - end_users: Diffuse victim (powerless/constrained) — experiences lock-in and reduced choice
 *   - new_entrants: Excluded (powerless/trapped) — faces high barriers to entry due to incumbent-favored standards
 *   - ietf_working_groups: Agenda setter (institutional/analytical) — administers the standards process, often influenced by well-resourced participants
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ietf_openness_commitment__capture_substrate_reading, 0.65).
domain_priors:suppression_score(ietf_openness_commitment__capture_substrate_reading, 0.45).
domain_priors:theater_ratio(ietf_openness_commitment__capture_substrate_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ietf_openness_commitment__capture_substrate_reading, tangled_rope).
narrative_ontology:human_readable(ietf_openness_commitment__capture_substrate_reading, "IETF Openness Commitment (Capture Substrate Reading)").
narrative_ontology:topic_domain(ietf_openness_commitment__capture_substrate_reading, "technology_governance/internet_standards/institutional_economics").

domain_priors:requires_active_enforcement(ietf_openness_commitment__capture_substrate_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ietf_openness_commitment__capture_substrate_reading, 'f1e3a5dc-1c85-403e-b4d2-124b3a43a590').
narrative_ontology:cs_kernel_codification('f1e3a5dc-1c85-403e-b4d2-124b3a43a590', formalized).
narrative_ontology:cs_authority_grounding('f1e3a5dc-1c85-403e-b4d2-124b3a43a590', practice).
narrative_ontology:cs_interpretation_layer_present('f1e3a5dc-1c85-403e-b4d2-124b3a43a590').
narrative_ontology:cs_reading_relation('f1e3a5dc-1c85-403e-b4d2-124b3a43a590', ietf_openness_commitment__commons_stewardship_reading, influences).
narrative_ontology:cs_reading_relation('f1e3a5dc-1c85-403e-b4d2-124b3a43a590', ietf_openness_commitment__legitimacy_erosion_reading, coexists_with).
narrative_ontology:cs_axiom('f1e3a5dc-1c85-403e-b4d2-124b3a43a590', foundational, resource_advantage_shapes_standards).
narrative_ontology:cs_axiom_status(resource_advantage_shapes_standards, holdable).
narrative_ontology:cs_axiom_grounding('f1e3a5dc-1c85-403e-b4d2-124b3a43a590', resource_advantage_shapes_standards, empirically_contingent).
narrative_ontology:cs_axiom('f1e3a5dc-1c85-403e-b4d2-124b3a43a590', secondary, proprietary_extensions_create_lockin).
narrative_ontology:cs_axiom_status(proprietary_extensions_create_lockin, holdable).
narrative_ontology:cs_axiom_grounding('f1e3a5dc-1c85-403e-b4d2-124b3a43a590', proprietary_extensions_create_lockin, empirically_contingent).
narrative_ontology:cs_reference_frame('f1e3a5dc-1c85-403e-b4d2-124b3a43a590', open_process_with_resource_asymmetry).
narrative_ontology:cs_drift_state('f1e3a5dc-1c85-403e-b4d2-124b3a43a590', contemporary_platform_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f1e3a5dc-1c85-403e-b4d2-124b3a43a590', '').
narrative_ontology:cs_kernel_id(ietf_openness_commitment__capture_substrate_reading, ietf_openness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__capture_substrate_reading, large_platform_operators).
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__capture_substrate_reading, incumbent_vendors).
narrative_ontology:constraint_victim(ietf_openness_commitment__capture_substrate_reading, small_implementers).
narrative_ontology:constraint_victim(ietf_openness_commitment__capture_substrate_reading, end_users).
narrative_ontology:constraint_victim(ietf_openness_commitment__capture_substrate_reading, new_entrants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the development and adoption of technical standards for the internet, ensuring interoperability and a common technical foundation for diverse systems and applications.
% TRANSFER_FUNCTION: Transfers market advantage and de facto control over internet infrastructure from smaller, less resourced implementers and new entrants to large platform operators and incumbent vendors, through the encoding of their preferred technical approaches into 'open' standards.
% ABSENT_VOICES: New entrants and truly independent developers who lack the resources to participate consistently in IETF working groups are effectively absent. They would advocate for processes that genuinely level the playing field and prevent proprietary capture, but their voices are diluted by the well-resourced participants.
% DISAPPEARANCE_RATIONALE: If the IETF's openness commitment and its associated standards process vanished, the internet's interoperability would rapidly degrade. Dominant platforms would likely diverge with proprietary protocols, creating walled gardens and fragmenting the global network, forcing a reorganization of how digital services are built and accessed.
% FOUNDING_PROBLEM: The internet needed common, interoperable technical standards to function as a global network, preventing fragmentation and vendor lock-in, and enabling innovation from diverse participants.
% FOUNDING_PROBLEM_CORROBORATION: The IETF and large platform operators assert the problem is still live, citing the ongoing need for coordination. However, small implementers, new entrants, and some academic observers argue that while the *need* for standards is live, the *process* has been captured, and the original problem of preventing vendor lock-in is now exacerbated by the very mechanism meant to solve it. Independent research on market concentration and implementation divergence supports this contested status.
narrative_ontology:disappearance_verdict(ietf_openness_commitment__capture_substrate_reading, world_rearranges).
narrative_ontology:founding_problem_status(ietf_openness_commitment__capture_substrate_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ietf_openness_commitment__capture_substrate_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(ietf_openness_commitment__capture_substrate_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ietf_openness_commitment__capture_substrate_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ietf_openness_commitment__capture_substrate_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ietf_openness_commitment__capture_substrate_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is moderate because the standards are technically 'open,' but their practical implementation and evolution are heavily influenced by resource-rich actors. Suppression (0.45) is present through the high cost of participation and the 'running code' requirement, which favors those with existing infrastructure. Theater ratio (0.30) reflects the performative aspect of 'openness' that masks underlying capture. Accessibility collapse (0.40) is moderate; alternatives are not completely foreclosed but are made significantly harder to pursue. Resistance (0.55) comes from smaller players and advocates pushing for more genuinely open processes.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of large platform operators, the IETF process is a legitimate coordination mechanism for technical interoperability. From the perspective of small implementers and new entrants, it functions as a gatekeeping mechanism that entrenches incumbents. The engine's per-seat classification will reflect this divergence based on their declared power, exit options, and beneficiary/victim status.
 *
 * DIRECTIONALITY LOGIC:
 *   Large platform operators and incumbent vendors are beneficiaries (low d) as they can leverage their resources to shape standards to their advantage, effectively encoding their market power. Small implementers, end-users, and new entrants are victims (high d) as they bear the costs of adopting standards that favor incumbents, leading to lock-in or exclusion. The IETF working groups, while ostensibly neutral, act as agenda setters whose process can be influenced by well-resourced participants.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading suggests the IETF's openness commitment is a 'tangled rope' rather than a 'rope' or 'mountain.' It still provides a coordination function (interoperability), but this function is intertwined with asymmetric extraction. The 'mandate' of open standards has not atrophied, but its *function* has drifted from pure commons stewardship to a substrate for capture. Recognizing this prevents mislabeling it as a purely benign coordination mechanism or a natural outcome of technical evolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the IETF''s ''rough consensus and running code'' commitment primarily a commons stewardship mechanism, a substrate for capture, or an eroding legitimacy claim?',
    'Empirical analysis of standard adoption patterns, market concentration trends, and the success rate of proprietary extensions versus truly open implementations over time.',
    'If resolved as commons stewardship, the constraint would classify closer to a Rope; if as legitimacy erosion, closer to a Piton. This reading (capture substrate) implies a Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Ambiguity in the core function of the IETF''s openness commitment.').

omega_variable(
    proprietary_extension_detection,
    'To what extent are ''open standards'' being extended with proprietary features that create de facto vendor lock-in, and how much does this contribute to extraction?',
    'Technical audits of implementations, analysis of API usage, and market share data for proprietary extensions versus base standards.',
    'Higher incidence of proprietary extensions would increase measured extractiveness and suppression, pushing the classification further towards Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proprietary_extension_detection, empirical, 'Quantifying proprietary extensions as a mechanism of capture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ietf_openness_commitment__capture_substrate_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ietf_tr_t0, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(ietf_tr_t10, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(ietf_tr_t20, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(ietf_tr_t30, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 30, 0.3).

% Extraction over time
narrative_ontology:measurement(ietf_be_t0, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(ietf_be_t10, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(ietf_be_t20, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(ietf_be_t30, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 30, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(ietf_su_t0, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(ietf_su_t10, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 10, 0.35).
narrative_ontology:measurement(ietf_su_t20, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(ietf_su_t30, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 30, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ietf_openness_commitment__capture_substrate_reading, information_standard).
narrative_ontology:affects_constraint(ietf_openness_commitment__capture_substrate_reading, ietf_openness_commitment__commons_stewardship_reading).
narrative_ontology:affects_constraint(ietf_openness_commitment__capture_substrate_reading, ietf_openness_commitment__legitimacy_erosion_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'IETF openness commitment' kernel. It focuses on how resource advantages translate into encoded gatekeeping within the standards process, leading to a Tangled Rope classification. Sibling readings explore the commons stewardship and legitimacy erosion aspects.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
