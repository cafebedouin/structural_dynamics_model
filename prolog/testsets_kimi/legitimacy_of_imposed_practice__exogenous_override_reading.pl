% ============================================================================
% CONSTRAINT STORY: legitimacy_of_imposed_practice__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_imposed_practice__exogenous_override_reading, []).

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
 *   constraint_id: legitimacy_of_imposed_practice__exogenous_override_reading
 *   human_readable: State Decree Override of Traditional Practice
 *   domain: political/historical/cultural
 *
 * SUMMARY:
 *   This constraint instantiates the exogenous_override_reading of the
 *   legitimacy_of_imposed_practice kernel: the claim that state decree
 *   authority is sufficient to displace prior practice, and that compliance
 *   follows from legal mandate regardless of internalization. The reading
 *   treats calendar abolition and dress reform as achievements of centralized
 *   command, acknowledging rural non-compliance and workarounds as residual
 *   friction rather than structural failure. Sibling readings
 *   (endogenous_climb_reading, hybrid_scaffolding_reading) handle bottom-up
 *   adoption and mixed mechanisms; this story isolates the pure decree claim
 *   as a separate, epsilon-invariant constraint.
 *
 * KEY AGENTS:
 *   - central_state: Agenda-setter and beneficiary (institutional/constrained) â enforces decree and captures authority gains.
 *   - rural_communities: Primary payer (powerless/constrained) â bear adjustment costs and maintain covert resistance.
 *   - traditional_authorities: Excluded voice (moderate/constrained) â displaced jurisdiction, absent from policy design.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_imposed_practice__exogenous_override_reading, 0.72).
domain_priors:suppression_score(legitimacy_of_imposed_practice__exogenous_override_reading, 0.76).
domain_priors:theater_ratio(legitimacy_of_imposed_practice__exogenous_override_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_imposed_practice__exogenous_override_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_imposed_practice__exogenous_override_reading, "State Decree Override of Traditional Practice").
narrative_ontology:topic_domain(legitimacy_of_imposed_practice__exogenous_override_reading, "political/historical/cultural").

domain_priors:requires_active_enforcement(legitimacy_of_imposed_practice__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_imposed_practice__exogenous_override_reading, 'be4785af-feae-4722-bfe3-22a0d168d631').
narrative_ontology:cs_kernel_codification('be4785af-feae-4722-bfe3-22a0d168d631', formalized).
narrative_ontology:cs_authority_grounding('be4785af-feae-4722-bfe3-22a0d168d631', extraction).
narrative_ontology:cs_interpretation_layer_present('be4785af-feae-4722-bfe3-22a0d168d631').
narrative_ontology:cs_reading_relation('be4785af-feae-4722-bfe3-22a0d168d631', legitimacy_of_imposed_practice__endogenous_climb_reading, forecloses).
narrative_ontology:cs_reading_relation('be4785af-feae-4722-bfe3-22a0d168d631', legitimacy_of_imposed_practice__hybrid_scaffolding_reading, forecloses).
narrative_ontology:cs_axiom('be4785af-feae-4722-bfe3-22a0d168d631', foundational, state_decree_supreme_over_local_practice).
narrative_ontology:cs_axiom_status(state_decree_supreme_over_local_practice, holdable).
narrative_ontology:cs_axiom_grounding('be4785af-feae-4722-bfe3-22a0d168d631', state_decree_supreme_over_local_practice, conventional).
narrative_ontology:cs_axiom('be4785af-feae-4722-bfe3-22a0d168d631', foundational, compliance_independent_of_internalization).
narrative_ontology:cs_axiom_status(compliance_independent_of_internalization, holdable).
narrative_ontology:cs_axiom_grounding('be4785af-feae-4722-bfe3-22a0d168d631', compliance_independent_of_internalization, empirically_contingent).
narrative_ontology:cs_reference_frame('be4785af-feae-4722-bfe3-22a0d168d631', centralized_decree_supremacy).
narrative_ontology:cs_drift_state('be4785af-feae-4722-bfe3-22a0d168d631', post_implementation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('be4785af-feae-4722-bfe3-22a0d168d631', '').
narrative_ontology:cs_kernel_id(legitimacy_of_imposed_practice__exogenous_override_reading, legitimacy_of_imposed_practice).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__exogenous_override_reading, central_state).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__exogenous_override_reading, rural_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues and enforces legal decrees abolishing traditional calendars, dress codes, and customary rituals. Frames compliance as modernization and legal obligation. Benefits from expanded territorial administration, tax registration, and symbolic homogenization. Enforcement is coercive and centralized.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, central_state, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_imposed_practice__exogenous_override_reading, central_state, beneficiary).

% Bear the costs of abandoning long-established seasonal calendars, attire, and ceremonial practices. Subject to legal penalties and administrative surveillance for non-compliance. Engage in passive resistance, practical workarounds, and covert maintenance of prior practice where enforcement is lax.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, rural_communities, payer,
    powerless, biographical, constrained, regional).

% Local elders, religious leaders, and customary courts whose jurisdiction is superseded by state decree. Would articulate the legitimacy of inherited practice but are structurally excluded from the modernizing legal framework and treated as obstacles to progress.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, traditional_authorities, excluded,
    moderate, biographical, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimacy_of_imposed_practice__exogenous_override_reading, central_state).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates a uniform national legal and cultural framework, overriding regional fragmentation with centralized standards for timekeeping, attire, and public ritual.
% TRANSFER_FUNCTION: Moves compliance burden and cultural adjustment costs from the state to rural populations; transfers symbolic authority from local traditional structures to central state institutions.
% ABSENT_VOICES: Traditional authorities and rural communities themselves are excluded from the policy design process; their objections are treated as backwardness or illegality rather than input.
% DISAPPEARANCE_RATIONALE: If the decree authority vanished, rural communities would revert to prior practices, the state's territorial-cultural homogenization project would stall, and local traditional authorities would regain symbolic jurisdiction â the political-cultural order would rearrange.
% FOUNDING_PROBLEM: Political fragmentation under empire or early nation-state, where diverse local practices impede centralized taxation, military conscription, legal uniformity, and national identity formation.
% FOUNDING_PROBLEM_CORROBORATION: State historians attest the fragmentation problem; anthropologists and post-colonial historians outside the beneficiary set attest that local practices were functional governance systems and the problem was a projection of central state categories. No uncontested corroboration exists.
narrative_ontology:disappearance_verdict(legitimacy_of_imposed_practice__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_imposed_practice__exogenous_override_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_imposed_practice__exogenous_override_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legitimacy_of_imposed_practice__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_imposed_practice__exogenous_override_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_imposed_practice__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimacy_of_imposed_practice__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimacy_of_imposed_practice__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72) because rural populations bear cultural and economic adjustment costs without consultation or compensation. Suppression is higher (0.76) because the constraint's persistence depends on active enforcement against non-compliance and workarounds. Theater_ratio is moderate (0.42): enforcement is partly functional (legal compliance is real in public spaces) and partly performative (state assertions of control where covert practice persists). Accessibility_collapse is substantial (0.68) because legal abolition formally removes alternatives, yet practical workarounds keep them alive underground. Resistance is moderate (0.52) because rural non-compliance is diffuse rather than organized. The measurement series share one time grid to prevent misaligned substitution.
 *
 * PERSPECTIVAL GAP:
 *   From the central_state seat, the arrangement is necessary modernization and territorial integration; from the rural_communities seat, the same structure is coercive extraction of cultural autonomy. The engine computes this divergence from the structural data â the agenda_setter benefits while the payer bears costs with constrained exit.
 *
 * DIRECTIONALITY LOGIC:
 *   The central_state sits near the beneficiary end: it collects territorial authority, tax registration, and symbolic homogenization. Rural_communities sit near the target end: they pay the costs and have limited exit (geographically trapped within the national jurisdiction, culturally constrained by identity-fusion with prior practice). Traditional_authorities are excluded â their authority is extracted and transferred to state institutions, giving them high derived directionality despite moderate nominal power.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by requiring both a coordination function (national legal uniformity) and an identifiable victim set (rural communities). Without declared victims, the high suppression and enforcement might be misread as a scaffold or rope serving modernization; with them, the asymmetric extraction is structurally visible as a tangled_rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'Is the exogenous_override_reading a descriptively accurate model of state capacity, or a post-hoc ideological rationalization that obscures hybrid enforcement mechanisms?',
    'Archival analysis of state campaign records to detect whether ideological scaffolding and local co-optation were present alongside decree in cases claimed as pure override.',
    'If hybrid mechanisms are always present, this reading collapses into a false theoretical construct (snare or tangled rope legitimized as coordination) and the kernel should be reclassified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Structural position of this reading within the legitimacy_of_imposed_practice kernel.').

omega_variable(
    domain_heterogeneity,
    'Does the exogenous_override reading apply uniformly across all practice domains (calendar, dress, ritual), or does its sufficiency vary by domain?',
    'Decompose into per-domain constraint stories and measure differential extraction and suppression.',
    'If sufficiency varies by domain, the reading is overgeneralized and should split into multiple epsilon-invariant constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domain_heterogeneity, conceptual, 'Whether the reading''s claim holds across all domains of imposed practice.').

omega_variable(
    state_beneficiary_concentration,
    'Does the extraction from rural communities concentrate to the central state apparatus, or diffuse among urban modernizers without a single capturer?',
    'Trace resource flows and authority gains: tax revenue, conscription yields, symbolic legitimacy accrual.',
    'If diffuse, gain_flow should be revised to ''diffuse'', pushing classification toward piton or requiring redistribution analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_beneficiary_concentration, empirical, 'Whether the state or a broader modernizing coalition captures the extracted compliance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_imposed_practice__exogenous_override_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(legi_tr_t4, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 4, 0.35).
narrative_ontology:measurement(legi_tr_t8, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 8, 0.4).
narrative_ontology:measurement(legi_tr_t12, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 12, 0.45).
narrative_ontology:measurement(legi_tr_t16, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 16, 0.47).
narrative_ontology:measurement(legi_tr_t20, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 20, 0.42).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(legi_be_t4, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 4, 0.66).
narrative_ontology:measurement(legi_be_t8, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 8, 0.71).
narrative_ontology:measurement(legi_be_t12, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 12, 0.74).
narrative_ontology:measurement(legi_be_t16, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 16, 0.73).
narrative_ontology:measurement(legi_be_t20, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 20, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(legi_su_t4, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 4, 0.7).
narrative_ontology:measurement(legi_su_t8, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 8, 0.78).
narrative_ontology:measurement(legi_su_t12, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 12, 0.8).
narrative_ontology:measurement(legi_su_t16, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 16, 0.77).
narrative_ontology:measurement(legi_su_t20, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 20, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_imposed_practice__exogenous_override_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__exogenous_override_reading, endogenous_climb_reading).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__exogenous_override_reading, hybrid_scaffolding_reading).

% DUAL FORMULATION NOTE:
% This constraint is the exogenous_override_reading of the legitimacy_of_imposed_practice kernel. It decomposes from the colloquial concept of state-imposed cultural change by isolating the claim that decree alone suffices. Sibling readings handle endogenous adoption and hybrid scaffolding.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
