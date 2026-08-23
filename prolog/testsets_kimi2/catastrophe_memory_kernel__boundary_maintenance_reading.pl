% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_kernel__boundary_maintenance_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_kernel__boundary_maintenance_reading, []).

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
 *   constraint_id: catastrophe_memory_kernel__boundary_maintenance_reading
 *   human_readable: Catastrophe Memory Ritual as Group Boundary Enforcement
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   A religious or ethnic community responds to a historical catastrophe
 *   through a shared mourning-practice that is presented as collective
 *   remembrance. In this reading, the ritual structurally enforces group
 *   boundaries: proper performance marks in-group membership, while deviation
 *   or out-group participation is treated as a threat to the memory's
 *   integrity. The constraint coordinates genuine solidarity but
 *   asymmetrically extracts individual autonomy and out-group relational
 *   capacity.
 *
 * KEY AGENTS:
 *   - community_leaders (agenda_setter/beneficiary): administer the ritual and interpret its boundaries
 *   - ingroup_members (beneficiary/payer): gain solidarity but bear conformity pressure
 *   - individual_dissenters (payer): lose autonomy to prescribed mourning forms
 *   - out_group_communities (payer): bear the cost of enforced separation
 *   - memory_studies_scholars (observer): analyze the ritual from an analytical seat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_kernel__boundary_maintenance_reading, 0.6).
domain_priors:suppression_score(catastrophe_memory_kernel__boundary_maintenance_reading, 0.65).
domain_priors:theater_ratio(catastrophe_memory_kernel__boundary_maintenance_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_kernel__boundary_maintenance_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_kernel__boundary_maintenance_reading, "Catastrophe Memory Ritual as Group Boundary Enforcement").
narrative_ontology:topic_domain(catastrophe_memory_kernel__boundary_maintenance_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_kernel__boundary_maintenance_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_kernel__boundary_maintenance_reading, '387dbe45-5c36-4d9a-a154-48ff1ca590d3').
narrative_ontology:cs_kernel_codification('387dbe45-5c36-4d9a-a154-48ff1ca590d3', fixed_text).
narrative_ontology:cs_authority_grounding('387dbe45-5c36-4d9a-a154-48ff1ca590d3', lineage).
narrative_ontology:cs_interpretation_layer_present('387dbe45-5c36-4d9a-a154-48ff1ca590d3').
narrative_ontology:cs_reading_relation('387dbe45-5c36-4d9a-a154-48ff1ca590d3', catastrophe_memory_kernel__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('387dbe45-5c36-4d9a-a154-48ff1ca590d3', catastrophe_memory_kernel__survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('387dbe45-5c36-4d9a-a154-48ff1ca590d3', catastrophe_memory_kernel__trauma_encoding_reading, coexists_with).
narrative_ontology:cs_axiom('387dbe45-5c36-4d9a-a154-48ff1ca590d3', foundational, group_boundaries_over_individual_expression).
narrative_ontology:cs_axiom_status(group_boundaries_over_individual_expression, holdable).
narrative_ontology:cs_axiom_grounding('387dbe45-5c36-4d9a-a154-48ff1ca590d3', group_boundaries_over_individual_expression, conventional).
narrative_ontology:cs_reference_frame('387dbe45-5c36-4d9a-a154-48ff1ca590d3', post_catastrophe_community_integrity).
narrative_ontology:cs_drift_state('387dbe45-5c36-4d9a-a154-48ff1ca590d3', contemporary_generational_remove, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('387dbe45-5c36-4d9a-a154-48ff1ca590d3', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_kernel__boundary_maintenance_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__boundary_maintenance_reading, community_leaders).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__boundary_maintenance_reading, ingroup_members).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__boundary_maintenance_reading, individual_dissenters).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__boundary_maintenance_reading, out_group_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__boundary_maintenance_reading, ingroup_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the shared mourning-practice, set the norms for correct participation, and interpret the tradition's boundaries. Their authority and social standing depend on the ritual's continued performance and on maintaining a clear distinction between proper members and outsiders or dissenters.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, community_leaders, agenda_setter,
    organized, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_kernel__boundary_maintenance_reading, community_leaders, beneficiary).

% Receive belonging, solidarity, and a structured way to process catastrophe through the ritual. In exchange they bear conformity pressure: their mourning must follow the group's script, and visible deviation risks marginalization within the community.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, ingroup_members, beneficiary,
    moderate, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_kernel__boundary_maintenance_reading, ingroup_members, payer).

% Bear the cost of suppressed individual expression in grief. They are pressured to perform the collective mourning-practice even when it conflicts with personal experience, and face social sanctions if they question the ritual's necessity or form.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, individual_dissenters, payer,
    powerless, biographical, constrained, regional).

% Bear the cost of reinforced separation. The ritual's boundary-work treats them as symbolic outsiders whose presence or participation would dilute the group's catastrophe memory, limiting inter-group contact and cooperation.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, out_group_communities, payer,
    moderate, generational, constrained, regional).

% Study how catastrophe rituals function across cultures, observing the tension between solidarity-building and boundary-enforcement without being subject to the ritual's social pressures themselves.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, memory_studies_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_kernel__boundary_maintenance_reading, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_memory_kernel__boundary_maintenance_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of maintaining group identity and social solidarity after a catastrophe by providing a shared, embodied practice that signals membership and collective commitment.
% TRANSFER_FUNCTION: Moves conformity pressure and symbolic capital from individual dissenters and out-group communities to the in-group collective and its leadership, extracting individual autonomy and cross-group relational capacity in exchange for belonging.
% ABSENT_VOICES: Individual members who experience the catastrophe differently and question the prescribed mourning form, out-group communities excluded by the ritual's boundary logic, and universalist voices who would advocate for non-exclusive commemoration are not present in the ritual's design.
% DISAPPEARANCE_RATIONALE: If the shared mourning-practice vanished overnight, the group's primary mechanism for enforcing in-group/out-group distinctions would collapse; solidarity would have to find alternative channels, and the specific social boundary maintained by the ritual would erode, rearranging community-individual and inter-community relations.
% FOUNDING_PROBLEM: How to maintain group cohesion and a recognizable collective identity after a catastrophe that could fragment the community or dissolve its boundaries with outsiders.
% FOUNDING_PROBLEM_CORROBORATION: Community historians and religious leaders inside the tradition attest that the catastrophe threatened communal dissolution. Secular historians and out-group observers argue the original survival threat has passed and the ritual now serves primarily to consolidate authority and exclude outsiders rather than respond to an active cohesion problem.
narrative_ontology:disappearance_verdict(catastrophe_memory_kernel__boundary_maintenance_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_kernel__boundary_maintenance_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_kernel__boundary_maintenance_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_kernel__boundary_maintenance_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_kernel__boundary_maintenance_reading, 0.6, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_kernel__boundary_maintenance_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_kernel__boundary_maintenance_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_kernel__boundary_maintenance_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.60) because the ritual genuinely produces solidarity while also enforcing costly conformity. Suppression (0.65) reflects the active social pressure required to maintain boundary distinctions and exclude alternative mourning expressions. Theater ratio (0.55) rises over the interval as the catastrophe recedes from living memory and the ritual's performative boundary-function eclipses its original grief-processing function. Accessibility collapse is high (0.75) because once a member is embedded in the community, the social cost of exiting or challenging the ritual is nearly prohibitive. Resistance is moderate (0.40) because dissent exists but is routinely absorbed through social sanction.
 *
 * PERSPECTIVAL GAP:
 *   From the community leaders' seat the ritual is necessary continuity that holds the group together; from the dissenters' and out-groups' seats the same structure operates as enforced exclusion dressed in the language of memory. The engine computes this divergence from the structural data rather than the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Community leaders and ingroup members are beneficiaries (low d) because the constraint subsidizes their solidarity and social identity. Individual dissenters and out-group communities are victims (high d) because the constraint extracts their autonomy and relational options. The identity_locked exit of ingroup members amplifies effective extraction for dissenters trapped within the same communal scope.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as tangled_rope prevents misreading the solidarity function as pure extraction (snare) or the boundary enforcement as pure coordination (rope). The genuine coordination of grief and the asymmetric extraction of autonomy are structurally fused in the same ritual act; neither can be understood without the other.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_sibling_ambiguity,
    'Does the shared mourning-practice primarily enforce group boundaries (this reading), or is the boundary effect an epiphenomenon of trauma encoding (trauma_encoding_reading), survival competence transmission (survival_competence_reading), or symbolic continuity (symbol_continuity_reading)?',
    'Comparative ethnographic analysis of ritual variation: if boundary-marking elements persist even when the catastrophe context changes, boundary maintenance is likely a primary rather than derivative function.',
    'If boundary maintenance is derivative, this reading overstates extractiveness and the constraint may reclassify toward rope; if primary, the moderate extraction score is vindicated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_sibling_ambiguity, conceptual, 'Uncertainty about whether boundary enforcement is the primary or derivative function of the ritual').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the conformity pressure in the ritual structural (external sanctions for non-performance) or internalized (members believe deviation is morally wrong or dangerous)?',
    'Post-exit trajectory study: if suppression of dissent persists after the extractive structure is removed, reclassify as partially internalized.',
    'If internalized, effective suppression is higher than structural measures suggest, raising the extraction experienced by individual dissenters.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism in ritual conformity').

omega_variable(
    boundary_maintenance_adaptive_or_extractive,
    'Can the genuine solidarity and grief-processing function of the ritual be separated from the boundary-exclusion mechanism, or are they structurally fused?',
    'Natural experiment or comparative case analysis where communities maintain catastrophe memory without ritual exclusion; if solidarity holds without boundary enforcement, the functions are separable.',
    'If separable, the boundary mechanism is extractive overhead riding on genuine coordination; if fused, the extraction is an inherent cost of the coordination type.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(boundary_maintenance_adaptive_or_extractive, conceptual, 'Whether solidarity and boundary enforcement are structurally separable').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_kernel__boundary_maintenance_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(cata_tr_t5, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 5, 0.35).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 10, 0.4).
narrative_ontology:measurement(cata_tr_t15, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 15, 0.45).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 20, 0.48).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 30, 0.52).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 40, 0.55).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(cata_be_t5, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(cata_be_t15, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 15, 0.53).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 20, 0.56).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 40, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(cata_su_t5, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 5, 0.53).
narrative_ontology:measurement(cata_su_t10, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(cata_su_t15, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 15, 0.58).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(cata_su_t30, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 30, 0.63).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 40, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_kernel__boundary_maintenance_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__boundary_maintenance_reading, symbol_continuity_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__boundary_maintenance_reading, survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__boundary_maintenance_reading, trauma_encoding_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the catastrophe_memory_kernel. The kernel decomposes into four structurally distinct constraints because the attributed function of the ritual (boundary maintenance, symbol continuity, survival competence, trauma encoding) changes the epsilon, beneficiary structure, and directionality of the same practice.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
