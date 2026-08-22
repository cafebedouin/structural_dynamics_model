% ============================================================================
% CONSTRAINT STORY: state_commitment_installation_mechanism__exogenous_imposition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_commitment_installation_mechanism__exogenous_imposition_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: state_commitment_installation_mechanism__exogenous_imposition_reading
 *   human_readable: State Commitment Installation via Exogenous Imposition
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   This constraint models the exogenous imposition reading of the state
 *   commitment installation mechanism kernel. The reading holds that new
 *   commitments gain legitimacy through top-down installation by an authority
 *   holding a transformation mandate — the state installs, the population
 *   complies, legitimacy follows from successful enforcement. The structural
 *   delta: state as primary beneficiary, no grassroots advocacy, abrupt
 *   adoption via decree, resistance at the base. This is one of three
 *   readings of the kernel; the others are endogenous climb (commitments
 *   climb from fringes through demonstrated superiority) and hybrid cascade
 *   (apex installation requires fringe validation to stabilize).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_commitment_installation_mechanism__exogenous_imposition_reading, 0.78).
domain_priors:suppression_score(state_commitment_installation_mechanism__exogenous_imposition_reading, 0.82).
domain_priors:theater_ratio(state_commitment_installation_mechanism__exogenous_imposition_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_commitment_installation_mechanism__exogenous_imposition_reading, snare).
narrative_ontology:human_readable(state_commitment_installation_mechanism__exogenous_imposition_reading, "State Commitment Installation via Exogenous Imposition").
narrative_ontology:topic_domain(state_commitment_installation_mechanism__exogenous_imposition_reading, "historical_sociology/state_formation/cultural_authority").

domain_priors:requires_active_enforcement(state_commitment_installation_mechanism__exogenous_imposition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_commitment_installation_mechanism__exogenous_imposition_reading, '7f65015a-ea36-425d-bc0e-10e960caa443').
narrative_ontology:cs_kernel_codification('7f65015a-ea36-425d-bc0e-10e960caa443', implicit).
narrative_ontology:cs_authority_grounding('7f65015a-ea36-425d-bc0e-10e960caa443', extraction).
narrative_ontology:cs_interpretation_layer_present('7f65015a-ea36-425d-bc0e-10e960caa443').
narrative_ontology:cs_reading_relation('7f65015a-ea36-425d-bc0e-10e960caa443', state_commitment_installation_mechanism__endogenous_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('7f65015a-ea36-425d-bc0e-10e960caa443', state_commitment_installation_mechanism__hybrid_cascade_reading, influences).
narrative_ontology:cs_axiom('7f65015a-ea36-425d-bc0e-10e960caa443', foundational, mandate_authorizes_rupture).
narrative_ontology:cs_axiom_status(mandate_authorizes_rupture, holdable).
narrative_ontology:cs_axiom_grounding('7f65015a-ea36-425d-bc0e-10e960caa443', mandate_authorizes_rupture, instrumental).
narrative_ontology:cs_axiom('7f65015a-ea36-425d-bc0e-10e960caa443', foundational, installation_creates_legitimacy).
narrative_ontology:cs_axiom_status(installation_creates_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('7f65015a-ea36-425d-bc0e-10e960caa443', installation_creates_legitimacy, conventional).
narrative_ontology:cs_reference_frame('7f65015a-ea36-425d-bc0e-10e960caa443', pre_mandate_traditional_order).
narrative_ontology:cs_drift_state('7f65015a-ea36-425d-bc0e-10e960caa443', post_installation_consolidation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('7f65015a-ea36-425d-bc0e-10e960caa443', '').
narrative_ontology:cs_kernel_id(state_commitment_installation_mechanism__exogenous_imposition_reading, state_commitment_installation_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__exogenous_imposition_reading, transforming_state).
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__exogenous_imposition_reading, mandate_holding_authority).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__exogenous_imposition_reading, subject_population).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__exogenous_imposition_reading, traditional_elites).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__exogenous_imposition_reading, local_institutions).
narrative_ontology:constraint_vindicates(state_commitment_installation_mechanism__exogenous_imposition_reading, transformation_mandate_legitimacy).
narrative_ontology:constraint_vindicates(state_commitment_installation_mechanism__exogenous_imposition_reading, revolutionary_break_necessity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Wields transformation mandate to install new commitments by decree. Controls enforcement apparatus, defines legitimate discourse, captures the rents of social reorganization. Exit means regime collapse — not a live option from inside the apparatus.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, transforming_state, agenda_setter,
    institutional, generational, arbitrage, national).

% The specific office, party, or revolutionary council that holds the transformation mandate. Gains concentrated authority, resource control, and historical legitimacy from successful installation. Its interests fuse with the transforming state but can diverge on pace and scope.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, mandate_holding_authority, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(state_commitment_installation_mechanism__exogenous_imposition_reading, mandate_holding_authority, agenda_setter).

% Bears the costs of abrupt adoption: disrupted livelihoods, suppressed traditions, coerced participation in new rituals, punishment for non-compliance. No organized voice, no exit — geographic, economic, and identity-bound to the territory.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, subject_population, payer,
    powerless, biographical, trapped, local).

% Formerly legitimate authorities (religious, aristocratic, communal) whose status and resources are stripped by the installation. They resist where they can, accommodate where they must, and are excluded from the new legitimate discourse. Exit means exile, conversion, or quiet disappearance.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, traditional_elites, payer,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(state_commitment_installation_mechanism__exogenous_imposition_reading, traditional_elites, excluded).

% Village councils, guilds, parish structures, kinship networks — the mesh of everyday governance. They are hollowed out, repurposed, or replaced by state-administered forms. Their compliance is extracted; their autonomy is the price of the installation.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, local_institutions, payer,
    moderate, generational, constrained, local).

% Analyze the mechanism across cases (French Revolution, Meiji Restoration, Soviet collectivization, Cultural Revolution, post-colonial state-building). They see the structural pattern: mandate + enforcement + extraction + suppression of alternatives. Their seat has no stake in the outcome.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, historical_sociologists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of legitimate rupture: how a society can break with its past commitments and adopt a new normative order without dissolving into anarchy. The mandate provides the warrant; the installation provides the mechanism.
% TRANSFER_FUNCTION: Moves legitimacy, resources, and compliance from traditional elites and local institutions to the transforming state and its mandate-holding authority. The subject population pays in disrupted lives, suppressed culture, and coerced participation.
% ABSENT_VOICES: The subject population at the moment of installation — they are the ones acted upon, not consulted. Pre-installation dissidents are suppressed; post-installation objectors are re-educated or eliminated. No seat at the table exists for those the commitment is installed upon.
% DISAPPEARANCE_RATIONALE: If the installation mechanism vanished, the new commitments would lose their enforcement backbone. Traditional elites and local institutions would reclaim authority in their domains. The subject population would revert to prior practices where possible. The transforming state would lose its primary legitimating tool and face crisis of authority.
% FOUNDING_PROBLEM: How to legitimate a total break with the past when the past's legitimacy structures are precisely what must be overcome. The transformation mandate claims to solve this by authorizing the break itself — the mandate IS the new legitimacy.
% FOUNDING_PROBLEM_CORROBORATION: Revolutionary leaders and state-builders attest the mandate remains necessary (founding problem live). Historical sociologists of the revisionist tradition (Skocpol, Tilly, Mann) attest the founding problem is often constructed — the mandate creates the rupture it claims to heal. No corroboration outside the benefiting parties for the mandate's self-justification.
narrative_ontology:disappearance_verdict(state_commitment_installation_mechanism__exogenous_imposition_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_commitment_installation_mechanism__exogenous_imposition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_commitment_installation_mechanism__exogenous_imposition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(state_commitment_installation_mechanism__exogenous_imposition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_commitment_installation_mechanism__exogenous_imposition_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_commitment_installation_mechanism__exogenous_imposition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_commitment_installation_mechanism__exogenous_imposition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_commitment_installation_mechanism__exogenous_imposition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   High extractiveness (0.78): the installation transfers legitimacy, resources, and autonomy from traditional structures to the state. High suppression (0.82): persistence depends on actively suppressing alternatives (traditional elites, local institutions, dissenting voices). Moderate theater (0.38): the mandate's ideological apparatus performs legitimacy work, but enforcement is the real backbone. Accessibility collapse (0.71): once the installation succeeds, alternatives are largely foreclosed — the new commitments become the only legitimate framework. Resistance (0.68): substantial and persistent from traditional elites and subject population, requiring continuous enforcement.
 *
 * PERSPECTIVAL GAP:
 *   From the state's seat, the mechanism is genuine coordination solving the rupture problem. From the subject population's seat, it is pure extraction enforced by violence. From traditional elites' seat, it is dispossession. The engine computes this divergence from the structural data — the claimed type (snare) reflects the victim-weighted view, while the state's self-perception would compute as rope or scaffold.
 *
 * DIRECTIONALITY LOGIC:
 *   Transforming state and mandate-holding authority are structural beneficiaries (d near 0.0): they collect legitimacy rents, resource control, and authority concentration. Subject population, traditional elites, and local institutions are targets (d near 1.0): they bear extraction, lose autonomy, face coercion. Exit options differentiate: subject population is trapped (geographic/identity bound), traditional elites constrained (some mobility, some conversion), local institutions constrained (some adaptation possible). The analytical observer sits at d=0.5 (symmetric).
 *
 * MANDATROPHY ANALYSIS:
 *   The transformation mandate's founding problem (legitimating rupture) is contested as live vs. constructed. If the mandate persists after the rupture is consolidated, it becomes a piton or snare — the coordination function (solving rupture) is achieved, but the extraction machinery remains. The measurement series shows extractiveness rising then plateauing, theater rising, suppression staying high — consistent with coordination function achieved but extraction machinery persisting.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structure,
    'Does the exogenous imposition reading''s core premise (legitimacy derives from top-down installation by mandate-holding authority) logically foreclose the endogenous climb reading (legitimacy derives from demonstrated superiority at fringes), or do they coexist as competing explanations for different historical cases?',
    'Comparative case analysis: if the same historical case can be plausibly read both ways, they coexist; if the readings make mutually exclusive claims about the same mechanism in the same case, foreclosure is possible.',
    'If forecloses, the kernel has a structural fault line — only one reading can be structurally true per case. If coexists_with, the kernel admits multiple legitimate mechanisms and the classification depends on which reading''s structural conditions obtain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_structure, conceptual, 'Structural relationship between exogenous imposition and endogenous climb readings of the same kernel').

omega_variable(
    mandate_authenticity,
    'Is the transformation mandate a genuine authorization from a recognized source (popular sovereignty, divine will, historical necessity) or a post-hoc justification constructed by the installing authority?',
    'Genealogical analysis of mandate claims across cases: trace the mandate''s claimed source, its recognition by third parties, and its durability after installation.',
    'If genuine authorization, the mechanism has a coordination function (solving the rupture problem) alongside extraction — potentially tangled_rope. If constructed justification, the mechanism is pure snare with ideological cover.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mandate_authenticity, conceptual, 'Whether the transformation mandate is structurally authentic or constructed cover').

omega_variable(
    suppression_internalization,
    'Is the measured suppression (0.82) primarily structural (enforcement apparatus, legal penalties, resource control) or partially internalized (subject population accepts the new commitments as legitimate through socialization, education, identity fusion)?',
    'Post-installation trajectory analysis: if suppression requirements decay as internalized legitimacy grows, the constraint shifts from snare toward rope/scaffold. If suppression stays high despite generations of socialization, structural suppression dominates.',
    'If substantially internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression forward. Classification may shift from snare to tangled_rope if internalization creates genuine coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Structural vs. internalized suppression mechanism in exogenous installation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_commitment_installation_mechanism__exogenous_imposition_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(state_commitment_installation_mechanism__exogenous_imposition_reading_tr_t0, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(state_commitment_installation_mechanism__exogenous_imposition_reading_tr_t10, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement(state_commitment_installation_mechanism__exogenous_imposition_reading_tr_t20, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 20, 0.36).
narrative_ontology:measurement(state_commitment_installation_mechanism__exogenous_imposition_reading_tr_t30, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement(state_commitment_installation_mechanism__exogenous_imposition_reading_tr_t40, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement(state_commitment_installation_mechanism__exogenous_imposition_reading_tr_t50, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 50, 0.38).

% Extraction over time
narrative_ontology:measurement(state_commitment_installation_mechanism__exogenous_imposition_reading_be_t0, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(state_commitment_installation_mechanism__exogenous_imposition_reading_be_t10, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 10, 0.72).
narrative_ontology:measurement(state_commitment_installation_mechanism__exogenous_imposition_reading_be_t20, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 20, 0.76).
narrative_ontology:measurement(state_commitment_installation_mechanism__exogenous_imposition_reading_be_t30, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 30, 0.78).
narrative_ontology:measurement(state_commitment_installation_mechanism__exogenous_imposition_reading_be_t40, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 40, 0.77).
narrative_ontology:measurement(state_commitment_installation_mechanism__exogenous_imposition_reading_be_t50, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 50, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(state_commitment_installation_mechanism__exogenous_imposition_reading_su_t0, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(state_commitment_installation_mechanism__exogenous_imposition_reading_su_t10, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 10, 0.82).
narrative_ontology:measurement(state_commitment_installation_mechanism__exogenous_imposition_reading_su_t20, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 20, 0.81).
narrative_ontology:measurement(state_commitment_installation_mechanism__exogenous_imposition_reading_su_t30, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 30, 0.82).
narrative_ontology:measurement(state_commitment_installation_mechanism__exogenous_imposition_reading_su_t40, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 40, 0.8).
narrative_ontology:measurement(state_commitment_installation_mechanism__exogenous_imposition_reading_su_t50, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 50, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_commitment_installation_mechanism__exogenous_imposition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__exogenous_imposition_reading, state_commitment_installation_mechanism__endogenous_climb_reading).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__exogenous_imposition_reading, state_commitment_installation_mechanism__hybrid_cascade_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the kernel 'state_commitment_installation_mechanism' into three readings with distinct ε values and beneficiary/victim structures. Exogenous imposition (this story) has high ε (0.78), state as beneficiary, subject population as victim. Endogenous climb would have lower ε, fringe actors as beneficiaries, traditional center as victim. Hybrid cascade would have intermediate ε, dual beneficiary structure (apex + validated fringe).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(state_commitment_installation_mechanism__exogenous_imposition_reading, institutional, 0.05).
constraint_indexing:directionality_override(state_commitment_installation_mechanism__exogenous_imposition_reading, powerless, 0.95).
constraint_indexing:directionality_override(state_commitment_installation_mechanism__exogenous_imposition_reading, organized, 0.85).
constraint_indexing:directionality_override(state_commitment_installation_mechanism__exogenous_imposition_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
