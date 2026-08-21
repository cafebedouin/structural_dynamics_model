% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_kernel__symbol_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_kernel__symbol_continuity_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: catastrophe_memory_kernel__symbol_continuity_reading
 *   human_readable: Ritual Preserves Symbolic Continuity and Collective Identity (Symbol Continuity Reading)
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint describes the function of ritual in preserving symbolic
 *   continuity and collective identity, particularly in the context of
 *   remembering historical catastrophes. It is one reading of the
 *   'catastrophe_memory_kernel', focusing on the transmission of meaning and
 *   identity across generations. The ritual acts as a 'rope' by coordinating
 *   collective memory, but its emphasis on continuity can impose costs on
 *   adaptive modification, making it a D1/D4 constraint (identity-marker with
 *   rigidity costs).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_kernel__symbol_continuity_reading, 0.25).
domain_priors:suppression_score(catastrophe_memory_kernel__symbol_continuity_reading, 0.4).
domain_priors:theater_ratio(catastrophe_memory_kernel__symbol_continuity_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_kernel__symbol_continuity_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_kernel__symbol_continuity_reading, "Ritual Preserves Symbolic Continuity and Collective Identity (Symbol Continuity Reading)").
narrative_ontology:topic_domain(catastrophe_memory_kernel__symbol_continuity_reading, "religious_studies/collective_memory/ritual_practice").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_kernel__symbol_continuity_reading, '9f6a5de9-b028-4769-88be-9e9b00c424d0').
narrative_ontology:cs_kernel_codification('9f6a5de9-b028-4769-88be-9e9b00c424d0', implicit).
narrative_ontology:cs_authority_grounding('9f6a5de9-b028-4769-88be-9e9b00c424d0', practice).
narrative_ontology:cs_interpretation_layer_present('9f6a5de9-b028-4769-88be-9e9b00c424d0').
narrative_ontology:cs_reading_relation('9f6a5de9-b028-4769-88be-9e9b00c424d0', catastrophe_memory_kernel__survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('9f6a5de9-b028-4769-88be-9e9b00c424d0', catastrophe_memory_kernel__trauma_encoding_reading, coexists_with).
narrative_ontology:cs_reading_relation('9f6a5de9-b028-4769-88be-9e9b00c424d0', catastrophe_memory_kernel__boundary_maintenance_reading, coexists_with).
narrative_ontology:cs_axiom('9f6a5de9-b028-4769-88be-9e9b00c424d0', foundational, collective_identity_requires_unbroken_symbolic_chain).
narrative_ontology:cs_axiom_status(collective_identity_requires_unbroken_symbolic_chain, holdable).
narrative_ontology:cs_axiom_grounding('9f6a5de9-b028-4769-88be-9e9b00c424d0', collective_identity_requires_unbroken_symbolic_chain, deontological).
narrative_ontology:cs_axiom('9f6a5de9-b028-4769-88be-9e9b00c424d0', secondary, ritual_form_is_vehicle_for_symbolic_transmission).
narrative_ontology:cs_axiom_status(ritual_form_is_vehicle_for_symbolic_transmission, holdable).
narrative_ontology:cs_axiom_grounding('9f6a5de9-b028-4769-88be-9e9b00c424d0', ritual_form_is_vehicle_for_symbolic_transmission, conventional).
narrative_ontology:cs_reference_frame('9f6a5de9-b028-4769-88be-9e9b00c424d0', unbroken_symbolic_transmission).
narrative_ontology:cs_drift_state('9f6a5de9-b028-4769-88be-9e9b00c424d0', contemporary_globalized_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('9f6a5de9-b028-4769-88be-9e9b00c424d0', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_kernel__symbol_continuity_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__symbol_continuity_reading, community_members).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__symbol_continuity_reading, tradition_continuity).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__symbol_continuity_reading, adaptive_modification).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participate in rituals that reinforce their collective identity and connection to a shared past, particularly in the face of historical catastrophe. They derive a sense of belonging and meaning from this continuity, but may experience friction when ritual forms resist adaptation to contemporary needs.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, community_members, beneficiary,
    organized, generational, identity_locked, local).

% The abstract concept of an unbroken chain of symbolic transmission and collective memory. It 'benefits' by the successful perpetuation of the ritual, which ensures its continued existence and relevance across generations.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, tradition_continuity, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_kernel__symbol_continuity_reading, tradition_continuity).

% The potential for rituals to evolve and adapt to changing circumstances. It 'pays' in the form of rigidity and resistance to change, as the emphasis on symbolic continuity often prioritizes adherence to established forms over functional or contextual adjustments.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, adaptive_modification, payer,
    analytical, biographical, analytical, universal).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_kernel__symbol_continuity_reading, adaptive_modification).

% Administer and interpret the rituals, ensuring their correct performance and transmission. Their authority is often tied to their role in preserving symbolic continuity, making them resistant to changes that might dilute this function.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, ritual_leaders, agenda_setter,
    institutional, generational, constrained, local).

% Academics or researchers who study the ritual's function in preserving identity and continuity. They analyze its structure and effects without direct participation or investment in its perpetuation.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, external_observers, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective memory and identity by providing a shared symbolic framework and practices that link past catastrophe to present community, ensuring a coherent narrative across generations.
% TRANSFER_FUNCTION: Transfers symbolic meaning, historical narrative, and a sense of collective identity from past generations to present and future community members, at the cost of flexibility in ritual form.
% ABSENT_VOICES: Those who advocate for radical ritual reform or abandonment, viewing the emphasis on symbolic continuity as an impediment to adaptation or a perpetuation of outdated forms, are often marginalized or excluded from the core interpretive community.
% DISAPPEARANCE_RATIONALE: If the ritual vanished, the community's sense of shared identity and historical connection would fragment, leading to a loss of collective memory and a re-evaluation of their relationship to past catastrophes. The social fabric would need to re-form around new, potentially less cohesive, narratives.
% FOUNDING_PROBLEM: The threat of collective amnesia and identity dissolution following a catastrophic historical event, where the community needed a mechanism to remember, mourn, and maintain its distinct identity across time.
% FOUNDING_PROBLEM_CORROBORATION: Community elders and historians attest to the ongoing need for symbolic continuity to counter assimilation pressures and maintain a distinct cultural identity. Anthropological studies from outside the benefiting parties corroborate the ritual's role in identity preservation.
narrative_ontology:disappearance_verdict(catastrophe_memory_kernel__symbol_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_kernel__symbol_continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_kernel__symbol_continuity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(catastrophe_memory_kernel__symbol_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_kernel__symbol_continuity_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_kernel__symbol_continuity_reading_tests).
:- end_tests(catastrophe_memory_kernel__symbol_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.25) because the primary function is symbolic transmission and identity reinforcement, not material gain. The 'cost' is the rigidity imposed on adaptive modification, which is diffuse and not captured by any single agent. Suppression is moderate (0.4) as adherence to ritual forms is largely maintained through social norms and identity-lock, rather than overt coercion. Theater ratio is low (0.1) because the ritual's symbolic function is genuinely performed and valued by participants; there is little performative maintenance for an atrophied function.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of community members, the ritual is a vital source of identity and connection, a pure coordination mechanism. From the perspective of those seeking adaptive change, the same ritual structure imposes a cost of rigidity. The engine's classification will reflect this divergence based on the declared roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Community members are beneficiaries, gaining collective identity and meaning (d low). Tradition continuity, as an abstract entity, also 'benefits' from its perpetuation. Adaptive modification is the 'victim' as its potential is constrained by ritual rigidity (d high). Ritual leaders are agenda-setters, enforcing the forms. External observers are analytical, neither benefiting nor paying.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    symbolic_vs_adaptive_function,
    'Is the ritual''s primary function truly symbolic continuity, or does it also encode adaptive survival competence, and if so, what is the balance?',
    'Longitudinal ethnographic studies tracking ritual changes in response to new threats, and analysis of ritual content for explicit survival instructions vs. purely symbolic gestures.',
    'If a strong adaptive function is found, the extractiveness (cost of rigidity) might be re-evaluated as a necessary trade-off for survival, potentially shifting the classification towards a more ''rope-like'' or even ''mountain-like'' (natural law of survival) type from a different reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(symbolic_vs_adaptive_function, empirical, 'Ambiguity between symbolic and adaptive functions of ritual.').

omega_variable(
    identity_lock_vs_coercion,
    'To what extent is participation in the ritual driven by genuine identity-lock (internalized belonging) versus subtle social coercion or fear of exclusion?',
    'Sociological surveys measuring perceived freedom of exit and social pressure, and analysis of community responses to non-participation or attempts at ritual modification.',
    'If social coercion is a significant factor, the ''suppression'' metric would be higher, and the ''identity_locked'' exit option for community members might be re-evaluated as ''constrained'' or even ''trapped'', potentially shifting the classification towards a ''tangled_rope'' or ''snare'' from a different reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_coercion, empirical, 'Distinguishing internalized identity-lock from social coercion in ritual participation.').

omega_variable(
    kernel_reading_divergence,
    'Given the ''catastrophe_memory_kernel'', how do the ''symbol_continuity_reading'' and its siblings (''survival_competence_reading'', ''trauma_encoding_reading'', ''boundary_maintenance_reading'') structurally diverge in their core claims and implications for extractiveness?',
    'Comparative analysis of each reading''s declared beneficiaries, victims, and core axioms, and their implications for the constraint''s operational metrics.',
    'This omega highlights the irreducible conceptual uncertainty inherent in interpreting the kernel. Each reading generates a distinct constraint with its own classification, and the choice of reading fundamentally alters the analysis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'Conceptual divergence between different readings of the catastrophe memory kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_kernel__symbol_continuity_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cata_tr_t25, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 25, 0.1).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 50, 0.1).
narrative_ontology:measurement(cata_tr_t75, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 75, 0.1).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(cata_be_t25, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 25, 0.22).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 50, 0.23).
narrative_ontology:measurement(cata_be_t75, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 75, 0.24).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 100, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(cata_su_t25, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 25, 0.37).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 50, 0.38).
narrative_ontology:measurement(cata_su_t75, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 75, 0.39).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 100, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_kernel__symbol_continuity_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
