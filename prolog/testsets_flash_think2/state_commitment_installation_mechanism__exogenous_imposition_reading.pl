% ============================================================================
% CONSTRAINT STORY: state_commitment_installation_mechanism__exogenous_imposition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: state_commitment_installation_mechanism__exogenous_imposition_reading
 *   human_readable: State-Imposed Commitment Installation (Exogenous Imposition Reading)
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   This constraint instantiates the 'exogenous_imposition_reading' of the
 *   'state_commitment_installation_mechanism' kernel. This reading posits
 *   that new commitments gain legitimacy primarily through top-down
 *   installation by a central authority holding a transformation mandate. It
 *   describes a process where a state actively imposes new norms, laws, or
 *   institutions onto a populace, often overriding existing local or
 *   traditional structures, and typically encountering significant
 *   resistance. The high extraction and suppression reflect the coercive
 *   nature of state-building and cultural transformation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_commitment_installation_mechanism__exogenous_imposition_reading, 0.85).
domain_priors:suppression_score(state_commitment_installation_mechanism__exogenous_imposition_reading, 0.9).
domain_priors:theater_ratio(state_commitment_installation_mechanism__exogenous_imposition_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_commitment_installation_mechanism__exogenous_imposition_reading, snare).
narrative_ontology:human_readable(state_commitment_installation_mechanism__exogenous_imposition_reading, "State-Imposed Commitment Installation (Exogenous Imposition Reading)").
narrative_ontology:topic_domain(state_commitment_installation_mechanism__exogenous_imposition_reading, "historical_sociology/state_formation/cultural_authority").

domain_priors:requires_active_enforcement(state_commitment_installation_mechanism__exogenous_imposition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_commitment_installation_mechanism__exogenous_imposition_reading, '4ea04346-65c6-4566-9a49-99b83e049354').
narrative_ontology:cs_kernel_codification('4ea04346-65c6-4566-9a49-99b83e049354', formalized).
narrative_ontology:cs_authority_grounding('4ea04346-65c6-4566-9a49-99b83e049354', extraction).
narrative_ontology:cs_interpretation_layer_present('4ea04346-65c6-4566-9a49-99b83e049354').
narrative_ontology:cs_reading_relation('4ea04346-65c6-4566-9a49-99b83e049354', state_commitment_installation_mechanism__endogenous_climb_reading, forecloses).
narrative_ontology:cs_reading_relation('4ea04346-65c6-4566-9a49-99b83e049354', state_commitment_installation_mechanism__hybrid_cascade_reading, influences).
narrative_ontology:cs_axiom('4ea04346-65c6-4566-9a49-99b83e049354', foundational, state_as_sole_legitimate_source_of_order).
narrative_ontology:cs_axiom_status(state_as_sole_legitimate_source_of_order, holdable).
narrative_ontology:cs_axiom_grounding('4ea04346-65c6-4566-9a49-99b83e049354', state_as_sole_legitimate_source_of_order, conventional).
narrative_ontology:cs_axiom('4ea04346-65c6-4566-9a49-99b83e049354', foundational, centralized_control_as_engine_of_progress).
narrative_ontology:cs_axiom_status(centralized_control_as_engine_of_progress, holdable).
narrative_ontology:cs_axiom_grounding('4ea04346-65c6-4566-9a49-99b83e049354', centralized_control_as_engine_of_progress, instrumental).
narrative_ontology:cs_reference_frame('4ea04346-65c6-4566-9a49-99b83e049354', centralized_state_sovereignty).
narrative_ontology:cs_drift_state('4ea04346-65c6-4566-9a49-99b83e049354', post_colonial_critique_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('4ea04346-65c6-4566-9a49-99b83e049354', '').
narrative_ontology:cs_kernel_id(state_commitment_installation_mechanism__exogenous_imposition_reading, state_commitment_installation_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__exogenous_imposition_reading, transformative_state_authority).
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__exogenous_imposition_reading, intellectual_architects).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__exogenous_imposition_reading, local_elites_and_institutions).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__exogenous_imposition_reading, traditional_social_groups).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The central government or ruling party that holds a mandate for societal transformation. It actively designs, decrees, and enforces new commitments, consolidating power and legitimacy by replacing existing local or traditional structures. It benefits directly from the expansion of its authority and the resources it can command.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, transformative_state_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Existing local power holders (e.g., regional governors, traditional chiefs, religious leaders) whose authority and institutions are directly challenged or superseded by the state's new commitments. They bear the cost of losing autonomy, resources, and social standing, often facing coercion to comply or be removed.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, local_elites_and_institutions, payer,
    organized, biographical, constrained, local).

% Communities and populations whose long-standing cultural practices, norms, and social structures are deemed incompatible with the state's new commitments. They are forced to abandon or modify deeply ingrained ways of life, experiencing cultural disruption and loss of identity, with few options for resistance beyond passive non-compliance or localized rebellion.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, traditional_social_groups, payer,
    powerless, generational, identity_locked, local).

% Academics, ideologues, or policy experts whose theories and visions for societal transformation are adopted and implemented by the state. They gain influence, prestige, and career advancement as their ideas become official policy, often serving as advisors or administrators in the new system.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, intellectual_architects, beneficiary,
    moderate, biographical, mobile, national).

% Historians, political scientists, and human rights organizations who analyze and document the process of state-imposed commitment installation. They provide critical perspectives on the motivations, methods, and consequences of such transformations, often highlighting the human cost and the contested nature of legitimacy.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, international_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_commitment_installation_mechanism__exogenous_imposition_reading, transformative_state_authority).
narrative_ontology:fixing_cost_class(state_commitment_installation_mechanism__exogenous_imposition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unifies diverse local and traditional social practices, legal systems, or cultural norms under a single, centrally defined and state-sanctioned framework, aiming for administrative efficiency and national cohesion.
% TRANSFER_FUNCTION: Transfers legitimacy, authority, and control from existing local/traditional institutions to the central state, and extracts compliance, resources, and cultural conformity from the populace to serve the state's transformative agenda.
% ABSENT_VOICES: Local and traditional leaders, religious authorities, and social groups whose practices are being superseded are often actively silenced or marginalized. Their perspectives, which would emphasize the value of existing norms and the costs of forced change, are excluded from the official discourse of legitimacy.
% DISAPPEARANCE_RATIONALE: If the state's imposed commitments and their enforcement vanished overnight, the vacuum would likely lead to a resurgence of prior local norms, traditional authorities, or new forms of social organization, as the populace would no longer be bound by the top-down structure. The social and political landscape would fundamentally reorganize.
% FOUNDING_PROBLEM: To consolidate state power, rationalize governance, implement a specific ideological vision (e.g., modernization, national identity), or overcome perceived 'backwardness' and fragmentation across a diverse populace.
% FOUNDING_PROBLEM_CORROBORATION: From the perspective of the transformative state authority and its intellectual architects, the founding problem of fragmentation or 'underdevelopment' remains live, justifying ongoing centralized control. However, historians and political scientists, often drawing on the testimonies of affected populations, frequently attest that the problem's definition is contested and its 'solution' often serves the state's power interests more than genuine societal needs.
narrative_ontology:disappearance_verdict(state_commitment_installation_mechanism__exogenous_imposition_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_commitment_installation_mechanism__exogenous_imposition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_commitment_installation_mechanism__exogenous_imposition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(state_commitment_installation_mechanism__exogenous_imposition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_commitment_installation_mechanism__exogenous_imposition_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   Extraction is very high (0.85) because the state directly appropriates legitimacy, resources, and control from existing social formations, imposing its will without broad consent. Suppression is also very high (0.90) as the persistence of these new commitments relies heavily on active coercion, surveillance, and the suppression of alternative forms of social organization. Theater ratio is low (0.10) because the state is actively engaged in a transformative project, with enforcement being direct and functional rather than merely performative. Accessibility collapse is high (0.80) as the state systematically dismantles or discredits alternatives, while resistance is also high (0.75) due to the imposed nature of the changes.
 *
 * PERSPECTIVAL GAP:
 *   The transformative state authority perceives this process as necessary modernization, unification, or progress, viewing resistance as 'backwardness' or 'sedition.' Conversely, local elites and traditional social groups experience it as an oppressive imposition, a loss of autonomy, and cultural destruction. The engine's per-seat classification will reflect this divergence, with the state as a beneficiary and local groups as targets.
 *
 * DIRECTIONALITY LOGIC:
 *   The transformative state authority is the primary beneficiary, gaining expanded power, legitimacy, and control (d near 0.0). Intellectual architects also benefit from the implementation of their ideas. Local elites and traditional social groups are the primary targets, bearing the costs of lost autonomy, cultural disruption, and forced compliance (d near 1.0).
 *
 * MANDATROPHY ANALYSIS:
 *   The 'transformation mandate' often serves as a self-perpetuating justification for ongoing imposition, even as the initial 'problems' evolve or are resolved. This classification as a Snare highlights that the coordination story (unification, modernization) is often a cover for the underlying extraction of power and resources, preventing it from being mislabeled as a genuine Rope or Scaffold.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mandate_legitimacy_ambiguity,
    'Is the state''s ''transformation mandate'' genuinely derived from a broad societal consensus or is it a self-proclaimed justification for power consolidation?',
    'Historical analysis of primary sources, including popular petitions, public debates, and records of resistance, to assess the extent of popular buy-in versus elite imposition.',
    'If the mandate is found to be largely self-proclaimed, it further strengthens the Snare classification by exposing the coordination narrative as a cover. If genuine, it might suggest a more complex Tangled Rope dynamic, where some coordination function is present, albeit with extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_legitimacy_ambiguity, empirical, 'Ambiguity regarding the source and legitimacy of the state''s transformative mandate.').

omega_variable(
    long_term_legitimacy_vs_resistance,
    'Does top-down imposition ultimately lead to internalized legitimacy and stable social order, or does it generate latent resistance and instability over generations?',
    'Longitudinal sociological studies tracking compliance, cultural retention, and political stability in subsequent generations, comparing regions with varying degrees of initial imposition.',
    'If latent resistance persists, the constraint''s effective suppression and extractiveness remain high even if overt resistance diminishes. If internalized legitimacy develops, the constraint might drift towards a more Rope-like or even Mountain-like (if naturalized) classification over very long time horizons.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(long_term_legitimacy_vs_resistance, empirical, 'Whether imposed commitments achieve genuine long-term legitimacy or merely suppress overt resistance.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (e.g., military force, legal prohibitions) or does it lead to internalized compliance and identity fusion over generations?',
    'Post-exit suppression trajectory: if suppression persists (e.g., self-censorship, cultural amnesia) after the direct coercive mechanisms are removed, reclassify as partially internalized. Ethnographic studies of cultural memory and identity formation.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as the target carries the suppression with them. This would make the constraint more resilient to external challenges.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in cultural transformation.').

omega_variable(
    kernel_reading_identification,
    'This constraint is one reading of the ''state_commitment_installation_mechanism'' kernel, specifically the ''exogenous_imposition_reading''. What structural elements would change if a sibling reading were adopted?',
    'Comparative historical analysis of cases aligning with ''endogenous_climb_reading'' or ''hybrid_cascade_reading'' to observe differences in power distribution, extraction levels, and resistance dynamics.',
    'The ''endogenous_climb_reading'' would emphasize bottom-up adoption, leading to significantly lower extraction and suppression. The ''hybrid_cascade_reading'' would acknowledge top-down initiation but require local validation, suggesting a more nuanced extraction profile and potentially a Tangled Rope classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Identifying the specific reading of the state commitment installation kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_commitment_installation_mechanism__exogenous_imposition_reading, 1900, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1900, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 1900, 0.05).
narrative_ontology:measurement(stat_tr_t1910, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 1910, 0.07).
narrative_ontology:measurement(stat_tr_t1920, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 1920, 0.08).
narrative_ontology:measurement(stat_tr_t1930, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 1930, 0.09).
narrative_ontology:measurement(stat_tr_t1940, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 1940, 0.1).
narrative_ontology:measurement(stat_tr_t1950, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 1950, 0.1).

% Extraction over time
narrative_ontology:measurement(stat_be_t1900, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 1900, 0.7).
narrative_ontology:measurement(stat_be_t1910, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 1910, 0.75).
narrative_ontology:measurement(stat_be_t1920, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 1920, 0.8).
narrative_ontology:measurement(stat_be_t1930, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 1930, 0.83).
narrative_ontology:measurement(stat_be_t1940, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 1940, 0.86).
narrative_ontology:measurement(stat_be_t1950, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 1950, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1900, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 1900, 0.75).
narrative_ontology:measurement(stat_su_t1910, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 1910, 0.8).
narrative_ontology:measurement(stat_su_t1920, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 1920, 0.85).
narrative_ontology:measurement(stat_su_t1930, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 1930, 0.88).
narrative_ontology:measurement(stat_su_t1940, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 1940, 0.9).
narrative_ontology:measurement(stat_su_t1950, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 1950, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_commitment_installation_mechanism__exogenous_imposition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__exogenous_imposition_reading, local_governance_structures).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__exogenous_imposition_reading, national_education_curriculum).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__exogenous_imposition_reading, economic_rationalization_policies).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'state_commitment_installation_mechanism' kernel. The other readings are 'endogenous_climb_reading' and 'hybrid_cascade_reading', each representing a distinct mechanism of commitment installation with different structural properties and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
