% ============================================================================
% CONSTRAINT STORY: state_commitment_installation_mechanism__exogenous_imposition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   human_readable: Exogenous Imposition Reading of State Commitment Installation
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   This constraint instantiates the exogenous imposition reading of the
 *   state commitment installation kernel: new normative commitments gain
 *   legitimacy solely through top-down decree by an authority claiming a
 *   transformation mandate. The mechanism is historically central to
 *   revolutionary and colonial state formation, where apex authorities
 *   override local heterogeneity by fiat. It is distinguished from its
 *   siblings by the absence of grassroots advocacy, the abruptness of
 *   adoption, and the concentration of legitimation rents at the state apex.
 *   KEY AGENTS (by structural relationship): - state_apex_authority: Primary
 *   beneficiary and agenda-setter (institutional/arbitrage) â monopolizes
 *   legitimate installation. - base_communities: Primary target and payer
 *   (powerless/trapped) â bear extraction through forced compliance. -
 *   grassroots_institutional_actors: Excluded voices (moderate/constrained)
 *   â would generate endogenous legitimacy but are bypassed. -
 *   comparative_historical_analyst: Analytical observer â sees the full
 *   structural asymmetry.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_commitment_installation_mechanism__exogenous_imposition_reading, 0.62).
domain_priors:suppression_score(state_commitment_installation_mechanism__exogenous_imposition_reading, 0.71).
domain_priors:theater_ratio(state_commitment_installation_mechanism__exogenous_imposition_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_commitment_installation_mechanism__exogenous_imposition_reading, tangled_rope).
narrative_ontology:human_readable(state_commitment_installation_mechanism__exogenous_imposition_reading, "Exogenous Imposition Reading of State Commitment Installation").
narrative_ontology:topic_domain(state_commitment_installation_mechanism__exogenous_imposition_reading, "historical_sociology/state_formation/cultural_authority").

domain_priors:requires_active_enforcement(state_commitment_installation_mechanism__exogenous_imposition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_commitment_installation_mechanism__exogenous_imposition_reading, '7eaf1a46-dce8-4328-bb1d-00df07a4a080').
narrative_ontology:cs_kernel_codification('7eaf1a46-dce8-4328-bb1d-00df07a4a080', formalized).
narrative_ontology:cs_authority_grounding('7eaf1a46-dce8-4328-bb1d-00df07a4a080', extraction).
narrative_ontology:cs_interpretation_layer_present('7eaf1a46-dce8-4328-bb1d-00df07a4a080').
narrative_ontology:cs_reading_relation('7eaf1a46-dce8-4328-bb1d-00df07a4a080', state_commitment_installation_mechanism__endogenous_climb_reading, forecloses).
narrative_ontology:cs_reading_relation('7eaf1a46-dce8-4328-bb1d-00df07a4a080', state_commitment_installation_mechanism__hybrid_cascade_reading, influences).
narrative_ontology:cs_axiom('7eaf1a46-dce8-4328-bb1d-00df07a4a080', foundational, legitimacy_resides_in_mandate_holder).
narrative_ontology:cs_axiom_status(legitimacy_resides_in_mandate_holder, holdable).
narrative_ontology:cs_axiom_grounding('7eaf1a46-dce8-4328-bb1d-00df07a4a080', legitimacy_resides_in_mandate_holder, conventional).
narrative_ontology:cs_axiom('7eaf1a46-dce8-4328-bb1d-00df07a4a080', foundational, grassroots_validity_is_irrelevant).
narrative_ontology:cs_axiom_status(grassroots_validity_is_irrelevant, holdable).
narrative_ontology:cs_axiom_grounding('7eaf1a46-dce8-4328-bb1d-00df07a4a080', grassroots_validity_is_irrelevant, conventional).
narrative_ontology:cs_reference_frame('7eaf1a46-dce8-4328-bb1d-00df07a4a080', apex_mandate_legitimacy).
narrative_ontology:cs_drift_state('7eaf1a46-dce8-4328-bb1d-00df07a4a080', post_decolonization_endogenous_revival, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('7eaf1a46-dce8-4328-bb1d-00df07a4a080', '').
narrative_ontology:cs_kernel_id(state_commitment_installation_mechanism__exogenous_imposition_reading, state_commitment_installation_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__exogenous_imposition_reading, state_apex_authority).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__exogenous_imposition_reading, base_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds a self-asserted transformation mandate and monopolizes the right to install new societal commitments by decree. Structures legitimation so that its own top-down action is the sole valid source of new normative order, capturing compliance and symbolic centrality from a unified territorial population.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, state_apex_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Receive abrupt decrees imposing new commitments without participatory input or grassroots advocacy. Bear the costs of forced compliance, cultural dislocation, and suppressed local practice. Resistance is structurally present but lacks effective institutional channels against the mandate.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, base_communities, payer,
    powerless, biographical, trapped, local).

% Local associations, customary authorities, and normative entrepreneurs who would otherwise generate bottom-up legitimacy through demonstration and fringe innovation. Their exclusion is constitutive of the exogenous mechanism; they are not admitted to the installation process.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, grassroots_institutional_actors, excluded,
    moderate, biographical, constrained, regional).

% Observes cross-case patterns of state formation and commitment installation. Notes the structural asymmetry between mandate-holding apex authorities and base populations, and the systematic suppression of endogenous validation pathways.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, comparative_historical_analyst, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_commitment_installation_mechanism__exogenous_imposition_reading, state_apex_authority).
narrative_ontology:fixing_cost_class(state_commitment_installation_mechanism__exogenous_imposition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates rapid normative alignment across heterogeneous territories by centralizing the right to introduce new commitments, eliminating the need for prior local consensus and overriding fragmented authority.
% TRANSFER_FUNCTION: Moves compliance, symbolic legitimacy, and transformative capacity from base communities and local institutions to the state apex and its administrative apparatus.
% ABSENT_VOICES: Grassroots normative entrepreneurs, local customary authorities, and fringe institutional actors who would generate legitimacy through demonstration and climb; they are structurally absent from the installation process because their inclusion would contradict the monopoly of the transformation mandate.
% DISAPPEARANCE_RATIONALE: If this mechanism vanished overnight, the state's capacity to override local resistance and impose universal commitments would collapse. The political field would fragment into heterogeneous local arrangements, and legitimacy generation would shift to slower endogenous or hybrid processes.
% FOUNDING_PROBLEM: How to establish uniform normative commitments across a territory with heterogeneous local practices during state formation or revolutionary rupture, when no shared deliberative infrastructure exists and speed is imperative.
% FOUNDING_PROBLEM_CORROBORATION: Historical sociologists of the state corroborate the need to break local autonomy, but institutional sociologists and social movement theorists argue that endogenous validation was always necessary and the pure imposition model is a retrospective fiction. No corroboration from outside the benefiting parties supports the claim that pure imposition successfully solved the problem without hybridization.
narrative_ontology:disappearance_verdict(state_commitment_installation_mechanism__exogenous_imposition_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_commitment_installation_mechanism__exogenous_imposition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_commitment_installation_mechanism__exogenous_imposition_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(state_commitment_installation_mechanism__exogenous_imposition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_commitment_installation_mechanism__exogenous_imposition_reading, 0.62, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.62) is substantial because the state captures legitimacy and compliance without reciprocating participatory voice; suppression (0.71) is higher because the mechanism's persistence depends on actively excluding grassroots validation and containing base resistance. Theater ratio (0.45) is moderate: the transformation mandate is partly performative (rituals of revolutionary or civilizational renewal), but enforcement is materially real. The measurement series shows extraction rising as enforcement machinery matures, then plateauing as the mechanism normalizes; theater peaks during the initial revolutionary phase and settles into institutional routine.
 *
 * PERSPECTIVAL GAP:
 *   The state apex seat experiences the constraint as genuine coordination: it solves the chaos of heterogeneous local practice by unifying authority. The base community seat experiences the same structure as extraction: it receives commitments it did not request, bears compliance costs, and faces suppressed alternatives. The engine computes this divergence from the structural data â identical scope but opposite directionality.
 *
 * DIRECTIONALITY LOGIC:
 *   The state_apex_authority is the structural beneficiary (low d): the constraint subsidizes its capacity to rule by delegitimizing local autonomy. Base_communities are the structural targets (high d): they pay the transfer of compliance and symbolic subordination. Grassroots_institutional_actors are excluded entirely (maximal d in the limit): their exclusion is the enforcement object. The analyst seat sits at analytical/arbitrage with neutral d.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents mislabeling this mechanism as either pure coordination (rope) or pure extraction (snare). There is a genuine coordination function: rapid territorial alignment under a single normative umbrella is a real collective-action solution for state formation. However, asymmetric extraction is structurally present â the state captures concentrated legitimacy while base communities pay diffuse compliance costs. Without the tangled_rope gate, one might see only the decree and call it a snare, or only the unification and call it a rope. The active enforcement requirement and the identified victim set force the hybrid classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    exogenous_imposition_stability,
    'Can commitments installed purely by exogenous decree achieve long-term stabilization without any endogenous validation from base communities or fringe institutions?',
    'Comparative historical analysis tracking the survival rate of purely imposed commitments versus hybrid or endogenous ones across multiple state-formation cases.',
    'If pure imposition never stabilizes without downstream validation, the exogenous reading describes a transitional extraction mechanism rather than a genuine legitimation pathway, pushing classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exogenous_imposition_stability, empirical, 'Whether pure exogenous imposition can produce stable legitimacy.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is base compliance driven by structural enforcement (sanctions, surveillance, administrative penetration) or by internalized acceptance of the state''s transformative mandate?',
    'Post-regime or post-reform trajectory analysis: if compliance collapses rapidly after enforcement removal, suppression was structural; if it persists, internalization was significant.',
    'If internalized, effective suppression and extraction are higher than structural measures suggest, and the mechanism functions partly as identity coordination. If purely structural, the tangled rope leans toward the extraction pole.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism.').

omega_variable(
    kernel_reading_boundary,
    'Does the exogenous imposition reading foreclose the hybrid cascade reading because they share a logical framework, or do they represent incommensurable premises about legitimacy?',
    'Discourse analysis of state ideological texts to determine whether hybridization is treated as betrayal of the mandate or as tactical adjustment.',
    'If the exogenous framework logically forecloses hybridity, the family is one of contradiction; if it merely influences it, the readings are variants within a shared spectrum. This changes how the engine computes contamination between the constraints.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Logical relationship between exogenous and hybrid readings of the kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_commitment_installation_mechanism__exogenous_imposition_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(stat_tr_t10, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 10, 0.6).
narrative_ontology:measurement(stat_tr_t20, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 20, 0.52).
narrative_ontology:measurement(stat_tr_t30, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 30, 0.45).
narrative_ontology:measurement(stat_tr_t40, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement(stat_tr_t50, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 50, 0.45).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(stat_be_t10, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(stat_be_t20, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(stat_be_t30, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 30, 0.65).
narrative_ontology:measurement(stat_be_t40, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 40, 0.63).
narrative_ontology:measurement(stat_be_t50, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 50, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(stat_su_t10, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(stat_su_t20, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(stat_su_t30, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 30, 0.75).
narrative_ontology:measurement(stat_su_t40, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(stat_su_t50, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 50, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_commitment_installation_mechanism__exogenous_imposition_reading, identity_coordination).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__exogenous_imposition_reading, endogenous_climb_reading).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__exogenous_imposition_reading, hybrid_cascade_reading).

% DUAL FORMULATION NOTE:
% This constraint and its siblings form a decomposition family of the kernel 'state_commitment_installation_mechanism'. The exogenous imposition reading has a higher epsilon (more extractive) than the endogenous climb reading because it suppresses grassroots alternatives. The hybrid cascade reading sits between them. Each is a distinct constraint with its own epsilon, linked because they compete to explain the same historical phenomenon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
