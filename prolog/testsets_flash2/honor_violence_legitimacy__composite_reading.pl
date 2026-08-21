% ============================================================================
% CONSTRAINT STORY: honor_violence_legitimacy__composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_violence_legitimacy__composite_reading, []).

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
 *   constraint_id: honor_violence_legitimacy__composite_reading
 *   human_readable: Honor Violence Legitimacy (Composite Reading)
 *   domain: historical_sociology/legal_anthropology/commitment_systems
 *
 * SUMMARY:
 *   This constraint represents a 'composite reading' of the decline of honor
 *   violence legitimacy, arguing that its decline was overdetermined by two
 *   simultaneous mechanisms: a 'drop' due to rising external costs (e.g.,
 *   state legal intervention, migration to contexts with stronger rule of
 *   law) and a 'contraction' due to an internal redefinition of honor itself
 *   to exclude violence. This reading posits that the contraction mechanism,
 *   by redefining the conceptual kernel of honor, made the drop mechanism
 *   insufficient alone to explain the full decline. It is claimed as a
 *   Tangled Rope because it serves a coordination function for patriarchal
 *   social order while extracting heavily from specific victim groups,
 *   requiring active enforcement to persist.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_violence_legitimacy__composite_reading, 0.65).
domain_priors:suppression_score(honor_violence_legitimacy__composite_reading, 0.4).
domain_priors:theater_ratio(honor_violence_legitimacy__composite_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_violence_legitimacy__composite_reading, tangled_rope).
narrative_ontology:human_readable(honor_violence_legitimacy__composite_reading, "Honor Violence Legitimacy (Composite Reading)").
narrative_ontology:topic_domain(honor_violence_legitimacy__composite_reading, "historical_sociology/legal_anthropology/commitment_systems").

domain_priors:requires_active_enforcement(honor_violence_legitimacy__composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_violence_legitimacy__composite_reading, 'd19a3cab-5de6-4422-84f0-926bcccca0d3').
narrative_ontology:cs_kernel_codification('d19a3cab-5de6-4422-84f0-926bcccca0d3', implicit).
narrative_ontology:cs_authority_grounding('d19a3cab-5de6-4422-84f0-926bcccca0d3', practice).
narrative_ontology:cs_interpretation_layer_present('d19a3cab-5de6-4422-84f0-926bcccca0d3').
narrative_ontology:cs_reading_relation('d19a3cab-5de6-4422-84f0-926bcccca0d3', honor_violence_legitimacy__drop_reading, influences).
narrative_ontology:cs_reading_relation('d19a3cab-5de6-4422-84f0-926bcccca0d3', honor_violence_legitimacy__contraction_reading, influences).
narrative_ontology:cs_axiom('d19a3cab-5de6-4422-84f0-926bcccca0d3', foundational, honor_requires_violence_and_control).
narrative_ontology:cs_axiom_status(honor_requires_violence_and_control, holdable).
narrative_ontology:cs_axiom_grounding('d19a3cab-5de6-4422-84f0-926bcccca0d3', honor_requires_violence_and_control, conventional).
narrative_ontology:cs_axiom('d19a3cab-5de6-4422-84f0-926bcccca0d3', foundational, decline_is_overdetermined_by_internal_and_external_factors).
narrative_ontology:cs_axiom_status(decline_is_overdetermined_by_internal_and_external_factors, holdable).
narrative_ontology:cs_axiom_grounding('d19a3cab-5de6-4422-84f0-926bcccca0d3', decline_is_overdetermined_by_internal_and_external_factors, empirically_contingent).
narrative_ontology:cs_reference_frame('d19a3cab-5de6-4422-84f0-926bcccca0d3', traditional_patriarchal_honor_system).
narrative_ontology:cs_drift_state('d19a3cab-5de6-4422-84f0-926bcccca0d3', contemporary_globalized_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d19a3cab-5de6-4422-84f0-926bcccca0d3', '').
narrative_ontology:cs_kernel_id(honor_violence_legitimacy__composite_reading, honor_violence_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__composite_reading, patriarchal_family_heads).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__composite_reading, community_elders).
narrative_ontology:constraint_victim(honor_violence_legitimacy__composite_reading, female_family_members).
narrative_ontology:constraint_victim(honor_violence_legitimacy__composite_reading, younger_male_family_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain family honor through control over members' behavior, including the use of violence. Their social standing and authority are deeply intertwined with upholding these norms. Exit means loss of status and community standing.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, patriarchal_family_heads, agenda_setter,
    institutional, generational, identity_locked, local).

% Benefit from the stability and traditional order maintained by honor violence norms. They adjudicate disputes and reinforce the legitimacy of the system, but do not directly initiate violence. Their authority is derived from the persistence of these norms.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, community_elders, beneficiary,
    organized, generational, constrained, local).

% Are often the primary targets of honor violence, facing severe restrictions on autonomy and personal safety. Their options for exit are extremely limited due to social isolation, economic dependence, and physical threats.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, female_family_members, payer,
    powerless, immediate, trapped, local).

% Are subject to the demands of family honor, sometimes compelled to participate in or enforce violence. They bear the psychological and social costs of the system, with limited ability to challenge it without risking ostracization or violence themselves.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, younger_male_family_members, payer,
    moderate, biographical, constrained, local).

% Often formally condemn honor violence but may face challenges in enforcement due to cultural resistance, lack of reporting, or local political dynamics. They represent an external force attempting to delegitimize and suppress the practice.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, state_legal_systems, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates social order and family reputation within traditional communities by establishing clear, albeit violent, mechanisms for maintaining honor and responding to perceived transgressions.
% TRANSFER_FUNCTION: Transfers social control, autonomy, and often physical safety from individual family members (especially women and youth) to patriarchal family heads and community structures, in exchange for perceived family and community honor.
% ABSENT_VOICES: Victims of honor violence, particularly those who have been killed or permanently silenced, are absent. Those who have managed to escape are often ostracized and cannot participate in community discourse. Their voices would unequivocally condemn the practice and expose its extractive nature.
% DISAPPEARANCE_RATIONALE: If the legitimacy of honor violence vanished overnight, the social structures, power dynamics, and family hierarchies in many traditional communities would undergo profound and rapid reorganization. Patriarchal authority would be challenged, individual autonomy would increase, and new mechanisms for dispute resolution and social control would emerge.
% FOUNDING_PROBLEM: To maintain social order, family reputation, and patriarchal authority in communities where honor is a central organizing principle, by providing a clear, culturally sanctioned response to perceived threats to that honor.
% FOUNDING_PROBLEM_CORROBORATION: Patriarchal family heads and community elders attest that the problem of maintaining honor and order is still live, citing perceived threats from modernity or external influences. State legal systems and human rights organizations, from outside the benefiting parties, corroborate that the *practice* persists, but dispute its legitimacy as a 'solution' to any valid problem.
narrative_ontology:disappearance_verdict(honor_violence_legitimacy__composite_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_violence_legitimacy__composite_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_violence_legitimacy__composite_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(honor_violence_legitimacy__composite_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_violence_legitimacy__composite_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_violence_legitimacy__composite_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(honor_violence_legitimacy__composite_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(honor_violence_legitimacy__composite_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is high due to the severe costs imposed on victims, but has declined over time as external pressures and internal redefinitions have eroded its absolute reach. Suppression (0.40) is moderate, reflecting the strong social and familial pressures, but also the increasing presence of external legal systems that offer some, albeit often difficult, avenues for resistance. Theater ratio is low (0.10) as the violence, when it occurs, is typically direct and functional to its stated purpose of maintaining honor, rather than performative. The composite reading acknowledges that while external costs made honor violence more difficult to enact (the 'drop'), the conceptual redefinition of honor (the 'contraction') fundamentally undermined its internal legitimacy, leading to a more profound and overdetermined decline.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiaries perceive the constraint as a necessary mechanism for social order and honor, while the victims experience it as pure extraction and oppression. The composite reading attempts to bridge these by showing how both external and internal forces contribute to the constraint's erosion, even if the experience of it remains starkly different across seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Patriarchal family heads and community elders are beneficiaries and agenda-setters, deriving authority and social capital from the system. Female and younger male family members are victims, bearing the direct costs of violence and control. State legal systems are observers, attempting to intervene and alter the constraint's operation. The 'contraction' aspect means that even within some communities, the internal directionality of some members has shifted against the violence, even if they remain constrained by the 'drop' mechanisms.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    relative_weight_of_drop_vs_contraction,
    'What is the relative causal weight of external costs (''drop'') versus internal conceptual redefinition (''contraction'') in the decline of honor violence legitimacy?',
    'Comparative historical analysis across different cultural contexts and legal regimes, isolating cases where one mechanism was dominant or absent, and measuring the rate and nature of decline.',
    'If ''drop'' mechanisms are found to be overwhelmingly dominant, the constraint might be reclassified closer to a Snare, where external enforcement is the primary lever. If ''contraction'' is more dominant, it suggests a deeper, more internal shift in the commitment system, potentially leading to a more complete delegitimization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(relative_weight_of_drop_vs_contraction, empirical, 'Determining the primary driver of decline in honor violence legitimacy.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (external barriers) or internalized (cognitive patterns that persist after barrier removal)?',
    'Post-exit suppression trajectory: if suppression persists after the extractive mechanism is removed (e.g., victims who escape still self-censor or fear retribution even in safe contexts), reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making true freedom more elusive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in honor violence.').

omega_variable(
    kernel_reading_distinction,
    'Is this ''composite_reading'' truly distinct from simply combining the ''drop_reading'' and ''contraction_reading'', or does it represent a unique structural claim?',
    'Formal logical analysis of the ''overdetermined'' claim: does the ''contraction'' mechanism genuinely make the ''drop'' mechanism ''insufficient alone'' in a way that alters the constraint''s core structure or victim sets, beyond a simple additive effect?',
    'If not distinct, this reading should be decomposed into its constituent ''drop'' and ''contraction'' components, each as a separate constraint. If distinct, it validates the claim of a unique, synergistic decline mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_distinction, conceptual, 'Conceptual distinctness of the composite reading from its constituent parts.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_violence_legitimacy__composite_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t0, honor_violence_legitimacy__composite_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(hono_tr_t10, honor_violence_legitimacy__composite_reading, theater_ratio, 10, 0.07).
narrative_ontology:measurement(hono_tr_t20, honor_violence_legitimacy__composite_reading, theater_ratio, 20, 0.08).
narrative_ontology:measurement(hono_tr_t30, honor_violence_legitimacy__composite_reading, theater_ratio, 30, 0.09).
narrative_ontology:measurement(hono_tr_t40, honor_violence_legitimacy__composite_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(hono_tr_t50, honor_violence_legitimacy__composite_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(hono_be_t0, honor_violence_legitimacy__composite_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(hono_be_t10, honor_violence_legitimacy__composite_reading, base_extractiveness, 10, 0.7).
narrative_ontology:measurement(hono_be_t20, honor_violence_legitimacy__composite_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(hono_be_t30, honor_violence_legitimacy__composite_reading, base_extractiveness, 30, 0.67).
narrative_ontology:measurement(hono_be_t40, honor_violence_legitimacy__composite_reading, base_extractiveness, 40, 0.66).
narrative_ontology:measurement(hono_be_t50, honor_violence_legitimacy__composite_reading, base_extractiveness, 50, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t0, honor_violence_legitimacy__composite_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(hono_su_t10, honor_violence_legitimacy__composite_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(hono_su_t20, honor_violence_legitimacy__composite_reading, suppression_requirement, 20, 0.45).
narrative_ontology:measurement(hono_su_t30, honor_violence_legitimacy__composite_reading, suppression_requirement, 30, 0.43).
narrative_ontology:measurement(hono_su_t40, honor_violence_legitimacy__composite_reading, suppression_requirement, 40, 0.41).
narrative_ontology:measurement(hono_su_t50, honor_violence_legitimacy__composite_reading, suppression_requirement, 50, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_violence_legitimacy__composite_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_violence_legitimacy__composite_reading, honor_violence_legitimacy__drop_reading).
narrative_ontology:affects_constraint(honor_violence_legitimacy__composite_reading, honor_violence_legitimacy__contraction_reading).

% DUAL FORMULATION NOTE:
% This constraint is a 'composite reading' of the honor_violence_legitimacy kernel, arguing for an overdetermined decline due to both external costs ('drop') and internal conceptual redefinition ('contraction'). It is linked to its sibling readings, 'drop_reading' and 'contraction_reading', as part of a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
