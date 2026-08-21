% ============================================================================
% CONSTRAINT STORY: herem_command_dt7__durable_separation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_herem_command_dt7__durable_separation_reading, []).

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
 *   constraint_id: herem_command_dt7__durable_separation_reading
 *   human_readable: Herem Command: Durable Separation Reading
 *   domain: religious_ethics/biblical_hermeneutics/commitment_system
 *
 * SUMMARY:
 *   This constraint represents the 'durable separation' reading of the
 *   biblical Herem command, which interprets it as a timeless divine mandate
 *   for the preservation of a distinct covenantal identity through strict
 *   boundaries and categorical separation from designated outsiders. This
 *   reading emphasizes the literal and ongoing applicability of the command,
 *   leading to high extractiveness on individual autonomy (e.g.,
 *   intermarriage) and severe suppression of alternatives to communal purity.
 *   The constraint is a reading of the 'herem_command_dt7' kernel, alongside
 *   'contextual_supersession_reading' and 'allegorical_displacement_reading'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(herem_command_dt7__durable_separation_reading, 0.85).
domain_priors:suppression_score(herem_command_dt7__durable_separation_reading, 0.9).
domain_priors:theater_ratio(herem_command_dt7__durable_separation_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(herem_command_dt7__durable_separation_reading, tangled_rope).
narrative_ontology:human_readable(herem_command_dt7__durable_separation_reading, "Herem Command: Durable Separation Reading").
narrative_ontology:topic_domain(herem_command_dt7__durable_separation_reading, "religious_ethics/biblical_hermeneutics/commitment_system").

domain_priors:requires_active_enforcement(herem_command_dt7__durable_separation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(herem_command_dt7__durable_separation_reading, '956ea129-2b0b-4934-be67-9964abdd3dea').
narrative_ontology:cs_kernel_codification('956ea129-2b0b-4934-be67-9964abdd3dea', fixed_text).
narrative_ontology:cs_authority_grounding('956ea129-2b0b-4934-be67-9964abdd3dea', lineage).
narrative_ontology:cs_interpretation_layer_present('956ea129-2b0b-4934-be67-9964abdd3dea').
narrative_ontology:cs_reading_relation('956ea129-2b0b-4934-be67-9964abdd3dea', herem_command_dt7__contextual_supersession_reading, forecloses).
narrative_ontology:cs_reading_relation('956ea129-2b0b-4934-be67-9964abdd3dea', herem_command_dt7__allegorical_displacement_reading, forecloses).
narrative_ontology:cs_axiom('956ea129-2b0b-4934-be67-9964abdd3dea', foundational, divine_mandate_timelessness).
narrative_ontology:cs_axiom_status(divine_mandate_timelessness, holdable).
narrative_ontology:cs_axiom_grounding('956ea129-2b0b-4934-be67-9964abdd3dea', divine_mandate_timelessness, theological).
narrative_ontology:cs_axiom('956ea129-2b0b-4934-be67-9964abdd3dea', foundational, covenant_purity_requires_physical_separation).
narrative_ontology:cs_axiom_status(covenant_purity_requires_physical_separation, holdable).
narrative_ontology:cs_axiom_grounding('956ea129-2b0b-4934-be67-9964abdd3dea', covenant_purity_requires_physical_separation, theological).
narrative_ontology:cs_reference_frame('956ea129-2b0b-4934-be67-9964abdd3dea', original_divine_command).
narrative_ontology:cs_drift_state('956ea129-2b0b-4934-be67-9964abdd3dea', contemporary_theological_discourse, gap(stable, minor, false)).
narrative_ontology:cs_created_at('956ea129-2b0b-4934-be67-9964abdd3dea', '').
narrative_ontology:cs_kernel_id(herem_command_dt7__durable_separation_reading, herem_command_dt7).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(herem_command_dt7__durable_separation_reading, covenant_community_members).
narrative_ontology:constraint_beneficiary(herem_command_dt7__durable_separation_reading, religious_authorities).
narrative_ontology:constraint_victim(herem_command_dt7__durable_separation_reading, designated_outsiders).
narrative_ontology:constraint_victim(herem_command_dt7__durable_separation_reading, intermarriage_seeking_members).
narrative_ontology:constraint_vindicates(herem_command_dt7__durable_separation_reading, divine_sovereignty).
narrative_ontology:constraint_vindicates(herem_command_dt7__durable_separation_reading, covenant_purity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from a clear, bounded identity and perceived spiritual purity. They are identity-locked into the community structure, internalizing the mandate for separation as essential to their collective existence and divine favor. They enforce the norms on themselves and others.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, covenant_community_members, beneficiary,
    organized, generational, identity_locked, national).

% Are the targets of the separation mandate, denied integration, and potentially subject to violence or displacement. Their very existence is framed as a threat to the covenant community's purity, with no legitimate path to inclusion or equal standing.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, designated_outsiders, payer,
    powerless, immediate, trapped, local).

% Desire integration or relational bonds with designated outsiders but face severe social, religious, and potentially legal penalties for violating the separation mandate. Their identity is deeply intertwined with the community, making exit or defiance extremely costly.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, intermarriage_seeking_members, payer,
    moderate, biographical, identity_locked, local).

% Interpret, transmit, and enforce the Herem command, deriving their authority and the community's legitimacy from its strict adherence. They actively suppress alternative interpretations and maintain the boundaries of separation.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, religious_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Study and critique the Herem command's historical context, theological implications, and ethical consequences. They analyze its function within the commitment system but are external to its direct enforcement.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, analytical_theologians, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To preserve the distinct identity, spiritual purity, and covenantal integrity of the community by establishing clear boundaries and preventing assimilation with designated outsiders.
% TRANSFER_FUNCTION: Transfers autonomy, social standing, and potentially life itself from designated outsiders and intermarriage-seeking members to the religious authorities and the perceived purity and stability of the covenant community.
% ABSENT_VOICES: Designated outsiders and those advocating for universalist ethics or inter-communal integration are structurally excluded from the interpretive and enforcement processes. Their perspectives are actively suppressed or demonized as threats to the community's existence.
% DISAPPEARANCE_RATIONALE: If the Herem command and its durable separation reading vanished overnight, the entire social, religious, and political structure of the covenant community would fundamentally shift. Identity boundaries would dissolve, inter-communal relations would reorganize, and the authority of religious leaders would be severely challenged, leading to a profound redefinition of the community itself.
% FOUNDING_PROBLEM: The perceived existential threat of assimilation and loss of distinct covenant identity and spiritual purity in a polytheistic and morally divergent environment.
% FOUNDING_PROBLEM_CORROBORATION: The reading's proponents attest the problem is live, citing scriptural authority and ongoing threats to community distinctiveness and spiritual integrity. External ethical observers and proponents of sibling readings contest this, arguing the original context is no longer relevant or the command is morally superseded.
narrative_ontology:disappearance_verdict(herem_command_dt7__durable_separation_reading, world_rearranges).
narrative_ontology:founding_problem_status(herem_command_dt7__durable_separation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(herem_command_dt7__durable_separation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(herem_command_dt7__durable_separation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(herem_command_dt7__durable_separation_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(herem_command_dt7__durable_separation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(herem_command_dt7__durable_separation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(herem_command_dt7__durable_separation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.85) is high due to the severe costs imposed on those who violate the separation mandate, including loss of community, social ostracization, and potential violence. Suppression (0.90) is extremely high because the mandate is presented as divine and timeless, leaving virtually no legitimate internal alternatives for dissent or reinterpretation that would challenge the core principle of separation. The theater ratio (0.10) is low, as the command is actively and genuinely enforced as a core tenet, not merely performed. Accessibility collapse is near total (0.95) as the reading forecloses any path to integration or alternative identity. Resistance is low (0.30) due to the divine authority and severe penalties for non-compliance, though some internal tension or quiet dissent may exist.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of religious authorities and many covenant community members, this constraint is a necessary and divinely ordained mechanism for identity preservation and spiritual well-being (a coordination function). From the perspective of designated outsiders and intermarriage-seeking members, it is a profoundly extractive and suppressive mechanism that denies fundamental rights and autonomy, enforced through coercion and social control.
 *
 * DIRECTIONALITY LOGIC:
 *   Covenant community members and religious authorities are beneficiaries, as they gain identity clarity, communal cohesion, and authority from the constraint's operation. Designated outsiders and intermarriage-seeking members are clear victims, bearing the full cost of exclusion and loss of autonomy. The identity-locked exit option for community members reflects the deep internal and external pressures to conform.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the Herem command a timeless divine mandate for physical separation, or is it historically bounded or allegorical?',
    'Further theological and historical scholarship, comparative religious ethics, and the emergence of new interpretive traditions within the affected communities.',
    'If resolved as historically bounded or allegorical, the constraint''s extractiveness and suppression would be reclassified as illegitimate, potentially leading to its dissolution or reinterpretation as a Rope or Piton. If confirmed as timeless, its current classification as Tangled Rope would be reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Ambiguity regarding the timelessness and literal interpretation of the Herem command.').

omega_variable(
    violence_legitimation_potential,
    'Does this reading inherently legitimate violence against designated outsiders, or is violence a contingent outcome of its application?',
    'Analysis of historical applications of the Herem command under this reading, and explicit theological rulings on the permissibility of violence against outsiders.',
    'If it inherently legitimates violence, the constraint''s effective extractiveness and suppression are amplified to their most severe forms, potentially pushing it towards a Snare. If contingent, the constraint remains a Tangled Rope, but with a recognized severe failure mode.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(violence_legitimation_potential, preference, 'Whether the durable separation reading of Herem inherently legitimates violence.').

omega_variable(
    identity_lock_mechanism_composition,
    'Is the identity-lock experienced by covenant community members primarily theological (belief-driven) or social (community pressure-driven)?',
    'Sociological studies of ex-members'' experiences, analysis of internal dissent, and the impact of external social changes on community cohesion.',
    'If primarily theological, the suppression is deeply internalized and harder to dislodge. If primarily social, external support networks and alternative community structures could more effectively reduce the identity-lock and thus the effective suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_composition, empirical, 'Composition of identity-lock mechanism (theological vs. social).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(herem_command_dt7__durable_separation_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(here_tr_t0, herem_command_dt7__durable_separation_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(here_tr_t20, herem_command_dt7__durable_separation_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(here_tr_t40, herem_command_dt7__durable_separation_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(here_tr_t60, herem_command_dt7__durable_separation_reading, theater_ratio, 60, 0.1).
narrative_ontology:measurement(here_tr_t80, herem_command_dt7__durable_separation_reading, theater_ratio, 80, 0.1).
narrative_ontology:measurement(here_tr_t100, herem_command_dt7__durable_separation_reading, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(here_be_t0, herem_command_dt7__durable_separation_reading, base_extractiveness, 0, 0.85).
narrative_ontology:measurement(here_be_t20, herem_command_dt7__durable_separation_reading, base_extractiveness, 20, 0.85).
narrative_ontology:measurement(here_be_t40, herem_command_dt7__durable_separation_reading, base_extractiveness, 40, 0.85).
narrative_ontology:measurement(here_be_t60, herem_command_dt7__durable_separation_reading, base_extractiveness, 60, 0.85).
narrative_ontology:measurement(here_be_t80, herem_command_dt7__durable_separation_reading, base_extractiveness, 80, 0.85).
narrative_ontology:measurement(here_be_t100, herem_command_dt7__durable_separation_reading, base_extractiveness, 100, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(here_su_t0, herem_command_dt7__durable_separation_reading, suppression_requirement, 0, 0.9).
narrative_ontology:measurement(here_su_t20, herem_command_dt7__durable_separation_reading, suppression_requirement, 20, 0.9).
narrative_ontology:measurement(here_su_t40, herem_command_dt7__durable_separation_reading, suppression_requirement, 40, 0.9).
narrative_ontology:measurement(here_su_t60, herem_command_dt7__durable_separation_reading, suppression_requirement, 60, 0.9).
narrative_ontology:measurement(here_su_t80, herem_command_dt7__durable_separation_reading, suppression_requirement, 80, 0.9).
narrative_ontology:measurement(here_su_t100, herem_command_dt7__durable_separation_reading, suppression_requirement, 100, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(herem_command_dt7__durable_separation_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'herem_command_dt7' kernel, each representing a distinct structural interpretation of the biblical command. This reading emphasizes timeless, literal separation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
