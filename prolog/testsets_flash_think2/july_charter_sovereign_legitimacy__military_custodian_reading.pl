% ============================================================================
% CONSTRAINT STORY: july_charter_sovereign_legitimacy__military_custodian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_july_charter_sovereign_legitimacy__military_custodian_reading, []).

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
 *   constraint_id: july_charter_sovereign_legitimacy__military_custodian_reading
 *   human_readable: July Charter's Military Custodian Reading of Sovereign Legitimacy
 *   domain: constitutional_law/political_transitions/post_revolutionary_state_building
 *
 * SUMMARY:
 *   This constraint describes the 'military custodian' reading of a
 *   foundational July Charter, where the military is ratified as the
 *   permanent institutional guardian of national stability. This reading
 *   subordinates civilian institutions to military veto authority and bounds
 *   political contestation through the security apparatus. While presented as
 *   a necessary coordination function for stability, the metrics reflect high
 *   extraction of political power and severe suppression of civilian agency,
 *   making it a Tangled Rope. The claimed type (Tangled Rope) acknowledges
 *   the narrative of coordination (stability) while the metrics (high
 *   extractiveness, suppression, and rising theater) describe its actual
 *   operation as a mechanism for military dominance.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__military_custodian_reading, 0.85).
domain_priors:suppression_score(july_charter_sovereign_legitimacy__military_custodian_reading, 0.9).
domain_priors:theater_ratio(july_charter_sovereign_legitimacy__military_custodian_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(july_charter_sovereign_legitimacy__military_custodian_reading, tangled_rope).
narrative_ontology:human_readable(july_charter_sovereign_legitimacy__military_custodian_reading, "July Charter's Military Custodian Reading of Sovereign Legitimacy").
narrative_ontology:topic_domain(july_charter_sovereign_legitimacy__military_custodian_reading, "constitutional_law/political_transitions/post_revolutionary_state_building").

domain_priors:requires_active_enforcement(july_charter_sovereign_legitimacy__military_custodian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(july_charter_sovereign_legitimacy__military_custodian_reading, '7c3731b8-8e39-4631-b979-d8419b80dd54').
narrative_ontology:cs_kernel_codification('7c3731b8-8e39-4631-b979-d8419b80dd54', formalized).
narrative_ontology:cs_authority_grounding('7c3731b8-8e39-4631-b979-d8419b80dd54', extraction).
narrative_ontology:cs_interpretation_layer_present('7c3731b8-8e39-4631-b979-d8419b80dd54').
narrative_ontology:cs_reading_relation('7c3731b8-8e39-4631-b979-d8419b80dd54', july_charter_sovereign_legitimacy__secular_democratic_reading, forecloses).
narrative_ontology:cs_reading_relation('7c3731b8-8e39-4631-b979-d8419b80dd54', july_charter_sovereign_legitimacy__guided_nationalism_reading, coexists_with).
narrative_ontology:cs_axiom('7c3731b8-8e39-4631-b979-d8419b80dd54', foundational, military_as_sole_guarantor_of_stability).
narrative_ontology:cs_axiom_status(military_as_sole_guarantor_of_stability, holdable).
narrative_ontology:cs_axiom_grounding('7c3731b8-8e39-4631-b979-d8419b80dd54', military_as_sole_guarantor_of_stability, conventional).
narrative_ontology:cs_axiom('7c3731b8-8e39-4631-b979-d8419b80dd54', secondary, civilian_political_activity_subordinate_to_security).
narrative_ontology:cs_axiom_status(civilian_political_activity_subordinate_to_security, holdable).
narrative_ontology:cs_axiom_grounding('7c3731b8-8e39-4631-b979-d8419b80dd54', civilian_political_activity_subordinate_to_security, conventional).
narrative_ontology:cs_reference_frame('7c3731b8-8e39-4631-b979-d8419b80dd54', military_as_ultimate_arbiter).
narrative_ontology:cs_drift_state('7c3731b8-8e39-4631-b979-d8419b80dd54', contemporary_political_contestation, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('7c3731b8-8e39-4631-b979-d8419b80dd54', '').
narrative_ontology:cs_kernel_id(july_charter_sovereign_legitimacy__military_custodian_reading, july_charter_sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__military_custodian_reading, military_establishment).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__military_custodian_reading, state_security_apparatus).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__military_custodian_reading, loyalist_citizens).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__military_custodian_reading, autonomous_political_parties).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__military_custodian_reading, student_movement).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__military_custodian_reading, civilian_government_officials).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__military_custodian_reading, human_rights_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the Charter as granting it ultimate authority to ensure national stability, exercising veto power over civilian decisions and controlling key state functions. Benefits from expanded budgets, political influence, and immunity from civilian oversight.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, military_establishment, agenda_setter,
    institutional, generational, arbitrage, national).

% Operates under the military's directive, benefiting from broad powers, reduced accountability, and resources allocated to maintain 'stability' through surveillance and suppression of dissent. Its existence is tied to the military's guardian role.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, state_security_apparatus, beneficiary,
    institutional, biographical, constrained, national).

% Their ability to organize, contest elections, and influence policy is severely curtailed by military oversight and security laws. They face harassment, arrests, and bans, making their political activity highly constrained and risky.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, autonomous_political_parties, payer,
    powerless, immediate, trapped, national).

% Faces direct suppression for protesting military rule or advocating for civilian supremacy. Their gatherings are dispersed, leaders arrested, and academic freedoms restricted, making organized dissent extremely difficult.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, student_movement, payer,
    powerless, immediate, trapped, local).

% Hold formal positions within the government but operate under the constant threat of military intervention or veto. Their policy initiatives are often subject to military approval, and their authority is ultimately subordinate, limiting their effective power.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, civilian_government_officials, payer,
    moderate, biographical, constrained, national).

% Perceive the military's role as essential for maintaining order and preventing chaos, especially in a post-revolutionary context. They benefit from the perceived stability and may genuinely support the military's narrative, often prioritizing security over democratic freedoms.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, loyalist_citizens, beneficiary,
    moderate, biographical, constrained, national).

% Monitor human rights abuses, suppression of political freedoms, and the erosion of civilian rule. They document violations, issue reports, and advocate for international pressure, but have no direct power to alter the constraint.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, international_human_rights_organizations, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To prevent national fragmentation and ensure stability in a volatile post-revolutionary environment by providing a strong, unified institutional guardian against internal and external threats.
% TRANSFER_FUNCTION: Transfers ultimate political authority, decision-making power, and significant state resources from civilian institutions and political actors to the military establishment, in exchange for perceived security and order.
% ABSENT_VOICES: Exiled political leaders, independent journalists, and international democratic institutions are structurally excluded or marginalized. They would advocate for full civilian supremacy and democratic accountability, but their voices are suppressed or dismissed as external interference.
% DISAPPEARANCE_RATIONALE: If the military's 'guardian' role and its enforcement vanished overnight, the entire political system would undergo a fundamental re-ordering. A power vacuum would emerge, likely leading to intense political contestation, potential instability, but also an opportunity for genuine civilian democratic institutions to establish themselves without military veto.
% FOUNDING_PROBLEM: The Charter was established to address severe post-revolutionary instability, the threat of civil war, and perceived weaknesses of nascent civilian institutions, which were seen as incapable of securing the nation's future.
% FOUNDING_PROBLEM_CORROBORATION: The military establishment and its supporters assert that the founding problems of instability and external threats remain live, justifying their continued role. Autonomous political parties, student movements, and international human rights organizations contend that these problems are either resolved or are used as a pretext for maintaining military power, citing the suppression of legitimate political activity as the real issue. Independent analyses often support the latter view, highlighting the shift from genuine security concerns to power consolidation.
narrative_ontology:disappearance_verdict(july_charter_sovereign_legitimacy__military_custodian_reading, world_rearranges).
narrative_ontology:founding_problem_status(july_charter_sovereign_legitimacy__military_custodian_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(july_charter_sovereign_legitimacy__military_custodian_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(july_charter_sovereign_legitimacy__military_custodian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(july_charter_sovereign_legitimacy__military_custodian_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(july_charter_sovereign_legitimacy__military_custodian_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__military_custodian_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(july_charter_sovereign_legitimacy__military_custodian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.85) because the military effectively controls the state, diverting resources and political capital to its own ends, and civilian institutions are stripped of meaningful autonomy. Suppression is extremely high (0.90) as political dissent and alternative power structures are actively and often violently suppressed to maintain the military's 'guardian' role. The theater ratio is high (0.60) and rising, indicating that while some genuine security functions exist, a significant portion of military activity and rhetoric is performative, designed to legitimize its continued political dominance under the guise of stability, even as the original threats recede. Accessibility collapse is high (0.80) for political alternatives, and resistance is substantial (0.70) from those who bear the costs.
 *
 * PERSPECTIVAL GAP:
 *   From the military's perspective, this arrangement is a necessary and legitimate coordination mechanism for national survival. From the perspective of civilian political actors and human rights advocates, it is a highly extractive and suppressive system that uses the narrative of stability as a cover for maintaining military power. The engine's classification will highlight this divergence by computing a Tangled Rope from the authored metrics, despite the military's self-proclaimed 'guardian' role.
 *
 * DIRECTIONALITY LOGIC:
 *   The military establishment and state security apparatus are clear beneficiaries (low d), collecting political power, resources, and immunity. Loyalist citizens, who prioritize stability, also benefit from the perceived order. Autonomous political parties, student movements, and civilian government officials are targets (high d), as their agency is curtailed, and they bear the costs of suppression and subordination. International human rights organizations act as analytical observers.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_interpretation_ambiguity,
    'Is the Charter''s ''guardian'' clause a genuine stability mechanism, or has it been reinterpreted as a pretext for military power and resource extraction?',
    'Independent historical analysis of the Charter''s drafting intent, comparative analysis with other post-revolutionary constitutions, and empirical assessment of whether military intervention correlates with actual threats or political challenges to military authority.',
    'If primarily a pretext, the constraint''s effective extractiveness and suppression are higher than even the current high measures suggest, as the coordination narrative is largely theatrical. If a genuine mechanism, the coordination function is more central, potentially shifting the classification towards a more balanced Tangled Rope or even a Rope if extraction were lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_interpretation_ambiguity, conceptual, 'Ambiguity between genuine coordination and extractive reinterpretation of the Charter''s military clause.').

omega_variable(
    mandate_obsolescence_ambiguity,
    'Has the original problem of post-revolutionary instability, which justified military guardianship, genuinely receded, or is it actively maintained/exaggerated as a pretext for continued military political dominance?',
    'Empirical assessment of internal and external security threats over time, independent of military reporting, and analysis of the military''s response to non-security-related political challenges.',
    'If the founding problem is dead or exaggerated, the constraint''s persistence is purely inertial or extractive, supporting a stronger Snare or Piton classification. If genuinely live, it reinforces the coordination aspect, even if extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_obsolescence_ambiguity, empirical, 'Whether the military''s founding mandate for stability is still relevant or obsolete.').

omega_variable(
    suppression_legitimacy_ambiguity,
    'Is the suppression of political dissent and civilian agency a necessary component of national security and stability, or is it a tool primarily for maintaining military political dominance?',
    'Analysis of the targets of suppression (e.g., violent extremists vs. peaceful political opposition), and comparison with states that achieve stability with greater political freedoms.',
    'If suppression is primarily for dominance, the constraint''s effective suppression is amplified, and its coordination claim is further undermined, strengthening a Snare classification. If genuinely necessary for security, it would be a cost of coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_legitimacy_ambiguity, preference, 'Whether political suppression is a security necessity or a tool of dominance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(july_charter_sovereign_legitimacy__military_custodian_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(july_tr_t0, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(july_tr_t10, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 10, 0.4).
narrative_ontology:measurement(july_tr_t20, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 20, 0.5).
narrative_ontology:measurement(july_tr_t30, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 30, 0.55).
narrative_ontology:measurement(july_tr_t40, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 40, 0.58).
narrative_ontology:measurement(july_tr_t50, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 50, 0.6).

% Extraction over time
narrative_ontology:measurement(july_be_t0, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(july_be_t10, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 10, 0.75).
narrative_ontology:measurement(july_be_t20, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 20, 0.8).
narrative_ontology:measurement(july_be_t30, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 30, 0.83).
narrative_ontology:measurement(july_be_t40, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 40, 0.84).
narrative_ontology:measurement(july_be_t50, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 50, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(july_su_t0, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(july_su_t10, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 10, 0.8).
narrative_ontology:measurement(july_su_t20, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 20, 0.85).
narrative_ontology:measurement(july_su_t30, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 30, 0.88).
narrative_ontology:measurement(july_su_t40, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 40, 0.89).
narrative_ontology:measurement(july_su_t50, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 50, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(july_charter_sovereign_legitimacy__military_custodian_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
