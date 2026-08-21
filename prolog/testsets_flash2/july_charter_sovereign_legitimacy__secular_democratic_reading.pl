% ============================================================================
% CONSTRAINT STORY: july_charter_sovereign_legitimacy__secular_democratic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_july_charter_sovereign_legitimacy__secular_democratic_reading, []).

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
 *   constraint_id: july_charter_sovereign_legitimacy__secular_democratic_reading
 *   human_readable: July Charter: Secular Democratic Reading (Military Subordination)
 *   domain: constitutional_law/political_transitions/post_revolutionary_state_building
 *
 * SUMMARY:
 *   This constraint represents the 'secular democratic' reading of the July
 *   Charter, which mandates secular democratic institutions and the
 *   subordination of the military to civilian authority. This reading is
 *   contested by other interpretations of the same Charter. The constraint
 *   functions as a Tangled Rope, providing a coordination framework for
 *   secular actors while actively extracting from and suppressing religious
 *   political movements and military autonomy. The metrics reflect the
 *   ongoing struggle to enforce this reading against powerful
 *   counter-interpretations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.65).
domain_priors:suppression_score(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.78).
domain_priors:theater_ratio(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(july_charter_sovereign_legitimacy__secular_democratic_reading, tangled_rope).
narrative_ontology:human_readable(july_charter_sovereign_legitimacy__secular_democratic_reading, "July Charter: Secular Democratic Reading (Military Subordination)").
narrative_ontology:topic_domain(july_charter_sovereign_legitimacy__secular_democratic_reading, "constitutional_law/political_transitions/post_revolutionary_state_building").

domain_priors:requires_active_enforcement(july_charter_sovereign_legitimacy__secular_democratic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(july_charter_sovereign_legitimacy__secular_democratic_reading, '5d1a7071-e3fc-47b0-9a94-6b2a3af435fd').
narrative_ontology:cs_kernel_codification('5d1a7071-e3fc-47b0-9a94-6b2a3af435fd', fixed_text).
narrative_ontology:cs_authority_grounding('5d1a7071-e3fc-47b0-9a94-6b2a3af435fd', lineage).
narrative_ontology:cs_interpretation_layer_present('5d1a7071-e3fc-47b0-9a94-6b2a3af435fd').
narrative_ontology:cs_reading_relation('5d1a7071-e3fc-47b0-9a94-6b2a3af435fd', july_charter_sovereign_legitimacy__guided_nationalism_reading, coexists_with).
narrative_ontology:cs_reading_relation('5d1a7071-e3fc-47b0-9a94-6b2a3af435fd', july_charter_sovereign_legitimacy__military_custodian_reading, coexists_with).
narrative_ontology:cs_axiom('5d1a7071-e3fc-47b0-9a94-6b2a3af435fd', foundational, civilian_supremacy_over_military).
narrative_ontology:cs_axiom_status(civilian_supremacy_over_military, holdable).
narrative_ontology:cs_axiom_grounding('5d1a7071-e3fc-47b0-9a94-6b2a3af435fd', civilian_supremacy_over_military, conventional).
narrative_ontology:cs_axiom('5d1a7071-e3fc-47b0-9a94-6b2a3af435fd', foundational, state_secularism_as_neutrality).
narrative_ontology:cs_axiom_status(state_secularism_as_neutrality, holdable).
narrative_ontology:cs_axiom_grounding('5d1a7071-e3fc-47b0-9a94-6b2a3af435fd', state_secularism_as_neutrality, conventional).
narrative_ontology:cs_reference_frame('5d1a7071-e3fc-47b0-9a94-6b2a3af435fd', post_revolutionary_democratic_ideal).
narrative_ontology:cs_drift_state('5d1a7071-e3fc-47b0-9a94-6b2a3af435fd', contemporary_political_contest, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5d1a7071-e3fc-47b0-9a94-6b2a3af435fd', '').
narrative_ontology:cs_kernel_id(july_charter_sovereign_legitimacy__secular_democratic_reading, july_charter_sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__secular_democratic_reading, secular_political_parties).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__secular_democratic_reading, civil_society_organizations).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__secular_democratic_reading, jamaat_e_islami).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__secular_democratic_reading, military_autonomous_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These parties derive their legitimacy and operational space from the Charter's secular and democratic provisions. They benefit from the exclusion or constraint of religious political rivals and the theoretical subordination of the military, though this subordination is often challenged in practice.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, secular_political_parties, beneficiary,
    organized, biographical, constrained, national).

% Advocate for and operate within the framework of secular democratic institutions. They benefit from the Charter's stated principles, which provide a legal and normative basis for their activities, but face suppression when their advocacy challenges powerful actors.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, civil_society_organizations, beneficiary,
    moderate, biographical, constrained, national).

% A major religious political party whose ideology is fundamentally at odds with the Charter's secular mandate. They are systematically excluded from formal power structures and face legal restrictions, bearing the cost of the Charter's enforcement against their political program. Their identity is fused with their religious-political mission, making exit unthinkable.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, jamaat_e_islami, payer,
    organized, generational, identity_locked, national).

% The military, as an institution, is theoretically subordinated to civilian authority by this reading of the Charter. This constrains its historical role as a political arbiter and guardian, forcing it to operate under civilian oversight, which it often resists or circumvents. It pays the cost of reduced autonomy and political influence.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, military_autonomous_authority, payer,
    institutional, generational, constrained, national).

% Monitor the implementation of the Charter's democratic and secular principles, often providing reports and recommendations. They view the Charter as a foundational document for a democratic transition and assess its adherence to international norms.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, international_democratic_observers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for a secular democratic state, coordinating the roles of various political and civil actors under civilian rule and defining the boundaries of legitimate political participation.
% TRANSFER_FUNCTION: Transfers political authority and legitimacy from religious or military institutions to civilian, secular democratic bodies. It also transfers the right to define national identity from religious groups to a broader, secular citizenry.
% ABSENT_VOICES: Actors advocating for a theocratic state or for permanent military oversight are structurally excluded from the legitimate discourse defined by this reading of the Charter. Their voices are suppressed by the very framework that secular democratic parties benefit from.
% DISAPPEARANCE_RATIONALE: If this reading of the Charter vanished, the foundational principles of the state would be immediately contested. Religious political parties and the military would likely assert greater authority, leading to a rapid reordering of political power and potentially civil unrest as the secular democratic framework collapses.
% FOUNDING_PROBLEM: The Charter was established to transition the nation from a period of authoritarian rule and religious-military influence towards a stable, civilian-led, secular democratic system, ensuring political participation and human rights.
% FOUNDING_PROBLEM_CORROBORATION: Secular political parties and civil society organizations attest that the founding problem of establishing a stable secular democracy is still live, citing ongoing challenges from religious extremism and military interference. However, religious parties and military factions contest this, arguing the Charter's original intent was different or that the problem has evolved beyond its scope.
narrative_ontology:disappearance_verdict(july_charter_sovereign_legitimacy__secular_democratic_reading, world_rearranges).
narrative_ontology:founding_problem_status(july_charter_sovereign_legitimacy__secular_democratic_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(july_charter_sovereign_legitimacy__secular_democratic_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(july_charter_sovereign_legitimacy__secular_democratic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(july_charter_sovereign_legitimacy__secular_democratic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__secular_democratic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(july_charter_sovereign_legitimacy__secular_democratic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) and suppression (0.78) are high because this reading of the Charter actively excludes and constrains significant political forces (religious parties, military autonomy). The coordination function for secular democratic actors is genuine, but it comes at a substantial cost to those whose visions for the state are incompatible. The theater ratio (0.40) indicates that while the secular democratic framework is genuinely pursued, a significant portion of its public presentation and enforcement is performative, masking the underlying contestation and the military's de facto influence.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of secular democratic parties, the Charter is a Rope, coordinating a just and inclusive political order. From the perspective of Jamaat-e-Islami, it is a Snare, designed to exclude and suppress their legitimate political expression. The military, while officially subordinate, often operates as if it were an agenda-setter, creating a gap between the Charter's stated intent and its practical application.
 *
 * DIRECTIONALITY LOGIC:
 *   Secular political parties and civil society organizations are beneficiaries, as their existence and legitimacy are affirmed by this reading. Jamaat-e-Islami and the military's autonomous authority are victims, as their political and institutional power is curtailed. International observers are analytical, assessing adherence to democratic norms. The high suppression is directed at the victims to maintain the dominance of this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    military_de_facto_power,
    'To what extent does the military''s de facto political power contradict its de jure subordination under this reading of the Charter?',
    'Empirical analysis of military interventions in civilian governance, budget allocations, and influence on policy decisions over time. Comparison of constitutional text with actual practice.',
    'If de facto power is high, the ''military subordination'' aspect of this reading is largely theatrical, increasing the constraint''s effective extractiveness and suppression for civilian actors, potentially reclassifying it closer to a Snare or a Piton for civilian authority.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(military_de_facto_power, empirical, 'Gap between military''s constitutional role and actual political influence.').

omega_variable(
    secularism_vs_religious_identity,
    'Is the Charter''s secular mandate fundamentally incompatible with the religious identity of a significant portion of the population, or can a pluralistic secularism accommodate religious political expression?',
    'Sociological studies of public opinion on secularism and religious politics, and comparative analysis with other states that successfully integrate religious identity within a secular framework.',
    'If fundamentally incompatible, the suppression of religious parties is an inherent feature of this reading, making it more extractive. If accommodable, the current suppression is a policy choice, and alternatives exist that would reduce extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secularism_vs_religious_identity, conceptual, 'Compatibility of Charter''s secularism with national religious identity.').

omega_variable(
    reading_legitimacy_contest,
    'Which reading of the July Charter (secular democratic, guided nationalism, or military custodian) holds the greatest popular legitimacy and institutional support?',
    'Longitudinal public opinion surveys, analysis of electoral outcomes, and assessment of institutional adherence to each reading''s principles by key state actors (judiciary, bureaucracy).',
    'If this secular democratic reading loses legitimacy, its enforcement becomes more costly and its persistence more precarious, potentially shifting its classification towards a Piton or even collapse. If it gains legitimacy, its extractiveness might decrease as resistance wanes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_legitimacy_contest, empirical, 'The relative legitimacy and support for the competing readings of the Charter.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(july_charter_sovereign_legitimacy__secular_democratic_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(july_tr_t0, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(july_tr_t5, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 5, 0.33).
narrative_ontology:measurement(july_tr_t10, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement(july_tr_t15, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement(july_tr_t20, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement(july_tr_t25, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 25, 0.4).

% Extraction over time
narrative_ontology:measurement(july_be_t0, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(july_be_t5, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(july_be_t10, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(july_be_t15, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(july_be_t20, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 20, 0.64).
narrative_ontology:measurement(july_be_t25, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 25, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(july_su_t0, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(july_su_t5, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 5, 0.72).
narrative_ontology:measurement(july_su_t10, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 10, 0.74).
narrative_ontology:measurement(july_su_t15, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 15, 0.76).
narrative_ontology:measurement(july_su_t20, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 20, 0.77).
narrative_ontology:measurement(july_su_t25, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 25, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(july_charter_sovereign_legitimacy__secular_democratic_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__secular_democratic_reading, july_charter_sovereign_legitimacy__guided_nationalism_reading).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__secular_democratic_reading, july_charter_sovereign_legitimacy__military_custodian_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'July Charter Sovereign Legitimacy' kernel. Each reading defines a different state structure and set of legitimate actors, leading to different extraction and suppression profiles. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
