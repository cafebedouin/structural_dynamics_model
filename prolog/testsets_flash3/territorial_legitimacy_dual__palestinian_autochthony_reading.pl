% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy_dual__palestinian_autochthony_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy_dual__palestinian_autochthony_reading, []).

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
 *   constraint_id: territorial_legitimacy_dual__palestinian_autochthony_reading
 *   human_readable: Palestinian Autochthony and Right of Return
 *   domain: political_theory/international_relations/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint story instantiates the 'Palestinian autochthony' reading
 *   of the broader 'territorial_legitimacy_dual' kernel. It frames
 *   Palestinian legitimacy as grounded in continuous habitation, the trauma
 *   of displacement, and the non-negotiable right of return. The 1948
 *   displacement is seen as an ongoing injustice requiring remedy,
 *   territorial reduction as severe deprivation, and the legitimacy of the
 *   Israeli state is contested from this perspective. The constraint operates
 *   as a snare, as it extracts heavily from displaced Palestinians and those
 *   in occupied territories, while its persistence relies on active
 *   resistance against counter-narratives and the suppression of alternative
 *   solutions that do not prioritize the right of return.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.95).
domain_priors:suppression_score(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.9).
domain_priors:theater_ratio(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, extractiveness, 0.95).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, resistance, 0.9).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy_dual__palestinian_autochthony_reading, snare).
narrative_ontology:human_readable(territorial_legitimacy_dual__palestinian_autochthony_reading, "Palestinian Autochthony and Right of Return").
narrative_ontology:topic_domain(territorial_legitimacy_dual__palestinian_autochthony_reading, "political_theory/international_relations/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy_dual__palestinian_autochthony_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy_dual__palestinian_autochthony_reading, '1ccbe2c4-e5eb-4678-b69f-b5e2366086ab').
narrative_ontology:cs_kernel_codification('1ccbe2c4-e5eb-4678-b69f-b5e2366086ab', distributed).
narrative_ontology:cs_authority_grounding('1ccbe2c4-e5eb-4678-b69f-b5e2366086ab', practice).
narrative_ontology:cs_interpretation_layer_present('1ccbe2c4-e5eb-4678-b69f-b5e2366086ab').
narrative_ontology:cs_reading_relation('1ccbe2c4-e5eb-4678-b69f-b5e2366086ab', territorial_legitimacy_dual__zionist_refuge_reading, coexists_with).
narrative_ontology:cs_reading_relation('1ccbe2c4-e5eb-4678-b69f-b5e2366086ab', territorial_legitimacy_dual__two_state_coexistence_reading, influences).
narrative_ontology:cs_axiom('1ccbe2c4-e5eb-4678-b69f-b5e2366086ab', foundational, continuous_habitation_confers_primary_sovereignty).
narrative_ontology:cs_axiom_status(continuous_habitation_confers_primary_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('1ccbe2c4-e5eb-4678-b69f-b5e2366086ab', continuous_habitation_confers_primary_sovereignty, deontological).
narrative_ontology:cs_axiom('1ccbe2c4-e5eb-4678-b69f-b5e2366086ab', foundational, right_of_return_is_inalienable).
narrative_ontology:cs_axiom_status(right_of_return_is_inalienable, holdable).
narrative_ontology:cs_axiom_grounding('1ccbe2c4-e5eb-4678-b69f-b5e2366086ab', right_of_return_is_inalienable, deontological).
narrative_ontology:cs_reference_frame('1ccbe2c4-e5eb-4678-b69f-b5e2366086ab', pre_1948_palestinian_sovereignty).
narrative_ontology:cs_drift_state('1ccbe2c4-e5eb-4678-b69f-b5e2366086ab', contemporary, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('1ccbe2c4-e5eb-4678-b69f-b5e2366086ab', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy_dual__palestinian_autochthony_reading, territorial_legitimacy_dual).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_diaspora).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_political_leadership).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_refugees).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_residents_occupied_territories).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Displaced from their ancestral lands in 1948 and subsequent conflicts, they bear the direct cost of dispossession and are denied the right to return. Their identity is deeply tied to their original homes.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_refugees, payer,
    powerless, generational, trapped, regional).

% Live under military occupation, facing restrictions on movement, land confiscation, and limited self-governance. They bear the costs of territorial reduction and contested sovereignty.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_residents_occupied_territories, payer,
    powerless, biographical, constrained, local).

% Maintains a strong connection to the homeland and the right of return, which serves as a unifying principle for their identity and political advocacy. While not directly paying, they benefit from the moral claim of continuous habitation.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_diaspora, beneficiary,
    moderate, generational, identity_locked, global).

% Articulates and defends the narrative of autochthony and the right of return, using it as a foundational claim for statehood and international recognition. Their legitimacy is tied to upholding these principles.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_political_leadership, agenda_setter,
    organized, generational, constrained, regional).

% The primary counter-party whose legitimacy is contested by this reading. It actively denies the right of return and asserts its own sovereignty over the disputed territories. It is excluded from the internal framing of Palestinian autochthony.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, israeli_state, excluded,
    institutional, generational, arbitrage, national).

% Document human rights violations and advocate for the rights of Palestinian refugees, including the right of return, often aligning with the autochthony narrative in their legal interpretations.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, international_human_rights_organizations, observer,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates Palestinian national identity and political aspirations around a shared narrative of historical connection to the land and the injustice of displacement, providing a basis for collective action and international advocacy.
% TRANSFER_FUNCTION: Transfers moral and political legitimacy from continuous habitation and historical grievance to the Palestinian national movement, while imposing the cost of displacement and territorial loss on refugees and residents.
% ABSENT_VOICES: The Israeli state and its supporters are structurally excluded from this narrative's internal logic; they would argue for their own historical claims and security needs, which are rendered illegitimate by this reading.
% DISAPPEARANCE_RATIONALE: If the claim of Palestinian autochthony and the right of return vanished, the core unifying principle of Palestinian national identity would dissolve, leading to a fundamental reordering of political goals, international advocacy, and the self-conception of millions of refugees and residents.
% FOUNDING_PROBLEM: The dispossession and displacement of Palestinians in 1948 and subsequent conflicts, leading to a loss of land, identity, and self-determination.
% FOUNDING_PROBLEM_CORROBORATION: Palestinian refugees, international human rights organizations, and UN resolutions attest to the ongoing nature of the displacement and the unresolved question of return. The problem is widely recognized outside the immediate beneficiaries of the narrative.
narrative_ontology:disappearance_verdict(territorial_legitimacy_dual__palestinian_autochthony_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy_dual__palestinian_autochthony_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy_dual__palestinian_autochthony_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(territorial_legitimacy_dual__palestinian_autochthony_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.95, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy_dual__palestinian_autochthony_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy_dual__palestinian_autochthony_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy_dual__palestinian_autochthony_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.95) because the core of this constraint is the ongoing dispossession and denial of rights for a large population, which is a severe form of extraction. Suppression is also very high (0.9) as the counter-narratives and political alternatives (e.g., full acceptance of the Israeli state without right of return) are actively resisted and delegitimized within this framework. Theater ratio is low (0.1) because the claims are deeply held and actively pursued, with little performative maintenance; the struggle is existential. Accessibility collapse is high (0.85) because for those who adopt this reading, alternatives that do not center autochthony and return are largely unthinkable or unacceptable. Resistance is high (0.9) reflecting the continuous struggle and advocacy by Palestinians and their supporters.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Palestinian refugees and residents, the constraint is a deeply felt, ongoing injustice. For the Palestinian political leadership, it is a foundational claim for national liberation. The Israeli state, from its own perspective, would view this as a challenge to its very existence, not a 'constraint' it imposes.
 *
 * DIRECTIONALITY LOGIC:
 *   Palestinian refugees and residents of occupied territories are the primary targets/victims, bearing the direct costs of displacement and territorial reduction (high d). The Palestinian diaspora and political leadership are beneficiaries, as this narrative provides a unifying identity and political platform (low d). The Israeli state is excluded from this framing, as its legitimacy is contested by the core tenets of this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in the traditional sense, as its mandate (addressing historical injustice and displacement) is considered 'live' and ongoing by its proponents. The high extractiveness and suppression are not signs of atrophy but rather of an active, contested struggle for fundamental rights and recognition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_narrative_contestation,
    'To what extent is the historical narrative of continuous habitation and displacement universally accepted, or is it contested by alternative historical accounts?',
    'Comprehensive, independently verified historical research and archaeological evidence, accepted by a broad international consensus.',
    'If the historical narrative is found to be significantly flawed or selectively presented, the moral and political force of the autochthony claim would diminish, potentially shifting the constraint''s extractiveness and suppression metrics downwards. If corroborated, its legitimacy would be strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_narrative_contestation, empirical, 'Ambiguity regarding the empirical grounding of the historical narrative.').

omega_variable(
    right_of_return_feasibility,
    'Is the full implementation of the ''right of return'' for all refugees practically feasible without creating new displacements or severe demographic instability?',
    'Detailed demographic and logistical studies, coupled with political negotiations exploring various models of return, compensation, and integration.',
    'If deemed infeasible, the ''right of return'' might transition from a non-negotiable demand to a basis for compensation or symbolic recognition, altering the constraint''s core demands and potentially reducing its perceived extractiveness for some parties. If feasible, it reinforces the current framing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(right_of_return_feasibility, preference, 'Uncertainty about the practical and political feasibility of the core demand.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (external barriers) or internalized (cognitive patterns that persist after barrier removal)?',
    'Post-exit suppression trajectory: if suppression persists after the extractive mechanism is removed, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy_dual__palestinian_autochthony_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1948, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 1948, 0.05).
narrative_ontology:measurement(terr_tr_t1967, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 1967, 0.08).
narrative_ontology:measurement(terr_tr_t1987, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 1987, 0.1).
narrative_ontology:measurement(terr_tr_t2000, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(terr_tr_t2014, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 2014, 0.1).
narrative_ontology:measurement(terr_tr_t2024, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(terr_be_t1948, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 1948, 0.85).
narrative_ontology:measurement(terr_be_t1967, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 1967, 0.9).
narrative_ontology:measurement(terr_be_t1987, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 1987, 0.92).
narrative_ontology:measurement(terr_be_t2000, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 2000, 0.93).
narrative_ontology:measurement(terr_be_t2014, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 2014, 0.94).
narrative_ontology:measurement(terr_be_t2024, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 2024, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1948, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 1948, 0.8).
narrative_ontology:measurement(terr_su_t1967, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 1967, 0.85).
narrative_ontology:measurement(terr_su_t1987, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 1987, 0.88).
narrative_ontology:measurement(terr_su_t2000, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 2000, 0.89).
narrative_ontology:measurement(terr_su_t2014, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 2014, 0.9).
narrative_ontology:measurement(terr_su_t2024, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy_dual__palestinian_autochthony_reading, identity_coordination).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__palestinian_autochthony_reading, territorial_legitimacy_dual__zionist_refuge_reading).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__palestinian_autochthony_reading, territorial_legitimacy_dual__two_state_coexistence_reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
