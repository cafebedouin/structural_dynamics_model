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
 *   human_readable: Herem Command (DT7): Durable Separation Reading
 *   domain: religious_ethics/commitment_system
 *
 * SUMMARY:
 *   This constraint represents the 'durable separation' reading of the
 *   biblical Herem command (DT7), which interprets it as a timeless divine
 *   mandate for the preservation of a distinct covenant identity through
 *   strict boundary maintenance and categorical separation from designated
 *   outsiders. This reading leads to high extraction of individual autonomy,
 *   particularly regarding intermarriage, and legitimizes exclusion or
 *   violence against those deemed a threat to communal purity. The constraint
 *   is classified as a Snare due to its high extractiveness, active
 *   enforcement, and identifiable victims, despite its claim to be a divinely
 *   ordained, identity-preserving mechanism.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(herem_command_dt7__durable_separation_reading, 0.88).
domain_priors:suppression_score(herem_command_dt7__durable_separation_reading, 0.92).
domain_priors:theater_ratio(herem_command_dt7__durable_separation_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(herem_command_dt7__durable_separation_reading, snare).
narrative_ontology:human_readable(herem_command_dt7__durable_separation_reading, "Herem Command (DT7): Durable Separation Reading").
narrative_ontology:topic_domain(herem_command_dt7__durable_separation_reading, "religious_ethics/commitment_system").

domain_priors:requires_active_enforcement(herem_command_dt7__durable_separation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(herem_command_dt7__durable_separation_reading, 'dcc41f38-c7e4-4110-945b-cbb2b895fca6').
narrative_ontology:cs_kernel_codification('dcc41f38-c7e4-4110-945b-cbb2b895fca6', fixed_text).
narrative_ontology:cs_authority_grounding('dcc41f38-c7e4-4110-945b-cbb2b895fca6', lineage).
narrative_ontology:cs_interpretation_layer_present('dcc41f38-c7e4-4110-945b-cbb2b895fca6').
narrative_ontology:cs_reading_relation('dcc41f38-c7e4-4110-945b-cbb2b895fca6', herem_command_dt7__contextual_supersession_reading, forecloses).
narrative_ontology:cs_reading_relation('dcc41f38-c7e4-4110-945b-cbb2b895fca6', herem_command_dt7__allegorical_displacement_reading, forecloses).
narrative_ontology:cs_axiom('dcc41f38-c7e4-4110-945b-cbb2b895fca6', foundational, divine_mandate_timeless_literal).
narrative_ontology:cs_axiom_status(divine_mandate_timeless_literal, holdable).
narrative_ontology:cs_axiom_grounding('dcc41f38-c7e4-4110-945b-cbb2b895fca6', divine_mandate_timeless_literal, theological).
narrative_ontology:cs_axiom('dcc41f38-c7e4-4110-945b-cbb2b895fca6', foundational, identity_purity_requires_separation).
narrative_ontology:cs_axiom_status(identity_purity_requires_separation, holdable).
narrative_ontology:cs_axiom_grounding('dcc41f38-c7e4-4110-945b-cbb2b895fca6', identity_purity_requires_separation, deontological).
narrative_ontology:cs_reference_frame('dcc41f38-c7e4-4110-945b-cbb2b895fca6', ancient_covenant_purity).
narrative_ontology:cs_drift_state('dcc41f38-c7e4-4110-945b-cbb2b895fca6', contemporary_pluralistic_society, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('dcc41f38-c7e4-4110-945b-cbb2b895fca6', '').
narrative_ontology:cs_kernel_id(herem_command_dt7__durable_separation_reading, herem_command_dt7).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(herem_command_dt7__durable_separation_reading, covenant_community_leaders).
narrative_ontology:constraint_beneficiary(herem_command_dt7__durable_separation_reading, covenant_community_members).
narrative_ontology:constraint_victim(herem_command_dt7__durable_separation_reading, designated_outsiders).
narrative_ontology:constraint_victim(herem_command_dt7__durable_separation_reading, intermarried_individuals).
narrative_ontology:constraint_victim(herem_command_dt7__durable_separation_reading, dissenting_community_members).
narrative_ontology:constraint_vindicates(herem_command_dt7__durable_separation_reading, divine_mandate_for_purity).
narrative_ontology:constraint_vindicates(herem_command_dt7__durable_separation_reading, covenant_identity_preservation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and enforce the Herem command as a timeless divine mandate for the community's identity and purity. They benefit from the authority derived from this interpretation and the social cohesion it enforces, but are identity-locked into upholding it.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, covenant_community_leaders, agenda_setter,
    institutional, generational, identity_locked, local).

% Benefit from a clear, divinely sanctioned identity and a sense of belonging and purity. They are also subject to the strictures of the command, particularly regarding separation, and face social pressure to conform.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, covenant_community_members, beneficiary,
    moderate, biographical, constrained, local).

% Are categorically separated from the covenant community, often facing exclusion, demonization, or even violence as a result of the Herem command. They bear the full cost of this separation without recourse.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, designated_outsiders, payer,
    powerless, generational, trapped, local).

% Individuals within the covenant community who marry outside the designated group. They face severe social ostracization, excommunication, or even violence, as their actions are seen as a direct threat to the community's purity and divine mandate.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, intermarried_individuals, payer,
    powerless, immediate, identity_locked, local).

% Members who question or resist the strict interpretation of Herem, advocating for more inclusive or contextual readings. They face social pressure, accusations of heresy, and potential exclusion from the community.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, dissenting_community_members, payer,
    powerless, biographical, constrained, local).

% Analyze the historical, linguistic, and ethical implications of the Herem command and its various interpretations. They are not directly subject to the command but can influence its understanding through their academic work.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, theological_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the covenant community's identity by establishing clear boundaries and rules for membership and interaction with outsiders, ensuring perceived divine favor and cultural preservation.
% TRANSFER_FUNCTION: Transfers autonomy and self-determination from individuals (especially intermarried individuals and designated outsiders) to the collective identity and its leadership, in exchange for perceived divine protection and communal purity.
% ABSENT_VOICES: The voices of designated outsiders and those who have been excommunicated for intermarriage are entirely absent from the interpretive and enforcement processes. They would argue for universal human dignity, contextual ethics, and the right to self-determination.
% DISAPPEARANCE_RATIONALE: If the Herem command, as interpreted for durable separation, vanished overnight, the covenant community's social structure, leadership authority, and self-understanding would undergo a profound crisis. Intermarriage would likely increase, boundaries with outsiders would blur, and the community would need to redefine its identity and relationship with the divine, leading to significant social and theological reorganization.
% FOUNDING_PROBLEM: The problem of maintaining a distinct covenant identity and preventing assimilation into surrounding cultures, ensuring the community's survival and adherence to divine law in a hostile or tempting environment.
% FOUNDING_PROBLEM_CORROBORATION: Community leaders and many members attest that the problem of identity preservation and cultural assimilation remains live, citing contemporary secular pressures and external influences. Theological scholars, from outside the benefiting parties, corroborate the historical context of the founding problem but often contest its contemporary applicability or the ethical implications of its 'durable separation' reading.
narrative_ontology:disappearance_verdict(herem_command_dt7__durable_separation_reading, world_rearranges).
narrative_ontology:founding_problem_status(herem_command_dt7__durable_separation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(herem_command_dt7__durable_separation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(herem_command_dt7__durable_separation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(herem_command_dt7__durable_separation_reading, 0.88, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is very high (0.88) because this reading demands significant personal sacrifice (e.g., foregoing intermarriage, accepting exclusion of outsiders) for the sake of communal purity and divine favor. Suppression is also very high (0.92) as the command is enforced through strong social pressure, theological condemnation, and potentially physical coercion or violence, with severe consequences for non-compliance. Theater ratio is low (0.1) because the command's function is actively maintained and believed to be directly efficacious in preserving identity and divine relationship, not merely performative. Accessibility collapse is high (0.75) as alternatives to strict separation are largely foreclosed by the theological framework, and resistance is moderate (0.3) due to the severe penalties for dissent.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of covenant community leaders and many members, this constraint is a divinely ordained, necessary mechanism for identity preservation and spiritual purity. From the perspective of designated outsiders, intermarried individuals, and dissenting members, it is a highly extractive and suppressive mechanism that denies fundamental rights and imposes severe costs under the guise of religious mandate. The engine's classification as a Snare highlights this divergence, exposing the extractive reality beneath the coordination claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Covenant community leaders are agenda-setters and beneficiaries, deriving authority and social cohesion from enforcing the command. Community members are beneficiaries of a clear identity but also bear costs of conformity. Designated outsiders and intermarried individuals are clear victims, bearing the full brunt of exclusion and loss of autonomy. Dissenting community members are also payers, facing social and theological penalties for questioning the command. The 'identity_locked' exit option for leaders and intermarried individuals reflects the deep personal and communal stakes involved.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_mandate_authenticity,
    'Is the ''durable separation'' reading of Herem a genuine, timeless divine mandate, or a human interpretation that serves institutional power and identity preservation?',
    'Theological consensus across diverse, independent interpretive traditions, or a direct, unambiguous divine revelation that clarifies the command''s scope and duration.',
    'If confirmed as a timeless divine mandate, the constraint''s ''naturalness'' (emerges_naturally) would increase, potentially shifting its classification towards a Mountain (though its extractiveness would remain high). If revealed as a human construct, its Snare classification would be further solidified, highlighting its coercive nature.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(divine_mandate_authenticity, conceptual, 'Ambiguity regarding the divine vs. human origin and timelessness of the ''durable separation'' interpretation.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (e.g., excommunication, social ostracism) or internalized (e.g., fear of divine wrath, deep-seated belief in purity requirements)?',
    'Post-exit suppression trajectory: if individuals who leave the community continue to self-regulate their behavior (e.g., avoid intermarriage) due to internalized beliefs, reclassify as partially internalized. If suppression immediately dissipates upon exit, it is primarily structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making true exit more difficult and costly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in maintaining separation.').

omega_variable(
    violence_legitimation_scope,
    'Does the ''durable separation'' reading implicitly or explicitly legitimate violence against designated outsiders or dissenting members, and what is the actual frequency/severity of such violence?',
    'Analysis of historical and contemporary community practices, theological rulings, and legal records. Empirical study of violence incidence and its direct attribution to this interpretation.',
    'If violence is a direct and frequent consequence, the constraint''s extractiveness and suppression would be even higher, and its Snare classification would be reinforced with a stronger ''victim'' component. If violence is rare or explicitly condemned by the reading, the severity of extraction might be slightly lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(violence_legitimation_scope, empirical, 'The extent to which the reading legitimates and results in violence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(herem_command_dt7__durable_separation_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(here_tr_t0, herem_command_dt7__durable_separation_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(here_tr_t10, herem_command_dt7__durable_separation_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(here_tr_t20, herem_command_dt7__durable_separation_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(here_tr_t30, herem_command_dt7__durable_separation_reading, theater_ratio, 30, 0.1).
narrative_ontology:measurement(here_tr_t40, herem_command_dt7__durable_separation_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(here_tr_t50, herem_command_dt7__durable_separation_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(here_be_t0, herem_command_dt7__durable_separation_reading, base_extractiveness, 0, 0.8).
narrative_ontology:measurement(here_be_t10, herem_command_dt7__durable_separation_reading, base_extractiveness, 10, 0.82).
narrative_ontology:measurement(here_be_t20, herem_command_dt7__durable_separation_reading, base_extractiveness, 20, 0.85).
narrative_ontology:measurement(here_be_t30, herem_command_dt7__durable_separation_reading, base_extractiveness, 30, 0.86).
narrative_ontology:measurement(here_be_t40, herem_command_dt7__durable_separation_reading, base_extractiveness, 40, 0.87).
narrative_ontology:measurement(here_be_t50, herem_command_dt7__durable_separation_reading, base_extractiveness, 50, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(here_su_t0, herem_command_dt7__durable_separation_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(here_su_t10, herem_command_dt7__durable_separation_reading, suppression_requirement, 10, 0.87).
narrative_ontology:measurement(here_su_t20, herem_command_dt7__durable_separation_reading, suppression_requirement, 20, 0.89).
narrative_ontology:measurement(here_su_t30, herem_command_dt7__durable_separation_reading, suppression_requirement, 30, 0.9).
narrative_ontology:measurement(here_su_t40, herem_command_dt7__durable_separation_reading, suppression_requirement, 40, 0.91).
narrative_ontology:measurement(here_su_t50, herem_command_dt7__durable_separation_reading, suppression_requirement, 50, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(herem_command_dt7__durable_separation_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
