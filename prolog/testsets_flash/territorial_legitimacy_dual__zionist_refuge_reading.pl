% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy_dual__zionist_refuge_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy_dual__zionist_refuge_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: territorial_legitimacy_dual__zionist_refuge_reading
 *   human_readable: Zionist Refuge Reading of Israeli Territorial Legitimacy
 *   domain: political_theory/international_relations/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint represents the 'Zionist Refuge' reading of Israeli
 *   territorial legitimacy, which grounds the State of Israel's right to
 *   exist and control territory in historical persecution (the Holocaust), a
 *   divine promise (biblical claims to the land), and the acceptance of the
 *   1947 UN Partition Plan. This reading frames the 1948 establishment of
 *   Israel as uncontested and views subsequent territorial control,
 *   particularly after 1967, through the lens of security imperatives and the
 *   consequences of Arab rejection of partition, leading to Palestinian
 *   displacement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy_dual__zionist_refuge_reading, 0.6).
domain_priors:suppression_score(territorial_legitimacy_dual__zionist_refuge_reading, 0.7).
domain_priors:theater_ratio(territorial_legitimacy_dual__zionist_refuge_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy_dual__zionist_refuge_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy_dual__zionist_refuge_reading, "Zionist Refuge Reading of Israeli Territorial Legitimacy").
narrative_ontology:topic_domain(territorial_legitimacy_dual__zionist_refuge_reading, "political_theory/international_relations/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy_dual__zionist_refuge_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy_dual__zionist_refuge_reading, '28c89a64-56cd-49bb-9095-eaf035ee220e').
narrative_ontology:cs_kernel_codification('28c89a64-56cd-49bb-9095-eaf035ee220e', formalized).
narrative_ontology:cs_authority_grounding('28c89a64-56cd-49bb-9095-eaf035ee220e', lineage).
narrative_ontology:cs_interpretation_layer_present('28c89a64-56cd-49bb-9095-eaf035ee220e').
narrative_ontology:cs_reading_relation('28c89a64-56cd-49bb-9095-eaf035ee220e', territorial_legitimacy_dual__palestinian_autochthony_reading, coexists_with).
narrative_ontology:cs_reading_relation('28c89a64-56cd-49bb-9095-eaf035ee220e', territorial_legitimacy_dual__two_state_coexistence_reading, coexists_with).
narrative_ontology:cs_axiom('28c89a64-56cd-49bb-9095-eaf035ee220e', foundational, jewish_people_right_to_self_determination_in_ancestral_land).
narrative_ontology:cs_axiom_status(jewish_people_right_to_self_determination_in_ancestral_land, holdable).
narrative_ontology:cs_axiom_grounding('28c89a64-56cd-49bb-9095-eaf035ee220e', jewish_people_right_to_self_determination_in_ancestral_land, deontological).
narrative_ontology:cs_axiom('28c89a64-56cd-49bb-9095-eaf035ee220e', foundational, security_imperative_justifies_territorial_control).
narrative_ontology:cs_axiom_status(security_imperative_justifies_territorial_control, holdable).
narrative_ontology:cs_axiom_grounding('28c89a64-56cd-49bb-9095-eaf035ee220e', security_imperative_justifies_territorial_control, instrumental).
narrative_ontology:cs_reference_frame('28c89a64-56cd-49bb-9095-eaf035ee220e', un_partition_and_1948_establishment).
narrative_ontology:cs_drift_state('28c89a64-56cd-49bb-9095-eaf035ee220e', contemporary_international_law_and_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('28c89a64-56cd-49bb-9095-eaf035ee220e', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy_dual__zionist_refuge_reading, territorial_legitimacy_dual).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__zionist_refuge_reading, state_of_israel).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__zionist_refuge_reading, israeli_citizens).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_refugees).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_population_occupied_territories).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary actor asserting and enforcing this reading of legitimacy, controlling territory, and managing security. Its existence is predicated on this foundational narrative, making exit from it an existential threat.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, state_of_israel, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from the security and territorial control afforded by this legitimacy claim. Their identity and sense of belonging are deeply intertwined with the state's narrative, making alternative frameworks difficult to accept.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, israeli_citizens, beneficiary,
    organized, biographical, identity_locked, national).

% Bear the costs of displacement and the denial of their right of return, as this reading of legitimacy does not accommodate their claims to ancestral lands within Israel's 1948 borders. They are trapped by international inaction and Israeli policy.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_refugees, payer,
    powerless, generational, trapped, regional).

% Experience daily life under military occupation and administrative control, with restricted movement, land confiscation, and limited self-determination, all justified by security concerns stemming from this legitimacy framework. Their options are resistance or emigration.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_population_occupied_territories, payer,
    powerless, generational, trapped, local).

% Observes and often mediates the conflict, with varying degrees of acceptance or challenge to this legitimacy reading. Its actions are constrained by geopolitical interests and the complexities of international law.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, international_community, observer,
    institutional, generational, analytical, global).

% Document and condemn human rights abuses stemming from the enforcement of this legitimacy reading, but often lack the power to directly alter the constraint. They are excluded from the core decision-making processes.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, human_rights_organizations, excluded,
    moderate, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_legitimacy_dual__zionist_refuge_reading, state_of_israel).
narrative_ontology:fixing_cost_class(territorial_legitimacy_dual__zionist_refuge_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a foundational narrative for the existence and security of the State of Israel, coordinating the identity and actions of its citizens and institutions around a shared purpose of national self-preservation.
% TRANSFER_FUNCTION: Transfers territorial control, sovereignty, and security from a contested status to the State of Israel, while transferring displacement, loss of land, and restricted rights to the Palestinian population.
% ABSENT_VOICES: Palestinian voices, particularly those advocating for the right of return or a single binational state, are largely absent from the dominant discourse that upholds this reading of legitimacy. Their narratives are actively suppressed or marginalized within the Israeli political system and often in international forums that prioritize a two-state solution.
% DISAPPEARANCE_RATIONALE: If this reading of legitimacy vanished, the State of Israel's foundational claims to its current territory and its right to enforce security measures would be fundamentally challenged. This would necessitate a complete re-evaluation of borders, refugee status, and the rights of all inhabitants, leading to a profound rearrangement of the political and territorial landscape.
% FOUNDING_PROBLEM: The historical persecution of Jewish people, culminating in the Holocaust, created an urgent need for a secure homeland and refuge, leading to the Zionist movement's call for a Jewish state in Palestine.
% FOUNDING_PROBLEM_CORROBORATION: The State of Israel and its citizens attest that the founding problem of Jewish security remains live, citing ongoing regional threats and historical antisemitism. International Jewish organizations and some Western governments corroborate the historical persecution and the need for a Jewish homeland. However, Palestinian narratives and some international bodies contest that the current territorial arrangements, justified by this problem, have created new injustices that must be addressed, suggesting the problem's 'solution' has become a new problem.
narrative_ontology:disappearance_verdict(territorial_legitimacy_dual__zionist_refuge_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy_dual__zionist_refuge_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy_dual__zionist_refuge_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(territorial_legitimacy_dual__zionist_refuge_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy_dual__zionist_refuge_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy_dual__zionist_refuge_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy_dual__zionist_refuge_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.6) is substantial, as this reading justifies the ongoing control of territory and resources, and the denial of return for Palestinian refugees. Suppression (0.7) is high due to the active military and administrative enforcement required to maintain territorial control and manage the Palestinian population in the occupied territories. Theater ratio (0.2) is low, as the justifications are actively invoked and defended, and the enforcement is functional in maintaining the status quo, not merely performative. Accessibility collapse (0.6) is moderate, as alternatives like a single binational state or a fully independent Palestinian state are actively suppressed but remain conceptual possibilities. Resistance (0.8) is high, reflecting ongoing Palestinian resistance and international diplomatic challenges.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the State of Israel and its citizens, this reading provides a foundational and existential justification for their presence and security, framing the constraint as a necessary 'rope' for survival. From the perspective of Palestinian refugees and the population in occupied territories, the same constraint is experienced as a 'snare' that denies their rights and extracts their land and sovereignty. The engine's classification will reflect this divergence based on the declared roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   The State of Israel and Israeli citizens are primary beneficiaries (d near 0.0) as they gain security, sovereignty, and control over territory. Palestinian refugees and the population in occupied territories are primary victims (d near 1.0) as they bear the costs of displacement, loss of land, and restricted movement. International bodies and human rights organizations act as observers or sometimes as payers, bearing the diplomatic and humanitarian costs of the conflict.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in the traditional sense, as its core justifications (historical persecution, divine promise, UN partition) are seen as immutable by its proponents. However, the 'founding problem' of Jewish refuge and security is contested in its 'status' (live vs. dead) by external observers, who argue that the current territorial arrangements exceed the original mandate and have become extractive. The persistence of the constraint relies on active enforcement and the suppression of alternative narratives, rather than a self-evident, universally accepted coordination function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_narrative_ambiguity,
    'Is the historical persecution and divine promise a foundational justification for exclusive territorial claim, or a contributing factor to a claim that must also be reconciled with other historical narratives?',
    'International legal consensus on self-determination and territorial rights, or a negotiated settlement acknowledging multiple historical claims.',
    'If foundational and exclusive, the constraint remains highly extractive for Palestinians. If reconciled, the constraint''s legitimacy shifts towards a shared framework, reducing extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_narrative_ambiguity, conceptual, 'Ambiguity of historical narrative as exclusive vs. shared justification.').

omega_variable(
    un_partition_interpretation,
    'Is the UN Partition Plan (Resolution 181) a permanent and sufficient basis for the State of Israel''s territorial legitimacy, or was it a transitional proposal whose terms were superseded by subsequent events and international law?',
    'Re-evaluation by international legal bodies or a new UN resolution addressing the current territorial dispute.',
    'If permanent, it reinforces the 1948 legitimacy and frames Palestinian displacement as a consequence of rejection. If superseded, it weakens the historical legal claim and opens space for alternative territorial arrangements.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(un_partition_interpretation, empirical, 'Interpretation of UN Partition Plan as permanent or transitional.').

omega_variable(
    kernel_reading_identification,
    'This constraint is a specific reading of the ''territorial_legitimacy_dual'' kernel. What would change if the ''palestinian_autochthony_reading'' or ''two_state_coexistence_reading'' were adopted?',
    'Adoption of a different reading by a dominant political or international actor.',
    'Adopting ''palestinian_autochthony_reading'' would invert the beneficiary/victim structure and challenge the 1948 legitimacy. Adopting ''two_state_coexistence_reading'' would shift towards mutual recognition and a negotiated settlement, reducing the unilateral extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Impact of alternative readings of the territorial_legitimacy_dual kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy_dual__zionist_refuge_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t0, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(terr_tr_t10, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(terr_tr_t20, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(terr_be_t0, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(terr_be_t10, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(terr_be_t20, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 20, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t0, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(terr_su_t10, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(terr_su_t20, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy_dual__zionist_refuge_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_autochthony_reading).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__zionist_refuge_reading, two_state_coexistence_reading).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__zionist_refuge_reading, israeli_settlement_expansion).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__zionist_refuge_reading, gaza_blockade).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
