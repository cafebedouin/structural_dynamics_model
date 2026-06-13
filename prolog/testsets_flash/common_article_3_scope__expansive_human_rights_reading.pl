% ============================================================================
% CONSTRAINT STORY: common_article_3_scope__expansive_human_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_article_3_scope__expansive_human_rights_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: common_article_3_scope__expansive_human_rights_reading
 *   human_readable: Common Article 3 Scope: Expansive Human Rights Reading
 *   domain: international_humanitarian_law/human_rights
 *
 * SUMMARY:
 *   This constraint represents the 'expansive human rights reading' of Common
 *   Article 3 (CA3) of the Geneva Conventions, which asserts that CA3's
 *   minimum humanitarian standards apply to any organized armed violence,
 *   regardless of its formal classification as an international or
 *   non-international armed conflict. This reading broadens the scope of
 *   protection for detainees and affected populations and subjects state
 *   security operations to external monitoring and potential prosecution,
 *   even in situations traditionally considered internal disturbances or law
 *   enforcement actions. It is a contested interpretation, often championed
 *   by human rights organizations and international courts, but resisted by
 *   states concerned about sovereignty and operational flexibility.
 *
 * KEY AGENTS:
 *   - human_rights_advocates: Primary beneficiary (institutional/arbitrage) — benefits from broader application, uses it to hold states accountable.
 *   - international_courts: Primary beneficiary (institutional/analytical) — gains jurisdiction and legal basis for prosecution.
 *   - state_security_forces: Primary victim (institutional/constrained) — faces increased scrutiny, legal obligations, and potential prosecution.
 *   - non_state_armed_groups: Primary victim (organized/constrained) — theoretically bound by CA3, but enforcement is difficult and often asymmetric.
 *   - detained_populations: Primary victim (powerless/trapped) — direct targets of violence, but also beneficiaries of the protections this reading seeks to enforce.
 *   - affected_civilians: Primary victim (powerless/trapped) — direct targets of violence, but also beneficiaries of the protections this reading seeks to enforce.
 *   - states_resisting_expansion: Payer (institutional/constrained) — bears the costs of increased legal obligations and reduced operational flexibility.
 *   - icrc_legal_scholars: Observer (analytical/analytical) — tracks state practice and customary law, often taking a more conservative view of CA3's scope.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_article_3_scope__expansive_human_rights_reading, 0.65).
domain_priors:suppression_score(common_article_3_scope__expansive_human_rights_reading, 0.7).
domain_priors:theater_ratio(common_article_3_scope__expansive_human_rights_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_article_3_scope__expansive_human_rights_reading, tangled_rope).
narrative_ontology:human_readable(common_article_3_scope__expansive_human_rights_reading, "Common Article 3 Scope: Expansive Human Rights Reading").
narrative_ontology:topic_domain(common_article_3_scope__expansive_human_rights_reading, "international_humanitarian_law/human_rights").

domain_priors:requires_active_enforcement(common_article_3_scope__expansive_human_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_article_3_scope__expansive_human_rights_reading, '5df5982e-909e-45d1-b5a7-994bc0ec21dd').
narrative_ontology:cs_kernel_codification('5df5982e-909e-45d1-b5a7-994bc0ec21dd', fixed_text).
narrative_ontology:cs_authority_grounding('5df5982e-909e-45d1-b5a7-994bc0ec21dd', lineage).
narrative_ontology:cs_interpretation_layer_present('5df5982e-909e-45d1-b5a7-994bc0ec21dd').
narrative_ontology:cs_reading_relation('5df5982e-909e-45d1-b5a7-994bc0ec21dd', common_article_3_scope__state_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('5df5982e-909e-45d1-b5a7-994bc0ec21dd', common_article_3_scope__icrc_customary_reading, coexists_with).
narrative_ontology:cs_axiom('5df5982e-909e-45d1-b5a7-994bc0ec21dd', foundational, universal_human_dignity_in_conflict).
narrative_ontology:cs_axiom_status(universal_human_dignity_in_conflict, holdable).
narrative_ontology:cs_axiom_grounding('5df5982e-909e-45d1-b5a7-994bc0ec21dd', universal_human_dignity_in_conflict, deontological).
narrative_ontology:cs_axiom('5df5982e-909e-45d1-b5a7-994bc0ec21dd', foundational, applicability_independent_of_classification).
narrative_ontology:cs_axiom_status(applicability_independent_of_classification, holdable).
narrative_ontology:cs_axiom_grounding('5df5982e-909e-45d1-b5a7-994bc0ec21dd', applicability_independent_of_classification, conventional).
narrative_ontology:cs_reference_frame('5df5982e-909e-45d1-b5a7-994bc0ec21dd', post_nuremberg_human_rights_era).
narrative_ontology:cs_drift_state('5df5982e-909e-45d1-b5a7-994bc0ec21dd', contemporary_asymmetric_conflict_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('5df5982e-909e-45d1-b5a7-994bc0ec21dd', '').
narrative_ontology:cs_kernel_id(common_article_3_scope__expansive_human_rights_reading, common_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_article_3_scope__expansive_human_rights_reading, human_rights_advocates).
narrative_ontology:constraint_beneficiary(common_article_3_scope__expansive_human_rights_reading, international_courts).
narrative_ontology:constraint_victim(common_article_3_scope__expansive_human_rights_reading, state_security_forces).
narrative_ontology:constraint_victim(common_article_3_scope__expansive_human_rights_reading, non_state_armed_groups).
narrative_ontology:constraint_victim(common_article_3_scope__expansive_human_rights_reading, detained_populations).
narrative_ontology:constraint_victim(common_article_3_scope__expansive_human_rights_reading, affected_civilians).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(common_article_3_scope__expansive_human_rights_reading, states_resisting_expansion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Organizations and legal experts who champion the broadest possible application of IHL, using this expansive reading to advocate for greater protection and accountability in all forms of armed violence. They benefit from the increased scope for their advocacy and legal interventions.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, human_rights_advocates, beneficiary,
    institutional, generational, arbitrage, global).

% Judicial bodies (e.g., ICC, ICJ) that interpret and apply IHL. This expansive reading provides a broader basis for their jurisdiction over alleged war crimes and human rights violations, increasing their influence and caseload.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, international_courts, beneficiary,
    institutional, generational, analytical, global).

% Military and police forces of states engaged in armed violence, including internal security operations. They face increased legal obligations, scrutiny, and potential prosecution under this expansive reading, which limits their operational flexibility and increases their costs of compliance.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, state_security_forces, payer,
    institutional, biographical, constrained, national).

% Organized groups engaged in armed violence against states or other groups. While theoretically bound by CA3, enforcement is challenging, and they often resist its application, viewing it as a tool of state power. They bear the cost of potential legal accountability if captured or if their actions are internationally condemned.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, non_state_armed_groups, payer,
    organized, biographical, constrained, local).

% Individuals held in detention by parties to armed violence. They are direct victims of the conflict but are also the primary intended beneficiaries of CA3's protections. The expansive reading aims to extend these protections to a wider range of detention scenarios, but they bear the immediate costs of violence and lack agency.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, detained_populations, payer,
    powerless, immediate, trapped, local).

% Non-combatant populations directly impacted by armed violence. They are victims of the conflict but are also the primary intended beneficiaries of CA3's protections. The expansive reading aims to extend these protections to a wider range of conflict scenarios, but they bear the immediate costs of violence and displacement.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, affected_civilians, payer,
    powerless, immediate, trapped, local).

% States that actively oppose or limit the expansive interpretation of CA3, arguing for a more traditional, state-centric view of IHL. They bear the costs of diplomatic pressure, legal challenges, and potential reputational damage for non-compliance with the expansive reading.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, states_resisting_expansion, payer,
    institutional, generational, constrained, national).

% Legal experts associated with the International Committee of the Red Cross (ICRC) who analyze and promote IHL. While generally supportive of humanitarian protection, their interpretation of CA3's scope is often grounded in state practice and customary law, which can be more conservative than the expansive human rights reading.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, icrc_legal_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(common_article_3_scope__expansive_human_rights_reading, international_courts).
narrative_ontology:fixing_cost_class(common_article_3_scope__expansive_human_rights_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a universal floor of minimum humanitarian standards applicable to all forms of organized armed violence, ensuring basic human dignity and protection for those not participating in hostilities, regardless of the conflict's legal classification.
% TRANSFER_FUNCTION: Transfers legal obligations and potential accountability from a narrow set of formally recognized conflicts to a broader range of situations involving organized armed violence, from states and non-state armed groups to international legal bodies and human rights advocates.
% ABSENT_VOICES: States that prioritize national sovereignty and internal security over international humanitarian law, and non-state armed groups that reject any external legal constraints, are often absent from the discourse or actively resist it. They would argue that this expansive reading is an infringement on sovereignty or an illegitimate imposition.
% DISAPPEARANCE_RATIONALE: If this expansive reading vanished, the legal landscape for internal conflicts and state security operations would revert to a more permissive, state-centric model. Protections for detainees and civilians in such contexts would diminish, and international accountability mechanisms would lose a significant basis for intervention, leading to a substantial rearrangement of legal and humanitarian norms.
% FOUNDING_PROBLEM: The original problem was the lack of minimum humanitarian standards for conflicts not meeting the threshold of international armed conflict, leaving victims of internal violence without clear legal protection.
% FOUNDING_PROBLEM_CORROBORATION: Human rights organizations, international legal bodies, and many states attest that the problem of protecting victims in diverse forms of armed violence remains live, and the expansive reading is a necessary response. However, states resisting the expansion contest the scope of the problem, arguing that existing domestic law is sufficient for many situations, making the status 'contested'.
narrative_ontology:disappearance_verdict(common_article_3_scope__expansive_human_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(common_article_3_scope__expansive_human_rights_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_article_3_scope__expansive_human_rights_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(common_article_3_scope__expansive_human_rights_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_article_3_scope__expansive_human_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(common_article_3_scope__expansive_human_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(common_article_3_scope__expansive_human_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely aims to coordinate minimum humanitarian standards (a coordination function) but does so with significant asymmetric extraction from states and non-state armed groups, requiring active enforcement against their resistance. Extractiveness is high (0.65) because it imposes substantial legal and operational costs on parties to a conflict, expanding their obligations beyond narrower interpretations. Suppression is also high (0.7) as it requires active legal and political pressure to enforce against states that prefer a more limited scope. The theater ratio (0.4) reflects that while some compliance is genuine, a significant portion is performative, aimed at avoiding international condemnation or prosecution rather than full adherence to the expansive interpretation. Resistance is high (0.75) from states and armed groups, who actively contest this broad application.
 *
 * PERSPECTIVAL GAP:
 *   Human rights advocates and international courts experience this reading as a vital Rope, coordinating universal humanitarian protection. State security forces and non-state armed groups, however, experience it as a Snare, imposing burdensome and often politically motivated obligations that restrict their operations and expose them to legal risk. Detained populations and affected civilians are direct victims of violence, but also potential beneficiaries of the protections this reading seeks to enforce, creating a complex, dual-edged experience.
 *
 * DIRECTIONALITY LOGIC:
 *   Human rights advocates and international courts are clear beneficiaries (d=0.0-0.2) as the expansive reading empowers their mandates and provides legal tools. State security forces and non-state armed groups are targets (d=0.8-1.0) due to increased obligations and legal exposure. Detained populations and affected civilians are victims of the underlying violence, but the constraint aims to protect them, making their directionality complex – they are targets of the violence but beneficiaries of the constraint's intent. States resisting the expansion are payers (d=0.7-0.9).
 *
 * MANDATROPHY ANALYSIS:
 *   The expansive human rights reading of CA3 is not experiencing mandatrophy; rather, its mandate is actively expanding and contested. The classification as Tangled Rope prevents mislabeling it as a pure Rope (ignoring the extraction from states) or a pure Snare (ignoring the genuine humanitarian coordination function). The ongoing resistance and active enforcement indicate a live, dynamic constraint, not an atrophied one. The contestation over its founding problem status ('contested') further supports its active, non-mandatrophied state.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ca3_scope_natural_law_vs_construct,
    'Is the expansive application of Common Article 3 a natural law of humanitarian necessity, or a constructed legal interpretation that benefits identifiable agents?',
    'Analysis of state practice and opinio juris over time, particularly in non-international armed conflicts and internal disturbances. If states consistently resist this expansive application, it points to a constructed interpretation.',
    'If a natural law, its extractiveness is inherent to its function; if constructed, its extractiveness is a product of advocacy and enforcement, making it a Tangled Rope or Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ca3_scope_natural_law_vs_construct, conceptual, 'Ambiguity between natural law and legal construct for CA3''s expansive scope.').

omega_variable(
    expansive_reading_legitimacy,
    'Does the expansive human rights reading of CA3 genuinely reflect universal humanitarian principles, or does it overreach the original intent and practical limits of IHL?',
    'Consensus among international legal scholars and states regarding the interpretation''s consistency with the broader framework of IHL and state sovereignty. Evidence of widespread state rejection or non-compliance would challenge its legitimacy.',
    'If overreaching, the constraint''s legitimacy as a ''Rope'' for humanitarian coordination is undermined, pushing it towards a ''Snare'' for states. If genuinely universal, its coordination function is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(expansive_reading_legitimacy, conceptual, 'Legitimacy of the expansive reading in IHL.').

omega_variable(
    kernel_reading_identification,
    'This constraint is the ''expansive_human_rights_reading'' of the ''common_article_3_scope'' kernel. What structural elements would change if a sibling reading were adopted?',
    'Comparing the application criteria: the ''state_centric_reading'' would narrow the scope to high-intensity conflicts, reducing the victim set and enforcement burden on states. The ''icrc_customary_reading'' would tie scope to evolving state practice, making it more dynamic and potentially less expansive than this reading.',
    'Adopting the ''state_centric_reading'' would reduce extractiveness and suppression on states, potentially reclassifying it as a Rope or even a Piton for low-level conflicts. Adopting the ''icrc_customary_reading'' would introduce more variability in classification over time.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Impact of alternative readings of the Common Article 3 scope kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_article_3_scope__expansive_human_rights_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement_basis(comm_tr_t0, observed).
narrative_ontology:measurement(comm_tr_t10, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement_basis(comm_tr_t10, observed).
narrative_ontology:measurement(comm_tr_t20, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(comm_tr_t20, observed).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement_basis(comm_be_t0, observed).
narrative_ontology:measurement(comm_be_t10, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement_basis(comm_be_t10, observed).
narrative_ontology:measurement(comm_be_t20, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement_basis(comm_be_t20, observed).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t0, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(comm_su_t0, observed).
narrative_ontology:measurement(comm_su_t10, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement_basis(comm_su_t10, observed).
narrative_ontology:measurement(comm_su_t20, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(comm_su_t20, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_article_3_scope__expansive_human_rights_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(common_article_3_scope__expansive_human_rights_reading, ihl_detention_standards).
narrative_ontology:affects_constraint(common_article_3_scope__expansive_human_rights_reading, ihl_targeting_rules).
narrative_ontology:affects_constraint(common_article_3_scope__expansive_human_rights_reading, international_criminal_court_jurisdiction).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'common_article_3_scope' kernel. The other readings are 'state_centric_reading' and 'icrc_customary_reading', each representing a distinct interpretation of CA3's applicability.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
