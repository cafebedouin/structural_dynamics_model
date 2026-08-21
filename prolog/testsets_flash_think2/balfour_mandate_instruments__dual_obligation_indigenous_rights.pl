% ============================================================================
% CONSTRAINT STORY: balfour_mandate_instruments__dual_obligation_indigenous_rights
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_balfour_mandate_instruments__dual_obligation_indigenous_rights, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: balfour_mandate_instruments__dual_obligation_indigenous_rights
 *   human_readable: Balfour Mandate: Dual Obligation for Indigenous Rights
 *   domain: international_law/colonial_administration/state_formation
 *
 * SUMMARY:
 *   This constraint represents a specific reading of the Balfour Mandate
 *   instruments, emphasizing the 'dual obligation' to protect existing Arab
 *   civil/political rights and land tenure, subordinating the 'national home'
 *   concept to self-determination norms and minority protection principles.
 *   This reading posits that the mandate was intended as a trusteeship for
 *   the indigenous population, with the Jewish national home being a
 *   secondary, non-prejudicial objective. The constraint operates as a
 *   tangled rope due to its genuine coordination function (managing a complex
 *   political situation) intertwined with asymmetric extraction (the
 *   administrative burden on Britain, and the limitations placed on Zionist
 *   aspirations).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.75).
domain_priors:suppression_score(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.8).
domain_priors:theater_ratio(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, extractiveness, 0.75).
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(balfour_mandate_instruments__dual_obligation_indigenous_rights, tangled_rope).
narrative_ontology:human_readable(balfour_mandate_instruments__dual_obligation_indigenous_rights, "Balfour Mandate: Dual Obligation for Indigenous Rights").
narrative_ontology:topic_domain(balfour_mandate_instruments__dual_obligation_indigenous_rights, "international_law/colonial_administration/state_formation").

domain_priors:requires_active_enforcement(balfour_mandate_instruments__dual_obligation_indigenous_rights).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(balfour_mandate_instruments__dual_obligation_indigenous_rights, 'a5e4ed78-bf19-4054-8e71-27706e61ac04').
narrative_ontology:cs_kernel_codification('a5e4ed78-bf19-4054-8e71-27706e61ac04', formalized).
narrative_ontology:cs_authority_grounding('a5e4ed78-bf19-4054-8e71-27706e61ac04', lineage).
narrative_ontology:cs_interpretation_layer_present('a5e4ed78-bf19-4054-8e71-27706e61ac04').
narrative_ontology:cs_reading_relation('a5e4ed78-bf19-4054-8e71-27706e61ac04', balfour_mandate_instruments__jewish_national_home_primacy, forecloses).
narrative_ontology:cs_reading_relation('a5e4ed78-bf19-4054-8e71-27706e61ac04', balfour_mandate_instruments__mandatory_interpretive_discretion, coexists_with).
narrative_ontology:cs_axiom('a5e4ed78-bf19-4054-8e71-27706e61ac04', foundational, indigenous_rights_supremacy).
narrative_ontology:cs_axiom_status(indigenous_rights_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('a5e4ed78-bf19-4054-8e71-27706e61ac04', indigenous_rights_supremacy, deontological).
narrative_ontology:cs_axiom('a5e4ed78-bf19-4054-8e71-27706e61ac04', foundational, mandate_as_trusteeship).
narrative_ontology:cs_axiom_status(mandate_as_trusteeship, holdable).
narrative_ontology:cs_axiom_grounding('a5e4ed78-bf19-4054-8e71-27706e61ac04', mandate_as_trusteeship, conventional).
narrative_ontology:cs_reference_frame('a5e4ed78-bf19-4054-8e71-27706e61ac04', league_of_nations_mandate_principles).
narrative_ontology:cs_drift_state('a5e4ed78-bf19-4054-8e71-27706e61ac04', post_balfour_declaration_implementation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a5e4ed78-bf19-4054-8e71-27706e61ac04', '').
narrative_ontology:cs_kernel_id(balfour_mandate_instruments__dual_obligation_indigenous_rights, balfour_mandate_instruments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_elites).
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_communities).
narrative_ontology:constraint_victim(balfour_mandate_instruments__dual_obligation_indigenous_rights, zionist_organizations).
narrative_ontology:constraint_victim(balfour_mandate_instruments__dual_obligation_indigenous_rights, british_administrators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the mandate's stated protection of existing civil/political rights and land tenure, which provides a legal basis to resist displacement and demand self-governance. However, their political aspirations are constrained by the overall mandate framework and British authority.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_elites, beneficiary,
    powerful, biographical, constrained, regional).

% Their land and civil rights are theoretically protected by the mandate's dual obligation, offering a bulwark against unconstrained land acquisition and demographic change. Yet, they remain under colonial administration and face ongoing pressure.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_communities, beneficiary,
    moderate, biographical, constrained, local).

% Bear the costs of restrictions on land transfers and immigration quotas, which impede their project of establishing a Jewish majority and sovereign state. They actively lobby against these constraints, viewing them as obstacles to the 'national home'.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, zionist_organizations, payer,
    powerful, generational, constrained, global).

% Are tasked with implementing the mandate's dual and often conflicting obligations. They bear the administrative and political burden of balancing Arab and Zionist demands, facing constant pressure and criticism from both sides, making their position highly extractive of their political capital and resources.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, british_administrators, agenda_setter,
    institutional, immediate, constrained, national).
narrative_ontology:stakeholder_secondary_role(balfour_mandate_instruments__dual_obligation_indigenous_rights, british_administrators, payer).

% The ultimate authority granting the mandate, it theoretically oversees its implementation to ensure adherence to international principles. However, its enforcement power is limited, and it primarily serves as a forum for review and debate rather than direct intervention.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, league_of_nations, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(balfour_mandate_instruments__dual_obligation_indigenous_rights, diffuse).
narrative_ontology:fixing_cost_class(balfour_mandate_instruments__dual_obligation_indigenous_rights, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a framework for the administration of Palestine that simultaneously facilitates the establishment of a Jewish national home and protects the civil and religious rights of existing non-Jewish communities, guiding the territory towards eventual self-determination.
% TRANSFER_FUNCTION: Transfers administrative burden and political friction to British administrators, while attempting to preserve land and civil rights for Palestinian Arabs, and limiting land/demographic expansion for Zionist organizations. It also transfers political legitimacy from the League of Nations to the British Mandate.
% ABSENT_VOICES: Palestinian Arab political leadership was often excluded from direct negotiations with British and Zionist representatives, limiting their ability to shape the mandate's implementation. International human rights advocates, as a formalized movement, were nascent at the time but would have strongly emphasized the self-determination aspect.
% DISAPPEARANCE_RATIONALE: If this specific interpretation of the mandate (emphasizing dual obligation) had vanished, the legal and political landscape of Palestine would have been fundamentally different. Without the explicit obligation to protect indigenous rights, unconstrained land acquisition and demographic change would have accelerated, leading to a different trajectory of conflict and state formation.
% FOUNDING_PROBLEM: To reconcile the Balfour Declaration's promise of a Jewish national home with the League of Nations' mandate system principle of protecting existing indigenous populations and guiding them towards self-determination, in a territory with competing national aspirations.
% FOUNDING_PROBLEM_CORROBORATION: Palestinian Arab historians and international legal scholars corroborate that the problem of reconciling these obligations was central and largely unresolved throughout the mandate period. Zionist narratives often downplay the indigenous rights aspect, while British official histories acknowledge the difficulty but emphasize their mediating role. Legislative-hearing testimony and independent historical analyses from outside the benefiting parties support the contested status.
narrative_ontology:disappearance_verdict(balfour_mandate_instruments__dual_obligation_indigenous_rights, world_rearranges).
narrative_ontology:founding_problem_status(balfour_mandate_instruments__dual_obligation_indigenous_rights, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(balfour_mandate_instruments__dual_obligation_indigenous_rights, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(balfour_mandate_instruments__dual_obligation_indigenous_rights, 'none', 1).
narrative_ontology:epsilon_provenance(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.75, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(balfour_mandate_instruments__dual_obligation_indigenous_rights_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(balfour_mandate_instruments__dual_obligation_indigenous_rights, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(balfour_mandate_instruments__dual_obligation_indigenous_rights_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.75) because maintaining this dual obligation imposed significant costs and limitations on Zionist organizations' goals and created immense administrative friction for British administrators. Suppression is also high (0.80) as active enforcement (e.g., land transfer ordinances, immigration quotas) was required to uphold the indigenous rights aspect against strong political pressure and competing interpretations. The theater ratio is moderate (0.40) reflecting genuine administrative efforts to balance, but also the performative aspect of maintaining a 'neutral' stance while often failing to fully protect Arab rights in practice. Resistance is high (0.70) due to constant lobbying and occasional uprisings from both Arab and Zionist factions against the perceived imbalances of the mandate.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Palestinian Arabs, this reading of the mandate, while imperfectly realized, offered a crucial (though often insufficient) legal shield. From the Zionist perspective, it was an impediment to their national aspirations. British administrators experienced it as an intractable problem, a constant source of friction and political cost. The engine's per-seat classification would reflect these divergent experiences, with beneficiaries seeing a protective (if weak) rope, and payers experiencing a highly extractive snare.
 *
 * DIRECTIONALITY LOGIC:
 *   Palestinian Arab elites and communities are beneficiaries as this reading provides a legal and political framework for the protection of their rights and land, even if imperfectly implemented. Zionist organizations are targets (payers) as this reading imposes significant constraints on their core objectives of land acquisition and demographic growth. British administrators are also targets (payers) due to the immense political and administrative costs of attempting to enforce this inherently contradictory dual obligation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is this constraint a true reflection of the mandate''s intent, or one of several competing interpretations that shaped its implementation?',
    'Analysis of primary source documents from the League of Nations, British Foreign Office, and contemporary international legal scholarship to assess the prevalence and legal weight of this ''dual obligation'' interpretation versus others.',
    'If this reading is found to be a minority or later interpretation, its effective extractiveness and suppression might be re-evaluated as lower, reflecting its weaker de facto influence. If it is confirmed as a primary, intended interpretation, the high extraction and suppression reflect the true cost of its (failed) enforcement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'This constraint is one reading of the ''balfour_mandate_instruments'' kernel. Sibling readings (e.g., ''jewish_national_home_primacy'') would shift beneficiaries/victims and extraction significantly.').

omega_variable(
    land_tenure_protection_effectiveness,
    'To what extent did the mandate''s land transfer restrictions actually protect Palestinian Arab land tenure, given ongoing land sales and British administrative practices?',
    'Empirical study of land registry records, land transfer statistics, and British administrative reports throughout the mandate period, comparing stated policy with actual outcomes.',
    'If protection was largely ineffective, the ''beneficiary'' status of Palestinian Arabs would be weakened, and the constraint''s overall extractiveness (from them) would be higher, reclassifying it closer to a snare for that seat. If effective, it reinforces the tangled rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(land_tenure_protection_effectiveness, empirical, 'The actual impact of land tenure protection measures on Palestinian Arab communities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(balfour_mandate_instruments__dual_obligation_indigenous_rights, 1922, 1948).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(balf_tr_t0, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 0, 0.3).
narrative_ontology:measurement(balf_tr_t5, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 5, 0.33).
narrative_ontology:measurement(balf_tr_t10, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 10, 0.36).
narrative_ontology:measurement(balf_tr_t15, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 15, 0.38).
narrative_ontology:measurement(balf_tr_t20, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 20, 0.4).
narrative_ontology:measurement(balf_tr_t26, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 26, 0.4).

% Extraction over time
narrative_ontology:measurement(balf_be_t0, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(balf_be_t5, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 5, 0.68).
narrative_ontology:measurement(balf_be_t10, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 10, 0.71).
narrative_ontology:measurement(balf_be_t15, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 15, 0.73).
narrative_ontology:measurement(balf_be_t20, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 20, 0.75).
narrative_ontology:measurement(balf_be_t26, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 26, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(balf_su_t0, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(balf_su_t5, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 5, 0.74).
narrative_ontology:measurement(balf_su_t10, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 10, 0.77).
narrative_ontology:measurement(balf_su_t15, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 15, 0.79).
narrative_ontology:measurement(balf_su_t20, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 20, 0.8).
narrative_ontology:measurement(balf_su_t26, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 26, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(balfour_mandate_instruments__dual_obligation_indigenous_rights, enforcement_mechanism).
narrative_ontology:affects_constraint(balfour_mandate_instruments__dual_obligation_indigenous_rights, balfour_mandate_instruments__jewish_national_home_primacy).
narrative_ontology:affects_constraint(balfour_mandate_instruments__dual_obligation_indigenous_rights, balfour_mandate_instruments__mandatory_interpretive_discretion).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'balfour_mandate_instruments' kernel. Each reading represents a different structural interpretation of the mandate's core obligations and has a unique beneficiary/victim structure and metric profile.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
