% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy__partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy__partition_reading, []).

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
 *   constraint_id: territorial_legitimacy__partition_reading
 *   human_readable: Territorial Legitimacy via International Legal Partition (Partition Reading)
 *   domain: political_theory/international_law/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint represents the 'partition reading' of territorial
 *   legitimacy in the Israeli-Palestinian conflict. It asserts that
 *   legitimacy derives from international legal partition (UN Resolution 181)
 *   and subsequent state recognition, implying the legitimacy of both Israeli
 *   and Palestinian states within recognized borders (e.g., 1967 lines) and
 *   the illegitimacy of settlements beyond these lines. The two-state
 *   solution is seen as structurally possible and legally mandated. The
 *   constraint is classified as a Tangled Rope because it provides a
 *   coordination function (framework for statehood) but also involves
 *   asymmetric extraction (displacement of refugees, territorial limitations)
 *   and requires active enforcement by the international community.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy__partition_reading, 0.65).
domain_priors:suppression_score(territorial_legitimacy__partition_reading, 0.7).
domain_priors:theater_ratio(territorial_legitimacy__partition_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy__partition_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy__partition_reading, "Territorial Legitimacy via International Legal Partition (Partition Reading)").
narrative_ontology:topic_domain(territorial_legitimacy__partition_reading, "political_theory/international_law/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy__partition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy__partition_reading, '7e074bae-6b13-4a9d-9d38-b06471682192').
narrative_ontology:cs_kernel_codification('7e074bae-6b13-4a9d-9d38-b06471682192', formalized).
narrative_ontology:cs_authority_grounding('7e074bae-6b13-4a9d-9d38-b06471682192', lineage).
narrative_ontology:cs_interpretation_layer_present('7e074bae-6b13-4a9d-9d38-b06471682192').
narrative_ontology:cs_reading_relation('7e074bae-6b13-4a9d-9d38-b06471682192', territorial_legitimacy__security_necessity_reading, coexists_with).
narrative_ontology:cs_reading_relation('7e074bae-6b13-4a9d-9d38-b06471682192', territorial_legitimacy__indigenous_continuity_reading, coexists_with).
narrative_ontology:cs_axiom('7e074bae-6b13-4a9d-9d38-b06471682192', foundational, state_sovereignty_via_international_recognition).
narrative_ontology:cs_axiom_status(state_sovereignty_via_international_recognition, holdable).
narrative_ontology:cs_axiom_grounding('7e074bae-6b13-4a9d-9d38-b06471682192', state_sovereignty_via_international_recognition, conventional).
narrative_ontology:cs_axiom('7e074bae-6b13-4a9d-9d38-b06471682192', foundational, territorial_integrity_based_on_1967_borders).
narrative_ontology:cs_axiom_status(territorial_integrity_based_on_1967_borders, holdable).
narrative_ontology:cs_axiom_grounding('7e074bae-6b13-4a9d-9d38-b06471682192', territorial_integrity_based_on_1967_borders, conventional).
narrative_ontology:cs_reference_frame('7e074bae-6b13-4a9d-9d38-b06471682192', un_resolution_181_framework).
narrative_ontology:cs_drift_state('7e074bae-6b13-4a9d-9d38-b06471682192', contemporary_geopolitical_reality, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('7e074bae-6b13-4a9d-9d38-b06471682192', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(territorial_legitimacy__partition_reading, territorial_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy__partition_reading, state_of_israel).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__partition_reading, state_of_palestine).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__partition_reading, international_legal_order).
narrative_ontology:constraint_victim(territorial_legitimacy__partition_reading, palestinian_refugees).
narrative_ontology:constraint_victim(territorial_legitimacy__partition_reading, israeli_settlers_beyond_67_lines).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from international recognition of its statehood and borders, particularly the 1948 partition lines. Faces pressure regarding settlements beyond the 1967 lines, which this reading deems illegitimate. Its legitimacy is tied to adherence to international law.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, state_of_israel, beneficiary,
    institutional, generational, constrained, national).

% Benefits from the principle of self-determination and the recognition of its right to statehood within the 1967 borders, as derived from the partition plan. Faces challenges in achieving full sovereignty and territorial contiguity due to ongoing occupation and settlement activity.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, state_of_palestine, beneficiary,
    institutional, generational, constrained, national).

% Acts as the arbiter and enforcer of international law, including UN resolutions and the principle of self-determination. Its legitimacy is reinforced by the adherence of states to these principles, even as it struggles to enforce them consistently.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, international_legal_order, agenda_setter,
    institutional, civilizational, analytical, global).

% Bear the cost of displacement and the denial of their right of return, as the partition reading prioritizes state recognition over individual claims to ancestral lands. Their situation is a direct consequence of the territorial division and subsequent conflicts.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, palestinian_refugees, payer,
    powerless, generational, trapped, regional).

% Their presence in territories beyond the 1967 borders is deemed illegitimate by this reading, creating legal and political precarity. They face potential displacement or loss of property if a two-state solution based on these borders is implemented.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, israeli_settlers_beyond_67_lines, payer,
    moderate, biographical, constrained, local).

% Monitor human rights and humanitarian law compliance in the region, often highlighting the impact of occupation and settlement expansion on Palestinian populations. Their reports influence international opinion and policy debates.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, international_humanitarian_organizations, observer,
    organized, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_legitimacy__partition_reading, international_legal_order).
narrative_ontology:fixing_cost_class(territorial_legitimacy__partition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for the peaceful coexistence of two states within recognized borders, providing a legal basis for international relations and conflict resolution.
% TRANSFER_FUNCTION: Transfers territorial sovereignty and the right to self-determination to both Israeli and Palestinian entities, while implicitly transferring the burden of displacement and territorial loss to specific populations.
% ABSENT_VOICES: Those who reject the very premise of partition, whether on indigenous rights grounds or maximalist territorial claims, are marginalized by this framework. Their perspectives are excluded from the dominant international legal discourse.
% DISAPPEARANCE_RATIONALE: If the international legal framework for partition and state recognition vanished, the entire basis for the two-state solution would collapse. Territorial claims would revert to pure power dynamics, leading to intensified conflict and a complete reordering of regional and international alliances.
% FOUNDING_PROBLEM: The problem of competing national aspirations and territorial claims in Mandate Palestine, requiring a legal framework for division and state formation to prevent perpetual conflict.
% FOUNDING_PROBLEM_CORROBORATION: The UN and most international bodies continue to affirm the necessity of a two-state solution based on the partition principle. While contested by some parties on the ground, the international consensus outside the directly benefiting parties (e.g., non-aligned states, legal scholars) largely corroborates the ongoing relevance of this founding problem.
narrative_ontology:disappearance_verdict(territorial_legitimacy__partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy__partition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy__partition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(territorial_legitimacy__partition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy__partition_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy__partition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy__partition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy__partition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.65) due to the significant costs borne by displaced populations and those whose territorial claims are curtailed by the partition framework. Suppression (0.70) is also high, reflecting the active enforcement by international bodies and states to maintain the legal framework, often against resistance from those whose claims are suppressed. The theater ratio (0.40) indicates that while the legal framework has genuine coordinating functions, a substantial portion of its maintenance involves performative diplomacy and resolutions that are not fully implemented on the ground. The temporal measurements reflect an increase in extractiveness and suppression after 1967 due to occupation and settlement expansion, with some stabilization after the Oslo Accords (1993) but persistent high levels.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the international legal order, this constraint is a necessary framework for peace and stability, a coordination mechanism. From the perspective of Palestinian refugees, it is a source of ongoing displacement and denial of rights, an extractive mechanism. The engine's per-seat classification will reflect these divergent experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   The State of Israel and the State of Palestine are beneficiaries, as their statehood and territorial claims are legitimized by this reading. The international legal order acts as an agenda-setter, defining and enforcing the framework. Palestinian refugees and Israeli settlers beyond the 1967 lines are victims, bearing the costs of displacement or territorial curtailment. Their exit options are severely constrained by the geopolitical realities and legal framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_efficacy_ambiguity,
    'To what extent is the international legal order capable of effectively enforcing the partition framework and its associated borders?',
    'Analysis of compliance rates with UN resolutions, effectiveness of sanctions regimes, and success of international mediation efforts over time.',
    'If enforcement is consistently weak, the constraint''s ''requires_active_enforcement'' becomes more theatrical, pushing it towards a Piton or a Snare where the coordination function is merely cover for power dynamics. If enforcement strengthens, it reinforces the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_efficacy_ambiguity, empirical, 'Uncertainty regarding the actual capacity of the international legal order to enforce its own resolutions and principles.').

omega_variable(
    two_state_solution_viability,
    'Is a two-state solution, as envisioned by the partition reading, still practically viable given current demographic and territorial realities?',
    'Empirical assessment of settlement expansion, demographic trends, and the contiguity of proposed Palestinian territories. Expert consensus on the feasibility of a contiguous, sovereign Palestinian state.',
    'If deemed no longer viable, the partition reading''s core coordination function (peaceful coexistence of two states) collapses, pushing the constraint towards a Snare (pure extraction by one party) or a Piton (an obsolete framework maintained theatrically).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(two_state_solution_viability, empirical, 'Uncertainty about the practical feasibility of the two-state solution, which is central to this reading''s legitimacy.').

omega_variable(
    partition_vs_indigenous_rights,
    'Does the partition framework adequately address or inherently conflict with the claims of indigenous continuity and self-determination for all populations?',
    'Conceptual analysis of international law principles, historical review of land claims, and comparative studies of post-colonial state formation. Dialogue with representatives of indigenous populations.',
    'If the conflict is inherent and unresolvable within the partition framework, this reading''s legitimacy is fundamentally challenged by the indigenous_continuity_reading, potentially leading to a reclassification as a Snare from the perspective of those whose indigenous rights are suppressed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(partition_vs_indigenous_rights, conceptual, 'Conceptual tension between the legal framework of partition and the claims of indigenous rights.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy__partition_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1948, territorial_legitimacy__partition_reading, theater_ratio, 1948, 0.2).
narrative_ontology:measurement(terr_tr_t1967, territorial_legitimacy__partition_reading, theater_ratio, 1967, 0.3).
narrative_ontology:measurement(terr_tr_t1993, territorial_legitimacy__partition_reading, theater_ratio, 1993, 0.45).
narrative_ontology:measurement(terr_tr_t2024, territorial_legitimacy__partition_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(terr_be_t1948, territorial_legitimacy__partition_reading, base_extractiveness, 1948, 0.5).
narrative_ontology:measurement(terr_be_t1967, territorial_legitimacy__partition_reading, base_extractiveness, 1967, 0.6).
narrative_ontology:measurement(terr_be_t1993, territorial_legitimacy__partition_reading, base_extractiveness, 1993, 0.68).
narrative_ontology:measurement(terr_be_t2024, territorial_legitimacy__partition_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1948, territorial_legitimacy__partition_reading, suppression_requirement, 1948, 0.6).
narrative_ontology:measurement(terr_su_t1967, territorial_legitimacy__partition_reading, suppression_requirement, 1967, 0.75).
narrative_ontology:measurement(terr_su_t1993, territorial_legitimacy__partition_reading, suppression_requirement, 1993, 0.7).
narrative_ontology:measurement(terr_su_t2024, territorial_legitimacy__partition_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy__partition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(territorial_legitimacy__partition_reading, security_necessity_reading).
narrative_ontology:affects_constraint(territorial_legitimacy__partition_reading, indigenous_continuity_reading).
narrative_ontology:affects_constraint(territorial_legitimacy__partition_reading, right_of_return_claims).
narrative_ontology:affects_constraint(territorial_legitimacy__partition_reading, settlement_legitimacy_dispute).

% DUAL FORMULATION NOTE:
% This constraint is one reading ('partition_reading') of the 'territorial_legitimacy' kernel. It defines legitimacy through international legal partition and state recognition, contrasting with the 'security_necessity_reading' and 'indigenous_continuity_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
