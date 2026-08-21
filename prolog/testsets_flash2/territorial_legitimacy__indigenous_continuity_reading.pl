% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy__indigenous_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy__indigenous_continuity_reading, []).

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
 *   constraint_id: territorial_legitimacy__indigenous_continuity_reading
 *   human_readable: Territorial Legitimacy via Indigenous Continuity (Nakba Reading)
 *   domain: political_theory/international_law/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint story represents the 'indigenous continuity' reading of
 *   territorial legitimacy in historic Palestine, where 1948 is understood as
 *   the Nakba (catastrophe) rather than a legitimate partition. From this
 *   perspective, the legitimacy of the Israeli state is fundamentally
 *   challenged as a settler-colonial entity, and Palestinian sovereignty over
 *   all of historic Palestine is asserted. The right of return for 1948
 *   refugees is structurally central to this reading. The constraint is
 *   classified as a Snare due to its high extractiveness and suppression of
 *   the indigenous population.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy__indigenous_continuity_reading, 0.95).
domain_priors:suppression_score(territorial_legitimacy__indigenous_continuity_reading, 0.98).
domain_priors:theater_ratio(territorial_legitimacy__indigenous_continuity_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, extractiveness, 0.95).
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 0.98).
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, resistance, 0.99).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy__indigenous_continuity_reading, snare).
narrative_ontology:human_readable(territorial_legitimacy__indigenous_continuity_reading, "Territorial Legitimacy via Indigenous Continuity (Nakba Reading)").
narrative_ontology:topic_domain(territorial_legitimacy__indigenous_continuity_reading, "political_theory/international_law/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy__indigenous_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy__indigenous_continuity_reading, 'a2ddb155-59b0-4619-8fa4-f220bdba696b').
narrative_ontology:cs_kernel_codification('a2ddb155-59b0-4619-8fa4-f220bdba696b', distributed).
narrative_ontology:cs_authority_grounding('a2ddb155-59b0-4619-8fa4-f220bdba696b', extraction).
narrative_ontology:cs_interpretation_layer_present('a2ddb155-59b0-4619-8fa4-f220bdba696b').
narrative_ontology:cs_reading_relation('a2ddb155-59b0-4619-8fa4-f220bdba696b', territorial_legitimacy__partition_reading, forecloses).
narrative_ontology:cs_reading_relation('a2ddb155-59b0-4619-8fa4-f220bdba696b', territorial_legitimacy__security_necessity_reading, forecloses).
narrative_ontology:cs_axiom('a2ddb155-59b0-4619-8fa4-f220bdba696b', foundational, indigenous_sovereignty_is_primary).
narrative_ontology:cs_axiom_status(indigenous_sovereignty_is_primary, holdable).
narrative_ontology:cs_axiom_grounding('a2ddb155-59b0-4619-8fa4-f220bdba696b', indigenous_sovereignty_is_primary, deontological).
narrative_ontology:cs_axiom('a2ddb155-59b0-4619-8fa4-f220bdba696b', foundational, settler_colonialism_is_illegitimate).
narrative_ontology:cs_axiom_status(settler_colonialism_is_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('a2ddb155-59b0-4619-8fa4-f220bdba696b', settler_colonialism_is_illegitimate, deontological).
narrative_ontology:cs_reference_frame('a2ddb155-59b0-4619-8fa4-f220bdba696b', pre_1948_indigenous_sovereignty).
narrative_ontology:cs_drift_state('a2ddb155-59b0-4619-8fa4-f220bdba696b', contemporary, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('a2ddb155-59b0-4619-8fa4-f220bdba696b', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy__indigenous_continuity_reading, territorial_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_victim(territorial_legitimacy__indigenous_continuity_reading, palestinian_people).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__indigenous_continuity_reading, zionist_movement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the indigenous population, they bear the full cost of dispossession, displacement, and denial of self-determination. Their existence is continuously suppressed, and their right of return is denied. Exit means abandoning their identity and ancestral lands.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, palestinian_people, payer,
    powerless, generational, trapped, regional).

% The Israeli state, from this reading, is the settler-colonial entity whose existence is predicated on the dispossession of the Palestinian people. It actively enforces the constraint through military occupation, legal frameworks, and demographic policies. Its legitimacy is derived from the denial of indigenous rights.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, israeli_state, agenda_setter,
    institutional, generational, constrained, regional).

% Advocates within the international community who support Palestinian self-determination and the right of return. They observe the ongoing dispossession and advocate for international legal remedies, but their power to enforce change is limited.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, international_community_proponents, observer,
    organized, generational, analytical, global).

% Benefits from the establishment and maintenance of a Jewish state in historic Palestine, which this reading views as inherently colonial and extractive. Its identity is fused with the project of settlement and state-building, making exit from this framework unthinkable.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, zionist_movement, beneficiary,
    institutional, generational, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From this reading's perspective, the constraint does not solve a genuine coordination problem but rather imposes a colonial order. Any 'coordination' serves the settler-colonial project.
% TRANSFER_FUNCTION: Transfers land, resources, and sovereignty from the indigenous Palestinian people to the settler-colonial Israeli state and the Zionist movement.
% ABSENT_VOICES: The voices of displaced Palestinians, those living under occupation, and those denied the right of return are systematically marginalized or suppressed within dominant international discourse. Their narratives are often reframed or dismissed.
% DISAPPEARANCE_RATIONALE: If the constraint of denying indigenous continuity and self-determination vanished, the entire political and territorial arrangement of historic Palestine would fundamentally rearrange. It would necessitate the dismantling of the settler-colonial structure, the return of refugees, and the establishment of Palestinian sovereignty.
% FOUNDING_PROBLEM: The problem this constraint was built to 'solve' (from the perspective of its beneficiaries) was the establishment of a Jewish state in Palestine, which necessitated the displacement and dispossession of the indigenous population.
% FOUNDING_PROBLEM_CORROBORATION: The Palestinian people and their advocates attest that the 'problem' of indigenous self-determination remains live and unresolved. The Israeli state and its allies attest that the problem of Jewish self-determination and security remains live. Corroboration for the Nakba narrative comes from extensive historical records, UN resolutions on refugee rights, and testimonies from Palestinian survivors and historians.
narrative_ontology:disappearance_verdict(territorial_legitimacy__indigenous_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy__indigenous_continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy__indigenous_continuity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(territorial_legitimacy__indigenous_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy__indigenous_continuity_reading, 0.95, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy__indigenous_continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy__indigenous_continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy__indigenous_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is extremely high (0.95) because the constraint fundamentally denies the self-determination and territorial rights of the indigenous Palestinian people, leading to ongoing dispossession and displacement. Suppression is also extremely high (0.98) as the persistence of the Israeli state, from this reading, relies on continuous military, legal, and demographic control to prevent Palestinian return and sovereignty. Theater ratio is low (0.1) because the mechanisms of control are direct and functional, with little performative cover for their primary purpose of maintaining the existing power structure. Resistance is very high (0.99) reflecting the continuous and intense Palestinian struggle against this constraint.
 *
 * PERSPECTIVAL GAP:
 *   The Israeli state and Zionist movement perceive their actions as legitimate self-determination and defense, while the Palestinian people and their advocates perceive the same actions as colonial extraction and suppression. This fundamental divergence in framing is central to the conflict, with each side claiming a different 'founding problem' and 'legitimacy' for their actions. The engine's classification of Snare from this reading's perspective highlights the structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   The Palestinian people are the primary victims and targets (d=1.0), bearing the full costs of dispossession. The Israeli state and the Zionist movement are the beneficiaries (d=0.0-0.1), as their existence and project are enabled by this constraint. International community proponents are observers (d=0.5), analyzing and advocating without direct benefit or cost from the constraint's operation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_narrative_ambiguity,
    'Is the 1948 event primarily a ''Nakba'' (catastrophe of displacement) or a ''War of Independence'' (founding of a state)?',
    'Comprehensive, internationally mediated historical truth and reconciliation process, acknowledging multiple narratives without denying historical facts of displacement.',
    'Resolution would fundamentally alter the perceived legitimacy of the Israeli state and the Palestinian claim to return, shifting the entire basis of territorial claims.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(historical_narrative_ambiguity, conceptual, 'Ambiguity in the foundational historical narrative of 1948.').

omega_variable(
    indigenous_status_definition,
    'How is ''indigenous'' status defined and applied in this context, particularly concerning historical migrations and claims?',
    'Application of UN Declaration on the Rights of Indigenous Peoples (UNDRIP) criteria, adapted to the specific historical and demographic context, with expert legal and anthropological review.',
    'A clear, internationally recognized definition would strengthen or weaken the foundational claim of indigenous continuity, impacting the legal basis for Palestinian self-determination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigenous_status_definition, conceptual, 'Definition and application of ''indigenous'' status.').

omega_variable(
    right_of_return_feasibility,
    'What are the practical and demographic implications of implementing the right of return for 1948 refugees, and how would it be managed?',
    'Detailed demographic studies, infrastructure planning, and international negotiation on modalities of return, compensation, and integration.',
    'The perceived feasibility and practical implementation plan for the right of return significantly impacts its status as a live claim versus a symbolic demand, influencing the perceived ''fixability'' of the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(right_of_return_feasibility, empirical, 'Practical feasibility of the right of return.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy__indigenous_continuity_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1948, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 1948, 0.1).
narrative_ontology:measurement(terr_tr_t1967, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 1967, 0.1).
narrative_ontology:measurement(terr_tr_t1993, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 1993, 0.15).
narrative_ontology:measurement(terr_tr_t2000, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 2000, 0.12).
narrative_ontology:measurement(terr_tr_t2024, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(terr_be_t1948, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 1948, 0.9).
narrative_ontology:measurement(terr_be_t1967, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 1967, 0.95).
narrative_ontology:measurement(terr_be_t1993, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 1993, 0.93).
narrative_ontology:measurement(terr_be_t2000, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 2000, 0.96).
narrative_ontology:measurement(terr_be_t2024, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 2024, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1948, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 1948, 0.9).
narrative_ontology:measurement(terr_su_t1967, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 1967, 0.95).
narrative_ontology:measurement(terr_su_t1993, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 1993, 0.93).
narrative_ontology:measurement(terr_su_t2000, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 2000, 0.96).
narrative_ontology:measurement(terr_su_t2024, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 2024, 0.98).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy__indigenous_continuity_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'territorial_legitimacy' kernel. It focuses on indigenous continuity and anti-colonial self-determination, contrasting with the 'partition_reading' and 'security_necessity_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
