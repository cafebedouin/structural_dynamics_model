% ============================================================================
% CONSTRAINT STORY: waitangi_sovereignty_allocation__rangatiratanga_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_waitangi_sovereignty_allocation__rangatiratanga_reading, []).

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
 *   constraint_id: waitangi_sovereignty_allocation__rangatiratanga_reading
 *   human_readable: Waitangi Treaty: Retained Tino Rangatiratanga (Māori Reading)
 *   domain: constitutional_law/indigenous_rights/post_colonial_governance
 *
 * SUMMARY:
 *   This constraint is the `rangatiratanga_reading` of the
 *   `waitangi_sovereignty_allocation` kernel, which posits that Māori
 *   retained full authority (`tino rangatiratanga`) over their lands,
 *   resources, and treasures, with the Crown gaining only governorship
 *   (`kāwanatanga`) over settlers. This reading stands in contrast to the
 *   `crown_sovereignty_reading` (which asserts full Crown cession) and the
 *   `partnership_reading` (which posits a co-governance model). The
 *   constraint itself, as a claim of retained authority, is inherently
 *   non-extractive (low epsilon). However, its recognition is actively
 *   suppressed by the dominant legal and political framework, leading to high
 *   suppression and resistance.
 *
 * KEY AGENTS:
 *   - maori_iwi_hapu: Beneficiary/Agenda Setter (organized/identity_locked)
 *   - maori_communities: Beneficiary/Payer (powerless/identity_locked)
 *   - new_zealand_crown: Payer/Agenda Setter (institutional/constrained)
 *   - new_zealand_judiciary: Agenda Setter (institutional/constrained)
 *   - settler_population: Payer (moderate/mobile)
 *   - international_indigenous_rights_advocates: Observer (organized/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(waitangi_sovereignty_allocation__rangatiratanga_reading, 0.15).
domain_priors:suppression_score(waitangi_sovereignty_allocation__rangatiratanga_reading, 0.8).
domain_priors:theater_ratio(waitangi_sovereignty_allocation__rangatiratanga_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(waitangi_sovereignty_allocation__rangatiratanga_reading, rope).
narrative_ontology:human_readable(waitangi_sovereignty_allocation__rangatiratanga_reading, "Waitangi Treaty: Retained Tino Rangatiratanga (Māori Reading)").
narrative_ontology:topic_domain(waitangi_sovereignty_allocation__rangatiratanga_reading, "constitutional_law/indigenous_rights/post_colonial_governance").

domain_priors:requires_active_enforcement(waitangi_sovereignty_allocation__rangatiratanga_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(waitangi_sovereignty_allocation__rangatiratanga_reading, '713ed32e-6458-4bd0-b56f-db5b26e2503f').
narrative_ontology:cs_kernel_codification('713ed32e-6458-4bd0-b56f-db5b26e2503f', fixed_text).
narrative_ontology:cs_authority_grounding('713ed32e-6458-4bd0-b56f-db5b26e2503f', lineage).
narrative_ontology:cs_interpretation_layer_present('713ed32e-6458-4bd0-b56f-db5b26e2503f').
narrative_ontology:cs_reading_relation('713ed32e-6458-4bd0-b56f-db5b26e2503f', waitangi_sovereignty_allocation__crown_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('713ed32e-6458-4bd0-b56f-db5b26e2503f', waitangi_sovereignty_allocation__partnership_reading, influences).
narrative_ontology:cs_axiom('713ed32e-6458-4bd0-b56f-db5b26e2503f', foundational, tino_rangatiratanga_retained).
narrative_ontology:cs_axiom_status(tino_rangatiratanga_retained, holdable).
narrative_ontology:cs_axiom_grounding('713ed32e-6458-4bd0-b56f-db5b26e2503f', tino_rangatiratanga_retained, deontological).
narrative_ontology:cs_axiom('713ed32e-6458-4bd0-b56f-db5b26e2503f', foundational, kawanatanga_limited_to_settlers).
narrative_ontology:cs_axiom_status(kawanatanga_limited_to_settlers, holdable).
narrative_ontology:cs_axiom_grounding('713ed32e-6458-4bd0-b56f-db5b26e2503f', kawanatanga_limited_to_settlers, conventional).
narrative_ontology:cs_reference_frame('713ed32e-6458-4bd0-b56f-db5b26e2503f', maori_inherent_sovereignty).
narrative_ontology:cs_drift_state('713ed32e-6458-4bd0-b56f-db5b26e2503f', contemporary_legal_framework, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('713ed32e-6458-4bd0-b56f-db5b26e2503f', '').
narrative_ontology:cs_kernel_id(waitangi_sovereignty_allocation__rangatiratanga_reading, waitangi_sovereignty_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__rangatiratanga_reading, maori_iwi_hapu).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__rangatiratanga_reading, maori_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__rangatiratanga_reading, maori_communities).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__rangatiratanga_reading, new_zealand_crown).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__rangatiratanga_reading, settler_population).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the collective tribal groups and sub-tribes, they are the primary holders of tino rangatiratanga and would benefit from its full recognition. They actively assert their authority over lands, resources, and taonga, and seek self-determination.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, maori_iwi_hapu, beneficiary,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(waitangi_sovereignty_allocation__rangatiratanga_reading, maori_iwi_hapu, agenda_setter).

% Individual Māori and their communities benefit from the assertion of tino rangatiratanga, which underpins their cultural identity, land rights, and self-governance aspirations. They also bear the costs of the ongoing struggle for recognition.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, maori_communities, beneficiary,
    powerless, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(waitangi_sovereignty_allocation__rangatiratanga_reading, maori_communities, payer).

% The Crown (government of New Zealand) currently asserts full sovereignty, which this reading challenges. Full recognition of rangatiratanga would require the Crown to cede significant control and adjust its governance structures, incurring political and administrative costs.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, new_zealand_crown, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(waitangi_sovereignty_allocation__rangatiratanga_reading, new_zealand_crown, agenda_setter).

% The judiciary plays a critical role in interpreting the Treaty of Waitangi. While some rulings have acknowledged Treaty principles, the dominant legal framework has historically upheld Crown sovereignty, often suppressing the rangatiratanga reading.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, new_zealand_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% The non-Māori population benefits from the current system of Crown sovereignty, which this reading would alter. Adjusting to a system of shared authority or Māori self-governance would require changes in land ownership, resource management, and political representation, which some perceive as a cost.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, settler_population, payer,
    moderate, biographical, mobile, national).

% These advocates observe and support Māori claims for self-determination and the recognition of inherent rights, often citing the rangatiratanga reading as consistent with international indigenous rights frameworks.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, international_indigenous_rights_advocates, observer,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a framework for two distinct authorities (Māori tino rangatiratanga and Crown kāwanatanga) to coexist and govern their respective spheres, preventing unilateral assertion of power and ensuring mutual respect.
% TRANSFER_FUNCTION: This constraint, if upheld, prevents the transfer of Māori authority, lands, resources, and taonga to the Crown, ensuring their retention by Māori. Its violation, however, facilitates such transfers.
% ABSENT_VOICES: The voices of future generations of Māori, who would inherit the full benefits of tino rangatiratanga, are often marginalized in contemporary legal and political discourse that prioritizes existing Crown structures.
% DISAPPEARANCE_RATIONALE: If the claim of retained tino rangatiratanga vanished overnight, it would solidify Crown sovereignty as absolute, removing a fundamental basis for Māori rights claims, land grievances, and self-determination movements. The constitutional and social fabric of New Zealand would reorganize around a unitary, non-bicultural state.
% FOUNDING_PROBLEM: The Treaty of Waitangi was intended to establish a basis for peaceful coexistence and shared governance between Māori and the British Crown, ensuring Māori retained their inherent authority and possessions while allowing the Crown to govern its settlers.
% FOUNDING_PROBLEM_CORROBORATION: The problem of reconciling Māori authority with Crown governance remains central to New Zealand's constitutional landscape. This is corroborated by ongoing Waitangi Tribunal inquiries, Māori legal scholarship, and continuous political activism from Māori leaders and communities, as well as international indigenous rights bodies.
narrative_ontology:disappearance_verdict(waitangi_sovereignty_allocation__rangatiratanga_reading, world_rearranges).
narrative_ontology:founding_problem_status(waitangi_sovereignty_allocation__rangatiratanga_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(waitangi_sovereignty_allocation__rangatiratanga_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(waitangi_sovereignty_allocation__rangatiratanga_reading, 'none', 1).
narrative_ontology:epsilon_provenance(waitangi_sovereignty_allocation__rangatiratanga_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(waitangi_sovereignty_allocation__rangatiratanga_reading_tests).
:- end_tests(waitangi_sovereignty_allocation__rangatiratanga_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The `base_extractiveness` for this constraint is low (0.15) because the constraint itself is the claim that Māori *retained* authority and resources, implying a non-extractive state if upheld. The high `suppression` (0.80) and `resistance` (0.75) reflect the ongoing struggle for this reading's recognition against the dominant Crown sovereignty narrative. The `theater_ratio` is low (0.10) as the claim of rangatiratanga is a substantive assertion of inherent rights, not a performative act. The coercion grid illustrates how the suppression of this reading and the resistance to it manifest differently across structural, organizational, class, and individual levels over time, with resistance growing particularly strong at the class and individual levels.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Māori, this constraint represents a fundamental truth about their inherent authority, which should lead to a system of coordinated governance. From the Crown's perspective, this reading is a challenge to its asserted unitary sovereignty, requiring active suppression to maintain the status quo. The engine's computation will highlight this divergence between the claimed 'rope' (coordination) and the high suppression it faces.
 *
 * DIRECTIONALITY LOGIC:
 *   Māori iwi, hapū, and communities are the direct beneficiaries of this constraint, as it asserts their inherent authority and rights (low d). The New Zealand Crown and the settler population are effectively 'payers' in this context, as upholding this reading would require them to relinquish asserted sovereignty and adjust to a different power dynamic (high d). The judiciary acts as an agenda-setter, interpreting the Treaty, but its historical bias towards Crown sovereignty means it often contributes to the suppression of this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    treaty_text_interpretation_ambiguity,
    'Is the difference between the Māori text (tino rangatiratanga, kāwanatanga) and the English text (cession of sovereignty) a genuine textual ambiguity or a deliberate misrepresentation?',
    'Forensic linguistic analysis of 19th-century Māori and English legal terminology, combined with historical records of the drafting and signing process, and contemporary Māori oral histories.',
    'If a deliberate misrepresentation is established, it strengthens the claim that Crown sovereignty was never legitimately ceded, reinforcing the rangatiratanga reading''s foundational status and undermining the legitimacy of the crown_sovereignty_reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(treaty_text_interpretation_ambiguity, empirical, 'Ambiguity in Treaty text interpretation.').

omega_variable(
    kawanatanga_scope_ambiguity,
    'What was the intended scope of ''kāwanatanga'' (governorship) as understood by Māori signatories at the time of the Treaty signing?',
    'Analysis of pre-Treaty Māori political concepts, contemporary Māori usage of ''kāwanatanga'', and comparison with other indigenous treaties of the era. This would involve extensive historical and linguistic research.',
    'A narrow interpretation of kāwanatanga (e.g., limited to governing settlers) would strongly support the rangatiratanga reading. A broader interpretation (e.g., general administrative authority) would lend more credence to the partnership reading, but still fall short of full Crown sovereignty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kawanatanga_scope_ambiguity, conceptual, 'The scope of Crown governorship.').

omega_variable(
    legal_precedent_vs_inherent_rights,
    'To what extent should established legal precedent upholding Crown sovereignty override the inherent rights and original understanding of tino rangatiratanga?',
    'This is a fundamental question of legal philosophy and constitutional theory, requiring a shift in the foundational principles of the New Zealand legal system, potentially through constitutional reform or a re-evaluation of legal positivism versus natural/indigenous law.',
    'If inherent rights are prioritized, the rangatiratanga reading gains significant legal force, potentially leading to a re-structuring of governance. If precedent is strictly maintained, the rangatiratanga reading remains largely a moral/political claim outside the dominant legal framework.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legal_precedent_vs_inherent_rights, preference, 'The tension between legal precedent and inherent rights.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(waitangi_sovereignty_allocation__rangatiratanga_reading, 1975, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wait_tr_t1975, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 1975, 0.1).
narrative_ontology:measurement(wait_tr_t1985, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 1985, 0.1).
narrative_ontology:measurement(wait_tr_t1995, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 1995, 0.1).
narrative_ontology:measurement(wait_tr_t2005, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 2005, 0.1).
narrative_ontology:measurement(wait_tr_t2015, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(wait_tr_t2025, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 2025, 0.1).

% Extraction over time
narrative_ontology:measurement(wait_be_t1975, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 1975, 0.15).
narrative_ontology:measurement(wait_be_t1985, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 1985, 0.15).
narrative_ontology:measurement(wait_be_t1995, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 1995, 0.15).
narrative_ontology:measurement(wait_be_t2005, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 2005, 0.15).
narrative_ontology:measurement(wait_be_t2015, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 2015, 0.15).
narrative_ontology:measurement(wait_be_t2025, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 2025, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(wait_su_t1975, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 1975, 0.7).
narrative_ontology:measurement(wait_su_t1985, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 1985, 0.75).
narrative_ontology:measurement(wait_su_t1995, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 1995, 0.75).
narrative_ontology:measurement(wait_su_t2005, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 2005, 0.7).
narrative_ontology:measurement(wait_su_t2015, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 2015, 0.7).
narrative_ontology:measurement(wait_su_t2025, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 2025, 0.78).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1975, tn=2025
narrative_ontology:measurement(wait_grid_01, waitangi_sovereignty_allocation__rangatiratanga_reading, accessibility_collapse(class), 1975, 0.7).
narrative_ontology:measurement(wait_grid_02, waitangi_sovereignty_allocation__rangatiratanga_reading, accessibility_collapse(class), 2025, 0.75).
narrative_ontology:measurement(wait_grid_03, waitangi_sovereignty_allocation__rangatiratanga_reading, accessibility_collapse(individual), 1975, 0.6).
narrative_ontology:measurement(wait_grid_04, waitangi_sovereignty_allocation__rangatiratanga_reading, accessibility_collapse(individual), 2025, 0.65).
narrative_ontology:measurement(wait_grid_05, waitangi_sovereignty_allocation__rangatiratanga_reading, accessibility_collapse(organizational), 1975, 0.7).
narrative_ontology:measurement(wait_grid_06, waitangi_sovereignty_allocation__rangatiratanga_reading, accessibility_collapse(organizational), 2025, 0.75).
narrative_ontology:measurement(wait_grid_07, waitangi_sovereignty_allocation__rangatiratanga_reading, accessibility_collapse(structural), 1975, 0.8).
narrative_ontology:measurement(wait_grid_08, waitangi_sovereignty_allocation__rangatiratanga_reading, accessibility_collapse(structural), 2025, 0.85).
narrative_ontology:measurement(wait_grid_09, waitangi_sovereignty_allocation__rangatiratanga_reading, resistance(class), 1975, 0.6).
narrative_ontology:measurement(wait_grid_10, waitangi_sovereignty_allocation__rangatiratanga_reading, resistance(class), 2025, 0.8).
narrative_ontology:measurement(wait_grid_11, waitangi_sovereignty_allocation__rangatiratanga_reading, resistance(individual), 1975, 0.65).
narrative_ontology:measurement(wait_grid_12, waitangi_sovereignty_allocation__rangatiratanga_reading, resistance(individual), 2025, 0.85).
narrative_ontology:measurement(wait_grid_13, waitangi_sovereignty_allocation__rangatiratanga_reading, resistance(organizational), 1975, 0.5).
narrative_ontology:measurement(wait_grid_14, waitangi_sovereignty_allocation__rangatiratanga_reading, resistance(organizational), 2025, 0.7).
narrative_ontology:measurement(wait_grid_15, waitangi_sovereignty_allocation__rangatiratanga_reading, resistance(structural), 1975, 0.4).
narrative_ontology:measurement(wait_grid_16, waitangi_sovereignty_allocation__rangatiratanga_reading, resistance(structural), 2025, 0.6).
narrative_ontology:measurement(wait_grid_17, waitangi_sovereignty_allocation__rangatiratanga_reading, stakes_inflation(class), 1975, 0.6).
narrative_ontology:measurement(wait_grid_18, waitangi_sovereignty_allocation__rangatiratanga_reading, stakes_inflation(class), 2025, 0.7).
narrative_ontology:measurement(wait_grid_19, waitangi_sovereignty_allocation__rangatiratanga_reading, stakes_inflation(individual), 1975, 0.5).
narrative_ontology:measurement(wait_grid_20, waitangi_sovereignty_allocation__rangatiratanga_reading, stakes_inflation(individual), 2025, 0.6).
narrative_ontology:measurement(wait_grid_21, waitangi_sovereignty_allocation__rangatiratanga_reading, stakes_inflation(organizational), 1975, 0.6).
narrative_ontology:measurement(wait_grid_22, waitangi_sovereignty_allocation__rangatiratanga_reading, stakes_inflation(organizational), 2025, 0.7).
narrative_ontology:measurement(wait_grid_23, waitangi_sovereignty_allocation__rangatiratanga_reading, stakes_inflation(structural), 1975, 0.7).
narrative_ontology:measurement(wait_grid_24, waitangi_sovereignty_allocation__rangatiratanga_reading, stakes_inflation(structural), 2025, 0.8).
narrative_ontology:measurement(wait_grid_25, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression(class), 1975, 0.6).
narrative_ontology:measurement(wait_grid_26, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression(class), 2025, 0.7).
narrative_ontology:measurement(wait_grid_27, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression(individual), 1975, 0.55).
narrative_ontology:measurement(wait_grid_28, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression(individual), 2025, 0.65).
narrative_ontology:measurement(wait_grid_29, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression(organizational), 1975, 0.65).
narrative_ontology:measurement(wait_grid_30, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression(organizational), 2025, 0.75).
narrative_ontology:measurement(wait_grid_31, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression(structural), 1975, 0.7).
narrative_ontology:measurement(wait_grid_32, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression(structural), 2025, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(waitangi_sovereignty_allocation__rangatiratanga_reading, identity_coordination).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__rangatiratanga_reading, waitangi_sovereignty_allocation__crown_sovereignty_reading).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__rangatiratanga_reading, waitangi_sovereignty_allocation__partnership_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'waitangi_sovereignty_allocation' kernel. This 'rangatiratanga_reading' focuses on Māori retention of full authority, contrasting with the 'crown_sovereignty_reading' (full cession) and the 'partnership_reading' (co-governance).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
