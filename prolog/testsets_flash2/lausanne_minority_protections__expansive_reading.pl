% ============================================================================
% CONSTRAINT STORY: lausanne_minority_protections__expansive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lausanne_minority_protections__expansive_reading, []).

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
 *   constraint_id: lausanne_minority_protections__expansive_reading
 *   human_readable: Lausanne Protections: Expansive Reading of Minority Religious Governance
 *   domain: international_law/religious_governance/minority_rights
 *
 * SUMMARY:
 *   This constraint represents an 'expansive reading' of the Lausanne
 *   Treaty's protections for non-Muslim minorities in Turkey. Under this
 *   reading, the treaty guarantees not just individual worship rights, but
 *   also the functional continuity of pre-1923 religious governance,
 *   including institutional self-administration, property rights, and the
 *   ability to form clergy via theological schools. It is claimed as a Rope
 *   because it genuinely coordinates the existence of minority institutions,
 *   but its operation involves moderate extractiveness due to ongoing
 *   challenges and suppressive efforts by the Turkish state to limit its
 *   scope. The metrics reflect a period of increasing pressure on these
 *   rights, followed by some stabilization.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lausanne_minority_protections__expansive_reading, 0.25).
domain_priors:suppression_score(lausanne_minority_protections__expansive_reading, 0.4).
domain_priors:theater_ratio(lausanne_minority_protections__expansive_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lausanne_minority_protections__expansive_reading, rope).
narrative_ontology:human_readable(lausanne_minority_protections__expansive_reading, "Lausanne Protections: Expansive Reading of Minority Religious Governance").
narrative_ontology:topic_domain(lausanne_minority_protections__expansive_reading, "international_law/religious_governance/minority_rights").

domain_priors:requires_active_enforcement(lausanne_minority_protections__expansive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lausanne_minority_protections__expansive_reading, '47a563ce-1919-4f84-8aa8-ccbe2883c0da').
narrative_ontology:cs_kernel_codification('47a563ce-1919-4f84-8aa8-ccbe2883c0da', fixed_text).
narrative_ontology:cs_authority_grounding('47a563ce-1919-4f84-8aa8-ccbe2883c0da', lineage).
narrative_ontology:cs_interpretation_layer_present('47a563ce-1919-4f84-8aa8-ccbe2883c0da').
narrative_ontology:cs_reading_relation('47a563ce-1919-4f84-8aa8-ccbe2883c0da', lausanne_minority_protections__restrictive_reading, coexists_with).
narrative_ontology:cs_reading_relation('47a563ce-1919-4f84-8aa8-ccbe2883c0da', lausanne_minority_protections__guarantor_reading, coexists_with).
narrative_ontology:cs_axiom('47a563ce-1919-4f84-8aa8-ccbe2883c0da', foundational, institutional_autonomy_as_core_right).
narrative_ontology:cs_axiom_status(institutional_autonomy_as_core_right, holdable).
narrative_ontology:cs_axiom_grounding('47a563ce-1919-4f84-8aa8-ccbe2883c0da', institutional_autonomy_as_core_right, deontological).
narrative_ontology:cs_axiom('47a563ce-1919-4f84-8aa8-ccbe2883c0da', foundational, functional_continuity_of_pre_1923_governance).
narrative_ontology:cs_axiom_status(functional_continuity_of_pre_1923_governance, holdable).
narrative_ontology:cs_axiom_grounding('47a563ce-1919-4f84-8aa8-ccbe2883c0da', functional_continuity_of_pre_1923_governance, conventional).
narrative_ontology:cs_reference_frame('47a563ce-1919-4f84-8aa8-ccbe2883c0da', post_ottoman_minority_protection_framework).
narrative_ontology:cs_drift_state('47a563ce-1919-4f84-8aa8-ccbe2883c0da', contemporary_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('47a563ce-1919-4f84-8aa8-ccbe2883c0da', '').
narrative_ontology:cs_kernel_id(lausanne_minority_protections__expansive_reading, lausanne_minority_protections).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__expansive_reading, non_muslim_minority_foundations).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__expansive_reading, minority_religious_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These foundations (e.g., Greek, Armenian, Jewish) rely on the expansive reading of Lausanne to maintain their property, administer their institutions, and operate theological schools. Their functional continuity is directly tied to the treaty's interpretation and enforcement.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, non_muslim_minority_foundations, beneficiary,
    organized, generational, constrained, national).

% The communities themselves benefit from the institutional stability and self-governance provided by this reading, allowing them to preserve their cultural and religious heritage. Their identity is deeply intertwined with these institutions.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, minority_religious_communities, beneficiary,
    moderate, generational, identity_locked, local).

% As the signatory state, Turkey is the primary enforcer and interpreter of the Lausanne Treaty. Its actions determine the practical scope of these protections. While bound by the treaty, its domestic legal framework often seeks to limit the expansive interpretation.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, turkish_state, agenda_setter,
    institutional, civilizational, constrained, national).

% The other signatories to the Lausanne Treaty (e.g., UK, France, Italy) are nominal guarantors of its provisions. Their diplomatic pressure and engagement with international human rights mechanisms can influence the Turkish state's interpretation and enforcement.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, guarantor_states, observer,
    institutional, generational, analytical, global).

% Provides an external legal avenue for minority communities to challenge violations of their rights, including those related to Lausanne. Its rulings can influence the domestic application of the treaty, though enforcement depends on the Turkish state's compliance.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, european_court_of_human_rights, observer,
    institutional, generational, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for the functional continuity and self-administration of non-Muslim minority religious institutions, ensuring their ability to operate schools, manage property, and form clergy, thereby preventing their dissolution and preserving their distinct identities.
% TRANSFER_FUNCTION: Transfers the right to self-administer religious institutions and property from the general purview of the Turkish state to the specific minority communities, as guaranteed by the treaty. It also transfers the responsibility for upholding these rights to the Turkish state.
% ABSENT_VOICES: Hardline nationalist factions within Turkey would argue that such expansive protections infringe on national sovereignty and should be curtailed, advocating for a purely restrictive interpretation. They are present in domestic political discourse but are not direct parties to the treaty's enforcement mechanisms.
% DISAPPEARANCE_RATIONALE: If these protections vanished, minority religious institutions would lose their legal basis for self-administration and property rights, likely leading to their absorption or dissolution under general domestic law, fundamentally altering the landscape of minority religious life in Turkey.
% FOUNDING_PROBLEM: The problem of ensuring the rights and functional continuity of non-Muslim minorities in the newly formed Republic of Turkey after the collapse of the Ottoman Empire, preventing their forced assimilation or displacement.
% FOUNDING_PROBLEM_CORROBORATION: Minority community leaders and international human rights organizations attest that the problem of protecting minority rights and ensuring their continuity remains live, citing ongoing challenges to institutional autonomy and property rights. The Turkish state acknowledges the treaty's existence but often interprets its scope more narrowly.
narrative_ontology:disappearance_verdict(lausanne_minority_protections__expansive_reading, world_rearranges).
narrative_ontology:founding_problem_status(lausanne_minority_protections__expansive_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lausanne_minority_protections__expansive_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(lausanne_minority_protections__expansive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lausanne_minority_protections__expansive_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lausanne_minority_protections__expansive_reading_tests).
:- end_tests(lausanne_minority_protections__expansive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.25) is moderate because while the treaty provides a framework, its implementation often involves friction and costs for minority institutions in defending their rights. Suppression (0.4) is present as the Turkish state, at various times, has actively sought to restrict the interpretation of these rights, for example, by closing theological schools or challenging property ownership. Theater ratio (0.1) is low, indicating that the core function of protecting these rights is still active, though sometimes contested. The slight dip in extractiveness and suppression towards the end of the interval reflects periods of increased international scrutiny and domestic legal reforms.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of minority communities, this reading is a vital Rope, ensuring their survival. From the perspective of the Turkish state, it is a binding international obligation that must be managed within its sovereign legal framework, often leading to a more restrictive interpretation. The engine's classification will reflect the tension between the coordination function and the costs imposed by the state's interpretive stance.
 *
 * DIRECTIONALITY LOGIC:
 *   Non-Muslim minority foundations and communities are clear beneficiaries, as the constraint directly enables their existence and self-governance. The Turkish state, as the primary enforcer and interpreter, acts as the agenda-setter, whose actions determine the practical scope of the protections. Guarantor states and the ECHR act as observers, influencing the constraint's application through diplomatic and legal means.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretive_contest_resolution,
    'Will the interpretive contest between the expansive, restrictive, and guarantor readings of the Lausanne protections be resolved, and if so, by what mechanism?',
    'A definitive ruling by an international court with binding authority over all parties, or a renegotiation of the treaty''s specific provisions.',
    'Resolution in favor of the expansive reading would reduce extractiveness and suppression for minority communities; resolution in favor of the restrictive reading would increase them, potentially reclassifying this constraint as a Snare or Piton for the minority seats.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(interpretive_contest_resolution, conceptual, 'The fundamental ambiguity in the treaty''s scope and enforcement.').

omega_variable(
    state_sovereignty_vs_minority_rights,
    'To what extent does the Turkish state''s assertion of national sovereignty legitimately override or limit the expansive interpretation of minority rights under Lausanne?',
    'A shift in international legal norms regarding minority rights and state sovereignty, or a domestic constitutional amendment explicitly clarifying the treaty''s precedence and scope.',
    'If state sovereignty is deemed to legitimately limit the expansive reading, the constraint''s effective scope for minority institutions would shrink, increasing their vulnerability. If minority rights are prioritized, the state''s suppressive actions would be more clearly identified as violations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_sovereignty_vs_minority_rights, preference, 'The tension between national sovereignty and international minority protection.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lausanne_minority_protections__expansive_reading, 1923, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(laus_tr_t1923, lausanne_minority_protections__expansive_reading, theater_ratio, 1923, 0.05).
narrative_ontology:measurement(laus_tr_t1948, lausanne_minority_protections__expansive_reading, theater_ratio, 1948, 0.08).
narrative_ontology:measurement(laus_tr_t1973, lausanne_minority_protections__expansive_reading, theater_ratio, 1973, 0.1).
narrative_ontology:measurement(laus_tr_t1998, lausanne_minority_protections__expansive_reading, theater_ratio, 1998, 0.12).
narrative_ontology:measurement(laus_tr_t2023, lausanne_minority_protections__expansive_reading, theater_ratio, 2023, 0.1).

% Extraction over time
narrative_ontology:measurement(laus_be_t1923, lausanne_minority_protections__expansive_reading, base_extractiveness, 1923, 0.15).
narrative_ontology:measurement(laus_be_t1948, lausanne_minority_protections__expansive_reading, base_extractiveness, 1948, 0.2).
narrative_ontology:measurement(laus_be_t1973, lausanne_minority_protections__expansive_reading, base_extractiveness, 1973, 0.25).
narrative_ontology:measurement(laus_be_t1998, lausanne_minority_protections__expansive_reading, base_extractiveness, 1998, 0.28).
narrative_ontology:measurement(laus_be_t2023, lausanne_minority_protections__expansive_reading, base_extractiveness, 2023, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(laus_su_t1923, lausanne_minority_protections__expansive_reading, suppression_requirement, 1923, 0.3).
narrative_ontology:measurement(laus_su_t1948, lausanne_minority_protections__expansive_reading, suppression_requirement, 1948, 0.35).
narrative_ontology:measurement(laus_su_t1973, lausanne_minority_protections__expansive_reading, suppression_requirement, 1973, 0.4).
narrative_ontology:measurement(laus_su_t1998, lausanne_minority_protections__expansive_reading, suppression_requirement, 1998, 0.45).
narrative_ontology:measurement(laus_su_t2023, lausanne_minority_protections__expansive_reading, suppression_requirement, 2023, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lausanne_minority_protections__expansive_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(lausanne_minority_protections__expansive_reading, lausanne_minority_protections__restrictive_reading).
narrative_ontology:affects_constraint(lausanne_minority_protections__expansive_reading, lausanne_minority_protections__guarantor_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the Lausanne minority protections kernel. This 'expansive reading' focuses on institutional autonomy, property, and education, contrasting with the 'restrictive reading' (individual worship only) and the 'guarantor reading' (international enforcement).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
