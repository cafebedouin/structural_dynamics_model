% ============================================================================
% CONSTRAINT STORY: lausanne_minority_protections__restrictive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lausanne_minority_protections__restrictive_reading, []).

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
 *   constraint_id: lausanne_minority_protections__restrictive_reading
 *   human_readable: Lausanne Treaty Minority Protections (Restrictive Reading)
 *   domain: international_law/religious_governance/minority_rights
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lausanne_minority_protections__restrictive_reading, 0.88).
domain_priors:suppression_score(lausanne_minority_protections__restrictive_reading, 0.92).
domain_priors:theater_ratio(lausanne_minority_protections__restrictive_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lausanne_minority_protections__restrictive_reading, snare).
narrative_ontology:human_readable(lausanne_minority_protections__restrictive_reading, "Lausanne Treaty Minority Protections (Restrictive Reading)").
narrative_ontology:topic_domain(lausanne_minority_protections__restrictive_reading, "international_law/religious_governance/minority_rights").

domain_priors:requires_active_enforcement(lausanne_minority_protections__restrictive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lausanne_minority_protections__restrictive_reading, '37659a25-5a32-469c-95d2-27f4e0d7a136').
narrative_ontology:cs_kernel_codification('37659a25-5a32-469c-95d2-27f4e0d7a136', fixed_text).
narrative_ontology:cs_authority_grounding('37659a25-5a32-469c-95d2-27f4e0d7a136', extraction).
narrative_ontology:cs_interpretation_layer_present('37659a25-5a32-469c-95d2-27f4e0d7a136').
narrative_ontology:cs_reading_relation('37659a25-5a32-469c-95d2-27f4e0d7a136', lausanne_minority_protections__expansive_reading, forecloses).
narrative_ontology:cs_reading_relation('37659a25-5a32-469c-95d2-27f4e0d7a136', lausanne_minority_protections__guarantor_reading, influences).
narrative_ontology:cs_axiom('37659a25-5a32-469c-95d2-27f4e0d7a136', foundational, sovereignty_over_institutional_autonomy).
narrative_ontology:cs_axiom_status(sovereignty_over_institutional_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('37659a25-5a32-469c-95d2-27f4e0d7a136', sovereignty_over_institutional_autonomy, conventional).
narrative_ontology:cs_axiom('37659a25-5a32-469c-95d2-27f4e0d7a136', foundational, individual_worship_only_protection).
narrative_ontology:cs_axiom_status(individual_worship_only_protection, holdable).
narrative_ontology:cs_axiom_grounding('37659a25-5a32-469c-95d2-27f4e0d7a136', individual_worship_only_protection, conventional).
narrative_ontology:cs_reference_frame('37659a25-5a32-469c-95d2-27f4e0d7a136', national_sovereignty_supremacy).
narrative_ontology:cs_drift_state('37659a25-5a32-469c-95d2-27f4e0d7a136', contemporary_human_rights_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('37659a25-5a32-469c-95d2-27f4e0d7a136', '').
narrative_ontology:cs_kernel_id(lausanne_minority_protections__restrictive_reading, lausanne_minority_protections).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__restrictive_reading, turkish_state_apparatus).
narrative_ontology:constraint_victim(lausanne_minority_protections__restrictive_reading, non_muslim_minority_institutions).
narrative_ontology:constraint_victim(lausanne_minority_protections__restrictive_reading, non_muslim_minority_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the Lausanne Treaty narrowly, asserting that institutional autonomy, property ownership, and theological education for non-Muslim minorities fall under general domestic law, not treaty protections. Benefits from consolidating control over minority institutional capacity and assets.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, turkish_state_apparatus, agenda_setter,
    institutional, generational, constrained, national).

% Subject to property confiscation, denial of legal personality, and closure of theological schools. Their existence and function are severely constrained by domestic laws that override treaty protections under this reading. Exit means dissolution.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, non_muslim_minority_institutions, payer,
    powerless, generational, trapped, local).

% Experience the erosion of their cultural and religious heritage due to the weakening of their institutions. Their identity is deeply tied to these institutions, making 'exit' from the community or faith unthinkable, but their ability to sustain their traditions is severely hampered.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, non_muslim_minority_communities, payer,
    powerless, generational, identity_locked, local).

% Signatories to the Lausanne Treaty who, under this restrictive reading, have limited grounds for intervention beyond individual worship rights. Their role as international guarantors is minimized, allowing domestic interpretation to prevail.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, guarantor_states, excluded,
    institutional, generational, analytical, global).

% Monitor human rights compliance but face jurisdictional and political challenges in enforcing an expansive reading of Lausanne against a sovereign state's domestic law, especially when the state asserts the treaty's narrow scope.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, european_human_rights_mechanisms, observer,
    institutional, generational, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This reading coordinates the Turkish state's internal legal framework with its interpretation of international treaty obligations, ensuring domestic law takes precedence over minority institutional autonomy.
% TRANSFER_FUNCTION: Transfers control over minority institutional assets, educational capacity, and legal personality from non-Muslim minority communities to the Turkish state apparatus.
% ABSENT_VOICES: Non-Muslim minority institutions and communities, whose historical autonomy and property rights are denied, are effectively silenced in the interpretive process. Guarantor states and international human rights bodies are marginalized in their ability to advocate for an expansive reading.
% DISAPPEARANCE_RATIONALE: If this restrictive reading vanished, non-Muslim minority institutions would immediately seek to reclaim property, re-establish legal personality, and reopen theological schools, fundamentally altering the landscape of religious governance and minority rights in Turkey. The state's control over these domains would be significantly diminished.
% FOUNDING_PROBLEM: The Lausanne Treaty aimed to establish peace and define the borders of modern Turkey, including provisions for the protection of non-Muslim minorities following the collapse of the Ottoman Empire.
% FOUNDING_PROBLEM_CORROBORATION: The Turkish state apparatus asserts that the treaty's primary problem (establishing sovereignty and peace) is live and requires a narrow interpretation of minority rights to prevent external interference. Non-Muslim minority communities and international legal scholars (outside the benefiting parties) argue that the problem of minority protection remains live and is exacerbated by this restrictive reading, which undermines the treaty's original intent for their functional continuity.
narrative_ontology:disappearance_verdict(lausanne_minority_protections__restrictive_reading, world_rearranges).
narrative_ontology:founding_problem_status(lausanne_minority_protections__restrictive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lausanne_minority_protections__restrictive_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(lausanne_minority_protections__restrictive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lausanne_minority_protections__restrictive_reading, 0.88, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lausanne_minority_protections__restrictive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(lausanne_minority_protections__restrictive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(lausanne_minority_protections__restrictive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */


/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    treaty_original_intent,
    'What was the original, mutually understood intent of the Lausanne Treaty''s minority protection clauses regarding institutional autonomy and property rights?',
    'Historical-legal analysis of diplomatic archives, preparatory works (travaux préparatoires), and contemporary state practice of all signatories.',
    'If original intent supports an expansive reading, the restrictive reading is revealed as a deviation from the treaty''s foundational coordination, strengthening arguments for international intervention. If original intent supports the restrictive reading, the current situation is consistent with the treaty''s design.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(treaty_original_intent, empirical, 'Ambiguity regarding the original intent of the Lausanne Treaty''s minority protection clauses.').

omega_variable(
    domestic_law_vs_international_obligation,
    'To what extent does international law permit a state to interpret treaty obligations concerning minority rights as purely domestic matters, overriding institutional autonomy?',
    'Advisory opinions from international courts (e.g., ICJ, ECtHR) on the hierarchy and interpretation of human rights treaties versus domestic law in similar contexts.',
    'A strong international legal opinion against the restrictive interpretation would delegitimize the state''s position, increasing pressure for compliance. If international law is ambiguous, the state''s position gains tacit support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domestic_law_vs_international_obligation, conceptual, 'The conceptual boundary between domestic legal jurisdiction and international treaty obligations for minority rights.').

omega_variable(
    identity_lock_strength,
    'How deeply are non-Muslim minority communities'' identities fused with their traditional institutions (churches, schools, foundations), and what is the long-term impact of institutional erosion on their cultural survival?',
    'Sociological studies, ethnographic research, and demographic analysis of minority communities over time, tracking cultural transmission and identity markers.',
    'Strong identity fusion and severe cultural impact would amplify the effective extraction and suppression, highlighting the existential threat posed by the restrictive reading. Weak fusion would suggest less severe long-term consequences.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_strength, empirical, 'The degree to which minority identity is locked into institutions targeted by the restrictive reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lausanne_minority_protections__restrictive_reading, 1923, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(laus_be_t1923, lausanne_minority_protections__restrictive_reading, base_extractiveness, 1923, 0.6).
narrative_ontology:measurement(laus_be_t1945, lausanne_minority_protections__restrictive_reading, base_extractiveness, 1945, 0.7).
narrative_ontology:measurement(laus_be_t1970, lausanne_minority_protections__restrictive_reading, base_extractiveness, 1970, 0.8).
narrative_ontology:measurement(laus_be_t1995, lausanne_minority_protections__restrictive_reading, base_extractiveness, 1995, 0.85).
narrative_ontology:measurement(laus_be_t2024, lausanne_minority_protections__restrictive_reading, base_extractiveness, 2024, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(laus_su_t1923, lausanne_minority_protections__restrictive_reading, suppression_requirement, 1923, 0.7).
narrative_ontology:measurement(laus_su_t1945, lausanne_minority_protections__restrictive_reading, suppression_requirement, 1945, 0.78).
narrative_ontology:measurement(laus_su_t1970, lausanne_minority_protections__restrictive_reading, suppression_requirement, 1970, 0.85).
narrative_ontology:measurement(laus_su_t1995, lausanne_minority_protections__restrictive_reading, suppression_requirement, 1995, 0.9).
narrative_ontology:measurement(laus_su_t2024, lausanne_minority_protections__restrictive_reading, suppression_requirement, 2024, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
