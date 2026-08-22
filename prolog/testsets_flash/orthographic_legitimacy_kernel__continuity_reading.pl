% ============================================================================
% CONSTRAINT STORY: orthographic_legitimacy_kernel__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_orthographic_legitimacy_kernel__continuity_reading, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: orthographic_legitimacy_kernel__continuity_reading
 *   human_readable: Orthographic Legitimacy: Continuity with Tradition
 *   domain: political_linguistics/state_formation/commitment_systems
 *
 * SUMMARY:
 *   This constraint represents the 'continuity reading' of orthographic
 *   legitimacy, where the value of a script derives from its ability to
 *   preserve direct access to historical, religious, and literary traditions.
 *   It is framed as a Mountain because the incompatibility between different
 *   scripts (e.g., Arabic vs. Latin script for Turkish) is a physical fact of
 *   the written medium, creating an irreducible barrier for those educated in
 *   a different system. The 'extraction' is the loss of direct access to
 *   heritage for post-reform generations, which is an inherent cost of script
 *   change, not an actively extracted rent. The beneficiaries are those whose
 *   roles depend on this continuity.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(orthographic_legitimacy_kernel__continuity_reading, 0.15).
domain_priors:suppression_score(orthographic_legitimacy_kernel__continuity_reading, 0.05).
domain_priors:theater_ratio(orthographic_legitimacy_kernel__continuity_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(orthographic_legitimacy_kernel__continuity_reading, mountain).
narrative_ontology:human_readable(orthographic_legitimacy_kernel__continuity_reading, "Orthographic Legitimacy: Continuity with Tradition").
narrative_ontology:topic_domain(orthographic_legitimacy_kernel__continuity_reading, "political_linguistics/state_formation/commitment_systems").

domain_priors:emerges_naturally(orthographic_legitimacy_kernel__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(orthographic_legitimacy_kernel__continuity_reading, '7f667013-aecd-4403-82be-af54c040849c').
narrative_ontology:cs_kernel_codification('7f667013-aecd-4403-82be-af54c040849c', fixed_text).
narrative_ontology:cs_authority_grounding('7f667013-aecd-4403-82be-af54c040849c', lineage).
narrative_ontology:cs_interpretation_layer_present('7f667013-aecd-4403-82be-af54c040849c').
narrative_ontology:cs_reading_relation('7f667013-aecd-4403-82be-af54c040849c', orthographic_legitimacy_kernel__modernist_reading, coexists_with).
narrative_ontology:cs_reading_relation('7f667013-aecd-4403-82be-af54c040849c', orthographic_legitimacy_kernel__instrumentalist_reading, coexists_with).
narrative_ontology:cs_axiom('7f667013-aecd-4403-82be-af54c040849c', foundational, textual_continuity_is_sacred).
narrative_ontology:cs_axiom_status(textual_continuity_is_sacred, holdable).
narrative_ontology:cs_axiom_grounding('7f667013-aecd-4403-82be-af54c040849c', textual_continuity_is_sacred, deontological).
narrative_ontology:cs_axiom('7f667013-aecd-4403-82be-af54c040849c', foundational, historical_access_is_identity).
narrative_ontology:cs_axiom_status(historical_access_is_identity, holdable).
narrative_ontology:cs_axiom_grounding('7f667013-aecd-4403-82be-af54c040849c', historical_access_is_identity, conventional).
narrative_ontology:cs_reference_frame('7f667013-aecd-4403-82be-af54c040849c', unbroken_textual_tradition).
narrative_ontology:cs_drift_state('7f667013-aecd-4403-82be-af54c040849c', post_script_reform_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7f667013-aecd-4403-82be-af54c040849c', '').
narrative_ontology:cs_kernel_id(orthographic_legitimacy_kernel__continuity_reading, orthographic_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__continuity_reading, scholars_of_classical_texts).
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__continuity_reading, religious_authorities).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__continuity_reading, post_reform_generations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Their professional identity and access to knowledge are directly tied to the preservation of traditional orthography. They benefit from the continuity of script, which ensures their expertise remains relevant and the historical archive accessible.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__continuity_reading, scholars_of_classical_texts, beneficiary,
    moderate, generational, identity_locked, national).

% The legitimacy of religious texts and their interpretation often depends on the stability of the script in which they are written. They benefit from the constraint by maintaining direct access to sacred traditions without linguistic mediation.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__continuity_reading, religious_authorities, beneficiary,
    institutional, civilizational, identity_locked, national).

% These generations, educated in a reformed or new script, face a structural barrier to accessing pre-reform historical, religious, and literary texts. They bear the cost of needing translation or specialized training to engage with their own cultural heritage.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__continuity_reading, post_reform_generations, payer,
    powerless, biographical, trapped, national).

% Actively advocate for policies that preserve traditional orthography, viewing it as essential for national identity and cultural heritage. They set the agenda for educational and linguistic policy to resist script reforms.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__continuity_reading, cultural_conservatives, agenda_setter,
    organized, generational, constrained, national).

% Study the evolution of scripts and their impact on cultural transmission. They observe the effects of orthographic changes on historical continuity and access to past knowledge, providing analytical insights without direct participation in the policy debate.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__continuity_reading, linguistic_historians, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures a consistent written medium across generations, allowing direct access to historical, religious, and literary texts without the need for translation or transliteration.
% TRANSFER_FUNCTION: Transfers cultural and historical knowledge across time by maintaining a stable orthographic link, from past generations to present and future ones. The cost is borne by those who might prefer a more 'efficient' or 'modern' script.
% ABSENT_VOICES: Future generations who might prefer a more phonetically consistent or simpler script are not present to advocate for their needs, and their potential severance from historical texts is a deferred cost.
% DISAPPEARANCE_RATIONALE: If the constraint of orthographic continuity vanished, the direct link to historical and religious texts would be broken for new generations, requiring massive translation efforts or leading to cultural amnesia. The entire educational and cultural infrastructure would need to reorganize.
% FOUNDING_PROBLEM: The problem of maintaining cultural and religious continuity across generations, ensuring that foundational texts remain directly accessible and authoritative.
% FOUNDING_PROBLEM_CORROBORATION: Scholars of classical texts and religious authorities consistently attest to the ongoing importance of direct textual access for their disciplines and for cultural identity. Linguistic historians corroborate the challenge of maintaining such continuity across orthographic shifts.
narrative_ontology:disappearance_verdict(orthographic_legitimacy_kernel__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(orthographic_legitimacy_kernel__continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(orthographic_legitimacy_kernel__continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(orthographic_legitimacy_kernel__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(orthographic_legitimacy_kernel__continuity_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(orthographic_legitimacy_kernel__continuity_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, ExtMetricName, E),
    domain_priors:suppression_score(orthographic_legitimacy_kernel__continuity_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(orthographic_legitimacy_kernel__continuity_reading),
    narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(orthographic_legitimacy_kernel__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness (0.15) reflects that the 'cost' is primarily a structural barrier (incompatibility) rather than an active transfer of resources. Suppression is very low (0.05) because the constraint persists due to the nature of script itself, not active coercion. Theater ratio is 0.0 as there's no performative maintenance; the constraint is a direct consequence of the physical medium. Accessibility collapse is high (0.88) because once a script changes, direct access to the old script's texts collapses for new generations. Resistance is low (0.1) because the 'resistance' is typically against the *change* of script, not against the continuity itself.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of those who value continuity, the constraint is a natural, unavoidable aspect of cultural transmission. From the perspective of post-reform generations, it is a barrier to their own heritage, a 'cost' imposed by historical choices. The engine's classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Scholars and religious authorities are beneficiaries (d near 0.0) as their roles and knowledge are preserved by orthographic continuity. Post-reform generations are payers (d near 1.0) as they bear the cost of severed access. Cultural conservatives are agenda-setters, actively defending this continuity. Linguistic historians are observers.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_vs_constructed_barrier,
    'Is the ''severance'' from historical texts a natural consequence of script change (a Mountain), or is it actively maintained by institutions that benefit from the continuity (a Snare)?',
    'Analysis of educational curricula and state funding for translation/transliteration projects: if active efforts are made to *prevent* access to old texts, it leans Snare; if it''s merely a lack of resources, it leans Mountain.',
    'If found to be actively maintained, the constraint would reclassify from Mountain to Snare or Tangled Rope, with higher extractiveness and suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_vs_constructed_barrier, conceptual, 'Ambiguity between inherent script incompatibility and active institutional maintenance of historical textual barriers.').

omega_variable(
    identity_lock_strength,
    'How strongly are the identities of scholars and religious authorities tied to the traditional script, and would they genuinely lose their ''power'' if the script changed?',
    'Sociological studies of professional identity in linguistic reform contexts, and analysis of how religious authority adapts to translated texts.',
    'If identity-lock is weaker than assumed, their beneficiary status might be less pronounced, potentially shifting the overall classification towards a more neutral coordination (Rope) rather than a Mountain with beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_strength, empirical, 'The degree to which professional and religious identities are genuinely dependent on orthographic continuity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(orthographic_legitimacy_kernel__continuity_reading, 1900, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orth_tr_t1900, orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 1900, 0.0).
narrative_ontology:measurement(orth_tr_t1920, orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 1920, 0.0).
narrative_ontology:measurement(orth_tr_t1940, orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 1940, 0.0).
narrative_ontology:measurement(orth_tr_t1960, orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 1960, 0.0).
narrative_ontology:measurement(orth_tr_t1980, orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 1980, 0.0).
narrative_ontology:measurement(orth_tr_t2000, orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 2000, 0.0).
narrative_ontology:measurement(orth_tr_t2024, orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 2024, 0.0).

% Extraction over time
narrative_ontology:measurement(orth_be_t1900, orthographic_legitimacy_kernel__continuity_reading, base_extractiveness, 1900, 0.1).
narrative_ontology:measurement(orth_be_t1920, orthographic_legitimacy_kernel__continuity_reading, base_extractiveness, 1920, 0.12).
narrative_ontology:measurement(orth_be_t1940, orthographic_legitimacy_kernel__continuity_reading, base_extractiveness, 1940, 0.15).
narrative_ontology:measurement(orth_be_t1960, orthographic_legitimacy_kernel__continuity_reading, base_extractiveness, 1960, 0.15).
narrative_ontology:measurement(orth_be_t1980, orthographic_legitimacy_kernel__continuity_reading, base_extractiveness, 1980, 0.15).
narrative_ontology:measurement(orth_be_t2000, orthographic_legitimacy_kernel__continuity_reading, base_extractiveness, 2000, 0.15).
narrative_ontology:measurement(orth_be_t2024, orthographic_legitimacy_kernel__continuity_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(orth_su_t1900, orthographic_legitimacy_kernel__continuity_reading, suppression_requirement, 1900, 0.05).
narrative_ontology:measurement(orth_su_t1920, orthographic_legitimacy_kernel__continuity_reading, suppression_requirement, 1920, 0.05).
narrative_ontology:measurement(orth_su_t1940, orthographic_legitimacy_kernel__continuity_reading, suppression_requirement, 1940, 0.05).
narrative_ontology:measurement(orth_su_t1960, orthographic_legitimacy_kernel__continuity_reading, suppression_requirement, 1960, 0.05).
narrative_ontology:measurement(orth_su_t1980, orthographic_legitimacy_kernel__continuity_reading, suppression_requirement, 1980, 0.05).
narrative_ontology:measurement(orth_su_t2000, orthographic_legitimacy_kernel__continuity_reading, suppression_requirement, 2000, 0.05).
narrative_ontology:measurement(orth_su_t2024, orthographic_legitimacy_kernel__continuity_reading, suppression_requirement, 2024, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(orthographic_legitimacy_kernel__continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(orthographic_legitimacy_kernel__continuity_reading, orthographic_legitimacy_kernel__modernist_reading).
narrative_ontology:affects_constraint(orthographic_legitimacy_kernel__continuity_reading, orthographic_legitimacy_kernel__instrumentalist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'orthographic_legitimacy_kernel'. This 'continuity_reading' emphasizes the preservation of historical and religious tradition through stable orthography.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
