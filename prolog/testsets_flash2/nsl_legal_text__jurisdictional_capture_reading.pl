% ============================================================================
% CONSTRAINT STORY: nsl_legal_text__jurisdictional_capture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nsl_legal_text__jurisdictional_capture_reading, []).

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
 *   constraint_id: nsl_legal_text__jurisdictional_capture_reading
 *   human_readable: National Security Law as Jurisdictional Capture
 *   domain: constitutional_law/political_sociology/international_relations
 *
 * SUMMARY:
 *   This constraint models the National Security Law (NSL) in Hong Kong as a
 *   mechanism for the transplantation of mainland China's legal system,
 *   leading to the erosion of Hong Kong's common law autonomy. This reading
 *   focuses on the structural changes to judicial independence and the legal
 *   profession, rather than solely on the criminalization of dissent. The NSL
 *   is claimed as a Tangled Rope, reflecting its dual function of
 *   coordinating legal integration while extracting institutional
 *   independence.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nsl_legal_text__jurisdictional_capture_reading, 0.78).
domain_priors:suppression_score(nsl_legal_text__jurisdictional_capture_reading, 0.88).
domain_priors:theater_ratio(nsl_legal_text__jurisdictional_capture_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nsl_legal_text__jurisdictional_capture_reading, tangled_rope).
narrative_ontology:human_readable(nsl_legal_text__jurisdictional_capture_reading, "National Security Law as Jurisdictional Capture").
narrative_ontology:topic_domain(nsl_legal_text__jurisdictional_capture_reading, "constitutional_law/political_sociology/international_relations").

domain_priors:requires_active_enforcement(nsl_legal_text__jurisdictional_capture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nsl_legal_text__jurisdictional_capture_reading, '37f22eb1-ef3d-4350-bbb2-488c346d9a27').
narrative_ontology:cs_kernel_codification('37f22eb1-ef3d-4350-bbb2-488c346d9a27', fixed_text).
narrative_ontology:cs_authority_grounding('37f22eb1-ef3d-4350-bbb2-488c346d9a27', extraction).
narrative_ontology:cs_interpretation_layer_present('37f22eb1-ef3d-4350-bbb2-488c346d9a27').
narrative_ontology:cs_reading_relation('37f22eb1-ef3d-4350-bbb2-488c346d9a27', nsl_legal_text__sovereignty_restoration_reading, coexists_with).
narrative_ontology:cs_reading_relation('37f22eb1-ef3d-4350-bbb2-488c346d9a27', nsl_legal_text__democratic_enclosure_reading, influences).
narrative_ontology:cs_axiom('37f22eb1-ef3d-4350-bbb2-488c346d9a27', foundational, common_law_autonomy_is_sacrosanct).
narrative_ontology:cs_axiom_status(common_law_autonomy_is_sacrosanct, holdable).
narrative_ontology:cs_axiom_grounding('37f22eb1-ef3d-4350-bbb2-488c346d9a27', common_law_autonomy_is_sacrosanct, deontological).
narrative_ontology:cs_axiom('37f22eb1-ef3d-4350-bbb2-488c346d9a27', foundational, mainland_legal_transplantation_is_extractive).
narrative_ontology:cs_axiom_status(mainland_legal_transplantation_is_extractive, holdable).
narrative_ontology:cs_axiom_grounding('37f22eb1-ef3d-4350-bbb2-488c346d9a27', mainland_legal_transplantation_is_extractive, empirically_contingent).
narrative_ontology:cs_reference_frame('37f22eb1-ef3d-4350-bbb2-488c346d9a27', high_autonomy_common_law_framework).
narrative_ontology:cs_drift_state('37f22eb1-ef3d-4350-bbb2-488c346d9a27', post_nsl_implementation, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('37f22eb1-ef3d-4350-bbb2-488c346d9a27', '').
narrative_ontology:cs_kernel_id(nsl_legal_text__jurisdictional_capture_reading, nsl_legal_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nsl_legal_text__jurisdictional_capture_reading, mainland_security_apparatus).
narrative_ontology:constraint_beneficiary(nsl_legal_text__jurisdictional_capture_reading, hong_kong_executive).
narrative_ontology:constraint_victim(nsl_legal_text__jurisdictional_capture_reading, hong_kong_judiciary).
narrative_ontology:constraint_victim(nsl_legal_text__jurisdictional_capture_reading, hong_kong_legal_profession).
narrative_ontology:constraint_victim(nsl_legal_text__jurisdictional_capture_reading, hong_kong_citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Directs the interpretation and enforcement of the NSL, expanding its jurisdiction and operational reach into Hong Kong. Benefits from the erosion of common law autonomy, which facilitates the transplantation of mainland legal norms and control mechanisms. Has full discretion over NSL cases and personnel.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, mainland_security_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Gains enhanced powers to suppress dissent and enforce central government directives, bypassing traditional common law checks. Benefits from increased stability and alignment with mainland policy, but operates under the ultimate authority of the mainland security apparatus.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, hong_kong_executive, beneficiary,
    institutional, biographical, constrained, local).

% Experiences a direct erosion of its common law autonomy and interpretive authority. Judges are increasingly constrained by NSL interpretations from Beijing, and their independence is compromised by executive appointments and the threat of mainland jurisdiction. Exit means abandoning the principles of common law justice.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, hong_kong_judiciary, payer,
    institutional, generational, identity_locked, local).

% Faces a shrinking scope for independent legal practice, increased professional risk, and the imposition of mainland legal concepts. Their expertise in common law is devalued, and their ability to defend clients against NSL charges is severely limited. Exit means leaving Hong Kong or changing professions.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, hong_kong_legal_profession, payer,
    organized, biographical, constrained, local).

% Lose the protections and predictability of the common law system, facing arbitrary detention, secret trials, and the application of mainland legal principles. Their rights are curtailed, and their ability to seek redress through independent courts is diminished. Exit means emigration, which is costly and not available to all.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, hong_kong_citizens, payer,
    powerless, biographical, trapped, local).

% Monitor the erosion of Hong Kong's legal autonomy and issue reports, but lack direct enforcement power. Their observations provide external corroboration of the jurisdictional capture but cannot directly alter the constraint's operation.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, international_legal_bodies, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The NSL coordinates the integration of Hong Kong's legal system with mainland China's, ensuring alignment on national security matters and centralizing interpretive authority.
% TRANSFER_FUNCTION: Transfers legal and judicial autonomy from Hong Kong's common law institutions to mainland security and legal frameworks, along with the associated power and control over legal outcomes.
% ABSENT_VOICES: Independent legal scholars and international human rights advocates, who would argue for the preservation of common law principles and judicial independence, are systematically excluded from the interpretive and enforcement processes of the NSL.
% DISAPPEARANCE_RATIONALE: If the NSL and its enforcement mechanisms vanished, Hong Kong's common law system would immediately reassert its autonomy, the judiciary would regain its interpretive independence, and the legal profession would operate under traditional principles. The mainland security apparatus would lose its direct jurisdictional leverage, leading to a significant rearrangement of legal and political power dynamics.
% FOUNDING_PROBLEM: The NSL was introduced to address perceived threats to national security in Hong Kong, particularly after the 2019 protests, which Beijing viewed as undermining sovereign authority and stability.
% FOUNDING_PROBLEM_CORROBORATION: The mainland security apparatus and the Hong Kong executive attest that the founding problem of national security threats remains live. However, the Hong Kong judiciary, legal profession, and international legal bodies contend that the NSL's scope far exceeds genuine security concerns and is primarily a tool for legal transplantation and control, with independent analysis supporting this view.
narrative_ontology:disappearance_verdict(nsl_legal_text__jurisdictional_capture_reading, world_rearranges).
narrative_ontology:founding_problem_status(nsl_legal_text__jurisdictional_capture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nsl_legal_text__jurisdictional_capture_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(nsl_legal_text__jurisdictional_capture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nsl_legal_text__jurisdictional_capture_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nsl_legal_text__jurisdictional_capture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nsl_legal_text__jurisdictional_capture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nsl_legal_text__jurisdictional_capture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the NSL fundamentally alters the balance of legal power, transferring significant authority from Hong Kong's independent judiciary to mainland-controlled bodies. Suppression is very high (0.88) due to the broad scope of the law, its extraterritorial reach, and the severe penalties for non-compliance, which actively suppress any legal or political challenge. Theater ratio is moderate (0.45) as the Hong Kong legal system maintains a facade of common law operation, but its core autonomy is increasingly undermined by NSL interpretations and enforcement. The rising trend in extractiveness and suppression reflects the ongoing tightening of control since the NSL's implementation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the mainland security apparatus, the NSL is a legitimate exercise of sovereignty and a necessary coordination mechanism for national security. From the perspective of the Hong Kong judiciary and legal profession, it is a coercive instrument that extracts their institutional independence and undermines the rule of law. The engine's classification will highlight this divergence, showing a beneficial outcome for the agenda-setters and an extractive one for the targets.
 *
 * DIRECTIONALITY LOGIC:
 *   The mainland security apparatus and the Hong Kong executive are clear beneficiaries, gaining expanded powers and control (low directionality). The Hong Kong judiciary, legal profession, and citizens are the primary targets, experiencing direct loss of autonomy, professional scope, and rights (high directionality). The judiciary is identity-locked, as its very identity is tied to common law principles now under threat. International legal bodies act as observers, documenting the changes without direct involvement.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extent_of_legal_transplantation,
    'To what extent will mainland legal principles and practices fully replace or merely coexist with common law in Hong Kong under the NSL?',
    'Longitudinal analysis of judicial decisions, legal education curricula, and legislative amendments over the next decade, specifically tracking the adoption of mainland legal concepts and the decline of common law precedents.',
    'If full transplantation occurs, the constraint''s extractiveness and suppression would be reclassified as even higher, approaching a pure Snare, as the common law system would be effectively dismantled. If coexistence prevails, it remains a Tangled Rope with a more complex hybrid legal structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extent_of_legal_transplantation, empirical, 'Ambiguity regarding the ultimate degree of legal system transplantation.').

omega_variable(
    judicial_identity_lock_strength,
    'How deeply is the Hong Kong judiciary''s identity fused with common law principles, and what is the threshold for ''identity_locked'' exit to become ''trapped''?',
    'Qualitative sociological studies of judicial decision-making, public statements, and private sentiments among judges, combined with analysis of resignation rates and early retirements in response to NSL pressures.',
    'If identity fusion is weaker than assumed, judges might adapt more readily, reducing the ''identity_locked'' aspect and shifting their exit options towards ''constrained''. If stronger, the psychological and professional costs of remaining would be higher, reinforcing the ''identity_locked'' classification and potentially leading to a ''trapped'' state for those who cannot reconcile.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(judicial_identity_lock_strength, conceptual, 'Uncertainty about the strength and persistence of judicial identity tied to common law.').

omega_variable(
    framing_of_sovereignty_vs_autonomy,
    'Is the NSL primarily a legitimate exercise of sovereign power to restore order, or a pretext for undermining Hong Kong''s promised autonomy?',
    'This is a preference-based omega. Resolution depends on which normative framework (state sovereignty vs. ''one country, two systems'' autonomy) is prioritized by the observer. No empirical data can fully resolve this conceptual tension.',
    'If framed as legitimate sovereignty restoration (sovereignty_restoration_reading), the constraint might be seen as a Rope or even a Mountain from that perspective. If framed as undermining autonomy (this reading), it is clearly extractive. The classification depends on the chosen normative lens.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framing_of_sovereignty_vs_autonomy, preference, 'Conceptual ambiguity regarding the primary justification for the NSL.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nsl_legal_text__jurisdictional_capture_reading, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nsl__tr_t0, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(nsl__tr_t1, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 1, 0.35).
narrative_ontology:measurement(nsl__tr_t2, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 2, 0.38).
narrative_ontology:measurement(nsl__tr_t3, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 3, 0.4).
narrative_ontology:measurement(nsl__tr_t4, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 4, 0.43).
narrative_ontology:measurement(nsl__tr_t5, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 5, 0.45).

% Extraction over time
narrative_ontology:measurement(nsl__be_t0, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(nsl__be_t1, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 1, 0.7).
narrative_ontology:measurement(nsl__be_t2, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 2, 0.73).
narrative_ontology:measurement(nsl__be_t3, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 3, 0.75).
narrative_ontology:measurement(nsl__be_t4, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 4, 0.77).
narrative_ontology:measurement(nsl__be_t5, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 5, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(nsl__su_t0, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(nsl__su_t1, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 1, 0.8).
narrative_ontology:measurement(nsl__su_t2, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 2, 0.83).
narrative_ontology:measurement(nsl__su_t3, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 3, 0.85).
narrative_ontology:measurement(nsl__su_t4, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 4, 0.87).
narrative_ontology:measurement(nsl__su_t5, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 5, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nsl_legal_text__jurisdictional_capture_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(nsl_legal_text__jurisdictional_capture_reading, nsl_legal_text__democratic_enclosure_reading).
narrative_ontology:affects_constraint(nsl_legal_text__jurisdictional_capture_reading, nsl_legal_text__sovereignty_restoration_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'nsl_legal_text' kernel. This 'jurisdictional_capture_reading' focuses on the erosion of common law autonomy and legal transplantation, distinct from the 'democratic_enclosure_reading' (criminalization of dissent) and the 'sovereignty_restoration_reading' (legitimate security instrument).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
