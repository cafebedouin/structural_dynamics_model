% ============================================================================
% CONSTRAINT STORY: salic_prohibition__cognatic_reversion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_salic_prohibition__cognatic_reversion_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: salic_prohibition__cognatic_reversion_reading
 *   human_readable: Salic Law: Cognatic Reversion Reading (Territorial Integrity)
 *   domain: constitutional_law/dynastic_succession/political_history
 *
 * SUMMARY:
 *   This constraint represents the 'cognatic reversion' reading of Salic Law,
 *   which interprets the law as a Frankish anachronism never properly binding
 *   on non-Frankish territories. It prioritizes territorial integrity and
 *   stable succession through cognatic primogeniture (eldest child regardless
 *   of sex) over strict agnatic purity. This reading emerged as a pragmatic
 *   solution to succession crises in various European monarchies,
 *   particularly when strict Salic Law would lead to foreign claims or
 *   dynastic instability.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(salic_prohibition__cognatic_reversion_reading, 0.4).
domain_priors:suppression_score(salic_prohibition__cognatic_reversion_reading, 0.6).
domain_priors:theater_ratio(salic_prohibition__cognatic_reversion_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(salic_prohibition__cognatic_reversion_reading, tangled_rope).
narrative_ontology:human_readable(salic_prohibition__cognatic_reversion_reading, "Salic Law: Cognatic Reversion Reading (Territorial Integrity)").
narrative_ontology:topic_domain(salic_prohibition__cognatic_reversion_reading, "constitutional_law/dynastic_succession/political_history").

domain_priors:requires_active_enforcement(salic_prohibition__cognatic_reversion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(salic_prohibition__cognatic_reversion_reading, 'c705f4f7-dd7b-4089-ae7a-f7f2a84f0b25').
narrative_ontology:cs_kernel_codification('c705f4f7-dd7b-4089-ae7a-f7f2a84f0b25', fixed_text).
narrative_ontology:cs_authority_grounding('c705f4f7-dd7b-4089-ae7a-f7f2a84f0b25', practice).
narrative_ontology:cs_interpretation_layer_present('c705f4f7-dd7b-4089-ae7a-f7f2a84f0b25').
narrative_ontology:cs_reading_relation('c705f4f7-dd7b-4089-ae7a-f7f2a84f0b25', salic_prohibition__immutable_mandate_reading, forecloses).
narrative_ontology:cs_reading_relation('c705f4f7-dd7b-4089-ae7a-f7f2a84f0b25', salic_prohibition__sovereign_override_reading, coexists_with).
narrative_ontology:cs_axiom('c705f4f7-dd7b-4089-ae7a-f7f2a84f0b25', foundational, salic_law_is_territorially_limited).
narrative_ontology:cs_axiom_status(salic_law_is_territorially_limited, holdable).
narrative_ontology:cs_axiom_grounding('c705f4f7-dd7b-4089-ae7a-f7f2a84f0b25', salic_law_is_territorially_limited, empirically_contingent).
narrative_ontology:cs_axiom('c705f4f7-dd7b-4089-ae7a-f7f2a84f0b25', foundational, territorial_integrity_trumps_agnatic_purity).
narrative_ontology:cs_axiom_status(territorial_integrity_trumps_agnatic_purity, holdable).
narrative_ontology:cs_axiom_grounding('c705f4f7-dd7b-4089-ae7a-f7f2a84f0b25', territorial_integrity_trumps_agnatic_purity, conventional).
narrative_ontology:cs_reference_frame('c705f4f7-dd7b-4089-ae7a-f7f2a84f0b25', historical_cognatic_precedent).
narrative_ontology:cs_drift_state('c705f4f7-dd7b-4089-ae7a-f7f2a84f0b25', enlightenment_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('c705f4f7-dd7b-4089-ae7a-f7f2a84f0b25', '').
narrative_ontology:cs_kernel_id(salic_prohibition__cognatic_reversion_reading, salic_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(salic_prohibition__cognatic_reversion_reading, territorial_nobility).
narrative_ontology:constraint_beneficiary(salic_prohibition__cognatic_reversion_reading, female_claimants_and_their_descendants).
narrative_ontology:constraint_victim(salic_prohibition__cognatic_reversion_reading, agnatic_succession_advocates).
narrative_ontology:constraint_victim(salic_prohibition__cognatic_reversion_reading, dynastic_purity_ideologues).
narrative_ontology:constraint_vindicates(salic_prohibition__cognatic_reversion_reading, territorial_integrity_doctrine).
narrative_ontology:constraint_vindicates(salic_prohibition__cognatic_reversion_reading, cognatic_primogeniture_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the stability of local succession and the avoidance of foreign claims that agnatic-only rules might introduce. Supports female succession to maintain local power structures and prevent fragmentation.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, territorial_nobility, beneficiary,
    organized, generational, constrained, regional).

% Directly benefits from the recognition of their right to inherit, which would be denied under strict Salic Law. Their legitimacy is tied to this interpretation.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, female_claimants_and_their_descendants, beneficiary,
    moderate, generational, constrained, national).

% Bears the cost of losing exclusive claims to succession based on male lineage. They actively resist this interpretation, viewing it as a dilution of dynastic purity and historical precedent.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, agnatic_succession_advocates, payer,
    powerful, generational, constrained, national).

% Their worldview is deeply invested in the principle of unbroken male lineage. They experience the cognatic reversion reading as an existential threat to their understanding of legitimate rule and dynastic identity.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, dynastic_purity_ideologues, payer,
    moderate, civilizational, identity_locked, national).

% Analyze the historical application and legal evolution of Salic Law, providing evidence for its original territorial limitations and the historical precedents for cognatic succession in various regions. Their analysis informs the debate but does not directly enforce outcomes.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, historical_scholars, observer,
    analytical, civilizational, analytical, global).

% Those in power who must navigate succession disputes. They may strategically adopt or enforce the cognatic reversion reading to consolidate power, ensure stability, or prevent foreign intervention, balancing dynastic tradition with political pragmatism.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, ruling_monarchs_or_regents, agenda_setter,
    institutional, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates dynastic succession in territories where strict agnatic Salic Law was historically inapplicable or politically destabilizing, ensuring a clear line of inheritance and preventing fragmentation or foreign claims.
% TRANSFER_FUNCTION: Transfers the right of succession from an exclusively male line to include female heirs, thereby transferring potential power and territorial control to cognatic lines and away from purely agnatic claimants.
% ABSENT_VOICES: The original Frankish legal scholars who codified Salic Law would object, arguing for its universal application based on ancient custom. Their voices are absent due to historical distance and the evolution of legal thought.
% DISAPPEARANCE_RATIONALE: If this reading disappeared, succession in many European monarchies would be thrown into chaos, potentially leading to new dynastic wars, territorial disputes, and a complete reordering of royal houses and national boundaries, as purely agnatic claims would resurface.
% FOUNDING_PROBLEM: The problem of maintaining territorial integrity and stable succession in diverse European kingdoms where the strict Frankish Salic Law was either culturally alien or politically unworkable, leading to frequent succession crises and wars.
% FOUNDING_PROBLEM_CORROBORATION: Historical records of succession crises, dynastic wars, and the political necessity of female rulers in various European states corroborate the ongoing relevance of this problem. Legal scholars and political historians, outside the direct beneficiaries, attest to the historical and ongoing challenges of succession without flexible rules.
narrative_ontology:disappearance_verdict(salic_prohibition__cognatic_reversion_reading, world_rearranges).
narrative_ontology:founding_problem_status(salic_prohibition__cognatic_reversion_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(salic_prohibition__cognatic_reversion_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(salic_prohibition__cognatic_reversion_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(salic_prohibition__cognatic_reversion_reading_tests).
:- end_tests(salic_prohibition__cognatic_reversion_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it serves a genuine coordination function (stable succession) but involves significant extraction from those who adhere to strict agnatic principles. Extractiveness is moderate (0.4) as it reallocates power and legitimacy. Suppression (0.6) is required to overcome resistance from traditionalists and agnatic claimants. Theater ratio is low (0.2) as the arguments for territorial integrity and historical precedent are genuinely invoked, not merely performative. The cyclical nature of the measurements reflects periods of heightened succession crisis (higher extraction/suppression) followed by periods of relative stability.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of territorial nobility and female claimants, this reading is a necessary and just coordination mechanism. From the perspective of agnatic purists, it is an illegitimate subversion of ancient law and dynastic tradition, an act of extraction. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Territorial nobility and female claimants are beneficiaries, as this reading secures their interests. Agnatic succession advocates and dynastic purity ideologues are victims, as their claims are undermined. Ruling monarchs act as agenda-setters, strategically enforcing this reading to maintain power and stability. Historical scholars provide analytical observation without direct stake.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling a pragmatic solution to a genuine coordination problem (stable succession) as pure extraction. While it extracts from agnatic claimants, it simultaneously coordinates territorial stability, which was a live problem. The 'contested' status of the founding problem reflects the ongoing tension between dynastic tradition and political necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_applicability_ambiguity,
    'Was Salic Law ever genuinely intended to apply universally beyond its original Frankish context, or was its application always contingent on local custom and political power?',
    'Further historical and legal scholarship examining the original intent and early reception of Salic Law in non-Frankish territories, focusing on primary sources and contemporary legal interpretations.',
    'If universal intent is disproven, this reading gains stronger historical grounding, reducing its extractiveness from traditionalists. If universal intent is supported, this reading becomes more clearly a political construct, increasing its perceived extractiveness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_applicability_ambiguity, empirical, 'Ambiguity regarding the historical scope and intent of Salic Law.').

omega_variable(
    territorial_integrity_vs_dynastic_purity,
    'Is the prioritization of territorial integrity over strict agnatic dynastic purity a legitimate legal principle, or a political expediency that masks a power grab?',
    'Analysis of long-term political stability and economic prosperity in states that adopted cognatic succession versus those that adhered strictly to agnatic rules, alongside philosophical debate on the sources of monarchical legitimacy.',
    'If territorial integrity is widely accepted as a superior principle, this reading''s coordination function is strengthened. If it''s seen as mere expediency, its extractive nature is amplified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(territorial_integrity_vs_dynastic_purity, preference, 'Conceptual ambiguity regarding the normative priority of territorial integrity versus dynastic purity.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of agnatic claims structural (legal precedent, political power) or internalized (dynastic ideologues eventually accepting the new reality)?',
    'Post-succession analysis of resistance movements and ideological shifts: if agnatic claims persist as active political forces, suppression is structural; if they fade into historical grievance, it''s partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as the target carries the suppression with them after formal defeat. If purely structural, resistance remains a live political force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for agnatic claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(salic_prohibition__cognatic_reversion_reading, 1300, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sali_tr_t1300, salic_prohibition__cognatic_reversion_reading, theater_ratio, 1300, 0.1).
narrative_ontology:measurement(sali_tr_t1400, salic_prohibition__cognatic_reversion_reading, theater_ratio, 1400, 0.15).
narrative_ontology:measurement(sali_tr_t1500, salic_prohibition__cognatic_reversion_reading, theater_ratio, 1500, 0.2).
narrative_ontology:measurement(sali_tr_t1600, salic_prohibition__cognatic_reversion_reading, theater_ratio, 1600, 0.25).
narrative_ontology:measurement(sali_tr_t1700, salic_prohibition__cognatic_reversion_reading, theater_ratio, 1700, 0.2).
narrative_ontology:measurement(sali_tr_t1800, salic_prohibition__cognatic_reversion_reading, theater_ratio, 1800, 0.15).
narrative_ontology:measurement(sali_tr_t1900, salic_prohibition__cognatic_reversion_reading, theater_ratio, 1900, 0.1).

% Extraction over time
narrative_ontology:measurement(sali_be_t1300, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 1300, 0.3).
narrative_ontology:measurement(sali_be_t1400, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 1400, 0.35).
narrative_ontology:measurement(sali_be_t1500, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 1500, 0.4).
narrative_ontology:measurement(sali_be_t1600, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 1600, 0.45).
narrative_ontology:measurement(sali_be_t1700, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 1700, 0.4).
narrative_ontology:measurement(sali_be_t1800, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 1800, 0.35).
narrative_ontology:measurement(sali_be_t1900, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 1900, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(sali_su_t1300, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 1300, 0.5).
narrative_ontology:measurement(sali_su_t1400, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 1400, 0.55).
narrative_ontology:measurement(sali_su_t1500, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 1500, 0.6).
narrative_ontology:measurement(sali_su_t1600, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 1600, 0.65).
narrative_ontology:measurement(sali_su_t1700, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 1700, 0.6).
narrative_ontology:measurement(sali_su_t1800, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 1800, 0.55).
narrative_ontology:measurement(sali_su_t1900, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 1900, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(salic_prohibition__cognatic_reversion_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'salic_prohibition' kernel. It focuses on the historical and political arguments for cognatic succession in non-Frankish territories, contrasting with readings that emphasize immutable law or sovereign legislative power.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
