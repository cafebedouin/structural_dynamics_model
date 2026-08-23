% ============================================================================
% CONSTRAINT STORY: common_law_precedent_corpus__strict_stare_decisis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_law_precedent_corpus__strict_stare_decisis, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: common_law_precedent_corpus__strict_stare_decisis
 *   human_readable: Strict Stare Decisis: Precedent as Backward-Binding Constraint
 *   domain: legal/constitutional
 *
 * SUMMARY:
 *   This constraint story captures the strict_stare_decisis reading of the
 *   common law precedent kernel: the view that precedent binds as a backward
 *   constraint and departure requires extraordinary justification. The
 *   reading instantiates a constraint with high rigidity — overruling is
 *   rare, contested, and treated as exceptional. The judiciary both
 *   administers and is constrained by the accumulated corpus; litigants face
 *   narrow pathways for challenge; established interests benefit from the
 *   suppression of doctrinal evolution. The claimed_type is tangled_rope
 *   because the constraint has a genuine coordination function (stability,
 *   predictability) AND asymmetric extraction (past decisions bind present
 *   parties who had no voice, entrenching outdated norms). The engine will
 *   compute per-seat classifications from the structural data.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_law_precedent_corpus__strict_stare_decisis, 0.65).
domain_priors:suppression_score(common_law_precedent_corpus__strict_stare_decisis, 0.75).
domain_priors:theater_ratio(common_law_precedent_corpus__strict_stare_decisis, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, extractiveness, 0.65).
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_law_precedent_corpus__strict_stare_decisis, tangled_rope).
narrative_ontology:human_readable(common_law_precedent_corpus__strict_stare_decisis, "Strict Stare Decisis: Precedent as Backward-Binding Constraint").
narrative_ontology:topic_domain(common_law_precedent_corpus__strict_stare_decisis, "legal/constitutional").

domain_priors:requires_active_enforcement(common_law_precedent_corpus__strict_stare_decisis).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_law_precedent_corpus__strict_stare_decisis, '70488f2d-d426-43c2-a4a8-ad4a6405ebab').
narrative_ontology:cs_kernel_codification('70488f2d-d426-43c2-a4a8-ad4a6405ebab', formalized).
narrative_ontology:cs_authority_grounding('70488f2d-d426-43c2-a4a8-ad4a6405ebab', lineage).
narrative_ontology:cs_interpretation_layer_present('70488f2d-d426-43c2-a4a8-ad4a6405ebab').
narrative_ontology:cs_reading_relation('70488f2d-d426-43c2-a4a8-ad4a6405ebab', common_law_precedent_corpus__evolutionary_framework, coexists_with).
narrative_ontology:cs_reading_relation('70488f2d-d426-43c2-a4a8-ad4a6405ebab', common_law_precedent_corpus__pluralist_balancing, coexists_with).
narrative_ontology:cs_axiom('70488f2d-d426-43c2-a4a8-ad4a6405ebab', foundational, precedent_creates_binding_obligation).
narrative_ontology:cs_axiom_status(precedent_creates_binding_obligation, holdable).
narrative_ontology:cs_axiom_grounding('70488f2d-d426-43c2-a4a8-ad4a6405ebab', precedent_creates_binding_obligation, conventional).
narrative_ontology:cs_axiom('70488f2d-d426-43c2-a4a8-ad4a6405ebab', foundational, overruling_requires_extraordinary_justification).
narrative_ontology:cs_axiom_status(overruling_requires_extraordinary_justification, holdable).
narrative_ontology:cs_axiom_grounding('70488f2d-d426-43c2-a4a8-ad4a6405ebab', overruling_requires_extraordinary_justification, conventional).
narrative_ontology:cs_reference_frame('70488f2d-d426-43c2-a4a8-ad4a6405ebab', classical_stare_decisis_framework).
narrative_ontology:cs_drift_state('70488f2d-d426-43c2-a4a8-ad4a6405ebab', contemporary_living_constitutionalism_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('70488f2d-d426-43c2-a4a8-ad4a6405ebab', '').
narrative_ontology:cs_kernel_id(common_law_precedent_corpus__strict_stare_decisis, common_law_precedent_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__strict_stare_decisis, established_legal_interests).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__strict_stare_decisis, institutional_stability_beneficiaries).
narrative_ontology:constraint_victim(common_law_precedent_corpus__strict_stare_decisis, litigants_seeking_norm_change).
narrative_ontology:constraint_victim(common_law_precedent_corpus__strict_stare_decisis, future_generations_bound_by_past).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(common_law_precedent_corpus__strict_stare_decisis, judiciary).
narrative_ontology:constraint_vindicates(common_law_precedent_corpus__strict_stare_decisis, legal_certainty_doctrine).
narrative_ontology:constraint_vindicates(common_law_precedent_corpus__strict_stare_decisis, rule_of_law_as_stability).
narrative_ontology:constraint_vindicates(common_law_precedent_corpus__strict_stare_decisis, judicial_restraint_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers and enforces precedent through the court hierarchy; simultaneously constrained by the accumulated weight of prior decisions that limit doctrinal innovation. Judicial identity and legitimacy are fused with the practice of following precedent — departure is experienced as institutional failure rather than policy choice. Exit from the constraint would require abandoning the professional self-concept of 'a judge who follows the law.'
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, judiciary, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(common_law_precedent_corpus__strict_stare_decisis, judiciary, payer).

% Bear the cost of precedent that blocks their claims; must persuade courts to distinguish or overrule binding authority — a high bar requiring extraordinary justification. Their pathway is narrow: they can litigate within the constraint (distinguishing), seek en banc or supreme review (rare), or pursue legislative override (slow, uncertain). The constraint extracts their normative agency by making past decisions binding on present disputes.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, litigants_seeking_norm_change, payer,
    moderate, biographical, constrained, national).

% Entities (corporations, government agencies, institutional litigants) whose settled expectations and accumulated advantages rest on existing precedent. They benefit from the constraint's suppression of doctrinal change — their reliance interests are protected by the high bar for overruling. They can navigate the system expertly, forum-shop, and deploy resources to defend favorable precedent; their exit options include lobbying for legislative codification of favorable rules.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, established_legal_interests, beneficiary,
    powerful, generational, arbitrage, national).

% Actors who value systemic predictability over any particular doctrinal outcome — bar associations, lower courts needing clear guidance, commercial parties relying on stable rules. They benefit from the coordination function (predictability, reduced litigation costs) without necessarily capturing the extraction. Their exit is mobile: they could operate under a different stability regime but prefer the current one.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, institutional_stability_beneficiaries, beneficiary,
    organized, generational, mobile, national).

% Those who will live under precedents they had no voice in creating and no mechanism to challenge until they become litigants. They are structurally excluded from the precedent-formation process (which occurs in past litigation) and from the overruling process (which requires standing and extraordinary justification). The constraint binds them retroactively through the accumulation of holdings they never consented to.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, future_generations_bound_by_past, excluded,
    powerless, generational, trapped, national).

% Analyze, critique, and theorize the constraint from outside its operational machinery. They do not bear its costs directly nor collect its rents, but their discourse shapes the legitimacy conditions under which the judiciary operates. Some advocate strict adherence; others argue for evolutionary or pluralist readings. Their exit is analytical — they can adopt any reading without material consequence.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, legal_scholars, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides legal stability and predictability by binding courts to prior decisions, allowing reliance interests to settle and reducing the cognitive and transaction costs of continual re-litigation of settled questions.
% TRANSFER_FUNCTION: Moves normative authority from present democratic and evolutionary processes to past judicial decisions, constraining current litigants and judges by the accumulated weight of historical holdings. The transfer runs from the living (who might adapt norms) to the dead (whose decisions govern).
% ABSENT_VOICES: Future generations who will live under precedents they had no voice in creating; historically marginalized groups whose rights were not recognized in the precedent corpus; constitutional amendment advocates who see judicial precedent as blocking democratic change. These voices are structurally excluded because precedent formation occurs in past litigation and overruling requires standing + extraordinary justification.
% DISAPPEARANCE_RATIONALE: If strict stare decisis vanished overnight, courts would freely overrule precedent, legal stability would decrease but adaptive capacity would increase, reliance interests would be disrupted, and the judiciary's role would shift toward continuous normative updating rather than incremental development. The common law system would reorganize around a different coordination mechanism.
% FOUNDING_PROBLEM: The problem of legal instability and unpredictability in early common law systems where each court decided anew without binding authority, making law uncertain and litigation a gamble.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians outside the beneficiary set (e.g., non-originalist scholars like Morton Horwitz, Gerald Rosenberg) attest the founding problem was real but substantially solved by the late 19th century; originalist and formalist scholars (e.g., Antonin Scalia's judicial writings, contemporary originalist academics) attest the problem remains live because any relaxation of binding precedent reintroduces instability. The corroboration is split across interpretive traditions.
narrative_ontology:disappearance_verdict(common_law_precedent_corpus__strict_stare_decisis, world_rearranges).
narrative_ontology:founding_problem_status(common_law_precedent_corpus__strict_stare_decisis, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_law_precedent_corpus__strict_stare_decisis, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(common_law_precedent_corpus__strict_stare_decisis, 'none', 1).
narrative_ontology:epsilon_provenance(common_law_precedent_corpus__strict_stare_decisis, 0.65, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_law_precedent_corpus__strict_stare_decisis_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(common_law_precedent_corpus__strict_stare_decisis, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(common_law_precedent_corpus__strict_stare_decisis_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) reflects the transfer of normative authority from living actors to past decisions — litigants and judges lose agency. Suppression (0.75) is high because the constraint actively prevents alternative normative pathways (overruling is extraordinary, not routine). Theater_ratio (0.3) captures performative adherence: courts distinguish precedent rather than overrule, maintaining the fiction of continuity while achieving functional change. Accessibility_collapse (0.8) is high because once a precedent is accepted as binding, alternatives (legislative override, constitutional amendment, waiting for court composition change) are practically collapsed for most litigants. Resistance (0.4) is moderate: litigants resist through distinguishing, scholars through critique, but the constraint's institutional embedding limits effective resistance.
 *
 * PERSPECTIVAL GAP:
 *   The agenda_setter seat (judiciary) experiences the constraint as both coordination infrastructure they maintain AND extraction they suffer (identity-locked). The payer seats (litigants, future generations) experience it as pure extraction with suppressed alternatives. The beneficiary seats experience it as coordination they value. The engine computes this divergence from the structural data — the strict reading's claim that precedent 'just is' binding law masks the extraction from the constrained seats.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary is identity_locked — their professional self-concept fuses with precedent-following, making exit unthinkable (d near 1.0 as target of their own constraint, but also agenda_setter). Litigants seeking change are constrained (d ~0.7-0.8) — they pay the extraction with limited exit. Established interests are arbitrage-grade beneficiaries (d ~0.1-0.2) — they capture stability rents and can exit to legislative codification. Future generations are trapped (d ~1.0) — no voice, no exit. Legal scholars are analytical (d=0.5) — they observe without material stake.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (legal instability) was substantially solved by the late 19th century, but the constraint persists at heightened rigidity. The constraint prevents mislabeling coordination as pure extraction — stability IS a genuine coordination good — but the extraction component has accumulated as the precedent corpus grew and overruling standards hardened. The mandatrophy is unresolved: the coordination function remains live, but the extraction-to-coordination ratio has drifted upward.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    precedent_binding_natural_vs_constructed,
    'Is the binding force of precedent a natural feature of legal reasoning (a Mountain of jurisprudence) or a constructed practice that serves identifiable beneficiaries?',
    'Cross-system comparison: if legal systems without stare decisis (civil law) achieve comparable stability through other mechanisms, the binding force is constructed, not necessary. Historical analysis of when ''binding precedent'' became doctrinal (19th century UK/US) vs. earlier practice.',
    'If constructed, the constraint is a false summit candidate (Mountain claim with beneficiaries) — FSM would reclassify to tangled_rope. If natural, the high accessibility_collapse and low resistance are warranted and the classification reflects jurisprudential reality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(precedent_binding_natural_vs_constructed, conceptual, 'Whether stare decisis is a natural law of legal systems or a historical contingency with beneficiaries').

omega_variable(
    coordination_extraction_boundary_in_precedent,
    'How much of the constraint''s operation is genuine coordination (stability for all) vs. asymmetric extraction (entrenchment for established interests)?',
    'Empirical study of overruling rates vs. reliance interest protection: if overruling is denied primarily when established interests would lose, extraction dominates. Comparative analysis of doctrinal areas with high vs. low reliance interests.',
    'If extraction dominates, the tangled_rope classification is confirmed and the beneficiary/victim structure is validated. If coordination dominates, the constraint may be closer to rope with incidental extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary_in_precedent, empirical, 'Whether the constraint''s coordination function is genuine or cover for extraction').

omega_variable(
    judicial_identity_lock_mechanism,
    'Is the judiciary''s identity_lock to precedent structural (institutional role requirements) or internalized (professional socialization making departure unthinkable)?',
    'Study judicial behavior when institutional constraints relax (e.g., supreme courts with no higher review, constitutional courts with explicit overruling power). If departure rates increase, the lock is partly structural; if they remain low, internalized.',
    'If internalized, the constraint''s effective suppression is higher than structural measures suggest — judges carry the constraint internally. This affects the omega on suppression mechanism and the computed directionality for the judiciary seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_identity_lock_mechanism, empirical, 'Structural vs. internalized identity lock for the judiciary').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_law_precedent_corpus__strict_stare_decisis, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 0, 0.15).
narrative_ontology:measurement(comm_tr_t10, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 10, 0.18).
narrative_ontology:measurement(comm_tr_t20, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 20, 0.22).
narrative_ontology:measurement(comm_tr_t30, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 30, 0.26).
narrative_ontology:measurement(comm_tr_t40, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 40, 0.28).
narrative_ontology:measurement(comm_tr_t50, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 50, 0.3).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(comm_be_t10, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(comm_be_t20, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(comm_be_t30, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(comm_be_t40, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 40, 0.64).
narrative_ontology:measurement(comm_be_t50, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 50, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t0, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(comm_su_t10, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(comm_su_t20, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(comm_su_t30, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(comm_su_t40, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 40, 0.74).
narrative_ontology:measurement(comm_su_t50, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 50, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_law_precedent_corpus__strict_stare_decisis, identity_coordination).
narrative_ontology:boltzmann_floor_override(common_law_precedent_corpus__strict_stare_decisis, 0.08).
narrative_ontology:affects_constraint(common_law_precedent_corpus__strict_stare_decisis, constitutional_interpretation_methodology).
narrative_ontology:affects_constraint(common_law_precedent_corpus__strict_stare_decisis, legislative_supremacy_doctrine).
narrative_ontology:affects_constraint(common_law_precedent_corpus__strict_stare_decisis, judicial_appointment_politics).

% DUAL FORMULATION NOTE:
% This constraint (strict_stare_decisis) and its siblings (evolutionary_framework, pluralist_balancing) form a constraint family decomposing the 'common law precedent' label. Each has distinct ε: strict reading has high ε (constrains evolution); evolutionary reading has lower ε (permits adaptation); pluralist reading has context-variable ε. The strict reading influences the others by setting the default rigidity against which they define themselves.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(common_law_precedent_corpus__strict_stare_decisis, institutional, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
