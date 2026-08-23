% ============================================================================
% CONSTRAINT STORY: gdpr_article_3_scope__effects_jurisdiction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gdpr_article_3_scope__effects_jurisdiction_reading, []).

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
 *   constraint_id: gdpr_article_3_scope__effects_jurisdiction_reading
 *   human_readable: GDPR Article 3(2) Effects Jurisdiction — Extraterritorial Protection via Targeting/Monitoring Test
 *   domain: technology_governance/international_law/privacy_regulation
 *
 * SUMMARY:
 *   GDPR Article 3(2) asserts jurisdiction over non-EU controllers who target
 *   EU residents (offering goods/services) or monitor their behavior within
 *   the EU. This effects jurisdiction reading treats extraterritorial reach
 *   as a legitimate jurisdictional claim grounded in the effects on EU data
 *   subjects' fundamental rights. The constraint operates through active
 *   enforcement: supervisory authorities impose fines, the EDPB coordinates
 *   cross-border enforcement, adequacy decisions condition data flows, and
 *   the CJEU authoritatively interprets the targeting/monitoring test. Non-EU
 *   controllers bear high compliance costs; EU data subjects receive
 *   protection that follows their data. The coordination function (harmonized
 *   standard preventing regulatory arbitrage) coexists with asymmetric
 *   extraction (disproportionate burden on non-EU controllers, especially
 *   smaller ones).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gdpr_article_3_scope__effects_jurisdiction_reading, 0.72).
domain_priors:suppression_score(gdpr_article_3_scope__effects_jurisdiction_reading, 0.78).
domain_priors:theater_ratio(gdpr_article_3_scope__effects_jurisdiction_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gdpr_article_3_scope__effects_jurisdiction_reading, tangled_rope).
narrative_ontology:human_readable(gdpr_article_3_scope__effects_jurisdiction_reading, "GDPR Article 3(2) Effects Jurisdiction — Extraterritorial Protection via Targeting/Monitoring Test").
narrative_ontology:topic_domain(gdpr_article_3_scope__effects_jurisdiction_reading, "technology_governance/international_law/privacy_regulation").

domain_priors:requires_active_enforcement(gdpr_article_3_scope__effects_jurisdiction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gdpr_article_3_scope__effects_jurisdiction_reading, '30d496ab-67f0-47fa-a1d5-c421a470bd8f').
narrative_ontology:cs_kernel_codification('30d496ab-67f0-47fa-a1d5-c421a470bd8f', formalized).
narrative_ontology:cs_authority_grounding('30d496ab-67f0-47fa-a1d5-c421a470bd8f', lineage).
narrative_ontology:cs_interpretation_layer_present('30d496ab-67f0-47fa-a1d5-c421a470bd8f').
narrative_ontology:cs_reading_relation('30d496ab-67f0-47fa-a1d5-c421a470bd8f', gdpr_article_3_scope__territorial_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('30d496ab-67f0-47fa-a1d5-c421a470bd8f', gdpr_article_3_scope__market_access_reading, influences).
narrative_ontology:cs_axiom('30d496ab-67f0-47fa-a1d5-c421a470bd8f', foundational, effects_basis_legitimate_jurisdiction).
narrative_ontology:cs_axiom_status(effects_basis_legitimate_jurisdiction, holdable).
narrative_ontology:cs_axiom_grounding('30d496ab-67f0-47fa-a1d5-c421a470bd8f', effects_basis_legitimate_jurisdiction, conventional).
narrative_ontology:cs_axiom('30d496ab-67f0-47fa-a1d5-c421a470bd8f', secondary, fundamental_rights_follow_data_subject).
narrative_ontology:cs_axiom_status(fundamental_rights_follow_data_subject, holdable).
narrative_ontology:cs_axiom_grounding('30d496ab-67f0-47fa-a1d5-c421a470bd8f', fundamental_rights_follow_data_subject, deontological).
narrative_ontology:cs_reference_frame('30d496ab-67f0-47fa-a1d5-c421a470bd8f', effects_jurisdiction_principle).
narrative_ontology:cs_drift_state('30d496ab-67f0-47fa-a1d5-c421a470bd8f', post_schrems_ii, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('30d496ab-67f0-47fa-a1d5-c421a470bd8f', '').
narrative_ontology:cs_kernel_id(gdpr_article_3_scope__effects_jurisdiction_reading, gdpr_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__effects_jurisdiction_reading, eu_data_subjects).
narrative_ontology:constraint_victim(gdpr_article_3_scope__effects_jurisdiction_reading, non_eu_controllers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__effects_jurisdiction_reading, non_eu_controllers).
narrative_ontology:constraint_vindicates(gdpr_article_3_scope__effects_jurisdiction_reading, eu_data_protection_fundamental_right).
narrative_ontology:constraint_vindicates(gdpr_article_3_scope__effects_jurisdiction_reading, extraterritorial_effects_jurisdiction_principle).
narrative_ontology:constraint_vindicates(gdpr_article_3_scope__effects_jurisdiction_reading, targeting_monitoring_test_legitimate).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enforce GDPR against controllers worldwide through fines (up to 4% global turnover), adequacy decisions, and coordination via the European Data Protection Board. They interpret the targeting/monitoring test, issue guidelines, and pursue cross-border cases. Their authority derives from EU law but reaches non-EU entities through the effects jurisdiction claim.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, eu_supervisory_authorities, agenda_setter,
    institutional, generational, analytical, universal).

% Receive data protection rights (access, erasure, portability, objection) regardless of where the controller is established. Their protection follows them extraterritorially when a non-EU controller targets EU markets or monitors EU behavior. They can lodge complaints with any EU supervisory authority, but practical enforcement against distant controllers depends on cooperation mechanisms.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, eu_data_subjects, beneficiary,
    organized, generational, constrained, continental).

% Bear substantial compliance costs: appointing EU representatives, conducting DPIAs, implementing technical measures, responding to data subject requests, and facing liability for violations. Large platforms (US tech firms) absorb this as cost of EU market access; smaller firms may exit EU markets entirely. They benefit from legal certainty and unified standard but the cost asymmetry is pronounced. Exit means foregoing the EU market or restructuring to avoid targeting/monitoring triggers.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, non_eu_controllers, payer,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(gdpr_article_3_scope__effects_jurisdiction_reading, non_eu_controllers, beneficiary).

% Object to extraterritorial application of EU law as infringing sovereignty (e.g., US CLOUD Act tensions, blocking statutes). They would argue for territorial jurisdiction limits and mutual legal assistance treaties instead of unilateral effects jurisdiction. Their voice is absent from the EDPB/CJEU interpretive process but they shape the enforcement environment through adequacy negotiations and trade pressure.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, non_eu_governments, excluded,
    institutional, generational, analytical, national).

% Assess whether non-EU legal frameworks provide essentially equivalent protection (adequacy decisions). Their decisions determine whether data can flow freely or require additional safeguards. They operate within the effects jurisdiction framework but their assessments create de facto standards that non-EU states adopt to maintain data flows — a structural influence channel.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, adequacy_decision_bodies, observer,
    institutional, generational, analytical, global).

% Authoritatively interprets Article 3(2) targeting/monitoring test (e.g., Google Spain, Weltimmo, Schrems II). Its rulings define the extraterritorial reach and bind all EU supervisory authorities. It does not directly collect extraction but its interpretations determine the constraint's operational scope and enforcement intensity.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, eu_court_of_justice, agenda_setter,
    institutional, generational, analytical, universal).
narrative_ontology:stakeholder_secondary_role(gdpr_article_3_scope__effects_jurisdiction_reading, eu_court_of_justice, observer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, harmonized data protection standard that follows EU residents' data wherever it is processed, eliminating regulatory arbitrage where controllers locate in weak jurisdictions to evade protection obligations.
% TRANSFER_FUNCTION: Transfers compliance burden and operational costs from EU data subjects (who would otherwise bear privacy harms) to non-EU controllers who target or monitor EU residents. The transfer is mediated by enforcement: fines, adequacy conditions, and the threat of market exclusion move resources toward the EU data protection regime.
% ABSENT_VOICES: Non-EU governments and their regulators who view effects jurisdiction as sovereignty infringement; non-EU controllers below the radar of enforcement (small firms, non-commercial actors) who bear disproportionate compliance costs without representation in EDPB guideline formation; data subjects in non-EU jurisdictions whose data gets caught in adequacy negotiations but who have no standing in EU process.
% DISAPPEARANCE_RATIONALE: If effects jurisdiction vanished overnight, non-EU controllers would no longer face GDPR obligations for processing EU resident data unless they had EU establishments. Global data flows would reorganize around territorial jurisdiction; adequacy mechanisms would collapse; EU data subjects would lose protection for cross-border processing; non-EU states would assert exclusive jurisdiction over controllers on their territory. The Brussels Effect standard-setting dynamic would dissolve.
% FOUNDING_PROBLEM: Pre-GDPR directive allowed controllers to evade EU data protection by processing EU resident data outside the EU, creating a regulatory race to the bottom and leaving EU residents unprotected against foreign surveillance and commercial exploitation.
% FOUNDING_PROBLEM_CORROBORATION: CJEU Schrems II (2020) confirmed the problem persists: US surveillance law still permits access to EU data transferred to US controllers. EDPB annual reports document ongoing cross-border enforcement gaps. Academic commentary (Kuner, Bygrave, Greenleaf) outside EU institutions corroborates that the founding problem — extraterritorial data processing evading protection — remains live despite GDPR.
narrative_ontology:disappearance_verdict(gdpr_article_3_scope__effects_jurisdiction_reading, world_rearranges).
narrative_ontology:founding_problem_status(gdpr_article_3_scope__effects_jurisdiction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gdpr_article_3_scope__effects_jurisdiction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gdpr_article_3_scope__effects_jurisdiction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gdpr_article_3_scope__effects_jurisdiction_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gdpr_article_3_scope__effects_jurisdiction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gdpr_article_3_scope__effects_jurisdiction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gdpr_article_3_scope__effects_jurisdiction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) reflects the substantial compliance burden transferred to non-EU controllers — DPIAs, EU representatives, technical measures, breach notification, data subject request handling — which scales with the controller's EU-facing activity but is decoupled from marginal cost of protection. Suppression (0.78) is high because the constraint's persistence depends on active enforcement machinery (fines up to 4% global turnover, adequacy withdrawal, CJEU invalidation of transfer mechanisms) that suppresses exit alternatives. Theater ratio (0.38) is moderate: the protection function is real but a growing share of enforcement activity targets procedural compliance (record-keeping, DPIA formalities) rather than substantive privacy outcomes. Accessibility collapse (0.58) reflects that alternatives exist (adequacy, SCCs, BCRs, not targeting EU) but each carries significant cost or uncertainty. Resistance (0.65) captures sustained pushback: US-EU trade tensions, Schrems litigation, blocking statute threats, and controller non-compliance.
 *
 * PERSPECTIVAL GAP:
 *   From the EU supervisory authority seat, the constraint is genuine coordination: a unified standard solving the regulatory arbitrage problem. From the non-EU controller seat (especially mid-size firms), the same structure operates as enforced extraction with no meaningful negotiation. The engine computes this divergence from the declared roles, power levels, and exit options — the claimed_type (tangled_rope) acknowledges both coordination and extraction are structurally present.
 *
 * DIRECTIONALITY LOGIC:
 *   EU supervisory authorities and CJEU are structural beneficiaries (d near 0.0) — they gain authority, resources, and global standard-setting power. EU data subjects are beneficiaries (d ~ 0.2) — they receive protection but face practical enforcement gaps. Non-EU controllers are targets (d ~ 0.85) — they bear costs with constrained exit (market access vs. compliance). Non-EU governments are excluded (no directionality in the engine's sense — they are outside the constraint's operational scope but structurally opposed). Adequacy bodies sit near symmetric (d ~ 0.5) — they exercise authority but within the constraint's framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (regulatory arbitrage evading EU data protection) remains live per Schrems II and ongoing cross-border enforcement gaps. However, the constraint has accumulated extraction layers: fines as revenue, adequacy as leverage, procedural compliance industry. The mandatrophy risk is that the coordination function (protecting EU residents) becomes a cover for institutional empire-building (global standard-setting authority, fine revenue, regulatory reach). The founding_problem_status = live and disappearance_verdict = world_rearranges together flag that the arrangement still solves a real problem but has grown extractive appendages.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_kernel_reading_identity,
    'This constraint is one reading (effects_jurisdiction_reading) of the contested kernel gdpr_article_3_scope. How does the kernel''s multiplicity affect the classification of this specific reading?',
    'Decompose the kernel into its sibling readings (territorial_sovereignty_reading, market_access_reading) as separate constraint stories. Compare their ε values, beneficiary/victim structures, and computed types. The kernel''s contestation is irreducible to a single story.',
    'If the sibling readings compute to different types (e.g., market_access_reading as rope, territorial_sovereignty_reading as mountain), the kernel itself has no single classification — only the readings do. This validates the ε-invariance principle: one kernel, multiple constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_kernel_reading_identity, conceptual, 'Kernel-reading decomposition identity: this story is one reading of a contested kernel, not the kernel itself.').

omega_variable(
    effects_vs_territorial_jurisdiction_ambiguity,
    'Is the effects jurisdiction claim a genuine extension of protective jurisdiction (recognized in international law) or an unprecedented assertion of legislative jurisdiction that violates sovereign equality?',
    'Track state practice and opinio juris: do non-EU states accept effects jurisdiction for data protection (acquiesce via adequacy, mirror laws) or reject it (blocking statutes, diplomatic protests)? The trajectory of acceptance/rejection resolves the ambiguity.',
    'If accepted as protective jurisdiction, the constraint''s coordination function strengthens (rope-ward). If rejected as legislative overreach, the extraction/suppression character dominates (snare-ward). The current ambiguous state sustains the tangled_rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(effects_vs_territorial_jurisdiction_ambiguity, empirical, 'Whether effects jurisdiction is accepted international law or sovereign overreach.').

omega_variable(
    brussels_effect_vs_jurisdictional_assertion,
    'Does the extraterritorial reach operate primarily as jurisdictional assertion (binding law) or as market-access conditioning (de facto standard-setting through the Brussels Effect)?',
    'Measure compliance behavior: do non-EU controllers comply because they accept EU jurisdiction''s legitimacy, or because EU market access is economically indispensable? Survey controller motivation; track adequacy adoption vs. genuine legal internalization.',
    'If market-access conditioning dominates, the constraint''s extraction is economic coercion rather than legal obligation — shifting classification toward snare. If jurisdictional acceptance dominates, the coordination function is genuine — sustaining tangled_rope or rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(brussels_effect_vs_jurisdictional_assertion, conceptual, 'Whether compliance reflects legal legitimacy or economic coercion.').

omega_variable(
    enforcement_effectiveness_vs_paper_tiger,
    'Is the suppression metric (0.78) reflective of actual enforcement capacity against non-EU controllers, or does it measure paper powers (fines on paper, uncollectible judgments)?',
    'Track collection rates on GDPR fines against non-EU controllers, adequacy withdrawal frequency, and CJEU ruling implementation. Compare to domestic enforcement effectiveness.',
    'If enforcement is largely ineffective (paper tiger), the constraint''s suppression is performative — theater_ratio understates the gap. The true extraction may be lower (controllers ignore it) or higher (compliance theater costs without protection benefit). Reclassification toward piton or snare depending on which side captures the gap.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_effectiveness_vs_paper_tiger, empirical, 'Whether active enforcement machinery delivers real suppression or performative threat.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gdpr_article_3_scope__effects_jurisdiction_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gdpr_art3_effects_tr_t0, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(gdpr_art3_effects_tr_t1, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 1, 0.28).
narrative_ontology:measurement(gdpr_art3_effects_tr_t2, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 2, 0.3).
narrative_ontology:measurement(gdpr_art3_effects_tr_t3, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 3, 0.33).
narrative_ontology:measurement(gdpr_art3_effects_tr_t4, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 4, 0.35).
narrative_ontology:measurement(gdpr_art3_effects_tr_t5, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 5, 0.37).
narrative_ontology:measurement(gdpr_art3_effects_tr_t6, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 6, 0.38).

% Extraction over time
narrative_ontology:measurement(gdpr_art3_effects_be_t0, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(gdpr_art3_effects_be_t1, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 1, 0.6).
narrative_ontology:measurement(gdpr_art3_effects_be_t2, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 2, 0.65).
narrative_ontology:measurement(gdpr_art3_effects_be_t3, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 3, 0.68).
narrative_ontology:measurement(gdpr_art3_effects_be_t4, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 4, 0.7).
narrative_ontology:measurement(gdpr_art3_effects_be_t5, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 5, 0.71).
narrative_ontology:measurement(gdpr_art3_effects_be_t6, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 6, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(gdpr_art3_effects_su_t0, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(gdpr_art3_effects_su_t1, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 1, 0.68).
narrative_ontology:measurement(gdpr_art3_effects_su_t2, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 2, 0.7).
narrative_ontology:measurement(gdpr_art3_effects_su_t3, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 3, 0.73).
narrative_ontology:measurement(gdpr_art3_effects_su_t4, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 4, 0.75).
narrative_ontology:measurement(gdpr_art3_effects_su_t5, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 5, 0.77).
narrative_ontology:measurement(gdpr_art3_effects_su_t6, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 6, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gdpr_article_3_scope__effects_jurisdiction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(gdpr_article_3_scope__effects_jurisdiction_reading, 0.1).
narrative_ontology:affects_constraint(gdpr_article_3_scope__effects_jurisdiction_reading, gdpr_chapter_v_transfers).
narrative_ontology:affects_constraint(gdpr_article_3_scope__effects_jurisdiction_reading, gdpr_article_27_representative).
narrative_ontology:affects_constraint(gdpr_article_3_scope__effects_jurisdiction_reading, eu_us_data_privacy_framework).
narrative_ontology:affects_constraint(gdpr_article_3_scope__effects_jurisdiction_reading, schrems_ii_transfer_mechanisms).

% DUAL FORMULATION NOTE:
% This constraint (effects_jurisdiction_reading) is one of three readings of the gdpr_article_3_scope kernel. The territorial_sovereignty_reading treats jurisdiction as territorially bounded (mountain candidate). The market_access_reading treats extraterritoriality as conditional market access/Brussels Effect (rope candidate). This reading treats it as legitimate effects jurisdiction with active enforcement (tangled_rope). The three readings have different ε values, different victim/beneficiary structures, and different computed types — they are distinct constraints linked by kernel membership.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gdpr_article_3_scope__effects_jurisdiction_reading, powerful, 0.82).
constraint_indexing:directionality_override(gdpr_article_3_scope__effects_jurisdiction_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
