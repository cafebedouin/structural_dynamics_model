% ============================================================================
% CONSTRAINT STORY: nsl_legal_text__jurisdictional_capture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
 *   constraint_id: nsl_legal_text__jurisdictional_capture_reading
 *   human_readable: NSL as Vehicle for Mainland Legal System Transplantation, Eroding Common Law Autonomy
 *   domain: constitutional_law/political_sociology/international_relations
 *
 * SUMMARY:
 *   The National Security Law (NSL), promulgated by the NPCSC on 30 June
 *   2020, is read here as a vehicle for transplanting mainland Chinese legal
 *   concepts, procedures, and institutional logics into Hong Kong's common
 *   law system. This reading focuses on jurisdictional capture: the NSL's
 *   Article 55 (central government jurisdiction over 'complex' or 'serious'
 *   cases), Article 63 (Office for Safeguarding National Security
 *   operations), and NPCSC interpretation powers (Article 65) structurally
 *   reposition HK's judiciary from an independent common law apex to a
 *   subordinate node in a mainland-directed hierarchy. The common law's
 *   defining features — precedent-based reasoning, procedural fairness,
 *   judicial independence, open justice — are displaced by mainland criminal
 *   procedure norms (extended detention, restricted defense access, closed
 *   hearings) and substantive concepts (state secrets, broadly defined
 *   subversion) with no common law analogue. The constraint presents as
 *   coordination (unified national security framework) but operates as
 *   extraction (institutional independence transferred to mainland organs).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nsl_legal_text__jurisdictional_capture_reading, 0.68).
domain_priors:suppression_score(nsl_legal_text__jurisdictional_capture_reading, 0.72).
domain_priors:theater_ratio(nsl_legal_text__jurisdictional_capture_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nsl_legal_text__jurisdictional_capture_reading, tangled_rope).
narrative_ontology:human_readable(nsl_legal_text__jurisdictional_capture_reading, "NSL as Vehicle for Mainland Legal System Transplantation, Eroding Common Law Autonomy").
narrative_ontology:topic_domain(nsl_legal_text__jurisdictional_capture_reading, "constitutional_law/political_sociology/international_relations").

domain_priors:requires_active_enforcement(nsl_legal_text__jurisdictional_capture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nsl_legal_text__jurisdictional_capture_reading, '8ecc6219-045e-4f85-88b7-1ea7a37aae63').
narrative_ontology:cs_kernel_codification('8ecc6219-045e-4f85-88b7-1ea7a37aae63', formalized).
narrative_ontology:cs_authority_grounding('8ecc6219-045e-4f85-88b7-1ea7a37aae63', extraction).
narrative_ontology:cs_interpretation_layer_present('8ecc6219-045e-4f85-88b7-1ea7a37aae63').
narrative_ontology:cs_reading_relation('8ecc6219-045e-4f85-88b7-1ea7a37aae63', nsl_legal_text__sovereignty_restoration_reading, forecloses).
narrative_ontology:cs_reading_relation('8ecc6219-045e-4f85-88b7-1ea7a37aae63', nsl_legal_text__democratic_enclosure_reading, coexists_with).
narrative_ontology:cs_axiom('8ecc6219-045e-4f85-88b7-1ea7a37aae63', foundational, mainland_legal_supremacy_in_national_security).
narrative_ontology:cs_axiom_status(mainland_legal_supremacy_in_national_security, holdable).
narrative_ontology:cs_axiom_grounding('8ecc6219-045e-4f85-88b7-1ea7a37aae63', mainland_legal_supremacy_in_national_security, conventional).
narrative_ontology:cs_axiom('8ecc6219-045e-4f85-88b7-1ea7a37aae63', foundational, common_law_autonomy_subordinate_to_national_security).
narrative_ontology:cs_axiom_status(common_law_autonomy_subordinate_to_national_security, holdable).
narrative_ontology:cs_axiom_grounding('8ecc6219-045e-4f85-88b7-1ea7a37aae63', common_law_autonomy_subordinate_to_national_security, conventional).
narrative_ontology:cs_reference_frame('8ecc6219-045e-4f85-88b7-1ea7a37aae63', basic_law_original_design).
narrative_ontology:cs_drift_state('8ecc6219-045e-4f85-88b7-1ea7a37aae63', post_nsl_promulgation_2020, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('8ecc6219-045e-4f85-88b7-1ea7a37aae63', '').
narrative_ontology:cs_kernel_id(nsl_legal_text__jurisdictional_capture_reading, nsl_legal_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nsl_legal_text__jurisdictional_capture_reading, mainland_security_apparatus).
narrative_ontology:constraint_beneficiary(nsl_legal_text__jurisdictional_capture_reading, pro_beijing_establishment).
narrative_ontology:constraint_victim(nsl_legal_text__jurisdictional_capture_reading, hk_judiciary).
narrative_ontology:constraint_victim(nsl_legal_text__jurisdictional_capture_reading, hk_legal_profession).
narrative_ontology:constraint_victim(nsl_legal_text__jurisdictional_capture_reading, hk_common_law_practitioners).
narrative_ontology:constraint_victim(nsl_legal_text__jurisdictional_capture_reading, academic_legal_commentators).
narrative_ontology:constraint_vindicates(nsl_legal_text__jurisdictional_capture_reading, mainland_legal_supremacy_doctrine).
narrative_ontology:constraint_vindicates(nsl_legal_text__jurisdictional_capture_reading, national_security_supremacy_over_local_autonomy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafts, promulgates, and enforces the NSL through the NPCSC and national security organs. Uses the law to extend mainland legal concepts (state secrets, subversion definitions, procedural norms) into HK's common law system. Gains direct operational jurisdiction over selected cases via Article 55 and the Office for Safeguarding National Security. Collects institutional authority and precedent-setting power.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, mainland_security_apparatus, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(nsl_legal_text__jurisdictional_capture_reading, mainland_security_apparatus, beneficiary).

% Local political and business elites aligned with Beijing. Benefit from the NSL's suppression of opposition forces and the reinterpretation of HK's legal framework to favor mainland-aligned outcomes. Their position depends on the constraint's persistence; exit would mean loss of privileged access and patronage networks.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, pro_beijing_establishment, beneficiary,
    organized, biographical, constrained, national).

% Sits at the apex of HK's common law system. Forced to apply NSL provisions that override common law presumptions (bail thresholds, jury trial modifications, judicial review limits). Judges appointed by the Chief Executive on advice of the Judicial Officers Recommendation Commission now face implicit loyalty expectations. Professional identity is fused with the common law tradition; leaving the bench or the jurisdiction means abandoning a life's work and the institutional memory they embody.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, hk_judiciary, payer,
    powerful, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(nsl_legal_text__jurisdictional_capture_reading, hk_judiciary, excluded).

% Barristers and solicitors trained in common law advocacy, precedent-based reasoning, and procedural fairness. The NSL introduces mainland-style criminal procedure (detention periods, restricted lawyer access, closed hearings) and substantive offenses with no common law analogue. Professional competence requires mastering a hybrid system where the mainland component is opaque and politically directed. Exit means retraining or emigration; many remain because their professional self-concept is constituted through the common law.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, hk_legal_profession, payer,
    organized, biographical, identity_locked, local).

% Specialist practitioners in commercial, public law, and human rights fields who rely on common law methodology. Face direct extraction: cases removed from their docket, precedents narrowed, client bases chilled. Some adapt by developing NSL expertise; others lose the work that defined their practice. Exit options exist (commercial arbitration, foreign qualification) but are costly and incomplete.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, hk_common_law_practitioners, payer,
    moderate, biographical, constrained, local).

% University law faculties and independent scholars who previously critiqued government action through common law frameworks. Now face self-censorship pressures, funding reviews, and the risk that critical analysis is treated as evidence of subversion. Their intellectual project — developing HK's common law as a distinct tradition — is being displaced by a transplanted framework they did not choose and cannot influence.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, academic_legal_commentators, excluded,
    moderate, biographical, constrained, local).

% Foreign courts, bar associations, and treaty bodies monitoring HK's compliance with ICCPR, Basic Law, and common law standards. Document the transplantation of mainland legal concepts and the erosion of judicial independence. Their assessments affect HK's international legal reputation and capital-market status but cannot alter the constraint's operation.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, international_legal_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nsl_legal_text__jurisdictional_capture_reading, mainland_security_apparatus).
narrative_ontology:fixing_cost_class(nsl_legal_text__jurisdictional_capture_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified national security legal framework across the PRC/HK boundary, resolving the pre-2020 gap where HK's common law system had no equivalent offences for secession, subversion, terrorism, or collusion with foreign forces — offences the central government deemed necessary for sovereignty protection.
% TRANSFER_FUNCTION: Moves interpretive authority and precedent-setting power from HK's common law judiciary to mainland security organs and NPCSC interpretations. Transfers procedural protections (bail, jury trial, open justice) from defendants to the prosecution. Transfers the definition of legal professionalism from common law advocacy to compliance with national security directives.
% ABSENT_VOICES: Hong Kong residents who would have participated in a legislative process for national security legislation (as contemplated by Basic Law Article 23) but were excluded when the NPCSC enacted the NSL directly. Also excluded: common law jurists from other Commonwealth jurisdictions who previously sat as non-permanent judges on the CFA but have resigned or declined reappointment due to NSL concerns.
% DISAPPEARANCE_RATIONALE: If the NSL's jurisdictional capture provisions vanished overnight, HK's judiciary would revert to common law presumptions in national security cases, the Office for Safeguarding National Security would lose its Article 55 case-removal power, and the legal profession would resume practice under the pre-2020 framework. The mainland security apparatus would lose its direct operational foothold in HK's courts. The constitutional settlement would reorganize around the Basic Law's original design.
% FOUNDING_PROBLEM: The central government perceived a sovereignty gap after 2019: HK's common law system lacked tools to prosecute acts Beijing classified as secession, subversion, terrorism, and collusion. The NPCSC enacted the NSL to close this gap without waiting for HK's own Article 23 legislation, which had stalled since 2003.
% FOUNDING_PROBLEM_CORROBORATION: The central government and pro-establishment figures attest the founding problem remains live, citing ongoing security threats. The HK Bar Association, international bar associations, departed non-permanent judges, and academic legal scholars attest the founding problem was a pretext: the NSL's transplantation of mainland legal concepts goes far beyond the stated offences and serves to restructure HK's legal system. The UN Human Rights Committee's 2022 concluding observations support the shifted-function reading.
narrative_ontology:disappearance_verdict(nsl_legal_text__jurisdictional_capture_reading, world_rearranges).
narrative_ontology:founding_problem_status(nsl_legal_text__jurisdictional_capture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nsl_legal_text__jurisdictional_capture_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(nsl_legal_text__jurisdictional_capture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nsl_legal_text__jurisdictional_capture_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Base extractiveness (0.68) reflects the transfer of interpretive sovereignty from HK courts to mainland organs — the judiciary retains form but loses final authority over the constraint's core domain. Suppression (0.72) captures active enforcement: Article 55 case removal, national security judges designation, bail threshold reversal, and the chilling effect on legal practice. Theater ratio (0.38) acknowledges the genuine coordination function (national security offences filled a real gap) but notes the growing performative gap: 'national security' increasingly covers ordinary dissent, and common law forms are maintained while their substance is hollowed out. Accessibility collapse (0.65) reflects that alternatives (Article 23 legislation, common law development) have been foreclosed by the NPCSC's direct enactment and interpretation monopoly. Resistance (0.58) records the judiciary's partial pushback (bail rulings, jury trial defenses) and the legal profession's adaptive strategies, but notes the structural asymmetry: resistance occurs within a framework the constraint itself defines.
 *
 * PERSPECTIVAL GAP:
 *   From the mainland security apparatus seat (agenda_setter/beneficiary, institutional/arbitrage), the NSL is genuine coordination: it solves a real sovereignty gap with a unified legal framework. From the HK judiciary seat (payer/excluded, powerful/identity_locked), the same structure is enforced extraction: their institutional independence is the resource being transferred. The HK legal profession (payer, organized/identity_locked) experiences it as professional identity capture — their common law self-concept is the target. The engine computes this divergence from the structural data; the authored claim (tangled_rope) states the author's structural judgment that both coordination and extraction are genuine and inseparable.
 *
 * DIRECTIONALITY LOGIC:
 *   Mainland security apparatus and pro-Beijing establishment are beneficiaries (collect institutional authority, precedent-setting power, political control — d near 0.0). HK judiciary, legal profession, common law practitioners are payers (bear institutional subordination, professional displacement, identity capture — d near 1.0). Identity-locked exit for judiciary and profession reflects professional self-concept fused with common law tradition; exit means abandoning the identity that makes them who they are. Academic commentators are excluded (would object, constrained exit). International observers are analytical (analytical/analytical).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (sovereignty gap in national security offences) is contested: the central government says it remains live; the legal profession says it was a pretext for systemic restructuring. The constraint's current operation (mainland legal transplantation via Article 55/63/65) exceeds the founding problem's scope — this is the mandatrophy signal. The arrangement persists not because the gap remains, but because the transplantation itself creates beneficiaries (mainland organs, pro-establishment) who now depend on the extended framework. The theater ratio rise (0.15→0.48) tracks the shift: early enforcement targeted genuine security cases; later enforcement targets ordinary dissent while maintaining common law forms.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    common_law_vs_mainland_legal_transplantation_boundary,
    'Is the NSL''s operation in HK a genuine transplantation of mainland legal system logics (procedural norms, substantive concepts, institutional hierarchy), or is it a sui generis national security framework that merely borrows terminology?',
    'Track NPCSC interpretations, Article 55 case removals, and Court of Final Appeal judgments over time: if mainland concepts (state secrets law, subversion definitions, procuratorial supervision models) are actively imported and applied, transplantation is confirmed. If the NSL develops its own HK-specific jurisprudence within common law methodology, the transplantation claim weakens.',
    'If transplantation is confirmed, the constraint is structurally tangled_rope (coordination + extraction). If the NSL develops a distinct HK jurisprudence, the extraction component diminishes and the constraint may trend toward rope or scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(common_law_vs_mainland_legal_transplantation_boundary, empirical, 'Whether mainland legal system logics are being actively transplanted or the NSL is developing a sui generis HK framework.').

omega_variable(
    judicial_independence_residual_space,
    'How much genuine common law decision-space remains for HK judges in national security cases after NPCSC interpretations and Article 55 removals?',
    'Analyze CFA and High Court national security judgments for: (a) citations of common law authorities vs. mainland sources, (b) procedural innovations protecting defendants, (c) judicial review of executive action in NSL cases. Compare with pre-2020 baselines.',
    'If residual space is substantial, the judiciary is not fully captured — the constraint may be rope with extraction overlay. If residual space is minimal, jurisdictional capture is near-complete and the constraint trends toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(judicial_independence_residual_space, empirical, 'The degree of genuine common law autonomy remaining in HK''s national security adjudication.').

omega_variable(
    kernel_reading_foreclosure_structure,
    'Does the jurisdictional_capture_reading logically foreclose the sovereignty_restoration_reading, or do they coexist as competing framings held by different institutional actors?',
    'Examine whether any single institutional actor (e.g., a judge, a prosecutor, a policy official) can simultaneously hold both readings as internally coherent frameworks. If the core premises are mutually exclusive within one framework, foreclosure holds. If different actors hold each reading without internal contradiction, coexistence holds.',
    'If forecloses, the kernel has a structural fault line: one reading''s validity requires the other''s falsity. If coexists_with, the kernel sustains multiple stable readings across different institutional positions — the constraint family is genuinely plural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_structure, conceptual, 'Structural relationship between jurisdictional_capture_reading and sovereignty_restoration_reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nsl_legal_text__jurisdictional_capture_reading, 2020, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nsl_jc_tr_t2020, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 2020, 0.15).
narrative_ontology:measurement(nsl_jc_tr_t2022, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 2022, 0.22).
narrative_ontology:measurement(nsl_jc_tr_t2024, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 2024, 0.31).
narrative_ontology:measurement(nsl_jc_tr_t2026, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 2026, 0.38).
narrative_ontology:measurement(nsl_jc_tr_t2028, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 2028, 0.44).
narrative_ontology:measurement(nsl_jc_tr_t2030, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 2030, 0.48).

% Extraction over time
narrative_ontology:measurement(nsl_jc_be_t2020, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 2020, 0.42).
narrative_ontology:measurement(nsl_jc_be_t2022, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 2022, 0.52).
narrative_ontology:measurement(nsl_jc_be_t2024, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 2024, 0.61).
narrative_ontology:measurement(nsl_jc_be_t2026, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 2026, 0.68).
narrative_ontology:measurement(nsl_jc_be_t2028, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 2028, 0.73).
narrative_ontology:measurement(nsl_jc_be_t2030, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 2030, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(nsl_jc_su_t2020, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 2020, 0.45).
narrative_ontology:measurement(nsl_jc_su_t2022, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 2022, 0.55).
narrative_ontology:measurement(nsl_jc_su_t2024, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 2024, 0.64).
narrative_ontology:measurement(nsl_jc_su_t2026, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 2026, 0.72).
narrative_ontology:measurement(nsl_jc_su_t2028, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 2028, 0.78).
narrative_ontology:measurement(nsl_jc_su_t2030, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 2030, 0.81).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nsl_legal_text__jurisdictional_capture_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(nsl_legal_text__jurisdictional_capture_reading, 0.12).
narrative_ontology:affects_constraint(nsl_legal_text__jurisdictional_capture_reading, nsl_legal_text__democratic_enclosure_reading).
narrative_ontology:affects_constraint(nsl_legal_text__jurisdictional_capture_reading, nsl_legal_text__sovereignty_restoration_reading).
narrative_ontology:affects_constraint(nsl_legal_text__jurisdictional_capture_reading, hk_article_23_legislation).
narrative_ontology:affects_constraint(nsl_legal_text__jurisdictional_capture_reading, basic_law_article_19_judicial_independence).
narrative_ontology:affects_constraint(nsl_legal_text__jurisdictional_capture_reading, iccpr_hk_implementation).

% DUAL FORMULATION NOTE:
% This constraint is one member of the nsl_legal_text kernel family. The jurisdictional_capture_reading centers mainland legal system transplantation into HK's common law. The democratic_enclosure_reading centers democratic space closure and dissent criminalization. The sovereignty_restoration_reading centers sovereign legitimacy restoration. All three share the same referent (the NSL text) but instantiate different constraints with different beneficiary/victim structures and extractiveness values. The ε-invariance principle requires separate stories; this decomposition follows the BGS pattern.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nsl_legal_text__jurisdictional_capture_reading, powerful, 0.88).
constraint_indexing:directionality_override(nsl_legal_text__jurisdictional_capture_reading, organized, 0.75).
constraint_indexing:directionality_override(nsl_legal_text__jurisdictional_capture_reading, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
