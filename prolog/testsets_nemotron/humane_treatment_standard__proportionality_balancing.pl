% ============================================================================
% CONSTRAINT STORY: humane_treatment_standard__proportionality_balancing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_humane_treatment_standard__proportionality_balancing, []).

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
 *   constraint_id: humane_treatment_standard__proportionality_balancing
 *   human_readable: Common Article 3 Proportionality Balancing Standard
 *   domain: international_humanitarian_law/state_security/human_rights
 *
 * SUMMARY:
 *   This constraint story instantiates the proportionality_balancing reading
 *   of the humane_treatment_standard kernel. Common Article 3's 'humane
 *   treatment' requirement is indeterminate; this reading holds that it
 *   requires case-by-case proportional balancing between detainee dignity and
 *   security imperatives. Courts become gatekeepers; treatment permissibility
 *   is decided through judicial review rather than absolute rules. The
 *   constraint coordinates a minimal standard across asymmetric conflicts
 *   while extracting compliance costs from operational personnel and
 *   transferring authority to judicial actors. It is a tangled rope: genuine
 *   coordination (a workable standard states accept) combined with asymmetric
 *   extraction (interrogators bear legal risk, detainees get partial
 *   protection).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(humane_treatment_standard__proportionality_balancing, 0.42).
domain_priors:suppression_score(humane_treatment_standard__proportionality_balancing, 0.35).
domain_priors:theater_ratio(humane_treatment_standard__proportionality_balancing, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, extractiveness, 0.42).
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(humane_treatment_standard__proportionality_balancing, tangled_rope).
narrative_ontology:human_readable(humane_treatment_standard__proportionality_balancing, "Common Article 3 Proportionality Balancing Standard").
narrative_ontology:topic_domain(humane_treatment_standard__proportionality_balancing, "international_humanitarian_law/state_security/human_rights").

domain_priors:requires_active_enforcement(humane_treatment_standard__proportionality_balancing).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(humane_treatment_standard__proportionality_balancing, '3f82c6c0-0071-459d-808c-0b44e44a8d2f').
narrative_ontology:cs_kernel_codification('3f82c6c0-0071-459d-808c-0b44e44a8d2f', fixed_text).
narrative_ontology:cs_authority_grounding('3f82c6c0-0071-459d-808c-0b44e44a8d2f', lineage).
narrative_ontology:cs_interpretation_layer_present('3f82c6c0-0071-459d-808c-0b44e44a8d2f').
narrative_ontology:cs_reading_relation('3f82c6c0-0071-459d-808c-0b44e44a8d2f', humane_treatment_standard__absolute_prohibition, coexists_with).
narrative_ontology:cs_reading_relation('3f82c6c0-0071-459d-808c-0b44e44a8d2f', humane_treatment_standard__contextual_necessity, influences).
narrative_ontology:cs_axiom('3f82c6c0-0071-459d-808c-0b44e44a8d2f', foundational, proportional_balancing_required).
narrative_ontology:cs_axiom_status(proportional_balancing_required, holdable).
narrative_ontology:cs_axiom_grounding('3f82c6c0-0071-459d-808c-0b44e44a8d2f', proportional_balancing_required, conventional).
narrative_ontology:cs_axiom('3f82c6c0-0071-459d-808c-0b44e44a8d2f', foundational, judicial_gatekeeping_function).
narrative_ontology:cs_axiom_status(judicial_gatekeeping_function, holdable).
narrative_ontology:cs_axiom_grounding('3f82c6c0-0071-459d-808c-0b44e44a8d2f', judicial_gatekeeping_function, conventional).
narrative_ontology:cs_reference_frame('3f82c6c0-0071-459d-808c-0b44e44a8d2f', common_article_3_minimal_text).
narrative_ontology:cs_drift_state('3f82c6c0-0071-459d-808c-0b44e44a8d2f', post_war_on_terror_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3f82c6c0-0071-459d-808c-0b44e44a8d2f', '').
narrative_ontology:cs_kernel_id(humane_treatment_standard__proportionality_balancing, humane_treatment_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(humane_treatment_standard__proportionality_balancing, detainees_in_niac).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__proportionality_balancing, state_legal_advisors).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__proportionality_balancing, judicial_gatekeepers).
narrative_ontology:constraint_victim(humane_treatment_standard__proportionality_balancing, interrogation_personnel).
narrative_ontology:constraint_victim(humane_treatment_standard__proportionality_balancing, operational_commanders).
narrative_ontology:constraint_vindicates(humane_treatment_standard__proportionality_balancing, proportionality_balancing_doctrine).
narrative_ontology:constraint_vindicates(humane_treatment_standard__proportionality_balancing, judicial_gatekeeping_function).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Persons deprived of liberty in non-international armed conflicts. They receive procedural safeguards and a proportionality review of their treatment, but remain entirely dependent on state compliance. Exit from the constraint's reach is impossible — they cannot leave the detention context. The balancing standard gives them a legal foothold but not absolute protection; treatment is assessed case-by-case against security claims.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, detainees_in_niac, beneficiary,
    powerless, biographical, trapped, regional).

% Government legal officers who advise on detention and interrogation policy. The proportionality standard gives them a structured framework to authorize, limit, and defend interrogation practices in court. They benefit from the flexibility of case-by-case balancing rather than rigid rules that could be more easily challenged. Their institutional position allows them to shape how the standard is applied operationally.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, state_legal_advisors, beneficiary,
    institutional, generational, arbitrage, national).

% Courts and tribunals that review detention and treatment decisions. The proportionality standard makes them the ultimate arbiters of what treatment is permissible — they define the balance between dignity and security. This gatekeeping role expands judicial authority into operational military territory. They benefit from the institutional prestige and doctrinal influence, but also bear the burden of making security determinations without operational expertise.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, judicial_gatekeepers, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(humane_treatment_standard__proportionality_balancing, judicial_gatekeepers, beneficiary).

% Military and intelligence personnel conducting interrogations. They face case-by-case legal uncertainty: each technique must be justified against a proportionality test they are not trained to apply. Violations carry criminal liability. They cannot easily exit — reassignment is possible but career paths are tied to operational roles. The standard constrains their discretion without giving them clear bright-line rules, creating compliance risk that falls on individual operators.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, interrogation_personnel, payer,
    moderate, biographical, constrained, regional).

% Field commanders responsible for detention operations and intelligence collection. They bear the operational cost of the balancing standard: delayed intelligence, legal review requirements, risk of judicial second-guessing. They benefit from the standard's flexibility compared to absolute prohibition, but pay in procedural overhead and strategic constraint. Exit means leaving command roles; their institutional identity is fused with operational authority.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, operational_commanders, payer,
    organized, biographical, constrained, regional).

% Advocacy organizations that push for absolute prohibition and monitor compliance. They are excluded from the operational and legal decision-making where the balancing test is applied. They would object that proportionality balancing legitimizes abusive practices by making them judicially reviewable rather than categorically forbidden. Their exit option is continuing external pressure — they are not trapped by the constraint but structurally locked out of its application.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, human_rights_ngos, excluded,
    organized, generational, mobile, global).

% Prosecutors assessing whether state practices meet international standards. They apply the proportionality framework when evaluating war crimes allegations but have no role in shaping the standard's domestic implementation. Their analytical seat gives them a view of how the balancing test operates across jurisdictions, but they neither collect benefits nor bear costs from its day-to-day operation.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, international_criminal_court_prosecutors, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a common legal framework for states, courts, and armed groups to assess detainee treatment in non-international armed conflicts without requiring absolute prohibitions that states would reject or unlimited discretion that would permit abuse. The balancing standard coordinates expectations: states know the outer bounds, detainees know there is a review mechanism, courts have a doctrinal structure for adjudication.
% TRANSFER_FUNCTION: Transfers decision-making authority from operational commanders (who would exercise unlimited discretion) to judicial gatekeepers (who apply the proportionality test). Transfers legal risk from the state as an institution to individual interrogators and commanders who must justify each measure. Transfers protective benefit to detainees who gain a reviewable standard rather than pure executive discretion.
% ABSENT_VOICES: Detainees themselves have no voice in how the proportionality balance is struck — they are the objects of the balancing, not participants. Human rights NGOs arguing for absolute prohibition are excluded from the doctrinal framework that treats proportionality as the governing standard. Victims of past abuses whose cases established the standard are not consulted on its ongoing calibration.
% DISAPPEARANCE_RATIONALE: If the proportionality balancing standard vanished overnight, states would revert to either absolute prohibition (which many would ignore) or unlimited executive discretion (which many would exploit). Courts would lose their gatekeeping role in detention treatment. Interrogation personnel would face either categorical bans or no legal guidance. The entire doctrinal architecture of NIAC detention review would collapse into a binary prohibition/discretion choice.
% FOUNDING_PROBLEM: Common Article 3's 'humane treatment' language was deliberately minimal to achieve consensus in 1949, but provided no operational guidance for non-international armed conflicts where states fight non-state actors. States needed a standard they could apply without conceding that insurgents have full POW protections, while the international community needed a floor against total executive discretion. The proportionality balancing reading emerged from 1990s-2000s jurisprudence (ICTY, ICJ, national courts) trying to give 'humane treatment' concrete meaning in asymmetric conflicts.
% FOUNDING_PROBLEM_CORROBORATION: The ICTY and ICJ jurisprudence (Tadic, Hamdan, Al-Skeini) corroborates that proportionality balancing was the judicial solution to Common Article 3's indeterminacy. State legal advisors (US OLC, UK MoD, Israeli AG opinions) corroborate that the standard was adopted because absolute prohibition was politically impossible and unlimited discretion was legally indefensible. Human rights NGOs and the ICRC attest that the founding problem — indeterminate 'humane treatment' in NIAC — persists, but dispute that proportionality balancing solves it rather than legitimizing abuse.
narrative_ontology:disappearance_verdict(humane_treatment_standard__proportionality_balancing, world_rearranges).
narrative_ontology:founding_problem_status(humane_treatment_standard__proportionality_balancing, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(humane_treatment_standard__proportionality_balancing, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(humane_treatment_standard__proportionality_balancing, 'none', 1).
narrative_ontology:epsilon_provenance(humane_treatment_standard__proportionality_balancing, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(humane_treatment_standard__proportionality_balancing_tests).
:- end_tests(humane_treatment_standard__proportionality_balancing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) reflects that the standard transfers real costs to interrogation personnel and commanders (legal uncertainty, procedural burden, criminal liability risk) while delivering partial, reviewable protection to detainees — not a free coordination good. Suppression (0.35) is moderate: the constraint operates through judicial review and institutional compliance mechanisms, not brute force, but deviation carries war crimes liability. Theater ratio (0.28) captures that proportionality review has become partly performative — courts often defer to executive security claims — but the review mechanism itself is real and occasionally bites. Accessibility collapse (0.45) and resistance (0.55) reflect that alternatives (absolute prohibition, unlimited discretion) remain live and contested; the standard has not naturalized.
 *
 * PERSPECTIVAL GAP:
 *   From the judicial gatekeeper seat, the constraint is genuine coordination: a principled framework for adjudicating treatment disputes. From the interrogation personnel seat, the same constraint is extractive: vague standards, personal criminal liability, no operational clarity. From the detainee seat, it is partial protection with no exit. The engine computes these divergences from the structural data — the claimed tangled_rope type reflects that all three seats see different constraint types.
 *
 * DIRECTIONALITY LOGIC:
 *   Detainees are beneficiaries with trapped exit — they receive the standard's protection but cannot leave its reach. State legal advisors and judicial gatekeepers are institutional beneficiaries: the former gain a defensible framework, the latter gain authority. Interrogation personnel and operational commanders are payers: they bear legal risk, procedural costs, and constrained discretion. Human rights NGOs are excluded — they argue for absolute prohibition but the balancing framework structurally locks them out. ICC prosecutors observe from an analytical seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (indeterminate 'humane treatment' in NIAC) remains contested — states still need a workable standard, detainees still need protection, but the proportionality mechanism has accumulated theater (judicial deference) and extraction (operator liability). The mandate has not atrophied into piton because the coordination function is still actively litigated and the standard still shapes operational doctrine. But the extraction-theater drift since 2001 suggests mandatrophy risk if judicial review becomes purely performative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_vs_absolute_prohibition_boundary,
    'Does the proportionality balancing reading foreclose the absolute prohibition reading within a single legal framework, or can a system hold both as alternative interpretive paths?',
    'Examine whether any jurisdiction applies absolute prohibition for some treatment categories (e.g., torture) while using proportionality balancing for others (e.g., degrading treatment) — or whether adopting proportionality as the governing standard logically eliminates absolute prohibition as a live option.',
    'If forecloses, the kernel has a genuine structural split — proportionality and absolute prohibition are mutually exclusive frameworks. If coexists_with, both readings remain live in different doctrinal niches (e.g., absolute prohibition for torture, proportionality for lesser ill-treatment).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_vs_absolute_prohibition_boundary, conceptual, 'Whether proportionality balancing and absolute prohibition are mutually exclusive frameworks or complementary layers').

omega_variable(
    judicial_deference_as_extraction_mechanism,
    'Is the observed judicial deference to executive security claims a bug (institutional failure) or a feature (the proportionality standard''s actual operating logic)?',
    'Compare reversal rates in detainee treatment cases across jurisdictions and over time. If deference is systematic and stable, it is structural — the standard was designed to legitimate executive discretion under a judicial veneer. If deference varies with judicial independence, it is contingent.',
    'If structural feature, the constraint is more snare-like: the coordination story (judicial gatekeeping) is cover for executive discretion. If contingent bug, the constraint remains a tangled rope with a genuine coordination function that is imperfectly implemented.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(judicial_deference_as_extraction_mechanism, empirical, 'Whether judicial deference in proportionality review is structural or contingent').

omega_variable(
    kernel_reading_structure,
    'How does this reading relate structurally to the sibling readings of the humane_treatment_standard kernel?',
    'Track doctrinal citations: when courts invoke proportionality balancing, do they distinguish it from absolute prohibition, treat it as a refinement, or present it as the only viable reading? Map the citation network across the three readings.',
    'Determines the reading_relations in cs_structure. If proportionality forecloses absolute prohibition, the kernel has a hard split. If they coexist, the kernel supports multiple simultaneous frameworks. If proportionality influences contextual necessity by setting a floor, there is a directional pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_structure, conceptual, 'Structural relationship between this reading and its sibling readings of the contested kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(humane_treatment_standard__proportionality_balancing, 1995, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(huma_tr_t1995, humane_treatment_standard__proportionality_balancing, theater_ratio, 1995, 0.1).
narrative_ontology:measurement(huma_tr_t2001, humane_treatment_standard__proportionality_balancing, theater_ratio, 2001, 0.22).
narrative_ontology:measurement(huma_tr_t2006, humane_treatment_standard__proportionality_balancing, theater_ratio, 2006, 0.3).
narrative_ontology:measurement(huma_tr_t2011, humane_treatment_standard__proportionality_balancing, theater_ratio, 2011, 0.28).
narrative_ontology:measurement(huma_tr_t2016, humane_treatment_standard__proportionality_balancing, theater_ratio, 2016, 0.25).
narrative_ontology:measurement(huma_tr_t2021, humane_treatment_standard__proportionality_balancing, theater_ratio, 2021, 0.26).
narrative_ontology:measurement(huma_tr_t2025, humane_treatment_standard__proportionality_balancing, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(huma_be_t1995, humane_treatment_standard__proportionality_balancing, base_extractiveness, 1995, 0.25).
narrative_ontology:measurement(huma_be_t2001, humane_treatment_standard__proportionality_balancing, base_extractiveness, 2001, 0.35).
narrative_ontology:measurement(huma_be_t2006, humane_treatment_standard__proportionality_balancing, base_extractiveness, 2006, 0.42).
narrative_ontology:measurement(huma_be_t2011, humane_treatment_standard__proportionality_balancing, base_extractiveness, 2011, 0.4).
narrative_ontology:measurement(huma_be_t2016, humane_treatment_standard__proportionality_balancing, base_extractiveness, 2016, 0.38).
narrative_ontology:measurement(huma_be_t2021, humane_treatment_standard__proportionality_balancing, base_extractiveness, 2021, 0.4).
narrative_ontology:measurement(huma_be_t2025, humane_treatment_standard__proportionality_balancing, base_extractiveness, 2025, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(huma_su_t1995, humane_treatment_standard__proportionality_balancing, suppression_requirement, 1995, 0.15).
narrative_ontology:measurement(huma_su_t2001, humane_treatment_standard__proportionality_balancing, suppression_requirement, 2001, 0.4).
narrative_ontology:measurement(huma_su_t2006, humane_treatment_standard__proportionality_balancing, suppression_requirement, 2006, 0.38).
narrative_ontology:measurement(huma_su_t2011, humane_treatment_standard__proportionality_balancing, suppression_requirement, 2011, 0.35).
narrative_ontology:measurement(huma_su_t2016, humane_treatment_standard__proportionality_balancing, suppression_requirement, 2016, 0.32).
narrative_ontology:measurement(huma_su_t2021, humane_treatment_standard__proportionality_balancing, suppression_requirement, 2021, 0.33).
narrative_ontology:measurement(huma_su_t2025, humane_treatment_standard__proportionality_balancing, suppression_requirement, 2025, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(humane_treatment_standard__proportionality_balancing, enforcement_mechanism).
narrative_ontology:affects_constraint(humane_treatment_standard__proportionality_balancing, humane_treatment_standard__absolute_prohibition).
narrative_ontology:affects_constraint(humane_treatment_standard__proportionality_balancing, humane_treatment_standard__contextual_necessity).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the humane_treatment_standard kernel. The absolute_prohibition reading (non-derogable minimums) and contextual_necessity reading (security override) are separate constraint stories with different ε values, beneficiary/victim structures, and claimed types. This reading's ε (0.42) reflects its hybrid coordination-extraction character; absolute_prohibition would have lower ε (genuine coordination, minimal extraction); contextual_necessity would have higher ε (extraction-heavy). They are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(humane_treatment_standard__proportionality_balancing, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
