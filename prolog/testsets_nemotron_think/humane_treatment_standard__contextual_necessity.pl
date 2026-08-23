% ============================================================================
% CONSTRAINT STORY: humane_treatment_standard__contextual_necessity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_humane_treatment_standard__contextual_necessity, []).

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
 *   constraint_id: humane_treatment_standard__contextual_necessity
 *   human_readable: Contextual Necessity Exception to Common Article 3 Humane Treatment Standard
 *   domain: international_humanitarian_law/state_security/human_rights
 *
 * SUMMARY:
 *   The contextual necessity reading of Common Article 3 emerged post-9/11 as
 *   a U.S. government interpretation permitting 'enhanced interrogation
 *   techniques' when national security imperatives were deemed to override
 *   the baseline humane treatment standard. The reading asserts that Common
 *   Article 3 sets a floor but allows contextual adjustment — effectively
 *   making the prohibition on cruel, inhuman, or degrading treatment
 *   contingent on the detaining state's security assessment. This reading was
 *   instantiated through OLC memoranda (2002-2005), military orders, and
 *   classification decisions that placed high-value detainees outside
 *   standard protections. The constraint operates by transferring
 *   interpretive authority from the treaty text and international supervisory
 *   bodies to the executive branch's security apparatus. The claim/metric gap
 *   is structural: the reading claims to be a legitimate interpretation
 *   (rope-like coordination) while the metrics reveal substantial extraction
 *   (detainees lose absolute protections) and active suppression (secrecy,
 *   classification, exclusion of oversight).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(humane_treatment_standard__contextual_necessity, 0.75).
domain_priors:suppression_score(humane_treatment_standard__contextual_necessity, 0.82).
domain_priors:theater_ratio(humane_treatment_standard__contextual_necessity, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, extractiveness, 0.75).
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(humane_treatment_standard__contextual_necessity, tangled_rope).
narrative_ontology:human_readable(humane_treatment_standard__contextual_necessity, "Contextual Necessity Exception to Common Article 3 Humane Treatment Standard").
narrative_ontology:topic_domain(humane_treatment_standard__contextual_necessity, "international_humanitarian_law/state_security/human_rights").

domain_priors:requires_active_enforcement(humane_treatment_standard__contextual_necessity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(humane_treatment_standard__contextual_necessity, 'c983f725-e2e6-441c-99c2-3468b6864941').
narrative_ontology:cs_kernel_codification('c983f725-e2e6-441c-99c2-3468b6864941', fixed_text).
narrative_ontology:cs_authority_grounding('c983f725-e2e6-441c-99c2-3468b6864941', lineage).
narrative_ontology:cs_interpretation_layer_present('c983f725-e2e6-441c-99c2-3468b6864941').
narrative_ontology:cs_reading_relation('c983f725-e2e6-441c-99c2-3468b6864941', humane_treatment_standard__absolute_prohibition, coexists_with).
narrative_ontology:cs_reading_relation('c983f725-e2e6-441c-99c2-3468b6864941', humane_treatment_standard__proportionality_balancing, influences).
narrative_ontology:cs_axiom('c983f725-e2e6-441c-99c2-3468b6864941', foundational, national_security_necessity_overrides_humane_baseline).
narrative_ontology:cs_axiom_status(national_security_necessity_overrides_humane_baseline, holdable).
narrative_ontology:cs_axiom_grounding('c983f725-e2e6-441c-99c2-3468b6864941', national_security_necessity_overrides_humane_baseline, instrumental).
narrative_ontology:cs_axiom('c983f725-e2e6-441c-99c2-3468b6864941', secondary, humane_treatment_is_context_dependent_not_absolute).
narrative_ontology:cs_axiom_status(humane_treatment_is_context_dependent_not_absolute, holdable).
narrative_ontology:cs_axiom_grounding('c983f725-e2e6-441c-99c2-3468b6864941', humane_treatment_is_context_dependent_not_absolute, conventional).
narrative_ontology:cs_reference_frame('c983f725-e2e6-441c-99c2-3468b6864941', common_article_3_baseline_with_necessity_exception).
narrative_ontology:cs_drift_state('c983f725-e2e6-441c-99c2-3468b6864941', post_9_11_war_on_terror, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c983f725-e2e6-441c-99c2-3468b6864941', '').
narrative_ontology:cs_kernel_id(humane_treatment_standard__contextual_necessity, humane_treatment_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(humane_treatment_standard__contextual_necessity, security_agencies).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__contextual_necessity, state_executive).
narrative_ontology:constraint_victim(humane_treatment_standard__contextual_necessity, high_value_detainees).
narrative_ontology:constraint_victim(humane_treatment_standard__contextual_necessity, detainees_in_security_contexts).
narrative_ontology:constraint_vindicates(humane_treatment_standard__contextual_necessity, state_sovereignty_in_security_matters).
narrative_ontology:constraint_vindicates(humane_treatment_standard__contextual_necessity, executive_discretion_in_warfare).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and apply the 'contextual necessity' reading to define permissible interrogation techniques. Draft legal memos (e.g., OLC memoranda) that authorize enhanced interrogation when deemed necessary for national security. Control the classification of detainees and the definition of 'humane' in practice. Benefit from expanded operational latitude and reduced legal liability. Can shift between domestic and international legal frameworks to avoid accountability.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, security_agencies, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(humane_treatment_standard__contextual_necessity, security_agencies, beneficiary).

% Authorizes the legal framework permitting contextual interpretation of humane treatment. Gains political cover for aggressive counterterrorism operations. Benefits from the ability to claim compliance with Common Article 3 while effectively setting its meaning. Can invoke state secrets and executive privilege to shield the interpretation from judicial review. Moves between legal frameworks (domestic/international) to maximize discretion.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, state_executive, beneficiary,
    institutional, generational, arbitrage, national).

% Subject to enhanced interrogation techniques authorized under the contextual necessity reading. Have no meaningful exit from detention or the interrogation regime. Legal protections are conditional and contingent on the detaining state's security assessment. Cannot access courts effectively due to classification, jurisdictional bars, and physical isolation. Bear the full physical and psychological costs of the interpretation.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, high_value_detainees, payer,
    powerless, immediate, trapped, local).

% All detainees held in national security contexts (Guantanamo, black sites, battlefield detention) whose treatment standards are governed by the contextual necessity interpretation. The baseline Common Article 3 protections apply only when the detaining authority decides they do. No individual exit; collective exit depends on external pressure (courts, diplomatic, NGO). The class expands or contracts at the discretion of the agenda-setters.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, detainees_in_security_contexts, payer,
    powerless, immediate, trapped, local).

% International and regional courts (ICJ, ECHR, IACHR, ICC) that adjudicate whether state practices comply with Common Article 3 and the prohibition on torture. Their jurisprudence predominantly rejects the contextual necessity reading (e.g., ECHR Ireland v. UK, Al-Skeini). They observe the constraint's operation from outside the detaining state's legal order. Their rulings create normative pressure but lack direct enforcement against powerful states.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, human_rights_courts, observer,
    institutional, generational, analytical, global).

% Document, litigate, and advocate against the contextual necessity interpretation. Are structurally excluded from the detention sites and the legal memo-drafting process. Their access to detainees is controlled by the agenda-setters. They mobilize public opinion and feed cases to courts, but their exclusion from the operational space is a feature of the constraint's enforcement. Exit from exclusion requires state permission they cannot compel.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, human_rights_ngos, excluded,
    organized, generational, constrained, global).

% Judge Advocates and military legal advisors who must advise commanders on the lawfulness of interrogation policies. Some internalize the contextual necessity reading as binding; others resist (e.g., Navy JAGs opposing torture memos). Their professional standing and career progression depend on institutional loyalty. Exit from the role means leaving military service. They occupy a contested professional space where the reading's legitimacy is tested daily.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, military_lawyers, observer,
    organized, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Common Article 3 provides a baseline coordination mechanism: a minimal, universally agreed floor for treatment of persons hors de combat in non-international armed conflicts. It solves the coordination problem of mutual restraint between warring parties by establishing a shared reference point.
% TRANSFER_FUNCTION: The contextual necessity reading transfers the power to define 'humane treatment' from the treaty text and international supervisory bodies to the detaining state's security apparatus. It moves legal protection from detainees (who lose the absolute floor) to state agencies (who gain discretionary authority). The transfer is effected through legal memoranda, classification decisions, and operational orders.
% ABSENT_VOICES: The detainees themselves — especially high-value targets held incommunicado — are the primary absent voices. They cannot testify, litigate, or organize while the constraint operates on them. Their voices enter the record only years later through declassification, leaks, or post-release testimony. The constraint's enforcement architecture (secrecy, classification, physical isolation) is designed to maintain their absence.
% DISAPPEARANCE_RATIONALE: If the contextual necessity reading vanished overnight, the absolute prohibition reading would become the sole operative interpretation. Security agencies would lose legal authorization for enhanced interrogation; detainees would regain the non-derogable Common Article 3 floor. Prosecutions for past acts would become legally straightforward. The global counterterrorism detention architecture (Guantanamo, rendition, black sites) would face immediate legal collapse or require new legislative authorization.
% FOUNDING_PROBLEM: Post-9/11, the U.S. and allied states faced a perceived gap: Common Article 3 and the Convention Against Torture appeared to prohibit coercive interrogation of high-value terrorist suspects, but security agencies argued these suspects possessed time-sensitive intelligence critical to preventing attacks. The contextual necessity reading was constructed to resolve this perceived dilemma by making humane treatment conditional on security imperatives.
% FOUNDING_PROBLEM_CORROBORATION: The 9/11 Commission Report and subsequent Senate Torture Report (SSCI) corroborate that the perceived intelligence gap was real but also document that the resulting program produced little actionable intelligence. The CIA's own Inspector General report (2004) questioned the necessity claim. Independent experts (Human Rights First, ICRC) attest the founding problem was overstated and the reading was a policy choice, not a legal necessity. No corroborating source outside the benefiting security agencies supports the claim that absolute prohibition would have prevented threat disruption.
narrative_ontology:disappearance_verdict(humane_treatment_standard__contextual_necessity, world_rearranges).
narrative_ontology:founding_problem_status(humane_treatment_standard__contextual_necessity, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(humane_treatment_standard__contextual_necessity, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(humane_treatment_standard__contextual_necessity, 'none', 1).
narrative_ontology:epsilon_provenance(humane_treatment_standard__contextual_necessity, 0.75, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(humane_treatment_standard__contextual_necessity_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(humane_treatment_standard__contextual_necessity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(humane_treatment_standard__contextual_necessity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.75) is high because the reading transfers the defining power of 'humane treatment' from an objective treaty standard to the subjective security assessment of the detaining authority, extracting the full protective value of Common Article 3 from the detainee class. Suppression (0.82) is very high because the reading's persistence depends on active secrecy (classification of techniques, black sites, rendition), jurisdictional bars (Military Commissions Act, state secrets privilege), and physical isolation of victims. Theater ratio (0.42) reflects that the Common Article 3 baseline is genuinely invoked — the reading does not reject the treaty but hollows it out through the exception. Accessibility collapse (0.68) is substantial: the absolute prohibition alternative exists in law but is rendered practically inaccessible to detainees by the enforcement architecture. Resistance (0.72) is high: sustained pushback from courts (Hamdan, Boumediene, ECHR), Congress (McCain Amendment, DTA), military lawyers, and NGOs has not displaced the reading but has forced partial retractions and policy shifts.
 *
 * PERSPECTIVAL GAP:
 *   From the security agency seat, the reading is a necessary coordination adaptation — the treaty baseline is preserved for ordinary contexts while security imperatives get a necessary exception. From the detainee seat, the same structure is a snare: the exception swallows the rule, and the baseline exists only when the captor chooses. The engine computes this divergence from the declared beneficiaries/victims and exit structures. The claimed_type (tangled_rope) captures the author's structural judgment: genuine coordination baseline + asymmetric extraction via conditional exception.
 *
 * DIRECTIONALITY LOGIC:
 *   Security agencies and the state executive are structural beneficiaries (d near 0.0): they gain discretionary authority, legal cover, and operational latitude. The constraint subsidizes their preferred interpretation. High-value detainees and security-context detainees are structural targets (d near 1.0): they bear the full cost of the conditional protection regime with trapped exit. Human rights courts are analytical observers (d = 0.5): they experience neither benefit nor extraction but observe the constraint's operation. Human rights NGOs are excluded (d near 1.0 for exclusion): they would challenge the reading but are kept out of the operational space. Military lawyers are constrained observers: some internalize the reading (lower d), others resist (higher d), but their institutional role limits exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-9/11 intelligence gap) is contested and arguably dead per the SSCI report's findings on efficacy. Yet the reading persists in residual form (revised Army Field Manual Appendix M, continued Guantanamo detention, drone strike legal frameworks). The constraint exhibits mandatrophy: the original justification has atrophied but the interpretive architecture remains, maintained by institutional inertia and the precedent value for future 'necessity' claims. The theater ratio rise tracks this — more performance (compliance reviews, legal vetting) for less functional necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    necessity_exception_genuineness,
    'Is the contextual necessity exception a genuine treaty interpretation or a constructed cover for torture and CID treatment?',
    'Comparative analysis of treaty negotiating history (travaux préparatoires), subsequent state practice, and the object and purpose of Common Article 3. The ICRC''s authoritative commentary and the ICTY/ICTR jurisprudence on non-derogability provide evidence.',
    'If a constructed cover, the reading is a snare masquerading as a tangled rope — the coordination function is entirely performative. If a genuine (though contested) interpretation, the tangled rope classification holds: real coordination baseline + real asymmetric extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessity_exception_genuineness, conceptual, 'Whether the necessity exception is a legitimate reading or a bad-faith construction.').

omega_variable(
    baseline_survival_under_exception,
    'Does the Common Article 3 baseline retain any coordination function when the necessity exception is invoked, or does the exception swallow the rule entirely?',
    'Empirical study of state practice: do states invoking the necessity reading still observe any Common Article 3 protections for detainees not deemed ''high-value''? Do they treat the baseline as operative for ordinary detainees?',
    'If the baseline survives for some detainees, the tangled rope coordination function is real (partial coordination). If the exception becomes the general rule, the coordination collapses and the constraint becomes a snare with a vestigial coordination claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(baseline_survival_under_exception, empirical, 'Whether the coordination baseline survives the exception''s operation.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.82) primarily structural (secrecy, classification, physical isolation) or does it include a substantial internalized component (detainees'' learned helplessness, normalization of abuse)?',
    'Post-release testimony analysis: if former detainees report persistent psychological barriers to asserting rights even after physical release, internalized suppression is significant. Compare with detainees held under absolute prohibition regimes.',
    'If internalized suppression is substantial, the constraint''s effective suppression exceeds the structural measure — victims carry the suppression with them. This would increase effective extraction for the payer seats beyond the engine''s structural computation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanisms for detainees.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(humane_treatment_standard__contextual_necessity, 0, 23).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(humane_treatment_contextual_necessity_tr_t0, humane_treatment_standard__contextual_necessity, theater_ratio, 0, 0.18).
narrative_ontology:measurement(humane_treatment_contextual_necessity_tr_t3, humane_treatment_standard__contextual_necessity, theater_ratio, 3, 0.25).
narrative_ontology:measurement(humane_treatment_contextual_necessity_tr_t6, humane_treatment_standard__contextual_necessity, theater_ratio, 6, 0.32).
narrative_ontology:measurement(humane_treatment_contextual_necessity_tr_t9, humane_treatment_standard__contextual_necessity, theater_ratio, 9, 0.38).
narrative_ontology:measurement(humane_treatment_contextual_necessity_tr_t12, humane_treatment_standard__contextual_necessity, theater_ratio, 12, 0.4).
narrative_ontology:measurement(humane_treatment_contextual_necessity_tr_t15, humane_treatment_standard__contextual_necessity, theater_ratio, 15, 0.41).
narrative_ontology:measurement(humane_treatment_contextual_necessity_tr_t18, humane_treatment_standard__contextual_necessity, theater_ratio, 18, 0.42).
narrative_ontology:measurement(humane_treatment_contextual_necessity_tr_t21, humane_treatment_standard__contextual_necessity, theater_ratio, 21, 0.42).
narrative_ontology:measurement(humane_treatment_contextual_necessity_tr_t23, humane_treatment_standard__contextual_necessity, theater_ratio, 23, 0.42).

% Extraction over time
narrative_ontology:measurement(humane_treatment_contextual_necessity_be_t0, humane_treatment_standard__contextual_necessity, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(humane_treatment_contextual_necessity_be_t3, humane_treatment_standard__contextual_necessity, base_extractiveness, 3, 0.52).
narrative_ontology:measurement(humane_treatment_contextual_necessity_be_t6, humane_treatment_standard__contextual_necessity, base_extractiveness, 6, 0.61).
narrative_ontology:measurement(humane_treatment_contextual_necessity_be_t9, humane_treatment_standard__contextual_necessity, base_extractiveness, 9, 0.68).
narrative_ontology:measurement(humane_treatment_contextual_necessity_be_t12, humane_treatment_standard__contextual_necessity, base_extractiveness, 12, 0.72).
narrative_ontology:measurement(humane_treatment_contextual_necessity_be_t15, humane_treatment_standard__contextual_necessity, base_extractiveness, 15, 0.74).
narrative_ontology:measurement(humane_treatment_contextual_necessity_be_t18, humane_treatment_standard__contextual_necessity, base_extractiveness, 18, 0.73).
narrative_ontology:measurement(humane_treatment_contextual_necessity_be_t21, humane_treatment_standard__contextual_necessity, base_extractiveness, 21, 0.75).
narrative_ontology:measurement(humane_treatment_contextual_necessity_be_t23, humane_treatment_standard__contextual_necessity, base_extractiveness, 23, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(humane_treatment_contextual_necessity_su_t0, humane_treatment_standard__contextual_necessity, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(humane_treatment_contextual_necessity_su_t3, humane_treatment_standard__contextual_necessity, suppression_requirement, 3, 0.65).
narrative_ontology:measurement(humane_treatment_contextual_necessity_su_t6, humane_treatment_standard__contextual_necessity, suppression_requirement, 6, 0.72).
narrative_ontology:measurement(humane_treatment_contextual_necessity_su_t9, humane_treatment_standard__contextual_necessity, suppression_requirement, 9, 0.78).
narrative_ontology:measurement(humane_treatment_contextual_necessity_su_t12, humane_treatment_standard__contextual_necessity, suppression_requirement, 12, 0.8).
narrative_ontology:measurement(humane_treatment_contextual_necessity_su_t15, humane_treatment_standard__contextual_necessity, suppression_requirement, 15, 0.81).
narrative_ontology:measurement(humane_treatment_contextual_necessity_su_t18, humane_treatment_standard__contextual_necessity, suppression_requirement, 18, 0.82).
narrative_ontology:measurement(humane_treatment_contextual_necessity_su_t21, humane_treatment_standard__contextual_necessity, suppression_requirement, 21, 0.82).
narrative_ontology:measurement(humane_treatment_contextual_necessity_su_t23, humane_treatment_standard__contextual_necessity, suppression_requirement, 23, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(humane_treatment_standard__contextual_necessity, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(humane_treatment_standard__contextual_necessity, 0.12).
narrative_ontology:affects_constraint(humane_treatment_standard__contextual_necessity, humane_treatment_standard__absolute_prohibition).
narrative_ontology:affects_constraint(humane_treatment_standard__contextual_necessity, humane_treatment_standard__proportionality_balancing).
narrative_ontology:affects_constraint(humane_treatment_standard__contextual_necessity, command_responsibility_doctrine).
narrative_ontology:affects_constraint(humane_treatment_standard__contextual_necessity, non_refoulement_obligation).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the humane_treatment_standard kernel. The absolute_prohibition reading (non-derogable floor) and proportionality_balancing reading (case-by-case balancing) are the sibling constraints. All three share the same treaty kernel but instantiate different constraints with different ε, beneficiary/victim structures, and classifications. This reading's necessity exception structurally pressures the proportionality reading by expanding the 'necessity' pole of the balance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(humane_treatment_standard__contextual_necessity, institutional, 0.1).
constraint_indexing:directionality_override(humane_treatment_standard__contextual_necessity, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
