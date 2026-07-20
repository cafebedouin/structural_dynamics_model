% ============================================================================
% CONSTRAINT STORY: rome_statute_jurisdiction__sovereigntist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rome_statute_jurisdiction__sovereigntist_reading, []).

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
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: rome_statute_jurisdiction__sovereigntist_reading
 *   human_readable: Rome Statute Jurisdictional Framework â Sovereigntist Reading
 *   domain: international_law/treaty_interpretation/institutional_authority
 *
 * SUMMARY:
 *   This constraint story instantiates the sovereigntist reading of the Rome
 *   Statute jurisdiction kernel. The Rome Statute is read here as creating a
 *   conditional framework in which ICC jurisdiction is strictly limited by
 *   state consent (Articles 12-13), national courts retain primary authority
 *   through complementarity (Article 17), and non-party nationals enjoy
 *   immunity absent UNSC referral. This reading is contested by a
 *   universalist reading that claims the Statute establishes a transcendent
 *   international criminal mandate, and a hybrid reading that sees
 *   complementarity as a balance rather than deference. The sovereigntist
 *   reading is analytically distinct: its epsilon reflects the structural
 *   transfer of jurisdictional authority from international prosecution back
 *   to sovereign states, with identifiable costs borne by victims in
 *   non-consenting jurisdictions. The metrics and claim are authored
 *   independently; the engine measures any divergence between the
 *   sovereigntist claim and the operational extraction the framework
 *   produces.
 *
 * KEY AGENTS:
 *   - sovereign_states: Primary beneficiary and agenda-setter (institutional/constrained) â negotiated consent-based jurisdiction and retain primacy through complementarity.
 *   - non_party_states: Secondary beneficiary (institutional/mobile) â enjoy immunity for their nationals without treaty ratification.
 *   - victims_of_atrocities: Primary target (powerless/trapped) â denied ICC recourse when perpetrators are shielded by non-party status or admissibility rulings.
 *   - icc_office_of_prosecutor: Structural payer (institutional/constrained) â mandate narrowed by consent and complementarity gates.
 *   - un_security_council: Agenda-setter (institutional/constrained) â controls the universal-reach exception via referral.
 *   - national_judiciaries: Beneficiary (institutional/constrained) â receive deference and primary authority.
 *   - universalist_legal_scholars: Excluded voice (organized/analytical) â argue for inherent universal jurisdiction but are structurally outside the Statute's operative framework.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rome_statute_jurisdiction__sovereigntist_reading, 0.58).
domain_priors:suppression_score(rome_statute_jurisdiction__sovereigntist_reading, 0.52).
domain_priors:theater_ratio(rome_statute_jurisdiction__sovereigntist_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rome_statute_jurisdiction__sovereigntist_reading, tangled_rope).
narrative_ontology:human_readable(rome_statute_jurisdiction__sovereigntist_reading, "Rome Statute Jurisdictional Framework â Sovereigntist Reading").
narrative_ontology:topic_domain(rome_statute_jurisdiction__sovereigntist_reading, "international_law/treaty_interpretation/institutional_authority").

domain_priors:requires_active_enforcement(rome_statute_jurisdiction__sovereigntist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rome_statute_jurisdiction__sovereigntist_reading, 'e1dbf2ab-4ee2-498f-83e6-7dcd9c526ddf').
narrative_ontology:cs_kernel_codification('e1dbf2ab-4ee2-498f-83e6-7dcd9c526ddf', formalized).
narrative_ontology:cs_authority_grounding('e1dbf2ab-4ee2-498f-83e6-7dcd9c526ddf', lineage).
narrative_ontology:cs_interpretation_layer_present('e1dbf2ab-4ee2-498f-83e6-7dcd9c526ddf').
narrative_ontology:cs_reading_relation('e1dbf2ab-4ee2-498f-83e6-7dcd9c526ddf', rome_statute_jurisdiction__universalist_reading, forecloses).
narrative_ontology:cs_reading_relation('e1dbf2ab-4ee2-498f-83e6-7dcd9c526ddf', rome_statute_jurisdiction__hybrid_complementarity_reading, coexists_with).
narrative_ontology:cs_axiom('e1dbf2ab-4ee2-498f-83e6-7dcd9c526ddf', foundational, state_consent_prerequisite).
narrative_ontology:cs_axiom_status(state_consent_prerequisite, holdable).
narrative_ontology:cs_axiom_grounding('e1dbf2ab-4ee2-498f-83e6-7dcd9c526ddf', state_consent_prerequisite, conventional).
narrative_ontology:cs_axiom('e1dbf2ab-4ee2-498f-83e6-7dcd9c526ddf', foundational, national_court_primacy).
narrative_ontology:cs_axiom_status(national_court_primacy, holdable).
narrative_ontology:cs_axiom_grounding('e1dbf2ab-4ee2-498f-83e6-7dcd9c526ddf', national_court_primacy, conventional).
narrative_ontology:cs_reference_frame('e1dbf2ab-4ee2-498f-83e6-7dcd9c526ddf', westphalian_sovereign_consent).
narrative_ontology:cs_drift_state('e1dbf2ab-4ee2-498f-83e6-7dcd9c526ddf', post_kenya_sudan_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e1dbf2ab-4ee2-498f-83e6-7dcd9c526ddf', '').
narrative_ontology:cs_kernel_id(rome_statute_jurisdiction__sovereigntist_reading, rome_statute_jurisdiction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__sovereigntist_reading, sovereign_states).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__sovereigntist_reading, non_party_states).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__sovereigntist_reading, national_judiciaries).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__sovereigntist_reading, victims_of_atrocities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__sovereigntist_reading, icc_office_of_prosecutor).
narrative_ontology:constraint_vindicates(rome_statute_jurisdiction__sovereigntist_reading, state_consent_doctrine).
narrative_ontology:constraint_vindicates(rome_statute_jurisdiction__sovereigntist_reading, complementarity_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Negotiated the Rome Statute to create an international criminal court subject to their consent. Retain primary jurisdiction over their nationals and territory through the complementarity mechanism. Receive the ability to shield their judicial systems from international override while maintaining the appearance of supporting accountability.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, sovereign_states, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(rome_statute_jurisdiction__sovereigntist_reading, sovereign_states, beneficiary).

% Have not ratified the Rome Statute and remain outside the ICC's jurisdiction over their nationals and territory, except via UNSC referral. Their nationals are immune from ICC prosecution while the state avoids treaty obligations.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, non_party_states, beneficiary,
    institutional, generational, mobile, global).

% Suffer from international crimes and lack ICC recourse when the perpetrator's state has not consented to jurisdiction and no UNSC referral occurs, or when a state claims complementarity to block admissibility. Have no direct standing to trigger ICC jurisdiction.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, victims_of_atrocities, payer,
    powerless, immediate, trapped, global).

% Conducts investigations and prosecutions but is structurally bound by Articles 12, 13, and 17. Cannot open investigations in non-party states without UNSC referral and must defer to genuine national proceedings. Operates with a mandate narrower than universalist aspirations.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, icc_office_of_prosecutor, payer,
    institutional, biographical, constrained, global).

% May refer situations to the ICC involving non-party states under Article 13(b), acting as a gatekeeper for universal reach. Its referrals are subject to permanent-member veto and geopolitical bargaining.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, un_security_council, agenda_setter,
    institutional, immediate, constrained, global).

% Retain primary authority to investigate and prosecute Rome Statute crimes under the complementarity regime. Receive deference from the ICC unless they are unwilling or unable to act genuinely.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, national_judiciaries, beneficiary,
    institutional, biographical, constrained, national).

% Argue that international criminal jurisdiction inheres in the international community regardless of state consent. Their position is structurally excluded from the Rome Statute's operative framework by the consent requirement, though they influence academic and diplomatic discourse outside the treaty.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, universalist_legal_scholars, excluded,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a permanent international criminal court with jurisdiction over genocide, crimes against humanity, and war crimes, while preserving state sovereignty through consent-based jurisdiction and complementarity deference.
% TRANSFER_FUNCTION: Transfers primary criminal jurisdiction from the international plane back to sovereign states and their national courts, except where the state is unwilling or unable genuinely to carry out proceedings, or where the UNSC refers a situation.
% ABSENT_VOICES: Victims in non-party states and universalist legal scholars who argue for inherent international jurisdiction regardless of consent; they are structurally excluded by the consent requirement and complementarity deference.
% DISAPPEARANCE_RATIONALE: If the sovereigntist jurisdictional framework disappeared, the ICC could assert jurisdiction over non-party nationals without UNSC referral, national courts would lose their primacy shield, and the entire architecture of international criminal justice would shift toward universalist or hybrid models.
% FOUNDING_PROBLEM: How to establish a permanent international criminal court that can end impunity for the most serious crimes of international concern without trampling state sovereignty and the principle of non-intervention.
% FOUNDING_PROBLEM_CORROBORATION: State delegations at the 1998 Rome Conference attest the problem from the sovereignty-preserving seat. Universalist scholars and human rights NGOs attest that the problem has shifted and the arrangement now shields perpetrators in non-party states; UN General Assembly debates on universal jurisdiction and the independent ICC OTP annual reports corroborate the contested status from outside the benefiting parties.
narrative_ontology:disappearance_verdict(rome_statute_jurisdiction__sovereigntist_reading, world_rearranges).
narrative_ontology:founding_problem_status(rome_statute_jurisdiction__sovereigntist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rome_statute_jurisdiction__sovereigntist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(rome_statute_jurisdiction__sovereigntist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rome_statute_jurisdiction__sovereigntist_reading, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rome_statute_jurisdiction__sovereigntist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rome_statute_jurisdiction__sovereigntist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rome_statute_jurisdiction__sovereigntist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) is authored at moderate-high levels because the sovereigntist framework systematically transfers the capacity to shield perpetrators from international accountability back to states, particularly non-party states. Suppression (0.52) reflects the legal suppression of universal jurisdiction claims that would otherwise apply; the Rome Statute's consent architecture displaces alternative justice pathways. Theater ratio (0.25) is low: the legal procedures (admissibility hearings, complementarity determinations) are substantively functional, though some proceedings risk performative domestic sham trials designed to satisfy complementarity while shielding perpetrators. Accessibility collapse (0.70) is high because once a situation is routed through the Rome Statute framework, victims and prosecutors cannot bypass the consent/complementarity gates to reach direct international prosecution. Resistance (0.48) captures sustained universalist critique, African Union pushback against selective application, and scholarly contestation of the consent framework.
 *
 * PERSPECTIVAL GAP:
 *   The sovereigntist state seat perceives the constraint as a rope â a voluntary coordination mechanism that respects sovereignty while creating a court for consenting states. The victims' seat and the OTP seat perceive the same structure as extractive: the consent requirement blocks accountability pathways that would exist under a universalist or hybrid reading. The engine computes this divergence from the structural data â the consent framework that coordinates state participation simultaneously extracts justice access from victims.
 *
 * DIRECTIONALITY LOGIC:
 *   Sovereign states, non-party states, and national judiciaries are declared beneficiaries (low directionality: the constraint subsidizes their sovereignty and deference). Victims of atrocities are declared victims (high directionality: the constraint extracts justice access from them). The OTP is not declared in the victim array but is structurally a payer; the engine will derive its directionality from its constrained exit and non-beneficiary status, placing it toward the target end. Universalist scholars are excluded; their analytical exit keeps them from being targets.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was ending impunity for the most serious crimes while respecting sovereignty. Under the sovereigntist reading, the sovereignty-protection function has not atrophied; rather, it dominates. However, if the impunity-ending function is evaluated independently, it is partly dead: the consent framework leaves significant impunity gaps (non-party states, unwilling but able states). The R5 genealogy flags this as a contested founding problem status. The classification as tangled_rope prevents mislabeling the arrangement as pure coordination (rope) by naming the victims who pay for sovereignty, and prevents mislabeling it as pure extraction (snare) by acknowledging the genuine coordination function among consenting states.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consent_vs_universal_jurisdiction_ambiguity,
    'Is state consent a legitimate prerequisite for international criminal jurisdiction, or does it constitute a procedural shield for non-party perpetrators?',
    'Analysis of state practice and opinio juris on universal jurisdiction versus treaty-based jurisdiction, including ICJ and ICC jurisprudence on jurisdictional gaps.',
    'If consent is not required as a matter of structural legitimacy, the sovereigntist reading collapses and the constraint reclassifies toward snare or tangled_rope with higher extraction; if required, the coordination function is validated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_vs_universal_jurisdiction_ambiguity, conceptual, 'Whether the consent framework is a legitimate legal gate or an impunity shield.').

omega_variable(
    complementarity_genuineness,
    'Are national proceedings triggered by complementarity genuinely effective, or do states routinely invoke willingness or ability to shield perpetrators?',
    'Empirical tracking of complementarity admissibility proceedings and domestic prosecutions of Rome Statute crimes across all ICC situations.',
    'If states systematically abuse complementarity, the constraint''s coordination function is hollow and the extraction from victims dominates the classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(complementarity_genuineness, empirical, 'Whether complementarity functions as genuine deference or as a sham-trial shield.').

omega_variable(
    unsc_referral_politicization,
    'Does the UNSC referral mechanism function as a neutral enforcement gate or as a geopolitical instrument that reinforces asymmetry?',
    'Statistical analysis of UNSC referrals and vetoes in situations involving non-party states, correlated with permanent-member interests.',
    'If referrals are systematically blocked for powerful non-parties, the constraint''s enforcement is asymmetric and the sovereigntist framework becomes more extractive by shielding geopolitically favored states.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(unsc_referral_politicization, empirical, 'Whether UNSC referrals neutralize the non-party immunity gap or reinforce geopolitical asymmetry.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rome_statute_jurisdiction__sovereigntist_reading, 0, 22).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rome_statute_jsr_tr_t0, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(rome_statute_jsr_tr_t4, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 4, 0.15).
narrative_ontology:measurement(rome_statute_jsr_tr_t8, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement(rome_statute_jsr_tr_t12, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 12, 0.2).
narrative_ontology:measurement(rome_statute_jsr_tr_t16, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 16, 0.22).
narrative_ontology:measurement(rome_statute_jsr_tr_t20, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 20, 0.24).
narrative_ontology:measurement(rome_statute_jsr_tr_t22, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 22, 0.25).

% Extraction over time
narrative_ontology:measurement(rome_statute_jsr_be_t0, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(rome_statute_jsr_be_t4, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 4, 0.46).
narrative_ontology:measurement(rome_statute_jsr_be_t8, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(rome_statute_jsr_be_t12, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 12, 0.53).
narrative_ontology:measurement(rome_statute_jsr_be_t16, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(rome_statute_jsr_be_t20, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 20, 0.57).
narrative_ontology:measurement(rome_statute_jsr_be_t22, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 22, 0.58).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(rome_statute_jurisdiction__sovereigntist_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(rome_statute_jurisdiction__sovereigntist_reading, universalist_reading).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__sovereigntist_reading, hybrid_complementarity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Rome Statute jurisdiction kernel. It is decomposed from the colloquial label 'Rome Statute jurisdiction' because the sovereigntist, universalist, and hybrid readings produce structurally distinct epsilon values, beneficiary/victim structures, and classifications. See sibling constraints for the other readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
