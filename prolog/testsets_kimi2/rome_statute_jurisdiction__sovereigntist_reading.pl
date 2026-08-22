% ============================================================================
% CONSTRAINT STORY: rome_statute_jurisdiction__sovereigntist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: rome_statute_jurisdiction__sovereigntist_reading
 *   human_readable: Rome Statute Jurisdictional Framework â Sovereigntist Reading
 *   domain: international_law/treaty_interpretation/institutional_authority
 *
 * SUMMARY:
 *   This constraint story instantiates the sovereigntist reading of the Rome
 *   Statute jurisdiction kernel. Under this reading, the Rome Statute creates
 *   a conditional framework in which ICC jurisdiction is strictly limited to
 *   state consent, UN Security Council referral, or ad hoc acceptance.
 *   Non-party nationals enjoy immunity unless the Council acts, and national
 *   courts retain primary authority under complementarity conceived as
 *   deference rather than override. The reading treats the arrangement as a
 *   coordination mechanism among sovereign states that preserves Westphalian
 *   ordering while establishing a permanent international criminal court.
 *   However, the same structure asymmetrically excludes atrocity victims in
 *   non-consenting states from international judicial access, generating
 *   measurable extraction from the most vulnerable parties while benefiting
 *   state sovereignty holders. The constraint requires active enforcement
 *   through ICC prosecutorial filtering, Pre-Trial Chamber admissibility
 *   determinations, and ongoing complementarity negotiations.
 *
 * KEY AGENTS:
 *   - State Parties (institutional/constrained) â beneficiaries of a sovereignty-respecting court system
 *   - Non-Party States (institutional/arbitrage) â beneficiaries of immunity from jurisdiction
 *   - Atrocity Victims in Non-Party States (powerless/trapped) â bear the cost of the consent gap
 *   - ICC Prosecutor (institutional/constrained) â enforces jurisdictional boundaries
 *   - UN Security Council (institutional/arbitrage) â selective override via referral
 *   - Universalist Advocates (organized/constrained) â structurally excluded from the framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rome_statute_jurisdiction__sovereigntist_reading, 0.42).
domain_priors:suppression_score(rome_statute_jurisdiction__sovereigntist_reading, 0.48).
domain_priors:theater_ratio(rome_statute_jurisdiction__sovereigntist_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rome_statute_jurisdiction__sovereigntist_reading, tangled_rope).
narrative_ontology:human_readable(rome_statute_jurisdiction__sovereigntist_reading, "Rome Statute Jurisdictional Framework â Sovereigntist Reading").
narrative_ontology:topic_domain(rome_statute_jurisdiction__sovereigntist_reading, "international_law/treaty_interpretation/institutional_authority").

domain_priors:requires_active_enforcement(rome_statute_jurisdiction__sovereigntist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rome_statute_jurisdiction__sovereigntist_reading, '9f8cc9fa-57d0-467b-9dc4-3fa6857d64d4').
narrative_ontology:cs_kernel_codification('9f8cc9fa-57d0-467b-9dc4-3fa6857d64d4', formalized).
narrative_ontology:cs_authority_grounding('9f8cc9fa-57d0-467b-9dc4-3fa6857d64d4', lineage).
narrative_ontology:cs_interpretation_layer_present('9f8cc9fa-57d0-467b-9dc4-3fa6857d64d4').
narrative_ontology:cs_reading_relation('9f8cc9fa-57d0-467b-9dc4-3fa6857d64d4', rome_statute_jurisdiction__universalist_reading, forecloses).
narrative_ontology:cs_reading_relation('9f8cc9fa-57d0-467b-9dc4-3fa6857d64d4', rome_statute_jurisdiction__hybrid_complementarity_reading, influences).
narrative_ontology:cs_axiom('9f8cc9fa-57d0-467b-9dc4-3fa6857d64d4', foundational, state_consent_prerequisite_for_jurisdiction).
narrative_ontology:cs_axiom_status(state_consent_prerequisite_for_jurisdiction, holdable).
narrative_ontology:cs_axiom_grounding('9f8cc9fa-57d0-467b-9dc4-3fa6857d64d4', state_consent_prerequisite_for_jurisdiction, conventional).
narrative_ontology:cs_axiom('9f8cc9fa-57d0-467b-9dc4-3fa6857d64d4', foundational, national_court_primacy_over_international_adjudication).
narrative_ontology:cs_axiom_status(national_court_primacy_over_international_adjudication, holdable).
narrative_ontology:cs_axiom_grounding('9f8cc9fa-57d0-467b-9dc4-3fa6857d64d4', national_court_primacy_over_international_adjudication, conventional).
narrative_ontology:cs_reference_frame('9f8cc9fa-57d0-467b-9dc4-3fa6857d64d4', state_consent_international_order).
narrative_ontology:cs_drift_state('9f8cc9fa-57d0-467b-9dc4-3fa6857d64d4', contemporary_icc_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9f8cc9fa-57d0-467b-9dc4-3fa6857d64d4', '').
narrative_ontology:cs_kernel_id(rome_statute_jurisdiction__sovereigntist_reading, rome_statute_jurisdiction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__sovereigntist_reading, state_parties).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__sovereigntist_reading, non_party_states).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__sovereigntist_reading, atrocity_victims_in_non_party_states).
narrative_ontology:constraint_vindicates(rome_statute_jurisdiction__sovereigntist_reading, westphalian_sovereignty).
narrative_ontology:constraint_vindicates(rome_statute_jurisdiction__sovereigntist_reading, treaty_consent_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Ratified the Rome Statute and participate in the Assembly of States Parties. They gain a permanent international criminal court that can prosecute atrocities on their territory or by their nationals, but only with their consent or under specific conditions. They coordinate with other states to fund and govern the Court while retaining sovereign control over jurisdictional reach.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, state_parties, beneficiary,
    institutional, generational, constrained, global).

% Have not ratified the Rome Statute and are shielded from ICC jurisdiction over their nationals and territory unless the UN Security Council refers a situation. They benefit from sovereignty protection while still being able to engage with the Court ad hoc or accept jurisdiction for specific situations.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, non_party_states, beneficiary,
    institutional, generational, arbitrage, global).

% Suffer crimes within the jurisdiction of the ICC but are barred from access to the Court because their state has not consented and the UN Security Council has not referred the situation. They bear the cost of impunity when national courts fail and international jurisdiction is blocked by the consent gap.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, atrocity_victims_in_non_party_states, payer,
    powerless, immediate, trapped, local).

% Operates within the Office of the Prosecutor to assess whether situations meet the jurisdictional thresholds of the Rome Statute, including state consent, admissibility, and complementarity. Must filter out situations lacking territorial or personal jurisdiction under the Statute's consent framework, actively enforcing the jurisdictional boundaries even when political pressure favors expansion.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, icc_prosecutor, agenda_setter,
    institutional, biographical, constrained, global).

% Can refer situations involving non-party states to the ICC Prosecutor under Article 13(b), bypassing the consent requirement. This power is exercised selectively and politically, serving as the sole enforcement bridge between the sovereigntist framework and universal jurisdiction for non-party situations.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, un_security_council, agenda_setter,
    institutional, immediate, arbitrage, global).

% Human rights organizations and legal scholars advocating for universal jurisdiction and automatic ICC access regardless of state consent. They are structurally excluded from the sovereigntist framework's decision-making architecture, as their preferred jurisdictional pathway is barred by the consent requirement.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, universalist_advocates, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates sovereign states into a permanent international criminal court system by grounding jurisdiction in state consent, creating a shared legal infrastructure for prosecuting atrocity crimes while preserving Westphalian sovereignty.
% TRANSFER_FUNCTION: Moves the authority to prosecute the most serious international crimes from the international community's universal jurisdiction claims to state-consent-based adjudication; transfers the cost of impunity from state parties to atrocity victims in non-party states when national courts fail.
% ABSENT_VOICES: Victims in non-party states without UNSC referral are structurally absent from the courtroom; universalist legal scholars and affected communities in states like Myanmar, Syria, and Afghanistan are excluded from the jurisdictional architecture by the consent barrier they would reject.
% DISAPPEARANCE_RATIONALE: If the sovereigntist consent framework vanished overnight, the ICC would either expand to a universal jurisdiction model or collapse as non-party states withdrew cooperation; international criminal law would reorganize around either a UN-backed tribunal system or ad hoc hybrid courts, and the current treaty-based coordination among states parties would fragment.
% FOUNDING_PROBLEM: The absence of a permanent international criminal court meant atrocity prosecutions depended on ad hoc tribunals or victorious powers' justice, producing inconsistent standards and leaving gaps when great powers blocked action.
% FOUNDING_PROBLEM_CORROBORATION: State parties and the UN Secretary-General attest the problem persists. However, critics from non-party states and some international law scholars attest the founding problem has been addressed for state-party situations while the sovereigntist framework itself creates new gaps for non-party victims; the ad hoc tribunal era's critics corroborate the original problem, but victim-rights advocates outside the state-party bloc contest that the current arrangement solves it.
narrative_ontology:disappearance_verdict(rome_statute_jurisdiction__sovereigntist_reading, world_rearranges).
narrative_ontology:founding_problem_status(rome_statute_jurisdiction__sovereigntist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rome_statute_jurisdiction__sovereigntist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(rome_statute_jurisdiction__sovereigntist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rome_statute_jurisdiction__sovereigntist_reading, 0.42, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rome_statute_jurisdiction__sovereigntist_reading_tests).
:- end_tests(rome_statute_jurisdiction__sovereigntist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) reflects the consent gap: victims in non-party states are systematically denied ICC access unless the UNSC intervenes, which is rare and selective. Suppression (0.48) captures how the framework suppresses universal jurisdiction alternatives by establishing the ICC as the primary legitimate forum while blocking its reach. Theater ratio (0.25) is relatively low because the legal reasoning is functional, though complementarity arguments occasionally serve to shield politically sensitive suspects. Accessibility collapse (0.40) is moderate: alternatives like ad hoc tribunals or universal jurisdiction exist but are costlier and politically harder to activate once the ICC framework is understood as the default. Resistance (0.52) is moderate-to-high from universalist advocates and non-party states resisting expansive ICC interpretation. Temporal measurements show slowly rising extractiveness as the Court's caseload expands and jurisdictional boundaries become more contested.
 *
 * PERSPECTIVAL GAP:
 *   State parties and non-party states experience the constraint as sovereignty protection (low directionality, beneficiary seats), while atrocity victims in non-party states experience it as a hard barrier to justice (high directionality, payer seat). The ICC Prosecutor sits in between: structurally constrained by the consent framework but also administering it. The engine computes this divergence from the structural data â the sovereigntist claim of legitimate coordination does not erase the asymmetric cost on excluded victims.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to state parties and non-party states, who collect sovereignty protection and jurisdictional control. Victim declarations map to excluded atrocity victims, who pay through denial of access. The UNSC has arbitrage-grade exit (can refer or block), placing it near the beneficiary end. The ICC Prosecutor is constrained by the framework they enforce, giving them a mixed directionality. Universalist advocates are excluded entirely, mapping to high directionality if they were subject to the constraint, but as excluded observers they sit outside the derivation chain.
 *
 * MANDATROPHY ANALYSIS:
 *   The sovereigntist reading prevents mislabeling by separating the genuine coordination function (treaty-based international criminal justice among consenting states) from the extraction function (impunity for non-party situations). Without this distinction, the framework could be misread as pure extraction (snare) by victims or as pure coordination (rope) by states. The Tangled Rope classification captures that both are structurally present: the coordination is real, but the extraction is built into the same consent architecture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    icc_jurisdiction_consent_scope,
    'Does the Rome Statute''s territorial jurisdiction under Article 12 permit the Court to exercise jurisdiction over crimes committed on a state party''s territory even when the accused is a national of a non-party state?',
    'Advisory opinion from the International Court of Justice or amendment to the Rome Statute clarifying Article 12(2)(a) vs (b) interplay; empirical tracking of state reactions to expansive ICC interpretations.',
    'If territorial jurisdiction fully overrides nationality immunity for non-party nationals, the sovereigntist reading''s core claim is substantially eroded and effective extraction shifts toward non-party states.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(icc_jurisdiction_consent_scope, conceptual, 'Territorial jurisdiction over non-party nationals').

omega_variable(
    complementarity_deference_or_override,
    'Does the complementarity mechanism under Articles 17-19 function as genuine deference to national courts, or has ICC admissibility practice evolved into an override mechanism?',
    'Quantitative analysis of admissibility rulings measuring deference to national proceedings vs. ICC substitution; state compliance data.',
    'If complementarity operates as override, the sovereigntist reading''s claim of national court primacy is descriptively false and the constraint extracts sovereignty from states parties.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(complementarity_deference_or_override, empirical, 'Whether complementarity is deference or override').

omega_variable(
    unsc_referral_legitimacy,
    'Does UN Security Council referral under Article 13(b) legitimately bridge the consent gap, or does selective Council practice undermine the sovereigntist framework''s coherence?',
    'Pattern analysis of SC referrals (Darfur, Libya) vs. vetoes (Syria, Myanmar pre-2017); state legitimacy surveys.',
    'If referrals are perceived as politicized, the sovereigntist reading loses its proposed alternative pathway for non-party accountability, increasing the extraction from excluded victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unsc_referral_legitimacy, preference, 'UNSC referral legitimacy and selectivity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rome_statute_jurisdiction__sovereigntist_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rome_tr_t0, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(rome_tr_t4, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 4, 0.15).
narrative_ontology:measurement(rome_tr_t8, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement(rome_tr_t12, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 12, 0.22).
narrative_ontology:measurement(rome_tr_t16, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 16, 0.28).
narrative_ontology:measurement(rome_tr_t20, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 20, 0.32).

% Extraction over time
narrative_ontology:measurement(rome_be_t0, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(rome_be_t4, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 4, 0.34).
narrative_ontology:measurement(rome_be_t8, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 8, 0.36).
narrative_ontology:measurement(rome_be_t12, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 12, 0.4).
narrative_ontology:measurement(rome_be_t16, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 16, 0.46).
narrative_ontology:measurement(rome_be_t20, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 20, 0.5).

% Suppression requirement over time
narrative_ontology:measurement(rome_su_t0, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(rome_su_t4, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 4, 0.42).
narrative_ontology:measurement(rome_su_t8, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 8, 0.45).
narrative_ontology:measurement(rome_su_t12, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 12, 0.48).
narrative_ontology:measurement(rome_su_t16, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 16, 0.52).
narrative_ontology:measurement(rome_su_t20, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 20, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rome_statute_jurisdiction__sovereigntist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__sovereigntist_reading, rome_statute_jurisdiction__universalist_reading).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__sovereigntist_reading, rome_statute_jurisdiction__hybrid_complementarity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the rome_statute_jurisdiction kernel, decomposed per the Îµ-invariance principle because the natural-language label 'Rome Statute jurisdiction' conflates structurally distinct claims about consent, universality, and complementarity. The sovereigntist reading isolates the strict consent framework as a distinct constraint with its own Îµ, beneficiaries, and victims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
