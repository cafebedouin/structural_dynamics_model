% ============================================================================
% CONSTRAINT STORY: rome_statute_jurisdiction__sovereigntist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: Rome Statute Jurisdiction — Sovereigntist (Consent-Conditional) Reading
 *   domain: international_law/treaty_interpretation/institutional_authority
 *
 * SUMMARY:
 *   This story instantiates the sovereigntist reading of the Rome Statute's
 *   jurisdictional kernel: ICC authority is treated as strictly conditional
 *   on state consent (ratification, ad hoc declaration, or
 *   territorial/national linkage) or Security Council referral, with
 *   complementarity read as a strong presumption of deference to national
 *   courts rather than a genuine check on impunity. This is one reading among
 *   three siblings (universalist_reading, hybrid_complementarity_reading)
 *   sharing the same kernel text and drafting history but instantiating
 *   structurally distinct constraints with distinct beneficiary/victim sets
 *   and distinct ε trajectories — per the ε-invariance principle, each is
 *   authored as its own story rather than as an observable-dependent variant
 *   of one story.
 *
 * KEY AGENTS:
 *   - non_party_great_powers: primary beneficiary (institutional/arbitrage) — categorically immune absent referral
 *   - ratifying_states_shielding_nationals: beneficiary and co-agenda-setter (institutional/constrained) — retains first-and-final prosecutorial say via complementarity deference
 *   - icc_prosecutorial_office: primary payer (organized/constrained) — jurisdictional reach set by the states most likely to be investigated
 *   - atrocity_victims_in_non_referred_situations: primary victim (powerless/trapped) — no accountability path under this reading
 *   - un_security_council_permanent_members: gatekeeper agenda-setter (institutional/arbitrage) — sole non-consent pathway, subject to veto
 *   - international_law_scholars_and_treaty_bodies: analytical observer — adjudicates between competing kernel readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rome_statute_jurisdiction__sovereigntist_reading, 0.38).
domain_priors:suppression_score(rome_statute_jurisdiction__sovereigntist_reading, 0.42).
domain_priors:theater_ratio(rome_statute_jurisdiction__sovereigntist_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rome_statute_jurisdiction__sovereigntist_reading, tangled_rope).
narrative_ontology:human_readable(rome_statute_jurisdiction__sovereigntist_reading, "Rome Statute Jurisdiction — Sovereigntist (Consent-Conditional) Reading").
narrative_ontology:topic_domain(rome_statute_jurisdiction__sovereigntist_reading, "international_law/treaty_interpretation/institutional_authority").

domain_priors:requires_active_enforcement(rome_statute_jurisdiction__sovereigntist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rome_statute_jurisdiction__sovereigntist_reading, '48bf8735-40d0-4568-8ff0-55f998bef7ca').
narrative_ontology:cs_kernel_codification('48bf8735-40d0-4568-8ff0-55f998bef7ca', fixed_text).
narrative_ontology:cs_authority_grounding('48bf8735-40d0-4568-8ff0-55f998bef7ca', lineage).
narrative_ontology:cs_interpretation_layer_present('48bf8735-40d0-4568-8ff0-55f998bef7ca').
narrative_ontology:cs_reading_relation('48bf8735-40d0-4568-8ff0-55f998bef7ca', rome_statute_jurisdiction__universalist_reading, forecloses).
narrative_ontology:cs_reading_relation('48bf8735-40d0-4568-8ff0-55f998bef7ca', rome_statute_jurisdiction__hybrid_complementarity_reading, influences).
narrative_ontology:cs_axiom('48bf8735-40d0-4568-8ff0-55f998bef7ca', foundational, jurisdiction_requires_affirmative_state_consent).
narrative_ontology:cs_axiom_status(jurisdiction_requires_affirmative_state_consent, holdable).
narrative_ontology:cs_axiom_grounding('48bf8735-40d0-4568-8ff0-55f998bef7ca', jurisdiction_requires_affirmative_state_consent, conventional).
narrative_ontology:cs_axiom('48bf8735-40d0-4568-8ff0-55f998bef7ca', foundational, complementarity_as_strong_deference_presumption).
narrative_ontology:cs_axiom_status(complementarity_as_strong_deference_presumption, holdable).
narrative_ontology:cs_axiom_grounding('48bf8735-40d0-4568-8ff0-55f998bef7ca', complementarity_as_strong_deference_presumption, conventional).
narrative_ontology:cs_axiom('48bf8735-40d0-4568-8ff0-55f998bef7ca', secondary, national_prosecution_as_default_forum).
narrative_ontology:cs_axiom_status(national_prosecution_as_default_forum, holdable).
narrative_ontology:cs_axiom_grounding('48bf8735-40d0-4568-8ff0-55f998bef7ca', national_prosecution_as_default_forum, instrumental).
narrative_ontology:cs_reference_frame('48bf8735-40d0-4568-8ff0-55f998bef7ca', consent_based_treaty_sovereignty).
narrative_ontology:cs_drift_state('48bf8735-40d0-4568-8ff0-55f998bef7ca', post_afr_referral_crisis_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('48bf8735-40d0-4568-8ff0-55f998bef7ca', '').
narrative_ontology:cs_kernel_id(rome_statute_jurisdiction__sovereigntist_reading, rome_statute_jurisdiction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__sovereigntist_reading, non_party_great_powers).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__sovereigntist_reading, ratifying_states_shielding_nationals).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__sovereigntist_reading, national_judiciaries).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__sovereigntist_reading, atrocity_victims_in_non_referred_situations).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__sovereigntist_reading, icc_prosecutorial_office).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__sovereigntist_reading, weaker_ratifying_states_facing_asymmetric_scrutiny).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Have declined to ratify and, under this reading, their nationals are categorically immune from ICC jurisdiction absent a UN Security Council referral they can veto. They benefit from the consent architecture without bearing any of its obligations, and can invoke or block referrals affecting others while remaining structurally untouchable themselves.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, non_party_great_powers, beneficiary,
    institutional, generational, arbitrage, global).

% Have ratified but invoke complementarity as a strong deference doctrine: so long as a national investigation is nominally underway, the ICC must stand down. This reading lets them retain first and effectively final say over prosecuting their own nationals, using treaty membership's legitimacy benefits while minimizing supranational exposure.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, ratifying_states_shielding_nationals, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(rome_statute_jurisdiction__sovereigntist_reading, ratifying_states_shielding_nationals, agenda_setter).

% Retain primary and, in this reading, near-presumptive authority to investigate and prosecute international crimes committed by their nationals or on their territory. The ICC's role is triggered only on genuine unwillingness or inability, interpreted narrowly in favor of the national forum.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, national_judiciaries, beneficiary,
    institutional, generational, constrained, national).

% Operates under a jurisdictional gate that requires either territorial/national consent or a Security Council referral before it can act. Cannot open investigations into non-party nationals for conduct on non-party territory even when atrocities are documented, and must accept national admissibility challenges even where the national process is widely viewed as inadequate. Its practical reach is set by the very states most likely to be investigated.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, icc_prosecutorial_office, payer,
    organized, biographical, constrained, global).

% Suffer crimes committed by nationals of non-consenting states, or crimes where a Security Council veto blocks referral. Under this reading they have no path to ICC accountability regardless of the gravity of the conduct, because jurisdiction is conditioned entirely on state consent or great-power political will they cannot influence.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, atrocity_victims_in_non_referred_situations, payer,
    powerless, biographical, trapped, local).

% Having ratified, they are the states most likely to actually face ICC scrutiny in practice, since powerful non-parties are shielded and Security Council referrals of powerful states rarely pass. They bear the practical weight of the regime's enforcement while enjoying none of the immunity that non-ratification or veto power provides to others.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, weaker_ratifying_states_facing_asymmetric_scrutiny, payer,
    moderate, generational, constrained, national).

% Control the sole non-consent pathway to jurisdiction over non-party nationals via referral, and each holds a veto over any referral including of themselves or allies. This reading treats their gatekeeping role as legitimate and central rather than as a design flaw, reinforcing their control over which atrocities receive international attention.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, un_security_council_permanent_members, agenda_setter,
    institutional, generational, arbitrage, global).

% Analyze the text, travaux préparatoires, and state practice to adjudicate between competing readings of the Statute's jurisdictional architecture. They document how consent-based limits interact with complementarity and referral mechanisms, without themselves being bound by or benefiting materially from any particular reading.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, international_law_scholars_and_treaty_bodies, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides states a mechanism to commit credibly to international criminal accountability while retaining a veto over supranational intrusion into domestic prosecutorial sovereignty — allowing ratification without full surrender of adjudicative authority, which under this reading was necessary to secure broad state participation at all.
% TRANSFER_FUNCTION: Moves jurisdictional authority and enforcement discretion toward consenting states and non-party powers with Security Council leverage, and away from the ICC prosecutorial office and from victims in situations where no state consents and no referral issues. Practical enforcement burden shifts disproportionately onto weaker ratifying states.
% ABSENT_VOICES: Victims of atrocities committed by nationals of non-party states have no seat in the jurisdictional design and cannot trigger accountability through any mechanism this reading recognizes as legitimate; their exclusion is treated as a feature of sovereign consent rather than a gap to be closed.
% DISAPPEARANCE_RATIONALE: If the consent-conditional architecture were abolished overnight in favor of universal jurisdiction, ratifying states would face the prospect of losing the sovereignty-preserving bargain that induced their ratification in the first place, potentially triggering withdrawals; non-party powers would face genuinely novel legal exposure. Sovereigntist-reading proponents hold the world would destabilize (states exit the regime entirely); universalist-reading proponents hold accountability would simply become real for the first time. The verdict itself is a site of the kernel contest.
% FOUNDING_PROBLEM: States negotiating the Rome Statute in the 1990s needed a mechanism to prosecute the gravest international crimes when national systems failed, but many prospective parties — and especially major military powers — would not ratify a court with jurisdiction over their nationals absent their consent, so the drafters built a consent-gated jurisdictional trigger to make ratification politically possible.
% FOUNDING_PROBLEM_CORROBORATION: Diplomatic historians and delegates present at the 1998 Rome Conference (documented in the conference travaux préparatoires) attest that consent-based jurisdictional triggers were the explicit price of achieving the broad ratification needed for the Statute to exist at all — a corroboration independent of any state currently invoking the sovereigntist reading. Independent international law scholarship is split: some treat the founding problem (universal accountability) as still unmet by a consent-gated regime, others treat the consent gate itself as the founding compromise now functioning as designed.
narrative_ontology:disappearance_verdict(rome_statute_jurisdiction__sovereigntist_reading, contested).
narrative_ontology:founding_problem_status(rome_statute_jurisdiction__sovereigntist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rome_statute_jurisdiction__sovereigntist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(rome_statute_jurisdiction__sovereigntist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rome_statute_jurisdiction__sovereigntist_reading, 0.38, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction is moderate (0.38) rather than high: the sovereigntist reading is not primarily rent-extraction but a jurisdictional carve-out that redistributes accountability burden asymmetrically toward weaker ratifying states while insulating powerful non-parties — a real but bounded transfer. Suppression (0.42) reflects the active diplomatic and legal machinery (admissibility challenges, non-cooperation, veto threats) required to maintain the consent gate against expanding-jurisdiction pressure from universalist advocates. Theater ratio (0.30) is moderate: complementarity proceedings are sometimes genuine national prosecutions and sometimes performative shields, and this reading treats even nominal domestic process as sufficient, inflating the theatrical share over time as the metric shows.
 *
 * DIRECTIONALITY LOGIC:
 *   Non-party great powers and shielding ratifying states sit near the beneficiary end: they collect the coordination good (a functioning accountability regime they can point to) without bearing meaningful supranational exposure, and their exit options are effectively arbitrage-grade (never join, or join and then contest admissibility). The ICC prosecutorial office and victims in non-referred situations sit near the target end: the office cannot act without the consent it structurally lacks, and the victims cannot influence consent or referral at all. Weaker ratifying states occupy an intermediate but tilted position — nominally symmetric participants who in practice bear disproportionate enforcement exposure precisely because they lack the leverage to block referrals or dictate admissibility outcomes the way powerful states can.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (credible international accountability for atrocity crimes) remains partly live but the sovereigntist reading's mechanism for solving it — consent-gating — has drifted from bridging device (get powerful states to eventually join) toward permanent shield (powerful states never join, and the gate persists indefinitely). Whether this constitutes mandatrophy or the sovereigntist reading's own vindication depends on which reading of the kernel one holds, which is exactly the contest this story is one instance of, not a fact this story adjudicates.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consent_gate_as_bridge_or_permanent_shield,
    'Is the consent-conditional jurisdictional gate a transitional bridging mechanism intended to expand toward universal participation over time, or a permanent structural feature that the sovereigntist reading holds was always meant to be load-bearing indefinitely?',
    'Examine whether non-party great powers show any trajectory toward ratification over multi-decade horizons, versus entrenchment of non-ratification as a stable equilibrium; also examine whether Rome Conference drafters'' own statements support a transitional or permanent reading.',
    'If transitional, the sovereigntist reading''s current operation would look like an arrested scaffold rather than the mountain-like natural reading of consent it claims to be; if permanent by original design, it supports the reading''s own self-characterization as principled rather than merely strategic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_gate_as_bridge_or_permanent_shield, conceptual, 'Whether the consent gate is a bridge mechanism or a permanent structural feature.').

omega_variable(
    sovereignty_claim_vs_shielding_function,
    'Is the sovereigntist reading''s emphasis on state consent a principled claim about legitimate international lawmaking, or does it function primarily to shield identifiable beneficiaries (non-party great powers, shielding ratifying states) from accountability regardless of the underlying principle''s validity?',
    'Compare invocation patterns: does the consent principle get invoked symmetrically across strong and weak states, or does it correlate with which states have the political and military capacity to resist supranational jurisdiction regardless of the principle''s abstract merits?',
    'If invocation correlates strongly with power rather than principle, the reading functions as tangled_rope (a genuine consent-based coordination logic captured asymmetrically by powerful non-parties) rather than as a neutral doctrinal position; this bears directly on the beneficiary declarations authored here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_claim_vs_shielding_function, empirical, 'Whether sovereigntist consent doctrine is principle-driven or power-correlated in application.').

omega_variable(
    kernel_reading_selection_evidence,
    'What in the Rome Statute''s text, negotiating history, and subsequent state practice most supports treating the sovereigntist reading (rather than the universalist or hybrid_complementarity readings) as the operative structural claim, versus these being genuinely coexisting, unresolved interpretive positions?',
    'Systematic review of Rome Conference travaux préparatoires, ICC Appeals Chamber jurisprudence on admissibility and complementarity, and comparative state practice across ratifying and non-ratifying states over the Statute''s operative history.',
    'If state practice and jurisprudence trend toward the hybrid reading''s genuine-balancing interpretation of complementarity, this sovereigntist story''s strong-deference framing would be the less descriptively accurate of the coexisting readings, though not thereby invalid as an authored structural claim held by real state parties.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_evidence, conceptual, 'Evidentiary basis for treating the sovereigntist reading as structurally operative among coexisting kernel readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rome_statute_jurisdiction__sovereigntist_reading, 1998, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rome_tr_t1998, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 1998, 0.15).
narrative_ontology:measurement(rome_tr_t2002, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 2002, 0.18).
narrative_ontology:measurement(rome_tr_t2008, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 2008, 0.22).
narrative_ontology:measurement(rome_tr_t2014, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 2014, 0.26).
narrative_ontology:measurement(rome_tr_t2019, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 2019, 0.28).
narrative_ontology:measurement(rome_tr_t2024, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 2024, 0.3).

% Extraction over time
narrative_ontology:measurement(rome_be_t1998, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 1998, 0.22).
narrative_ontology:measurement(rome_be_t2002, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 2002, 0.26).
narrative_ontology:measurement(rome_be_t2008, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 2008, 0.3).
narrative_ontology:measurement(rome_be_t2014, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 2014, 0.33).
narrative_ontology:measurement(rome_be_t2019, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 2019, 0.36).
narrative_ontology:measurement(rome_be_t2024, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 2024, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(rome_su_t1998, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 1998, 0.3).
narrative_ontology:measurement(rome_su_t2002, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 2002, 0.33).
narrative_ontology:measurement(rome_su_t2008, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 2008, 0.36).
narrative_ontology:measurement(rome_su_t2014, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 2014, 0.39).
narrative_ontology:measurement(rome_su_t2019, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 2019, 0.41).
narrative_ontology:measurement(rome_su_t2024, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 2024, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rome_statute_jurisdiction__sovereigntist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(rome_statute_jurisdiction__sovereigntist_reading, 0.12).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__sovereigntist_reading, universalist_reading).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__sovereigntist_reading, hybrid_complementarity_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the rome_statute_jurisdiction kernel. universalist_reading treats ICC jurisdiction as a mandate transcending sovereign consent (lower ε, different beneficiary/victim structure emphasizing victim access over state prerogative); hybrid_complementarity_reading treats complementarity as a genuine balancing test rather than pure deference (intermediate ε). All three share the same underlying treaty text but instantiate structurally distinct constraints per the ε-invariance principle — each carries its own extraction profile, its own stakeholder set, and its own classification, linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
