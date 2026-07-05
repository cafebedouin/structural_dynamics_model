% ============================================================================
% CONSTRAINT STORY: rome_statute_jurisdiction__universalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rome_statute_jurisdiction__universalist_reading, []).

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
 *   constraint_id: rome_statute_jurisdiction__universalist_reading
 *   human_readable: Rome Statute Jurisdiction — Universalist Reading (Authority Transcends Consent)
 *   domain: international_law/treaty_interpretation/institutional_authority
 *
 * SUMMARY:
 *   This story instantiates the universalist reading of the Rome Statute
 *   kernel: the claim that the ICC's jurisdiction, once territorial or
 *   Security Council triggers are engaged, extends over non-party nationals
 *   and overrides state consent for core international crimes. This is a
 *   distinct constraint from the sovereigntist reading (which holds
 *   jurisdiction is conditional on consent) and the hybrid complementarity
 *   reading (which holds the Statute deliberately balances universal
 *   aspiration against sovereign primacy). Each reading has its own ε, its
 *   own beneficiary/victim structure, and its own classification; they are
 *   linked here only through network edges and the shared kernel_context, per
 *   the ε-invariance principle. Under this reading specifically, the Statute
 *   is read as asserting a jus cogens-grounded authority to prosecute
 *   atrocity crimes that does not depend on the accused's state having
 *   ratified the treaty — a structurally more extractive and more
 *   suppression-dependent claim than the sovereigntist reading, because it
 *   requires active enforcement (arrest warrants, referrals, diplomatic
 *   pressure) against non-consenting parties to hold.
 *
 * KEY AGENTS:
 *   - icc_prosecutorial_office: administers and defends the universalist jurisdictional claim (institutional/analytical)
 *   - victims_of_atrocity_crimes_globally: primary beneficiary of expanded reach (powerless/trapped)
 *   - non_party_state_nationals_prosecuted: primary target of extraterritorial jurisdiction (moderate/trapped)
 *   - non_party_states_sovereignty_claims: institutional payer whose non-ratification is overridden (powerful/constrained)
 *   - un_security_council: co-agenda-setter whose Chapter VII referrals can trigger jurisdiction regardless of consent, filtered through veto politics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rome_statute_jurisdiction__universalist_reading, 0.42).
domain_priors:suppression_score(rome_statute_jurisdiction__universalist_reading, 0.55).
domain_priors:theater_ratio(rome_statute_jurisdiction__universalist_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rome_statute_jurisdiction__universalist_reading, tangled_rope).
narrative_ontology:human_readable(rome_statute_jurisdiction__universalist_reading, "Rome Statute Jurisdiction — Universalist Reading (Authority Transcends Consent)").
narrative_ontology:topic_domain(rome_statute_jurisdiction__universalist_reading, "international_law/treaty_interpretation/institutional_authority").

domain_priors:requires_active_enforcement(rome_statute_jurisdiction__universalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rome_statute_jurisdiction__universalist_reading, 'e9268694-a3c3-406e-818b-2a18fc6ade8a').
narrative_ontology:cs_kernel_codification('e9268694-a3c3-406e-818b-2a18fc6ade8a', fixed_text).
narrative_ontology:cs_authority_grounding('e9268694-a3c3-406e-818b-2a18fc6ade8a', lineage).
narrative_ontology:cs_interpretation_layer_present('e9268694-a3c3-406e-818b-2a18fc6ade8a').
narrative_ontology:cs_reading_relation('e9268694-a3c3-406e-818b-2a18fc6ade8a', rome_statute_jurisdiction__sovereigntist_reading, forecloses).
narrative_ontology:cs_reading_relation('e9268694-a3c3-406e-818b-2a18fc6ade8a', rome_statute_jurisdiction__hybrid_complementarity_reading, influences).
narrative_ontology:cs_axiom('e9268694-a3c3-406e-818b-2a18fc6ade8a', foundational, jus_cogens_crimes_bind_erga_omnes).
narrative_ontology:cs_axiom_status(jus_cogens_crimes_bind_erga_omnes, holdable).
narrative_ontology:cs_axiom_grounding('e9268694-a3c3-406e-818b-2a18fc6ade8a', jus_cogens_crimes_bind_erga_omnes, deontological).
narrative_ontology:cs_axiom('e9268694-a3c3-406e-818b-2a18fc6ade8a', foundational, territorial_trigger_sufficient_absent_ratification).
narrative_ontology:cs_axiom_status(territorial_trigger_sufficient_absent_ratification, holdable).
narrative_ontology:cs_axiom_grounding('e9268694-a3c3-406e-818b-2a18fc6ade8a', territorial_trigger_sufficient_absent_ratification, conventional).
narrative_ontology:cs_reference_frame('e9268694-a3c3-406e-818b-2a18fc6ade8a', jus_cogens_universal_accountability_framework).
narrative_ontology:cs_drift_state('e9268694-a3c3-406e-818b-2a18fc6ade8a', post_africa_withdrawal_threats_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('e9268694-a3c3-406e-818b-2a18fc6ade8a', '').
narrative_ontology:cs_kernel_id(rome_statute_jurisdiction__universalist_reading, rome_statute_jurisdiction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__universalist_reading, victims_of_atrocity_crimes_globally).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__universalist_reading, icc_prosecutorial_office).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__universalist_reading, human_rights_advocacy_networks).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__universalist_reading, non_party_state_nationals_prosecuted).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__universalist_reading, non_party_states_sovereignty_claims).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__universalist_reading, great_power_militaries_operating_abroad).
narrative_ontology:constraint_vindicates(rome_statute_jurisdiction__universalist_reading, jus_cogens_supremacy_over_state_consent).
narrative_ontology:constraint_vindicates(rome_statute_jurisdiction__universalist_reading, individual_criminal_responsibility_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Initiates investigations and prosecutions, including proprio motu referrals and cases triggered by territorial jurisdiction over non-party nationals who commit crimes on party-state soil, or by UN Security Council referral regardless of any state's consent. Administers and defends the universalist reading of the Statute as the office's operating premise; its institutional relevance depends on jurisdiction reaching beyond ratifying states.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, icc_prosecutorial_office, agenda_setter,
    institutional, generational, analytical, global).

% Persons subjected to genocide, crimes against humanity, or war crimes, often in states that are not ICC parties or whose governments are complicit. Under the universalist reading, their access to accountability does not depend on their state's ratification or consent, since territorial or UNSC triggers can reach perpetrators from non-party states. They have no independent enforcement power and depend entirely on the Court's willingness and capacity to act.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, victims_of_atrocity_crimes_globally, beneficiary,
    powerless, biographical, trapped, global).

% NGOs, legal scholars, and transnational coalitions that lobby for expansive jurisdiction, submit amicus briefs, and document atrocities to feed prosecutorial referrals. They gain standing, funding, and moral authority from a strong universalist reading and can shift their advocacy elsewhere if the Court retrenches, but their institutional project is closely bound to the Statute's expansive interpretation.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, human_rights_advocacy_networks, beneficiary,
    organized, generational, mobile, global).

% Military officers, officials, or combatants from states that never ratified the Statute but face prosecution because their alleged crimes occurred on the territory of a party state, or because the Security Council referred the situation. They never consented to the Court's authority through any domestic ratification process and cannot exit the jurisdictional claim once territorial or UNSC triggers are invoked; arrest warrants can restrict their travel indefinitely.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, non_party_state_nationals_prosecuted, payer,
    moderate, biographical, trapped, national).

% Governments that deliberately declined to ratify the Rome Statute specifically to preserve immunity for their nationals, yet find that territorial jurisdiction over crimes committed by their forces on party-state soil (or Security Council referrals) creates binding exposure anyway. Their non-ratification, intended as an exit option, is structurally overridden by the universalist reading's territorial and UNSC triggers.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, non_party_states_sovereignty_claims, payer,
    powerful, generational, constrained, national).

% Armed forces of major non-party states conducting operations, interventions, or peacekeeping on the territory of ICC member states. Under the universalist reading their personnel are exposed to ICC jurisdiction for alleged crimes despite their home government's refusal to ratify; they respond with status-of-forces agreements, bilateral immunity deals, and political pressure campaigns aimed at the Court, but cannot fully escape the territorial trigger short of withdrawing entirely from operations in party states.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, great_power_militaries_operating_abroad, payer,
    powerful, biographical, constrained, global).

% Can refer situations in any state, party or non-party, to the ICC under Chapter VII authority, activating jurisdiction that overrides the non-party state's consent entirely. Permanent members with veto power can also block referrals or shield allies, meaning the universalist mechanism's actual reach is filtered through great-power politics even as its formal premise claims universal authority.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, un_security_council, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(rome_statute_jurisdiction__universalist_reading, un_security_council, observer).

% Party states whose domestic judicial systems operate alongside ICC jurisdiction under complementarity, but who, under the universalist reading, are also expected to arrest and surrender non-party nationals present on their territory pursuant to ICC warrants, sometimes straining bilateral relations with powerful non-party states.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, ratifying_states_domestic_courts, observer,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a permanent international mechanism to prosecute genocide, crimes against humanity, war crimes, and aggression when no domestic court will or can act, solving the collective-action problem that individual states cannot unilaterally hold foreign perpetrators accountable and that ad hoc tribunals (Nuremberg, ICTY, ICTR) are slow, selective, and non-renewable.
% TRANSFER_FUNCTION: Moves legal exposure and the practical burden of accountability from victims and the international community (who previously had no recourse against non-cooperating states) onto individuals and states that never consented to the Court's authority, via territorial and UN Security Council jurisdictional triggers that operate independent of ratification.
% ABSENT_VOICES: Non-party states whose nationals face prosecution were never in the room for the negotiation of the jurisdictional triggers that now bind them; they would argue that criminal jurisdiction requires either treaty consent or their own domestic prosecution, and that territorial triggers effectively legislate obligations onto non-signatories through the back door of where a crime happens to occur.
% DISAPPEARANCE_RATIONALE: If the universalist jurisdictional reading were abandoned in favor of strict consent, the Court's caseload would shrink dramatically to crimes committed by nationals of the roughly 120 ratifying states; several ongoing investigations touching non-party nationals (arising from conflicts in party-state territory) would collapse, non-party powers would face materially reduced exposure for extraterritorial military conduct, and victims in non-cooperating states would lose their primary recourse to international prosecution.
% FOUNDING_PROBLEM: The absence of any permanent, general-jurisdiction body to prosecute mass atrocity meant perpetrators in weak or complicit states routinely escaped accountability entirely, and prior tribunals (Nuremberg, Tokyo, ICTY, ICTR) were victor's-justice or situation-specific bodies without a durable institutional mandate.
% FOUNDING_PROBLEM_CORROBORATION: Human rights organizations and many international law scholars outside the Court itself attest the founding problem remains substantially live, citing ongoing impunity gaps in non-cooperating states. Non-party governments (notably several permanent Security Council members) and a body of sovereigntist international law scholarship dispute that the problem justifies jurisdiction untethered from consent, arguing the universalist reading has expanded beyond its founding justification into an assertion of general authority the treaty's own drafters did not unambiguously grant.
narrative_ontology:disappearance_verdict(rome_statute_jurisdiction__universalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(rome_statute_jurisdiction__universalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rome_statute_jurisdiction__universalist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(rome_statute_jurisdiction__universalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rome_statute_jurisdiction__universalist_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rome_statute_jurisdiction__universalist_reading_tests).
:- end_tests(rome_statute_jurisdiction__universalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at a moderate 0.42 (rising from 0.22 at founding) because the universalist reading's actual exercised reach has grown gradually as territorial and UNSC-triggered cases accumulated case law (Darfur, Libya referrals; conduct-based warrants touching non-party nationals), but the Court's practical enforcement capacity remains limited by lack of its own police power — most warrants go unexecuted for years. Suppression is authored higher (0.55) and rising because the coercive apparatus needed to sustain the universalist claim (arrest warrant regimes, diplomatic pressure campaigns, non-cooperation findings) has hardened over time even as compliance remains partial. Theater ratio is modest (0.28) reflecting that genuine prosecutions do occur, but a meaningful share of activity is symbolic (warrants that will likely never be executed against powerful non-party nationals) rather than functional accountability.
 *
 * DIRECTIONALITY LOGIC:
 *   Victims of atrocity crimes and rights advocacy networks sit near the beneficiary end: the universalist reading's very point is to make their access to justice independent of their state's cooperation. Non-party state nationals facing prosecution, and the non-party states themselves, sit near the target end: their non-ratification, structurally intended as an exit option, is overridden by territorial and UNSC triggers they never consented to. The ICC prosecutorial office is analytically positioned as agenda-setter — it does not personally collect rents but administers and depends institutionally on the expansive reading holding. The UN Security Council occupies a hybrid seat: institutionally it can trigger jurisdiction over any state, but its permanent members' veto power means the mechanism's universalism is itself filtered through great-power exemption, a tension the story surfaces via omega rather than resolving.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — impunity gaps where domestic courts would not or could not prosecute atrocity crimes — remains genuinely live in many contexts, which argues against treating this as mandatrophy. But the corroboration is contested: the office's own beneficiaries (victims, advocacy networks) attest continued necessity, while non-party governments and sovereigntist scholars argue the universalist reading has drifted from the treaty's negotiated compromise into an assertion of authority the ratifying states did not clearly grant. The disappearance_verdict of world_rearranges (ongoing cases would collapse, exposure would shrink) combined with founding_problem_status of contested is exactly the profile the R5 mismatch consumer is built to flag for review, not resolve by fiat.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consent_transcendence_legitimacy,
    'Does the Rome Statute''s text and negotiating history support jurisdiction over non-party nationals via territorial triggers as a genuine grant of universal authority, or is this an interpretive expansion beyond what ratifying states agreed to bind themselves and others to?',
    'Close textual and travaux préparatoires analysis of Articles 12-13 combined with comparative state practice (which states raised objections at Rome in 1998 versus which acquiesced), plus tracking of subsequent ICJ and domestic constitutional court rulings on the question.',
    'If the universalist reading reflects genuine original intent, its extractive profile toward non-party nationals is better characterized as a designed feature of the coordination mechanism; if it is interpretive overreach, the same jurisdictional exercises look more like unilateral extraction dressed in treaty language, pushing the classification toward snare rather than tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_transcendence_legitimacy, conceptual, 'Whether universalist jurisdiction is original design or interpretive expansion beyond consent.').

omega_variable(
    sibling_reading_delta_sovereigntist,
    'What structurally changes if the sovereigntist_reading (strict consent requirement) is adopted instead of this universalist reading?',
    'Compare the two readings'' case dockets: under sovereigntist reading, cases resting solely on territorial trigger over non-party nationals (e.g. certain Afghanistan-related and Palestine-related investigations) would be jurisdictionally barred entirely.',
    'The sovereigntist reading has a dramatically smaller victim set (only ratifying-state nationals) and near-zero suppression requirement against non-party states, since no coercive claim is made over non-consenting parties — it would likely classify as a rope or scaffold rather than tangled_rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_delta_sovereigntist, conceptual, 'Structural delta between universalist and sovereigntist readings, located in the treatment of non-party consent.').

omega_variable(
    sibling_reading_delta_hybrid,
    'Where precisely does the disagreement between this universalist reading and the hybrid_complementarity_reading live?',
    'Identify whether complementarity is read as a genuine limiting principle (hybrid reading: the ICC only acts when domestic courts fail, preserving sovereign primacy as the default) or as a procedural gate that, once triggered, still permits jurisdiction over non-consenting parties (universalist reading: complementarity governs admissibility, not the prior question of whether jurisdiction exists at all).',
    'If complementarity is read as jurisdiction-limiting (hybrid), the extraction against non-party nationals is bounded and conditional; if it is read as merely admissibility-limiting (universalist, this story), the underlying jurisdictional claim over non-consenting parties is unconditional and the extractive/suppressive profile is structurally larger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_delta_hybrid, conceptual, 'The disagreement is located in whether complementarity limits jurisdiction itself or only case admissibility.').

omega_variable(
    unsc_veto_filter_on_universalism,
    'Does the Security Council''s veto-gated referral power undermine the universalist reading''s claim to universal authority, since permanent members can shield their own allies from the very mechanism claimed to transcend consent?',
    'Track referral and non-referral patterns for situations involving P5 allies versus non-aligned states over the interval; a strong asymmetry would indicate the universalist claim is selectively enforced rather than genuinely universal.',
    'If enforcement is heavily filtered by P5 politics, the ''universal'' framing functions partly as theater over an underlying great-power-selective mechanism, which would raise the honestly-authored theater_ratio in future measurement updates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unsc_veto_filter_on_universalism, empirical, 'Whether Security Council veto politics undermines the universalist jurisdictional claim''s actual universality.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rome_statute_jurisdiction__universalist_reading, 1998, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rome_tr_t1998, rome_statute_jurisdiction__universalist_reading, theater_ratio, 1998, 0.15).
narrative_ontology:measurement(rome_tr_t2002, rome_statute_jurisdiction__universalist_reading, theater_ratio, 2002, 0.18).
narrative_ontology:measurement(rome_tr_t2008, rome_statute_jurisdiction__universalist_reading, theater_ratio, 2008, 0.21).
narrative_ontology:measurement(rome_tr_t2014, rome_statute_jurisdiction__universalist_reading, theater_ratio, 2014, 0.24).
narrative_ontology:measurement(rome_tr_t2019, rome_statute_jurisdiction__universalist_reading, theater_ratio, 2019, 0.26).
narrative_ontology:measurement(rome_tr_t2024, rome_statute_jurisdiction__universalist_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(rome_be_t1998, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 1998, 0.22).
narrative_ontology:measurement(rome_be_t2002, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 2002, 0.28).
narrative_ontology:measurement(rome_be_t2008, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 2008, 0.33).
narrative_ontology:measurement(rome_be_t2014, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 2014, 0.37).
narrative_ontology:measurement(rome_be_t2019, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 2019, 0.4).
narrative_ontology:measurement(rome_be_t2024, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(rome_su_t1998, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 1998, 0.35).
narrative_ontology:measurement(rome_su_t2002, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 2002, 0.42).
narrative_ontology:measurement(rome_su_t2008, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 2008, 0.47).
narrative_ontology:measurement(rome_su_t2014, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 2014, 0.5).
narrative_ontology:measurement(rome_su_t2019, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 2019, 0.53).
narrative_ontology:measurement(rome_su_t2024, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rome_statute_jurisdiction__universalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(rome_statute_jurisdiction__universalist_reading, 0.12).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__universalist_reading, sovereigntist_reading).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__universalist_reading, hybrid_complementarity_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the rome_statute_jurisdiction kernel. universalist_reading (this story) claims jurisdiction transcends consent via territorial/UNSC triggers; sovereigntist_reading claims jurisdiction requires strict state consent; hybrid_complementarity_reading claims the Statute deliberately balances both via the complementarity mechanism. Each is authored as an independent constraint with its own ε, beneficiary/victim structure, and classification per the ε-invariance principle; they are linked via network edges rather than merged into a single observer-relative story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
