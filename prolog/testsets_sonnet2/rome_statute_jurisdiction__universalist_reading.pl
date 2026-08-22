% ============================================================================
% CONSTRAINT STORY: rome_statute_jurisdiction__universalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: rome_statute_jurisdiction__universalist_reading
 *   human_readable: Rome Statute Jurisdiction — Universalist Reading (Sovereignty-Transcending Mandate)
 *   domain: international_law/treaty_interpretation/institutional_authority
 *
 * SUMMARY:
 *   This constraint instantiates the universalist reading of the Rome
 *   Statute's jurisdictional kernel: the ICC's authority to prosecute core
 *   international crimes extends via territorial or UN Security Council
 *   triggers to nationals of states that never ratified the Statute, on the
 *   theory that jus cogens crimes bind erga omnes and individual criminal
 *   responsibility for atrocity transcends the ordinary consent architecture
 *   of treaty law. This is a distinct constraint from the sovereigntist
 *   reading (which treats jurisdiction as strictly consent-bound) and the
 *   hybrid complementarity reading (which treats the Statute as balancing
 *   universal aspiration against sovereign primacy through the admissibility
 *   mechanism) — the three readings share a text but diverge on whether
 *   non-consent is jurisdictionally dispositive, and each is authored as its
 *   own constraint story per the ε-invariance principle.
 *
 * KEY AGENTS:
 *   - icc_prosecutorial_office: institutional agenda-setter, asserts and administers the expansive jurisdictional reading
 *   - atrocity_victims_in_non_party_states: powerless beneficiaries, gain forum access without national consent as precondition
 *   - non_party_state_nationals_prosecuted: powerless payers, bear prosecution risk their state never consented to
 *   - non_ratifying_sovereign_states: institutional payers, experience the reading as a direct override of consent-based treaty law
 *   - great_power_militaries_operating_abroad: powerful payers, absorb diplomatic and operational cost of theoretical exposure
 *   - un_security_council_permanent_members: excluded/agenda-setting hybrid, can trigger jurisdiction over others while remaining structurally shielded
 *   - international_law_scholars: analytical observers, assess customary law validity of binding non-parties
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rome_statute_jurisdiction__universalist_reading, 0.58).
domain_priors:suppression_score(rome_statute_jurisdiction__universalist_reading, 0.62).
domain_priors:theater_ratio(rome_statute_jurisdiction__universalist_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rome_statute_jurisdiction__universalist_reading, tangled_rope).
narrative_ontology:human_readable(rome_statute_jurisdiction__universalist_reading, "Rome Statute Jurisdiction — Universalist Reading (Sovereignty-Transcending Mandate)").
narrative_ontology:topic_domain(rome_statute_jurisdiction__universalist_reading, "international_law/treaty_interpretation/institutional_authority").

domain_priors:requires_active_enforcement(rome_statute_jurisdiction__universalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rome_statute_jurisdiction__universalist_reading, 'a70acf28-aca1-45be-8037-16808d2f569e').
narrative_ontology:cs_kernel_codification('a70acf28-aca1-45be-8037-16808d2f569e', formalized).
narrative_ontology:cs_authority_grounding('a70acf28-aca1-45be-8037-16808d2f569e', extraction).
narrative_ontology:cs_interpretation_layer_present('a70acf28-aca1-45be-8037-16808d2f569e').
narrative_ontology:cs_reading_relation('a70acf28-aca1-45be-8037-16808d2f569e', rome_statute_jurisdiction__sovereigntist_reading, forecloses).
narrative_ontology:cs_reading_relation('a70acf28-aca1-45be-8037-16808d2f569e', rome_statute_jurisdiction__hybrid_complementarity_reading, influences).
narrative_ontology:cs_axiom('a70acf28-aca1-45be-8037-16808d2f569e', foundational, core_crimes_bind_erga_omnes_regardless_of_consent).
narrative_ontology:cs_axiom_status(core_crimes_bind_erga_omnes_regardless_of_consent, holdable).
narrative_ontology:cs_axiom_grounding('a70acf28-aca1-45be-8037-16808d2f569e', core_crimes_bind_erga_omnes_regardless_of_consent, deontological).
narrative_ontology:cs_axiom('a70acf28-aca1-45be-8037-16808d2f569e', foundational, territorial_or_unsc_trigger_is_jurisdictionally_sufficient).
narrative_ontology:cs_axiom_status(territorial_or_unsc_trigger_is_jurisdictionally_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('a70acf28-aca1-45be-8037-16808d2f569e', territorial_or_unsc_trigger_is_jurisdictionally_sufficient, conventional).
narrative_ontology:cs_reference_frame('a70acf28-aca1-45be-8037-16808d2f569e', erga_omnes_jus_cogens_accountability).
narrative_ontology:cs_drift_state('a70acf28-aca1-45be-8037-16808d2f569e', post_2016_withdrawal_wave, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a70acf28-aca1-45be-8037-16808d2f569e', '').
narrative_ontology:cs_kernel_id(rome_statute_jurisdiction__universalist_reading, rome_statute_jurisdiction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__universalist_reading, atrocity_victims_in_non_party_states).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__universalist_reading, icc_prosecutorial_office).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__universalist_reading, international_human_rights_advocacy_networks).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__universalist_reading, non_party_state_nationals_prosecuted).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__universalist_reading, non_ratifying_sovereign_states).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__universalist_reading, great_power_militaries_operating_abroad).
narrative_ontology:constraint_vindicates(rome_statute_jurisdiction__universalist_reading, jus_cogens_crimes_bind_erga_omnes).
narrative_ontology:constraint_vindicates(rome_statute_jurisdiction__universalist_reading, individual_criminal_responsibility_transcends_state_consent).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Initiates investigations and prosecutions, including proprio motu, and asserts jurisdiction over nationals of non-party states where the alleged crime occurs on the territory of a party state or is referred by the UN Security Council. Reads the Statute as establishing jurisdiction grounded in the crime's gravity to humanity as a whole, not in the accused state's consent. Administers the institution and could, in principle, narrow its own jurisdictional reach, but its founding purpose and funding rationale depend on the expansive reading.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, icc_prosecutorial_office, agenda_setter,
    institutional, generational, analytical, global).

% Suffer genocide, crimes against humanity, or war crimes in states that have not ratified the Statute and whose domestic courts will not or cannot prosecute. Under this reading, they gain a forum of last resort through territorial or UNSC-triggered jurisdiction regardless of their government's consent. They have no direct voice in whether the Court accepts a case and depend entirely on the Prosecutor's charging discretion and political will.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, atrocity_victims_in_non_party_states, beneficiary,
    powerless, biographical, trapped, local).

% Individuals — often military commanders or state officials — who face ICC arrest warrants and trial despite their state never ratifying the Statute or consenting to its jurisdiction. They cannot exit the Court's reach through diplomatic non-ratification alone; travel to any state party exposes them to arrest and surrender obligations.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, non_party_state_nationals_prosecuted, payer,
    powerless, biographical, trapped, national).

% States that deliberately declined to join the Rome Statute to preserve control over prosecuting their own nationals nonetheless find their citizens subject to ICC process when crimes occur on party-state territory or via Security Council referral. Their non-consent is treated as immaterial to the territorial or UNSC jurisdictional trigger, which they experience as a direct override of the customary consent-based foundation of treaty law they believe should govern them.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, non_ratifying_sovereign_states, payer,
    institutional, generational, constrained, national).

% Personnel and commanders of major military powers conducting operations on the territory of Rome Statute member states face theoretical exposure to jurisdiction their home government never accepted. In practice their governments use diplomatic leverage, non-cooperation, and bilateral immunity agreements to blunt enforcement, but the exposure itself, and the diplomatic cost of resisting it, is real and constrains basing and operational agreements.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, great_power_militaries_operating_abroad, payer,
    powerful, biographical, constrained, global).

% NGOs, legal scholars, and transitional justice practitioners who campaign for and rely on the expansive jurisdictional reading to press cases against powerful non-party actors. They gain moral and legal leverage, funding, and institutional standing from the universalist interpretation succeeding, and face no direct cost if it fails to secure convictions.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, international_human_rights_advocacy_networks, beneficiary,
    organized, civilizational, mobile, global).

% Three of five permanent members are not Rome Statute parties yet hold veto power over Council referrals that can trigger jurisdiction over any state, party or not. They are structurally outside the treaty's consent framework while controlling one of its two jurisdictional triggers — able to activate universal jurisdiction against others while remaining functionally immune from having it activated against themselves via the same mechanism.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, un_security_council_permanent_members, excluded,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(rome_statute_jurisdiction__universalist_reading, un_security_council_permanent_members, agenda_setter).

% Assess whether the Statute's jurisdictional provisions reflect settled customary international law binding non-parties, or an ultra vires expansion by treaty parties purporting to bind non-consenting states. Their conclusions shape whether states view compliance as legal obligation or political accommodation.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, international_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rome_statute_jurisdiction__universalist_reading, diffuse).
narrative_ontology:fixing_cost_class(rome_statute_jurisdiction__universalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a standing international forum capable of prosecuting genocide, crimes against humanity, and war crimes when national courts are unwilling or unable, closing impunity gaps that ad hoc tribunals (Nuremberg, ICTY, ICTR) addressed only after the fact and only for specific conflicts.
% TRANSFER_FUNCTION: Moves prosecutorial authority and the practical risk of criminal liability from national governments (who might otherwise shield their own officials) to an international body, and moves a measure of protection and forum-access from politically powerless victims to a supranational institution — at the cost of exposing non-consenting states' nationals to a jurisdiction their governments never accepted.
% ABSENT_VOICES: Non-party states themselves are structurally absent from the Assembly of States Parties that governs the Court's rules, budget, and prosecutorial oversight, yet their nationals can be bound by its jurisdiction; they would object that binding non-signatories violates the pacta tertiis principle of treaty law, but they hold no seat in the body whose interpretation controls the outcome.
% DISAPPEARANCE_RATIONALE: If the universalist jurisdictional reading were abandoned overnight in favor of strict consent, dozens of active and potential situations involving non-party nationals (arising from territorial or UNSC triggers) would collapse; victims in non-ratifying states would lose their only supranational forum, and non-party states' military and diplomatic calculus around deploying forces into Rome Statute territory would shift substantially — arrangements currently built on the expansive reading would need to be renegotiated or abandoned.
% FOUNDING_PROBLEM: The international community lacked any standing mechanism to prosecute mass atrocity when national justice systems were captured, destroyed, or complicit — ad hoc tribunals were slow, selective, and created only after the fact, leaving perpetrators of gravity crimes in a structural impunity gap regardless of which state's consent framework nominally applied.
% FOUNDING_PROBLEM_CORROBORATION: The ICC and allied advocacy networks attest the impunity gap remains live and cite ongoing situations as proof. Non-ratifying states (including three UNSC permanent members) and a substantial body of international law scholarship outside the Court's own institutional interest attest that the consent-transcending jurisdictional claim was never validly grounded in customary law for non-parties, and that the founding problem — reachable impunity — could be addressed through consent-respecting mechanisms (universal jurisdiction statutes, hybrid tribunals) without the universalist override; this is a genuinely contested genealogy, not a settled one.
narrative_ontology:disappearance_verdict(rome_statute_jurisdiction__universalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(rome_statute_jurisdiction__universalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rome_statute_jurisdiction__universalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(rome_statute_jurisdiction__universalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rome_statute_jurisdiction__universalist_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rome_statute_jurisdiction__universalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rome_statute_jurisdiction__universalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rome_statute_jurisdiction__universalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.58 (substantial, not extreme) because the reading extends real, non-trivial legal exposure to non-consenting parties, but that exposure has so far translated into a relatively small number of actual prosecutions against powerful non-party nationals — enforcement capacity lags the jurisdictional claim considerably. Suppression is authored at 0.62, reflecting the coercive architecture (arrest warrants, surrender obligations, complementarity pressure on domestic courts) that the reading requires to hold, even though its bite is asymmetric — it lands hardest on weaker states and individuals while great powers largely arbitrage around it via non-cooperation. Theater ratio rises over the interval (0.25 to 0.40) as the gap between jurisdictional claims asserted and prosecutions actually completed against non-party nationals has widened — many warrants issued, few powerful defendants surrendered, which is itself diagnostic of a reading whose enforcement machinery increasingly outpaces its enforcement reality. Accessibility collapse is moderate (0.45): the sovereigntist and hybrid readings remain live, contested alternatives in state practice and scholarship, so alternatives to the universalist reading have not collapsed the way a genuine mountain's would.
 *
 * DIRECTIONALITY LOGIC:
 *   Atrocity victims in non-party states and advocacy networks are coded as beneficiaries — the reading's expansive jurisdiction is the mechanism that gives them any forum at all, so directionality sits near the beneficiary end despite victims' powerlessness (the constraint subsidizes their access to justice even though they hold no power over its operation). Non-party nationals facing prosecution, non-ratifying states, and exposed military powers are coded as payers/targets — the same jurisdictional claim that creates access for victims creates liability exposure for them, and their consent was never sought. The UNSC permanent members occupy a genuinely asymmetric dual position: three are non-parties who would ordinarily be targets, but their veto over Council referrals lets them wield the same mechanism against others while remaining functionally insulated from its application to themselves — this is exactly the kind of asymmetric institutional relationship the override mechanism exists for, though no override was needed here since the structural derivation from their dual beneficiary/payer declaration already captures the asymmetry via the secondary_role field.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — impunity for mass atrocity where national courts fail — remains genuinely live in ongoing conflicts, which argues against pure mandatrophy. But the corroboration is honestly split: the Court and advocacy networks affirm the problem persists and the universalist mechanism is necessary to reach it, while a body of scholarship and non-ratifying states argue the specific consent-transcending jurisdictional claim was never validly grounded and that the same founding problem could be addressed through mechanisms that respect consent (hybrid tribunals, universal jurisdiction statutes in domestic courts). Classifying this as tangled_rope rather than snare or mountain captures that both a genuine coordination function (closing the impunity gap) and asymmetric extraction (binding non-consenting parties, with enforcement asymmetrically evaded by the powerful) are simultaneously true and require active enforcement to persist — collapsing this into a pure snare would erase the real victim-access coordination function the Court performs; collapsing it into a mountain or rope would erase the non-consent problem the sovereigntist reading correctly identifies.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    customary_law_status_of_non_party_jurisdiction,
    'Has state practice and opinio juris crystallized into customary international law binding non-party nationals to ICC jurisdiction, or does the universalist reading claim a legal status the Statute''s drafting history and subsequent practice do not support?',
    'Systematic review of ICJ jurisprudence, state objection patterns (formal protests by non-party states to specific ICC proceedings), and scholarly consensus tracking over a defined period; a persistent-objector pattern from major non-party states would undercut the customary law claim.',
    'If customary law status is established, the universalist reading''s override of consent is legally grounded rather than merely asserted, weakening the sovereigntist reading''s core objection. If not established, the reading''s jurisdictional claim over non-parties rests on treaty-interpretation assertion rather than binding law, strengthening the case that this constraint is more extractive/asymmetric than coordinative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_law_status_of_non_party_jurisdiction, empirical, 'Whether non-party jurisdiction claim reflects settled customary law or contested treaty overreach.').

omega_variable(
    committer_kernel_disagreement_locus,
    'Where exactly does the universalist reading''s premise diverge from the sovereigntist and hybrid readings — is it a disagreement about the SOURCE of jurisdictional authority (treaty consent vs. jus cogens/erga omnes obligation), or about the SCOPE of the complementarity mechanism as a limiting filter?',
    'Structural comparison of the three readings'' treatment of Article 12 (preconditions to jurisdiction) and Article 17 (admissibility/complementarity) — the universalist reading treats Article 12''s territorial/UNSC triggers as jurisdictionally sufficient regardless of consent, the sovereigntist reading treats consent as an independent necessary condition the triggers cannot substitute for, and the hybrid reading treats complementarity as doing most of the real work of reconciling the two.',
    'If the disagreement is purely about jurisdictional SOURCE, this reading forecloses the sovereigntist reading''s core premise for the territorial-trigger case (they cannot both be right about whether consent is a necessary condition). If it is about complementarity SCOPE, the hybrid reading may simply be this reading''s operational instantiation viewed through a different lens, in which case the ''influences'' relation is more accurate than any foreclosure claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_kernel_disagreement_locus, conceptual, 'Locating the precise structural axis of disagreement among the three kernel readings.').

omega_variable(
    enforcement_asymmetry_persistence,
    'Is the growing gap between jurisdictional claims and completed prosecutions against powerful non-party nationals (rising theater_ratio) a temporary enforcement-capacity lag that will close as the Court matures, or a structural feature that will persist indefinitely because powerful states can always out-arbitrage the mechanism?',
    'Track completed prosecutions against nationals of powerful non-party states over the next decade relative to prosecutions against nationals of weaker non-party or party states; a persistent asymmetry ratio would indicate structural rather than transitional theater.',
    'If structural, the tangled_rope classification is stable long-term. If transitional, the constraint could evolve toward a genuine rope as enforcement capacity catches up to jurisdictional claims, or toward snare if the coordination function itself erodes while extraction from weaker parties continues.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_asymmetry_persistence, empirical, 'Whether enforcement asymmetry against powerful non-party nationals is transitional or structural.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rome_statute_jurisdiction__universalist_reading, 1998, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rome_tr_t1998, rome_statute_jurisdiction__universalist_reading, theater_ratio, 1998, 0.25).
narrative_ontology:measurement(rome_tr_t2002, rome_statute_jurisdiction__universalist_reading, theater_ratio, 2002, 0.28).
narrative_ontology:measurement(rome_tr_t2008, rome_statute_jurisdiction__universalist_reading, theater_ratio, 2008, 0.32).
narrative_ontology:measurement(rome_tr_t2014, rome_statute_jurisdiction__universalist_reading, theater_ratio, 2014, 0.36).
narrative_ontology:measurement(rome_tr_t2019, rome_statute_jurisdiction__universalist_reading, theater_ratio, 2019, 0.38).
narrative_ontology:measurement(rome_tr_t2024, rome_statute_jurisdiction__universalist_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(rome_be_t1998, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 1998, 0.3).
narrative_ontology:measurement(rome_be_t2002, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 2002, 0.38).
narrative_ontology:measurement(rome_be_t2008, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 2008, 0.46).
narrative_ontology:measurement(rome_be_t2014, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 2014, 0.52).
narrative_ontology:measurement(rome_be_t2019, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 2019, 0.55).
narrative_ontology:measurement(rome_be_t2024, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(rome_su_t1998, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 1998, 0.4).
narrative_ontology:measurement(rome_su_t2002, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 2002, 0.48).
narrative_ontology:measurement(rome_su_t2008, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 2008, 0.53).
narrative_ontology:measurement(rome_su_t2014, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 2014, 0.58).
narrative_ontology:measurement(rome_su_t2019, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 2019, 0.6).
narrative_ontology:measurement(rome_su_t2024, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 2024, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rome_statute_jurisdiction__universalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__universalist_reading, rome_statute_jurisdiction__sovereigntist_reading).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__universalist_reading, rome_statute_jurisdiction__hybrid_complementarity_reading).

% DUAL FORMULATION NOTE:
% Three constraint stories decompose the natural-language concept 'Rome Statute jurisdiction' per the kernel/reading framework: this story (universalist_reading, tangled_rope, epsilon=0.58) claims jurisdiction transcends consent via territorial/UNSC triggers; sovereigntist_reading claims jurisdiction requires strict consent (expected to authorize a much lower epsilon and possibly a rope or scaffold classification from the sovereigntist seat); hybrid_complementarity_reading claims the Statute balances both via the complementarity admissibility filter (expected epsilon between the two, likely also tangled_rope but with a different beneficiary/victim balance emphasizing domestic-court primacy). Each story authors its own epsilon from its own reading's lights per the fixed-referent rule; the readings are linked here rather than merged into one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
