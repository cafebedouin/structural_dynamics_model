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
 *   constraint_id: rome_statute_jurisdiction__sovereigntist_reading
 *   human_readable: Rome Statute Jurisdiction (Sovereigntist Reading)
 *   domain: international_law/institutional_authority
 *
 * SUMMARY:
 *   The Rome Statute (1998) established the International Criminal Court with
 *   jurisdiction over four core crimes: genocide, crimes against humanity,
 *   war crimes, and aggression. This constraint story instantiates the
 *   SOVEREIGNTIST READING of the Rome Statute's jurisdiction framework. In
 *   this reading, ICC jurisdiction is strictly limited to states that have
 *   consented to the treaty via accession or ratification. Non-party states
 *   cannot be bound unless the UN Security Council refers a situation
 *   (Article 13(b)), and even then, nationals of non-parties are immune from
 *   ICC jurisdiction unless their state accepts or the UNSC refers. National
 *   courts retain primary authority under the complementarity principle — the
 *   ICC acts only when national courts are unwilling or unable. This reading
 *   locates the kernel's authority grounding in state sovereignty and treaty
 *   consent; the Rome Statute is a conditional institutional framework, not a
 *   universal mandate. The Rome Statute itself is the kernel; the
 *   sovereigntist reading is one among three structural instantiations of
 *   what the treaty means. The universalist reading sees the Statute as
 *   grounding a universal human-rights mandate transcending consent. The
 *   hybrid-complementarity reading balances both: aspiration to universal
 *   justice, constrained by practical deference to state primacy. This story
 *   describes the sovereigntist instantiation only — the constraint as
 *   structured when consent and complementarity are interpreted as limits on
 *   ICC authority.
 *
 * KEY AGENTS:
 *   - consenting_state_governments — states party to Rome Statute (123 signatories at interval start, 187+ by 2024); exercise treaty-derived authority over ICC jurisdiction; benefit from controlled, limited ICC reach; coordinate international criminal accountability subject to consent gates
 *   - icc_institutional_authority — prosecutor, judges, registrar; administer jurisdiction within the consent framework; issue arrest warrants and conduct trials; enforce the treaty's terms; institutional beneficiary of the framework (job depends on ICC existence and operation)
 *   - non_party_nationals — individuals in non-consenting states; structurally immune from ICC jurisdiction except via UNSC referral; their states' refusal to accede shields them from prosecution; extracted from if their state undergoes UNSC referral
 *   - unwilling_non_consenting_states — countries (notably USA, Russia, China, India, major regional powers) that have not ratified Rome Statute; retain full sovereignty over prosecution of their nationals; subject to UNSC referral mechanism which can override their consent boundary; extracted from if referred despite non-accession
 *   - UN Security Council — authorization body for Article 13(b) referrals; can bind non-parties without consent; structural exception to sovereignty gate; observer of the sovereigntist framework (Council's referral power is the exception that tests the sovereignty rule)
 *   - national courts — primary prosecutors under complementarity principle; retain authority unless they are unwilling or unable; deferred to by ICC (complementarity as procedural priority, not override)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rome_statute_jurisdiction__sovereigntist_reading, 0.48).
domain_priors:suppression_score(rome_statute_jurisdiction__sovereigntist_reading, 0.62).
domain_priors:theater_ratio(rome_statute_jurisdiction__sovereigntist_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rome_statute_jurisdiction__sovereigntist_reading, tangled_rope).
narrative_ontology:human_readable(rome_statute_jurisdiction__sovereigntist_reading, "Rome Statute Jurisdiction (Sovereigntist Reading)").
narrative_ontology:topic_domain(rome_statute_jurisdiction__sovereigntist_reading, "international_law/institutional_authority").

domain_priors:requires_active_enforcement(rome_statute_jurisdiction__sovereigntist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rome_statute_jurisdiction__sovereigntist_reading, '735fcc8a-735b-4954-b76a-174c56df28e5').
narrative_ontology:cs_kernel_codification('735fcc8a-735b-4954-b76a-174c56df28e5', formalized).
narrative_ontology:cs_authority_grounding('735fcc8a-735b-4954-b76a-174c56df28e5', lineage).
narrative_ontology:cs_interpretation_layer_present('735fcc8a-735b-4954-b76a-174c56df28e5').
narrative_ontology:cs_reading_relation('735fcc8a-735b-4954-b76a-174c56df28e5', rome_statute_jurisdiction__universalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('735fcc8a-735b-4954-b76a-174c56df28e5', rome_statute_jurisdiction__hybrid_complementarity_reading, influences).
narrative_ontology:cs_axiom('735fcc8a-735b-4954-b76a-174c56df28e5', foundational, state_consent_limits_icc_jurisdiction).
narrative_ontology:cs_axiom_status(state_consent_limits_icc_jurisdiction, holdable).
narrative_ontology:cs_axiom_grounding('735fcc8a-735b-4954-b76a-174c56df28e5', state_consent_limits_icc_jurisdiction, conventional).
narrative_ontology:cs_axiom('735fcc8a-735b-4954-b76a-174c56df28e5', foundational, complementarity_defers_to_national_courts).
narrative_ontology:cs_axiom_status(complementarity_defers_to_national_courts, holdable).
narrative_ontology:cs_axiom_grounding('735fcc8a-735b-4954-b76a-174c56df28e5', complementarity_defers_to_national_courts, conventional).
narrative_ontology:cs_reference_frame('735fcc8a-735b-4954-b76a-174c56df28e5', treaty_based_conditional_jurisdiction).
narrative_ontology:cs_drift_state('735fcc8a-735b-4954-b76a-174c56df28e5', post_security_council_referral_expansion, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('735fcc8a-735b-4954-b76a-174c56df28e5', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(rome_statute_jurisdiction__sovereigntist_reading, rome_statute_jurisdiction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__sovereigntist_reading, consenting_state_governments).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__sovereigntist_reading, icc_institutional_authority).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__sovereigntist_reading, non_party_nationals).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__sovereigntist_reading, unwilling_non_consenting_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__sovereigntist_reading, national_courts).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States that have ratified the Rome Statute (123+ at baseline, 187+ by 2024). They exercise authority over ICC prosecution via treaty consent and can withdraw (though rarely do). They benefit from a shared accountability framework for the gravest crimes, burden-sharing on prosecution costs, and the legitimacy that comes from participating in a rule-of-law institution. They shape ICC policy through the Assembly of States Parties and can vote to amend the Statute. Their exit is costly (loss of influence, reputational damage) but legal.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, consenting_state_governments, beneficiary,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(rome_statute_jurisdiction__sovereigntist_reading, consenting_state_governments, agenda_setter).

% The International Criminal Court's prosecutor, judges, registry, and supporting staff. The institution depends on the Rome Statute's existence and legitimacy for its authority to prosecute. It benefits from the jurisdictional framework (consent gates are constraints on ICC reach, but they also confer legitimacy by framing the ICC as bound by treaty, not unilateral power). The ICC cannot exit the consent framework without ceasing to exist.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, icc_institutional_authority, beneficiary,
    institutional, generational, trapped, global).

% Individuals (potential perpetrators or witnesses) who are nationals of non-consenting states. They are structurally immune from ICC jurisdiction under the sovereigntist reading, EXCEPT if their state is referred by the UN Security Council or if they travel to a consenting state that might extradite them. Their identities are bound to nationality; they cannot change their home state without extraordinary effort (asylum, denaturalization). They depend on their state's refusal to consent for protection from ICC jurisdiction.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, non_party_nationals, payer,
    powerless, biographical, identity_locked, global).

% Powerful countries (USA, Russia, China, India, and others) that have not ratified the Rome Statute. They assert that the treaty conflicts with their sovereignty and legal systems, and they refuse to bind their nationals to ICC jurisdiction. Their refusal is legal under the sovereigntist reading. However, the UN Security Council can refer situations in their territory without their consent (Article 13(b)), exposing them to ICC jurisdiction despite non-accession. Their exit is constrained: they could accede, but at high political and legal cost; they could exit the UN, but that is prohibitive; they can contest UNSC referrals diplomatically and through Council vetoes (for P5 members), but this requires constant active defense.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, unwilling_non_consenting_states, payer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(rome_statute_jurisdiction__sovereigntist_reading, unwilling_non_consenting_states, excluded).

% Courts in consenting and non-consenting states that exercise primary jurisdiction over international crimes. Under complementarity, they retain authority and the ICC acts only when national courts are unwilling or unable. They benefit from the framework because it provides a backstop (the threat of ICC prosecution can encourage national prosecution), and it legitimizes their judgments as part of a shared system. Their role is procedural: they do not directly benefit from the transfer of rents, but they coordinate with the ICC under the complementarity principle.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, national_courts, beneficiary,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(rome_statute_jurisdiction__sovereigntist_reading, national_courts, observer).

% The UN Security Council's authority under Article 13(b) to refer situations to the ICC without state consent. This is the structural exception to the consent requirement: the Council can bind non-parties and override their sovereignty. From the sovereigntist reading's perspective, the UNSC referral mechanism is the exception that proves the rule (consent is normal; referral is exceptional). The Council has referred five situations (three African cases, plus Georgia and Ukraine by General Assembly vote-through). The Council observes the constraint from above: its referral power is the structural limit on sovereigntist sovereignty.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, un_security_council, observer,
    institutional, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rome_statute_jurisdiction__sovereigntist_reading, icc_institutional_authority).
narrative_ontology:fixing_cost_class(rome_statute_jurisdiction__sovereigntist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: International coordination for investigation and prosecution of genocide, crimes against humanity, war crimes, and aggression. The Rome Statute solves the collective-action problem that no single national court has jurisdiction over cross-border atrocities, and smaller states lack the capacity to investigate and prosecute perpetrators from powerful countries. Consenting states share evidentiary burdens, prosecutorial expertise, and enforcement (e.g., arrest warrant execution) through the ICC.
% TRANSFER_FUNCTION: Moves prosecutorial authority from exclusive national control to shared international control (consenting states). Moves evidentiary and investigative resources from scattered national systems into the ICC's centralized structure. Moves prestige and moral authority from national justice systems to the international system. For non-consenting states, the constraint also transfers exposure: if UNSC refers, they face prosecution authority they did not consent to.
% ABSENT_VOICES: Non-party states (USA, Russia, China, India, and others) that refrain from the discussion because they do not recognize ICC authority. If they were in the room, they would argue that state sovereignty is inviolable and that ICC prosecution without consent is neo-colonial institutional overreach. Universalist scholars and human-rights advocates who contest the sovereigntist reading's framing would argue that the consent boundary is a loophole protecting perpetrators in powerful states. Their absence from the formal treaty negotiation is structural — the Rome Statute is authored by consenting states, so universalist and sovereigntist critiques are external to the initial consensus.
% DISAPPEARANCE_RATIONALE: If the Rome Statute and its jurisdiction framework vanished overnight, the system of international criminal accountability would collapse. National courts would lack the coordination mechanism and capacity to prosecute cross-border atrocities, enforcement would fracture, and perpetrators in powerful countries would have dramatically lower risk of prosecution. Consenting states would lose the legitimacy gain from international rule-of-law institution. The constraint is not natural — it depends on active maintenance (Assembly of States Parties meetings, prosecutorial investigations, Security Council referrals, national enforcement of ICC arrest warrants). Its disappearance would rearrange the global system for addressing international crimes.
% FOUNDING_PROBLEM: The international community in the 1990s faced an urgent collective-action problem: atrocities in Yugoslavia, Rwanda, and elsewhere were prosecuted via ad hoc tribunals (ICTY, ICTR), which were expensive, slow, and lacked permanent capacity. The Rome Statute was negotiated to create a permanent, treaty-based institution that could prosecute the gravest crimes in any state without ad hoc tribunal creation. The founding problem was international coordination for accountability while respecting state sovereignty — building an institution that could prosecute without becoming a neo-colonial override of national justice systems.
% FOUNDING_PROBLEM_CORROBORATION: Consenting states and the ICC attest that the founding problem remains live: ad hoc tribunals are still needed (see ICC-referred situations in Uganda, DRC, Georgia, Ukraine), and the Rome Statute's existence has encouraged national prosecutions (complementarity working as designed, per ICC reports to the Assembly of States Parties). Universalist scholars and human-rights advocates attest the problem is partially dead: the coordination problem is solved for consenting states, but non-parties and unwilling states remain sheltered, creating accountability gaps that expose the consent boundary as a loophole protecting powerful perpetrators. US statements to the UN, scholarly literature questioning ICC legitimacy (see, e.g., critiques of ICC focus on African states and perceived bias), and UNSC veto dynamics (P5 reluctance to refer themselves) support the contested verdict. Non-consenting states do not formally attend the discussion but their behavior (non-accession, opposition to ICC universalism) attests they reject the founding problem as framed by consenting states.
narrative_ontology:disappearance_verdict(rome_statute_jurisdiction__sovereigntist_reading, world_rearranges).
narrative_ontology:founding_problem_status(rome_statute_jurisdiction__sovereigntist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rome_statute_jurisdiction__sovereigntist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(rome_statute_jurisdiction__sovereigntist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rome_statute_jurisdiction__sovereigntist_reading, 0.48, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is measured at 0.48 (interval end) because the sovereigntist reading frames ICC jurisdiction as legitimately constrained by consent, yet the mechanism produces asymmetry: consenting states gain coordination benefit (shared accountability framework, burden-sharing on prosecution, legitimacy from international rule of law) while non-parties face extraction if referred (exposed to ICC prosecution despite non-consent). Suppression is higher (0.62) because maintaining the sovereigntist frame requires actively preventing universalist interpretation: the ICC must not claim inherent universal jurisdiction; national governments must defend the treaty as limited; legal scholarship emphasizing consent and complementarity must be heard. Theater is moderate (0.41) because the constraint combines genuine coordination (states solving collective prosecution problems) with performative defense of sovereignty (states assert control they have partially surrendered to the Rome Statute itself by accession). The measurement series captures drift over 25 years (1999–2024): extractiveness rises as the ICC's effective reach expands via UNSC referrals and prosecutorial interpretation of jurisdiction, while states' ability to defend the consent boundary erodes. Suppression requirement rises as more voices advance universalist readings and challenge sovereignty as a limit on international justice. Theater rises as states increasingly perform sovereignty protection while institutional practice drifts toward universal reach.
 *
 * PERSPECTIVAL GAP:
 *   From the sovereigntist reading's structural position: the Rome Statute is a legitimately limited institution that respects state consent, with the UNSC referral mechanism as an exception for the most severe cases. From the universalist reading's position: the Rome Statute grounds universal jurisdiction for crimes against humanity, and consent/non-party immunity are cover stories for state power protecting perpetrators. From the hybrid reading: both aspects are real — genuine coordination aspirations coexist with practical deference to state primacy. The sovereigntist and universalist readings literally instantiate different constraints (different beneficiary/victim structures, different ε values), so the engine will compute different per-seat types for each.
 *
 * DIRECTIONALITY LOGIC:
 *   Consenting states (powerful, organized, institutional): low directionality (~0.2–0.3), they benefit from the framework and exercise authority within it. ICC institutional authority (institutional, powerful in its domain): symmetric-to-beneficiary (~0.3–0.4), it collects the right to exist and operate but is constrained by the consent gate. Non-party nationals (powerless, trapped): high directionality (~0.8–0.9), they bear extraction risk (exposure to ICC jurisdiction without their state's consent) and have no exit. Unwilling non-consenting states (powerful, organized, institutional): moderate-high directionality (~0.6–0.7), they face extraction via UNSC referral mechanism despite not ratifying, but they retain power to contest UNSC action and their international standing limits ICC reach. National courts (institutional, analytical): near-symmetric (~0.5), they coordinate prosecution under complementarity but defer to ICC when they are deemed unwilling/unable.
 *
 * MANDATROPHY ANALYSIS:
 *   The sovereigntist reading asserts that the Rome Statute's founding problem was 'international coordination for prosecution of the gravest crimes while respecting state sovereignty,' and that this problem remains live ('contested' status): states still defend consent and complementarity as legitimate limits, but universalist voices increasingly claim the founding problem is 'obsolete — sovereignty should not shield perpetrators.' If the problem is dead (universalist verdict) but the constraint persists (institutional inertia, UNSC override mechanism expanding), the reading exhibits mandatrophy: the sovereignty-respecting framework is maintained performatively while actual practice has drifted toward universal reach. The measurement series showing rising suppression_requirement and theater_ratio supports this reading: more active work is required to maintain the sovereigntist frame (defending consent in scholarly literature, negotiating UNSC referral boundaries, national governments asserting control they have partially surrendered).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consent_mechanism_vs_complementarity,
    'Does the Rome Statute''s complementarity principle instantiate sovereign deference or functional override? Can a non-consenting state be bound to ICC jurisdiction via UNSC referral despite never signing the treaty?',
    'Cross-textual analysis of complementarity clauses (Articles 17-18) and UNSC referral mechanism (Article 13(b)) against drafting history and subsequent state practice. Does treaty language support binding non-parties, or only procedural coordination?',
    'If complementarity is understood as deference-to-national courts (this reading''s premise), UNSC referral bypasses consent and instantiates universalist extraction. If complementarity is functional override, this reading understates the ICC''s effective reach.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consent_mechanism_vs_complementarity, empirical, 'Whether complementarity defers to national courts or overrides consent framework.').

omega_variable(
    kernel_codification_ambiguity,
    'Is the Rome Statute''s jurisdiction framework self-interpreting (formalized: explicit consent requirement in Articles 12-13), or does it depend on interpretive authority (UN bodies, ICJ precedent, custom) that has progressively expanded ICC reach beyond the literal text?',
    'Comparison of treaty text literal boundaries (consent required for Articles 12(1)-(2), UNSC exception in 13(b)) against institutional practice over 25 years: has the ICC or other actors claimed jurisdiction beyond the text? Have states accepted that practice as binding custom?',
    'If the framework is self-interpreting, sovereignty survives; if interpretation has shifted toward universalism via state acceptance, the sovereigntist reading is overridden in practice, and the kernel codification drifts from formalized toward implicit/distributed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_codification_ambiguity, empirical, 'Codification stability: does the treaty text govern, or has institutional practice rewritten consent boundaries?').

omega_variable(
    reading_coexistence_boundary,
    'Can the sovereigntist and universalist readings coexist within a single legal framework, or do they represent incompatible commitments to consent and enforcement authority?',
    'State practice in treaty accession and UNSC referral: when states accede to Rome Statute, do they assert individual consent as exhaustive, or do they acknowledge UNSC referral as co-equal authority? When UNSC refers, do non-parties accept ICC jurisdiction, or do they contest it as ultra vires?',
    'If coexistence is durable (both readings live in different institutional seats simultaneously), the constraint is stable as a Tangled Rope coordinating and extracting per-seat. If one reading has eroded the other via practice, the constraint''s reading_relations shift toward forecloses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_coexistence_boundary, conceptual, 'Whether sovereigntist and universalist readings remain coexistent or have collapsed toward one.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rome_statute_jurisdiction__sovereigntist_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rome_tr_t0, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(rome_tr_t5, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 5, 0.33).
narrative_ontology:measurement(rome_tr_t10, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 10, 0.38).
narrative_ontology:measurement(rome_tr_t15, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 15, 0.41).
narrative_ontology:measurement(rome_tr_t20, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 20, 0.43).
narrative_ontology:measurement(rome_tr_t25, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 25, 0.45).

% Extraction over time
narrative_ontology:measurement(rome_be_t0, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(rome_be_t5, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 5, 0.41).
narrative_ontology:measurement(rome_be_t10, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 10, 0.46).
narrative_ontology:measurement(rome_be_t15, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(rome_be_t20, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(rome_be_t25, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 25, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(rome_su_t0, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(rome_su_t5, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 5, 0.56).
narrative_ontology:measurement(rome_su_t10, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(rome_su_t15, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 15, 0.62).
narrative_ontology:measurement(rome_su_t20, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 20, 0.64).
narrative_ontology:measurement(rome_su_t25, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 25, 0.66).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rome_statute_jurisdiction__sovereigntist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(rome_statute_jurisdiction__sovereigntist_reading, 0.12).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__sovereigntist_reading, rome_statute_jurisdiction__universalist_reading).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__sovereigntist_reading, rome_statute_jurisdiction__hybrid_complementarity_reading).

% DUAL FORMULATION NOTE:
% The Rome Statute jurisdiction kernel generates three distinct constraint stories corresponding to three incompatible readings. Each reading instantiates different beneficiary/victim structures and different ε values: the sovereigntist reading (this story) frames ICC jurisdiction as legitimately consent-limited; the universalist reading frames it as universal mandate overriding consent; the hybrid reading balances both. The three stories are linked via network.affects_constraints to signal their kernel kinship. A consumer analyzing the corpus will observe that all three stories reference the same treaty text but compute different types and extraction metrics — that divergence is the signal that a single kernel (Rome Statute) is being read in structurally incompatible ways. The ε-invariance principle required decomposition: one reading, one constraint, one ε per story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rome_statute_jurisdiction__sovereigntist_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
