% ============================================================================
% CONSTRAINT STORY: rome_statute_jurisdiction__universalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: rome_statute_jurisdiction__universalist_reading
 *   human_readable: Rome Statute Universal Mandate Reading (Jurisdiction Transcending Consent)
 *   domain: legal/international/institutional
 *
 * SUMMARY:
 *   This story instantiates the universalist reading of the Rome Statute's
 *   jurisdictional kernel: the claim that the Court's mandate over core
 *   crimes transcends state consent, reaching non-party nationals through
 *   territorial triggers and Security Council referrals, with victims defined
 *   by the crime rather than by their state's ratification. The claim/metric
 *   gap is deliberate and independent: the reading is CLAIMED as a tangled
 *   rope because it possesses both a genuine coordination function (a
 *   standing backstop against impunity) and asymmetric, actively enforced
 *   costs (sovereignty override borne unevenly), while the metrics are
 *   authored from what descriptively holds of the arrangement's operation
 *   from the universalist seat — moderate extraction that has crept upward as
 *   territorial-trigger doctrine matured, rising theatrical share as
 *   unexecuted warrants accumulate, and hardening enforcement machinery. The
 *   engine computes per-seat classifications from the structural data;
 *   nothing here reconciles the claim to the metrics. KEY AGENTS (by
 *   structural relationship): - victims_of_atrocity_crimes: primary
 *   beneficiary (powerless/trapped) — receives forum access regardless of
 *   state consent - international_criminal_court: agenda-setter and
 *   institutional beneficiary (institutional/constrained) — administers
 *   jurisdiction and collects authority with each expansion -
 *   human_rights_advocacy_community: secondary beneficiary (organized/mobile)
 *   - weak_nonparty_state_leaderships: primary payer (moderate/constrained) —
 *   bears sovereignty override without great-power cover -
 *   accused_officials_of_nonparty_states: payer (powerless/trapped) -
 *   rome_statute_state_parties: dual-positioned beneficiary/payer
 *   (organized/constrained) - great_power_nonparties: excluded seat
 *   (powerful/arbitrage) — nominally targeted by the claim, practically
 *   insulated - public_international_law_doctrine: analytical observer — sees
 *   the full structure
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rome_statute_jurisdiction__universalist_reading, 0.48).
domain_priors:suppression_score(rome_statute_jurisdiction__universalist_reading, 0.52).
domain_priors:theater_ratio(rome_statute_jurisdiction__universalist_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rome_statute_jurisdiction__universalist_reading, tangled_rope).
narrative_ontology:human_readable(rome_statute_jurisdiction__universalist_reading, "Rome Statute Universal Mandate Reading (Jurisdiction Transcending Consent)").
narrative_ontology:topic_domain(rome_statute_jurisdiction__universalist_reading, "legal/international/institutional").

domain_priors:requires_active_enforcement(rome_statute_jurisdiction__universalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rome_statute_jurisdiction__universalist_reading, '9a12d2c6-2905-4ba8-ac5e-60a1d27e0cd3').
narrative_ontology:cs_kernel_codification('9a12d2c6-2905-4ba8-ac5e-60a1d27e0cd3', fixed_text).
narrative_ontology:cs_authority_grounding('9a12d2c6-2905-4ba8-ac5e-60a1d27e0cd3', lineage).
narrative_ontology:cs_interpretation_layer_present('9a12d2c6-2905-4ba8-ac5e-60a1d27e0cd3').
narrative_ontology:cs_reading_relation('9a12d2c6-2905-4ba8-ac5e-60a1d27e0cd3', rome_statute_jurisdiction__sovereigntist_reading, forecloses).
narrative_ontology:cs_reading_relation('9a12d2c6-2905-4ba8-ac5e-60a1d27e0cd3', rome_statute_jurisdiction__hybrid_complementarity_reading, influences).
narrative_ontology:cs_axiom('9a12d2c6-2905-4ba8-ac5e-60a1d27e0cd3', foundational, core_crime_jurisdiction_derives_from_crime_character_not_consent).
narrative_ontology:cs_axiom_status(core_crime_jurisdiction_derives_from_crime_character_not_consent, holdable).
narrative_ontology:cs_axiom_grounding('9a12d2c6-2905-4ba8-ac5e-60a1d27e0cd3', core_crime_jurisdiction_derives_from_crime_character_not_consent, deontological).
narrative_ontology:cs_axiom('9a12d2c6-2905-4ba8-ac5e-60a1d27e0cd3', secondary, territorial_presence_suffices_for_nonparty_exposure).
narrative_ontology:cs_axiom_status(territorial_presence_suffices_for_nonparty_exposure, holdable).
narrative_ontology:cs_axiom_grounding('9a12d2c6-2905-4ba8-ac5e-60a1d27e0cd3', territorial_presence_suffices_for_nonparty_exposure, conventional).
narrative_ontology:cs_reference_frame('9a12d2c6-2905-4ba8-ac5e-60a1d27e0cd3', nuremberg_lineage_universal_mandate).
narrative_ontology:cs_drift_state('9a12d2c6-2905-4ba8-ac5e-60a1d27e0cd3', contemporary_enforcement_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9a12d2c6-2905-4ba8-ac5e-60a1d27e0cd3', '').
narrative_ontology:cs_kernel_id(rome_statute_jurisdiction__universalist_reading, rome_statute_jurisdiction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__universalist_reading, victims_of_atrocity_crimes).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__universalist_reading, international_criminal_court).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__universalist_reading, human_rights_advocacy_community).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__universalist_reading, weak_nonparty_state_leaderships).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__universalist_reading, accused_officials_of_nonparty_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__universalist_reading, rome_statute_state_parties).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__universalist_reading, rome_statute_state_parties).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Survivors and families of genocide, war crimes, and crimes against humanity committed in territories whose governments never joined the Rome Statute or cannot prosecute. They receive access to a standing forum regardless of whether their state consented, and they bear continued insecurity wherever warrants go unexecuted. Relocating away from the aftermath of the crimes is rarely possible, so their access to the forum is tied to where they live.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, victims_of_atrocity_crimes, beneficiary,
    powerless, biographical, trapped, global).

% The judges, prosecutors, and registry staff operating the permanent court. They decide when situations fall within reach, issue warrants, and request state cooperation; each jurisdictional enlargement enlarges their docket, their budget case before the Assembly, and their institutional weight. They cannot relocate their authority or abandon pending situations without dissolving the purpose of the institution they constitute.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, international_criminal_court, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(rome_statute_jurisdiction__universalist_reading, international_criminal_court, beneficiary).

% NGOs, legal clinics, and campaign organizations that document atrocities, press for referrals, and demand arrests. They gain a concrete institutional target for advocacy and a forum in which victim narratives acquire legal form. If the court's usefulness faded they could redirect staff and funding to other causes, so their commitment is strong but not captive.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, human_rights_advocacy_community, beneficiary,
    organized, generational, mobile, global).

% Governments of states that never ratified the Statute and lack great-power protection, whose officials become prosecutable through territorial triggers or Security Council referrals. They lose control over criminal process touching their territory and personnel. Formal withdrawal from the treaty does not remove exposure where a neighboring state party supplies the territorial link or a Council referral supplies jurisdiction, so exit narrows to diplomatic resistance and non-cooperation.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, weak_nonparty_state_leaderships, payer,
    moderate, biographical, constrained, regional).

% Individuals — heads of state, commanders, militia leaders — indicted for conduct connected to non-party contexts. They face trial in a forum their government never accepted, custody depends on foreign arrest, and international travel and diplomacy narrow sharply once a warrant issues. Once in custody they have no parallel forum to appeal to.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, accused_officials_of_nonparty_states, payer,
    powerless, biographical, trapped, global).

% The roughly 125 ratifying states. They gain a shared backstop against atrocity impunity and mutual assurance that no member becomes a safe harbor. They also carry arrest duties that force costly choices when indicted officials transit their territory, and they fund the court through assessed contributions. Withdrawal is legally open and has been exercised, but it carries reputational cost and does not undo exposure accumulated while a member.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, rome_statute_state_parties, beneficiary,
    organized, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(rome_statute_jurisdiction__universalist_reading, rome_statute_state_parties, payer).

% The United States, Russia, and China — states outside the treaty whose consent the universalist claim treats as unnecessary. They are formally exposed by the reading through territorial links and allied-territory situations, yet practically insulated by non-membership, Security Council veto power, bilateral immunity agreements, and sanctions leverage directed at court officials. They never joined the negotiating consensus and register their opposition through non-cooperation and pressure rather than through any seat in the arrangement.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, great_power_nonparties, excluded,
    powerful, generational, arbitrage, global).

% Scholars, jurists, and professional associations debating what the Statute establishes. They observe the full structure — text, drafting history, case law, and state practice — and their writings shape how future judges and foreign ministries read the consent question. They hold no material stake in outcomes beyond professional reputation.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, public_international_law_doctrine, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rome_statute_jurisdiction__universalist_reading, international_criminal_court).
narrative_ontology:fixing_cost_class(rome_statute_jurisdiction__universalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a standing backstop forum for genocide, war crimes, and crimes against humanity when national systems are unwilling or unable to act, so that impunity does not depend on the perpetrator's own government or on the ad hoc convening of great-power will.
% TRANSFER_FUNCTION: Moves adjudicative authority — and the sovereign prerogative it rests on — from national governments, including non-consenting ones reached through territorial triggers or Council referrals, to a permanent international court; moves accountability costs onto accused individuals and their sponsoring states; moves recognition and procedural standing to victims.
% ABSENT_VOICES: Great-power nonparties and targeted non-party governments would object that jurisdiction without consent violates sovereign equality and equals taxation of the politically weak; their objection exists only as non-cooperation, withdrawal, and sanctions pressure, never as a seat, because they declined or were denied entry to the arrangement whose reach now touches them. Defense-counsel communities would also note the resource asymmetry between the permanent prosecution and individual accused.
% DISAPPEARANCE_RATIONALE: If the universalist jurisdictional claim vanished overnight, victims in non-party situations would lose their only available forum, the pending situations resting on territorial triggers and Council referrals would collapse, and the precedent that physical presence within a consenting state suffices to expose non-party nationals would evaporate — the entire enforcement architecture built on that reading would need to be renegotiated situation by situation.
% FOUNDING_PROBLEM: After Nuremberg and the ad hoc tribunals for Yugoslavia and Rwanda, the founding problem was that atrocity perpetrators were shielded by their own states and by sovereignty, and that case-by-case tribunals were slow, expensive, and dependent on great-power convenience; a permanent court with reach beyond consenting states was built so impunity would not hinge on where the crimes were committed or who committed them.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties by UN General Assembly practice treating impunity as a continuing problem, by national courts of transit states applying arrest duties over diplomatic objections (the Jordanian Court of Cassation in the al-Bashir matter), and by the court's own Independent Expert Review documenting unmet investigative and enforcement needs. No major non-party government attests the founding problem in terms that support this reading; their recorded dissent is itself signal about where the arrangement's costs fall.
narrative_ontology:disappearance_verdict(rome_statute_jurisdiction__universalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(rome_statute_jurisdiction__universalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rome_statute_jurisdiction__universalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(rome_statute_jurisdiction__universalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rome_statute_jurisdiction__universalist_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness sits at 0.48 at interval end because the universalist seat itself acknowledges the arrangement's asymmetric operation: territorial-trigger doctrine binds the unprotected first, great-power nonparties self-insulate, and the sovereignty cost falls on states with the least capacity to resist — while genuine convictions (Lubanga, Ntaganda, Ongwen, Al Mahdi) deliver the coordination good. Suppression is 0.52 because persistence depends on active machinery — arrest cooperation, non-cooperation findings, Assembly-level defense against sanctions episodes — not on participant preference; suppression is authored as a raw structural property and is not scaled by power or scope. Theater ratio 0.32 reflects a real functioning trial docket alongside a growing performative share: long-unexecuted warrants, announcements without follow-through, and resolutions that substitute statement for custody. Accessibility collapse is low (0.35) because alternatives persist and remain lawful — national prosecutions, ad hoc tribunals, hybrid courts — so understanding the universalist claim does not close the option space. Resistance is high (0.62): withdrawals, systematic non-cooperation, and direct sanctions pressure against court personnel. The temporal series run on one shared grid (points 0, 4, 8, 12, 16, 20, 24 of a 2002-to-present span); the suppression_requirement series is authored because this story specifically traces enforcement intensification — the warrant-execution confrontations of the last decade — not merely shifting extraction.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seat compute differently by construction. From the court's position the arrangement is the coordination mechanism it exists to administer, and each jurisdictional expansion is mission fulfillment; from the weak non-party leadership seat the same expansion is a loss of control over criminal process imposed without consent and without reciprocal protection. The great-power excluded seat computes a third way: nominally targeted by the universalist claim yet practically subsidized by insulation, so its experienced burden is far below its formal exposure. State parties straddle the line — beneficiaries of the backstop, payers of arrest-duty embarrassments and assessments. The engine computes these divergences from power, exit, and directional data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries map to the low-directionality end: victims of atrocity crimes (powerless, trapped) derive near-full-beneficiary directionality amplified by their trapped exit — they cannot shop for another forum; the court itself collects authority and budget with each expansion; the advocacy community benefits with mobile exit, damping its stake slightly. Declared victims map to the high end: weak non-party leaderships (constrained exit — withdrawal does not remove exposure) and accused officials (trapped) sit near full-target, and their effective extraction is amplified by the global-to-regional scope mismatch that makes verification and reciprocity impossible for them. The one override corrects the great-power nonparty seat: the derivation chain finds no beneficiary or victim declaration for it and would fall back to a power-atom default, but the true relationship is nominal targeting combined with practical insulation — d pinned at 0.30, below symmetric, because arbitration-grade exit converts formal exposure into effective subsidy.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — impunity shielded by sovereignty — is live, so no mandatrophy declaration is authored and none should fire. The tangled-rope classification is what prevents mislabeling in both directions: reading the arrangement as pure coordination (rope) would erase the documented asymmetry in whose sovereignty is overridden and whose nationals appear in the dock; reading it as pure extraction (snare) would erase the real accountability delivered to victims who demonstrably gain a forum they otherwise lack. The piton risk is real but conditional and is carried by the legitimacy-feedback omega: if unexecuted warrants and funding erosion continue compounding, the arrangement could decay toward theatrical maintenance of a mandate it can no longer execute — the theater-ratio series is the early-warning instrument for exactly that transition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_rome_statute,
    'Is the universalist reading settled by the Statute''s text and object-and-purpose, or is it one live interpretation among the sovereigntist and hybrid-complementarity readings of the same kernel?',
    'Textual and drafting-history analysis of Articles 12(2)(a), 13(b), and the preamble, together with Assembly practice and the judicial treatment of territorial jurisdiction in the Bangladesh/Myanmar and Palestine decisions.',
    'If the sovereigntist reading prevails, the victim set shrinks to populations of consenting states, non-party exposure disappears, and this constraint''s extraction profile collapses toward a narrow consent-based coordination arrangement; if the universalist reading consolidates, sovereignty override becomes the operative rule for core crimes and the payer seats'' burden stands as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_rome_statute, conceptual, 'This constraint is one reading of the rome_statute_jurisdiction kernel; sibling readings instantiate different victim sets and sovereignty treatments from the same text.').

omega_variable(
    selective_application_structural_or_political,
    'Does the concentration of cases on weaker non-party contexts reflect the territorial-trigger mechanism binding the unprotected first, or contingent referral politics that could in principle equalize?',
    'Comparative docket analysis controlling for atrocity incidence, territorial-link frequency, and state power across all eligible situations.',
    'If structural, effective extraction amplifies at the weak-payer seats and the arrangement drifts toward enforced asymmetry with snare-flavored per-seat classifications; if political, remedies lie in referral reform without reclassification of the underlying structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selective_application_structural_or_political, empirical, 'Whether the observed application asymmetry is a property of the mechanism or of its operators.').

omega_variable(
    complementarity_absorption_share,
    'What share of the universal mandate is absorbed by genuine national proceedings rather than overridden by admissibility findings?',
    'Longitudinal statistics on admissibility challenges, national-prosecution outcomes in situation countries, and the court''s own willingness-to-defer record.',
    'High absorption supports a coordination-dominant profile with modest payer burden; low absorption means the international layer carries the load and the sovereignty costs imposed on non-consenting states rise correspondingly.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(complementarity_absorption_share, empirical, 'How much of the mandate national systems actually execute versus the international layer overriding.').

omega_variable(
    legitimacy_feedback_loop,
    'Does accumulated non-enforcement — unexecuted warrants, state non-cooperation, funding shortfalls — erode the authority claim faster than new situations replenish it?',
    'Track warrant execution rates, non-cooperation findings, and assessed-contribution arrears across successive Assembly sessions against new situation openings.',
    'Net erosion pushes the arrangement toward inertial, performance-heavy maintenance and eventual piton-like decay; net consolidation stabilizes the tangled-rope profile with enforcement capacity matching the mandate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_feedback_loop, empirical, 'Persistence question: whether the mandate''s authority compounds or decays under enforcement failure.').

omega_variable(
    sovereignty_override_valence,
    'Is the sovereignty cost imposed on non-consenting states an extraction to be weighed against the coordination good, or the definitional price of any enforceable criminal law over atrocity?',
    'Not resolvable by data alone: depends on whether one frames criminal jurisdiction over core crimes as presumptively sovereign or presumptively universal — a framing choice made visible by comparing how domestic criminal law treats territorial strangers.',
    'If the latter, the measured burden at payer seats reads as inherent coordination cost and per-seat classifications soften toward rope; if the former, payer-seat extraction stands as authored and the asymmetry critique carries full weight.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sovereignty_override_valence, preference, 'Framing-dependent valuation of the sovereignty override at the heart of the universalist claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rome_statute_jurisdiction__universalist_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rome_tr_t0, rome_statute_jurisdiction__universalist_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(rome_tr_t4, rome_statute_jurisdiction__universalist_reading, theater_ratio, 4, 0.2).
narrative_ontology:measurement(rome_tr_t8, rome_statute_jurisdiction__universalist_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement(rome_tr_t12, rome_statute_jurisdiction__universalist_reading, theater_ratio, 12, 0.25).
narrative_ontology:measurement(rome_tr_t16, rome_statute_jurisdiction__universalist_reading, theater_ratio, 16, 0.28).
narrative_ontology:measurement(rome_tr_t20, rome_statute_jurisdiction__universalist_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(rome_tr_t24, rome_statute_jurisdiction__universalist_reading, theater_ratio, 24, 0.32).

% Extraction over time
narrative_ontology:measurement(rome_be_t0, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 0, 0.36).
narrative_ontology:measurement(rome_be_t4, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 4, 0.39).
narrative_ontology:measurement(rome_be_t8, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 8, 0.41).
narrative_ontology:measurement(rome_be_t12, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 12, 0.43).
narrative_ontology:measurement(rome_be_t16, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 16, 0.45).
narrative_ontology:measurement(rome_be_t20, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 20, 0.46).
narrative_ontology:measurement(rome_be_t24, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 24, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(rome_su_t0, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(rome_su_t4, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 4, 0.4).
narrative_ontology:measurement(rome_su_t8, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 8, 0.43).
narrative_ontology:measurement(rome_su_t12, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 12, 0.45).
narrative_ontology:measurement(rome_su_t16, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 16, 0.48).
narrative_ontology:measurement(rome_su_t20, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(rome_su_t24, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 24, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rome_statute_jurisdiction__universalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__universalist_reading, rome_statute_jurisdiction__sovereigntist_reading).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__universalist_reading, rome_statute_jurisdiction__hybrid_complementarity_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Rome Statute jurisdiction' decomposes into three structurally distinct constraints sharing one kernel text. The universalist reading (this file) reads the consent architecture as a default channel over a pre-existing universal mandate; the sovereigntist reading reads the same articles as a hard limit; the hybrid-complementarity reading balances the two through admissibility. Their epsilon values differ because their victim sets differ: universalist reach creates non-party payer seats the sovereigntist reading does not recognize. The universalist reading sits upstream of the hybrid reading — each territorial-trigger ruling shifts complementarity's baseline toward universal reach — and is cited by advocates as evidence against the sovereigntist limit. All three files link one another through network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rome_statute_jurisdiction__universalist_reading, powerful, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
