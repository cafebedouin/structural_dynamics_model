% ============================================================================
% CONSTRAINT STORY: quran_ontological_status__state_enforced_creation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_ontological_status__state_enforced_creation_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: quran_ontological_status__state_enforced_creation_reading
 *   human_readable: Mihna: State-Enforced Doctrine of Qur'anic Createdness
 *   domain: Islamic Theology / Philosophy of Language / Political Authority
 *
 * SUMMARY:
 *   This story instantiates the state_enforced_creation_reading of the
 *   contested quran_ontological_status kernel: the theological claim that the
 *   Qur'an is created divine speech, as adopted by Mu'tazilite rationalist
 *   theology, is not authored here as a metaphysical position on its own
 *   merits but as the doctrinal content of a state inquisition (mihna,
 *   roughly 833-848 CE under al-Ma'mun, al-Mu'tasim, and al-Wathiq). The
 *   mihna compelled judges, scholars, and other officials to publicly affirm
 *   createdness on pain of imprisonment, flogging, and professional
 *   destruction. Ahmad ibn Hanbal's imprisonment and torture for refusing to
 *   recant is the paradigm case. The reading treats the metaphysical claim as
 *   instrumentalized: whatever the truth or defensibility of Mu'tazilite
 *   kalam on its own theological terms (that question belongs to the sibling
 *   created_reading, not this story), this particular constraint is the
 *   fusion of that claim with caliphal coercive power, and it is that fusion,
 *   not the doctrine in isolation, that the extractiveness and suppression
 *   metrics describe.
 *
 * KEY AGENTS:
 *   - abbasid_caliphal_authority: agenda_setter/beneficiary (institutional/arbitrage) - runs the tribunals and gains centralized doctrinal authority
 *   - mutazilite_court_theologians: beneficiary (organized/mobile) - gain patronage and enforcement backing for a previously contested position
 *   - traditionalist_hadith_scholars: payer (moderate/trapped) - imprisoned, flogged, or dismissed for refusing to recant, exemplified by Ahmad ibn Hanbal
 *   - literalist_communities: payer (powerless/trapped) - pressured through local scholars with no direct voice in the tribunals
 *   - historians_of_islamic_theology: observer (analytical/analytical) - reconstruct the tribunal mechanics from primary sources
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_ontological_status__state_enforced_creation_reading, 0.81).
domain_priors:suppression_score(quran_ontological_status__state_enforced_creation_reading, 0.9).
domain_priors:theater_ratio(quran_ontological_status__state_enforced_creation_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_ontological_status__state_enforced_creation_reading, snare).
narrative_ontology:human_readable(quran_ontological_status__state_enforced_creation_reading, "Mihna: State-Enforced Doctrine of Qur'anic Createdness").
narrative_ontology:topic_domain(quran_ontological_status__state_enforced_creation_reading, "Islamic Theology / Philosophy of Language / Political Authority").

domain_priors:requires_active_enforcement(quran_ontological_status__state_enforced_creation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_ontological_status__state_enforced_creation_reading, 'af5594b5-3699-4d9f-8e67-900deb00c3b9').
narrative_ontology:cs_kernel_codification('af5594b5-3699-4d9f-8e67-900deb00c3b9', distributed).
narrative_ontology:cs_authority_grounding('af5594b5-3699-4d9f-8e67-900deb00c3b9', extraction).
narrative_ontology:cs_interpretation_layer_present('af5594b5-3699-4d9f-8e67-900deb00c3b9').
narrative_ontology:cs_reading_relation('af5594b5-3699-4d9f-8e67-900deb00c3b9', quran_ontological_status__uncreated_reading, coexists_with).
narrative_ontology:cs_reading_relation('af5594b5-3699-4d9f-8e67-900deb00c3b9', quran_ontological_status__created_reading, influences).
narrative_ontology:cs_axiom('af5594b5-3699-4d9f-8e67-900deb00c3b9', foundational, caliph_holds_authority_to_adjudicate_orthodoxy_by_force).
narrative_ontology:cs_axiom_status(caliph_holds_authority_to_adjudicate_orthodoxy_by_force, overridden).
narrative_ontology:cs_axiom_grounding('af5594b5-3699-4d9f-8e67-900deb00c3b9', caliph_holds_authority_to_adjudicate_orthodoxy_by_force, conventional).
narrative_ontology:cs_axiom('af5594b5-3699-4d9f-8e67-900deb00c3b9', secondary, public_affirmation_under_duress_constitutes_valid_doctrinal_consensus).
narrative_ontology:cs_axiom_status(public_affirmation_under_duress_constitutes_valid_doctrinal_consensus, overridden).
narrative_ontology:cs_axiom_grounding('af5594b5-3699-4d9f-8e67-900deb00c3b9', public_affirmation_under_duress_constitutes_valid_doctrinal_consensus, instrumental).
narrative_ontology:cs_reference_frame('af5594b5-3699-4d9f-8e67-900deb00c3b9', caliphal_religious_authority_precedent).
narrative_ontology:cs_drift_state('af5594b5-3699-4d9f-8e67-900deb00c3b9', post_mutawakkil_reversal, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('af5594b5-3699-4d9f-8e67-900deb00c3b9', '').
narrative_ontology:cs_kernel_id(quran_ontological_status__state_enforced_creation_reading, quran_ontological_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_ontological_status__state_enforced_creation_reading, abbasid_caliphal_authority).
narrative_ontology:constraint_beneficiary(quran_ontological_status__state_enforced_creation_reading, mutazilite_court_theologians).
narrative_ontology:constraint_victim(quran_ontological_status__state_enforced_creation_reading, traditionalist_hadith_scholars).
narrative_ontology:constraint_victim(quran_ontological_status__state_enforced_creation_reading, literalist_communities).
narrative_ontology:constraint_victim(quran_ontological_status__state_enforced_creation_reading, scholarly_pluralism_itself).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The caliph (initially al-Ma'mun, continued under al-Mu'tasim and al-Wathiq) institutes the mihna, compelling judges, scholars, and traders to publicly affirm the createdness of the Qur'an under threat of imprisonment, flogging, or removal from office. Frames the policy as rational theological correction and as an assertion of caliphal authority over religious interpretation itself, competing with the independent authority hadith scholars had accumulated. Controls the tribunal apparatus and can end or redirect it at will.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, abbasid_caliphal_authority, agenda_setter,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(quran_ontological_status__state_enforced_creation_reading, abbasid_caliphal_authority, beneficiary).

% Rationalist theologians whose doctrine of divine transcendence and created speech is elevated to state orthodoxy. Gain court patronage, judicial appointments, and the power to examine and disqualify rivals. Their theological position, previously one contested school among several, becomes backed by inquisitorial force; if the state's backing withdraws, their doctrinal position reverts to being one contested view among others.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, mutazilite_court_theologians, beneficiary,
    organized, biographical, mobile, regional).

% Scholars who hold the Qur'an to be uncreated eternal speech are summoned before tribunals and required to affirm createdness publicly. Ahmad ibn Hanbal, most prominently, refuses, is imprisoned and flogged over an extended period rather than recant. Others recant under duress, are dismissed from judicial and teaching posts, or flee. Their exit options are effectively closed: recantation, imprisonment, or flight are the only paths, and flight forfeits standing and livelihood built over a lifetime of scholarship.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, traditionalist_hadith_scholars, payer,
    moderate, biographical, trapped, regional).

% Ordinary believers, students, and local prayer leaders who hold the traditional creed are pressured through their local scholars and mosques to accept the new orthodoxy or face social and sometimes legal marginalization. They have no direct access to the tribunals to contest the policy and depend entirely on the resistance of scholars like Ibn Hanbal to have any voice at all.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, literalist_communities, payer,
    powerless, biographical, trapped, local).

% The prior norm under which multiple theological schools (Mu'tazilite, traditionalist, and others) coexisted and disputed without state coercion is displaced by a regime in which the caliph adjudicates orthodoxy by force. Even after the mihna ends, the precedent that the state can compel doctrinal affirmation persists as a template later invoked by other authorities against other minorities.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, scholarly_pluralism_itself, payer,
    powerless, civilizational, trapped, regional).
narrative_ontology:stakeholder_non_agent(quran_ontological_status__state_enforced_creation_reading, scholarly_pluralism_itself).

% The scholarly consensus that eventually forms after the mihna's collapse under al-Mutawakkil, affirming the Qur'an's uncreatedness as normative Sunni doctrine, has no voice inside the tribunals themselves — it exists only as the eventual historical verdict rendered after the fact, once the coercive apparatus is dismantled. Its later dominance retroactively frames the mihna as a failed and illegitimate imposition, but this vindication was unavailable to those coerced at the time.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, later_sunni_traditionalist_consensus, excluded,
    organized, civilizational, analytical, regional).

% Scholars examining primary sources (al-Tabari's chronicles, biographical dictionaries, Ibn Hanbal's own accounts) to reconstruct the tribunal mechanics, the coercion applied, and the eventual reversal. Their analysis is what allows the theological content of the createdness doctrine to be distinguished from its instrumentalization as a state suppression tool.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, historians_of_islamic_theology, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_ontological_status__state_enforced_creation_reading, abbasid_caliphal_authority).
narrative_ontology:fixing_cost_class(quran_ontological_status__state_enforced_creation_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In principle, a single state-recognized theological position could reduce sectarian friction in judicial appointments and public religious instruction by settling a contested metaphysical question through central authority rather than leaving it perpetually disputed among rival schools.
% TRANSFER_FUNCTION: Moves professional standing, judicial office, physical safety, and freedom of religious expression away from traditionalist scholars and literalist communities and toward the caliphal court and the Mu'tazilite theologians it backs; the mechanism of transfer is the tribunal's power to interrogate, imprison, flog, and dismiss.
% ABSENT_VOICES: Literalist believers outside the scholarly elite have no seat in the tribunals at all — their creed is adjudicated entirely by the fate of scholars like Ibn Hanbal, whose personal endurance becomes their only proxy voice. Rival theological schools other than Mu'tazilite rationalism (e.g. Ash'ari positions, which had not yet crystallized) are absent from the dispute as then framed.
% DISAPPEARANCE_RATIONALE: If the mihna's coercive apparatus were removed, tribunals would stop, imprisoned scholars would be released, judicial appointments would no longer hinge on doctrinal affirmation, and the theological dispute over createdness would revert to being contested among schools without state enforcement — which is in fact what happened when al-Mutawakkil ended the policy.
% FOUNDING_PROBLEM: The caliph sought both a rationally defensible account of divine transcendence (avoiding anthropomorphism implied by treating scripture as literally coeternal with God) and a mechanism to assert centralized doctrinal authority over an increasingly independent and popular class of hadith-based traditionalist scholars.
% FOUNDING_PROBLEM_CORROBORATION: Later Abbasid caliphs themselves, beginning with al-Mutawakkil, abandoned the policy and rehabilitated traditionalist scholars, indicating the political utility of the mihna was judged by the state's own successors to have failed or become counterproductive. Independent historians (al-Tabari among contemporaries, and subsequent generations of both Sunni and secular scholarship) corroborate that the inquisition produced martyrdom narratives that strengthened traditionalist legitimacy rather than weakening it — an outcome opposite to its founding aim, attested from outside the Mu'tazilite/caliphal beneficiary set.
narrative_ontology:disappearance_verdict(quran_ontological_status__state_enforced_creation_reading, world_rearranges).
narrative_ontology:founding_problem_status(quran_ontological_status__state_enforced_creation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_ontological_status__state_enforced_creation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(quran_ontological_status__state_enforced_creation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quran_ontological_status__state_enforced_creation_reading, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_ontological_status__state_enforced_creation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quran_ontological_status__state_enforced_creation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quran_ontological_status__state_enforced_creation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness and suppression are both authored high (0.81, 0.9) because the constraint's defining feature is not theological argument but coercive enforcement: tribunals, imprisonment, and flogging compelling public affirmation. Theater ratio is moderate-high (0.58) because much of the tribunal's activity - public interrogation sessions, formal affirmation ceremonies - functioned as performative demonstration of loyalty rather than genuine theological persuasion; the temporal series shows theater climbing as the policy matured into ritualized interrogation and then collapsing to near zero once al-Mutawakkil abolished the mihna. Accessibility collapse is moderate (0.62), not extreme, because exit via recantation remained available to most (few chose Ibn Hanbal's path of refusal), even though refusal carried severe cost. Resistance is high (0.78), reflecting Ibn Hanbal's sustained public refusal and the broader traditionalist community's eventual vindication.
 *
 * PERSPECTIVAL GAP:
 *   From the caliphal seat, the mihna appears as legitimate exercise of doctrinal authority proper to the ruler's office, continuous with earlier caliphal roles in adjudicating religious matters. From the traditionalist scholar's seat, the identical tribunal structure appears as unprecedented coercion of conscience with no theological legitimacy, since orthodoxy in their view is established by scholarly consensus (ijma) and transmitted authority (isnad), not caliphal decree. The engine's per-seat computation should reflect this as a genuine structural asymmetry, not merely differing opinions about the same experience.
 *
 * DIRECTIONALITY LOGIC:
 *   The caliphal authority sits at the full-beneficiary end: it designs the tribunal system, controls its scope and duration, and collects the political good of centralized doctrinal control. Mu'tazilite theologians are secondary beneficiaries whose doctrinal position is elevated by force rather than by argument alone - their benefit is contingent on state backing rather than independent theological victory, which is why their position collapses once the state withdraws support. Traditionalist scholars and literalist communities sit at the full-target end: trapped exit options, direct costs (imprisonment, flogging, dismissal, social marginalization) flowing from the same tribunal structure that the caliph administers.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - centralizing doctrinal authority to reduce reliance on an independently powerful traditionalist scholarly class - is authored as dead: the caliph's own successors abandoned the policy within roughly fifteen years, and the outcome (martyrdom narratives strengthening traditionalist legitimacy) was the opposite of what the policy sought. This is not a case of a coordination function outliving its usefulness and persisting through inertia (which would suggest piton); it is a case of enforced doctrine collapsing outright once its coercive backing was withdrawn, which is characteristic of snare rather than piton - there was no residual institutional momentum, only the tribunal apparatus itself, and when that apparatus was dismantled the constraint vanished rather than persisting as vestigial theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    doctrine_vs_instrument_separability,
    'Is the createdness doctrine''s extractive character intrinsic to the theological claim itself, or entirely a function of the coercive apparatus attached to it by the caliphate - such that the identical doctrine advanced without state backing (as in the sibling created_reading) would carry negligible extraction?',
    'Compare periods and regions where Mu''tazilite createdness doctrine was advanced through scholarly argument without state coercion (e.g., prior to al-Ma''mun''s adoption of it as policy, or in geographically distant regions the mihna did not reach) against the mihna period itself; if extraction and suppression metrics differ sharply, the doctrine and the enforcement are separable.',
    'If fully separable, this confirms the ε-invariance decomposition into three sibling stories was correct and necessary rather than an artifact of author choice; if not fully separable (e.g. because state adoption was sought BY Mu''tazilite theologians as their primary strategy for doctrinal dominance, implicating them beyond mere temporary beneficiary status), the beneficiary structure here may understate their agency in initiating rather than merely accepting state backing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_vs_instrument_separability, conceptual, 'Whether doctrinal content and coercive enforcement are structurally separable in this specific historical episode.').

omega_variable(
    kernel_reading_disagreement_location,
    'Where exactly does this reading''s account diverge from the uncreated_reading and created_reading siblings - is it solely in whether state coercion is present, or does it also involve a first-order disagreement about whether the createdness claim was ever theologically defensible on Mu''tazilite terms?',
    'Textual analysis of Mu''tazilite kalam arguments (e.g. from al-Nazzam, Abu al-Hudhayl) independent of the mihna''s political history, compared against traditionalist counter-arguments (e.g. Ibn Hanbal''s recorded responses under interrogation) to isolate the purely theological dispute from its later instrumentalization.',
    'If the disagreement is purely about the presence/absence of coercion, this reading''s classification (snare) is fully independent of the sibling readings'' classifications (likely rope or tangled_rope for created_reading, and mountain-adjacent or contested for uncreated_reading if authored as a faith claim). If a first-order theological disagreement is also present, that would need to be captured as a further omega in the sibling stories rather than resolved here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'Locating precisely where this reading''s structural claim differs from its siblings within the kernel.').

omega_variable(
    mutazilite_agency_ambiguity,
    'Did Mu''tazilite theologians primarily benefit passively from caliphal adoption of their doctrine, or did influential figures actively lobby for and design the inquisitorial mechanism, making them co-architects of the snare rather than incidental beneficiaries?',
    'Prosopographical study of which Mu''tazilite theologians held tribunal or advisory roles during the mihna versus those who distanced themselves from its coercive methods while retaining the doctrinal position.',
    'If leading Mu''tazilite theologians actively designed the tribunal mechanism, at least some among them should be reclassified from beneficiary toward agenda_setter or a dual role, strengthening rather than diluting the tangled-rope-adjacent reading of their position.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mutazilite_agency_ambiguity, empirical, 'Whether Mu''tazilite theologians were architects or merely beneficiaries of the coercive mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_ontological_status__state_enforced_creation_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(qura_tr_t3, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 3, 0.35).
narrative_ontology:measurement(qura_tr_t6, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 6, 0.5).
narrative_ontology:measurement(qura_tr_t10, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 10, 0.58).
narrative_ontology:measurement(qura_tr_t14, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 14, 0.62).
narrative_ontology:measurement(qura_tr_t17, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 17, 0.55).
narrative_ontology:measurement(qura_tr_t20, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(qura_be_t3, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 3, 0.55).
narrative_ontology:measurement(qura_be_t6, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 6, 0.72).
narrative_ontology:measurement(qura_be_t10, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 10, 0.81).
narrative_ontology:measurement(qura_be_t14, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 14, 0.79).
narrative_ontology:measurement(qura_be_t17, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 17, 0.6).
narrative_ontology:measurement(qura_be_t20, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 20, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(qura_su_t3, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 3, 0.6).
narrative_ontology:measurement(qura_su_t6, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 6, 0.85).
narrative_ontology:measurement(qura_su_t10, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 10, 0.9).
narrative_ontology:measurement(qura_su_t14, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 14, 0.88).
narrative_ontology:measurement(qura_su_t17, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 17, 0.65).
narrative_ontology:measurement(qura_su_t20, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 20, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_ontological_status__state_enforced_creation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(quran_ontological_status__state_enforced_creation_reading, created_reading).
narrative_ontology:affects_constraint(quran_ontological_status__state_enforced_creation_reading, uncreated_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the quran_ontological_status kernel. created_reading and uncreated_reading each author the metaphysical claim on its own theological terms, absent state coercion, and are expected to carry substantially lower ε (likely rope, mountain-adjacent-contested, or tangled_rope depending on how each reading's own advocates frame contested textual evidence). This story (state_enforced_creation_reading) isolates the historically specific fusion of the createdness claim with inquisitorial state power during the mihna (c. 833-848 CE) and is expected to carry the highest ε and suppression of the three, since it alone involves an enforcement apparatus with named victims (Ahmad ibn Hanbal chief among them). All three stories share the same underlying kernel commitment (what is the ontological status of the Qur'an as divine speech) but instantiate structurally distinct constraints with different ε, different stakeholders, and different classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
