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
 *   This story authors the UNIVERSALIST reading of the Rome Statute's
 *   jurisdictional kernel: the claim that ICC authority over core
 *   international crimes extends to non-party nationals through territorial
 *   or Security Council triggers, and that this authority is legitimate
 *   precisely because it does not depend on the accused state's consent. This
 *   is one of three structurally distinct constraints sharing the same kernel
 *   text (the Rome Statute's jurisdictional provisions, Articles 12-13). The
 *   sovereigntist_reading treats the same text as creating a strictly
 *   consent-bound framework where non-party exposure is an anomaly to be
 *   minimized; the hybrid_complementarity_reading treats the same text as a
 *   negotiated balance where national primacy is the default and
 *   international jurisdiction only activates on demonstrated unwillingness
 *   or inability. All three share the treaty text as their kernel but diverge
 *   sharply on what the text authorizes and against whom — they are not the
 *   same constraint measured differently; each carries its own epsilon and
 *   its own stakeholder set, linked here via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - icc_prosecutorial_apparatus: agenda_setter (institutional/analytical) — asserts and defends the sovereignty-transcending jurisdictional claim
 *   - atrocity_crime_victims_globally: primary beneficiary (powerless/trapped) — gains a justice mechanism independent of perpetrator-state consent
 *   - non_party_state_nationals: primary target (moderate/trapped) — exposed to jurisdiction their state never accepted
 *   - non_ratifying_sovereign_states: institutional target (powerful/constrained) — bear territorial-trigger exposure without treaty consent
 *   - permanent_security_council_members_outside_regime: asymmetric dual seat (institutional/arbitrage) — can trigger jurisdiction over others while shielding themselves via veto
 *   - comparative_legal_scholars: analytical observer — assesses the doctrinal basis for the extension beyond consent
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
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rome_statute_jurisdiction__universalist_reading, tangled_rope).
narrative_ontology:human_readable(rome_statute_jurisdiction__universalist_reading, "Rome Statute Jurisdiction — Universalist Reading (Sovereignty-Transcending Mandate)").
narrative_ontology:topic_domain(rome_statute_jurisdiction__universalist_reading, "international_law/treaty_interpretation/institutional_authority").

domain_priors:requires_active_enforcement(rome_statute_jurisdiction__universalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rome_statute_jurisdiction__universalist_reading, '4bd4e4c1-9cb2-4d57-b156-bed5eb5fa352').
narrative_ontology:cs_kernel_codification('4bd4e4c1-9cb2-4d57-b156-bed5eb5fa352', fixed_text).
narrative_ontology:cs_authority_grounding('4bd4e4c1-9cb2-4d57-b156-bed5eb5fa352', lineage).
narrative_ontology:cs_interpretation_layer_present('4bd4e4c1-9cb2-4d57-b156-bed5eb5fa352').
narrative_ontology:cs_reading_relation('4bd4e4c1-9cb2-4d57-b156-bed5eb5fa352', rome_statute_jurisdiction__sovereigntist_reading, forecloses).
narrative_ontology:cs_reading_relation('4bd4e4c1-9cb2-4d57-b156-bed5eb5fa352', rome_statute_jurisdiction__hybrid_complementarity_reading, influences).
narrative_ontology:cs_axiom('4bd4e4c1-9cb2-4d57-b156-bed5eb5fa352', foundational, core_crime_gravity_overrides_state_consent).
narrative_ontology:cs_axiom_status(core_crime_gravity_overrides_state_consent, holdable).
narrative_ontology:cs_axiom_grounding('4bd4e4c1-9cb2-4d57-b156-bed5eb5fa352', core_crime_gravity_overrides_state_consent, deontological).
narrative_ontology:cs_axiom('4bd4e4c1-9cb2-4d57-b156-bed5eb5fa352', secondary, territorial_trigger_binds_non_party_nationals).
narrative_ontology:cs_axiom_status(territorial_trigger_binds_non_party_nationals, holdable).
narrative_ontology:cs_axiom_grounding('4bd4e4c1-9cb2-4d57-b156-bed5eb5fa352', territorial_trigger_binds_non_party_nationals, conventional).
narrative_ontology:cs_reference_frame('4bd4e4c1-9cb2-4d57-b156-bed5eb5fa352', post_nuremberg_individual_accountability_norm).
narrative_ontology:cs_drift_state('4bd4e4c1-9cb2-4d57-b156-bed5eb5fa352', post_2016_withdrawal_wave, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('4bd4e4c1-9cb2-4d57-b156-bed5eb5fa352', '').
narrative_ontology:cs_kernel_id(rome_statute_jurisdiction__universalist_reading, rome_statute_jurisdiction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__universalist_reading, atrocity_crime_victims_globally).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__universalist_reading, icc_prosecutorial_apparatus).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__universalist_reading, human_rights_advocacy_networks).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__universalist_reading, non_party_state_nationals).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__universalist_reading, non_ratifying_sovereign_states).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__universalist_reading, permanent_security_council_members_outside_regime).
narrative_ontology:constraint_vindicates(rome_statute_jurisdiction__universalist_reading, jus_cogens_supremacy_over_state_consent).
narrative_ontology:constraint_vindicates(rome_statute_jurisdiction__universalist_reading, individual_criminal_responsibility_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Initiates investigations and prosecutions, including against nationals of states that never ratified the Statute, when the alleged crime occurs on the territory of a state party or is referred by the UN Security Council. Interprets its own mandate as flowing from the gravity of the crimes themselves rather than from the consent of the accused's state, and defends this reading actively in jurisdictional challenges.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, icc_prosecutorial_apparatus, agenda_setter,
    institutional, civilizational, analytical, global).

% Persons harmed by genocide, crimes against humanity, and war crimes, wherever committed. Under this reading, their claim to justice runs independent of whether their home government or the perpetrator's government consented to any tribunal, since a national justice system may be unwilling or unable to prosecute its own officials or nationals.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, atrocity_crime_victims_globally, beneficiary,
    powerless, generational, trapped, global).

% Nationals of states that never ratified the Rome Statute can still be prosecuted if their conduct occurs on the territory of a state party or is referred by the Security Council. They bear the exposure of an international jurisdiction their own government never accepted, with no mechanism to opt their nationality out of the territorial trigger.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, non_party_state_nationals, payer,
    moderate, biographical, trapped, global).

% States that declined to join the Rome Statute — including major military powers — find their armed forces and officials nonetheless exposed to ICC jurisdiction when operating on state-party territory. Their non-consent is treated as immaterial to the territorial trigger; their only real recourse is refusing to cooperate with warrants, which invites diplomatic and reputational costs rather than legal immunity.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, non_ratifying_sovereign_states, payer,
    powerful, generational, constrained, national).

% Non-party permanent members can trigger ICC jurisdiction over other states via Security Council referral while shielding themselves and allies from referral through veto power. They are simultaneously exposed to the universalist logic in principle and structurally insulated from its application to themselves in practice — an asymmetry the universalist reading treats as a political defect in implementation, not a refutation of the jurisdictional claim itself.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, permanent_security_council_members_outside_regime, payer,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(rome_statute_jurisdiction__universalist_reading, permanent_security_council_members_outside_regime, agenda_setter).

% NGOs, victim-representation groups, and international legal scholars who advocate for and litigate under the universalist reading gain standing, funding, and institutional legitimacy from an expansive jurisdictional mandate. Their professional and ideological investment is in the mandate's reach, not its narrowing.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, human_rights_advocacy_networks, beneficiary,
    organized, civilizational, mobile, global).

% States that did ratify bear the territorial trigger's exposure on their own soil for crimes by anyone, party or not, yet have little practical voice over whether powerful non-party states' nationals are actually surrendered when warrants issue — their consent secured the treaty's authority but does not secure its even application.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, ratifying_state_parties, excluded,
    powerful, generational, constrained, national).

% Study the doctrinal basis for the territoriality and UNSC-referral triggers, comparing them to customary international law on jurisdiction and to competing sovereigntist and complementarity readings of the same treaty text.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, comparative_legal_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rome_statute_jurisdiction__universalist_reading, diffuse).
narrative_ontology:fixing_cost_class(rome_statute_jurisdiction__universalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a standing international mechanism to prosecute the gravest crimes when national systems are unwilling or unable to act, preventing impunity gaps that a purely consent-based system would leave open whenever the accused's own state controls prosecution.
% TRANSFER_FUNCTION: Moves prosecutorial authority and the practical burden of legal exposure from the accused's national government (which under a strict-consent regime would hold exclusive say) to the ICC and to whichever state-party territory or Security Council referral triggers jurisdiction, without requiring the accused's home state's agreement.
% ABSENT_VOICES: Non-party states never had a vote in the Rome Statute's drafting compromises they are nonetheless subject to via the territorial trigger; their objection — that criminal jurisdiction requires state consent under classical international law — is treated by this reading as an argument the Statute's core-crimes logic has already answered, not as a live veto.
% DISAPPEARANCE_RATIONALE: Proponents say if the universalist mandate disappeared, atrocity accountability would collapse into whatever the perpetrator's own state permits, restoring near-total impunity for state-sponsored crimes. Non-party states and sovereigntist readings say the world would simply revert to the pre-Rome baseline of consent-based jurisdiction that governed international law for centuries — arrangements many states never structured around ICC exposure in the first place.
% FOUNDING_PROBLEM: The postwar and post-Cold War recognition that atrocity crimes committed by or with state complicity routinely go unprosecuted domestically because the very authorities who would prosecute are often complicit, creating a structural impunity gap that no purely national or purely consent-based international system could close.
% FOUNDING_PROBLEM_CORROBORATION: The ICC and allied advocacy networks attest the impunity gap remains live, citing ongoing situations where domestic prosecution is politically impossible. Independent corroboration exists from UN commissions of inquiry and non-governmental atrocity documentation bodies outside the Court's own structure. Non-party states and some international law scholars outside both the Court and the advocacy networks counter that the founding problem, as originally framed around state consent to adjudicate, has been reinterpreted expansively beyond what ratifying states bargained for — a status dispute this reading treats as resolved in favor of the broader mandate, others do not.
narrative_ontology:disappearance_verdict(rome_statute_jurisdiction__universalist_reading, contested).
narrative_ontology:founding_problem_status(rome_statute_jurisdiction__universalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rome_statute_jurisdiction__universalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extraction (0.58) is authored moderate-high because the universalist reading imposes real legal exposure and diplomatic costs on non-consenting states and their nationals, but the exposure is bounded by practical enforcement limits (no ICC police force; reliance on state cooperation for arrest and surrender), which caps how much can actually be extracted relative to a fully self-executing regime. Suppression (0.62) reflects the active diplomatic, legal, and at times economic pressure applied to secure cooperation with warrants against non-party nationals — arrest requests, extradition pressure, and reputational sanctioning of non-cooperating states. Theater ratio (0.40) is authored moderate because a meaningful share of the apparatus's activity (investigation standards, victim participation mechanisms, complementarity assessments) is functionally real, but a growing share is symbolic — warrants that will likely never be executed against powerful non-party nationals, which the Court nonetheless issues to assert the jurisdictional claim itself. Accessibility collapse (0.50) is moderate: non-party states retain the real alternative of simply refusing cooperation, so alternatives have not collapsed as completely as they would under a mountain-type claim. Resistance (0.78) is high and rising, reflecting sustained non-party pushback (non-ratification, non-cooperation, counter-legislation such as blocking statutes) precisely because the universalist claim is actively and visibly contested by powerful states rather than quietly accepted.
 *
 * PERSPECTIVAL GAP:
 *   From the ICC/advocacy seat, this is a rope or perhaps a scaffold toward a fully universal justice system — legitimate authority responding to a real gap. From the non-party-state seat, the same jurisdictional claim looks like an institution asserting authority it was never granted, enforced through reputational and diplomatic coercion rather than consent — closer to a tangled rope or, in the most skeptical non-party framing, a snare wearing coordination language. The engine computes these divergent seat classifications from the same structural data; this story does not adjudicate between them, only authors the data honestly from the universalist reading's own vantage.
 *
 * DIRECTIONALITY LOGIC:
 *   Atrocity crime victims and human rights advocacy networks sit near the beneficiary end: the expansive mandate is what they organize around and what serves their stated interest in closing impunity gaps. Non-party state nationals and non-ratifying states sit near the target end: they bear jurisdictional exposure they never consented to, with only diplomatic (not legal) exit options. The Security Council permanent members outside the regime occupy a genuinely asymmetric seat — they can wield the universalist logic offensively (referring situations involving others) while remaining practically insulated from it via veto, which is why they carry both agenda_setter and payer roles; a plain beneficiary/victim derivation would flatten this asymmetry, so the dual role is authored explicitly rather than resolved by override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — impunity for state-complicit atrocity crimes — remains genuinely live by most independent accounts (ongoing situations, documented by bodies outside the Court), which argues against a straightforward mandatrophy verdict; the mandate has not simply outlived its function. But the founding_problem_status is authored contested rather than live outright, because non-party states dispute that the specific jurisdictional REACH claimed today (over their nationals, without their consent) was ever what was bargained for at Rome, as opposed to a subsequent expansive interpretation. The classification prevents mislabeling this as pure extraction (a snare) by requiring the coordination function — closing genuine impunity gaps — to be named and evidenced, while also preventing it from being waved through as pure coordination (a rope) by requiring the identified victims and active enforcement machinery to be named. Tangled rope is the reading's own honest structural self-assessment: real coordination function, real asymmetric cost falling on non-consenting parties, held together by active diplomatic and legal enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consent_transcendence_legitimacy,
    'Does the Rome Statute''s territorial and UNSC-referral jurisdictional triggers genuinely establish authority independent of non-party consent under customary international law, or does the universalist reading over-extend a treaty-based compromise into a claim of universal jurisdiction the drafting states never actually agreed to confer?',
    'Comparative analysis of the Rome Conference travaux preparatoires against subsequent ICC jurisprudence (e.g., the Al-Bashir immunity rulings, the Philippines/Myanmar situations); tracking whether state practice among non-party states treats ICC warrants as legally binding or merely diplomatically costly.',
    'If the travaux and subsequent state practice support the universalist reading, this constraint''s classification as tangled_rope (genuine coordination function plus asymmetric cost) is well-grounded; if they support the sovereigntist reading instead, the universalist reading''s coordination claim is weaker than authored and the constraint would classify closer to snare from the non-party seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_transcendence_legitimacy, conceptual, 'Whether universalist jurisdiction is a legitimate reading of the treaty''s original consent structure or an interpretive overreach.').

omega_variable(
    unsc_permanent_member_asymmetry,
    'Is the structural asymmetry — permanent Security Council members can trigger universalist jurisdiction over others while shielding themselves via veto — a contingent political defect correctable within the universalist framework, or an irreducible structural feature that undermines the framework''s claim to universality?',
    'Track whether any Security Council reform or Rome Statute amendment process ever succeeds in closing the veto-shielding gap; absence of any successful reform over a multi-decade window would support the irreducibility reading.',
    'If irreducible, the universalist reading''s claim to transcend consent asymmetrically favors already-powerful non-party states over less powerful ones, which would push the constraint''s effective classification toward snare for less powerful non-party targets even while it remains rope-like in aspiration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unsc_permanent_member_asymmetry, empirical, 'Whether the P5 veto-shielding asymmetry is a fixable defect or a structural feature of the universalist reading.').

omega_variable(
    sibling_reading_framing_choice,
    'Given that the same treaty text supports three structurally coherent readings (universalist, sovereigntist, hybrid_complementarity), what signals guided classifying THIS constraint under the universalist frame rather than treating the complementarity mechanism as the primary operative text (which would push toward the hybrid reading as the ''true'' operative constraint)?',
    'The choice follows the ICC''s own institutional self-description in jurisdictional rulings (e.g., asserting jurisdiction over non-party nationals via territoriality) as the operative reading being modeled, rather than adjudicating which reading is normatively correct. A different choice — modeling the Court''s stated preference for complementarity as primary — would classify this constraint closer to rope with narrower victim set.',
    'If the hybrid_complementarity framing is adopted as primary instead, the beneficiary/victim structure narrows substantially (victims limited to situations where complementarity assessment finds the state unwilling/unable) and the classification would likely shift toward rope or scaffold rather than tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_framing_choice, conceptual, 'Documents the Omega_C framing choice between three coherent readings of the same kernel text and what would change under an alternative framing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rome_statute_jurisdiction__universalist_reading, 1998, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rome_tr_t1998, rome_statute_jurisdiction__universalist_reading, theater_ratio, 1998, 0.2).
narrative_ontology:measurement(rome_tr_t2002, rome_statute_jurisdiction__universalist_reading, theater_ratio, 2002, 0.25).
narrative_ontology:measurement(rome_tr_t2008, rome_statute_jurisdiction__universalist_reading, theater_ratio, 2008, 0.3).
narrative_ontology:measurement(rome_tr_t2014, rome_statute_jurisdiction__universalist_reading, theater_ratio, 2014, 0.35).
narrative_ontology:measurement(rome_tr_t2020, rome_statute_jurisdiction__universalist_reading, theater_ratio, 2020, 0.38).
narrative_ontology:measurement(rome_tr_t2024, rome_statute_jurisdiction__universalist_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(rome_be_t1998, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 1998, 0.35).
narrative_ontology:measurement(rome_be_t2002, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 2002, 0.42).
narrative_ontology:measurement(rome_be_t2008, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 2008, 0.48).
narrative_ontology:measurement(rome_be_t2014, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 2014, 0.52).
narrative_ontology:measurement(rome_be_t2020, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 2020, 0.56).
narrative_ontology:measurement(rome_be_t2024, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(rome_su_t1998, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 1998, 0.4).
narrative_ontology:measurement(rome_su_t2002, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 2002, 0.48).
narrative_ontology:measurement(rome_su_t2008, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 2008, 0.53).
narrative_ontology:measurement(rome_su_t2014, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 2014, 0.57).
narrative_ontology:measurement(rome_su_t2020, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 2020, 0.6).
narrative_ontology:measurement(rome_su_t2024, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 2024, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rome_statute_jurisdiction__universalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__universalist_reading, rome_statute_jurisdiction__sovereigntist_reading).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__universalist_reading, rome_statute_jurisdiction__hybrid_complementarity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the natural-language concept 'Rome Statute jurisdiction' per the ε-invariance principle: universalist_reading (this file, tangled_rope, moderate-high extraction with named non-party victims), sovereigntist_reading (expected lower extraction, consent-bound framework, likely rope or scaffold from the ratifying-state seat), and hybrid_complementarity_reading (expected lowest extraction of the three, coordination-dominant, complementarity as genuine safety valve). All three share the Rome Statute jurisdictional text as their kernel and must remain linked via affects_constraints; none should be treated as a fourth 'average' reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
