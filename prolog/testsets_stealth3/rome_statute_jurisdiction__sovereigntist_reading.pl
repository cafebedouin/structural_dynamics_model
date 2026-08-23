% ============================================================================
% CONSTRAINT STORY: rome_statute_jurisdiction__sovereigntist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: rome_statute_jurisdiction__sovereigntist_reading
 *   human_readable: Rome Statute Consent-Conditioned Jurisdiction (Sovereigntist Reading)
 *   domain: international_law/treaty_interpretation/institutional_authority
 *
 * SUMMARY:
 *   This story instantiates the sovereigntist reading of the Rome Statute's
 *   jurisdiction kernel: the International Criminal Court's reach over a
 *   state's territory or nationals is strictly conditioned on that state's
 *   consent (ratification, ad hoc acceptance, or a Security Council Chapter
 *   VII referral), so non-party nationals are immune absent referral,
 *   national courts retain primary authority, and complementarity operates as
 *   deference rather than supervision. The measured referent is the standing
 *   arrangement under contest: the consent-gated jurisdiction regime as it
 *   has actually operated since entry into force, a settlement that made mass
 *   ratification possible, keeps the great-power outsiders shielded, and
 *   concentrates the court's docket and the exposure it imposes on
 *   consenting, mostly weaker states. Epsilon is authored by this reading's
 *   own lights over that fixed referent: the reading holds the consent gate
 *   legitimate as the price of a standing court, so it authors moderate
 *   extractiveness located in the operated margins (the two-tier exposure
 *   asymmetry, the Council's referral monopoly over non-party situations, the
 *   bilateral non-surrender network), not the near-zero value of a
 *   pure-defense story or the high value the universalist sibling will author
 *   over the same referent. Family note: this is one of three readings of
 *   kernel rome_statute_jurisdiction; the universalist and hybrid siblings
 *   are separate constraint stories with their own epsilon over the same
 *   referent, linked through network.affects_constraints.
 *
 * KEY AGENTS:
 *   - non_party_great_powers: primary beneficiary (institutional/arbitrage) — shielded officials, obligations avoided, active defense of the gate
 *   - unsc_permanent_members: agenda-setter and beneficiary (institutional/arbitrage) — hold referral and deferral gates; three of five are non-parties
 *   - assembly_of_states_parties: agenda-setter (institutional/constrained) — administers the settlement; amendment authority itself consent-gated
 *   - consenting_state_parties: beneficiary (organized/constrained) — gain the backstop, bear exposure and cooperation duties
 *   - national_judiciaries: beneficiary (institutional/constrained) — retain primacy under complementarity deference
 *   - icc_prosecutor: payer and gate-operator (institutional/constrained) — mandate bounded by the gate, absorbs the pressure
 *   - officials_of_consenting_weak_states: primary target (powerless/trapped) — carry the exposure their non-party counterparts escape
 *   - victims_in_nonconsenting_territories: primary target (powerless/trapped) — forum closed where no consent and no referral
 *   - universalist_advocacy_coalition: excluded voice (organized/constrained) — no operative seat in the gate
 *   - public_international_law_academy: analytical observer (analytical/analytical) — maps the interpretive contest
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rome_statute_jurisdiction__sovereigntist_reading, 0.46).
domain_priors:suppression_score(rome_statute_jurisdiction__sovereigntist_reading, 0.48).
domain_priors:theater_ratio(rome_statute_jurisdiction__sovereigntist_reading, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, extractiveness, 0.46).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, accessibility_collapse, 0.36).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rome_statute_jurisdiction__sovereigntist_reading, tangled_rope).
narrative_ontology:human_readable(rome_statute_jurisdiction__sovereigntist_reading, "Rome Statute Consent-Conditioned Jurisdiction (Sovereigntist Reading)").
narrative_ontology:topic_domain(rome_statute_jurisdiction__sovereigntist_reading, "international_law/treaty_interpretation/institutional_authority").

domain_priors:requires_active_enforcement(rome_statute_jurisdiction__sovereigntist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rome_statute_jurisdiction__sovereigntist_reading, 'ba97c3fc-5b2e-4d15-aa05-5aec0554ba0e').
narrative_ontology:cs_kernel_codification('ba97c3fc-5b2e-4d15-aa05-5aec0554ba0e', fixed_text).
narrative_ontology:cs_authority_grounding('ba97c3fc-5b2e-4d15-aa05-5aec0554ba0e', lineage).
narrative_ontology:cs_interpretation_layer_present('ba97c3fc-5b2e-4d15-aa05-5aec0554ba0e').
narrative_ontology:cs_reading_relation('ba97c3fc-5b2e-4d15-aa05-5aec0554ba0e', rome_statute_jurisdiction__universalist_reading, forecloses).
narrative_ontology:cs_reading_relation('ba97c3fc-5b2e-4d15-aa05-5aec0554ba0e', rome_statute_jurisdiction__hybrid_complementarity_reading, coexists_with).
narrative_ontology:cs_axiom('ba97c3fc-5b2e-4d15-aa05-5aec0554ba0e', foundational, sovereign_consent_precondition_of_jurisdiction).
narrative_ontology:cs_axiom_status(sovereign_consent_precondition_of_jurisdiction, holdable).
narrative_ontology:cs_axiom_grounding('ba97c3fc-5b2e-4d15-aa05-5aec0554ba0e', sovereign_consent_precondition_of_jurisdiction, conventional).
narrative_ontology:cs_axiom('ba97c3fc-5b2e-4d15-aa05-5aec0554ba0e', foundational, complementarity_is_deference_not_override).
narrative_ontology:cs_axiom_status(complementarity_is_deference_not_override, holdable).
narrative_ontology:cs_axiom_grounding('ba97c3fc-5b2e-4d15-aa05-5aec0554ba0e', complementarity_is_deference_not_override, conventional).
narrative_ontology:cs_axiom('ba97c3fc-5b2e-4d15-aa05-5aec0554ba0e', secondary, nonparty_national_immunity_absent_referral).
narrative_ontology:cs_axiom_status(nonparty_national_immunity_absent_referral, holdable).
narrative_ontology:cs_axiom_grounding('ba97c3fc-5b2e-4d15-aa05-5aec0554ba0e', nonparty_national_immunity_absent_referral, conventional).
narrative_ontology:cs_reference_frame('ba97c3fc-5b2e-4d15-aa05-5aec0554ba0e', consent_conditioned_jurisdictional_compact).
narrative_ontology:cs_drift_state('ba97c3fc-5b2e-4d15-aa05-5aec0554ba0e', post_palestine_appeals_chamber_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ba97c3fc-5b2e-4d15-aa05-5aec0554ba0e', '').
narrative_ontology:cs_kernel_id(rome_statute_jurisdiction__sovereigntist_reading, rome_statute_jurisdiction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__sovereigntist_reading, non_party_great_powers).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__sovereigntist_reading, consenting_state_parties).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__sovereigntist_reading, national_judiciaries).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__sovereigntist_reading, victims_in_nonconsenting_territories).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__sovereigntist_reading, officials_of_consenting_weak_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__sovereigntist_reading, unsc_permanent_members).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__sovereigntist_reading, icc_prosecutor).
narrative_ontology:constraint_vindicates(rome_statute_jurisdiction__sovereigntist_reading, state_consent_doctrine).
narrative_ontology:constraint_vindicates(rome_statute_jurisdiction__sovereigntist_reading, pacta_tertiis_principle).
narrative_ontology:constraint_vindicates(rome_statute_jurisdiction__sovereigntist_reading, complementarity_primacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The treaty body of the states that ratified the Rome Statute. It administers the settlement: adopts amendments, elects judges and the prosecutor, sets the budget, and issues findings against states that fail to arrest indicted persons. Its members chose their exposure by ratifying; they can change the jurisdiction rules only through amendment procedures that themselves require supermajority consent and do not bind states that do not accept the change, and withdrawal (exercised by Burundi and the Philippines) carries reputational and legal cost.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, assembly_of_states_parties, agenda_setter,
    institutional, generational, constrained, global).

% The five Security Council permanent members hold the referral and deferral gates: a Chapter VII resolution can bring any situation to the court, and a resolution can pause any investigation for a year at a time. Three of the five are not party to the Statute, so they can direct the court at others while their own nationals remain outside its reach; each holds a veto over referrals touching its interests (Russia and China vetoed the Syria referrals; the United States pressured the Afghanistan investigation). They arbitrage between the treaty regime, their national courts, and Council politics.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, unsc_permanent_members, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(rome_statute_jurisdiction__sovereigntist_reading, unsc_permanent_members, beneficiary).

% Large military and economic powers that never ratified the Statute, principally the United States, China, and Russia, with states in their orbit. Their officials and soldiers sit outside the court's reach unless the Security Council refers their situation or they accept jurisdiction. They actively defend that position: a network of bilateral non-surrender agreements signed with scores of weaker states, domestic legislation threatening force against the court (the American Service-Members Protection Act), visa bans and asset freezes on court personnel, and funding pressure. They never entered the regime, and their size lets them arbitrage between it, their own courts, and bilateral leverage.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, non_party_great_powers, beneficiary,
    institutional, generational, arbitrage, global).

% The more than 120 states that ratified. They gain a standing backstop against atrocity impunity, useful against rivals, militias, and coups, and they keep the steering wheel through the Assembly. The price is exposure: their officials, commanders, and heads of state can be prosecuted (several sitting and former heads of state have been indicted), and they carry cooperation and funding duties. Exiting exposure means withdrawing, and withdrawal also removes the backstop.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, consenting_state_parties, beneficiary,
    organized, generational, constrained, global).

% The domestic courts of all states. Under complementarity they keep first claim on atrocity cases: the international court acts only where they are unwilling or unable. The consent architecture reinforces their primacy, because for non-party states' nationals their national courts are the only forum. Some (Germany, the Netherlands, Senegal) exercise universal jurisdiction actively; most do not.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, national_judiciaries, beneficiary,
    institutional, generational, constrained, national).

% The office that investigates and prosecutes before the International Criminal Court. Its docket is bounded by the consent gates: it may proceed only where a territorial or nationality state has consented by ratification or ad hoc declaration, or the Security Council has referred the situation. It has stretched the gates interpretively (deportation across a border into a state party for the Myanmar Rohingya case; a non-party neighbor's declarations for the Ukraine investigation) and it absorbs the criticism, non-cooperation, sanctions, and funding pressure that follow. It cannot exit the treaty it is bounded by.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, icc_prosecutor, payer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(rome_statute_jurisdiction__sovereigntist_reading, icc_prosecutor, agenda_setter).

% Heads of state, ministers, commanders, and soldiers of states that ratified. They carry the prosecution exposure that their counterparts in non-party powers escape: the court's indictments have fallen overwhelmingly on nationals of consenting, mostly weaker states. Their exposure follows from their state's ratification choice rather than from conduct different from protected peers; they cannot renounce nationality or un-serve their commands, and their states' resources for contesting cases are thin.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, officials_of_consenting_weak_states, payer,
    powerless, biographical, trapped, national).

% Survivors and victims' families in situations where no state with a jurisdictional hook has consented and the Security Council has not referred (Syria, referred never, with two vetoes). For them the international forum is closed: recourse runs to national courts that are unwilling or unable, or to nothing. They hold no seat in the Assembly of States Parties because their states never joined, and their advocacy reaches the regime only through intermediaries.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, victims_in_nonconsenting_territories, payer,
    powerless, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(rome_statute_jurisdiction__sovereigntist_reading, victims_in_nonconsenting_territories, excluded).

% Human rights organizations, victims' advocates, and scholars who read the Statute as establishing a mandate that does not depend on each state's consent. They press for broader jurisdiction through litigation positions, Assembly side-events, and scholarship, but hold no operative seat in the consent gate itself; their preferred reading governs nowhere institutionally, and their access runs through the states and organs that hold the seats.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, universalist_advocacy_coalition, excluded,
    organized, generational, constrained, global).

% Scholars and commentators who map the interpretive contest: the negotiating history, the case law's movement, and the positions of the competing readings. They hold no stake in the gate's operation and can observe the full structure, including the parts the participants do not advertise.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, public_international_law_academy, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rome_statute_jurisdiction__sovereigntist_reading, non_party_great_powers).
narrative_ontology:fixing_cost_class(rome_statute_jurisdiction__sovereigntist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the standing-institution problem: a permanent international criminal court is acceptable to states only if its reach over their territory and officials is conditioned on consent (ratification, ad hoc acceptance, or Security Council referral). The consent gate is what made ratification by 120-plus states possible and what keeps national courts' primacy intact: a shared settlement of the sovereignty-versus-accountability trade.
% TRANSFER_FUNCTION: Moves jurisdictional exposure and protection asymmetrically: immunity and flexibility flow to states outside the treaty, especially the large military powers, while prosecution exposure, cooperation duties, and funding burdens concentrate on consenting states; recourse is moved away from victims in territories where no jurisdictional hook exists.
% ABSENT_VOICES: Victims in non-consenting territories: their states never joined, so they hold no seat in the Assembly that administers the gate their recourse runs through. The universalist advocacy coalition holds no operative seat in the jurisdiction decision either; its objection, that consent gating abandons the unprotected, is heard only in scholarship, litigation filings, and side-events, never in the gate's operation.
% DISAPPEARANCE_RATIONALE: If the consent gate vanished overnight and the court asserted jurisdiction over any atrocity situation regardless of consent, the non-party great powers would escalate immediately (defunding, sanctions, expanded non-surrender networks, threats to court personnel), state cooperation would fracture, several parties would likely withdraw, and the court's caseload and legitimacy would reorganize around an open sovereignty confrontation. Which cases are reachable, which officials are exposed, and which victims have a forum is an arrangement many parties actively depend on.
% FOUNDING_PROBLEM: After Nuremberg, international criminal justice ran on ad hoc, victor-built tribunals (ICTY, ICTR) with no standing institution and legitimacy tied to Council politics. A permanent court needed a jurisdictional basis states would actually accept: hence a consent-conditioned framework with a Security Council referral channel, negotiated at Rome in 1998.
% FOUNDING_PROBLEM_CORROBORATION: The ad hoc tribunal record (ICTY, ICTR) predates the treaty's beneficiary coalition and attests the standing-institution gap from outside it; the 1998 negotiating record shows states that never ratified engaging the problem while rejecting the consent-gated answer; victims' organizations in non-consenting territories attest the problem's persistence from the paying side; and the drafting history is public and contestable by anyone. No attestation rests on the beneficiary set alone.
narrative_ontology:disappearance_verdict(rome_statute_jurisdiction__sovereigntist_reading, world_rearranges).
narrative_ontology:founding_problem_status(rome_statute_jurisdiction__sovereigntist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rome_statute_jurisdiction__sovereigntist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(rome_statute_jurisdiction__sovereigntist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rome_statute_jurisdiction__sovereigntist_reading, 0.46, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness 0.46: by this reading's lights the consent gate itself is the legitimate price of a standing, accepted court, so the base compact is coordination rather than extraction. The measured extraction sits in the operated margins the record makes visible: the docket's concentration on consenting weaker states while officials of non-party powers remain unreachable (the Libya-referral-versus-Syria-veto contrast; the Afghanistan investigation deferred under pressure), the Council's monopoly on reaching non-party situations, and the non-surrender agreement network extracted from weaker states. Suppression 0.48 is authored as a raw structural property, unscaled by power or scope: the gate is held not by participant preference alone but by active defense of it (the non-surrender campaign, domestic legislation threatening the court's personnel, visa bans and asset freezes, funding pressure), coercion aimed at keeping the universalist alternative from operating. Theater 0.34: the gate function is real, but a growing share of maintenance is performative (complementarity deference to nominally willing national proceedings; pause-and-resume investigation management that maintains the boundary theatrically under pressure). Accessibility collapse 0.36: alternatives to the consent-gated regime remain substantially available (national universal jurisdiction, ad hoc and hybrid tribunals, out-of-treaty evidence mechanisms, the sibling readings as interpretive routes), so the gate forecloses far less than a natural limit would. Resistance 0.42: universalist advocacy, victims' filings, and interpretive expansion from inside the court press against the gate continuously. The claimed type (tangled_rope) is authored from the sovereigntist seat as what is structurally true, and the metrics are authored independently as what descriptively holds; where the engine's per-seat computations diverge from the claim, that divergence is the datum. The temporal series run on one shared grid (T0 to T24, four-year steps) with every tracked metric authored at every point; the trajectories show extraction accumulating and enforcement hardening over the interval, with theater rising as maintenance grew more performative.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute different types from the same structure. From the non_party_great_powers seat the arrangement is a protective settlement those states shaped, stayed out of, and actively defend: effective extraction near the beneficiary end, arbitrage-grade exit. From the victims_in_nonconsenting_territories seat the same gate is a closed forum: high effective extraction with no exit and no seat. From the consenting_state_parties seat it is a bargain (real backstop, real exposure, roughly balanced), and from the icc_prosecutor seat it is a binding limit the office works around interpretively. Same-level dynamics: consenting_state_parties and non_party_great_powers hold the same nominal standing (sovereign states) but the ratification choice plus material power differentiate their exits; one seat can withdraw at reputational cost, the other never entered and arbitrages between regimes. Inter-institutional dynamics: the Council holds gates the Assembly cannot amend around, the court interprets text the non-parties reject, and national judiciaries hold primacy the treaty guarantees them, so each institution experiences the same jurisdictional settlement from a different seat. The paying seats cannot readily coalition: victims outside the treaty have no seat in it, and exposed officials are fragmented across jurisdictions with thin resources, which is part of what holds resistance at its authored level.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations: non_party_great_powers (immunity without obligation; derivation should land near the beneficiary end, reinforced by arbitrage exit), consenting_state_parties (coordination benefits with real offsetting exposure, nearer symmetric than a pure collector), national_judiciaries (primacy preserved by the gate and by complementarity deference). Victim declarations: victims_in_nonconsenting_territories (forum closed, trapped, near the full-target end) and officials_of_consenting_weak_states (exposure without the shield, trapped). The icc_prosecutor is genuinely dual-positioned: it operates the gate in each situation (agenda-setter function) while bearing the gate's narrowing of its mandate (payer function), declared payer with secondary agenda_setter. The gains from the two-tier asymmetry demonstrably accrue to the non-party great powers (immunity for personnel, obligations avoided, non-surrender agreements extracted from weaker states), so gain_flow names that seat rather than diffuse. Scope: the regime is global, and verifying consent, willingness, and genuineness at global scope is hard, which scales effective extraction upward for the target seats; suppression stays unscaled.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification keeps two mislabelings apart. Calling the arrangement pure coordination would launder the operated two-tier asymmetry: the extraction is a distributive outcome the structure produces and its principal beneficiaries actively defend, not a neutral coordination cost. Calling it pure extraction (the universalist sibling's temptation) would erase the genuine coordination function: without the consent gate there is no ratification coalition, no national-court primacy, and no court whose orders anyone executes. Tangled_rope holds both: coordination and asymmetric extraction through one structure, requiring active enforcement. Mandatrophy: the founding problem (a standing, accepted forum for atrocity accountability after the ad hoc tribunal era) is live, the arrangement's function has not atrophied, and there is no sunset; the R5 mismatch check (status live times verdict world_rearranges) raises no capture flag. Fixing is prohibitive for the only actor positioned to fix it: the Assembly's amendment path is itself consent-gated and does not bind non-accepting states, and moving past consent would trigger the defunding, sanctions, and withdrawals that threaten the court's existence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rome_jurisdiction_kernel_reading_contest,
    'This constraint is the sovereigntist_reading of kernel rome_statute_jurisdiction. Is strict sovereign consent the Statute''s operative jurisdictional foundation (jurisdiction limited to consenting states, non-party nationals immune absent Council referral, national courts primary, complementarity as deference), or does the same fixed text instead establish a mandate transcending consent (universalist_reading) or a complementarity-balanced middle (hybrid_complementarity_reading)?',
    'Convergence test across judicial practice (appeals-level doctrine on territorial consent), state reaction (non-surrender networks, withdrawals, Assembly amendment activity), and the negotiating record as tie-breaker: whichever reading''s structural claims the operated regime converges on becomes the operative constraint.',
    'If the universalist sibling prevails, the non-party immunity rule dissolves, the denied-recourse victim set shrinks, and this constraint''s extractiveness collapses toward the court''s own conduct; if the hybrid sibling prevails, the gate persists but complementarity''s supervisory character changes who bears admissibility risk; if this reading holds, the authored structure stands as measured.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(rome_jurisdiction_kernel_reading_contest, conceptual, 'Which reading of the Rome Statute jurisdiction kernel is operative; the disagreement is located in the consent gate''s reach and complementarity''s character.').

omega_variable(
    territorial_consent_sufficiency_faultline,
    'Does territorial-state consent alone open the court''s jurisdiction over a non-party state''s nationals (the position the Appeals Chamber took in the Palestine jurisdiction decision), or is nationality-state consent or Security Council referral required, as this reading''s core operational claim holds?',
    'Subsequent chamber decisions, state reactions (non-cooperation findings, expansion of non-surrender networks), and possible Assembly or amendment responses to the territorial-consent doctrine.',
    'If territorial consent suffices, the non-party-national immunity rule fails wherever the territorial state consents, the reading''s operative shield narrows to nationality-state consent plus referral, and the two-tier asymmetry partially closes; if nationality consent is required, the gate holds as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(territorial_consent_sufficiency_faultline, conceptual, 'The live doctrinal fault line on whether territorial-state consent reaches non-party nationals.').

omega_variable(
    complementarity_deference_depth,
    'Is complementarity in operation the deference this reading claims (national courts primary, the international court a backstop of last resort), or a supervisory override in which the international court second-guesses the genuineness of national proceedings?',
    'Admissibility rulings across a decade of cases: how often national proceedings are found genuine, how often unwillingness or inability is found, and what standard of review the chambers apply to national systems.',
    'If operation matches deference, the reading''s description of the arrangement is accurate and national primacy is real; if operation is supervisory, the reading misdescribes the arrangement and the hybrid sibling''s balance claim gains ground.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(complementarity_deference_depth, empirical, 'Whether operated complementarity is deference or supervision.').

omega_variable(
    consent_gate_asymmetry_origin,
    'Is the operated two-tier exposure (officials of consenting weak states indicted while officials of non-party great powers are unreachable) inherent to consent-conditioned design, or an artifact of which states happened to ratify and of Security Council composition?',
    'Comparative counterfactuals: membership expansion waves (Asian and Latin American accessions), referral patterns under different Council compositions, and whether exposure tracks ratification or state power once membership broadens.',
    'If inherent, the consent principle itself generates the asymmetric exposure and no membership pattern removes it; if artifact, broader membership or Council reform would dissolve the asymmetry without touching the consent principle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_gate_asymmetry_origin, empirical, 'Whether the two-tier exposure is built into consent design or contingent on membership history.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rome_statute_jurisdiction__sovereigntist_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rome_tr_t0, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(rome_tr_t0, observed).
narrative_ontology:measurement(rome_tr_t4, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 4, 0.24).
narrative_ontology:measurement_basis(rome_tr_t4, observed).
narrative_ontology:measurement(rome_tr_t8, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 8, 0.27).
narrative_ontology:measurement_basis(rome_tr_t8, observed).
narrative_ontology:measurement(rome_tr_t12, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 12, 0.29).
narrative_ontology:measurement_basis(rome_tr_t12, observed).
narrative_ontology:measurement(rome_tr_t16, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 16, 0.31).
narrative_ontology:measurement_basis(rome_tr_t16, observed).
narrative_ontology:measurement(rome_tr_t20, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 20, 0.33).
narrative_ontology:measurement_basis(rome_tr_t20, observed).
narrative_ontology:measurement(rome_tr_t24, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 24, 0.34).
narrative_ontology:measurement_basis(rome_tr_t24, observed).

% Extraction over time
narrative_ontology:measurement(rome_be_t0, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 0, 0.36).
narrative_ontology:measurement_basis(rome_be_t0, observed).
narrative_ontology:measurement(rome_be_t4, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 4, 0.4).
narrative_ontology:measurement_basis(rome_be_t4, observed).
narrative_ontology:measurement(rome_be_t8, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 8, 0.43).
narrative_ontology:measurement_basis(rome_be_t8, observed).
narrative_ontology:measurement(rome_be_t12, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 12, 0.44).
narrative_ontology:measurement_basis(rome_be_t12, observed).
narrative_ontology:measurement(rome_be_t16, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 16, 0.45).
narrative_ontology:measurement_basis(rome_be_t16, observed).
narrative_ontology:measurement(rome_be_t20, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 20, 0.46).
narrative_ontology:measurement_basis(rome_be_t20, observed).
narrative_ontology:measurement(rome_be_t24, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 24, 0.46).
narrative_ontology:measurement_basis(rome_be_t24, observed).

% Suppression requirement over time
narrative_ontology:measurement(rome_su_t0, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(rome_su_t0, observed).
narrative_ontology:measurement(rome_su_t4, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 4, 0.38).
narrative_ontology:measurement_basis(rome_su_t4, observed).
narrative_ontology:measurement(rome_su_t8, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 8, 0.42).
narrative_ontology:measurement_basis(rome_su_t8, observed).
narrative_ontology:measurement(rome_su_t12, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 12, 0.44).
narrative_ontology:measurement_basis(rome_su_t12, observed).
narrative_ontology:measurement(rome_su_t16, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 16, 0.45).
narrative_ontology:measurement_basis(rome_su_t16, observed).
narrative_ontology:measurement(rome_su_t20, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 20, 0.47).
narrative_ontology:measurement_basis(rome_su_t20, observed).
narrative_ontology:measurement(rome_su_t24, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 24, 0.48).
narrative_ontology:measurement_basis(rome_su_t24, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rome_statute_jurisdiction__sovereigntist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__sovereigntist_reading, rome_statute_jurisdiction__universalist_reading).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__sovereigntist_reading, rome_statute_jurisdiction__hybrid_complementarity_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'ICC jurisdiction' decomposes into three structurally distinct constraints with different epsilon over the same referent (the Statute's jurisdiction regime as operated): this sovereigntist story authors moderate epsilon (the gate as legitimate coordination carrying operated-margin extraction), the universalist sibling authors high epsilon (the gate as abandonment of the unprotected), and the hybrid sibling authors intermediate epsilon (complementarity as the balancing mechanism). The sovereigntist reading is the one the non-party powers' practice enforces, so it structurally conditions the operating environment of both siblings; the affects edges run from this story to both. Same referent, reading-indexed epsilon, separate stories per the epsilon-invariance rule.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
