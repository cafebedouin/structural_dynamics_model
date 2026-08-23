% ============================================================================
% CONSTRAINT STORY: nsl_legal_text__jurisdictional_capture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nsl_legal_text__jurisdictional_capture_reading, []).

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
 *   constraint_id: nsl_legal_text__jurisdictional_capture_reading
 *   human_readable: National Security Law — Jurisdictional Capture Reading (Mainland Legal System Transplantation)
 *   domain: constitutional/political/international
 *
 * SUMMARY:
 *   On 30 June 2020 the NPC Standing Committee promulgated the National
 *   Security Law directly into the Basic Law as Annex III, bypassing Hong
 *   Kong's legislature, and the arrangement began operating within hours.
 *   This story instantiates the jurisdictional_capture_reading of the
 *   contested kernel nsl_legal_text: it treats the law's operative
 *   significance as the transplantation of mainland legal-system control into
 *   Hong Kong — a Beijing-drafted text amendable and interpretable only in
 *   Beijing, a mainland security organ stationed locally beyond local
 *   jurisdiction, a Chief Executive-controlled designated-judge list,
 *   case-handover provisions opening a path to mainland trial, and procedural
 *   forms (closed sessions, jury-less panels, restricted bail) imported from
 *   mainland practice — progressively eroding the common law autonomy that
 *   distinguished Hong Kong's legal order. The referent for epsilon is the
 *   standing NSL arrangement itself, assessed by this reading's lights; the
 *   sibling readings (sovereignty_restoration, democratic_enclosure) are
 *   separate constraint stories with their own epsilon values, linked through
 *   network.affects_constraints. Claim and metrics are authored
 *   independently: claimed_type tangled_rope reflects this reading's
 *   structural assessment that a genuine coordination function (filling the
 *   vacant Article 23 security frame) and asymmetric institutional capture
 *   run through the same enforced structure; the metric values describe the
 *   arrangement's actual operation as this reading observes it.
 *
 * KEY AGENTS:
 *   - npcsc_central_authorities: Primary agenda-setter (institutional/arbitrage) — drafts, promulgates, interprets, and solely controls the text; unreachable by any Hong Kong process
 *   - mainland_security_apparatus: Primary beneficiary (institutional/arbitrage) — gains jurisdictional immunity, case intake, and a permanent local foothold
 *   - hk_judiciary: Primary target (institutional/constrained) — designated-list vetting, overridable rulings, resignations
 *   - hk_independent_legal_profession: Secondary target (organized/constrained) — counsel prosecutions, professional marginalization, emigration
 *   - nsl_case_defendants: Direct payers (powerless/trapped) — transplanted procedure, mainland-referral exposure
 *   - hk_government_executive: Dual-positioned implementer (institutional/constrained) — administers locally, gains vetting powers, bears sanctions
 *   - hk_pro_establishment_elites: Secondary beneficiaries (powerful/arbitrage) — staffing, legitimation, protected position
 *   - foreign_common_law_counterparties: Excluded seat (powerful/mobile) — relied on the two-systems legal firewall, re-pricing the venue
 *   - overseas_rule_of_law_monitors: Analytical observers (analytical/analytical) — document, sanction, and assess from outside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nsl_legal_text__jurisdictional_capture_reading, 0.71).
domain_priors:suppression_score(nsl_legal_text__jurisdictional_capture_reading, 0.78).
domain_priors:theater_ratio(nsl_legal_text__jurisdictional_capture_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nsl_legal_text__jurisdictional_capture_reading, tangled_rope).
narrative_ontology:human_readable(nsl_legal_text__jurisdictional_capture_reading, "National Security Law — Jurisdictional Capture Reading (Mainland Legal System Transplantation)").
narrative_ontology:topic_domain(nsl_legal_text__jurisdictional_capture_reading, "constitutional/political/international").

domain_priors:requires_active_enforcement(nsl_legal_text__jurisdictional_capture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nsl_legal_text__jurisdictional_capture_reading, 'b34f1a50-ad67-40b2-b082-1ca068394445').
narrative_ontology:cs_kernel_codification('b34f1a50-ad67-40b2-b082-1ca068394445', fixed_text).
narrative_ontology:cs_authority_grounding('b34f1a50-ad67-40b2-b082-1ca068394445', extraction).
narrative_ontology:cs_interpretation_layer_present('b34f1a50-ad67-40b2-b082-1ca068394445').
narrative_ontology:cs_reading_relation('b34f1a50-ad67-40b2-b082-1ca068394445', nsl_legal_text__sovereignty_restoration_reading, forecloses).
narrative_ontology:cs_reading_relation('b34f1a50-ad67-40b2-b082-1ca068394445', nsl_legal_text__democratic_enclosure_reading, coexists_with).
narrative_ontology:cs_axiom('b34f1a50-ad67-40b2-b082-1ca068394445', foundational, nsl_transplants_mainland_legal_control).
narrative_ontology:cs_axiom_status(nsl_transplants_mainland_legal_control, holdable).
narrative_ontology:cs_axiom_grounding('b34f1a50-ad67-40b2-b082-1ca068394445', nsl_transplants_mainland_legal_control, empirically_contingent).
narrative_ontology:cs_axiom('b34f1a50-ad67-40b2-b082-1ca068394445', foundational, basic_law_autonomy_guarantee_binding).
narrative_ontology:cs_axiom_status(basic_law_autonomy_guarantee_binding, holdable).
narrative_ontology:cs_axiom_grounding('b34f1a50-ad67-40b2-b082-1ca068394445', basic_law_autonomy_guarantee_binding, conventional).
narrative_ontology:cs_reference_frame('b34f1a50-ad67-40b2-b082-1ca068394445', joint_declaration_autonomy_settlement).
narrative_ontology:cs_drift_state('b34f1a50-ad67-40b2-b082-1ca068394445', post_nsl_implementation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b34f1a50-ad67-40b2-b082-1ca068394445', '').
narrative_ontology:cs_kernel_id(nsl_legal_text__jurisdictional_capture_reading, nsl_legal_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nsl_legal_text__jurisdictional_capture_reading, mainland_security_apparatus).
narrative_ontology:constraint_beneficiary(nsl_legal_text__jurisdictional_capture_reading, npcsc_central_authorities).
narrative_ontology:constraint_beneficiary(nsl_legal_text__jurisdictional_capture_reading, hk_pro_establishment_elites).
narrative_ontology:constraint_victim(nsl_legal_text__jurisdictional_capture_reading, hk_judiciary).
narrative_ontology:constraint_victim(nsl_legal_text__jurisdictional_capture_reading, hk_independent_legal_profession).
narrative_ontology:constraint_victim(nsl_legal_text__jurisdictional_capture_reading, nsl_case_defendants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(nsl_legal_text__jurisdictional_capture_reading, hk_government_executive).
narrative_ontology:constraint_vindicates(nsl_legal_text__jurisdictional_capture_reading, central_comprehensive_jurisdiction_doctrine).
narrative_ontology:constraint_vindicates(nsl_legal_text__jurisdictional_capture_reading, executive_led_governance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafted the National Security Law and inserted it into the Basic Law as Annex III by unilateral promulgation in June 2020, without referral to Hong Kong's legislature. Holds exclusive power to interpret the law — interpretations issued in Beijing bind Hong Kong courts — and is the only body that can amend it. Supervises implementation through central liaison organs. No process inside Hong Kong can revise the text or displace its interpretive rulings.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, npcsc_central_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% The mainland police and state-security services, including the office stationed in Hong Kong since mid-2020. Its personnel and premises in Hong Kong sit outside local jurisdiction and prosecutorial reach; it receives cases referred under the law's handover provision for processing under mainland procedure; it runs intelligence and enforcement coordination across the boundary. What accrues to it: operational immunity from the local legal system, a case-intake channel, and a permanent institutional foothold inside a jurisdiction previously beyond its reach. It answers to Beijing alone and can be expanded or redirected at will.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, mainland_security_apparatus, beneficiary,
    institutional, generational, arbitrage, national).

% Chairs the committee overseeing national security work in Hong Kong, compiles the designated-judge list for national security cases, and directs prosecution priorities through the dedicated police unit. Gains appointment and vetting powers it did not previously hold. Bears the costs of implementation: international sanctions on officials, diplomatic isolation, and the loss of policy discretion wherever central directives override local judgment. Leaving means resignation and political retirement; staying is rewarded with standing in Beijing.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, hk_government_executive, agenda_setter,
    institutional, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(nsl_legal_text__jurisdictional_capture_reading, hk_government_executive, beneficiary).

% Adjudicates national security cases through a designated-judge list compiled by the Chief Executive; judges who decline designation or rule against government preferences face removal from the list, public denunciation, or pressure to resign, and several sitting judges have resigned rather than continue. The institution's case mix, procedural norms, and appointment pipeline are progressively aligned with central expectations, and its rulings are overridable by Beijing's interpretations. Exit for the institution means ceding the caseload entirely; for individual judges it means resignation and career termination.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, hk_judiciary, payer,
    institutional, generational, constrained, regional).

% Barristers and solicitors who handle sensitive cases face prosecution risk themselves — several counsel have been arrested — while the Bar Association's leadership has been pressured, its statements denounced, and its members urged toward patriotic re-alignment. Senior practitioners have emigrated in growing numbers, thinning the bench of experienced advocates. Exit exists through relocation abroad, but at the cost of abandoning local practice and standing.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, hk_independent_legal_profession, payer,
    organized, biographical, constrained, regional).

% People prosecuted under the law's four offense categories: they face long sentences, closed proceedings, restricted bail, jury-less tribunals, and the possibility of referral to mainland jurisdiction under the handover provision, where procedure and detention conditions lie wholly outside Hong Kong oversight. Their families and chosen counsel have limited access; appeal paths run through the same institutions undergoing vetting and re-alignment. Exit is not available to them.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, nsl_case_defendants, payer,
    powerless, biographical, trapped, regional).

% Legislators, advisors, and community figures aligned with Beijing who staff the new committees, endorse the designated-judge arrangements, and supply the local legitimation the arrangement requires. They gain appointments, influence, and protection from the political competition the surrounding environment suppresses. Their position improves the more completely the arrangement consolidates; stepping away would mean forfeiting that standing.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, hk_pro_establishment_elites, beneficiary,
    powerful, biographical, arbitrage, regional).

% International businesses, arbitration users, and treaty partners who relied on Hong Kong's legal system remaining distinct from the mainland's. They were never consulted on the law and have no voice in its operation. As judicial independence erodes they re-price Hong Kong as a venue: some relocate deals and disputes to Singapore and London, others accept the added risk. Exit is available to them and exercised selectively.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, foreign_common_law_counterparties, excluded,
    powerful, biographical, mobile, global).

% Foreign governments, bar associations, UN treaty bodies, and academic constitutional scholars who track the law's operation, publish assessments, impose targeted sanctions, and document case-level practice. They see the full structure from outside and hold no stake in its continuation; their leverage is reputational and diplomatic rather than juridical.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, overseas_rule_of_law_monitors, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nsl_legal_text__jurisdictional_capture_reading, mainland_security_apparatus).
narrative_ontology:fixing_cost_class(nsl_legal_text__jurisdictional_capture_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Fills a security-governance gap that persisted since 1997: Hong Kong's own national security legislation (Basic Law Article 23) was never enacted after the 2003 bill was withdrawn, leaving cross-boundary security offenses without a dedicated legal frame. The law defines four offense categories, creates dedicated enforcement institutions, and connects Hong Kong enforcement to mainland security organs through a single framework.
% TRANSFER_FUNCTION: Moves adjudicative and procedural authority from Hong Kong's common law institutions to central control: the handover provision moves detained persons toward mainland jurisdiction; the designated-judge mechanism moves judicial-selection power to the Chief Executive; interpretation supremacy moves legal meaning-fixing to the NPC Standing Committee; professional risk moves onto judges and lawyers who handle sensitive cases.
% ABSENT_VOICES: Hong Kong's pan-democratic legal caucus, the Bar Association's critical leadership, affected defendants and their families, and the international legal community that relied on the two-systems legal firewall — none participated. The text was drafted in Beijing and promulgated without local legislative deliberation or public consultation on the final draft.
% DISAPPEARANCE_RATIONALE: If the law and its annexed institutions vanished overnight, the mainland security office would lose its jurisdictional footing, designated-judge vetting would lapse, referred cases would revert to ordinary Hong Kong procedure, and the practical boundary between the two legal systems would begin reconstituting. The arrangement is load-bearing for the current institutional configuration on both sides of the boundary.
% FOUNDING_PROBLEM: The 2019 unrest and the absence of any local national security statute left, in the central authorities' account, a sovereign-security gap: secessionist mobilization, foreign collusion, and prolonged civil disorder proceeded without a legal instrument the center controlled.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties: the Hong Kong Bar Association's public statements, foreign-government assessments, UN treaty-body reviews, and academic constitutional scholarship attest that large-scale disorder ended by early 2021 while the institutional apparatus continued expanding into ordinary legal administration — supporting the reading that the founding urgency passed and the arrangement's function shifted. The central authorities and the Hong Kong government attest the opposite (a persistent threat justifying permanence); no corroboration for that account exists outside the beneficiary set.
narrative_ontology:disappearance_verdict(nsl_legal_text__jurisdictional_capture_reading, world_rearranges).
narrative_ontology:founding_problem_status(nsl_legal_text__jurisdictional_capture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nsl_legal_text__jurisdictional_capture_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(nsl_legal_text__jurisdictional_capture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nsl_legal_text__jurisdictional_capture_reading, 0.71, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nsl_legal_text__jurisdictional_capture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nsl_legal_text__jurisdictional_capture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nsl_legal_text__jurisdictional_capture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon 0.71 (moderate-high, matching the reading's expected structural delta): the arrangement's principal yield is adjudicative and institutional control itself — the captured asset is the autonomy of a legal system — and that yield is decoupled from the delivery of the security service that nominally justifies the arrangement. Suppression 0.78: persistence depends on continuously operated machinery (designated-list vetting, referral provisions, prosecution of counsel, closure of amendment routes) rather than on voluntary compliance; suppression is authored as a raw structural property and is not scaled by power or scope — only extractiveness is scaled, by directionality and scope, in the engine's computation. Theater 0.22: the operations are substantively functional (real cases, real custody, real institutional change); performative activity (loyalty ceremonies, patriotic education campaigns surrounding the law) is a minor though growing share. Accessibility_collapse 0.60: inside the jurisdiction the common law procedural alternatives are largely foreclosed, but alternatives persist externally (competing arbitration venues, foreign courts), so collapse is partial rather than near-total. Resistance 0.60: sustained professional and diplomatic resistance — bar association statements, judicial resignations, emigration, foreign sanctions and treaty responses — raises the arrangement's maintenance cost without halting the transplantation. All three tracked series share one time grid (2020-2025, annual) so the engine samples every metric at every authored point; the series show extraction accumulating, enforcement hardening to a plateau, and theater creeping upward, with end-state values matching the base_properties scalars.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same text. From the central-authority and security-apparatus seats the arrangement presents as a functioning security-governance framework they built and exclusively control — coordination at negligible personal cost. From the judiciary and legal-profession seats the identical structure operates as progressive dispossession of professional autonomy under vetting and prosecution risk. Defendants experience the transplanted procedure directly, with no exit at all. The excluded foreign counterparties register venue and reputational risk rather than either coordination benefit or direct targeting. The engine derives these per-seat classifications from the declared roles, power atoms, and exit options; the divergence between the agenda-setter's computed type and the payers' computed type is the perspectival fact this story exists to record.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (mainland_security_apparatus, npcsc_central_authorities, hk_pro_establishment_elites) drive those seats toward the beneficiary end of directionality, reinforced by their arbitrage-grade exit. Victim declarations (hk_judiciary, hk_independent_legal_profession, nsl_case_defendants) drive those seats toward the target end; the judiciary's institutional entrapment and the defendants' total lack of exit push them nearest the full-target pole, while the profession's partial mobility moderates its position slightly. The executive is dual-positioned — it administers the arrangement and gains vetting powers (pulling toward beneficiary) while bearing sanctions and lost discretion (pushing toward target) — netting out low-to-moderate. Foreign counterparties, excluded rather than enrolled, sit near symmetric with a mild target tilt. No directionality overrides are authored: the beneficiary/victim declarations plus exit atoms already differentiate every seat correctly, and the derivation chain needs no correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — the 2019 breakdown and the 23-year vacancy of local national security legislation — was real even by this reading's lights, which is exactly why the arrangement cannot be dismissed as pure extraction: a coordination function existed to fill. But the acute phase of that problem closed by early 2021, while the institutional apparatus expanded into ordinary legal administration afterward; the mandate has outlived its founding urgency and the arrangement now grows on institutional momentum. Classifying the arrangement as a rope would erase the asymmetric capture running through it; classifying it as a snare would erase the genuine coordination function that gave it founding legitimacy and still performs real security work. The tangled_rope claim holds both truths in one structure. The R5 interview records the obsolescence signal: founding_problem_status is contested (the beneficiary seats attest persistent threat; every corroborating source outside the beneficiary set attests the founding urgency passed), paired with a world_rearranges disappearance verdict — the arrangement is load-bearing for the current configuration regardless of whether its founding problem remains live, which is the signature of a mandate persisting past its function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is the jurisdictional_capture_reading of kernel nsl_legal_text — what structurally changes if the kernel is read instead as sovereignty_restoration_reading or democratic_enclosure_reading?',
    'Classify the sibling reading-stories (nsl_legal_text__sovereignty_restoration_reading, nsl_legal_text__democratic_enclosure_reading) and compare seat structures: victim sets, beneficiary sets, and epsilon values across the family.',
    'Under the sovereignty reading the judiciary exits the victim set and the mainland apparatus becomes a coordination participant rather than a beneficiary; under the enclosure reading civil-society and opposition actors enter the victim set and suppression dominates extraction as the measured dimension.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: this story is one of three rival readings of the NSL text; the other readings are separate constraints, not parameters of this one.').

omega_variable(
    reading_disagreement_location,
    'Where exactly do the three readings disagree — at which structural element of the kernel?',
    'Locate the disputed element: whether the Basic Law autonomy boundary binds central security authority (the capture-versus-sovereignty axis) versus whether security enforcement necessarily criminalizes dissent (the enclosure axis).',
    'If the dispute sits at the boundary-binding premise, capture and sovereignty are mutually exclusive within any single framework (the foreclosure structure declared in reading_relations); if it sits at enforcement practice, the readings could in principle be reconciled descriptively and the foreclosure edge would be mis-declared.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_disagreement_location, conceptual, 'The readings divide on the constitutional status of the autonomy boundary, not on the law''s existence or the institutions it created.').

omega_variable(
    reserved_domain_vs_generalized_capture,
    'Does the NSL''s transplant operation remain confined to the statutorily reserved national security domain, or does it generalize into ordinary legal-institutional life?',
    'Longitudinal comparison of NSL-case and non-NSL-case judicial procedure, appointment and vetting data, and interpretation practice across 2020-2025.',
    'Confined: the capture concentrates in a bounded domain and the constraint leans rope-side within tangled_rope; generalized: capture pervades the legal system and the constraint leans snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reserved_domain_vs_generalized_capture, empirical, 'Scope of institutional transplantation beyond the statutory security domain.').

omega_variable(
    suppression_ratchet_trajectory,
    'Will the enforcement-suppression requirement continue ratcheting upward, plateau at normalization, or decay as resistant professionals emigrate?',
    'Track arrest and prosecution rates, designated-list expansion, and procedural hardening from 2025 forward.',
    'Continued ratchet supports drift toward snare; a stable plateau supports a stabilized tangled_rope; decay with the structure retained suggests eventual piton drift.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_ratchet_trajectory, empirical, 'Future trajectory of enforcement intensification beyond the observed plateau.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nsl_legal_text__jurisdictional_capture_reading, 2020, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nsl_capture_tr_t2020, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 2020, 0.12).
narrative_ontology:measurement_basis(nsl_capture_tr_t2020, observed).
narrative_ontology:measurement(nsl_capture_tr_t2021, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 2021, 0.14).
narrative_ontology:measurement_basis(nsl_capture_tr_t2021, observed).
narrative_ontology:measurement(nsl_capture_tr_t2022, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 2022, 0.17).
narrative_ontology:measurement_basis(nsl_capture_tr_t2022, observed).
narrative_ontology:measurement(nsl_capture_tr_t2023, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 2023, 0.19).
narrative_ontology:measurement_basis(nsl_capture_tr_t2023, observed).
narrative_ontology:measurement(nsl_capture_tr_t2024, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 2024, 0.21).
narrative_ontology:measurement_basis(nsl_capture_tr_t2024, observed).
narrative_ontology:measurement(nsl_capture_tr_t2025, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 2025, 0.22).
narrative_ontology:measurement_basis(nsl_capture_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(nsl_capture_be_t2020, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 2020, 0.55).
narrative_ontology:measurement_basis(nsl_capture_be_t2020, observed).
narrative_ontology:measurement(nsl_capture_be_t2021, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 2021, 0.61).
narrative_ontology:measurement_basis(nsl_capture_be_t2021, observed).
narrative_ontology:measurement(nsl_capture_be_t2022, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 2022, 0.65).
narrative_ontology:measurement_basis(nsl_capture_be_t2022, observed).
narrative_ontology:measurement(nsl_capture_be_t2023, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 2023, 0.68).
narrative_ontology:measurement_basis(nsl_capture_be_t2023, observed).
narrative_ontology:measurement(nsl_capture_be_t2024, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 2024, 0.7).
narrative_ontology:measurement_basis(nsl_capture_be_t2024, observed).
narrative_ontology:measurement(nsl_capture_be_t2025, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 2025, 0.71).
narrative_ontology:measurement_basis(nsl_capture_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(nsl_capture_su_t2020, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 2020, 0.62).
narrative_ontology:measurement_basis(nsl_capture_su_t2020, observed).
narrative_ontology:measurement(nsl_capture_su_t2021, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 2021, 0.69).
narrative_ontology:measurement_basis(nsl_capture_su_t2021, observed).
narrative_ontology:measurement(nsl_capture_su_t2022, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 2022, 0.74).
narrative_ontology:measurement_basis(nsl_capture_su_t2022, observed).
narrative_ontology:measurement(nsl_capture_su_t2023, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 2023, 0.77).
narrative_ontology:measurement_basis(nsl_capture_su_t2023, observed).
narrative_ontology:measurement(nsl_capture_su_t2024, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 2024, 0.78).
narrative_ontology:measurement_basis(nsl_capture_su_t2024, observed).
narrative_ontology:measurement(nsl_capture_su_t2025, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 2025, 0.78).
narrative_ontology:measurement_basis(nsl_capture_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nsl_legal_text__jurisdictional_capture_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(nsl_legal_text__jurisdictional_capture_reading, nsl_legal_text__sovereignty_restoration_reading).
narrative_ontology:affects_constraint(nsl_legal_text__jurisdictional_capture_reading, nsl_legal_text__democratic_enclosure_reading).

% DUAL FORMULATION NOTE:
% 'The National Security Law' is a colloquial label covering at least three structurally distinct claims, decomposed per the epsilon-invariance principle into a three-story constraint family: this story (jurisdictional_capture_reading — transplantation of mainland legal control; epsilon moderate-high; judiciary and legal profession in the victim set, mainland security apparatus in the beneficiary set), nsl_legal_text__sovereignty_restoration_reading (legitimate sovereign security instrument; coordination-forward from the state's seat), and nsl_legal_text__democratic_enclosure_reading (permanent closure of democratic space; suppression-forward; civil-society and opposition actors in the victim set). The sovereignty reading is the upstream legitimacy account — its premises are cited as the public justification for the arrangement this reading measures as capture — and the enclosure reading shares this reading's critical camp while locating the harm in a different mechanism (speech and association rather than adjudicative structure). Each story carries its own epsilon, beneficiaries, victims, and claimed type; the family edges propagate contamination analysis across the readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
