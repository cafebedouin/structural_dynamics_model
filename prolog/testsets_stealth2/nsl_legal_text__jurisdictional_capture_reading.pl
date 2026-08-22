% ============================================================================
% CONSTRAINT STORY: nsl_legal_text__jurisdictional_capture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
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
 *   human_readable: NSL Jurisdictional Capture — Mainland Legal Transplantation into the Hong Kong Common Law Order
 *   domain: constitutional/political/international
 *
 * SUMMARY:
 *   The National Security Law for Hong Kong, drafted by central authorities
 *   and imposed through the NPCSC on 30 June 2020, is read here as a vehicle
 *   for transplanting mainland legal-system features into Hong Kong's common
 *   law order: a mainland-staffed enforcement office beyond local court
 *   reach, a Chief Executive-appointed designated-judge panel replacing open
 *   case assignment, dispensed-with jury trials, tightened bail, warrantless
 *   surveillance powers, and a reserved Article 55 channel for direct
 *   mainland case handling. The epsilon referent is the standing arrangement
 *   — the law as imposed and operated — assessed by this reading's own
 *   lights; the restored-autonomy alternative this reading would prefer is
 *   not the referent. This file is one reading of the kernel nsl_legal_text;
 *   the sovereignty_restoration and democratic_enclosure readings are
 *   separate constraints with their own epsilon, beneficiary sets, and
 *   classifications, linked through network.affects_constraints.
 *
 * KEY AGENTS:
 *   - - central_national_security_authorities: agenda-setting beneficiary (institutional/arbitrage) — drafts, imposes, interprets; collects expanded jurisdiction
 *   - - office_for_safeguarding_national_security: resident beneficiary organ (institutional/arbitrage) — permanent mainland enforcement foothold inside Hong Kong
 *   - - hk_chief_executive_administration: dual-positioned administrator (institutional/constrained) — runs the machinery while ceding substantive control
 *   - - hong_kong_judiciary: primary institutional bearer of costs (organized/identity_locked) — designated panels, dispensed juries, narrowed independence
 *   - - hong_kong_independent_bar: secondary bearer of costs (organized/constrained) — oaths, colleague arrests, shrinking advocacy room
 *   - - hong_kong_residents_subject_to_nsl: diffuse bearer of costs (powerless/trapped) — transplanted procedure with no forum choice
 *   - - pro_beijing_legal_establishment: local beneficiary (organized/mobile) — careers rerouted through demonstrated reliability
 *   - - overseas_common_law_judges: excluded guarantors (powerful/arbitrage) — resigned rather than legitimize the new operation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nsl_legal_text__jurisdictional_capture_reading, 0.72).
domain_priors:suppression_score(nsl_legal_text__jurisdictional_capture_reading, 0.78).
domain_priors:theater_ratio(nsl_legal_text__jurisdictional_capture_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nsl_legal_text__jurisdictional_capture_reading, tangled_rope).
narrative_ontology:human_readable(nsl_legal_text__jurisdictional_capture_reading, "NSL Jurisdictional Capture — Mainland Legal Transplantation into the Hong Kong Common Law Order").
narrative_ontology:topic_domain(nsl_legal_text__jurisdictional_capture_reading, "constitutional/political/international").

domain_priors:requires_active_enforcement(nsl_legal_text__jurisdictional_capture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nsl_legal_text__jurisdictional_capture_reading, 'b98095e0-2b9a-423a-b872-2dbe9ee3819f').
narrative_ontology:cs_kernel_codification('b98095e0-2b9a-423a-b872-2dbe9ee3819f', fixed_text).
narrative_ontology:cs_authority_grounding('b98095e0-2b9a-423a-b872-2dbe9ee3819f', extraction).
narrative_ontology:cs_interpretation_layer_present('b98095e0-2b9a-423a-b872-2dbe9ee3819f').
narrative_ontology:cs_reading_relation('b98095e0-2b9a-423a-b872-2dbe9ee3819f', nsl_legal_text__sovereignty_restoration_reading, influences).
narrative_ontology:cs_reading_relation('b98095e0-2b9a-423a-b872-2dbe9ee3819f', nsl_legal_text__democratic_enclosure_reading, influences).
narrative_ontology:cs_axiom('b98095e0-2b9a-423a-b872-2dbe9ee3819f', foundational, common_law_autonomy_is_constitutive_promise).
narrative_ontology:cs_axiom_status(common_law_autonomy_is_constitutive_promise, holdable).
narrative_ontology:cs_axiom_grounding('b98095e0-2b9a-423a-b872-2dbe9ee3819f', common_law_autonomy_is_constitutive_promise, conventional).
narrative_ontology:cs_axiom('b98095e0-2b9a-423a-b872-2dbe9ee3819f', secondary, security_cases_require_local_adjudication).
narrative_ontology:cs_axiom_status(security_cases_require_local_adjudication, holdable).
narrative_ontology:cs_axiom_grounding('b98095e0-2b9a-423a-b872-2dbe9ee3819f', security_cases_require_local_adjudication, conventional).
narrative_ontology:cs_reference_frame('b98095e0-2b9a-423a-b872-2dbe9ee3819f', basic_law_autonomous_common_law_order).
narrative_ontology:cs_drift_state('b98095e0-2b9a-423a-b872-2dbe9ee3819f', post_nsl_imposition, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('b98095e0-2b9a-423a-b872-2dbe9ee3819f', '').
narrative_ontology:cs_kernel_id(nsl_legal_text__jurisdictional_capture_reading, nsl_legal_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nsl_legal_text__jurisdictional_capture_reading, central_national_security_authorities).
narrative_ontology:constraint_beneficiary(nsl_legal_text__jurisdictional_capture_reading, office_for_safeguarding_national_security).
narrative_ontology:constraint_beneficiary(nsl_legal_text__jurisdictional_capture_reading, pro_beijing_legal_establishment).
narrative_ontology:constraint_victim(nsl_legal_text__jurisdictional_capture_reading, hong_kong_judiciary).
narrative_ontology:constraint_victim(nsl_legal_text__jurisdictional_capture_reading, hong_kong_independent_bar).
narrative_ontology:constraint_victim(nsl_legal_text__jurisdictional_capture_reading, hong_kong_residents_subject_to_nsl).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(nsl_legal_text__jurisdictional_capture_reading, hk_chief_executive_administration).
narrative_ontology:constraint_vindicates(nsl_legal_text__jurisdictional_capture_reading, npcsc_interpretive_supremacy_doctrine).
narrative_ontology:constraint_vindicates(nsl_legal_text__jurisdictional_capture_reading, comprehensive_jurisdiction_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafted the law in Beijing and imposed it through the NPCSC on 30 June 2020 without Hong Kong legislative passage. Hold sole interpretive authority over the text, oversee the Committee for Safeguarding National Security through a Beijing-appointed adviser, and decide which cases fall under direct mainland handling under Article 55. Operate above the local court system and can route enforcement wherever convenient.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, central_national_security_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Mainland-staffed body established inside Hong Kong by the law itself. Its personnel answer to central authorities rather than local institutions and fall outside local court jurisdiction for acts under the law. Gains a permanent enforcement foothold in a jurisdiction that previously excluded mainland security operations: intelligence gathering, case referral, and the standing possibility of Article 55 case takeover.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, office_for_safeguarding_national_security, beneficiary,
    institutional, generational, arbitrage, national).

% Chairs the Committee for Safeguarding National Security, appoints the designated judges who may hear national security cases, and directs the new police national security department. Administers the machinery day to day while the substantive rules, interpretations, and escalation paths sit with Beijing. Each administrative act taken under the law narrows the administration's own discretionary space under the Basic Law framework it inherited.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, hk_chief_executive_administration, agenda_setter,
    institutional, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(nsl_legal_text__jurisdictional_capture_reading, hk_chief_executive_administration, payer).

% Judges hear national security cases under procedures that depart from common law norms: a Chief Executive-appointed designated-judge panel replaces open case assignment, jury trials are routinely dispensed with, and bail provisions tighten. Judicial independence rested on appointment and assignment practices the law now overrides. Individual judges cannot decline designated service without career consequence; the institution cannot refuse cases without inviting direct mainland handling under Article 55.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, hong_kong_judiciary, payer,
    organized, biographical, identity_locked, regional).

% Barristers and solicitors practice under new loyalty-oath requirements attached to certain offices and watch colleagues arrested for their representations or public commentary. Advocacy in national security cases proceeds within narrowed procedural room. Senior practitioners with international practices can relocate or withdraw; the profession's institutional voice carries diminishing weight against the new enforcement organs.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, hong_kong_independent_bar, payer,
    organized, biographical, constrained, regional).

% Persons investigated or prosecuted under the law encounter mainland-style procedure inside Hong Kong: closed hearings, restricted bail, warrantless communications surveillance, and offenses defined broadly enough to reach ordinary expression and association. Emigration offers individual exit for those with resources and visas; anyone remaining lives under the transplanted procedures with no choice of forum.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, hong_kong_residents_subject_to_nsl, payer,
    powerless, biographical, trapped, regional).

% Local lawyers, academics, and retired judges aligned with the central authorities gain appointments to designated panels, advisory committees, and national security education posts. Career advancement now runs through demonstrated reliability on national security matters rather than through the profession's traditional seniority channels.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, pro_beijing_legal_establishment, beneficiary,
    organized, biographical, mobile, regional).

% Distinguished foreign jurists formerly sat on the Court of Final Appeal as non-permanent judges, a visible guarantee of common law continuity. Several resigned after the law's imposition, stating they could no longer serve without lending legitimacy to its operation. They retain full freedom to speak and publish abroad but no longer sit inside the system whose direction they objected to.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, overseas_common_law_judges, excluded,
    powerful, biographical, arbitrage, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nsl_legal_text__jurisdictional_capture_reading, central_national_security_authorities).
narrative_ontology:fixing_cost_class(nsl_legal_text__jurisdictional_capture_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Before June 2020 Hong Kong had no national security legislation: Basic Law Article 23's mandate went unenacted for twenty-three years, leaving a category of offenses undefined and an unresolved enforcement question at the boundary between two legal systems. The law defines four offense categories, creates dedicated enforcement bodies, and establishes a single chain of command for security matters spanning the Hong Kong-mainland boundary.
% TRANSFER_FUNCTION: Moves adjudicative and coercive authority over security matters from Hong Kong's common law institutions — its courts, its legislature's oversight, its locally accountable police — to centrally controlled organs: NPCSC interpretation, the Beijing-advised security committee, the mainland-staffed office, and the reserved channel for direct mainland case handling. Alongside authority it moves procedural protections (open trial, jury participation, bail access, communications privacy) away from persons prosecuted under it.
% ABSENT_VOICES: Hong Kong's pan-democratic legislators were excluded from the law's drafting entirely — it was imposed without local legislative process — and were subsequently disqualified or resigned en masse. Defendants in national security cases have no seat in shaping the procedures applied to them. The independent bar's critical submissions and overseas judges' objections were received but carried no decision weight. These voices sit outside the room: disqualified from office, prosecuted, or withdrawn.
% DISAPPEARANCE_RATIONALE: If the law vanished overnight, the transplanted organs would lose their basis: the mainland-staffed office, the police national security department's special powers, and the designated-judge panel would dissolve, pending cases would revert to ordinary criminal procedure, and the pre-2020 boundary between the two legal systems would reassert itself. Hong Kong's current governance configuration is arranged around the law's continued operation.
% FOUNDING_PROBLEM: The 2019 protest cycle combined with the standing absence of national security legislation: central authorities judged that Hong Kong's institutions would not enact Article 23 legislation and that security threats, as Beijing defined them, were being handled inadequately under local law.
% FOUNDING_PROBLEM_CORROBORATION: Central authorities attest liveness through continuing prosecution counts and official threat assessments. Corroboration of the shifted-function reading comes from outside the benefiting parties: United Nations human rights treaty-body reviews, foreign judiciaries' resignation statements, and comparative constitutional scholarship documenting the law's institutional effects. No external source attests that the founding problem persists in its original form independently of the apparatus built around it.
narrative_ontology:disappearance_verdict(nsl_legal_text__jurisdictional_capture_reading, world_rearranges).
narrative_ontology:founding_problem_status(nsl_legal_text__jurisdictional_capture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nsl_legal_text__jurisdictional_capture_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(nsl_legal_text__jurisdictional_capture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nsl_legal_text__jurisdictional_capture_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is moderate-high (0.72 at interval end) because the law transfers adjudicative authority wholesale in its domain while retaining a genuine gap-filling function — the Article 23 void was real, and the offense categories address a definitional absence. Suppression is high (0.78) because persistence depends on active enforcement machinery — arrests, designated panels, the Article 55 shadow — rather than on participant preference. Theater is moderate-low (0.35): real enforcement dominates, but a growing share of activity is compliance performance (oath ceremonies, national security education, patriotic vetting of candidates and professionals). Accessibility collapse is 0.58: alternatives existed (local Article 23 legislation, judicial refusal, professional exit) and were foreclosed unevenly — completely for institutions, partially for mobile individuals. Resistance is 0.62: initial mass mobilization, international sanctions and extradition-treaty suspensions, bar association statements, overseas judge resignations, and an emigration wave. The suppression_requirement series is authored deliberately: this story traces enforcement-capacity maturation (department stand-up, first prosecutions, panel appointments, office activation), so the rising trajectory models hardening infrastructure, not merely shifting extraction. All three tracked metrics are authored on one shared grid (t = 0, 10, 20, 30, 40, 50, 60).
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setting seat the arrangement is a completed constitutional correction: the gap is filled, the boundary is secured, and local institutions execute faithfully. From the judiciary's seat the same structure reads as the progressive replacement of its own operating system — assignment, trial form, and bail norms it did not choose and cannot decline. The dual-positioned administration experiences both at once: it wields new powers whose exercise consumes the autonomy that justified its existence. The engine computes these divergent per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Central authorities and the resident office sit near the beneficiary pole: the arrangement subsidizes them with jurisdiction, physical presence, and interpretive monopoly, and their exit options are arbitrage-grade — they route enforcement wherever convenient. The judiciary sits near the target pole: it bears the procedural displacement and is identity-locked, since abandoning judicial office means abandoning the profession's constitutive commitments. The bar is targeted but partially mobile. Residents bear diffuse procedural costs with effectively trapped exit. The local administration derives mid-range directionality from its dual position: it collects new powers and posts while paying in autonomy. Pro-establishment beneficiaries collect career rents with mobile exit. Overseas judges are excluded rather than coordinated — their resignation is the observable signature of that exclusion.
 *
 * MANDATROPHY ANALYSIS:
 *   Classification discipline prevents two symmetrical mislabels. Reading the arrangement as pure coordination would erase the identifiable payers — the judiciary and bar whose operating norms are displaced — and launder jurisdictional transfer as gap-filling. Reading it as pure extraction would erase the genuine coordination surface: the Article 23 gap was real, and a security framework spanning the boundary solves a problem the pre-2020 arrangement left open. The tangled-rope structure holds both: coordination function and asymmetric transfer through the same instrument, held together by active enforcement. Mandatrophy risk runs in the other direction as well: if the founding security problem were ever authoritatively declared resolved, the enforcement apparatus would persist on inertia and performance — the theater-ratio series is the early-warning instrument for that transition, and the founding_problem_status omega tracks whether the problem stays live independently of the apparatus built for it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading of the kernel nsl_legal_text — the jurisdictional_capture_reading. How would instantiating the sibling readings change the structural data?',
    'Author the sibling stories (sovereignty_restoration_reading, democratic_enclosure_reading) against the same interval and compare computed classifications; the divergence locates the contest.',
    'Under sovereignty_restoration_reading the beneficiary set shrinks toward the sovereign''s constitutional order and epsilon falls toward coordination cost; under democratic_enclosure_reading the victim set expands to prosecuted dissidents and epsilon rises further.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame position: one of three readings of the NSL kernel.').

omega_variable(
    disagreement_location_structural_element,
    'Where exactly do the three readings locate the law''s operative center — transplantation of mainland legal practice (this reading), restoration of sovereign order, or closure of democratic space?',
    'Compare which structural elements each reading treats as load-bearing: beneficiary and victim sets, enforcement routing, and procedural change versus expressive-space change.',
    'The readings share a referent (the imposed law as operated) but author different epsilon and different victim sets; classification of the whole kernel is under-determined until the readings are compared.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disagreement_location_structural_element, conceptual, 'Location of the inter-reading disagreement within the kernel.').

omega_variable(
    article55_activation_status,
    'Has direct mainland jurisdiction under Article 55 actually been activated for any case, or does it operate purely as a deterrent shadow?',
    'Case-routing disclosure, prosecutorial records, and defense counsel testimony on where national security cases are actually handled.',
    'Activated Article 55 jurisdiction would push effective extraction higher and strengthen this reading against the restoration reading; confirmed non-use would indicate the transplantation operates through institutional presence rather than case takeover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article55_activation_status, empirical, 'Whether the law''s most extreme jurisdictional instrument is in use.').

omega_variable(
    designated_judge_functional_effect,
    'Does the designated-judge mechanism alter adjudication outcomes, or does it function as screening that would select the same judges anyway?',
    'Compare conviction rates, bail rates, and sentence severity in designated-panel cases against comparable ordinary-procedure cases before and after imposition.',
    'Functional alteration supports high institutional capture; a null effect would shift weight toward the law operating through offense definitions and enforcement routing rather than through the bench itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(designated_judge_functional_effect, empirical, 'Whether judicial capture is outcome-relevant or performative.').

omega_variable(
    common_law_erosion_reversibility,
    'Is the erosion of common law autonomy reversible if the political configuration changes, or have the transplanted structures acquired self-perpetuating form?',
    'Institutional removal-cost analysis: whether the offices, appointment pipelines, and doctrinal precedents could be dismantled by a future administration without constitutional-level action.',
    'Irreversibility pushes the arrangement toward permanent-fixture status and raises the cost side of any fixing assessment; reversibility keeps it contingent on the current enforcement coalition.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(common_law_erosion_reversibility, conceptual, 'Permanence of the transplanted structures.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nsl_legal_text__jurisdictional_capture_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nsl_capture_tr_t0, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(nsl_capture_tr_t0, observed).
narrative_ontology:measurement(nsl_capture_tr_t10, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 10, 0.24).
narrative_ontology:measurement_basis(nsl_capture_tr_t10, observed).
narrative_ontology:measurement(nsl_capture_tr_t20, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement_basis(nsl_capture_tr_t20, observed).
narrative_ontology:measurement(nsl_capture_tr_t30, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 30, 0.29).
narrative_ontology:measurement_basis(nsl_capture_tr_t30, observed).
narrative_ontology:measurement(nsl_capture_tr_t40, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 40, 0.31).
narrative_ontology:measurement_basis(nsl_capture_tr_t40, observed).
narrative_ontology:measurement(nsl_capture_tr_t50, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 50, 0.33).
narrative_ontology:measurement_basis(nsl_capture_tr_t50, observed).
narrative_ontology:measurement(nsl_capture_tr_t60, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 60, 0.35).
narrative_ontology:measurement_basis(nsl_capture_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(nsl_capture_be_t0, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement_basis(nsl_capture_be_t0, observed).
narrative_ontology:measurement(nsl_capture_be_t10, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 10, 0.64).
narrative_ontology:measurement_basis(nsl_capture_be_t10, observed).
narrative_ontology:measurement(nsl_capture_be_t20, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement_basis(nsl_capture_be_t20, observed).
narrative_ontology:measurement(nsl_capture_be_t30, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(nsl_capture_be_t30, observed).
narrative_ontology:measurement(nsl_capture_be_t40, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 40, 0.7).
narrative_ontology:measurement_basis(nsl_capture_be_t40, observed).
narrative_ontology:measurement(nsl_capture_be_t50, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 50, 0.71).
narrative_ontology:measurement_basis(nsl_capture_be_t50, observed).
narrative_ontology:measurement(nsl_capture_be_t60, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 60, 0.72).
narrative_ontology:measurement_basis(nsl_capture_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(nsl_capture_su_t0, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement_basis(nsl_capture_su_t0, observed).
narrative_ontology:measurement(nsl_capture_su_t10, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 10, 0.63).
narrative_ontology:measurement_basis(nsl_capture_su_t10, observed).
narrative_ontology:measurement(nsl_capture_su_t20, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 20, 0.66).
narrative_ontology:measurement_basis(nsl_capture_su_t20, observed).
narrative_ontology:measurement(nsl_capture_su_t30, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 30, 0.69).
narrative_ontology:measurement_basis(nsl_capture_su_t30, observed).
narrative_ontology:measurement(nsl_capture_su_t40, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(nsl_capture_su_t40, observed).
narrative_ontology:measurement(nsl_capture_su_t50, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 50, 0.75).
narrative_ontology:measurement_basis(nsl_capture_su_t50, observed).
narrative_ontology:measurement(nsl_capture_su_t60, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 60, 0.78).
narrative_ontology:measurement_basis(nsl_capture_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nsl_legal_text__jurisdictional_capture_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(nsl_legal_text__jurisdictional_capture_reading, sovereignty_restoration_reading).
narrative_ontology:affects_constraint(nsl_legal_text__jurisdictional_capture_reading, democratic_enclosure_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the NSL' conflates at least three structurally distinct claims — sovereign security provision, jurisdictional transplantation, and democratic closure. Per the epsilon-invariance principle each is authored as its own constraint story with its own epsilon, beneficiaries, and victims; this file authors the jurisdictional-capture claim (epsilon approximately 0.72, institutional victims: the judiciary and independent bar). Family links run through network.affects_constraints. The upstream restoration reading supplies the legitimating frame this reading critiques; the enclosure reading is downstream of the institutional capture documented here, since enclosed political space becomes cheaper to maintain once the courts are captured.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
