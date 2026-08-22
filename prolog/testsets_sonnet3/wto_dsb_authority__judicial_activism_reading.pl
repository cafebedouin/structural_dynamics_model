% ============================================================================
% CONSTRAINT STORY: wto_dsb_authority__judicial_activism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_wto_dsb_authority__judicial_activism_reading, []).

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
 *   constraint_id: wto_dsb_authority__judicial_activism_reading
 *   human_readable: WTO Dispute Settlement Body as Illegitimate Judicial Legislator
 *   domain: international_law/trade_governance/institutional_legitimacy
 *
 * SUMMARY:
 *   This story instantiates the judicial_activism_reading of the
 *   wto_dsb_authority kernel: the view that WTO dispute panels and the
 *   Appellate Body, over three decades, have progressively substituted their
 *   own evolving jurisprudence for the negotiated text of the covered
 *   agreements, generating compliance obligations member states never agreed
 *   to and treating de facto precedent as binding despite the WTO's explicit
 *   rejection of stare decisis. On this reading, the DSB's authority is not
 *   merely contested policy but structurally illegitimate judicial
 *   legislation: panels reach beyond dispute-specific textual questions to
 *   issue broad interpretive pronouncements, treat prior panel/AB reasoning
 *   as quasi-binding despite no such treaty basis, and authorize retaliation
 *   on the strength of these extended readings. This is a distinct constraint
 *   from the binding_referee_reading (which treats the same rulings as valid
 *   law fully within delegated authority) and the
 *   advisory_coordination_reading (which treats the same panels as
 *   non-binding facilitators) — same institutional apparatus, three
 *   structurally different constraints, per the epsilon-invariance principle.
 *
 * KEY AGENTS:
 *   - dsb_appellate_body_alumni: Primary beneficiary (institutional/arbitrage) — professional and reputational capital from expansive interpretive authority
 *   - trade_law_litigation_bar: Secondary beneficiary (organized/mobile) — fee income from doctrinal complexity
 *   - developing_country_members: Primary target (powerless/trapped) — absorb interpretive drift as binding law without capacity to resist
 *   - national_legislatures: Secondary target (powerful/constrained) — sovereign policy space foreclosed after ratification
 *   - united_states_trade_representative: Excluded-yet-agenda-setting actor (powerful/arbitrage) — blockaded Appellate Body appointments in protest of this exact dynamic
 *   - wto_general_council: Analytical observer (institutional/analytical) — holds unused Article IX interpretive authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wto_dsb_authority__judicial_activism_reading, 0.71).
domain_priors:suppression_score(wto_dsb_authority__judicial_activism_reading, 0.62).
domain_priors:theater_ratio(wto_dsb_authority__judicial_activism_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wto_dsb_authority__judicial_activism_reading, snare).
narrative_ontology:human_readable(wto_dsb_authority__judicial_activism_reading, "WTO Dispute Settlement Body as Illegitimate Judicial Legislator").
narrative_ontology:topic_domain(wto_dsb_authority__judicial_activism_reading, "international_law/trade_governance/institutional_legitimacy").

domain_priors:requires_active_enforcement(wto_dsb_authority__judicial_activism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(wto_dsb_authority__judicial_activism_reading, '3d84190d-5708-430a-a83f-91ced70d9853').
narrative_ontology:cs_kernel_codification('3d84190d-5708-430a-a83f-91ced70d9853', fixed_text).
narrative_ontology:cs_authority_grounding('3d84190d-5708-430a-a83f-91ced70d9853', extraction).
narrative_ontology:cs_interpretation_layer_present('3d84190d-5708-430a-a83f-91ced70d9853').
narrative_ontology:cs_reading_relation('3d84190d-5708-430a-a83f-91ced70d9853', wto_dsb_authority__binding_referee_reading, coexists_with).
narrative_ontology:cs_reading_relation('3d84190d-5708-430a-a83f-91ced70d9853', wto_dsb_authority__advisory_coordination_reading, coexists_with).
narrative_ontology:cs_axiom('3d84190d-5708-430a-a83f-91ced70d9853', foundational, panel_authority_limited_to_delegated_textual_scope).
narrative_ontology:cs_axiom_status(panel_authority_limited_to_delegated_textual_scope, holdable).
narrative_ontology:cs_axiom_grounding('3d84190d-5708-430a-a83f-91ced70d9853', panel_authority_limited_to_delegated_textual_scope, conventional).
narrative_ontology:cs_axiom('3d84190d-5708-430a-a83f-91ced70d9853', secondary, precedent_accretion_without_treaty_basis_is_illegitimate).
narrative_ontology:cs_axiom_status(precedent_accretion_without_treaty_basis_is_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('3d84190d-5708-430a-a83f-91ced70d9853', precedent_accretion_without_treaty_basis_is_illegitimate, empirically_contingent).
narrative_ontology:cs_reference_frame('3d84190d-5708-430a-a83f-91ced70d9853', uruguay_round_negotiated_text_baseline).
narrative_ontology:cs_drift_state('3d84190d-5708-430a-a83f-91ced70d9853', post_2019_appellate_body_paralysis, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('3d84190d-5708-430a-a83f-91ced70d9853', '').
narrative_ontology:cs_kernel_id(wto_dsb_authority__judicial_activism_reading, wto_dsb_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wto_dsb_authority__judicial_activism_reading, dsb_appellate_body_alumni).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__judicial_activism_reading, trade_law_litigation_bar).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__judicial_activism_reading, wto_secretariat_legal_staff).
narrative_ontology:constraint_victim(wto_dsb_authority__judicial_activism_reading, developing_country_members).
narrative_ontology:constraint_victim(wto_dsb_authority__judicial_activism_reading, domestic_regulatory_agencies).
narrative_ontology:constraint_victim(wto_dsb_authority__judicial_activism_reading, national_legislatures).
narrative_ontology:constraint_vindicates(wto_dsb_authority__judicial_activism_reading, member_state_sovereignty_over_undelegated_policy_domains).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Former and sitting panelists and Appellate Body members build professional reputation, academic careers, and consulting practices on an expansive reading of panel interpretive authority. The broader the panels construe their own mandate, the more their expertise is in demand across future disputes; they have no exposure to the compliance costs their rulings impose on member states.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, dsb_appellate_body_alumni, beneficiary,
    institutional, generational, arbitrage, global).

% A specialized bar of trade lawyers earns fees advising states and firms on DSB litigation strategy. An unpredictable, precedent-accreting jurisprudence increases demand for their interpretive expertise; they profit from doctrinal complexity regardless of which member state wins or loses.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, trade_law_litigation_bar, beneficiary,
    organized, biographical, mobile, global).

% Provides legal support to panels, drafts background analysis, and shapes the interpretive frameworks panels draw on. Institutional prestige and budgetary relevance are tied to the DSB continuing to be treated as the authoritative interpreter of covered agreements, giving the Secretariat's legal staff an interest in sustaining and expanding the panels' claimed interpretive reach.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, wto_secretariat_legal_staff, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(wto_dsb_authority__judicial_activism_reading, wto_secretariat_legal_staff, beneficiary).

% Face rulings that construe treaty silence as prohibition or create compliance obligations (on subsidies, industrial policy, regulatory sequencing) not textually agreed to at accession. Lack the legal capacity to litigate as thoroughly as wealthier members and cannot credibly threaten to defect from the system without risking market access, so they absorb interpretive drift as binding law regardless of its legitimacy.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, developing_country_members, payer,
    powerless, generational, trapped, global).

% Design health, safety, environmental, and industrial regulations that panels have struck down or narrowed using tests (e.g., 'necessity,' 'least trade restrictive') not specified in the underlying agreements' text. Must anticipate and pre-conform domestic rulemaking to unwritten panel doctrine or risk future adverse rulings and authorized retaliation against unrelated sectors.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, domestic_regulatory_agencies, payer,
    moderate, biographical, constrained, national).

% Ratified specific treaty text through domestic constitutional processes but find policy space they believed reserved to democratic deliberation foreclosed by panel interpretations layered atop the text years later. Formal exit (withdrawal from WTO) is available in principle but carries catastrophic economic cost, making the constrained exit largely theoretical.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, national_legislatures, payer,
    powerful, generational, constrained, national).

% Has for years blocked new Appellate Body appointments, arguing panels systematically exceed their mandate by treating precedent as binding, addressing issues not raised by parties, and creating obligations beyond the covered agreements. Effectively vetoed the enforcement mechanism itself rather than continuing to litigate within it, functioning simultaneously as an aggrieved excluded voice and as an agenda-setter reshaping the system by blockade.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, united_states_trade_representative, excluded,
    powerful, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(wto_dsb_authority__judicial_activism_reading, united_states_trade_representative, agenda_setter).

% Nominally holds authority to adopt authoritative interpretations by supermajority vote under Article IX of the Marrakesh Agreement, but has never exercised it, leaving panels as the de facto interpretive authority by default rather than by delegation. Watches the interpretive drift accumulate without correcting it.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, wto_general_council, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(wto_dsb_authority__judicial_activism_reading, diffuse).
narrative_ontology:fixing_cost_class(wto_dsb_authority__judicial_activism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In principle, a standing dispute mechanism solves a real problem: without it, trade disputes would be settled by unilateral retaliation and raw bargaining power, favoring the strongest economies. A neutral panel process could, if it stayed within its textual mandate, coordinate expectations around agreed rules.
% TRANSFER_FUNCTION: Interpretive rulings that go beyond the negotiated text move policy discretion from national legislatures and regulatory agencies to panels and the legal professionals who service them, and move compliance costs from powerful members (who can absorb or evade unfavorable rulings) to weaker members who cannot.
% ABSENT_VOICES: The original treaty negotiators whose textual bargains are being reinterpreted are gone from the table; domestic legislatures whose statutes get struck down had no seat in the panel proceeding; the WTO General Council, which holds the actual interpretive authority under Article IX, has never been convened to ratify or reject the panels' expansive readings.
% DISAPPEARANCE_RATIONALE: Proponents of this reading argue the world would improve if the DSB's claimed interpretive authority disappeared overnight — member states would revert to negotiating genuine consensus interpretations through the General Council, and domestic policy space would be restored. Beneficiaries (the legal bar, Secretariat staff, alumni) argue the trading system would collapse into unilateral retaliation. The dispute over the counterfactual is itself part of the kernel contest.
% FOUNDING_PROBLEM: The Uruguay Round created a standing dispute settlement mechanism specifically to end the GATT-era practice of unilateral, power-based retaliation and blocked panel reports, by giving disputes a rules-based, quasi-automatic adjudication path grounded in members' negotiated textual commitments.
% FOUNDING_PROBLEM_CORROBORATION: The USTR, several G20 developing-country delegations, and academic critics (writing outside the trade-law bar's own journals) attest that the DSB has drifted from adjudicating negotiated text to generating new obligations via evolving interpretive doctrine — a status corroborated by the unprecedented US blockade of Appellate Body appointments since 2019, itself an act by a member state rather than a beneficiary of the arrangement. No corroboration exists from a source independent of the system's own legal community affirming that the founding problem remains narrowly live in its original form.
narrative_ontology:disappearance_verdict(wto_dsb_authority__judicial_activism_reading, contested).
narrative_ontology:founding_problem_status(wto_dsb_authority__judicial_activism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(wto_dsb_authority__judicial_activism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(wto_dsb_authority__judicial_activism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(wto_dsb_authority__judicial_activism_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(wto_dsb_authority__judicial_activism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(wto_dsb_authority__judicial_activism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(wto_dsb_authority__judicial_activism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.71) and rising because, on this reading, each cycle of panel interpretation that goes beyond the specific dispute's textual question compounds into doctrine that binds future disputants who never litigated or consented to it — a ratchet, not a stable equilibrium. Suppression is authored substantial but not maximal (0.62): retaliation authorization is a real coercive lever, but this reading's own evidence (the US Appellate Body blockade, rising non-compliance) shows the suppression is incomplete and increasingly resisted, which is precisely the expected structural delta for this reading. Theater ratio is authored elevated and rising (0.58) because a growing share of DSB activity, on this reading, consists of panels performing textual fidelity ('we merely apply the treaty as written') while substantively expanding obligations — the performative gap between claimed and actual interpretive method is itself the theater. Resistance is authored very high (0.85), reflecting the reading's core empirical claim: this is not a quietly accepted constraint but one under open institutional siege.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (Appellate Body alumni, the litigation bar, Secretariat legal staff) sit near the full-beneficiary end: they collect professional and institutional rents from the system's operation regardless of who wins any given case, and have global arbitrage-grade mobility. Developing country members sit near the full-target end: trapped by market-access dependence, they cannot exit the enforcement system even as they bear compounding compliance costs from doctrine they had no hand in shaping. National legislatures and domestic regulators are targets with nominally greater exit options (formal withdrawal is legally available) but the option is not real-world exercisable given the economic stakes, so their exit_options are authored as constrained rather than mobile. The USTR is authored with a dual role deliberately: it is a powerful excluded voice within the adjudicative process (it cannot simply argue the panels are wrong and have that argument binding) but it is simultaneously an agenda-setter at the meta-level, having exercised the one lever it does control — blocking appointments — to force the issue. This dual role is the central structural fact of the judicial_activism_reading and is why it diverges sharply from the binding_referee_reading, where the same US action would be read as a violation of an obligation rather than a legitimate check on an illegitimate expansion.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (ending GATT-era unilateral retaliation through rules-based adjudication) is authored as contested-status rather than flatly dead, because the underlying coordination need has not vanished — trade disputes still require some adjudicative mechanism. What this reading contends has gone wrong is not that the function became obsolete but that the mechanism built to serve it exceeded its delegated scope and kept the trappings of textual fidelity (mandatrophy in the classic sense: the mandate persists in form while the substance drifts). The mismatch the engine should detect is exactly status=contested/dead-leaning against a disappearance_verdict of contested rather than world_unchanged: beneficiaries insist the system remains essential (arguing for world_rearranges-for-the-worse), while critics on this reading argue a reformed, textually-disciplined mechanism would produce a BETTER-coordinated world, not a worse one — the disagreement is over whether the current apparatus IS the coordination function or a captured deformation of it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_fidelity_vs_gap_filling,
    'Is panel/Appellate Body reasoning that extends beyond the plain text of a covered agreement properly characterized as illegitimate law-creation, or as ordinary interpretive gap-filling of the kind all adjudicative bodies perform when treaty text is silent or ambiguous?',
    'Systematic comparison of panel/AB reports against the interpretive methods authorized under the Vienna Convention on the Law of Treaties (Articles 31-32), coded by independent international-law scholars not affiliated with WTO dispute practice, assessing whether specific rulings (e.g., the ''necessity'' test under GATT Article XX, zeroing methodology disputes) are within ordinary treaty interpretation or constitute de facto new rules.',
    'If panel reasoning is substantially within VCLT-authorized interpretive method, this reading''s core premise weakens toward the binding_referee_reading; if panels are shown to be routinely creating obligations with no textual anchor, the judicial_activism_reading''s structural claim is strengthened and epsilon should be authored even higher in future iterations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_fidelity_vs_gap_filling, conceptual, 'Whether interpretive drift constitutes illegitimate law-creation or ordinary gap-filling — the central contested premise of this reading.').

omega_variable(
    kernel_reading_selection_evidence,
    'What observable evidence should govern which of the three kernel readings (binding_referee, advisory_coordination, judicial_activism) is the operative one for a given member state or dispute, given that the same DSB apparatus is read three different ways by different parties?',
    'Track member-state compliance behavior and public justificatory rhetoric across a sample of DSB rulings: near-automatic compliance without protest is consistent with the binding_referee_reading; negotiated settlement without formal compliance is consistent with advisory_coordination_reading; non-compliance accompanied by explicit claims of panel overreach (as the US has made) is the signature this reading''s stakeholders exhibit.',
    'If most members most of the time behave consistently with binding_referee_reading, the judicial_activism_reading describes a minority or currently-salient-but-historically-marginal position rather than the modal experience of the constraint; if non-compliance-with-overreach-claims is rising as a share of disputes, this reading''s structural delta (active resistance, withdrawal from enforcement) is empirically validated and strengthening.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_evidence, empirical, 'Which kernel reading best describes actual member-state behavior, and whether that is shifting over time toward this reading.').

omega_variable(
    article_ix_dormancy_significance,
    'Does the General Council''s three-decade non-use of its Article IX authoritative-interpretation power constitute tacit ratification of the panels'' expansive interpretive practice, or does it reflect only the supermajority voting threshold''s practical unattainability, carrying no substantive endorsement?',
    'Review of General Council session records and member statements for any explicit discussion of invoking Article IX to correct or ratify panel doctrine; absence of any serious attempt would support the dormancy-as-practical-obstacle reading rather than tacit ratification.',
    'If dormancy reflects genuine acquiescence, the judicial_activism_reading''s illegitimacy claim weakens (silence read as consent); if dormancy reflects only procedural unattainability, the reading''s claim that panels operate as a default authority never actually delegated to them by the membership is strengthened.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(article_ix_dormancy_significance, conceptual, 'Whether unused corrective authority implies consent to the status quo or merely reveals a broken corrective mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wto_dsb_authority__judicial_activism_reading, 1995, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wto__tr_t1995, wto_dsb_authority__judicial_activism_reading, theater_ratio, 1995, 0.15).
narrative_ontology:measurement(wto__tr_t2000, wto_dsb_authority__judicial_activism_reading, theater_ratio, 2000, 0.22).
narrative_ontology:measurement(wto__tr_t2005, wto_dsb_authority__judicial_activism_reading, theater_ratio, 2005, 0.3).
narrative_ontology:measurement(wto__tr_t2010, wto_dsb_authority__judicial_activism_reading, theater_ratio, 2010, 0.38).
narrative_ontology:measurement(wto__tr_t2015, wto_dsb_authority__judicial_activism_reading, theater_ratio, 2015, 0.46).
narrative_ontology:measurement(wto__tr_t2019, wto_dsb_authority__judicial_activism_reading, theater_ratio, 2019, 0.53).
narrative_ontology:measurement(wto__tr_t2024, wto_dsb_authority__judicial_activism_reading, theater_ratio, 2024, 0.58).

% Extraction over time
narrative_ontology:measurement(wto__be_t1995, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 1995, 0.28).
narrative_ontology:measurement(wto__be_t2000, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 2000, 0.35).
narrative_ontology:measurement(wto__be_t2005, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 2005, 0.44).
narrative_ontology:measurement(wto__be_t2010, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 2010, 0.52).
narrative_ontology:measurement(wto__be_t2015, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 2015, 0.6).
narrative_ontology:measurement(wto__be_t2019, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 2019, 0.67).
narrative_ontology:measurement(wto__be_t2024, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 2024, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(wto__su_t1995, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 1995, 0.4).
narrative_ontology:measurement(wto__su_t2000, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 2000, 0.45).
narrative_ontology:measurement(wto__su_t2005, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 2005, 0.5).
narrative_ontology:measurement(wto__su_t2010, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 2010, 0.53).
narrative_ontology:measurement(wto__su_t2015, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 2015, 0.55).
narrative_ontology:measurement(wto__su_t2019, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 2019, 0.58).
narrative_ontology:measurement(wto__su_t2024, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 2024, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wto_dsb_authority__judicial_activism_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(wto_dsb_authority__judicial_activism_reading, 0.1).
narrative_ontology:affects_constraint(wto_dsb_authority__judicial_activism_reading, wto_dsb_authority__binding_referee_reading).
narrative_ontology:affects_constraint(wto_dsb_authority__judicial_activism_reading, wto_dsb_authority__advisory_coordination_reading).
narrative_ontology:affects_constraint(wto_dsb_authority__judicial_activism_reading, wto_appellate_body_appointment_blockade).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraints decomposing the natural-language label 'WTO Dispute Settlement Body authority' per the epsilon-invariance principle: the same institutional apparatus (panels, Appellate Body, DSU procedures) is read as illegitimate judicial legislation here, as fully authorized binding adjudication in wto_dsb_authority__binding_referee_reading, and as non-binding advisory facilitation in wto_dsb_authority__advisory_coordination_reading. Each carries its own epsilon, beneficiary/victim structure, and claimed_type; they must not be merged or averaged. This story additionally links to wto_appellate_body_appointment_blockade, the concrete downstream institutional consequence (US refusal to confirm new AB members since 2019) that this reading treats as a legitimate structural check and the binding_referee_reading would treat as a treaty violation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(wto_dsb_authority__judicial_activism_reading, powerful, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
