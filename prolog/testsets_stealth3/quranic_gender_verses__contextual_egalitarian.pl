% ============================================================================
% CONSTRAINT STORY: quranic_gender_verses__contextual_egalitarian
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quranic_gender_verses__contextual_egalitarian, []).

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
 *   constraint_id: quranic_gender_verses__contextual_egalitarian
 *   human_readable: Contextual-Egalitarian Reading of the Qur'anic Gender Verses
 *   domain: religious/legal/hermeneutic
 *
 * SUMMARY:
 *   This story models ONE reading of a contested kernel. The kernel is the
 *   set of Qur'anic gender verses (4:11 inheritance shares, 2:282 testimony
 *   weight, 4:34 marital discipline); the reading instantiated here is the
 *   contextual-egalitarian one: these verses are historically situated
 *   progressive steps within seventh-century Arabia, and their legal force
 *   today must be derived by reinterpreting them under the Qur'an's
 *   overarching equity principles (maqasid). The standing arrangement under
 *   contest — and therefore the epsilon referent — is the institutionalized
 *   contextual-egalitarian interpretive regime itself, assessed by the
 *   reading's own lights: a regime that genuinely moves women out of the
 *   victim set of the classical application, while creating a new
 *   interpretive-authority class whose mediation women's claims must pass
 *   through, displacing traditional establishments that bear the costs, and
 *   generating legitimacy-conflict costs borne diffusely by communities and
 *   unconsulted laity. The claim and the metrics are independent authored
 *   facts: I claim tangled_rope because I judge the structure to possess both
 *   a genuine coordination function (bridging revelation and equality norms
 *   without schism) and asymmetric extraction (authority rents to the
 *   reformist class, displacement costs to traditional establishments); the
 *   metrics describe the regime's actual operation as I observe it. Sibling
 *   readings are separate constraints (linked via
 *   network.affects_constraints), not hedges inside this one.
 *
 * KEY AGENTS:
 *   - - reformist_scholars: Agenda-setting interpreter class (organized/mobile) — administers the maqasid method, collects interpretive authority and citation rents
 *   - - rights_based_ngos: Secondary beneficiary (organized/mobile) — collects funding, standing, and case outcomes from the framework's adoption
 *   - - muslim_women: Intended principal beneficiary (moderate/identity_locked) — gains formal claims, pays mediation dependence and community backlash
 *   - - patriarchal_lineage_elders: Payer (organized/constrained) — loses arbitral authority over property and household matters
 *   - - traditional_sharia_courts: Payer (institutional/constrained) — loses adjudicative discretion whichever way jurisdictional politics breaks
 *   - - traditionalist_clerical_establishment: Payer (institutional/identity_locked) — custodial identity forbids adopting the rival method
 *   - - lay_conservative_believers: Excluded voice (powerless/identity_locked) — absorbs doctrinal whiplash without a seat in the negotiation
 *   - - international_human_rights_monitors: Analytical observer (institutional/analytical) — documents the rhetoric-practice gap from outside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quranic_gender_verses__contextual_egalitarian, 0.45).
domain_priors:suppression_score(quranic_gender_verses__contextual_egalitarian, 0.35).
domain_priors:theater_ratio(quranic_gender_verses__contextual_egalitarian, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quranic_gender_verses__contextual_egalitarian, extractiveness, 0.45).
narrative_ontology:constraint_metric(quranic_gender_verses__contextual_egalitarian, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(quranic_gender_verses__contextual_egalitarian, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quranic_gender_verses__contextual_egalitarian, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(quranic_gender_verses__contextual_egalitarian, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quranic_gender_verses__contextual_egalitarian, tangled_rope).
narrative_ontology:human_readable(quranic_gender_verses__contextual_egalitarian, "Contextual-Egalitarian Reading of the Qur'anic Gender Verses").
narrative_ontology:topic_domain(quranic_gender_verses__contextual_egalitarian, "religious/legal/hermeneutic").

domain_priors:requires_active_enforcement(quranic_gender_verses__contextual_egalitarian).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quranic_gender_verses__contextual_egalitarian, '2958435e-052a-4910-9bea-7b857d22cb63').
narrative_ontology:cs_kernel_codification('2958435e-052a-4910-9bea-7b857d22cb63', fixed_text).
narrative_ontology:cs_authority_grounding('2958435e-052a-4910-9bea-7b857d22cb63', expertise).
narrative_ontology:cs_interpretation_layer_present('2958435e-052a-4910-9bea-7b857d22cb63').
narrative_ontology:cs_reading_relation('2958435e-052a-4910-9bea-7b857d22cb63', quranic_gender_verses__literal_hierarchical, forecloses).
narrative_ontology:cs_reading_relation('2958435e-052a-4910-9bea-7b857d22cb63', quranic_gender_verses__progressive_abrogation, influences).
narrative_ontology:cs_axiom('2958435e-052a-4910-9bea-7b857d22cb63', foundational, gender_verses_contextually_bound).
narrative_ontology:cs_axiom_status(gender_verses_contextually_bound, holdable).
narrative_ontology:cs_axiom_grounding('2958435e-052a-4910-9bea-7b857d22cb63', gender_verses_contextually_bound, empirically_contingent).
narrative_ontology:cs_axiom('2958435e-052a-4910-9bea-7b857d22cb63', foundational, maqasid_govern_specific_rulings).
narrative_ontology:cs_axiom_status(maqasid_govern_specific_rulings, holdable).
narrative_ontology:cs_axiom_grounding('2958435e-052a-4910-9bea-7b857d22cb63', maqasid_govern_specific_rulings, theological).
narrative_ontology:cs_reference_frame('2958435e-052a-4910-9bea-7b857d22cb63', maqasid_equity_trajectory).
narrative_ontology:cs_drift_state('2958435e-052a-4910-9bea-7b857d22cb63', contemporary_post_musawah_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('2958435e-052a-4910-9bea-7b857d22cb63', '').
narrative_ontology:cs_kernel_id(quranic_gender_verses__contextual_egalitarian, quranic_gender_verses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quranic_gender_verses__contextual_egalitarian, reformist_scholars).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__contextual_egalitarian, rights_based_ngos).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__contextual_egalitarian, muslim_women).
narrative_ontology:constraint_victim(quranic_gender_verses__contextual_egalitarian, patriarchal_lineage_elders).
narrative_ontology:constraint_victim(quranic_gender_verses__contextual_egalitarian, traditional_sharia_courts).
narrative_ontology:constraint_victim(quranic_gender_verses__contextual_egalitarian, traditionalist_clerical_establishment).
narrative_ontology:constraint_vindicates(quranic_gender_verses__contextual_egalitarian, maqasid_al_shariah_doctrine).
narrative_ontology:constraint_vindicates(quranic_gender_verses__contextual_egalitarian, historical_contextualism_in_tafsir).
narrative_ontology:constraint_vindicates(quranic_gender_verses__contextual_egalitarian, quranic_equity_principles).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Trained in both classical legal methodology and modern academic methods, they produce the commentaries, advisory opinions, and expert testimony that recast verses 4:11, 2:282, and 4:34 as historically situated steps requiring equitable reinterpretation. Legislators and judges cite their work when applying reinterpreted family-law rules. Their professional standing rests on continued demand for authoritative reinterpretation; they publish across transnational academic and religious networks and can shift venues if one turns hostile.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, reformist_scholars, agenda_setter,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(quranic_gender_verses__contextual_egalitarian, reformist_scholars, beneficiary).

% Litigate and campaign for equal inheritance shares and testimony weight using the contextual-egalitarian framework, drawing funding from international donors and credibility from scholarly endorsement. They collect grants, institutional standing, and case outcomes from the framework's adoption; their operational base lets them relocate campaigns across jurisdictions when one closes.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, rights_based_ngos, beneficiary,
    organized, biographical, mobile, global).

% Seek equal inheritance shares, full testimony weight, and protection from unilateral marital discipline inside the faith rather than outside it. Formal claims arrive through reformed codes and court rulings built on the reinterpretive framework; pursuing them usually requires aligning with reformist counsel, and pressing claims draws family and community backlash. Leaving the tradition entirely would cost faith, kinship, and belonging, so most stay and contend within it.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, muslim_women, beneficiary,
    moderate, generational, identity_locked, global).

% Heads of extended families whose authority over property division, marriage arrangement, and household discipline rests on the classical application of these verses. Reinterpreted rulings strip their arbitral role and redirect disputes to state courts applying egalitarian readings. Their standing is tied to the local moral order; adopting the new framework themselves would dissolve the very authority it displaces.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, patriarchal_lineage_elders, payer,
    organized, generational, constrained, regional).

% State religious courts staffed by classically trained judges. Where legislatures adopt contextual-egalitarian readings, these courts must apply them against judicial habit and training; where they resist, they face reversal on appeal and international criticism. Their adjudicative discretion narrows either way.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, traditional_sharia_courts, payer,
    institutional, generational, constrained, national).

% Seminaries, mufti councils, and transnational scholarly bodies whose custodianship of the classical corpus defines their office. They issue counter-opinions declaring the contextual method impermissible innovation, and their institutional identity forbids adopting the rival method without dissolving the tradition they exist to guard.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, traditionalist_clerical_establishment, payer,
    institutional, generational, identity_locked, global).

% Ordinary worshippers whose received understanding treats the verses as plain directives. Neither scholarly camp consults them: reformist publications address courts and policy elites, traditionalist responses address seminaries. They encounter the changes as finished rulings and absorb the resulting confusion in congregational life.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, lay_conservative_believers, excluded,
    powerless, biographical, identity_locked, global).

% Treaty bodies and comparative-law researchers tracking whether Muslim-family-law states meet equality commitments. They document the gap between reformist rhetoric and enforceable outcomes and supply the external record that the other seats argue over.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, international_human_rights_monitors, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quranic_gender_verses__contextual_egalitarian, reformist_scholars).
narrative_ontology:fixing_cost_class(quranic_gender_verses__contextual_egalitarian, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the legitimation crisis between revealed text and contemporary equality norms: it gives Muslim-majority legal systems a single framework in which scriptural fidelity and equal-inheritance/equal-testimony claims can be honored together, keeping egalitarian-minded believers inside the tradition instead of forcing a binary between literal submission and secular exit.
% TRANSFER_FUNCTION: Moves interpretive and adjudicative authority from classical clerical establishments and lineage elders to reformist scholars and rights institutions; moves inheritance shares and testimony weight toward women in reformed rulings; moves legitimacy-conflict costs onto conservative households, resisting courts, and unconsulted lay believers.
% ABSENT_VOICES: Lay conservative believers and non-elite women whose lived religiosity matches neither scholarly camp would object that the settlement is negotiated over their heads; classical-methodology specialists would object that maqasid reasoning is deployed loosely. They sit outside the conference circuit, the courtroom, and the seminary — in congregations and homes — and enter the record only as survey respondents or anecdote.
% DISAPPEARANCE_RATIONALE: If the contextual-egalitarian interpretive regime vanished overnight, reformed family codes would lose their jurisprudential foundation, recent gains in women's inheritance and testimony claims would revert to classical application, rights organizations would lose their litigation basis, and the community would face a starker choice between literal hierarchy and exit from the tradition — the founding collision would reopen at higher stakes.
% FOUNDING_PROBLEM: The nineteenth- and twentieth-century collision between colonial-era and international equality norms and the classical jurisprudence of personal status: Muslim-majority societies needed a way to reform family law — inheritance, testimony, marital discipline — without abandoning revelation as the source of legitimate law.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: UN treaty-body reviews of Muslim-family-law states document the ongoing collision; comparative family-law scholarship records it as a standing research problem; and the traditionalist clerical establishment itself attests the collision exists — while disputing the remedy. No party denies the founding problem; the parties divide over whether the contextual method solves it or relocates it.
narrative_ontology:disappearance_verdict(quranic_gender_verses__contextual_egalitarian, world_rearranges).
narrative_ontology:founding_problem_status(quranic_gender_verses__contextual_egalitarian, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quranic_gender_verses__contextual_egalitarian, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quranic_gender_verses__contextual_egalitarian, 'none', 1).
narrative_ontology:epsilon_provenance(quranic_gender_verses__contextual_egalitarian, 0.45, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quranic_gender_verses__contextual_egalitarian_tests).
:- end_tests(quranic_gender_verses__contextual_egalitarian_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45): by the reading's own lights the classical extraction from women is substantially dismantled, but the regime substitutes a mediation structure — claims route through reformist counsel, expert testimony, and NGO litigation — whose operators collect authority rents, and it levies displacement and conflict costs on traditional seats. Suppression (0.35) reflects canonical and institutional pressure on dissenting judges and clerics within adopting jurisdictions, not global silencing: the sibling readings remain fully legal and widely held elsewhere. Theater (0.31) tracks the growing share of performative output — conferences, shadow reports, symbolic rulings — relative to enforceable change. Accessibility collapse is low (0.25): understanding the contextual method does not close off the literal or abrogationist alternatives, which remain live and argued. Resistance is high (0.60): counter-fatwas, judicial pushback, and public controversy are constitutive of this regime's operating environment, not incidental to it. The temporal series run on one shared grid (t=0..60, mapping roughly 1965-2025) with all three metrics authored at every point. Suppression_requirement is tracked because the story's enforcement picture genuinely changed: the regime began as academic argument with almost no coercive machinery and accumulated courts, statutes, donor-conditioned advocacy, and canonical pressure over the interval — a maturing enforcement infrastructure, not a static one. The trajectories are monotonic (institutionalization, not oscillation), so no cyclical analysis is warranted.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the traditionalist seats the regime operates as usurpation: an unelected interpretive class strips established authority and brands inherited practice as backward. From the reformist and NGO seats the same structure is scholarly correction finally reaching enforceable law. From the women's seat it splits: formal claims gained, but access to them mediated by a new gatekeeping class and paid for in family and community backlash. From the excluded lay seat it arrives as finished rulings issued over their heads. The engine computes these divergent per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the reformist scholars, NGOs, and women toward the beneficiary end; victim declarations drive the elders, courts, and clerical establishment toward the target end. One override is authored: muslim_women derive near-pure-beneficiary directionality from their beneficiary declaration alone, but their benefit is conditional (claims execute only through reformist adjudication) and they bear real backlash costs, so their true position sits measurably toward the target end of pure beneficiary. Because the override surface is keyed at power-atom granularity, the correction is authored on the 'moderate' atom, which in this story only muslim_women occupy. No other override is needed: the derivation from declarations plus exit options already separates the mobile reformist class from the identity-locked traditionalist establishment correctly.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two opposite mislabels. Read as its advocates present it — pure liberation, coordination only — the regime would be scored a rope, concealing the mediation rents and the displacement costs that fund them; the tangled-rope classification forces the asymmetry into view. Read as its opponents present it — elite capture dressed as feminism — it would be scored a snare, erasing the genuine coordination function (keeping egalitarian believers inside the tradition, resolving the revelation/equality collision without schism) and the real exit of women from the victim set. The founding problem is live, so no mandatrophy is declared: the collision this regime was built to solve persists, and the regime's function has not yet outlived its mandate — though the mediation-rent omega marks the path by which it could.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_allocation,
    'This constraint instantiates the contextual_egalitarian reading of the quranic_gender_verses kernel; how would the classification change under the sibling readings?',
    'Track institutional allegiance over time: court citation patterns, family-code amendments, and seminary curricula adoption across the three readings. Whichever reading captures adjudicating institutions determines the operative victim set.',
    'Under literal_hierarchical, women return to the victim set and epsilon rises sharply; under progressive_abrogation, the verses'' operative force is voided and the mediation rents collected by the reformist class migrate to codification bodies. The moderate-extraction profile is specific to this reading''s settlement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_allocation, conceptual, 'Committer-frame uncertainty: one kernel, three readings, three different constraint structures.').

omega_variable(
    mediation_rent_persistence,
    'Is women''s access to equal shares and testimony weight permanently dependent on scholarly mediation, or does spreading lay competence in maqasid reasoning dissolve the mediation layer?',
    'Measure diffusion of interpretive literacy below the scholar class: lay-led study circles, women filing inheritance claims without scholar intermediaries, mosque-level teaching of contextual method.',
    'If mediation dissolves, effective extraction falls toward pure-coordination territory; if it hardens into a new gatekeeping class, the reading reproduces the clericalism it displaced with different personnel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mediation_rent_persistence, empirical, 'Whether the reformist mediation layer is transitional or self-perpetuating.').

omega_variable(
    rhetoric_practice_gap,
    'Does the reading''s egalitarian output reach enforceable rulings, or does it remain advisory rhetoric while courts continue classical application?',
    'Compare appellate outcomes and statutory amendments against scholarly publication volume per decade in adopting jurisdictions.',
    'A persistent gap raises the theater ratio and pushes the computed type toward inertial performance in nominally reformed jurisdictions; convergence confirms functional coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rhetoric_practice_gap, empirical, 'Implementation gap between reformist doctrine and enforceable practice.').

omega_variable(
    conflict_cost_incidence,
    'Who absorbs the intra-community legitimacy-conflict costs this reading generates, and do women bear disproportionate backlash when they press the claims it creates?',
    'Survey incidence of family estrangement, accusation campaigns, and violence following reformist rulings, disaggregated by who invoked the ruling.',
    'If women absorb the backlash, their net position worsens despite formal gains and their effective directionality sits further toward the target end than the authored override records.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conflict_cost_incidence, empirical, 'Incidence of the legitimacy-conflict costs the reading''s operation creates.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quranic_gender_verses__contextual_egalitarian, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quranic_gender_verses__contextual_egalitarian, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(qura_tr_t0, observed).
narrative_ontology:measurement(qura_tr_t10, quranic_gender_verses__contextual_egalitarian, theater_ratio, 10, 0.14).
narrative_ontology:measurement_basis(qura_tr_t10, observed).
narrative_ontology:measurement(qura_tr_t20, quranic_gender_verses__contextual_egalitarian, theater_ratio, 20, 0.18).
narrative_ontology:measurement_basis(qura_tr_t20, observed).
narrative_ontology:measurement(qura_tr_t30, quranic_gender_verses__contextual_egalitarian, theater_ratio, 30, 0.22).
narrative_ontology:measurement_basis(qura_tr_t30, observed).
narrative_ontology:measurement(qura_tr_t40, quranic_gender_verses__contextual_egalitarian, theater_ratio, 40, 0.26).
narrative_ontology:measurement_basis(qura_tr_t40, observed).
narrative_ontology:measurement(qura_tr_t50, quranic_gender_verses__contextual_egalitarian, theater_ratio, 50, 0.29).
narrative_ontology:measurement_basis(qura_tr_t50, observed).
narrative_ontology:measurement(qura_tr_t60, quranic_gender_verses__contextual_egalitarian, theater_ratio, 60, 0.31).
narrative_ontology:measurement_basis(qura_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quranic_gender_verses__contextual_egalitarian, base_extractiveness, 0, 0.22).
narrative_ontology:measurement_basis(qura_be_t0, observed).
narrative_ontology:measurement(qura_be_t10, quranic_gender_verses__contextual_egalitarian, base_extractiveness, 10, 0.3).
narrative_ontology:measurement_basis(qura_be_t10, observed).
narrative_ontology:measurement(qura_be_t20, quranic_gender_verses__contextual_egalitarian, base_extractiveness, 20, 0.36).
narrative_ontology:measurement_basis(qura_be_t20, observed).
narrative_ontology:measurement(qura_be_t30, quranic_gender_verses__contextual_egalitarian, base_extractiveness, 30, 0.4).
narrative_ontology:measurement_basis(qura_be_t30, observed).
narrative_ontology:measurement(qura_be_t40, quranic_gender_verses__contextual_egalitarian, base_extractiveness, 40, 0.43).
narrative_ontology:measurement_basis(qura_be_t40, observed).
narrative_ontology:measurement(qura_be_t50, quranic_gender_verses__contextual_egalitarian, base_extractiveness, 50, 0.44).
narrative_ontology:measurement_basis(qura_be_t50, observed).
narrative_ontology:measurement(qura_be_t60, quranic_gender_verses__contextual_egalitarian, base_extractiveness, 60, 0.45).
narrative_ontology:measurement_basis(qura_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quranic_gender_verses__contextual_egalitarian, suppression_requirement, 0, 0.12).
narrative_ontology:measurement_basis(qura_su_t0, observed).
narrative_ontology:measurement(qura_su_t10, quranic_gender_verses__contextual_egalitarian, suppression_requirement, 10, 0.16).
narrative_ontology:measurement_basis(qura_su_t10, observed).
narrative_ontology:measurement(qura_su_t20, quranic_gender_verses__contextual_egalitarian, suppression_requirement, 20, 0.21).
narrative_ontology:measurement_basis(qura_su_t20, observed).
narrative_ontology:measurement(qura_su_t30, quranic_gender_verses__contextual_egalitarian, suppression_requirement, 30, 0.26).
narrative_ontology:measurement_basis(qura_su_t30, observed).
narrative_ontology:measurement(qura_su_t40, quranic_gender_verses__contextual_egalitarian, suppression_requirement, 40, 0.3).
narrative_ontology:measurement_basis(qura_su_t40, observed).
narrative_ontology:measurement(qura_su_t50, quranic_gender_verses__contextual_egalitarian, suppression_requirement, 50, 0.33).
narrative_ontology:measurement_basis(qura_su_t50, observed).
narrative_ontology:measurement(qura_su_t60, quranic_gender_verses__contextual_egalitarian, suppression_requirement, 60, 0.35).
narrative_ontology:measurement_basis(qura_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quranic_gender_verses__contextual_egalitarian, identity_coordination).
narrative_ontology:affects_constraint(quranic_gender_verses__contextual_egalitarian, quranic_gender_verses__literal_hierarchical).
narrative_ontology:affects_constraint(quranic_gender_verses__contextual_egalitarian, quranic_gender_verses__progressive_abrogation).

% DUAL FORMULATION NOTE:
% The colloquial label 'what the Qur'an says about gender' covers three structurally distinct constraints sharing one kernel (quranic_gender_verses): the literal-hierarchical reading (timeless differentiated ordinance, women in the victim set, high epsilon), this contextual-egalitarian reading (historically situated verses reinterpreted under maqasid, women moved out of the victim set, moderate epsilon with new mediation rents), and the progressive-abrogation reading (verses superseded by later universal-dignity principles, operative force voided). Each gets its own epsilon, beneficiaries, and victims; they are linked here as a constraint family. The upstream literal reading historically supplied the enforcement baseline against which this reading's institutional gains are measured; this reading in turn pressures the abrogation reading's reason for existing.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quranic_gender_verses__contextual_egalitarian, moderate, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
