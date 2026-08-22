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
 *   constraint_id: wto_dsb_authority__judicial_activism_reading
 *   human_readable: WTO Dispute Settlement Body as Illegitimate Judicial Legislator (Judicial Activism Reading)
 *   domain: international_law/trade_governance/institutional_legitimacy
 *
 * SUMMARY:
 *   This story instantiates the judicial_activism_reading of the
 *   wto_dsb_authority kernel: the position that DSB panels and the Appellate
 *   Body, over three decades, progressively expanded treaty obligations
 *   beyond what member states ratified — reading implicit obligations into
 *   silences, extending tests like 'necessity' and 'like products' well past
 *   their negotiated scope, and effectively legislating trade law through
 *   case-by-case interpretation rather than applying agreed text. From this
 *   reading's own lights, the standing arrangement (DSB adjudication as
 *   currently practiced) is substantially extractive: it transfers regulatory
 *   discretion from national legislatures to an unelected panel process and
 *   to the litigation-capable states and professionals who benefit from an
 *   expanding jurisprudence. This is NOT a story about the alternative
 *   arrangement this reading would prefer (a narrowly textualist DSB, or none
 *   at all) — epsilon here describes the arrangement under contest, assessed
 *   as this reading sees it.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wto_dsb_authority__judicial_activism_reading, 0.68).
domain_priors:suppression_score(wto_dsb_authority__judicial_activism_reading, 0.58).
domain_priors:theater_ratio(wto_dsb_authority__judicial_activism_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, resistance, 0.82).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wto_dsb_authority__judicial_activism_reading, snare).
narrative_ontology:human_readable(wto_dsb_authority__judicial_activism_reading, "WTO Dispute Settlement Body as Illegitimate Judicial Legislator (Judicial Activism Reading)").
narrative_ontology:topic_domain(wto_dsb_authority__judicial_activism_reading, "international_law/trade_governance/institutional_legitimacy").

domain_priors:requires_active_enforcement(wto_dsb_authority__judicial_activism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(wto_dsb_authority__judicial_activism_reading, 'fce65b72-f318-417e-bc3c-dd8fed76b59f').
narrative_ontology:cs_kernel_codification('fce65b72-f318-417e-bc3c-dd8fed76b59f', fixed_text).
narrative_ontology:cs_authority_grounding('fce65b72-f318-417e-bc3c-dd8fed76b59f', extraction).
narrative_ontology:cs_interpretation_layer_present('fce65b72-f318-417e-bc3c-dd8fed76b59f').
narrative_ontology:cs_reading_relation('fce65b72-f318-417e-bc3c-dd8fed76b59f', wto_dsb_authority__binding_referee_reading, coexists_with).
narrative_ontology:cs_reading_relation('fce65b72-f318-417e-bc3c-dd8fed76b59f', wto_dsb_authority__advisory_coordination_reading, coexists_with).
narrative_ontology:cs_axiom('fce65b72-f318-417e-bc3c-dd8fed76b59f', foundational, adjudicative_bodies_bound_strictly_to_negotiated_text).
narrative_ontology:cs_axiom_status(adjudicative_bodies_bound_strictly_to_negotiated_text, holdable).
narrative_ontology:cs_axiom_grounding('fce65b72-f318-417e-bc3c-dd8fed76b59f', adjudicative_bodies_bound_strictly_to_negotiated_text, conventional).
narrative_ontology:cs_axiom('fce65b72-f318-417e-bc3c-dd8fed76b59f', foundational, accreted_precedent_without_renewed_consent_is_illegitimate).
narrative_ontology:cs_axiom_status(accreted_precedent_without_renewed_consent_is_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('fce65b72-f318-417e-bc3c-dd8fed76b59f', accreted_precedent_without_renewed_consent_is_illegitimate, deontological).
narrative_ontology:cs_reference_frame('fce65b72-f318-417e-bc3c-dd8fed76b59f', gatt_era_consent_based_dispute_resolution).
narrative_ontology:cs_drift_state('fce65b72-f318-417e-bc3c-dd8fed76b59f', post_appellate_body_paralysis_2024, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('fce65b72-f318-417e-bc3c-dd8fed76b59f', '').
narrative_ontology:cs_kernel_id(wto_dsb_authority__judicial_activism_reading, wto_dsb_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wto_dsb_authority__judicial_activism_reading, complainant_states_with_litigation_capacity).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__judicial_activism_reading, wto_secretariat_legal_staff).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__judicial_activism_reading, trade_law_bar).
narrative_ontology:constraint_victim(wto_dsb_authority__judicial_activism_reading, respondent_states_facing_novel_obligations).
narrative_ontology:constraint_victim(wto_dsb_authority__judicial_activism_reading, domestic_regulatory_agencies).
narrative_ontology:constraint_victim(wto_dsb_authority__judicial_activism_reading, national_legislatures).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret treaty text in rulings that, in this reading, progressively manufacture obligations no negotiating state agreed to — reading obligations into silences, extending 'like products' and necessity tests, and building a de facto common law of trade that no member ratified. They administer the mechanism and set its interpretive trajectory ruling by ruling.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, dsb_appellate_panelists, agenda_setter,
    institutional, generational, analytical, global).

% Well-resourced states with standing trade law bureaucracies file and win complaints that extend jurisprudence in directions favorable to their own exporters. They benefit from an activist reading because expansive precedent, once established, can be redeployed against future respondents regardless of original treaty text.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, complainant_states_with_litigation_capacity, beneficiary,
    powerful, biographical, arbitrage, global).

% Find domestic measures — public health regulation, environmental standards, industrial policy — struck down under obligations that were never explicitly negotiated but emerged through a chain of panel and Appellate Body readings. They can comply, absorb retaliatory tariffs, or defy the ruling; withdrawing from the system entirely risks broader trade isolation, so exit is constrained rather than free.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, respondent_states_facing_novel_obligations, payer,
    moderate, biographical, constrained, national).

% Draft food safety, environmental, and labor regulations that must now anticipate an evolving and unpredictable body of trade jurisprudence rather than a fixed treaty text. Regulatory design is chilled because the boundary of permissible measures shifts with each new ruling; they have no direct seat in the dispute process that reshapes their mandate.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, domestic_regulatory_agencies, payer,
    moderate, biographical, trapped, national).

% Ratified a specific treaty text through domestic constitutional processes; in this reading, panels have since layered obligations onto that text without any further act of domestic ratification or consent. Legislatures have no formal channel to contest or amend the accreted jurisprudence short of withdrawing from the WTO altogether.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, national_legislatures, payer,
    institutional, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(wto_dsb_authority__judicial_activism_reading, national_legislatures, excluded).

% Institutional staff whose relevance, budget, and professional standing grow with the scope and complexity of DSB jurisprudence. An expansive, self-elaborating body of case law increases demand for their expertise and secures the institution's centrality regardless of member state consent to that expansion.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, wto_secretariat_legal_staff, beneficiary,
    institutional, generational, analytical, global).

% Private law firms and trade litigators build lucrative practices interpreting and litigating the accreting jurisprudence. Interpretive drift generates billable ambiguity; a stable, narrowly textual regime would shrink the market for their services.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, trade_law_bar, beneficiary,
    organized, biographical, arbitrage, global).

% States that have begun blocking Appellate Body appointments, refusing to implement adverse rulings, or routing disputes outside the DSB entirely. They would object formally that the panels have exceeded their mandate, but the DSB process itself provides no mechanism to adjudicate that objection — the body accused of overreach is also the only forum for raising the complaint.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, member_states_withdrawing_cooperation, excluded,
    organized, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(wto_dsb_authority__judicial_activism_reading, diffuse).
narrative_ontology:fixing_cost_class(wto_dsb_authority__judicial_activism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Nominally, resolving trade disputes through neutral adjudication of agreed treaty text so members are not forced into unilateral retaliation cycles. This reading holds that function has been substantially displaced by interpretive expansion beyond the text.
% TRANSFER_FUNCTION: Moves regulatory sovereignty and policy discretion from national legislatures and regulatory agencies to DSB panels and, secondarily, to litigation-capable complainant states and the trade law professionals who service the resulting jurisprudence.
% ABSENT_VOICES: Domestic legislatures and regulatory agencies whose ratified text is being reinterpreted have no seat in the panel process itself; states attempting to resist compliance (by blocking appellate appointments or refusing implementation) are treated as violators of the system rather than as parties raising a legitimate legitimacy objection.
% DISAPPEARANCE_RATIONALE: If DSB authority as currently exercised disappeared, respondent states would regain unreviewed domestic regulatory discretion, complainant states would lose a forum for extending precedent favorable to their exporters, and the trade law bar's litigation practice around WTO jurisprudence would substantially contract. States already blocking Appellate Body appointments would have their de facto position vindicated rather than treated as noncompliance.
% FOUNDING_PROBLEM: Pre-WTO trade disputes were resolved through unilateral retaliation and diplomatic power politics (GATT panel reports could be blocked by the losing party), leaving weaker states exposed. The DSB was built to replace power-based dispute resolution with rule-based adjudication of specific negotiated commitments.
% FOUNDING_PROBLEM_CORROBORATION: Sovereignty scholars, several G20 trade ministries, and at least one major power's official trade policy statements attest that panels have moved from applying negotiated text to constructing obligations via evolving interpretive doctrine — a view corroborated by academic international law critiques written independently of the states currently blocking appellate appointments. The panels and secretariat, who benefit from the current scope, dispute this characterization, so corroboration exists on both sides but the strongest independent source (legal academics not party to any dispute) tends to support the drift diagnosis at least in specific doctrinal areas.
narrative_ontology:disappearance_verdict(wto_dsb_authority__judicial_activism_reading, world_rearranges).
narrative_ontology:founding_problem_status(wto_dsb_authority__judicial_activism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(wto_dsb_authority__judicial_activism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(wto_dsb_authority__judicial_activism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(wto_dsb_authority__judicial_activism_reading, 0.68, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness rises steadily across the interval (0.28 to 0.68) reflecting the accumulation of precedent this reading identifies as interpretive overreach — each ruling compounds on prior rulings rather than referring back cleanly to text. Theater ratio also rises (0.15 to 0.5): panels increasingly perform textual fidelity ('we merely interpret the covered agreements') while, in this reading, substantively creating new law — the theatrical gap between claimed method and actual function widens. Suppression rises through 2019 as the Appellate Body's authority hardens (near-automatic adoption of rulings, retaliation authorization) then dips slightly by 2024 as the appellate body itself becomes non-functional due to blocked appointments — an observable data point this reading treats as vindication rather than crisis. Resistance is authored high (0.82) because this reading's central empirical claim IS that states are actively resisting compliance and withdrawing cooperation from the enforcement mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Panelists and secretariat staff sit as agenda-setters/beneficiaries: they administer the interpretive process and their institutional relevance grows with its scope, so their directionality sits near the beneficiary end. Litigation-capable complainant states and the trade law bar also benefit structurally — expansive precedent is redeployable capital for future litigation. Respondent states, domestic regulators, and national legislatures are targets: they bear obligations they never explicitly consented to, with constrained or trapped exit (leaving the WTO is catastrophic; contesting a ruling inside the DSB process is not available as a real option). Member states now blocking appellate appointments are coded excluded rather than payer because their structural position in this reading is that of parties raising a legitimacy objection through the only lever available to them — non-cooperation — precisely because the forum that would adjudicate their objection is the body accused of the overreach.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (replacing unilateral retaliation with rule-based adjudication) is authored as contested rather than flatly dead: the reading does not claim adjudication itself is illegitimate, only that panels have drifted past their textual mandate into functional legislation. This keeps the classification honest — a pure snare reading would claim the entire coordination function was always pretextual; this reading instead traces a drift from a real founding coordination function toward accumulated extraction, which is why resistance and suppression both rise together rather than suppression alone explaining persistence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_interpretation_vs_gap_filling,
    'Is DSB jurisprudential evolution genuine treaty interpretation using accepted tools (VCLT Article 31-32, context, object and purpose) or is it functionally equivalent to legislating new obligations the negotiating parties never agreed to?',
    'Systematic comparison of specific doctrinal lines (e.g., the evolution of ''necessity'' tests under GATT Article XX, zeroing methodology in anti-dumping cases) against the plain text and negotiating history (travaux préparatoires) of the relevant agreements, ideally assessed by international law scholars with no litigation stake in either direction.',
    'If panels are found to be engaging in legitimate interpretive gap-filling within accepted doctrinal bounds, this reading''s premise collapses and the constraint is better modeled as the binding_referee_reading. If panels are found to be manufacturing obligations with no textual anchor, this reading''s high extractiveness score is vindicated as descriptively accurate rather than merely one party''s framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_interpretation_vs_gap_filling, conceptual, 'Whether DSB jurisprudential evolution is legitimate interpretation or illegitimate obligation-creation.').

omega_variable(
    which_reading_the_manifest_taxonomy_admits,
    'The kernel taxonomy declares three coexisting readings (advisory, binding referee, judicial activism) as though they are equally available framings of the same institutional practice — but is the judicial_activism_reading actually a minority position confined to specific dissenting states, or does it capture a broader, more mainstream critique of investor-state and trade adjudication systems generally?',
    'Survey of official government trade policy statements, WTO reform proposal submissions, and academic literature to establish whether the activism critique is a fringe position or a substantial current within international trade law scholarship and state practice circa the Appellate Body crisis (2017-2020).',
    'If the activism reading is a genuine mainstream position (not merely a rationalization by states seeking to evade unfavorable rulings), the reading''s structural claims about accumulated illegitimate extraction carry more evidentiary weight; if it is primarily deployed instrumentally by powerful states resisting adverse rulings, the reading itself may function partly as cover for those states'' own extraction (using ''legitimacy'' language to escape obligations they in fact accepted).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(which_reading_the_manifest_taxonomy_admits, empirical, 'Whether the judicial activism critique is a genuine mainstream legal position or an instrumentally deployed cover story for noncompliance.').

omega_variable(
    appellate_body_paralysis_causal_direction,
    'Did blocking Appellate Body appointments (rendering it non-functional since 2019) cause the drop in the suppression_requirement measurement, or does the drop reflect an independent decline in enforcement capacity that would have occurred regardless of the legitimacy dispute?',
    'Trace the timeline and stated justifications for the appointment blockage against WTO dispute filing and compliance rates in the same period to separate a deliberate legitimacy-driven withdrawal from an unrelated institutional decay.',
    'If appointment-blocking is a deliberate, reasoned response to perceived overreach, it corroborates this reading''s central claim of active, principled resistance. If it reflects unrelated great-power trade politics (e.g., using WTO paralysis as leverage in bilateral disputes), the resistance data point is weaker evidence for the judicial_activism_reading specifically.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(appellate_body_paralysis_causal_direction, empirical, 'Whether declining enforcement suppression reflects principled resistance to overreach or unrelated institutional politics.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wto_dsb_authority__judicial_activism_reading, 1995, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wto__tr_t1995, wto_dsb_authority__judicial_activism_reading, theater_ratio, 1995, 0.15).
narrative_ontology:measurement(wto__tr_t2001, wto_dsb_authority__judicial_activism_reading, theater_ratio, 2001, 0.22).
narrative_ontology:measurement(wto__tr_t2008, wto_dsb_authority__judicial_activism_reading, theater_ratio, 2008, 0.3).
narrative_ontology:measurement(wto__tr_t2014, wto_dsb_authority__judicial_activism_reading, theater_ratio, 2014, 0.38).
narrative_ontology:measurement(wto__tr_t2019, wto_dsb_authority__judicial_activism_reading, theater_ratio, 2019, 0.46).
narrative_ontology:measurement(wto__tr_t2024, wto_dsb_authority__judicial_activism_reading, theater_ratio, 2024, 0.5).

% Extraction over time
narrative_ontology:measurement(wto__be_t1995, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 1995, 0.28).
narrative_ontology:measurement(wto__be_t2001, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 2001, 0.38).
narrative_ontology:measurement(wto__be_t2008, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 2008, 0.5).
narrative_ontology:measurement(wto__be_t2014, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 2014, 0.6).
narrative_ontology:measurement(wto__be_t2019, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 2019, 0.66).
narrative_ontology:measurement(wto__be_t2024, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(wto__su_t1995, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 1995, 0.3).
narrative_ontology:measurement(wto__su_t2001, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 2001, 0.4).
narrative_ontology:measurement(wto__su_t2008, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 2008, 0.48).
narrative_ontology:measurement(wto__su_t2014, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 2014, 0.55).
narrative_ontology:measurement(wto__su_t2019, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 2019, 0.62).
narrative_ontology:measurement(wto__su_t2024, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 2024, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wto_dsb_authority__judicial_activism_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(wto_dsb_authority__judicial_activism_reading, wto_dsb_authority__binding_referee_reading).
narrative_ontology:affects_constraint(wto_dsb_authority__judicial_activism_reading, wto_dsb_authority__advisory_coordination_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposed from the single natural-language label 'WTO Dispute Settlement Body authority,' per the epsilon-invariance principle: the label conflates three structurally distinct claims about the same institutional practice, each with a different epsilon (this reading's 0.68 vs. a low epsilon in the binding_referee_reading and a very low epsilon in the advisory_coordination_reading) and different victim/beneficiary structures. All three are linked as siblings via affects_constraints; each carries its own claimed_type and stakeholder set rather than averaging across the contested interpretation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
