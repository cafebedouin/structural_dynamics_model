% ============================================================================
% CONSTRAINT STORY: article_17_complementarity__international_oversight_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_17_complementarity__international_oversight_reading, []).

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
 *   constraint_id: article_17_complementarity__international_oversight_reading
 *   human_readable: Article 17 Complementarity — International Oversight Reading
 *   domain: international_law/criminal_justice/state_sovereignty
 *
 * SUMMARY:
 *   This story instantiates the international-oversight reading of the
 *   Article 17 complementarity kernel: complementarity functions as an
 *   accountability-trigger, with the ICC acting as a guardian against
 *   impunity whenever domestic proceedings are judged to lack independence or
 *   genuine prosecutorial intent. Under this reading, 'unwilling or unable'
 *   is interpreted broadly enough to capture victor's justice arrangements,
 *   elite immunity deals, and institutionally hollowed-out judiciaries — not
 *   merely total legal collapse. This is a distinct constraint from the
 *   sibling national-primacy reading (constraint_id:
 *   national_primacy_reading), which treats national courts as presumptively
 *   adequate and places the burden of proof on the Court to demonstrate
 *   inadmissibility. The two readings produce different admissibility
 *   thresholds, different victim sets, and different cooperation-demand
 *   intensities from the same treaty text; they are linked in
 *   network.affects_constraints, not merged into one story.
 *
 * KEY AGENTS:
 *   - icc_office_of_the_prosecutor: primary agenda-setter (institutional/analytical) — determines admissibility and drives the broad reading
 *   - victims_in_complicit_states and victims_in_failed_states: primary beneficiaries (powerless/trapped) — gain a route to accountability otherwise foreclosed
 *   - domestic_elites_shielded_by_sham_proceedings and non_cooperating_state_governments: primary targets (powerful/institutional, constrained exit) — bear intensified scrutiny and cooperation demands
 *   - powerful_state_parties_and_non_parties: excluded voice — largely outside the practical reach of the broad standard, an asymmetry raised from outside the room
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_17_complementarity__international_oversight_reading, 0.42).
domain_priors:suppression_score(article_17_complementarity__international_oversight_reading, 0.55).
domain_priors:theater_ratio(article_17_complementarity__international_oversight_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_17_complementarity__international_oversight_reading, tangled_rope).
narrative_ontology:human_readable(article_17_complementarity__international_oversight_reading, "Article 17 Complementarity — International Oversight Reading").
narrative_ontology:topic_domain(article_17_complementarity__international_oversight_reading, "international_law/criminal_justice/state_sovereignty").

domain_priors:requires_active_enforcement(article_17_complementarity__international_oversight_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_17_complementarity__international_oversight_reading, 'a1d6a5fe-dc58-46f5-a667-f60f0ba8c15e').
narrative_ontology:cs_kernel_codification('a1d6a5fe-dc58-46f5-a667-f60f0ba8c15e', fixed_text).
narrative_ontology:cs_authority_grounding('a1d6a5fe-dc58-46f5-a667-f60f0ba8c15e', practice).
narrative_ontology:cs_interpretation_layer_present('a1d6a5fe-dc58-46f5-a667-f60f0ba8c15e').
narrative_ontology:cs_reading_relation('a1d6a5fe-dc58-46f5-a667-f60f0ba8c15e', article_17_complementarity__national_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('a1d6a5fe-dc58-46f5-a667-f60f0ba8c15e', foundational, accountability_backstop_overrides_sovereign_deference).
narrative_ontology:cs_axiom_status(accountability_backstop_overrides_sovereign_deference, holdable).
narrative_ontology:cs_axiom_grounding('a1d6a5fe-dc58-46f5-a667-f60f0ba8c15e', accountability_backstop_overrides_sovereign_deference, deontological).
narrative_ontology:cs_axiom('a1d6a5fe-dc58-46f5-a667-f60f0ba8c15e', secondary, institutional_capture_evidence_lowers_admissibility_bar).
narrative_ontology:cs_axiom_status(institutional_capture_evidence_lowers_admissibility_bar, holdable).
narrative_ontology:cs_axiom_grounding('a1d6a5fe-dc58-46f5-a667-f60f0ba8c15e', institutional_capture_evidence_lowers_admissibility_bar, empirically_contingent).
narrative_ontology:cs_reference_frame('a1d6a5fe-dc58-46f5-a667-f60f0ba8c15e', rome_statute_founding_compromise).
narrative_ontology:cs_drift_state('a1d6a5fe-dc58-46f5-a667-f60f0ba8c15e', post_africa_bias_criticism_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a1d6a5fe-dc58-46f5-a667-f60f0ba8c15e', '').
narrative_ontology:cs_kernel_id(article_17_complementarity__international_oversight_reading, article_17_complementarity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_17_complementarity__international_oversight_reading, victims_in_complicit_states).
narrative_ontology:constraint_beneficiary(article_17_complementarity__international_oversight_reading, victims_in_failed_states).
narrative_ontology:constraint_beneficiary(article_17_complementarity__international_oversight_reading, international_criminal_accountability_norm).
narrative_ontology:constraint_victim(article_17_complementarity__international_oversight_reading, domestic_elites_shielded_by_sham_proceedings).
narrative_ontology:constraint_victim(article_17_complementarity__international_oversight_reading, non_cooperating_state_governments).
narrative_ontology:constraint_victim(article_17_complementarity__international_oversight_reading, national_judiciaries_deemed_captured).
narrative_ontology:constraint_vindicates(article_17_complementarity__international_oversight_reading, no_impunity_for_atrocity_crimes).
narrative_ontology:constraint_vindicates(article_17_complementarity__international_oversight_reading, international_accountability_backstop_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Determines whether a state's domestic proceedings reflect genuine intent or are staged to shield perpetrators. Under this reading it applies a low admissibility threshold, opening or maintaining investigations whenever domestic process shows signs of political interference, sham charges, or elite protection. It carries the institutional risk of being seen as neo-colonial overreach if it intervenes too readily.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, icc_office_of_the_prosecutor, agenda_setter,
    institutional, generational, analytical, global).

% Live under a state whose domestic prosecutions of powerful perpetrators are symbolic or deliberately weak. Under this reading, the low admissibility bar gives them a route to accountability that would otherwise never open, since they have no capacity to compel domestic prosecution themselves.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, victims_in_complicit_states, beneficiary,
    powerless, biographical, trapped, national).

% Live where judicial institutions have collapsed entirely, leaving no credible domestic forum. The broad 'unable' reading treats institutional collapse itself as sufficient grounds for ICC intervention, giving these victims a path to a case that domestic breakdown would otherwise foreclose completely.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, victims_in_failed_states, beneficiary,
    powerless, biographical, trapped, national).

% Have historically relied on staging token domestic prosecutions, acquittals, or delayed proceedings to block ICC jurisdiction under a narrower complementarity reading. Under this broad reading, such tactics are read through and treated as evidence of unwillingness, exposing them to ICC prosecution they believed they had foreclosed.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, domestic_elites_shielded_by_sham_proceedings, payer,
    powerful, biographical, constrained, national).

% Face intensified cooperation demands — surrender requests, evidence-sharing obligations, and diplomatic pressure — once the Court determines their domestic proceedings are inadequate. Their sovereignty claims are treated with skepticism under this reading; non-cooperation itself becomes evidence supporting the inadmissibility finding against them.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, non_cooperating_state_governments, payer,
    institutional, generational, constrained, national).

% Domestic courts and prosecutorial bodies whose independence is publicly questioned once the ICC deems their proceedings insufficiently genuine. This finding damages institutional legitimacy and invites external scrutiny of judicial appointments, funding, and political interference, regardless of internal reform efforts already underway.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, national_judiciaries_deemed_captured, payer,
    institutional, generational, trapped, national).

% States with permanent Security Council seats, non-ratifying major powers, or strong bilateral leverage are rarely subject to this broad admissibility reading in practice — their nationals face referral, jurisdictional, and enforcement barriers that weaker states do not. Their absence from meaningful scrutiny under this reading is the central objection victims'-rights advocates raise, and the objection is voiced from outside the room where prosecutorial discretion is actually exercised.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, powerful_state_parties_and_non_parties, excluded,
    institutional, generational, arbitrage, global).

% Oversees the Court's budget, judicial appointments, and cooperation regime. Watches complementarity determinations for signs of selective enforcement or legitimacy erosion, without directly controlling case-by-case admissibility rulings.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, icc_states_parties_assembly, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_17_complementarity__international_oversight_reading, victims_in_complicit_states).
narrative_ontology:fixing_cost_class(article_17_complementarity__international_oversight_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Backstops accountability for atrocity crimes when domestic legal systems will not or cannot genuinely prosecute their own powerful actors, closing the impunity gap that pure state sovereignty would otherwise leave open.
% TRANSFER_FUNCTION: Moves prosecutorial authority and reputational exposure from a state's domestic institutions to an international court whenever domestic proceedings are judged sham, delayed, or absent — shifting the cost of accountability onto elites and governments who had relied on domestic capture to avoid it, and delivering a path to justice to victims who had none.
% ABSENT_VOICES: Powerful state parties and non-ratifying major powers rarely face the broad admissibility standard this reading applies to weaker states; their governments and militaries are largely outside the room where OTP discretion is exercised, and this asymmetry is the standing objection of Global South commentators and some ICC judges in dissent.
% DISAPPEARANCE_RATIONALE: If this reading were abandoned in favor of a strict national-primacy standard, dozens of pending or plausible situations involving weak or captured domestic judiciaries would become inadmissible on the mere existence of token proceedings; victims currently relying on ICC referral as their only route to any prosecution would lose that route entirely, and the Court's practical caseload would contract sharply toward failed-state and self-referral situations only.
% FOUNDING_PROBLEM: The Rome Statute's framers needed a jurisdictional trigger that would not require states to surrender primary prosecutorial authority outright, while still preventing states from using nominal domestic proceedings to shield perpetrators from any accountability whatsoever.
% FOUNDING_PROBLEM_CORROBORATION: UN Special Rapporteurs on transitional justice, independent NGOs monitoring domestic atrocity prosecutions (e.g. human rights documentation groups with no ICC funding relationship), and dissenting judges within ICC chambers itself have all attested that sham domestic proceedings remain an active tactic in multiple ongoing situations — corroboration from outside the OTP and outside the states benefiting from a narrow reading.
narrative_ontology:disappearance_verdict(article_17_complementarity__international_oversight_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_17_complementarity__international_oversight_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_17_complementarity__international_oversight_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article_17_complementarity__international_oversight_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_17_complementarity__international_oversight_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_17_complementarity__international_oversight_reading_tests).
:- end_tests(article_17_complementarity__international_oversight_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) because the mechanism's cost to targeted elites and governments is real but bounded by enforcement capacity — the ICC has no independent police power and depends on state cooperation it often does not receive. Suppression (0.55) reflects the active diplomatic, evidentiary, and cooperation-compulsion machinery required to make admissibility findings stick against resistant states. Theater ratio is comparatively low (0.28) because the coordination function — preventing atrocity impunity — is substantively pursued in most invoked cases, though a rising trend reflects growing criticism that referral patterns concentrate on weaker states while bypassing powerful ones. Accessibility collapse is moderate (0.4): domestic sham-proceeding tactics remain a live alternative elites can and do attempt, so the constraint has not fully closed off the evasion route it targets.
 *
 * PERSPECTIVAL GAP:
 *   From the OTP's seat, the broad reading is essential coordination infrastructure closing a real impunity gap. From a targeted state government's seat, the same admissibility finding functions as externally imposed extraction of sovereign prosecutorial authority, justified after the fact by a standard the state had no meaningful opportunity to satisfy on its own terms. The engine computes these divergent seat classifications from the structural power/exit data; the claim (tangled_rope) does not resolve the divergence, it names the coexistence of a real coordination function with real asymmetric cost.
 *
 * DIRECTIONALITY LOGIC:
 *   Victims in complicit or failed states are the structural beneficiaries — they had no viable domestic path to prosecution and this reading opens one, so their directionality sits near the beneficiary end. Domestic elites who staged sham proceedings and non-cooperating governments are the structural targets — the broad 'unwilling or unable' standard exists specifically to see through their evasion tactics, so their directionality sits near the target end, amplified by their constrained exit (they cannot simply leave the jurisdiction of international opinion or, eventually, arrest warrants). Powerful state parties are excluded rather than coordinated or targeted — their practical immunity from the standard is not a benefit conferred by the mechanism's logic but an artifact of enforcement asymmetry, which is why they are marked excluded rather than beneficiary.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — that domestic sham proceedings can permanently shield atrocity perpetrators — remains empirically live per outside corroboration (UN rapporteurs, independent monitors, dissenting judges), which prevents this reading from being classified as an atrophied mandate merely running on institutional momentum. However, the concentration of actual referrals on weaker and non-Western states relative to the theoretical global scope of the standard is the specific mandatrophy risk this reading must be watched for: if referral patterns diverge further from the stated universal standard, the coordination claim would increasingly describe an aspiration rather than the mechanism's actual operation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    broad_vs_narrow_admissibility_selection,
    'Is the broad ''unwilling or unable'' standard the historically dominant OTP practice, or is it a contested aspirational reading that OTP practice has only inconsistently applied?',
    'Comparative review of admissibility rulings and OTP preliminary examination reports across all situations since 2002, coded for how strict or lenient the applied threshold actually was relative to the text.',
    'If OTP practice has consistently applied the broad standard, this reading describes actual institutional behavior; if practice has been inconsistent or has drifted toward the narrower standard in politically sensitive cases, the broad reading is more aspirational than operative, and the extractiveness/suppression metrics authored here would need revision toward the sibling reading''s profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(broad_vs_narrow_admissibility_selection, empirical, 'Whether the broad admissibility reading reflects actual OTP practice or an idealized standard.').

omega_variable(
    selective_referral_asymmetry,
    'Does the practical concentration of ICC referrals on weaker states (versus the theoretical universality of the standard) constitute evidence that this reading functions as victor''s-justice enforcement rather than genuine impunity-prevention?',
    'Statistical analysis of situation and case selection against relative state power (military capability, UNSC veto status, non-ratification status), correlated with admissibility outcomes.',
    'Strong correlation would support characterizing this reading''s actual operation as partially extractive along a power axis not captured by the formal legal standard, strengthening the case for treating powerful_state_parties_and_non_parties'' exclusion as a structural feature rather than an incidental gap.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selective_referral_asymmetry, empirical, 'Whether selection effects undermine the reading''s claimed universality.').

omega_variable(
    kernel_reading_indeterminacy,
    'Is the choice between the international-oversight and national-primacy readings of Article 17 resolvable by treaty text alone, or is it an irreducibly contested interpretive commitment that different Chambers and Prosecutors will continue to make differently?',
    'Systematic review of Appeals Chamber jurisprudence on complementarity (e.g. Libya, Kenya, Colombia admissibility rulings) to determine whether a stable doctrinal convergence toward one reading has emerged or whether the text continues to support both readings roughly equally across different benches.',
    'If a doctrinal convergence exists, this reading (or its sibling) may deserve reclassification as the operative rather than merely one-of-two-live readings; if genuinely indeterminate, both readings remain simultaneously live and this story''s classification is properly read as one committed instantiation among coexisting alternatives, not a prediction of doctrinal settlement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Whether the kernel readings are doctrinally converging or remain genuinely indeterminate.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_17_complementarity__international_oversight_reading, 2002, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t2002, article_17_complementarity__international_oversight_reading, theater_ratio, 2002, 0.15).
narrative_ontology:measurement(arti_tr_t2006, article_17_complementarity__international_oversight_reading, theater_ratio, 2006, 0.18).
narrative_ontology:measurement(arti_tr_t2010, article_17_complementarity__international_oversight_reading, theater_ratio, 2010, 0.21).
narrative_ontology:measurement(arti_tr_t2014, article_17_complementarity__international_oversight_reading, theater_ratio, 2014, 0.24).
narrative_ontology:measurement(arti_tr_t2018, article_17_complementarity__international_oversight_reading, theater_ratio, 2018, 0.26).
narrative_ontology:measurement(arti_tr_t2022, article_17_complementarity__international_oversight_reading, theater_ratio, 2022, 0.27).
narrative_ontology:measurement(arti_tr_t2026, article_17_complementarity__international_oversight_reading, theater_ratio, 2026, 0.28).

% Extraction over time
narrative_ontology:measurement(arti_be_t2002, article_17_complementarity__international_oversight_reading, base_extractiveness, 2002, 0.28).
narrative_ontology:measurement(arti_be_t2006, article_17_complementarity__international_oversight_reading, base_extractiveness, 2006, 0.32).
narrative_ontology:measurement(arti_be_t2010, article_17_complementarity__international_oversight_reading, base_extractiveness, 2010, 0.36).
narrative_ontology:measurement(arti_be_t2014, article_17_complementarity__international_oversight_reading, base_extractiveness, 2014, 0.38).
narrative_ontology:measurement(arti_be_t2018, article_17_complementarity__international_oversight_reading, base_extractiveness, 2018, 0.4).
narrative_ontology:measurement(arti_be_t2022, article_17_complementarity__international_oversight_reading, base_extractiveness, 2022, 0.41).
narrative_ontology:measurement(arti_be_t2026, article_17_complementarity__international_oversight_reading, base_extractiveness, 2026, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t2002, article_17_complementarity__international_oversight_reading, suppression_requirement, 2002, 0.35).
narrative_ontology:measurement(arti_su_t2006, article_17_complementarity__international_oversight_reading, suppression_requirement, 2006, 0.4).
narrative_ontology:measurement(arti_su_t2010, article_17_complementarity__international_oversight_reading, suppression_requirement, 2010, 0.46).
narrative_ontology:measurement(arti_su_t2014, article_17_complementarity__international_oversight_reading, suppression_requirement, 2014, 0.49).
narrative_ontology:measurement(arti_su_t2018, article_17_complementarity__international_oversight_reading, suppression_requirement, 2018, 0.52).
narrative_ontology:measurement(arti_su_t2022, article_17_complementarity__international_oversight_reading, suppression_requirement, 2022, 0.54).
narrative_ontology:measurement(arti_su_t2026, article_17_complementarity__international_oversight_reading, suppression_requirement, 2026, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_17_complementarity__international_oversight_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_17_complementarity__international_oversight_reading, national_primacy_reading).

% DUAL FORMULATION NOTE:
% This constraint and national_primacy_reading are two readings of the single article_17_complementarity kernel (the Rome Statute's Article 17 admissibility text). They share the same treaty language but instantiate structurally distinct constraints: this reading applies a low admissibility threshold with an expanded 'unwilling or unable' standard and a victim set including sham-prosecution and elite-immunity scenarios, while national_primacy_reading applies a high threshold with the burden of proof on the Court and a narrower victim set limited to demonstrated total institutional failure. Per the ε-invariance principle, these are authored as separate stories rather than one story with a measurement parameter, and are linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
