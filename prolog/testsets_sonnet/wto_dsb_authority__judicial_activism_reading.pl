% ============================================================================
% CONSTRAINT STORY: wto_dsb_authority__judicial_activism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: wto_dsb_authority__judicial_activism_reading
 *   human_readable: WTO DSB Panels as Illegitimate Judicial Legislation (Judicial Activism Reading)
 *   domain: international_law/trade_governance/institutional_legitimacy
 *
 * SUMMARY:
 *   This story instantiates the judicial-activism reading of the contested
 *   WTO DSB authority kernel: the claim that DSB panels and the Appellate
 *   Body have, over three decades, progressively created obligations the
 *   negotiated treaty text does not support, converting a dispute-resolution
 *   mechanism into a quasi-legislative body no member state consented to.
 *   Under this reading, the 2019 US-driven paralysis of the Appellate Body
 *   appointment process was not obstruction of a legitimate court but a
 *   legitimate act of resistance against an institution that had exceeded its
 *   mandate. This is a DISTINCT constraint from the binding_referee_reading
 *   (which holds the same rulings are legitimate treaty-grounded obligations)
 *   and the advisory_coordination_reading (which holds panels never bound
 *   anyone in the first place) — the three readings share a kernel (the DSB's
 *   authority) but diverge on whether that authority is legitimate, and thus
 *   emit structurally different constraints with different victim sets and
 *   different ε. Extraction here rises steadily across the interval as case
 *   law accretes; the post-2019 dip in suppression_requirement and
 *   theater_ratio reflects functional paralysis of the mechanism, not
 *   resolution of the underlying legitimacy dispute.
 *
 * KEY AGENTS:
 *   - dsb_appellate_body_institutional_apparatus: primary agenda-setter, institutional/arbitrage — drives interpretive expansion and bears little cost from it
 *   - complainant_states_with_litigation_capacity: primary beneficiary, powerful/mobile — exploits interpretive ambiguity strategically
 *   - respondent_states_with_domestic_policy_autonomy_claims: primary target, powerful/constrained — absorbs compliance costs for policies negotiators never clearly prohibited
 *   - developing_country_members_with_limited_legal_capacity: secondary target, powerless/trapped — bears drift costs without capacity to resist or shape it
 *   - excluded_negotiating_membership: the voice this reading centers — bypassed by adjudicative interpretation despite holding the treaty's own designated interpretive authority under Article IX:2
 *   - trade_law_scholars_and_treaty_negotiators: analytical observer — documents the gap between Vienna Convention interpretive method and actual panel reasoning
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wto_dsb_authority__judicial_activism_reading, 0.68).
domain_priors:suppression_score(wto_dsb_authority__judicial_activism_reading, 0.55).
domain_priors:theater_ratio(wto_dsb_authority__judicial_activism_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, resistance, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wto_dsb_authority__judicial_activism_reading, snare).
narrative_ontology:human_readable(wto_dsb_authority__judicial_activism_reading, "WTO DSB Panels as Illegitimate Judicial Legislation (Judicial Activism Reading)").
narrative_ontology:topic_domain(wto_dsb_authority__judicial_activism_reading, "international_law/trade_governance/institutional_legitimacy").

domain_priors:requires_active_enforcement(wto_dsb_authority__judicial_activism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(wto_dsb_authority__judicial_activism_reading, '6ff49d45-bed1-448b-9c51-5485f244c60b').
narrative_ontology:cs_kernel_codification('6ff49d45-bed1-448b-9c51-5485f244c60b', fixed_text).
narrative_ontology:cs_authority_grounding('6ff49d45-bed1-448b-9c51-5485f244c60b', extraction).
narrative_ontology:cs_interpretation_layer_present('6ff49d45-bed1-448b-9c51-5485f244c60b').
narrative_ontology:cs_reading_relation('6ff49d45-bed1-448b-9c51-5485f244c60b', wto_dsb_authority__binding_referee_reading, forecloses).
narrative_ontology:cs_reading_relation('6ff49d45-bed1-448b-9c51-5485f244c60b', wto_dsb_authority__advisory_coordination_reading, coexists_with).
narrative_ontology:cs_axiom('6ff49d45-bed1-448b-9c51-5485f244c60b', foundational, adjudicative_interpretation_cannot_substitute_for_article_ix2_consensus).
narrative_ontology:cs_axiom_status(adjudicative_interpretation_cannot_substitute_for_article_ix2_consensus, holdable).
narrative_ontology:cs_axiom_grounding('6ff49d45-bed1-448b-9c51-5485f244c60b', adjudicative_interpretation_cannot_substitute_for_article_ix2_consensus, conventional).
narrative_ontology:cs_axiom('6ff49d45-bed1-448b-9c51-5485f244c60b', secondary, unilateral_appointment_blocking_is_legitimate_institutional_check).
narrative_ontology:cs_axiom_status(unilateral_appointment_blocking_is_legitimate_institutional_check, holdable).
narrative_ontology:cs_axiom_grounding('6ff49d45-bed1-448b-9c51-5485f244c60b', unilateral_appointment_blocking_is_legitimate_institutional_check, instrumental).
narrative_ontology:cs_reference_frame('6ff49d45-bed1-448b-9c51-5485f244c60b', uruguay_round_negotiated_bargain).
narrative_ontology:cs_drift_state('6ff49d45-bed1-448b-9c51-5485f244c60b', post_appellate_body_paralysis_2024, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('6ff49d45-bed1-448b-9c51-5485f244c60b', '').
narrative_ontology:cs_kernel_id(wto_dsb_authority__judicial_activism_reading, wto_dsb_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wto_dsb_authority__judicial_activism_reading, complainant_states_with_litigation_capacity).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__judicial_activism_reading, dsb_appellate_body_institutional_apparatus).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__judicial_activism_reading, trade_law_practitioner_class).
narrative_ontology:constraint_victim(wto_dsb_authority__judicial_activism_reading, respondent_states_with_domestic_policy_autonomy_claims).
narrative_ontology:constraint_victim(wto_dsb_authority__judicial_activism_reading, developing_country_members_with_limited_legal_capacity).
narrative_ontology:constraint_victim(wto_dsb_authority__judicial_activism_reading, domestic_regulatory_agencies_overridden_by_panel_findings).
narrative_ontology:constraint_vindicates(wto_dsb_authority__judicial_activism_reading, textualist_treaty_interpretation_doctrine).
narrative_ontology:constraint_vindicates(wto_dsb_authority__judicial_activism_reading, member_driven_organization_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues rulings interpreting covered agreements, including on questions the treaty text does not explicitly resolve. Under this reading, the panels and Appellate Body have progressively read obligations into ambiguous or silent treaty provisions (e.g., on subsidies, standards-setting, zeroing methodologies) that were never negotiated by the membership. The apparatus perpetuates itself through case law accretion — each ruling becomes precedent-like even though the DSU disclaims stare decisis, and the institution's authority grows with each unresisted ruling. It bears no direct cost when a ruling is later seen as overreach; reputational risk is diffuse and slow-moving.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, dsb_appellate_body_institutional_apparatus, agenda_setter,
    institutional, generational, arbitrage, global).

% States and blocs with deep legal budgets (the ability to hire specialized trade counsel, file amicus submissions, and litigate repeatedly) can exploit interpretive drift strategically — bringing claims that stretch panels toward favorable new readings of ambiguous text. They benefit from an activist DSB because it lets them win through litigation what they could not win at the negotiating table. Their exit option is real: they can walk away from any given case without much cost, and continue using the system opportunistically.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, complainant_states_with_litigation_capacity, beneficiary,
    powerful, biographical, mobile, global).

% Face adverse rulings requiring them to withdraw domestic measures (environmental standards, safeguard tariffs, subsidy programs) that were adopted through domestic democratic processes and, in this reading, never clearly prohibited by the negotiated text. Compliance means unwinding sovereign policy choices under threat of authorized retaliation; non-compliance means absorbing sanctioned trade retaliation. Exit from any single ruling is possible only by accepting reputational and retaliatory costs, and full exit from the system (leaving the WTO) is not realistically available given trade dependence.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, respondent_states_with_domestic_policy_autonomy_claims, payer,
    powerful, biographical, constrained, national).

% Lack the legal budgets to litigate defensively or to shape interpretive drift in their favor; when panels expand obligations through interpretation, these states absorb the resulting compliance burden without having had a realistic voice in the case law that created it. They cannot afford sustained non-compliance (retaliation from larger partners is asymmetrically damaging) and cannot afford to litigate proactively to resist drift. Their only real exit is disengagement from dispute settlement altogether, which forfeits protection when they are wronged.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, developing_country_members_with_limited_legal_capacity, payer,
    powerless, generational, trapped, national).

% National regulators who designed standards (food safety, environmental, technical) within what they understood as their sovereign discretion find those standards invalidated by panel interpretations of obligations the text does not explicitly state. They have no standing before the DSB itself and no direct recourse; their only channel is through their government's litigation posture, which they do not control.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, domestic_regulatory_agencies_overridden_by_panel_findings, payer,
    moderate, biographical, trapped, national).

% Specialist counsel, academics, and consultants whose careers and billings depend on the complexity and volume of DSB jurisprudence. Interpretive drift generates more litigable ambiguity, more precedent to parse, and more demand for their expertise. They have every incentive to defend the legitimacy of an expansive reading of panel authority regardless of whether it tracks the original treaty bargain.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, trade_law_practitioner_class, beneficiary,
    organized, biographical, arbitrage, global).

% The broader WTO membership that negotiated the original covered agreements (and that must unanimously agree to authoritative interpretations under Article IX:2 of the WTO Agreement) has effectively been bypassed: panels create de facto authoritative interpretations through adjudication without the political consensus process the treaty specifies for genuine interpretive change. Would object that interpretation-by-litigation circumvents the membership's own designated amendment and interpretation channel, but has no forum to raise this except by blocking Appellate Body appointments — which is what a subset of the membership (the US) in fact did.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, excluded_negotiating_membership, excluded,
    organized, generational, constrained, global).

% Assess whether specific panel and Appellate Body rulings track the Vienna Convention's interpretive rules (ordinary meaning, context, object and purpose) or exceed them by importing obligations the negotiating history does not support. Their scholarship documents the doctrinal drift that member states cite when justifying withdrawal from or paralysis of the enforcement mechanism.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, trade_law_scholars_and_treaty_negotiators, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(wto_dsb_authority__judicial_activism_reading, diffuse).
narrative_ontology:fixing_cost_class(wto_dsb_authority__judicial_activism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In principle, a rules-based dispute mechanism solves the real coordination problem of adjudicating trade disputes without resort to unilateral retaliation or power-based bargaining — a genuine public good if panels stay within the negotiated mandate.
% TRANSFER_FUNCTION: Moves domestic policy discretion from respondent states (and their regulatory agencies) to the DSB's interpretive apparatus and to complainant states able to exploit favorable rulings; moves litigation and compliance costs disproportionately onto states with limited legal capacity, and channels institutional legitimacy and specialist fees toward the practitioner class that depends on continued case volume.
% ABSENT_VOICES: The negotiating membership as a whole — which under Article IX:2 is the only body with authority to adopt binding interpretations — has no seat in individual disputes and no mechanism to correct interpretive drift except blocking appointments or walking away from enforcement, both of which are blunt, system-damaging instruments rather than targeted correction.
% DISAPPEARANCE_RATIONALE: If the DSB's adjudicative authority were withdrawn (as has functionally happened through Appellate Body paralysis since 2019), member states revert to unilateral retaliation, bilateral negotiation, and self-judged compliance — precisely what has occurred: filings shifted toward non-binding panels members can appeal 'into the void,' plurilateral arbitration arrangements emerged (MPIA), and enforcement lost its automatic teeth. The world has already partly rearranged around this reading's central claim.
% FOUNDING_PROBLEM: The Uruguay Round negotiators sought a rules-based alternative to the GATT-era practice where powerful states could block panel adoption and enforcement was politically contingent; the DSU was built to make rulings automatically adopted and legally binding, closing loopholes that had let non-compliance persist indefinitely.
% FOUNDING_PROBLEM_CORROBORATION: The original problem (blockable, non-binding GATT panels) is attested as solved by neutral trade-law historians studying the Uruguay Round negotiating record. Whether the SOLUTION itself has metastasized into a new problem — panels manufacturing obligations beyond the bargain — is attested from outside the beneficiary set by the US Trade Representative's institutional critique (spanning multiple administrations, not one), by academic treaty-interpretation scholars documenting specific instances of interpretation exceeding Vienna Convention bounds (e.g. on 'zeroing' and public body determinations), and by the negotiating membership's own Article IX:2 mechanism being the specified — and bypassed — channel for legitimate interpretive change.
narrative_ontology:disappearance_verdict(wto_dsb_authority__judicial_activism_reading, world_rearranges).
narrative_ontology:founding_problem_status(wto_dsb_authority__judicial_activism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(wto_dsb_authority__judicial_activism_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.68) and resistance (0.81) are both authored high because, under this reading, the mechanism's core legitimacy claim is contested at the root: it is not merely that specific rulings are unpopular, but that the ADJUDICATIVE PROCESS ITSELF is read as manufacturing obligations outside the treaty bargain, which is why resistance takes the extreme institutional form of blocking appointments rather than ordinary non-compliance. Accessibility_collapse is comparatively low (0.38) because, unlike a mountain, alternatives to the DSB (bilateral retaliation, plurilateral arbitration like the MPIA, unilateral Section 301-style action) remain visibly available and are being actively used — the constraint has not foreclosed exit, which is precisely the structural signature that distinguishes this reading from a legitimate binding referee (where exit would be treaty violation, not principled alternative-seeking). Theater_ratio climbs through 2019 as the institution increasingly performs continuity (issuing 'reports into the void,' maintaining docket activity) after functional paralysis, then dips slightly post-2019 as some performative activity is replaced by genuine plurilateral workarounds.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda_setter seat (the DSB apparatus), each individual ruling is a good-faith application of interpretive method to a genuine ambiguity in the text — the institution does not experience itself as legislating. From the payer seats, especially the excluded negotiating membership, the same accretion of rulings is experienced as a slow-motion transfer of interpretive authority from the treaty's designated amendment process (unanimous Article IX:2 interpretation) to an adjudicative body that faces no equivalent political check. The engine should compute these seats as diverging sharply in effective extraction despite reading the identical rulings.
 *
 * DIRECTIONALITY LOGIC:
 *   Complainant states with deep litigation capacity and the specialist practitioner class sit near the beneficiary end: they gain from interpretive expansion regardless of whether it tracks the original bargain, and they have real exit (arbitrage — use the system when favorable, route around it when not). Respondent states and, more severely, developing-country members and domestic regulatory agencies sit near the target end: they bear compliance costs for obligations this reading holds were manufactured rather than negotiated, and their exit options range from constrained (powerful respondents can absorb retaliation) to trapped (regulatory agencies with no standing at all, and powerless states with no litigation budget).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (unenforceable, blockable GATT-era panels) is genuinely dead — the DSU succeeded at solving it. But under this reading, the SOLUTION's mandate (adjudicate disputes under the negotiated text) has been quietly substituted for a different, unmandated function (expand the text's coverage through interpretation), and the institution continues to draw on the legitimacy of having solved the original problem to defend an activity the original problem never authorized. This is a classic mandatrophy signature: the founding_problem_status is contested precisely because the institution's defenders point to the solved original problem while its critics point to the new, unauthorized function riding on that solved problem's legitimacy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretive_drift_vs_gap_filling,
    'Do the specific panel rulings this reading points to (zeroing methodology, public body determinations, ''as such'' claims, non-violation nullification expansions) constitute genuine interpretive drift beyond the negotiated text, or legitimate gap-filling within a reasonable range of Vienna Convention interpretive method applied to ambiguous treaty language?',
    'Systematic comparison of specific disputed rulings against the negotiating history (Uruguay Round drafting records) and against Vienna Convention Articles 31-32 interpretive canons, ideally conducted by scholars with no institutional stake in either DSB legitimacy or member-state sovereignty claims.',
    'If the rulings are within a defensible interpretive range, this reading''s core premise collapses and the constraint more closely resembles the binding_referee_reading; if the rulings systematically exceed defensible interpretation, the judicial_activism_reading''s classification as extractive/illegitimate is well-grounded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_drift_vs_gap_filling, conceptual, 'Whether the specific interpretive moves at issue are gap-filling or genuine mandate exceedance.').

omega_variable(
    kernel_reading_selection_basis,
    'Which of the three sibling readings of DSB authority (judicial_activism, binding_referee, advisory_coordination) best describes the DSU''s actual design intent, and is the choice among them itself contested along predictable institutional lines (US and some sovereignty-focused states favor activism-reading critiques; EU and litigation-dependent exporters favor binding-referee reading)?',
    'Comparative analysis of ratification-era statements by negotiating parties, contemporaneous legal scholarship from 1994-1995, and correlation between a state''s structural position (frequent complainant vs. frequent respondent) and which reading it publicly advances.',
    'If reading selection correlates strongly with litigation posture rather than principled treaty interpretation, this suggests the kernel dispute is itself partly strategic rather than purely interpretive — which would not change this story''s ε but would inform how much weight to give the reading''s self-presentation as principled resistance versus convenient cover for non-compliance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_basis, conceptual, 'Whether reading choice tracks structural litigation position rather than neutral treaty interpretation.').

omega_variable(
    appointment_blocking_legitimacy,
    'Is blocking Appellate Body appointments (the US practice since 2017, causing full paralysis since December 2019) a legitimate exercise of member-state authority to check institutional overreach, or is it itself an act of treaty violation that undermines a mechanism the blocking state agreed to when joining the WTO?',
    'Legal analysis of whether DSU Article 17 appointment procedures create an affirmative obligation to appoint, versus discretion that can be exercised as a check; and empirical tracking of whether the blocking state''s stated legitimacy concerns are addressed by the 2022 Multi-Party Interim Appeal Arbitration Arrangement (MPIA) or whether blocking continues regardless.',
    'If blocking is found to be itself a treaty violation, the resistance this reading treats as principled becomes an additional extractive act rather than a corrective one — meaningfully changing the moral valence (though not necessarily the ε) of the excluded_negotiating_membership''s position.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(appointment_blocking_legitimacy, empirical, 'Whether Appellate Body appointment-blocking is itself legitimate or a further treaty breach.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wto_dsb_authority__judicial_activism_reading, 1995, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wto__tr_t1995, wto_dsb_authority__judicial_activism_reading, theater_ratio, 1995, 0.1).
narrative_ontology:measurement_basis(wto__tr_t1995, observed).
narrative_ontology:measurement(wto__tr_t2000, wto_dsb_authority__judicial_activism_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement_basis(wto__tr_t2000, observed).
narrative_ontology:measurement(wto__tr_t2005, wto_dsb_authority__judicial_activism_reading, theater_ratio, 2005, 0.2).
narrative_ontology:measurement_basis(wto__tr_t2005, observed).
narrative_ontology:measurement(wto__tr_t2010, wto_dsb_authority__judicial_activism_reading, theater_ratio, 2010, 0.28).
narrative_ontology:measurement_basis(wto__tr_t2010, observed).
narrative_ontology:measurement(wto__tr_t2015, wto_dsb_authority__judicial_activism_reading, theater_ratio, 2015, 0.35).
narrative_ontology:measurement_basis(wto__tr_t2015, observed).
narrative_ontology:measurement(wto__tr_t2019, wto_dsb_authority__judicial_activism_reading, theater_ratio, 2019, 0.45).
narrative_ontology:measurement_basis(wto__tr_t2019, observed).
narrative_ontology:measurement(wto__tr_t2024, wto_dsb_authority__judicial_activism_reading, theater_ratio, 2024, 0.42).
narrative_ontology:measurement_basis(wto__tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(wto__be_t1995, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 1995, 0.28).
narrative_ontology:measurement_basis(wto__be_t1995, observed).
narrative_ontology:measurement(wto__be_t2000, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 2000, 0.35).
narrative_ontology:measurement_basis(wto__be_t2000, observed).
narrative_ontology:measurement(wto__be_t2005, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 2005, 0.44).
narrative_ontology:measurement_basis(wto__be_t2005, observed).
narrative_ontology:measurement(wto__be_t2010, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 2010, 0.52).
narrative_ontology:measurement_basis(wto__be_t2010, observed).
narrative_ontology:measurement(wto__be_t2015, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 2015, 0.6).
narrative_ontology:measurement_basis(wto__be_t2015, observed).
narrative_ontology:measurement(wto__be_t2019, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 2019, 0.66).
narrative_ontology:measurement_basis(wto__be_t2019, observed).
narrative_ontology:measurement(wto__be_t2024, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 2024, 0.68).
narrative_ontology:measurement_basis(wto__be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(wto__su_t1995, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 1995, 0.35).
narrative_ontology:measurement_basis(wto__su_t1995, observed).
narrative_ontology:measurement(wto__su_t2000, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 2000, 0.42).
narrative_ontology:measurement_basis(wto__su_t2000, observed).
narrative_ontology:measurement(wto__su_t2005, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 2005, 0.48).
narrative_ontology:measurement_basis(wto__su_t2005, observed).
narrative_ontology:measurement(wto__su_t2010, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 2010, 0.55).
narrative_ontology:measurement_basis(wto__su_t2010, observed).
narrative_ontology:measurement(wto__su_t2015, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 2015, 0.6).
narrative_ontology:measurement_basis(wto__su_t2015, observed).
narrative_ontology:measurement(wto__su_t2019, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 2019, 0.7).
narrative_ontology:measurement_basis(wto__su_t2019, observed).
narrative_ontology:measurement(wto__su_t2024, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 2024, 0.55).
narrative_ontology:measurement_basis(wto__su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wto_dsb_authority__judicial_activism_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(wto_dsb_authority__judicial_activism_reading, wto_dsb_authority__binding_referee_reading).
narrative_ontology:affects_constraint(wto_dsb_authority__judicial_activism_reading, wto_dsb_authority__advisory_coordination_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints instantiating the contested wto_dsb_authority kernel. binding_referee_reading treats DSB rulings as legitimate treaty-grounded obligations (member states surrendered discretion; non-compliance is the extractive act). advisory_coordination_reading treats panels as facilitative and non-binding (no obligations were ever created; the entire legitimacy dispute dissolves). This story (judicial_activism_reading) treats the rulings themselves as the extractive mechanism — illegitimate obligation-creation via interpretive drift, with appointment-blocking as principled resistance. Each carries its own ε, its own beneficiary/victim structure, and its own claimed_type; they are linked via network edges rather than merged, per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
