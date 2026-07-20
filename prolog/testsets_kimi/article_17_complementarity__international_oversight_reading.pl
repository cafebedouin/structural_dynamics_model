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
    narrative_ontology:affects_constraint/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Article 17 Complementarity â International Oversight Reading
 *   domain: international_law/criminal_justice/state_sovereignty
 *
 * SUMMARY:
 *   This constraint instantiates the international_oversight_reading of the
 *   Article 17 complementarity kernel in the Rome Statute. Under this
 *   reading, the ICC acts as a guardian against impunity by interpreting the
 *   'unwilling or unable' standard broadly, setting a low admissibility
 *   threshold that triggers international jurisdiction when domestic
 *   proceedings lack independence, genuine intent, or adequate scope. The
 *   constraint coordinates accountability for atrocity-crime victims in
 *   failed or complicit states while extracting sovereignty costs from
 *   affected governments and criminal-process costs from elites under
 *   scrutiny. It is actively enforced through ICC Pre-Trial Chamber
 *   admissibility determinations, Prosecutor-initiated investigations, and
 *   state-cooperation demands. The structural delta from the
 *   national_primacy_reading is sharp: national courts are presumed
 *   inadequate until proven otherwise, the victim set expands to cover
 *   symbolic prosecutions, and state cooperation is intensified rather than
 *   deferential.
 *
 * KEY AGENTS:
 *   - icc_prosecutor_and_chambers: Agenda-setter (institutional/analytical) â interprets complementarity broadly and enforces admissibility
 *   - victims_in_failed_states: Primary beneficiary (powerless/trapped) â gains access to justice when domestic courts fail
 *   - state_executives_under_scrutiny: Primary target (powerful/constrained) â bears individual liability risk under broad ICC jurisdiction
 *   - affected_state_governments: Secondary target (institutional/constrained) â loses sovereignty over domestic criminal proceedings
 *   - international_human_rights_ngos: Secondary beneficiary (organized/mobile) â gains operational space and mission validation
 *   - national_judiciaries_in_affected_states: Excluded voice (institutional/constrained) â asserts genuine capacity but is overridden by low admissibility threshold
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_17_complementarity__international_oversight_reading, 0.62).
domain_priors:suppression_score(article_17_complementarity__international_oversight_reading, 0.58).
domain_priors:theater_ratio(article_17_complementarity__international_oversight_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_17_complementarity__international_oversight_reading, tangled_rope).
narrative_ontology:human_readable(article_17_complementarity__international_oversight_reading, "Article 17 Complementarity â International Oversight Reading").
narrative_ontology:topic_domain(article_17_complementarity__international_oversight_reading, "international_law/criminal_justice/state_sovereignty").

domain_priors:requires_active_enforcement(article_17_complementarity__international_oversight_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_17_complementarity__international_oversight_reading, 'a4efc28c-e209-42e5-8be2-a4dd548d3db9').
narrative_ontology:cs_kernel_codification('a4efc28c-e209-42e5-8be2-a4dd548d3db9', fixed_text).
narrative_ontology:cs_authority_grounding('a4efc28c-e209-42e5-8be2-a4dd548d3db9', lineage).
narrative_ontology:cs_interpretation_layer_present('a4efc28c-e209-42e5-8be2-a4dd548d3db9').
narrative_ontology:cs_reading_relation('a4efc28c-e209-42e5-8be2-a4dd548d3db9', article_17_complementarity__national_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('a4efc28c-e209-42e5-8be2-a4dd548d3db9', foundational, supranational_guardianship_over_domestic_proceedings).
narrative_ontology:cs_axiom_status(supranational_guardianship_over_domestic_proceedings, holdable).
narrative_ontology:cs_axiom_grounding('a4efc28c-e209-42e5-8be2-a4dd548d3db9', supranational_guardianship_over_domestic_proceedings, conventional).
narrative_ontology:cs_axiom('a4efc28c-e209-42e5-8be2-a4dd548d3db9', foundational, low_admissibility_threshold_default).
narrative_ontology:cs_axiom_status(low_admissibility_threshold_default, holdable).
narrative_ontology:cs_axiom_grounding('a4efc28c-e209-42e5-8be2-a4dd548d3db9', low_admissibility_threshold_default, conventional).
narrative_ontology:cs_reference_frame('a4efc28c-e209-42e5-8be2-a4dd548d3db9', international_guardianship_against_impunity).
narrative_ontology:cs_drift_state('a4efc28c-e209-42e5-8be2-a4dd548d3db9', contemporary_geopolitical_resistance_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a4efc28c-e209-42e5-8be2-a4dd548d3db9', '').
narrative_ontology:cs_kernel_id(article_17_complementarity__international_oversight_reading, article_17_complementarity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_17_complementarity__international_oversight_reading, victims_in_failed_states).
narrative_ontology:constraint_beneficiary(article_17_complementarity__international_oversight_reading, international_human_rights_ngos).
narrative_ontology:constraint_victim(article_17_complementarity__international_oversight_reading, state_executives_under_scrutiny).
narrative_ontology:constraint_victim(article_17_complementarity__international_oversight_reading, affected_state_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets Article 17's 'unwilling or unable' standard broadly to trigger ICC jurisdiction over domestic proceedings deemed lacking independence, genuine intent, or adequate scope. Issues admissibility rulings that override national claims of sufficiency and demands state cooperation with investigations and surrender requests.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, icc_prosecutor_and_chambers, agenda_setter,
    institutional, generational, analytical, global).

% Depend on ICC intervention when domestic courts are captured, complicit, or non-existent. Cannot compel the Prosecutor to open an investigation and have no alternative justice mechanism if the Court declines admissibility. Receive subsidized access to international criminal process when the low-threshold reading is activated.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, victims_in_failed_states, beneficiary,
    powerless, biographical, trapped, national).

% Face individual criminal liability before the ICC even where domestic proceedings exist, if those proceedings are deemed shielding or symbolic. Rely on state non-cooperation, diplomatic immunity, or statutory limitation arguments to avoid surrender. Bear the direct liberty and reputational costs of the broad admissibility standard.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, state_executives_under_scrutiny, payer,
    powerful, biographical, constrained, national).

% Lose default sovereignty over criminal proceedings when ICC Pre-Trial Chambers find national systems inadequate. Must divert diplomatic and legal resources to cooperation or face ASP censure and international pressure. Withdrawal from the Rome Statute is legally possible but carries significant diplomatic costs.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, affected_state_governments, payer,
    institutional, generational, constrained, national).

% Advance organizational mission through broad ICC interventions. Submit amicus observations, represent victim interests, and legitimize the low-threshold reading. Benefit from institutional growth, funding, and normative traction when the Court is actively asserting oversight jurisdiction.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, international_human_rights_ngos, beneficiary,
    organized, generational, mobile, global).

% Domestic courts that assert genuine capacity and willingness to prosecute atrocity crimes but are overridden by ICC admissibility determinations under the broad 'unwilling or unable' standard. Their rulings and proceedings are preempted, and their objections to ICC intervention are structurally sidelined in the admissibility calculus.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, national_judiciaries_in_affected_states, excluded,
    institutional, generational, constrained, national).

narrative_ontology:fixing_cost_class(article_17_complementarity__international_oversight_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a backstop criminal justice mechanism when national courts are unwilling or unable to genuinely prosecute atrocity crimes, ensuring accountability does not default to impunity in failed or complicit states.
% TRANSFER_FUNCTION: Transfers prosecutorial authority and legitimacy from national courts to the ICC when domestic proceedings are deemed inadequate; transfers cooperation burdens, sovereignty costs, and individual liability risk from affected states and their elites to the Rome Statute system.
% ABSENT_VOICES: National judiciaries in affected states asserting genuine capacity, state governments claiming sovereign primacy over criminal law, and accused elites advancing immunity defenses are structurally overridden by the low admissibility threshold; the African Union's political opposition is heard in the Assembly of States Parties but does not alter the legal operation of the broad complementarity standard.
% DISAPPEARANCE_RATIONALE: Without the broad oversight reading, the ICC would revert to a deferential posture toward national courts; victims in states with sham proceedings would lose their international backstop; affected governments would reclaim presumptive jurisdiction; and the international impunity gap would widen for elites shielded by domestic institutions.
% FOUNDING_PROBLEM: The Nuremberg and Tokyo legacy and the post-Cold War impunity gap: atrocity crimes committed by state agents or with state acquiescence going unpunished because national courts were complicit, non-existent, or lacked political will to prosecute leaders.
% FOUNDING_PROBLEM_CORROBORATION: Victims' groups and international human rights NGOs corroborate that impunity persists in many states. The African Union and several withdrawing or non-cooperating states corroborate that the problem definition has been weaponized against weak states; no neutral international body attests the founding problem remains unaltered in contemporary application.
narrative_ontology:disappearance_verdict(article_17_complementarity__international_oversight_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_17_complementarity__international_oversight_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_17_complementarity__international_oversight_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article_17_complementarity__international_oversight_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_17_complementarity__international_oversight_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_17_complementarity__international_oversight_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_17_complementarity__international_oversight_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_17_complementarity__international_oversight_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects the substantial sovereignty transfer from national to international institutions under a low admissibility threshold, coupled with the risk of selective application against weak-state elites. Suppression (0.58) captures active cooperation demands and diplomatic pressure on states, moderated by significant resistance (0.72) from the African Union, withdrawing states, and non-cooperating regimes. Theater_ratio (0.40) registers the growing gap between the ICC's interventionist rhetoric and its actual conviction rate, as well as the performative aspect of complementarity assessments that functionally prejudge national inadequacy. Accessibility_collapse (0.48) is partial: national courts still exist but their authority is structurally undermined by the presumption of ICC superiority under this reading.
 *
 * PERSPECTIVAL GAP:
 *   The victim seat experiences this constraint as protective coordination, while the affected-state seats experience it as sovereignty extraction. The ICC agenda-setter seat experiences it as legitimate legal interpretation. The engine computes these divergences from the structural data: identical formal rules produce opposite effective extraction depending on power, exit options, and beneficiary status. The metrics are authored independently of the claimed type to preserve this divergence as measurable signal.
 *
 * DIRECTIONALITY LOGIC:
 *   Victims_in_failed_states are structural beneficiaries (d near 0.0): the constraint subsidizes their access to justice and dampens effective extraction. State_executives_under_scrutiny and affected_state_governments are structural targets (d near 1.0): the constraint extracts sovereignty and liberty, amplified by constrained exit options. International_human_rights_ngos are near-beneficiaries (low d) through mission alignment. The ICC prosecutor and chambers sit at low-to-moderate d: they administer the constraint and gain institutional authority, though they also bear political backlash costs. No override is needed because the structural derivation captures the relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the ICC as pure extraction by preserving the genuine coordination function (impunity prevention for victims in failed states), which is why the claimed type is tangled_rope rather than snare. Conversely, it prevents mislabeling as pure coordination by requiring victim declarations for state actors and acknowledging the sovereignty costs and selectivity risks. If the founding problem (impunity gap) were dead and the constraint persisted purely as a sovereignty-redistribution mechanism, it would drift toward snare or piton; the contested founding_problem_status and elevated theater_ratio keep it in tangled_rope territory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Does the Rome Statute''s Article 17 structurally encode the international oversight reading or the national primacy reading as its default interpretation?',
    'Systematic analysis of travaux prÃ©paratoires and subsequent state practice; comparison of admissibility rulings to determine which reading dominates ICC jurisprudence.',
    'If the text structurally favors national primacy, this reading is an expansive judicial construction and its extraction is higher than warranted by the kernel. If the text is genuinely ambiguous, the measured extraction is the price of interpretive coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which reading the kernel text structurally defaults to.').

omega_variable(
    victors_justice_capture_risk,
    'Does the broad ''unwilling or unable'' standard actually prevent victor''s justice and elite immunity, or does it enable selective targeting of weak-state elites while powerful-state actors remain outside ICC reach?',
    'Comparative empirical analysis of ICC indictments by state power level and geopolitical alignment; assessment of complementarity assessments in situations involving powerful non-party states.',
    'If selectivity tracks geopolitical power, the constraint''s victim set is asymmetrically loaded toward weak states, and the claimed coordination function is partially cover for enforcement arbitrage.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victors_justice_capture_risk, empirical, 'Whether the reading avoids or replicates victor''s justice.').

omega_variable(
    state_cooperation_enforcement_gap,
    'Can the broad oversight reading function without coercive state cooperation, or does its effectiveness depend on enforcement mechanisms that themselves extract from state sovereignty?',
    'Track conviction and trial completion rates against state cooperation levels; measure correlation between ASP pressure tactics and domestic legal autonomy erosion.',
    'If effectiveness requires high coercion, the coordination-extraction balance shifts toward extraction. If the reading functions through voluntary cooperation, it remains primarily coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_cooperation_enforcement_gap, empirical, 'Whether the reading depends on coercive enforcement to operate.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_17_complementarity__international_oversight_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_17_complementarity__international_oversight_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(arti_tr_t4, article_17_complementarity__international_oversight_reading, theater_ratio, 4, 0.3).
narrative_ontology:measurement(arti_tr_t8, article_17_complementarity__international_oversight_reading, theater_ratio, 8, 0.42).
narrative_ontology:measurement(arti_tr_t12, article_17_complementarity__international_oversight_reading, theater_ratio, 12, 0.5).
narrative_ontology:measurement(arti_tr_t16, article_17_complementarity__international_oversight_reading, theater_ratio, 16, 0.48).
narrative_ontology:measurement(arti_tr_t20, article_17_complementarity__international_oversight_reading, theater_ratio, 20, 0.45).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_17_complementarity__international_oversight_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(arti_be_t4, article_17_complementarity__international_oversight_reading, base_extractiveness, 4, 0.5).
narrative_ontology:measurement(arti_be_t8, article_17_complementarity__international_oversight_reading, base_extractiveness, 8, 0.62).
narrative_ontology:measurement(arti_be_t12, article_17_complementarity__international_oversight_reading, base_extractiveness, 12, 0.68).
narrative_ontology:measurement(arti_be_t16, article_17_complementarity__international_oversight_reading, base_extractiveness, 16, 0.65).
narrative_ontology:measurement(arti_be_t20, article_17_complementarity__international_oversight_reading, base_extractiveness, 20, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_17_complementarity__international_oversight_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(arti_su_t4, article_17_complementarity__international_oversight_reading, suppression_requirement, 4, 0.48).
narrative_ontology:measurement(arti_su_t8, article_17_complementarity__international_oversight_reading, suppression_requirement, 8, 0.62).
narrative_ontology:measurement(arti_su_t12, article_17_complementarity__international_oversight_reading, suppression_requirement, 12, 0.7).
narrative_ontology:measurement(arti_su_t16, article_17_complementarity__international_oversight_reading, suppression_requirement, 16, 0.68).
narrative_ontology:measurement(arti_su_t20, article_17_complementarity__international_oversight_reading, suppression_requirement, 20, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(article_17_complementarity__international_oversight_reading, national_primacy_reading).

% DUAL FORMULATION NOTE:
% This constraint and national_primacy_reading are two structurally distinct readings of the same Article 17 kernel. Their epsilon values, beneficiary/victim structures, and directionalities differ because they interpret the same legal text to assign opposite default presumptions (national adequacy vs. international oversight). They form a constraint family linked by interpretive coupling.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
