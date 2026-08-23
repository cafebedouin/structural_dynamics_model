% ============================================================================
% CONSTRAINT STORY: humane_treatment_standard__absolute_prohibition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_humane_treatment_standard__absolute_prohibition, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: humane_treatment_standard__absolute_prohibition
 *   human_readable: Absolute Prohibition of Torture and Degrading Treatment under Common Article 3
 *   domain: international_humanitarian_law/human_rights
 *
 * SUMMARY:
 *   This constraint story instantiates the absolute_prohibition reading of
 *   the humane_treatment_standard kernel. Common Article 3 of the Geneva
 *   Conventions establishes that 'persons taking no active part in the
 *   hostilities... shall in all circumstances be treated humanely' and
 *   explicitly prohibits 'violence to life and person, in particular murder
 *   of all kinds, mutilation, cruel treatment and torture' and 'outrages upon
 *   personal dignity, in particular humiliating and degrading treatment.' The
 *   absolute_prohibition reading holds that these standards are non-derogable
 *   — they admit no exceptions for national security emergencies,
 *   ticking-bomb scenarios, or the status of the detainee. The constraint
 *   claims Mountain status: a fixed, natural-law-like floor that persists
 *   regardless of state consent or enforcement. The authored metrics reflect
 *   the reading's assessment of the prohibition's actual operation: low
 *   extractiveness (the prohibition itself does not extract from governed
 *   parties), low suppression (it does not suppress alternatives but sets a
 *   floor), high accessibility collapse (the alternative 'torture is
 *   sometimes permissible' collapses under the prohibition's logic), and low
 *   resistance (the norm itself meets little doctrinal resistance; violations
 *   do). The measurement series captures the post-9/11 period when state
 *   practice diverged sharply from the prohibition, driving up theater_ratio
 *   (performative compliance via legal memos redefining torture) and
 *   suppression_requirement (active suppression of accountability
 *   mechanisms), before partial renormalization.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(humane_treatment_standard__absolute_prohibition, 0.18).
domain_priors:suppression_score(humane_treatment_standard__absolute_prohibition, 0.12).
domain_priors:theater_ratio(humane_treatment_standard__absolute_prohibition, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, extractiveness, 0.18).
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, resistance, 0.14).

% --- Constraint claim ---
narrative_ontology:constraint_claim(humane_treatment_standard__absolute_prohibition, mountain).
narrative_ontology:human_readable(humane_treatment_standard__absolute_prohibition, "Absolute Prohibition of Torture and Degrading Treatment under Common Article 3").
narrative_ontology:topic_domain(humane_treatment_standard__absolute_prohibition, "international_humanitarian_law/human_rights").

domain_priors:emerges_naturally(humane_treatment_standard__absolute_prohibition).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(humane_treatment_standard__absolute_prohibition, '17a372c7-6a54-4207-b92a-3885cbe7c667').
narrative_ontology:cs_kernel_codification('17a372c7-6a54-4207-b92a-3885cbe7c667', formalized).
narrative_ontology:cs_authority_grounding('17a372c7-6a54-4207-b92a-3885cbe7c667', lineage).
narrative_ontology:cs_interpretation_layer_present('17a372c7-6a54-4207-b92a-3885cbe7c667').
narrative_ontology:cs_reading_relation('17a372c7-6a54-4207-b92a-3885cbe7c667', humane_treatment_standard__contextual_necessity, forecloses).
narrative_ontology:cs_reading_relation('17a372c7-6a54-4207-b92a-3885cbe7c667', humane_treatment_standard__proportionality_balancing, forecloses).
narrative_ontology:cs_axiom('17a372c7-6a54-4207-b92a-3885cbe7c667', foundational, torture_categorically_prohibited).
narrative_ontology:cs_axiom_status(torture_categorically_prohibited, holdable).
narrative_ontology:cs_axiom_grounding('17a372c7-6a54-4207-b92a-3885cbe7c667', torture_categorically_prohibited, deontological).
narrative_ontology:cs_axiom('17a372c7-6a54-4207-b92a-3885cbe7c667', foundational, human_dignity_non_derogable).
narrative_ontology:cs_axiom_status(human_dignity_non_derogable, holdable).
narrative_ontology:cs_axiom_grounding('17a372c7-6a54-4207-b92a-3885cbe7c667', human_dignity_non_derogable, deontological).
narrative_ontology:cs_axiom('17a372c7-6a54-4207-b92a-3885cbe7c667', secondary, no_security_exception_to_common_article_3).
narrative_ontology:cs_axiom_status(no_security_exception_to_common_article_3, holdable).
narrative_ontology:cs_axiom_grounding('17a372c7-6a54-4207-b92a-3885cbe7c667', no_security_exception_to_common_article_3, conventional).
narrative_ontology:cs_reference_frame('17a372c7-6a54-4207-b92a-3885cbe7c667', common_article_3_absolute_prohibition).
narrative_ontology:cs_drift_state('17a372c7-6a54-4207-b92a-3885cbe7c667', contemporary_war_on_terror_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('17a372c7-6a54-4207-b92a-3885cbe7c667', '').
narrative_ontology:cs_kernel_id(humane_treatment_standard__absolute_prohibition, humane_treatment_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(humane_treatment_standard__absolute_prohibition, detainees).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(humane_treatment_standard__absolute_prohibition, military_commanders).
narrative_ontology:constraint_vindicates(humane_treatment_standard__absolute_prohibition, human_dignity_inviolable).
narrative_ontology:constraint_vindicates(humane_treatment_standard__absolute_prohibition, non_derogable_rights).
narrative_ontology:constraint_vindicates(humane_treatment_standard__absolute_prohibition, common_article_3_customary_law).
narrative_ontology:constraint_vindicates(humane_treatment_standard__absolute_prohibition, jus_cogens_prohibition_torture).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Persons deprived of liberty in armed conflict or security detention. They rely entirely on the absolute prohibition for protection from torture and degrading treatment. They have no exit from state custody, no leverage to enforce the constraint, and their survival depends on the constraint's operation. The prohibition subsidizes their protection by imposing absolute costs on would-be perpetrators.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, detainees, beneficiary,
    powerless, immediate, trapped, global).

% State intelligence, military, and security agencies that conduct interrogations and manage detention. They set interrogation policy and administer the detention system. They are bound by the absolute prohibition but hold the operational power to violate it. Their institutional mandate to prevent attacks creates structural pressure to extract actionable intelligence, which the prohibition absolutely forecloses as a justification for crossing the threshold.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, state_security_apparatus, agenda_setter,
    institutional, biographical, constrained, national).

% Treaty bodies (UN Committee Against Torture, Human Rights Committee), the ICRC, and regional human rights courts. They monitor compliance, interpret the prohibition, investigate allegations, and issue authoritative readings. They lack enforcement power but their interpretations shape the constraint's operational meaning. They benefit from the constraint's clarity but bear institutional costs of documentation and advocacy.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, human_rights_bodies, observer,
    institutional, generational, analytical, global).

% Civilian populations threatened by terrorism. The contextual_necessity reading invokes their security to justify exceptions to the prohibition; the absolute_prohibition reading structurally excludes their voice from the constraint's operation, treating security imperatives as legally irrelevant to the threshold. Their exclusion is not accidental — it is the structural move that makes the prohibition absolute.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, victims_of_terrorism, excluded,
    organized, biographical, constrained, national).

% Domestic and international courts that adjudicate torture claims and apply the prohibition. They give the constraint legal effect through judgments, exclusionary rules, and reparations orders. They depend on state cooperation for enforcement. Their role is dual: they administer the constraint (agenda_setter) and are constrained by it in their own fact-finding (payer of institutional costs).
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, judicial_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Commanders responsible for detention operations and rules of engagement. They bear the operational costs of compliance — training, monitoring, disciplinary systems, and the forgone intelligence that coercive interrogation might have yielded. They are payers of the constraint's coordination costs but also its primary implementers. Their exit is constrained by chain of command and international law obligations.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, military_commanders, payer,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(humane_treatment_standard__absolute_prohibition, diffuse).
narrative_ontology:fixing_cost_class(humane_treatment_standard__absolute_prohibition, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a universal, non-derogable floor for the treatment of all persons in the power of a party to a conflict. Solves the coordination problem of reciprocal restraint in warfare: each party knows the other is bound by the same absolute threshold, enabling minimal trust for prisoner exchanges, surrender, and humanitarian access.
% TRANSFER_FUNCTION: Moves the power to inflict severe physical and mental suffering from the detaining authority to the constrained zone — the prohibition transfers the legal capacity to authorize torture from 'sovereign discretion' to 'nowhere.' The transfer is from state security apparatus (who lose the legal option of coercive interrogation) to detainees (who gain an absolute shield). No material resource moves; the transfer is of legal permission and physical vulnerability.
% ABSENT_VOICES: Victims of terrorism and their communities are structurally excluded — the absolute reading treats their security as irrelevant to the threshold. The contextual_necessity reading would make them central beneficiaries of exceptions. Also absent: the 'ticking bomb' detainee who possesses imminent threat intelligence — the absolute reading denies this category exists as a legal matter, while the siblings treat it as the decisive case.
% DISAPPEARANCE_RATIONALE: If the absolute prohibition vanished overnight, the legal architecture of detention would collapse into the contextual_necessity or proportionality_balancing frameworks. States would formally authorize graded interrogation techniques calibrated to threat levels. The ICRC's protective mandate would lose its non-derogable core. Prisoner exchanges and surrender incentives would degrade because captors could legally torture. The jus cogens status of the torture prohibition would unravel, affecting universal jurisdiction, non-refoulement, and command responsibility doctrines.
% FOUNDING_PROBLEM: The 1949 Geneva Conventions were drafted in the shadow of WWII atrocities — systematic torture, medical experimentation, and degrading treatment of POWs, civilians, and occupied populations. The founding problem was: how to establish a floor of humanity that survives the collapse of all other legal protections in war? Common Article 3 answered: a non-derogable minimum that applies 'in all circumstances' to 'persons taking no active part in hostilities,' binding even non-state parties in non-international conflicts.
% FOUNDING_PROBLEM_CORROBORATION: The ICRC's 2016 updated Commentary on Common Article 3 (authored outside the benefiting states) affirms the founding problem remains live: 'The nature of armed conflicts has changed, but the vulnerability of persons in the power of the enemy has not.' The UN Special Rapporteur on Torture (2023 report) corroborates that 'the absolute prohibition remains the only effective barrier against the normalization of coercive techniques.' No credible voice outside the state security apparatus claims the founding problem is solved.
narrative_ontology:disappearance_verdict(humane_treatment_standard__absolute_prohibition, world_rearranges).
narrative_ontology:founding_problem_status(humane_treatment_standard__absolute_prohibition, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(humane_treatment_standard__absolute_prohibition, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(humane_treatment_standard__absolute_prohibition, 'none', 1).
narrative_ontology:epsilon_provenance(humane_treatment_standard__absolute_prohibition, 0.18, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(humane_treatment_standard__absolute_prohibition_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, ExtMetricName, E),
    domain_priors:suppression_score(humane_treatment_standard__absolute_prohibition, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(humane_treatment_standard__absolute_prohibition),
    narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(humane_treatment_standard__absolute_prohibition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The absolute_prohibition reading claims Mountain because the prohibition derives from human dignity as an intrinsic, pre-legal status — not from state consent or utilitarian calculation. The metrics reflect this: extractiveness is low because the constraint governs by forbidding extraction (torture extracts from detainees; the prohibition forbids that extraction). Suppression is low because the constraint does not suppress alternative interrogation methods that comply with the floor; it only suppresses the crossing of the threshold. Theater_ratio is low in the base state but the temporal series reveals a significant spike during 2001-2009 when states maintained the prohibition's language while constructing legal architectures to circumvent it — the performative maintenance of the constraint's appearance while hollowing its substance. Accessibility_collapse is high because once the absolute prohibition is accepted as a structural premise, the space for 'regulated torture' or 'proportional cruel treatment' collapses; the constraint's logic admits no middle ground. Resistance is low because the prohibition itself faces little intellectual resistance — the resistance comes from state practice, not from the constraint's internal logic.
 *
 * PERSPECTIVAL GAP:
 *   The absolute_prohibition reading computes Mountain from every seat because the constraint's logic is binary: either the threshold holds or it does not. From the detainee's seat, the constraint is a Mountain — it is the only barrier between them and unlimited state violence. From the state interrogator's seat, the constraint is also a Mountain — it is an immutable limit they cannot legally cross, regardless of operational pressure. The contextual_necessity and proportionality_balancing readings compute differently (Tangled Rope or Snare) because they introduce a second variable (security imperative, proportionality calculation) that makes the constraint's operation conditional. The perspectival gap is not between seats within this reading but between this reading and its siblings: the absolute reading denies the structural relevance of the variables the siblings treat as decisive.
 *
 * DIRECTIONALITY LOGIC:
 *   The primary structural relationship is between detainees (full beneficiaries: the constraint subsidizes their protection by imposing costs on would-be torturers) and state security apparatus (constrained agenda_setters: they administer detention and interrogation but are absolutely barred from crossing the threshold). The absolute prohibition reading assigns directionality d ≈ 0.0 for detainees (full beneficiary) and d ≈ 1.0 for state interrogators (full target of the constraint's limiting force). Human rights bodies and judicial authorities occupy analytical/institutional seats with d ≈ 0.3 — they benefit from the constraint's clarity but bear institutional costs of enforcement. Victims of terrorism are excluded from the constraint's operation (role: excluded) — the absolute reading structurally forecloses their security claims as relevant to the threshold, which is the core disagreement with the contextual_necessity reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate (Common Article 3, 1949) has not atrophied — the founding problem (protecting non-combatants and hors de combat persons from the horrors of war) remains live. The prohibition has not become a piton: it is actively litigated, invoked in domestic and international courts, and shapes military doctrine. The theater_ratio spike during 2001-2009 was a stress test, not a degradation into performance. The constraint's persistence is not inertial; it is continually renewed by the recurrence of the founding problem (armed conflict, detention, interrogation). The mandatrophy_resolved flag is false — the mandate remains live and the constraint remains functionally anchored to it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mountain_vs_false_summit,
    'Is the absolute prohibition a genuine natural-law Mountain (non-derogable by nature) or a constructed constraint that functions as a false summit — claimed as absolute but selectively enforced to benefit states that violate it while claiming compliance?',
    'Cross-regime comparison of prosecution rates for torture vs. rates of alleged torture; analysis of whether states that violate the prohibition suffer systemic consequences or only reputational costs; examination of whether the prohibition''s ''absolute'' language serves as a legitimating cover for the contextual_necessity reading''s actual operation.',
    'If false summit, the constraint reclassifies to tangled_rope (coordination function: shared normative floor; extraction: states extract intelligence through torture while claiming the prohibition''s legitimacy). If genuine mountain, the violations are deviations from the constraint, not features of its operation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mountain_vs_false_summit, conceptual, 'Whether the absolute prohibition''s Mountain status is structural or performative.').

omega_variable(
    enforcement_as_extraction,
    'Does the enforcement machinery required to give the prohibition effect (investigations, prosecutions, monitoring) constitute a coordination cost (Rope) or an extraction mechanism (states use enforcement selectively against adversaries)?',
    'Analysis of enforcement patterns: are prosecutions distributed proportionally to violation severity regardless of perpetrator identity, or do they track geopolitical alignment? Comparison of domestic vs. international enforcement rates.',
    'If enforcement is selective extraction, the constraint''s effective extraction rises for targeted states and the prohibition functions as a geopolitical weapon rather than a universal floor. This would support tangled_rope classification from the targeted state''s seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_as_extraction, empirical, 'Whether the prohibition''s enforcement mechanism is a genuine coordination cost or a tool of asymmetric extraction.').

omega_variable(
    kernel_framing_underdetermination,
    'Does the humane_treatment_standard kernel admit only the three declared readings, or is there a fourth structural framing (e.g., the prohibition as a constitutive element of state legitimacy rather than a constraint on state action)?',
    'Genealogical analysis of Common Article 3''s drafting history: was the provision framed as a limit on sovereign power or as a constitutive condition of lawful belligerency? Examination of whether any state has ever claimed the right to torture as a sovereign prerogative (none have — all claim compliance).',
    'If the prohibition is constitutive of lawful statehood, its Mountain status is reinforced — a state that tortures places itself outside the legal order entirely. If it is merely a constraint, violations are breaches within the order. This changes the disappearance_verdict: world_rearranges (constitutive) vs. world_unchanged (constraint).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the kernel''s framing is constraint-on-power or constitutive-of-legitimacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(humane_treatment_standard__absolute_prohibition, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(humane_treatment_absolute_tr_t1949, humane_treatment_standard__absolute_prohibition, theater_ratio, 1949, 0.05).
narrative_ontology:measurement_basis(humane_treatment_absolute_tr_t1949, observed).
narrative_ontology:measurement(humane_treatment_absolute_tr_t1977, humane_treatment_standard__absolute_prohibition, theater_ratio, 1977, 0.08).
narrative_ontology:measurement_basis(humane_treatment_absolute_tr_t1977, observed).
narrative_ontology:measurement(humane_treatment_absolute_tr_t2001, humane_treatment_standard__absolute_prohibition, theater_ratio, 2001, 0.12).
narrative_ontology:measurement_basis(humane_treatment_absolute_tr_t2001, observed).
narrative_ontology:measurement(humane_treatment_absolute_tr_t2004, humane_treatment_standard__absolute_prohibition, theater_ratio, 2004, 0.42).
narrative_ontology:measurement_basis(humane_treatment_absolute_tr_t2004, observed).
narrative_ontology:measurement(humane_treatment_absolute_tr_t2009, humane_treatment_standard__absolute_prohibition, theater_ratio, 2009, 0.35).
narrative_ontology:measurement_basis(humane_treatment_absolute_tr_t2009, observed).
narrative_ontology:measurement(humane_treatment_absolute_tr_t2014, humane_treatment_standard__absolute_prohibition, theater_ratio, 2014, 0.22).
narrative_ontology:measurement_basis(humane_treatment_absolute_tr_t2014, observed).
narrative_ontology:measurement(humane_treatment_absolute_tr_t2024, humane_treatment_standard__absolute_prohibition, theater_ratio, 2024, 0.15).
narrative_ontology:measurement_basis(humane_treatment_absolute_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(humane_treatment_absolute_be_t1949, humane_treatment_standard__absolute_prohibition, base_extractiveness, 1949, 0.1).
narrative_ontology:measurement_basis(humane_treatment_absolute_be_t1949, observed).
narrative_ontology:measurement(humane_treatment_absolute_be_t1977, humane_treatment_standard__absolute_prohibition, base_extractiveness, 1977, 0.12).
narrative_ontology:measurement_basis(humane_treatment_absolute_be_t1977, observed).
narrative_ontology:measurement(humane_treatment_absolute_be_t2001, humane_treatment_standard__absolute_prohibition, base_extractiveness, 2001, 0.15).
narrative_ontology:measurement_basis(humane_treatment_absolute_be_t2001, observed).
narrative_ontology:measurement(humane_treatment_absolute_be_t2004, humane_treatment_standard__absolute_prohibition, base_extractiveness, 2004, 0.35).
narrative_ontology:measurement_basis(humane_treatment_absolute_be_t2004, observed).
narrative_ontology:measurement(humane_treatment_absolute_be_t2009, humane_treatment_standard__absolute_prohibition, base_extractiveness, 2009, 0.28).
narrative_ontology:measurement_basis(humane_treatment_absolute_be_t2009, observed).
narrative_ontology:measurement(humane_treatment_absolute_be_t2014, humane_treatment_standard__absolute_prohibition, base_extractiveness, 2014, 0.22).
narrative_ontology:measurement_basis(humane_treatment_absolute_be_t2014, observed).
narrative_ontology:measurement(humane_treatment_absolute_be_t2024, humane_treatment_standard__absolute_prohibition, base_extractiveness, 2024, 0.18).
narrative_ontology:measurement_basis(humane_treatment_absolute_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(humane_treatment_absolute_su_t1949, humane_treatment_standard__absolute_prohibition, suppression_requirement, 1949, 0.08).
narrative_ontology:measurement_basis(humane_treatment_absolute_su_t1949, observed).
narrative_ontology:measurement(humane_treatment_absolute_su_t1977, humane_treatment_standard__absolute_prohibition, suppression_requirement, 1977, 0.1).
narrative_ontology:measurement_basis(humane_treatment_absolute_su_t1977, observed).
narrative_ontology:measurement(humane_treatment_absolute_su_t2001, humane_treatment_standard__absolute_prohibition, suppression_requirement, 2001, 0.15).
narrative_ontology:measurement_basis(humane_treatment_absolute_su_t2001, observed).
narrative_ontology:measurement(humane_treatment_absolute_su_t2004, humane_treatment_standard__absolute_prohibition, suppression_requirement, 2004, 0.55).
narrative_ontology:measurement_basis(humane_treatment_absolute_su_t2004, observed).
narrative_ontology:measurement(humane_treatment_absolute_su_t2009, humane_treatment_standard__absolute_prohibition, suppression_requirement, 2009, 0.4).
narrative_ontology:measurement_basis(humane_treatment_absolute_su_t2009, observed).
narrative_ontology:measurement(humane_treatment_absolute_su_t2014, humane_treatment_standard__absolute_prohibition, suppression_requirement, 2014, 0.25).
narrative_ontology:measurement_basis(humane_treatment_absolute_su_t2014, observed).
narrative_ontology:measurement(humane_treatment_absolute_su_t2024, humane_treatment_standard__absolute_prohibition, suppression_requirement, 2024, 0.12).
narrative_ontology:measurement_basis(humane_treatment_absolute_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(humane_treatment_standard__absolute_prohibition, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(humane_treatment_standard__absolute_prohibition, 0.1).
narrative_ontology:affects_constraint(humane_treatment_standard__absolute_prohibition, humane_treatment_standard__contextual_necessity).
narrative_ontology:affects_constraint(humane_treatment_standard__absolute_prohibition, humane_treatment_standard__proportionality_balancing).
narrative_ontology:affects_constraint(humane_treatment_standard__absolute_prohibition, command_responsibility_doctrine).
narrative_ontology:affects_constraint(humane_treatment_standard__absolute_prohibition, non_refoulement_prohibition).
narrative_ontology:affects_constraint(humane_treatment_standard__absolute_prohibition, fair_trial_guarantees_detainees).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the humane_treatment_standard kernel. The absolute_prohibition reading claims Mountain status with zero exceptions. The contextual_necessity reading claims Tangled Rope (coordination floor + security exceptions as extraction). The proportionality_balancing reading claims Scaffold (transitional balancing test). All three share the same referent (Common Article 3) but instantiate different constraints with different ε values and different beneficiary/victim structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(humane_treatment_standard__absolute_prohibition, institutional, 0.85).
constraint_indexing:directionality_override(humane_treatment_standard__absolute_prohibition, powerless, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
