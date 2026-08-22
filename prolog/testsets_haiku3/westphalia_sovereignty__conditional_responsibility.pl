% ============================================================================
% CONSTRAINT STORY: westphalia_sovereignty__conditional_responsibility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-13
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalia_sovereignty__conditional_responsibility, []).

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
 *   constraint_id: westphalia_sovereignty__conditional_responsibility
 *   human_readable: Conditional Sovereignty: Atrocity Threshold Authority
 *   domain: international_law/political_theory
 *
 * SUMMARY:
 *   The conditional responsibility reading of Westphalian sovereignty
 *   restructures state authority around a protective obligation: states
 *   retain territorial inviolability only insofar as they protect their
 *   populations from mass atrocities. External powers gain authority to
 *   assess protective capacity, name atrocities, and authorize intervention
 *   when thresholds are breached. This reading generates a tangled rope
 *   constraint: genuine coordination function (preventing atrocities through
 *   shared enforcement mechanism) coupled with substantial asymmetric
 *   extraction (powerful states set the threshold, adjudicate breaches, and
 *   control intervention mechanisms; constrained states lose autonomy;
 *   atrocity victims lack voice). The doctrine is actively enforced through
 *   UN mechanisms, ICC prosecution, sanctions regimes, and military
 *   coalitions. Theater is substantial (0.58 at interval end): humanitarian
 *   framing legitimizes interventions that serve geopolitical interest;
 *   protective capacity assessments are selective; atrocity designations
 *   track powerful-state preference.
 *
 * KEY AGENTS:
 *   - humanitarian_intervention_coalitions (Western powers + established institutions): set threshold, adjudicate breaches, control intervention
 *   - non_interventionist_developing_regimes (China, Russia, India, African Union members): lose sovereignty protection, gain exposure to external override
 *   - atrocity_investigation_bodies (ICC, UN missions): gain mandate authority and investigative scope
 *   - atrocity_victims_unprotected_populations (excluded from authority structure, invoked as moral referent)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalia_sovereignty__conditional_responsibility, 0.68).
domain_priors:suppression_score(westphalia_sovereignty__conditional_responsibility, 0.71).
domain_priors:theater_ratio(westphalia_sovereignty__conditional_responsibility, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, extractiveness, 0.68).
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, resistance, 0.76).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalia_sovereignty__conditional_responsibility, tangled_rope).
narrative_ontology:human_readable(westphalia_sovereignty__conditional_responsibility, "Conditional Sovereignty: Atrocity Threshold Authority").
narrative_ontology:topic_domain(westphalia_sovereignty__conditional_responsibility, "international_law/political_theory").

domain_priors:requires_active_enforcement(westphalia_sovereignty__conditional_responsibility).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalia_sovereignty__conditional_responsibility, '876d172c-e7ee-4743-b83d-cdc7fab5c455').
narrative_ontology:cs_kernel_codification('876d172c-e7ee-4743-b83d-cdc7fab5c455', fixed_text).
narrative_ontology:cs_authority_grounding('876d172c-e7ee-4743-b83d-cdc7fab5c455', extraction).
narrative_ontology:cs_interpretation_layer_present('876d172c-e7ee-4743-b83d-cdc7fab5c455').
narrative_ontology:cs_reading_relation('876d172c-e7ee-4743-b83d-cdc7fab5c455', westphalia_sovereignty__absolute_non_intervention, forecloses).
narrative_ontology:cs_reading_relation('876d172c-e7ee-4743-b83d-cdc7fab5c455', westphalia_sovereignty__graded_sovereignty, influences).
narrative_ontology:cs_axiom('876d172c-e7ee-4743-b83d-cdc7fab5c455', foundational, sovereignty_conditional_on_protective_capacity).
narrative_ontology:cs_axiom_status(sovereignty_conditional_on_protective_capacity, holdable).
narrative_ontology:cs_axiom_grounding('876d172c-e7ee-4743-b83d-cdc7fab5c455', sovereignty_conditional_on_protective_capacity, deontological).
narrative_ontology:cs_axiom('876d172c-e7ee-4743-b83d-cdc7fab5c455', foundational, international_adjudication_authority_over_protective_standards).
narrative_ontology:cs_axiom_status(international_adjudication_authority_over_protective_standards, holdable).
narrative_ontology:cs_axiom_grounding('876d172c-e7ee-4743-b83d-cdc7fab5c455', international_adjudication_authority_over_protective_standards, conventional).
narrative_ontology:cs_reference_frame('876d172c-e7ee-4743-b83d-cdc7fab5c455', conditional_protective_authority).
narrative_ontology:cs_drift_state('876d172c-e7ee-4743-b83d-cdc7fab5c455', contemporary_selective_application, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('876d172c-e7ee-4743-b83d-cdc7fab5c455', '').
narrative_ontology:cs_kernel_id(westphalia_sovereignty__conditional_responsibility, westphalia_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__conditional_responsibility, humanitarian_intervention_coalitions).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__conditional_responsibility, global_governance_institutions).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__conditional_responsibility, atrocity_investigation_bodies).
narrative_ontology:constraint_victim(westphalia_sovereignty__conditional_responsibility, non_interventionist_developing_regimes).
narrative_ontology:constraint_victim(westphalia_sovereignty__conditional_responsibility, regional_hegemons_constrained_by_doctrine).
narrative_ontology:constraint_victim(westphalia_sovereignty__conditional_responsibility, populations_under_protective_state_regimes).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__conditional_responsibility, populations_under_protective_state_regimes).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Western military/diplomatic powers and established international institutions (NATO, EU, UN Security Council P5) that deploy force and economic pressure to enforce atrocity thresholds. They set the boundary conditions for when sovereignty is forfeited—mass killing triggers intervention authority justified by conditional responsibility doctrine. They determine the evidentiary bar, the urgency assessment, and the remedial action. Their discretion in naming atrocity and designating intervention subjects is vast; their exit from the enforcement role is nominally voluntary but sustained by institutional investment and perceived legitimacy claims.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, humanitarian_intervention_coalitions, agenda_setter,
    institutional, generational, arbitrage, global).

% ICC, UN fact-finding missions, human rights commissions gain mandate authority and investigative jurisdiction under the conditional responsibility reading. They benefit from expanded operational scope and from the doctrine's legitimation of their work as enforcing a binding international standard. They operate with limited enforcement power but significant norm-setting authority.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, atrocity_investigation_bodies, beneficiary,
    institutional, generational, mobile, global).

% Nations (China, Russia, India, African Union members, others) that oppose external intervention on principle or in practice face a lowered sovereignty threshold under this doctrine. Their territorial inviolability becomes conditional on meeting protective standards set by external adjudicators. They pay through reduced autonomy in domestic security choices, loss of non-intervention norms they historically relied on, and vulnerability to external military/economic intervention justified by atrocity framing. Exit from the doctrine requires either geopolitical realignment, developing counter-norms, or demonstrating protective capacity sufficient to satisfy the international standard.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, non_interventionist_developing_regimes, payer,
    powerful, generational, constrained, global).

% Regional powers (Turkey, Saudi Arabia, Iran, Brazil, others) that exercise control within their sphere face constraints from external powers using atrocity doctrine to justify counter-intervention. They cannot freely suppress dissent or manage ethnic/religious populations without risking external delegation and condemnation framed as atrocity prevention. The doctrine limits their extractive capacity within their region by empowering external actors to name legitimate intervention.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, regional_hegemons_constrained_by_doctrine, payer,
    powerful, biographical, constrained, continental).

% Civilian populations live under states that meet the protective threshold (minimal mass atrocity) but use the conditional responsibility doctrine to strengthen internal security apparatus, military presence, and surveillance justified as atrocity prevention. They benefit from genuine protective services but pay through reduced autonomy, militarized governance, and use of atrocity-prevention framing to suppress dissent. The doctrine empowers states that can frame opposition as security threat and external intervention advocates as paternalist powers.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, populations_under_protective_state_regimes, payer,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(westphalia_sovereignty__conditional_responsibility, populations_under_protective_state_regimes, beneficiary).

% Populations experiencing mass atrocities under regimes that fail the protective threshold nominally become the doctrine's beneficiaries—external powers claim authority to intervene on their behalf. In practice, they remain voiceless in the doctrine's operation: they do not adjudicate the threshold, do not choose intervention form or timing, and often experience intervention as external warfare and occupation, not protection. The doctrine invokes their suffering to justify external authority but does not seat them as participants in defining or executing that authority.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, atrocity_victims_unprotected_populations, excluded,
    powerless, immediate, trapped, local).

% The UN Security Council, General Assembly, ICJ, ICC, and informal coalitions of Western states collectively adjudicate what constitutes mass atrocity, when the protective threshold is breached, what intervention is warranted, and whether a state retains sovereignty. This authority structure gains power under the conditional responsibility reading; the doctrine formalizes their adjudicative role and legitimizes their assertion of override authority. They operate with significant discretion—the atrocity standard is not bright-line, and intervention thresholds are politically negotiated.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, international_legal_authority_structure, agenda_setter,
    institutional, generational, analytical, global).

% States and movements that hold categorical non-intervention as foundational (regional groupings like ALBA, African Union's prior Charter, many Global South nations) are structurally excluded from this doctrine's adjudicative authority. They would argue that the conditional responsibility framing privileges powerful states as judges of other states' protective capacity and enables disguised colonialism. Their voice is absent from the authority structure that defines and enforces the threshold.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, absolute_non_intervention_states, excluded,
    organized, generational, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(westphalia_sovereignty__conditional_responsibility, humanitarian_intervention_coalitions).
narrative_ontology:fixing_cost_class(westphalia_sovereignty__conditional_responsibility, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of externally enforcing minimum humanitarian standards across sovereign jurisdictions without requiring unanimous consent: establishes that states whose governance falls below a defined protective threshold lose immunity from external intervention, creating a shared expectation that atrocities will trigger external response and enabling coalition action without framing it as unilateral aggression.
% TRANSFER_FUNCTION: Transfers sovereignty authority from individual states to international adjudicators (UN institutions, powerful-state coalitions, ICC). States lose the right to claim territorial inviolability if they fail protective thresholds. Humanitarian/governance institutions gain mandate to investigate, judge, and authorize intervention. Populations experiencing atrocity gain nominal beneficiary status but no voice in execution.
% ABSENT_VOICES: Non-interventionist states, regional hegemons outside the Western coalition, and atrocity victims themselves are structurally excluded. Non-interventionist powers argue the threshold is set by powerful states to serve powerful-state interests; they would contest the evidentiary standards, the intervention authority, and the claim that external force protects rather than harms. Atrocity victims are invoked as the doctrine's moral referent but are not seated in the authority structure that decides intervention form or timing.
% DISAPPEARANCE_RATIONALE: If conditional responsibility disappeared, non-interventionist states would reclaim non-interference norms, regional powers would regain autonomous sphere control, and humanitarian intervention would revert to requiring explicit Security Council authorization (de jure) or naked coalition power (de facto, as now). The doctrine's disappearance would collapse the legitimation structure that frames external military action as protective mandate rather than power assertion.
% FOUNDING_PROBLEM: Sovereignty as categorical inviolability left no legal mechanism to prevent large-scale atrocities within state borders; the international community lacked authority to intervene even when genocide or mass killing was documented, because territorial sovereignty trumped all humanitarian claims.
% FOUNDING_PROBLEM_CORROBORATION: Humanitarian advocates and Western powers attest the founding problem persists (atrocities occur, sovereignty shields perpetrators). Non-interventionist states and many Global South scholars attest the founding problem has been reframed: the real problem is not absence of humanitarian authority but abuse of humanitarian framing to legitimize powerful-state intervention. Independent analysts note that conditional responsibility doctrine has been selectively invoked (Libya yes, Syria partially, Yemen not, Myanmar sporadically) suggesting the evidentiary standard tracks geopolitical interest rather than atrocity severity.
narrative_ontology:disappearance_verdict(westphalia_sovereignty__conditional_responsibility, world_rearranges).
narrative_ontology:founding_problem_status(westphalia_sovereignty__conditional_responsibility, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalia_sovereignty__conditional_responsibility, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(westphalia_sovereignty__conditional_responsibility, 'none', 1).
narrative_ontology:epsilon_provenance(westphalia_sovereignty__conditional_responsibility, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalia_sovereignty__conditional_responsibility_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalia_sovereignty__conditional_responsibility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalia_sovereignty__conditional_responsibility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises 0.44→0.68 over the interval (t=0 to t=35) as the doctrine's operational scope expands: initial articulation (R2P, 2005) with limited enforcement mechanisms gives way to normalized ICC prosecution, sanctions regimes, and intervention coalitions. The plateau at t≥25 reflects saturation—the doctrine's institutional embedding is complete; further extraction requires escalation (more interventions, more expansive atrocity designations) or acceptance by non-intervening states, neither occurring. Suppression is consistently high (0.54→0.71) because enforcement depends on excluding non-interventionist adjudicative authority and constraining victim voice—the doctrine legitimizes the exclusion by invoking humanitarian principle. Theater rises (0.32→0.58) as the gap widens between protective function and geopolitical application: early period (t=0-10) emphasizes genuine atrocity prevention; later period (t≥15) shows selective invocation, humanitarian rhetoric masking strategic interests (Libya vs. Syria, Myanmar, Yemen), and protective framing used to justify militarized governance. Accessibility collapse is moderate (0.62): alternatives persist (non-intervention coalitions, regional norms, bilateral diplomacy, non-military sanctions) but are constrained by institutional coordination around the conditional responsibility doctrine. Resistance is high (0.76): non-interventionist states mount sustained institutional opposition (UN voting blocs, African Union, BRICS), legal scholars challenge the evidentiary standards, and victims' movements object to external control framing.
 *
 * PERSPECTIVAL GAP:
 *   From the humanitarian intervention coalition seat: the constraint is genuine coordination that saves lives by creating enforceable standards. From the non-interventionist state seat: the constraint is powerful-state hegemony dressed as humanitarianism—a mechanism to override sovereigns and impose external governance under protective language. From the atrocity victim seat (if seated): the constraint invokes their suffering to legitimize external authority they do not control. The engine should compute these as sharply divergent types: beneficiary → rope-leaning; payer → snare-leaning; excluded → snare-clarity. This divergence reflects real structural asymmetry in who sets rules, who adjudicates, who benefits, and who bears costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Humanitarian intervention coalitions (institutional, arbitrage-level exit, global scope) are beneficiaries on d≈0.15: they gain authority, set rules, control intervention timing/form. Non-interventionist developing regimes (institutional, constrained exit, global scope) are victims on d≈0.85: they lose autonomy, face exposure to external override, cannot refuse participation in a regime they did not accept. Regional hegemons (powerful but constrained, continental scope) sit at d≈0.70: they face reduced regional autonomy without institutional power to reshape the doctrine. Atrocity victims (powerless, trapped, local scope) are victims on d≈0.95: they lose voice in the mechanism invoked in their name; external intervention may be protective or may be warfare and occupation; they cannot exit. Investigation bodies (institutional, mobile exit) are beneficiaries on d≈0.25: they gain mandate and scope but lack enforcement power; their authority derives from the doctrine, so they are not fully independent. Protective state populations (organized, constrained, local scope) sit near d≈0.50: they benefit from protective services but pay through militarized governance and suppression of dissent framed as security.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live but contested. Non-interventionist powers argue the stated problem (sovereignty prevents atrocity prevention) has been reframed into a different problem (powerful states need authority override). The doctrine claims to solve atrocity prevention through external accountability; the distribution of benefits (to intervention coalitions and investigation bodies) and costs (to non-interventionist states and victims) suggests it solves an extraction problem—powerful states extract sovereignty authority by reframing it as humanitarian necessity. The mandatrophy resolution turns on whether the protective function and the authority extraction are separable: if conditional responsibility could be enforced through victim-centered mechanisms (victim voice in adjudication, victim control over remedies) rather than powerful-state adjudication, the extraction would diminish. Currently, they are fused—authority and extraction ride together. The high theater ratio (0.58) indicates performative maintenance: the humanitarian framing is genuine but covers geopolitical selectivity. The rising extractiveness curve (0.44→0.68, plateau at t≥25) suggests the doctrine has matured into a stable extraction mechanism with entertainment value (humanitarian theater) sustaining it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    atrocity_evidentiary_standard_ambiguity,
    'What constitutes a mass atrocity triggering the protective threshold? Is the standard bright-line (deaths per capita, genocide convention criteria) or discretionary (international adjudicators assess severity)?',
    'Comparison of designated atrocities: do cases like Myanmar, Yemen, Syria, Ukraine, and past cases (Rwanda, Bosnia) show consistent application of a coherent standard, or do designations correlate with geopolitical interest?',
    'If bright-line: the doctrine is rule-based and reduces extraction discretion. If discretionary: powerful states retain authority to name atrocity and justify intervention selectively, increasing effective extraction and supporting the snare classification for payer seats.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(atrocity_evidentiary_standard_ambiguity, empirical, 'Whether the atrocity threshold is bright-line or discretionary; critical to distinguishing genuine coordination from disguised extraction.').

omega_variable(
    victim_voice_exclusion_mechanism,
    'Is atrocity victim exclusion from the adjudicative authority a structural feature of the doctrine, or could the authority structure be reformed to center victim participation?',
    'Examine ICC victim participation mechanisms, UN fact-finding mandate constraints, and proposals for victim-centered remedies; assess whether structural redesign could seat victims as co-adjudicators.',
    'If exclusion is structural and unreformable: victims are instrumentalized (invoked to justify external action they don''t control), supporting snare classification. If redesign is possible: the doctrine could be decoupled from hegemonic extraction, shifting toward tangled rope or rope for victim seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_voice_exclusion_mechanism, conceptual, 'Whether victim voice exclusion is essential to the doctrine''s structure or contingent on current institutional design.').

omega_variable(
    non_interventionist_reading_underrepresentation,
    'Do non-interventionist states and scholars have genuine alternative framings of the sovereignty kernel that compete with conditional responsibility on principled grounds, or are they merely defending status quo power?',
    'Systematic engagement with non-interventionist doctrine (African Union Charter, ALBA principles, Global South legal scholarship): can coherent principled positions on sovereignty be articulated that are not reducible to hegemonic resistance?',
    'If genuine alternatives exist: the reading-space is authentically contested and the doctrine''s authority claim must be defended on merits, not assumed. If primarily hegemonic resistance: conditional responsibility gains legitimacy as the only principled solution. Either way, this omega dissolves the false unanimity that humanitarian consensus claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_interventionist_reading_underrepresentation, conceptual, 'Whether non-interventionist resistance reflects principled alternative sovereignty doctrines or tactical opposition to Western hegemony.').

omega_variable(
    protective_capacity_assessment_standard,
    'What observable facts determine whether a state meets the protective threshold? Do assessments measure actual atrocity prevention capacity, regime type, or alignment with international standards?',
    'Examine which regimes are deemed to have sufficient protective capacity and which are not; assess whether protective status correlates with regime democracy, Western alliance, or actual conflict/atrocity rates.',
    'If capacity-based: the doctrine could rationally select intervention targets. If regime-based or alliance-based: the doctrine provides cover for selective intervention on geopolitical grounds, increasing extraction and supporting theater_ratio elevation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(protective_capacity_assessment_standard, empirical, 'Whether protective capacity assessments track actual prevention capacity or serve as cover for geopolitical selection.').

omega_variable(
    kernel_reading_foreclosure_relation,
    'Can absolute_non_intervention and conditional_responsibility coexist in a single institutional framework, or does one logically foreclose the other?',
    'Examine UN Charter Article 2(7) (non-intervention principle) against R2P doctrine and conditional responsibility: are they contradictory or compatible as different rules for different contexts?',
    'If foreclosed: one reading logically rules out the other and the kernel contest is decided by institutional power, not argumentation. If coexistent: both readings remain live and the doctrine competes with non-intervention on principled grounds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_relation, conceptual, 'Whether conditional responsibility and absolute non-intervention logically foreclose each other or can coexist in principle.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalia_sovereignty__conditional_responsibility, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t0, westphalia_sovereignty__conditional_responsibility, theater_ratio, 0, 0.32).
narrative_ontology:measurement(west_tr_t5, westphalia_sovereignty__conditional_responsibility, theater_ratio, 5, 0.39).
narrative_ontology:measurement(west_tr_t10, westphalia_sovereignty__conditional_responsibility, theater_ratio, 10, 0.46).
narrative_ontology:measurement(west_tr_t15, westphalia_sovereignty__conditional_responsibility, theater_ratio, 15, 0.52).
narrative_ontology:measurement(west_tr_t20, westphalia_sovereignty__conditional_responsibility, theater_ratio, 20, 0.55).
narrative_ontology:measurement(west_tr_t25, westphalia_sovereignty__conditional_responsibility, theater_ratio, 25, 0.57).
narrative_ontology:measurement(west_tr_t30, westphalia_sovereignty__conditional_responsibility, theater_ratio, 30, 0.58).
narrative_ontology:measurement(west_tr_t35, westphalia_sovereignty__conditional_responsibility, theater_ratio, 35, 0.58).

% Extraction over time
narrative_ontology:measurement(west_be_t0, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 0, 0.44).
narrative_ontology:measurement(west_be_t5, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 5, 0.51).
narrative_ontology:measurement(west_be_t10, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(west_be_t15, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(west_be_t20, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(west_be_t25, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(west_be_t30, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(west_be_t35, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 35, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t0, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 0, 0.54).
narrative_ontology:measurement(west_su_t5, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 5, 0.59).
narrative_ontology:measurement(west_su_t10, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 10, 0.64).
narrative_ontology:measurement(west_su_t15, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(west_su_t20, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(west_su_t25, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 25, 0.7).
narrative_ontology:measurement(west_su_t30, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 30, 0.71).
narrative_ontology:measurement(west_su_t35, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 35, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalia_sovereignty__conditional_responsibility, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(westphalia_sovereignty__conditional_responsibility, 0.12).
narrative_ontology:affects_constraint(westphalia_sovereignty__conditional_responsibility, westphalia_sovereignty__absolute_non_intervention).
narrative_ontology:affects_constraint(westphalia_sovereignty__conditional_responsibility, westphalia_sovereignty__graded_sovereignty).
narrative_ontology:affects_constraint(westphalia_sovereignty__conditional_responsibility, responsibility_to_protect_norm_institutionalization).
narrative_ontology:affects_constraint(westphalia_sovereignty__conditional_responsibility, humanitarian_intervention_authority_structure).

% DUAL FORMULATION NOTE:
% Westphalia_sovereignty kernel decomposes into three constraint stories, each instantiating a different reading with different ε values and beneficiary/victim structures. The conditional_responsibility reading (this story) stands in a influences relation to absolute_non_intervention (constrains that reading's operative scope by establishing conditionality precedent) and coexists_with graded_sovereignty (both accept conditionality but differ on mechanism). Each reading has its own ε, its own authority structure, and its own seat divergence. They are linked not as different observations of the same constraint, but as structural alternatives competing for institutional dominance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(westphalia_sovereignty__conditional_responsibility, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
