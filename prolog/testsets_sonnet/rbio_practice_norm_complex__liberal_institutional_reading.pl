% ============================================================================
% CONSTRAINT STORY: rbio_practice_norm_complex__liberal_institutional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rbio_practice_norm_complex__liberal_institutional_reading, []).

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
 *   constraint_id: rbio_practice_norm_complex__liberal_institutional_reading
 *   human_readable: Rules-Based International Order — Liberal Institutional Reading
 *   domain: international relations / international law / political economy
 *
 * SUMMARY:
 *   This constraint instantiates the liberal institutional reading of the
 *   RBIO (rules-based international order) practice-norm complex: RBIO norms
 *   are universal in scope, grounded in state consent (treaty ratification,
 *   UN Charter membership), and revisable only through legitimate
 *   multilateral processes (treaty amendment, Security Council resolution,
 *   General Assembly action). On this reading, the well-documented empirical
 *   pattern of selective enforcement — powerful states escape sanction for
 *   conduct that triggers intervention against weaker states — is explained
 *   by capacity and political-will constraints on collective action, not by a
 *   defect in the norms' legitimacy. The coordination function (a shared
 *   vocabulary for atrocity prevention and collective security) is genuine
 *   and is claimed to justify the residual extraction that selective
 *   enforcement produces. This is a distinct constraint from the
 *   hegemonic_extraction_reading (which holds the same enforcement pattern
 *   reveals extractive intent baked into a frozen hegemonic project) and the
 *   sovereignty_maximalist_reading (which holds humanitarian intervention is
 *   presumptively illegitimate). All three share the same underlying practice
 *   but diverge on ε, on beneficiary/victim framing, and on classification —
 *   they are linked here as sibling constraints in the same kernel family,
 *   not as alternative measurements of one constraint.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rbio_practice_norm_complex__liberal_institutional_reading, 0.42).
domain_priors:suppression_score(rbio_practice_norm_complex__liberal_institutional_reading, 0.38).
domain_priors:theater_ratio(rbio_practice_norm_complex__liberal_institutional_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rbio_practice_norm_complex__liberal_institutional_reading, tangled_rope).
narrative_ontology:human_readable(rbio_practice_norm_complex__liberal_institutional_reading, "Rules-Based International Order — Liberal Institutional Reading").
narrative_ontology:topic_domain(rbio_practice_norm_complex__liberal_institutional_reading, "international relations / international law / political economy").

domain_priors:requires_active_enforcement(rbio_practice_norm_complex__liberal_institutional_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rbio_practice_norm_complex__liberal_institutional_reading, '341af85a-4425-4796-9205-354b6d5406c6').
narrative_ontology:cs_kernel_codification('341af85a-4425-4796-9205-354b6d5406c6', formalized).
narrative_ontology:cs_authority_grounding('341af85a-4425-4796-9205-354b6d5406c6', lineage).
narrative_ontology:cs_interpretation_layer_present('341af85a-4425-4796-9205-354b6d5406c6').
narrative_ontology:cs_reading_relation('341af85a-4425-4796-9205-354b6d5406c6', rbio_practice_norm_complex__hegemonic_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('341af85a-4425-4796-9205-354b6d5406c6', rbio_practice_norm_complex__sovereignty_maximalist_reading, influences).
narrative_ontology:cs_axiom('341af85a-4425-4796-9205-354b6d5406c6', foundational, enforcement_selectivity_is_capacity_not_legitimacy_defect).
narrative_ontology:cs_axiom_status(enforcement_selectivity_is_capacity_not_legitimacy_defect, holdable).
narrative_ontology:cs_axiom_grounding('341af85a-4425-4796-9205-354b6d5406c6', enforcement_selectivity_is_capacity_not_legitimacy_defect, empirically_contingent).
narrative_ontology:cs_axiom('341af85a-4425-4796-9205-354b6d5406c6', foundational, state_consent_via_ratification_is_legitimating).
narrative_ontology:cs_axiom_status(state_consent_via_ratification_is_legitimating, holdable).
narrative_ontology:cs_axiom_grounding('341af85a-4425-4796-9205-354b6d5406c6', state_consent_via_ratification_is_legitimating, conventional).
narrative_ontology:cs_reference_frame('341af85a-4425-4796-9205-354b6d5406c6', un_charter_consent_based_multilateralism).
narrative_ontology:cs_drift_state('341af85a-4425-4796-9205-354b6d5406c6', post_iraq_libya_intervention_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('341af85a-4425-4796-9205-354b6d5406c6', '').
narrative_ontology:cs_kernel_id(rbio_practice_norm_complex__liberal_institutional_reading, rbio_practice_norm_complex).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__liberal_institutional_reading, intervening_states).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__liberal_institutional_reading, un_authorized_coalition_states).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__liberal_institutional_reading, reconstruction_and_sanctions_contractors).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__liberal_institutional_reading, multilateral_institution_secretariats).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__liberal_institutional_reading, sanctioned_state_civilian_populations).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__liberal_institutional_reading, targeted_state_governments).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__liberal_institutional_reading, smaller_states_lacking_veto_leverage).
narrative_ontology:constraint_vindicates(rbio_practice_norm_complex__liberal_institutional_reading, responsibility_to_protect_doctrine).
narrative_ontology:constraint_vindicates(rbio_practice_norm_complex__liberal_institutional_reading, multilateral_legitimacy_doctrine).
narrative_ontology:constraint_vindicates(rbio_practice_norm_complex__liberal_institutional_reading, consent_based_treaty_revisability_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sit on the UN Security Council or lead coalition efforts, invoking UNSC authorization or the Responsibility to Protect to justify sanctions or military intervention. They draft the resolutions, staff the enforcement bodies, and retain the option to abstain from enforcement action where their own interests are implicated. Their exit from any given enforcement obligation is effectively unconstrained by anything but their own veto or coalition-building calculus.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, intervening_states, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(rbio_practice_norm_complex__liberal_institutional_reading, intervening_states, beneficiary).

% Join sanctions regimes or interventions once multilateral authorization exists, gaining legitimacy cover and often preferential post-conflict reconstruction access. They can withdraw from a coalition with reputational but not existential cost.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, un_authorized_coalition_states, beneficiary,
    powerful, generational, mobile, global).

% Firms and NGOs that win reconstruction, monitoring, and compliance contracts once sanctions or interventions are authorized. They benefit directly from the enforcement machinery's operation regardless of whether the underlying norm violation is remedied.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, reconstruction_and_sanctions_contractors, beneficiary,
    organized, biographical, arbitrage, global).

% Administer the treaty bodies, sanctions committees, and monitoring mechanisms. They certify compliance, issue exemptions, and interpret ambiguous mandates, giving them discretion over how selectively norms are applied even though they present the process as procedurally neutral.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, multilateral_institution_secretariats, agenda_setter,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(rbio_practice_norm_complex__liberal_institutional_reading, multilateral_institution_secretariats, observer).

% Bear the humanitarian cost of sanctions regimes and intervention aftermath — currency collapse, medical shortages, displacement — regardless of whether their government is the sanctioned party. They have no seat at the Security Council table and no meaningful exit from the territory being sanctioned.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, sanctioned_state_civilian_populations, payer,
    powerless, immediate, trapped, national).

% Face sanctions, conditionality, or intervention once designated non-compliant with RBIO norms. They can contest designation through multilateral forums but hold little leverage against states with veto power; compliance is the only realistic exit, and even compliance does not guarantee sanctions relief.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, targeted_state_governments, payer,
    moderate, biographical, constrained, national).

% Are formally equal participants in multilateral revision processes (treaty conferences, General Assembly votes) but cannot compel enforcement action against P5 states or their allies when those states violate the same norms. Their formal voice in norm-setting is real but structurally disconnected from enforcement outcomes.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, smaller_states_lacking_veto_leverage, excluded,
    moderate, generational, constrained, regional).

% Study the gap between the formal universality of RBIO norms and the empirical pattern of selective enforcement, producing the scholarship this reading and its rivals both draw on.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, international_law_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rbio_practice_norm_complex__liberal_institutional_reading, reconstruction_and_sanctions_contractors).
narrative_ontology:fixing_cost_class(rbio_practice_norm_complex__liberal_institutional_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, consent-based normative vocabulary — sovereignty, non-aggression, human rights, humanitarian protection — through which states can coordinate on collective security and atrocity prevention without each state having to unilaterally judge and police every other state's conduct.
% TRANSFER_FUNCTION: Moves reconstruction contracts, sanctions-compliance business, and post-intervention institutional influence toward intervening/coalition states and their contractors; moves humanitarian and economic cost onto the civilian populations of sanctioned or intervened states, and moves formal-but-not-effective voice to smaller states in revision processes.
% ABSENT_VOICES: Civilian populations under sanction have no standing in the Security Council process that designates their state non-compliant; smaller states without veto power can vote in General Assembly revision processes but cannot compel symmetrical enforcement against powerful violators — their objection, when made, is procedurally heard but substantively unable to alter enforcement patterns.
% DISAPPEARANCE_RATIONALE: From this reading's own premises, if the RBIO framework disappeared, the loss of a shared multilateral vocabulary for atrocity prevention and collective security coordination would be a real cost — ad hoc unilateralism would replace consent-based process. Rival readings dispute whether anything of substance would actually change in enforcement outcomes, since selectivity already tracks power rather than the norms themselves; that dispute is exactly what makes the verdict contested rather than settled within this single reading.
% FOUNDING_PROBLEM: Post-1945 need for a multilateral, consent-based alternative to unilateral great-power intervention: a shared standard for sovereignty, aggression, and human rights violations that could be invoked and revised through legitimate collective process rather than by any single state's unilateral judgment.
% FOUNDING_PROBLEM_CORROBORATION: Multilateral institution secretariats and intervening states attest the founding problem remains live, citing ongoing atrocity-prevention and collective-security functions. Independent scholarship (international law academics, UN monitoring bodies' own internal reviews, and third-party enforcement-pattern studies) corroborates that the formal consent-based process persists, but disputes whether enforcement selectivity reflects mere capacity constraints as this reading holds, or a deeper legitimacy problem as the sibling hegemonic-extraction reading holds — that corroboration is itself split along the kernel's contested lines, which is disclosed rather than resolved here.
narrative_ontology:disappearance_verdict(rbio_practice_norm_complex__liberal_institutional_reading, contested).
narrative_ontology:founding_problem_status(rbio_practice_norm_complex__liberal_institutional_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rbio_practice_norm_complex__liberal_institutional_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(rbio_practice_norm_complex__liberal_institutional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rbio_practice_norm_complex__liberal_institutional_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rbio_practice_norm_complex__liberal_institutional_reading_tests).
:- end_tests(rbio_practice_norm_complex__liberal_institutional_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate (0.42) rather than low: this reading concedes a real transfer occurs (sanctions and reconstruction contracts flow to intervening states and their contractors, costs land on sanctioned populations) but holds that transfer is the acceptable byproduct of legitimate collective enforcement, not the norm's purpose. Suppression is moderate (0.38): the constraint requires active enforcement machinery (sanctions committees, peacekeeping mandates, monitoring bodies) but does not, on this reading, foreclose exit for norm-compliant states — only for designated violators. Theater ratio (0.30) reflects the reading's own acknowledgment that some enforcement activity (monitoring reports, compliance certifications) is more performative than substantively effective, while maintaining that the underlying coordination function is real. The claimed_type (tangled_rope) is authored independently of these metrics: this reading believes the arrangement combines genuine coordination with unavoidable asymmetric cost, which is precisely the tangled_rope structure, but does not tune the metrics to force that classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Intervening/coalition states and their contractors sit near the beneficiary end: they set enforcement agendas, retain veto-shielded exit from symmetric application, and capture reconstruction and compliance-monitoring value. Sanctioned civilian populations sit at the full-target end: trapped by territory, bearing humanitarian cost, with no seat in the authorizing body. Targeted state governments are constrained rather than fully trapped — compliance is a genuine if costly exit path recognized by this reading. Smaller states without veto leverage are treated as excluded rather than beneficiary or payer in the strict sense: they participate formally in revision processes but cannot convert that voice into enforcement parity, which this reading treats as a capacity gap rather than a designed extraction, in contrast to the hegemonic_extraction_reading's structural-capture account of the same fact pattern.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists mandatrophy classification by holding the founding problem (multilateral alternative to unilateral great-power judgment) as still live and corroborated by ongoing atrocity-prevention practice, not merely by the beneficiaries' own testimony. The contested founding_problem_status and disappearance_verdict fields disclose, rather than resolve, the fact that this self-assessment is disputed by outside scholarship and by the sibling readings — the mismatch between this reading's 'contested-but-live' self-account and a hypothetical 'dead-but-persists' finding from independent audit is exactly the signal the mandatrophy detector is built to catch, and this story deliberately leaves that signal visible rather than smoothing it into a clean verdict.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capacity_vs_legitimacy_selectivity,
    'Is the empirically observed selectivity in RBIO enforcement (powerful states escaping sanction for conduct that triggers intervention against weaker states) best explained by genuine capacity/political-will constraints on collective action, or does it reveal that the norms were never legitimately universal in application?',
    'Comparative case analysis of enforcement outcomes controlling for veto power, alliance structure, and severity of violation; if severity and veto-status jointly and fully predict enforcement outcome (rather than severity alone), the capacity account loses explanatory force relative to the extraction account.',
    'If selectivity tracks veto power more than violation severity, this reading''s core premise (capacity problem, not legitimacy problem) is substantially undermined and the constraint would need reclassification toward the hegemonic_extraction_reading''s structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capacity_vs_legitimacy_selectivity, empirical, 'Whether enforcement selectivity is a capacity artifact or a legitimacy defect — the central axis separating this reading from its principal sibling.').

omega_variable(
    consent_under_asymmetric_power,
    'Is state consent to RBIO norms (treaty ratification, Charter membership) meaningfully voluntary for states with limited bargaining power at the founding moment and limited exit options subsequently, or is ''consent'' doing rhetorical work to legitimate an arrangement that was substantially imposed?',
    'Historical analysis of founding-era negotiation leverage (1945 San Francisco Conference, subsequent accession conditions) and empirical tracking of whether smaller states'' revision proposals have altered enforcement practice versus merely altering formal text.',
    'If consent was substantially coerced or the revision channel is formally open but practically inert, the ''consent-based, revisable through legitimate process'' framing central to this reading''s axioms is weakened, moving the constraint''s classification closer to the sovereignty_maximalist or hegemonic_extraction readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_under_asymmetric_power, conceptual, 'Whether founding and ongoing consent is structurally meaningful or rhetorically load-bearing.').

omega_variable(
    kernel_reading_selection_evidence,
    'What structural signals guided the choice to author this constraint as the liberal_institutional_reading rather than defaulting to the hegemonic_extraction_reading''s ε and beneficiary structure, given both readings describe the same underlying practice?',
    'The choice follows the manifest''s explicit reading assignment (liberal_institutional_reading) and its declared expected structural delta (UNSC/atrocity-threshold justification, contract-like conditionality, intervening-state beneficiary set). An alternative framing collapsing this story into the hegemonic_extraction_reading would require adopting that reading''s premise that formal revisability is practically inert due to P5 veto — a premise this reading''s axioms explicitly reject as holdable.',
    'Adopting the alternative framing would lower this story''s claimed legitimacy self-assessment and likely shift claimed_type toward snare; the two framings produce different cs_pattern classifications from the same underlying fact pattern, which is why they are authored as separate sibling constraints rather than reconciled into one.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_selection_evidence, conceptual, 'CS-framing under-determination: documents why this reading, rather than its structurally adjacent sibling, was instantiated as this constraint file.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rbio_practice_norm_complex__liberal_institutional_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rbio_tr_t1945, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 1945, 0.18).
narrative_ontology:measurement(rbio_tr_t1960, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 1960, 0.2).
narrative_ontology:measurement(rbio_tr_t1990, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 1990, 0.24).
narrative_ontology:measurement(rbio_tr_t2003, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 2003, 0.28).
narrative_ontology:measurement(rbio_tr_t2014, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 2014, 0.29).
narrative_ontology:measurement(rbio_tr_t2025, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 2025, 0.3).

% Extraction over time
narrative_ontology:measurement(rbio_be_t1945, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 1945, 0.28).
narrative_ontology:measurement(rbio_be_t1960, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 1960, 0.32).
narrative_ontology:measurement(rbio_be_t1990, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 1990, 0.35).
narrative_ontology:measurement(rbio_be_t2003, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 2003, 0.4).
narrative_ontology:measurement(rbio_be_t2014, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 2014, 0.41).
narrative_ontology:measurement(rbio_be_t2025, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 2025, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(rbio_su_t1945, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 1945, 0.3).
narrative_ontology:measurement(rbio_su_t1960, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 1960, 0.32).
narrative_ontology:measurement(rbio_su_t1990, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 1990, 0.33).
narrative_ontology:measurement(rbio_su_t2003, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 2003, 0.36).
narrative_ontology:measurement(rbio_su_t2014, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 2014, 0.37).
narrative_ontology:measurement(rbio_su_t2025, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 2025, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rbio_practice_norm_complex__liberal_institutional_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__liberal_institutional_reading, rbio_practice_norm_complex__hegemonic_extraction_reading).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__liberal_institutional_reading, rbio_practice_norm_complex__sovereignty_maximalist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the rbio_practice_norm_complex kernel, each authored as a separate ε-invariant constraint per the ε-invariance principle: liberal_institutional_reading (this file, moderate ε, tangled_rope), hegemonic_extraction_reading (higher ε, frozen-hegemony framing, likely snare or tangled_rope), and sovereignty_maximalist_reading (intervention itself treated as illegitimate absent sovereignty-protective purpose, likely snare from the targeted-state seat). All three describe the same underlying practice of multilateral norm-setting and selective enforcement but diverge in claimed beneficiary/victim structure, in what counts as legitimate revision, and in classification. They are linked bidirectionally via affects_constraints rather than merged into one story with an observable parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rbio_practice_norm_complex__liberal_institutional_reading, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
