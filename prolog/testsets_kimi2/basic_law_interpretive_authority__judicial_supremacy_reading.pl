% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_authority__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_authority__judicial_supremacy_reading, []).

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
 *   constraint_id: basic_law_interpretive_authority__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy in Constitutional Interpretation
 *   domain: constitutional_law/political_theory
 *
 * SUMMARY:
 *   This constraint instantiates the judicial_supremacy_reading of the
 *   contested kernel basic_law_interpretive_authority. Under this reading,
 *   courts hold final interpretive authority over constitutional meaning,
 *   justified by specialized legal expertise and independence from political
 *   pressure. The judiciary enters the beneficiary set by acquiring
 *   institutional authority, prestige, and agenda-setting power; elected
 *   legislatures and electoral majorities enter the victim set when judicial
 *   review invalidates legislation or creates anticipatory gridlock. Sibling
 *   readings include parliamentary_sovereignty_reading (legislature retains
 *   final authority) and popular_constitutionalism_reading (meaning emerges
 *   from democratic contestation). The constraint exhibits both genuine
 *   coordinationâconstitutional stability, uniform rights protection, and
 *   inter-branch conflict resolutionâand asymmetric extraction: the
 *   authority to define binding constitutional meaning is concentrated in an
 *   unelected institution, while democratic majorities bear the costs of
 *   blocked preferences.
 *
 * KEY AGENTS:
 *   - judicial_institution: Primary agenda-setter and beneficiary (institutional/generational/identity_locked) â administers final interpretive authority and derives institutional centrality from it.
 *   - elected_legislature: Primary payer (institutional/biographical/constrained) â enacts laws subject to judicial veto and bears gridlock costs.
 *   - electoral_majorities: Secondary payer (organized/biographical/constrained) â preferences blocked by constitutional interpretation.
 *   - constitutional_scholars: Analytical observer (analytical/generational/analytical) â critiques the allocation of authority without direct stake.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_authority__judicial_supremacy_reading, 0.62).
domain_priors:suppression_score(basic_law_interpretive_authority__judicial_supremacy_reading, 0.58).
domain_priors:theater_ratio(basic_law_interpretive_authority__judicial_supremacy_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_authority__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_authority__judicial_supremacy_reading, "Judicial Supremacy in Constitutional Interpretation").
narrative_ontology:topic_domain(basic_law_interpretive_authority__judicial_supremacy_reading, "constitutional_law/political_theory").

domain_priors:requires_active_enforcement(basic_law_interpretive_authority__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_authority__judicial_supremacy_reading, '7a1ad0f7-949a-49c5-87ee-b88f988420cf').
narrative_ontology:cs_kernel_codification('7a1ad0f7-949a-49c5-87ee-b88f988420cf', fixed_text).
narrative_ontology:cs_authority_grounding('7a1ad0f7-949a-49c5-87ee-b88f988420cf', lineage).
narrative_ontology:cs_interpretation_layer_present('7a1ad0f7-949a-49c5-87ee-b88f988420cf').
narrative_ontology:cs_reading_relation('7a1ad0f7-949a-49c5-87ee-b88f988420cf', basic_law_interpretive_authority__parliamentary_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('7a1ad0f7-949a-49c5-87ee-b88f988420cf', basic_law_interpretive_authority__popular_constitutionalism_reading, influences).
narrative_ontology:cs_axiom('7a1ad0f7-949a-49c5-87ee-b88f988420cf', foundational, judicial_finality_norm).
narrative_ontology:cs_axiom_status(judicial_finality_norm, holdable).
narrative_ontology:cs_axiom_grounding('7a1ad0f7-949a-49c5-87ee-b88f988420cf', judicial_finality_norm, conventional).
narrative_ontology:cs_axiom('7a1ad0f7-949a-49c5-87ee-b88f988420cf', foundational, institutional_independence_imperative).
narrative_ontology:cs_axiom_status(institutional_independence_imperative, holdable).
narrative_ontology:cs_axiom_grounding('7a1ad0f7-949a-49c5-87ee-b88f988420cf', institutional_independence_imperative, instrumental).
narrative_ontology:cs_reference_frame('7a1ad0f7-949a-49c5-87ee-b88f988420cf', judicial_finality_framework).
narrative_ontology:cs_drift_state('7a1ad0f7-949a-49c5-87ee-b88f988420cf', contemporary_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('7a1ad0f7-949a-49c5-87ee-b88f988420cf', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_authority__judicial_supremacy_reading, basic_law_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__judicial_supremacy_reading, judicial_institution).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__judicial_supremacy_reading, elected_legislature).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__judicial_supremacy_reading, electoral_majorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds and exercises final authority to interpret the constitutional text, invalidating legislation and executive actions inconsistent with its readings. Derives institutional prestige, budgetary support, and existential purpose from this adjudicative role. Cannot exit the constitutional framework without ceasing to be the judiciary.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, judicial_institution, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_authority__judicial_supremacy_reading, judicial_institution, beneficiary).

% Enacts legislation subject to judicial review and potential invalidation. Bears gridlock costs when constitutional uncertainty delays or deters statutory schemes. Cannot reliably predict which laws will survive review, and cannot override constitutional interpretations without extraordinary majorities or constitutional amendment.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, elected_legislature, payer,
    institutional, biographical, constrained, national).

% Express policy preferences through elections that may be blocked by judicial interpretation of constitutional limits. Bear the cost of seeing majority-preferred legislation invalidated or never enacted due to anticipated judicial veto. Exit is limited to long-term constitutional amendment or changing the composition of the judiciary through subsequent elections.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, electoral_majorities, payer,
    organized, biographical, constrained, national).

% Analyze and critique the allocation of interpretive authority across branches, producing competing theories of judicial supremacy, parliamentary sovereignty, and popular constitutionalism. Neither directly benefit from nor pay the operational costs of judicial review, though scholarly reputations may be tied to particular readings.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, constitutional_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(basic_law_interpretive_authority__judicial_supremacy_reading, judicial_institution).
narrative_ontology:fixing_cost_class(basic_law_interpretive_authority__judicial_supremacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified, final arbiter for constitutional disputes, reducing inter-branch conflict and protecting entrenched rights against transient legislative majorities.
% TRANSFER_FUNCTION: Transfers final interpretive authority and constitutional veto power from elected legislatures and electoral majorities to the judiciary; transfers gridlock and anticipation costs to the legislative process.
% ABSENT_VOICES: Popular constitutionalists who argue meaning emerges from democratic contestation, and parliamentary sovereignty advocates who would locate final authority in the elected legislature, are structurally marginalized in jurisdictions where judicial supremacy is the operating assumption.
% DISAPPEARANCE_RATIONALE: If judicial supremacy vanished, constitutional interpretation would fragment across branches or shift to popular processes; the legislative agenda would recalibrate without anticipatory judicial vetoes, and the judiciary would lose its institutional center of gravity.
% FOUNDING_PROBLEM: How to maintain a written constitution as supreme law against ordinary legislative majorities while resolving inter-branch disputes without violence.
% FOUNDING_PROBLEM_CORROBORATION: Federalist Papers (Hamilton, No. 78; Madison, No. 51) attest from outside the modern judiciary. Comparative constitutional scholars note that stable democracies without judicial supremacy challenge the necessity claim.
narrative_ontology:disappearance_verdict(basic_law_interpretive_authority__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_authority__judicial_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_authority__judicial_supremacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(basic_law_interpretive_authority__judicial_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_authority__judicial_supremacy_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_authority__judicial_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(basic_law_interpretive_authority__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(basic_law_interpretive_authority__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects a substantial but not total transfer of lawmaking authority: judicial review genuinely coordinates constitutional continuity and rights protection, but it also extracts democratic decision-making capacity from electorally accountable bodies. Suppression (0.58) captures the binding force of judicial precedent and the rarity of successful legislative or popular override; it is moderated by the theoretical availability of constitutional amendment and jurisdiction stripping. Theater ratio (0.25) is low because judicial reasoning involves real expertise and doctrinal craft, though legitimation narratives about neutrality contain performative elements. Accessibility collapse (0.65) registers that departmentalism and legislative override are structurally known but practically collapsed as live options in most constitutional cultures. Resistance (0.55) reflects persistent inter-branch friction, court-packing threats, and popular backlash. Temporal measurements trace the gradual intensification of judicial review from the early republic to the modern activist judiciary.
 *
 * PERSPECTIVAL GAP:
 *   The judiciary experiences this constraint as professional duty and constitutional fidelity; the payer seats experience it as an external check that substitutes elite legal reasoning for electoral accountability. The engine computes this divergence from beneficiary/victim declarations and exit asymmetry: the judiciary is identity-locked to the constraint (it would cease to be the judiciary without interpretive authority), while legislatures and majorities are merely constrained (exit is theoretically possible but institutionally prohibitive).
 *
 * DIRECTIONALITY LOGIC:
 *   The judicial_institution is declared in beneficiaries and carries agenda_setter role with secondary_role beneficiary, placing it at the low-d (subsidy) end: the constraint subsidizes its authority and institutional existence. The elected_legislature and electoral_majorities are declared in victims and carry payer role, placing them at the high-d (target) end: the constraint extracts lawmaking capacity and democratic efficacy from them. Constitutional_scholars are observers with analytical exit, sitting near the symmetric middle. No directionality overrides are needed because the structural derivation matches the actual relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as tangled_rope prevents the mandatrophy error of treating judicial supremacy as either a pure snare (ignoring the genuine coordination function in constitutional stability and rights protection) or a pure rope (ignoring the asymmetric concentration of final authority in one unelected branch). The founding problemâmaintaining a written constitution against ordinary majoritiesâmay be partially dead or contested in contemporary conditions, but the coordination function is not fully theatrical, so piton is inappropriate. The metrics and claim are authored independently: the claim is tangled_rope while the metrics describe a moderately extractive, actively enforced arrangement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_extraction_boundary,
    'Is judicial review primarily a coordination mechanism for constitutional stability and rights protection, or primarily an asymmetric extraction of democratic authority to an unelected institution?',
    'Comparative analysis of democracies with and without judicial supremacy, measuring constitutional stability and rights outcomes.',
    'If the coordination function is dominant, effective extraction is lower and the constraint moves toward rope; if extraction dominates, it moves toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Ambiguity between coordination and extraction in judicial review').

omega_variable(
    suppression_internalization,
    'Is legislative compliance with judicial supremacy driven by structural legal finality (binding precedent, contempt power) or by internalized norms of judicial legitimacy?',
    'Observe legislative behavior in jurisdictions where judicial decisions are formally non-binding or during inter-branch crises.',
    'If internalized, suppression is higher than structural measures suggest and resistance may be lower; if purely structural, resistance is the true floor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Structural versus internalized suppression mechanism').

omega_variable(
    axiom_empirical_resilience,
    'Has the instrumental axiom that judicial independence produces faithful constitutional interpretation been empirically overridden by evidence of partisan polarization in appointments and outcomes?',
    'Empirical studies of judicial voting patterns, appointment polarization, and public perception of judicial neutrality.',
    'If overridden, the authority_erosion drift state deepens and the reading''s foundational grounding weakens, potentially shifting classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(axiom_empirical_resilience, empirical, 'Empirical status of judicial neutrality axiom').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_authority__judicial_supremacy_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t0, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(basi_tr_t12, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 12, 0.18).
narrative_ontology:measurement(basi_tr_t24, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 24, 0.2).
narrative_ontology:measurement(basi_tr_t36, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 36, 0.22).
narrative_ontology:measurement(basi_tr_t48, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 48, 0.24).
narrative_ontology:measurement(basi_tr_t60, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 60, 0.25).

% Extraction over time
narrative_ontology:measurement(basi_be_t0, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(basi_be_t12, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 12, 0.42).
narrative_ontology:measurement(basi_be_t24, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 24, 0.5).
narrative_ontology:measurement(basi_be_t36, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 36, 0.55).
narrative_ontology:measurement(basi_be_t48, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 48, 0.6).
narrative_ontology:measurement(basi_be_t60, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 60, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t0, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(basi_su_t12, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 12, 0.45).
narrative_ontology:measurement(basi_su_t24, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 24, 0.5).
narrative_ontology:measurement(basi_su_t36, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 36, 0.53).
narrative_ontology:measurement(basi_su_t48, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 48, 0.56).
narrative_ontology:measurement(basi_su_t60, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 60, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_authority__judicial_supremacy_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
