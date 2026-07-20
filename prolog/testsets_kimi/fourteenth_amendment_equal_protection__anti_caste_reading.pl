% ============================================================================
% CONSTRAINT STORY: fourteenth_amendment_equal_protection__anti_caste_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fourteenth_amendment_equal_protection__anti_caste_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: fourteenth_amendment_equal_protection__anti_caste_reading
 *   human_readable: Fourteenth Amendment Equal Protection â Anti-Caste Reading
 *   domain: constitutional_law/political_philosophy/civil_rights
 *
 * SUMMARY:
 *   This constraint instantiates the anti-caste reading of the Fourteenth
 *   Amendment Equal Protection Clause: the claim that Equal Protection
 *   requires active state dismantling of racial, gender, and status hierarchy
 *   through race-conscious corrective action. It is one reading of a
 *   contested kernel; the sibling formal equality reading treats the same
 *   text as mandating colorblind neutrality and prohibiting explicit racial
 *   classification. The anti-caste reading dominated federal constitutional
 *   jurisprudence from Brown v. Board (1954) through the civil rights and
 *   busing eras, ordering states to implement affirmative remedies. Its
 *   operation coordinates subordinated groups into a protected status while
 *   extracting from dominant groups and state governments the costs of
 *   remediation.
 *
 * KEY AGENTS:
 *   - subordinated_groups (beneficiary; powerless; identity-locked) â receive race-conscious judicial and legislative remedies
 *   - dominant_groups (payer; powerful; constrained exit) â bear costs of dismantled privileges and remedial programs
 *   - state_governments (payer; institutional; constrained exit) â implement costly court-ordered remedies and lose policy autonomy
 *   - federal_judiciary (agenda-setter; institutional; analytical exit) â interprets and enforces the anti-caste mandate against resistant states
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fourteenth_amendment_equal_protection__anti_caste_reading, 0.78).
domain_priors:suppression_score(fourteenth_amendment_equal_protection__anti_caste_reading, 0.75).
domain_priors:theater_ratio(fourteenth_amendment_equal_protection__anti_caste_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, resistance, 0.82).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fourteenth_amendment_equal_protection__anti_caste_reading, tangled_rope).
narrative_ontology:human_readable(fourteenth_amendment_equal_protection__anti_caste_reading, "Fourteenth Amendment Equal Protection â Anti-Caste Reading").
narrative_ontology:topic_domain(fourteenth_amendment_equal_protection__anti_caste_reading, "constitutional_law/political_philosophy/civil_rights").

domain_priors:requires_active_enforcement(fourteenth_amendment_equal_protection__anti_caste_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fourteenth_amendment_equal_protection__anti_caste_reading, '007d3112-cdd2-499f-9f06-35f6b6fb3984').
narrative_ontology:cs_kernel_codification('007d3112-cdd2-499f-9f06-35f6b6fb3984', fixed_text).
narrative_ontology:cs_authority_grounding('007d3112-cdd2-499f-9f06-35f6b6fb3984', lineage).
narrative_ontology:cs_interpretation_layer_present('007d3112-cdd2-499f-9f06-35f6b6fb3984').
narrative_ontology:cs_reading_relation('007d3112-cdd2-499f-9f06-35f6b6fb3984', fourteenth_amendment_equal_protection__formal_equality_reading, forecloses).
narrative_ontology:cs_axiom('007d3112-cdd2-499f-9f06-35f6b6fb3984', foundational, antisubordination_as_constitutional_mandate).
narrative_ontology:cs_axiom_status(antisubordination_as_constitutional_mandate, holdable).
narrative_ontology:cs_axiom_grounding('007d3112-cdd2-499f-9f06-35f6b6fb3984', antisubordination_as_constitutional_mandate, deontological).
narrative_ontology:cs_axiom('007d3112-cdd2-499f-9f06-35f6b6fb3984', foundational, race_conscious_remediation_required).
narrative_ontology:cs_axiom_status(race_conscious_remediation_required, holdable).
narrative_ontology:cs_axiom_grounding('007d3112-cdd2-499f-9f06-35f6b6fb3984', race_conscious_remediation_required, deontological).
narrative_ontology:cs_reference_frame('007d3112-cdd2-499f-9f06-35f6b6fb3984', antisubordination_constitutional_order).
narrative_ontology:cs_drift_state('007d3112-cdd2-499f-9f06-35f6b6fb3984', formal_equality_retrenchment_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('007d3112-cdd2-499f-9f06-35f6b6fb3984', '').
narrative_ontology:cs_kernel_id(fourteenth_amendment_equal_protection__anti_caste_reading, fourteenth_amendment_equal_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__anti_caste_reading, subordinated_groups).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__anti_caste_reading, dominant_groups).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__anti_caste_reading, state_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Are the subjects of judicial orders and legislative programs designed to dismantle racial, gender, and status hierarchies; receive race-conscious remedies in education, employment, and voting; their social identity is the basis for constitutional protection and remedial classification, and they cannot exit the identity categories that trigger the constraint's protection.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, subordinated_groups, beneficiary,
    powerless, generational, identity_locked, national).

% Bear the direct and indirect costs of dismantling programs including reduced relative access to historically exclusive institutions, status competition from remedial preferences, and tax burdens for compliance programs; their prior advantages are targeted for reduction by federal court order, and exit to jurisdictions without such programs is increasingly limited.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, dominant_groups, payer,
    powerful, biographical, constrained, national).

% Are ordered by federal courts to design and implement race-conscious remedial programs such as school busing, legislative districting, and affirmative action; bear fiscal costs and intense political backlash from constituents; lose traditional autonomy over education, employment, and electoral policy to federal structural injunctions.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, state_governments, payer,
    institutional, generational, constrained, national).

% Interprets the Fourteenth Amendment to mandate active state dismantling of hierarchy; issues structural injunctions against states and localities; reviews legislative remedies for adequacy; faces institutional legitimacy crises when imposing unpopular race-conscious orders against majoritarian resistance.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, federal_judiciary, agenda_setter,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fourteenth_amendment_equal_protection__anti_caste_reading, subordinated_groups).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates federal judicial power and state remedial action to dismantle entrenched racial, gender, and status hierarchies that persist despite formal legal equality, ensuring subordinated groups receive substantive equal citizenship through targeted state intervention.
% TRANSFER_FUNCTION: Transfers status, opportunity, and material resources from dominant groups and state policy autonomy to subordinated groups through race-conscious remedial programs, structural injunctions, and judicial oversight of state action.
% ABSENT_VOICES: Formal equality advocates and colorblind constitutionalists who reject race-conscious state classification are present in legal discourse but structurally marginalized when this reading controls judicial outcomes; originalist interpreters who deny the Amendment encompasses group-based remedies at all are treated as outside the legitimate interpretive framework.
% DISAPPEARANCE_RATIONALE: If the anti-caste mandate vanished overnight, federal courts would cease ordering race-conscious remedies, state affirmative action and busing programs would lose constitutional footing and likely collapse, subordinated groups would lose a major doctrinal vehicle for challenging systemic hierarchy, and the constitutional order would shift toward formal neutrality â the social and legal landscape would rearrange around colorblind constitutionalism.
% FOUNDING_PROBLEM: The persistence of caste-like status hierarchies after the abolition of slavery, particularly the Black Codes and Jim Crow systems that maintained subordination through formal and informal mechanisms despite the Thirteenth Amendment's abolition of chattel bondage.
% FOUNDING_PROBLEM_CORROBORATION: Reconstruction-era framers such as Stevens and Sumner, alongside modern historians, attest the caste problem as the Amendment's target. However, the claim that the clause mandates ongoing race-conscious state dismantling (as opposed to merely prohibiting state-enforced caste) is contested by originalist jurists and the current Supreme Court majority, who attest from outside the anti-caste beneficiary set that the founding problem of state-mandated segregation is solved and that ongoing race-conscious remedies exceed the original constitutional mandate.
narrative_ontology:disappearance_verdict(fourteenth_amendment_equal_protection__anti_caste_reading, world_rearranges).
narrative_ontology:founding_problem_status(fourteenth_amendment_equal_protection__anti_caste_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fourteenth_amendment_equal_protection__anti_caste_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(fourteenth_amendment_equal_protection__anti_caste_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fourteenth_amendment_equal_protection__anti_caste_reading, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fourteenth_amendment_equal_protection__anti_caste_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fourteenth_amendment_equal_protection__anti_caste_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fourteenth_amendment_equal_protection__anti_caste_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is a tangled rope because it possesses both a genuine coordination function (remedying systemic subordination and coordinating federal protection for subordinated groups) and asymmetric extraction (dominant groups lose relative status and opportunity; state governments lose fiscal resources and autonomy). Extractiveness is high (0.78 at interval end) because remedial programs such as busing, race-conscious districting, and affirmative action impose concentrated costs on identifiable groups and institutions. Suppression is high (0.75) because the constraint's persistence depends on federal courts actively suppressing local majoritarian preferences and state autonomy claims that maintain hierarchy. Theater is low-moderate (0.28) because the remedial function is substantively real, though some compliance activity becomes performative over time. Resistance is very high (0.82) due to massive resistance movements, white flight, state interposition, and ongoing doctrinal warfare.
 *
 * PERSPECTIVAL GAP:
 *   The seat divergence is severe. From subordinated_groups, the constraint reads as essential coordination against entrenched caste violence; from dominant_groups, it reads as illegitimate redistribution of status and opportunity; from state_governments, it reads as federal coercion sacrificing local autonomy. The federal_judiciary experiences it as constitutional duty. The engine computes this divergence from the structural data rather than adjudicating it.
 *
 * DIRECTIONALITY LOGIC:
 *   Subordinated_groups are declared beneficiaries and carry identity_locked exit, placing their directionality near the full-beneficiary end (low d). Dominant_groups and state_governments are declared victims with constrained exit, placing their directionality near the full-target end (high d). The federal_judiciary is neither beneficiary nor victim; as institutional agenda-setter with analytical exit, it sits near the center but leans toward low d because it controls and legitimates the constraint rather than bearing its costs.
 *
 * MANDATROPHY ANALYSIS:
 *   Without the tangled rope classification, this constraint would be misread as either a pure rope (if one ignored the extraction from dominant groups and states) or a pure snare (if one ignored the genuine protective coordination for subordinated groups). The mandated structural declarations â beneficiaries, victims, and active enforcement â force the dual character into the open. The high theater ratio threshold is not met, preventing piton misclassification: the constraint is not merely performative, though its enforcement has generated performative compliance in some jurisdictions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indeterminacy,
    'Does the Fourteenth Amendment''s Equal Protection Clause encode an anti-caste mandate requiring race-conscious remediation, or merely a formal neutrality requirement prohibiting explicit racial classification?',
    'Historical-semantic analysis of Reconstruction-era understanding combined with corpus linguistics of ''equal protection'' usage 1866-1868; however, deep normative disagreement about legitimate interpretive method (originalism vs. moral reading) may leave the question structurally open regardless of historical findings.',
    'Resolving toward formal neutrality would reclassify this constraint as a snare if the anti-caste machinery were maintained despite rejection of its core premise, or dissolve it entirely; resolving toward anti-caste would confirm the current tangled rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'The fundamental indeterminacy between anti-caste and formal equality readings of the same constitutional text.').

omega_variable(
    remedial_efficacy_and_backlash,
    'Do race-conscious remedial programs mandated by the anti-caste reading effectively dismantle social hierarchy, or do they generate political backlash and categorical entrenchment that exceeds their remedial benefit?',
    'Longitudinal sociological studies of intergenerational mobility under race-conscious versus race-neutral regimes, controlling for confounding economic variables; political science measurement of backlash intensity and policy durability.',
    'If remedial programs are shown to increase hierarchy or backlash without net mobility gains, the coordination function weakens and the constraint slides toward snare; if shown effective, the tangled rope classification strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remedial_efficacy_and_backlash, empirical, 'Whether the constraint''s coordination function produces its intended remedial effects.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fourteenth_amendment_equal_protection__anti_caste_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ep_ac_tr_t0, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(ep_ac_tr_t6, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 6, 0.18).
narrative_ontology:measurement(ep_ac_tr_t12, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 12, 0.22).
narrative_ontology:measurement(ep_ac_tr_t18, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 18, 0.25).
narrative_ontology:measurement(ep_ac_tr_t24, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 24, 0.27).
narrative_ontology:measurement(ep_ac_tr_t30, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 30, 0.28).

% Extraction over time
narrative_ontology:measurement(ep_ac_be_t0, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(ep_ac_be_t6, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 6, 0.65).
narrative_ontology:measurement(ep_ac_be_t12, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 12, 0.82).
narrative_ontology:measurement(ep_ac_be_t18, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 18, 0.85).
narrative_ontology:measurement(ep_ac_be_t24, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 24, 0.82).
narrative_ontology:measurement(ep_ac_be_t30, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 30, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(ep_ac_su_t0, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(ep_ac_su_t6, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 6, 0.85).
narrative_ontology:measurement(ep_ac_su_t12, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 12, 0.88).
narrative_ontology:measurement(ep_ac_su_t18, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 18, 0.85).
narrative_ontology:measurement(ep_ac_su_t24, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 24, 0.8).
narrative_ontology:measurement(ep_ac_su_t30, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 30, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(fourteenth_amendment_equal_protection__anti_caste_reading, formal_equality_reading).

% DUAL FORMULATION NOTE:
% This constraint and formal_equality_reading are decomposed from the natural-language label 'Equal Protection' per the Îµ-invariance principle. They share the same constitutional text but instantiate structurally distinct constraints with different beneficiary/victim structures, opposing Îµ profiles, and irreconcilable core premises.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
