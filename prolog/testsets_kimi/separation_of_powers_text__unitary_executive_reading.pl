% ============================================================================
% CONSTRAINT STORY: separation_of_powers_text__unitary_executive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_separation_of_powers_text__unitary_executive_reading, []).

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
 *   constraint_id: separation_of_powers_text__unitary_executive_reading
 *   human_readable: Unitary Executive Reading of Article II Vesting Clause
 *   domain: constitutional_law/political_theory/administrative_law
 *
 * SUMMARY:
 *   This constraint instantiates the unitary-executive reading of the
 *   separation-of-powers kernel: the claim that Article II's vesting clause
 *   concentrates all executive power in the President, rendering independent
 *   agencies and for-cause removal protections unconstitutional. Unlike the
 *   formalist reading (strict non-delegation and impermeable boundaries) or
 *   the functionalist reading (flexible overlapping authority under
 *   intelligible principles), this reading makes presidential removal power
 *   absolute and places independent regulatory agencies in the victim set.
 *   The executive branch is the concentrated beneficiary, while Congress, the
 *   federal judiciary, and the agencies themselves bear the costs of
 *   consolidated control.
 *
 * KEY AGENTS:
 *   - executive_branch: Primary agenda-setter and beneficiary (institutional/arbitrage) â asserts the reading and captures removal power
 *   - independent_regulatory_agencies: Primary target (institutional/trapped) â lose statutory insulation and face subordination or dissolution
 *   - congress: Secondary target (institutional/constrained) â loses delegation and agency-design authority
 *   - federal_judiciary: Secondary target (institutional/constrained) â loses administrative-law flexibility as executive gains come at its expense
 *   - administrative_law_scholars: Analytical observer (organized/analytical) â critiques the historical and functional basis of the reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(separation_of_powers_text__unitary_executive_reading, 0.72).
domain_priors:suppression_score(separation_of_powers_text__unitary_executive_reading, 0.68).
domain_priors:theater_ratio(separation_of_powers_text__unitary_executive_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(separation_of_powers_text__unitary_executive_reading, tangled_rope).
narrative_ontology:human_readable(separation_of_powers_text__unitary_executive_reading, "Unitary Executive Reading of Article II Vesting Clause").
narrative_ontology:topic_domain(separation_of_powers_text__unitary_executive_reading, "constitutional_law/political_theory/administrative_law").

domain_priors:requires_active_enforcement(separation_of_powers_text__unitary_executive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(separation_of_powers_text__unitary_executive_reading, '7c4ec327-39e2-44f0-af50-45b1f8aa44ec').
narrative_ontology:cs_kernel_codification('7c4ec327-39e2-44f0-af50-45b1f8aa44ec', fixed_text).
narrative_ontology:cs_authority_grounding('7c4ec327-39e2-44f0-af50-45b1f8aa44ec', lineage).
narrative_ontology:cs_interpretation_layer_present('7c4ec327-39e2-44f0-af50-45b1f8aa44ec').
narrative_ontology:cs_reading_relation('7c4ec327-39e2-44f0-af50-45b1f8aa44ec', separation_of_powers_text__formalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('7c4ec327-39e2-44f0-af50-45b1f8aa44ec', separation_of_powers_text__functionalist_reading, forecloses).
narrative_ontology:cs_axiom('7c4ec327-39e2-44f0-af50-45b1f8aa44ec', foundational, article_ii_vests_all_executive_power_in_president).
narrative_ontology:cs_axiom_status(article_ii_vests_all_executive_power_in_president, holdable).
narrative_ontology:cs_axiom_grounding('7c4ec327-39e2-44f0-af50-45b1f8aa44ec', article_ii_vests_all_executive_power_in_president, empirically_contingent).
narrative_ontology:cs_axiom('7c4ec327-39e2-44f0-af50-45b1f8aa44ec', foundational, independent_agencies_are_unconstitutional_headless_fourth_branch).
narrative_ontology:cs_axiom_status(independent_agencies_are_unconstitutional_headless_fourth_branch, holdable).
narrative_ontology:cs_axiom_grounding('7c4ec327-39e2-44f0-af50-45b1f8aa44ec', independent_agencies_are_unconstitutional_headless_fourth_branch, conventional).
narrative_ontology:cs_reference_frame('7c4ec327-39e2-44f0-af50-45b1f8aa44ec', article_ii_original_public_meaning_unified_executive).
narrative_ontology:cs_drift_state('7c4ec327-39e2-44f0-af50-45b1f8aa44ec', contemporary_administrative_state, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('7c4ec327-39e2-44f0-af50-45b1f8aa44ec', '').
narrative_ontology:cs_kernel_id(separation_of_powers_text__unitary_executive_reading, separation_of_powers_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(separation_of_powers_text__unitary_executive_reading, executive_branch).
narrative_ontology:constraint_victim(separation_of_powers_text__unitary_executive_reading, independent_regulatory_agencies).
narrative_ontology:constraint_victim(separation_of_powers_text__unitary_executive_reading, congress).
narrative_ontology:constraint_victim(separation_of_powers_text__unitary_executive_reading, federal_judiciary).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Asserts that Article II vests all executive power in the President and advances judicial doctrines to invalidate or subordinate independent agencies; gains unified removal power and direct control over the administrative state through unitary-executive litigation and presidential directives.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, executive_branch, agenda_setter,
    institutional, generational, arbitrage, national).

% Operate under statutory independence (FTC, NLRB, Fed) with multi-member leadership and for-cause removal protections; the unitary reading invalidates these structures, forcing subordination to presidential direction or structural dissolution.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, independent_regulatory_agencies, payer,
    institutional, generational, trapped, national).

% Enacts delegation and agency-independence statutes that the unitary reading treats as unconstitutional; loses legislative capacity to design administrative architecture insulated from direct presidential removal and control.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, congress, payer,
    institutional, generational, constrained, national).

% Pressed to invalidate longstanding independent agencies and for-cause removal statutes under Article II; loses administrative-law flexibility and deference discretion as executive-branch gains come at the expense of judicial autonomy in separation-of-powers cases.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, federal_judiciary, payer,
    institutional, generational, constrained, national).

% Produce historical and functionalist critiques of the unitary executive theory; their institutionalist arguments are overridden by the vesting-clause formalism advanced by the reading, though they remain active in amicus and academic discourse.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, administrative_law_scholars, observer,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(separation_of_powers_text__unitary_executive_reading, executive_branch).
narrative_ontology:fixing_cost_class(separation_of_powers_text__unitary_executive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Concentrates all executive authority under a single elected President to eliminate fragmented, unaccountable administration and ensure unified democratic control over the federal bureaucracy.
% TRANSFER_FUNCTION: Moves administrative and regulatory control from congressionally insulated independent agencies and the federal judiciary's discretionary administrative law to the President and the executive office.
% ABSENT_VOICES: Future administrations that might prefer agency independence, and the diffuse public beneficiaries of non-politicized regulation (e.g., monetary stability, antitrust enforcement, labor protection), are not directly represented in constitutional litigation; their interests are mediated by agency counsel who are structurally subordinated in this frame.
% DISAPPEARANCE_RATIONALE: If the unitary executive reading vanished as a controlling legal principle, independent agencies would retain or regain for-cause removal protections, Congress would reassert broader delegation authority, and the executive branch would lose its constitutional trump to direct or dismantle the administrative state; federal regulatory architecture would shift toward dispersed, multi-principal accountability.
% FOUNDING_PROBLEM: The Founders sought to prevent a fragmented, multi-headed executive like the colonial councils and to ensure energy and accountability in administration by placing all executive power in a single President.
% FOUNDING_PROBLEM_CORROBORATION: Originalist historians and unitary-executive legal scholars attest the founding problem through Federalist No. 70 and the Constitutional Convention. Administrative historians and functionalist jurists attest that the problem is anachronistically framed and that the post-1787 development of independent agencies was accepted by early Congresses; no neutral institutional voice resolves the dispute, and corroboration is split across opposed seats.
narrative_ontology:disappearance_verdict(separation_of_powers_text__unitary_executive_reading, world_rearranges).
narrative_ontology:founding_problem_status(separation_of_powers_text__unitary_executive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(separation_of_powers_text__unitary_executive_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(separation_of_powers_text__unitary_executive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(separation_of_powers_text__unitary_executive_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(separation_of_powers_text__unitary_executive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(separation_of_powers_text__unitary_executive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(separation_of_powers_text__unitary_executive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the reading strips congressionally created independence from agencies and concentrates control in the executive. Suppression (0.68) is moderate-high because the constraint's persistence requires active judicial enforcement to invalidate longstanding statutes and agency structures. Theater ratio (0.45) reflects that originalist fidelity rhetoric performs constitutional restoration while functioning as power consolidation. Accessibility collapse (0.60) is moderate: alternatives (independent agencies) are legally disfavored but historically and functionally entrenched. Resistance (0.70) is high because the administrative state, Congress, and the legal academy actively contest the reading. Temporal measurements show monotonic drift from 1980s doctrinal emergence through the present Court's aggressive removal-power jurisprudence.
 *
 * PERSPECTIVAL GAP:
 *   The executive branch experiences this constraint as restoring constitutional order and democratic accountability; from this seat the reading is rope-like coordination. Independent agencies and Congress experience it as an extraction of their constitutional powers; from these seats it is snare-like. The federal judiciary experiences a dual pressure: textual fidelity pulls toward enforcement, while institutional self-interest resists the loss of administrative-law discretion. The engine computes this divergence from the structural data rather than adjudicating it.
 *
 * DIRECTIONALITY LOGIC:
 *   The executive_branch is the structural beneficiary (low d, near full subsidy): it gains removal power, control over administration, and constitutional trump authority. Independent_regulatory_agencies, congress, and federal_judiciary are structural targets (high d, amplified extraction): each loses institutional autonomyâagencies lose insulation, Congress loses delegation design space, and the judiciary loses doctrinal flexibility. The directionality derives directly from the beneficiary and victim declarations paired with exit options; the executive can abandon or modulate the theory (arbitrage), while agencies are trapped, and Congress and the judiciary are constrained by the constitutional framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The coordination functionâdemocratic accountability through a single elected executiveâprevents classification as pure snare. The reading genuinely solves a delegation-and-accountability problem in democratic theory. However, the asymmetric extractionâstripping independence only from agencies that regulate the executive's political and economic alliesâprevents classification as pure rope. The R5 genealogy shows the founding problem (energetic unified executive) is contested and arguably anachronistic for the modern administrative state, creating a mandatrophy risk if the reading is treated as settled original meaning rather than active doctrinal contestation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutional_text_vs_constructed_doctrine,
    'Is the unitary executive principle an inherent structural feature of Article II, or a modern doctrinal construction that concentrates power?',
    'Comparative historical analysis of executive-branch design in the early Republic versus the modern administrative state; judicial adoption or rejection in pending removal-power cases.',
    'If inherent text, classification leans toward commitment-system mountain; if constructed doctrine with identifiable partisan beneficiaries, classification confirms tangled_rope or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_text_vs_constructed_doctrine, conceptual, 'Whether the reading tracks natural law of the text or is a constructed power-consolidation mechanism').

omega_variable(
    independent_agency_efficiency_tradeoff,
    'Does subordinating independent agencies to presidential control improve administrative efficiency and democratic accountability, or degrade regulatory expertise and long-term stability?',
    'Empirical studies of agency output under political control versus insulation across comparable regulatory domains.',
    'If efficiency gains are real and broadly distributed, the coordination function strengthens; if degradation is severe, extraction dominates and the victim set expands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(independent_agency_efficiency_tradeoff, empirical, 'Whether agency subordination produces genuine coordination benefits or pure power extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(separation_of_powers_text__unitary_executive_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sep_pow_ue_tr_t0, separation_of_powers_text__unitary_executive_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(sep_pow_ue_tr_t5, separation_of_powers_text__unitary_executive_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement(sep_pow_ue_tr_t10, separation_of_powers_text__unitary_executive_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement(sep_pow_ue_tr_t15, separation_of_powers_text__unitary_executive_reading, theater_ratio, 15, 0.36).
narrative_ontology:measurement(sep_pow_ue_tr_t20, separation_of_powers_text__unitary_executive_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(sep_pow_ue_tr_t25, separation_of_powers_text__unitary_executive_reading, theater_ratio, 25, 0.43).
narrative_ontology:measurement(sep_pow_ue_tr_t30, separation_of_powers_text__unitary_executive_reading, theater_ratio, 30, 0.45).

% Extraction over time
narrative_ontology:measurement(sep_pow_ue_be_t0, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sep_pow_ue_be_t5, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(sep_pow_ue_be_t10, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(sep_pow_ue_be_t15, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 15, 0.55).
narrative_ontology:measurement(sep_pow_ue_be_t20, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(sep_pow_ue_be_t25, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement(sep_pow_ue_be_t30, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 30, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(sep_pow_ue_su_t0, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(sep_pow_ue_su_t5, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 5, 0.48).
narrative_ontology:measurement(sep_pow_ue_su_t10, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement(sep_pow_ue_su_t15, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 15, 0.58).
narrative_ontology:measurement(sep_pow_ue_su_t20, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(sep_pow_ue_su_t25, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 25, 0.66).
narrative_ontology:measurement(sep_pow_ue_su_t30, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 30, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(separation_of_powers_text__unitary_executive_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(separation_of_powers_text__unitary_executive_reading, formalist_reading).
narrative_ontology:affects_constraint(separation_of_powers_text__unitary_executive_reading, functionalist_reading).

% DUAL FORMULATION NOTE:
% The separation-of-powers text decomposes into three structurally distinct readings: formalist (strict non-delegation), functionalist (flexible overlapping authority), and unitary-executive (absolute presidential control of all executive power). Each reading has a different epsilon, beneficiary/victim structure, and institutional seat map. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
