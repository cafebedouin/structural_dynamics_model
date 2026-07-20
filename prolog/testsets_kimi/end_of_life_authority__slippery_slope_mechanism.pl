% ============================================================================
% CONSTRAINT STORY: end_of_life_authority__slippery_slope_mechanism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_end_of_life_authority__slippery_slope_mechanism, []).

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
 *   constraint_id: end_of_life_authority__slippery_slope_mechanism
 *   human_readable: End-of-Life Authority Slippery-Slope Expansion
 *   domain: medical ethics/bioethics/end-of-life policy
 *
 * SUMMARY:
 *   This constraint story instantiates the slippery_slope_mechanism reading
 *   of the end_of_life_authority kernel. In jurisdictions that have legalized
 *   medically assisted dying on autonomy grounds for competent terminal
 *   patients, the framework has empirically expanded to include incompetent
 *   patients (via advance directives and substituted judgment) and
 *   non-terminal chronically ill populations (via unbearable suffering
 *   criteria). The constraint is the operative legal-medical framework as it
 *   functions after this expansion. The autonomy rhetoric that legitimized
 *   the kernel persists, but it now governs populations who cannot exercise
 *   the autonomy that grounds the framework's legitimacy. This creates a
 *   structural divergence between the coordination function (dignified death
 *   for competent terminal adults) and the extraction function (removal of
 *   protective barriers for incompetent and chronically ill populations).
 *
 * KEY AGENTS:
 *   - competent_terminal_patients: Primary beneficiary (powerless/immediate/constrained) â receives coordinated access to dignified death
 *   - incompetent_patients: Primary victim (powerless/immediate/trapped) â drawn into scope through proxy decision-making, cannot exercise autonomy
 *   - chronically_ill_non_terminal: Secondary victim (powerless/biographical/constrained) â eligibility drifted from terminality to chronic suffering
 *   - medical_gatekeepers: Agenda setter (institutional/generational/constrained) â administers and enforces expanding eligibility criteria
 *   - disability_rights_advocates: Excluded voice (organized/generational/mobile) â objects to expansion but marginalized in autonomy-framed discourse
 *   - sanctity_advocates: Excluded voice (organized/generational/mobile) â opposes framework on dignity grounds, treated as sectarian
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_authority__slippery_slope_mechanism, 0.72).
domain_priors:suppression_score(end_of_life_authority__slippery_slope_mechanism, 0.68).
domain_priors:theater_ratio(end_of_life_authority__slippery_slope_mechanism, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, extractiveness, 0.72).
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_authority__slippery_slope_mechanism, tangled_rope).
narrative_ontology:human_readable(end_of_life_authority__slippery_slope_mechanism, "End-of-Life Authority Slippery-Slope Expansion").
narrative_ontology:topic_domain(end_of_life_authority__slippery_slope_mechanism, "medical ethics/bioethics/end-of-life policy").

domain_priors:requires_active_enforcement(end_of_life_authority__slippery_slope_mechanism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_authority__slippery_slope_mechanism, 'affc1264-55b7-4756-8c01-e85bf7f10cd6').
narrative_ontology:cs_kernel_codification('affc1264-55b7-4756-8c01-e85bf7f10cd6', formalized).
narrative_ontology:cs_authority_grounding('affc1264-55b7-4756-8c01-e85bf7f10cd6', lineage).
narrative_ontology:cs_interpretation_layer_present('affc1264-55b7-4756-8c01-e85bf7f10cd6').
narrative_ontology:cs_reading_relation('affc1264-55b7-4756-8c01-e85bf7f10cd6', end_of_life_authority__autonomy_reading, influences).
narrative_ontology:cs_reading_relation('affc1264-55b7-4756-8c01-e85bf7f10cd6', end_of_life_authority__sanctity_reading, coexists_with).
narrative_ontology:cs_axiom('affc1264-55b7-4756-8c01-e85bf7f10cd6', foundational, eligibility_expansion_harms_vulnerable).
narrative_ontology:cs_axiom_status(eligibility_expansion_harms_vulnerable, holdable).
narrative_ontology:cs_axiom_grounding('affc1264-55b7-4756-8c01-e85bf7f10cd6', eligibility_expansion_harms_vulnerable, empirically_contingent).
narrative_ontology:cs_reference_frame('affc1264-55b7-4756-8c01-e85bf7f10cd6', narrow_statutory_boundary).
narrative_ontology:cs_drift_state('affc1264-55b7-4756-8c01-e85bf7f10cd6', expanded_eligibility_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('affc1264-55b7-4756-8c01-e85bf7f10cd6', '').
narrative_ontology:cs_kernel_id(end_of_life_authority__slippery_slope_mechanism, end_of_life_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_authority__slippery_slope_mechanism, competent_terminal_patients).
narrative_ontology:constraint_victim(end_of_life_authority__slippery_slope_mechanism, incompetent_patients).
narrative_ontology:constraint_victim(end_of_life_authority__slippery_slope_mechanism, chronically_ill_non_terminal).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Face terminal illness and seek legal, medically supervised control over the timing and manner of death. They navigate gatekeeping assessments to receive authorization. They benefit from a regulated pathway but cannot access it outside the formal framework.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, competent_terminal_patients, beneficiary,
    powerless, immediate, constrained, national).

% Cannot exercise autonomous choice due to cognitive incapacity, yet are drawn into the framework through advance directive interpretation, substituted judgment, or proxy decision-making. They bear the cost of losing protective barriers against life-ending authority without ability to contest application.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, incompetent_patients, payer,
    powerless, immediate, trapped, national).

% Live with chronic, severe suffering but are not terminal. Eligibility criteria drifted to include their condition, exposing them to state-sanctioned life-ending options. They face subtle and overt pressure to relieve caregiving burdens, and the existence of the framework reshapes their care environment.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, chronically_ill_non_terminal, payer,
    powerless, biographical, constrained, national).

% Physicians, review boards, and legal oversight bodies that administer eligibility assessments, interpret statutory criteria, and authorize or refuse life-ending interventions. They control the boundary between terminal and chronic, competent and incompetent, through professional discretion and institutional protocols.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, medical_gatekeepers, agenda_setter,
    institutional, generational, constrained, national).

% Argue that expanded eligibility criteria devalue disabled and chronically ill lives and that unbearable suffering standards embed discriminatory assumptions. They are structurally marginalized in the autonomy-framed discourse and treated as external to the medical-legal conversation.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, disability_rights_advocates, excluded,
    organized, generational, mobile, national).

% Oppose state-sanctioned life-ending on grounds of intrinsic human dignity. Their objections are dismissed as sectarian or non-autonomy-based within the framework's legitimizing discourse, excluding them from effective participation in eligibility design.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, sanctity_advocates, excluded,
    organized, generational, mobile, national).

narrative_ontology:fixing_cost_class(end_of_life_authority__slippery_slope_mechanism, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a structured, legal-medical pathway for competent adults facing terminal illness to control the timing and manner of death, replacing covert, unregulated, or violent practices with a supervised, accountable process.
% TRANSFER_FUNCTION: Moves authority over life-ending interventions from natural death and prohibition to medical-legal gatekeeping, while transferring the burden of expanded eligibility onto incompetent and chronically ill populations who were not part of the original coordination problem.
% ABSENT_VOICES: Incompetent patients who cannot speak to their own interests; disability rights advocates who see expanded criteria as devaluation of disabled life; and sanctity advocates whose dignity-based objections are excluded as non-autonomy-based.
% DISAPPEARANCE_RATIONALE: If the expanded framework vanished, competent terminal patients would lose a regulated pathway and potentially return to covert practices; incompetent and chronically ill populations would lose the looming threat of proxy-eligibility but also whatever nominal protections the framework provides. The medical-legal gatekeeping apparatus would reorganize around palliative-only models, and the broader healthcare culture would shift its framing of chronic suffering.
% FOUNDING_PROBLEM: Uncontrolled, covert, and often violent deaths among terminally ill competent adults who sought relief from unbearable suffering but lacked legal or medical authorization, leading to botched suicides, underground practices, and unregulated suffering.
% FOUNDING_PROBLEM_CORROBORATION: Palliative care physicians and hospice organizations outside the direct beneficiary set attest to the historical reality of uncontrolled terminal suffering. Disability rights advocates and independent bioethicists corroborate the original problem's existence but attest that the arrangement has shifted function, using the founding problem to justify expansion to non-terminal populations.
narrative_ontology:disappearance_verdict(end_of_life_authority__slippery_slope_mechanism, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_authority__slippery_slope_mechanism, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_authority__slippery_slope_mechanism, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(end_of_life_authority__slippery_slope_mechanism, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_authority__slippery_slope_mechanism, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(end_of_life_authority__slippery_slope_mechanism_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(end_of_life_authority__slippery_slope_mechanism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(end_of_life_authority__slippery_slope_mechanism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the framework has expanded beyond its legitimizing population to extract protective barriers from incompetent and chronically ill people who were not part of the initial coordination problem. Suppression (0.68) reflects the active medical-legal enforcement required to maintain gatekeeping and exclude palliative-only alternatives for the expanded population. Theater_ratio (0.45) captures the growing gap between autonomy rhetoric and the reality of proxy decisions for incompetent patients. Accessibility_collapse (0.65) registers that once the expanded framework is normalized, pure palliative alternatives lose institutional support. Resistance (0.75) is high due to sustained opposition from disability rights and sanctity advocates. The measurement series tracks the drift from narrow terminal-competent access (T=0) to broad chronic-and-incompetent application (T=24).
 *
 * PERSPECTIVAL GAP:
 *   The competent_terminal_patient seat experiences the constraint as a rope â a hard-won coordination mechanism that solves the problem of uncontrolled dying. The incompetent_patient seat experiences the same constraint as a snare â a mechanism that uses autonomy language to erode their protective barriers. The engine computes this divergence from the structural data: same constraint, opposite directionalities. The agenda_setter seat (medical_gatekeepers) may experience it as legitimate professional practice, while excluded observers see extraction. The claim/metric independence is maintained: the constraint is claimed as tangled_rope because both coordination and extraction are structurally present, while the metrics describe the high-extraction end-state after expansion.
 *
 * DIRECTIONALITY LOGIC:
 *   Competent_terminal_patients sit near the beneficiary end: the constraint was built to coordinate their access to dignified death, and they experience it as subsidy. Incompetent_patients and chronically_ill_non_terminal sit near the full-target end: the constraint extracts protective barriers and exposes them to state-sanctioned life-ending authority they cannot meaningfully refuse. Medical_gatekeepers sit near symmetric: they wield substantial power within the framework but are also bound by professional and legal obligations that constrain their exit. Their directionality is structurally closer to administrator than capturer because no specific gain flows to them.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents mislabeling the initial autonomy framework as pure extraction (it did solve a genuine coordination problem for competent terminal patients) while also preventing mislabeling the expanded framework as pure coordination (it now extracts from populations who cannot exercise autonomy). If the founding problem were truly solved and the framework had merely persisted without expansion, it might be a piton; but the active expansion of eligibility criteria indicates ongoing institutional agency, not inertia. If it lacked the initial coordination function, it would be a snare; the presence of genuine beneficiaries in the terminal population makes tangled_rope the accurate structural classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    eligibility_expansion_driver,
    'Is the expansion of eligibility criteria driven by organic medical practice drift, deliberate legislative amendment, or judicial interpretation?',
    'Comparative jurisdiction analysis tracking whether expansion originated in legislatures, courts, or medical gatekeeper practice, and whether each pathway produced different victim profiles.',
    'If legislative, expansion is democratically accountable and may read as rope-to-scaffold drift; if practice drift or judicial, it reads as tangled-rope extraction through interpretive capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(eligibility_expansion_driver, empirical, 'Whether expansion is driven by legislation, courts, or medical practice drift.').

omega_variable(
    autonomy_rhetoric_function,
    'Does the autonomy framework''s expansion represent well-intentioned coordination that slipped its bounds, or was the narrow initial framing always strategic cover for broader application?',
    'Historical analysis of legislative intent, advocacy discourse, and statutory text at founding versus subsequent amendment trajectories.',
    'If cover, the constraint is a snare; if drift, tangled rope. This resolves whether the theater_ratio registers genuine function loss or revealed true function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(autonomy_rhetoric_function, conceptual, 'Whether expansion is revealed true function or coordination drift.').

omega_variable(
    incompetent_patient_victim_status,
    'Are incompetent patients genuinely victims of expanded life-ending authority, or does substituted judgment and advance-directive fidelity protect their authentic interests?',
    'Outcome studies comparing incompetent patients in expanded-eligibility jurisdictions versus palliative-only jurisdictions, tracking rates of non-voluntary life-ending and proxy decision quality.',
    'If harmed, the victim designation and extraction metrics are validated; if protected, the victim set may be overstated and extraction should be revised downward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incompetent_patient_victim_status, empirical, 'Whether incompetent patients are genuinely extracted from or protected by the framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_authority__slippery_slope_mechanism, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eol_slippery_slope_tr_t0, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 0, 0.18).
narrative_ontology:measurement(eol_slippery_slope_tr_t4, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 4, 0.22).
narrative_ontology:measurement(eol_slippery_slope_tr_t8, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 8, 0.28).
narrative_ontology:measurement(eol_slippery_slope_tr_t12, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 12, 0.33).
narrative_ontology:measurement(eol_slippery_slope_tr_t16, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 16, 0.38).
narrative_ontology:measurement(eol_slippery_slope_tr_t20, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 20, 0.42).
narrative_ontology:measurement(eol_slippery_slope_tr_t24, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 24, 0.45).

% Extraction over time
narrative_ontology:measurement(eol_slippery_slope_be_t0, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(eol_slippery_slope_be_t4, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 4, 0.4).
narrative_ontology:measurement(eol_slippery_slope_be_t8, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(eol_slippery_slope_be_t12, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 12, 0.56).
narrative_ontology:measurement(eol_slippery_slope_be_t16, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 16, 0.64).
narrative_ontology:measurement(eol_slippery_slope_be_t20, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 20, 0.69).
narrative_ontology:measurement(eol_slippery_slope_be_t24, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 24, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(eol_slippery_slope_su_t0, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(eol_slippery_slope_su_t4, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 4, 0.52).
narrative_ontology:measurement(eol_slippery_slope_su_t8, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 8, 0.56).
narrative_ontology:measurement(eol_slippery_slope_su_t12, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 12, 0.6).
narrative_ontology:measurement(eol_slippery_slope_su_t16, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 16, 0.64).
narrative_ontology:measurement(eol_slippery_slope_su_t20, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 20, 0.67).
narrative_ontology:measurement(eol_slippery_slope_su_t24, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 24, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_authority__slippery_slope_mechanism, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
