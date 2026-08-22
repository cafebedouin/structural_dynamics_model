% ============================================================================
% CONSTRAINT STORY: end_of_life_authority__slippery_slope_mechanism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: End-of-Life Autonomy Framework Expansion Mechanism
 *   domain: medical ethics/bioethics/end-of-life policy
 *
 * SUMMARY:
 *   This constraint instantiates the slippery_slope_mechanism reading of the
 *   end_of_life_authority kernel. The kernel concerns who has legitimate
 *   authority to end a human life and under what conditions. The
 *   autonomy_reading frames this as an individual right of the competent
 *   sufferer; the sanctity_reading frames all intentional life-ending as
 *   prohibited. This reading tracks the empirical institutional mechanism
 *   whereby autonomy-based frameworks, initially legislated for narrow
 *   terminal and competent populations, expand through interpretive drift to
 *   include incompetent and non-terminal patients. The expected structural
 *   delta is realized: incompetent patients enter the victim set, eligibility
 *   criteria drift from terminal-only to chronic suffering, and the autonomy
 *   framework becomes the vehicle through which sanctity concerns (about
 *   broad killing) are realized. This is a kernel-reading constraint authored
 *   under the committer frame.
 *
 * KEY AGENTS:
 *   - Competent terminal patients: Primary beneficiaries (powerless/constrained) â gain autonomous choice pathway
 *   - Incompetent patients: Primary targets (powerless/trapped) â subjected to surrogate life-ending decisions without consent
 *   - Chronic non-terminal patients: Secondary targets (moderate/constrained) â eligibility drift captures them under suffering-based criteria
 *   - Medical ethics institutions: Agenda setters (institutional/constrained) â interpret and expand criteria, administer the framework
 *   - Assisted dying advocates: Beneficiaries (organized/mobile) â achieve broader access through drift
 *   - Disability rights groups: Excluded voice (organized/constrained) â resist expansion but are sidelined in autonomy-framed debate
 *   - Sanctity advocates: Analytical observers (organized/constrained) â document gap between promised limits and practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_authority__slippery_slope_mechanism, 0.68).
domain_priors:suppression_score(end_of_life_authority__slippery_slope_mechanism, 0.72).
domain_priors:theater_ratio(end_of_life_authority__slippery_slope_mechanism, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, extractiveness, 0.68).
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_authority__slippery_slope_mechanism, tangled_rope).
narrative_ontology:human_readable(end_of_life_authority__slippery_slope_mechanism, "End-of-Life Autonomy Framework Expansion Mechanism").
narrative_ontology:topic_domain(end_of_life_authority__slippery_slope_mechanism, "medical ethics/bioethics/end-of-life policy").

domain_priors:requires_active_enforcement(end_of_life_authority__slippery_slope_mechanism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_authority__slippery_slope_mechanism, '66e1c2f4-5802-4ed4-841f-7519728cf645').
narrative_ontology:cs_kernel_codification('66e1c2f4-5802-4ed4-841f-7519728cf645', formalized).
narrative_ontology:cs_authority_grounding('66e1c2f4-5802-4ed4-841f-7519728cf645', lineage).
narrative_ontology:cs_interpretation_layer_present('66e1c2f4-5802-4ed4-841f-7519728cf645').
narrative_ontology:cs_reading_relation('66e1c2f4-5802-4ed4-841f-7519728cf645', end_of_life_authority__autonomy_reading, influences).
narrative_ontology:cs_reading_relation('66e1c2f4-5802-4ed4-841f-7519728cf645', end_of_life_authority__sanctity_reading, coexists_with).
narrative_ontology:cs_axiom('66e1c2f4-5802-4ed4-841f-7519728cf645', foundational, eligibility_creep_inevitable).
narrative_ontology:cs_axiom_status(eligibility_creep_inevitable, holdable).
narrative_ontology:cs_axiom_grounding('66e1c2f4-5802-4ed4-841f-7519728cf645', eligibility_creep_inevitable, empirically_contingent).
narrative_ontology:cs_axiom('66e1c2f4-5802-4ed4-841f-7519728cf645', secondary, terminal_boundary_unenforceable).
narrative_ontology:cs_axiom_status(terminal_boundary_unenforceable, holdable).
narrative_ontology:cs_axiom_grounding('66e1c2f4-5802-4ed4-841f-7519728cf645', terminal_boundary_unenforceable, empirically_contingent).
narrative_ontology:cs_reference_frame('66e1c2f4-5802-4ed4-841f-7519728cf645', terminal_competent_autonomy).
narrative_ontology:cs_drift_state('66e1c2f4-5802-4ed4-841f-7519728cf645', post_legalization_expansion_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('66e1c2f4-5802-4ed4-841f-7519728cf645', '').
narrative_ontology:cs_kernel_id(end_of_life_authority__slippery_slope_mechanism, end_of_life_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_authority__slippery_slope_mechanism, competent_terminal_patients).
narrative_ontology:constraint_beneficiary(end_of_life_authority__slippery_slope_mechanism, medical_ethics_institutions).
narrative_ontology:constraint_beneficiary(end_of_life_authority__slippery_slope_mechanism, assisted_dying_advocates).
narrative_ontology:constraint_victim(end_of_life_authority__slippery_slope_mechanism, incompetent_patients).
narrative_ontology:constraint_victim(end_of_life_authority__slippery_slope_mechanism, chronic_non_terminal_patients).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Face a terminal prognosis and retain decisional capacity. They gain a legal pathway to choose the timing of death through medically assisted frameworks, reducing prolonged suffering. They cannot opt out of the broader cultural shift the framework creates, but directly benefit from its availability for their specific circumstance.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, competent_terminal_patients, beneficiary,
    powerless, biographical, constrained, national).

% Lack decisional capacity due to dementia, coma, or developmental disability. They become subject to life-ending decisions made by surrogates or advance directive interpretation under frameworks originally designed for competent choosers. They cannot protest, appeal, or reframe the criteria that include them.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, incompetent_patients, payer,
    powerless, immediate, trapped, national).

% Experience severe chronic suffering without a terminal diagnosis. In jurisdictions where eligibility drifts, they become candidates for life-ending procedures under suffering-based rather than terminal criteria. They retain some capacity to refuse but face institutional pressure to qualify for the expanded framework rather than receive long-term support.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, chronic_non_terminal_patients, payer,
    moderate, biographical, constrained, national).

% Hospital ethics committees, review boards, and regulatory bodies that interpret eligibility criteria, approve cases, and manage the interface between law and clinical practice. They expand their jurisdiction as criteria loosen, moving from terminal to chronic cases and from competent to surrogate decision-making.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, medical_ethics_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Organizations and clinicians campaigning for and implementing assisted dying frameworks. They benefit from each successful expansion of eligibility as validation of the autonomy principle and broadening of lawful access, though they do not directly administer enforcement.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, assisted_dying_advocates, beneficiary,
    organized, generational, mobile, national).

% Collectives representing people with disabilities who experience chronic conditions. They argue that expanding eligibility to non-terminal suffering devalues disabled lives and pressures vulnerable people toward death rather than support. They are routinely sidelined in policy debates framed exclusively around individual autonomy.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, disability_rights_groups, excluded,
    organized, generational, constrained, national).

% Religious and philosophical communities holding that human life has intrinsic value regardless of condition. They observe the empirical expansion of frameworks and document the gap between initial promises of strict limits and actual practice, without being direct targets or beneficiaries of the medical mechanism.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, sanctity_advocates, observer,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a structured, legally authorized pathway for terminally ill, competent patients to exercise control over the timing and manner of death, reducing arbitrary suffering and unregulated physician behavior.
% TRANSFER_FUNCTION: Transfers authority over life-ending decisions from blanket prohibition to individualized medical-ethical review; transfers risk of wrongful death from the state to expanded patient categories as criteria drift from terminal-only to chronic and incompetent populations.
% ABSENT_VOICES: Disability rights advocates who resist expansion to non-terminal conditions; sanctity-of-life advocates who reject the entire framework; and incompetent patients themselves, who cannot speak in the policy debate but are affected by surrogate decision-making standards.
% DISAPPEARANCE_RATIONALE: Medical institutions, patients, and families have reorganized around the framework's availability; its disappearance would eliminate authorized pathways, force legal reconsideration of aiding death, and remove the interpretive infrastructure that currently manages end-of-life decisions.
% FOUNDING_PROBLEM: Competent patients facing unbearable terminal suffering were denied agency over their death, forced into prolonged agony or covert, unregulated euthanasia.
% FOUNDING_PROBLEM_CORROBORATION: Medical historians and palliative care scholars from outside the assisted-dying advocacy community attest that covert, unregulated end-of-life practices caused suffering. However, disability rights researchers and sanctity advocates contest that the current framework remains limited to its founding problem, citing empirical expansion to non-terminal and incompetent populations.
narrative_ontology:disappearance_verdict(end_of_life_authority__slippery_slope_mechanism, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_authority__slippery_slope_mechanism, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_authority__slippery_slope_mechanism, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(end_of_life_authority__slippery_slope_mechanism, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_authority__slippery_slope_mechanism, 0.68, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.68) is high because the framework extracts life-years and protective boundaries from incompetent and non-terminal populations who did not autonomously choose to enter it. Suppression (0.72) is higher because the expansion persists only through active enforcement of broadened criteria and suppression of sanctity-based and disability-rights alternatives. Theater_ratio (0.55) reflects performative safeguards: nominal requirements of terminal diagnosis or competent consent are maintained as formal rules while functionally eroded through interpretive expansion. Accessibility_collapse (0.78) is high because once the autonomy framework is institutionalized, the alternative of robust sanctity protection collapses in policy discourse. Resistance (0.58) captures ongoing but structurally marginalized opposition from sanctity and disability advocates. The measurement series shows monotonic drift over 25 years: extractiveness, theater, and suppression_requirement all rise as criteria loosen, reflecting the slippery slope mechanism.
 *
 * PERSPECTIVAL GAP:
 *   The competent terminal patient experiences the constraint as liberating coordination that solves the problem of forced suffering. The incompetent patient experiences the identical framework as a lethal threat imposed by surrogates. The chronic non-terminal patient experiences it as institutional pressure to qualify for death rather than long-term care. Medical ethics institutions experience it as a rational, evolving framework for difficult decisions. The engine computes these divergences from the same structural data: beneficiary declarations for those who choose and receive the coordination; victim declarations for those drawn in without autonomous consent.
 *
 * DIRECTIONALITY LOGIC:
 *   Competent terminal patients are declared beneficiaries with constrained exit â their directionality sits near the beneficiary end, yielding damped effective extraction. Incompetent patients are declared victims with trapped exit and minimal power â their directionality sits near the full-target end, amplifying effective extraction. Chronic non-terminal patients are victims with constrained exit â high directionality, though slightly less than the fully trapped. Medical ethics institutions and assisted dying advocates are beneficiaries, with low directionality. The derivation requires no override: the structural declarations plus exit options already produce the correct asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â unregulated suffering of competent terminal patients denied agency â has been substantially solved by the initial framework. However, the arrangement persists and expands beyond its mandate, now extracting from populations not part of the original problem. This prevents mislabeling as pure rope (there are clear victims of expansion) and prevents mislabeling as pure snare (there remains a genuine coordination function for the original beneficiary population). The R5 genealogy flags a live contested status: founding problem corroboration exists from outside the beneficiary set, but those same external voices attest that the framework has exceeded its mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is the slippery_slope_mechanism reading of the end_of_life_authority kernel. How would classification change if the autonomy_reading (individual self-determination bounded to competent choice) or sanctity_reading (inviolability of life) were adopted as the operative framing?',
    'Comparative analysis across the three sibling constraints in the kernel family, examining how the same institutional phenomena map to different beneficiary/victim structures under each reading.',
    'Under autonomy_reading, the expansion may read as legitimate broadening of rights rather than extraction; under sanctity_reading, the entire framework reads as snare. The slippery slope reading isolates the expansion mechanism as structurally distinct.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Sibling reading relationship and framing under-determination for this kernel').

omega_variable(
    expansion_intentionality,
    'Is the expansion of eligibility criteria to incompetent and non-terminal populations an intentional strategy by framework advocates, or an emergent institutional drift driven by interpretive logic and case-by-case precedent?',
    'Archival and discourse analysis of legislative history, court decisions, and medical ethics committee protocols in jurisdictions with mature assisted-dying frameworks.',
    'If intentional, the mechanism is designed extraction layered onto coordination; if emergent, it is drift toward tangled rope through institutional creep. This affects the theater_ratio and mandatrophy classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(expansion_intentionality, empirical, 'Whether expansion is strategic or emergent').

omega_variable(
    incompetent_patient_autonomy_possibility,
    'Can substitute decision-making or advance directives for incompetent patients ever realize genuine autonomy, or does this category inevitably become extractive by construction within an autonomy framework?',
    'Comparative outcome studies of incompetent patients under regimes with and without assisted dying frameworks, measuring rates of non-voluntary life-ending and surrogate decision quality.',
    'If genuine autonomy is structurally impossible for this group, their inclusion in the framework is extractive by design, strengthening the victim classification and raising effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incompetent_patient_autonomy_possibility, conceptual, 'Whether incompetent patient inclusion is inherently extractive').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_authority__slippery_slope_mechanism, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eol_slippery_slope_tr_t0, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 0, 0.2).
narrative_ontology:measurement(eol_slippery_slope_tr_t5, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 5, 0.25).
narrative_ontology:measurement(eol_slippery_slope_tr_t10, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 10, 0.32).
narrative_ontology:measurement(eol_slippery_slope_tr_t15, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 15, 0.4).
narrative_ontology:measurement(eol_slippery_slope_tr_t20, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 20, 0.47).
narrative_ontology:measurement(eol_slippery_slope_tr_t25, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 25, 0.55).

% Extraction over time
narrative_ontology:measurement(eol_slippery_slope_be_t0, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(eol_slippery_slope_be_t5, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(eol_slippery_slope_be_t10, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(eol_slippery_slope_be_t15, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 15, 0.56).
narrative_ontology:measurement(eol_slippery_slope_be_t20, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 20, 0.61).
narrative_ontology:measurement(eol_slippery_slope_be_t25, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(eol_slippery_slope_su_t0, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(eol_slippery_slope_su_t5, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 5, 0.48).
narrative_ontology:measurement(eol_slippery_slope_su_t10, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 10, 0.56).
narrative_ontology:measurement(eol_slippery_slope_su_t15, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 15, 0.64).
narrative_ontology:measurement(eol_slippery_slope_su_t20, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(eol_slippery_slope_su_t25, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 25, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_authority__slippery_slope_mechanism, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(end_of_life_authority__slippery_slope_mechanism, 0.1).
narrative_ontology:affects_constraint(end_of_life_authority__slippery_slope_mechanism, autonomy_reading).
narrative_ontology:affects_constraint(end_of_life_authority__slippery_slope_mechanism, sanctity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the end_of_life_authority kernel. It decomposes the natural-language debate over end-of-life authority into three structurally distinct claims: autonomy_reading (coordination through self-determination), sanctity_reading (prohibition on intentional life-ending), and slippery_slope_mechanism (empirical expansion of autonomy frameworks to unintended populations). Each reading carries a different epsilon, beneficiary/victim structure, and classification. They are linked as a constraint family through mutual affects_constraints edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
