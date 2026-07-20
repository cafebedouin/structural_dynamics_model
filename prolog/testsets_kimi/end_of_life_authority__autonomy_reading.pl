% ============================================================================
% CONSTRAINT STORY: end_of_life_authority__autonomy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_end_of_life_authority__autonomy_reading, []).

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
 *   constraint_id: end_of_life_authority__autonomy_reading
 *   human_readable: End-of-Life Authority: Autonomy Reading
 *   domain: medical ethics/bioethics
 *
 * SUMMARY:
 *   This constraint story models the autonomy reading of the end-of-life
 *   authority kernel: the medico-legal framework that grounds the right to
 *   assisted dying in individual autonomy when facing unbearable suffering.
 *   The constraint coordinates end-of-life practice by providing a legal
 *   pathway, but asymmetrically extracts from patients who fall outside
 *   eligibility criteria and are left to prolonged suffering. It suppresses
 *   paternalistic medical restrictions and shows an empirical pattern of
 *   eligibility expansion over time. The claim is tangled_rope because the
 *   coordination is genuine and necessary for legal clarity, yet the
 *   gatekeeping function produces identifiable victims among the
 *   suffering-prolonged. This is one reading of a contested kernel; sibling
 *   readings include the sanctity reading (intrinsic life value prohibits
 *   intentional ending) and the slippery-slope mechanism reading (autonomy
 *   frameworks expand beyond initial bounds).
 *
 * KEY AGENTS:
 *   - competent_terminal_patients (beneficiary/powerless/constrained): gain legal choice pathway but face procedural burdens
 *   - suffering_prolonged_patients (payer/powerless/trapped): excluded by eligibility criteria and left in prolonged suffering
 *   - medical_gatekeepers (agenda_setter/institutional/constrained): administer assessments, lose paternalistic authority
 *   - patient_autonomy_advocates (beneficiary/organized/constrained): gain institutional vindication of autonomy principles
 *   - disability_rights_advocates (excluded/organized/constrained): oppose framework but excluded from design
 *   - religious_authorities (excluded/organized/constrained): sanctity framework structurally excluded
 *   - bioethics_observers (observer/analytical/analytical): track drift and expansion patterns
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_authority__autonomy_reading, 0.65).
domain_priors:suppression_score(end_of_life_authority__autonomy_reading, 0.78).
domain_priors:theater_ratio(end_of_life_authority__autonomy_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_authority__autonomy_reading, tangled_rope).
narrative_ontology:human_readable(end_of_life_authority__autonomy_reading, "End-of-Life Authority: Autonomy Reading").
narrative_ontology:topic_domain(end_of_life_authority__autonomy_reading, "medical ethics/bioethics").

domain_priors:requires_active_enforcement(end_of_life_authority__autonomy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_authority__autonomy_reading, '42137207-af2b-4c4f-91fc-c50f9b30b84a').
narrative_ontology:cs_kernel_codification('42137207-af2b-4c4f-91fc-c50f9b30b84a', formalized).
narrative_ontology:cs_authority_grounding('42137207-af2b-4c4f-91fc-c50f9b30b84a', expertise).
narrative_ontology:cs_interpretation_layer_present('42137207-af2b-4c4f-91fc-c50f9b30b84a').
narrative_ontology:cs_reading_relation('42137207-af2b-4c4f-91fc-c50f9b30b84a', end_of_life_authority__sanctity_reading, forecloses).
narrative_ontology:cs_reading_relation('42137207-af2b-4c4f-91fc-c50f9b30b84a', end_of_life_authority__slippery_slope_mechanism, influences).
narrative_ontology:cs_axiom('42137207-af2b-4c4f-91fc-c50f9b30b84a', foundational, autonomy_as_trump_over_life_preservation).
narrative_ontology:cs_axiom_status(autonomy_as_trump_over_life_preservation, holdable).
narrative_ontology:cs_axiom_grounding('42137207-af2b-4c4f-91fc-c50f9b30b84a', autonomy_as_trump_over_life_preservation, deontological).
narrative_ontology:cs_reference_frame('42137207-af2b-4c4f-91fc-c50f9b30b84a', patient_sovereignty_at_end_of_life).
narrative_ontology:cs_drift_state('42137207-af2b-4c4f-91fc-c50f9b30b84a', contemporary_expansion_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('42137207-af2b-4c4f-91fc-c50f9b30b84a', '').
narrative_ontology:cs_kernel_id(end_of_life_authority__autonomy_reading, end_of_life_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_authority__autonomy_reading, competent_terminal_patients).
narrative_ontology:constraint_beneficiary(end_of_life_authority__autonomy_reading, patient_autonomy_advocates).
narrative_ontology:constraint_victim(end_of_life_authority__autonomy_reading, suffering_prolonged_patients).
narrative_ontology:constraint_vindicates(end_of_life_authority__autonomy_reading, individual_autonomy_doctrine).
narrative_ontology:constraint_vindicates(end_of_life_authority__autonomy_reading, medical_paternalism_rejection).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Patients who meet strict competence, terminal illness, and unbearable suffering criteria gain a legal right to medical assistance in dying. They must navigate multi-step assessment processes, repeated requests, and waiting periods to exercise autonomy over the timing and manner of death.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, competent_terminal_patients, beneficiary,
    powerless, immediate, constrained, national).

% Patients experiencing unbearable suffering who fall outside eligibility criteriaâdue to non-terminal status, psychiatric conditions, dementia, or procedural barriersâare denied access to assisted dying and remain subjected to prolonged suffering against their will. The framework's gatekeeping function prevents their exit from this suffering.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, suffering_prolonged_patients, payer,
    powerless, immediate, trapped, national).

% Physicians, psychiatrists, and review panels who assess eligibility, certify repeated requests, and administer the assisted-dying protocol. They must follow legally mandated autonomy-centered procedures and suppress traditional paternalistic impulses to override patient choice, losing historical medical authority over end-of-life decisions.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, medical_gatekeepers, agenda_setter,
    institutional, biographical, constrained, national).

% Organizations and movements campaigning for legal recognition of individual control over death timing. They benefit from institutional vindication of autonomy principles and the creation of lawful pathways, though they do not directly administer the constraint or capture its extracted value.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, patient_autonomy_advocates, beneficiary,
    organized, generational, constrained, national).

% Organizations representing disabled persons who argue that autonomy frameworks encode ableist assumptions and devalue life with disability. They are systematically excluded from eligibility design committees and their objections are treated as external to the autonomy paradigm.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, disability_rights_advocates, excluded,
    organized, generational, constrained, national).

% Institutions holding sanctity-of-life doctrines that prohibit intentional life-ending. Their moral framework is structurally excluded from the legal architecture, which suppresses theological objections in favor of secular autonomy principles.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, religious_authorities, excluded,
    organized, civilizational, constrained, national).

% Academic bioethicists and empirical researchers who track eligibility expansion over time, document procedural burdens, and analyze whether the framework genuinely serves autonomy or produces new forms of medical coercion and gatekeeping.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, bioethics_observers, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(end_of_life_authority__autonomy_reading, diffuse).
narrative_ontology:fixing_cost_class(end_of_life_authority__autonomy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a uniform legal and medical protocol for managing requests to end life when patients face unbearable suffering, replacing unregulated or covert practices with a reviewable, standardized process that assigns clear roles to patients, assessors, and administrators.
% TRANSFER_FUNCTION: Moves decision-making authority over the timing and manner of death from traditional medical paternalism and default life-preservation norms to individual patients who satisfy competence and terminal-illness criteria; moves gatekeeping power, liability exposure, and procedural burden to credentialed medical assessors.
% ABSENT_VOICES: Disability rights advocates who argue the framework encodes ableist devaluation of life with impairment; religious authorities holding sanctity-of-life commitments; patients with psychiatric or dementia-related suffering who are categorically excluded from eligibility; and family members who bear witness to prolonged suffering but lack legal standing.
% DISAPPEARANCE_RATIONALE: If the autonomy framework vanished, competent terminal patients would lose legal access to assisted dying and revert to covert or unregulated methods; medical professionals would lose liability shields and procedural guidance, returning to ambiguous legal territory; the medico-legal field would revert to paternalistic decision-making or legislative vacuum, fundamentally rearranging end-of-life practice.
% FOUNDING_PROBLEM: Untreated suffering at the end of life, covert euthanasia without safeguards, and the routine override of competent patient refusal by paternalistic medical authority.
% FOUNDING_PROBLEM_CORROBORATION: Palliative care researchers and some patient advocates attest to the reality of unrelieved suffering. However, disability rights organizations and hospice movements contest that the founding problem requires an autonomy-based assisted-dying solution; they argue improved palliative care addresses suffering without the framework's exclusionary gatekeeping. Corroboration from outside the direct beneficiary set is split.
narrative_ontology:disappearance_verdict(end_of_life_authority__autonomy_reading, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_authority__autonomy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_authority__autonomy_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(end_of_life_authority__autonomy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_authority__autonomy_reading, 0.65, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(end_of_life_authority__autonomy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(end_of_life_authority__autonomy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(end_of_life_authority__autonomy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) reflects that the framework's eligibility gatekeeping actively denies recourse to suffering patients who fail procedural or categorical criteria, imposing a direct cost on them. Suppression (0.78) is high because the constraint actively suppresses paternalistic medical override and alternative ethical frameworks (sanctity, disability-rights) through legal enforcement and professional discipline. Theater ratio (0.45) captures the growing bureaucratic performance of safeguard reviews that increasingly protect institutions more than patients. Accessibility collapse (0.50) is moderate: covert alternatives persist but are legally dangerous; paternalistic alternatives are structurally barred. Resistance (0.60) reflects ongoing opposition from medical paternalists, disability advocates, and religious institutions.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of competent_terminal_patients and autonomy_advocates, the constraint is protective coordination that vindicates self-determination. From the seat of suffering_prolonged_patients denied access, the same constraint is an enforced gate that prolongs agony. The engine computes this divergence from the structural data: identical power levels (powerless) but divergent exit options (constrained vs trapped) and opposite beneficiary/victim declarations produce very different directionality and effective extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Competent_terminal_patients are declared beneficiaries with constrained exit; their directionality sits near the beneficiary end (low d), dampening effective extraction. Suffering_prolonged_patients are declared victims with trapped exit; their directionality sits near the full-target end (high d), amplifying effective extraction. Medical_gatekeepers are not in either base list; their institutional power and constrained exit place them near symmetric, but their loss of paternalistic authority imposes a moderate target-like component. Patient_autonomy_advocates are beneficiaries but with organized power and constrained exit, placing them at low d despite their indirect relationship.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâunrelieved suffering and covert euthanasiaâwas genuine, but its status is contested. The framework has not outlived its function (it is not a piton), nor is it purely transitional (no sunset clause). The mandatrophy risk here is mislabeling the constraint as pure coordination (rope) because it serves autonomy, while ignoring the excluded suffering patients who bear its costs. The Tangled Rope classification prevents this by requiring both beneficiaries and victims. Conversely, mislabeling it as a snare would ignore the genuine coordination it provides to dying patients who would otherwise face legal ambiguity or unregulated practice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    eligibility_expansion_boundary,
    'Does the empirical pattern of eligibility criteria expansion over time represent a bounded extension of autonomy rights or an unbounded slippery slope toward non-terminal and incompetent populations?',
    'Longitudinal cross-jurisdictional analysis comparing strict vs. permissive regimes for expansion rates and safeguard integrity.',
    'If unbounded, the constraint''s extractiveness increases and it may drift toward snare as it governs populations who cannot meaningfully consent. If bounded, the expansion represents progressive inclusion within a tangled coordination/extraction structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(eligibility_expansion_boundary, empirical, 'Whether eligibility expansion is bounded or slippery slope').

omega_variable(
    competence_assessment_extraction,
    'Does the competence assessment process protect patient autonomy or function as a bureaucratic extraction mechanism that denies choice to suffering patients through procedural delay and assessor bias?',
    'Outcome studies comparing rates of denied requests due to procedural delays vs. genuine competence failures; analysis of assessor demographics and decision patterns.',
    'If procedural denial dominates, victims are more numerous and the constraint is more extractive. If competence failures dominate, the gatekeeping is protective coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_assessment_extraction, empirical, 'Whether competence assessment is protective or extractive gatekeeping').

omega_variable(
    paternalism_suppression_mechanism,
    'Is the suppression of paternalistic medical authority structural (legal coercion and professional discipline) or internalized (cultural shift among physicians toward patient sovereignty)?',
    'Survey of physician attitudes toward paternalistic override and analysis of disciplinary cases for unjustified refusal to assess.',
    'Structural suppression is reversible and counts toward active enforcement; internalized suppression indicates deeper normalization but may mask latent resistance that could resurface.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(paternalism_suppression_mechanism, conceptual, 'Structural vs internalized suppression of medical paternalism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_authority__autonomy_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eol_autonomy_tr_t0, end_of_life_authority__autonomy_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(eol_autonomy_tr_t5, end_of_life_authority__autonomy_reading, theater_ratio, 5, 0.25).
narrative_ontology:measurement(eol_autonomy_tr_t10, end_of_life_authority__autonomy_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(eol_autonomy_tr_t15, end_of_life_authority__autonomy_reading, theater_ratio, 15, 0.35).
narrative_ontology:measurement(eol_autonomy_tr_t20, end_of_life_authority__autonomy_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(eol_autonomy_tr_t25, end_of_life_authority__autonomy_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement(eol_autonomy_tr_t30, end_of_life_authority__autonomy_reading, theater_ratio, 30, 0.45).

% Extraction over time
narrative_ontology:measurement(eol_autonomy_be_t0, end_of_life_authority__autonomy_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(eol_autonomy_be_t5, end_of_life_authority__autonomy_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(eol_autonomy_be_t10, end_of_life_authority__autonomy_reading, base_extractiveness, 10, 0.53).
narrative_ontology:measurement(eol_autonomy_be_t15, end_of_life_authority__autonomy_reading, base_extractiveness, 15, 0.57).
narrative_ontology:measurement(eol_autonomy_be_t20, end_of_life_authority__autonomy_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(eol_autonomy_be_t25, end_of_life_authority__autonomy_reading, base_extractiveness, 25, 0.63).
narrative_ontology:measurement(eol_autonomy_be_t30, end_of_life_authority__autonomy_reading, base_extractiveness, 30, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(eol_autonomy_su_t0, end_of_life_authority__autonomy_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(eol_autonomy_su_t5, end_of_life_authority__autonomy_reading, suppression_requirement, 5, 0.66).
narrative_ontology:measurement(eol_autonomy_su_t10, end_of_life_authority__autonomy_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(eol_autonomy_su_t15, end_of_life_authority__autonomy_reading, suppression_requirement, 15, 0.73).
narrative_ontology:measurement(eol_autonomy_su_t20, end_of_life_authority__autonomy_reading, suppression_requirement, 20, 0.75).
narrative_ontology:measurement(eol_autonomy_su_t25, end_of_life_authority__autonomy_reading, suppression_requirement, 25, 0.77).
narrative_ontology:measurement(eol_autonomy_su_t30, end_of_life_authority__autonomy_reading, suppression_requirement, 30, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(end_of_life_authority__autonomy_reading, end_of_life_authority__sanctity_reading).
narrative_ontology:affects_constraint(end_of_life_authority__autonomy_reading, end_of_life_authority__slippery_slope_mechanism).

% DUAL FORMULATION NOTE:
% The end_of_life_authority kernel decomposes into three structurally distinct constraints per the epsilon-invariance principle: the autonomy reading (this file), which locates authority in individual patient choice; the sanctity reading, which locates authority in intrinsic life value; and the slippery-slope reading, which treats autonomy frameworks as expansionary mechanisms. Each has distinct beneficiary/victim structures, epsilon values, and empirical profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
