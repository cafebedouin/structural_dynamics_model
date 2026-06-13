% ============================================================================
% CONSTRAINT STORY: end_of_life_decision_authority__sanctity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_end_of_life_decision_authority__sanctity_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: end_of_life_decision_authority__sanctity_reading
 *   human_readable: Sanctity of Life Principle in End-of-Life Decisions
 *   domain: medical_ethics/bioethics/end_of_life_policy
 *
 * SUMMARY:
 *   This constraint represents the 'sanctity of life' reading of end-of-life
 *   decision authority, asserting that human life has intrinsic value
 *   independent of individual will and that intentional life-ending violates
 *   this value. It is a tangled rope because it coordinates medical practice
 *   around a moral principle (benefiting institutions and bioethicists) but
 *   extracts from terminally ill patients by denying them autonomy over their
 *   death, requiring active enforcement through legal and ethical
 *   prohibitions.
 *
 * KEY AGENTS:
 *   - religious_institutions: Agenda-setter (institutional/identity_locked) — primary proponent and beneficiary
 *   - conservative_bioethicists: Beneficiary (organized/constrained) — professional validation
 *   - terminally_ill_patients: Payer (powerless/trapped) — denied autonomy, prolonged suffering
 *   - patients_with_intractable_suffering: Payer (powerless/trapped) — denied autonomy, prolonged suffering
 *   - physicians: Agenda-setter/Beneficiary (institutional/constrained) — constrained to healer-only role, some benefit from clear ethical boundary
 *   - advocates_for_death_with_dignity: Excluded (organized/constrained) — actively campaigning for change, but outside core decision-making
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_decision_authority__sanctity_reading, 0.6).
domain_priors:suppression_score(end_of_life_decision_authority__sanctity_reading, 0.7).
domain_priors:theater_ratio(end_of_life_decision_authority__sanctity_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_decision_authority__sanctity_reading, tangled_rope).
narrative_ontology:human_readable(end_of_life_decision_authority__sanctity_reading, "Sanctity of Life Principle in End-of-Life Decisions").
narrative_ontology:topic_domain(end_of_life_decision_authority__sanctity_reading, "medical_ethics/bioethics/end_of_life_policy").

domain_priors:requires_active_enforcement(end_of_life_decision_authority__sanctity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_decision_authority__sanctity_reading, '1c4a0acf-becf-4a1a-95db-da88ba864514').
narrative_ontology:cs_kernel_codification('1c4a0acf-becf-4a1a-95db-da88ba864514', formalized).
narrative_ontology:cs_authority_grounding('1c4a0acf-becf-4a1a-95db-da88ba864514', lineage).
narrative_ontology:cs_interpretation_layer_present('1c4a0acf-becf-4a1a-95db-da88ba864514').
narrative_ontology:cs_reading_relation('1c4a0acf-becf-4a1a-95db-da88ba864514', end_of_life_decision_authority__autonomy_reading, forecloses).
narrative_ontology:cs_reading_relation('1c4a0acf-becf-4a1a-95db-da88ba864514', end_of_life_decision_authority__vulnerability_protection_reading, influences).
narrative_ontology:cs_axiom('1c4a0acf-becf-4a1a-95db-da88ba864514', foundational, human_life_intrinsic_value).
narrative_ontology:cs_axiom_status(human_life_intrinsic_value, holdable).
narrative_ontology:cs_axiom_grounding('1c4a0acf-becf-4a1a-95db-da88ba864514', human_life_intrinsic_value, deontological).
narrative_ontology:cs_axiom('1c4a0acf-becf-4a1a-95db-da88ba864514', secondary, physician_role_healer_only).
narrative_ontology:cs_axiom_status(physician_role_healer_only, holdable).
narrative_ontology:cs_axiom_grounding('1c4a0acf-becf-4a1a-95db-da88ba864514', physician_role_healer_only, conventional).
narrative_ontology:cs_reference_frame('1c4a0acf-becf-4a1a-95db-da88ba864514', traditional_medical_ethics_life_preservation).
narrative_ontology:cs_drift_state('1c4a0acf-becf-4a1a-95db-da88ba864514', contemporary_bioethics_debate, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('1c4a0acf-becf-4a1a-95db-da88ba864514', '').
narrative_ontology:cs_kernel_id(end_of_life_decision_authority__sanctity_reading, end_of_life_decision_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__sanctity_reading, religious_institutions).
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__sanctity_reading, conservative_bioethicists).
narrative_ontology:constraint_victim(end_of_life_decision_authority__sanctity_reading, terminally_ill_patients).
narrative_ontology:constraint_victim(end_of_life_decision_authority__sanctity_reading, patients_with_intractable_suffering).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__sanctity_reading, physicians).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocate for the sanctity of life principle, influencing legislation and medical guidelines to prohibit euthanasia and assisted suicide. They derive moral authority and institutional coherence from this stance.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, religious_institutions, agenda_setter,
    institutional, generational, identity_locked, global).

% Their professional careers and academic standing are often built upon defending and elaborating the sanctity of life principle. They benefit from the persistence of this constraint as it validates their intellectual framework.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, conservative_bioethicists, beneficiary,
    organized, biographical, constrained, national).

% Are denied the option of physician-assisted dying, even when facing intractable suffering and a clear desire to end their lives. Their suffering is prolonged by the constraint, and their autonomy is overridden.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, terminally_ill_patients, payer,
    powerless, immediate, trapped, local).

% Experience prolonged pain and loss of dignity without legal recourse to end their lives. The constraint forces them to endure suffering that they deem unacceptable, externalizing the cost of their suffering onto them.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, patients_with_intractable_suffering, payer,
    powerless, immediate, trapped, local).

% Are constrained to a 'healer-only' role, prohibited from directly participating in life-ending acts. This maintains a clear ethical boundary for the profession, which some physicians view as a benefit, while others feel it compromises patient care and autonomy.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, physicians, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(end_of_life_decision_authority__sanctity_reading, physicians, beneficiary).

% Represent patients seeking greater autonomy in end-of-life decisions. They are actively campaigning for legislative changes but face significant institutional and ideological barriers from proponents of the sanctity of life principle.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, advocates_for_death_with_dignity, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates medical practice and public policy around a shared moral framework that prioritizes the preservation of life, ensuring that medical professionals are uniformly committed to healing and preventing intentional harm.
% TRANSFER_FUNCTION: Transfers the authority over the timing and manner of death from the individual to a collective moral framework, externalizing the burden of intractable suffering onto individuals and their families.
% ABSENT_VOICES: Terminally ill patients and those with intractable suffering, whose voices are often marginalized by their condition and by the dominant moral framework, would advocate for the right to choose the timing of their death. Advocates for death with dignity represent these voices but are often excluded from the core decision-making bodies.
% DISAPPEARANCE_RATIONALE: If the sanctity of life principle as a binding constraint on end-of-life decisions vanished, medical ethics, legal frameworks, and patient care protocols would undergo a profound reorganization. Euthanasia and assisted suicide would likely become legal options, shifting the locus of decision-making authority and altering the role of physicians.
% FOUNDING_PROBLEM: The problem of preventing arbitrary or coerced termination of life, particularly for vulnerable individuals, and maintaining a clear moral boundary for medical practice.
% FOUNDING_PROBLEM_CORROBORATION: Religious institutions and conservative bioethicists attest that the problem of protecting vulnerable life remains live, citing concerns about potential abuses if euthanasia were legalized. Advocates for death with dignity acknowledge the historical problem but argue that modern safeguards can address it, and that the constraint now primarily serves to deny autonomy rather than protect the vulnerable.
narrative_ontology:disappearance_verdict(end_of_life_decision_authority__sanctity_reading, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_decision_authority__sanctity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_decision_authority__sanctity_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(end_of_life_decision_authority__sanctity_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(end_of_life_decision_authority__sanctity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(end_of_life_decision_authority__sanctity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(end_of_life_decision_authority__sanctity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.6) is substantial because it imposes a significant cost (prolonged suffering, loss of autonomy) on individuals. Suppression (0.7) is high due to legal prohibitions and strong institutional resistance to alternatives. Theater ratio (0.2) is low, indicating that the constraint is genuinely enforced, though some performative aspects exist in public discourse. The values for extractiveness, suppression, and theater ratio show a slight increase over time as societal debates intensify and the constraint's enforcement becomes more contested, before a slight dip as some jurisdictions begin to allow alternatives.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of religious institutions and conservative bioethicists, this constraint is a necessary moral safeguard (a rope or even a mountain of natural law). From the perspective of terminally ill patients, it is a snare that prolongs suffering and denies fundamental autonomy. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious institutions and conservative bioethicists are beneficiaries (low d) as the constraint validates their moral framework and institutional power. Terminally ill patients and those with intractable suffering are clear targets (high d) as they bear the direct costs of denied autonomy and prolonged suffering. Physicians are complex: they are agenda-setters in upholding the principle (low d) but also constrained in their practice, which can be a cost.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate to protect vulnerable life is still live, but its application has shifted. While it genuinely prevents some forms of coercion, it also denies autonomy to competent individuals, leading to a contested status. The classification as a tangled rope reflects this hybrid function: it coordinates around a moral principle but extracts from a specific group through active enforcement, preventing it from being mislabeled as a pure rope or a mountain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intrinsic_value_vs_autonomy,
    'Is the intrinsic value of life a universal, non-negotiable moral truth, or is individual autonomy over one''s own body and death a higher moral imperative?',
    'This is a conceptual and preference-based omega. Resolution would require a societal shift in foundational moral frameworks or a legal precedent that redefines the hierarchy of values.',
    'If autonomy is prioritized, the constraint would be reclassified as a snare for patients; if intrinsic value is absolute, it would be closer to a mountain for all.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intrinsic_value_vs_autonomy, conceptual, 'The fundamental conflict between two core moral principles.').

omega_variable(
    vulnerability_protection_efficacy,
    'Does the prohibition on euthanasia genuinely protect vulnerable individuals from coercion, or does it primarily deny autonomy to competent individuals?',
    'Empirical studies on the implementation of physician-assisted dying in jurisdictions where it is legal, assessing the incidence of coercion or abuse among vulnerable populations.',
    'If coercion is rare with safeguards, the constraint''s protective function is overstated, increasing its extractiveness for autonomous patients. If coercion is significant, the protective function is validated, reducing its effective extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vulnerability_protection_efficacy, empirical, 'Assessing the actual protective vs. autonomy-denying effects of the constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_decision_authority__sanctity_reading, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(end__tr_t1950, end_of_life_decision_authority__sanctity_reading, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(end__tr_t1970, end_of_life_decision_authority__sanctity_reading, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(end__tr_t1990, end_of_life_decision_authority__sanctity_reading, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(end__tr_t2010, end_of_life_decision_authority__sanctity_reading, theater_ratio, 2010, 0.25).
narrative_ontology:measurement(end__tr_t2024, end_of_life_decision_authority__sanctity_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(end__be_t1950, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 1950, 0.5).
narrative_ontology:measurement(end__be_t1970, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 1970, 0.55).
narrative_ontology:measurement(end__be_t1990, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 1990, 0.6).
narrative_ontology:measurement(end__be_t2010, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 2010, 0.65).
narrative_ontology:measurement(end__be_t2024, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 2024, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(end__su_t1950, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 1950, 0.6).
narrative_ontology:measurement(end__su_t1970, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 1970, 0.65).
narrative_ontology:measurement(end__su_t1990, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 1990, 0.7).
narrative_ontology:measurement(end__su_t2010, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 2010, 0.75).
narrative_ontology:measurement(end__su_t2024, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_decision_authority__sanctity_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'end_of_life_decision_authority' kernel. It is linked to 'end_of_life_decision_authority__autonomy_reading' and 'end_of_life_decision_authority__vulnerability_protection_reading' through the kernel structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
