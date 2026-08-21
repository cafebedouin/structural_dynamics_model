% ============================================================================
% CONSTRAINT STORY: end_of_life_authority__autonomy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: end_of_life_authority__autonomy_reading
 *   human_readable: End-of-Life Authority: Autonomy Reading
 *   domain: medical_ethics/bioethics/end_of_life_policy
 *
 * SUMMARY:
 *   This constraint represents the 'autonomy reading' of end-of-life
 *   authority, where individual self-determination is paramount in decisions
 *   regarding death when facing unbearable suffering. It frames restrictions
 *   on aid-in-dying as extractive, prolonging suffering and denying agency.
 *   The constraint is a Tangled Rope because it coordinates patient choice
 *   with medical practice, but also extracts from those denied choice through
 *   paternalistic restrictions, requiring active enforcement to maintain its
 *   boundaries.
 *
 * KEY AGENTS:
 *   - patients_seeking_aid_in_dying: Primary beneficiary (powerless/trapped) — gains control over death.
 *   - advocacy_organizations_for_patient_choice: Agenda setter (organized/mobile) — shapes policy.
 *   - patients_denied_choice_due_to_paternalistic_restrictions: Primary victim (powerless/trapped) — bears prolonged suffering.
 *   - religious_institutions_and_pro_life_groups: Excluded (organized/mobile) — actively opposes but is outside the direct policy implementation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_authority__autonomy_reading, 0.65).
domain_priors:suppression_score(end_of_life_authority__autonomy_reading, 0.7).
domain_priors:theater_ratio(end_of_life_authority__autonomy_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_authority__autonomy_reading, tangled_rope).
narrative_ontology:human_readable(end_of_life_authority__autonomy_reading, "End-of-Life Authority: Autonomy Reading").
narrative_ontology:topic_domain(end_of_life_authority__autonomy_reading, "medical_ethics/bioethics/end_of_life_policy").

domain_priors:requires_active_enforcement(end_of_life_authority__autonomy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_authority__autonomy_reading, 'a1aa84f4-f613-4c0c-9fd0-9bc602cbb0bb').
narrative_ontology:cs_kernel_codification('a1aa84f4-f613-4c0c-9fd0-9bc602cbb0bb', formalized).
narrative_ontology:cs_authority_grounding('a1aa84f4-f613-4c0c-9fd0-9bc602cbb0bb', lineage).
narrative_ontology:cs_interpretation_layer_present('a1aa84f4-f613-4c0c-9fd0-9bc602cbb0bb').
narrative_ontology:cs_reading_relation('a1aa84f4-f613-4c0c-9fd0-9bc602cbb0bb', end_of_life_authority__sanctity_reading, coexists_with).
narrative_ontology:cs_reading_relation('a1aa84f4-f613-4c0c-9fd0-9bc602cbb0bb', end_of_life_authority__slippery_slope_mechanism, influences).
narrative_ontology:cs_axiom('a1aa84f4-f613-4c0c-9fd0-9bc602cbb0bb', foundational, individual_self_determination_is_paramount).
narrative_ontology:cs_axiom_status(individual_self_determination_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('a1aa84f4-f613-4c0c-9fd0-9bc602cbb0bb', individual_self_determination_is_paramount, deontological).
narrative_ontology:cs_axiom('a1aa84f4-f613-4c0c-9fd0-9bc602cbb0bb', foundational, unbearable_suffering_justifies_aid_in_dying).
narrative_ontology:cs_axiom_status(unbearable_suffering_justifies_aid_in_dying, holdable).
narrative_ontology:cs_axiom_grounding('a1aa84f4-f613-4c0c-9fd0-9bc602cbb0bb', unbearable_suffering_justifies_aid_in_dying, empirically_contingent).
narrative_ontology:cs_reference_frame('a1aa84f4-f613-4c0c-9fd0-9bc602cbb0bb', patient_autonomy_as_primary_ethical_principle).
narrative_ontology:cs_drift_state('a1aa84f4-f613-4c0c-9fd0-9bc602cbb0bb', contemporary_legal_expansion, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a1aa84f4-f613-4c0c-9fd0-9bc602cbb0bb', '').
narrative_ontology:cs_kernel_id(end_of_life_authority__autonomy_reading, end_of_life_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_authority__autonomy_reading, patients_seeking_aid_in_dying).
narrative_ontology:constraint_beneficiary(end_of_life_authority__autonomy_reading, advocacy_organizations_for_patient_choice).
narrative_ontology:constraint_victim(end_of_life_authority__autonomy_reading, patients_denied_choice_due_to_paternalistic_restrictions).
narrative_ontology:constraint_victim(end_of_life_authority__autonomy_reading, medical_professionals_constrained_by_paternalistic_laws).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals facing unbearable suffering who wish to exercise control over the timing and circumstances of their death. They benefit from the legal and medical frameworks that permit aid-in-dying, but are often trapped by their medical condition.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, patients_seeking_aid_in_dying, beneficiary,
    powerless, immediate, trapped, local).

% Groups that lobby for and support legislation and policies expanding patient autonomy in end-of-life decisions. They actively shape the legal and ethical landscape.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, advocacy_organizations_for_patient_choice, agenda_setter,
    organized, generational, mobile, national).

% Physicians and other healthcare providers who are willing to offer aid-in-dying services within legal and ethical guidelines. They navigate complex regulations and ethical dilemmas, often facing professional and moral scrutiny.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, medical_professionals_providing_aid, agenda_setter,
    powerful, biographical, constrained, local).

% Individuals who, under a paternalistic framework, are denied the option of aid-in-dying despite their autonomous request and unbearable suffering. They bear the cost of prolonged suffering and loss of control.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, patients_denied_choice_due_to_paternalistic_restrictions, payer,
    powerless, immediate, trapped, local).

% Healthcare providers who believe in patient autonomy but are legally or institutionally prevented from offering aid-in-dying, leading to moral distress and professional limitations.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, medical_professionals_constrained_by_paternalistic_laws, payer,
    moderate, biographical, constrained, local).

% Organizations that oppose aid-in-dying on moral or religious grounds, advocating for the sanctity of life. While they actively participate in public discourse, their views are often excluded from the direct implementation of autonomy-based end-of-life policies.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, religious_institutions_and_pro_life_groups, excluded,
    organized, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the process by which a patient's autonomous request for aid-in-dying is recognized, evaluated, and, if criteria are met, facilitated by the medical system, ensuring legal and ethical safeguards.
% TRANSFER_FUNCTION: Transfers the ultimate authority over the timing and manner of death from medical paternalism or religious doctrine to the individual patient, in cases of unbearable suffering.
% ABSENT_VOICES: Voices emphasizing the intrinsic value of life and the potential for abuse (often from religious institutions and disability rights advocates) are often marginalized in the direct policy implementation of autonomy-based frameworks, though they remain active in public debate.
% DISAPPEARANCE_RATIONALE: If the autonomy-based framework vanished, patients would lose a critical right to self-determination at the end of life, leading to prolonged suffering for many. Medical professionals would face increased ethical dilemmas, and the legal landscape would revert to more paternalistic or restrictive approaches, fundamentally altering end-of-life care.
% FOUNDING_PROBLEM: Patients facing terminal illness and unbearable suffering lacked control over their dying process, often experiencing prolonged pain and loss of dignity due to medical and legal restrictions.
% FOUNDING_PROBLEM_CORROBORATION: Patient testimonials, medical ethics literature, and public opinion surveys consistently corroborate the ongoing problem of suffering and the desire for end-of-life autonomy. Legal reforms in various jurisdictions reflect a societal recognition of this problem, supported by independent bioethicists and patient advocacy groups.
narrative_ontology:disappearance_verdict(end_of_life_authority__autonomy_reading, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_authority__autonomy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_authority__autonomy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(end_of_life_authority__autonomy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_authority__autonomy_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.65) because the denial of autonomy in end-of-life decisions imposes significant costs (prolonged suffering, loss of dignity) on individuals. Suppression is also high (0.70) as legal and medical systems actively enforce restrictions on aid-in-dying, limiting patient choice and professional action. The 'theater ratio' is low (0.10) because the debate is highly substantive, with little performative maintenance. The increasing extractiveness and suppression over time reflect the ongoing struggle to expand and protect end-of-life autonomy against persistent opposition.
 *
 * PERSPECTIVAL GAP:
 *   Patients denied choice experience this as a Snare, while advocacy groups see it as a Rope that needs strengthening. Medical professionals providing aid navigate it as a Tangled Rope, balancing patient autonomy with legal and ethical boundaries. The engine's per-seat classification will capture these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   Patients seeking aid-in-dying are beneficiaries (d near 0.0) as the constraint enables their choice. Patients denied choice are victims (d near 1.0) as the constraint's restrictions directly extract from them. Advocacy organizations are beneficiaries/agenda-setters (d near 0.0) as they actively shape the constraint to their benefit. Medical professionals providing aid are agenda-setters/beneficiaries (d near 0.2-0.3) as they facilitate the process but also bear professional risks. Excluded groups (religious institutions) are targets of the constraint's expansion, but their exclusion from direct policy implementation means their d is not directly tied to the constraint's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as a pure Rope (ignoring the victims of paternalistic restrictions) or a pure Snare (ignoring the genuine coordination function for those who gain choice). The 'contested' status of the founding problem highlights that the debate is not about whether suffering exists, but about the legitimate means to address it, and whose authority prevails.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_unbearable_suffering,
    'How is ''unbearable suffering'' defined, and does its interpretation expand over time to include non-terminal or non-physical conditions?',
    'Longitudinal analysis of legal precedents and medical guidelines in jurisdictions with aid-in-dying laws; empirical studies on patient requests and physician interpretations.',
    'If the definition expands significantly beyond initial intent, it would support the ''slippery_slope_mechanism'' reading, potentially increasing the victim set and extractiveness for those who oppose such expansion. If it remains stable, it strengthens the autonomy reading''s claim of controlled application.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_unbearable_suffering, empirical, 'Ambiguity in the definition and application of ''unbearable suffering''.').

omega_variable(
    paternalism_vs_protection,
    'Are restrictions on end-of-life autonomy genuinely paternalistic (denying choice) or protective (safeguarding vulnerable populations from coercion)?',
    'Sociological studies on patient vulnerability and coercion in end-of-life decisions; ethical analysis of ''best interest'' standards versus ''substituted judgment'' for incapacitated patients.',
    'If restrictions are primarily paternalistic, the extractiveness and suppression of this constraint are accurately high. If they are genuinely protective, the victim set might be smaller, and the constraint''s coordination function (safeguarding) would be more prominent, potentially lowering effective extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(paternalism_vs_protection, conceptual, 'Distinguishing paternalistic denial of autonomy from legitimate protection of vulnerable individuals.').

omega_variable(
    identity_lock_for_medical_professionals,
    'To what extent are medical professionals ''identity_locked'' into a ''do no harm'' ethos that conflicts with aid-in-dying, even when legally permitted?',
    'Qualitative studies on physician attitudes and moral distress regarding aid-in-dying; analysis of professional codes of conduct and their evolution.',
    'If identity lock is strong, it increases the effective suppression on medical professionals who might otherwise provide aid, making them a more constrained payer. If it weakens, it facilitates the expansion of autonomy-based practices.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_for_medical_professionals, empirical, 'The role of professional identity in constraining medical professionals'' participation in aid-in-dying.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_authority__autonomy_reading, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(end__be_t1990, end_of_life_authority__autonomy_reading, base_extractiveness, 1990, 0.4).
narrative_ontology:measurement(end__be_t2000, end_of_life_authority__autonomy_reading, base_extractiveness, 2000, 0.5).
narrative_ontology:measurement(end__be_t2010, end_of_life_authority__autonomy_reading, base_extractiveness, 2010, 0.58).
narrative_ontology:measurement(end__be_t2024, end_of_life_authority__autonomy_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(end__su_t1990, end_of_life_authority__autonomy_reading, suppression_requirement, 1990, 0.5).
narrative_ontology:measurement(end__su_t2000, end_of_life_authority__autonomy_reading, suppression_requirement, 2000, 0.6).
narrative_ontology:measurement(end__su_t2010, end_of_life_authority__autonomy_reading, suppression_requirement, 2010, 0.65).
narrative_ontology:measurement(end__su_t2024, end_of_life_authority__autonomy_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_authority__autonomy_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'end_of_life_authority' kernel. Its structural delta includes suffering-prolonged patients entering the victim set and high suppression of paternalistic restrictions, with an empirical pattern of expanding eligibility criteria over time.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
