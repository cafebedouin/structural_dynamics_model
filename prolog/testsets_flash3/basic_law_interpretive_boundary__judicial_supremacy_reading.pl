% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_boundary__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_boundary__judicial_supremacy_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: basic_law_interpretive_boundary__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy over Basic Laws (Supreme Court Reading)
 *   domain: constitutional_law/comparative_constitutionalism/judicial_review_theory
 *
 * SUMMARY:
 *   This constraint describes the 'judicial supremacy' reading of the Basic
 *   Laws' interpretive boundary, where the Supreme Court holds ultimate
 *   authority to interpret and enforce these laws, including the power to
 *   invalidate contradictory legislation. This reading positions the Court as
 *   the primary guardian of constitutional principles and individual rights,
 *   binding the Knesset to its constitutional interpretations. The metrics
 *   reflect the substantial extraction of legislative power from the Knesset
 *   and the active suppression of legislative alternatives that challenge
 *   judicial authority.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.7).
domain_priors:suppression_score(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.65).
domain_priors:theater_ratio(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_boundary__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_boundary__judicial_supremacy_reading, "Judicial Supremacy over Basic Laws (Supreme Court Reading)").
narrative_ontology:topic_domain(basic_law_interpretive_boundary__judicial_supremacy_reading, "constitutional_law/comparative_constitutionalism/judicial_review_theory").

domain_priors:requires_active_enforcement(basic_law_interpretive_boundary__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_boundary__judicial_supremacy_reading, '4c0c7975-d28b-45f7-922a-3555752e3dd0').
narrative_ontology:cs_kernel_codification('4c0c7975-d28b-45f7-922a-3555752e3dd0', formalized).
narrative_ontology:cs_authority_grounding('4c0c7975-d28b-45f7-922a-3555752e3dd0', lineage).
narrative_ontology:cs_interpretation_layer_present('4c0c7975-d28b-45f7-922a-3555752e3dd0').
narrative_ontology:cs_reading_relation('4c0c7975-d28b-45f7-922a-3555752e3dd0', basic_law_interpretive_boundary__parliamentary_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('4c0c7975-d28b-45f7-922a-3555752e3dd0', basic_law_interpretive_boundary__balanced_contestation_reading, influences).
narrative_ontology:cs_axiom('4c0c7975-d28b-45f7-922a-3555752e3dd0', foundational, basic_laws_as_supreme_law).
narrative_ontology:cs_axiom_status(basic_laws_as_supreme_law, holdable).
narrative_ontology:cs_axiom_grounding('4c0c7975-d28b-45f7-922a-3555752e3dd0', basic_laws_as_supreme_law, deontological).
narrative_ontology:cs_axiom('4c0c7975-d28b-45f7-922a-3555752e3dd0', foundational, judicial_review_as_constitutional_safeguard).
narrative_ontology:cs_axiom_status(judicial_review_as_constitutional_safeguard, holdable).
narrative_ontology:cs_axiom_grounding('4c0c7975-d28b-45f7-922a-3555752e3dd0', judicial_review_as_constitutional_safeguard, deontological).
narrative_ontology:cs_reference_frame('4c0c7975-d28b-45f7-922a-3555752e3dd0', constitutional_supremacy_framework).
narrative_ontology:cs_drift_state('4c0c7975-d28b-45f7-922a-3555752e3dd0', contemporary_political_polarization, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('4c0c7975-d28b-45f7-922a-3555752e3dd0', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_boundary__judicial_supremacy_reading, basic_law_interpretive_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__judicial_supremacy_reading, supreme_court).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__judicial_supremacy_reading, rights_claimants).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__judicial_supremacy_reading, knesset).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__judicial_supremacy_reading, government_coalition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the Basic Laws as a higher-order framework, invalidating legislation that contradicts them. This reading grants the Court ultimate authority in constitutional matters, making its rulings binding on the Knesset.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, supreme_court, agenda_setter,
    institutional, generational, constrained, national).

% Its legislative power is constrained by the Supreme Court's interpretation of Basic Laws. Legislation passed by the Knesset can be nullified, limiting its ability to enact policy freely, especially on matters touching on rights.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, knesset, payer,
    institutional, biographical, constrained, national).

% Gain a powerful mechanism to challenge legislation that infringes on their rights, using the Supreme Court as a veto point against parliamentary majorities. Their ability to secure rights is enhanced by judicial review.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, rights_claimants, beneficiary,
    powerless, immediate, constrained, national).

% Represents the executive and legislative majority whose policy agenda can be blocked or overturned by judicial review. This limits their ability to implement their electoral mandate if it conflicts with the Court's constitutional interpretation.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, government_coalition, payer,
    powerful, biographical, constrained, national).

% Analyze and advocate for this reading, providing intellectual and public support for judicial review and the Supreme Court's role in upholding constitutional principles. They influence public discourse and legal education.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, legal_scholars_and_activists, observer,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear hierarchy of legal norms, ensuring that ordinary legislation conforms to fundamental constitutional principles, thereby providing stability and predictability in the legal system and protecting individual rights from legislative overreach.
% TRANSFER_FUNCTION: Transfers ultimate interpretive authority over Basic Laws from the Knesset to the Supreme Court, effectively transferring legislative veto power on constitutional matters from the elected legislature to the judiciary. This also transfers power to rights-claimants to block legislation.
% ABSENT_VOICES: Proponents of pure parliamentary sovereignty, who believe the Knesset should be the sole arbiter of constitutional meaning, are structurally marginalized in this framework. They would argue for the supremacy of the elected body and against judicial activism.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the Supreme Court would lose its power to invalidate legislation, fundamentally altering the balance of power between the judiciary and the legislature. The Knesset would gain unchecked power to amend Basic Laws and pass ordinary legislation, potentially leading to a rapid erosion of rights protections and a shift towards parliamentary majoritarianism.
% FOUNDING_PROBLEM: The need to establish a stable constitutional framework that protects fundamental rights and limits the power of the legislative majority, especially in a system without a formal, entrenched constitution.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, civil society organizations, and international human rights bodies corroborate the ongoing need for a mechanism to protect rights and constitutional principles against potential legislative overreach, supporting the idea that the founding problem remains live.
narrative_ontology:disappearance_verdict(basic_law_interpretive_boundary__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_boundary__judicial_supremacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_boundary__judicial_supremacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(basic_law_interpretive_boundary__judicial_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_boundary__judicial_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(basic_law_interpretive_boundary__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(basic_law_interpretive_boundary__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.7) is high because the Supreme Court's power to nullify legislation significantly curtails the Knesset's legislative autonomy. Suppression (0.65) is also high, as the Court actively enforces its interpretations, effectively suppressing legislative attempts to bypass or contradict its rulings. Theater ratio is low (0.1) because the Court's actions are genuinely functional in shaping the legal landscape, not merely performative. The claimed type is 'tangled_rope' because it provides a coordination function (constitutional stability, rights protection) but also involves asymmetric extraction of power from the legislature.
 *
 * PERSPECTIVAL GAP:
 *   From the Supreme Court's perspective, this is a necessary 'rope' for constitutional order and rights protection. From the Knesset's perspective, especially the government coalition, it is a 'snare' that extracts legislative power and frustrates the democratic mandate. Rights claimants, on the other hand, experience it as a 'rope' that protects their interests. The engine will compute these per-seat classifications.
 *
 * DIRECTIONALITY LOGIC:
 *   The Supreme Court and rights claimants are beneficiaries (low d) as they gain power and protection. The Knesset and the government coalition are targets (high d) as their legislative power is curtailed. The constraint actively enforces judicial review, requiring continuous defense against legislative challenges.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as a 'mountain' (natural law) or a pure 'rope' (simple coordination). While it provides a coordination function, the significant and actively enforced extraction of legislative power from the Knesset, coupled with ongoing political contestation, indicates it is a 'tangled_rope' rather than a benign or natural arrangement. The rising extractiveness and suppression over time suggest an accumulation of judicial power.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judicial_legitimacy_source,
    'Is the Supreme Court''s authority to invalidate legislation derived from an explicit constitutional grant, or is it an evolved practice based on interpretive tradition?',
    'Historical legal analysis of the Basic Laws'' drafting and subsequent judicial precedents; comparative analysis with other constitutional systems'' judicial review origins.',
    'If explicit, the constraint''s naturalness claim (emerges_naturally) would be stronger, potentially shifting it closer to a ''mountain'' from an analytical perspective. If evolved practice, it reinforces its ''tangled_rope'' nature as a constructed and contestable arrangement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(judicial_legitimacy_source, conceptual, 'Source of judicial review authority: explicit grant vs. evolved practice.').

omega_variable(
    political_contestation_impact,
    'To what extent does ongoing political contestation over judicial review (e.g., legislative attempts to curb court powers) affect the stability and perceived legitimacy of this reading?',
    'Analysis of public opinion polls on judicial trust, legislative outcomes of judicial reform attempts, and the frequency/intensity of constitutional crises.',
    'High contestation and successful legislative challenges would increase ''resistance'' and ''theater_ratio'', potentially pushing the constraint towards a ''piton'' if its functional power erodes, or a ''snare'' if enforcement becomes purely coercive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_contestation_impact, empirical, 'Impact of political contestation on judicial supremacy''s stability.').

omega_variable(
    rights_protection_efficacy,
    'How effectively does this reading of judicial supremacy actually protect individual and minority rights in practice, compared to alternative mechanisms or readings?',
    'Empirical studies on human rights outcomes, analysis of specific court rulings'' impact on vulnerable populations, and comparison with rights protection in systems with different judicial review models.',
    'If rights protection is demonstrably weak or uneven, it would undermine the ''coordination function'' claim, increasing the ''extractiveness'' and potentially reclassifying it closer to a ''snare'' by revealing the coordination story as cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rights_protection_efficacy, empirical, 'Empirical efficacy of judicial supremacy in protecting rights.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_boundary__judicial_supremacy_reading, 1992, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t1992, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 1992, 0.05).
narrative_ontology:measurement_basis(basi_tr_t1992, observed).
narrative_ontology:measurement(basi_tr_t2000, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 2000, 0.07).
narrative_ontology:measurement_basis(basi_tr_t2000, observed).
narrative_ontology:measurement(basi_tr_t2008, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 2008, 0.08).
narrative_ontology:measurement_basis(basi_tr_t2008, observed).
narrative_ontology:measurement(basi_tr_t2016, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 2016, 0.09).
narrative_ontology:measurement_basis(basi_tr_t2016, observed).
narrative_ontology:measurement(basi_tr_t2024, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 2024, 0.1).
narrative_ontology:measurement_basis(basi_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(basi_be_t1992, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 1992, 0.4).
narrative_ontology:measurement_basis(basi_be_t1992, observed).
narrative_ontology:measurement(basi_be_t2000, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 2000, 0.5).
narrative_ontology:measurement_basis(basi_be_t2000, observed).
narrative_ontology:measurement(basi_be_t2008, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 2008, 0.6).
narrative_ontology:measurement_basis(basi_be_t2008, observed).
narrative_ontology:measurement(basi_be_t2016, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 2016, 0.65).
narrative_ontology:measurement_basis(basi_be_t2016, observed).
narrative_ontology:measurement(basi_be_t2024, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 2024, 0.7).
narrative_ontology:measurement_basis(basi_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t1992, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 1992, 0.3).
narrative_ontology:measurement_basis(basi_su_t1992, observed).
narrative_ontology:measurement(basi_su_t2000, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 2000, 0.45).
narrative_ontology:measurement_basis(basi_su_t2000, observed).
narrative_ontology:measurement(basi_su_t2008, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 2008, 0.55).
narrative_ontology:measurement_basis(basi_su_t2008, observed).
narrative_ontology:measurement(basi_su_t2016, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 2016, 0.6).
narrative_ontology:measurement_basis(basi_su_t2016, observed).
narrative_ontology:measurement(basi_su_t2024, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 2024, 0.65).
narrative_ontology:measurement_basis(basi_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_boundary__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__judicial_supremacy_reading, basic_law_interpretive_boundary__parliamentary_sovereignty_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__judicial_supremacy_reading, basic_law_interpretive_boundary__balanced_contestation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'basic_law_interpretive_boundary' kernel. Each reading represents a distinct structural claim about the balance of power between the Supreme Court and the Knesset regarding Basic Laws.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
