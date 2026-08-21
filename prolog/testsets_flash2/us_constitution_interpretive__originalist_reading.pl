% ============================================================================
% CONSTRAINT STORY: us_constitution_interpretive__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_interpretive__originalist_reading, []).

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
 *   constraint_id: us_constitution_interpretive__originalist_reading
 *   human_readable: Originalist Reading of US Constitutional Meaning
 *   domain: constitutional_law/legal_interpretation/political_theory
 *
 * SUMMARY:
 *   This constraint represents the originalist reading of the US
 *   Constitution, where meaning is fixed at ratification and interpretive
 *   authority derives from fidelity to framers' intent or original public
 *   meaning. This reading narrows judicial power, constrains federal
 *   authority to 1787 understandings, and limits enumerated rights to their
 *   historical scope. It benefits federalism advocates and certain rights
 *   claimants while disadvantaging those seeking unenumerated rights or
 *   federal regulatory expansion. This is one reading of the
 *   'us_constitution_interpretive' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_interpretive__originalist_reading, 0.65).
domain_priors:suppression_score(us_constitution_interpretive__originalist_reading, 0.7).
domain_priors:theater_ratio(us_constitution_interpretive__originalist_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_interpretive__originalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_interpretive__originalist_reading, "Originalist Reading of US Constitutional Meaning").
narrative_ontology:topic_domain(us_constitution_interpretive__originalist_reading, "constitutional_law/legal_interpretation/political_theory").

domain_priors:requires_active_enforcement(us_constitution_interpretive__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_interpretive__originalist_reading, '2517ab55-7d1c-44a0-baaf-df7536238866').
narrative_ontology:cs_kernel_codification('2517ab55-7d1c-44a0-baaf-df7536238866', fixed_text).
narrative_ontology:cs_authority_grounding('2517ab55-7d1c-44a0-baaf-df7536238866', lineage).
narrative_ontology:cs_interpretation_layer_present('2517ab55-7d1c-44a0-baaf-df7536238866').
narrative_ontology:cs_reading_relation('2517ab55-7d1c-44a0-baaf-df7536238866', us_constitution_interpretive__living_constitution_reading, forecloses).
narrative_ontology:cs_reading_relation('2517ab55-7d1c-44a0-baaf-df7536238866', us_constitution_interpretive__popular_constitutionalism_reading, coexists_with).
narrative_ontology:cs_axiom('2517ab55-7d1c-44a0-baaf-df7536238866', foundational, constitutional_meaning_fixed_at_ratification).
narrative_ontology:cs_axiom_status(constitutional_meaning_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('2517ab55-7d1c-44a0-baaf-df7536238866', constitutional_meaning_fixed_at_ratification, conventional).
narrative_ontology:cs_axiom('2517ab55-7d1c-44a0-baaf-df7536238866', foundational, judicial_fidelity_to_original_intent).
narrative_ontology:cs_axiom_status(judicial_fidelity_to_original_intent, holdable).
narrative_ontology:cs_axiom_grounding('2517ab55-7d1c-44a0-baaf-df7536238866', judicial_fidelity_to_original_intent, deontological).
narrative_ontology:cs_reference_frame('2517ab55-7d1c-44a0-baaf-df7536238866', framers_original_intent_1787).
narrative_ontology:cs_drift_state('2517ab55-7d1c-44a0-baaf-df7536238866', contemporary_legal_discourse, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('2517ab55-7d1c-44a0-baaf-df7536238866', '').
narrative_ontology:cs_kernel_id(us_constitution_interpretive__originalist_reading, us_constitution_interpretive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__originalist_reading, federalism_advocates).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__originalist_reading, religious_liberty_claimants_originalist).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__originalist_reading, property_rights_defenders).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__originalist_reading, conservative_legal_movement).
narrative_ontology:constraint_victim(us_constitution_interpretive__originalist_reading, unenumerated_rights_claimants).
narrative_ontology:constraint_victim(us_constitution_interpretive__originalist_reading, federal_regulatory_expansion_advocates).
narrative_ontology:constraint_victim(us_constitution_interpretive__originalist_reading, progressive_legal_movement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Judges who adhere to originalist interpretive methodologies, shaping legal outcomes based on their understanding of the Constitution's meaning at the time of its ratification. Their careers and legitimacy are tied to this interpretive framework.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, originalist_judges, agenda_setter,
    institutional, generational, identity_locked, national).

% Groups and political actors who benefit from a narrow interpretation of federal power and broader reserved powers for states, aligning with an originalist understanding of the constitutional structure.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, federalism_advocates, beneficiary,
    organized, generational, constrained, national).

% Individuals and groups whose claims to religious liberty are strengthened by an originalist interpretation of the First Amendment, often leading to exemptions from generally applicable laws.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, religious_liberty_claimants_originalist, beneficiary,
    moderate, biographical, constrained, national).

% Advocacy groups and economic actors who benefit from originalist interpretations that protect property rights and limit government regulation, particularly regarding economic activity.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, property_rights_defenders, beneficiary,
    organized, generational, constrained, national).

% Individuals and groups seeking recognition or protection for rights not explicitly listed in the Constitution, such as privacy or reproductive autonomy, which are often curtailed by originalist interpretations.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, unenumerated_rights_claimants, payer,
    powerless, biographical, trapped, national).

% Government agencies, progressive advocacy groups, and academics who advocate for federal power to address modern social and economic problems, finding their efforts constrained by originalist readings of federal authority.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, federal_regulatory_expansion_advocates, payer,
    organized, generational, constrained, national).

% Legal scholars, activists, and litigators who oppose originalism, viewing it as an impediment to social progress and the adaptation of constitutional principles to contemporary society. They bear the costs of adverse legal rulings.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, progressive_legal_movement, payer,
    organized, generational, constrained, national).

% Judges who adhere to a living constitutionalist approach, finding their interpretive methods and desired outcomes often foreclosed or marginalized by the dominance of originalist thought in certain judicial contexts.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, living_constitutionalist_judges, excluded,
    institutional, generational, identity_locked, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, historically grounded framework for interpreting the Constitution, aiming to limit judicial discretion and ensure fidelity to the original compact, thereby coordinating expectations about legal outcomes.
% TRANSFER_FUNCTION: Transfers interpretive authority from contemporary societal values or judicial discretion to historical texts and intentions, effectively transferring power to those whose interests align with the original understanding and away from those seeking evolving rights or expanded federal powers.
% ABSENT_VOICES: Advocates for unenumerated rights, expanded federal regulatory power, and those who believe constitutional meaning must evolve with society are structurally marginalized in originalist discourse, as their interpretive premises are often deemed illegitimate from the outset.
% DISAPPEARANCE_RATIONALE: If originalism as a dominant interpretive methodology vanished overnight, judicial decisions would immediately shift towards other interpretive modes (e.g., living constitutionalism, pragmatism), leading to significant changes in federal-state power balances, the scope of individual rights, and the legitimacy of federal regulation. The entire legal and political landscape would reorganize.
% FOUNDING_PROBLEM: The problem of judicial overreach and the need to constrain judges from imposing their own policy preferences under the guise of constitutional interpretation, ensuring the Constitution remains a fixed, ascertainable law.
% FOUNDING_PROBLEM_CORROBORATION: Originalist scholars and judges attest that judicial overreach remains a live problem, justifying the continued application of originalism. Critics, while acknowledging the historical concern, argue that originalism itself can lead to judicial activism by imposing outdated views, making the 'status' of the problem contested in its contemporary application.
narrative_ontology:disappearance_verdict(us_constitution_interpretive__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_interpretive__originalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_interpretive__originalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(us_constitution_interpretive__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_interpretive__originalist_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_interpretive__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_interpretive__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_interpretive__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high because this reading imposes a specific, historically bounded meaning that often conflicts with contemporary needs and values, leading to significant costs for those whose interests are not aligned with the original understanding. Suppression is also high, as it actively delegitimizes alternative interpretive methodologies and their outcomes, requiring continuous intellectual and political enforcement. The theater ratio is moderate, reflecting genuine scholarly effort in historical reconstruction alongside performative appeals to 'original intent' to justify specific policy outcomes. The increasing extractiveness and suppression over time reflect the growing dominance and enforcement of this interpretive mode in the legal system.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of originalist judges and beneficiaries, this constraint is a legitimate 'rope' that provides stability and fidelity to the Constitution. From the perspective of victims and excluded parties, it operates as a 'snare' or 'tangled rope,' extracting costs and suppressing alternative interpretations through an enforced, historically rigid framework. The engine's classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Originalist judges and the conservative legal movement act as agenda-setters and primary beneficiaries, as their interpretive framework gains institutional power. Federalism advocates, religious liberty claimants, and property rights defenders are direct beneficiaries. Unenumerated rights claimants, federal regulatory expansion advocates, and the progressive legal movement are victims, bearing the costs of constrained rights and governmental action. Living constitutionalist judges are excluded, as their interpretive approach is often dismissed as illegitimate within the originalist framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_meaning_ascertainability,
    'To what extent is ''original public meaning'' or ''framers'' intent'' genuinely ascertainable, given historical distance, incomplete records, and interpretive biases?',
    'Ongoing historical and linguistic scholarship, critical analysis of interpretive methodologies, and empirical studies of judicial decision-making. No definitive resolution is expected, but confidence in ascertainability can shift.',
    'If ascertainability is low, the ''originalist'' claim becomes more theatrical, and the constraint''s classification shifts towards a higher theater_ratio or even a piton, as its justification becomes more performative than functional. If high, it reinforces the coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_meaning_ascertainability, empirical, 'Ambiguity in the empirical basis of originalist interpretation.').

omega_variable(
    judicial_discretion_paradox,
    'Does originalism genuinely constrain judicial discretion, or does it merely shift the locus of discretion from contemporary values to historical interpretation, potentially allowing judges to ''find'' original meanings that align with their policy preferences?',
    'Comparative legal analysis of originalist vs. non-originalist judicial outcomes, and qualitative studies of judicial reasoning. This is a conceptual debate with ongoing empirical dimensions.',
    'If discretion is merely shifted, the constraint''s suppression of alternative interpretations becomes more extractive, as the coordination story (constraining judges) is undermined by the reality of continued, albeit re-framed, judicial power. This would push classification towards snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_discretion_paradox, conceptual, 'The paradox of originalism''s claim to constrain judicial discretion.').

omega_variable(
    originalism_vs_living_constitution_foreclosure,
    'Does the originalist reading logically foreclose the living constitution reading, or do they merely represent competing, coexisting interpretive frameworks?',
    'Philosophical analysis of the foundational premises of each reading. If one asserts a fixed meaning and the other an evolving meaning, they are logically contradictory within a single coherent legal system, leading to foreclosure. If they are merely different approaches to a shared text, they coexist.',
    'If originalism forecloses living constitutionalism, then the suppression of the latter is a structural consequence of the former''s adoption. If they merely coexist, then the suppression is an active, extractive choice to marginalize a viable alternative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(originalism_vs_living_constitution_foreclosure, conceptual, 'Whether originalism logically forecloses living constitutionalism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_interpretive__originalist_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t1970, us_constitution_interpretive__originalist_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(us_c_tr_t1985, us_constitution_interpretive__originalist_reading, theater_ratio, 1985, 0.12).
narrative_ontology:measurement(us_c_tr_t2000, us_constitution_interpretive__originalist_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(us_c_tr_t2010, us_constitution_interpretive__originalist_reading, theater_ratio, 2010, 0.18).
narrative_ontology:measurement(us_c_tr_t2024, us_constitution_interpretive__originalist_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(us_c_be_t1970, us_constitution_interpretive__originalist_reading, base_extractiveness, 1970, 0.4).
narrative_ontology:measurement(us_c_be_t1985, us_constitution_interpretive__originalist_reading, base_extractiveness, 1985, 0.5).
narrative_ontology:measurement(us_c_be_t2000, us_constitution_interpretive__originalist_reading, base_extractiveness, 2000, 0.58).
narrative_ontology:measurement(us_c_be_t2010, us_constitution_interpretive__originalist_reading, base_extractiveness, 2010, 0.62).
narrative_ontology:measurement(us_c_be_t2024, us_constitution_interpretive__originalist_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t1970, us_constitution_interpretive__originalist_reading, suppression_requirement, 1970, 0.45).
narrative_ontology:measurement(us_c_su_t1985, us_constitution_interpretive__originalist_reading, suppression_requirement, 1985, 0.55).
narrative_ontology:measurement(us_c_su_t2000, us_constitution_interpretive__originalist_reading, suppression_requirement, 2000, 0.63).
narrative_ontology:measurement(us_c_su_t2010, us_constitution_interpretive__originalist_reading, suppression_requirement, 2010, 0.68).
narrative_ontology:measurement(us_c_su_t2024, us_constitution_interpretive__originalist_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_interpretive__originalist_reading, identity_coordination).
narrative_ontology:affects_constraint(us_constitution_interpretive__originalist_reading, us_constitution_interpretive__living_constitution_reading).
narrative_ontology:affects_constraint(us_constitution_interpretive__originalist_reading, us_constitution_interpretive__popular_constitutionalism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'us_constitution_interpretive' kernel. This originalist reading emphasizes fixed meaning at ratification, contrasting with the living constitution's evolving meaning and popular constitutionalism's democratic contestation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
