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
    narrative_ontology:constraint_vindicates/2,
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
 *   human_readable: US Constitution: Originalist Interpretation
 *   domain: constitutional_law/legal_interpretation/political_theory
 *
 * SUMMARY:
 *   This constraint represents the originalist reading of the US
 *   Constitution, where its meaning is fixed at the time of ratification, and
 *   interpretive authority derives from fidelity to the framers' intent or
 *   original public meaning. This reading aims to provide stability and limit
 *   judicial discretion but often results in outcomes that constrain modern
 *   policy and rights claims. This is one reading of the
 *   'us_constitution_interpretive' kernel, alongside
 *   'living_constitution_reading' and 'popular_constitutionalism_reading'.
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
narrative_ontology:human_readable(us_constitution_interpretive__originalist_reading, "US Constitution: Originalist Interpretation").
narrative_ontology:topic_domain(us_constitution_interpretive__originalist_reading, "constitutional_law/legal_interpretation/political_theory").

domain_priors:requires_active_enforcement(us_constitution_interpretive__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_interpretive__originalist_reading, '44c087bc-9191-4400-9653-b6415360c043').
narrative_ontology:cs_kernel_codification('44c087bc-9191-4400-9653-b6415360c043', fixed_text).
narrative_ontology:cs_authority_grounding('44c087bc-9191-4400-9653-b6415360c043', lineage).
narrative_ontology:cs_interpretation_layer_present('44c087bc-9191-4400-9653-b6415360c043').
narrative_ontology:cs_reading_relation('44c087bc-9191-4400-9653-b6415360c043', us_constitution_interpretive__living_constitution_reading, forecloses).
narrative_ontology:cs_reading_relation('44c087bc-9191-4400-9653-b6415360c043', us_constitution_interpretive__popular_constitutionalism_reading, coexists_with).
narrative_ontology:cs_axiom('44c087bc-9191-4400-9653-b6415360c043', foundational, constitutional_meaning_fixed_at_ratification).
narrative_ontology:cs_axiom_status(constitutional_meaning_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('44c087bc-9191-4400-9653-b6415360c043', constitutional_meaning_fixed_at_ratification, conventional).
narrative_ontology:cs_axiom('44c087bc-9191-4400-9653-b6415360c043', foundational, judicial_fidelity_to_original_intent).
narrative_ontology:cs_axiom_status(judicial_fidelity_to_original_intent, holdable).
narrative_ontology:cs_axiom_grounding('44c087bc-9191-4400-9653-b6415360c043', judicial_fidelity_to_original_intent, deontological).
narrative_ontology:cs_reference_frame('44c087bc-9191-4400-9653-b6415360c043', constitutional_text_as_fixed_law).
narrative_ontology:cs_drift_state('44c087bc-9191-4400-9653-b6415360c043', contemporary_legal_discourse, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('44c087bc-9191-4400-9653-b6415360c043', '').
narrative_ontology:cs_kernel_id(us_constitution_interpretive__originalist_reading, us_constitution_interpretive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__originalist_reading, federalism_advocates).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__originalist_reading, religious_liberty_originalists).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__originalist_reading, property_rights_defenders).
narrative_ontology:constraint_victim(us_constitution_interpretive__originalist_reading, unenumerated_rights_claimants).
narrative_ontology:constraint_victim(us_constitution_interpretive__originalist_reading, federal_regulatory_expansion_advocates).
narrative_ontology:constraint_victim(us_constitution_interpretive__originalist_reading, social_justice_advocates).
narrative_ontology:constraint_vindicates(us_constitution_interpretive__originalist_reading, rule_of_law_doctrine).
narrative_ontology:constraint_vindicates(us_constitution_interpretive__originalist_reading, separation_of_powers_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Judges who adhere to originalist principles, interpreting the Constitution based on its meaning at the time of ratification. They actively enforce this interpretive method, shaping legal outcomes and limiting the scope of federal power and individual rights to historical understandings.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, originalist_judges, agenda_setter,
    institutional, generational, identity_locked, national).

% Political and legal groups who benefit from a narrow interpretation of federal power and broader reserved powers for states, aligning with the original understanding of the Constitution's structure. They gain through judicial decisions that limit federal overreach.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, federalism_advocates, beneficiary,
    organized, generational, constrained, national).

% Advocates who seek to protect religious freedoms based on the original understanding of the First Amendment, often leading to outcomes that favor religious practices over state regulations or other individual rights claims.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, religious_liberty_originalists, beneficiary,
    organized, biographical, constrained, national).

% Groups that benefit from originalist interpretations that strengthen property rights protections, often limiting government's ability to regulate land use or economic activity.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, property_rights_defenders, beneficiary,
    organized, biographical, constrained, national).

% Individuals and groups seeking recognition or protection for rights not explicitly listed in the Constitution (e.g., privacy, reproductive rights). Originalist interpretations often deny or severely limit these claims, making them victims of this interpretive constraint.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, unenumerated_rights_claimants, payer,
    powerless, biographical, trapped, national).

% Groups and government agencies that advocate for broader federal power to address modern social and economic issues. Originalism constrains their ability to enact and enforce regulations by limiting federal authority to its 1787 scope.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, federal_regulatory_expansion_advocates, payer,
    organized, biographical, constrained, national).

% Advocates for civil rights, equality, and social welfare who find their goals constrained by originalist interpretations that prioritize historical understandings over evolving societal norms and needs. They bear the cost of limited judicial avenues for progressive change.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, social_justice_advocates, payer,
    organized, generational, constrained, national).

% Legal scholars, judges, and activists who argue for an evolving constitutional meaning. While present in the broader legal discourse, their interpretive approach is actively suppressed within the originalist framework, limiting their influence on judicial outcomes.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, living_constitution_advocates, excluded,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, predictable framework for constitutional interpretation, aiming to limit judicial discretion and ensure fidelity to the original compact, thereby coordinating legal actors around a fixed meaning.
% TRANSFER_FUNCTION: Transfers interpretive authority from contemporary societal values and judicial discretion to historical texts and intentions, effectively transferring power from evolving majorities to past framers and their historical context. This constrains federal power and expands state power in certain areas, while limiting the scope of individual rights.
% ABSENT_VOICES: Advocates for unenumerated rights, federal regulatory expansion, and social justice are often marginalized or excluded from the interpretive process, as their claims are deemed outside the original understanding. Their voices would challenge the fixed nature of constitutional meaning and argue for a more adaptable framework.
% DISAPPEARANCE_RATIONALE: If originalist interpretation vanished overnight, the legal landscape would undergo significant rearrangement. Judicial decisions would likely shift towards more expansive federal powers and a broader recognition of unenumerated rights, leading to new legislative and regulatory frameworks. The balance of power between federal and state governments would be fundamentally altered.
% FOUNDING_PROBLEM: The problem of judicial overreach and the desire to prevent judges from imposing their own policy preferences under the guise of constitutional interpretation, ensuring a stable and democratically legitimate legal framework.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, political scientists, and public opinion polls outside the immediate originalist movement corroborate the ongoing concern about judicial activism and the need for interpretive constraint, even if they disagree on the specific method. This concern is widely attested across the political spectrum.
narrative_ontology:disappearance_verdict(us_constitution_interpretive__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_interpretive__originalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_interpretive__originalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates legal interpretation around a historical text (beneficiaries exist) but also involves significant asymmetric extraction (victims exist) and requires active enforcement by originalist judges to suppress alternative interpretive methods. Extractiveness is high (0.65) due to the denial of evolving rights and the limitation of federal power to address contemporary issues. Suppression is also high (0.70) as originalist judges actively reject and delegitimize non-originalist arguments. The theater ratio is low (0.20) because the interpretive method is genuinely applied, not merely performed, though some arguments may be framed to fit originalist rhetoric.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of originalist judges and beneficiaries, this constraint is a legitimate and necessary framework for constitutional governance, ensuring stability and democratic accountability. From the perspective of victims (e.g., unenumerated rights claimants), it is an extractive mechanism that denies fundamental rights and entrenches historical inequalities, sustained by judicial power.
 *
 * DIRECTIONALITY LOGIC:
 *   Originalist judges are the agenda-setters, actively shaping and enforcing the interpretive framework. Federalism advocates, religious liberty originalists, and property rights defenders are beneficiaries, as their policy goals are advanced by originalist rulings. Unenumerated rights claimants, federal regulatory expansion advocates, and social justice advocates are victims, as their claims are often denied or constrained by this interpretive method. Living Constitution advocates are excluded, as their interpretive framework is actively resisted within the originalist discourse.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Tangled Rope prevents mislabeling this as a pure Mountain (natural law) or a pure Rope (simple coordination). While it provides a coordination function (interpretive stability), the significant and increasing extraction from identifiable victims, coupled with active enforcement, indicates it is not a benign or self-sustaining coordination mechanism. The persistence of the founding problem (judicial overreach) is contested, suggesting that while the original mandate may still be relevant, its application has become substantially extractive for certain groups.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_meaning_determinacy,
    'To what extent is ''original public meaning'' or ''framers'' intent'' a truly determinate and discoverable concept, rather than a construct influenced by contemporary interpretive biases?',
    'Extensive historical and linguistic analysis across a wide range of constitutional provisions, coupled with empirical studies of judicial decision-making to detect patterns of ''motivated reasoning'' in originalist application.',
    'If highly indeterminate, the constraint''s claimed stability and objectivity would be undermined, revealing a higher degree of judicial discretion and potential for extraction than acknowledged, potentially reclassifying it closer to a Snare or a more extractive Tangled Rope. If highly determinate, it would strengthen the claim of a stable, non-discretionary interpretive framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_meaning_determinacy, empirical, 'The determinacy of original meaning as an interpretive guide.').

omega_variable(
    legitimacy_of_historical_constraint,
    'Is it normatively legitimate for the constitutional meaning to be fixed by historical understandings, even if those understandings conflict with contemporary moral or social values?',
    'Philosophical and political theory debates on intergenerational equity, democratic legitimacy, and the nature of constitutionalism. This is a preference-based question with no empirical resolution.',
    'If deemed illegitimate, the constraint''s moral authority would collapse, leading to calls for its abandonment or fundamental revision, regardless of its structural properties. If deemed legitimate, its persistence would be justified on normative grounds, even if extractive for some.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_of_historical_constraint, preference, 'Normative legitimacy of historical constitutional constraint.').

omega_variable(
    judicial_vs_popular_sovereignty,
    'Does originalism genuinely constrain judicial power in favor of popular sovereignty, or does it merely shift the locus of interpretive authority from contemporary judges to historical figures, still bypassing contemporary democratic processes?',
    'Comparative analysis of judicial outcomes under originalist vs. non-originalist regimes, assessing the degree of deference to legislative bodies and the responsiveness to popular will. Conceptual analysis of the nature of sovereignty in a constitutional republic.',
    'If it primarily shifts authority to historical figures, the claim of democratic constraint is weakened, and the interpretive method''s extractive potential (by denying contemporary democratic expression) is amplified. If it genuinely defers to contemporary legislative processes, its coordination function is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_vs_popular_sovereignty, conceptual, 'Originalism''s impact on the balance between judicial and popular sovereignty.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_interpretive__originalist_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t1970, us_constitution_interpretive__originalist_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(us_c_tr_t1985, us_constitution_interpretive__originalist_reading, theater_ratio, 1985, 0.15).
narrative_ontology:measurement(us_c_tr_t2000, us_constitution_interpretive__originalist_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(us_c_tr_t2010, us_constitution_interpretive__originalist_reading, theater_ratio, 2010, 0.2).
narrative_ontology:measurement(us_c_tr_t2024, us_constitution_interpretive__originalist_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(us_c_be_t1970, us_constitution_interpretive__originalist_reading, base_extractiveness, 1970, 0.4).
narrative_ontology:measurement(us_c_be_t1985, us_constitution_interpretive__originalist_reading, base_extractiveness, 1985, 0.5).
narrative_ontology:measurement(us_c_be_t2000, us_constitution_interpretive__originalist_reading, base_extractiveness, 2000, 0.58).
narrative_ontology:measurement(us_c_be_t2010, us_constitution_interpretive__originalist_reading, base_extractiveness, 2010, 0.62).
narrative_ontology:measurement(us_c_be_t2024, us_constitution_interpretive__originalist_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t1970, us_constitution_interpretive__originalist_reading, suppression_requirement, 1970, 0.3).
narrative_ontology:measurement(us_c_su_t1985, us_constitution_interpretive__originalist_reading, suppression_requirement, 1985, 0.45).
narrative_ontology:measurement(us_c_su_t2000, us_constitution_interpretive__originalist_reading, suppression_requirement, 2000, 0.55).
narrative_ontology:measurement(us_c_su_t2010, us_constitution_interpretive__originalist_reading, suppression_requirement, 2010, 0.65).
narrative_ontology:measurement(us_c_su_t2024, us_constitution_interpretive__originalist_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_interpretive__originalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_interpretive__originalist_reading, us_constitution_interpretive__living_constitution_reading).
narrative_ontology:affects_constraint(us_constitution_interpretive__originalist_reading, us_constitution_interpretive__popular_constitutionalism_reading).
narrative_ontology:affects_constraint(us_constitution_interpretive__originalist_reading, federal_regulatory_power_scope).
narrative_ontology:affects_constraint(us_constitution_interpretive__originalist_reading, individual_rights_scope).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'us_constitution_interpretive' kernel. Its structural influence on other readings and related constraints is explicitly modeled.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
