% ============================================================================
% CONSTRAINT STORY: us_constitution_meaning__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_meaning__originalist_reading, []).

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
 *   constraint_id: us_constitution_meaning__originalist_reading
 *   human_readable: US Constitutional Meaning (Originalist Reading)
 *   domain: constitutional_law/legal_theory/political_philosophy
 *
 * SUMMARY:
 *   This constraint describes the 'originalist' reading of US Constitutional
 *   meaning, where judges are bound by the historical public meaning of the
 *   text at the time of its ratification or amendment. This is one reading of
 *   the 'us_constitution_meaning' kernel. The constraint operates to suppress
 *   interpretations that deviate from this fixed historical meaning,
 *   benefiting those who advocate for counter-majoritarian constraints and
 *   limiting the claims of rights based on evolving societal standards. The
 *   metrics reflect a growing entrenchment and extractiveness of this
 *   interpretive approach over time.
 *
 * KEY AGENTS:
 *   - originalist_advocates: Agenda setter (institutional/identity_locked) — actively promotes and applies originalism, benefits from its perceived objectivity.
 *   - counter_majoritarian_constraint_advocates: Beneficiary (organized/constrained) — uses originalism to limit contemporary majorities.
 *   - rights_claimants_lacking_historical_support: Payer (powerless/trapped) — bears the cost of having claims denied due to lack of historical basis.
 *   - legislative_bodies_seeking_flexibility: Payer (institutional/constrained) — constrained in adapting laws to contemporary needs.
 *   - living_constitutionalist_advocates: Excluded (institutional/identity_locked) — their interpretive methodology is often dismissed or suppressed.
 *   - constitutional_scholars_and_historians: Observer (analytical/analytical) — critiques originalism without direct participation as a party.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_meaning__originalist_reading, 0.65).
domain_priors:suppression_score(us_constitution_meaning__originalist_reading, 0.78).
domain_priors:theater_ratio(us_constitution_meaning__originalist_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_meaning__originalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_meaning__originalist_reading, "US Constitutional Meaning (Originalist Reading)").
narrative_ontology:topic_domain(us_constitution_meaning__originalist_reading, "constitutional_law/legal_theory/political_philosophy").

domain_priors:requires_active_enforcement(us_constitution_meaning__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_meaning__originalist_reading, '2a834677-ebd5-44fe-a098-2991e5ded0b2').
narrative_ontology:cs_kernel_codification('2a834677-ebd5-44fe-a098-2991e5ded0b2', fixed_text).
narrative_ontology:cs_authority_grounding('2a834677-ebd5-44fe-a098-2991e5ded0b2', lineage).
narrative_ontology:cs_interpretation_layer_present('2a834677-ebd5-44fe-a098-2991e5ded0b2').
narrative_ontology:cs_reading_relation('2a834677-ebd5-44fe-a098-2991e5ded0b2', us_constitution_meaning__living_constitutionalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('2a834677-ebd5-44fe-a098-2991e5ded0b2', us_constitution_meaning__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('2a834677-ebd5-44fe-a098-2991e5ded0b2', foundational, constitutional_meaning_fixed_at_ratification).
narrative_ontology:cs_axiom_status(constitutional_meaning_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('2a834677-ebd5-44fe-a098-2991e5ded0b2', constitutional_meaning_fixed_at_ratification, conventional).
narrative_ontology:cs_axiom('2a834677-ebd5-44fe-a098-2991e5ded0b2', foundational, judicial_role_limited_to_historical_meaning).
narrative_ontology:cs_axiom_status(judicial_role_limited_to_historical_meaning, holdable).
narrative_ontology:cs_axiom_grounding('2a834677-ebd5-44fe-a098-2991e5ded0b2', judicial_role_limited_to_historical_meaning, deontological).
narrative_ontology:cs_reference_frame('2a834677-ebd5-44fe-a098-2991e5ded0b2', original_public_meaning_framework).
narrative_ontology:cs_drift_state('2a834677-ebd5-44fe-a098-2991e5ded0b2', contemporary_legal_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('2a834677-ebd5-44fe-a098-2991e5ded0b2', '').
narrative_ontology:cs_kernel_id(us_constitution_meaning__originalist_reading, us_constitution_meaning).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_meaning__originalist_reading, originalist_advocates).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__originalist_reading, counter_majoritarian_constraint_advocates).
narrative_ontology:constraint_victim(us_constitution_meaning__originalist_reading, rights_claimants_lacking_historical_support).
narrative_ontology:constraint_victim(us_constitution_meaning__originalist_reading, legislative_bodies_seeking_flexibility).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Judges, scholars, and political actors who actively promote and apply the originalist methodology. They benefit from the perceived stability and objectivity of fixed meaning, which limits judicial discretion and reinforces a particular vision of constitutional governance. Their professional and ideological identities are deeply tied to this interpretive approach.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, originalist_advocates, agenda_setter,
    institutional, generational, identity_locked, national).

% Groups and individuals who seek to limit the power of contemporary majorities and legislative bodies, often by appealing to a fixed, higher law. Originalism provides a powerful tool for this, as it anchors constitutional meaning in a past moment, making it resistant to current political pressures. They benefit from the outcomes originalism produces.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, counter_majoritarian_constraint_advocates, beneficiary,
    organized, generational, constrained, national).

% Individuals and groups whose claims to rights or protections are based on evolving social norms, scientific understanding, or contemporary moral principles, but lack clear support in the historical public meaning of the Constitution at its ratification or amendment. They bear the cost of having their claims denied or curtailed by originalist interpretations.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, rights_claimants_lacking_historical_support, payer,
    powerless, biographical, trapped, national).

% Elected officials and legislative institutions that seek to adapt laws and policies to contemporary challenges and societal values. Originalism constrains their ability to interpret the Constitution in a way that allows for such adaptation, forcing them to either amend the Constitution (a high bar) or operate within historically defined limits.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, legislative_bodies_seeking_flexibility, payer,
    institutional, biographical, constrained, national).

% Judges, scholars, and political actors who argue that the Constitution's meaning evolves over time to meet contemporary needs. While they participate in the broader legal discourse, their interpretive methodology is often dismissed or actively suppressed by originalist-dominated courts, effectively excluding their approach from becoming dominant.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, living_constitutionalist_advocates, excluded,
    institutional, generational, identity_locked, national).

% Academics who study the Constitution's history, text, and interpretation. They provide the historical evidence and theoretical frameworks that originalists claim to rely on, but often critique the selective use of history or the philosophical underpinnings of originalism. They observe the contest without directly participating as a party.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, constitutional_scholars_and_historians, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, predictable framework for constitutional interpretation by fixing meaning at a specific historical point, aiming to reduce judicial arbitrariness and ensure fidelity to the framers' intent.
% TRANSFER_FUNCTION: Transfers interpretive authority from contemporary judges and evolving societal norms to historical evidence and the presumed intent of the framers/ratifiers, thereby limiting the scope of rights and legislative flexibility not explicitly supported by that historical record.
% ABSENT_VOICES: Future generations and their evolving moral and social understandings are structurally absent from the originalist interpretive process, as their perspectives are deemed irrelevant to the fixed meaning of the past. Their 'voice' is only heard through the difficult process of constitutional amendment.
% DISAPPEARANCE_RATIONALE: If originalism as a dominant interpretive methodology vanished overnight, constitutional jurisprudence would immediately shift towards more flexible, evolving interpretations. Many established precedents would be re-evaluated, new rights claims would gain traction, and the balance of power between the judiciary and other branches would be reconfigured, leading to a significant rearrangement of legal and political structures.
% FOUNDING_PROBLEM: The problem of judicial activism and the perceived politicization of constitutional law, where judges were seen as imposing their own policy preferences rather than interpreting the law.
% FOUNDING_PROBLEM_CORROBORATION: Originalist advocates strongly attest that judicial activism remains a live problem, justifying the need for fixed meaning. Critics, including living constitutionalist advocates and many legal scholars, acknowledge the historical problem of judicial overreach but argue that originalism itself has become a tool for ideological outcomes, making the 'solution' part of a new problem. Independent legal analysis from outside the benefiting parties supports the view that the problem of judicial discretion, in various forms, persists.
narrative_ontology:disappearance_verdict(us_constitution_meaning__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_meaning__originalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_meaning__originalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(us_constitution_meaning__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_meaning__originalist_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_meaning__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_meaning__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_meaning__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high because originalism, while claiming objectivity, often produces outcomes that align with conservative political goals, effectively transferring power and limiting rights in ways that benefit specific ideological factions. Suppression is very high because the methodology actively seeks to delegitimize and exclude alternative interpretive approaches, requiring significant institutional effort to maintain its dominance. Theater ratio is low because the historical research and legal argumentation involved are genuinely complex, though critics argue some of it serves to rationalize predetermined outcomes. The increasing trend in extractiveness and suppression reflects the growing institutionalization and political salience of originalism over the given interval.
 *
 * PERSPECTIVAL GAP:
 *   Originalist advocates perceive this constraint as a 'rope' or even a 'mountain' – a necessary, objective method for maintaining constitutional fidelity and limiting judicial overreach. From the perspective of rights claimants and legislative bodies, it operates as a 'snare' or 'tangled rope,' extracting flexibility and denying claims based on an arbitrarily fixed past, enforced by institutional power. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Originalist advocates and counter-majoritarian constraint advocates are clear beneficiaries, as the constraint's operation aligns with their ideological and political goals. Rights claimants lacking historical support and legislative bodies seeking flexibility are clear victims, as their interests are directly curtailed. Living constitutionalist advocates are excluded, as their interpretive framework is actively marginalized. Constitutional scholars are observers, analyzing the constraint's operation without direct benefit or cost.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate is to prevent judicial activism and ensure stable constitutional meaning. While the problem of judicial discretion remains 'live,' critics argue that originalism itself has become a vehicle for a new form of activism, where historical interpretation is used to achieve specific policy outcomes. This suggests a potential for mandatrophy, where the original coordination function (limiting judicial overreach) is being subverted by an extractive function (achieving ideological ends). The high extractiveness and suppression, coupled with the 'contested' status of the founding problem, indicate that the constraint is not merely coordinating but actively extracting and suppressing alternatives under the guise of its original mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine reading of the US Constitution, or a political ideology masquerading as a legal methodology?',
    'Analysis of the consistency and coherence of originalist application across diverse legal issues, and the degree to which its outcomes are predictable from its stated methodology versus the political leanings of its proponents.',
    'If it is primarily a political ideology, its classification would shift towards a ''snare'' or ''tangled rope'' with higher extractiveness and suppression, as its coordination function would be revealed as cover for political ends. If it is a genuine methodology, its classification as a ''tangled rope'' would remain, but the balance between coordination and extraction might be re-evaluated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Ambiguity between a legal methodology and a political ideology.').

omega_variable(
    historical_meaning_determinacy,
    'Is the ''historical public meaning'' of the Constitution genuinely determinate and discoverable, or is it inherently ambiguous and subject to contemporary interpretive choices?',
    'Empirical studies by historians and linguists on the determinacy of 18th-century legal language, and analysis of the degree of consensus among originalist scholars on specific historical meanings.',
    'If historical meaning is largely indeterminate, the ''fixed'' nature of the constraint is theatrical, and its suppression of alternative readings is based on a false premise, increasing its ''theater_ratio'' and ''extractiveness''. If it is determinate, the constraint''s claims to objectivity are strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_meaning_determinacy, empirical, 'The determinacy of historical public meaning.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (institutional barriers to non-originalist interpretations) or internalized (judicial self-censorship due to professional pressure)?',
    'Post-exit suppression trajectory: if judges who leave originalist-dominated courts continue to self-censor, reclassify as partially internalized. Analysis of judicial opinions for explicit vs. implicit rejection of non-originalist arguments.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — judges carry the suppression with them. If purely structural, removing the institutional barriers would immediately free up interpretive space.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in judicial interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_meaning__originalist_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t1970, us_constitution_meaning__originalist_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(us_c_tr_t1985, us_constitution_meaning__originalist_reading, theater_ratio, 1985, 0.12).
narrative_ontology:measurement(us_c_tr_t2000, us_constitution_meaning__originalist_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(us_c_tr_t2010, us_constitution_meaning__originalist_reading, theater_ratio, 2010, 0.18).
narrative_ontology:measurement(us_c_tr_t2024, us_constitution_meaning__originalist_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(us_c_be_t1970, us_constitution_meaning__originalist_reading, base_extractiveness, 1970, 0.4).
narrative_ontology:measurement(us_c_be_t1985, us_constitution_meaning__originalist_reading, base_extractiveness, 1985, 0.5).
narrative_ontology:measurement(us_c_be_t2000, us_constitution_meaning__originalist_reading, base_extractiveness, 2000, 0.58).
narrative_ontology:measurement(us_c_be_t2010, us_constitution_meaning__originalist_reading, base_extractiveness, 2010, 0.62).
narrative_ontology:measurement(us_c_be_t2024, us_constitution_meaning__originalist_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t1970, us_constitution_meaning__originalist_reading, suppression_requirement, 1970, 0.5).
narrative_ontology:measurement(us_c_su_t1985, us_constitution_meaning__originalist_reading, suppression_requirement, 1985, 0.6).
narrative_ontology:measurement(us_c_su_t2000, us_constitution_meaning__originalist_reading, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement(us_c_su_t2010, us_constitution_meaning__originalist_reading, suppression_requirement, 2010, 0.75).
narrative_ontology:measurement(us_c_su_t2024, us_constitution_meaning__originalist_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_meaning__originalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_meaning__originalist_reading, us_constitution_meaning__living_constitutionalist_reading).
narrative_ontology:affects_constraint(us_constitution_meaning__originalist_reading, us_constitution_meaning__positivist_reading).
narrative_ontology:affects_constraint(us_constitution_meaning__originalist_reading, judicial_review_doctrine).
narrative_ontology:affects_constraint(us_constitution_meaning__originalist_reading, separation_of_powers_doctrine).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'us_constitution_meaning' kernel. This originalist reading directly influences the legitimacy and operational space of the living constitutionalist and positivist readings by asserting a fixed interpretive methodology.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
