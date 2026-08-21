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
 *   constraint_id: us_constitution_meaning__originalist_reading
 *   human_readable: US Constitutional Meaning: Originalist Reading
 *   domain: constitutional_law/legal_theory/political_philosophy
 *
 * SUMMARY:
 *   This constraint describes the 'originalist reading' of US Constitutional
 *   meaning, where judges are bound by the historical public meaning of the
 *   text at the time of its ratification or amendment. This reading asserts
 *   that contemporary circumstances are irrelevant to the Constitution's
 *   meaning, though they may be relevant to its application. It functions as
 *   a counter-majoritarian constraint, often suppressing rights claims that
 *   lack 18th-century historical support. The constraint is claimed as a
 *   'rope' by its proponents (a stable, objective interpretive method) but
 *   operates with significant extraction and suppression, leading to a
 *   computed 'tangled_rope' or 'snare' classification from the perspective of
 *   those it constrains.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_meaning__originalist_reading, 0.68).
domain_priors:suppression_score(us_constitution_meaning__originalist_reading, 0.75).
domain_priors:theater_ratio(us_constitution_meaning__originalist_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_meaning__originalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_meaning__originalist_reading, "US Constitutional Meaning: Originalist Reading").
narrative_ontology:topic_domain(us_constitution_meaning__originalist_reading, "constitutional_law/legal_theory/political_philosophy").

domain_priors:requires_active_enforcement(us_constitution_meaning__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_meaning__originalist_reading, '2975468e-ea9f-4d2b-ad10-33eb283ef3c3').
narrative_ontology:cs_kernel_codification('2975468e-ea9f-4d2b-ad10-33eb283ef3c3', fixed_text).
narrative_ontology:cs_authority_grounding('2975468e-ea9f-4d2b-ad10-33eb283ef3c3', lineage).
narrative_ontology:cs_interpretation_layer_present('2975468e-ea9f-4d2b-ad10-33eb283ef3c3').
narrative_ontology:cs_reading_relation('2975468e-ea9f-4d2b-ad10-33eb283ef3c3', us_constitution_meaning__living_constitutionalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('2975468e-ea9f-4d2b-ad10-33eb283ef3c3', us_constitution_meaning__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('2975468e-ea9f-4d2b-ad10-33eb283ef3c3', foundational, constitutional_meaning_fixed_at_ratification).
narrative_ontology:cs_axiom_status(constitutional_meaning_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('2975468e-ea9f-4d2b-ad10-33eb283ef3c3', constitutional_meaning_fixed_at_ratification, conventional).
narrative_ontology:cs_axiom('2975468e-ea9f-4d2b-ad10-33eb283ef3c3', foundational, judicial_role_limited_to_original_meaning).
narrative_ontology:cs_axiom_status(judicial_role_limited_to_original_meaning, holdable).
narrative_ontology:cs_axiom_grounding('2975468e-ea9f-4d2b-ad10-33eb283ef3c3', judicial_role_limited_to_original_meaning, deontological).
narrative_ontology:cs_reference_frame('2975468e-ea9f-4d2b-ad10-33eb283ef3c3', original_public_meaning_framework).
narrative_ontology:cs_drift_state('2975468e-ea9f-4d2b-ad10-33eb283ef3c3', contemporary_legal_discourse, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('2975468e-ea9f-4d2b-ad10-33eb283ef3c3', '').
narrative_ontology:cs_kernel_id(us_constitution_meaning__originalist_reading, us_constitution_meaning).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_meaning__originalist_reading, originalist_advocates).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__originalist_reading, conservative_legal_movement).
narrative_ontology:constraint_victim(us_constitution_meaning__originalist_reading, rights_claimants_lacking_historical_support).
narrative_ontology:constraint_victim(us_constitution_meaning__originalist_reading, progressive_legal_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(us_constitution_meaning__originalist_reading, the_public).
narrative_ontology:constraint_vindicates(us_constitution_meaning__originalist_reading, rule_of_law_doctrine).
narrative_ontology:constraint_vindicates(us_constitution_meaning__originalist_reading, judicial_restraint_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Judges, scholars, and activists who actively promote and apply the originalist methodology. They benefit from the stability and perceived legitimacy of a fixed constitutional meaning, which often aligns with their policy preferences or judicial philosophy. Their professional identity is deeply tied to this interpretive approach.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, originalist_advocates, agenda_setter,
    institutional, generational, identity_locked, national).

% A broad coalition of legal professionals and political actors who find that originalist interpretations often produce outcomes consistent with their policy goals, particularly in limiting government power or preserving traditional social structures. They benefit from the constraint's counter-majoritarian potential.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, conservative_legal_movement, beneficiary,
    organized, generational, constrained, national).

% Individuals or groups seeking to assert rights (e.g., privacy, environmental protection, evolving equality claims) that are not explicitly enumerated or clearly supported by the historical public meaning of the Constitution at the time of its ratification or amendment. They bear the cost of having their claims denied or severely limited by this interpretive framework.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, rights_claimants_lacking_historical_support, payer,
    powerless, immediate, trapped, national).

% Academics and legal practitioners who advocate for interpretations of the Constitution that evolve with societal norms and contemporary challenges. They find their arguments often suppressed or dismissed by originalist courts, limiting their influence on legal development and policy.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, progressive_legal_scholars, payer,
    moderate, biographical, constrained, national).

% Judges who believe the Constitution's principles should be applied in light of contemporary circumstances and values. While they may sit on the same courts, their interpretive methodology is often marginalized or explicitly rejected by originalist majorities, effectively excluding their approach from becoming binding precedent.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, living_constitutionalist_judges, excluded,
    institutional, biographical, identity_locked, national).

% The general populace, whose ability to shape constitutional meaning through democratic processes is constrained by the originalist framework's emphasis on historical intent over contemporary consensus. They pay the cost of a less responsive constitutional order, though some segments may benefit from its stability.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, the_public, payer,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, objective, and predictable framework for constitutional interpretation, aiming to limit judicial discretion and ensure fidelity to the original public meaning, thereby coordinating legal actors around a common interpretive method.
% TRANSFER_FUNCTION: Transfers interpretive authority from contemporary judges and evolving societal norms to historical evidence and the original public meaning of the text, effectively transferring power from present-day majorities to past framers and ratifiers. This results in the denial of certain rights claims and the suppression of alternative legal arguments.
% ABSENT_VOICES: Living constitutionalist judges and scholars, as well as rights claimants whose arguments rely on evolving standards, are often structurally excluded from the interpretive conversation when originalism dominates. Their perspectives are deemed irrelevant to constitutional meaning, though they may influence application.
% DISAPPEARANCE_RATIONALE: If the originalist reading of constitutional meaning vanished overnight, the interpretive landscape would immediately shift. Judges would be free to consider contemporary values and circumstances more broadly, leading to new legal arguments, different outcomes in rights cases, and a re-evaluation of precedent. The balance of power between the judiciary and other branches, and between past and present generations, would fundamentally reorganize.
% FOUNDING_PROBLEM: To prevent judicial activism and ensure that constitutional interpretation remains tethered to a fixed, ascertainable meaning, thereby preserving the democratic legitimacy of the Constitution by preventing judges from imposing their own policy preferences.
% FOUNDING_PROBLEM_CORROBORATION: Originalist advocates and conservative legal movements strongly attest that the problem of judicial overreach and subjective interpretation remains live. Critics, including living constitutionalists, acknowledge the concern about judicial activism but argue that originalism itself can be a form of activism, or that its solutions are inadequate for a dynamic society. The debate over judicial legitimacy is ongoing, corroborating the problem's persistence from outside the benefiting parties.
narrative_ontology:disappearance_verdict(us_constitution_meaning__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_meaning__originalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_meaning__originalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(us_constitution_meaning__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_meaning__originalist_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness (0.68) is high because the originalist framework systematically denies or limits rights claims that do not align with historical understandings, imposing a significant cost on those seeking to adapt the Constitution to modern challenges. Suppression (0.75) is also high, as it actively excludes alternative interpretive methodologies and arguments based on evolving societal norms. The 'requires_active_enforcement' flag is true because originalist majorities on courts must actively suppress non-originalist arguments and precedents to maintain the coherence and dominance of their framework. Theater ratio (0.20) is relatively low, as the commitment to historical inquiry is genuine, though critics argue its application can be selective.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of originalist advocates, this constraint is a legitimate 'rope' that ensures judicial restraint and fidelity to the text. From the perspective of rights claimants and progressive scholars, it operates as a 'snare' or 'tangled_rope,' extracting from them by denying their claims and suppressing alternative interpretations. The engine's classification will reflect this divergence based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Originalist advocates and the conservative legal movement are clear beneficiaries (d near 0.0), as this reading aligns with their judicial philosophy and often produces desired policy outcomes. Rights claimants lacking historical support and progressive legal scholars are clear targets (d near 1.0), as their arguments are systematically disadvantaged. The public is a payer, bearing the cost of a less adaptable constitutional order, though some segments may benefit from its perceived stability.
 *
 * MANDATROPHY ANALYSIS:
 *   The originalist reading aims to prevent judicial activism, a problem its proponents argue is still live. However, critics contend that the constraint itself has become a tool for a different form of judicial activism, or that its original coordination function (ensuring stability) has been overshadowed by its extractive function (limiting progressive legal development). The 'founding_problem_status: contested' reflects this ongoing debate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_meaning_determinacy,
    'Is the ''original public meaning'' of the Constitution truly ascertainable and determinate, or is it inherently ambiguous and subject to contemporary interpretive biases?',
    'Extensive empirical studies of historical linguistic usage and framers'' intent, coupled with meta-analysis of interpretive disagreements among originalist scholars themselves. If consistent, unambiguous meaning is rarely found, the claim of determinacy is weakened.',
    'If the original public meaning is found to be largely indeterminate, the constraint''s justification as an objective interpretive method weakens, increasing its perceived theater_ratio and potentially reclassifying it towards a snare (if the indeterminacy is exploited for partisan outcomes) or a piton (if it persists purely by institutional inertia despite its lack of functional determinacy).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_meaning_determinacy, empirical, 'The determinacy of original public meaning as an interpretive anchor.').

omega_variable(
    originalism_as_judicial_activism,
    'Does the originalist reading genuinely constrain judicial discretion, or does it merely channel it into historical research and selective application, potentially leading to its own form of judicial activism?',
    'Comparative analysis of judicial outcomes under originalist vs. non-originalist methodologies across a range of cases, assessing whether originalism consistently leads to more restrained or predictable results, or if it enables judges to reach preferred outcomes by manipulating historical evidence.',
    'If originalism is found to be a vehicle for a different form of activism, its claimed coordination function (limiting discretion) would be undermined, increasing its perceived extractiveness and suppression, and potentially reclassifying it towards a snare (if the activism benefits specific political factions) or a tangled_rope (if it still provides some coordination but with significant asymmetric extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originalism_as_judicial_activism, conceptual, 'Whether originalism truly constrains or merely redirects judicial discretion.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of non-originalist arguments structural (due to formal legal precedent and judicial hierarchy) or internalized (due to professional norms and self-censorship among legal scholars and practitioners)?',
    'Post-exit suppression trajectory: if non-originalist arguments persist and gain traction in legal discourse even after a shift in judicial composition, it suggests a stronger internalized component. If they immediately re-emerge and are adopted by courts, the suppression was primarily structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — legal actors carry the suppression with them after exit, making the constraint more resilient. If primarily structural, a change in judicial composition could more easily dismantle the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for non-originalist arguments.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_meaning__originalist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_constitution_meaning__originalist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(us_c_tr_t10, us_constitution_meaning__originalist_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(us_c_tr_t20, us_constitution_meaning__originalist_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(us_c_tr_t30, us_constitution_meaning__originalist_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement(us_c_tr_t40, us_constitution_meaning__originalist_reading, theater_ratio, 40, 0.2).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_constitution_meaning__originalist_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(us_c_be_t10, us_constitution_meaning__originalist_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(us_c_be_t20, us_constitution_meaning__originalist_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(us_c_be_t30, us_constitution_meaning__originalist_reading, base_extractiveness, 30, 0.67).
narrative_ontology:measurement(us_c_be_t40, us_constitution_meaning__originalist_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t0, us_constitution_meaning__originalist_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(us_c_su_t10, us_constitution_meaning__originalist_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(us_c_su_t20, us_constitution_meaning__originalist_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(us_c_su_t30, us_constitution_meaning__originalist_reading, suppression_requirement, 30, 0.73).
narrative_ontology:measurement(us_c_su_t40, us_constitution_meaning__originalist_reading, suppression_requirement, 40, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_meaning__originalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_meaning__originalist_reading, us_constitution_meaning__living_constitutionalist_reading).
narrative_ontology:affects_constraint(us_constitution_meaning__originalist_reading, us_constitution_meaning__positivist_reading).
narrative_ontology:affects_constraint(us_constitution_meaning__originalist_reading, judicial_review_legitimacy).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'us_constitution_meaning' kernel. It is linked to sibling readings (living constitutionalist, positivist) which represent alternative interpretive frameworks for the same constitutional text. Each reading is a distinct constraint with its own structural properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
