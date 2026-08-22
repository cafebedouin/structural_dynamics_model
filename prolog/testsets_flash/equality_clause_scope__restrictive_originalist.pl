% ============================================================================
% CONSTRAINT STORY: equality_clause_scope__restrictive_originalist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equality_clause_scope__restrictive_originalist, []).

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
 *   constraint_id: equality_clause_scope__restrictive_originalist
 *   human_readable: Restrictive Originalist Reading of Equality Clause Scope
 *   domain: constitutional_law/political_philosophy/civil_rights_history
 *
 * SUMMARY:
 *   This constraint represents the restrictive originalist reading of the
 *   equality clause's scope, limiting its application to propertied white
 *   males as political actors within an 18th-century social contract
 *   framework. This reading asserts that any expansion of equality to other
 *   groups (women, racial minorities, non-propertied individuals) requires
 *   explicit constitutional amendment, rather than judicial reinterpretation.
 *   The constraint is claimed as a 'rope' by its proponents, framing it as a
 *   faithful adherence to foundational principles, but its operation is
 *   highly extractive and suppressive for those excluded from its narrow
 *   scope, making it compute as a 'tangled_rope' or 'snare' from their seats.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equality_clause_scope__restrictive_originalist, 0.65).
domain_priors:suppression_score(equality_clause_scope__restrictive_originalist, 0.78).
domain_priors:theater_ratio(equality_clause_scope__restrictive_originalist, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, extractiveness, 0.65).
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equality_clause_scope__restrictive_originalist, tangled_rope).
narrative_ontology:human_readable(equality_clause_scope__restrictive_originalist, "Restrictive Originalist Reading of Equality Clause Scope").
narrative_ontology:topic_domain(equality_clause_scope__restrictive_originalist, "constitutional_law/political_philosophy/civil_rights_history").

domain_priors:requires_active_enforcement(equality_clause_scope__restrictive_originalist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equality_clause_scope__restrictive_originalist, '43556e4e-e428-4b25-ac56-b6493543834d').
narrative_ontology:cs_kernel_codification('43556e4e-e428-4b25-ac56-b6493543834d', fixed_text).
narrative_ontology:cs_authority_grounding('43556e4e-e428-4b25-ac56-b6493543834d', lineage).
narrative_ontology:cs_interpretation_layer_present('43556e4e-e428-4b25-ac56-b6493543834d').
narrative_ontology:cs_reading_relation('43556e4e-e428-4b25-ac56-b6493543834d', equality_clause_scope__expansive_universalist, coexists_with).
narrative_ontology:cs_reading_relation('43556e4e-e428-4b25-ac56-b6493543834d', equality_clause_scope__progressive_textualist, coexists_with).
narrative_ontology:cs_axiom('43556e4e-e428-4b25-ac56-b6493543834d', foundational, original_intent_fixed_meaning).
narrative_ontology:cs_axiom_status(original_intent_fixed_meaning, holdable).
narrative_ontology:cs_axiom_grounding('43556e4e-e428-4b25-ac56-b6493543834d', original_intent_fixed_meaning, conventional).
narrative_ontology:cs_axiom('43556e4e-e428-4b25-ac56-b6493543834d', foundational, equality_limited_to_political_actors).
narrative_ontology:cs_axiom_status(equality_limited_to_political_actors, holdable).
narrative_ontology:cs_axiom_grounding('43556e4e-e428-4b25-ac56-b6493543834d', equality_limited_to_political_actors, conventional).
narrative_ontology:cs_reference_frame('43556e4e-e428-4b25-ac56-b6493543834d', id_18th_century_social_contract).
narrative_ontology:cs_drift_state('43556e4e-e428-4b25-ac56-b6493543834d', contemporary_civil_rights_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('43556e4e-e428-4b25-ac56-b6493543834d', '').
narrative_ontology:cs_kernel_id(equality_clause_scope__restrictive_originalist, equality_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equality_clause_scope__restrictive_originalist, propertied_white_males_historical).
narrative_ontology:constraint_beneficiary(equality_clause_scope__restrictive_originalist, contemporary_originalist_jurists).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, women_historical).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, racial_minorities_historical).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, non_propertied_males_historical).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, contemporary_civil_rights_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The original beneficiaries of the equality clause, whose political and economic status was secured by its narrow application. They defined the terms of the social contract.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, propertied_white_males_historical, beneficiary,
    institutional, generational, arbitrage, national).

% Judges and legal scholars who interpret the equality clause strictly according to its perceived 18th-century meaning, limiting its application to the original framers' intent. Their careers and legitimacy are tied to this interpretive method.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, contemporary_originalist_jurists, agenda_setter,
    institutional, generational, identity_locked, national).

% Excluded from the original scope of equality, denied political and often economic rights. Their struggle for inclusion required constitutional amendments and social movements.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, women_historical, payer,
    powerless, generational, trapped, national).

% Systematically denied equality under the law, often enslaved or subjected to discriminatory codes. Their fight for civil rights was a direct challenge to this restrictive interpretation.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, racial_minorities_historical, payer,
    powerless, generational, trapped, national).

% Initially excluded from full political participation due to property requirements, they eventually gained rights through political struggle, but outside the original 'equality' framework.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, non_propertied_males_historical, payer,
    powerless, biographical, constrained, local).

% Organizations and individuals who continuously challenge restrictive interpretations of equality, arguing for universal application. They bear the cost of litigation and political organizing against this constraint.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, contemporary_civil_rights_advocates, payer,
    organized, generational, constrained, national).

% Judges and legal scholars who argue for a broad, universal application of equality principles, often clashing with originalist interpretations. Their arguments are often dismissed as 'judicial activism' by originalists.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, expansive_universalist_jurists, excluded,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, historically grounded framework for interpreting constitutional equality, ensuring judicial consistency with the original intent of the framers.
% TRANSFER_FUNCTION: Transfers political and social power, and the legitimacy of its exercise, to those groups and interpretations that align with the 18th-century understanding of the social contract, away from those seeking broader application.
% ABSENT_VOICES: The voices of women, racial minorities, and non-propertied individuals from the 18th century were absent from the original framing and are systematically excluded from this reading's interpretive authority. Their contemporary advocates are present but often marginalized in the interpretive discourse.
% DISAPPEARANCE_RATIONALE: If this restrictive originalist reading vanished, the legal landscape for civil rights would fundamentally shift. Courts would likely adopt more expansive interpretations of equality, leading to new legal challenges and potentially overturning precedents based on originalist reasoning. The political power of originalist legal movements would diminish significantly.
% FOUNDING_PROBLEM: To establish a stable, limited government based on a written constitution, ensuring that the scope of rights and political participation was clearly defined and not subject to arbitrary expansion.
% FOUNDING_PROBLEM_CORROBORATION: Originalist legal scholars and conservative political movements attest that the problem of judicial overreach and interpretive instability is still live. Historians and legal academics outside the originalist movement corroborate the historical context of limited framers' intent, but dispute its contemporary normative force.
narrative_ontology:disappearance_verdict(equality_clause_scope__restrictive_originalist, world_rearranges).
narrative_ontology:founding_problem_status(equality_clause_scope__restrictive_originalist, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equality_clause_scope__restrictive_originalist, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(equality_clause_scope__restrictive_originalist, 'none', 1).
narrative_ontology:epsilon_provenance(equality_clause_scope__restrictive_originalist, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equality_clause_scope__restrictive_originalist_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(equality_clause_scope__restrictive_originalist, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(equality_clause_scope__restrictive_originalist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.65) because this reading systematically denies rights and political power to large segments of the population, channeling benefits to a narrow group. Suppression is also high (0.78) as it actively resists and invalidates attempts to expand equality through judicial means, requiring significant social and political struggle (amendments, civil rights movements) to overcome. The theater ratio (0.40) reflects the performative aspect of 'original intent' arguments, which often serve to rationalize existing power structures rather than neutrally interpret historical texts. The historical measurements show fluctuations in extractiveness and suppression, often peaking during periods of intense civil rights struggle, as the constraint's enforcement machinery worked to maintain its narrow scope.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of originalist jurists, this constraint is a legitimate 'rope' that ensures constitutional fidelity and stability. From the perspective of civil rights advocates and historically excluded groups, it operates as a 'snare' or 'tangled_rope,' actively extracting rights and suppressing their claims. The engine's computation of per-seat classification will reflect this divergence based on the declared structural relationships and metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   Historical propertied white males and contemporary originalist jurists are beneficiaries/agenda-setters, as the constraint secures their power and interpretive authority. Women, racial minorities, and non-propertied males (both historically and their contemporary advocates) are payers, bearing the costs of exclusion and having to fight for their rights outside this framework. Expansive universalist jurists are excluded, as their interpretive approach is fundamentally at odds with this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    framers_intent_ambiguity,
    'Is the ''original intent'' of the framers regarding equality truly as narrow as this reading asserts, or is there ambiguity that allows for broader interpretation?',
    'Further historical and textual analysis of founding documents, including debates and contemporary philosophical understandings of ''equality'' beyond the immediate political context.',
    'If ambiguity is found, it weakens the ''naturalness'' claim of this reading, potentially reclassifying it closer to a ''snare'' by revealing the interpretive choice as a mechanism of extraction rather than a discovery of fixed meaning.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framers_intent_ambiguity, empirical, 'Ambiguity in the historical ''original intent'' regarding equality.').

omega_variable(
    legitimacy_of_judicial_expansion,
    'Is judicial reinterpretation to expand equality an illegitimate act of ''judicial activism'' (as this reading claims), or a necessary function of a living constitution?',
    'A shift in societal consensus regarding the role of the judiciary in constitutional evolution, or a definitive ruling by a super-majority court that explicitly rejects or affirms the legitimacy of such expansion.',
    'If judicial expansion is deemed legitimate, the ''suppression'' metric of this reading would be seen as an illegitimate barrier, further pushing its classification towards ''snare'' or ''tangled_rope'' from the perspective of those seeking expansion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_of_judicial_expansion, conceptual, 'The conceptual legitimacy of judicial expansion of equality.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equality_clause_scope__restrictive_originalist, 1787, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1787, equality_clause_scope__restrictive_originalist, theater_ratio, 1787, 0.1).
narrative_ontology:measurement(equa_tr_t1865, equality_clause_scope__restrictive_originalist, theater_ratio, 1865, 0.25).
narrative_ontology:measurement(equa_tr_t1920, equality_clause_scope__restrictive_originalist, theater_ratio, 1920, 0.3).
narrative_ontology:measurement(equa_tr_t1965, equality_clause_scope__restrictive_originalist, theater_ratio, 1965, 0.45).
narrative_ontology:measurement(equa_tr_t2000, equality_clause_scope__restrictive_originalist, theater_ratio, 2000, 0.4).
narrative_ontology:measurement(equa_tr_t2024, equality_clause_scope__restrictive_originalist, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(equa_be_t1787, equality_clause_scope__restrictive_originalist, base_extractiveness, 1787, 0.6).
narrative_ontology:measurement(equa_be_t1865, equality_clause_scope__restrictive_originalist, base_extractiveness, 1865, 0.68).
narrative_ontology:measurement(equa_be_t1920, equality_clause_scope__restrictive_originalist, base_extractiveness, 1920, 0.62).
narrative_ontology:measurement(equa_be_t1965, equality_clause_scope__restrictive_originalist, base_extractiveness, 1965, 0.7).
narrative_ontology:measurement(equa_be_t2000, equality_clause_scope__restrictive_originalist, base_extractiveness, 2000, 0.65).
narrative_ontology:measurement(equa_be_t2024, equality_clause_scope__restrictive_originalist, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1787, equality_clause_scope__restrictive_originalist, suppression_requirement, 1787, 0.7).
narrative_ontology:measurement(equa_su_t1865, equality_clause_scope__restrictive_originalist, suppression_requirement, 1865, 0.85).
narrative_ontology:measurement(equa_su_t1920, equality_clause_scope__restrictive_originalist, suppression_requirement, 1920, 0.75).
narrative_ontology:measurement(equa_su_t1965, equality_clause_scope__restrictive_originalist, suppression_requirement, 1965, 0.8).
narrative_ontology:measurement(equa_su_t2000, equality_clause_scope__restrictive_originalist, suppression_requirement, 2000, 0.78).
narrative_ontology:measurement(equa_su_t2024, equality_clause_scope__restrictive_originalist, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equality_clause_scope__restrictive_originalist, enforcement_mechanism).
narrative_ontology:affects_constraint(equality_clause_scope__restrictive_originalist, civil_rights_legislation_legitimacy).
narrative_ontology:affects_constraint(equality_clause_scope__restrictive_originalist, voting_rights_enforcement).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'equality_clause_scope' kernel. Its narrow interpretation influences the legitimacy and enforcement of civil rights legislation and voting rights.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
