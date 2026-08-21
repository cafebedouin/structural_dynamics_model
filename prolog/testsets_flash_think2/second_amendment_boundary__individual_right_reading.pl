% ============================================================================
% CONSTRAINT STORY: second_amendment_boundary__individual_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_boundary__individual_right_reading, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: second_amendment_boundary__individual_right_reading
 *   human_readable: Second Amendment: Individual Right Interpretation
 *   domain: constitutional_law/firearms_policy/political_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the 'individual right' reading of the
 *   Second Amendment, which posits that the operative clause establishes a
 *   pre-existing individual right to bear arms, and the prefatory militia
 *   clause states a purpose but does not limit the right's scope. This
 *   interpretation has been increasingly adopted by federal courts,
 *   significantly impacting firearms policy. The constraint is CLAIMED as a
 *   Mountain by its proponents, asserting it as a fixed, fundamental aspect
 *   of constitutional law. However, the authored metrics reflect its highly
 *   extractive and suppressive operation against state regulatory efforts and
 *   public safety, leading to a False Summit Mountain (FSM) classification by
 *   the engine.
 *
 * KEY AGENTS:
 *   - Gun Owners: Primary beneficiaries, whose right to bear arms is protected.
 *   - Firearms Industry: Major beneficiaries, operating in a constitutionally shielded market.
 *   - Public Safety Advocates: Primary targets, whose efforts to regulate firearms are suppressed.
 *   - Victims of Gun Violence: Direct targets, bearing the costs of unrestricted access.
 *   - State Legislatures: Agenda-setters whose regulatory power is constrained.
 *   - Federal Courts: Agenda-setters who interpret and enforce this reading.
 *   - Militia Members: Excluded from the core focus of this individual right interpretation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_boundary__individual_right_reading, 0.8).
domain_priors:suppression_score(second_amendment_boundary__individual_right_reading, 0.9).
domain_priors:theater_ratio(second_amendment_boundary__individual_right_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_boundary__individual_right_reading, mountain).
narrative_ontology:human_readable(second_amendment_boundary__individual_right_reading, "Second Amendment: Individual Right Interpretation").
narrative_ontology:topic_domain(second_amendment_boundary__individual_right_reading, "constitutional_law/firearms_policy/political_theory").

domain_priors:requires_active_enforcement(second_amendment_boundary__individual_right_reading).
domain_priors:emerges_naturally(second_amendment_boundary__individual_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_boundary__individual_right_reading, '8f20f8fd-d048-41fa-a5b6-114a5ec5d143').
narrative_ontology:cs_kernel_codification('8f20f8fd-d048-41fa-a5b6-114a5ec5d143', fixed_text).
narrative_ontology:cs_authority_grounding('8f20f8fd-d048-41fa-a5b6-114a5ec5d143', lineage).
narrative_ontology:cs_interpretation_layer_present('8f20f8fd-d048-41fa-a5b6-114a5ec5d143').
narrative_ontology:cs_reading_relation('8f20f8fd-d048-41fa-a5b6-114a5ec5d143', second_amendment_boundary__militia_conditioned_reading, forecloses).
narrative_ontology:cs_reading_relation('8f20f8fd-d048-41fa-a5b6-114a5ec5d143', second_amendment_boundary__insurrectionist_reading, coexists_with).
narrative_ontology:cs_axiom('8f20f8fd-d048-41fa-a5b6-114a5ec5d143', foundational, individual_right_preexists_constitution).
narrative_ontology:cs_axiom_status(individual_right_preexists_constitution, holdable).
narrative_ontology:cs_axiom_grounding('8f20f8fd-d048-41fa-a5b6-114a5ec5d143', individual_right_preexists_constitution, deontological).
narrative_ontology:cs_axiom('8f20f8fd-d048-41fa-a5b6-114a5ec5d143', foundational, militia_clause_prefatory_only).
narrative_ontology:cs_axiom_status(militia_clause_prefatory_only, holdable).
narrative_ontology:cs_axiom_grounding('8f20f8fd-d048-41fa-a5b6-114a5ec5d143', militia_clause_prefatory_only, conventional).
narrative_ontology:cs_reference_frame('8f20f8fd-d048-41fa-a5b6-114a5ec5d143', founding_era_individual_liberty).
narrative_ontology:cs_drift_state('8f20f8fd-d048-41fa-a5b6-114a5ec5d143', contemporary_judicial_expansion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8f20f8fd-d048-41fa-a5b6-114a5ec5d143', '').
narrative_ontology:cs_kernel_id(second_amendment_boundary__individual_right_reading, second_amendment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_boundary__individual_right_reading, gun_owners).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__individual_right_reading, firearms_industry).
narrative_ontology:constraint_victim(second_amendment_boundary__individual_right_reading, public_safety_advocates).
narrative_ontology:constraint_victim(second_amendment_boundary__individual_right_reading, victims_of_gun_violence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals who own firearms for self-defense, sport, or collection. This interpretation protects their right to possess a wide range of firearms without significant state interference, treating it as a fundamental liberty.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, gun_owners, beneficiary,
    organized, biographical, mobile, national).

% Manufacturers, distributors, and retailers of firearms and ammunition. This interpretation creates a constitutionally protected market for their products, shielding it from many forms of state regulation and ensuring demand.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, firearms_industry, beneficiary,
    institutional, generational, arbitrage, national).

% Organizations and individuals advocating for stricter gun control measures to reduce gun violence. This interpretation imposes significant legal barriers to their policy goals, forcing them into protracted legislative and judicial battles.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, public_safety_advocates, payer,
    organized, generational, constrained, national).

% Individuals and communities directly affected by gun violence. This interpretation is seen as contributing to the prevalence of firearms and the difficulty of implementing preventative measures, leaving them vulnerable.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, victims_of_gun_violence, payer,
    powerless, immediate, trapped, local).

% Government bodies responsible for enacting laws. This interpretation severely limits their ability to pass comprehensive firearms regulations, often leading to their laws being challenged and overturned by federal courts.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, state_legislatures, agenda_setter,
    institutional, biographical, constrained, national).

% The judiciary, particularly the Supreme Court, which interprets the Second Amendment and enforces this reading through rulings that strike down state and federal gun laws. They are the primary arbiters of the right's scope.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, federal_courts, agenda_setter,
    institutional, civilizational, analytical, national).

% Individuals who participate in organized, state-sanctioned or informal militias. While the Second Amendment mentions militias, this reading largely detaches the individual right from the militia context, making their role secondary to the core interpretation.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, militia_members, excluded,
    moderate, biographical, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, albeit broad, legal framework for individual firearm ownership, reducing ambiguity for gun owners and manufacturers regarding the scope of their rights and permissible commerce.
% TRANSFER_FUNCTION: Transfers significant regulatory power over firearms from state and federal legislatures to individual citizens and the federal judiciary; it also transfers the societal costs of widespread firearm access (e.g., gun violence) from gun owners to the general public.
% ABSENT_VOICES: The collective security concerns of communities, particularly those disproportionately affected by gun violence, are often marginalized in the legal discourse dominated by individual rights. Voices emphasizing the historical context of the militia clause as a collective, not individual, right are also structurally sidelined by this interpretation.
% DISAPPEARANCE_RATIONALE: If this individual right interpretation vanished overnight, state and federal governments would swiftly enact far-reaching gun control legislation, fundamentally altering the firearms market, public safety policies, and the legal landscape of gun ownership in the United States.
% FOUNDING_PROBLEM: The Second Amendment was adopted to ensure the capacity for self-defense and to maintain a citizenry capable of forming a militia, reflecting post-revolutionary concerns about federal overreach and the need for a free state's security.
% FOUNDING_PROBLEM_CORROBORATION: Gun rights advocates and some legal scholars attest that the founding problem of individual liberty and self-defense remains live. Public safety advocates and other historians argue that the original militia context is largely obsolete, and the problem has transformed into one of managing widespread gun violence. Independent historical and legal analyses from outside the immediate advocacy groups offer diverse perspectives on the amendment's original intent and contemporary relevance.
narrative_ontology:disappearance_verdict(second_amendment_boundary__individual_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_boundary__individual_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_boundary__individual_right_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(second_amendment_boundary__individual_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_boundary__individual_right_reading, 0.8, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_boundary__individual_right_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_boundary__individual_right_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, ExtMetricName, E),
    domain_priors:suppression_score(second_amendment_boundary__individual_right_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(second_amendment_boundary__individual_right_reading),
    narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(second_amendment_boundary__individual_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.80) reflects the significant transfer of regulatory power and societal costs from gun owners/industry to the public. Suppression (0.90) is severe because this interpretation actively nullifies state and federal attempts at comprehensive gun control, requiring constant judicial enforcement. The theater ratio (0.15) is low because the legal battles and judicial rulings are genuinely functional in shaping policy, not merely performative. Accessibility collapse (0.85) is high for states seeking to regulate, as their alternatives are severely curtailed. Resistance (0.70) is high from public safety groups who continuously challenge this interpretation. The claimed type 'mountain' is used because this reading is presented by its proponents as an unchangeable, fundamental constitutional truth, despite its clear beneficiaries and victims, triggering the FSM mechanism.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of gun owners and the firearms industry, this interpretation is a fundamental protection of liberty, a true 'mountain' that secures their rights. From the perspective of public safety advocates and victims of gun violence, it functions as a 'snare' or 'tangled rope,' actively extracting public safety for private benefit and suppressing democratic efforts to address gun violence. The engine's computation of per-seat classifications will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Gun owners and the firearms industry are clear beneficiaries, experiencing low directionality (d near 0.0) as the constraint subsidizes their activities and protects their market. Public safety advocates and victims of gun violence are targets, experiencing high directionality (d near 1.0) as the constraint extracts from their well-being and suppresses their policy goals. State legislatures are constrained agenda-setters, their power to regulate being targeted. Federal courts, as interpreters and enforcers, act as agenda-setters for this specific reading.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling a judicially constructed and enforced constraint as a natural, unchangeable 'mountain.' By claiming 'mountain' but showing high extractiveness and suppression, the FSM mechanism detects that this 'natural law' claim serves identifiable beneficiaries at the expense of identifiable victims, indicating a potential false summit. The founding problem's contested status further supports this, as the original mandate (militia for a free state) is arguably distinct from the modern individual right interpretation's effects.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_judicial_construct,
    'Is the individual right to bear arms, as interpreted by this reading, a pre-existing natural right or a judicial construct that has evolved through legal precedent?',
    'Historical and legal scholarship analyzing the intent of the framers, the evolution of common law, and the philosophical underpinnings of rights in the founding era, alongside contemporary judicial philosophy.',
    'If primarily a judicial construct, the ''mountain'' claim is weakened, supporting a reclassification towards a ''tangled_rope'' or ''snare'' due to its active enforcement and identifiable beneficiaries/victims. If genuinely pre-existing, the ''mountain'' claim is strengthened, though its extractive effects would still be measured.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_judicial_construct, conceptual, 'Ambiguity regarding the ontological status of the individual right.').

omega_variable(
    scope_of_protected_arms,
    'What types of firearms are protected by this individual right, and what criteria should be used to distinguish protected from unprotected arms?',
    'Further judicial rulings clarifying the ''common use'' test, historical analysis of arms available at the founding, and empirical data on the lethality and commonality of various weapon types.',
    'A narrow interpretation of protected arms would reduce the constraint''s extractiveness and suppression by allowing more regulation. A broad interpretation would amplify these effects.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_protected_arms, empirical, 'Uncertainty regarding the specific objects covered by the right.').

omega_variable(
    balancing_test_applicability,
    'To what extent can public safety concerns be balanced against the individual right to bear arms, and what is the appropriate standard of judicial review for gun control laws?',
    'Future Supreme Court decisions establishing a clear standard of review (e.g., strict scrutiny, intermediate scrutiny, or a historical-only test) and clarifying the role of empirical evidence in assessing gun laws.',
    'A robust balancing test would allow for greater state regulatory capacity, reducing the constraint''s suppressive effect. A purely historical test, as favored by some, would further entrench the constraint''s extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balancing_test_applicability, preference, 'Ambiguity in balancing individual rights against collective welfare.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_boundary__individual_right_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1970, second_amendment_boundary__individual_right_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(seco_tr_t1980, second_amendment_boundary__individual_right_reading, theater_ratio, 1980, 0.12).
narrative_ontology:measurement(seco_tr_t1990, second_amendment_boundary__individual_right_reading, theater_ratio, 1990, 0.13).
narrative_ontology:measurement(seco_tr_t2000, second_amendment_boundary__individual_right_reading, theater_ratio, 2000, 0.14).
narrative_ontology:measurement(seco_tr_t2010, second_amendment_boundary__individual_right_reading, theater_ratio, 2010, 0.15).
narrative_ontology:measurement(seco_tr_t2024, second_amendment_boundary__individual_right_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(seco_be_t1970, second_amendment_boundary__individual_right_reading, base_extractiveness, 1970, 0.6).
narrative_ontology:measurement(seco_be_t1980, second_amendment_boundary__individual_right_reading, base_extractiveness, 1980, 0.65).
narrative_ontology:measurement(seco_be_t1990, second_amendment_boundary__individual_right_reading, base_extractiveness, 1990, 0.7).
narrative_ontology:measurement(seco_be_t2000, second_amendment_boundary__individual_right_reading, base_extractiveness, 2000, 0.75).
narrative_ontology:measurement(seco_be_t2010, second_amendment_boundary__individual_right_reading, base_extractiveness, 2010, 0.78).
narrative_ontology:measurement(seco_be_t2024, second_amendment_boundary__individual_right_reading, base_extractiveness, 2024, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1970, second_amendment_boundary__individual_right_reading, suppression_requirement, 1970, 0.7).
narrative_ontology:measurement(seco_su_t1980, second_amendment_boundary__individual_right_reading, suppression_requirement, 1980, 0.75).
narrative_ontology:measurement(seco_su_t1990, second_amendment_boundary__individual_right_reading, suppression_requirement, 1990, 0.8).
narrative_ontology:measurement(seco_su_t2000, second_amendment_boundary__individual_right_reading, suppression_requirement, 2000, 0.85).
narrative_ontology:measurement(seco_su_t2010, second_amendment_boundary__individual_right_reading, suppression_requirement, 2010, 0.88).
narrative_ontology:measurement(seco_su_t2024, second_amendment_boundary__individual_right_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_boundary__individual_right_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_boundary__individual_right_reading, firearms_market_regulation).
narrative_ontology:affects_constraint(second_amendment_boundary__individual_right_reading, public_safety_policy).
narrative_ontology:affects_constraint(second_amendment_boundary__individual_right_reading, militia_conditioned_reading).
narrative_ontology:affects_constraint(second_amendment_boundary__individual_right_reading, insurrectionist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'second_amendment_boundary' kernel. This 'individual_right_reading' focuses on private possession, distinct from the 'militia_conditioned_reading' (collective right) and the 'insurrectionist_reading' (right to resist tyranny). Each reading generates a structurally distinct constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
