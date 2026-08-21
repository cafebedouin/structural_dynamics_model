% ============================================================================
% CONSTRAINT STORY: unclos_maritime_sovereignty__hybrid_effective_control_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_maritime_sovereignty__hybrid_effective_control_reading, []).

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
 *   constraint_id: unclos_maritime_sovereignty__hybrid_effective_control_reading
 *   human_readable: UNCLOS Maritime Sovereignty: Hybrid Effective Control Reading
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   This constraint represents a 'hybrid effective control' reading of UNCLOS
 *   maritime sovereignty, where natural features grant full maritime zones,
 *   but artificial features can mature into territorial claims through
 *   prolonged, unchallenged effective control. This reading sits between a
 *   strict geographic interpretation and an expansive constructionist view.
 *   It is a contested interpretation that allows powerful states to expand
 *   their maritime claims, leading to an intermediate level of extraction and
 *   suppression, primarily borne by weaker claimants. The claimed type is
 *   'tangled_rope' because it offers a coordination function (graduated
 *   claims) but also enables asymmetric extraction through the leverage of
 *   construction and enforcement capacity.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.65).
domain_priors:suppression_score(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.7).
domain_priors:theater_ratio(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_maritime_sovereignty__hybrid_effective_control_reading, tangled_rope).
narrative_ontology:human_readable(unclos_maritime_sovereignty__hybrid_effective_control_reading, "UNCLOS Maritime Sovereignty: Hybrid Effective Control Reading").
narrative_ontology:topic_domain(unclos_maritime_sovereignty__hybrid_effective_control_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_maritime_sovereignty__hybrid_effective_control_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_maritime_sovereignty__hybrid_effective_control_reading, 'e153c9be-4daa-4268-8d23-b8f9ed436de0').
narrative_ontology:cs_kernel_codification('e153c9be-4daa-4268-8d23-b8f9ed436de0', formalized).
narrative_ontology:cs_authority_grounding('e153c9be-4daa-4268-8d23-b8f9ed436de0', lineage).
narrative_ontology:cs_interpretation_layer_present('e153c9be-4daa-4268-8d23-b8f9ed436de0').
narrative_ontology:cs_reading_relation('e153c9be-4daa-4268-8d23-b8f9ed436de0', unclos_maritime_sovereignty__strict_geographic_reading, coexists_with).
narrative_ontology:cs_reading_relation('e153c9be-4daa-4268-8d23-b8f9ed436de0', unclos_maritime_sovereignty__expansive_construction_reading, coexists_with).
narrative_ontology:cs_axiom('e153c9be-4daa-4268-8d23-b8f9ed436de0', foundational, graduated_sovereignty_by_feature_type).
narrative_ontology:cs_axiom_status(graduated_sovereignty_by_feature_type, holdable).
narrative_ontology:cs_axiom_grounding('e153c9be-4daa-4268-8d23-b8f9ed436de0', graduated_sovereignty_by_feature_type, conventional).
narrative_ontology:cs_axiom('e153c9be-4daa-4268-8d23-b8f9ed436de0', foundational, effective_control_as_legitimizing_factor).
narrative_ontology:cs_axiom_status(effective_control_as_legitimizing_factor, holdable).
narrative_ontology:cs_axiom_grounding('e153c9be-4daa-4268-8d23-b8f9ed436de0', effective_control_as_legitimizing_factor, conventional).
narrative_ontology:cs_reference_frame('e153c9be-4daa-4268-8d23-b8f9ed436de0', unclos_original_intent_with_evolving_practice).
narrative_ontology:cs_drift_state('e153c9be-4daa-4268-8d23-b8f9ed436de0', contemporary_maritime_expansion_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('e153c9be-4daa-4268-8d23-b8f9ed436de0', '').
narrative_ontology:cs_kernel_id(unclos_maritime_sovereignty__hybrid_effective_control_reading, unclos_maritime_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__hybrid_effective_control_reading, states_with_construction_capacity).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__hybrid_effective_control_reading, regional_powers).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__hybrid_effective_control_reading, militarily_weaker_claimants).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__hybrid_effective_control_reading, states_without_construction_capacity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These states benefit from the ambiguity, allowing them to gradually assert claims over artificial features through sustained presence and development, potentially expanding their maritime zones without direct military confrontation.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, states_with_construction_capacity, beneficiary,
    powerful, generational, mobile, regional).

% These states actively shape the interpretation of UNCLOS through their actions and diplomatic pressure, leveraging their naval and construction capabilities to establish 'effective control' over contested areas, thereby expanding their influence.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, regional_powers, agenda_setter,
    institutional, generational, arbitrage, regional).

% These states bear the cost of expanded claims by more powerful neighbors, losing potential access to resources and navigation rights. Their limited military and diplomatic capacity makes challenging established 'effective control' difficult and costly.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, militarily_weaker_claimants, payer,
    powerless, generational, trapped, regional).

% These states are disadvantaged by a reading that rewards artificial construction and prolonged control, as they lack the resources to build and maintain such features, further entrenching existing power imbalances.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, states_without_construction_capacity, payer,
    moderate, biographical, constrained, regional).

% These bodies interpret and apply UNCLOS, but their rulings often face challenges in enforcement, especially when powerful states reject their jurisdiction or findings. They observe the evolving practice of states.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, international_tribunals, observer,
    institutional, civilizational, analytical, global).

% All states that have ratified UNCLOS are theoretically bound by its provisions. However, the ambiguity in interpretation allows powerful states to push for readings that benefit them, effectively excluding the collective voice of signatories who might prefer a stricter interpretation.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, unclos_signatories, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for states to assert and recognize maritime claims, reducing the potential for direct conflict by offering a graduated system of sovereignty based on feature type and the duration of effective control.
% TRANSFER_FUNCTION: Transfers potential maritime zones (territorial sea, EEZ) and associated resource rights from the global commons or weaker claimants to states capable of establishing and maintaining prolonged effective control over artificial features.
% ABSENT_VOICES: Militarily weaker claimants and states without significant construction capacity are often marginalized in the practical application of this reading, as their objections to 'effective control' are difficult to enforce. Their voices would advocate for a stricter adherence to natural geographic features.
% DISAPPEARANCE_RATIONALE: If this hybrid reading vanished, the legal status of numerous artificial features and their associated claims would become entirely ambiguous, leading to increased disputes, potential military confrontations, and a scramble to establish new de facto control, fundamentally altering maritime governance.
% FOUNDING_PROBLEM: The original UNCLOS framework did not fully anticipate the scale and strategic importance of artificial island construction and the subsequent claims, leading to a gap in clear legal guidance for these novel situations.
% FOUNDING_PROBLEM_CORROBORATION: International legal scholars and geopolitical analysts, independent of claimant states, corroborate that the ambiguity regarding artificial features and effective control remains a live and contested issue, driving ongoing maritime disputes and strategic competition.
narrative_ontology:disappearance_verdict(unclos_maritime_sovereignty__hybrid_effective_control_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_maritime_sovereignty__hybrid_effective_control_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_maritime_sovereignty__hybrid_effective_control_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(unclos_maritime_sovereignty__hybrid_effective_control_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_maritime_sovereignty__hybrid_effective_control_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unclos_maritime_sovereignty__hybrid_effective_control_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unclos_maritime_sovereignty__hybrid_effective_control_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.65) because this reading allows powerful states to gain significant maritime territory and resources that would otherwise be international waters or belong to other claimants. Suppression (0.70) is also high, as the 'effective control' aspect relies on the ability to project power and deter challenges, making it difficult for weaker states to contest. Theater ratio (0.20) is moderate; while there's a genuine legal framework, the 'maturation' of claims through control involves a degree of performative assertion of sovereignty. The temporal measurements show an increase in extractiveness and suppression as states increasingly leverage this interpretation, with a slight recent dip in extractiveness reflecting increased international scrutiny and counter-assertions.
 *
 * PERSPECTIVAL GAP:
 *   Powerful states view this reading as a pragmatic and necessary evolution of international law to address new realities of maritime construction and security. Weaker states, however, perceive it as a mechanism for 'might makes right,' undermining the spirit of UNCLOS and creating an uneven playing field. The engine's classification as a Tangled Rope captures this dual nature, where coordination for some (managing claims) enables extraction from others (losing claims).
 *
 * DIRECTIONALITY LOGIC:
 *   States with construction capacity and regional powers are beneficiaries, as this reading provides a legal-diplomatic pathway to expand their maritime claims. Militarily weaker claimants and states without construction capacity are victims, as they lose out on potential maritime zones and face an uphill battle to challenge established control. International tribunals act as observers, while UNCLOS signatories are often excluded from effectively shaping the interpretation against powerful actors.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definition_of_effective_control,
    'What constitutes ''prolonged effective control absent challenge'' in a legally binding sense, and how is ''challenge'' defined (diplomatic protest vs. military action)?',
    'A definitive ruling by an international tribunal with universal acceptance, or the emergence of a clear, consistent state practice that is universally recognized as customary international law.',
    'A clear definition would reduce ambiguity, potentially lowering extractiveness if it sets a high bar for control or a low bar for challenge, or increasing it if it legitimizes current expansive practices.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(definition_of_effective_control, conceptual, 'Ambiguity in what constitutes ''effective control'' and ''challenge'' for artificial features.').

omega_variable(
    legitimacy_of_artificial_feature_claims,
    'To what extent can artificial features, by their nature, ever generate sovereign rights beyond a safety zone, given the UNCLOS emphasis on natural land features?',
    'A re-negotiation of UNCLOS or a universally accepted amendment specifically addressing the legal status of artificial islands and structures.',
    'If artificial features are deemed inherently incapable of generating full maritime zones, this reading''s extractiveness would collapse, reclassifying it closer to a Rope or even a Mountain (for natural features only). If fully legitimized, it would become a more entrenched Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_artificial_feature_claims, preference, 'The fundamental normative question of whether artificial features should generate sovereign rights.').

omega_variable(
    enforcement_capacity_asymmetry,
    'How much of the ''effective control'' is genuinely about legal precedent and how much is simply a function of military and economic power asymmetry?',
    'Empirical studies correlating successful claims with military and economic power, controlling for other factors, and analysis of cases where weaker states successfully challenge stronger ones.',
    'If power asymmetry is the dominant factor, the constraint''s suppression and extractiveness are higher than currently measured, and its coordination function is more theatrical, pushing it closer to a Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_capacity_asymmetry, empirical, 'The role of power asymmetry in establishing and maintaining ''effective control''.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_maritime_sovereignty__hybrid_effective_control_reading, 1982, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t1982, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 1982, 0.1).
narrative_ontology:measurement(uncl_tr_t1995, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 1995, 0.15).
narrative_ontology:measurement(uncl_tr_t2005, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 2005, 0.2).
narrative_ontology:measurement(uncl_tr_t2015, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 2015, 0.25).
narrative_ontology:measurement(uncl_tr_t2024, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(uncl_be_t1982, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 1982, 0.4).
narrative_ontology:measurement(uncl_be_t1995, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 1995, 0.5).
narrative_ontology:measurement(uncl_be_t2005, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 2005, 0.6).
narrative_ontology:measurement(uncl_be_t2015, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 2015, 0.68).
narrative_ontology:measurement(uncl_be_t2024, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t1982, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 1982, 0.45).
narrative_ontology:measurement(uncl_su_t1995, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 1995, 0.55).
narrative_ontology:measurement(uncl_su_t2005, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 2005, 0.65).
narrative_ontology:measurement(uncl_su_t2015, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 2015, 0.75).
narrative_ontology:measurement(uncl_su_t2024, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_maritime_sovereignty__hybrid_effective_control_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__hybrid_effective_control_reading, unclos_maritime_sovereignty__strict_geographic_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__hybrid_effective_control_reading, unclos_maritime_sovereignty__expansive_construction_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__hybrid_effective_control_reading, freedom_of_navigation_operations).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'unclos_maritime_sovereignty' kernel. This 'hybrid effective control' reading influences and coexists with the 'strict geographic' and 'expansive construction' readings, as state practice under this interpretation directly impacts the perceived legitimacy and contestability of the other two.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
