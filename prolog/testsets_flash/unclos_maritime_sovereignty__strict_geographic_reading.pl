% ============================================================================
% CONSTRAINT STORY: unclos_maritime_sovereignty__strict_geographic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_maritime_sovereignty__strict_geographic_reading, []).

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
 *   constraint_id: unclos_maritime_sovereignty__strict_geographic_reading
 *   human_readable: UNCLOS Strict Geographic Definition of Island Sovereignty
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   This constraint represents the strict geographic reading of UNCLOS
 *   Article 121(3), which dictates that only naturally formed features above
 *   water at high tide qualify as islands generating territorial sea and EEZ.
 *   Artificial construction does not alter legal status. This reading is
 *   foundational to maintaining freedom of navigation and limiting unilateral
 *   expansion of maritime claims, particularly in contested regions. It is a
 *   'mountain' in the sense that it is presented as an unchangeable principle
 *   of international law, though its application is actively contested by
 *   states seeking to expand their maritime zones through artificial means.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_maritime_sovereignty__strict_geographic_reading, 0.15).
domain_priors:suppression_score(unclos_maritime_sovereignty__strict_geographic_reading, 0.6).
domain_priors:theater_ratio(unclos_maritime_sovereignty__strict_geographic_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_maritime_sovereignty__strict_geographic_reading, mountain).
narrative_ontology:human_readable(unclos_maritime_sovereignty__strict_geographic_reading, "UNCLOS Strict Geographic Definition of Island Sovereignty").
narrative_ontology:topic_domain(unclos_maritime_sovereignty__strict_geographic_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_maritime_sovereignty__strict_geographic_reading).
domain_priors:emerges_naturally(unclos_maritime_sovereignty__strict_geographic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_maritime_sovereignty__strict_geographic_reading, '67837295-ad08-4788-ba30-625282ee0337').
narrative_ontology:cs_kernel_codification('67837295-ad08-4788-ba30-625282ee0337', fixed_text).
narrative_ontology:cs_authority_grounding('67837295-ad08-4788-ba30-625282ee0337', lineage).
narrative_ontology:cs_interpretation_layer_present('67837295-ad08-4788-ba30-625282ee0337').
narrative_ontology:cs_reading_relation('67837295-ad08-4788-ba30-625282ee0337', unclos_maritime_sovereignty__expansive_construction_reading, coexists_with).
narrative_ontology:cs_reading_relation('67837295-ad08-4788-ba30-625282ee0337', unclos_maritime_sovereignty__hybrid_effective_control_reading, coexists_with).
narrative_ontology:cs_axiom('67837295-ad08-4788-ba30-625282ee0337', foundational, natural_formation_is_sole_basis_for_sovereignty).
narrative_ontology:cs_axiom_status(natural_formation_is_sole_basis_for_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('67837295-ad08-4788-ba30-625282ee0337', natural_formation_is_sole_basis_for_sovereignty, conventional).
narrative_ontology:cs_axiom('67837295-ad08-4788-ba30-625282ee0337', secondary, artificial_structures_are_installations_not_islands).
narrative_ontology:cs_axiom_status(artificial_structures_are_installations_not_islands, holdable).
narrative_ontology:cs_axiom_grounding('67837295-ad08-4788-ba30-625282ee0337', artificial_structures_are_installations_not_islands, conventional).
narrative_ontology:cs_reference_frame('67837295-ad08-4788-ba30-625282ee0337', unclos_original_intent_1982).
narrative_ontology:cs_drift_state('67837295-ad08-4788-ba30-625282ee0337', contemporary_geopolitical_contest, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('67837295-ad08-4788-ba30-625282ee0337', '').
narrative_ontology:cs_kernel_id(unclos_maritime_sovereignty__strict_geographic_reading, unclos_maritime_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__strict_geographic_reading, naval_powers).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__strict_geographic_reading, non_claimant_states).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__strict_geographic_reading, international_shipping).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__strict_geographic_reading, expansionist_coastal_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from clear, limited territorial claims, ensuring freedom of navigation in international waters and minimizing potential for conflict over artificially extended zones. They uphold this reading to maintain global maritime access.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, naval_powers, beneficiary,
    institutional, generational, arbitrage, global).

% Benefit from the stability and predictability of maritime boundaries, avoiding arbitrary expansion of territorial claims by other states. They support this reading to protect their own economic and security interests.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, non_claimant_states, beneficiary,
    organized, generational, mobile, global).

% Relies on clearly defined international waters for efficient transit. This reading prevents the proliferation of new territorial seas and EEZs that could impede navigation and increase costs.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, international_shipping, beneficiary,
    organized, biographical, mobile, global).

% Bear the cost of this strict interpretation as it prevents them from extending their maritime claims through artificial island construction. They seek alternative readings to justify their geopolitical ambitions and resource control.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, expansionist_coastal_states, payer,
    powerful, generational, constrained, regional).

% Interpret and apply UNCLOS provisions, including the definition of an island. Their rulings reinforce or challenge specific readings, shaping the practical application of maritime law.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, international_tribunals, agenda_setter,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, universally recognized definition for what constitutes an island capable of generating maritime zones, thereby coordinating expectations and reducing disputes over sovereignty and resource rights.
% TRANSFER_FUNCTION: Limits the transfer of potential resource rights and sovereign control over vast maritime areas from the global commons to individual coastal states, by restricting the criteria for generating such claims.
% ABSENT_VOICES: States with significant artificial island construction programs, particularly those in contested regions, are present but actively seek to undermine or reinterpret this strict reading. Their 'voice' is one of active contestation rather than absence.
% DISAPPEARANCE_RATIONALE: If this strict definition vanished, there would be immediate and widespread claims of territorial seas and EEZs around artificial structures, leading to significant international disputes, conflicts over resource exploitation, and disruption of freedom of navigation. The entire framework of maritime law would need to be renegotiated.
% FOUNDING_PROBLEM: Before UNCLOS, there was ambiguity and conflicting claims regarding maritime boundaries, leading to disputes over resource exploitation and navigation rights. A clear, universally accepted definition of an island was needed to stabilize international relations.
% FOUNDING_PROBLEM_CORROBORATION: International legal scholars, UN bodies, and the majority of states (especially non-claimant and naval powers) corroborate that the problem of maritime boundary disputes remains live, and the strict definition is crucial for maintaining order. Expansionist states contest its continued relevance in light of technological advancements.
narrative_ontology:disappearance_verdict(unclos_maritime_sovereignty__strict_geographic_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_maritime_sovereignty__strict_geographic_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_maritime_sovereignty__strict_geographic_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(unclos_maritime_sovereignty__strict_geographic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_maritime_sovereignty__strict_geographic_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_maritime_sovereignty__strict_geographic_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, ExtMetricName, E),
    domain_priors:suppression_score(unclos_maritime_sovereignty__strict_geographic_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(unclos_maritime_sovereignty__strict_geographic_reading),
    narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(unclos_maritime_sovereignty__strict_geographic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low (0.15) because this reading primarily prevents extraction by limiting claims, rather than actively extracting. Suppression is moderate (0.6) as it requires ongoing diplomatic and legal pressure to prevent states from asserting claims based on artificial features. Theater ratio is low (0.1) because the principle is genuinely applied, though some states engage in performative construction. Accessibility collapse is high (0.8) as the legal alternatives for generating maritime zones are severely limited by this definition. Resistance is moderate (0.3) from states that wish to expand their claims.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of naval powers and non-claimant states, this is a clear, stable rule essential for global order. From the perspective of expansionist coastal states, it is an unfair restriction on their sovereign rights and development potential, designed to maintain the status quo power balance. The engine will compute different classifications for these seats based on their declared structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Naval powers, non-claimant states, and international shipping are beneficiaries as this reading preserves their access to international waters and prevents arbitrary expansion of claims. Expansionist coastal states are victims as their ability to extend sovereignty through artificial means is curtailed. International tribunals act as agenda-setters, interpreting and enforcing this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_vs_artificial_ambiguity,
    'Is the distinction between ''naturally formed'' and ''artificial construction'' sufficiently clear and robust to prevent future legal challenges, especially with advancing engineering capabilities?',
    'International legal precedent from future cases involving novel construction techniques, or a new UNCLOS amendment clarifying the definition.',
    'If the distinction becomes blurred, the constraint''s suppressive force could weaken, leading to increased extractiveness as states exploit ambiguities. This could shift the classification towards a Tangled Rope or Snare for the international community.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_vs_artificial_ambiguity, conceptual, 'Ambiguity in the core definitional criterion of the constraint.').

omega_variable(
    enforcement_capacity_drift,
    'Will the international community''s capacity and willingness to enforce this strict reading against powerful, expansionist states remain sufficient over time?',
    'Observation of future international tribunal rulings, diplomatic responses to new claims, and naval freedom of navigation operations.',
    'If enforcement capacity erodes, the constraint''s effective suppression will decrease, allowing expansionist states to de facto establish claims, shifting the constraint towards a Snare for the international community and a Rope for the expansionist states.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_capacity_drift, empirical, 'The persistence of the constraint depends on active enforcement against powerful actors.').

omega_variable(
    false_summit_naturalness,
    'Is this constraint a genuine natural law (a ''mountain'' of geography and international consensus), or a constructed constraint that benefits identifiable agents (naval powers, non-claimant states) by limiting others'' claims?',
    'Analysis of historical negotiations and state practice: if the ''naturalness'' was primarily a rhetorical device to secure specific geopolitical outcomes, it leans towards constructed.',
    'If primarily constructed, the ''mountain'' claim is a false summit, and the constraint would reclassify to a Tangled Rope or Snare, reflecting the underlying power dynamics and extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_naturalness, conceptual, 'Ambiguity between natural law and constructed benefit for specific actors.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_maritime_sovereignty__strict_geographic_reading, 1982, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t1982, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 1982, 0.05).
narrative_ontology:measurement(uncl_tr_t1995, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 1995, 0.07).
narrative_ontology:measurement(uncl_tr_t2008, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 2008, 0.09).
narrative_ontology:measurement(uncl_tr_t2024, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(uncl_be_t1982, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 1982, 0.1).
narrative_ontology:measurement(uncl_be_t1995, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 1995, 0.12).
narrative_ontology:measurement(uncl_be_t2008, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 2008, 0.14).
narrative_ontology:measurement(uncl_be_t2024, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t1982, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 1982, 0.5).
narrative_ontology:measurement(uncl_su_t1995, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 1995, 0.55).
narrative_ontology:measurement(uncl_su_t2008, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 2008, 0.58).
narrative_ontology:measurement(uncl_su_t2024, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_maritime_sovereignty__strict_geographic_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__strict_geographic_reading, unclos_maritime_sovereignty__expansive_construction_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__strict_geographic_reading, unclos_maritime_sovereignty__hybrid_effective_control_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the UNCLOS maritime sovereignty kernel. This 'strict_geographic_reading' emphasizes natural features, influencing and coexisting with other readings that allow for more expansive claims based on artificial construction or effective control.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
