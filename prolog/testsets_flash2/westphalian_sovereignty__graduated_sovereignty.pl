% ============================================================================
% CONSTRAINT STORY: westphalian_sovereignty__graduated_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalian_sovereignty__graduated_sovereignty, []).

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
 *   constraint_id: westphalian_sovereignty__graduated_sovereignty
 *   human_readable: Graduated Sovereignty Doctrine
 *   domain: international_law/political_philosophy/global_governance
 *
 * SUMMARY:
 *   This constraint represents the 'graduated sovereignty' reading of the
 *   Westphalian sovereignty kernel, which posits that state sovereignty is
 *   not absolute but exists on a spectrum determined by state capacity and
 *   governance legitimacy. This reading grants external interveners (powerful
 *   states, international organizations) discretion to classify states,
 *   leading to weak states becoming victims of reclassification and enabling
 *   neo-colonial extraction. The high extractiveness (0.65) and suppression
 *   (0.78) reflect the power asymmetry inherent in this doctrine, where
 *   'legitimacy' is often defined by those with the power to intervene.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalian_sovereignty__graduated_sovereignty, 0.65).
domain_priors:suppression_score(westphalian_sovereignty__graduated_sovereignty, 0.78).
domain_priors:theater_ratio(westphalian_sovereignty__graduated_sovereignty, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, extractiveness, 0.65).
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalian_sovereignty__graduated_sovereignty, snare).
narrative_ontology:human_readable(westphalian_sovereignty__graduated_sovereignty, "Graduated Sovereignty Doctrine").
narrative_ontology:topic_domain(westphalian_sovereignty__graduated_sovereignty, "international_law/political_philosophy/global_governance").

domain_priors:requires_active_enforcement(westphalian_sovereignty__graduated_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalian_sovereignty__graduated_sovereignty, '938ab62a-6020-4c15-8cb9-633d05ca2e84').
narrative_ontology:cs_kernel_codification('938ab62a-6020-4c15-8cb9-633d05ca2e84', distributed).
narrative_ontology:cs_authority_grounding('938ab62a-6020-4c15-8cb9-633d05ca2e84', extraction).
narrative_ontology:cs_interpretation_layer_present('938ab62a-6020-4c15-8cb9-633d05ca2e84').
narrative_ontology:cs_reading_relation('938ab62a-6020-4c15-8cb9-633d05ca2e84', westphalian_sovereignty__absolute_sovereignty, influences).
narrative_ontology:cs_reading_relation('938ab62a-6020-4c15-8cb9-633d05ca2e84', westphalian_sovereignty__conditional_sovereignty, coexists_with).
narrative_ontology:cs_axiom('938ab62a-6020-4c15-8cb9-633d05ca2e84', foundational, sovereignty_is_contingent_on_capacity_and_legitimacy).
narrative_ontology:cs_axiom_status(sovereignty_is_contingent_on_capacity_and_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('938ab62a-6020-4c15-8cb9-633d05ca2e84', sovereignty_is_contingent_on_capacity_and_legitimacy, conventional).
narrative_ontology:cs_axiom('938ab62a-6020-4c15-8cb9-633d05ca2e84', foundational, external_actors_have_discretion_to_assess_sovereignty).
narrative_ontology:cs_axiom_status(external_actors_have_discretion_to_assess_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('938ab62a-6020-4c15-8cb9-633d05ca2e84', external_actors_have_discretion_to_assess_sovereignty, conventional).
narrative_ontology:cs_reference_frame('938ab62a-6020-4c15-8cb9-633d05ca2e84', post_cold_war_interventionism).
narrative_ontology:cs_drift_state('938ab62a-6020-4c15-8cb9-633d05ca2e84', contemporary_multipolar_world, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('938ab62a-6020-4c15-8cb9-633d05ca2e84', '').
narrative_ontology:cs_kernel_id(westphalian_sovereignty__graduated_sovereignty, westphalian_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__graduated_sovereignty, powerful_states).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__graduated_sovereignty, international_organizations).
narrative_ontology:constraint_victim(westphalian_sovereignty__graduated_sovereignty, weak_states).
narrative_ontology:constraint_victim(westphalian_sovereignty__graduated_sovereignty, marginalized_populations_in_weak_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These states define the criteria for 'state capacity' and 'governance legitimacy,' and exercise the discretion to classify other states along the sovereignty spectrum. They benefit from the flexibility to intervene or withhold recognition based on their strategic interests, often framing interventions as humanitarian or capacity-building.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, powerful_states, agenda_setter,
    institutional, generational, arbitrage, global).

% These organizations gain expanded mandates and legitimacy for intervention, aid, and governance programs in states deemed to have 'lower' sovereignty. They benefit from the increased scope of their operations and influence, often aligning with powerful states' interpretations of legitimacy.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, international_organizations, beneficiary,
    institutional, generational, constrained, global).

% These states are subject to external classification and intervention, losing effective control over domestic policy and resource management. Their 'sovereignty' becomes conditional on external approval, leading to a loss of self-determination and increased vulnerability to neo-colonial pressures. Exit means defying powerful international actors, often at great cost.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, weak_states, payer,
    powerless, generational, trapped, national).

% These populations often bear the direct consequences of external interventions and the erosion of state capacity. While interventions are sometimes framed as beneficial, they frequently lead to instability, resource exploitation, and the imposition of governance models that do not align with local needs or values. Their identity is often tied to the state, making 'exit' from its fate impossible.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, marginalized_populations_in_weak_states, payer,
    powerless, biographical, identity_locked, local).

% These scholars analyze the theoretical underpinnings and practical implications of graduated sovereignty. They document its historical evolution, its impact on state relations, and its potential for both positive and negative outcomes, often critiquing its potential for abuse by powerful actors.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, scholars_of_international_law, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for international actors to coordinate responses to perceived governance failures or humanitarian crises within states, by offering a nuanced understanding of state authority beyond a binary 'sovereign/non-sovereign' distinction.
% TRANSFER_FUNCTION: Transfers discretion and authority over domestic affairs from states deemed to have 'lower' capacity or legitimacy to powerful states and international organizations, often accompanied by transfers of resources (aid, military support) that reinforce the power asymmetry.
% ABSENT_VOICES: States and populations that advocate for strict non-interference and the absolute equality of states, regardless of internal capacity, are often marginalized in discussions dominated by powerful states and international organizations. Their arguments for self-determination are often dismissed as outdated or obstructive.
% DISAPPEARANCE_RATIONALE: If the doctrine of graduated sovereignty vanished, powerful states would lose a key justification for intervention and conditionality, forcing a return to either strict non-interference or a more explicit assertion of power politics. Weak states would regain a stronger claim to non-interference, but might also lose access to conditional aid and security guarantees, leading to a significant rearrangement of global governance dynamics.
% FOUNDING_PROBLEM: The traditional Westphalian model of absolute sovereignty struggled to address humanitarian crises, state collapse, and cross-border threats originating from within states, leading to calls for a more flexible approach to international intervention.
% FOUNDING_PROBLEM_CORROBORATION: Powerful states and many international organizations attest that the problem of internal state failures and their international repercussions remains live. Critics, including many scholars from the Global South, argue that while the problem is real, the 'solution' of graduated sovereignty has become a tool for neo-colonial extraction rather than genuine problem-solving; their corroboration is that the problem persists, but the constraint's function has shifted.
narrative_ontology:disappearance_verdict(westphalian_sovereignty__graduated_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(westphalian_sovereignty__graduated_sovereignty, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalian_sovereignty__graduated_sovereignty, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(westphalian_sovereignty__graduated_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(westphalian_sovereignty__graduated_sovereignty, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalian_sovereignty__graduated_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalian_sovereignty__graduated_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalian_sovereignty__graduated_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is high because the doctrine allows powerful actors to define and enforce 'legitimacy' criteria, leading to interventions that serve their interests while weakening the autonomy of target states. Suppression is also high, as weak states have limited means to resist reclassification or intervention without facing severe consequences. The theater ratio is moderate, as there is a genuine, if often co-opted, coordination function in addressing global challenges, but a significant portion of the doctrine's application serves to legitimize power projection.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of powerful states, graduated sovereignty is a necessary evolution of international law to address complex global challenges. From the perspective of weak states, it is a justification for intervention and a tool for maintaining global hierarchies. The engine's classification will highlight this divergence, likely classifying it as a Snare for weak states and a Tangled Rope for powerful states.
 *
 * DIRECTIONALITY LOGIC:
 *   Powerful states and international organizations are clear beneficiaries and agenda-setters, gaining discretion and expanded mandates. Weak states and their marginalized populations are the primary victims, bearing the costs of lost autonomy and external interference. Scholars act as observers, analyzing the doctrine's implications.
 *
 * MANDATROPHY ANALYSIS:
 *   The doctrine's mandate to address global challenges (e.g., humanitarian crises) is still live, but its application has drifted significantly towards enabling extraction and power projection. The classification as a Snare for weak states prevents mislabeling this as pure coordination, highlighting the asymmetric costs and the suppression of alternatives for those at the lower end of the 'sovereignty spectrum.'
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    criteria_for_legitimacy_and_capacity,
    'Are the criteria for ''state capacity'' and ''governance legitimacy'' objectively defined and universally applied, or are they subject to the political interests of powerful states?',
    'Empirical analysis of intervention patterns and recognition practices: if interventions correlate strongly with the strategic interests of powerful states rather than universal criteria, it suggests political influence.',
    'If criteria are politically driven, the extractiveness and suppression of the constraint are higher than currently measured, as the ''coordination'' aspect is further revealed as cover for power projection.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(criteria_for_legitimacy_and_capacity, empirical, 'Ambiguity in the objectivity vs. political nature of sovereignty criteria.').

omega_variable(
    impact_of_intervention_on_local_populations,
    'Do interventions justified by graduated sovereignty consistently improve the well-being and self-determination of marginalized populations in weak states, or do they often exacerbate existing problems or create new forms of dependency?',
    'Longitudinal studies and post-intervention assessments conducted by independent, locally-rooted organizations, focusing on local perspectives and outcomes rather than donor-defined metrics.',
    'If interventions consistently harm or disempower local populations, the ''victim'' status of these populations is amplified, and the constraint''s overall extractiveness is higher, revealing a deeper snare-like quality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(impact_of_intervention_on_local_populations, empirical, 'Uncertainty regarding the true impact of interventions on local populations.').

omega_variable(
    framing_of_sovereignty_as_a_spectrum,
    'Is the concept of ''graduated sovereignty'' a genuine analytical tool for understanding complex statehood, or is it a conceptual reframing that legitimizes existing power asymmetries and interventions?',
    'Conceptual analysis of the doctrine''s historical development and its discursive function in international relations, particularly how it is used by powerful vs. weak states.',
    'If primarily a legitimizing reframing, the ''snare'' classification is reinforced, and the ''coordination function'' is revealed as largely theatrical, increasing the effective extractiveness for victims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framing_of_sovereignty_as_a_spectrum, conceptual, 'Whether graduated sovereignty is an analytical tool or a legitimizing frame for power.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalian_sovereignty__graduated_sovereignty, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t1990, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(west_tr_t1998, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 1998, 0.15).
narrative_ontology:measurement(west_tr_t2006, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 2006, 0.18).
narrative_ontology:measurement(west_tr_t2014, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 2014, 0.19).
narrative_ontology:measurement(west_tr_t2024, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(west_be_t1990, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 1990, 0.5).
narrative_ontology:measurement(west_be_t1998, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 1998, 0.55).
narrative_ontology:measurement(west_be_t2006, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 2006, 0.6).
narrative_ontology:measurement(west_be_t2014, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 2014, 0.63).
narrative_ontology:measurement(west_be_t2024, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t1990, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 1990, 0.65).
narrative_ontology:measurement(west_su_t1998, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 1998, 0.7).
narrative_ontology:measurement(west_su_t2006, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 2006, 0.75).
narrative_ontology:measurement(west_su_t2014, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 2014, 0.77).
narrative_ontology:measurement(west_su_t2024, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalian_sovereignty__graduated_sovereignty, enforcement_mechanism).
narrative_ontology:affects_constraint(westphalian_sovereignty__graduated_sovereignty, westphalian_sovereignty__absolute_sovereignty).
narrative_ontology:affects_constraint(westphalian_sovereignty__graduated_sovereignty, westphalian_sovereignty__conditional_sovereignty).
narrative_ontology:affects_constraint(westphalian_sovereignty__graduated_sovereignty, responsibility_to_protect_doctrine).
narrative_ontology:affects_constraint(westphalian_sovereignty__graduated_sovereignty, international_aid_conditionality).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'westphalian_sovereignty' kernel. It influences and is influenced by other readings, particularly 'absolute_sovereignty' (which it challenges) and 'conditional_sovereignty' (which it provides a framework for).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
