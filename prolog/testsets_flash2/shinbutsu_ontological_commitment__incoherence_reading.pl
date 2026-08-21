% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_commitment__incoherence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_ontological_commitment__incoherence_reading, []).

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
 *   constraint_id: shinbutsu_ontological_commitment__incoherence_reading
 *   human_readable: Shinbutsu-shūgō as Institutionally Tolerated Ontological Incoherence
 *   domain: religious_studies/japanese_history/ontology_of_practice
 *
 * SUMMARY:
 *   This constraint describes shinbutsu-shūgō (the syncretic fusion of Shinto
 *   and Buddhism in Japan) as an institutionally tolerated ontological
 *   incoherence, rather than a deeply integrated syncretism. This reading
 *   emphasizes the pragmatic, often superficial, nature of the fusion, which
 *   allowed for easy separation during the Meiji Restoration's state-building
 *   efforts. The constraint's extractiveness and suppression rise sharply
 *   towards the end of the interval, reflecting the Meiji state's forceful
 *   dismantling of the prior arrangement.
 *
 * KEY AGENTS:
 *   - meiji_state_builders: Primary beneficiary (institutional/arbitrage) — benefited from ease of separation
 *   - shinto_priests_post_meiji: Beneficiary (organized/mobile) — gained status and resources
 *   - buddhist_institutions_pre_meiji: Primary payer (institutional/constrained) — suffered losses during Haibutsu Kishaku
 *   - local_communities_pre_meiji: Payer (powerless/trapped) — experienced disruption and loss of familiar practices
 *   - scholars_of_religion: Observer (analytical/analytical) — analyze historical and theological aspects
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_commitment__incoherence_reading, 0.65).
domain_priors:suppression_score(shinbutsu_ontological_commitment__incoherence_reading, 0.7).
domain_priors:theater_ratio(shinbutsu_ontological_commitment__incoherence_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_commitment__incoherence_reading, tangled_rope).
narrative_ontology:human_readable(shinbutsu_ontological_commitment__incoherence_reading, "Shinbutsu-shūgō as Institutionally Tolerated Ontological Incoherence").
narrative_ontology:topic_domain(shinbutsu_ontological_commitment__incoherence_reading, "religious_studies/japanese_history/ontology_of_practice").

domain_priors:requires_active_enforcement(shinbutsu_ontological_commitment__incoherence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_commitment__incoherence_reading, '6b446b3a-a0fa-4fde-b731-927e2fa03aa6').
narrative_ontology:cs_kernel_codification('6b446b3a-a0fa-4fde-b731-927e2fa03aa6', implicit).
narrative_ontology:cs_authority_grounding('6b446b3a-a0fa-4fde-b731-927e2fa03aa6', extraction).
narrative_ontology:cs_interpretation_layer_present('6b446b3a-a0fa-4fde-b731-927e2fa03aa6').
narrative_ontology:cs_reading_relation('6b446b3a-a0fa-4fde-b731-927e2fa03aa6', shinbutsu_ontological_commitment__syncretic_reading, coexists_with).
narrative_ontology:cs_reading_relation('6b446b3a-a0fa-4fde-b731-927e2fa03aa6', shinbutsu_ontological_commitment__partition_reading, coexists_with).
narrative_ontology:cs_axiom('6b446b3a-a0fa-4fde-b731-927e2fa03aa6', foundational, ontological_pluralism_tolerated).
narrative_ontology:cs_axiom_status(ontological_pluralism_tolerated, holdable).
narrative_ontology:cs_axiom_grounding('6b446b3a-a0fa-4fde-b731-927e2fa03aa6', ontological_pluralism_tolerated, conventional).
narrative_ontology:cs_axiom('6b446b3a-a0fa-4fde-b731-927e2fa03aa6', secondary, functional_integration_without_unity).
narrative_ontology:cs_axiom_status(functional_integration_without_unity, holdable).
narrative_ontology:cs_axiom_grounding('6b446b3a-a0fa-4fde-b731-927e2fa03aa6', functional_integration_without_unity, empirically_contingent).
narrative_ontology:cs_reference_frame('6b446b3a-a0fa-4fde-b731-927e2fa03aa6', pragmatic_coexistence_framework).
narrative_ontology:cs_drift_state('6b446b3a-a0fa-4fde-b731-927e2fa03aa6', meiji_restoration_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('6b446b3a-a0fa-4fde-b731-927e2fa03aa6', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_commitment__incoherence_reading, shinbutsu_ontological_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__incoherence_reading, meiji_state_builders).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__incoherence_reading, shinto_priests_post_meiji).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__incoherence_reading, buddhist_institutions_pre_meiji).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__incoherence_reading, local_communities_pre_meiji).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively sought to establish a unified national identity and state religion, viewing the prior shinbutsu-shūgō as an obstacle. Benefited from the ease of separating Shinto and Buddhist elements due to the lack of deep ontological integration, allowing for the creation of State Shinto.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, meiji_state_builders, agenda_setter,
    institutional, generational, arbitrage, national).

% Benefited from the Meiji government's promotion of Shinto as the national religion, gaining institutional support, land, and prestige. Their role was elevated by the formal separation from Buddhism, which this reading suggests was structurally easy to achieve.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, shinto_priests_post_meiji, beneficiary,
    organized, biographical, mobile, local).

% Suffered significant losses during the Haibutsu Kishaku (abolish Buddhism, destroy Shaka) movement, including confiscation of land, destruction of temples, and forced defrocking of monks. Their integrated status with Shinto was forcibly dismantled, leading to a loss of influence and resources.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, buddhist_institutions_pre_meiji, payer,
    institutional, generational, constrained, local).

% Experienced the forced separation of Shinto and Buddhist practices, which had been deeply intertwined in their daily lives and rituals. This caused confusion, disruption, and the loss of familiar religious structures, with little agency to resist the state's directives.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, local_communities_pre_meiji, payer,
    powerless, biographical, trapped, local).

% Analyze the historical and theological underpinnings of shinbutsu-shūgō, debating the extent of its syncretism versus its practical, often incoherent, coexistence. This reading aligns with interpretations emphasizing the pragmatic rather than deeply integrated nature of the pre-Meiji religious landscape.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, scholars_of_religion, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allowed for the practical coexistence of Shinto and Buddhist practices and institutions across Japan for centuries, providing a flexible framework for local religious life without requiring deep theological reconciliation.
% TRANSFER_FUNCTION: Transferred legitimacy and resources between Shinto and Buddhist institutions based on local power dynamics and patronage, often without a clear, unified ontological basis. Post-Meiji, it facilitated the transfer of power and resources from Buddhist to Shinto institutions.
% ABSENT_VOICES: Theological proponents of a deeply unified honji-suijaku cosmology, who would argue against the 'incoherence' reading, were marginalized by the Meiji state's agenda. Local practitioners who simply lived the integrated reality without theorizing it also lacked a voice in the state-driven redefinition.
% DISAPPEARANCE_RATIONALE: If the institutional tolerance of ontological incoherence vanished, the Meiji state's ability to rapidly dismantle shinbutsu-shūgō would have been severely hampered. The subsequent formation of State Shinto and the persecution of Buddhism relied on the structural ease of separating these elements, which this reading emphasizes.
% FOUNDING_PROBLEM: The need to integrate or manage the coexistence of indigenous kami worship with the imported Buddhist tradition, which offered sophisticated philosophical and soteriological frameworks.
% FOUNDING_PROBLEM_CORROBORATION: Scholarly consensus largely agrees that the 'problem' of integrating Shinto and Buddhism was historically managed through various pragmatic arrangements, rather than a single, stable ontological commitment. The Meiji state's actions confirm that the prior arrangement was seen as a problem to be 'solved' by separation, indicating its 'dead' status as a functional integration strategy.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_commitment__incoherence_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_commitment__incoherence_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_commitment__incoherence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(shinbutsu_ontological_commitment__incoherence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_commitment__incoherence_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_commitment__incoherence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(shinbutsu_ontological_commitment__incoherence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(shinbutsu_ontological_commitment__incoherence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) and suppression (0.70) are high because the Meiji state leveraged the inherent 'incoherence' of shinbutsu-shūgō to forcibly dismantle Buddhist institutions and elevate Shinto. The 'coordination' function of pragmatic coexistence was replaced by state-enforced separation, which extracted resources and legitimacy from Buddhist entities and local communities. The theater ratio (0.40) reflects the performative maintenance of a 'unified' religious landscape that, in this reading, lacked deep ontological integration, making it vulnerable to political re-engineering. The sharp rise in metrics towards the end of the interval (1868-1870) directly corresponds to the Haibutsu Kishaku movement.
 *
 * PERSPECTIVAL GAP:
 *   The Meiji state builders experienced this 'incoherence' as an opportunity for national unification and resource reallocation, making them beneficiaries. Buddhist institutions and local communities, however, experienced it as a violent rupture and extraction, making them victims. The ease of separation, from the state's perspective, was a structural feature that enabled their agenda, while for others, it meant the collapse of their religious world.
 *
 * DIRECTIONALITY LOGIC:
 *   Meiji state builders are full beneficiaries (d=0.0) as the structural incoherence allowed them to easily implement their agenda. Shinto priests post-Meiji are also beneficiaries (d=0.1) due to their elevated status. Buddhist institutions pre-Meiji are full targets (d=1.0) as they bore the brunt of the state's extractive policies. Local communities are also targets (d=0.9) due to the disruption and lack of agency. Scholars are analytical observers (d=0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the Meiji state's actions as pure coordination. While the state claimed to be 'restoring' a pure Shinto, this reading highlights how the prior 'incoherence' was exploited for political extraction. The constraint's mandate (pragmatic coexistence) was resolved by state force, not by internal evolution, leading to a new, highly extractive arrangement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    depth_of_syncretism_ambiguity,
    'To what extent was shinbutsu-shūgō a superficial coexistence versus a deeply integrated syncretism at the ontological level?',
    'Further archaeological and textual analysis of local religious practices and theological treatises from the pre-Meiji period, focusing on explicit statements of ontological unity versus pragmatic functional integration.',
    'If deeper ontological integration is found, this ''incoherence_reading'' would be weakened, suggesting the Meiji separation required more active suppression than this reading implies. If superficiality is confirmed, this reading is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(depth_of_syncretism_ambiguity, empirical, 'Ambiguity regarding the true depth of Shinto-Buddhist syncretism.').

omega_variable(
    meiji_state_ideology_vs_pragmatism,
    'Was the Meiji state''s dismantling of shinbutsu-shūgō primarily driven by a genuine ideological commitment to ''pure'' Shinto, or by pragmatic state-building goals (e.g., national unity, resource acquisition)?',
    'Analysis of primary historical documents, government decrees, and internal debates within the Meiji leadership, weighing ideological rhetoric against material outcomes and political expediency.',
    'If pragmatic goals were dominant, it strengthens the ''extraction'' aspect of this constraint, as the ''incoherence'' was a tool. If ideological purity was paramount, it suggests a different form of ''coordination'' (of national identity) with extraction as a side effect.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(meiji_state_ideology_vs_pragmatism, conceptual, 'Ambiguity regarding the primary motivation behind the Meiji state''s actions.').

omega_variable(
    local_resistance_measurement,
    'How much active, organized resistance did local communities mount against the forced separation of Shinto and Buddhism, and how effective was it?',
    'Detailed local historical studies, examining village records, petitions, and accounts of unrest or non-compliance during the Haibutsu Kishaku period.',
    'Higher levels of effective resistance would suggest that the ''incoherence'' was less universally accepted at the local level than this reading implies, and that the suppression metric might be even higher to overcome local attachment. Lower resistance would support the ease-of-separation aspect.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(local_resistance_measurement, empirical, 'Measurement of local resistance to the forced separation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_commitment__incoherence_reading, 1600, 1870).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t1600, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 1600, 0.05).
narrative_ontology:measurement(shin_tr_t1700, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 1700, 0.1).
narrative_ontology:measurement(shin_tr_t1800, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 1800, 0.2).
narrative_ontology:measurement(shin_tr_t1860, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 1860, 0.3).
narrative_ontology:measurement(shin_tr_t1868, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 1868, 0.35).
narrative_ontology:measurement(shin_tr_t1870, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 1870, 0.4).

% Extraction over time
narrative_ontology:measurement(shin_be_t1600, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 1600, 0.1).
narrative_ontology:measurement(shin_be_t1700, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 1700, 0.15).
narrative_ontology:measurement(shin_be_t1800, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 1800, 0.25).
narrative_ontology:measurement(shin_be_t1860, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 1860, 0.4).
narrative_ontology:measurement(shin_be_t1868, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 1868, 0.6).
narrative_ontology:measurement(shin_be_t1870, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 1870, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t1600, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 1600, 0.1).
narrative_ontology:measurement(shin_su_t1700, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 1700, 0.15).
narrative_ontology:measurement(shin_su_t1800, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 1800, 0.2).
narrative_ontology:measurement(shin_su_t1860, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 1860, 0.35).
narrative_ontology:measurement(shin_su_t1868, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 1868, 0.65).
narrative_ontology:measurement(shin_su_t1870, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 1870, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_commitment__incoherence_reading, identity_coordination).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__incoherence_reading, meiji_state_shinto_establishment).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__incoherence_reading, japanese_national_identity_formation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'shinbutsu_ontological_commitment' kernel. This 'incoherence_reading' emphasizes the pragmatic, rather than deeply integrated, nature of shinbutsu-shūgō, which facilitated its dismantling by the Meiji state. Sibling readings include 'syncretic_reading' (emphasizing a unified cosmology) and 'partition_reading' (emphasizing separate domains).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
