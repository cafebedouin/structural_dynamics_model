% ============================================================================
% CONSTRAINT STORY: feud_obligation_kernel__extraction_cycle_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feud_obligation_kernel__extraction_cycle_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: feud_obligation_kernel__extraction_cycle_reading
 *   human_readable: Blood-Feud Extraction Cycle
 *   domain: legal_anthropology/political_systems
 *
 * SUMMARY:
 *   This constraint describes blood-feud obligations as a destructive
 *   extraction cycle, depleting productive capacity and preventing
 *   territorial consolidation. It is one reading of the
 *   'feud_obligation_kernel', which is also interpreted as a stateless
 *   coordination mechanism or a violation of divine law. This
 *   'extraction_cycle_reading' focuses on the material costs and the way
 *   feuds inadvertently create conditions for centralized power to emerge by
 *   offering an alternative to chaos. The metrics reflect high extraction and
 *   suppression, with low theater, as the violence is very real and
 *   functional in its destructive capacity.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feud_obligation_kernel__extraction_cycle_reading, 0.85).
domain_priors:suppression_score(feud_obligation_kernel__extraction_cycle_reading, 0.75).
domain_priors:theater_ratio(feud_obligation_kernel__extraction_cycle_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feud_obligation_kernel__extraction_cycle_reading, snare).
narrative_ontology:human_readable(feud_obligation_kernel__extraction_cycle_reading, "Blood-Feud Extraction Cycle").
narrative_ontology:topic_domain(feud_obligation_kernel__extraction_cycle_reading, "legal_anthropology/political_systems").

domain_priors:requires_active_enforcement(feud_obligation_kernel__extraction_cycle_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feud_obligation_kernel__extraction_cycle_reading, 'd25c5ca1-a4c8-46d6-ad4b-02d615616f43').
narrative_ontology:cs_kernel_codification('d25c5ca1-a4c8-46d6-ad4b-02d615616f43', implicit).
narrative_ontology:cs_authority_grounding('d25c5ca1-a4c8-46d6-ad4b-02d615616f43', practice).
narrative_ontology:cs_interpretation_layer_present('d25c5ca1-a4c8-46d6-ad4b-02d615616f43').
narrative_ontology:cs_reading_relation('d25c5ca1-a4c8-46d6-ad4b-02d615616f43', feud_obligation_kernel__stateless_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('d25c5ca1-a4c8-46d6-ad4b-02d615616f43', feud_obligation_kernel__christianized_pacification_reading, coexists_with).
narrative_ontology:cs_axiom('d25c5ca1-a4c8-46d6-ad4b-02d615616f43', foundational, violence_as_net_negative_sum_game).
narrative_ontology:cs_axiom_status(violence_as_net_negative_sum_game, holdable).
narrative_ontology:cs_axiom_grounding('d25c5ca1-a4c8-46d6-ad4b-02d615616f43', violence_as_net_negative_sum_game, empirically_contingent).
narrative_ontology:cs_axiom('d25c5ca1-a4c8-46d6-ad4b-02d615616f43', secondary, centralized_monopoly_on_violence_as_efficiency_gain).
narrative_ontology:cs_axiom_status(centralized_monopoly_on_violence_as_efficiency_gain, holdable).
narrative_ontology:cs_axiom_grounding('d25c5ca1-a4c8-46d6-ad4b-02d615616f43', centralized_monopoly_on_violence_as_efficiency_gain, instrumental).
narrative_ontology:cs_reference_frame('d25c5ca1-a4c8-46d6-ad4b-02d615616f43', pre_state_formation_anarchy).
narrative_ontology:cs_drift_state('d25c5ca1-a4c8-46d6-ad4b-02d615616f43', early_state_consolidation_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('d25c5ca1-a4c8-46d6-ad4b-02d615616f43', '').
narrative_ontology:cs_kernel_id(feud_obligation_kernel__extraction_cycle_reading, feud_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__extraction_cycle_reading, emerging_royal_authority).
narrative_ontology:constraint_victim(feud_obligation_kernel__extraction_cycle_reading, feud_participants).
narrative_ontology:constraint_victim(feud_obligation_kernel__extraction_cycle_reading, kin_groups).
narrative_ontology:constraint_victim(feud_obligation_kernel__extraction_cycle_reading, local_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(feud_obligation_kernel__extraction_cycle_reading, peasantry).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals directly involved in feuds, bound by kinship and honor to seek vengeance. They bear the direct costs of violence, injury, death, and loss of property. Exit is nearly impossible due to social pressure and the cycle of retaliation.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, feud_participants, payer,
    powerless, biographical, identity_locked, local).

% Extended families and clans whose members are drawn into feuds. They suffer collective resource depletion, loss of productive members, and constant insecurity. Their options are limited to escalating the feud, seeking external mediation, or migrating.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, kin_groups, payer,
    moderate, generational, constrained, local).

% Territorial units affected by ongoing feuds. They experience disruption of trade, agriculture, and social order, leading to overall economic decline and population displacement. They are often caught between feuding parties.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, local_communities, payer,
    moderate, generational, constrained, local).

% Centralized power structures (e.g., kings, dukes) that benefit from the breakdown of local order. Feuds create a demand for a monopoly on violence, which the royal authority can offer in exchange for loyalty, taxes, and consolidation of power. They actively suppress kinship-based enforcement to assert their own legitimacy.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, emerging_royal_authority, beneficiary,
    institutional, generational, arbitrage, regional).

% The agricultural labor force, often caught in the crossfire of feuds. They suffer property damage, forced conscription, and increased insecurity, leading to reduced productivity and famine. They have virtually no exit options.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, peasantry, payer,
    powerless, immediate, trapped, local).

% Historians, anthropologists, and political scientists who analyze the long-term effects of blood feuds on societal development and state formation. They observe the destructive patterns and their role in legitimizing centralized power.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, scholarly_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint does not solve a genuine coordination problem; rather, it is a self-perpetuating cycle of violence driven by honor and vengeance, which actively prevents broader coordination and consolidation.
% TRANSFER_FUNCTION: Transfers productive capacity, human lives, and social stability from feud participants, kin groups, and local communities to the destructive cycle itself, indirectly benefiting emerging centralized authorities by creating a vacuum for their 'order'.
% ABSENT_VOICES: Future generations and those seeking peaceful coexistence are absent from the immediate decision-making, as the cycle of vengeance prioritizes past grievances over future prosperity. They would advocate for alternative dispute resolution and state-backed justice.
% DISAPPEARANCE_RATIONALE: If blood-feud obligations vanished overnight, local communities would experience a dramatic increase in stability and productive capacity. Emerging royal authorities would lose a key mechanism for legitimizing their monopoly on violence, forcing them to find alternative means of territorial consolidation and tax extraction. The social fabric would reorganize around new forms of justice and conflict resolution.
% FOUNDING_PROBLEM: The perceived need for justice and retribution for wrongs committed against kin, in the absence of a universally recognized and effective centralized legal system.
% FOUNDING_PROBLEM_CORROBORATION: Scholarly observers and historical records attest that while the initial problem of justice was real, the feud system itself became the primary problem, generating more conflict than it resolved. Emerging royal authorities, while benefiting, also actively worked to suppress feuds, indicating a recognition of their destructive nature. No current benefiting party genuinely claims the problem is still live in its original form.
narrative_ontology:disappearance_verdict(feud_obligation_kernel__extraction_cycle_reading, world_rearranges).
narrative_ontology:founding_problem_status(feud_obligation_kernel__extraction_cycle_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feud_obligation_kernel__extraction_cycle_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(feud_obligation_kernel__extraction_cycle_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feud_obligation_kernel__extraction_cycle_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(feud_obligation_kernel__extraction_cycle_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(feud_obligation_kernel__extraction_cycle_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because the system imposes severe costs (lives, resources, security) on participants without providing commensurate benefits. Suppression is high (0.75) due to the intense social pressure, honor codes, and the lack of viable alternatives for dispute resolution, effectively trapping participants in the cycle. Theater ratio is low (0.1) because the violence and its consequences are very real and not merely performative; the system is genuinely destructive. Accessibility collapse is high (0.9) as the social structure and norms make it extremely difficult to opt out of the feud cycle once initiated. Resistance is low (0.2) because individual or kin-group resistance is often met with further violence, and collective resistance is difficult to organize against deeply ingrained social norms.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of feud participants, the obligation is a matter of honor and justice, a necessary response to a wrong. From the perspective of emerging royal authority, it is a chaotic force that undermines their control but also creates an opportunity to assert a monopoly on violence. The engine's classification as a snare reflects the objective, systemic extraction, regardless of the participants' subjective justifications.
 *
 * DIRECTIONALITY LOGIC:
 *   Feud participants, kin groups, and local communities are clear targets (payers) due to the direct and indirect costs they bear. The emerging royal authority is a beneficiary, as the chaos of feuds legitimizes their claim to provide order and extract taxes. The peasantry is also a target, suffering the consequences without agency. The 'identity_locked' exit option for feud participants highlights the powerful social and cultural mechanisms that bind them to the cycle.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (justice in absence of state) is 'dead' because the feud system itself became the primary source of injustice and instability. The persistence of the obligation, despite its destructive nature, is maintained by deeply ingrained social norms and the lack of viable alternatives, rather than by its original function. This aligns with a snare classification, where the coordination story (justice) is cover for a destructive, self-perpetuating cycle that benefits an external party (emerging royal authority) by creating a power vacuum.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_vs_constructed_obligation,
    'To what extent are blood-feud obligations a ''natural'' response to statelessness, versus a socially constructed and perpetuated system?',
    'Comparative historical analysis of different stateless societies and their conflict resolution mechanisms; anthropological studies of cultural evolution of honor codes.',
    'If more ''natural'', the extractiveness might be seen as an unavoidable cost of a particular social structure. If more ''constructed'', it strengthens the ''snare'' classification by highlighting the agency in its perpetuation and the possibility of alternative constructions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_vs_constructed_obligation, conceptual, 'Ambiguity between inherent social dynamics and cultural construction of feud obligations.').

omega_variable(
    legitimacy_of_royal_intervention,
    'Is the emerging royal authority''s intervention against feuds a genuine act of pacification, or primarily a strategic move to consolidate power and extract resources?',
    'Analysis of royal decrees, legal reforms, and historical outcomes: do they genuinely reduce violence and improve welfare, or primarily centralize control and increase taxation?',
    'If primarily strategic, it reinforces the ''snare'' classification by highlighting the external beneficiary''s role in perpetuating the conditions that legitimize their extraction. If genuinely pacifying, it might suggest a transition towards a ''scaffold'' or ''rope'' as the state provides a new coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_royal_intervention, empirical, 'Motivation behind royal authority''s suppression of feuds.').

omega_variable(
    framing_under_determination_feud_kernel,
    'Given the ''feud_obligation_kernel'', is this ''extraction_cycle_reading'' the most accurate framing, or do the ''stateless_coordination_reading'' or ''christianized_pacification_reading'' offer equally valid, albeit different, structural insights?',
    'Cross-reading comparison of predictive power and explanatory scope across diverse historical and anthropological cases. Which reading best accounts for the observed dynamics and outcomes?',
    'If alternative readings are equally valid, it highlights the conceptual ambiguity inherent in classifying such complex social phenomena, suggesting that no single classification fully captures the constraint''s multifaceted nature. This reading''s classification as a snare would be contextualized as one valid perspective among others.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framing_under_determination_feud_kernel, conceptual, 'Under-determination of the ''feud_obligation_kernel'' by competing readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feud_obligation_kernel__extraction_cycle_reading, 0, 500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t0, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(feud_tr_t100, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 100, 0.12).
narrative_ontology:measurement(feud_tr_t200, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 200, 0.1).
narrative_ontology:measurement(feud_tr_t300, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 300, 0.1).
narrative_ontology:measurement(feud_tr_t400, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 400, 0.1).
narrative_ontology:measurement(feud_tr_t500, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 500, 0.1).

% Extraction over time
narrative_ontology:measurement(feud_be_t0, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(feud_be_t100, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 100, 0.78).
narrative_ontology:measurement(feud_be_t200, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 200, 0.82).
narrative_ontology:measurement(feud_be_t300, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 300, 0.85).
narrative_ontology:measurement(feud_be_t400, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 400, 0.85).
narrative_ontology:measurement(feud_be_t500, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 500, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(feud_su_t0, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(feud_su_t100, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 100, 0.65).
narrative_ontology:measurement(feud_su_t200, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 200, 0.7).
narrative_ontology:measurement(feud_su_t300, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 300, 0.75).
narrative_ontology:measurement(feud_su_t400, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 400, 0.75).
narrative_ontology:measurement(feud_su_t500, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 500, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feud_obligation_kernel__extraction_cycle_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is the 'extraction_cycle_reading' of the 'feud_obligation_kernel'. It focuses on the destructive economic and social costs of feuds, and how they create a power vacuum that benefits emerging centralized authorities. This contrasts with the 'stateless_coordination_reading' (which views feuds as a form of justice in stateless societies) and the 'christianized_pacification_reading' (which frames feuds as a moral transgression against divine law).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
