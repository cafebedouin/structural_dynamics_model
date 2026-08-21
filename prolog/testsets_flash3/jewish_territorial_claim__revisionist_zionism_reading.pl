% ============================================================================
% CONSTRAINT STORY: jewish_territorial_claim__revisionist_zionism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_territorial_claim__revisionist_zionism_reading, []).

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
 *   constraint_id: jewish_territorial_claim__revisionist_zionism_reading
 *   human_readable: Revisionist Zionist Maximalist Territorial Claim (Both Banks of Jordan)
 *   domain: political_history/settler_colonialism/nationalism_studies
 *
 * SUMMARY:
 *   This constraint represents the Revisionist Zionist reading of the Jewish
 *   territorial claim, which asserts an inalienable right to a maximalist
 *   territory (both banks of the Jordan River) and explicitly rejects the
 *   need for Arab consent, relying instead on military force ('Iron Wall') to
 *   compel acceptance. This reading is distinct from other Zionist currents
 *   by its immediate and non-negotiable demand for sovereignty over the
 *   entire claimed territory and its emphasis on military strength as the
 *   primary means to achieve this. The constraint is classified as a Snare
 *   due to its high extractiveness, active suppression of alternatives, and
 *   identifiable victims.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_territorial_claim__revisionist_zionism_reading, 0.95).
domain_priors:suppression_score(jewish_territorial_claim__revisionist_zionism_reading, 0.9).
domain_priors:theater_ratio(jewish_territorial_claim__revisionist_zionism_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, extractiveness, 0.95).
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_territorial_claim__revisionist_zionism_reading, snare).
narrative_ontology:human_readable(jewish_territorial_claim__revisionist_zionism_reading, "Revisionist Zionist Maximalist Territorial Claim (Both Banks of Jordan)").
narrative_ontology:topic_domain(jewish_territorial_claim__revisionist_zionism_reading, "political_history/settler_colonialism/nationalism_studies").

domain_priors:requires_active_enforcement(jewish_territorial_claim__revisionist_zionism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_territorial_claim__revisionist_zionism_reading, '7cdca292-c96a-4e26-ac24-52ff91aa2e82').
narrative_ontology:cs_kernel_codification('7cdca292-c96a-4e26-ac24-52ff91aa2e82', formalized).
narrative_ontology:cs_authority_grounding('7cdca292-c96a-4e26-ac24-52ff91aa2e82', lineage).
narrative_ontology:cs_interpretation_layer_present('7cdca292-c96a-4e26-ac24-52ff91aa2e82').
narrative_ontology:cs_reading_relation('7cdca292-c96a-4e26-ac24-52ff91aa2e82', jewish_territorial_claim__cultural_zionism_reading, forecloses).
narrative_ontology:cs_reading_relation('7cdca292-c96a-4e26-ac24-52ff91aa2e82', jewish_territorial_claim__labor_zionism_reading, forecloses).
narrative_ontology:cs_reading_relation('7cdca292-c96a-4e26-ac24-52ff91aa2e82', jewish_territorial_claim__political_zionism_reading, forecloses).
narrative_ontology:cs_axiom('7cdca292-c96a-4e26-ac24-52ff91aa2e82', foundational, inalienable_right_to_greater_israel).
narrative_ontology:cs_axiom_status(inalienable_right_to_greater_israel, holdable).
narrative_ontology:cs_axiom_grounding('7cdca292-c96a-4e26-ac24-52ff91aa2e82', inalienable_right_to_greater_israel, theological).
narrative_ontology:cs_axiom('7cdca292-c96a-4e26-ac24-52ff91aa2e82', foundational, arab_consent_irrelevant_to_sovereignty).
narrative_ontology:cs_axiom_status(arab_consent_irrelevant_to_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('7cdca292-c96a-4e26-ac24-52ff91aa2e82', arab_consent_irrelevant_to_sovereignty, conventional).
narrative_ontology:cs_reference_frame('7cdca292-c96a-4e26-ac24-52ff91aa2e82', maximalist_sovereignty_over_greater_israel).
narrative_ontology:cs_drift_state('7cdca292-c96a-4e26-ac24-52ff91aa2e82', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('7cdca292-c96a-4e26-ac24-52ff91aa2e82', '').
narrative_ontology:cs_kernel_id(jewish_territorial_claim__revisionist_zionism_reading, jewish_territorial_claim).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__revisionist_zionism_reading, revisionist_zionist_movement).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__revisionist_zionism_reading, jewish_settlers_in_greater_israel).
narrative_ontology:constraint_victim(jewish_territorial_claim__revisionist_zionism_reading, palestinian_arabs).
narrative_ontology:constraint_victim(jewish_territorial_claim__revisionist_zionism_reading, neighboring_arab_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocates for and actively pursues the establishment of a Jewish state on both banks of the Jordan River, viewing this as an inalienable historical right. It rejects any notion of Arab consent as a prerequisite and prioritizes military strength to achieve and maintain this claim.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, revisionist_zionist_movement, agenda_setter,
    institutional, generational, identity_locked, regional).

% Directly benefit from the expansion of Jewish sovereignty and settlement into areas claimed by the maximalist vision. Their presence on the ground reinforces the territorial claim and is actively supported by the movement's ideology and resources.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, jewish_settlers_in_greater_israel, beneficiary,
    organized, biographical, constrained, local).

% Are the primary targets of this territorial claim, facing dispossession, displacement, and subjugation under a system that explicitly denies their national rights and seeks to compel their acceptance through force. Their resistance is met with suppression.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, palestinian_arabs, payer,
    powerless, generational, trapped, local).

% Are compelled to accept the maximalist claim through military deterrence, facing constant pressure and potential conflict if they do not. Their sovereignty and regional influence are directly challenged by the expansionist vision.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, neighboring_arab_states, payer,
    powerful, generational, constrained, regional).

% Observes the conflict, often condemning the maximalist claims and the use of force, but frequently unable or unwilling to impose effective counter-constraints due to geopolitical complexities and the entrenched nature of the conflict.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, international_community, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the actions and ideological commitment of a nationalist movement towards a singular, maximalist territorial goal, ensuring internal cohesion and a unified front against external opposition.
% TRANSFER_FUNCTION: Transfers land, resources, and sovereignty from Palestinian Arabs and neighboring Arab states to the Jewish state, secured through military force and political assertion.
% ABSENT_VOICES: Any voices advocating for a shared, binational, or non-ethno-nationalist future for the land are systematically excluded or suppressed, as their proposals directly contradict the maximalist, exclusive claim.
% DISAPPEARANCE_RATIONALE: If the maximalist territorial claim and its enforcement vanished, the entire political and demographic landscape of the region would be fundamentally reshaped. Borders would be redrawn, Palestinian national aspirations would gain immediate traction, and the 'Iron Wall' of military force would collapse, leading to a complete reorganization of power and land ownership.
% FOUNDING_PROBLEM: The perceived existential threat to Jewish people globally, requiring a secure, sovereign Jewish state with defensible borders, and the historical claim to the entirety of Eretz Israel.
% FOUNDING_PROBLEM_CORROBORATION: The Revisionist Zionist movement and its adherents attest that the founding problem of Jewish insecurity and the historical claim to the land remain live. Critics and Palestinian voices dispute the necessity of maximalist claims for security and challenge the exclusive interpretation of historical rights, arguing that the problem has evolved into one of settler-colonial expansion rather than defensive necessity.
narrative_ontology:disappearance_verdict(jewish_territorial_claim__revisionist_zionism_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_territorial_claim__revisionist_zionism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_territorial_claim__revisionist_zionism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(jewish_territorial_claim__revisionist_zionism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_territorial_claim__revisionist_zionism_reading, 0.95, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_territorial_claim__revisionist_zionism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_territorial_claim__revisionist_zionism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_territorial_claim__revisionist_zionism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.95) because the claim demands the complete transfer of sovereignty and resources from the indigenous population and neighboring states without compensation or consent. Suppression is also very high (0.9) as the claim's persistence relies on continuous military enforcement and the active suppression of Palestinian resistance and national aspirations. Theater ratio is low (0.1) because the constraint is primarily functional in its coercive intent; there is little performative activity masking a degraded function. Accessibility collapse is high (0.8) for Palestinians, as the claim seeks to eliminate their political and territorial alternatives. Resistance is high (0.85) due to ongoing Palestinian and Arab opposition.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Revisionist Zionist movement, this claim is a necessary and just assertion of historical rights and security. From the perspective of Palestinian Arabs, it is a clear act of settler-colonial extraction and oppression. The engine's classification as a Snare reflects the structural reality of extraction and suppression, independent of the ideological justifications.
 *
 * DIRECTIONALITY LOGIC:
 *   The Revisionist Zionist movement and Jewish settlers are clear beneficiaries, gaining land and sovereignty. Palestinian Arabs are the primary victims, facing dispossession and subjugation. Neighboring Arab states are also victims, compelled to accept the claim through military deterrence. The international community acts as an observer, often critical but with limited capacity to alter the constraint's fundamental dynamics.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_legitimacy_of_claim,
    'Is the historical claim to ''both banks of the Jordan'' a legitimate basis for exclusive sovereignty, or is it a selective interpretation of history used to justify expansion?',
    'Comprehensive, multi-disciplinary historical and archaeological research, combined with international legal arbitration that considers competing historical narratives and indigenous rights.',
    'If the claim is found to be a selective interpretation, the constraint''s ''naturalness'' argument collapses, reclassifying it more firmly as a constructed Snare. If a unique, exclusive historical right is universally affirmed, it would lend a (contested) Mountain-like aspect to the claim''s foundation, though its enforcement would remain extractive.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(historical_legitimacy_of_claim, conceptual, 'Ambiguity regarding the historical and legal legitimacy of the maximalist territorial claim.').

omega_variable(
    iron_wall_efficacy_vs_resistance,
    'Does the ''Iron Wall'' strategy of compelling acceptance through military force genuinely achieve long-term security and stability, or does it perpetually fuel resistance and instability?',
    'Longitudinal empirical studies comparing security outcomes in contexts where consent was compelled versus negotiated, and analysis of the causal links between military force and sustained resistance.',
    'If the ''Iron Wall'' is shown to perpetuate conflict, the constraint''s claimed coordination function (security) would be revealed as a cover for extraction, further solidifying its Snare classification and highlighting its self-defeating nature. If it demonstrably leads to stable, secure outcomes, it would challenge the Snare classification by showing a (coercive) coordination function, though still highly extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(iron_wall_efficacy_vs_resistance, empirical, 'Whether military force achieves its stated goal of security or perpetuates conflict.').

omega_variable(
    identity_lock_of_revisionist_zionism,
    'To what extent is the Revisionist Zionist movement''s commitment to maximalist territory an ''identity_locked'' position, where the self-concept of the movement is fused with the claim, making compromise unthinkable?',
    'Sociological and psychological studies of movement members, analysis of internal discourse, and observation of responses to external pressures or alternative proposals. If the movement consistently rejects pragmatic compromises even when faced with significant costs, it suggests a high degree of identity lock.',
    'If identity-locked, the constraint''s persistence is less about rational calculation of benefits and more about an irreducible ideological commitment, making resolution through conventional negotiation extremely difficult. This would amplify the ''trapped'' nature of the victims, as the agenda-setter''s position is non-negotiable by internal definition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(identity_lock_of_revisionist_zionism, conceptual, 'Degree to which the Revisionist Zionist movement is identity-locked to its maximalist territorial claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_territorial_claim__revisionist_zionism_reading, 1920, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1920, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 1920, 0.05).
narrative_ontology:measurement(jewi_tr_t1948, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 1948, 0.08).
narrative_ontology:measurement(jewi_tr_t1967, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 1967, 0.1).
narrative_ontology:measurement(jewi_tr_t1993, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 1993, 0.12).
narrative_ontology:measurement(jewi_tr_t2024, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1920, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 1920, 0.7).
narrative_ontology:measurement(jewi_be_t1948, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 1948, 0.85).
narrative_ontology:measurement(jewi_be_t1967, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 1967, 0.9).
narrative_ontology:measurement(jewi_be_t1993, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 1993, 0.88).
narrative_ontology:measurement(jewi_be_t2024, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 2024, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1920, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 1920, 0.6).
narrative_ontology:measurement(jewi_su_t1948, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 1948, 0.8).
narrative_ontology:measurement(jewi_su_t1967, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 1967, 0.85).
narrative_ontology:measurement(jewi_su_t1993, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 1993, 0.8).
narrative_ontology:measurement(jewi_su_t2024, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_territorial_claim__revisionist_zionism_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_territorial_claim__revisionist_zionism_reading, palestinian_right_of_return).
narrative_ontology:affects_constraint(jewish_territorial_claim__revisionist_zionism_reading, israeli_settlement_expansion).
narrative_ontology:affects_constraint(jewish_territorial_claim__revisionist_zionism_reading, gaza_blockade).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the broader 'jewish_territorial_claim' kernel. Its maximalist and coercive nature directly influences and often forecloses alternative, more moderate readings of the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
