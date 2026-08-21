% ============================================================================
% CONSTRAINT STORY: territorial_sovereignty_legitimacy__self_determination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_sovereignty_legitimacy__self_determination_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: territorial_sovereignty_legitimacy__self_determination_reading
 *   human_readable: Territorial Sovereignty Legitimacy (Self-Determination Reading)
 *   domain: political_theory/international_relations/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint story instantiates the 'self_determination_reading' of
 *   the 'territorial_sovereignty_legitimacy' kernel. It asserts that
 *   legitimate sovereignty in the territory derives from the modern principle
 *   of self-determination, applied to the Arab population based on their
 *   demographic majority and continuous residence during the 19th-20th
 *   centuries. This reading frames the current state of affairs as a denial
 *   of this right, leading to high extraction and suppression for the Arab
 *   population. The Israeli state is viewed as an external imposition, and
 *   the right of return is seen as a restoration of the status quo ante.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__self_determination_reading, 0.85).
domain_priors:suppression_score(territorial_sovereignty_legitimacy__self_determination_reading, 0.9).
domain_priors:theater_ratio(territorial_sovereignty_legitimacy__self_determination_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, resistance, 0.95).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_sovereignty_legitimacy__self_determination_reading, snare).
narrative_ontology:human_readable(territorial_sovereignty_legitimacy__self_determination_reading, "Territorial Sovereignty Legitimacy (Self-Determination Reading)").
narrative_ontology:topic_domain(territorial_sovereignty_legitimacy__self_determination_reading, "political_theory/international_relations/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_sovereignty_legitimacy__self_determination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_sovereignty_legitimacy__self_determination_reading, 'd6e9e219-3fe4-4a1a-92b9-3f1c7d195538').
narrative_ontology:cs_kernel_codification('d6e9e219-3fe4-4a1a-92b9-3f1c7d195538', formalized).
narrative_ontology:cs_authority_grounding('d6e9e219-3fe4-4a1a-92b9-3f1c7d195538', lineage).
narrative_ontology:cs_interpretation_layer_present('d6e9e219-3fe4-4a1a-92b9-3f1c7d195538').
narrative_ontology:cs_reading_relation('d6e9e219-3fe4-4a1a-92b9-3f1c7d195538', territorial_sovereignty_legitimacy__covenant_continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('d6e9e219-3fe4-4a1a-92b9-3f1c7d195538', territorial_sovereignty_legitimacy__existential_matrix_reading, coexists_with).
narrative_ontology:cs_axiom('d6e9e219-3fe4-4a1a-92b9-3f1c7d195538', foundational, self_determination_is_universal_right).
narrative_ontology:cs_axiom_status(self_determination_is_universal_right, holdable).
narrative_ontology:cs_axiom_grounding('d6e9e219-3fe4-4a1a-92b9-3f1c7d195538', self_determination_is_universal_right, deontological).
narrative_ontology:cs_axiom('d6e9e219-3fe4-4a1a-92b9-3f1c7d195538', foundational, modern_demographic_majority_confers_sovereignty).
narrative_ontology:cs_axiom_status(modern_demographic_majority_confers_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('d6e9e219-3fe4-4a1a-92b9-3f1c7d195538', modern_demographic_majority_confers_sovereignty, empirically_contingent).
narrative_ontology:cs_reference_frame('d6e9e219-3fe4-4a1a-92b9-3f1c7d195538', post_ww1_self_determination_era).
narrative_ontology:cs_drift_state('d6e9e219-3fe4-4a1a-92b9-3f1c7d195538', contemporary_geopolitical_realities, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('d6e9e219-3fe4-4a1a-92b9-3f1c7d195538', '').
narrative_ontology:cs_kernel_id(territorial_sovereignty_legitimacy__self_determination_reading, territorial_sovereignty_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__self_determination_reading, arab_population_advocates).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__self_determination_reading, international_law_scholars_self_determination).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__self_determination_reading, arab_population).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The population whose right to self-determination and continuous residence in the territory is asserted by this reading. They bear the direct costs of the denial of this legitimacy claim, including displacement, statelessness, and lack of sovereign control.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, arab_population, payer,
    powerless, generational, trapped, regional).

% Political movements, NGOs, and states that champion this reading. They gain legitimacy, political capital, and moral authority by advocating for the self-determination of the Arab population based on modern demographic principles.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, arab_population_advocates, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(territorial_sovereignty_legitimacy__self_determination_reading, arab_population_advocates, agenda_setter).

% The existing state in the territory, whose legitimacy is challenged by this reading. It actively enforces its own sovereignty claims, which this reading frames as a suppression of the self-determination of the Arab population. It bears the costs of international resistance and internal dissent related to this challenge.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, israeli_state, agenda_setter,
    institutional, generational, constrained, national).

% Academics and legal experts who interpret international law, often affirming the principle of self-determination as applied to indigenous or majority populations in a modern context. Their work provides intellectual grounding for this reading, enhancing their professional standing and influence.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, international_law_scholars_self_determination, observer,
    analytical, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(territorial_sovereignty_legitimacy__self_determination_reading, international_law_scholars_self_determination, beneficiary).

% Those who advocate for the legitimacy of the Israeli state based on historical, religious, and existential claims. They are structurally excluded from the legitimating framework of this 'self-determination' reading, as its premises directly contradict their own.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, zionist_movement_advocates, excluded,
    powerful, generational, identity_locked, global).

% International organizations tasked with upholding international law and promoting self-determination. They often issue resolutions and reports that align with aspects of this reading, but their enforcement power is limited by member state sovereignty and geopolitical realities.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, un_bodies, observer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(territorial_sovereignty_legitimacy__self_determination_reading, un_bodies, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_sovereignty_legitimacy__self_determination_reading, arab_population_advocates).
narrative_ontology:fixing_cost_class(territorial_sovereignty_legitimacy__self_determination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for adjudicating territorial sovereignty claims based on modern, secular principles of population self-determination, aiming to coordinate international recognition and state formation around these criteria.
% TRANSFER_FUNCTION: Transfers the moral and legal authority for territorial control from historical or religious claims to the demographic majority with continuous modern residence, thereby legitimizing the claims of the Arab population and delegitimizing the existing Israeli state.
% ABSENT_VOICES: Advocates for the Israeli state's legitimacy based on ancient covenant, historical Jewish presence, or existential necessity are structurally excluded from this reading's framework; they would argue for alternative legitimating principles.
% DISAPPEARANCE_RATIONALE: If this reading of sovereignty legitimacy vanished, the international legal and political discourse surrounding the Israeli-Palestinian conflict would fundamentally shift. The primary moral and legal arguments for Palestinian statehood based on self-determination would be severely weakened, leading to a re-evaluation of international support and a different geopolitical landscape.
% FOUNDING_PROBLEM: The problem of establishing legitimate statehood and territorial boundaries in the post-colonial era, particularly in regions with diverse historical claims and populations, aiming to prevent arbitrary colonial partitions and ensure indigenous rights.
% FOUNDING_PROBLEM_CORROBORATION: International legal scholars, UN resolutions, and historical records of decolonization movements corroborate the founding problem and its ongoing relevance. The continued statelessness and displacement of the Arab population further attest to its live status, as do ongoing international diplomatic efforts.
narrative_ontology:disappearance_verdict(territorial_sovereignty_legitimacy__self_determination_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_sovereignty_legitimacy__self_determination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_sovereignty_legitimacy__self_determination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(territorial_sovereignty_legitimacy__self_determination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_sovereignty_legitimacy__self_determination_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_sovereignty_legitimacy__self_determination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__self_determination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_sovereignty_legitimacy__self_determination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.85) because this reading views the existing territorial control as fundamentally illegitimate and extractive from the Arab population. Suppression is also very high (0.90) as the current state is maintained through active enforcement against the claims of self-determination. Theater ratio is low (0.10) because this reading is a deeply held, actively asserted political and legal claim, not a performative one. Resistance is very high (0.95) due to the ongoing conflict and active opposition to the existing arrangements. Accessibility collapse is high (0.75) because the alternative (a state based on this self-determination principle) is actively suppressed.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Arab population and its advocates, this constraint is a snare, representing profound extraction and suppression. From the perspective of the Israeli state and its advocates, this reading itself is a threat to their existential claims, and its enforcement would be highly extractive from them. The engine's per-seat classification will capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The Arab population is the primary target, bearing the costs of denied self-determination. Advocates for this reading (Arab population advocates, international law scholars) are beneficiaries, gaining moral and political leverage. The Israeli state, while an agenda-setter for its own legitimacy, is a 'payer' in the context of this reading, as it bears the costs of resisting this alternative framework. Zionist movement advocates are excluded from this framework's legitimating logic.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'How does this ''self_determination_reading'' structurally differ from its sibling readings of the ''territorial_sovereignty_legitimacy'' kernel?',
    'Comparative analysis of foundational axioms and their implications for territorial claims, victim sets, and temporal scope across all readings.',
    'Clarifies the specific structural contribution of this reading to the overall contest over sovereignty legitimacy. If the structural differences are less pronounced than claimed, it might suggest a single, more complex constraint rather than distinct readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Distinguishes this reading from its siblings within the kernel.').

omega_variable(
    definition_of_modern_period,
    'What specific historical period constitutes the ''modern period'' for establishing demographic majority and continuous residence, and how is this period delimited?',
    'Historical consensus among demographers and historians regarding the onset of modern national consciousness and population movements in the region, or explicit legal definition in international instruments.',
    'A narrower or later definition of the ''modern period'' could weaken the demographic claims of the Arab population, while a broader or earlier definition could strengthen them, altering the perceived legitimacy of the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definition_of_modern_period, empirical, 'Ambiguity in the temporal scope of ''modern period''.').

omega_variable(
    continuous_residence_vs_displacement,
    'How is ''continuous residence'' defined in the context of historical displacement and forced migration, and does it account for the right of return?',
    'Legal interpretation by international courts or expert bodies on the status of displaced populations and the application of ''continuous residence'' criteria in post-conflict or colonial contexts.',
    'A strict interpretation of ''continuous residence'' that disregards forced displacement would weaken the claims of the Arab population, while an interpretation that includes the right of return would strengthen them, impacting the perceived victim set and extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(continuous_residence_vs_displacement, conceptual, 'Ambiguity in defining ''continuous residence'' amidst historical displacement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_sovereignty_legitimacy__self_determination_reading, 1918, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1918, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 1918, 0.15).
narrative_ontology:measurement(terr_tr_t1948, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 1948, 0.12).
narrative_ontology:measurement(terr_tr_t1967, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 1967, 0.1).
narrative_ontology:measurement(terr_tr_t1993, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 1993, 0.1).
narrative_ontology:measurement(terr_tr_t2005, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 2005, 0.1).
narrative_ontology:measurement(terr_tr_t2024, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(terr_be_t1918, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 1918, 0.6).
narrative_ontology:measurement(terr_be_t1948, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 1948, 0.75).
narrative_ontology:measurement(terr_be_t1967, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 1967, 0.8).
narrative_ontology:measurement(terr_be_t1993, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 1993, 0.82).
narrative_ontology:measurement(terr_be_t2005, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 2005, 0.84).
narrative_ontology:measurement(terr_be_t2024, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1918, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 1918, 0.65).
narrative_ontology:measurement(terr_su_t1948, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 1948, 0.8).
narrative_ontology:measurement(terr_su_t1967, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 1967, 0.85).
narrative_ontology:measurement(terr_su_t1993, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 1993, 0.88).
narrative_ontology:measurement(terr_su_t2005, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 2005, 0.89).
narrative_ontology:measurement(terr_su_t2024, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_sovereignty_legitimacy__self_determination_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
