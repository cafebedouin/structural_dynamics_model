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
 *   human_readable: Revisionist Zionist Maximalist Territorial Claim ('Iron Wall')
 *   domain: political_history/settler_colonialism/nationalism_studies
 *
 * SUMMARY:
 *   This constraint represents the Revisionist Zionist reading of the Jewish
 *   territorial claim, advocating for a maximalist state on both banks of the
 *   Jordan River, achieved through military force and without requiring Arab
 *   consent (the 'Iron Wall' doctrine). It is a highly extractive and
 *   suppressive constraint, actively enforced against the indigenous
 *   population and neighboring states. This story focuses solely on this
 *   specific reading, treating it as a distinct constraint with its own
 *   structural properties, independent of other Zionist interpretations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_territorial_claim__revisionist_zionism_reading, 0.95).
domain_priors:suppression_score(jewish_territorial_claim__revisionist_zionism_reading, 0.98).
domain_priors:theater_ratio(jewish_territorial_claim__revisionist_zionism_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, extractiveness, 0.95).
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 0.98).
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_territorial_claim__revisionist_zionism_reading, snare).
narrative_ontology:human_readable(jewish_territorial_claim__revisionist_zionism_reading, "Revisionist Zionist Maximalist Territorial Claim ('Iron Wall')").
narrative_ontology:topic_domain(jewish_territorial_claim__revisionist_zionism_reading, "political_history/settler_colonialism/nationalism_studies").

domain_priors:requires_active_enforcement(jewish_territorial_claim__revisionist_zionism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_territorial_claim__revisionist_zionism_reading, '718b5760-7b07-458b-99ea-ae745e23f00a').
narrative_ontology:cs_kernel_codification('718b5760-7b07-458b-99ea-ae745e23f00a', formalized).
narrative_ontology:cs_authority_grounding('718b5760-7b07-458b-99ea-ae745e23f00a', lineage).
narrative_ontology:cs_interpretation_layer_present('718b5760-7b07-458b-99ea-ae745e23f00a').
narrative_ontology:cs_reading_relation('718b5760-7b07-458b-99ea-ae745e23f00a', jewish_territorial_claim__political_zionism_reading, influences).
narrative_ontology:cs_reading_relation('718b5760-7b07-458b-99ea-ae745e23f00a', jewish_territorial_claim__labor_zionism_reading, influences).
narrative_ontology:cs_reading_relation('718b5760-7b07-458b-99ea-ae745e23f00a', jewish_territorial_claim__cultural_zionism_reading, coexists_with).
narrative_ontology:cs_axiom('718b5760-7b07-458b-99ea-ae745e23f00a', foundational, land_of_israel_whole_and_undivided).
narrative_ontology:cs_axiom_status(land_of_israel_whole_and_undivided, holdable).
narrative_ontology:cs_axiom_grounding('718b5760-7b07-458b-99ea-ae745e23f00a', land_of_israel_whole_and_undivided, theological).
narrative_ontology:cs_axiom('718b5760-7b07-458b-99ea-ae745e23f00a', foundational, arab_consent_irrelevant_to_sovereignty).
narrative_ontology:cs_axiom_status(arab_consent_irrelevant_to_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('718b5760-7b07-458b-99ea-ae745e23f00a', arab_consent_irrelevant_to_sovereignty, conventional).
narrative_ontology:cs_reference_frame('718b5760-7b07-458b-99ea-ae745e23f00a', maximalist_sovereignty_by_force).
narrative_ontology:cs_drift_state('718b5760-7b07-458b-99ea-ae745e23f00a', contemporary_international_law_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('718b5760-7b07-458b-99ea-ae745e23f00a', '').
narrative_ontology:cs_kernel_id(jewish_territorial_claim__revisionist_zionism_reading, jewish_territorial_claim).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__revisionist_zionism_reading, revisionist_zionist_movement).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__revisionist_zionism_reading, jewish_settlers).
narrative_ontology:constraint_victim(jewish_territorial_claim__revisionist_zionism_reading, palestinian_arabs).
narrative_ontology:constraint_victim(jewish_territorial_claim__revisionist_zionism_reading, neighboring_arab_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocates for and actively pursues the establishment of a Jewish state on both banks of the Jordan River, viewing this as an inalienable right. They believe Arab consent is irrelevant and that military force is necessary to compel acceptance of this reality. They benefit from the expansion of territory and the consolidation of power.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, revisionist_zionist_movement, agenda_setter,
    institutional, generational, identity_locked, regional).

% Directly benefit from the territorial expansion and the establishment of new settlements, receiving land, resources, and security guarantees under the protection of the revisionist agenda. Their identity and future are often tied to the success of this maximalist claim.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, jewish_settlers, beneficiary,
    organized, biographical, constrained, local).

% Bear the brunt of the territorial claim through displacement, loss of land, restrictions on movement, and military occupation. Their national aspirations and self-determination are directly suppressed by this constraint. Exit options are severely limited, often to forced migration or resistance.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, palestinian_arabs, payer,
    powerless, generational, trapped, local).

% Experience geopolitical instability, refugee crises, and military confrontations as a direct consequence of the maximalist territorial claim and the 'Iron Wall' policy. They are compelled to react to the imposed reality, often at significant economic and human cost.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, neighboring_arab_states, payer,
    powerful, generational, constrained, regional).

% Observes and often condemns the maximalist claims and the use of force, but its ability to alter the constraint is limited by geopolitical realities and the resolve of the revisionist movement. It attempts to mediate or impose sanctions, often with mixed results.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, international_community, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the actions and aspirations of the revisionist Zionist movement and its supporters towards a unified, maximalist territorial goal, providing a clear ideological and strategic framework for expansion and defense.
% TRANSFER_FUNCTION: Transfers land, resources, and sovereignty from Palestinian Arabs and neighboring states to the Jewish state, secured through military and political force.
% ABSENT_VOICES: Palestinian voices advocating for self-determination, land rights, and a non-militarized resolution are systematically excluded from the decision-making processes that define this constraint. Their perspectives are actively suppressed by the 'Iron Wall' doctrine.
% DISAPPEARANCE_RATIONALE: If this maximalist claim and its enforcement vanished, the entire geopolitical landscape of the Middle East would fundamentally shift. Palestinian national aspirations would immediately re-emerge as a primary political force, land ownership and borders would be contested, and the regional power balance would be dramatically altered, leading to a complete reorganization of political and social structures.
% FOUNDING_PROBLEM: The perceived existential threat to Jewish people globally, the historical connection to the land of Israel, and the failure of other Zionist approaches to secure a viable, defensible Jewish state.
% FOUNDING_PROBLEM_CORROBORATION: The revisionist Zionist movement and its supporters attest that the founding problem of Jewish insecurity and the need for a strong, sovereign state remains live. Critics, including Palestinian historians and international legal scholars, argue that while the initial problem may have been real, the maximalist solution has created new, equally severe problems for others, and that the 'live' status is maintained to justify ongoing expansion.
narrative_ontology:disappearance_verdict(jewish_territorial_claim__revisionist_zionism_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_territorial_claim__revisionist_zionism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_territorial_claim__revisionist_zionism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is very high (0.95) because the claim involves the direct appropriation of land and resources from a non-consenting population. Suppression is near maximal (0.98) due to the explicit reliance on military force and the rejection of Arab political agency. Theater ratio is low (0.1) because the constraint is primarily functional in its coercive intent, with little performative cover for its core operations. Resistance is high (0.85) reflecting ongoing Palestinian and Arab opposition.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Revisionist Zionist movement, this is a necessary and just assertion of national rights, a 'rope' for Jewish self-determination. From the perspective of Palestinian Arabs, it is a 'snare' of dispossession and occupation. The engine's classification will reflect the latter due to the high extraction and suppression, despite the internal 'rope' framing of its proponents.
 *
 * DIRECTIONALITY LOGIC:
 *   The Revisionist Zionist Movement and Jewish settlers are clear beneficiaries, gaining territory and political power. Palestinian Arabs and neighboring Arab states are the primary victims, experiencing displacement, loss of sovereignty, and military pressure. The international community acts as an observer, often condemning but rarely able to decisively alter the constraint's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (securing a Jewish state) is still considered 'live' by its proponents, but its methods and maximalist scope are highly contested. The high extractiveness and suppression, coupled with ongoing resistance, prevent it from being mislabeled as a coordination mechanism. The 'Iron Wall' doctrine explicitly rejects the need for coordination with the affected population, making it a clear case of pure extraction and suppression.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    arab_consent_necessity,
    'Is Arab consent a necessary prerequisite for the legitimacy and long-term stability of a Jewish state in the region, or can it be compelled by force?',
    'Empirical observation of long-term stability and security outcomes in the absence of consent, compared to scenarios where consent is achieved. Historical analysis of other settler-colonial projects.',
    'If consent is necessary, the ''Iron Wall'' strategy is inherently unstable and unsustainable, leading to reclassification towards a more fragile or self-defeating constraint. If consent can be compelled, the constraint''s long-term viability is higher, though its ethical status remains contested.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(arab_consent_necessity, empirical, 'Whether the ''Iron Wall'' doctrine of compelling acceptance is viable for long-term stability.').

omega_variable(
    territorial_maximalism_justification,
    'Is the maximalist territorial claim (both banks of Jordan) a historical right, a strategic necessity for security, or an expansionist political agenda?',
    'Historical and legal scholarship on land claims, geopolitical analysis of security needs, and critical discourse analysis of revisionist Zionist rhetoric.',
    'If primarily an expansionist agenda, the constraint''s extractiveness is further amplified, and its justification as a ''right'' is undermined. If a genuine historical right or security necessity, the ethical calculus shifts, though the impact on victims remains.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(territorial_maximalism_justification, conceptual, 'The underlying justification for the maximalist territorial claim.').

omega_variable(
    identity_lock_sustainability,
    'Is the identity-locked commitment of the revisionist movement sustainable in the face of persistent resistance and international pressure, or will it eventually lead to internal fragmentation or external collapse?',
    'Longitudinal study of internal cohesion within the movement, analysis of demographic trends, and the impact of sustained international sanctions or legal challenges.',
    'If unsustainable, the constraint''s long-term persistence is lower, and its current high suppression may be a symptom of increasing internal fragility. If sustainable, the constraint is more robust than external observers might assume.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_sustainability, empirical, 'Sustainability of the revisionist movement''s identity-locked commitment.').


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
narrative_ontology:measurement(jewi_tr_t2000, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(jewi_tr_t2024, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1920, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 1920, 0.7).
narrative_ontology:measurement(jewi_be_t1948, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 1948, 0.85).
narrative_ontology:measurement(jewi_be_t1967, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 1967, 0.9).
narrative_ontology:measurement(jewi_be_t1993, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 1993, 0.88).
narrative_ontology:measurement(jewi_be_t2000, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 2000, 0.92).
narrative_ontology:measurement(jewi_be_t2024, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 2024, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1920, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 1920, 0.6).
narrative_ontology:measurement(jewi_su_t1948, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 1948, 0.8).
narrative_ontology:measurement(jewi_su_t1967, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 1967, 0.9).
narrative_ontology:measurement(jewi_su_t1993, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 1993, 0.85).
narrative_ontology:measurement(jewi_su_t2000, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 2000, 0.92).
narrative_ontology:measurement(jewi_su_t2024, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 2024, 0.98).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_territorial_claim__revisionist_zionism_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_territorial_claim__revisionist_zionism_reading, jewish_territorial_claim__political_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__revisionist_zionism_reading, jewish_territorial_claim__labor_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__revisionist_zionism_reading, jewish_territorial_claim__cultural_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__revisionist_zionism_reading, palestinian_right_of_return).
narrative_ontology:affects_constraint(jewish_territorial_claim__revisionist_zionism_reading, arab_israeli_conflict_dynamics).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'jewish_territorial_claim' kernel. Its maximalist and coercive nature significantly influences other readings and related regional constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
