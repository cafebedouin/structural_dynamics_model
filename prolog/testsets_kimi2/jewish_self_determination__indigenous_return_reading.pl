% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__indigenous_return_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_self_determination__indigenous_return_reading, []).

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
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: jewish_self_determination__indigenous_return_reading
 *   human_readable: Jewish Indigenous Return and Decolonization Claim
 *   domain: political/nationalism/postcolonial
 *
 * SUMMARY:
 *   This constraint instantiates the indigenous_return_reading of the
 *   jewish_self_determination kernel. It asserts that Jewish people are
 *   indigenous to the land with an unbroken historical connection, which
 *   morally and politically reframes Zionism as decolonization rather than
 *   colonization. The reading treats indigenous status as a binary,
 *   origin-based historical fact that naturally privileges Jewish territorial
 *   claims over Palestinian claims, which are reframed as later arrival or
 *   subordinate co-indigeneity. As a kernel reading, it competes with
 *   liberal-nationalist, religious-covenant, diasporist, and settler-colonial
 *   readings of the same underlying political question.
 *
 * KEY AGENTS:
 *   - Jewish claimants (beneficiary): Assert unbroken indigenous connection and claim territorial legitimacy through origin-based priority.
 *   - Zionist political institutions (agenda_setter): Administer and enforce the indigenous return narrative through state policy and diplomacy.
 *   - Palestinian claimants (excluded): Hold competing territorial claims; reframed within this reading as later arrivals or co-indigenous with subordinate status.
 *   - Postcolonial academics (observer): Contest the indigenous framing and classify Zionism as settler colonialism.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__indigenous_return_reading, 0.72).
domain_priors:suppression_score(jewish_self_determination__indigenous_return_reading, 0.65).
domain_priors:theater_ratio(jewish_self_determination__indigenous_return_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__indigenous_return_reading, mountain).
narrative_ontology:human_readable(jewish_self_determination__indigenous_return_reading, "Jewish Indigenous Return and Decolonization Claim").
narrative_ontology:topic_domain(jewish_self_determination__indigenous_return_reading, "political/nationalism/postcolonial").

domain_priors:emerges_naturally(jewish_self_determination__indigenous_return_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__indigenous_return_reading, 'a35e6d4c-19ba-40fd-be80-c0fcf1ff96eb').
narrative_ontology:cs_kernel_codification('a35e6d4c-19ba-40fd-be80-c0fcf1ff96eb', distributed).
narrative_ontology:cs_authority_grounding('a35e6d4c-19ba-40fd-be80-c0fcf1ff96eb', lineage).
narrative_ontology:cs_interpretation_layer_present('a35e6d4c-19ba-40fd-be80-c0fcf1ff96eb').
narrative_ontology:cs_reading_relation('a35e6d4c-19ba-40fd-be80-c0fcf1ff96eb', jewish_self_determination__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('a35e6d4c-19ba-40fd-be80-c0fcf1ff96eb', jewish_self_determination__settler_colonial_reading, forecloses).
narrative_ontology:cs_reading_relation('a35e6d4c-19ba-40fd-be80-c0fcf1ff96eb', jewish_self_determination__religious_covenant_reading, coexists_with).
narrative_ontology:cs_reading_relation('a35e6d4c-19ba-40fd-be80-c0fcf1ff96eb', jewish_self_determination__diasporist_reading, forecloses).
narrative_ontology:cs_axiom('a35e6d4c-19ba-40fd-be80-c0fcf1ff96eb', foundational, jewish_indigeneity_unbroken_origin_based).
narrative_ontology:cs_axiom_status(jewish_indigeneity_unbroken_origin_based, holdable).
narrative_ontology:cs_axiom_grounding('a35e6d4c-19ba-40fd-be80-c0fcf1ff96eb', jewish_indigeneity_unbroken_origin_based, empirically_contingent).
narrative_ontology:cs_axiom('a35e6d4c-19ba-40fd-be80-c0fcf1ff96eb', foundational, zionism_is_decolonization_not_colonization).
narrative_ontology:cs_axiom_status(zionism_is_decolonization_not_colonization, holdable).
narrative_ontology:cs_axiom_grounding('a35e6d4c-19ba-40fd-be80-c0fcf1ff96eb', zionism_is_decolonization_not_colonization, empirically_contingent).
narrative_ontology:cs_reference_frame('a35e6d4c-19ba-40fd-be80-c0fcf1ff96eb', continuous_indigenous_homeland_presence).
narrative_ontology:cs_drift_state('a35e6d4c-19ba-40fd-be80-c0fcf1ff96eb', contemporary_postcolonial_discourse, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('a35e6d4c-19ba-40fd-be80-c0fcf1ff96eb', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__indigenous_return_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__indigenous_return_reading, jewish_claimants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Assert ancestral indigenous status to the land based on historical origin and unbroken connection, claiming that return and self-determination constitute decolonization rather than colonization. The claim ties collective identity to territorial sovereignty and derives political legitimacy from historical priority.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, jewish_claimants, beneficiary,
    organized, generational, identity_locked, global).

% Formulate, propagate, and enforce the indigenous return narrative through state policy, education, diplomacy, and legal frameworks. They maintain archives, fund archaeological research, and present historical continuity as settled fact in international forums.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, zionist_political_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Hold competing territorial and national claims rooted in long-standing presence. Within this reading's framework they are reframed as later arrivals, co-indigenous with subordinate claim, or non-indigenous, and their objections are not admitted as primary to the legitimacy question.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, palestinian_claimants, excluded,
    organized, generational, trapped, regional).

% Analyze and contest the indigenous return framing, often classifying Zionism as a settler-colonial project. They dispute the unbroken-connection narrative and the decolonization classification through historical and critical-theoretical methods.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, postcolonial_academics, observer,
    institutional, generational, analytical, global).

narrative_ontology:fixing_cost_class(jewish_self_determination__indigenous_return_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves competing territorial claims by establishing a priority rule based on indigenous origin and historical continuity, coordinating Jewish collective self-determination around a single legitimating narrative.
% TRANSFER_FUNCTION: Transfers territorial legitimacy, international legal priority, and moral authority from competing claimants to Jewish claimants by framing Zionism as decolonization and return rather than as foreign colonization.
% ABSENT_VOICES: Palestinian claimants asserting primary indigenous status are reframed as later arrivals or co-indigenous with subordinate claim; anti-Zionist Jewish voices and diasporist scholars contesting the unbroken-connection narrative are excluded from the primary legitimacy discourse.
% DISAPPEARANCE_RATIONALE: For beneficiaries, the claim is the foundational legitimating framework for territorial sovereignty; its disappearance would force reliance on alternative justifications such as liberal-nationalist or religious-covenant readings, rearranging the moral architecture of the state. For observers and excluded parties, the material world would remain largely unchanged while discourse opened to alternative framings.
% FOUNDING_PROBLEM: The vulnerability of stateless Jewish minorities in Europe and the need for a collective national home, confronted by competing Arab and Palestinian presence in the same territory.
% FOUNDING_PROBLEM_CORROBORATION: No fully independent corroboration from outside the beneficiary set exists. Zionist institutions and Jewish claimant organizations attest the problem of statelessness and the indigenous solution; Palestinian claimants and postcolonial scholars attest the problem was formulated to justify territorial acquisition and demographic transformation.
narrative_ontology:disappearance_verdict(jewish_self_determination__indigenous_return_reading, contested).
narrative_ontology:founding_problem_status(jewish_self_determination__indigenous_return_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__indigenous_return_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jewish_self_determination__indigenous_return_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_self_determination__indigenous_return_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_self_determination__indigenous_return_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_self_determination__indigenous_return_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, ExtMetricName, E),
    domain_priors:suppression_score(jewish_self_determination__indigenous_return_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(jewish_self_determination__indigenous_return_reading),
    narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(jewish_self_determination__indigenous_return_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is claimed as mountain because the reading presents Jewish indigenous status as an unchangeable historical fact akin to natural law. However, the authored metrics diverge: extractiveness is high (0.72) because the claim actively subordinates competing Palestinian claims and extracts legitimacy from the discourse of indigenous rights; suppression is substantial (0.65) because the narrative's persistence requires active reframing and exclusion of competing claims; accessibility collapse is very high (0.88) because indigenous status is treated as binaryâonce accepted, alternatives collapse completely; resistance is moderate (0.55) due to sustained postcolonial and Palestinian counter-narratives. Theater ratio is low-moderate (0.25) because belief in the narrative is genuine, though performative diplomatic maintenance exists. The measurement series track rising extraction and suppression over the interval as the claim became more contested and required harder enforcement to maintain.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seat (Jewish claimants and Zionist institutions) experiences the constraint as an irreducible historical fact that legitimizes return and sovereignty. The excluded seat (Palestinian claimants) experiences the same constraint as a discursive mechanism that subordinates their presence and reframes their dispossession as secondary. The observer seat (postcolonial academics) sees a historical narrative functioning as political legitimation. The engine will compute different per-seat classifications from this same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish claimants are declared beneficiaries with identity_locked exit, placing their directionality near the full-beneficiary end (low d). Palestinian claimants are not declared victims in this reading's own framework, but their structural position as excluded and trapped implies high directionality; the omission from victims is itself a feature of the reading's reframing. Zionist institutions as agenda_setter sit near the beneficiary end with constrained exit. Postcolonial academics as analytical observers have analytical exit and neutral directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâJewish statelessness and minority vulnerabilityâwas live in the early twentieth century. The indigenous return claim was one solution. Over time, the claim has become foundational to territorial legitimacy and state identity, persisting well beyond the original problem's contours. The arrangement has not atrophied into pure performance (theater remains moderate), but its extraction component has risen as the claim works harder to exclude competing indigenous counter-claims. The R5 genealogy flags a possible mandatrophy: the problem the constraint was built to solve (statelessness) is either dead or transformed, yet the constraint persists because it has become constitutive of state legitimacy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_fact_vs_political_construct,
    'Is Jewish indigenous status with unbroken connection a discoverable historical fact independent of political framework, or a constructed narrative serving territorial claims?',
    'Interdisciplinary consensus from archaeology, historical linguistics, genetics, and critical historiography on continuity claims; genealogical analysis of when the indigenous framing entered Zionist political discourse.',
    'If the narrative is substantially constructed, the constraint is a false summit: a mountain claim overlaying extraction and subordination, triggering reclassification to tangled_rope or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_fact_vs_political_construct, conceptual, 'Natural-law versus constructed ambiguity for indigenous status').

omega_variable(
    contested_classification_ambiguity,
    'Does the contested classification of Zionism as decolonization versus colonization reflect an empirical dispute about history, or a normative dispute about political legitimacy?',
    'Disentangle empirical claims (archaeological continuity, demographic history, material practices) from normative claims (hierarchy of rights, indigenous priority, return as remedy).',
    'If primarily normative, the constraint''s empirically_contingent axioms serve deontological or instrumental political goals, raising effective extraction and undermining the mountain framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contested_classification_ambiguity, conceptual, 'Empirical versus normative ambiguity in decolonization classification').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__indigenous_return_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t0, jewish_self_determination__indigenous_return_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(jewi_tr_t20, jewish_self_determination__indigenous_return_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(jewi_tr_t40, jewish_self_determination__indigenous_return_reading, theater_ratio, 40, 0.18).
narrative_ontology:measurement(jewi_tr_t60, jewish_self_determination__indigenous_return_reading, theater_ratio, 60, 0.22).
narrative_ontology:measurement(jewi_tr_t80, jewish_self_determination__indigenous_return_reading, theater_ratio, 80, 0.24).
narrative_ontology:measurement(jewi_tr_t100, jewish_self_determination__indigenous_return_reading, theater_ratio, 100, 0.25).

% Extraction over time
narrative_ontology:measurement(jewi_be_t0, jewish_self_determination__indigenous_return_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(jewi_be_t20, jewish_self_determination__indigenous_return_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(jewi_be_t40, jewish_self_determination__indigenous_return_reading, base_extractiveness, 40, 0.55).
narrative_ontology:measurement(jewi_be_t60, jewish_self_determination__indigenous_return_reading, base_extractiveness, 60, 0.62).
narrative_ontology:measurement(jewi_be_t80, jewish_self_determination__indigenous_return_reading, base_extractiveness, 80, 0.68).
narrative_ontology:measurement(jewi_be_t100, jewish_self_determination__indigenous_return_reading, base_extractiveness, 100, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t0, jewish_self_determination__indigenous_return_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(jewi_su_t20, jewish_self_determination__indigenous_return_reading, suppression_requirement, 20, 0.35).
narrative_ontology:measurement(jewi_su_t40, jewish_self_determination__indigenous_return_reading, suppression_requirement, 40, 0.45).
narrative_ontology:measurement(jewi_su_t60, jewish_self_determination__indigenous_return_reading, suppression_requirement, 60, 0.55).
narrative_ontology:measurement(jewi_su_t80, jewish_self_determination__indigenous_return_reading, suppression_requirement, 80, 0.6).
narrative_ontology:measurement(jewi_su_t100, jewish_self_determination__indigenous_return_reading, suppression_requirement, 100, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__indigenous_return_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_self_determination__indigenous_return_reading, liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__indigenous_return_reading, settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_self_determination__indigenous_return_reading, religious_covenant_reading).
narrative_ontology:affects_constraint(jewish_self_determination__indigenous_return_reading, diasporist_reading).

% DUAL FORMULATION NOTE:
% The jewish_self_determination kernel decomposes into multiple constraint stories because the natural-language label conflates structurally distinct claims: indigenous return, liberal nationalism, religious covenant, diasporist rejection, and settler-colonial critique. Each reading has a different epsilon, beneficiary structure, and classification. They are linked as a constraint family via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
