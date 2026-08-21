% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__religious_covenant_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_self_determination__religious_covenant_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: jewish_self_determination__religious_covenant_reading
 *   human_readable: Jewish Self-Determination: Religious Covenant Reading
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   This constraint models the 'religious covenant' reading of Jewish
 *   self-determination, where the claim to the land is seen as a divine,
 *   immutable obligation. While claimed as a Mountain (divine command), its
 *   operationalization within a state framework makes it function as a
 *   Tangled Rope, extracting from secular political processes and Palestinian
 *   populations. The high extractiveness and suppression reflect the absolute
 *   nature of the religious claim when applied to territorial control,
 *   effectively foreclosing secular compromise. The 'emerges_naturally: true'
 *   reflects the internal logic of the religious claim, not an external
 *   empirical assessment.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__religious_covenant_reading, 0.85).
domain_priors:suppression_score(jewish_self_determination__religious_covenant_reading, 0.9).
domain_priors:theater_ratio(jewish_self_determination__religious_covenant_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__religious_covenant_reading, mountain).
narrative_ontology:human_readable(jewish_self_determination__religious_covenant_reading, "Jewish Self-Determination: Religious Covenant Reading").
narrative_ontology:topic_domain(jewish_self_determination__religious_covenant_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

domain_priors:requires_active_enforcement(jewish_self_determination__religious_covenant_reading).
domain_priors:emerges_naturally(jewish_self_determination__religious_covenant_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__religious_covenant_reading, '995ab54e-bdce-46d0-a671-129a6b95325f').
narrative_ontology:cs_kernel_codification('995ab54e-bdce-46d0-a671-129a6b95325f', fixed_text).
narrative_ontology:cs_authority_grounding('995ab54e-bdce-46d0-a671-129a6b95325f', lineage).
narrative_ontology:cs_interpretation_layer_present('995ab54e-bdce-46d0-a671-129a6b95325f').
narrative_ontology:cs_reading_relation('995ab54e-bdce-46d0-a671-129a6b95325f', jewish_self_determination__liberal_nationalist_reading, influences).
narrative_ontology:cs_reading_relation('995ab54e-bdce-46d0-a671-129a6b95325f', jewish_self_determination__indigenous_return_reading, forecloses).
narrative_ontology:cs_reading_relation('995ab54e-bdce-46d0-a671-129a6b95325f', jewish_self_determination__settler_colonial_reading, forecloses).
narrative_ontology:cs_reading_relation('995ab54e-bdce-46d0-a671-129a6b95325f', jewish_self_determination__diasporist_reading, forecloses).
narrative_ontology:cs_axiom('995ab54e-bdce-46d0-a671-129a6b95325f', foundational, divine_covenant_absolute_land_claim).
narrative_ontology:cs_axiom_status(divine_covenant_absolute_land_claim, holdable).
narrative_ontology:cs_axiom_grounding('995ab54e-bdce-46d0-a671-129a6b95325f', divine_covenant_absolute_land_claim, theological).
narrative_ontology:cs_axiom('995ab54e-bdce-46d0-a671-129a6b95325f', secondary, halakhic_sovereignty_over_all_eretz_yisrael).
narrative_ontology:cs_axiom_status(halakhic_sovereignty_over_all_eretz_yisrael, holdable).
narrative_ontology:cs_axiom_grounding('995ab54e-bdce-46d0-a671-129a6b95325f', halakhic_sovereignty_over_all_eretz_yisrael, conventional).
narrative_ontology:cs_reference_frame('995ab54e-bdce-46d0-a671-129a6b95325f', biblical_divine_mandate).
narrative_ontology:cs_drift_state('995ab54e-bdce-46d0-a671-129a6b95325f', contemporary_political_reality, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('995ab54e-bdce-46d0-a671-129a6b95325f', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__religious_covenant_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__religious_covenant_reading, religious_zionist_movement).
narrative_ontology:constraint_beneficiary(jewish_self_determination__religious_covenant_reading, settlement_enterprise).
narrative_ontology:constraint_victim(jewish_self_determination__religious_covenant_reading, secular_political_frameworks).
narrative_ontology:constraint_victim(jewish_self_determination__religious_covenant_reading, palestinian_negotiators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the divine covenant as a mandate for Jewish sovereignty over the entire land, actively shaping policy and promoting settlement. Their identity is fused with this interpretation, making compromise unthinkable.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, religious_zionist_movement, agenda_setter,
    institutional, generational, identity_locked, national).

% Directly benefits from policies driven by the religious covenant reading, receiving state support and legal protection for expansion. Their existence is justified by the religious claim.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, settlement_enterprise, beneficiary,
    organized, generational, constrained, local).

% Are undermined by the religious covenant reading, which asserts a higher authority than state law or international diplomacy. They bear the cost of reduced flexibility in territorial negotiations and international isolation.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, secular_political_frameworks, payer,
    institutional, biographical, constrained, national).

% Are directly targeted by the religious covenant reading, which forecloses their claims to self-determination and territorial rights based on secular or historical grounds. They are trapped by a framework that denies their legitimacy.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, palestinian_negotiators, payer,
    powerless, generational, trapped, regional).

% Observes the conflict, attempting to mediate based on international law and secular principles. Their efforts are consistently frustrated by the religious covenant reading's rejection of these frameworks.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, international_diplomatic_community, observer,
    institutional, biographical, analytical, global).

% Often reject the religious covenant reading as a basis for territorial claims, advocating for Jewish flourishing in diaspora and universal human rights. Their perspective is marginalized within the dominant discourse of the religious Zionist movement.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, diasporist_jewish_communities, excluded,
    moderate, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: For adherents, it coordinates a collective identity and purpose around a divinely ordained mission, providing a clear, immutable framework for territorial claims and national aspirations.
% TRANSFER_FUNCTION: Transfers ultimate authority over land claims from secular political processes to religious interpretation, effectively transferring territorial control and resources to those who adhere to the religious covenant reading.
% ABSENT_VOICES: Secular Jewish voices, Palestinian indigenous rights advocates, and international legal bodies are often dismissed or excluded from the conversation, as their frameworks are deemed irrelevant or subordinate to divine command.
% DISAPPEARANCE_RATIONALE: If the belief in a divine covenant as the basis for territorial sovereignty vanished, the entire ideological foundation for the religious Zionist movement and settlement enterprise would collapse, forcing a radical re-evaluation of land claims and political strategy within a secular framework.
% FOUNDING_PROBLEM: The historical problem of Jewish statelessness and persecution, and the theological imperative to return to the biblical land of Israel.
% FOUNDING_PROBLEM_CORROBORATION: Adherents of the religious covenant reading attest that the theological imperative remains live and absolute. Critics, including some secular Jewish scholars and international legal experts, acknowledge the historical context of statelessness but dispute the theological interpretation as a basis for modern state policy, arguing it has been instrumentalized.
narrative_ontology:disappearance_verdict(jewish_self_determination__religious_covenant_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_self_determination__religious_covenant_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__religious_covenant_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(jewish_self_determination__religious_covenant_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_self_determination__religious_covenant_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_self_determination__religious_covenant_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_self_determination__religious_covenant_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, ExtMetricName, E),
    domain_priors:suppression_score(jewish_self_determination__religious_covenant_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(jewish_self_determination__religious_covenant_reading),
    narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(jewish_self_determination__religious_covenant_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is high because the religious claim, when enforced by state power, overrides and extracts from alternative secular frameworks for land allocation and negotiation. Suppression is very high as any challenge to the divine mandate is met with strong ideological and often physical resistance. Theater ratio is low because the religious conviction is genuine for its adherents, and the enforcement is direct, not performative. The claimed type 'mountain' reflects the internal, immutable nature of divine command, but its interaction with political power means its effective classification for external observers will be more extractive.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the religious Zionist movement, this is an immutable divine command (Mountain) with zero extraction, as it is simply fulfilling a religious obligation. From the perspective of secular political actors or Palestinian negotiators, it is a highly extractive and suppressive force (Snare/Tangled Rope) that forecloses legitimate claims. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The religious Zionist movement and settlement enterprise are clear beneficiaries, as the constraint provides the ultimate justification for their actions and existence. Secular political frameworks and Palestinian negotiators are victims, as their claims are systematically undermined or rejected by the religious mandate. The international diplomatic community is an observer, attempting to apply secular norms to a religiously framed conflict.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_vs_political_authority,
    'Is the claim to the land genuinely derived from an immutable divine covenant, or is the divine framing an instrumental justification for political and territorial expansion?',
    'Analysis of historical shifts in interpretation, the selective application of religious texts, and the correlation between religious claims and geopolitical objectives. If the religious interpretation consistently aligns with political expansion, it suggests instrumentalization.',
    'If instrumental, the ''emerges_naturally'' property would be false, reclassifying the constraint from a Mountain (even a false summit) to a pure Snare, as its primary function would be extraction under religious cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_vs_political_authority, conceptual, 'Ambiguity between genuine religious conviction and political instrumentalization of divine command.').

omega_variable(
    foreclosure_of_secular_negotiation,
    'To what extent does the religious covenant reading genuinely foreclose secular political negotiation, versus merely raising its costs?',
    'Empirical analysis of negotiation outcomes in contexts where the religious claim is dominant. If no compromise on territorial division is ever achieved, it suggests foreclosure. If compromises are made, but at extreme cost to the secular party, it suggests high costs rather than absolute foreclosure.',
    'If it genuinely forecloses, the constraint''s suppression and extractiveness are absolute within its domain. If it only raises costs, there is a theoretical (though perhaps practically impossible) path to resolution, making the constraint less absolute.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(foreclosure_of_secular_negotiation, empirical, 'Whether the religious claim creates an absolute barrier or merely a high-cost barrier to secular negotiation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__religious_covenant_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1948, jewish_self_determination__religious_covenant_reading, theater_ratio, 1948, 0.05).
narrative_ontology:measurement(jewi_tr_t1967, jewish_self_determination__religious_covenant_reading, theater_ratio, 1967, 0.08).
narrative_ontology:measurement(jewi_tr_t1993, jewish_self_determination__religious_covenant_reading, theater_ratio, 1993, 0.1).
narrative_ontology:measurement(jewi_tr_t2024, jewish_self_determination__religious_covenant_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1948, jewish_self_determination__religious_covenant_reading, base_extractiveness, 1948, 0.6).
narrative_ontology:measurement(jewi_be_t1967, jewish_self_determination__religious_covenant_reading, base_extractiveness, 1967, 0.75).
narrative_ontology:measurement(jewi_be_t1993, jewish_self_determination__religious_covenant_reading, base_extractiveness, 1993, 0.8).
narrative_ontology:measurement(jewi_be_t2024, jewish_self_determination__religious_covenant_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1948, jewish_self_determination__religious_covenant_reading, suppression_requirement, 1948, 0.7).
narrative_ontology:measurement(jewi_su_t1967, jewish_self_determination__religious_covenant_reading, suppression_requirement, 1967, 0.8).
narrative_ontology:measurement(jewi_su_t1993, jewish_self_determination__religious_covenant_reading, suppression_requirement, 1993, 0.85).
narrative_ontology:measurement(jewi_su_t2024, jewish_self_determination__religious_covenant_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__religious_covenant_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jewish_self_determination__religious_covenant_reading, 0.08).
narrative_ontology:affects_constraint(jewish_self_determination__religious_covenant_reading, jewish_self_determination__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__religious_covenant_reading, jewish_self_determination__indigenous_return_reading).
narrative_ontology:affects_constraint(jewish_self_determination__religious_covenant_reading, jewish_self_determination__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_self_determination__religious_covenant_reading, jewish_self_determination__diasporist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'jewish_self_determination' kernel. Its divine covenant claim directly influences and often forecloses other secular or diasporist readings by asserting a higher, immutable authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
