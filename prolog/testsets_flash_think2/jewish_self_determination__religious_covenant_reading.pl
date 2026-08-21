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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: jewish_self_determination__religious_covenant_reading
 *   human_readable: Jewish Territorial Claim: Divine Covenant Reading
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory/religious_studies
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__religious_covenant_reading, 0.15).
domain_priors:suppression_score(jewish_self_determination__religious_covenant_reading, 0.8).
domain_priors:theater_ratio(jewish_self_determination__religious_covenant_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__religious_covenant_reading, mountain).
narrative_ontology:human_readable(jewish_self_determination__religious_covenant_reading, "Jewish Territorial Claim: Divine Covenant Reading").
narrative_ontology:topic_domain(jewish_self_determination__religious_covenant_reading, "political_philosophy/nationalism_studies/postcolonial_theory/religious_studies").

domain_priors:requires_active_enforcement(jewish_self_determination__religious_covenant_reading).
domain_priors:emerges_naturally(jewish_self_determination__religious_covenant_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__religious_covenant_reading, '11938c18-6bfb-4352-b585-2afa2980c0a3').
narrative_ontology:cs_kernel_codification('11938c18-6bfb-4352-b585-2afa2980c0a3', fixed_text).
narrative_ontology:cs_authority_grounding('11938c18-6bfb-4352-b585-2afa2980c0a3', lineage).
narrative_ontology:cs_interpretation_layer_present('11938c18-6bfb-4352-b585-2afa2980c0a3').
narrative_ontology:cs_reading_relation('11938c18-6bfb-4352-b585-2afa2980c0a3', jewish_self_determination__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('11938c18-6bfb-4352-b585-2afa2980c0a3', jewish_self_determination__indigenous_return_reading, coexists_with).
narrative_ontology:cs_reading_relation('11938c18-6bfb-4352-b585-2afa2980c0a3', jewish_self_determination__settler_colonial_reading, influences).
narrative_ontology:cs_reading_relation('11938c18-6bfb-4352-b585-2afa2980c0a3', jewish_self_determination__diasporist_reading, forecloses).
narrative_ontology:cs_axiom('11938c18-6bfb-4352-b585-2afa2980c0a3', foundational, divine_mandate_for_land).
narrative_ontology:cs_axiom_status(divine_mandate_for_land, holdable).
narrative_ontology:cs_axiom_grounding('11938c18-6bfb-4352-b585-2afa2980c0a3', divine_mandate_for_land, theological).
narrative_ontology:cs_axiom('11938c18-6bfb-4352-b585-2afa2980c0a3', foundational, territorial_sovereignty_religious_obligation).
narrative_ontology:cs_axiom_status(territorial_sovereignty_religious_obligation, holdable).
narrative_ontology:cs_axiom_grounding('11938c18-6bfb-4352-b585-2afa2980c0a3', territorial_sovereignty_religious_obligation, deontological).
narrative_ontology:cs_reference_frame('11938c18-6bfb-4352-b585-2afa2980c0a3', divine_covenant_unconditional).
narrative_ontology:cs_drift_state('11938c18-6bfb-4352-b585-2afa2980c0a3', contemporary_secular_challenge, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('11938c18-6bfb-4352-b585-2afa2980c0a3', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__religious_covenant_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__religious_covenant_reading, religious_zionist_movement).
narrative_ontology:constraint_beneficiary(jewish_self_determination__religious_covenant_reading, settlement_enterprise).
narrative_ontology:constraint_victim(jewish_self_determination__religious_covenant_reading, secular_negotiation_frameworks).
narrative_ontology:constraint_victim(jewish_self_determination__religious_covenant_reading, palestinian_self_determination_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adheres to the belief that the land of Israel was granted to the Jewish people by divine covenant, making territorial sovereignty a religious obligation. They actively promote policies and settlement activities based on this claim, viewing it as immutable and non-negotiable.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, religious_zionist_movement, agenda_setter,
    institutional, generational, identity_locked, national).

% Directly benefits from the religious covenant claim, which provides a powerful justification for establishing and expanding settlements. Their actions are often framed as fulfilling a divine mandate, reinforcing their commitment and resisting external pressures.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, settlement_enterprise, beneficiary,
    organized, biographical, identity_locked, local).

% Bear the cost of the religious claim's non-negotiability, as it fundamentally challenges the premises of land-for-peace or two-state solutions. Their diplomatic efforts and legal principles are often rendered ineffective or irrelevant by the assertion of divine right.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, secular_negotiation_frameworks, payer,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__religious_covenant_reading, secular_negotiation_frameworks, excluded).

% Are directly targeted by the territorial implications of the religious covenant claim, which often denies or subordinates their own claims to self-determination and land. Their ability to negotiate or assert rights within secular frameworks is severely constrained.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, palestinian_self_determination_advocates, payer,
    organized, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__religious_covenant_reading, palestinian_self_determination_advocates, excluded).

% Observe and analyze the conflict through the lens of international law, human rights, and self-determination. They find the religious covenant claim difficult to reconcile with secular legal principles, leading to ongoing condemnations and diplomatic impasses.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, international_law_bodies, observer,
    institutional, civilizational, analytical, global).

% Often advocate for Jewish collective survival and flourishing through diaspora pluralism, rather than territorial sovereignty. Their perspective, which challenges the centrality of land to Jewish identity, is often marginalized or dismissed by adherents of the religious covenant claim.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, diasporist_jewish_communities, excluded,
    organized, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_self_determination__religious_covenant_reading, religious_zionist_movement).
narrative_ontology:fixing_cost_class(jewish_self_determination__religious_covenant_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unifies a segment of the Jewish people around a shared religious identity and purpose tied to the land of Israel, providing a basis for collective action, settlement, and self-determination understood as a divine mandate.
% TRANSFER_FUNCTION: Transfers ultimate legitimacy and territorial claims from secular political discourse and international law to religious doctrine, effectively transferring land and resources to those acting on the religious claim, and away from those operating on secular frameworks.
% ABSENT_VOICES: Secular political actors, international legal bodies, and Palestinian communities are either excluded or their frameworks are deemed irrelevant by the religious claim. Diasporist Jewish communities also offer alternative visions for Jewish identity and security that are often marginalized.
% DISAPPEARANCE_RATIONALE: If the divine covenant claim vanished overnight, the primary ideological and theological grounding for certain territorial claims would disappear, fundamentally altering the political landscape, the legitimacy of settlements, and the basis for negotiation. The entire framework of religious Zionism would be undermined, leading to a profound reorganization of political and social structures.
% FOUNDING_PROBLEM: To establish and maintain Jewish sovereignty and presence in the land of Israel, understood as a divine mandate, in the face of historical exile, persecution, and contemporary political challenges.
% FOUNDING_PROBLEM_CORROBORATION: The problem is attested as live by religious texts, historical traditions, and theological scholarship within the religious Zionist movement. Outside corroboration is contested by secular historians and international legal scholars who view it as a political claim, not a divinely mandated solution to an existential problem.
narrative_ontology:disappearance_verdict(jewish_self_determination__religious_covenant_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_self_determination__religious_covenant_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__religious_covenant_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(jewish_self_determination__religious_covenant_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_self_determination__religious_covenant_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_self_determination__religious_covenant_reading_tests).

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


/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine divine command (Mountain) or a constructed political claim (Snare/Tangled Rope) that benefits identifiable agents?',
    'Analysis of the historical and sociological development of the claim, its enforcement mechanisms, and the material benefits it confers, independent of its theological assertions.',
    'If primarily a constructed political claim, the constraint''s base extractiveness would be re-evaluated as significantly higher, and its classification would shift from Mountain to Tangled Rope or Snare, reflecting its operational reality for all parties.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Ambiguity between divine command and political construct.').

omega_variable(
    identity_lock_impact,
    'To what extent does the ''identity_locked'' exit option for religious adherents genuinely reflect an internal, theological commitment versus external social or political pressures?',
    'Sociological studies of religious communities, analysis of internal dissent and its consequences, and comparison with other religious groups facing similar territorial claims.',
    'If external pressures are dominant, the ''identity_locked'' status might be re-evaluated as ''constrained'' or ''trapped'', increasing the effective extraction for these beneficiaries by revealing hidden costs of adherence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_impact, empirical, 'Internal vs. external drivers of identity-locked exit.').

omega_variable(
    effective_epsilon_discrepancy,
    'How does the very low base extractiveness (epsilon) of this reading reconcile with the high effective extraction (chi) experienced by external parties due to the framework''s contestation?',
    'The engine''s computation of effective extraction (chi) for different seats, which incorporates directionality and scope, directly addresses this. Further analysis would involve mapping the specific mechanisms by which the ''contestation'' amplifies extraction.',
    'The discrepancy highlights the core function of the classification system: to reveal how a constraint perceived as benign internally can be highly extractive externally. No reclassification of epsilon is needed, but the divergence is a key diagnostic.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(effective_epsilon_discrepancy, conceptual, 'Explaining the gap between internal (low epsilon) and external (high chi) extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__religious_covenant_reading, 0, 76).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t0, jewish_self_determination__religious_covenant_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(jewi_tr_t15, jewish_self_determination__religious_covenant_reading, theater_ratio, 15, 0.11).
narrative_ontology:measurement(jewi_tr_t30, jewish_self_determination__religious_covenant_reading, theater_ratio, 30, 0.12).
narrative_ontology:measurement(jewi_tr_t45, jewish_self_determination__religious_covenant_reading, theater_ratio, 45, 0.13).
narrative_ontology:measurement(jewi_tr_t60, jewish_self_determination__religious_covenant_reading, theater_ratio, 60, 0.14).
narrative_ontology:measurement(jewi_tr_t76, jewish_self_determination__religious_covenant_reading, theater_ratio, 76, 0.15).

% Extraction over time
narrative_ontology:measurement(jewi_be_t0, jewish_self_determination__religious_covenant_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(jewi_be_t15, jewish_self_determination__religious_covenant_reading, base_extractiveness, 15, 0.16).
narrative_ontology:measurement(jewi_be_t30, jewish_self_determination__religious_covenant_reading, base_extractiveness, 30, 0.16).
narrative_ontology:measurement(jewi_be_t45, jewish_self_determination__religious_covenant_reading, base_extractiveness, 45, 0.17).
narrative_ontology:measurement(jewi_be_t60, jewish_self_determination__religious_covenant_reading, base_extractiveness, 60, 0.17).
narrative_ontology:measurement(jewi_be_t76, jewish_self_determination__religious_covenant_reading, base_extractiveness, 76, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t0, jewish_self_determination__religious_covenant_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(jewi_su_t15, jewish_self_determination__religious_covenant_reading, suppression_requirement, 15, 0.6).
narrative_ontology:measurement(jewi_su_t30, jewish_self_determination__religious_covenant_reading, suppression_requirement, 30, 0.7).
narrative_ontology:measurement(jewi_su_t45, jewish_self_determination__religious_covenant_reading, suppression_requirement, 45, 0.75).
narrative_ontology:measurement(jewi_su_t60, jewish_self_determination__religious_covenant_reading, suppression_requirement, 60, 0.78).
narrative_ontology:measurement(jewi_su_t76, jewish_self_determination__religious_covenant_reading, suppression_requirement, 76, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__religious_covenant_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_self_determination__religious_covenant_reading, jewish_self_determination__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__religious_covenant_reading, jewish_self_determination__indigenous_return_reading).
narrative_ontology:affects_constraint(jewish_self_determination__religious_covenant_reading, jewish_self_determination__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_self_determination__religious_covenant_reading, jewish_self_determination__diasporist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of several readings of the 'Jewish self-determination' kernel. Each reading presents a distinct structural claim with different ε values and stakeholder dynamics. They are linked to show their interrelationship within the broader contestation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
