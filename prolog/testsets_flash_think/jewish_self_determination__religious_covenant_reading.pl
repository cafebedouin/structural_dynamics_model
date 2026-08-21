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
 *   human_readable: Jewish Claim to Land via Divine Covenant (Religious Covenant Reading)
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the 'religious covenant' reading of
 *   the broader 'Jewish self-determination' kernel. From this perspective,
 *   the Jewish claim to the land is derived from an immutable divine
 *   covenant, making territorial sovereignty a religious obligation that
 *   transcends and is independent of secular political frameworks. The
 *   constraint is claimed as a Mountain due to its perceived divine origin
 *   and immutability, but its operationalization in a contested political
 *   space leads to high effective extraction and suppression, which the
 *   engine will detect as a divergence from the claimed type.
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
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__religious_covenant_reading, mountain).
narrative_ontology:human_readable(jewish_self_determination__religious_covenant_reading, "Jewish Claim to Land via Divine Covenant (Religious Covenant Reading)").
narrative_ontology:topic_domain(jewish_self_determination__religious_covenant_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

domain_priors:requires_active_enforcement(jewish_self_determination__religious_covenant_reading).
domain_priors:emerges_naturally(jewish_self_determination__religious_covenant_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__religious_covenant_reading, '4a82181f-6ef0-4983-bd77-1a2f07d525ae').
narrative_ontology:cs_kernel_codification('4a82181f-6ef0-4983-bd77-1a2f07d525ae', fixed_text).
narrative_ontology:cs_authority_grounding('4a82181f-6ef0-4983-bd77-1a2f07d525ae', lineage).
narrative_ontology:cs_interpretation_layer_present('4a82181f-6ef0-4983-bd77-1a2f07d525ae').
narrative_ontology:cs_reading_relation('4a82181f-6ef0-4983-bd77-1a2f07d525ae', jewish_self_determination__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('4a82181f-6ef0-4983-bd77-1a2f07d525ae', jewish_self_determination__indigenous_return_reading, coexists_with).
narrative_ontology:cs_reading_relation('4a82181f-6ef0-4983-bd77-1a2f07d525ae', jewish_self_determination__settler_colonial_reading, forecloses).
narrative_ontology:cs_reading_relation('4a82181f-6ef0-4983-bd77-1a2f07d525ae', jewish_self_determination__diasporist_reading, forecloses).
narrative_ontology:cs_axiom('4a82181f-6ef0-4983-bd77-1a2f07d525ae', foundational, divine_covenant_absolute).
narrative_ontology:cs_axiom_status(divine_covenant_absolute, holdable).
narrative_ontology:cs_axiom_grounding('4a82181f-6ef0-4983-bd77-1a2f07d525ae', divine_covenant_absolute, theological).
narrative_ontology:cs_axiom('4a82181f-6ef0-4983-bd77-1a2f07d525ae', foundational, territorial_sovereignty_religious_obligation).
narrative_ontology:cs_axiom_status(territorial_sovereignty_religious_obligation, holdable).
narrative_ontology:cs_axiom_grounding('4a82181f-6ef0-4983-bd77-1a2f07d525ae', territorial_sovereignty_religious_obligation, deontological).
narrative_ontology:cs_reference_frame('4a82181f-6ef0-4983-bd77-1a2f07d525ae', biblical_covenantal_mandate).
narrative_ontology:cs_drift_state('4a82181f-6ef0-4983-bd77-1a2f07d525ae', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('4a82181f-6ef0-4983-bd77-1a2f07d525ae', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(jewish_self_determination__religious_covenant_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__religious_covenant_reading, religious_zionist_movement).
narrative_ontology:constraint_beneficiary(jewish_self_determination__religious_covenant_reading, settlement_enterprise).
narrative_ontology:constraint_victim(jewish_self_determination__religious_covenant_reading, secular_political_actors).
narrative_ontology:constraint_victim(jewish_self_determination__religious_covenant_reading, palestinian_self_determination_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adheres to the belief that the land was granted by divine covenant, making its sovereignty a religious obligation. Actively promotes and implements policies based on this claim, benefiting from the territorial control and the legitimization of their actions through religious doctrine.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, religious_zionist_movement, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__religious_covenant_reading, religious_zionist_movement, beneficiary).

% Directly benefits from the expansion and consolidation of settlements, viewing their presence as fulfilling a divine commandment. Their existence and growth are legitimized by the covenantal claim, providing a strong ideological foundation against political challenges.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, settlement_enterprise, beneficiary,
    organized, biographical, identity_locked, regional).

% Operate within a framework that theoretically allows for territorial negotiation based on international law and political compromise. They bear the cost of the religious claim foreclosing these options, leading to diplomatic isolation and internal political friction. Their ability to negotiate is severely constrained by the absolute nature of the covenantal claim.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, secular_political_actors, payer,
    institutional, biographical, constrained, national).

% Represent the indigenous population whose claims to the land are directly contradicted and suppressed by the divine covenant narrative. They bear the most direct costs in terms of dispossession, loss of sovereignty, and denial of self-determination. Their options for exit or alternative frameworks are severely limited by the enforcement of the covenantal claim.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, palestinian_self_determination_advocates, payer,
    organized, generational, trapped, regional).

% Advocate for Jewish flourishing through diaspora pluralism and minority rights, rejecting territorial sovereignty as a central or religiously mandated aspect of Jewish identity. Their perspective is excluded from the dominant discourse that frames territorial control as a divine obligation, and they are often marginalized within broader Jewish political discussions.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, diasporist_jewish_communities, excluded,
    organized, generational, mobile, global).

% Attempt to adjudicate territorial disputes based on secular principles of international law, human rights, and self-determination. They observe the conflict between the religious covenant claim and these secular frameworks, but their authority is often challenged or dismissed by adherents of the covenantal claim, limiting their practical impact.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, international_law_bodies, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_self_determination__religious_covenant_reading, religious_zionist_movement).
narrative_ontology:fixing_cost_class(jewish_self_determination__religious_covenant_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the identity and actions of adherents around a shared, divinely mandated purpose of territorial sovereignty, providing a coherent framework for collective action and legitimizing settlement and governance.
% TRANSFER_FUNCTION: Transfers territorial control, resources, and political legitimacy from secular frameworks and non-adherents to the religious Zionist movement and settlement enterprise, based on the assertion of divine right.
% ABSENT_VOICES: The voices of Palestinian self-determination advocates and diasporist Jewish communities are structurally excluded from the internal logic of the covenantal claim. They would argue for secular political solutions, international law, or alternative forms of Jewish identity, but their perspectives are deemed irrelevant or illegitimate by the covenantal framework.
% DISAPPEARANCE_RATIONALE: If the belief in divine covenant as the basis for territorial sovereignty vanished overnight, the entire ideological and legal foundation for the religious Zionist movement and the settlement enterprise would collapse. This would fundamentally alter the political landscape, opening avenues for secular negotiation and international law that are currently foreclosed, leading to a profound reorganization of territorial claims and governance.
% FOUNDING_PROBLEM: The historical problem of Jewish statelessness and vulnerability, coupled with the religious imperative to return to and inhabit the biblical land of Israel.
% FOUNDING_PROBLEM_CORROBORATION: Adherents of the religious covenant reading attest that the founding problem of Jewish security and the fulfillment of divine prophecy remain live. Critics, including secular political actors and international bodies, contest this, arguing that the problem has evolved into one of occupation and dispossession, but the religious claim itself is primarily attested by its adherents and their theological traditions.
narrative_ontology:disappearance_verdict(jewish_self_determination__religious_covenant_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_self_determination__religious_covenant_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__religious_covenant_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   The base extractiveness is high (0.85) because, while the claim itself is seen as non-extractive by its adherents (a divine gift), its application in a contested territory results in significant material and political costs for non-adherents and secular frameworks. Suppression is very high (0.90) because the absolute nature of the divine claim actively forecloses and delegitimizes alternative secular or indigenous claims to the same territory, requiring active enforcement to maintain. Accessibility collapse is near total (0.95) as, from this reading's perspective, no legitimate alternative to divine sovereignty exists. Resistance is high (0.80) due to the ongoing conflict with those whose claims are suppressed. Theater ratio is low (0.10) because the religious conviction is genuinely held and acted upon, not merely performed.
 *
 * PERSPECTIVAL GAP:
 *   Adherents of this reading perceive the constraint as a Mountain – an unchangeable divine command. However, from the perspective of those whose claims are suppressed or whose political frameworks are overridden, the same constraint operates as a highly extractive and coercive force. The engine's classification will highlight this divergence between the internal claim of naturalness and the external experience of extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The religious Zionist movement and settlement enterprise are clear beneficiaries, gaining territorial control and ideological justification. Secular political actors and Palestinian self-determination advocates are victims, bearing the costs of foreclosed negotiation and dispossession. Diasporist Jewish communities are excluded, as their alternative vision of Jewish identity is marginalized. International law bodies act as observers, attempting to apply a different framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_vs_political_authority,
    'Is the claim to the land based on divine covenant a genuine theological imperative, or is it primarily a political tool used to legitimize territorial expansion and suppress alternative claims?',
    'Analysis of historical and contemporary religious texts and interpretations, alongside political science analysis of its instrumental use in policy and rhetoric. Examination of whether the claim is consistently applied in all contexts or selectively invoked.',
    'If primarily a political tool, the ''emerges_naturally'' property would be re-evaluated as false, shifting the constraint''s structural basis from Mountain to a constructed type (e.g., Snare or Tangled Rope), significantly increasing its effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_vs_political_authority, conceptual, 'Ambiguity between theological imperative and political instrumentalization.').

omega_variable(
    framework_contest_impact,
    'To what extent does the contestation of the religious covenant framework by secular and indigenous claims contribute to the measured extractiveness and suppression, versus the inherent nature of the claim itself?',
    'Counterfactual analysis: model the constraint''s operation in a hypothetical scenario where the religious covenant claim is universally accepted or where it operates in a politically uncontested space. Compare the resulting extractiveness and suppression.',
    'If contestation is the primary driver, the base extractiveness might be lower in an ''ideal'' (uncontested) scenario, suggesting the constraint''s extractive nature is amplified by the conflict it generates. If inherent, the high extractiveness would persist regardless of external contestation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(framework_contest_impact, empirical, 'Impact of external contestation on effective extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__religious_covenant_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1948, jewish_self_determination__religious_covenant_reading, theater_ratio, 1948, 0.1).
narrative_ontology:measurement(jewi_tr_t1967, jewish_self_determination__religious_covenant_reading, theater_ratio, 1967, 0.1).
narrative_ontology:measurement(jewi_tr_t1987, jewish_self_determination__religious_covenant_reading, theater_ratio, 1987, 0.1).
narrative_ontology:measurement(jewi_tr_t2000, jewish_self_determination__religious_covenant_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(jewi_tr_t2010, jewish_self_determination__religious_covenant_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(jewi_tr_t2024, jewish_self_determination__religious_covenant_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1948, jewish_self_determination__religious_covenant_reading, base_extractiveness, 1948, 0.6).
narrative_ontology:measurement(jewi_be_t1967, jewish_self_determination__religious_covenant_reading, base_extractiveness, 1967, 0.7).
narrative_ontology:measurement(jewi_be_t1987, jewish_self_determination__religious_covenant_reading, base_extractiveness, 1987, 0.75).
narrative_ontology:measurement(jewi_be_t2000, jewish_self_determination__religious_covenant_reading, base_extractiveness, 2000, 0.8).
narrative_ontology:measurement(jewi_be_t2010, jewish_self_determination__religious_covenant_reading, base_extractiveness, 2010, 0.83).
narrative_ontology:measurement(jewi_be_t2024, jewish_self_determination__religious_covenant_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1948, jewish_self_determination__religious_covenant_reading, suppression_requirement, 1948, 0.7).
narrative_ontology:measurement(jewi_su_t1967, jewish_self_determination__religious_covenant_reading, suppression_requirement, 1967, 0.78).
narrative_ontology:measurement(jewi_su_t1987, jewish_self_determination__religious_covenant_reading, suppression_requirement, 1987, 0.83).
narrative_ontology:measurement(jewi_su_t2000, jewish_self_determination__religious_covenant_reading, suppression_requirement, 2000, 0.87).
narrative_ontology:measurement(jewi_su_t2010, jewish_self_determination__religious_covenant_reading, suppression_requirement, 2010, 0.89).
narrative_ontology:measurement(jewi_su_t2024, jewish_self_determination__religious_covenant_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__religious_covenant_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_self_determination__religious_covenant_reading, jewish_self_determination__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__religious_covenant_reading, jewish_self_determination__indigenous_return_reading).
narrative_ontology:affects_constraint(jewish_self_determination__religious_covenant_reading, jewish_self_determination__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_self_determination__religious_covenant_reading, jewish_self_determination__diasporist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'Jewish self-determination' kernel. Its structural properties and metrics are distinct from other readings, which are modeled as separate constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
