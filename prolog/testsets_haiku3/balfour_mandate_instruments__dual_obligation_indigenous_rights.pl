% ============================================================================
% CONSTRAINT STORY: balfour_mandate_instruments__dual_obligation_indigenous_rights
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_balfour_mandate_instruments__dual_obligation_indigenous_rights, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: balfour_mandate_instruments__dual_obligation_indigenous_rights
 *   human_readable: Balfour Mandate Dual-Obligation Reading: Indigenous Arab Rights Primacy
 *   domain: international_law/colonial_administration/state_formation
 *
 * SUMMARY:
 *   This constraint story instantiates ONE reading of the contested Balfour
 *   Mandate kernel. The reading asserts that mandate instruments impose EQUAL
 *   OR SUPERIOR obligation to protect existing Arab civil/political rights
 *   and land tenure, with the 'national home' commitment subordinated to
 *   self-determination norms and minority-protection principles. This is the
 *   dual-obligation indigenous-rights reading: it prioritizes the mandate
 *   text's clause protecting non-Jewish inhabitants' existing rights over the
 *   Balfour Declaration's national-home language. Under this reading, Arab
 *   majority status anchors a claim to representative government and a
 *   sovereignty path grounded in self-determination doctrine. The constraint
 *   is therefore a tangled_rope: it coordinates the dual commitments
 *   (national home + existing-inhabitant protection) while asymmetrically
 *   extracting from Zionist organizations (blocked from unlimited land
 *   acquisition and demographic parity achievement) and British
 *   administrators (constrained in discretion to satisfy Zionist demands).
 *   The reading is formally contestable; sibling readings
 *   (jewish_national_home_primacy and mandatory_interpretive_discretion) hold
 *   different interpretations of the same kernel text.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.68).
domain_priors:suppression_score(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.71).
domain_priors:theater_ratio(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, extractiveness, 0.68).
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(balfour_mandate_instruments__dual_obligation_indigenous_rights, tangled_rope).
narrative_ontology:human_readable(balfour_mandate_instruments__dual_obligation_indigenous_rights, "Balfour Mandate Dual-Obligation Reading: Indigenous Arab Rights Primacy").
narrative_ontology:topic_domain(balfour_mandate_instruments__dual_obligation_indigenous_rights, "international_law/colonial_administration/state_formation").

domain_priors:requires_active_enforcement(balfour_mandate_instruments__dual_obligation_indigenous_rights).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(balfour_mandate_instruments__dual_obligation_indigenous_rights, '905371f5-ae8a-48ec-a00d-d16edc5648e2').
narrative_ontology:cs_kernel_codification('905371f5-ae8a-48ec-a00d-d16edc5648e2', fixed_text).
narrative_ontology:cs_authority_grounding('905371f5-ae8a-48ec-a00d-d16edc5648e2', extraction).
narrative_ontology:cs_interpretation_layer_present('905371f5-ae8a-48ec-a00d-d16edc5648e2').
narrative_ontology:cs_reading_relation('905371f5-ae8a-48ec-a00d-d16edc5648e2', balfour_mandate_instruments__jewish_national_home_primacy, forecloses).
narrative_ontology:cs_reading_relation('905371f5-ae8a-48ec-a00d-d16edc5648e2', balfour_mandate_instruments__mandatory_interpretive_discretion, coexists_with).
narrative_ontology:cs_axiom('905371f5-ae8a-48ec-a00d-d16edc5648e2', foundational, existing_inhabitant_protection_subordinates_national_home).
narrative_ontology:cs_axiom_status(existing_inhabitant_protection_subordinates_national_home, holdable).
narrative_ontology:cs_axiom_grounding('905371f5-ae8a-48ec-a00d-d16edc5648e2', existing_inhabitant_protection_subordinates_national_home, deontological).
narrative_ontology:cs_axiom('905371f5-ae8a-48ec-a00d-d16edc5648e2', foundational, self_determination_overrides_colonial_facilitation).
narrative_ontology:cs_axiom_status(self_determination_overrides_colonial_facilitation, holdable).
narrative_ontology:cs_axiom_grounding('905371f5-ae8a-48ec-a00d-d16edc5648e2', self_determination_overrides_colonial_facilitation, deontological).
narrative_ontology:cs_reference_frame('905371f5-ae8a-48ec-a00d-d16edc5648e2', dual_obligation_covenant_framework).
narrative_ontology:cs_drift_state('905371f5-ae8a-48ec-a00d-d16edc5648e2', late_mandate_period, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('905371f5-ae8a-48ec-a00d-d16edc5648e2', '').
narrative_ontology:cs_kernel_id(balfour_mandate_instruments__dual_obligation_indigenous_rights, balfour_mandate_instruments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_elites_and_communities).
narrative_ontology:constraint_victim(balfour_mandate_instruments__dual_obligation_indigenous_rights, zionist_organizations).
narrative_ontology:constraint_victim(balfour_mandate_instruments__dual_obligation_indigenous_rights, british_mandatory_administrators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Under this reading's interpretation of the mandate, Arab communities retain civil and political rights superior to the Jewish national home project. Land tenure is protected by restrictive transfer clauses; immigration quotas preserve Arab demographic majority; representative government institutions vest majority-rule authority with the Arab population. The constraint recognizes their claim to sovereignty derived from existing inhabitant status and self-determination doctrine.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_elites_and_communities, beneficiary,
    moderate, generational, identity_locked, regional).

% Under this reading, Zionist land-purchase ambitions and immigration facilitation are subordinated to Arab majority preservation. Restrictive land-transfer covenants prevent unlimited Jewish acquisition; immigration quotas are set to prevent demographic displacement of Arabs. This reading treats Zionist institutional and territorial expansion as constrained by the superior mandate obligation to protect existing inhabitant rights. Zionist organizations contest this reading's interpretation of the mandate text and its prioritization of minority status over national home establishment.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, zionist_organizations, payer,
    organized, generational, constrained, global).

% Under this reading, the British mandatory hold authority to implement the dual obligation: protecting Arab civil/political rights AND facilitating the Jewish national home—but with the former obligation taking structural precedence when conflict arises. British administrators bear the cost of managing conflicting constituency demands and face pressure from Zionist organizations to reinterpret the mandate in ways that relax Arab majority protection. This reading constrains their discretion to satisfy Zionist demands without formal reinterpretation.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, british_mandatory_administrators, agenda_setter,
    institutional, biographical, mobile, regional).

% The League of Nations mandate system carries formal oversight authority, including periodic review of mandate compliance. Under this reading, the League's oversight function is invoked to validate the interpretation that dual obligation means Arab rights protection takes precedence in case of conflict. League oversight bodies receive petitions from both Arab and Zionist constituencies and commission reports on compliance.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, league_of_nations_oversight, observer,
    institutional, generational, analytical, global).

% This reading vindicates the self-determination principle as a supreme norm: the mandate's obligation to protect existing inhabitant rights is grounded in the doctrine that peoples have the right to self-determination. This doctrine, not the specific wording of the Balfour Declaration or mandate text, is the authoritative reading frame.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, international_self_determination_doctrine, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(balfour_mandate_instruments__dual_obligation_indigenous_rights, international_self_determination_doctrine).

% This reading vindicates the principle that minority rights cannot be overridden by majority or institutional interests. While Jews are a minority in the mandate territory under this reading, the doctrine of minority protection is invoked to constrain how the national home project can be pursued—not to exempt it from Arab majority rule.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, minority_rights_protection_principle, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(balfour_mandate_instruments__dual_obligation_indigenous_rights, minority_rights_protection_principle).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_elites_and_communities).
narrative_ontology:fixing_cost_class(balfour_mandate_instruments__dual_obligation_indigenous_rights, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The dual-obligation reading coordinates two normative commitments: (1) facilitating Jewish institutional and cultural development within a national home framework, and (2) protecting the civil, political, and property rights of the existing Arab majority population. The coordination problem solved is how to honor both the Balfour Declaration's commitment and the mandate's explicit obligation not to prejudice existing inhabitant rights.
% TRANSFER_FUNCTION: The constraint transfers authority to determine land and immigration policy from Zionist organizations and their British administrative allies to a framework centered on Arab majority preservation. Under this reading, land tenure remains in Arab hands (restrictive transfer clauses); immigration flows are subordinated to demographic stability (quotas prevent Jewish population parity); representative government authority flows to the Arab majority (self-determination principle grants sovereignty path to the existing inhabitants).
% ABSENT_VOICES: Secular Arab nationalists and international legal scholars who were excluded from the original Balfour Declaration and mandate-drafting processes would emphasize that this reading vindicates their position. Conversely, Zionist organizations actively contest this reading and argue for the jewish_national_home_primacy interpretation; they are not absent but actively engaged in the structural contest.
% DISAPPEARANCE_RATIONALE: If this dual-obligation interpretation were formally renounced and replaced with the jewish_national_home_primacy reading, the mandate system's operational constraint structure would shift dramatically: land-transfer restrictions would lift, immigration quotas would dissolve, Arab majority-rule authority would be denied, and the institutional basis for Arab self-determination would collapse. The world of the mandate territory would rearrange from Arab-majority-anchored governance toward demographically mixed or Jewish-institutional-dominated governance.
% FOUNDING_PROBLEM: The founding problem is the tension in the Balfour Declaration and League Mandate: how to reconcile Britain's commitment to establish a Jewish national home in Palestine with the mandate's explicit duty not to prejudice the civil and political rights of existing non-Jewish inhabitants. This reading resolves the tension by reading the mandate's existing-inhabitant-protection clause as the superior obligation.
% FOUNDING_PROBLEM_CORROBORATION: Arab legal scholars and Palestinian national representatives argue this reading reflects the mandate text's plain language and the self-determination doctrine's supremacy in international law. Corroboration comes from international legal historians (e.g., Susan Pedersen, 'The Guardians') and League of Nations archival records showing Arab petitions invoking the existing-inhabitant-protection clause. Zionist organizations and sympathetic British administrators contest this reading, arguing the Balfour Declaration's national-home language takes precedence. No independent party has formally adjudicated the dispute; the reading remains contestable.
narrative_ontology:disappearance_verdict(balfour_mandate_instruments__dual_obligation_indigenous_rights, world_rearranges).
narrative_ontology:founding_problem_status(balfour_mandate_instruments__dual_obligation_indigenous_rights, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(balfour_mandate_instruments__dual_obligation_indigenous_rights, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(balfour_mandate_instruments__dual_obligation_indigenous_rights, 'none', 1).
narrative_ontology:epsilon_provenance(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(balfour_mandate_instruments__dual_obligation_indigenous_rights_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(balfour_mandate_instruments__dual_obligation_indigenous_rights, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(balfour_mandate_instruments__dual_obligation_indigenous_rights_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is measured at 0.68 because under this reading, the constraint systematically blocks Zionist land acquisition and immigration facilitation while preserving Arab property and demographic position. The mechanism is not coercive brutality but formal restriction: land-transfer clauses are law; immigration quotas are policy; representative-government authority is constitutional. Suppression is high (0.71) because the constraint's persistence depends on actively enforcing the land-transfer restrictions and immigration quotas against Zionist organizations' continuous pressure to relax them. Theater is moderate (0.44) because the constraint embeds real coordination—the dual obligation is genuinely articulated in the mandate text—but a growing share of British administrative activity shifts from implementing dual obligation toward managing Zionist demands and downplaying the existing-inhabitant-protection clause. The measurement series shows extractiveness and suppression rising and plateauing around mid-interval, consistent with historical patterns: early mandate years see higher British commitment to dual-obligation implementation, but administrative energy gradually shifts toward accommodating Zionist pressure without formal mandate reinterpretation. Theater rises as the gap widens between stated dual obligation and actual discretionary drift.
 *
 * DIRECTIONALITY LOGIC:
 *   Under this reading, Palestinian Arab elites and communities are positioned as beneficiaries (d ≈ 0.2): the constraint protects their land, their demographic majority, and their sovereignty claim. Zionist organizations are positioned as victims (d ≈ 0.8): they are directly targeted by the land-transfer restrictions and immigration quotas; their exit options are constrained (organized effort to reinterpret the mandate, but trapped within the formal mandate system). British administrators sit near the symmetric point (d ≈ 0.5): they are dual-bound—required to implement dual obligation but constrained by Zionist pressure, international oversight, and Arab majority expectations. Their power (institutional) and exit options (mobile, can resign or advocate reinterpretation) differentiate them from the other payers.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reconciling dual commitment to national home + existing-inhabitant protection) is contestable but substantive at interval-start. By interval-end, the founding problem status has shifted toward 'dead' in practice: British administrative energy has increasingly drifted toward Zionist accommodation, and the existing-inhabitant-protection clause has been subordinated in practice (though not formally renounced). The theater ratio's rise from 0.25 to 0.44 indicates growing performative maintenance of the dual-obligation framing while actual enforcement drifts. However, the constraint has NOT become a piton: there is no undifferentiated beneficiary (Palestinian Arab polities continue to assert the dual-obligation reading and derive real benefits from land-transfer restrictions); there IS active resistance (both Arab and Zionist constituencies contest the mandate's interpretation). The constraint shows mandatrophy trajectory—the founding problem's operational force is attenuating—but retains enough functional teeth and constituency contest to remain a tangled_rope rather than degrading to piton status during this interval.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mandate_text_interpretation_underdetermination,
    'Does the Balfour Mandate text''s explicit clause on existing inhabitant rights (''nothing shall be done which may prejudice the civil and political rights of existing non-Jewish communities'') subordinate or coordinate with the national-home commitment?',
    'Formal international legal adjudication or binding reinterpretation by League of Nations organs. Historical-textual analysis of drafting intent and contemporaneous state practice. Comparative construction of similar protective clauses in other mandate instruments.',
    'If subordination is the correct reading, this dual-obligation reading holds and tangled_rope classification is justified. If coordination-without-hierarchy is correct, the national-home project is effectively unconstrained by existing-inhabitant protection, shifting classification toward snare or rope-for-national-home-beneficiaries. If the text is ambiguous beyond resolution, mandatory_interpretive_discretion reading becomes the actual constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mandate_text_interpretation_underdetermination, conceptual, 'The fundamental interpretive ambiguity: does ''existing inhabitant protection'' constrain or only coordinate with ''national home''?').

omega_variable(
    self_determination_doctrine_priority,
    'In the League of Nations mandate system, does the self-determination principle hold superior normative status to colonial administrative commitments (like the national home)? Or are they parallel commitments requiring balance?',
    'League-era legal scholarship and mandate-review decisions. Post-League international legal doctrine development (UN Charter era). Archival evidence of League deliberations on mandate conflicts between self-determination and other commitments.',
    'If self-determination doctrine is superior, the dual-obligation reading anchors Arab majority-rule sovereignty and the tangled_rope classification holds. If parallel-commitment doctrine prevails, national home and existing-inhabitant protection must be balanced discretionally, shifting authority to mandatory_interpretive_discretion reading. If self-determination doctrine is subordinate to imperial authority, jewish_national_home_primacy reading becomes the justified interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(self_determination_doctrine_priority, conceptual, 'Whether self-determination doctrine is normatively supreme in the mandate system.').

omega_variable(
    british_discretion_scope_ambiguity,
    'Does the mandate instrument grant Britain discretionary authority to adjudicate between its dual obligations when they conflict, or is the mandate text itself the binding constraint that precludes British reinterpretation without League approval?',
    'League of Nations oversight records and mandate-review decisions. Analysis of League responses to British reinterpretive efforts. Comparative study of British discretion claims vs. League enforcement in mandate disputes.',
    'If mandate text is binding and British discretion is limited, this dual-obligation reading holds and British administrators are constrained payers. If British discretion is broad, mandatory_interpretive_discretion becomes the primary constraint, and the dual-obligation reading is demoted to a normative claim within discretionary scope. The classification would shift from measuring the dual-obligation constraint to measuring the discretion-constraint instead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(british_discretion_scope_ambiguity, empirical, 'The scope of British mandatory power discretion relative to the mandate text.').

omega_variable(
    arab_majority_sovereignty_claim_grounding,
    'Under this reading, the Arab majority''s claim to representative government and sovereignty path rests on self-determination doctrine applied to existing inhabitants. Is this claim grounded in ethnic/national self-determination (Arabs as a nation) or in democratic majority-rule principle (residents as a demos)?',
    'Clarification of which normative grounding the reading''s advocates actually invoke. Legal analysis of whether self-determination doctrine applies to ethnic nations, resident demoi, or both. Historical evidence of how Arab national and Palestinian representative leaders framed the claim.',
    'If ethnic-national grounding is primary, the reading risks essentialist nationality claims that later conflict with minority-rights principles (if minorities are presumed not to be ''the nation''). If demos-grounding is primary, the reading''s minority-protection principle becomes internally coherent but may not support Arab ethnic self-determination per se, only majority-rule democracy. This affects whether the reading can coherently claim to vindicate both self-determination and minority-protection simultaneously.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(arab_majority_sovereignty_claim_grounding, conceptual, 'The normative grounding of Arab sovereignty claim under this reading: ethnic-national vs. democratic-majoritaire.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression of Zionist land acquisition and immigration facilitation sustained by external legal barriers (structural suppression) or by internalized British administrative belief in the legitimacy of the dual-obligation reading (internalized suppression)? If British administrators internalize the dual-obligation interpretation as the authentic mandate requirement, they become less costly to the constraint''s enforcement; if they view it as politically imposed constraint, enforcement costs remain high.',
    'Post-mandate memoir and archival evidence of British administrative attitudes toward the dual-obligation interpretation. Records of British discretionary drift (if present) would indicate declining internalization. Comparison with periods of stronger vs. weaker British commitment to existing-inhabitant protection.',
    'If suppression is primarily structural (legal), the constraint''s persistence depends on continued formal enforcement; if internalized, the constraint becomes lower-cost and more durable. Rising theater_ratio could indicate either mechanism: growing performative maintenance of exterior structures while belief erodes, or growing administrative confidence that dual obligation is the correct reading (genuine belief capture). The measurement cannot distinguish the mechanisms; the omega names the ambiguity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether measured suppression is sustained by external legal enforcement or by internalized British administrative commitment to the dual-obligation reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(balf_tr_t0, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 0, 0.25).
narrative_ontology:measurement(balf_tr_t5, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 5, 0.3).
narrative_ontology:measurement(balf_tr_t10, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 10, 0.36).
narrative_ontology:measurement(balf_tr_t15, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 15, 0.4).
narrative_ontology:measurement(balf_tr_t20, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 20, 0.42).
narrative_ontology:measurement(balf_tr_t25, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 25, 0.43).
narrative_ontology:measurement(balf_tr_t30, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 30, 0.44).

% Extraction over time
narrative_ontology:measurement(balf_be_t0, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(balf_be_t5, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(balf_be_t10, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 10, 0.63).
narrative_ontology:measurement(balf_be_t15, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 15, 0.66).
narrative_ontology:measurement(balf_be_t20, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(balf_be_t25, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 25, 0.68).
narrative_ontology:measurement(balf_be_t30, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 30, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(balf_su_t0, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(balf_su_t5, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(balf_su_t10, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(balf_su_t15, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 15, 0.69).
narrative_ontology:measurement(balf_su_t20, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(balf_su_t25, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 25, 0.71).
narrative_ontology:measurement(balf_su_t30, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 30, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(balfour_mandate_instruments__dual_obligation_indigenous_rights, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.12).
narrative_ontology:affects_constraint(balfour_mandate_instruments__dual_obligation_indigenous_rights, balfour_mandate_instruments__jewish_national_home_primacy).
narrative_ontology:affects_constraint(balfour_mandate_instruments__dual_obligation_indigenous_rights, balfour_mandate_instruments__mandatory_interpretive_discretion).

% DUAL FORMULATION NOTE:
% The Balfour Mandate kernel decomposes into three structurally distinct constraint stories, each instantiating one reading of the foundational commitment text. All three share the same kernel (the Balfour Declaration and mandate instruments) but differ in ε, beneficiary/victim structure, and claimed type. This story (dual_obligation_indigenous_rights) authorizes a high-extractiveness tangled_rope where Arab rights protection subordinates national-home facilitation. The jewish_national_home_primacy reading instantiates a different tangled_rope where demographic/territorial transformation subordinates existing-inhabitant protection, with reversed beneficiary/victim roles. The mandatory_interpretive_discretion reading treats the constraint as the British administrative discretion system itself, making the mandate text formally underdetermined. The three readings are linked via network.affects_constraints: this reading influences the other two by establishing a substantive interpretive position that constrains what readings are available without explicit kernel reinterpretation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(balfour_mandate_instruments__dual_obligation_indigenous_rights, institutional, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
