% ============================================================================
% CONSTRAINT STORY: one_country_two_systems_framework__balanced_coexistence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_one_country_two_systems_framework__balanced_coexistence_reading, []).

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
 *   constraint_id: one_country_two_systems_framework__balanced_coexistence_reading
 *   human_readable: One Country, Two Systems â Balanced Coexistence Reading
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   The balanced-coexistence reading of One Country, Two Systems treats the
 *   framework as a dynamic constitutional settlement in which neither
 *   sovereignty nor autonomy is absolute. Contested boundaries between
 *   Beijing and Hong Kong are resolved through political accommodation rather
 *   than through the legal supremacy of either system. This reading stands
 *   between sovereignty primacy (Beijing's authority overrides Hong Kong when
 *   interests conflict) and autonomy primacy (Hong Kong retains
 *   treaty-guaranteed substantive autonomy with enforceable limits on
 *   mainland interference). The constraint coordinates genuine functional
 *   differentiation while extracting autonomy from Hong Kong civil society
 *   and the legal profession through periodic boundary reassertions by
 *   Beijing.
 *
 * KEY AGENTS:
 *   - Beijing authority (agenda-setter, institutional/arbitrage) â sets sovereignty parameters and captures sovereignty consolidation
 *   - Hong Kong government (agenda-setter/beneficiary, institutional/constrained) â administers the dual framework, squeezed between directives and local expectations
 *   - Hong Kong civil society (payer, organized/constrained) â bears autonomy-erosion costs but retains economic/international bargaining leverage
 *   - Hong Kong legal profession (payer, organized/constrained) â defends rule of law under political-accommodation override
 *   - Hong Kong business elite (beneficiary, powerful/mobile) â captures economic bridge benefits with exit capacity
 *   - International community (observer, institutional/analytical) â monitors and shapes external legitimacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(one_country_two_systems_framework__balanced_coexistence_reading, 0.55).
domain_priors:suppression_score(one_country_two_systems_framework__balanced_coexistence_reading, 0.75).
domain_priors:theater_ratio(one_country_two_systems_framework__balanced_coexistence_reading, 0.46).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 0.46).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(one_country_two_systems_framework__balanced_coexistence_reading, tangled_rope).
narrative_ontology:human_readable(one_country_two_systems_framework__balanced_coexistence_reading, "One Country, Two Systems â Balanced Coexistence Reading").
narrative_ontology:topic_domain(one_country_two_systems_framework__balanced_coexistence_reading, "constitutional/political").

domain_priors:requires_active_enforcement(one_country_two_systems_framework__balanced_coexistence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(one_country_two_systems_framework__balanced_coexistence_reading, '4035e489-20ec-461d-819b-f0194b208d4a').
narrative_ontology:cs_kernel_codification('4035e489-20ec-461d-819b-f0194b208d4a', formalized).
narrative_ontology:cs_authority_grounding('4035e489-20ec-461d-819b-f0194b208d4a', lineage).
narrative_ontology:cs_interpretation_layer_present('4035e489-20ec-461d-819b-f0194b208d4a').
narrative_ontology:cs_reading_relation('4035e489-20ec-461d-819b-f0194b208d4a', one_country_two_systems_framework__sovereignty_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('4035e489-20ec-461d-819b-f0194b208d4a', one_country_two_systems_framework__autonomy_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('4035e489-20ec-461d-819b-f0194b208d4a', foundational, neither_sovereignty_nor_autonomy_absolute).
narrative_ontology:cs_axiom_status(neither_sovereignty_nor_autonomy_absolute, holdable).
narrative_ontology:cs_axiom_grounding('4035e489-20ec-461d-819b-f0194b208d4a', neither_sovereignty_nor_autonomy_absolute, conventional).
narrative_ontology:cs_axiom('4035e489-20ec-461d-819b-f0194b208d4a', foundational, political_accommodation_over_legal_supremacy).
narrative_ontology:cs_axiom_status(political_accommodation_over_legal_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('4035e489-20ec-461d-819b-f0194b208d4a', political_accommodation_over_legal_supremacy, conventional).
narrative_ontology:cs_reference_frame('4035e489-20ec-461d-819b-f0194b208d4a', balanced_coexistence_framework).
narrative_ontology:cs_drift_state('4035e489-20ec-461d-819b-f0194b208d4a', post_national_security_law_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4035e489-20ec-461d-819b-f0194b208d4a', '').
narrative_ontology:cs_kernel_id(one_country_two_systems_framework__balanced_coexistence_reading, one_country_two_systems_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__balanced_coexistence_reading, beijing_authority).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_government).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_business_elite).
narrative_ontology:constraint_victim(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_civil_society).
narrative_ontology:constraint_victim(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_legal_profession).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the ultimate sovereignty parameters for Hong Kong through the NPCSC and its interpretation of the Basic Law. Asserts territorial integrity and national security as non-negotiable bottom lines. Can unilaterally reinterpret or override Hong Kong legal outcomes when it judges core sovereign interests to be at stake.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, beijing_authority, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Administers Hong Kong affairs under the Basic Law and implements both locally derived policy and Beijing-directed mandates. Retains institutional legitimacy and career continuity from the arrangement's persistence, but is squeezed between sovereign directives and local expectations.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_government, agenda_setter,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_government, beneficiary).

% Participates in Hong Kong's political and social life under increasingly narrowed autonomy. Bears the costs of eroded civil liberties, electoral constraints, and contested boundaries. Retains some bargaining power through economic relevance and international visibility, but faces tightening political space.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_civil_society, payer,
    organized, biographical, constrained, regional).

% Maintains the common-law tradition and judicial independence within the Basic Law framework. Bears extraction when political accommodation overrides legal supremacy or when NPCSC interpretations preempt local judicial review.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_legal_profession, payer,
    organized, biographical, constrained, regional).

% Operates as the economic bridge between mainland China and global markets. Benefits from Hong Kong's distinct commercial law, currency, and access while retaining mainland opportunities. Can relocate capital and operations if the framework collapses, using mobility as leverage.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_business_elite, beneficiary,
    powerful, biographical, mobile, global).

% Monitors the Sino-British Joint Declaration and human-rights commitments. Issues diplomatic statements, sanctions, and trade assessments. Does not participate in the internal constitutional arrangement but shapes its external legitimacy and economic cost.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, international_community, observer,
    institutional, generational, analytical, global).

narrative_ontology:fixing_cost_class(one_country_two_systems_framework__balanced_coexistence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables a sovereign state to incorporate a territory with a historically distinct legal, economic, and social system without immediate full assimilation, preserving functional differentiation while maintaining territorial unity and avoiding capital-flight or geopolitical rupture.
% TRANSFER_FUNCTION: Moves jurisdictional authority and policy autonomy between the sovereign center and the territorial unit, with the allocation determined by periodic political bargaining. Transfers sovereignty consolidation and national-security assurance to Beijing, stability and market access to Hong Kong's business sector, and autonomy-erosion costs to civil society and the legal profession.
% ABSENT_VOICES: Hong Kong pro-independence advocates and full-universal-suffrage activists are structurally excluded from the negotiation framework; their absence is the condition that makes the balanced-coexistence bargain appear as the moderate center.
% DISAPPEARANCE_RATIONALE: If the balanced-coexistence framework vanished overnight, Hong Kong would face either full integration into the mainland system (triggering capital flight and brain drain) or a rupture toward full autonomy or independence. Beijing's territorial-management strategy would collapse, and the regional constitutional order would reorganize fundamentally.
% FOUNDING_PROBLEM: How to recover sovereign control over a territorially reintegrated but economically and legally distinct region after colonial handover without triggering capital flight, brain drain, or geopolitical rupture.
% FOUNDING_PROBLEM_CORROBORATION: The Sino-British Joint Declaration and the Basic Law attest the founding problem from the enacting parties' view. Independent constitutional scholars, Hong Kong civil-society organizations, and international human-rights monitors outside the benefiting parties attest that the arrangement has substantially drifted from its founding purpose; Beijing asserts the problem remains live and the framework is still serving it.
narrative_ontology:disappearance_verdict(one_country_two_systems_framework__balanced_coexistence_reading, world_rearranges).
narrative_ontology:founding_problem_status(one_country_two_systems_framework__balanced_coexistence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(one_country_two_systems_framework__balanced_coexistence_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(one_country_two_systems_framework__balanced_coexistence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(one_country_two_systems_framework__balanced_coexistence_reading, 0.55, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(one_country_two_systems_framework__balanced_coexistence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(one_country_two_systems_framework__balanced_coexistence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(one_country_two_systems_framework__balanced_coexistence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is medium (0.55) because the framework extracts meaningful autonomy from Hong Kongâparticularly post-2019/2020âwhile still preserving a distinct economic and legal identity. Suppression is high (0.75) because the constraint's persistence depends on actively suppressing independence advocacy, disqualifying dissenting candidates, and preempting judicial review through NPCSC interpretations. Theater ratio is moderate-high (0.46) because an increasing share of maintenance activity is performative: affirming two-systems rhetoric while operating a sovereignty-primacy structure in practice. Accessibility collapse (0.58) reflects that alternatives such as full independence or genuine competitive democracy are structurally blocked, though emigration remains a costly exit. Resistance (0.62) captures sustained protest cycles, emigration waves, international sanctions, and legal challenges.
 *
 * PERSPECTIVAL GAP:
 *   From Beijing's seat the constraint is necessary coordination preserving territorial integrity while avoiding costly full integration; from Hong Kong civil society's seat the same structure operates as progressively extractive sovereignty compression dressed in constitutional language. The business elite experiences a hybrid: genuine economic benefit alongside growing political risk. The engine computes these divergences from the structural data rather than adjudicating which perception is correct.
 *
 * DIRECTIONALITY LOGIC:
 *   Beijing authority sits at the beneficiary end (d near 0.0): it subsidizes its sovereignty consolidation through the constraint. Hong Kong business elite also sits beneficiary-ward (d low) due to mobile exit and net economic gain. Hong Kong civil society and the legal profession sit target-ward (d high) as the seats from which autonomy and judicial supremacy are extracted. The Hong Kong government sits near the middle: it coordinates locally but implements sovereign extraction, with constrained exit producing a directionality between symmetric and target.
 *
 * MANDATROPHY ANALYSIS:
 *   The balanced-coexistence reading prevents mislabeling by acknowledging both the genuine coordination function (preserving Hong Kong's distinct economic and legal system avoids integration shock) and the asymmetric extraction function (sovereignty boundaries are enforced against civil society and the judiciary). Without the tangled-rope classification, the framework would be misread either as pure coordination (ignoring the autonomy extraction) or as pure snare (ignoring the real functional differentiation and business-elite benefits that would collapse under full integration).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_operational_dominance,
    'Does the balanced-coexistence reading accurately describe the operational structure of the framework, or has sovereignty primacy become the de facto regime with balanced rhetoric serving as legitimizing cover?',
    'Longitudinal analysis of NPCSC interpretation frequency, Basic Law Article 23 implementation scope, and the ratio of political-accommodation overrides to judicially resolved boundary disputes.',
    'If sovereignty primacy is operationally dominant, the constraint''s extractiveness is higher than the balanced reading suggests and the classification edges toward snare; if genuine accommodation persists, the tangled-rope reading holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_operational_dominance, empirical, 'Whether balanced coexistence or sovereignty primacy is the operational regime').

omega_variable(
    bargaining_power_leverage,
    'Does Hong Kong civil society retain meaningful bargaining power through economic and international leverage, or has that leverage been hollowed out by mainland integration and emigration self-selection?',
    'Track the responsiveness of Beijing and the Hong Kong government to business-community lobbying and international pressure on specific policy disputes over a multi-year window.',
    'If leverage is hollowed out, civil society''s directionality moves closer to full target and the constraint''s effective extraction rises; if leverage remains real, the balanced-coexistence claim is structurally supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bargaining_power_leverage, empirical, 'Whether civil society bargaining power is sustained or eroded').

omega_variable(
    accommodation_vs_law_mechanism,
    'Are contested boundaries genuinely resolved through political accommodation, or does the legal supremacy of the mainland NPCSC ultimately decide all boundary disputes?',
    'Comparative case-study analysis of boundary disputes: measure the share resolved through bilateral political negotiation versus NPCSC interpretation or unilateral central-government directive.',
    'If NPCSC supremacy decides disputes, the political-accommodation axiom is performative and the constraint''s theater_ratio is higher than surface metrics suggest.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(accommodation_vs_law_mechanism, empirical, 'Whether boundary resolution is genuinely accommodative or legally predetermined').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(one_country_two_systems_framework__balanced_coexistence_reading, 0, 27).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(oc2s_balanced_tr_t0, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(oc2s_balanced_tr_t5, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 5, 0.2).
narrative_ontology:measurement(oc2s_balanced_tr_t10, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 10, 0.24).
narrative_ontology:measurement(oc2s_balanced_tr_t15, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 15, 0.3).
narrative_ontology:measurement(oc2s_balanced_tr_t20, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(oc2s_balanced_tr_t25, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 25, 0.44).
narrative_ontology:measurement(oc2s_balanced_tr_t27, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 27, 0.46).

% Extraction over time
narrative_ontology:measurement(oc2s_balanced_be_t0, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(oc2s_balanced_be_t5, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 5, 0.33).
narrative_ontology:measurement(oc2s_balanced_be_t10, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(oc2s_balanced_be_t15, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 15, 0.44).
narrative_ontology:measurement(oc2s_balanced_be_t20, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(oc2s_balanced_be_t25, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 25, 0.54).
narrative_ontology:measurement(oc2s_balanced_be_t27, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 27, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(oc2s_balanced_su_t0, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(oc2s_balanced_su_t5, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 5, 0.33).
narrative_ontology:measurement(oc2s_balanced_su_t10, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(oc2s_balanced_su_t15, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 15, 0.5).
narrative_ontology:measurement(oc2s_balanced_su_t20, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(oc2s_balanced_su_t25, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement(oc2s_balanced_su_t27, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 27, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(one_country_two_systems_framework__balanced_coexistence_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(one_country_two_systems_framework__balanced_coexistence_reading, sovereignty_primacy_reading).
narrative_ontology:affects_constraint(one_country_two_systems_framework__balanced_coexistence_reading, autonomy_primacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel one_country_two_systems_framework. The three readings (sovereignty_primacy, balanced_coexistence, autonomy_primacy) represent structurally distinct interpretations of the same constitutional settlement. They are linked as a constraint family; each reading carries a distinct epsilon value and beneficiary/victim structure derived from its normative premises.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
