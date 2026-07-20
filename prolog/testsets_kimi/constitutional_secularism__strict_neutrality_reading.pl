% ============================================================================
% CONSTRAINT STORY: constitutional_secularism__strict_neutrality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_secularism__strict_neutrality_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: constitutional_secularism__strict_neutrality_reading
 *   human_readable: Constitutional Secularism: Strict Neutrality Reading
 *   domain: constitutional_law/political_theory/religious_governance
 *
 * SUMMARY:
 *   The strict neutrality reading of constitutional secularism construes the
 *   state as constitutionally required to maintain equal distance from all
 *   religions, prohibiting both preferential treatment and interference. This
 *   reading coordinates religious pluralism by preventing state capture by
 *   dominant groups, but it also extracts from marginalized subgroups within
 *   religious communitiesâwomen, lower castes, and dissentersâby denying
 *   them state-backed reform and remedial intervention. The constraint is
 *   actively enforced through constitutional jurisprudence that strikes down
 *   preferential laws and dismisses intervention petitions.
 *
 * KEY AGENTS:
 *   - Minority religious communities (beneficiary/organized/constrained): Protected from state majoritarianism, granted institutional autonomy
 *   - Intra-community marginalized groups (payer/powerless/trapped): Denied state intervention against discriminatory customs, bear the cost of non-interference
 *   - State institutions (agenda_setter/institutional/analytical): Enforce neutrality through constitutional interpretation and judicial review
 *   - Reformist advocates (excluded/moderate/constrained): Excluded from constitutional conversation; their intervention claims treated as neutrality violations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_secularism__strict_neutrality_reading, 0.62).
domain_priors:suppression_score(constitutional_secularism__strict_neutrality_reading, 0.55).
domain_priors:theater_ratio(constitutional_secularism__strict_neutrality_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_secularism__strict_neutrality_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_secularism__strict_neutrality_reading, "Constitutional Secularism: Strict Neutrality Reading").
narrative_ontology:topic_domain(constitutional_secularism__strict_neutrality_reading, "constitutional_law/political_theory/religious_governance").

domain_priors:requires_active_enforcement(constitutional_secularism__strict_neutrality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_secularism__strict_neutrality_reading, 'b42eb330-f236-4bf6-a278-7f10cfb6de45').
narrative_ontology:cs_kernel_codification('b42eb330-f236-4bf6-a278-7f10cfb6de45', formalized).
narrative_ontology:cs_authority_grounding('b42eb330-f236-4bf6-a278-7f10cfb6de45', lineage).
narrative_ontology:cs_interpretation_layer_present('b42eb330-f236-4bf6-a278-7f10cfb6de45').
narrative_ontology:cs_reading_relation('b42eb330-f236-4bf6-a278-7f10cfb6de45', constitutional_secularism__principled_intervention_reading, coexists_with).
narrative_ontology:cs_reading_relation('b42eb330-f236-4bf6-a278-7f10cfb6de45', constitutional_secularism__reformist_reading, coexists_with).
narrative_ontology:cs_axiom('b42eb330-f236-4bf6-a278-7f10cfb6de45', foundational, equal_distance_imperative).
narrative_ontology:cs_axiom_status(equal_distance_imperative, holdable).
narrative_ontology:cs_axiom_grounding('b42eb330-f236-4bf6-a278-7f10cfb6de45', equal_distance_imperative, deontological).
narrative_ontology:cs_axiom('b42eb330-f236-4bf6-a278-7f10cfb6de45', foundational, non_interference_as_neutrality).
narrative_ontology:cs_axiom_status(non_interference_as_neutrality, holdable).
narrative_ontology:cs_axiom_grounding('b42eb330-f236-4bf6-a278-7f10cfb6de45', non_interference_as_neutrality, conventional).
narrative_ontology:cs_reference_frame('b42eb330-f236-4bf6-a278-7f10cfb6de45', state_religious_distance_equilibrium).
narrative_ontology:cs_drift_state('b42eb330-f236-4bf6-a278-7f10cfb6de45', contemporary_reform_demands, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b42eb330-f236-4bf6-a278-7f10cfb6de45', '').
narrative_ontology:cs_kernel_id(constitutional_secularism__strict_neutrality_reading, constitutional_secularism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_secularism__strict_neutrality_reading, minority_religious_communities).
narrative_ontology:constraint_victim(constitutional_secularism__strict_neutrality_reading, intra_community_marginalized_groups).
narrative_ontology:constraint_vindicates(constitutional_secularism__strict_neutrality_reading, state_religious_neutrality_doctrine).
narrative_ontology:constraint_vindicates(constitutional_secularism__strict_neutrality_reading, religious_autonomy_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Protected from state-sponsored majoritarianism and direct interference in religious affairs by the constitutional commitment to equal state distance. They gain autonomy to manage religious institutions and personal law, but remain exposed to social majoritarian pressures in the broader public sphere that the state declines to counter under the neutrality frame.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, minority_religious_communities, beneficiary,
    organized, generational, constrained, national).

% Women, lower castes, and dissenters within religious communities who seek state protection against discriminatory religious customs. The strict neutrality reading blocks state intervention in religious affairs, denying them constitutional remedies and leaving them subject to community norms they cannot individually escape without severe social and economic costs.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, intra_community_marginalized_groups, payer,
    powerless, biographical, trapped, national).

% The judiciary and executive administer the neutrality principle by striking down laws that show religious preference and refusing petitions that demand state intervention in religious practice. Their authority derives from interpreting the constitutional text as mandating non-interference; they maintain the constraint by actively excluding reformist claims.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, state_institutions, agenda_setter,
    institutional, civilizational, analytical, national).

% Feminist, social-justice, and reformist actors who argue for state intervention to protect marginalized members within religious communities. Their claims are constitutionally excluded under the strict neutrality reading, which treats such intervention as preferential interference rather than protective remediation.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, reformist_advocates, excluded,
    moderate, generational, constrained, national).

narrative_ontology:fixing_cost_class(constitutional_secularism__strict_neutrality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents religious conflict over state patronage and protects minority communities from majoritarian state capture by committing the state to equal distance from all religions.
% TRANSFER_FUNCTION: Transfers protective non-interference and institutional autonomy to religious communities, while transferring the cost of that non-interference to marginalized subgroups within those communities who are denied access to state-backed reform.
% ABSENT_VOICES: Reformist advocates and internal minorities seeking state protection against discriminatory religious customs are excluded; their claims for intervention are treated as violations of neutrality rather than legitimate demands for rights enforcement.
% DISAPPEARANCE_RATIONALE: If strict neutrality vanished overnight, religious communities would compete directly for state patronage, minority protections would likely erode under majoritarian capture or expand into state interventionism, and the current equilibrium of managed pluralism through state withdrawal would collapse into either preferential establishment or affirmative reform.
% FOUNDING_PROBLEM: Post-colonial and pluralistic societies facing sectarian competition for state patronage, where state favoritism toward dominant religions generates systemic minority exclusion and communal conflict.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians and comparative political scientists attest to the founding pluralism crisis; minority community leaders corroborate the need for protection from state majoritarianism. Feminist and social-justice scholars from outside the benefiting communities attest that the current arrangement fails to protect internal minorities, documenting the extraction from intra-community marginalized groups.
narrative_ontology:disappearance_verdict(constitutional_secularism__strict_neutrality_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_secularism__strict_neutrality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_secularism__strict_neutrality_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_secularism__strict_neutrality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_secularism__strict_neutrality_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_secularism__strict_neutrality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_secularism__strict_neutrality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_secularism__strict_neutrality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is moderate-high because the constraint systematically blocks state intervention that could protect internal minorities, converting constitutional neutrality into a structural denial of remedy. Suppression (0.55) reflects the constitutional exclusion of reformist claims rather than violent coercion; it rises over the interval as reform movements grow and are judicially rebuffed. Theater ratio (0.28) is low-moderate: neutrality is largely genuine but becomes performative where state inaction enables social majoritarianism while official discourse claims distance. Accessibility collapse (0.60) is moderate because interventionist alternatives are constitutionally marginalized but remain intellectually available. Resistance (0.55) is moderate and rising, driven by excluded reformist voices and internal minorities contesting the frame.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seat (minority communities) experiences the constraint as protective coordination against state majoritarianism; the payer seat (internal minorities) experiences it as extraction through denial of state remedy. The agenda-setter seat (state institutions) experiences it as a principled interpretive framework; the excluded seat (reformists) experiences it as a closure of constitutional possibility. The engine computes this divergence from the structural data rather than the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Minority religious communities are structural beneficiaries of non-interference (low directionality), receiving protective autonomy from state withdrawal. Intra-community marginalized groups are structural targets (high directionality), paying the cost of blocked state intervention. State institutions sit near symmetric but agenda-setting power gives them low extraction. Reformist advocates are excluded from the arrangement entirely, bearing its opportunity cost.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled rope prevents mislabeling the constraint as pure extraction (it genuinely coordinates pluralism and protects minorities from state capture) and as pure coordination (it actively excludes and suppresses reformist claims, generating identifiable victims among internal minorities). The per-seat computation captures this duality without collapsing it to either pole.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutional_text_ambiguity,
    'Does the constitutional kernel textually encode strict neutrality, or is this reading an interpretive construction projected onto ambiguous provisions that also support interventionist readings?',
    'Historical-semantic analysis of constitutional drafting records and comparative textual analysis across jurisdictions with similar provisions.',
    'If the text is genuinely ambiguous, strict neutrality is one contested construction among several rather than the constitutional default, and its extractive costs must be weighed against alternative interpretive possibilities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_text_ambiguity, conceptual, 'Framing under-determination of strict neutrality as constitutional text reading').

omega_variable(
    neutrality_majoritarian_mask,
    'Does strict state neutrality function as genuine protection for minorities, or as a legitimizing frame that allows majoritarian social norms to dominate while the state claims equal distance?',
    'Empirical analysis of minority institutional autonomy outcomes alongside majority community social-power indicators in jurisdictions claiming strict neutrality.',
    'If neutrality masks majoritarianism, the constraint''s coordination function is partially illusory and its extraction from internal minorities is compounded by external majority pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(neutrality_majoritarian_mask, empirical, 'Whether neutrality protects minorities or legitimates majoritarian social dominance').

omega_variable(
    reform_exclusion_necessity,
    'Is the exclusion of state-backed religious reform a necessary entailment of religious autonomy, or an incidental extraction from internal minorities that could be remedied without abandoning neutrality?',
    'Comparative constitutional analysis of jurisdictions that separate protective non-interference from remedial intervention within religious communities.',
    'If separable, the constraint''s extractiveness is lower than measured because reform and neutrality can coexist; if inseparable, the strict reading is structurally committed to extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reform_exclusion_necessity, conceptual, 'Whether reform exclusion is necessary to neutrality or separable from it').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_secularism__strict_neutrality_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cs_strict_neutrality_tr_t0, constitutional_secularism__strict_neutrality_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cs_strict_neutrality_tr_t15, constitutional_secularism__strict_neutrality_reading, theater_ratio, 15, 0.13).
narrative_ontology:measurement(cs_strict_neutrality_tr_t30, constitutional_secularism__strict_neutrality_reading, theater_ratio, 30, 0.17).
narrative_ontology:measurement(cs_strict_neutrality_tr_t45, constitutional_secularism__strict_neutrality_reading, theater_ratio, 45, 0.21).
narrative_ontology:measurement(cs_strict_neutrality_tr_t60, constitutional_secularism__strict_neutrality_reading, theater_ratio, 60, 0.25).
narrative_ontology:measurement(cs_strict_neutrality_tr_t75, constitutional_secularism__strict_neutrality_reading, theater_ratio, 75, 0.28).

% Extraction over time
narrative_ontology:measurement(cs_strict_neutrality_be_t0, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(cs_strict_neutrality_be_t15, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 15, 0.45).
narrative_ontology:measurement(cs_strict_neutrality_be_t30, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 30, 0.5).
narrative_ontology:measurement(cs_strict_neutrality_be_t45, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 45, 0.55).
narrative_ontology:measurement(cs_strict_neutrality_be_t60, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 60, 0.59).
narrative_ontology:measurement(cs_strict_neutrality_be_t75, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 75, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(cs_strict_neutrality_su_t0, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(cs_strict_neutrality_su_t15, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 15, 0.38).
narrative_ontology:measurement(cs_strict_neutrality_su_t30, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 30, 0.42).
narrative_ontology:measurement(cs_strict_neutrality_su_t45, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 45, 0.47).
narrative_ontology:measurement(cs_strict_neutrality_su_t60, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 60, 0.51).
narrative_ontology:measurement(cs_strict_neutrality_su_t75, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 75, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_secularism__strict_neutrality_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(constitutional_secularism__strict_neutrality_reading, 0.08).
narrative_ontology:affects_constraint(constitutional_secularism__strict_neutrality_reading, principled_intervention_reading).
narrative_ontology:affects_constraint(constitutional_secularism__strict_neutrality_reading, reformist_reading).

% DUAL FORMULATION NOTE:
% The constitutional_secularism kernel decomposes into three structurally distinct readings: strict_neutrality (non-interference), principled_intervention (permissive reform), and reformist (mandatory reform). Each reading instantiates a different constraint with distinct beneficiary/victim structures and epsilon values. Strict neutrality coordinates pluralism through state withdrawal; the interventionist readings coordinate through state management. They compete for doctrinal dominance within the same constitutional text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
