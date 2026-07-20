% ============================================================================
% CONSTRAINT STORY: constitutional_secularism__reformist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_secularism__reformist_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: constitutional_secularism__reformist_reading
 *   human_readable: Reformist Reading of Constitutional Secularism: Affirmative State Duty to Eliminate Oppressive Religious Practices
 *   domain: constitutional_law/political_theory/religious_governance
 *
 * SUMMARY:
 *   This constraint instantiates the reformist reading of the constitutional
 *   secularism kernel: the Indian state's affirmative constitutional duty to
 *   eliminate religious practices oppressing marginalized groups,
 *   particularly scheduled castes and women, even when this requires
 *   superseding religious autonomy claims. The kernel label 'constitutional
 *   secularism' conflates three structurally distinct claimsâstrict
 *   neutrality (equal non-interference), principled intervention (permissive
 *   reform), and reformist duty (mandatory supersession of autonomy)âand
 *   must be decomposed per the Îµ-invariance principle. This reading is the
 *   most extractive on religious autonomy and the most protective of
 *   subordinated groups. Its operation is contested between a genuine
 *   social-reform framing and a majoritarian-capture framing.
 *
 * KEY AGENTS:
 *   - state_apparatus: Agenda-setter (institutional/national) â enforces the affirmative duty through legislation, criminal law, and executive action
 *   - scheduled_castes: Beneficiary (moderate/national) â gain constitutional remedies against caste-based religious exclusion and untouchability
 *   - women_in_religious_communities: Beneficiary (moderate/national) â gain state-backed protection against patriarchal religious customs
 *   - religious_conservatives: Payer (organized/national) â bear the loss of communal autonomy and customary authority
 *   - religious_institutions: Payer (institutional/national) â lose regulatory authority over internal practices to state oversight
 *   - judiciary: Observer (institutional/national) â interprets the boundary between permissible religious practice and oppressive custom
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_secularism__reformist_reading, 0.72).
domain_priors:suppression_score(constitutional_secularism__reformist_reading, 0.78).
domain_priors:theater_ratio(constitutional_secularism__reformist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_secularism__reformist_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_secularism__reformist_reading, "Reformist Reading of Constitutional Secularism: Affirmative State Duty to Eliminate Oppressive Religious Practices").
narrative_ontology:topic_domain(constitutional_secularism__reformist_reading, "constitutional_law/political_theory/religious_governance").

domain_priors:requires_active_enforcement(constitutional_secularism__reformist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_secularism__reformist_reading, '13520f27-5b0a-4326-ba65-8ee89ae704f8').
narrative_ontology:cs_kernel_codification('13520f27-5b0a-4326-ba65-8ee89ae704f8', fixed_text).
narrative_ontology:cs_authority_grounding('13520f27-5b0a-4326-ba65-8ee89ae704f8', lineage).
narrative_ontology:cs_interpretation_layer_present('13520f27-5b0a-4326-ba65-8ee89ae704f8').
narrative_ontology:cs_reading_relation('13520f27-5b0a-4326-ba65-8ee89ae704f8', constitutional_secularism__strict_neutrality_reading, forecloses).
narrative_ontology:cs_reading_relation('13520f27-5b0a-4326-ba65-8ee89ae704f8', constitutional_secularism__principled_intervention_reading, coexists_with).
narrative_ontology:cs_axiom('13520f27-5b0a-4326-ba65-8ee89ae704f8', foundational, affirmative_state_duty_to_reform_religion).
narrative_ontology:cs_axiom_status(affirmative_state_duty_to_reform_religion, holdable).
narrative_ontology:cs_axiom_grounding('13520f27-5b0a-4326-ba65-8ee89ae704f8', affirmative_state_duty_to_reform_religion, conventional).
narrative_ontology:cs_axiom('13520f27-5b0a-4326-ba65-8ee89ae704f8', foundational, religious_autonomy_subordinate_to_equality).
narrative_ontology:cs_axiom_status(religious_autonomy_subordinate_to_equality, holdable).
narrative_ontology:cs_axiom_grounding('13520f27-5b0a-4326-ba65-8ee89ae704f8', religious_autonomy_subordinate_to_equality, deontological).
narrative_ontology:cs_reference_frame('13520f27-5b0a-4326-ba65-8ee89ae704f8', constitutional_reformist_mandate).
narrative_ontology:cs_drift_state('13520f27-5b0a-4326-ba65-8ee89ae704f8', contemporary_majoritarian_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('13520f27-5b0a-4326-ba65-8ee89ae704f8', '').
narrative_ontology:cs_kernel_id(constitutional_secularism__reformist_reading, constitutional_secularism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_secularism__reformist_reading, scheduled_castes).
narrative_ontology:constraint_beneficiary(constitutional_secularism__reformist_reading, women_in_religious_communities).
narrative_ontology:constraint_victim(constitutional_secularism__reformist_reading, religious_conservatives).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(constitutional_secularism__reformist_reading, religious_institutions).
narrative_ontology:constraint_vindicates(constitutional_secularism__reformist_reading, constitutional_equality_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exercises constitutional authority under Articles 17 and 25(2)(b) to legislate against and proscribe religious practices deemed oppressive to scheduled castes and women. Expands regulatory jurisdiction over religious domains previously governed by communal autonomy. Bears political backlash and enforcement costs but gains centralized authority over religious norms.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, state_apparatus, agenda_setter,
    institutional, generational, constrained, national).

% Receive constitutional and statutory remedies against caste-based religious exclusion and untouchability practices. Gain legal standing to challenge discriminatory customs in courts. Exit from the constraint is constrained because caste identity and its associated vulnerabilities are not voluntarily escapable.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, scheduled_castes, beneficiary,
    moderate, biographical, constrained, national).

% Benefit from state-backed protections against patriarchal religious customs such as unequal inheritance, exclusion from places of worship, or unilateral divorce norms. Their social and familial identity is fused with religious community membership, making exit from the community costly even when legal exit is formally available.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, women_in_religious_communities, beneficiary,
    moderate, biographical, identity_locked, national).

% Bear the loss of communal autonomy over internal religious practices. Face legal penalties, social stigma as regressive, and erosion of customary authority when the state mandates reform. Their capacity to preserve traditional practices is constrained by criminal law and judicial orders.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, religious_conservatives, payer,
    organized, generational, constrained, national).

% Temples, mosques, churches, and religious endowments lose regulatory authority over admission, internal discipline, and customary norms to state oversight. They are subject to administrative and judicial directives that override scriptural or traditional interpretations.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, religious_institutions, payer,
    institutional, generational, constrained, national).

% Interprets the boundary between permissible religious practice and oppressive custom under constitutional provisions. Its pronouncements determine which practices the state must eliminate and which remain protected, operating as the active interpretive layer between constitutional text and state action.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, judiciary, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_secularism__reformist_reading, state_apparatus).
narrative_ontology:fixing_cost_class(constitutional_secularism__reformist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the collective-action problem of caste and gender subordication sanctified by religious tradition, by providing marginalized individuals a centralized, state-backed enforcement mechanism against discriminatory communal practices that local social power would otherwise suppress.
% TRANSFER_FUNCTION: Moves regulatory authority over religious practices from religious communities and institutions to the state and judiciary; transfers legal standing and protective remedy from the communal sphere to individual claimants from scheduled castes and women.
% ABSENT_VOICES: Religious minorities who argue that reformist intervention disproportionately destroys their cultural integrity while sparing majority customs; theological scholars who deny the state's competence to interpret doctrine; libertarian constitutionalists who oppose state entanglement in religion even for egalitarian ends; and conservative women within religious communities who reject state-defined emancipation.
% DISAPPEARANCE_RATIONALE: If the state's affirmative duty vanished overnight, religious institutions would reclaim jurisdiction over contested practices, personal laws would revert toward communal autonomy, scheduled castes would lose a specific constitutional remedy against temple exclusion and untouchability, and the current equilibrium between group rights and individual equality would shift substantially toward religious communal authority.
% FOUNDING_PROBLEM: Post-colonial independence required dismantling caste and gender hierarchies that were historically reinforced by religious sanction, without dissolving religious communities entirely; the specific problem was untouchability, temple exclusion, and patriarchal personal laws that the colonial state had left intact.
% FOUNDING_PROBLEM_CORROBORATION: Scheduled caste organizations and women's rights movements attest the problem remains live in contemporary practice. Religious conservative organizations and some academic historians attest the problem is either exaggerated or that state intervention has mutated into majoritarian bias against minority religions. There is no neutral consensus authority; corroboration is split across partisan lines.
narrative_ontology:disappearance_verdict(constitutional_secularism__reformist_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_secularism__reformist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_secularism__reformist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_secularism__reformist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_secularism__reformist_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_secularism__reformist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_secularism__reformist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_secularism__reformist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72) because the constraint systematically transfers authority from religious communities to the state and to marginalized claimants; suppression is higher (0.78) because the constraint's persistence requires active state enforcement against deeply held religious norms and against institutional resistance. Theater ratio is moderate (0.42) because while genuine protective outcomes occur, a substantial share of state action is performativeâasserting reformist credentials while selectively enforcing against politically weaker communities. Accessibility collapse (0.65) reflects that legal and political alternatives for religious conservatives have substantially narrowed under this reading, though not completely. Resistance (0.75) is high because religious conservatives and institutions mount sustained legal and social opposition. The temporal series show extraction and suppression rising over the interval, with a notable spike in theater ratio during periods of majoritarian consolidation.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (scheduled castes, women) experience the constraint as protective coordination that expands their effective rights against communal domination. The payer seats (religious conservatives, religious institutions) experience the identical constraint as coercive extraction that dissolves their autonomy. The state apparatus occupies a mediating position: it gains regulatory capacity and political capital from the constraint, but also bears enforcement costs and political backlash. The engine computes this divergence from the structural data; the same legal provision reads as liberation from one seat and as oppression from another.
 *
 * DIRECTIONALITY LOGIC:
 *   Scheduled castes and women are declared beneficiaries; their structural relationship to the constraint is subsidizing (low d) because the constraint expands their legal standing and protections. Religious conservatives and religious institutions are declared payers/victims; their structural relationship is targeting (high d) because the constraint directly suppresses their authority and practices. The state apparatus is the agenda-setter and the seat to which extracted regulatory authority flows (gain_flow). The judiciary is an analytical observer with analytical exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy risk here is mislabeling a historically live reformist project as either pure extraction (snare) or pure coordination (rope). The constraint was built to solve the founding problem of caste and gender subordication sanctified by religion. That problem retains live empirical support from marginalized groups. The Tangled Rope classification captures the genuine coordination function (protecting the weak) without denying the asymmetric extraction (suppressing religious autonomy). If the founding problem were dead and the constraint persisted purely for state aggrandizement, it would compute as a snare or piton; if religious autonomy were costless to suppress, it might compute as a rope. Neither is structurally accurate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is this constraint a principled intervention reading (permissive state power), a strict neutrality reading (non-interference), or a reformist reading (mandatory duty superseding autonomy)?',
    'Comparative constitutional analysis of the three sibling readings; the reformist reading is distinguished by its mandatory (not permissive) state duty and its prioritization of equality over religious autonomy.',
    'If the strict neutrality reading prevails, this constraint''s classification shifts toward snare (state overreach without legitimate coordination function); if the reformist reading is correct as framed, the coordination function and extraction are structurally inseparable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'This constraint is one reading of a contested kernel; sibling readings change the victim/beneficiary structure.').

omega_variable(
    majoritarian_capture_ambiguity,
    'Has the reformist duty been captured by majoritarian forces to target minority religions, or does it remain a genuine shield for marginalized groups within all communities?',
    'Empirical audit of enforcement patterns: compare rates of state intervention against Hindu traditional practices versus minority religious practices, controlling for judicial pronouncements and legislative action.',
    'If captured, the beneficiary set narrows to the majority community''s subordinate groups while the victim set expands disproportionately to minorities, increasing extractiveness and potentially shifting classification toward snare; if not captured, the tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(majoritarian_capture_ambiguity, empirical, 'Whether reformist secularism is genuinely egalitarian or majoritarian in practice.').

omega_variable(
    oppression_definition_scope,
    'What constitutes ''oppressive religious practice'' â is the definition fixed by constitutional text, or does it drift with political majorities?',
    'Textual analysis of constitutional amendments and judicial doctrine tracing the scope of Articles 17 and 25(2)(b); comparative analysis across different judicial eras.',
    'If the definition drifts politically, the constraint lacks a stable kernel and effective extraction varies by regime, undermining Îµ-invariance; if fixed by text, the constraint is more stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(oppression_definition_scope, conceptual, 'Whether the core term oppression is stable or politically mobile.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_secularism__reformist_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cs_reformist_tr_t0, constitutional_secularism__reformist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cs_reformist_tr_t15, constitutional_secularism__reformist_reading, theater_ratio, 15, 0.25).
narrative_ontology:measurement(cs_reformist_tr_t30, constitutional_secularism__reformist_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement(cs_reformist_tr_t45, constitutional_secularism__reformist_reading, theater_ratio, 45, 0.3).
narrative_ontology:measurement(cs_reformist_tr_t60, constitutional_secularism__reformist_reading, theater_ratio, 60, 0.48).
narrative_ontology:measurement(cs_reformist_tr_t75, constitutional_secularism__reformist_reading, theater_ratio, 75, 0.42).

% Extraction over time
narrative_ontology:measurement(cs_reformist_be_t0, constitutional_secularism__reformist_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(cs_reformist_be_t15, constitutional_secularism__reformist_reading, base_extractiveness, 15, 0.45).
narrative_ontology:measurement(cs_reformist_be_t30, constitutional_secularism__reformist_reading, base_extractiveness, 30, 0.4).
narrative_ontology:measurement(cs_reformist_be_t45, constitutional_secularism__reformist_reading, base_extractiveness, 45, 0.52).
narrative_ontology:measurement(cs_reformist_be_t60, constitutional_secularism__reformist_reading, base_extractiveness, 60, 0.65).
narrative_ontology:measurement(cs_reformist_be_t75, constitutional_secularism__reformist_reading, base_extractiveness, 75, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(cs_reformist_su_t0, constitutional_secularism__reformist_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(cs_reformist_su_t15, constitutional_secularism__reformist_reading, suppression_requirement, 15, 0.42).
narrative_ontology:measurement(cs_reformist_su_t30, constitutional_secularism__reformist_reading, suppression_requirement, 30, 0.38).
narrative_ontology:measurement(cs_reformist_su_t45, constitutional_secularism__reformist_reading, suppression_requirement, 45, 0.55).
narrative_ontology:measurement(cs_reformist_su_t60, constitutional_secularism__reformist_reading, suppression_requirement, 60, 0.72).
narrative_ontology:measurement(cs_reformist_su_t75, constitutional_secularism__reformist_reading, suppression_requirement, 75, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(constitutional_secularism__reformist_reading, strict_neutrality_reading).
narrative_ontology:affects_constraint(constitutional_secularism__reformist_reading, principled_intervention_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the constitutional_secularism kernel, decomposed per the Îµ-invariance principle because the kernel label conflates structurally distinct claims about state-religion relations. The reformist reading has the highest extractiveness and the most aggressive victim set among the three readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
