% ============================================================================
% CONSTRAINT STORY: combatant_status_definition__state_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_combatant_status_definition__state_centric_reading, []).

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
 *   constraint_id: combatant_status_definition__state_centric_reading
 *   human_readable: State-Centric Combatant Status under GC III Article 4
 *   domain: international_humanitarian_law
 *
 * SUMMARY:
 *   This constraint instantiates the state_centric_reading of the
 *   combatant_status_definition kernel. Under Geneva Convention III Article
 *   4, combatant statusâand the accompanying POW protections and
 *   domestic-law immunityâis reserved for members of formal state
 *   militaries meeting organization, command, sign, and conduct criteria.
 *   Non-state fighters are categorically excluded. The constraint carries a
 *   genuine coordination function (reciprocal protection of state forces,
 *   incentivizing compliance with the laws of war) alongside asymmetric
 *   extraction (denial of protections to non-state actors who may comply with
 *   identical conduct norms). The claim/metric independence is maintained:
 *   the constraint is claimed as tangled_rope because the coordination
 *   function is structurally real, while the authored metrics describe a
 *   heavily extractive, actively enforced regime.
 *
 * KEY AGENTS:
 *   - State parties (agenda_setter / institutional / global): Drafted and maintain Article 4; set the criteria and control treaty amendment.
 *   - State militaries (beneficiary / powerful / global): Receive POW immunity and combatant privileges when criteria are met.
 *   - Non-state fighters (payer / powerless / regional): Excluded from POW status; subject to domestic prosecution.
 *   - ICRC (observer / organized / global): Monitors compliance and promotes IHL but lacks enforcement authority.
 *   - Anti-colonial movements (excluded / powerless / regional): Would claim combatant status under alternative readings but are absent from the state-centric framework.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(combatant_status_definition__state_centric_reading, 0.75).
domain_priors:suppression_score(combatant_status_definition__state_centric_reading, 0.8).
domain_priors:theater_ratio(combatant_status_definition__state_centric_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(combatant_status_definition__state_centric_reading, tangled_rope).
narrative_ontology:human_readable(combatant_status_definition__state_centric_reading, "State-Centric Combatant Status under GC III Article 4").
narrative_ontology:topic_domain(combatant_status_definition__state_centric_reading, "international_humanitarian_law").

domain_priors:requires_active_enforcement(combatant_status_definition__state_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(combatant_status_definition__state_centric_reading, 'd2d54070-dcff-4656-b88e-adc897dc66ed').
narrative_ontology:cs_kernel_codification('d2d54070-dcff-4656-b88e-adc897dc66ed', formalized).
narrative_ontology:cs_authority_grounding('d2d54070-dcff-4656-b88e-adc897dc66ed', lineage).
narrative_ontology:cs_interpretation_layer_present('d2d54070-dcff-4656-b88e-adc897dc66ed').
narrative_ontology:cs_reading_relation('d2d54070-dcff-4656-b88e-adc897dc66ed', combatant_status_definition__national_liberation_reading, coexists_with).
narrative_ontology:cs_reading_relation('d2d54070-dcff-4656-b88e-adc897dc66ed', combatant_status_definition__functional_protection_reading, influences).
narrative_ontology:cs_axiom('d2d54070-dcff-4656-b88e-adc897dc66ed', foundational, state_military_organization_prerequisite).
narrative_ontology:cs_axiom_status(state_military_organization_prerequisite, holdable).
narrative_ontology:cs_axiom_grounding('d2d54070-dcff-4656-b88e-adc897dc66ed', state_military_organization_prerequisite, conventional).
narrative_ontology:cs_axiom('d2d54070-dcff-4656-b88e-adc897dc66ed', foundational, reciprocal_immunity_bargain).
narrative_ontology:cs_axiom_status(reciprocal_immunity_bargain, holdable).
narrative_ontology:cs_axiom_grounding('d2d54070-dcff-4656-b88e-adc897dc66ed', reciprocal_immunity_bargain, conventional).
narrative_ontology:cs_reference_frame('d2d54070-dcff-4656-b88e-adc897dc66ed', interstate_reciprocal_protection_framework).
narrative_ontology:cs_drift_state('d2d54070-dcff-4656-b88e-adc897dc66ed', contemporary_asymmetric_conflict_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d2d54070-dcff-4656-b88e-adc897dc66ed', '').
narrative_ontology:cs_kernel_id(combatant_status_definition__state_centric_reading, combatant_status_definition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(combatant_status_definition__state_centric_reading, state_militaries).
narrative_ontology:constraint_beneficiary(combatant_status_definition__state_centric_reading, state_parties).
narrative_ontology:constraint_victim(combatant_status_definition__state_centric_reading, non_state_fighters).
narrative_ontology:constraint_vindicates(combatant_status_definition__state_centric_reading, state_sovereignty_monopoly_on_lawful_force).
narrative_ontology:constraint_vindicates(combatant_status_definition__state_centric_reading, regular_forces_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafted and ratified the 1949 Geneva Conventions; retain exclusive authority to interpret, amend, and enforce Article 4 criteria through diplomatic conferences and domestic military law. They control the treaty architecture that defines lawful combatant status and resist expansions that would erode state control over organized violence.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, state_parties, agenda_setter,
    institutional, generational, arbitrage, global).

% Receive full POW protections and combatant immunity from domestic prosecution for acts of lawful war when meeting Article 4 criteria. The state-centric definition guarantees their members a legal shield that non-state fighters cannot access, regardless of equivalent conduct.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, state_militaries, beneficiary,
    powerful, generational, constrained, global).

% Excluded from POW status and combatant immunity under the state-centric reading if they are not integrated into a formal state military meeting Article 4. Subject to domestic criminal prosecution for bearing arms, regardless of whether they comply with the laws of war or operate under a responsible command.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, non_state_fighters, payer,
    powerless, immediate, trapped, regional).

% Promotes and monitors compliance with international humanitarian law; issues interpretive guidance on combatant status but lacks enforcement authority. Observes the growing gap between the state-centric framework and the reality of asymmetric, non-international armed conflict.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, icrc, observer,
    organized, generational, analytical, global).

% Would claim combatant status and POW protections under alternative legal readings such as AP I Article 1(4), but are structurally excluded from the state-centric regime. Their claims are dismissed in formal treaty interpretation and domestic prosecutions because they lack state sponsorship.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, anti_colonial_movements, excluded,
    powerless, biographical, trapped, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(combatant_status_definition__state_centric_reading, state_militaries).
narrative_ontology:fixing_cost_class(combatant_status_definition__state_centric_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Regularizes interstate armed conflict by granting legal immunity from domestic prosecution to state-organized militaries that meet command, fixed-sign, arms-open, and law-of-war criteria, thereby establishing reciprocal protections among state parties and incentivizing compliance with international norms.
% TRANSFER_FUNCTION: Moves legal immunity, humane-capture standards, and post-capture protections from the international legal order to state-sanctioned militaries, while moving domestic criminal liability, denial of protections, and exposure to prosecution to non-state fighters who take up arms.
% ABSENT_VOICES: Non-state armed groups, anti-colonial liberation movements, and humanitarian legal scholars arguing for functional or membership-based protection criteria are structurally excluded from treaty-drafting and interpretive processes that maintain the state-centric definition.
% DISAPPEARANCE_RATIONALE: If the state-centric combatant definition vanished overnight, state militaries would lose guaranteed POW immunity and face potential prosecution for acts of lawful war, while non-state fighters would gain access to Geneva protections; the architecture of legal immunity in armed conflict would reorganize around functional or membership criteria rather than state formalism.
% FOUNDING_PROBLEM: Post-World War II need to protect lawful state combatants from domestic prosecution for acts permitted under international law, and to create reciprocal obligations among states regarding treatment of captured regular forces.
% FOUNDING_PROBLEM_CORROBORATION: State parties and military historians attest the problem was interstate war regularization. Humanitarian organizations and post-colonial legal scholars attest the problem framing excluded anti-colonial and internal conflicts from protection, corroborating that the 'solution' encoded state-sovereignty assumptions rather than pure humanitarian need. No fully neutral corroborator exists; the ICRC occupies a hybrid position.
narrative_ontology:disappearance_verdict(combatant_status_definition__state_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(combatant_status_definition__state_centric_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(combatant_status_definition__state_centric_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(combatant_status_definition__state_centric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(combatant_status_definition__state_centric_reading, 0.75, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(combatant_status_definition__state_centric_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(combatant_status_definition__state_centric_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(combatant_status_definition__state_centric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.75 at interval end) because the categorical exclusion of non-state actors from POW status imposes severe legal costs regardless of conduct. Suppression is higher (0.80) because the constraint's persistence depends on active legal enforcementâdomestic prosecution, tribunal practice, and diplomatic resistance to AP I expansion. Theater is moderate (0.45): the reciprocity and civilization justification is partially genuine, but a growing share of the regime's maintenance serves to preserve state sovereignty over the monopoly on lawful force rather than purely humanitarian ends. Accessibility collapse is high (0.82) because once the Article 4 criteria are understood, non-state actors recognize they have no procedural path to POW status under this reading. Resistance is moderate (0.60) because excluded parties resist through non-compliance, alternative legal arguments, and the development of parallel protection frameworks.
 *
 * PERSPECTIVAL GAP:
 *   From the state-party seat, the constraint is essential reciprocal coordination that civilizes war and protects their forces. From the non-state-fighter seat, the identical structure is an enforced exclusion that criminalizes their participation regardless of conduct. The engine computes this divergence from beneficiary/victim declarations and exit asymmetry; the authored claim does not adjudicate between the seats.
 *
 * DIRECTIONALITY LOGIC:
 *   State parties and state militaries are structural beneficiaries: they authored the rule, meet its criteria by definition, and collect legal immunity (low d). Non-state fighters are structural targets: they are categorically excluded, prosecuted under domestic law, and lack standing to amend the regime (high d). The ICRC sits near symmetric, providing coordination benefits to the system without collecting extraction. Anti-colonial movements are excluded entirely, experiencing the constraint as pure external imposition.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâprotecting lawful combatants in interstate warâhas not fully expired, but it has substantially shifted. The majority of contemporary armed conflict is non-international or asymmetric, yet the state-centric definition persists because state parties continue to benefit from the sovereignty-preserving exclusion. The constraint has not atrophied into a piton because the coordination function for state forces remains operationally relevant and the beneficiaries are concentrated and powerful enough to maintain it. It is not a pure snare because the reciprocal protection it provides to state militaries is structurally genuine.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    state_centric_vs_functional_protection,
    'Does the state-centric reading of combatant status remain structurally viable in an era of predominant non-international armed conflict, or has the functional protection reading superseded it in practice?',
    'Measure state tribunal and international criminal court practice: if courts increasingly apply Common Article 3 and functional criteria regardless of state status, the state-centric reading is becoming overridden in practice despite its formal hold.',
    'If functional criteria are applied in practice, the state-centric constraint''s effective extractiveness is lower than its formal text suggests; if state formalism still governs, extraction remains high.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_centric_vs_functional_protection, conceptual, 'Whether the state-centric reading maintains practical dominance over functional alternatives.').

omega_variable(
    non_state_actor_exclusion_necessity,
    'Is the categorical exclusion of non-state actors from POW status a necessary condition for interstate reciprocity and compliance with the laws of war, or is it a sovereignty-preserving construction that extracts from non-state fighters to benefit state monopoly on violence?',
    'Comparative analysis of armed conflict outcomes in jurisdictions that extended protections versus those that maintained strict exclusion; examination of whether reciprocity collapsed when AP I Article 1(4) was introduced.',
    'If exclusion is necessary for coordination, the constraint is more rope-like for state parties; if exclusion is sovereignty-preservation, the extraction is asymmetric and the tangled_rope classification is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_state_actor_exclusion_necessity, empirical, 'Whether the exclusion of non-state actors is coordination-necessary or sovereignty-extractive.').

omega_variable(
    enforcement_consistency,
    'To what extent is the state-centric reading actively enforced uniformly versus selectively against politically disfavored non-state groups?',
    'Case-law survey of domestic military tribunals and international prosecutions comparing treatment of state-allied paramilitaries versus independent insurgents.',
    'Selective enforcement would indicate the constraint operates as a political snare rather than a neutral legal classification; uniform enforcement supports the coordination-function claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_consistency, empirical, 'Uniformity versus selectivity of enforcement against non-state fighters.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(combatant_status_definition__state_centric_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(csd_state_tr_t0, combatant_status_definition__state_centric_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(csd_state_tr_t15, combatant_status_definition__state_centric_reading, theater_ratio, 15, 0.25).
narrative_ontology:measurement(csd_state_tr_t30, combatant_status_definition__state_centric_reading, theater_ratio, 30, 0.3).
narrative_ontology:measurement(csd_state_tr_t45, combatant_status_definition__state_centric_reading, theater_ratio, 45, 0.35).
narrative_ontology:measurement(csd_state_tr_t60, combatant_status_definition__state_centric_reading, theater_ratio, 60, 0.4).
narrative_ontology:measurement(csd_state_tr_t75, combatant_status_definition__state_centric_reading, theater_ratio, 75, 0.45).

% Extraction over time
narrative_ontology:measurement(csd_state_be_t0, combatant_status_definition__state_centric_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(csd_state_be_t15, combatant_status_definition__state_centric_reading, base_extractiveness, 15, 0.6).
narrative_ontology:measurement(csd_state_be_t30, combatant_status_definition__state_centric_reading, base_extractiveness, 30, 0.65).
narrative_ontology:measurement(csd_state_be_t45, combatant_status_definition__state_centric_reading, base_extractiveness, 45, 0.68).
narrative_ontology:measurement(csd_state_be_t60, combatant_status_definition__state_centric_reading, base_extractiveness, 60, 0.72).
narrative_ontology:measurement(csd_state_be_t75, combatant_status_definition__state_centric_reading, base_extractiveness, 75, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(csd_state_su_t0, combatant_status_definition__state_centric_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(csd_state_su_t15, combatant_status_definition__state_centric_reading, suppression_requirement, 15, 0.58).
narrative_ontology:measurement(csd_state_su_t30, combatant_status_definition__state_centric_reading, suppression_requirement, 30, 0.65).
narrative_ontology:measurement(csd_state_su_t45, combatant_status_definition__state_centric_reading, suppression_requirement, 45, 0.7).
narrative_ontology:measurement(csd_state_su_t60, combatant_status_definition__state_centric_reading, suppression_requirement, 60, 0.76).
narrative_ontology:measurement(csd_state_su_t75, combatant_status_definition__state_centric_reading, suppression_requirement, 75, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(combatant_status_definition__state_centric_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(combatant_status_definition__state_centric_reading, combatant_status_definition__national_liberation_reading).
narrative_ontology:affects_constraint(combatant_status_definition__state_centric_reading, combatant_status_definition__functional_protection_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the combatant_status_definition kernel, which decomposes into three structurally distinct claims per the epsilon-invariance principle. The state-centric reading (this file) maintains high extractiveness for non-state actors; the national_liberation reading lowers extractiveness for liberation movements under AP I; the functional_protection reading lowers extractiveness across the board via Common Article 3 minimum guarantees.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
