% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__liberal_nationalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_self_determination__liberal_nationalist_reading, []).

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
 *   constraint_id: jewish_self_determination__liberal_nationalist_reading
 *   human_readable: Jewish National Self-Determination (Liberal Nationalist Reading)
 *   domain: political_philosophy/nationalism/postcolonial_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the liberal nationalist reading of the
 *   jewish_self_determination kernel: the claim that Jewish people constitute
 *   a nation with an equal and legitimate claim to territorial sovereignty
 *   and self-determination as other peoples. The reading treats the State of
 *   Israel as the legitimate expression of this universal national right,
 *   assuming that territorial partition and mutual recognition can resolve
 *   competing claims. The referent is the standing arrangement of Jewish
 *   territorial sovereignty in contested Palestine/Israel, assessed by this
 *   reading's own lights. Sibling readings include indigenous return
 *   (blood/ancestry), religious covenant (divine promise), settler-colonial
 *   (European dispossession), and diasporist (minority-rights pluralism).
 *   This reading frames the arrangement as a coordination mechanism (rope)
 *   rather than an extraction mechanism; the authored metrics are independent
 *   of that claim and reflect modest drift upward as partition feasibility
 *   declines.
 *
 * KEY AGENTS:
 *   - Jewish diaspora: Primary beneficiary (organized/global) â gains sovereignty, refuge, and institutional capacity.
 *   - Israeli state: Agenda setter (institutional/national) â administers the territorial expression of the national claim.
 *   - Palestinian national movement: Excluded competing claimant (organized/national) â advances overlapping self-determination claim but is structurally marginalized in the institutional design.
 *   - Diasporist critics: Excluded dissenters (moderate/global) â reject territorial sovereignty as the primary Jewish future.
 *   - Liberal nationalist theorists: Analytical observer (analytical/global) â evaluates fit between arrangement and liberal nationalist normative criteria.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__liberal_nationalist_reading, 0.34).
domain_priors:suppression_score(jewish_self_determination__liberal_nationalist_reading, 0.22).
domain_priors:theater_ratio(jewish_self_determination__liberal_nationalist_reading, 0.24).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, extractiveness, 0.34).
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, theater_ratio, 0.24).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, resistance, 0.28).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__liberal_nationalist_reading, rope).
narrative_ontology:human_readable(jewish_self_determination__liberal_nationalist_reading, "Jewish National Self-Determination (Liberal Nationalist Reading)").
narrative_ontology:topic_domain(jewish_self_determination__liberal_nationalist_reading, "political_philosophy/nationalism/postcolonial_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__liberal_nationalist_reading, 'e7e700a9-6f91-4e6f-9272-907a71464152').
narrative_ontology:cs_kernel_codification('e7e700a9-6f91-4e6f-9272-907a71464152', formalized).
narrative_ontology:cs_authority_grounding('e7e700a9-6f91-4e6f-9272-907a71464152', lineage).
narrative_ontology:cs_interpretation_layer_present('e7e700a9-6f91-4e6f-9272-907a71464152').
narrative_ontology:cs_reading_relation('e7e700a9-6f91-4e6f-9272-907a71464152', jewish_self_determination__indigenous_return_reading, coexists_with).
narrative_ontology:cs_reading_relation('e7e700a9-6f91-4e6f-9272-907a71464152', jewish_self_determination__settler_colonial_reading, forecloses).
narrative_ontology:cs_reading_relation('e7e700a9-6f91-4e6f-9272-907a71464152', jewish_self_determination__religious_covenant_reading, coexists_with).
narrative_ontology:cs_reading_relation('e7e700a9-6f91-4e6f-9272-907a71464152', jewish_self_determination__diasporist_reading, coexists_with).
narrative_ontology:cs_axiom('e7e700a9-6f91-4e6f-9272-907a71464152', foundational, national_equality_principle).
narrative_ontology:cs_axiom_status(national_equality_principle, holdable).
narrative_ontology:cs_axiom_grounding('e7e700a9-6f91-4e6f-9272-907a71464152', national_equality_principle, deontological).
narrative_ontology:cs_axiom('e7e700a9-6f91-4e6f-9272-907a71464152', foundational, partition_resolves_conflict).
narrative_ontology:cs_axiom_status(partition_resolves_conflict, holdable).
narrative_ontology:cs_axiom_grounding('e7e700a9-6f91-4e6f-9272-907a71464152', partition_resolves_conflict, instrumental).
narrative_ontology:cs_reference_frame('e7e700a9-6f91-4e6f-9272-907a71464152', equal_national_self_determination).
narrative_ontology:cs_drift_state('e7e700a9-6f91-4e6f-9272-907a71464152', post_oslo_erosion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e7e700a9-6f91-4e6f-9272-907a71464152', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__liberal_nationalist_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__liberal_nationalist_reading, jewish_diaspora).
narrative_ontology:constraint_vindicates(jewish_self_determination__liberal_nationalist_reading, liberal_nationalist_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Dispersed national collective exercising the option of sovereign self-determination through state institutions; benefits from citizenship access, cultural reproduction, and collective security guarantees that the territorial state provides.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, jewish_diaspora, beneficiary,
    organized, generational, mobile, global).

% Administers the territorial and institutional expression of Jewish national self-determination; sets citizenship, immigration, and security policy; represents the national claim in international law and diplomacy.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, israeli_state, agenda_setter,
    institutional, generational, arbitrage, national).

% Reject territorial sovereignty as the primary vehicle of Jewish continuity; argue that nation-state frameworks endanger diasporic flourishing and misallocate Jewish political energy; excluded from the institutional architecture of the state.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, diasporist_critics, excluded,
    moderate, generational, mobile, global).

% Advances a competing national self-determination claim on overlapping territory; structurally marginalized in the liberal nationalist framing despite the reading's theoretical commitment to equal national rights for all peoples.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, palestinian_national_movement, excluded,
    organized, generational, constrained, national).

% Evaluate whether the institutional arrangement satisfies the normative criteria of liberal nationalism: equal legitimacy, mutual recognition, territorial partition, and non-domination of other nations.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, liberal_nationalist_theorists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a sovereign territorial framework for a stateless nation to exercise collective self-determination, secure refuge from persecution, and participate in the international state system on equal footing with other nations.
% TRANSFER_FUNCTION: Moves political authority, security capacity, and institutional resources from a globally dispersed diaspora into a centralized territorial state; channels collective energy into citizenship, military service, and national institutions.
% ABSENT_VOICES: Diasporist critics who reject territorial sovereignty as the primary Jewish future, and Palestinian national actors whose competing claim to the same territory is assumed to be resolvable through partition rather than integrated into the current institutional design.
% DISAPPEARANCE_RATIONALE: If the constraint vanished overnight, Jewish diaspora would lose the primary institutional vehicle for sovereign self-determination and territorial refuge; the regional state system would reorganize around binational, diasporic, or other alternatives, and the international norm of equal national rights would lose a major test case.
% FOUNDING_PROBLEM: Jewish statelessness and minority vulnerability in Europe and the Middle East during the era of rising nationalism and antisemitic persecution; the absence of a territorial polity capable of exercising diplomatic and military self-defense on behalf of the nation.
% FOUNDING_PROBLEM_CORROBORATION: Zionist historiography and Israeli state institutions attest the problem remains live, citing ongoing antisemitism and security threats. Post-Zionist and diasporist scholars outside the benefiting parties attest the founding problem has been substantially addressed by liberal-democratic integration elsewhere, while Palestinian and postcolonial analysts argue the arrangement reproduced the problem as dispossession of another people. International human rights law provides partial corroboration for continued vulnerability readings.
narrative_ontology:disappearance_verdict(jewish_self_determination__liberal_nationalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_self_determination__liberal_nationalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__liberal_nationalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jewish_self_determination__liberal_nationalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_self_determination__liberal_nationalist_reading, 0.34, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_self_determination__liberal_nationalist_reading_tests).
:- end_tests(jewish_self_determination__liberal_nationalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low-moderate (0.34 at interval end) because the liberal nationalist reading genuinely treats the arrangement as solving a coordination problem (statelessness) rather than extracting from a captive population. Suppression is modest (0.22) and rising slowly, reflecting the increasing enforcement required to maintain the arrangement as partition becomes less feasible. Theater ratio (0.24) indicates a modest performative component â ritual declarations of commitment to two states while facts on the ground shift â but the majority of the constraint remains functional. Accessibility collapse (0.38) captures the reality that diasporic and binational alternatives remain conceptually available but are institutionally less accessible once the state is entrenched. Resistance (0.28) reflects persistent but not overwhelming opposition from competing national and diasporist seats.
 *
 * PERSPECTIVAL GAP:
 *   The agenda setter and beneficiary seats experience the constraint as legitimate coordination that secures national existence against historical vulnerability. The excluded competing-claimant seats experience the same territorial arrangement as foreclosing their own self-determination. The engine computes this divergence from the structural data â the same state institutions read as refuge from one seat and as domination from another â without requiring the author to reconcile the frames.
 *
 * DIRECTIONALITY LOGIC:
 *   The Jewish diaspora sits near the beneficiary end (low d) because the constraint is designed to channel sovereignty and security to them. The Israeli state, as agenda setter, also sits near the beneficiary side though with administrative costs. Palestinian national actors and diasporist critics sit nearer the target end because the arrangement forecloses their preferred political futures; however, they are not declared victims in this reading's ideal theory because partition is assumed to resolve competing claims. The liberal nationalist theorist occupies an analytical seat with neutral directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The liberal nationalist reading is structurally vulnerable to mandatrophy if partition fails and the arrangement persists as sovereign control over a population that rejects it. The founding problem (Jewish statelessness) is contested: diasporist and post-Zionist analysts argue it is solved by liberal integration elsewhere, while Zionist institutions argue it persists. If the problem is dead but the arrangement persists without achieving its sunset condition (mutual recognition and partition), the rope degrades toward tangled_rope or piton. The temporal measurements show modest monotonic drift upward in extractiveness, theater, and suppression requirement, consistent with lifecycle degradation of a coordination mechanism whose founding justification has eroded.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    partition_feasibility,
    'Is territorial partition still a viable resolution mechanism for competing national claims given settlement expansion, demographic integration, and political fragmentation?',
    'Observational study of territorial contiguity, settlement geography, and polling data on mutual recognition across both national publics.',
    'If partition is empirically infeasible, the foundational axiom that partition resolves conflict is undermined, pushing the constraint toward tangled_rope or higher extraction profiles.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_feasibility, empirical, 'Whether the two-state framework remains structurally possible.').

omega_variable(
    diaspora_national_cohesion,
    'Does the Jewish diaspora maintain sufficient national cohesion to sustain a nation-state claim under liberal nationalist criteria, or has identity fragmented into purely religious, cultural, or civic affiliations?',
    'Sociological and demographic studies of Jewish identity formation, attachment to Israel, and intermarriage rates across major diaspora communities.',
    'If national cohesion has fragmented, the ''nation'' premise weakens and the reading''s grounding collapses toward identity_coordination or piton status.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diaspora_national_cohesion, empirical, 'Whether diaspora cohesion sustains the national claim.').

omega_variable(
    committer_frame_ambiguity,
    'Does the liberal nationalist reading''s universalist framing genuinely minimize asymmetric extraction, or does it merely redistribute the same territorial costs under a more legitimate-sounding vocabulary compared to the indigenous return and religious covenant readings?',
    'Comparative structural analysis of all five kernel readings to measure beneficiary concentration, victim incidence, and extraction profiles under each framing.',
    'If the universalist frame is found to obscure extraction rather than prevent it, the rope classification degrades toward tangled_rope; if it genuinely coordinates at lower cost, the reading is validated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_frame_ambiguity, conceptual, 'Committer-frame ambiguity for the liberal nationalist reading within the jewish_self_determination kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__liberal_nationalist_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t0, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(jewi_tr_t14, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 14, 0.12).
narrative_ontology:measurement(jewi_tr_t28, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 28, 0.14).
narrative_ontology:measurement(jewi_tr_t42, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 42, 0.17).
narrative_ontology:measurement(jewi_tr_t56, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 56, 0.2).
narrative_ontology:measurement(jewi_tr_t70, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 70, 0.24).

% Extraction over time
narrative_ontology:measurement(jewi_be_t0, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(jewi_be_t14, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 14, 0.2).
narrative_ontology:measurement(jewi_be_t28, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 28, 0.23).
narrative_ontology:measurement(jewi_be_t42, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 42, 0.26).
narrative_ontology:measurement(jewi_be_t56, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 56, 0.3).
narrative_ontology:measurement(jewi_be_t70, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 70, 0.34).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t0, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 0, 0.12).
narrative_ontology:measurement(jewi_su_t14, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 14, 0.14).
narrative_ontology:measurement(jewi_su_t28, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 28, 0.16).
narrative_ontology:measurement(jewi_su_t42, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 42, 0.18).
narrative_ontology:measurement(jewi_su_t56, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 56, 0.2).
narrative_ontology:measurement(jewi_su_t70, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 70, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(jewish_self_determination__liberal_nationalist_reading, jewish_self_determination__indigenous_return_reading).
narrative_ontology:affects_constraint(jewish_self_determination__liberal_nationalist_reading, jewish_self_determination__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_self_determination__liberal_nationalist_reading, jewish_self_determination__religious_covenant_reading).
narrative_ontology:affects_constraint(jewish_self_determination__liberal_nationalist_reading, jewish_self_determination__diasporist_reading).

% DUAL FORMULATION NOTE:
% The jewish_self_determination kernel decomposes into five structurally distinct constraints (readings) because the natural-language label conflates multiple incompatible grounding narratives: universal national rights, indigenous return, divine covenant, settler-colonial critique, and diasporist pluralism. Each reading carries a distinct epsilon, beneficiary structure, and classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
