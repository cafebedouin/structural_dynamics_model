% ============================================================================
% CONSTRAINT STORY: jewish_sovereignty_palestine__liberal_nationalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_sovereignty_palestine__liberal_nationalist_reading, []).

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
 *   constraint_id: jewish_sovereignty_palestine__liberal_nationalist_reading
 *   human_readable: Jewish Sovereignty in Palestine (Liberal Nationalist Reading)
 *   domain: political/nationalism/postcolonial
 *
 * SUMMARY:
 *   This constraint story instantiates the liberal nationalist reading of the
 *   contested kernel jewish_sovereignty_palestine. The reading affirms that
 *   the Jewish people possess a collective right to self-determination and
 *   that statehood in the ancestral homeland is a legitimate exercise of that
 *   right. It differs from religious Zionist, cultural Zionist,
 *   settler-colonial, and post-Zionist readings by grounding legitimacy in
 *   secular national self-determination rather than divine promise, colonial
 *   necessity, or cultural renewal alone. Crucially, this reading
 *   structurally incorporates Palestinians as co-equal self-determination
 *   claimants, which moderates its extractiveness score: the arrangement is
 *   understood to require territorial compromise (partition or binational
 *   power-sharing) rather than permanent subordination. Nevertheless, the
 *   standing arrangement under contestâJewish sovereignty exercised over
 *   the full territory of the former British Mandateâstill extracts from
 *   the Palestinian people through displacement, occupation, and denial of
 *   return. The claim/metric gap is intentional: the reading claims the
 *   arrangement is legitimate self-determination (a coordination function)
 *   while the metrics describe the asymmetric extraction that the current
 *   implementation entails.
 *
 * KEY AGENTS:
 *   - Israeli state (agenda_setter): institutional power, constrained exit, administers sovereignty and enforcement
 *   - Jewish collective (beneficiary): organized power, identity-locked exit, receives self-determination goods
 *   - Palestinian people (payer): powerless, trapped exit, bears costs of partition and occupation
 *   - Liberal Zionist advocates (observer): analytical power, evaluate drift between partition ideal and settlement practice
 *   - International community (observer): institutional power, analytical exit, mediates through law and diplomacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__liberal_nationalist_reading, 0.48).
domain_priors:suppression_score(jewish_sovereignty_palestine__liberal_nationalist_reading, 0.62).
domain_priors:theater_ratio(jewish_sovereignty_palestine__liberal_nationalist_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__liberal_nationalist_reading, tangled_rope).
narrative_ontology:human_readable(jewish_sovereignty_palestine__liberal_nationalist_reading, "Jewish Sovereignty in Palestine (Liberal Nationalist Reading)").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__liberal_nationalist_reading, "political/nationalism/postcolonial").

domain_priors:requires_active_enforcement(jewish_sovereignty_palestine__liberal_nationalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__liberal_nationalist_reading, '96065a92-fb7e-4a42-8347-77574265e509').
narrative_ontology:cs_kernel_codification('96065a92-fb7e-4a42-8347-77574265e509', formalized).
narrative_ontology:cs_authority_grounding('96065a92-fb7e-4a42-8347-77574265e509', lineage).
narrative_ontology:cs_interpretation_layer_present('96065a92-fb7e-4a42-8347-77574265e509').
narrative_ontology:cs_reading_relation('96065a92-fb7e-4a42-8347-77574265e509', jewish_sovereignty_palestine__settler_colonial_reading, coexists_with).
narrative_ontology:cs_reading_relation('96065a92-fb7e-4a42-8347-77574265e509', jewish_sovereignty_palestine__religious_zionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('96065a92-fb7e-4a42-8347-77574265e509', jewish_sovereignty_palestine__cultural_zionist_reading, influences).
narrative_ontology:cs_reading_relation('96065a92-fb7e-4a42-8347-77574265e509', jewish_sovereignty_palestine__post_zionist_reading, coexists_with).
narrative_ontology:cs_axiom('96065a92-fb7e-4a42-8347-77574265e509', foundational, jewish_nation_self_determination_right).
narrative_ontology:cs_axiom_status(jewish_nation_self_determination_right, holdable).
narrative_ontology:cs_axiom_grounding('96065a92-fb7e-4a42-8347-77574265e509', jewish_nation_self_determination_right, deontological).
narrative_ontology:cs_axiom('96065a92-fb7e-4a42-8347-77574265e509', foundational, palestinian_co_equal_claim_recognition).
narrative_ontology:cs_axiom_status(palestinian_co_equal_claim_recognition, holdable).
narrative_ontology:cs_axiom_grounding('96065a92-fb7e-4a42-8347-77574265e509', palestinian_co_equal_claim_recognition, deontological).
narrative_ontology:cs_reference_frame('96065a92-fb7e-4a42-8347-77574265e509', nation_state_self_determination_framework).
narrative_ontology:cs_drift_state('96065a92-fb7e-4a42-8347-77574265e509', post_1967_occupation_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('96065a92-fb7e-4a42-8347-77574265e509', '').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_collective).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_people).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the sovereign state apparatus, military, borders, and immigration policy in the territory of the former British Mandate. Sets laws governing land allocation, citizenship, and security. Cannot dissolve the state without existential risk to the beneficiary community it represents.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, israeli_state, agenda_setter,
    institutional, generational, constrained, national).

% A dispersed nation linked by ethnic, cultural, and historical ties that claims the right to self-determination through statehood in the ancestral homeland. Benefits from immigration rights, cultural center, and collective political agency. Exit from this arrangement would mean abandoning the national project entirely.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_collective, beneficiary,
    organized, generational, identity_locked, global).

% Co-equal self-determination claimants over the same territory who bear the costs of partition, displacement, refugee status, and military occupation. Their ability to exit the constraint is blocked by border regimes, occupation infrastructure, and denial of refugee return rights.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_people, payer,
    powerless, generational, trapped, national).

% Intellectual and political advocates who argue for Jewish statehood as a legitimate exercise of self-determination while insisting on territorial compromise and Palestinian co-equal claims. They observe and critique the drift between the reference frame of partition and the practice of settlement expansion.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, liberal_zionist_advocates, observer,
    analytical, biographical, analytical, global).

% Bodies of international law and state diplomacy that recognize Israeli statehood while simultaneously affirming Palestinian self-determination. Observes the conflict through human rights frameworks and diplomatic initiative, with limited enforceable leverage over either party.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, international_community, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_collective).
narrative_ontology:fixing_cost_class(jewish_sovereignty_palestine__liberal_nationalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a nation-state framework through which the Jewish people can exercise collective self-determination, secure collective survival, and maintain cultural and political continuity in the ancestral homeland.
% TRANSFER_FUNCTION: Transfers territorial sovereignty and state institutions in the ancestral homeland to the Jewish collective, while partially subordinating Palestinian co-equal self-determination claims pending territorial compromise or binational power-sharing.
% ABSENT_VOICES: Palestinian refugees structurally excluded from return negotiations; anti-Zionist Jewish voices rejecting ethnic nationalism as a basis for statehood; one-state and binational advocates marginalized within mainstream liberal nationalist discourse.
% DISAPPEARANCE_RATIONALE: Jewish sovereignty in Palestine underpins the Israeli state, regional security architecture, and diaspora-Jewish political identity. Its disappearance would trigger fundamental geopolitical reorganization, refugee crises, and a sovereignty vacuum in the Levant.
% FOUNDING_PROBLEM: Jewish statelessness, dispersion, and vulnerability to persecution in diaspora (the Jewish Question).
% FOUNDING_PROBLEM_CORROBORATION: International legal scholars attest to historical Jewish vulnerability in diaspora. Palestinian and post-Zionist scholars outside the benefiting parties contest that ethnic statehood in Palestine was the necessary or appropriate remedy. No single uncontested corroborator exists; corroboration is split along disciplinary and political lines.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__liberal_nationalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__liberal_nationalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__liberal_nationalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jewish_sovereignty_palestine__liberal_nationalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_sovereignty_palestine__liberal_nationalist_reading, 0.48, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_sovereignty_palestine__liberal_nationalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_sovereignty_palestine__liberal_nationalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_sovereignty_palestine__liberal_nationalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.48 (moderate) because the liberal nationalist reading recognizes Palestinian co-equal claims and expects territorial compromise, which limits the extraction ceiling relative to readings that deny Palestinian claims entirely. Suppression is 0.62 because the arrangement requires active enforcement: military occupation, border regimes, the Law of Return, and denial of refugee return. Theater_ratio is 0.25 because democratic norms and temporary occupation rhetoric are performed, but the reading genuinely believes in the two-state reference frame, so the gap between performance and belief is not maximal. Accessibility_collapse is 0.58 because alternatives (one-state, binationalism, no Jewish state) are marginalized in mainstream discourse but not fully erased. Resistance is 0.55 due to persistent Palestinian opposition, international BDS pressure, and diplomatic critique. The temporal series shows extraction spiking after 1967 occupation and again during Second Intifada, with a slight moderation at interval end reflecting liberal nationalist hope for renewed compromise.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat (Palestinian people) experiences the constraint as territorial dispossession, military rule, and denial of collective returnâhigh effective extraction amplified by trapped exit. The beneficiary seat (Jewish collective) experiences it as national liberation, collective security, and cultural continuityâlow effective extraction, possibly negative (subsidy). The agenda-setter seat (Israeli state) experiences it as a security necessity with diplomatic costs, sitting between the two. The analytical observer (liberal Zionist advocates) experiences the drift between the reference frame of partition and the practice of settlement expansion as cognitive dissonance. These divergences are structurally produced by directionality and exit asymmetries, not by opinion alone.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish collective is declared beneficiary, producing low directionality toward subsidy. Palestinian people is declared victim/payer, producing high directionality toward extraction. Israeli state administers the constraint and is not a net collector of extraction in the receipt sense; its directionality sits near symmetric but slightly toward beneficiary because its institutional survival is fused with the constraint's persistence. Liberal Zionist advocates and the international community are observers with analytical exit; their directionality is analytically neutral.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was Jewish statelessness and vulnerability. For the liberal nationalist reading, this problem is contested: statehood was achieved in 1948, but the ethnic-national framework now risks mandatrophy if it persists without territorial compromise. The drift between the partition reference frame and the practice of settlement expansion means the arrangement may be outliving its liberal justification. If the two-state framework collapses entirely, the constraint would lose its coordination rationale for this reading and slide toward snare or piton. The temporal measurements capture this drift: base_extractiveness remains moderate but theater_ratio trends upward as the performative maintenance of temporary occupation rhetoric grows distant from the reality of permanent control.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_sibling_structural_delta,
    'This constraint is the liberal nationalist reading of kernel jewish_sovereignty_palestine. What structural classification changes if the settler-colonial or religious Zionist reading is adopted instead?',
    'Generate the sibling constraint stories and compare their base_properties, beneficiary/victim structures, and claimed types across the kernel family.',
    'Adopting the settler-colonial reading would likely reclassify the constraint as snare (high extraction, no coordination function for the displaced); adopting the religious Zionist reading might shift claimed type depending on whether divine mandate is treated as mountain-like or as scaffold-theological.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_sibling_structural_delta, conceptual, 'Kernel reading position and sibling structural deltas').

omega_variable(
    territorial_compromise_form,
    'Does legitimate resolution require territorial partition into two states or a binational power-sharing framework?',
    'Political settlement or constitutional design implementing one model, followed by assessment of stability, minority rights, and self-determination metrics.',
    'A binational framework would likely lower base extractiveness by eliminating ethnic sovereignty; partition maintains two distinct self-determination units but may embed irreducible border and resource conflicts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(territorial_compromise_form, preference, 'Ambiguity about the legitimate form of territorial compromise').

omega_variable(
    palestinian_suppression_mechanism,
    'Is Palestinian subordination maintained primarily by structural force or by internalized acceptance of partition frameworks?',
    'Survey of Palestinian political attitudes and resistance behavior; test whether suppression persists during periods of political opening or leadership change.',
    'If internalized, the constraint''s effective suppression exceeds structural measures and would persist even after enforcement removal; if purely structural, dismantling enforcement could permit rapid rearrangement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(palestinian_suppression_mechanism, empirical, 'Structural vs internalized suppression mechanism for Palestinian people').

omega_variable(
    self_determination_collision,
    'Can two co-equal self-determination claims over the same territory be reconciled without one party bearing extractive cost?',
    'Comparative historical analysis of binational, consociational, or power-sharing states and their long-term stability and justice records.',
    'If collision is structurally unavoidable, the constraint''s base extractiveness is irreducible regardless of implementation; if reconcilable, current extraction is contingent on political failure rather than structural necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(self_determination_collision, conceptual, 'Whether co-equal self-determination claims are structurally zero-sum').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__liberal_nationalist_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t0, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(jewi_tr_t19, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 19, 0.2).
narrative_ontology:measurement(jewi_tr_t30, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement(jewi_tr_t45, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 45, 0.25).
narrative_ontology:measurement(jewi_tr_t55, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 55, 0.28).
narrative_ontology:measurement(jewi_tr_t65, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 65, 0.3).
narrative_ontology:measurement(jewi_tr_t75, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 75, 0.25).

% Extraction over time
narrative_ontology:measurement(jewi_be_t0, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(jewi_be_t19, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 19, 0.45).
narrative_ontology:measurement(jewi_be_t30, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 30, 0.42).
narrative_ontology:measurement(jewi_be_t45, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 45, 0.4).
narrative_ontology:measurement(jewi_be_t55, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 55, 0.5).
narrative_ontology:measurement(jewi_be_t65, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 65, 0.52).
narrative_ontology:measurement(jewi_be_t75, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 75, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t0, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(jewi_su_t19, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 19, 0.6).
narrative_ontology:measurement(jewi_su_t30, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 30, 0.58).
narrative_ontology:measurement(jewi_su_t45, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 45, 0.55).
narrative_ontology:measurement(jewi_su_t55, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 55, 0.65).
narrative_ontology:measurement(jewi_su_t65, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 65, 0.68).
narrative_ontology:measurement(jewi_su_t75, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 75, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_sovereignty_palestine__liberal_nationalist_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_sovereignty_palestine__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_sovereignty_palestine__religious_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_sovereignty_palestine__cultural_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_sovereignty_palestine__post_zionist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the jewish_sovereignty_palestine kernel family. Each sibling reading shares the same referent (Jewish sovereignty in Palestine) but authors a distinct epsilon, beneficiary/victim structure, and claimed type based on its normative grounding. The liberal nationalist reading is distinguished by its secular self-determination framework and its structural incorporation of Palestinians as co-equal claimants.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
