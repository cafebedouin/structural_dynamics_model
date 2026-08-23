% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__religious_covenant_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:coordination_type/2,
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
 *   human_readable: Divine Covenant Reading of Jewish Territorial Sovereignty
 *   domain: political/religious/nationalism
 *
 * SUMMARY:
 *   This constraint story instantiates the religious covenant reading of the
 *   Jewish self-determination kernel. It treats territorial sovereignty not
 *   as a contingent political achievement but as a divine obligation grounded
 *   in biblical covenant. The reading claims mountain status (immutable
 *   divine command) but is operationalized through state power, creating
 *   structural extraction from secular negotiation frameworks and Palestinian
 *   national existence. The beneficiary/victim structure and high metric
 *   scores are deliberately independent of the mountain claim â the
 *   divergence is the signal the corpus exists to measure.
 *
 * KEY AGENTS:
 *   - religious_zionist_movement (beneficiary/identity_locked)
 *   - settlement_enterprise (beneficiary/constrained)
 *   - rabbinic_authority (agenda_setter/analytical)
 *   - palestinian_national_movement (target/trapped)
 *   - secular_israeli_negotiators (target/constrained)
 *   - international_peace_institutions (observer/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__religious_covenant_reading, 0.82).
domain_priors:suppression_score(jewish_self_determination__religious_covenant_reading, 0.78).
domain_priors:theater_ratio(jewish_self_determination__religious_covenant_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__religious_covenant_reading, mountain).
narrative_ontology:human_readable(jewish_self_determination__religious_covenant_reading, "Divine Covenant Reading of Jewish Territorial Sovereignty").
narrative_ontology:topic_domain(jewish_self_determination__religious_covenant_reading, "political/religious/nationalism").

domain_priors:emerges_naturally(jewish_self_determination__religious_covenant_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__religious_covenant_reading, '4b6330b7-cdf3-43d6-a0f4-f38c89d8325e').
narrative_ontology:cs_kernel_codification('4b6330b7-cdf3-43d6-a0f4-f38c89d8325e', fixed_text).
narrative_ontology:cs_authority_grounding('4b6330b7-cdf3-43d6-a0f4-f38c89d8325e', lineage).
narrative_ontology:cs_interpretation_layer_present('4b6330b7-cdf3-43d6-a0f4-f38c89d8325e').
narrative_ontology:cs_reading_relation('4b6330b7-cdf3-43d6-a0f4-f38c89d8325e', jewish_self_determination__diasporist_reading, forecloses).
narrative_ontology:cs_reading_relation('4b6330b7-cdf3-43d6-a0f4-f38c89d8325e', jewish_self_determination__settler_colonial_reading, influences).
narrative_ontology:cs_reading_relation('4b6330b7-cdf3-43d6-a0f4-f38c89d8325e', jewish_self_determination__indigenous_return_reading, coexists_with).
narrative_ontology:cs_reading_relation('4b6330b7-cdf3-43d6-a0f4-f38c89d8325e', jewish_self_determination__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_axiom('4b6330b7-cdf3-43d6-a0f4-f38c89d8325e', foundational, territorial_sovereignty_as_divine_obligation).
narrative_ontology:cs_axiom_status(territorial_sovereignty_as_divine_obligation, holdable).
narrative_ontology:cs_axiom_grounding('4b6330b7-cdf3-43d6-a0f4-f38c89d8325e', territorial_sovereignty_as_divine_obligation, theological).
narrative_ontology:cs_axiom('4b6330b7-cdf3-43d6-a0f4-f38c89d8325e', foundational, secular_negotiation_covenant_invalid).
narrative_ontology:cs_axiom_status(secular_negotiation_covenant_invalid, holdable).
narrative_ontology:cs_axiom_grounding('4b6330b7-cdf3-43d6-a0f4-f38c89d8325e', secular_negotiation_covenant_invalid, theological).
narrative_ontology:cs_reference_frame('4b6330b7-cdf3-43d6-a0f4-f38c89d8325e', divine_covenant_authority).
narrative_ontology:cs_drift_state('4b6330b7-cdf3-43d6-a0f4-f38c89d8325e', contemporary_secular_state_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4b6330b7-cdf3-43d6-a0f4-f38c89d8325e', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__religious_covenant_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__religious_covenant_reading, religious_zionist_movement).
narrative_ontology:constraint_beneficiary(jewish_self_determination__religious_covenant_reading, settlement_enterprise).
narrative_ontology:constraint_victim(jewish_self_determination__religious_covenant_reading, palestinian_national_movement).
narrative_ontology:constraint_victim(jewish_self_determination__religious_covenant_reading, secular_israeli_negotiators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Derives political power, state resources, and territorial expansion from the divine covenant framing. Religious identity is fused with territorial maximalism; exit from the covenant framework would mean abandoning a core tenet of their worldview and political project.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, religious_zionist_movement, beneficiary,
    powerful, civilizational, identity_locked, national).

% Receives land, housing subsidies, and military protection justified by the divine covenant claim. Economic and communal infrastructure is built on contested territory; exit would require dismantling communities and forfeiting sunk investment.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, settlement_enterprise, beneficiary,
    organized, generational, constrained, regional).

% Interprets and administers the divine covenant framework through halakhic rulings, religious education, and political endorsements. Sets the theological parameters for which territorial concessions are permissible. Can reinterpret but operates within the covenant paradigm.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, rabbinic_authority, agenda_setter,
    institutional, civilizational, analytical, national).

% Bears the cost of dispossession, displacement, and denial of self-determination under a sovereignty claim that treats their presence as temporally illegitimate. Exit options include fragmented autonomy, prolonged statelessness, or armed resistance, all heavily constrained by military and legal enforcement of the covenant-based territorial claim.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, palestinian_national_movement, payer,
    powerless, generational, trapped, national).

% Secular political parties and citizens who seek territorial compromise through negotiation find their framework foreclosed by the religious covenant claim. Democratic institutions and electoral coalitions are constrained by the necessity of accommodating religious parties who wield veto power over concessions.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, secular_israeli_negotiators, payer,
    moderate, biographical, constrained, national).

% Observe and attempt to mediate the conflict through international law and human rights frameworks. They treat the divine covenant claim as illegitimate in secular international law but lack enforcement capacity to alter the territorial arrangement.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, international_peace_institutions, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective Jewish territorial commitment by grounding it in transcendent obligation rather than contingent political agreement, solving the problem of intergenerational continuity and sacrifice without relying on secular state legitimacy.
% TRANSFER_FUNCTION: Moves territorial control, state resources, and political legitimacy from secular negotiation frameworks and Palestinian habitation to religious settlement institutions and the maximalist territorial project.
% ABSENT_VOICES: Palestinian refugees and exiles are structurally excluded from the covenant framework; diaspora Jewish anti-Zionist and secular voices who reject divine territorial obligation are marginalized within the political coalition.
% DISAPPEARANCE_RATIONALE: If the divine covenant constraint vanished, the theological justification for territorial maximalism would collapse, enabling compromise frameworks that are currently foreclosed. The settlement enterprise would lose its primary legitimating narrative, and Israeli politics would likely reorganize around civic or security-based rather than metaphysical grounds.
% FOUNDING_PROBLEM: Jewish collective existence in the modern era threatened by assimilation, genocide, and statelessness; need for an indestructible claim to territory that transcends secular political fortune and justifies national return.
% FOUNDING_PROBLEM_CORROBORATION: Religious Zionist theologians attest the founding problem is live and the covenant is the answer. Secular Israeli historians, Palestinian scholars, and international legal institutions outside the benefiting parties attest the founding problem is a modern political construction and the covenant framework serves territorial expansion; no independent corroboration of divine mandate exists outside the theological tradition.
narrative_ontology:disappearance_verdict(jewish_self_determination__religious_covenant_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_self_determination__religious_covenant_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__religious_covenant_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jewish_self_determination__religious_covenant_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_self_determination__religious_covenant_reading, 0.82, 'kimi-k2.6', 'none', direct).

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
 *   Accessibility collapse is very high (0.88) because within the covenant framework, territorial compromise is metaphysically illegitimate â alternatives collapse completely once the premise is accepted. Resistance is high (0.72) because Palestinian national movement, secular Israelis, and international institutions actively contest the constraint. Theater ratio (0.55) reflects the growing gap between the covenant's theological purity and its operationalization through mundane state violence and bureaucracy. Extractiveness (0.82) captures the degree to which the divine claim extracts from secular political possibility and Palestinian self-determination.
 *
 * PERSPECTIVAL GAP:
 *   From the religious Zionist seat, the constraint is a sacred duty that coordinates collective survival across generations â a mountain. From the Palestinian and secular Israeli seats, the same structure operates as an enforced extraction of land and political agency that forecloses negotiation. The engine computes this divergence from the structural data: identity-locked beneficiaries with civilizational time horizons versus trapped and constrained payers.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious Zionist movement and settlement enterprise are structural beneficiaries (d near 0.0 â the constraint subsidizes their territorial and political project). Palestinian national movement and secular Israeli negotiators are targets (d near 1.0 â the constraint extracts land and political options from them). Rabbinic authority sits near symmetric but low, as both interpreter and beneficiary of the framework's persistence. International observers sit at analytical distance.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling by separating the covenant's claimed immutability (mountain) from its operational characteristics (tangled_rope/snare via metrics). If the constraint were purely theological (private religious belief), it would be a low-extraction mountain. Its entanglement with state enforcement, settlement allocation, and legal discrimination transforms it into an extractive structure while maintaining the mountain claim â the false summit pattern.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_origin_vs_political_construction,
    'Is the divine covenant a genuine metaphysical constraint or a constructed political narrative serving territorial expansion?',
    'Comparative historical analysis of when the covenant-land nexus became politically operative versus its textual origins; examination of whether the constraint persists without political beneficiaries.',
    'If constructed, the mountain claim is a false summit and the constraint reclassifies as tangled_rope or snare; if genuine, the high extraction metrics represent necessary cost rather than rent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_origin_vs_political_construction, empirical, 'Whether the divine covenant is natural law or constructed ideology').

omega_variable(
    operationalization_ambiguity,
    'Does the religious covenant remain a theological abstraction, or does its entanglement with state military, legal, and settlement apparatus make it a coercive extractive structure?',
    'Disaggregation of state actions justified by covenant versus those justified by security or civic nationalism; measurement of resource flows to settlement enterprise.',
    'If purely theological, effective extraction is lower than base epsilon suggests; if fully operationalized, the gap between mountain claim and tangled_rope operation is the classification signal.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(operationalization_ambiguity, conceptual, 'Theological abstraction versus state operationalization').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the constraint''s persistence enforced primarily by state violence and legal discrimination (structural), or by theological internalization and identity fusion (internalized)?',
    'Post-withdrawal or post-demobilization trajectory analysis: if territorial claims persist via religious education and identity norms after state enforcement relaxes, suppression is partially internalized.',
    'If internalized, effective suppression exceeds structural measures because the target population carries the constraint even where state enforcement is absent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized enforcement mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__religious_covenant_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t0, jewish_self_determination__religious_covenant_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(jewi_tr_t15, jewish_self_determination__religious_covenant_reading, theater_ratio, 15, 0.22).
narrative_ontology:measurement(jewi_tr_t30, jewish_self_determination__religious_covenant_reading, theater_ratio, 30, 0.3).
narrative_ontology:measurement(jewi_tr_t45, jewish_self_determination__religious_covenant_reading, theater_ratio, 45, 0.38).
narrative_ontology:measurement(jewi_tr_t60, jewish_self_determination__religious_covenant_reading, theater_ratio, 60, 0.46).
narrative_ontology:measurement(jewi_tr_t75, jewish_self_determination__religious_covenant_reading, theater_ratio, 75, 0.55).

% Extraction over time
narrative_ontology:measurement(jewi_be_t0, jewish_self_determination__religious_covenant_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(jewi_be_t15, jewish_self_determination__religious_covenant_reading, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(jewi_be_t30, jewish_self_determination__religious_covenant_reading, base_extractiveness, 30, 0.55).
narrative_ontology:measurement(jewi_be_t45, jewish_self_determination__religious_covenant_reading, base_extractiveness, 45, 0.62).
narrative_ontology:measurement(jewi_be_t60, jewish_self_determination__religious_covenant_reading, base_extractiveness, 60, 0.72).
narrative_ontology:measurement(jewi_be_t75, jewish_self_determination__religious_covenant_reading, base_extractiveness, 75, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t0, jewish_self_determination__religious_covenant_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(jewi_su_t15, jewish_self_determination__religious_covenant_reading, suppression_requirement, 15, 0.52).
narrative_ontology:measurement(jewi_su_t30, jewish_self_determination__religious_covenant_reading, suppression_requirement, 30, 0.58).
narrative_ontology:measurement(jewi_su_t45, jewish_self_determination__religious_covenant_reading, suppression_requirement, 45, 0.65).
narrative_ontology:measurement(jewi_su_t60, jewish_self_determination__religious_covenant_reading, suppression_requirement, 60, 0.72).
narrative_ontology:measurement(jewi_su_t75, jewish_self_determination__religious_covenant_reading, suppression_requirement, 75, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__religious_covenant_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
