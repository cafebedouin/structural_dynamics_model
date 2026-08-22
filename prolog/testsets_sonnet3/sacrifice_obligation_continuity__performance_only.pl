% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_continuity__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_continuity__performance_only, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: sacrifice_obligation_continuity__performance_only
 *   human_readable: Sacrifice Obligation as Unfulfillable Physical Performance Requirement
 *   domain: religious_law/ritual_studies/textual_tradition
 *
 * SUMMARY:
 *   This story instantiates one reading — performance_only — of the contested
 *   kernel governing the continuity of the sacrificial obligation after the
 *   Temple's destruction. On this reading, the commandment to offer sacrifice
 *   is a positive obligation whose only recognized fulfillment is physical
 *   performance at the Temple; study of the sacrificial laws, however
 *   intensive, is preparatory scholarship for a future restored performance,
 *   not a substitute satisfaction. This places every adherent living without
 *   an operative Temple in a structurally unfulfillable position: bound by an
 *   obligation they cannot discharge, indefinitely. The reading is authored
 *   as its own clean, ε-invariant constraint — it does not describe or
 *   average over the sibling readings (study_as_performance,
 *   messianic_suspension, archival_preservation), each of which is a separate
 *   constraint with its own beneficiary/victim structure and its own ε.
 *
 * KEY AGENTS:
 *   - current_generation_adherents: primary target (powerless/identity_locked) — bears the unfulfillable-obligation burden
 *   - temple_restoration_institutions: primary beneficiary (organized/arbitrage) — draws purpose and resources from unresolved restoration urgency
 *   - rabbinic_authorities_administering_study_curricula: secondary beneficiary and agenda-setter (institutional/mobile) — administers which reading is taught
 *   - study_as_performance_adherents: excluded voice (moderate/constrained) — holds the competing resolution that would relieve the burden
 *   - textual_tradition_scholars: analytical observer (analytical/analytical) — documents the reading's history without adjudicating it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_continuity__performance_only, 0.71).
domain_priors:suppression_score(sacrifice_obligation_continuity__performance_only, 0.62).
domain_priors:theater_ratio(sacrifice_obligation_continuity__performance_only, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, extractiveness, 0.71).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, resistance, 0.47).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_continuity__performance_only, tangled_rope).
narrative_ontology:human_readable(sacrifice_obligation_continuity__performance_only, "Sacrifice Obligation as Unfulfillable Physical Performance Requirement").
narrative_ontology:topic_domain(sacrifice_obligation_continuity__performance_only, "religious_law/ritual_studies/textual_tradition").

domain_priors:requires_active_enforcement(sacrifice_obligation_continuity__performance_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_continuity__performance_only, 'a53861a3-b437-4277-862c-df3f48a75ee2').
narrative_ontology:cs_kernel_codification('a53861a3-b437-4277-862c-df3f48a75ee2', fixed_text).
narrative_ontology:cs_authority_grounding('a53861a3-b437-4277-862c-df3f48a75ee2', lineage).
narrative_ontology:cs_interpretation_layer_present('a53861a3-b437-4277-862c-df3f48a75ee2').
narrative_ontology:cs_reading_relation('a53861a3-b437-4277-862c-df3f48a75ee2', sacrifice_obligation_continuity__study_as_performance, forecloses).
narrative_ontology:cs_reading_relation('a53861a3-b437-4277-862c-df3f48a75ee2', sacrifice_obligation_continuity__messianic_suspension, coexists_with).
narrative_ontology:cs_reading_relation('a53861a3-b437-4277-862c-df3f48a75ee2', sacrifice_obligation_continuity__archival_preservation, influences).
narrative_ontology:cs_axiom('a53861a3-b437-4277-862c-df3f48a75ee2', foundational, physical_act_is_sole_satisfaction).
narrative_ontology:cs_axiom_status(physical_act_is_sole_satisfaction, holdable).
narrative_ontology:cs_axiom_grounding('a53861a3-b437-4277-862c-df3f48a75ee2', physical_act_is_sole_satisfaction, deontological).
narrative_ontology:cs_axiom('a53861a3-b437-4277-862c-df3f48a75ee2', secondary, textual_engagement_is_preparatory_not_constitutive).
narrative_ontology:cs_axiom_status(textual_engagement_is_preparatory_not_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('a53861a3-b437-4277-862c-df3f48a75ee2', textual_engagement_is_preparatory_not_constitutive, conventional).
narrative_ontology:cs_reference_frame('a53861a3-b437-4277-862c-df3f48a75ee2', temple_era_sacrificial_praxis).
narrative_ontology:cs_drift_state('a53861a3-b437-4277-862c-df3f48a75ee2', post_destruction_diaspora_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('a53861a3-b437-4277-862c-df3f48a75ee2', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_continuity__performance_only, sacrifice_obligation_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__performance_only, temple_restoration_institutions).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__performance_only, rabbinic_authorities_administering_study_curricula).
narrative_ontology:constraint_victim(sacrifice_obligation_continuity__performance_only, current_generation_adherents).
narrative_ontology:constraint_vindicates(sacrifice_obligation_continuity__performance_only, physical_performance_supremacy_doctrine).
narrative_ontology:constraint_vindicates(sacrifice_obligation_continuity__performance_only, temple_centrality_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Are bound by a commandment structure that, on this reading, they cannot fulfill in their lifetimes because the Temple does not exist and physical performance is the only recognized satisfaction. They study the sacrificial law in detail, knowing on this reading that study is preparatory rather than constitutive of the obligation. They carry the weight of an unfulfilled positive commandment with no available remedy, and exit from the framework means abandoning an identity-constituting religious commitment.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, current_generation_adherents, payer,
    powerless, biographical, identity_locked, global).

% Organizations and movements oriented toward eventual Temple rebuilding derive their purpose, funding, and communal standing from the continued unfulfilled status of the obligation. The reading that only physical performance satisfies the commandment sustains the felt urgency of restoration projects, fundraising, and political advocacy for rebuilding, and administers how the obligation is taught and transmitted.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, temple_restoration_institutions, beneficiary,
    organized, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_continuity__performance_only, temple_restoration_institutions, agenda_setter).

% Set and enforce which reading of the obligation is taught within their institutions. Under performance_only, they retain authority to declare study valuable-but-insufficient, which preserves their gatekeeping role over both the study curriculum and the deferred judgment about what would count as adequate restoration. They are largely insulated from the psychological cost the doctrine imposes on ordinary adherents.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, rabbinic_authorities_administering_study_curricula, beneficiary,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_continuity__performance_only, rabbinic_authorities_administering_study_curricula, agenda_setter).

% Hold the sibling view that textual study itself constitutes fulfillment. Within a performance_only-administered institution their position is treated as pious but doctrinally insufficient; their argument that guilt without remedy is itself evidence against the performance-only reading is not accepted as authoritative within this framework.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, study_as_performance_adherents, excluded,
    moderate, biographical, constrained, global).

% Study how the obligation-continuity kernel has been read across historical periods, tracking how communities absent a functioning Temple have resolved or failed to resolve the unfulfillability problem. They document but do not adjudicate between readings.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, textual_tradition_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_obligation_continuity__performance_only, temple_restoration_institutions).
narrative_ontology:fixing_cost_class(sacrifice_obligation_continuity__performance_only, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves detailed transmission of sacrificial law and Temple procedure across generations so that, should restoration become possible, the community retains the operational knowledge to perform it correctly and immediately.
% TRANSFER_FUNCTION: Moves psychological and social costs — unresolved guilt, unfulfilled-commandment status, deferred moral standing — onto ordinary adherents living in the present, while transferring institutional legitimacy, fundraising capacity, and gatekeeping authority to restoration-oriented institutions and the rabbinic authorities who administer the reading.
% ABSENT_VOICES: Adherents who hold the study_as_performance reading are present in the community but not authoritative within institutions committed to performance_only; their argument would relieve the current generation's unfulfillable status but is treated as a lesser or apologetic position rather than a competing final answer.
% DISAPPEARANCE_RATIONALE: If the performance_only reading were abandoned in favor of study_as_performance or messianic_suspension, the psychological burden on current adherents would lift, restoration-fundraising urgency would likely soften, and rabbinic institutions would lose a distinctive claim to gatekeeping authority over what counts as adequate religious performance.
% FOUNDING_PROBLEM: The sacrificial cult ceased with the Temple's destruction, and the tradition needed to explain how a commandment requiring a now-impossible physical act remains binding without either declaring the law abrogated or declaring the community in permanent violation.
% FOUNDING_PROBLEM_CORROBORATION: Restoration institutions and administering rabbinic authorities attest the founding problem remains fully live and requires physical performance to resolve. Adherents holding the study_as_performance reading, and secular textual-tradition scholars documenting the history of the doctrine, attest from outside the performance_only institutional structure that the 'physical performance only' answer is one contested solution among several rather than the self-evidently correct resolution, and that its persistence serves institutional functions independent of its textual necessity.
narrative_ontology:disappearance_verdict(sacrifice_obligation_continuity__performance_only, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_continuity__performance_only, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_continuity__performance_only, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sacrifice_obligation_continuity__performance_only, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_continuity__performance_only, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_continuity__performance_only_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sacrifice_obligation_continuity__performance_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sacrifice_obligation_continuity__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.71 at interval end) because the reading manufactures an unresolvable moral debt in the present generation: guilt is generated by the obligation's structure but no performance is available to discharge it, and the study alternative is explicitly denied satisfaction-status. Suppression (0.62) reflects that abandoning the performance_only reading in favor of a satisfaction-granting alternative is treated within performance_only-administering institutions as doctrinally illegitimate, foreclosing the exit that would relieve the burden. Theater ratio (0.44) is moderate-rising: intensive textual study of sacrificial procedure is maintained and even intensified as institutional practice, but on this reading's own terms that maintenance does not resolve the underlying claim it is theater relative to — a performative preparation for something that, on this reading, has not happened and cannot happen without external restoration.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setting institutional seat, this reading is a coherent theological structure: fidelity today is measured by preserved readiness, and unfulfillability is a feature of exile, not a defect of the doctrine. From the current-generation-adherent seat, the identical structure computes as a standing extraction of guilt with no offered remedy — the obligation is real, binding, and permanently out of reach through any act available to them. The engine is expected to compute these as structurally different seat outcomes from the same authored data; this divergence is exactly what a tangled_rope classification is meant to register — genuine transmission/coordination function (preserving sacrificial knowledge) riding alongside asymmetric extraction (unresolvable guilt concentrated on the powerless, unfulfillable-status-holding seat).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (restoration institutions, administering rabbinic authorities) hold organized/institutional power with mobile-to-arbitrage exit — they set or benefit from the reading and are not themselves exposed to the unfulfillable-obligation status in the way ordinary adherents are; their directionality sits near the beneficiary end. Current-generation adherents are powerless with identity-locked exit — leaving the framework means abandoning a constitutive religious identity, not merely switching providers — so their directionality sits near the full-target end, and the engine's amplification for trapped/identity-locked targets is appropriate here rather than an artifact.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — how to maintain the sacrificial commandment's bindingness after the Temple's destruction — is contested rather than dead: restoration-oriented seats attest it is fully live, while adherents holding the study-as-performance reading and outside textual scholars attest that the performance_only resolution is a policy choice serving institutional continuity rather than the only textually necessitated answer. This mismatch (contested founding-problem status against a persisting, cost-imposing arrangement) is the signal the tangled_rope classification is built to catch: it prevents the story from either dismissing the reading as pure fabricated extraction (it does preserve real transmitted knowledge) or from certifying it as pure benign coordination (it does impose asymmetric, unrelieved cost on the least powerful seat).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    performance_only_vs_study_as_performance_textual_warrant,
    'Does the authoritative textual tradition actually require physical performance as the sole satisfaction of the sacrificial commandment, or is this a later institutional consolidation of what was originally a more open interpretive question?',
    'Comparative analysis of early rabbinic sources on obligation-suspension versus performance-exclusivity, cross-checked against which reading correlates historically with periods of stronger versus weaker centralized rabbinic authority.',
    'If the performance-exclusivity reading is shown to be a later consolidation rather than an original textual necessity, the extraction this story documents is better characterized as institutionally constructed rather than doctrinally inevitable, strengthening the case for reclassification toward snare on repeated measurement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performance_only_vs_study_as_performance_textual_warrant, conceptual, 'Whether physical-performance exclusivity is textually necessitated or institutionally selected among live alternatives.').

omega_variable(
    kernel_reading_selection_mechanism,
    'Which structural or institutional factors determine which of the four sibling readings (performance_only, study_as_performance, messianic_suspension, archival_preservation) a given community or authority adopts, and does that selection track anything other than which institutions the selecting authority administers?',
    'Cross-community survey correlating denominational/institutional affiliation with reading adopted, checking whether adoption correlates with the adopting institution''s stake in restoration-oriented fundraising or curricular gatekeeping.',
    'If reading-selection correlates strongly with institutional self-interest rather than independent textual argument, this supports treating performance_only''s persistence as partly extraction-serving rather than purely doctrinal; if selection tracks independent hermeneutic argument, the coordination function should be weighted more heavily.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_mechanism, empirical, 'Whether kernel-reading selection tracks institutional interest or independent textual argument.').

omega_variable(
    guilt_without_remedy_psychological_measurement,
    'Is the psychological burden this reading imposes on current-generation adherents (guilt over an unfulfillable obligation) an actual, measurable lived experience across the relevant population, or is it a theoretical construct emphasized more by external critics than by adherents themselves?',
    'Qualitative and survey research among adherents who hold the performance_only reading, asking directly about felt obligation-guilt versus felt readiness/anticipation framing.',
    'If adherents report the anticipatory framing (readiness, not guilt) as their lived experience, the extractiveness score authored here may overstate the psychological cost component relative to what this reading''s own practitioners report; if guilt is widely reported, the extraction claim is corroborated independently of institutional critique.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(guilt_without_remedy_psychological_measurement, empirical, 'Whether the unfulfillable-obligation burden is an authored inference or a corroborated lived experience.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_continuity__performance_only, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_continuity__performance_only, theater_ratio, 0, 0.28).
narrative_ontology:measurement(sacr_tr_t12, sacrifice_obligation_continuity__performance_only, theater_ratio, 12, 0.32).
narrative_ontology:measurement(sacr_tr_t24, sacrifice_obligation_continuity__performance_only, theater_ratio, 24, 0.36).
narrative_ontology:measurement(sacr_tr_t36, sacrifice_obligation_continuity__performance_only, theater_ratio, 36, 0.39).
narrative_ontology:measurement(sacr_tr_t48, sacrifice_obligation_continuity__performance_only, theater_ratio, 48, 0.42).
narrative_ontology:measurement(sacr_tr_t60, sacrifice_obligation_continuity__performance_only, theater_ratio, 60, 0.44).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_continuity__performance_only, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(sacr_be_t12, sacrifice_obligation_continuity__performance_only, base_extractiveness, 12, 0.6).
narrative_ontology:measurement(sacr_be_t24, sacrifice_obligation_continuity__performance_only, base_extractiveness, 24, 0.64).
narrative_ontology:measurement(sacr_be_t36, sacrifice_obligation_continuity__performance_only, base_extractiveness, 36, 0.67).
narrative_ontology:measurement(sacr_be_t48, sacrifice_obligation_continuity__performance_only, base_extractiveness, 48, 0.69).
narrative_ontology:measurement(sacr_be_t60, sacrifice_obligation_continuity__performance_only, base_extractiveness, 60, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_obligation_continuity__performance_only, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(sacr_su_t12, sacrifice_obligation_continuity__performance_only, suppression_requirement, 12, 0.54).
narrative_ontology:measurement(sacr_su_t24, sacrifice_obligation_continuity__performance_only, suppression_requirement, 24, 0.57).
narrative_ontology:measurement(sacr_su_t36, sacrifice_obligation_continuity__performance_only, suppression_requirement, 36, 0.59).
narrative_ontology:measurement(sacr_su_t48, sacrifice_obligation_continuity__performance_only, suppression_requirement, 48, 0.61).
narrative_ontology:measurement(sacr_su_t60, sacrifice_obligation_continuity__performance_only, suppression_requirement, 60, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_continuity__performance_only, identity_coordination).
narrative_ontology:boltzmann_floor_override(sacrifice_obligation_continuity__performance_only, 0.08).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__performance_only, sacrifice_obligation_continuity__study_as_performance).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__performance_only, sacrifice_obligation_continuity__messianic_suspension).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__performance_only, sacrifice_obligation_continuity__archival_preservation).

% DUAL FORMULATION NOTE:
% This story is one of four constraints decomposed from the natural-language label 'the sacrifice obligation continuity question' (kernel: sacrifice_obligation_continuity), per the ε-invariance principle: performance_only, study_as_performance, messianic_suspension, and archival_preservation each assign a structurally different beneficiary/victim set and a different ε to the same underlying kernel text. performance_only is authored here as the highest-extraction reading because it is the only reading that keeps the obligation fully binding while denying the currently available remedy (study) any satisfaction-status, concentrating cost on current-generation adherents with no offered discharge. The other three readings each relieve some portion of that unfulfillability — study_as_performance by granting study constitutive status, messianic_suspension by reclassifying the obligation as suspended rather than violated, and archival_preservation by dissolving the obligation's normative force altogether.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
