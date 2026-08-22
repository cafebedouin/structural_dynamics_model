% ============================================================================
% CONSTRAINT STORY: kodashim_corpus__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_corpus__performance_only, []).

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
 *   constraint_id: kodashim_corpus__performance_only
 *   human_readable: Kodashim Corpus as Dormant Blueprint — Performance-Only Reading
 *   domain: religious/commitment_system_theory
 *
 * SUMMARY:
 *   Within rabbinic Judaism, the Kodashim corpus — the talmudic orders
 *   codifying altar service, offerings, and ritual purity — is maintained by
 *   academies, restoration institutes, and donor networks under a framing
 *   that treats it as an archived operating blueprint whose execution awaits
 *   messianic restoration. This story instantiates the performance-only
 *   reading of that kernel: the corpus is a husk, valid only through physical
 *   performance, currently suspended, and studied strictly as preparation.
 *   Assessed by that reading's own lights, the standing arrangement —
 *   institutional maintenance of the corpus under the preparation framing —
 *   runs on an indefinitely deferred payoff: institutions collect tuition,
 *   donations, and legitimacy against a resumption they cannot schedule or
 *   deliver, while devotees allocate decades of study and money to procedure
 *   they will never perform. Epsilon's referent is that standing arrangement,
 *   not any endorsed alternative. Per the epsilon-invariance principle this
 *   is one member of a three-story family: the study-as-exercise and
 *   substitution-archive readings are separate constraints (separate files)
 *   with their own epsilon values, linked through
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - messianic_preparation_institutions: agenda-setting custodian (institutional / identity_locked) — administers the corpus-as-blueprint framing and collects its revenues
 *   - sacrificial_law_expert_class: concentrated beneficiary (organized / identity_locked) — authority and livelihood ride on corpus mastery
 *   - kodashim_specialist_students: primary target (powerless / identity_locked) — bears misallocated devotion and tuition
 *   - temple_restoration_donors: secondary target (moderate / constrained) — funds restoration against an undated promise
 *   - priestly_lineage_trainees: dual-positioned participant (moderate / identity_locked) — receives present status, bears locked opportunity cost
 *   - restoration_skeptics_in_pews: excluded voice (powerless / constrained) — doubts silently from inside the funding base
 *   - comparative_religion_analysts: analytical observer (analytical / analytical) — sees the full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_corpus__performance_only, 0.78).
domain_priors:suppression_score(kodashim_corpus__performance_only, 0.68).
domain_priors:theater_ratio(kodashim_corpus__performance_only, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, extractiveness, 0.78).
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_corpus__performance_only, snare).
narrative_ontology:human_readable(kodashim_corpus__performance_only, "Kodashim Corpus as Dormant Blueprint — Performance-Only Reading").
narrative_ontology:topic_domain(kodashim_corpus__performance_only, "religious/commitment_system_theory").

domain_priors:requires_active_enforcement(kodashim_corpus__performance_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_corpus__performance_only, '7cfc32e2-128c-495c-90f7-2e79f4cef440').
narrative_ontology:cs_kernel_codification('7cfc32e2-128c-495c-90f7-2e79f4cef440', fixed_text).
narrative_ontology:cs_authority_grounding('7cfc32e2-128c-495c-90f7-2e79f4cef440', lineage).
narrative_ontology:cs_interpretation_layer_present('7cfc32e2-128c-495c-90f7-2e79f4cef440').
narrative_ontology:cs_reading_relation('7cfc32e2-128c-495c-90f7-2e79f4cef440', kodashim_corpus__study_as_exercise, forecloses).
narrative_ontology:cs_reading_relation('7cfc32e2-128c-495c-90f7-2e79f4cef440', kodashim_corpus__substitution_archive, forecloses).
narrative_ontology:cs_axiom('7cfc32e2-128c-495c-90f7-2e79f4cef440', foundational, physical_performance_exclusive_validity).
narrative_ontology:cs_axiom_status(physical_performance_exclusive_validity, holdable).
narrative_ontology:cs_axiom_grounding('7cfc32e2-128c-495c-90f7-2e79f4cef440', physical_performance_exclusive_validity, theological).
narrative_ontology:cs_axiom('7cfc32e2-128c-495c-90f7-2e79f4cef440', foundational, study_is_preparation_not_performance).
narrative_ontology:cs_axiom_status(study_is_preparation_not_performance, holdable).
narrative_ontology:cs_axiom_grounding('7cfc32e2-128c-495c-90f7-2e79f4cef440', study_is_preparation_not_performance, instrumental).
narrative_ontology:cs_reference_frame('7cfc32e2-128c-495c-90f7-2e79f4cef440', suspended_service_blueprint).
narrative_ontology:cs_drift_state('7cfc32e2-128c-495c-90f7-2e79f4cef440', contemporary_restoration_activism, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('7cfc32e2-128c-495c-90f7-2e79f4cef440', '').
narrative_ontology:cs_kernel_id(kodashim_corpus__performance_only, kodashim_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_corpus__performance_only, messianic_preparation_institutions).
narrative_ontology:constraint_beneficiary(kodashim_corpus__performance_only, sacrificial_law_expert_class).
narrative_ontology:constraint_beneficiary(kodashim_corpus__performance_only, priestly_lineage_trainees).
narrative_ontology:constraint_victim(kodashim_corpus__performance_only, kodashim_specialist_students).
narrative_ontology:constraint_victim(kodashim_corpus__performance_only, temple_restoration_donors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(kodashim_corpus__performance_only, priestly_lineage_trainees).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Run the academies and institutes that teach the sacrificial orders as operative procedure awaiting resumption: they set the curriculum, certify mastery, publish restoration research (altar dimensions, vestment fabrication, purity pipelines), and raise funds on the strength of anticipated resumption. Tuition, donations, and scholarly prestige flow to them; their charter, revenue, and public standing are bound to the corpus remaining a pending blueprint. Abandoning the preparation framing would dissolve the institution's reason to exist, so none of its officers treats that as an option.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, messianic_preparation_institutions, agenda_setter,
    institutional, generational, identity_locked, global).

% Individual masters whose authority, income, and marriage-market standing derive from certified corpus mastery. They examine students, adjudicate hypothetical sacrificial cases, and staff the institutions' faculties. Their expertise commands no market outside the preparation framing, and retraining for adjacent fields would forfeit decades of accumulated standing.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, sacrificial_law_expert_class, beneficiary,
    organized, generational, identity_locked, global).

% Spend their formative decades mastering procedures they cannot perform under present conditions, paying tuition and bearing the opportunity cost of skills realizable only at an undated future resumption. Departure mid-training forfeits communal standing, arranged matches, and an already-formed identity; completion delivers standing payable only within the same framing.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, kodashim_specialist_students, payer,
    powerless, biographical, identity_locked, global).

% Fund red-heifer breeding, vestment workshops, and architectural studies; what returns to them is honored status, participation in anticipation, and assurance of place in the restored order. Their gifts are recoverable only if resumption occurs; continued giving is sustained by community recognition and a horizon that recedes as it is approached.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, temple_restoration_donors, payer,
    moderate, generational, constrained, global).

% Descendants of the priestly line who train for resumed altar service. Present status and purpose flow to them now from the preparation framing, while their career and family planning locks against a restoration date no one controls; they hold a genuine stake on both sides of the ledger.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, priestly_lineage_trainees, beneficiary,
    moderate, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(kodashim_corpus__performance_only, priestly_lineage_trainees, payer).

% Members who privately doubt that resumption will occur on any horizon but raise no objection, since questioning the preparation framing risks their own standing, their children's matches, and their belonging. They sit inside the funding base and outside the conversation.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, restoration_skeptics_in_pews, excluded,
    powerless, biographical, constrained, global).

% Scholars of religion who study the arrangement from outside the community's commitments, tracing its financing, curricula, and deferral rhetoric across archives and fieldwork. They hold no stake in resumption or refusal and can see the whole structure at once.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, comparative_religion_analysts, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kodashim_corpus__performance_only, messianic_preparation_institutions).
narrative_ontology:fixing_cost_class(kodashim_corpus__performance_only, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves a complete procedural technology (altar service) across an indefinite suspension, coordinating a dispersed population's curriculum, expectations, and boundary maintenance so the practice can resume without discontinuity; sustains communal identity and intergenerational transmission during the political impossibility of performance.
% TRANSFER_FUNCTION: Moves tuition, donations, and devoted labor from students, donors, and laity to preparation institutions and the expert class, in exchange for maintained restoration-expectation, communal standing, and assured place in the anticipated restored order.
% ABSENT_VOICES: Skeptical insiders who doubt restoration timing but stay silent to preserve standing; former members who exited and no longer sit in the study hall; non-communal scholars of the corpus whose readings carry no weight inside. They would object that the deferral is open-ended and the preparation claim unfalsifiable on any horizon.
% DISAPPEARANCE_RATIONALE: If the corpus-as-blueprint arrangement vanished overnight, yeshiva curricula would reorganize around the remaining orders, restoration projects would lose their funding base within a fundraising cycle, the expert class's certification authority would dissolve, and donor networks would redirect giving — thousands of institutional arrangements depend on the framing continuing.
% FOUNDING_PROBLEM: After the Temple's destruction, how does a sacrificial religion preserve the full operating procedure of a service it cannot perform — intact, transmissible, and executable — until performance becomes possible again?
% FOUNDING_PROBLEM_CORROBORATION: Students and donors — payers, not beneficiaries — attest the anticipation is sincerely held even while bearing its costs; historians of rabbinic literature outside the tradition corroborate that the preservation function was originally necessary. No source outside the benefiting parties attests that resumption is imminent; the imminence claim is asserted only by the institutions that collect on it.
narrative_ontology:disappearance_verdict(kodashim_corpus__performance_only, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_corpus__performance_only, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_corpus__performance_only, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kodashim_corpus__performance_only, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_corpus__performance_only, 0.78, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_corpus__performance_only_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kodashim_corpus__performance_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kodashim_corpus__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.78 at interval end) because the arrangement's returns flow against an undeliverable-on-demand future: the promise that legitimates collection has no delivery date, so collection can continue indefinitely without settlement. Suppression (0.68) is authored as a raw structural property — unscaled by power or scope — reflecting curriculum gatekeeping, communal sanction, and marriage-network pressure rather than physical coercion. Theater_ratio (0.52) reflects that, with execution impossible, just over half of observable activity is commemorative and ceremonial rehearsal rather than functional preservation work. Accessibility_collapse is low-moderate (0.38): understanding the husk-claim does not close exits — rival framings of the same corpus remain live and departure remains possible at identity cost. Resistance (0.55) records persistent intra-traditional contestation. All three tracked series run on one shared grid (t = 0, 15, 30, 45, 60, 75, mapping to 1950–2025 CE) with every metric authored at every point; trajectories rise monotonically, no cyclical oscillation is claimed, and no intermittent-reinforcement reading is offered.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setting custodian seat the same structure computes as faithful stewardship of a divine trust: preserving an executable procedure across political impossibility is the arrangement's whole point, and deferral is obedience, not predation. From the student and donor seats the identical structure computes as open-ended harvesting: payment now against settlement never. The engine computes per-seat classifications from the structural data; the divergence between those computations is the measurement this story exists to take, and the authored snare claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Preparation institutions and the expert class sit near the beneficiary end: they collect revenue, authority, and standing from the framing and control its administration, with identity-locked exit (the institution has become its function). Specialist students and restoration donors sit near the target end: they transfer money and devoted labor against a deferred settlement, with exit priced in lost standing, arranged matches, and formed identity. Priestly trainees occupy a genuinely dual position — present status flows to them (pulling toward the beneficiary end) while career and family plans lock against a date no one controls (pushing toward the target end); the declared secondary_role encodes this, and no directionality override is needed because the derivation from declared roles and exit options already places each seat correctly. Silent skeptics bear costs without a formal seat; their exclusion is recorded on the stakeholder surface, not converted into a classification input.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preserving an executable sacrificial procedure across an indefinite suspension — is still live: no restoration has occurred, so the preservation mandate has not outlived its stated function, and no mandatrophy resolution is declared. The snare classification prevents two mislabelings: reading the arrangement as rope (pure coordination of preservation) would erase the asymmetry between those who collect on the deferral and those who fund it; reading it as piton (inertial husk) would miss that enforcement is active and strengthening, with a concentrated receipt seat. Keeping the coordination function visible through the declared identity_coordination type lets the engine price the genuine preservation good separately from the extraction riding on it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'Does the high-extraction classification attach to the Kodashim kernel as such, or only to its performance-only reading?',
    'Generate and compare the sibling reading files (study_as_exercise, substitution_archive): if their computed types and epsilon values differ materially, the classification is reading-indexed and the kernel carries multiple simultaneous constraint structures.',
    'If reading-indexed, corpus-level critique or policy must address each reading separately; a single verdict on ''the Kodashim corpus'' would average over structurally distinct constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Classification is indexed to one reading of a contested kernel; sibling readings are separate constraints.').

omega_variable(
    restoration_realizability_horizon,
    'Is the promised resumption of altar service reachable on any determinate horizon, or is it indefinitely deferrable?',
    'Track whether restoration preconditions (ritual readiness, site access, certified personnel) converge toward executability or recede as each is approached; perpetual receding-under-approach indicates a deferral structure.',
    'A converging horizon supports reclassification toward scaffold (transitional preparation awaiting handover); an indefinitely receding horizon confirms the snare verdict.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(restoration_realizability_horizon, conceptual, 'Whether the legitimizing future state is realizable or structurally unfalsifiable.').

omega_variable(
    devotion_allocation_counterfactual,
    'Is devotee devotion actually misallocated, or does corpus mastery yield compensating goods (analytic training, communal standing, identity coherence) that offset the suspended performance?',
    'Longitudinal comparison of Kodashim-specialist students against matched peers on vocational, economic, and welfare outcomes, inside and outside the community.',
    'Substantial compensating goods would downgrade effective extraction toward tangled_rope; negligible compensation confirms misallocation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(devotion_allocation_counterfactual, empirical, 'Whether the devotion borne by payers is net-misallocated.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the suppression keeping payers in frame structural (communal sanction, marriage network, curriculum gatekeeping) or internalized (identity fusion making exit unthinkable)?',
    'Post-exit suppression trajectory of leavers: if deference to the framing persists after removal from enforcing communities, a substantial share is internalized.',
    'Internalized suppression raises effective suppression above the structural measure and deepens the trapped character of the payer seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Split between structural and internalized suppression mechanisms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_corpus__performance_only, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_corpus__performance_only, theater_ratio, 0, 0.34).
narrative_ontology:measurement(koda_tr_t15, kodashim_corpus__performance_only, theater_ratio, 15, 0.38).
narrative_ontology:measurement(koda_tr_t30, kodashim_corpus__performance_only, theater_ratio, 30, 0.42).
narrative_ontology:measurement(koda_tr_t45, kodashim_corpus__performance_only, theater_ratio, 45, 0.46).
narrative_ontology:measurement(koda_tr_t60, kodashim_corpus__performance_only, theater_ratio, 60, 0.49).
narrative_ontology:measurement(koda_tr_t75, kodashim_corpus__performance_only, theater_ratio, 75, 0.52).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_corpus__performance_only, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(koda_be_t15, kodashim_corpus__performance_only, base_extractiveness, 15, 0.66).
narrative_ontology:measurement(koda_be_t30, kodashim_corpus__performance_only, base_extractiveness, 30, 0.7).
narrative_ontology:measurement(koda_be_t45, kodashim_corpus__performance_only, base_extractiveness, 45, 0.73).
narrative_ontology:measurement(koda_be_t60, kodashim_corpus__performance_only, base_extractiveness, 60, 0.76).
narrative_ontology:measurement(koda_be_t75, kodashim_corpus__performance_only, base_extractiveness, 75, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t0, kodashim_corpus__performance_only, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(koda_su_t15, kodashim_corpus__performance_only, suppression_requirement, 15, 0.58).
narrative_ontology:measurement(koda_su_t30, kodashim_corpus__performance_only, suppression_requirement, 30, 0.61).
narrative_ontology:measurement(koda_su_t45, kodashim_corpus__performance_only, suppression_requirement, 45, 0.63).
narrative_ontology:measurement(koda_su_t60, kodashim_corpus__performance_only, suppression_requirement, 60, 0.66).
narrative_ontology:measurement(koda_su_t75, kodashim_corpus__performance_only, suppression_requirement, 75, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_corpus__performance_only, identity_coordination).
narrative_ontology:affects_constraint(kodashim_corpus__performance_only, kodashim_corpus__study_as_exercise).
narrative_ontology:affects_constraint(kodashim_corpus__performance_only, kodashim_corpus__substitution_archive).

% DUAL FORMULATION NOTE:
% Constraint family: the natural-language label 'the Kodashim corpus' conflates three structurally distinct claims about one fixed text. This file authors the performance-only reading (dormant blueprint awaiting resumption; high epsilon; snare-flavored). The study-as-exercise reading (kernel occupied through study; study is the performance) and the substitution-archive reading (sacrifice superseded by prayer and study; memorial archive) are separate stories with their own epsilon values, beneficiary structures, and classifications. Each file links the other two via network.affects_constraints; upstream/downstream pressure runs from whichever reading a given community adopts to the resources available to the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
