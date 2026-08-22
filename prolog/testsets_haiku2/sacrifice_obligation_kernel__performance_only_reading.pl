% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_kernel__performance_only_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_kernel__performance_only_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: sacrifice_obligation_kernel__performance_only_reading
 *   human_readable: Sacrifice Obligation: Performance-Only Reading
 *   domain: religious_law/halakhic_authority
 *
 * SUMMARY:
 *   The sacrifice obligation (korban) is a foundational command in Torah:
 *   Israel is commanded to bring animal sacrifices for atonement, communion,
 *   and thanksgiving. The Second Temple was destroyed in 70 CE; animal
 *   sacrifice became impossible. The Jewish tradition produced four major
 *   readings of this rupture, each providing a different halakhic and
 *   theological path. This constraint instantiates ONE reading: the
 *   performance-only reading holds that the obligation remains unchanged and
 *   unfulfilled. Study of sacrifice law, replacement liturgies, and symbolic
 *   practices do not discharge it. The obligation persists as a binding
 *   command that cannot be executed — a 1,900-year structural impossibility
 *   that the reading maintains as the correct interpretation of Torah's
 *   immutability. The claim/metric gap is structural: this reading is CLAIMED
 *   as a legitimate halakhic position (which it is, among competing readings)
 *   while the authored metrics describe the constraint's actual structural
 *   operation: extreme extractiveness (perpetual non-compliance), high
 *   suppression (alternative readings are subordinated), rising theater
 *   (liturgical and study-based substitute practices occupy the space the
 *   obligation would if it could be fulfilled). The performance-only reading
 *   is one choice among several; this constraint models it as its own logical
 *   system.
 *
 * KEY AGENTS:
 *   - Jewish people post-70 CE: trapped in an obligation to perform sacrifice that is structurally impossible; identity-locked to the covenant; no exit; obligated but unable to comply
 *   - Rabbinic authority (performance-only faction): maintains and enforces the reading; does not benefit materially but administers the interpretation; observes that alternative readings exist but insists on the performance-only frame
 *   - Competing readings: present in tradition but subordinated; study-as-exercise, messianic-suspension, symbolic-archive readings would change the obligation's structure if adopted
 *   - Divine authority (kernel): the Torah itself, upheld as unchanged and literal; no agent benefits, but the framework's integrity (commandment immutability) is vindicated by maintaining the reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_kernel__performance_only_reading, 0.92).
domain_priors:suppression_score(sacrifice_obligation_kernel__performance_only_reading, 0.78).
domain_priors:theater_ratio(sacrifice_obligation_kernel__performance_only_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, extractiveness, 0.92).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_kernel__performance_only_reading, snare).
narrative_ontology:human_readable(sacrifice_obligation_kernel__performance_only_reading, "Sacrifice Obligation: Performance-Only Reading").
narrative_ontology:topic_domain(sacrifice_obligation_kernel__performance_only_reading, "religious_law/halakhic_authority").

domain_priors:requires_active_enforcement(sacrifice_obligation_kernel__performance_only_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_kernel__performance_only_reading, '5c37acba-e449-445f-a73c-d9ad58fa7808').
narrative_ontology:cs_kernel_codification('5c37acba-e449-445f-a73c-d9ad58fa7808', fixed_text).
narrative_ontology:cs_authority_grounding('5c37acba-e449-445f-a73c-d9ad58fa7808', lineage).
narrative_ontology:cs_interpretation_layer_present('5c37acba-e449-445f-a73c-d9ad58fa7808').
narrative_ontology:cs_reading_relation('5c37acba-e449-445f-a73c-d9ad58fa7808', sacrifice_obligation_kernel__study_as_exercise_reading, forecloses).
narrative_ontology:cs_reading_relation('5c37acba-e449-445f-a73c-d9ad58fa7808', sacrifice_obligation_kernel__messianic_suspension_reading, forecloses).
narrative_ontology:cs_reading_relation('5c37acba-e449-445f-a73c-d9ad58fa7808', sacrifice_obligation_kernel__symbolic_archive_reading, forecloses).
narrative_ontology:cs_axiom('5c37acba-e449-445f-a73c-d9ad58fa7808', foundational, divine_commandment_immutability).
narrative_ontology:cs_axiom_status(divine_commandment_immutability, holdable).
narrative_ontology:cs_axiom_grounding('5c37acba-e449-445f-a73c-d9ad58fa7808', divine_commandment_immutability, deontological).
narrative_ontology:cs_axiom('5c37acba-e449-445f-a73c-d9ad58fa7808', foundational, performance_exclusivity_of_fulfillment).
narrative_ontology:cs_axiom_status(performance_exclusivity_of_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('5c37acba-e449-445f-a73c-d9ad58fa7808', performance_exclusivity_of_fulfillment, deontological).
narrative_ontology:cs_reference_frame('5c37acba-e449-445f-a73c-d9ad58fa7808', torah_literal_obligation_framework).
narrative_ontology:cs_drift_state('5c37acba-e449-445f-a73c-d9ad58fa7808', contemporary_diaspora_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5c37acba-e449-445f-a73c-d9ad58fa7808', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_kernel__performance_only_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_victim(sacrifice_obligation_kernel__performance_only_reading, jewish_people_post_temple_destruction).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__performance_only_reading, divine_commandment_immutability).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__performance_only_reading, performance_as_exclusive_fulfillment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Commanded to perform animal sacrifice according to Torah law; Temple destroyed 70 CE and sacrifice becomes structurally impossible. Under the performance-only reading, the obligation persists unfulfilled — there is no valid substitute, study does not discharge it, and no redemptive path exists until Temple restoration. The Jewish people bear the status of perpetual non-compliance with a binding command that cannot be physically executed.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__performance_only_reading, jewish_people_post_temple_destruction, payer,
    powerless, civilizational, identity_locked, global).

% Maintains and enforces the performance-only reading of the sacrifice obligation within halakhic discourse. Holds that study, symbolic practice, and other substitutes do not discharge the mitzvah; the obligation remains binding and unfulfilled. Administers this interpretation through liturgy, legal rulings, and theological argument. Does not itself benefit from the obligation's non-fulfillment, but maintains its interpretive authority by upholding the reading's logical structure.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__performance_only_reading, rabbinic_authority_performance_only_faction, agenda_setter,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_kernel__performance_only_reading, rabbinic_authority_performance_only_faction, observer).

% Alternative halakhic readings (study-as-exercise, messianic-suspension, symbolic-archive) that would either discharge the obligation through study, suspend it until redemption, or reframe it as cultural rather than binding. These readings are present in rabbinic literature but subordinated in favor of the performance-only reading, which insists on the obligation's literal, continuing force.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__performance_only_reading, competing_study_based_readings, excluded,
    institutional, civilizational, analytical, global).

% The standing arrangement (Torah's sacrifice commandments) that the performance-only reading upholds. The reading vindicates the proposition that divine commandments retain their original form and force; no reading agent benefits materially, but the framework itself (divine immutability and commandment literalism) is vindicated structurally.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__performance_only_reading, divine_authority_kernel, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(sacrifice_obligation_kernel__performance_only_reading, divine_authority_kernel).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_obligation_kernel__performance_only_reading, diffuse).
narrative_ontology:fixing_cost_class(sacrifice_obligation_kernel__performance_only_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None — this constraint is not a coordination mechanism. It is a command structure: the obligation to perform sacrifices coordinates Israel's relationship to the divine, but under the performance-only reading that coordination is suspended, not solved.
% TRANSFER_FUNCTION: No transfer occurs — no material goods move. What persists is a status condition: the Jewish people remain obligated but unable to comply. The 'transfer' is existential: obligation without fulfillment, command without capacity.
% ABSENT_VOICES: Every halakhic reading that would dissolve or discharge the obligation is present in rabbinic literature but treated as subordinate to the performance-only reading. Study-based readings, messianic suspension, and symbolic reframing schools would testify that their readings make the obligation coherent or suspendable, but the performance-only frame excludes them from the primary authority structure.
% DISAPPEARANCE_RATIONALE: If the performance-only reading disappeared and another reading (study-as-exercise or messianic suspension) took its place, the obligation's structure would change radically — either discharged by study or suspended until redemption. The Jewish people's halakhic status would shift from perpetual non-compliance to either compliance-through-study or deferred compliance. The framework itself would reorganize.
% FOUNDING_PROBLEM: God commanded Israel to perform animal sacrifices as the central mode of atonement and worship. The Temple was destroyed; sacrifice became impossible. The performance-only reading insists that the command remains unchanged and unfulfilled — maintaining literal fidelity to the original obligation despite its structural impossibility.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic authorities of the performance-only faction testify to the persistence of the obligation and its unfulfilled status. Competing readings (study-based, messianic) from within rabbinic literature testify that the obligation requires solution, differing only on which solution is correct. Historians and textual scholars outside the benefiting parties note that the performance-only reading is one major interpretive strand, not the only one, and that its maintenance despite 1,900 years of non-fulfillment reflects a choice to uphold the reading rather than a natural law.
narrative_ontology:disappearance_verdict(sacrifice_obligation_kernel__performance_only_reading, contested).
narrative_ontology:founding_problem_status(sacrifice_obligation_kernel__performance_only_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_kernel__performance_only_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sacrifice_obligation_kernel__performance_only_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_kernel__performance_only_reading, 0.92, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_kernel__performance_only_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sacrifice_obligation_kernel__performance_only_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sacrifice_obligation_kernel__performance_only_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is extremely high (0.92) because the obligation persists unfulfilled for 1,900+ years with no valid discharge mechanism under this reading. The victim set (Jewish people) bears the status of perpetual non-compliance — not through deliberate violation but through structural impossibility. There is no beneficiary in the ordinary sense: no agent collects from this arrangement. What persists is the reading's logical structure: commandments do not change, performance is the only valid fulfillment, and therefore the obligation remains. Suppression is high (0.78) because alternative readings are present in rabbinic literature but the performance-only reading is maintained as authoritative, foreclosing other interpretive paths. Theater is rising over the interval (0.15 to 0.62) because study-based practices, liturgical substitutes, and remembrance ceremonies increasingly occupy the functional space that physical sacrifice would. The performance-only reading permits these practices as preparatory but denies they fulfill the mitzvah — they are theater, meaningful performance that does not discharge the obligation. The measurement series are sparse at early points because the reading's operation is relatively stable over centuries (high extractiveness persists; suppression slight relaxes as alternative readings gain acceptance but the framework holds); theater rises over the latter half as substitutes become more elaborate and symbolic weight accumulates.
 *
 * PERSPECTIVAL GAP:
 *   From the performance-only reading's own internal seat, the constraint is a logical necessity: if commandments are immutable and performance is the only valid fulfillment, then the obligation must persist unfulfilled. The reading does not claim to benefit from this; it claims to maintain fidelity to Torah's original form. From the seats of alternative readings (study-as-exercise, messianic suspension, symbolic archive), the same obligation is seen as either dischargeable through study, suspended by divine plan, or reframed as a cultural archive rather than a binding halakhic claim. From the seat of the Jewish people under this reading, the constraint operates as a permanent status condition: you are obligated and unable to comply, and no legitimate solution exists. The engine's per-seat computation should show the performance-only reading's own authority seat as consistent with the reading (not a victim, not extracting), while the Jewish people's seat computes as trapped, identity-locked, and bearing extreme extraction. The gap is not a disagreement about facts but about which reading's interpretive framework governs.
 *
 * DIRECTIONALITY LOGIC:
 *   The Jewish people post-70 CE are the structural victims: identity-locked (covenantal membership is non-negotiable), trapped (diaspora and statelessness foreclose escape), obligated to perform a command that is physically impossible. Their directionality is d ≈ 1.0 (full target). The rabbinic authority seat is dual-positioned: as agenda-setter (maintains the reading, administers its authority), it has agency in choosing to uphold this interpretation rather than another; as observer (the reading is not extracting benefit from them, they are maintaining a logical structure), they are not themselves victims. The alternative reading factions are excluded rather than coordinated — their voices are present in tradition but subordinated. Divine authority (the kernel itself) is a non-agent entity (agent: false) that the reading vindicates but does not benefit from. No material goods flow; the constraint is a status condition and a reading choice.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (God commanded sacrifice; Temple destroyed) is live: the obligation remains and the people remain unable to perform it. The performance-only reading keeps the problem alive by refusing to accept substitute solutions that other readings offer. This is structurally distinct from mandatrophy (where a constraint persists after its function atrophies): the obligation's function (atonement, worship, covenant maintenance) has not atrophied, but its only legitimate mode (physical performance) has become impossible. The reading maintains the obligation precisely to preserve what it sees as Torah's integrity. The constraint is a snare not because it benefits an identifiable agent but because it imposes a permanent status condition (perpetual non-compliance) on an entire population who cannot exit or discharge it. The rising theater_ratio indicates that substitute practices proliferate to fill the gap, but the performance-only reading explicitly denies they fulfill the obligation — making the theater itself evidence that the constraint persists despite functional workarounds. This is the opposite of piton (where theater masks atrophied function); here theater masks an obligation that cannot be fulfilled, which is why suppression remains high: the reading must actively deny that substitutes count, or the reading would collapse.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    obligation_persistence_vs_impossibility,
    'Can a binding halakhic obligation persist indefinitely when its only legitimate mode of fulfillment is structurally impossible? Is perpetual non-compliance a coherent status under Jewish law, or must an obligation either be discharged or suspended?',
    'Textual analysis of rabbinic authorities who address the obligation''s status post-Temple; determination of whether the tradition treats unfulfillable commands as suspended de facto even if maintained de jure; examination of whether practical halakhic rulings accept the perpetual non-compliance status or work toward substitutes.',
    'If perpetual non-compliance is incoherent, the performance-only reading''s claim to maintain the obligation becomes untenable, and a competing reading (study-as-exercise or messianic suspension) becomes logically necessary. If perpetual non-compliance is coherent, the performance-only reading''s structure holds and the high extractiveness is justified.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(obligation_persistence_vs_impossibility, conceptual, 'Whether an obligation to perform an impossible act can persist as binding.').

omega_variable(
    reading_choice_vs_logical_necessity,
    'Is the performance-only reading a logical requirement of Torah''s text, or is it one choice among equally defensible interpretations? Do the study-as-exercise and messianic-suspension readings have equal textual warrant, or is the performance-only reading the only coherent reading of the sacrifice commandments?',
    'Comparative textual exegesis of the four readings; examination of which authorities held which reading and on what textual grounds; determination of whether alternative readings require eisegesis (reading in) or can be grounded in the text''s own logic.',
    'If the performance-only reading is the unique logical necessity, then maintaining it is not a suppressive choice but a structural requirement. If alternative readings have equal textual warrant, then the performance-only reading''s dominance reflects a choice to suppress competing interpretations, making suppression genuinely operative rather than a consequence of logical necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_choice_vs_logical_necessity, conceptual, 'Whether the performance-only reading is textually required or one choice among alternatives.').

omega_variable(
    victim_set_coherence,
    'Are ''the Jewish people post-Temple destruction'' a coherent victim set for this constraint, or is the constraint personal/generational rather than collective? Does each generation renew the obligation and its unfulfillment, or does the obligation persist across generations as a trans-generational debt?',
    'Examination of whether rabbinic law treats the sacrifice obligation as individual or collective; whether each person born after Temple destruction acquires a new obligation or inherits a pre-existing one; whether there is a moment at which the obligation would be discharged (redemption, Temple restoration, or halakhic suspension).',
    'If the obligation is trans-generational and collective, the victim set extends across all Jews post-70 CE, and the extractiveness reflects a civilization-scale structural impossibility. If the obligation is individual and renewable, each generation faces a fresh choice about how to interpret it, and alternative readings become possible responses rather than suppressions of a pre-existing binding command.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_coherence, empirical, 'Whether the sacrifice obligation is trans-generational or renewed per generation.').

omega_variable(
    theater_mechanism_direction,
    'Do study-based practices, liturgical substitutes, and memorial ceremonies rise because the performance-only reading is being softened or abandoned (theater masking a constraint that is losing authority), or do they rise because the performance-only reading is being maintained and elaborated (theater as creative reinterpretation while holding the obligation''s literal form)?',
    'Textual tracking of how rabbinic authorities over centuries describe the relationship between study and performance; whether later authorities explicitly endorse substitutes or continue to insist they do not discharge the obligation; whether the rising theater correlates with rejection of the performance-only reading or with its institutional elaboration.',
    'If theater masks a collapsing obligation, the reading is a piton maintained by institutional inertia rather than a snare maintained by structural logic. If theater masks an obligation maintained as binding while acknowledging its impossibility, the snare classification holds — the reading persists precisely by denying that functional substitutes count.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(theater_mechanism_direction, empirical, 'Whether rising theater indicates weakening or elaboration of the performance-only reading.').

omega_variable(
    committer_frame_reading_selection,
    'This constraint is one reading of the sacrifice_obligation_kernel. What conditions determined that the performance-only reading would be selected for instantiation (rather than study-as-exercise, messianic-suspension, or symbolic-archive)? Was selection based on authority/prevalence within tradition, on contemporary institutional dominance, on textual warrant, or on the desire to model the most extractive reading?',
    'Comparison of the four readings'' historical prevalence, contemporary institutional support, textual warrant, and structural metrics; determination of which reading would be selected by a neutral (non-committed) observer as the ''canonical'' or ''dominant'' reading versus which would be selected by an observer seeking to maximize extractiveness or model a particular interpretive school.',
    'The reading''s selection affects the entire corpus analysis: if performance-only is dominant because it is textually required, then the high extractiveness is structurally necessary; if it is dominant because an institutional faction chose to privilege it, then the extractiveness reflects that choice and could be changed by adopting another reading. This omega documents the committer-axis ambiguity (OQ-250): the choice to author this reading as a snare rather than study-as-exercise as a rope reflects unstated premises about which reading is ''correct'' or ''canonical.''',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_frame_reading_selection, preference, 'The premises underlying selection of the performance-only reading for instantiation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_kernel__performance_only_reading, 70, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t70, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 70, 0.15).
narrative_ontology:measurement_basis(sacr_tr_t70, observed).
narrative_ontology:measurement(sacr_tr_t500, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 500, 0.35).
narrative_ontology:measurement_basis(sacr_tr_t500, projected).
narrative_ontology:measurement(sacr_tr_t1000, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 1000, 0.5).
narrative_ontology:measurement_basis(sacr_tr_t1000, projected).
narrative_ontology:measurement(sacr_tr_t1500, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 1500, 0.58).
narrative_ontology:measurement_basis(sacr_tr_t1500, projected).
narrative_ontology:measurement(sacr_tr_t1900, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 1900, 0.62).
narrative_ontology:measurement_basis(sacr_tr_t1900, observed).
narrative_ontology:measurement(sacr_tr_t2026, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 2026, 0.62).
narrative_ontology:measurement_basis(sacr_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(sacr_be_t70, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 70, 0.95).
narrative_ontology:measurement_basis(sacr_be_t70, observed).
narrative_ontology:measurement(sacr_be_t500, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 500, 0.94).
narrative_ontology:measurement_basis(sacr_be_t500, projected).
narrative_ontology:measurement(sacr_be_t1000, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 1000, 0.93).
narrative_ontology:measurement_basis(sacr_be_t1000, projected).
narrative_ontology:measurement(sacr_be_t1500, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 1500, 0.92).
narrative_ontology:measurement_basis(sacr_be_t1500, projected).
narrative_ontology:measurement(sacr_be_t1900, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 1900, 0.92).
narrative_ontology:measurement_basis(sacr_be_t1900, observed).
narrative_ontology:measurement(sacr_be_t2026, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 2026, 0.92).
narrative_ontology:measurement_basis(sacr_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t70, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 70, 0.82).
narrative_ontology:measurement_basis(sacr_su_t70, observed).
narrative_ontology:measurement(sacr_su_t500, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 500, 0.79).
narrative_ontology:measurement_basis(sacr_su_t500, projected).
narrative_ontology:measurement(sacr_su_t1000, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 1000, 0.78).
narrative_ontology:measurement_basis(sacr_su_t1000, projected).
narrative_ontology:measurement(sacr_su_t1500, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 1500, 0.78).
narrative_ontology:measurement_basis(sacr_su_t1500, projected).
narrative_ontology:measurement(sacr_su_t1900, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 1900, 0.78).
narrative_ontology:measurement_basis(sacr_su_t1900, observed).
narrative_ontology:measurement(sacr_su_t2026, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 2026, 0.78).
narrative_ontology:measurement_basis(sacr_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_kernel__performance_only_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(sacrifice_obligation_kernel__performance_only_reading, 0.18).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__performance_only_reading, sacrifice_obligation_kernel__study_as_exercise_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__performance_only_reading, sacrifice_obligation_kernel__messianic_suspension_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__performance_only_reading, sacrifice_obligation_kernel__symbolic_archive_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__performance_only_reading, temple_reconstruction_obligation).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__performance_only_reading, covenantal_membership_identity_lock).

% DUAL FORMULATION NOTE:
% The sacrifice obligation forms a constraint family of four readings, each modeling the kernel differently. Performance-only (this constraint) models the obligation as persisting unfulfilled; study-as-exercise models it as discharged through study; messianic-suspension models it as divinely paused; symbolic-archive models it as cultural rather than halakhically binding. All four share the referent (Torah's sacrifice commandments) but differ in ε (extractiveness), victim/beneficiary structure, and type. Each reading instantiates a closed logical system; they do not coexist within a single seat's framework but compete across seats and generations. Network edges link the readings so corpus analysis can track how one reading's adoption affects the family's overall extractiveness profile.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sacrifice_obligation_kernel__performance_only_reading, powerless, 1.0).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
