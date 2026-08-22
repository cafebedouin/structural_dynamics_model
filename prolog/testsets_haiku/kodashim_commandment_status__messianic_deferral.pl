% ============================================================================
% CONSTRAINT STORY: kodashim_commandment_status__messianic_deferral
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_commandment_status__messianic_deferral, []).

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
 *   constraint_id: kodashim_commandment_status__messianic_deferral
 *   human_readable: Kodashim Commandment Deferral under Messianic Reading
 *   domain: religious/halakhic
 *
 * SUMMARY:
 *   After the Second Temple's destruction, the rabbinical tradition faced a
 *   foundational halakhic crisis: sacrifice commandments became materially
 *   impossible yet remained obligatory. The messianic-deferral reading
 *   resolves this by interpreting the commandments as suspended in
 *   performance but not obsolete — they remain operative through study,
 *   preparation, and the eschatological hope for restoration. This reading
 *   coordinates the preservation of the commandment corpus while justifying
 *   present-generation resource allocation to preparation for a future event
 *   whose timeline is unknown. The constraint operates as a tangled rope:
 *   study provides genuine coordination (keeps the law-code intelligible,
 *   maintains community continuity, prepares for possible restoration) while
 *   simultaneously extracting resources from present needs under the cover of
 *   messianic promise.
 *
 * KEY AGENTS:
 *   - Rabbinical interpretive authority: maintains and enforces the deferral reading; controls what counts as 'readiness'
 *   - Present-generation needs: bear opportunity cost; inherit an obligation for unknown future
 *   - Study practitioners: benefit from meaningful scholarly role; suppressed from questioning deferral frame
 *   - Messianic narrative: vindicated by the reading's structure but collects resources through rabbinical authority
 *   - Alternative readings: excluded from normative conversation despite representing live contestation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_commandment_status__messianic_deferral, 0.62).
domain_priors:suppression_score(kodashim_commandment_status__messianic_deferral, 0.45).
domain_priors:theater_ratio(kodashim_commandment_status__messianic_deferral, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, extractiveness, 0.62).
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_commandment_status__messianic_deferral, tangled_rope).
narrative_ontology:human_readable(kodashim_commandment_status__messianic_deferral, "Kodashim Commandment Deferral under Messianic Reading").
narrative_ontology:topic_domain(kodashim_commandment_status__messianic_deferral, "religious/halakhic").

domain_priors:requires_active_enforcement(kodashim_commandment_status__messianic_deferral).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_commandment_status__messianic_deferral, 'f1047d91-5b0c-4d90-8978-14808bf979d0').
narrative_ontology:cs_kernel_codification('f1047d91-5b0c-4d90-8978-14808bf979d0', distributed).
narrative_ontology:cs_authority_grounding('f1047d91-5b0c-4d90-8978-14808bf979d0', lineage).
narrative_ontology:cs_interpretation_layer_present('f1047d91-5b0c-4d90-8978-14808bf979d0').
narrative_ontology:cs_reading_relation('f1047d91-5b0c-4d90-8978-14808bf979d0', kodashim_commandment_status__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('f1047d91-5b0c-4d90-8978-14808bf979d0', kodashim_commandment_status__study_as_performance, influences).
narrative_ontology:cs_axiom('f1047d91-5b0c-4d90-8978-14808bf979d0', foundational, commandment_restoration_possible_messianic).
narrative_ontology:cs_axiom_status(commandment_restoration_possible_messianic, holdable).
narrative_ontology:cs_axiom_grounding('f1047d91-5b0c-4d90-8978-14808bf979d0', commandment_restoration_possible_messianic, theological).
narrative_ontology:cs_axiom('f1047d91-5b0c-4d90-8978-14808bf979d0', foundational, suspension_vs_obsolescence_distinction_binding).
narrative_ontology:cs_axiom_status(suspension_vs_obsolescence_distinction_binding, holdable).
narrative_ontology:cs_axiom_grounding('f1047d91-5b0c-4d90-8978-14808bf979d0', suspension_vs_obsolescence_distinction_binding, conventional).
narrative_ontology:cs_reference_frame('f1047d91-5b0c-4d90-8978-14808bf979d0', temple_dependent_commandment_framework).
narrative_ontology:cs_drift_state('f1047d91-5b0c-4d90-8978-14808bf979d0', contemporary_post_two_millennia_absence, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('f1047d91-5b0c-4d90-8978-14808bf979d0', '').
narrative_ontology:cs_kernel_id(kodashim_commandment_status__messianic_deferral, kodashim_commandment_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__messianic_deferral, messianic_eschatological_narrative).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__messianic_deferral, rabbinical_interpretive_authority).
narrative_ontology:constraint_victim(kodashim_commandment_status__messianic_deferral, present_generation_needs).
narrative_ontology:constraint_victim(kodashim_commandment_status__messianic_deferral, alternative_fulfillment_frameworks).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__messianic_deferral, study_practitioners).
narrative_ontology:constraint_victim(kodashim_commandment_status__messianic_deferral, study_practitioners).
narrative_ontology:constraint_vindicates(kodashim_commandment_status__messianic_deferral, commandment_restoration_doctrine).
narrative_ontology:constraint_vindicates(kodashim_commandment_status__messianic_deferral, temporal_suspension_vs_obsolescence_distinction).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enforces the halakhic interpretation that sacrifice commandments are suspended but not obsolete, justifying continued study preparation despite 2000-year Temple absence. Maintains interpretive control over what counts as 'readiness' and legitimate deferral. Sustains this reading through educational transmission, textual commentary, and resistance to competing readings that would either abandon the commandment or declare study equivalent to performance.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, rabbinical_interpretive_authority, agenda_setter,
    institutional, generational, constrained, global).

% Bear the opportunity cost of maintaining readiness for an event (Temple restoration) that has not occurred for two millennia and whose timeline is unknown. Resources devoted to sacrifice study, Temple-service preparation, and messianic anticipatory structures are unavailable for immediate communal needs, spiritual development in the present moment, or alternative commandment fulfillment frameworks. This generation inherits an obligation formulated for a future they did not choose.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, present_generation_needs, payer,
    powerless, immediate, trapped, global).

% The messianic narrative is sustained as the authoritative temporal frame by interpreting present commandment obligations through the lens of future restoration. This reading vindicates the narrative's truth-status by embedding it in present practice: study is meaningful precisely because restoration is possible. The narrative itself collects no resources but commands them through the interpretive authority that sponsors the reading.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, messianic_eschatological_narrative, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(kodashim_commandment_status__messianic_deferral, messianic_eschatological_narrative).

% Frameworks that reinterpret sacrifice laws as permanently obsolete (rationalist reform readings), or that claim study itself constitutes full performance (intellectual-fulfillment readings), are suppressed by the rabbinical insistence that deferral preserves the commandment's future restoration. Adherents of these readings are excluded from the conversation about what 'readiness' looks like or whether the deferral is justified. The constraint's enforcement machinery actively maintains this exclusion.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, alternative_fulfillment_frameworks, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(kodashim_commandment_status__messianic_deferral, alternative_fulfillment_frameworks, excluded).

% Scholars and pious practitioners who engage in sacrifice-law study receive meaningful intellectual and spiritual activity, social status within their communities, and the sense of participating in readiness for restoration. They also bear the constraint's suppression: their study must be justified as preparation rather than evaluated on its own merits as intellectual inquiry or spiritual practice. Identity as a proper student of Torah is fused with accepting the messianic deferral frame; exit from the deferral reading would require reframing one's own scholarly identity.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, study_practitioners, beneficiary,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(kodashim_commandment_status__messianic_deferral, study_practitioners, payer).

% Other rabbinic interpretive authorities and philosophical schools that offer competing readings (performance-only, study-as-performance) maintain positions within the broader halakhic corpus but are structurally excluded from setting the normative interpretation of deferral. They represent live contestation that the constraint's enforcement (textual authority, educational transmission, community consensus-building) works to suppress in favor of the messianic-deferral reading.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, competing_rabbinic_schools, excluded,
    powerful, generational, constrained, global).

% An external analytical seat examining how the messianic-deferral reading structures the relationship between present obligation and future contingency; whether the deferral mechanism extracts present resources under cover of future possibility; what the competitive landscape of readings reveals about institutional interests.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, theological_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kodashim_commandment_status__messianic_deferral, rabbinical_interpretive_authority).
narrative_ontology:fixing_cost_class(kodashim_commandment_status__messianic_deferral, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the practical problem of maintaining a commandment corpus (sacrifice laws, Temple service, priestly order) whose original performance site has been destroyed and whose restoration timeline is radically uncertain. The deferral reading coordinates this by placing study and preparation in the present while anchoring future fulfillment to messianic redemption — it keeps the commandments 'alive' in collective memory and readiness without requiring their immediate execution.
% TRANSFER_FUNCTION: Moves resources (study time, scholarly effort, community prioritization, identity investment) from present-generation needs and immediate spiritual fulfillment toward messianic preparation and rabbinical authority maintenance. The constraint transfers the cost of keeping sacrifice laws intelligible and actionable to contemporary practitioners, justified by the narrative that this readiness is essential to the commandment's restoration.
% ABSENT_VOICES: Rationalist reformers, modernist interpreters, and alternative fulfillment frameworks (performance-only, study-as-performance) would argue that two millennia of Temple absence constitutes effective obsolescence; that study unsupported by restoration should be reinterpreted as spiritually complete in itself, or abandoned; or that commandments properly understood are embedded in their material conditions and cannot be suspended indefinitely. These voices are structurally excluded from the rabbinical conversation about what deferral legitimately means.
% DISAPPEARANCE_RATIONALE: If the messianic-deferral reading vanished and were replaced by a performance-only reading, sacrifice study would be reframed as historical scholarship rather than commandment preparation, and the resource commitment to Temple-service readiness would collapse. If replaced by study-as-performance, present satisfaction would increase and the opportunity-cost structure would dissolve. The world rearranges for believers whose identity is fused with deferral readiness; it remains roughly unchanged for communities that have already adopted alternative readings.
% FOUNDING_PROBLEM: After the Temple's destruction (70 CE and definitively 135 CE), the sacrificial commandments became performatively impossible. The halakhic question: does their impossibility render them obsolete, suspended, or transformed? The rabbis faced a choice between abandoning a commandment corpus or reinterpreting its present form. The deferral reading solves this by suspending performance while preserving the commandment's future status and present intellectual obligation.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinical tradition and halakhic texts themselves attest to the founding problem and validate the deferral solution. Competing halakhic schools (performance-only, study-as-performance) acknowledge the same problem but argue for different solutions. External theological analysis and historical scholarship from outside the rabbinical tradition support the claim that deferral is one contested reading among several, not a necessary or inevitable response.
narrative_ontology:disappearance_verdict(kodashim_commandment_status__messianic_deferral, contested).
narrative_ontology:founding_problem_status(kodashim_commandment_status__messianic_deferral, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_commandment_status__messianic_deferral, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(kodashim_commandment_status__messianic_deferral, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_commandment_status__messianic_deferral, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_commandment_status__messianic_deferral_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kodashim_commandment_status__messianic_deferral, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kodashim_commandment_status__messianic_deferral_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness at 0.62 reflects moderate opportunity cost: sacrifice study consumes resources that could address present communal needs, spiritual development in the immediate, or alternative commandment frameworks, yet is justified by future contingency (messianic restoration) rather than present fulfillment. The trajectory shows a slight rise in extractiveness (0.48 → 0.62) over 2000 years as the absence of restoration accumulates but the deferral commitment deepens through institutional entrenchment. Theater ratio rises from 0.35 to 0.58, indicating increasing performative element: the constraint becomes increasingly about maintaining the appearance of readiness and keeping the law-code alive rather than preparing for imminent performance. Suppression requirement remains moderate (0.45) because the deferral reading is institutionally legitimated and competes against rather than silently suppresses alternatives — the suppression is active enforcement of one reading's dominance, not background structural coercion. The measurement series share one grid: all three metrics are authored at six shared time points spanning the interval.
 *
 * PERSPECTIVAL GAP:
 *   From the rabbinical seat, deferral is a coordinating solution to an impossible problem — it preserves the commandment while acknowledging material impossibility. From the present-generation-needs seat, it is extraction justified by an unknown future. From the study-practitioner seat (identity-locked), the reading is integral to intellectual identity; exit would require radical reframing of scholarly role. From the excluded-alternatives seat, the constraint is an institutional lock-in that suppresses live competing interpretations. The engine computes these divergences from the structural data (power, exit, beneficiary/victim declarations); the authored claim-type (tangled rope) reflects the reading's structure without adjudicating these seat-level conflicts.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinical authority sits at d ≈ 0.1–0.2 (beneficiary: controls interpretation, sustains institutional authority through the deferral reading's transmission). Present-generation needs sit at d ≈ 0.8–0.9 (target: bear opportunity costs, identity-locked to inherited obligation structure). Study practitioners sit at d ≈ 0.4–0.5 (mixed: genuine benefit from intellectual role, but suppressed from questioning the deferral frame's legitimacy). Alternative frameworks sit near d ≈ 0.7–0.8 (targets: excluded from normative conversation, their resources and intellectual space suppressed). The messianic narrative itself is not an agent but a vindicated proposition that the reading embeds in practice.
 *
 * MANDATROPHY ANALYSIS:
 *   The messianic-deferral reading avoids mandatrophy collapse by embedding present action (study, preparation, community maintenance) in future contingency (restoration). Unlike pure piton (atrophied function), the deferral reading's founding problem (impossible commandment after Temple destruction) remains live — the constraint is not vestigial, it is actively maintained as a solution to an ongoing interpretive crisis. Unlike pure snare (coercive extraction with no coordination), the reading provides genuine coordination (preserves law-code, maintains community, establishes readiness protocol). The classification as tangled rope reflects this: coordination (study structures knowledge) + extraction (opportunity cost on present generation) + active enforcement (suppression of competing readings) + structural asymmetry (rabbinical authority benefits, present generation bears cost). The key mandatrophy test: would rabbinical authorities themselves maintain this constraint if its extraction were removed? Yes — the coordination function (preserving sacrifice law, preparing for restoration) is real. Would present-generation adherents accept it if they could reframe their identity outside deferral? The identity-lock on study practitioners suggests suppression runs deeper than rational cost-benefit; this indicates internalized suppression, which is addressed in the omegas.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    messianic_timeline_contingency,
    'Is the messianic restoration a live eschatological expectation with meaningful probability, or has it effectively become a mythological placeholder that justifies indefinite deferral without genuine temporal horizon?',
    'Analysis of rabbinical texts: do they treat restoration as contingent-but-possible, or as axiomatic narrative structure divorced from empirical expectation? Ethnographic observation: do study practitioners report genuine anticipation of restoration, or ritualized performance of anticipation?',
    'If restoration is live expectation, the deferral is justified and extractiveness is moderate (a real future contingency). If restoration has become mythological structure, extractiveness rises sharply — present generation bears opportunity cost for indefinite maintenance of a narrative whose future realization is not genuinely expected. This would shift the reading closer to snare (pure extraction under coordination cover).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(messianic_timeline_contingency, empirical, 'Whether messianic restoration retains genuine eschatological force or has become a mythological justification.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is the constraint''s suppression primarily structural (rabbinical authority''s institutional control over interpretive transmission) or internalized (study practitioners have fused their identity with the deferral reading such that exit feels unthinkable)?',
    'Examination of scholars who have exited the deferral reading: do they report having broken institutional barriers (structural exit) or having undergone identity transformation (psychological/internalized exit)? Interview data on practitioners'' experience of interpretive constraints.',
    'If suppression is primarily structural, removing institutional enforcement would reduce the constraint''s hold. If internalized, practitioners would carry suppression with them after exit — the constraint''s grip persists even when external machinery is removed. This affects whether interventions at the institutional level would sufficiently undermine the constraint or whether deeper identity-work is required.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Whether suppression of alternative readings operates through institutional control or through internalized identity fusion.').

omega_variable(
    alternative_reading_foreclosure,
    'Do the messianic-deferral and study-as-performance readings logically foreclose each other within a single halakhic framework, or do they coexist as genuinely competing but live positions?',
    'Textual analysis of whether a single rabbinical authority or school could coherently hold both readings simultaneously, or whether they are structured as mutually exclusive axioms. Historical survey of whether schools have held both positions across time.',
    'If they foreclose each other (core premises contradict), the reading_relations should use forecloses. If they coexist as live alternatives, the relation should be coexists_with. This affects whether the constraint actively suppresses a logically impossible alternative or actively suppresses a live competitor — different structural stories about what the enforcement machinery is doing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_reading_foreclosure, conceptual, 'Logical relationship between messianic-deferral and study-as-performance readings.').

omega_variable(
    opportunity_cost_measurement,
    'What fraction of halakhic study time and community resource allocation is genuinely devoted to sacrifice-law preparation for messianic restoration, versus how much is embedded in broader Torah study with no explicit deferral justification?',
    'Curriculum analysis of study institutions; textual frequency analysis of sacrifice-law discussions relative to total halakhic corpus; survey data on practitioners'' stated reasons for studying sacrifice laws.',
    'High opportunity cost (substantial explicit sacrifice-study allocation) would support higher extractiveness measure. Low opportunity cost (sacrifice-law study as incidental to broader corpus) would support lower extractiveness. The measured extractiveness of 0.62 assumes moderate allocation; if allocation is lower, extractiveness should be revised downward.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(opportunity_cost_measurement, empirical, 'Actual resource allocation to sacrifice-law study versus broader halakhic curriculum.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_commandment_status__messianic_deferral, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_commandment_status__messianic_deferral, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(koda_tr_t0, observed).
narrative_ontology:measurement(koda_tr_t200, kodashim_commandment_status__messianic_deferral, theater_ratio, 200, 0.42).
narrative_ontology:measurement_basis(koda_tr_t200, observed).
narrative_ontology:measurement(koda_tr_t600, kodashim_commandment_status__messianic_deferral, theater_ratio, 600, 0.51).
narrative_ontology:measurement_basis(koda_tr_t600, observed).
narrative_ontology:measurement(koda_tr_t1000, kodashim_commandment_status__messianic_deferral, theater_ratio, 1000, 0.56).
narrative_ontology:measurement_basis(koda_tr_t1000, observed).
narrative_ontology:measurement(koda_tr_t1600, kodashim_commandment_status__messianic_deferral, theater_ratio, 1600, 0.58).
narrative_ontology:measurement_basis(koda_tr_t1600, observed).
narrative_ontology:measurement(koda_tr_t2000, kodashim_commandment_status__messianic_deferral, theater_ratio, 2000, 0.58).
narrative_ontology:measurement_basis(koda_tr_t2000, observed).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_commandment_status__messianic_deferral, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(koda_be_t0, observed).
narrative_ontology:measurement(koda_be_t200, kodashim_commandment_status__messianic_deferral, base_extractiveness, 200, 0.52).
narrative_ontology:measurement_basis(koda_be_t200, observed).
narrative_ontology:measurement(koda_be_t600, kodashim_commandment_status__messianic_deferral, base_extractiveness, 600, 0.58).
narrative_ontology:measurement_basis(koda_be_t600, observed).
narrative_ontology:measurement(koda_be_t1000, kodashim_commandment_status__messianic_deferral, base_extractiveness, 1000, 0.61).
narrative_ontology:measurement_basis(koda_be_t1000, observed).
narrative_ontology:measurement(koda_be_t1600, kodashim_commandment_status__messianic_deferral, base_extractiveness, 1600, 0.62).
narrative_ontology:measurement_basis(koda_be_t1600, observed).
narrative_ontology:measurement(koda_be_t2000, kodashim_commandment_status__messianic_deferral, base_extractiveness, 2000, 0.62).
narrative_ontology:measurement_basis(koda_be_t2000, observed).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t0, kodashim_commandment_status__messianic_deferral, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(koda_su_t0, observed).
narrative_ontology:measurement(koda_su_t200, kodashim_commandment_status__messianic_deferral, suppression_requirement, 200, 0.38).
narrative_ontology:measurement_basis(koda_su_t200, observed).
narrative_ontology:measurement(koda_su_t600, kodashim_commandment_status__messianic_deferral, suppression_requirement, 600, 0.42).
narrative_ontology:measurement_basis(koda_su_t600, observed).
narrative_ontology:measurement(koda_su_t1000, kodashim_commandment_status__messianic_deferral, suppression_requirement, 1000, 0.44).
narrative_ontology:measurement_basis(koda_su_t1000, observed).
narrative_ontology:measurement(koda_su_t1600, kodashim_commandment_status__messianic_deferral, suppression_requirement, 1600, 0.45).
narrative_ontology:measurement_basis(koda_su_t1600, observed).
narrative_ontology:measurement(koda_su_t2000, kodashim_commandment_status__messianic_deferral, suppression_requirement, 2000, 0.45).
narrative_ontology:measurement_basis(koda_su_t2000, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_commandment_status__messianic_deferral, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(kodashim_commandment_status__messianic_deferral, 0.12).
narrative_ontology:affects_constraint(kodashim_commandment_status__messianic_deferral, kodashim_commandment_status__performance_only).
narrative_ontology:affects_constraint(kodashim_commandment_status__messianic_deferral, kodashim_commandment_status__study_as_performance).

% DUAL FORMULATION NOTE:
% This constraint and its siblings (performance_only, study_as_performance) form a constraint family decomposed from the single kernel kodashim_commandment_status. Each reading instantiates a distinct ε-invariant constraint with different beneficiary/victim structures, different extractiveness profiles, and different suppression mechanisms. The readings are not perspectives on one constraint; they are separate constraints sharing a contested kernel. The messianic-deferral reading emphasizes opportunity cost and institutional authority maintenance; performance_only emphasizes obsolescence with low extraction; study_as_performance emphasizes intellectual fulfillment with different suppression dynamics. Network edges link the three constraints to enable contamination analysis: if one reading's empirical foundation erodes (e.g., through historical scholarship on messianic expectations), downstream pressure cascades to the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(kodashim_commandment_status__messianic_deferral, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
