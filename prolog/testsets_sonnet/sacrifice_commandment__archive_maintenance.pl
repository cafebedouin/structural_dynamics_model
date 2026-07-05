% ============================================================================
% CONSTRAINT STORY: sacrifice_commandment__archive_maintenance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_commandment__archive_maintenance, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: sacrifice_commandment__archive_maintenance
 *   human_readable: Sacrificial Law Study as Technical Archive for Future Temple Restoration
 *   domain: religious_studies/halakhic_theory
 *
 * SUMMARY:
 *   In post-Temple rabbinic tradition, detailed study of sacrificial law (the
 *   Kodashim order of the Mishnah/Talmud) persists as a major curricular
 *   subject despite having no operative referent — there is no functioning
 *   Temple and no priesthood performing the rites described. The
 *   archive_maintenance reading resolves the apparent absurdity of studying
 *   an inoperative ritual system by reframing study as technical
 *   preservation: the community keeps the detailed procedural knowledge alive
 *   and transmissible so that, should the Temple be rebuilt (a messianic-era
 *   expectation), the knowledge needed to resume service will not have been
 *   lost. This differs structurally from study-as-performance (where the
 *   study itself IS the commandment's fulfillment, present-tense, terminal
 *   value) and from performance-only (where the commandment is simply
 *   suspended, unfulfillable, and study carries no commandment-fulfilling
 *   weight at all).
 *
 * KEY AGENTS:
 *   - rabbinic_scholars_specializing_in_sacrificial_law: agenda_setter/beneficiary (institutional/identity_locked) — administers curriculum and collects institutional prestige from the archive framing
 *   - future_temple_generation: beneficiary (analytical/analytical) — the hypothetical eventual recipient of the preserved knowledge, unable to corroborate the arrangement
 *   - present_day_students_of_kodashim: payer (moderate/constrained) — bears the opportunity cost of years spent on procedurally detailed, currently inoperative material
 *   - yeshiva_institutions_teaching_kodashim: beneficiary (organized/constrained) — draws funding and prestige from the curricular apparatus the archive framing legitimizes
 *   - halakhic_historians: observer (analytical/analytical) — traces the framing's historical emergence as one of several available theological responses
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_commandment__archive_maintenance, 0.42).
domain_priors:suppression_score(sacrifice_commandment__archive_maintenance, 0.35).
domain_priors:theater_ratio(sacrifice_commandment__archive_maintenance, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, extractiveness, 0.42).
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_commandment__archive_maintenance, scaffold).
narrative_ontology:human_readable(sacrifice_commandment__archive_maintenance, "Sacrificial Law Study as Technical Archive for Future Temple Restoration").
narrative_ontology:topic_domain(sacrifice_commandment__archive_maintenance, "religious_studies/halakhic_theory").

narrative_ontology:has_sunset_clause(sacrifice_commandment__archive_maintenance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_commandment__archive_maintenance, 'c197fd21-fa52-49c0-b0fe-365204a9f86f').
narrative_ontology:cs_kernel_codification('c197fd21-fa52-49c0-b0fe-365204a9f86f', fixed_text).
narrative_ontology:cs_authority_grounding('c197fd21-fa52-49c0-b0fe-365204a9f86f', lineage).
narrative_ontology:cs_interpretation_layer_present('c197fd21-fa52-49c0-b0fe-365204a9f86f').
narrative_ontology:cs_reading_relation('c197fd21-fa52-49c0-b0fe-365204a9f86f', sacrifice_commandment__study_as_performance, coexists_with).
narrative_ontology:cs_reading_relation('c197fd21-fa52-49c0-b0fe-365204a9f86f', sacrifice_commandment__performance_only, influences).
narrative_ontology:cs_axiom('c197fd21-fa52-49c0-b0fe-365204a9f86f', foundational, study_has_instrumental_not_terminal_value).
narrative_ontology:cs_axiom_status(study_has_instrumental_not_terminal_value, holdable).
narrative_ontology:cs_axiom_grounding('c197fd21-fa52-49c0-b0fe-365204a9f86f', study_has_instrumental_not_terminal_value, instrumental).
narrative_ontology:cs_axiom('c197fd21-fa52-49c0-b0fe-365204a9f86f', foundational, restoration_is_a_live_structural_expectation).
narrative_ontology:cs_axiom_status(restoration_is_a_live_structural_expectation, holdable).
narrative_ontology:cs_axiom_grounding('c197fd21-fa52-49c0-b0fe-365204a9f86f', restoration_is_a_live_structural_expectation, theological).
narrative_ontology:cs_reference_frame('c197fd21-fa52-49c0-b0fe-365204a9f86f', second_temple_operative_service).
narrative_ontology:cs_drift_state('c197fd21-fa52-49c0-b0fe-365204a9f86f', contemporary_diaspora_study_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c197fd21-fa52-49c0-b0fe-365204a9f86f', '').
narrative_ontology:cs_kernel_id(sacrifice_commandment__archive_maintenance, sacrifice_commandment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_commandment__archive_maintenance, future_temple_generation).
narrative_ontology:constraint_beneficiary(sacrifice_commandment__archive_maintenance, yeshiva_institutions_teaching_kodashim).
narrative_ontology:constraint_beneficiary(sacrifice_commandment__archive_maintenance, rabbinic_scholars_specializing_in_sacrificial_law).
narrative_ontology:constraint_victim(sacrifice_commandment__archive_maintenance, present_day_students_of_kodashim).
narrative_ontology:constraint_vindicates(sacrifice_commandment__archive_maintenance, messianic_restoration_doctrine).
narrative_ontology:constraint_vindicates(sacrifice_commandment__archive_maintenance, temple_reconstruction_feasibility).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and transmits the curriculum of Kodashim (sacrificial law) tractates, framing detailed study of slaughter procedure, altar dimensions, and priestly service as the maintenance of a technical archive that will be needed the moment the Temple is rebuilt. Their scholarly standing, institutional position, and life's intellectual output are constituted by this framing; they administer which details count as halakhically load-bearing and which are academic curiosity.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, rabbinic_scholars_specializing_in_sacrificial_law, agenda_setter,
    institutional, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_commandment__archive_maintenance, rabbinic_scholars_specializing_in_sacrificial_law, beneficiary).

% A not-yet-existing population who would, in the messianic-restoration scenario, need functioning technical knowledge of sacrificial procedure to actually operate a rebuilt Temple. They cannot currently object, consent, or corroborate that the knowledge preserved for them is accurate, sufficient, or will ever be needed; their benefit is entirely conditional on an event with no fixed date.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, future_temple_generation, beneficiary,
    analytical, civilizational, analytical, national).

% Spend years of yeshiva study on procedural minutiae of a rite none of them will perform in their lifetimes, at the cost of time that could go to areas of law and ethics with present application. Formally free to specialize elsewhere, but curricular structure, communal prestige hierarchies, and family/institutional expectation make Kodashim study a default track that is costly to opt out of once entered.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, present_day_students_of_kodashim, payer,
    moderate, biographical, constrained, national).

% Draw funding, enrollment, and prestige from offering deep Kodashim tracks; the archive-maintenance framing legitimizes an entire curricular and institutional apparatus (dedicated faculty, publishing, endowed chairs) whose present-tense payoff is otherwise hard to state without appeal to future restoration.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, yeshiva_institutions_teaching_kodashim, beneficiary,
    organized, generational, constrained, national).

% Hold the sibling view that studying the sacrifice laws IS itself the fulfillment of the commandment, not preparation for a future performance. They are not consulted in curricular decisions that frame study as archival/preparatory rather than as present-tense religious fulfillment, and their reading would relieve present students of any 'waiting for Temple' anxiety the archive framing produces.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, study_as_performance_adherents, excluded,
    moderate, civilizational, constrained, national).

% Study how the archive-maintenance framing emerged historically (post-Temple-destruction rabbinic literature) as one of several available theological responses, and can compare its institutional effects to the performance-only and study-as-performance readings without holding a stake in which reading prevails.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, halakhic_historians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_commandment__archive_maintenance, diffuse).
narrative_ontology:fixing_cost_class(sacrifice_commandment__archive_maintenance, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves highly detailed, procedurally specific technical knowledge (measurements, sequences, materials, priestly qualifications) across many centuries in a durable, transmissible form, guarding against the loss of information that would be needed if Temple service were ever resumed — solving a genuine long-horizon information-preservation problem that no other institution is positioned to solve.
% TRANSFER_FUNCTION: Moves years of study time, institutional funding, and curricular priority from present students and communities toward the maintenance of a technical corpus whose beneficiary is a hypothetical future population; the transfer is time and attention now, in exchange for readiness later.
% ABSENT_VOICES: Adherents of the study-as-performance reading, who would argue the archival framing needlessly defers the commandment's fulfillment and produces unnecessary present-tense anxiety about non-fulfillment; also absent is any actual future-generation voice that could confirm the preserved material will in fact be needed or sufficient.
% DISAPPEARANCE_RATIONALE: If the archive-maintenance framing disappeared, the underlying Kodashim texts would not vanish, but the institutional apparatus built specifically to justify deep present-day study as preparation (dedicated faculties, curricular priority, communal prestige attached to Kodashim mastery) would lose its present-tense justification and likely shrink toward the level of ordinary historical/textual study; whether this counts as 'world rearranges' or 'world unchanged' is itself contested between the archive-maintenance and study-as-performance camps.
% FOUNDING_PROBLEM: After the Temple's destruction, the technical knowledge needed to operate sacrificial service risked being lost entirely within a few generations if not deliberately preserved through structured study; the archive-maintenance reading answers the question of why continue detailed study of an inoperative rite.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic tradition itself (via classical commentators framing Mishnah study of Kodashim as substituting for sacrifice, e.g. Taanit 27b-adjacent material) attests the preservation problem as real; halakhic historians outside the beneficiary institutions corroborate that the archival framing is one of several post-destruction theological responses rather than a self-evidently necessary one, and note that no external body can corroborate whether the preserved technical detail will in fact be adequate or needed, since the restoration event has no fixed occurrence.
narrative_ontology:disappearance_verdict(sacrifice_commandment__archive_maintenance, contested).
narrative_ontology:founding_problem_status(sacrifice_commandment__archive_maintenance, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_commandment__archive_maintenance, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sacrifice_commandment__archive_maintenance, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_commandment__archive_maintenance, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_commandment__archive_maintenance_tests).
:- end_tests(sacrifice_commandment__archive_maintenance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at a moderate 0.42, rising gently over the interval (0.30 to 0.42), reflecting the expected structural delta: present cost is real (years of specialized study time redirected from areas of present halakhic application) but the framing is not primarily coercive — no one is forced into Kodashim specialization by external force, and the suppression score (0.35) is correspondingly moderate rather than high, capturing social/institutional/curricular pressure rather than legal or physical coercion. Theater ratio starts low (0.15) and drifts upward (0.28) reflecting a slow institutional tendency for archive-maintenance rhetoric to substitute for genuinely rigorous technical preservation work as generations pass further from any living memory of Temple operation — the further removed the community is from an operative Temple, the more the 'preservation' claim risks becoming rhetorical rather than functionally verified against any operative standard.
 *
 * PERSPECTIVAL GAP:
 *   From the rabbinic scholar / yeshiva institution seat, this is Scaffold-shaped coordination: a genuinely necessary, self-consciously transitional preservation function bridging destruction and restoration, sunsetted by the restoration event itself. From the present student's seat, absent any confirmed restoration timeline, the same activity can register as an open-ended transfer of present time toward a benefit that may never materialize — structurally closer to a tangled_rope reading if the sunset condition never triggers. The engine's per-seat computation is expected to surface exactly this gap; the claimed_type (scaffold) is authored from the tradition's own self-description, independent of whether the metrics support it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are the future hypothetical population (near-total beneficiary position, though its unrealized/analytical status makes this benefit structurally uncertain — captured in the omega below) and the present institutions and scholars who derive funding, prestige, and identity from teaching and maintaining the archive. Present-day students carry the extraction: their years of study are the transfer, and their exit is constrained rather than trapped — they could in principle redirect their studies, but curricular structure and communal expectation make this costly. The rabbinic scholars occupy a dual role: they administer the framing (agenda_setter) and are also its direct beneficiaries (secondary_role beneficiary), which is the structural seam that makes divergence between the agenda_setter's and payer's computed classification most informative.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (risk of losing operative technical knowledge before any restoration) is authored as contested rather than flatly dead or flatly live: it is technically true no living generation has needed the preserved procedures, which could support a 'dead founding problem, apparatus persists' reading (mandatrophy), but the tradition's own theological commitments hold restoration as a standing possibility at any time, which keeps the founding problem live from inside the framework. This is precisely the ambiguity a sunset-clause-bearing Scaffold classification is suited to hold open without resolving prematurely in either direction — declaring it a settled mountain (necessary, timeless) or a settled snare (pure extraction dressed as piety) would both overclaim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    restoration_contingency_uncertainty,
    'Does the archive-maintenance framing''s entire justification depend on an event (Temple restoration) whose occurrence, and thus the framing''s terminal payoff, cannot be independently verified or dated?',
    'There is no empirical resolution mechanism available within the tradition''s own epistemic commitments; the closest available proxy is comparing communities that hold the archive-maintenance framing against those that hold performance-only or study-as-performance, and observing whether curricular investment in Kodashim correlates with independent measures of institutional health or student outcomes over multi-generational timescales.',
    'If restoration is understood as a live, structurally expected event, present cost is a reasonable investment in future readiness (supports scaffold classification with a genuine, if unscheduled, sunset). If restoration is treated as effectively deferred indefinitely by the tradition''s own practice (no active reconstruction planning, no priestly genealogical certification maintained at operative readiness), the sunset clause is nominal rather than functional, and the constraint drifts toward tangled_rope or piton.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(restoration_contingency_uncertainty, conceptual, 'Whether the restoration contingency underwriting archive-maintenance is a genuine sunset condition or a nominal one.').

omega_variable(
    kernel_reading_selection_pressure,
    'Why does a given community, yeshiva, or scholar adopt the archive-maintenance reading rather than the study-as-performance or performance-only readings of the same kernel?',
    'Historical and sociological analysis of which reading correlates with which institutional structures — e.g., whether archive-maintenance is more prevalent in institutions with heavier investment in specialized Kodashim faculties (self-interested selection) versus communities with lighter institutional stakes (more theologically neutral selection).',
    'If reading-selection correlates strongly with institutional self-interest (archive-maintenance chosen precisely because it justifies existing curricular infrastructure), that supports reading the moderate extractiveness score as partly a self-serving institutional artifact rather than a neutral theological conclusion. If selection is largely independent of institutional stake, the extraction reading is weaker and the coordination (preservation) function should be weighted more heavily.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_pressure, empirical, 'Whether adoption of the archive_maintenance reading tracks institutional self-interest or independent theological reasoning.').

omega_variable(
    sufficiency_of_preserved_knowledge,
    'Even granting the preservation goal is genuine, is the knowledge actually being preserved (as transmitted through centuries of commentary layers) sufficient and accurate enough to actually operate a rebuilt Temple, or has transmission drift degraded its technical fidelity to the point where the archive''s practical value is itself uncertain?',
    'Comparative textual analysis of Kodashim commentary across centuries for internal consistency and specificity decay; comparison against any surviving archaeological or Second-Temple-period external sources.',
    'If fidelity has degraded substantially, the archive''s actual future utility (its core justifying claim) is undermined independent of whether restoration occurs, which would push the classification toward theater/piton rather than functional scaffold regardless of the restoration-timing question.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sufficiency_of_preserved_knowledge, empirical, 'Whether centuries of commentary-layer transmission preserves or degrades the technical fidelity the archive-maintenance justification depends on.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_commandment__archive_maintenance, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_commandment__archive_maintenance, theater_ratio, 0, 0.15).
narrative_ontology:measurement(sacr_tr_t20, sacrifice_commandment__archive_maintenance, theater_ratio, 20, 0.18).
narrative_ontology:measurement(sacr_tr_t40, sacrifice_commandment__archive_maintenance, theater_ratio, 40, 0.21).
narrative_ontology:measurement(sacr_tr_t60, sacrifice_commandment__archive_maintenance, theater_ratio, 60, 0.24).
narrative_ontology:measurement(sacr_tr_t80, sacrifice_commandment__archive_maintenance, theater_ratio, 80, 0.26).
narrative_ontology:measurement(sacr_tr_t100, sacrifice_commandment__archive_maintenance, theater_ratio, 100, 0.28).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_commandment__archive_maintenance, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(sacr_be_t20, sacrifice_commandment__archive_maintenance, base_extractiveness, 20, 0.33).
narrative_ontology:measurement(sacr_be_t40, sacrifice_commandment__archive_maintenance, base_extractiveness, 40, 0.37).
narrative_ontology:measurement(sacr_be_t60, sacrifice_commandment__archive_maintenance, base_extractiveness, 60, 0.4).
narrative_ontology:measurement(sacr_be_t80, sacrifice_commandment__archive_maintenance, base_extractiveness, 80, 0.41).
narrative_ontology:measurement(sacr_be_t100, sacrifice_commandment__archive_maintenance, base_extractiveness, 100, 0.42).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(sacrifice_commandment__archive_maintenance, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_commandment__archive_maintenance, identity_coordination).
narrative_ontology:boltzmann_floor_override(sacrifice_commandment__archive_maintenance, 0.1).
narrative_ontology:affects_constraint(sacrifice_commandment__archive_maintenance, sacrifice_commandment__study_as_performance).
narrative_ontology:affects_constraint(sacrifice_commandment__archive_maintenance, sacrifice_commandment__performance_only).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language concept 'the sacrifice commandment after Temple destruction' per the epsilon-invariance principle. archive_maintenance (this story) claims moderate extractiveness (0.42) with the future generation as beneficiary and present students bearing opportunity cost. study_as_performance is expected to claim substantially lower extractiveness (study terminates in itself, no deferred beneficiary structure) and performance_only is expected to claim a different type entirely (the commandment as suspended obligation, likely mountain-adjacent or scaffold-adjacent with a very different beneficiary/victim structure, since if study carries no fulfillment weight at all, the extraction question becomes about whether ANY current activity is compelled by the commandment). Each reading is authored as its own constraint with its own epsilon; they are linked here via affects_constraints rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
