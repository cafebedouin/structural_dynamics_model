% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_continuity__archival_preservation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_continuity__archival_preservation, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: sacrifice_obligation_continuity__archival_preservation
 *   human_readable: Sacrifice Law as Preserved Textual Tradition (Archival-Preservation Reading)
 *   domain: religious/textual/cultural
 *
 * SUMMARY:
 *   This story instantiates the archival_preservation reading of the
 *   sacrifice-obligation kernel: sacrifice law no longer imposes normative
 *   obligation on anyone, and study of its texts functions as cultural memory
 *   preservation and scholarly/liturgical continuity, not as a live legal
 *   duty or a substitute for the physical practice. Under this reading
 *   obligation exits constraint space entirely — there is no binding rule
 *   left to extract compliance from, no victim of non-performance, and no
 *   enforcement apparatus, because nothing is any longer required. The
 *   coordination function is genuine and mild: keeping a demanding historical
 *   corpus alive across dispersion and generations. This is a sibling reading
 *   to study_as_performance (which holds study itself fulfills the
 *   commandment, keeping obligation alive in textual form), performance_only
 *   (which holds the obligation persists but only physical performance
 *   fulfills it, with study as mere preparation), and messianic_suspension
 *   (which holds the obligation is suspended, neither fulfilled nor violated,
 *   pending restoration). All four readings share the same kernel text and
 *   history; they diverge entirely on whether any normative claim survives
 *   the Temple's destruction, and that divergence is the reason this is
 *   authored as four separate constraint stories rather than one story with
 *   an ambiguous ε.
 *
 * KEY AGENTS:
 *   - textual_scholars: beneficiary (moderate/mobile) — gain scholarly and pedagogical value from a live intellectual tradition
 *   - cultural_memory_communities: beneficiary (moderate/mobile) — sustain communal identity and calendar-linked liturgy through study without compliance burden
 *   - diaspora_communal_identity: beneficiary/observer (organized/mobile) — anchors continuity claims across dispersion
 *   - restorationist_communities: excluded — hold a different reading of the same kernel, not represented here
 *   - textual_tradition_itself: observer, non-agent — the corpus persisting as an object regardless of reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_continuity__archival_preservation, 0.03).
domain_priors:suppression_score(sacrifice_obligation_continuity__archival_preservation, 0.05).
domain_priors:theater_ratio(sacrifice_obligation_continuity__archival_preservation, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__archival_preservation, extractiveness, 0.03).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__archival_preservation, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__archival_preservation, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__archival_preservation, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__archival_preservation, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_continuity__archival_preservation, rope).
narrative_ontology:human_readable(sacrifice_obligation_continuity__archival_preservation, "Sacrifice Law as Preserved Textual Tradition (Archival-Preservation Reading)").
narrative_ontology:topic_domain(sacrifice_obligation_continuity__archival_preservation, "religious/textual/cultural").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_continuity__archival_preservation, '17962915-b7ae-4e87-a1cb-ed9a287bf7a5').
narrative_ontology:cs_kernel_codification('17962915-b7ae-4e87-a1cb-ed9a287bf7a5', fixed_text).
narrative_ontology:cs_authority_grounding('17962915-b7ae-4e87-a1cb-ed9a287bf7a5', lineage).
narrative_ontology:cs_interpretation_layer_present('17962915-b7ae-4e87-a1cb-ed9a287bf7a5').
narrative_ontology:cs_reading_relation('17962915-b7ae-4e87-a1cb-ed9a287bf7a5', sacrifice_obligation_continuity__study_as_performance, coexists_with).
narrative_ontology:cs_reading_relation('17962915-b7ae-4e87-a1cb-ed9a287bf7a5', sacrifice_obligation_continuity__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('17962915-b7ae-4e87-a1cb-ed9a287bf7a5', sacrifice_obligation_continuity__messianic_suspension, coexists_with).
narrative_ontology:cs_axiom('17962915-b7ae-4e87-a1cb-ed9a287bf7a5', foundational, obligation_terminates_with_impossibility_of_object).
narrative_ontology:cs_axiom_status(obligation_terminates_with_impossibility_of_object, holdable).
narrative_ontology:cs_axiom_grounding('17962915-b7ae-4e87-a1cb-ed9a287bf7a5', obligation_terminates_with_impossibility_of_object, conventional).
narrative_ontology:cs_axiom('17962915-b7ae-4e87-a1cb-ed9a287bf7a5', secondary, study_constitutes_memory_not_fulfillment).
narrative_ontology:cs_axiom_status(study_constitutes_memory_not_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('17962915-b7ae-4e87-a1cb-ed9a287bf7a5', study_constitutes_memory_not_fulfillment, conventional).
narrative_ontology:cs_reference_frame('17962915-b7ae-4e87-a1cb-ed9a287bf7a5', temple_era_binding_practice).
narrative_ontology:cs_drift_state('17962915-b7ae-4e87-a1cb-ed9a287bf7a5', post_temple_destruction_contemporary, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('17962915-b7ae-4e87-a1cb-ed9a287bf7a5', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_continuity__archival_preservation, sacrifice_obligation_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__archival_preservation, textual_scholars).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__archival_preservation, cultural_memory_communities).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__archival_preservation, diaspora_communal_identity).
narrative_ontology:constraint_vindicates(sacrifice_obligation_continuity__archival_preservation, historical_continuity_of_tradition).
narrative_ontology:constraint_vindicates(sacrifice_obligation_continuity__archival_preservation, textual_preservation_value_independent_of_practice).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Study sacrifice law texts as historical and literary material within a broader corpus. Gain scholarly standing, teaching material, and continuity with an interpretive tradition. Free to treat the subject as any other historical-legal corpus; no personal normative stake in whether sacrifices are ever again offered.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__archival_preservation, textual_scholars, beneficiary,
    moderate, generational, mobile, global).

% Read, teach, and transmit the sacrificial texts as part of communal identity and liturgical calendar commemoration (fast days, holiday liturgy referencing the Temple service) without treating the underlying law as binding conduct. Participation is voluntary and carries no compliance cost; leaving the practice of study carries no sanction.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__archival_preservation, cultural_memory_communities, beneficiary,
    moderate, civilizational, mobile, global).

% Uses continued study of the sacrificial corpus as a marker of historical continuity and communal self-understanding across dispersion, independent of any expectation of restored practice. Benefits from the archive's persistence as a cultural anchor; no cost is imposed on those who do not study it.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__archival_preservation, diaspora_communal_identity, beneficiary,
    organized, civilizational, mobile, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_continuity__archival_preservation, diaspora_communal_identity, observer).

% Hold that sacrifice law remains binding pending restoration and that study without practice-readiness misreads the tradition. Their view is not represented within this reading's framework, though they are free to hold and practice it under a different reading (performance_only or messianic_suspension).
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__archival_preservation, restorationist_communities, excluded,
    moderate, civilizational, mobile, global).

% The corpus of sacrificial law as a body of text — persists as an object of study regardless of which reading communities adopt; not itself an actor, included for completeness of the archival function this reading identifies.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__archival_preservation, textual_tradition_itself, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(sacrifice_obligation_continuity__archival_preservation, textual_tradition_itself).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_obligation_continuity__archival_preservation, diffuse).
narrative_ontology:fixing_cost_class(sacrifice_obligation_continuity__archival_preservation, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves a specific and demanding body of legal-ritual text, its interpretive apparatus, and its associated communal memory across generations and geographic dispersion, so that historical and religious continuity is not lost even though the underlying practice has ceased.
% TRANSFER_FUNCTION: No material transfer occurs. Time and attention move from students and communities toward sustained engagement with the text; nothing is extracted from any party as a condition of participation, and non-participation carries no penalty.
% ABSENT_VOICES: Restorationist and messianic-suspension readers, who hold the obligation remains live in some form, are not represented in this reading's own framework — they would object that treating the law as pure archive concedes too much and forecloses readiness for restoration. They are not silenced globally; they simply hold a different reading, addressed in sibling constraints.
% DISAPPEARANCE_RATIONALE: If study of the sacrificial corpus vanished overnight, communal liturgy referencing the Temple service and its associated fast-day commemorations would lose an interpretive anchor, and a strand of scholarly and communal identity would erode over a generation — but no one's ritual duty, legal status, or material welfare would rearrange, since the reading holds no normative force is at stake. Whether that erosion counts as 'the world rearranging' is itself disputed between this reading's adherents (who would say little changes materially) and communities for whom the ongoing study is understood as a form of extended identity constitution (who would say a great deal changes).
% FOUNDING_PROBLEM: Following the destruction of the Temple, a fixed physical practice (sacrificial offering) became structurally impossible to perform, creating a genealogical and normative gap: what happens to a body of binding law whose object no longer exists?
% FOUNDING_PROBLEM_CORROBORATION: Historians of the post-Temple rabbinic period and comparative religion scholars outside any observant community attest that the practical possibility of Temple sacrifice ended with the physical destruction of the site and has not been restored in the intervening two millennia; this is corroborated by archaeological and historical consensus independent of any religious community's own doctrinal claims about whether the underlying obligation persists in some other form.
narrative_ontology:disappearance_verdict(sacrifice_obligation_continuity__archival_preservation, contested).
narrative_ontology:founding_problem_status(sacrifice_obligation_continuity__archival_preservation, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_continuity__archival_preservation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sacrifice_obligation_continuity__archival_preservation, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_continuity__archival_preservation, 0.03, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_continuity__archival_preservation_tests).
:- end_tests(sacrifice_obligation_continuity__archival_preservation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored near zero (0.03) because under this reading no one owes anything to anyone by virtue of the law's continued textual existence — there is no beneficiary collecting rents and no target bearing costs; scholarly and communal engagement is voluntary and reciprocally rewarding. Suppression is near zero (0.05) since nothing enforces study or penalizes non-study. Theater ratio is modest and declining (0.2 to 0.1) reflecting some initial performative overlay (commemorative recitation carrying residual ritual weight) that thins as the reading stabilizes into straightforward historical/cultural practice over the measured interval. Accessibility collapse is low (0.15): alternative framings (performance_only, messianic_suspension, study_as_performance) remain fully available to any community; nothing about this reading forecloses adopting a different one. Resistance is low (0.1): the reading is not actively fought by adherents of sibling readings so much as simply not shared by them — this is disagreement, not suppression.
 *
 * DIRECTIONALITY LOGIC:
 *   Every named party sits near the beneficiary end of directionality: textual scholars, cultural memory communities, and diaspora identity groups all gain from the archive's persistence and pay no structural cost for either participating or abstaining. There is no victim group because there is no obligation left to be victimized by non-compliance with. Restorationist communities are excluded from this reading's own framework (they hold a different reading), but they are not harmed by this reading's operation — they simply operate under a sibling constraint with a different ε.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (a binding sacrificial law whose object, the Temple, no longer exists) is dead under this reading's own account — the practice cannot occur and the reading does not claim it should. This is the case where mandatrophy resolution is clean rather than contested: the mandate (binding sacrificial obligation) is explicitly retired, and what remains (study) is honestly re-described as cultural/scholarly practice rather than continued fulfillment of the old mandate. This prevents the classic mandatrophy error of relabeling an obsolete obligation as still-live disguised extraction — this reading does the opposite of that error, which is precisely what distinguishes it from study_as_performance (which keeps the obligation alive through reinterpretation) and performance_only (which keeps the obligation alive by deferring its object).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    obligation_exit_vs_dormancy,
    'Has the sacrificial obligation genuinely exited constraint space (as this reading holds), or is it merely dormant/suspended pending conditions that could reactivate it (as messianic_suspension and performance_only hold)?',
    'No empirical resolution is possible from outside the interpretive traditions themselves; the question is a live doctrinal dispute within religious jurisprudence turning on whether a commandment tied to a destroyed physical object can be said to have ended or merely be held in abeyance. Comparative analysis of how each tradition''s own authorities treat analogous destroyed-object commandments could triangulate consistency, but not adjudicate the underlying claim.',
    'If genuinely exited (this reading), ε correctly stays near zero indefinitely. If merely dormant, a restoration event (real or perceived) would instantly reactivate normative force for adherents of that reading, and this reading''s own zero-extraction account would need revision for those adherents — though the archival_preservation reading as authored here would persist unchanged for communities who continue to hold it even after such an event.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(obligation_exit_vs_dormancy, conceptual, 'Whether the obligation has ended or is merely suspended is not resolvable by evidence; it is the central doctrinal fork the kernel''s four readings represent.').

omega_variable(
    study_as_identity_cost,
    'Does treating study as pure cultural memory (rather than as commandment-fulfillment) impose a diffuse identity cost on communities who psychologically experience the practice as more religiously weighty than ''archive maintenance,'' even if no material extraction occurs?',
    'Ethnographic or survey research into how practitioners describe their own motivation and psychological stakes in continued study, compared across communities holding different kernel readings.',
    'If such a cost exists, this reading''s ε=0.03 may understate a subtle affective extraction (the archival framing itself devalues what practitioners experience as sacred obligation) — though this would not change the reading''s own internal account, only flag a gap between the reading''s self-description and lived experience of some adherents.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(study_as_identity_cost, conceptual, 'Whether recasting obligation as archive imposes an unmeasured psychological cost distinct from material extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_continuity__archival_preservation, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_continuity__archival_preservation, theater_ratio, 0, 0.2).
narrative_ontology:measurement(sacr_tr_t20, sacrifice_obligation_continuity__archival_preservation, theater_ratio, 20, 0.16).
narrative_ontology:measurement(sacr_tr_t40, sacrifice_obligation_continuity__archival_preservation, theater_ratio, 40, 0.14).
narrative_ontology:measurement(sacr_tr_t60, sacrifice_obligation_continuity__archival_preservation, theater_ratio, 60, 0.12).
narrative_ontology:measurement(sacr_tr_t80, sacrifice_obligation_continuity__archival_preservation, theater_ratio, 80, 0.11).
narrative_ontology:measurement(sacr_tr_t100, sacrifice_obligation_continuity__archival_preservation, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_continuity__archival_preservation, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(sacr_be_t20, sacrifice_obligation_continuity__archival_preservation, base_extractiveness, 20, 0.04).
narrative_ontology:measurement(sacr_be_t40, sacrifice_obligation_continuity__archival_preservation, base_extractiveness, 40, 0.04).
narrative_ontology:measurement(sacr_be_t60, sacrifice_obligation_continuity__archival_preservation, base_extractiveness, 60, 0.03).
narrative_ontology:measurement(sacr_be_t80, sacrifice_obligation_continuity__archival_preservation, base_extractiveness, 80, 0.03).
narrative_ontology:measurement(sacr_be_t100, sacrifice_obligation_continuity__archival_preservation, base_extractiveness, 100, 0.03).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(sacrifice_obligation_continuity__archival_preservation, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_continuity__archival_preservation, identity_coordination).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__archival_preservation, sacrifice_obligation_continuity__study_as_performance).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__archival_preservation, sacrifice_obligation_continuity__performance_only).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__archival_preservation, sacrifice_obligation_continuity__messianic_suspension).

% DUAL FORMULATION NOTE:
% This is one of four sibling stories decomposing the natural-language label 'the sacrifice law obligation question' per the ε-invariance principle. Each reading (archival_preservation, study_as_performance, performance_only, messianic_suspension) is authored with its own ε because the readings make structurally distinct claims about whether obligation persists and, if so, how it is discharged. archival_preservation (this story) authors the lowest ε (0.03) because it is the only reading under which obligation has fully exited constraint space; the siblings retain some normative claim and correspondingly higher ε. All four stories link to each other via affects_constraints as members of one kernel family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
