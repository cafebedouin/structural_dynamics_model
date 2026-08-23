% ============================================================================
% CONSTRAINT STORY: kodashim_obligation__study_as_performance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_obligation__study_as_performance, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: kodashim_obligation__study_as_performance
 *   human_readable: Study-as-Performance Reading of the Kodashim Study Obligation
 *   domain: religious/textual_preservation
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the kodashim_obligation kernel:
 *   that studying the sacrificial orders enacts the cosmic function of
 *   sacrifice itself, such that the Temple's physical absence leaves the
 *   law's spiritual efficacy untouched. The arrangement rests on Hosea 14:3
 *   ('we will render as bulls the offerings of our lips') as activated in
 *   rabbinic law and formalized in Menachot 110a: whoever engages the laws of
 *   a burnt-offering is accounted as one who offered it. Under this reading
 *   the constraint is presented as a feature of the spiritual order rather
 *   than a human expedient — the service continues because its carrier is the
 *   studied word, not the altar. KEY AGENTS (by structural relationship):
 *   kodashim_students — primary practitioners (moderate/constrained), bearing
 *   the corpus's notorious difficulty and receiving merit and enacted service
 *   in return; rabbinic_academies — administering institutions
 *   (institutional/mobile), setting curricular weight and transmitting the
 *   reading; supporting_lay_communities — secondary beneficiaries
 *   (organized/mobile), funding and honoring the discipline;
 *   temple_restoration_movements — excluded objectors (organized/trapped)
 *   standing outside the curricular conversation;
 *   comparative_religion_scholars — analytical observers. Per the
 *   epsilon-invariance principle this file authors a single stable epsilon
 *   for the standing arrangement (the study-substitution practice as this
 *   reading assesses it); the sibling readings are separate constraints with
 *   their own epsilon values, linked via network.affects_constraints and
 *   documented in network.dual_formulation_note. The claim/metric
 *   independence rule is observed: claimed_type is mountain because the
 *   reading asserts a cosmic invariant, while the metrics are authored
 *   descriptively — near-zero extraction, minimal suppression, negligible
 *   resistance.
 *
 * KEY AGENTS:
 *   - kodashim_students: primary practitioners (moderate/constrained) — bear the study's labor, receive its merit and enacted service
 *   - rabbinic_academies: administering institutions (institutional/mobile) — set the discipline's weight, ordain its teachers, transmit the reading
 *   - supporting_lay_communities: secondary beneficiaries (organized/mobile) — fund and honor the study, receive vicarious participation
 *   - temple_restoration_movements: excluded objectors (organized/trapped) — press for physical restoration, outside the beit midrash conversation
 *   - comparative_religion_scholars: analytical observers (analytical/analytical) — document the substitution doctrine from outside the authority structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_obligation__study_as_performance, 0.06).
domain_priors:suppression_score(kodashim_obligation__study_as_performance, 0.08).
domain_priors:theater_ratio(kodashim_obligation__study_as_performance, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, extractiveness, 0.06).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, resistance, 0.07).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_obligation__study_as_performance, mountain).
narrative_ontology:human_readable(kodashim_obligation__study_as_performance, "Study-as-Performance Reading of the Kodashim Study Obligation").
narrative_ontology:topic_domain(kodashim_obligation__study_as_performance, "religious/textual_preservation").

domain_priors:emerges_naturally(kodashim_obligation__study_as_performance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_obligation__study_as_performance, 'b973b841-8ea9-498e-88c7-744dce4a81af').
narrative_ontology:cs_kernel_codification('b973b841-8ea9-498e-88c7-744dce4a81af', formalized).
narrative_ontology:cs_authority_grounding('b973b841-8ea9-498e-88c7-744dce4a81af', lineage).
narrative_ontology:cs_interpretation_layer_present('b973b841-8ea9-498e-88c7-744dce4a81af').
narrative_ontology:cs_reading_relation('b973b841-8ea9-498e-88c7-744dce4a81af', kodashim_obligation__study_as_preparation, coexists_with).
narrative_ontology:cs_reading_relation('b973b841-8ea9-498e-88c7-744dce4a81af', kodashim_obligation__study_as_archive, forecloses).
narrative_ontology:cs_axiom('b973b841-8ea9-498e-88c7-744dce4a81af', foundational, study_enacts_sacrificial_function).
narrative_ontology:cs_axiom_status(study_enacts_sacrificial_function, holdable).
narrative_ontology:cs_axiom_grounding('b973b841-8ea9-498e-88c7-744dce4a81af', study_enacts_sacrificial_function, theological).
narrative_ontology:cs_axiom('b973b841-8ea9-498e-88c7-744dce4a81af', foundational, temple_absence_irrelevant_to_efficacy).
narrative_ontology:cs_axiom_status(temple_absence_irrelevant_to_efficacy, holdable).
narrative_ontology:cs_axiom_grounding('b973b841-8ea9-498e-88c7-744dce4a81af', temple_absence_irrelevant_to_efficacy, theological).
narrative_ontology:cs_reference_frame('b973b841-8ea9-498e-88c7-744dce4a81af', study_performs_sacrifice).
narrative_ontology:cs_drift_state('b973b841-8ea9-498e-88c7-744dce4a81af', contemporary_academic_scrutiny, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b973b841-8ea9-498e-88c7-744dce4a81af', '').
narrative_ontology:cs_kernel_id(kodashim_obligation__study_as_performance, kodashim_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_performance, kodashim_students).
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_performance, rabbinic_academies).
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_performance, supporting_lay_communities).
narrative_ontology:constraint_vindicates(kodashim_obligation__study_as_performance, study_equals_sacrifice_doctrine).
narrative_ontology:constraint_vindicates(kodashim_obligation__study_as_performance, hosea_lips_as_bulls_principle).
narrative_ontology:constraint_vindicates(kodashim_obligation__study_as_performance, cosmic_service_continuity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Undertake the multi-year mastery of the sacrificial orders — Zevachim, Menachot, and their kin — as a core devotional discipline. The corpus is notoriously difficult and the labor substantial; the return, in the practice's own terms, is merit and the enacted service itself. Leaving the discipline carries identity and communal cost but no material penalty; nothing compels entry or continuation.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_performance, kodashim_students, beneficiary,
    moderate, biographical, constrained, global).

% Set the curricular weight of Kodashim, ordain its teachers, and transmit the teaching that study enacts the service. Institutional continuity, enrollment, and standing flow through the discipline's centrality. They could reallocate emphasis toward other orders at real administrative and reputational cost, but face no external compulsion in either direction.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_performance, rabbinic_academies, agenda_setter,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(kodashim_obligation__study_as_performance, rabbinic_academies, beneficiary).

% Fund and honor the study without undertaking its full rigor, receiving vicarious participation in the service and the communal identity it anchors. Their support is voluntary and revocable, and their connection runs through the academies they sustain.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_performance, supporting_lay_communities, beneficiary,
    organized, generational, mobile, global).

% Organize for rebuilding the Temple and resuming physical sacrifice; they regard the substitution of study for sacrifice as at best partial and press for the physical site. They stand outside the academies' curricular conversation — their objection is registered in activist and liturgical-political venues, not in the institutions that administer the discipline — and their goal remains blocked by political and halakhic conditions beyond their control.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_performance, temple_restoration_movements, excluded,
    organized, generational, trapped, regional).

% Study the substitution doctrine as a documented adaptation to the destruction of 70 CE, comparing it with other traditions' textualization of sacrifice. They take no seat in the practice, bear none of its obligations, and publish analyses that circulate outside the authority structure that would have to acknowledge them.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_performance, comparative_religion_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kodashim_obligation__study_as_performance, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Sustains continuous communal enactment of a sacrificial covenant whose physical site is unavailable, coordinating the community's ongoing relationship to the avodah through distributed textual mastery rather than a centralized altar.
% TRANSFER_FUNCTION: Moves devotional attention and scholarly labor from individual students into the maintained enactment of the sacrificial order; returns merit and standing to the students, institutional continuity to the academies, and — in the frame's own register — completed service to the cosmic order.
% ABSENT_VOICES: Temple restoration movements would object that substitution demotes the actual service and would press for physical restoration; secular descendants of studying families would decline the obligation altogether; historians reading the doctrine as post-destruction adaptation would object to its presentation as received cosmic fact. All three speak from outside the beit midrash in which the arrangement is administered.
% DISAPPEARANCE_RATIONALE: Within this reading's own frame the rearrangement is real and located in the cosmic register: the sacrificial service's operative channel ceases, and what the arrangement exists to perform goes unperformed. Materially, the visible rearrangement is narrower — yeshiva curricula reallocate hours, commentarial lines thin — but the reading's claim is precisely that the important register is not the material one.
% FOUNDING_PROBLEM: After the Temple's destruction in 70 CE, the covenantal service lost its site: how does Israel continue the avodah when the altar is gone? The rabbinic answer, activating Hosea 14:3 and formalized in Menachot 110a, relocated the service into the studied word.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested from outside the arrangement's administration: the daily liturgy itself retains the sacrificial passages (composed by parties outside the study arrangement's governing seats), academic historians of the Second Temple-to-rabbinic transition document the service gap and the deliberate substitution, and restorationist movements serve as hostile witnesses that the gap remains unfilled in their judgment. Stated plainly: no source outside the tradition can attest the reading's cosmic-efficacy claim itself — corroboration extends to the founding problem (the service gap), not to the metaphysical solution this reading offers for it.
narrative_ontology:disappearance_verdict(kodashim_obligation__study_as_performance, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_obligation__study_as_performance, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_obligation__study_as_performance, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kodashim_obligation__study_as_performance, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_obligation__study_as_performance, 0.06, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_obligation__study_as_performance_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, ExtMetricName, E),
    domain_priors:suppression_score(kodashim_obligation__study_as_performance, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(kodashim_obligation__study_as_performance),
    narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(kodashim_obligation__study_as_performance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored near zero (0.06 at interval end) because the arrangement channels voluntary devotional labor with no seat paying what another collects: students expend effort and receive merit and the enacted service itself within the frame; academies receive continuity as a byproduct of administering a practice they hold sacred, not as captured rent. Suppression is low (0.08): there is no enforcement machinery — requires_active_enforcement is false — only the ordinary social gravity of a honored discipline; suppression_requirement time-series are deliberately NOT authored because the enforcement picture is static (nothing is built up or eroded), and the scalar carries that fact. Theater ratio is low (0.10): under this reading the act IS the function, so nearly nothing is performative stand-in; the small residual and its gentle rise track the growing phenomenological distance from living sacrifice (elaborated Yom Kippur avodah recitations, simulated ceremonies), documented in the phenomenological_distance_theater_drift omega. Accessibility collapse is high (0.85): once the reading is granted, the option space closes almost completely — physical sacrifice is unavailable and no rival channel to the service exists, which is the mountain profile's expected shape. Resistance is near zero (0.07): virtually no one actively opposes the study; restorationists oppose the substitution CLAIM, not the discipline. All measurements run on one shared seven-point grid (70, 300, 700, 1100, 1500, 1850, 2020) with both tracked metrics authored at every point.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same structure. From the student seat the arrangement is near-symmetric: real labor paid, real merit and identity returned, exit costly mainly in identity rather than material terms — the identity-fusion mechanism here is professional-devotional (the scholar has become the kind of person for whom Kodashim mastery is self-constitutive); if that identity frame broke under secularization, the practicing population thins, which is precisely the erosion vector recorded in cs_structure.drift_state. From the academy seat the arrangement is pure stewardship: administering and benefiting with no extraction borne. From the excluded restorationist seat the same structure reads as a dignified substitute that quietly demotes the real service — but that seat is outside the beneficiary/victim structure and registers only as absent voice. The engine computes these divergences from the structural data; the authored mountain claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive every seat toward the low-d (subsidized) end: students, academies, and lay communities all sit near or below symmetry, and with no victims declared there is no high-d target seat at all. Residual effective extraction is therefore minimal; the global spatial scope would amplify whatever extraction exists, but the base sits beneath the identity_coordination Boltzmann floor (0.06 against a 0.08 floor), placing the entire measured burden inside inherent coordination cost. Suppression is authored as a raw structural property and is intentionally left unscaled — only extractiveness is scaled by directionality and scope in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — sustaining covenantal service after the sacrificial site's loss — remains live for as long as the Temple stands absent, so no mandate has outlived its function and no mandatrophy resolution is declared. The classification guards against two opposite misreadings: a pure-extraction lens would hunt for victims and find none, wrongly smelling a snare in a devotional obligation; a pure-social-coordination lens would flatten the reading's own claim, which is not merely that study coordinates the community but that it performs a cosmic function — the mountain claim encodes that metaphysical assertion. Because real actors do benefit (scholars, academies), the story is authored as an FSM candidate: the false-summit machinery is invited to test whether a constructed constraint wearing natural-law dress is detected behind the mountain claim, with the natural_law_vs_post_destruction_adaptation omega carrying the irreducible uncertainty the schema requires.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_post_destruction_adaptation,
    'Is the equation of Kodashim study with sacrificial performance a discovered feature of the cosmic order, or a post-70 CE rabbinic construction adopted to manage the crisis of the Temple''s loss?',
    'Historical-philological tracing of the substitution doctrine''s emergence: Hosea 14:3 predates the destruction but is activated as a legal principle at Yavneh; Menachot 110a formalizes the equivalence. Marks of adaptive invention versus received tradition in the tannaitic strata would discriminate.',
    'If constructed, the mountain claim fails and the constraint reclassifies through the false-summit path toward a coordination type, since identifiable actors (academies, scholars) demonstrably benefit from the doctrine; if received, the mountain profile stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_post_destruction_adaptation, conceptual, 'Whether the study-sacrifice equivalence is cosmic invariant or adaptive construction.').

omega_variable(
    kernel_reading_structural_delta,
    'This constraint is one reading of the kodashim_obligation kernel — what would the sibling readings (study_as_preparation, study_as_archive) change structurally if adopted?',
    'Adopting study_as_archive dissolves the obligation (study becomes voluntary identity-maintenance; the beneficiary set collapses to heritage communities and epsilon stays low but the normative structure vanishes). Adopting study_as_preparation raises epsilon (maintaining technically unused knowledge carries real cost borne by students for a distant beneficiary), adds a future messianic community to the beneficiary set, and makes Temple restoration structurally necessary.',
    'Archive adoption yields a rope-like identity-coordination constraint with no cosmic claim; preparation adoption yields a transitional arrangement closer to scaffold logic pending restoration. This file''s classification holds only for the performance reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer structure: sibling readings would alter epsilon, beneficiary sets, and the necessity of Temple restoration.').

omega_variable(
    efficacy_mechanism_location,
    'Where exactly is the load-bearing claim located: in the metaphysics of avodah (whether divine service requires physical matter) or in the normative status of study (obligation versus voluntariness)?',
    'Analytic separation of the two claims against rishonic positions on sacrificial rationale (Rambam''s teleological account versus Ramban''s ontological account bears directly on whether text can carry the service).',
    'If the service mechanism requires physical matter, the performance reading collapses into the preparation reading; if service is intention-and-word-carried, the performance reading stands independently of any restoration scenario.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficacy_mechanism_location, conceptual, 'Locating the disagreement between readings in the mechanism of divine service versus the normativity of study.').

omega_variable(
    phenomenological_distance_theater_drift,
    'Does the widening historical distance from living sacrifice gradually convert the study-function into performed memory rather than enacted service?',
    'Longitudinal ethnography of Kodashim pedagogy: whether students across generations report the study as avodah or as academic exercise; comparison of early amoraic testimony with contemporary yeshiva phenomenology.',
    'A sustained theater rise would push the constraint toward piton symptoms despite the frame''s claim; the flat low trajectory authored here sustains the mountain profile. The gentle rise to 0.10 tracks documented phenomenological distance while remaining far below degeneration thresholds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(phenomenological_distance_theater_drift, empirical, 'Whether temporal distance from actual sacrifice converts function into theater.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_obligation__study_as_performance, 70, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kodashim_perf_tr_t70, kodashim_obligation__study_as_performance, theater_ratio, 70, 0.04).
narrative_ontology:measurement(kodashim_perf_tr_t300, kodashim_obligation__study_as_performance, theater_ratio, 300, 0.05).
narrative_ontology:measurement(kodashim_perf_tr_t700, kodashim_obligation__study_as_performance, theater_ratio, 700, 0.06).
narrative_ontology:measurement(kodashim_perf_tr_t1100, kodashim_obligation__study_as_performance, theater_ratio, 1100, 0.07).
narrative_ontology:measurement(kodashim_perf_tr_t1500, kodashim_obligation__study_as_performance, theater_ratio, 1500, 0.08).
narrative_ontology:measurement(kodashim_perf_tr_t1850, kodashim_obligation__study_as_performance, theater_ratio, 1850, 0.09).
narrative_ontology:measurement(kodashim_perf_tr_t2020, kodashim_obligation__study_as_performance, theater_ratio, 2020, 0.1).

% Extraction over time
narrative_ontology:measurement(kodashim_perf_be_t70, kodashim_obligation__study_as_performance, base_extractiveness, 70, 0.03).
narrative_ontology:measurement(kodashim_perf_be_t300, kodashim_obligation__study_as_performance, base_extractiveness, 300, 0.03).
narrative_ontology:measurement(kodashim_perf_be_t700, kodashim_obligation__study_as_performance, base_extractiveness, 700, 0.04).
narrative_ontology:measurement(kodashim_perf_be_t1100, kodashim_obligation__study_as_performance, base_extractiveness, 1100, 0.04).
narrative_ontology:measurement(kodashim_perf_be_t1500, kodashim_obligation__study_as_performance, base_extractiveness, 1500, 0.05).
narrative_ontology:measurement(kodashim_perf_be_t1850, kodashim_obligation__study_as_performance, base_extractiveness, 1850, 0.05).
narrative_ontology:measurement(kodashim_perf_be_t2020, kodashim_obligation__study_as_performance, base_extractiveness, 2020, 0.06).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(kodashim_obligation__study_as_performance, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_obligation__study_as_performance, identity_coordination).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_performance, kodashim_obligation__study_as_preparation).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_performance, kodashim_obligation__study_as_archive).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'obligation to study Kodashim' decomposes into three structurally distinct claims sharing one kernel. This file (study_as_performance) authors epsilon ~0.06 with no victim set and no structural necessity of Temple restoration. study_as_preparation authors higher epsilon (carrying cost of unused technical knowledge borne by present students for a future beneficiary) and makes restoration structurally necessary. study_as_archive authors low epsilon but dissolves the obligation entirely, reducing the arrangement to voluntary identity-maintenance. The upstream/downstream gradient runs from this reading (highest empirical confidence within the tradition, least contingent) toward the archive reading (most dependent on secular-historical framing); each family member links the others via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
