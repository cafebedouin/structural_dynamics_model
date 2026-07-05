% ============================================================================
% CONSTRAINT STORY: kodashim_corpus__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: kodashim_corpus__performance_only
 *   human_readable: Kodashim as Suspended Blueprint Awaiting Messianic Restoration
 *   domain: religious/institutional
 *
 * SUMMARY:
 *   Tractates of Kodashim (Zevachim, Menachot, Chullin, Bekhorot, and related
 *   material) describe in exhaustive procedural detail the operation of the
 *   Temple sacrificial cultus. Since 70 CE no such cultus has existed. This
 *   story instantiates the performance_only reading of the kodashim_corpus
 *   kernel: the corpus is a husk — an archived operational blueprint whose
 *   legitimacy depends entirely on a specific future state (resumed physical
 *   sacrifice under messianic restoration) that has not occurred, cannot be
 *   verified as forthcoming, and cannot be falsified as never-forthcoming.
 *   Under this reading, institutions organized around active preparation for
 *   that restoration extract present-day legitimacy, funding, and devotional
 *   investment from a performance they structurally cannot deliver on any
 *   timeline they control or are accountable to. This is distinct from the
 *   study_as_exercise reading (where the study itself IS the mitzvah's
 *   performance, no future state required) and the substitution_archive
 *   reading (where prayer/study formally replaced sacrifice and the corpus is
 *   memorial, not operational) — those are separate constraints, not
 *   alternate measurements of this one, per the ε-invariance principle.
 *
 * KEY AGENTS:
 *   - messianic_preparation_institutions: primary beneficiary (institutional/arbitrage) — derive legitimacy and resources from perpetually-deferred performance
 *   - devotional_practitioners_treating_archive_as_living_practice: primary target (moderate/constrained) — invest devotion in an unverifiable future state
 *   - ordinary_talmud_students: excluded voice (moderate/mobile) — experience the corpus under a different reading entirely, not consulted
 *   - diaspora_religious_authorities: analytical observer (institutional/analytical) — adjudicate adjacent questions without resolving the kernel dispute
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_corpus__performance_only, 0.71).
domain_priors:suppression_score(kodashim_corpus__performance_only, 0.58).
domain_priors:theater_ratio(kodashim_corpus__performance_only, 0.66).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, extractiveness, 0.71).
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, theater_ratio, 0.66).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_corpus__performance_only, snare).
narrative_ontology:human_readable(kodashim_corpus__performance_only, "Kodashim as Suspended Blueprint Awaiting Messianic Restoration").
narrative_ontology:topic_domain(kodashim_corpus__performance_only, "religious/institutional").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_corpus__performance_only, '8d5428e8-5ca8-49f5-8854-13e3129d3662').
narrative_ontology:cs_kernel_codification('8d5428e8-5ca8-49f5-8854-13e3129d3662', fixed_text).
narrative_ontology:cs_authority_grounding('8d5428e8-5ca8-49f5-8854-13e3129d3662', lineage).
narrative_ontology:cs_interpretation_layer_present('8d5428e8-5ca8-49f5-8854-13e3129d3662').
narrative_ontology:cs_reading_relation('8d5428e8-5ca8-49f5-8854-13e3129d3662', kodashim_corpus__study_as_exercise, coexists_with).
narrative_ontology:cs_reading_relation('8d5428e8-5ca8-49f5-8854-13e3129d3662', kodashim_corpus__substitution_archive, coexists_with).
narrative_ontology:cs_axiom('8d5428e8-5ca8-49f5-8854-13e3129d3662', foundational, study_derives_legitimacy_from_future_performance).
narrative_ontology:cs_axiom_status(study_derives_legitimacy_from_future_performance, holdable).
narrative_ontology:cs_axiom_grounding('8d5428e8-5ca8-49f5-8854-13e3129d3662', study_derives_legitimacy_from_future_performance, theological).
narrative_ontology:cs_axiom('8d5428e8-5ca8-49f5-8854-13e3129d3662', secondary, restoration_timeline_unaccountable_to_present_verification).
narrative_ontology:cs_axiom_status(restoration_timeline_unaccountable_to_present_verification, holdable).
narrative_ontology:cs_axiom_grounding('8d5428e8-5ca8-49f5-8854-13e3129d3662', restoration_timeline_unaccountable_to_present_verification, theological).
narrative_ontology:cs_reference_frame('8d5428e8-5ca8-49f5-8854-13e3129d3662', temple_era_operational_cultus).
narrative_ontology:cs_drift_state('8d5428e8-5ca8-49f5-8854-13e3129d3662', contemporary_diaspora_observance, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('8d5428e8-5ca8-49f5-8854-13e3129d3662', '').
narrative_ontology:cs_kernel_id(kodashim_corpus__performance_only, kodashim_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_corpus__performance_only, messianic_preparation_institutions).
narrative_ontology:constraint_victim(kodashim_corpus__performance_only, devotional_practitioners_treating_archive_as_living_practice).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Yeshivot and institutes (notably in the Kohanic-preparation and Temple-restoration movements) that teach Kodashim as literal operational readiness for a coming Third Temple. They derive communal legitimacy, funding, and recruitment from the claim that this study constitutes concrete preparation for an imminent, specific future performance. Their standing does not depend on the performance ever actually occurring — it depends on the plausibility of its eventual arrival remaining uncontested. They can reframe timelines indefinitely without cost.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, messianic_preparation_institutions, beneficiary,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(kodashim_corpus__performance_only, messianic_preparation_institutions, agenda_setter).

% Individual students and lay devotees who invest years of study, emotional identification, and sometimes financial support into Kodashim on the understanding that they are rehearsing an act they may yet perform or witness performed. Their devotion is real and costly in time and attention; the object of that devotion — an actual functioning sacrificial cultus — has not existed for nearly two millennia and its return is not within their control or verification. Exit is constrained by community embeddedness, not by law: leaving the belief structure risks social and identity rupture within observant communities.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, devotional_practitioners_treating_archive_as_living_practice, payer,
    moderate, biographical, constrained, national).

% Students who study Kodashim as part of standard Talmudic curriculum without messianic urgency, treating it as intellectual/legal exercise (the sibling reading's territory). Their experience of the same texts is structurally different from the performance_only reading and they would object to being folded into a restoration-preparation framing, but the messianic institutions' framing dominates public and fundraising discourse about the tractates' purpose, and the study-as-exercise voice is not solicited when preparation-institutions describe why the study matters.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, ordinary_talmud_students, excluded,
    moderate, biographical, mobile, national).

% Rabbinic bodies and halachic authorities who must adjudicate, generation after generation, whether and how Kodashim study functions religiously in the absence of the Temple. They observe the messianic-preparation framing operating alongside competing frames and occasionally rule on questions bearing on it (e.g., permissibility of practical steps toward restoration) without being able to resolve the underlying kernel dispute.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, diaspora_religious_authorities, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves detailed legal-ritual knowledge of the sacrificial system across a multi-millennium interruption, so that if and when the conditions for performance recur, the corpus is available rather than lost — a genuine transmission/preservation function independent of when or whether restoration occurs.
% TRANSFER_FUNCTION: Moves communal legitimacy, institutional funding, and devotional time-investment from ordinary study-communities toward messianic-preparation institutions, in exchange for a promised future state (restored sacrificial performance) that the institutions cannot deliver and are not accountable for failing to deliver.
% ABSENT_VOICES: Practitioners of the study_as_exercise and substitution_archive readings would object that framing the corpus as a suspended operational blueprint misdescribes what the study actually accomplishes for them; their objection is rarely heard because messianic-preparation institutions control much of the public and fundraising narrative about why Kodashim study matters.
% DISAPPEARANCE_RATIONALE: If the performance_only framing vanished overnight, the underlying text-study would continue essentially unchanged (supporting world_unchanged, since the study_as_exercise and substitution_archive readings would simply absorb the practice) — but the specific institutions built around restoration-preparation, their fundraising, recruitment, and communal identity, would lose their distinguishing claim and would have to either dissolve or re-found themselves on different grounds (supporting world_rearranges for that specific institutional layer). The verdict differs by which layer of the constraint you ask about, which is itself the contested feature.
% FOUNDING_PROBLEM: The Mishnaic and Talmudic redactors needed to preserve exact operational knowledge of Temple sacrifice after its physical cessation (70 CE), so that the law would not be lost to memory even without a functioning Temple to test it against.
% FOUNDING_PROBLEM_CORROBORATION: Messianic-preparation institutions attest the founding problem is fully live — restoration is imminent and the preparatory function is active and urgent. Academic historians of rabbinic Judaism and comparative-religion scholars outside these institutions (and many mainstream halachic authorities who favor the substitution_archive or study_as_exercise readings) attest that the original preservation problem was substantially resolved by successful textual transmission itself centuries ago, and that the 'active preparation for imminent restoration' framing is a later devotional overlay rather than the founding function.
narrative_ontology:disappearance_verdict(kodashim_corpus__performance_only, contested).
narrative_ontology:founding_problem_status(kodashim_corpus__performance_only, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_corpus__performance_only, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(kodashim_corpus__performance_only, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_corpus__performance_only, 0.71, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored high and rising (0.35→0.71) because the performance_only framing's legitimacy claim compounds over time: each generation of non-restoration does not falsify the framing (since messianic timelines are unfalsifiable by design) but does increase the ratio of extracted devotion/resources to any demonstrated progress toward the promised state. Theater ratio is authored moderately high and rising in parallel (0.40→0.66) because an increasing share of preparation-institution activity (ritual garment fabrication, priestly lineage verification, red-heifer breeding programs) is symbolic anticipation rather than anything that could actually be tested against a functioning cultus — the performative content of 'preparation' grows precisely because the underlying performance remains permanently unavailable to check it against. Suppression (0.58) is moderate rather than extreme: no one is coerced into this reading, but community embeddedness and identity-fusion with restoration hope make internal dissent costly. Accessibility collapse is only moderate (0.42) because the sibling readings remain visibly available within the same tradition — a practitioner CAN adopt study_as_exercise or substitution_archive instead, which is exactly why this is authored as one reading among coexisting readings rather than a totalizing mountain.
 *
 * DIRECTIONALITY LOGIC:
 *   Messianic-preparation institutions sit at the low-d/beneficiary end: they collect communal standing and resources and bear no accountability cost when restoration fails to arrive, because the promise has no due date they set. Devotional practitioners sit at the high-d/target end: their investment of years and identity is real and irreversible even though the promised return (participating in or witnessing restored sacrifice) is structurally deferred past any checkable horizon. Ordinary Talmud students are not directly extracted from by THIS reading — they are simply excluded from having their (different) experience of the same corpus represented in the dominant public narrative, which is a distinct harm (misrepresentation) from extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preserve operational sacrificial law against loss) was resolved by successful textual transmission itself, likely many centuries ago — the corpus survived; the knowledge is not lost. What has NOT resolved, and cannot resolve by the reading's own terms, is the performance the preparation institutions claim to be preparing for. This is the mandatrophy signature: an arrangement whose original problem (preservation) is dead but which persists by continuously re-asserting a DIFFERENT, unfalsifiable problem (imminent restoration) that was never the actual founding function. Classifying this reading as snare rather than mountain or rope prevents the coordination story (legitimate textual preservation, which is real) from laundering the extraction story (perpetual-deferral legitimacy claims, which are not the same thing) into a single unexamined 'sacred study' bundle.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indeterminacy,
    'Is the kodashim_corpus kernel genuinely read by its own tradition as performance_only (suspended blueprint), or is that reading itself a minority/contested framing within a tradition that mostly holds study_as_exercise or substitution_archive?',
    'Comparative survey of halachic literature and communal practice across denominations and eras to establish which reading(s) predominate at which times; the performance_only reading''s prevalence is itself an empirical-historical question distinct from its internal coherence.',
    'If performance_only is a genuine minority reading actively promoted by a specific institutional cluster rather than the tradition''s dominant self-understanding, the snare classification applies narrowly to that cluster and its adherents, not to Kodashim study broadly — this bounds the constraint''s scope considerably.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Whether performance_only is the tradition''s dominant reading or one contested reading among several live options.').

omega_variable(
    unfalsifiability_vs_genuine_hope,
    'Does the unfalsifiable structure of the restoration promise (no verifiable timeline, no accountability mechanism) constitute extraction, or is it simply the honest epistemic shape of any sincerely-held eschatological hope, which should not be penalized as extractive merely for being long-term and unverifiable?',
    'Compare structurally similar unfalsifiable future-oriented commitments (other messianic/eschatological traditions) for whether resource concentration correlates with institutional benefit independent of adherent benefit, versus cases where the hope-structure produces no institutional capture.',
    'If no institutional capture is demonstrable — if messianic-preparation institutions do not disproportionately benefit relative to ordinary adherents — the classification should move toward rope (genuine shared hope-coordination) rather than snare (asymmetric extraction).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(unfalsifiability_vs_genuine_hope, conceptual, 'Whether unfalsifiability itself is extractive or whether extraction requires demonstrated asymmetric institutional benefit beyond the unfalsifiability structure.').

omega_variable(
    sibling_reading_resource_competition,
    'Do the three kernel readings (performance_only, study_as_exercise, substitution_archive) compete for the same finite pool of communal legitimacy and resources, such that performance_only''s institutional success comes partly at the others'' expense?',
    'Track funding, enrollment, and public-narrative share across institutions organized around each reading over time; a rising performance_only share coinciding with declining resources for study_as_exercise-oriented institutions would support competitive displacement.',
    'If the readings are in zero-sum resource competition, the performance_only reading''s extraction includes a displacement effect on sibling-reading institutions and adherents, not just the direct extraction from its own devotees — this would raise the effective extractiveness beyond what this story alone captures.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_resource_competition, empirical, 'Whether the kernel readings compete for finite institutional resources such that one reading''s extraction displaces the others.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_corpus__performance_only, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_corpus__performance_only, theater_ratio, 0, 0.4).
narrative_ontology:measurement_basis(koda_tr_t0, observed).
narrative_ontology:measurement(koda_tr_t20, kodashim_corpus__performance_only, theater_ratio, 20, 0.46).
narrative_ontology:measurement_basis(koda_tr_t20, observed).
narrative_ontology:measurement(koda_tr_t40, kodashim_corpus__performance_only, theater_ratio, 40, 0.52).
narrative_ontology:measurement_basis(koda_tr_t40, observed).
narrative_ontology:measurement(koda_tr_t60, kodashim_corpus__performance_only, theater_ratio, 60, 0.57).
narrative_ontology:measurement_basis(koda_tr_t60, observed).
narrative_ontology:measurement(koda_tr_t80, kodashim_corpus__performance_only, theater_ratio, 80, 0.62).
narrative_ontology:measurement_basis(koda_tr_t80, observed).
narrative_ontology:measurement(koda_tr_t100, kodashim_corpus__performance_only, theater_ratio, 100, 0.66).
narrative_ontology:measurement_basis(koda_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_corpus__performance_only, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(koda_be_t0, observed).
narrative_ontology:measurement(koda_be_t20, kodashim_corpus__performance_only, base_extractiveness, 20, 0.42).
narrative_ontology:measurement_basis(koda_be_t20, observed).
narrative_ontology:measurement(koda_be_t40, kodashim_corpus__performance_only, base_extractiveness, 40, 0.5).
narrative_ontology:measurement_basis(koda_be_t40, observed).
narrative_ontology:measurement(koda_be_t60, kodashim_corpus__performance_only, base_extractiveness, 60, 0.58).
narrative_ontology:measurement_basis(koda_be_t60, observed).
narrative_ontology:measurement(koda_be_t80, kodashim_corpus__performance_only, base_extractiveness, 80, 0.65).
narrative_ontology:measurement_basis(koda_be_t80, observed).
narrative_ontology:measurement(koda_be_t100, kodashim_corpus__performance_only, base_extractiveness, 100, 0.71).
narrative_ontology:measurement_basis(koda_be_t100, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(kodashim_corpus__performance_only, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_corpus__performance_only, identity_coordination).
narrative_ontology:affects_constraint(kodashim_corpus__performance_only, kodashim_corpus__study_as_exercise).
narrative_ontology:affects_constraint(kodashim_corpus__performance_only, kodashim_corpus__substitution_archive).

% DUAL FORMULATION NOTE:
% These three stories are sibling readings of a single contested kernel (kodashim_corpus): performance_only (this story, snare — legitimacy from an unrealizable future performance), study_as_exercise (expected rope/mountain-adjacent — the study itself completes the mitzvah, no future dependency), and substitution_archive (expected low-extraction memorial record — prayer/study formally superseded sacrifice). They share underlying text but instantiate structurally distinct constraints with different ε, different beneficiary/victim structures, and different classifications, per the ε-invariance principle. Link all three via affects_constraints; do not merge them into one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
