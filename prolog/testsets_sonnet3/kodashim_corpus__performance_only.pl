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
 *   human_readable: Kodashim as Archived Blueprint Awaiting Messianic Restoration (Performance-Only Reading)
 *   domain: religious/institutional
 *
 * SUMMARY:
 *   This constraint captures the performance-only reading of the Kodashim
 *   kernel: the position that the corpus is not itself the discharge of the
 *   mitzvah (contra study-as-exercise) and not merely a memorial of a
 *   superseded practice (contra substitution_archive), but an archived
 *   operational blueprint held in reserve, whose full legitimacy is realized
 *   only when physical sacrifice resumes under messianic restoration. Under
 *   this reading, present-day study derives its urgency and much of its
 *   institutional funding from a future state of affairs — resumed Temple
 *   service — that the reading itself acknowledges cannot presently be
 *   delivered and cannot be scheduled. That gap between claimed future
 *   performance and deliverable present state is the extraction mechanism:
 *   legitimacy, devotion, and resources are drawn now against a performance
 *   that remains permanently deferred by the reading's own terms. This is
 *   authored as one reading among three siblings (study_as_exercise,
 *   substitution_archive) sharing the same kernel; each sibling reading is a
 *   separate constraint story with its own ε and stakeholder structure, per
 *   the ε-invariance principle.
 *
 * KEY AGENTS:
 *   - messianic_preparation_institutions: institutional beneficiary drawing funding and standing from imminence framing
 *   - kollel_kodashim_faculty: organized beneficiary whose career legitimacy rides on the operational-readiness framing
 *   - lay_students_treating_archive_as_living_practice: identity-locked payer whose devotion is misallocated toward an unrealizable near-term state
 *   - communities_funding_restoration_preparation: constrained-exit payer funding preparatory projects under urgency premises
 *   - study_as_exercise_adherents: excluded sibling-reading holders whose competing claim is not admitted into this reading's self-justification
 *   - rabbinic_courts_of_precedent: analytical observer supplying the doctrinal record against which imminence claims can be checked
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_corpus__performance_only, 0.71).
domain_priors:suppression_score(kodashim_corpus__performance_only, 0.48).
domain_priors:theater_ratio(kodashim_corpus__performance_only, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, extractiveness, 0.71).
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_corpus__performance_only, snare).
narrative_ontology:human_readable(kodashim_corpus__performance_only, "Kodashim as Archived Blueprint Awaiting Messianic Restoration (Performance-Only Reading)").
narrative_ontology:topic_domain(kodashim_corpus__performance_only, "religious/institutional").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_corpus__performance_only, '10bb8438-ee21-4659-8600-4b341be4508d').
narrative_ontology:cs_kernel_codification('10bb8438-ee21-4659-8600-4b341be4508d', fixed_text).
narrative_ontology:cs_authority_grounding('10bb8438-ee21-4659-8600-4b341be4508d', lineage).
narrative_ontology:cs_interpretation_layer_present('10bb8438-ee21-4659-8600-4b341be4508d').
narrative_ontology:cs_reading_relation('10bb8438-ee21-4659-8600-4b341be4508d', kodashim_corpus__study_as_exercise, coexists_with).
narrative_ontology:cs_reading_relation('10bb8438-ee21-4659-8600-4b341be4508d', kodashim_corpus__substitution_archive, influences).
narrative_ontology:cs_axiom('10bb8438-ee21-4659-8600-4b341be4508d', foundational, physical_performance_is_metaphysically_required).
narrative_ontology:cs_axiom_status(physical_performance_is_metaphysically_required, holdable).
narrative_ontology:cs_axiom_grounding('10bb8438-ee21-4659-8600-4b341be4508d', physical_performance_is_metaphysically_required, deontological).
narrative_ontology:cs_axiom('10bb8438-ee21-4659-8600-4b341be4508d', secondary, study_confers_preparatory_not_terminal_merit).
narrative_ontology:cs_axiom_status(study_confers_preparatory_not_terminal_merit, holdable).
narrative_ontology:cs_axiom_grounding('10bb8438-ee21-4659-8600-4b341be4508d', study_confers_preparatory_not_terminal_merit, conventional).
narrative_ontology:cs_reference_frame('10bb8438-ee21-4659-8600-4b341be4508d', temple_era_operational_sacrificial_order).
narrative_ontology:cs_drift_state('10bb8438-ee21-4659-8600-4b341be4508d', contemporary_post_destruction_diaspora, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('10bb8438-ee21-4659-8600-4b341be4508d', '').
narrative_ontology:cs_kernel_id(kodashim_corpus__performance_only, kodashim_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_corpus__performance_only, messianic_preparation_institutions).
narrative_ontology:constraint_beneficiary(kodashim_corpus__performance_only, kollel_kodashim_faculty).
narrative_ontology:constraint_victim(kodashim_corpus__performance_only, lay_students_treating_archive_as_living_practice).
narrative_ontology:constraint_victim(kodashim_corpus__performance_only, communities_funding_restoration_preparation).
narrative_ontology:constraint_vindicates(kodashim_corpus__performance_only, temple_service_requires_physical_performance).
narrative_ontology:constraint_vindicates(kodashim_corpus__performance_only, study_without_performance_cannot_discharge_the_mitzvah).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Yeshivot, kollelim, and organizations dedicated to preparing halachic and practical readiness for a rebuilt Temple (e.g. priestly-lineage training, vessel reconstruction projects). They derive institutional legitimacy, funding, and a distinct communal identity from framing Kodashim study as preparation for an event that has not occurred and whose timing they do not control. Their exit is effectively arbitrage: they can shift emphasis, rebrand as historical or halachic scholarship, or double down on imminence claims as circumstances demand, without ever being falsified in their own lifetime.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, messianic_preparation_institutions, beneficiary,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(kodashim_corpus__performance_only, messianic_preparation_institutions, agenda_setter).

% Scholars and teachers who build careers, stipends, and communal standing on transmitting Kodashim law as a corpus of practical readiness rather than pure intellectual archive. They can relocate to other tractates or reframe their standing (mobile exit) if the performance-only framing loses institutional traction, but currently benefit from the elevated stakes the framing confers on their expertise.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, kollel_kodashim_faculty, beneficiary,
    organized, generational, mobile, national).

% Individuals who invest years of study, emotional anticipation, and identity formation around the idea that they are preparing to actually perform sacrificial service. Their devotion is directed at a state of affairs (resumed physical sacrifice) that the reading itself declares unrealizable within any horizon they control. Exit is identity-locked: reframing the study as 'merely' historical or substitutionary would require dismantling a self-concept built around imminent restoration, which the community around them actively discourages.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, lay_students_treating_archive_as_living_practice, payer,
    moderate, biographical, identity_locked, local).

% Donor networks and congregations that allocate charitable resources toward Temple-restoration-adjacent projects (vessel replicas, priestly genealogy verification, red heifer breeding programs) on the premise that these are urgent preparatory acts. Their exit is constrained by communal social pressure and the reputational cost of publicly withdrawing support from a sacred project, even where the material return on preparation is nil under this reading's own terms.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, communities_funding_restoration_preparation, payer,
    organized, generational, constrained, national).

% Those who hold that intellectual engagement with Kodashim IS the performance of the mitzvah, not preparation for a future one. Under the performance-only reading, their position is treated as a consolation or lesser form rather than a live alternative, and their objection — that the husk framing manufactures unrealizable stakes where none are structurally necessary — does not enter the performance-only institutions' self-justification.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, study_as_exercise_adherents, excluded,
    organized, generational, mobile, national).

% Historical and contemporary halachic authorities who have ruled on the status of Temple-era law across centuries of non-performance. They supply the doctrinal record against which any given generation's performance-only claims can be checked, without themselves being parties who benefit or pay under this specific reading.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, rabbinic_courts_of_precedent, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kodashim_corpus__performance_only, messianic_preparation_institutions).
narrative_ontology:fixing_cost_class(kodashim_corpus__performance_only, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves, in complete and technically precise form, the legal architecture of sacrificial service, so that if performance ever becomes possible again there is no gap in transmitted knowledge — a genuine hedge against irrecoverable loss of a practice.
% TRANSFER_FUNCTION: Moves devotion, funding, career investment, and identity-formation from lay students and donor communities toward institutions and faculty whose standing depends on the corpus being read as urgent operational preparation rather than closed historical record.
% ABSENT_VOICES: Adherents of the study-as-exercise reading are structurally present in the same batei midrash but treated as holding a fallback position rather than a coequal claim; their view that the mitzvah is already fully discharged through study is not admitted into the performance-only institutions' fundraising or curricular self-description.
% DISAPPEARANCE_RATIONALE: If the performance-only framing vanished overnight — if the corpus were universally read as either closed memorial or already-discharged study-mitzvah — messianic-preparation institutions would lose their distinctive fundraising rationale, restoration-adjacent projects would lose their urgency premise, and lay students currently oriented toward imminent performance would need to reconstitute their relationship to the material entirely.
% FOUNDING_PROBLEM: Following the Temple's destruction, the sacrificial law risked being lost entirely as living legal knowledge; the corpus was maintained so that the tradition would survive intact regardless of when or whether performance resumed.
% FOUNDING_PROBLEM_CORROBORATION: Historical rabbinic authorities across centuries (documented in halachic responsa on the status of Kodashim study, e.g. discussions in Talmudic and post-Talmudic sources on why one studies sacrificial order despite no functioning Temple) attest the founding problem was preservation against loss, not manufactured operational urgency. Contemporary critics outside the messianic-preparation institutions — including adherents of the study-as-exercise and substitution-archive readings, and some academic scholars of halachic history — attest that the performance-only framing's emphasis on imminent restoration is a later interpretive layer serving institutional and psychological functions distinct from the original preservation rationale; no source independent of the messianic-preparation institutions themselves corroborates that restoration is imminent or that current study functions as literal operational readiness.
narrative_ontology:disappearance_verdict(kodashim_corpus__performance_only, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_corpus__performance_only, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_corpus__performance_only, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored high (0.71 at interval end) because the reading structurally requires an unrealized future condition (resumed sacrifice) as the source of its present legitimacy — a textbook unrealizable-future-state extraction pattern. Suppression is moderate (0.48) rather than high: there is no coercive apparatus forcing belief, but social and communal pressure against reframing is real. Theater ratio is authored substantial and rising (0.40 to 0.62) because an increasing share of institutional activity (vessel reconstruction, priestly genealogy projects) is performative preparation for an event with no scheduled occurrence, rather than the more modest, historically-grounded preservation function the corpus originally served. Accessibility collapse is moderate (0.40): alternative readings (study-as-exercise, substitution-archive) remain visibly available within the same tradition, so alternatives have not fully collapsed, but resistance to reframing is real (0.55) because identity and institutional investment are already sunk into the performance-only frame.
 *
 * DIRECTIONALITY LOGIC:
 *   Messianic-preparation institutions and their faculty sit near the beneficiary end: they set the interpretive agenda, collect funding and status, and retain arbitrage-grade exit (able to reframe if the premise becomes untenable) or mobile exit (faculty can shift emphasis). Lay students and funding communities sit near the target end: they bear the cost of misallocated devotion and resources, and their exit is identity-locked or socially constrained respectively — reframing would require dismantling communal identity or incurring reputational cost. The study-as-exercise adherents are excluded rather than coordinated: their competing claim is not defeated on the merits within this reading, simply omitted from its self-justifying narrative.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preserving sacrificial law against irrecoverable loss — is corroborated as historically live by rabbinic sources across centuries and remains a defensible preservation rationale on its own terms. What distinguishes the performance-only reading as extractive is not preservation itself (a genuine coordination function, which is why this is authored as a snare with a real coordination story as cover, not a pure fabrication) but the specific move of treating the preserved archive as continuously operationally urgent, generation after generation, without any mechanism by which imminence claims could be falsified or retired. The classification prevents mislabeling the underlying preservation function (which the study-as-exercise and substitution-archive readings capture without the extraction) as identical to this reading's performance-contingent urgency framing — the two are structurally distinct even though they share a text.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    imminence_falsifiability,
    'Is there any communally-recognized mechanism by which a claim of messianic imminence could be falsified or retired, or is the performance-only framing structurally unfalsifiable across all time horizons?',
    'Survey of historical instances where messianic-preparation institutions revised or abandoned imminence claims in response to disconfirming events, versus instances of claim-reassertion after disconfirmation (a documented pattern of unfalsifiability would strengthen the snare classification).',
    'If no retraction mechanism exists and claims are reasserted after every disconfirmation, the extraction is structurally permanent rather than a temporary overreach correctable by evidence — strengthening snare over tangled_rope. If retraction has occurred historically, this reading may be better modeled as scaffold-like (temporary urgency framing pending correction) rather than snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(imminence_falsifiability, empirical, 'Whether messianic-imminence claims within this reading are falsifiable in practice.').

omega_variable(
    reading_boundary_location,
    'Where exactly does the performance_only reading diverge from study_as_exercise — is the dividing line the metaphysical claim (sacrifice must be physically resumed) or the practical claim (present study confers less merit than future performance would)?',
    'Close textual analysis of primary halachic sources distinguishing ''the mitzvah is discharged by study'' positions from ''study earns credit toward, but does not equal, future performance'' positions — these are two distinct metaphysical claims sometimes conflated under the same institutional label.',
    'If the dividing line is primarily the practical-merit claim rather than the metaphysical-necessity claim, the performance_only reading and study_as_exercise reading may be closer structural neighbors than the coexists_with relation suggests, with implications for how much this reading forecloses versus merely competes with its sibling.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_boundary_location, conceptual, 'Precise location of the structural boundary between this reading and study_as_exercise.').

omega_variable(
    coordination_extraction_separability,
    'Is the preservation-of-legal-knowledge coordination function structurally separable from the imminence-urgency extraction mechanism, or does removing the urgency framing degrade the preservation motivation itself?',
    'Comparative study of communities that maintain rigorous Kodashim study under explicitly non-imminent framings (e.g. study_as_exercise or substitution_archive communities) — if preservation quality and study intensity remain comparable, the functions are separable and the urgency framing is pure extractive overlay.',
    'If separable, the extraction is cleanly severable from the legitimate preservation function, supporting a clean snare classification for the urgency-specific overlay. If inseparable, some of the measured extraction is better understood as the price of sustaining preservation motivation across generations, which would argue for tangled_rope rather than pure snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether imminence-urgency extraction can be separated from the genuine preservation coordination function.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_corpus__performance_only, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_corpus__performance_only, theater_ratio, 0, 0.4).
narrative_ontology:measurement(koda_tr_t8, kodashim_corpus__performance_only, theater_ratio, 8, 0.46).
narrative_ontology:measurement(koda_tr_t16, kodashim_corpus__performance_only, theater_ratio, 16, 0.51).
narrative_ontology:measurement(koda_tr_t24, kodashim_corpus__performance_only, theater_ratio, 24, 0.56).
narrative_ontology:measurement(koda_tr_t32, kodashim_corpus__performance_only, theater_ratio, 32, 0.59).
narrative_ontology:measurement(koda_tr_t40, kodashim_corpus__performance_only, theater_ratio, 40, 0.62).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_corpus__performance_only, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(koda_be_t8, kodashim_corpus__performance_only, base_extractiveness, 8, 0.55).
narrative_ontology:measurement(koda_be_t16, kodashim_corpus__performance_only, base_extractiveness, 16, 0.6).
narrative_ontology:measurement(koda_be_t24, kodashim_corpus__performance_only, base_extractiveness, 24, 0.65).
narrative_ontology:measurement(koda_be_t32, kodashim_corpus__performance_only, base_extractiveness, 32, 0.68).
narrative_ontology:measurement(koda_be_t40, kodashim_corpus__performance_only, base_extractiveness, 40, 0.71).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(kodashim_corpus__performance_only, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_corpus__performance_only, identity_coordination).
narrative_ontology:affects_constraint(kodashim_corpus__performance_only, kodashim_corpus__study_as_exercise).
narrative_ontology:affects_constraint(kodashim_corpus__performance_only, kodashim_corpus__substitution_archive).

% DUAL FORMULATION NOTE:
% This story, kodashim_corpus__study_as_exercise, and kodashim_corpus__substitution_archive are three readings of a single contested kernel (kodashim_corpus): whether present engagement with the sacrificial-law corpus is (a) a husk awaiting future performance it cannot deliver [this story, snare], (b) itself the full performance of the mitzvah [study_as_exercise, expected rope-like], or (c) a closed memorial of a superseded practice [substitution_archive, expected mountain-or-rope-like]. Each reading has its own ε, beneficiary/victim structure, and type, per the ε-invariance principle — they are not the same constraint viewed three ways but three structurally distinct constraints sharing a text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
