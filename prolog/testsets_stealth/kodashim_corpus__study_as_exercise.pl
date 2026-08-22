% ============================================================================
% CONSTRAINT STORY: kodashim_corpus__study_as_exercise
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_corpus__study_as_exercise, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: kodashim_corpus__study_as_exercise
 *   human_readable: Kodashim Corpus Occupied Through Study (Study-as-Exercise Reading)
 *   domain: religious_studies/rabbinic_judaism/commitment_system
 *
 * SUMMARY:
 *   After 70 CE the sacrificial commandments became unperformable, and the
 *   rabbinic tradition answered with the doctrine recorded at Menachot 110a:
 *   whoever engages in the study of the laws of a burnt-offering is as if he
 *   offered it. This story instantiates the study_as_exercise READING of the
 *   kodashim_corpus kernel — the claim that the kernel is not a husk awaiting
 *   restoration and not a memorial archive, but a living commandment occupied
 *   through continuous intellectual-spiritual engagement. Per the
 *   epsilon-invariance discipline, this file authors ONLY this reading as one
 *   clean constraint with one stable referent (the standing arrangement: the
 *   norm and practice that studying the sacrificial tractates constitutes
 *   performing them); the sibling readings are separate constraint files
 *   linked through the network block, not folded into this one. The claim and
 *   the metrics are independent authored facts: the type is claimed from
 *   structure (pure coordination, net beneficiaries, no victim set), and the
 *   metrics are authored from the constraint's actual operation — they happen
 *   to agree here, which is the finding, not a tuning outcome.
 *
 * KEY AGENTS:
 *   - - kodashim_scholars: Primary beneficiary (institutional/constrained) — occupy the kernel through analysis; collect meaning, standing, and continuity
 *   - - yeshiva_students: Secondary beneficiary (moderate/constrained) — formed through guided engagement with the corpus
 *   - - daily_study_participants: Broad beneficiary base (organized/mobile) — mass cyclical engagement at freely chosen cost
 *   - - torah_curriculum_administrators: Agenda-setter and secondary beneficiary (institutional/identity_locked) — set curricula, run cycles, certify mastery; their authority is constituted by the practice
 *   - - secular_critics_of_sacrificial_study: Excluded voice (moderate/mobile) — object from outside publics with no standing in the conversation
 *   - - academic_religion_scholars: Analytical observer (institutional/analytical) — document the practice comparatively without normative position
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_corpus__study_as_exercise, 0.03).
domain_priors:suppression_score(kodashim_corpus__study_as_exercise, 0.08).
domain_priors:theater_ratio(kodashim_corpus__study_as_exercise, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, extractiveness, 0.03).
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_corpus__study_as_exercise, rope).
narrative_ontology:human_readable(kodashim_corpus__study_as_exercise, "Kodashim Corpus Occupied Through Study (Study-as-Exercise Reading)").
narrative_ontology:topic_domain(kodashim_corpus__study_as_exercise, "religious_studies/rabbinic_judaism/commitment_system").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_corpus__study_as_exercise, '334e269e-e0a8-43fb-9e1c-7714e52982cb').
narrative_ontology:cs_kernel_codification('334e269e-e0a8-43fb-9e1c-7714e52982cb', fixed_text).
narrative_ontology:cs_authority_grounding('334e269e-e0a8-43fb-9e1c-7714e52982cb', lineage).
narrative_ontology:cs_interpretation_layer_present('334e269e-e0a8-43fb-9e1c-7714e52982cb').
narrative_ontology:cs_reading_relation('334e269e-e0a8-43fb-9e1c-7714e52982cb', kodashim_corpus__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('334e269e-e0a8-43fb-9e1c-7714e52982cb', kodashim_corpus__substitution_archive, coexists_with).
narrative_ontology:cs_axiom('334e269e-e0a8-43fb-9e1c-7714e52982cb', foundational, study_constitutes_offering).
narrative_ontology:cs_axiom_status(study_constitutes_offering, holdable).
narrative_ontology:cs_axiom_grounding('334e269e-e0a8-43fb-9e1c-7714e52982cb', study_constitutes_offering, theological).
narrative_ontology:cs_axiom('334e269e-e0a8-43fb-9e1c-7714e52982cb', secondary, continuous_engagement_sustains_order).
narrative_ontology:cs_axiom_status(continuous_engagement_sustains_order, holdable).
narrative_ontology:cs_axiom_grounding('334e269e-e0a8-43fb-9e1c-7714e52982cb', continuous_engagement_sustains_order, theological).
narrative_ontology:cs_reference_frame('334e269e-e0a8-43fb-9e1c-7714e52982cb', study_occupied_kernel).
narrative_ontology:cs_drift_state('334e269e-e0a8-43fb-9e1c-7714e52982cb', contemporary_mass_learning_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('334e269e-e0a8-43fb-9e1c-7714e52982cb', '').
narrative_ontology:cs_kernel_id(kodashim_corpus__study_as_exercise, kodashim_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_corpus__study_as_exercise, kodashim_scholars).
narrative_ontology:constraint_beneficiary(kodashim_corpus__study_as_exercise, yeshiva_students).
narrative_ontology:constraint_beneficiary(kodashim_corpus__study_as_exercise, daily_study_participants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kodashim_corpus__study_as_exercise, torah_curriculum_administrators).
narrative_ontology:constraint_vindicates(kodashim_corpus__study_as_exercise, study_equals_offering_doctrine).
narrative_ontology:constraint_vindicates(kodashim_corpus__study_as_exercise, oral_torah_transmission_continuity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advanced teachers and analysts who devote their working lives to the sacrificial tractates: lecturing, composing novellae, resolving textual difficulties across the corpus. Their livelihood, standing, and daily rhythm run through the material. Stepping away would mean leaving the profession and the community that formed them; individuals occasionally do, at real cost.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, kodashim_scholars, beneficiary,
    institutional, generational, constrained, global).

% Adolescent and young-adult learners whose formation centers on mastering these tractates under mentorship. They receive structure, purpose, and belonging from the work. Leaving usually entails relocating, retraining, and renegotiating family expectations — costly but not impossible, and some do.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, yeshiva_students, beneficiary,
    moderate, biographical, constrained, global).

% Working adults who study a fixed page-per-day cycle through the canon, sacrificial tractates included, in evening groups and online forums. Participation costs time they freely allocate; stopping carries no penalty beyond falling behind the shared calendar.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, daily_study_participants, beneficiary,
    organized, biographical, mobile, global).

% Heads of academies and organizers of the great study cycles who decide which tractates are taught, in what sequence, and to whom; they publish curricula, schedule completion celebrations, and certify mastery. Their personal authority is inseparable from the practice they administer — stepping outside it would dissolve the basis of their own standing.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, torah_curriculum_administrators, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(kodashim_corpus__study_as_exercise, torah_curriculum_administrators, beneficiary).

% Educated outsiders — secular descendants of the tradition, animal-ethics advocates, rationalist critics — who regard devoting elite intellect to an unusable slaughter manual as obscurantist or morally tone-deaf. They hold no standing inside the study conversation; their critique circulates in separate publics.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, secular_critics_of_sacrificial_study, excluded,
    moderate, biographical, mobile, global).

% Historians and scholars of religion who document how communities sustain elaborate legal corpora without enactment, and what the study-equals-performance doctrine does communally across centuries. They take no position inside the normative frame.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, academic_religion_scholars, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kodashim_corpus__study_as_exercise, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains continuous communal occupation of the sacrificial-law corpus: synchronizes interpretive practice across dispersed communities, transmits technical competence in a legal system with no current enactment, and integrates the scholarly community across generations through shared daily engagement (paired study, page-a-day cycles, standardized curricula).
% TRANSFER_FUNCTION: Moves attention, time, and interpretive labor from individual learners into the collective maintenance of the corpus, and moves honor and standing toward demonstrated mastery. Nothing material is taken from anyone: the transfer is voluntary investment of attention, repaid in meaning, competence, and communal place.
% ABSENT_VOICES: Secular rationalists and animal-ethics advocates would object that reverent study of a slaughter apparatus launders violence into liturgy; heirs of movements that abandoned the category would ask why medieval casuistry commands thousands of hours; rejectors of the Oral-Torah premise would deny the whole framework. They sit outside the beit midrash — the conversation's boundaries are drawn by prior acceptance of the kernel's authority, so dissent enters only as reportage, never as a vote.
% DISAPPEARANCE_RATIONALE: If the norm that study constitutes performance vanished overnight, the interpretive community would lose its central integrative practice: academy curricula would restructure around practical law alone, the page-a-day cycles would collapse mid-calendar, the corpus would survive only in university departments, and a principal vehicle of communal identity and intergenerational transmission would close. Every named seat rearranges.
% FOUNDING_PROBLEM: After the destruction of the Temple in 70 CE, the community possessed a large body of commandments whose performance had become impossible: how to remain faithful to commandments one cannot enact — preserving covenantal fidelity, technical knowledge, and the possibility of restoration without the cultic site.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: academic historians of post-70 Judaism document the adaptation problem the community faced; the Talmud's own record of competing post-destruction answers attests it from within the era; and the existence of the rival readings themselves (a dormant-blueprint reading and a superseded-archive reading) evidences a live problem that all parties accept while disputing the answer. Comparative cases of other traditions confronting cult discontinuity independently attest the problem's structure. No party to the kernel contest denies the founding problem — they disagree only about what to do with it.
narrative_ontology:disappearance_verdict(kodashim_corpus__study_as_exercise, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_corpus__study_as_exercise, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_corpus__study_as_exercise, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth+rescue1', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kodashim_corpus__study_as_exercise, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_corpus__study_as_exercise, 0.03, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_corpus__study_as_exercise_tests).
:- end_tests(kodashim_corpus__study_as_exercise_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.03: nothing transfers from anyone to anyone through the constraint's operation — participation is voluntary, the animals are not being sacrificed (that absence is the point), and non-participants lose nothing. The small residual reflects opportunity cost and mild communal expectation, and sits below the identity-coordination floor, i.e., within inherent coordination cost. Suppression 0.08: there is no enforcement machinery; persistence rests on attraction rather than compulsion, with a small internalized component (communal expectation that a serious person learns). Theater 0.22: completion ceremonies, calendar-driven pacing, and mass-celebration events add a ceremonial layer atop genuine engagement; the core activity remains functional under the reading's own criterion (engagement IS the performance), but the mass-participation era has grown the ceremonial share measurably — hence the rising series. Accessibility_collapse 0.20: alternatives remain fully live — prayer-centered and ethics-centered framings, and the two sibling readings compete hermeneutically rather than being suppressed; the reading persuades, it does not foreclose. Resistance 0.15: external criticism exists but mounts no campaign inside the practice, where acceptance is broad. One shared time grid spans both tracked metrics at all eight points; suppression_requirement is intentionally NOT serialized because the enforcement picture is static (there never was much enforcement to build up or decay) — the scalar carries it. Sub-interval oscillation exists (multi-year study cycles produce completion surges and restart dips in engagement intensity) but is faster than this centennial grid resolves; the grid tracks the long-run trend.
 *
 * PERSPECTIVAL GAP:
 *   Seat divergence is unusually narrow because the structure is symmetric — nearly every seated agent sits near the beneficiary end, so per-seat computed types cluster. The residual spread runs along the excluded/observer boundary: the excluded critic seat experiences the arrangement as wasteful devotion to an unusable system; the administrator seat experiences it as constitutive duty (and is the most identity-fused — institutional identity fusion: the practice made the office, so exit would dissolve the office-holder's standing, which is why this seat is the reading's most committed maintainer); the mass participant experiences it as manageable discipline with a clear exit. The engine computes these differences from power and exit atoms; the muted spread is itself diagnostic of a low-conflict coordination arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map directly onto real relationships: scholars, students, and mass participants are all subsidized by the practice — it supplies meaning, identity, competence, and community at a cost they freely bear — so each derives a directionality near the full-beneficiary end. The administrators are dual-positioned (they set the agenda and collect meaning and standing from running it) but collect nothing extracted from anyone, so their d likewise sits near the beneficiary end. With no victim group declared, no seat approaches the full-target end; effective extraction is damped everywhere, and the global scope's verification-amplification has almost nothing to amplify. Gain_flow is authored as diffuse as an affirmative checked claim: re-reading every named seat, none receives the constraint's extraction, because the operation transfers attention inward to the corpus rather than outward into any pocket; the prestige concentrating on elite scholars and the revenue accruing to publishers of standard editions are market and reputational byproducts of voluntary demand, not receipts of the constraint's transfer.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — how to remain faithful to commandments one cannot enact — is live, so the mandatrophy question reduces to whether the ANSWER has outlived its function; it has not: the study-as-performance answer still solves the still-live problem, and mandatrophy_resolved is therefore not declared. The classification guards against two mislabels. First, the naive outside view — 'elaborate perfection of skill at something that cannot be done' — reads as atrophy or theatrical maintenance; the structural data contradicts this: under this reading the skill IS the point, function is intact, and theater is low-to-moderate. Second, the extraction view finds no purchase: there is no victim set to extract from, no suppressed alternative, no enforcement ratchet. The genuine obsolescence risk is conditional and tracked by omega: if the Temple were rebuilt and the dormant-blueprint reading won, this reading would face a real sunset question — whether study yields to performance — and the constraint would need re-authoring as transitional or persistent accordingly.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_status,
    'Is the study-equals-offering equivalence the kernel''s own native logic, or a post-destruction accommodation retrofitted onto an unperformable corpus?',
    'Philological genealogy of the Menachot 110a dictum and its precursors: whether pre-destruction sources already treat study of sacrificial law as partial performance, and how the doctrine''s reception history weights native versus adaptive readings.',
    'If accommodative, this reading''s authority weakens toward the substitution_archive framing (study as consolation rather than fulfillment); if native, the reading''s claim to occupy rather than replace the kernel is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_status, conceptual, 'Whether the equivalence doctrine is native to the kernel or an adaptive response to loss.').

omega_variable(
    sibling_disagreement_location,
    'Across the three readings of the kodashim_corpus kernel, where exactly does the disagreement sit — in the mitzvah''s essence (physical act versus engaged intention), in the kernel''s present status (dormant, occupied, superseded), or in the restoration timeline?',
    'Structured comparison of the three sibling files'' axioms, reference frames, and drift states; identify which pairs of core premises can be held within one framework and which cannot.',
    'Locating the disagreement in essence-versus-archive terms determines which sibling pairs genuinely coexist in single frameworks and which approach foreclosure; it also fixes which omega resolutions would shift adherents between readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_disagreement_location, conceptual, 'Committer-frame mapping of where the kernel contest is structurally located.').

omega_variable(
    mass_theater_drift,
    'Is the rising theater_ratio of the mass-participation era proxy substitution (completion ceremonies replacing engagement — Goodhart drift) or ceremony additive to genuine study?',
    'Longitudinal comprehension and depth-of-engagement surveys of study-cycle completers across successive cycles, benchmarked against elite-track learners.',
    'A sustained rise past 0.5 would mark piton drift in the mass layer and warrant decomposing this story into an elite-engagement constraint and a mass-ceremonial constraint with separate epsilon values; a plateau supports additive ceremony.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mass_theater_drift, empirical, 'Whether mass-cycle ceremonial growth substitutes for or supplements real engagement.').

omega_variable(
    credential_gatekeeping_residual,
    'Does mastery-certification in the sacrificial tractates convert the practice into status gatekeeping — a sorting mechanism yielding extraction residue above the coordination floor to certified elites?',
    'Track exit outcomes of learners who disengage: measure whether they face sanction and standing loss (gatekeeping signature) or exit smoothly with relationships intact; compare certification-linked income and authority premiums against equivalent non-Kodashim credentials.',
    'A found gatekeeping premium would localize a tangled_rope reclassification at the certification margin (coordination for learners, asymmetric standing rents for certifiers); its absence confirms the floor-level reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(credential_gatekeeping_residual, empirical, 'Whether certification layers extractive sorting onto the coordination practice.').

omega_variable(
    restoration_sunset_contingency,
    'Does this reading carry an implicit sunset clause — does study-as-performance yield to physical performance upon messianic restoration, or does the reading treat study as permanently constitutive?',
    'Survey the reading''s own authorities on the post-restoration status of sacrificial-law study; analyze whether the tradition''s internal disputes (e.g., which offerings persist in the future) imply continuation or lapse of the study obligation.',
    'An implicit sunset would give the constraint scaffold-flavored lifecycle dynamics despite the rope classification; a settled permanence view confirms rope steady-state and closes the obsolescence question raised in the mandatrophy analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(restoration_sunset_contingency, conceptual, 'Whether the reading is transitional by its own lights or permanent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_corpus__study_as_exercise, 500, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t500, kodashim_corpus__study_as_exercise, theater_ratio, 500, 0.05).
narrative_ontology:measurement_basis(koda_tr_t500, observed).
narrative_ontology:measurement(koda_tr_t800, kodashim_corpus__study_as_exercise, theater_ratio, 800, 0.07).
narrative_ontology:measurement_basis(koda_tr_t800, observed).
narrative_ontology:measurement(koda_tr_t1200, kodashim_corpus__study_as_exercise, theater_ratio, 1200, 0.09).
narrative_ontology:measurement_basis(koda_tr_t1200, observed).
narrative_ontology:measurement(koda_tr_t1550, kodashim_corpus__study_as_exercise, theater_ratio, 1550, 0.11).
narrative_ontology:measurement_basis(koda_tr_t1550, observed).
narrative_ontology:measurement(koda_tr_t1800, kodashim_corpus__study_as_exercise, theater_ratio, 1800, 0.13).
narrative_ontology:measurement_basis(koda_tr_t1800, observed).
narrative_ontology:measurement(koda_tr_t1945, kodashim_corpus__study_as_exercise, theater_ratio, 1945, 0.14).
narrative_ontology:measurement_basis(koda_tr_t1945, observed).
narrative_ontology:measurement(koda_tr_t1990, kodashim_corpus__study_as_exercise, theater_ratio, 1990, 0.18).
narrative_ontology:measurement_basis(koda_tr_t1990, observed).
narrative_ontology:measurement(koda_tr_t2025, kodashim_corpus__study_as_exercise, theater_ratio, 2025, 0.22).
narrative_ontology:measurement_basis(koda_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(koda_be_t500, kodashim_corpus__study_as_exercise, base_extractiveness, 500, 0.04).
narrative_ontology:measurement_basis(koda_be_t500, observed).
narrative_ontology:measurement(koda_be_t800, kodashim_corpus__study_as_exercise, base_extractiveness, 800, 0.04).
narrative_ontology:measurement_basis(koda_be_t800, observed).
narrative_ontology:measurement(koda_be_t1200, kodashim_corpus__study_as_exercise, base_extractiveness, 1200, 0.05).
narrative_ontology:measurement_basis(koda_be_t1200, observed).
narrative_ontology:measurement(koda_be_t1550, kodashim_corpus__study_as_exercise, base_extractiveness, 1550, 0.06).
narrative_ontology:measurement_basis(koda_be_t1550, observed).
narrative_ontology:measurement(koda_be_t1800, kodashim_corpus__study_as_exercise, base_extractiveness, 1800, 0.05).
narrative_ontology:measurement_basis(koda_be_t1800, observed).
narrative_ontology:measurement(koda_be_t1945, kodashim_corpus__study_as_exercise, base_extractiveness, 1945, 0.04).
narrative_ontology:measurement_basis(koda_be_t1945, observed).
narrative_ontology:measurement(koda_be_t1990, kodashim_corpus__study_as_exercise, base_extractiveness, 1990, 0.03).
narrative_ontology:measurement_basis(koda_be_t1990, observed).
narrative_ontology:measurement(koda_be_t2025, kodashim_corpus__study_as_exercise, base_extractiveness, 2025, 0.03).
narrative_ontology:measurement_basis(koda_be_t2025, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(kodashim_corpus__study_as_exercise, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_corpus__study_as_exercise, identity_coordination).
narrative_ontology:affects_constraint(kodashim_corpus__study_as_exercise, kodashim_corpus__performance_only).
narrative_ontology:affects_constraint(kodashim_corpus__study_as_exercise, kodashim_corpus__substitution_archive).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'Kodashim study' conflates three structurally distinct commitments that share one kernel. performance_only (kernel dormant, blueprint awaiting restoration), study_as_exercise (this file: kernel occupied through study, epsilon ~0.03, no victim set, rope), and substitution_archive (kernel superseded, archive memorial, study-as-replacement rather than study-as-performance). Each file carries its own epsilon, beneficiary structure, and classification; the upstream Talmudic locus (Menachot 110a) is cited as evidence by all three, so this reading structurally influences its siblings' legitimacy conditions without foreclosing either — adoption of study-as-complete-performance tensions the dormant-husk claim but does not logically eliminate it, since the two claims concern different objects (mitzvah-performance versus the physical apparatus). All three files link one another through affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
