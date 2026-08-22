% ============================================================================
% CONSTRAINT STORY: kodashim_corpus__study_as_exercise
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: kodashim_corpus__study_as_exercise
 *   human_readable: Study of Kodashim as Living Fulfillment of the Sacrificial Mitzvah
 *   domain: religious_studies/rabbinic_judaism/commitment_system_theory
 *
 * SUMMARY:
 *   This story authors the study_as_exercise reading of the kodashim_corpus
 *   kernel: the rabbinic doctrine (rooted in sources like Menachot 110a) that
 *   sustained, rigorous study of the laws of sacrifice (Kodashim) is not
 *   preparation for or memory of a suspended practice but is itself the full
 *   and present occupation of the mitzvah of sacrifice. On this reading, the
 *   destruction of the Temple did not leave the kernel vacant or archived —
 *   continuous intellectual-spiritual engagement with the sacrificial corpus
 *   IS the performance. This is authored as a distinct, ε-invariant
 *   constraint from its siblings: performance_only (kernel as dormant husk
 *   awaiting messianic restoration) and substitution_archive (Kodashim as
 *   memorial record of a superseded practice, with prayer/study as
 *   replacement rather than occupation). Each reading has a different
 *   beneficiary structure, a different extraction profile, and a different
 *   verdict on what would happen if the practice vanished — hence three
 *   separate files linked through the kernel, not one story averaged across
 *   readings.
 *
 * KEY AGENTS:
 *   - yeshiva_scholars: Primary practitioners (moderate/mobile) — occupy the kernel through study
 *   - kollel_communities: Organized beneficiaries (organized/mobile) — sustain the practice communally
 *   - rabbinic_academies: Institutional beneficiaries (institutional/mobile) — transmit and legitimize the doctrine across generations
 *   - broader_observant_community: Analytical/participating observer — recognizes the practice's legitimacy without being compelled
 *   - sibling_reading_advocates: Excluded — hold competing readings of the same kernel, not represented in this story
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_corpus__study_as_exercise, 0.03).
domain_priors:suppression_score(kodashim_corpus__study_as_exercise, 0.05).
domain_priors:theater_ratio(kodashim_corpus__study_as_exercise, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, extractiveness, 0.03).
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_corpus__study_as_exercise, rope).
narrative_ontology:human_readable(kodashim_corpus__study_as_exercise, "Study of Kodashim as Living Fulfillment of the Sacrificial Mitzvah").
narrative_ontology:topic_domain(kodashim_corpus__study_as_exercise, "religious_studies/rabbinic_judaism/commitment_system_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_corpus__study_as_exercise, '6f83f143-ef65-4a96-ac76-768a0730e37c').
narrative_ontology:cs_kernel_codification('6f83f143-ef65-4a96-ac76-768a0730e37c', fixed_text).
narrative_ontology:cs_authority_grounding('6f83f143-ef65-4a96-ac76-768a0730e37c', lineage).
narrative_ontology:cs_interpretation_layer_present('6f83f143-ef65-4a96-ac76-768a0730e37c').
narrative_ontology:cs_reading_relation('6f83f143-ef65-4a96-ac76-768a0730e37c', kodashim_corpus__performance_only, forecloses).
narrative_ontology:cs_reading_relation('6f83f143-ef65-4a96-ac76-768a0730e37c', kodashim_corpus__substitution_archive, forecloses).
narrative_ontology:cs_axiom('6f83f143-ef65-4a96-ac76-768a0730e37c', foundational, study_constitutes_full_present_occupation).
narrative_ontology:cs_axiom_status(study_constitutes_full_present_occupation, holdable).
narrative_ontology:cs_axiom_grounding('6f83f143-ef65-4a96-ac76-768a0730e37c', study_constitutes_full_present_occupation, theological).
narrative_ontology:cs_axiom('6f83f143-ef65-4a96-ac76-768a0730e37c', secondary, intellectual_engagement_generates_rather_than_defers_fulfillment).
narrative_ontology:cs_axiom_status(intellectual_engagement_generates_rather_than_defers_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('6f83f143-ef65-4a96-ac76-768a0730e37c', intellectual_engagement_generates_rather_than_defers_fulfillment, theological).
narrative_ontology:cs_reference_frame('6f83f143-ef65-4a96-ac76-768a0730e37c', temple_era_sacrificial_praxis).
narrative_ontology:cs_drift_state('6f83f143-ef65-4a96-ac76-768a0730e37c', post_destruction_rabbinic_reconstitution, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('6f83f143-ef65-4a96-ac76-768a0730e37c', '').
narrative_ontology:cs_kernel_id(kodashim_corpus__study_as_exercise, kodashim_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_corpus__study_as_exercise, yeshiva_scholars).
narrative_ontology:constraint_beneficiary(kodashim_corpus__study_as_exercise, kollel_communities).
narrative_ontology:constraint_beneficiary(kodashim_corpus__study_as_exercise, rabbinic_academies).
narrative_ontology:constraint_vindicates(kodashim_corpus__study_as_exercise, torah_study_equals_sacrifice_doctrine).
narrative_ontology:constraint_vindicates(kodashim_corpus__study_as_exercise, continuous_engagement_maintains_cosmic_order).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Engage daily in structured analysis of Zevachim, Menachot, and related tractates. This engagement is understood, on this reading, not as preparation for a future act but as the present and complete discharge of the sacrificial mitzvah. They set the interpretive agenda through generations of commentary and are free to enter or leave this study tradition without external coercion; their standing within the community follows from participation but nothing traps them structurally.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, yeshiva_scholars, beneficiary,
    moderate, civilizational, mobile, global).
narrative_ontology:stakeholder_secondary_role(kodashim_corpus__study_as_exercise, yeshiva_scholars, agenda_setter).

% Organize collective, sustained learning schedules around Kodashim material. They receive communal cohesion, continuity of tradition, and the stated spiritual benefit of the study-as-performance framework. Participation is voluntary and reversible; members who leave lose communal position but are not coerced into staying.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, kollel_communities, beneficiary,
    organized, generational, mobile, regional).

% Institutionalize the curriculum that treats study of sacrificial law as complete fulfillment, training successive generations of scholars and preserving the interpretive chain. They gain legitimacy and continuity from the practice but do not extract resources from any excluded or coerced party to sustain it.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, rabbinic_academies, beneficiary,
    institutional, civilizational, mobile, global).

% Are not required to engage in Kodashim study to the same depth but recognize the practice as a legitimate, non-exclusive form of piety. They may participate, support financially through voluntary communal structures, or simply hold the belief that this study confers benefit without being compelled to.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, broader_observant_community, observer,
    moderate, generational, mobile, regional).

% Hold the performance_only or substitution_archive readings of the same kernel and would contest that study alone constitutes occupation of the kernel. They are not silenced or coerced, but this constraint story does not represent their position — it is authored from within the study_as_exercise reading only, per the ε-invariance decomposition rule.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, sibling_reading_advocates, excluded,
    moderate, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a shared interpretive practice: scholars across generations and locations converge on a common body of sacrificial law (Kodashim) as the object of continuous study, sustaining a coherent tradition of legal-spiritual reasoning without requiring the Temple's physical existence.
% TRANSFER_FUNCTION: Moves attention, scholarly effort, and communal prestige toward those who engage deeply with Kodashim; moves essentially nothing coercively from any excluded party — the mitzvah's fulfillment is generated by the act of study itself, not extracted from a third party.
% ABSENT_VOICES: Advocates of the performance_only reading (who hold the kernel dormant, awaiting messianic restoration) and advocates of the substitution_archive reading (who hold Kodashim as superseded memorial text) are not represented within this reading's structure — they hold the sibling constraints, not this one.
% DISAPPEARANCE_RATIONALE: Within this reading, if organized Kodashim study vanished, something real would be lost: the ongoing occupation of the sacrificial kernel through intellectual-spiritual engagement would cease, and (on this reading's own premises) the cosmic-maintenance function performed by that engagement would lapse. Sibling readings would say nothing changes because, on their view, the kernel was already dormant or the practice already superseded — hence 'contested' rather than a clean verdict, since the answer depends on which reading of the kernel one holds.
% FOUNDING_PROBLEM: After the Temple's destruction, physical sacrifice became impossible; the tradition needed an account of how the sacrificial mitzvot could continue to be fulfilled, not merely commemorated or deferred.
% FOUNDING_PROBLEM_CORROBORATION: Talmudic sources (e.g., Menachot 110a, associating Torah study of sacrificial portions with the sacrifices themselves) are cited by the tradition's own scholars, which is an internal attestation. Outside corroboration is thinner: comparative religion scholars studying rabbinic responses to cultic disruption note the functional role such doctrines play in sustaining communal identity post-destruction, but they do not attest to the theological claim itself — they attest only that the doctrine functions as claimed to function, not that the kernel is metaphysically occupied. No fully external corroboration of the theological claim exists; this is stated plainly rather than papered over.
narrative_ontology:disappearance_verdict(kodashim_corpus__study_as_exercise, contested).
narrative_ontology:founding_problem_status(kodashim_corpus__study_as_exercise, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_corpus__study_as_exercise, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(kodashim_corpus__study_as_exercise, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_corpus__study_as_exercise, 0.03, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored near-zero (0.02-0.03) because on this reading no party is deprived of anything to sustain the practice: scholars give time and attention voluntarily and receive the claimed spiritual/cosmic benefit directly from the act itself, not from a transfer extracted from someone else. Suppression is low (0.05) because no coercive apparatus compels participation — one enters or exits the study tradition freely, at the cost only of communal standing, not of external force. Theater ratio is kept low (0.08 across the interval) because, unlike institutional dynamics tracked elsewhere in the corpus, there is no drift toward performative substitution here: the study itself is claimed as substantively constitutive of the mitzvah, not a decorative proxy for something else. Accessibility collapse is authored moderate-low (0.15): once inside the tradition, the study_as_exercise framing becomes fairly totalizing for its adherents (little room within the tradition to hold that study is NOT occupation), but the tradition itself does not suppress the existence of sibling readings held by other communities — accessibility collapse is internal to the reading's own adherents, not imposed externally.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (scholars, kollel communities, academies) sit near the pure-beneficiary end of directionality: the constraint, as authored, subsidizes their spiritual, social, and institutional standing without imposing an offsetting cost on any declared victim group. There is no victim set in this reading — that absence is itself the structural claim being tested: the study_as_exercise reading asserts that occupation of the kernel is intrinsically non-extractive because the fulfillment is generated (not transferred) by the act of engagement. The broader observant community sits closer to symmetric/beneficiary: they benefit from communal cohesion and the availability of a legitimate substitute-practice without bearing the study burden themselves.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists mandatrophy mislabeling in a specific way: because the founding problem (how sacrificial mitzvot continue after the Temple's destruction) is authored as 'live' rather than 'dead,' the practice cannot be read as a mandate that has outlived its function while persisting on inertia — on the reading's own terms, the function (occupation of the kernel) is continuously and presently discharged by the study itself, not deferred to a future restoration. This is precisely what distinguishes it from the performance_only sibling, where the mandate IS suspended (awaiting messianic restoration) and from substitution_archive, where the original mandate is treated as superseded rather than fulfilled. The corroboration field notes plainly that outside verification of the underlying theological claim does not exist — only internal doctrinal attestation and external functional description are available — which keeps the founding-problem status honestly contested rather than settled by the benefiting parties' own say-so.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    study_as_ontologically_sufficient,
    'Does the rabbinic tradition treat study of sacrificial law as ontologically equivalent to the sacrificial act itself, or merely as a licensed substitute practice with its own independent value?',
    'Close textual analysis of the grounding sources (Menachot 110a and related Talmudic and later halachic discussions) to determine whether the language asserts identity (''is as if he offered'') or mere functional equivalence/permission. Comparative analysis across halachic authorities (Rambam, Ramban, later poskim) on whether they treat the kernel as occupied or merely bridged.',
    'If the sources assert strict ontological identity, this reading''s zero-extraction, full-occupation structure is well-grounded. If the sources assert only a licensed substitute or partial equivalence, this reading collapses partly toward the substitution_archive sibling, and the claimed type/metrics here would need revision.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(study_as_ontologically_sufficient, conceptual, 'Whether study-as-performance is textually asserted as identity with sacrifice or merely as a sanctioned substitute.').

omega_variable(
    cosmic_order_maintenance_claim,
    'Is the claim that continuous Kodashim study ''maintains cosmic order'' a verifiable structural claim within the tradition''s own framework, or an unfalsifiable theological assertion that cannot be adjudicated by any evidence?',
    'No empirical resolution mechanism exists for the metaphysical claim itself; the closest available check is examining whether the tradition''s own authorities treat lapses in study as having observable consequences (communal crisis narratives, theodicy literature) versus treating the claim as purely devotional framing without causal content.',
    'If treated as purely devotional/non-causal, the beneficiary structure here is best understood as psychological and communal (meaning-making, cohesion) rather than literally cosmological, which would not change the ε or type classification but would reframe the mechanism by which scholars are ''beneficiaries.''',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cosmic_order_maintenance_claim, preference, 'Whether the cosmic-maintenance claim is a causal claim or a devotional framing within the tradition.').

omega_variable(
    kernel_reading_selection_basis,
    'On what basis should an analytical observer favor the study_as_exercise reading over performance_only or substitution_archive when characterizing ''the'' status of Kodashim study in contemporary practice?',
    'Survey of which reading is operative in which communities (e.g., predominant framing in Lithuanian-style yeshiva culture versus liturgical communities that recite sacrificial passages as memorial) and whether institutional self-description aligns with one reading over others.',
    'This does not change this story''s own internal ε or classification (each reading is authored as its own clean constraint per the decomposition rule) but affects which reading is most representative when analyzing a specific real-world community''s practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_basis, conceptual, 'How to select among sibling readings when characterizing a specific community''s actual practice, without collapsing the readings into one averaged constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_corpus__study_as_exercise, 0, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_corpus__study_as_exercise, theater_ratio, 0, 0.05).
narrative_ontology:measurement(koda_tr_t400, kodashim_corpus__study_as_exercise, theater_ratio, 400, 0.06).
narrative_ontology:measurement(koda_tr_t800, kodashim_corpus__study_as_exercise, theater_ratio, 800, 0.07).
narrative_ontology:measurement(koda_tr_t1200, kodashim_corpus__study_as_exercise, theater_ratio, 1200, 0.07).
narrative_ontology:measurement(koda_tr_t1600, kodashim_corpus__study_as_exercise, theater_ratio, 1600, 0.08).
narrative_ontology:measurement(koda_tr_t1950, kodashim_corpus__study_as_exercise, theater_ratio, 1950, 0.08).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_corpus__study_as_exercise, base_extractiveness, 0, 0.02).
narrative_ontology:measurement(koda_be_t400, kodashim_corpus__study_as_exercise, base_extractiveness, 400, 0.02).
narrative_ontology:measurement(koda_be_t800, kodashim_corpus__study_as_exercise, base_extractiveness, 800, 0.03).
narrative_ontology:measurement(koda_be_t1200, kodashim_corpus__study_as_exercise, base_extractiveness, 1200, 0.03).
narrative_ontology:measurement(koda_be_t1600, kodashim_corpus__study_as_exercise, base_extractiveness, 1600, 0.03).
narrative_ontology:measurement(koda_be_t1950, kodashim_corpus__study_as_exercise, base_extractiveness, 1950, 0.03).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(kodashim_corpus__study_as_exercise, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(kodashim_corpus__study_as_exercise, kodashim_corpus__performance_only).
narrative_ontology:affects_constraint(kodashim_corpus__study_as_exercise, kodashim_corpus__substitution_archive).

% DUAL FORMULATION NOTE:
% This story is one of three linked constraints decomposing the natural-language concept 'the status of Kodashim study after the Temple's destruction' per the ε-invariance principle. performance_only treats the kernel as dormant (extraction and beneficiary structure differ: the kernel awaits future restoration, so present study functions more like archival preservation than fulfillment). substitution_archive treats Kodashim as a memorial record of a superseded practice with prayer/study as replacement rather than occupation (also low extraction but a different founding-problem status — the founding problem is treated as resolved-by-substitution rather than live-and-continuously-discharged). study_as_exercise (this file) asserts the strongest claim: study itself IS full and present occupation of the kernel, yielding the lowest extraction and a 'live' founding-problem status. All three share the same underlying halachic-textual kernel but diverge in what they claim the kernel's current status to be, producing different ε, beneficiary structures, and disappearance verdicts. Linked via affects_constraints rather than merged into one averaged story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
