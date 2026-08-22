% ============================================================================
% CONSTRAINT STORY: kodashim_corpus__study_as_exercise
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: kodashim_corpus__study_as_exercise
 *   human_readable: Kodashim Corpus as Occupied Kernel: Study of Sacrifice Law as Performance of the Mitzvah
 *   domain: religious_studies/rabbinic_judaism/commitment_system_theory
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the kodashim_corpus kernel: the
 *   claim that after the Temple's destruction, study of the sacrificial laws
 *   is itself the performance of the sacrificial mitzvot, so the corpus
 *   remains an occupied kernel sustained by continuous intellectual-spiritual
 *   engagement rather than a dormant blueprint or a memorial archive. The
 *   standing arrangement under contest — and therefore the ε referent — is
 *   the live practice of Kodashim study as avodah, assessed by this reading's
 *   own lights: participants discharge the covenant's central obligation
 *   through engagement, so the arrangement's costs are constitutive of its
 *   benefit (hence the reading's name, study-as-exercise) and no one is
 *   deprived. The constraint family decomposes the colloquial label 'status
 *   of the Kodashim corpus' into three structurally distinct stories: this
 *   reading (ε ≈ 0.06, beneficiaries, no victims), performance_only (corpus
 *   as archived blueprint awaiting restored sacrifice — obligational force
 *   retained, engagement provisional), and substitution_archive (corpus as
 *   memorial of a rite superseded by prayer — occupancy denied). Each sibling
 *   gets its own file, its own ε, its own stakeholder set; they enter this
 *   story only through network.affects_constraints,
 *   cs_structure.reading_relations, and the committer omega. Claim and
 *   metrics are authored independently: the claimed type is what I take to be
 *   structurally true (coordination around shared interpretive practice,
 *   participants as net beneficiaries, no coercive overhead), and the metrics
 *   describe the arrangement's actual operation as the record shows it.
 *
 * KEY AGENTS:
 *   - - kodashim_scholars: Primary beneficiary (organized/constrained) — performs the avodah through study; bears only the constitutive cost of engagement
 *   - - yeshiva_institutions: Agenda-setter with secondary beneficiary position (institutional/arbitrage) — administers curriculum, funding, and ordination; collects and recycles prestige
 *   - - lay_study_participants: Secondary beneficiary (moderate/mobile) — broad cyclical engagement base; free exit
 *   - - women_excluded_from_formal_study: Excluded voice (powerless/trapped) — historically gated away from the arrangement through which the mitzvah could be performed
 *   - - commitment_system_theorists: Analytical observer (analytical/analytical) — sees the full structure from outside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_corpus__study_as_exercise, 0.06).
domain_priors:suppression_score(kodashim_corpus__study_as_exercise, 0.04).
domain_priors:theater_ratio(kodashim_corpus__study_as_exercise, 0.14).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, extractiveness, 0.06).
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, suppression_requirement, 0.04).
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, theater_ratio, 0.14).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_corpus__study_as_exercise, rope).
narrative_ontology:human_readable(kodashim_corpus__study_as_exercise, "Kodashim Corpus as Occupied Kernel: Study of Sacrifice Law as Performance of the Mitzvah").
narrative_ontology:topic_domain(kodashim_corpus__study_as_exercise, "religious_studies/rabbinic_judaism/commitment_system_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_corpus__study_as_exercise, '043a5d8b-d635-4b22-9e48-c3107ff47f53').
narrative_ontology:cs_kernel_codification('043a5d8b-d635-4b22-9e48-c3107ff47f53', fixed_text).
narrative_ontology:cs_authority_grounding('043a5d8b-d635-4b22-9e48-c3107ff47f53', lineage).
narrative_ontology:cs_interpretation_layer_present('043a5d8b-d635-4b22-9e48-c3107ff47f53').
narrative_ontology:cs_reading_relation('043a5d8b-d635-4b22-9e48-c3107ff47f53', kodashim_corpus__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('043a5d8b-d635-4b22-9e48-c3107ff47f53', kodashim_corpus__substitution_archive, forecloses).
narrative_ontology:cs_axiom('043a5d8b-d635-4b22-9e48-c3107ff47f53', foundational, study_constitutes_performance).
narrative_ontology:cs_axiom_status(study_constitutes_performance, holdable).
narrative_ontology:cs_axiom_grounding('043a5d8b-d635-4b22-9e48-c3107ff47f53', study_constitutes_performance, conventional).
narrative_ontology:cs_axiom('043a5d8b-d635-4b22-9e48-c3107ff47f53', foundational, sacrificial_obligation_persists).
narrative_ontology:cs_axiom_status(sacrificial_obligation_persists, holdable).
narrative_ontology:cs_axiom_grounding('043a5d8b-d635-4b22-9e48-c3107ff47f53', sacrificial_obligation_persists, deontological).
narrative_ontology:cs_reference_frame('043a5d8b-d635-4b22-9e48-c3107ff47f53', kernel_occupied_by_study).
narrative_ontology:cs_drift_state('043a5d8b-d635-4b22-9e48-c3107ff47f53', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('043a5d8b-d635-4b22-9e48-c3107ff47f53', '').
narrative_ontology:cs_kernel_id(kodashim_corpus__study_as_exercise, kodashim_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_corpus__study_as_exercise, kodashim_scholars).
narrative_ontology:constraint_beneficiary(kodashim_corpus__study_as_exercise, lay_study_participants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kodashim_corpus__study_as_exercise, yeshiva_institutions).
narrative_ontology:constraint_vindicates(kodashim_corpus__study_as_exercise, study_equivalence_doctrine).
narrative_ontology:constraint_vindicates(kodashim_corpus__study_as_exercise, torah_study_sustains_creation).
narrative_ontology:constraint_vindicates(kodashim_corpus__study_as_exercise, post_destruction_covenantal_continuity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Devote their lives to mastering and teaching the orders of sacrificial law. In the tradition's own accounting, their engagement is not preparation for a rite but the rite itself: the daily act of study discharges the obligation of divine service. What flows to them is merit, interpretive standing, and the continuity of a practice they regard as load-bearing for the world. Leaving the practice is materially easy — nothing compels attendance — but existentially costly for someone formed by decades inside it.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, kodashim_scholars, beneficiary,
    organized, generational, constrained, global).

% Set the curricula, fund the study, ordain the teachers, and decide how much of the learning cycle is devoted to the sacrificial orders versus other subjects. They collect prestige, enrollment, and endowed support, and recycle nearly all of it back into sustaining the practice. They retain full discretion to reweight the curriculum; no external party can force them to keep Kodashim central, and none tries.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, yeshiva_institutions, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(kodashim_corpus__study_as_exercise, yeshiva_institutions, beneficiary).

% Join daily and cyclical study programs that pass through the sacrificial tractates alongside everything else. They bear only a time cost, receive the mitzvah's fulfillment and communal belonging in return, and may stop at any time without penalty. Their engagement is the broad base that keeps the corpus in living circulation beyond the professional class.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, lay_study_participants, beneficiary,
    moderate, biographical, mobile, global).

% Historically barred from the advanced textual institutions where the sacrificial orders are studied, they stood outside the arrangement through which the mitzvah of avodah could be performed. The benefit the arrangement distributes — fulfillment of the covenant's central obligation through engagement — was gated behind male-only access for most of the interval. Contemporary institutions are opening access, and the shape of the historical exclusion is now openly debated inside the community.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, women_excluded_from_formal_study, excluded,
    powerless, biographical, trapped, global).

% Study the arrangement from outside as a case of a community holding a legal corpus in continuous operational use long after the institutions it legislates for ceased to exist. They neither participate in the practice nor bear its costs; their seat is the analytical vantage from which the whole structure is visible at once.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, commitment_system_theorists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kodashim_corpus__study_as_exercise, diffuse).
narrative_ontology:fixing_cost_class(kodashim_corpus__study_as_exercise, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Keeps the covenant's central rite performable after the destruction that ended its physical form: a distributed, synchronized practice through which the obligation of avodah remains dischargeable, the corpus stays internally coherent under continuous interpretation, and the technical knowledge of the sacrificial system is transmitted intact across generations.
% TRANSFER_FUNCTION: Moves time, attention, and scholarly labor from individuals into the corpus; moves interpretive authority and communal standing to demonstrated proficiency; moves covenantal merit, in the tradition's own accounting, to those who engage. No material goods change hands — the transfer is of effort, status, and obligation-discharge.
% ABSENT_VOICES: Women excluded from formal yeshiva study would object that the arrangement gated the performance of the covenant's central mitzvah itself behind institutions closed to them — not a marginal benefit but the rite's only available form. Unlettered Jews without access to the learning tradition stood similarly outside. Both exclusions trace partly to the surrounding social order rather than to the study arrangement narrowly; the modern widening of access is the live test of how constitutive they were.
% DISAPPEARANCE_RATIONALE: If the practice of study-as-performance vanished overnight, yeshiva curricula, daily study cycles, and the community's entire account of post-Temple covenantal continuity would lose their center; the corpus would revert to inert archival text; the claim that divine service continues would lose its operating mechanism; and the institutions built around the practice would reorganize around the remaining subjects.
% FOUNDING_PROBLEM: After the destruction of the Second Temple in 70 CE, the sacrificial commandments became physically unperformable. A covenant constituted around divine service needed a way to continue without its central rite — neither to lapse the obligation nor to abandon the corpus that specifies it.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set by historians of early rabbinic Judaism, who independently document the post-destruction reconstruction of Jewish practice around text, prayer, and study; the continued physical impossibility of sacrifice is public record that no party disputes. Within the tradition, the Talmudic sources themselves (e.g., Menachot 110a) attest the problem and the proposed solution. No corroborating source claims the founding problem is resolved.
narrative_ontology:disappearance_verdict(kodashim_corpus__study_as_exercise, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_corpus__study_as_exercise, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_corpus__study_as_exercise, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kodashim_corpus__study_as_exercise, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_corpus__study_as_exercise, 0.06, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness sits at 0.06 because the arrangement's only cost — the effort of study — is the very act the reading counts as fulfillment; there is no surplus transferred to any seat and no party deprived of anything they would otherwise hold. Suppression is 0.04 and is authored as a raw structural property, unscaled by power or scope: nothing enforces the practice, and its persistence across sixteen centuries under steadily weakening communal enforcement capacity is the strongest descriptive evidence that it runs on voluntary religious motivation. Theater ratio is low (0.14) because the activity is its own point — there is no separate function being mimicked — with a gentle modern rise as completion-oriented study cycles add a thin layer of finish-the-page performance atop engagement. Accessibility_collapse is 0.50: understanding the arrangement does not eliminate alternatives, since the sibling readings and ordinary secular life remain live options; resistance is 0.25, reflecting recurring internal critique (study-without-intent objections, rejection of the oral framework outside rabbinic boundaries) but no organized opposition within holding communities. The temporal series share one grid (nine points, century-scale units from the reading's Talmudic consolidation) so every tracked metric is authored at every examined time point. The suppression_requirement series is included deliberately: it tracks enforcement-capacity decay, from quasi-autonomous communal norms in the medieval kehillah era down to near-zero in open modern societies — the arrangement shed its coercive shell and persisted, which is the signature of voluntary coordination rather than enforced extraction.
 *
 * PERSPECTIVAL GAP:
 *   Seat divergence here runs along an unusual axis: nearly every seated party is a beneficiary, so the computed divergence is between degrees of benefit rather than benefit versus extraction. From the scholar's seat the arrangement is the covenant's living center — the rite itself. From the institution's seat it is a continuity mission with discretionary curriculum control. From the lay participant's seat it is a low-cost belonging practice. From the excluded seat, however, the same arrangement appears as a gate placed in front of the mitzvah's only available performance — the one seat for which the structure withheld rather than delivered. The engine computes these per-seat classifications from the structural data; the authored rope claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   All participant seats derive directionality near the beneficiary end: the beneficiary declarations are explicit, exits are mobile to constrained (never trapped for participants), and no seat bears a transfer it did not undertake. No target seats exist — there is no victim set to amplify χ. Global spatial scope nominally raises verification difficulty, but with ε already at the coordination-cost floor there is no extractive base for scope to amplify. The excluded seat is not a directionality input (R3: authored absence is commentary-grade, never correction-grade), but it is recorded honestly on the stakeholder surface for the consensus-provenance check.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — how a covenant of divine service continues when its rite is physically impossible — remains live: the Temple is still unbuilt, and no corroborating source inside or outside the tradition claims otherwise. Nothing has outlived its function, so no mandatrophy declaration is made and none is warranted. The mismatch consumer reads founding_problem_status=live against disappearance_verdict=world_rearranges and finds alignment — no zombie flag, no capture signal. The one genuine transition question facing this reading is messianic: if sacrifice were restored, would study-as-performance sunset into preparation, continue alongside the rite, or remain the rite's intellectual core? The tradition itself holds divergent views; this is routed to the committer omega rather than forced into a sunset clause, since no sunset is declared within the reading as it stands.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading (study_as_exercise) of the kodashim_corpus kernel; what structurally changes if a sibling reading is adopted instead, and where exactly is the disagreement located?',
    'Classify the sibling files independently and compare: the disagreement is located in the kernel''s present operative status — occupied through study (this reading), dormant pending restoration (performance_only), or superseded and archived (substitution_archive).',
    'Adopting performance_only preserves obligational force but recasts the arrangement as provisional, with a conditional sunset at restoration (scaffold-flavored transition). Adopting substitution_archive removes occupancy entirely, leaving archival maintenance of a dead function (piton-flavored) with a different beneficiary set and no discharge mechanism. This file''s ε, beneficiaries, and rope classification are valid only under the study_as_exercise reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: which kernel, which reading, what siblings would change, where the disagreement sits.').

omega_variable(
    equivalence_legal_or_metaphysical,
    'Is the study-performance equivalence a legal enactment (the obligation discharged by rabbinic decree that study counts as offering) or a metaphysical identity (study literally constitutes the avodah before God)?',
    'Conceptual analysis of the Talmudic loci (Menachot 110a; Taanit 27b–28a) and their treatment in the later codes: an enactment reading leaves a residual distinction between the discharge and the ideal rite; an identity reading collapses it.',
    'Under the enactment reading the practice is a substitute with an unmet remainder outstanding, slightly raising the effective gap between fulfillment and obligation; under the identity reading fulfillment is complete and ε''s referent closes fully. The classification stays rope under either, but the beneficiary claim''s strength differs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(equivalence_legal_or_metaphysical, conceptual, 'Whether the equivalence doctrine is juridical substitution or ontological identity.').

omega_variable(
    cosmic_maintenance_claim_status,
    'Does scholarly engagement sustain cosmic order (the claim that the world stands on Torah study) constitutively, or is that homiletic framing layered onto a practice whose function is covenantal continuity?',
    'Internal-theological adjudication within the tradition''s own commitments; externally undecidable in principle, so the omega tracks which framing the practicing community itself treats as load-bearing.',
    'If constitutive, the scholars'' beneficiary claim is stronger than civic continuity — they are load-bearing for creation, and the arrangement''s stakes are maximal. If homiletic, the claimed stakes shrink to cultural and legal continuity without altering the structural classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cosmic_maintenance_claim_status, conceptual, 'Status of the cosmic-order claim within the reading''s own lights.').

omega_variable(
    exclusion_constitutive_or_contextual,
    'Was the historical exclusion of women from formal Kodashim study constitutive of the study-as-performance arrangement, or an artifact of the surrounding social order that the arrangement merely inherited?',
    'Compare access regimes across communities and eras as they diverge, and observe whether the arrangement''s function, ε, and beneficiary structure shift as access widens in the contemporary period.',
    'If contextual, the beneficiary set simply widens and the historical record records a contingent injustice adjacent to, not inside, the arrangement. If constitutive, the historical arrangement carried a suppressed victim set that the reading''s own lights (which count only participants) structurally obscured — raising the historical ε above the authored value for the pre-modern portion of the interval.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exclusion_constitutive_or_contextual, empirical, 'Whether the gendered access regime was part of the arrangement or its environment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_corpus__study_as_exercise, 0, 1600).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_corpus__study_as_exercise, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(koda_tr_t0, observed).
narrative_ontology:measurement(koda_tr_t200, kodashim_corpus__study_as_exercise, theater_ratio, 200, 0.1).
narrative_ontology:measurement_basis(koda_tr_t200, observed).
narrative_ontology:measurement(koda_tr_t400, kodashim_corpus__study_as_exercise, theater_ratio, 400, 0.11).
narrative_ontology:measurement_basis(koda_tr_t400, observed).
narrative_ontology:measurement(koda_tr_t600, kodashim_corpus__study_as_exercise, theater_ratio, 600, 0.11).
narrative_ontology:measurement_basis(koda_tr_t600, observed).
narrative_ontology:measurement(koda_tr_t800, kodashim_corpus__study_as_exercise, theater_ratio, 800, 0.12).
narrative_ontology:measurement_basis(koda_tr_t800, observed).
narrative_ontology:measurement(koda_tr_t1000, kodashim_corpus__study_as_exercise, theater_ratio, 1000, 0.12).
narrative_ontology:measurement_basis(koda_tr_t1000, observed).
narrative_ontology:measurement(koda_tr_t1200, kodashim_corpus__study_as_exercise, theater_ratio, 1200, 0.13).
narrative_ontology:measurement_basis(koda_tr_t1200, observed).
narrative_ontology:measurement(koda_tr_t1400, kodashim_corpus__study_as_exercise, theater_ratio, 1400, 0.13).
narrative_ontology:measurement_basis(koda_tr_t1400, observed).
narrative_ontology:measurement(koda_tr_t1600, kodashim_corpus__study_as_exercise, theater_ratio, 1600, 0.14).
narrative_ontology:measurement_basis(koda_tr_t1600, observed).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_corpus__study_as_exercise, base_extractiveness, 0, 0.05).
narrative_ontology:measurement_basis(koda_be_t0, observed).
narrative_ontology:measurement(koda_be_t200, kodashim_corpus__study_as_exercise, base_extractiveness, 200, 0.05).
narrative_ontology:measurement_basis(koda_be_t200, observed).
narrative_ontology:measurement(koda_be_t400, kodashim_corpus__study_as_exercise, base_extractiveness, 400, 0.05).
narrative_ontology:measurement_basis(koda_be_t400, observed).
narrative_ontology:measurement(koda_be_t600, kodashim_corpus__study_as_exercise, base_extractiveness, 600, 0.05).
narrative_ontology:measurement_basis(koda_be_t600, observed).
narrative_ontology:measurement(koda_be_t800, kodashim_corpus__study_as_exercise, base_extractiveness, 800, 0.05).
narrative_ontology:measurement_basis(koda_be_t800, observed).
narrative_ontology:measurement(koda_be_t1000, kodashim_corpus__study_as_exercise, base_extractiveness, 1000, 0.05).
narrative_ontology:measurement_basis(koda_be_t1000, observed).
narrative_ontology:measurement(koda_be_t1200, kodashim_corpus__study_as_exercise, base_extractiveness, 1200, 0.06).
narrative_ontology:measurement_basis(koda_be_t1200, observed).
narrative_ontology:measurement(koda_be_t1400, kodashim_corpus__study_as_exercise, base_extractiveness, 1400, 0.06).
narrative_ontology:measurement_basis(koda_be_t1400, observed).
narrative_ontology:measurement(koda_be_t1600, kodashim_corpus__study_as_exercise, base_extractiveness, 1600, 0.06).
narrative_ontology:measurement_basis(koda_be_t1600, observed).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t0, kodashim_corpus__study_as_exercise, suppression_requirement, 0, 0.18).
narrative_ontology:measurement_basis(koda_su_t0, observed).
narrative_ontology:measurement(koda_su_t200, kodashim_corpus__study_as_exercise, suppression_requirement, 200, 0.16).
narrative_ontology:measurement_basis(koda_su_t200, observed).
narrative_ontology:measurement(koda_su_t400, kodashim_corpus__study_as_exercise, suppression_requirement, 400, 0.15).
narrative_ontology:measurement_basis(koda_su_t400, observed).
narrative_ontology:measurement(koda_su_t600, kodashim_corpus__study_as_exercise, suppression_requirement, 600, 0.14).
narrative_ontology:measurement_basis(koda_su_t600, observed).
narrative_ontology:measurement(koda_su_t800, kodashim_corpus__study_as_exercise, suppression_requirement, 800, 0.12).
narrative_ontology:measurement_basis(koda_su_t800, observed).
narrative_ontology:measurement(koda_su_t1000, kodashim_corpus__study_as_exercise, suppression_requirement, 1000, 0.1).
narrative_ontology:measurement_basis(koda_su_t1000, observed).
narrative_ontology:measurement(koda_su_t1200, kodashim_corpus__study_as_exercise, suppression_requirement, 1200, 0.08).
narrative_ontology:measurement_basis(koda_su_t1200, observed).
narrative_ontology:measurement(koda_su_t1400, kodashim_corpus__study_as_exercise, suppression_requirement, 1400, 0.06).
narrative_ontology:measurement_basis(koda_su_t1400, observed).
narrative_ontology:measurement(koda_su_t1600, kodashim_corpus__study_as_exercise, suppression_requirement, 1600, 0.04).
narrative_ontology:measurement_basis(koda_su_t1600, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_corpus__study_as_exercise, identity_coordination).
narrative_ontology:affects_constraint(kodashim_corpus__study_as_exercise, kodashim_corpus__performance_only).
narrative_ontology:affects_constraint(kodashim_corpus__study_as_exercise, kodashim_corpus__substitution_archive).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the Kodashim corpus after the destruction' covers three structurally distinct claims that must not share one story. Study_as_exercise (this file): occupied kernel, ε ≈ 0.06, beneficiaries only, live function. Performance_only: dormant blueprint, obligational force retained but engagement provisional pending restoration — different time_horizon profile and a conditional sunset at restoration. Substitution_archive: superseded rite documented, ε near zero but function dead — archival-maintenance economics with no occupancy claim. The upstream reading (this one) carries the highest empirical continuity and historically supplies the legitimacy conditions under which the other two are articulated; each member links to the others via affects_constraints per the family rule.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
