% ============================================================================
% CONSTRAINT STORY: correct_latin__discontinuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_correct_latin__discontinuity_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: correct_latin__discontinuity_reading
 *   human_readable: Discontinuity Reading of Correct Latin: Ancient Texts as Sole Standard, Medieval Usage Disqualified
 *   domain: historical linguistics/philology/intellectual history
 *
 * SUMMARY:
 *   Since the Renaissance, classical scholarship has operated a normative
 *   regime defining correct Latin as the form fixed in ancient texts and
 *   grading the intervening millennium as corruption to be repaired by
 *   reconstruction. The regime solved a real problem — a literary language
 *   without native speakers needs a criterion — while converting that
 *   criterion into exclusive professional authority: whoever owns the
 *   reconstruction owns correctness. This story instantiates only the
 *   discontinuity reading of the kernel correct_latin; its epsilon describes
 *   THIS regime as the standing arrangement since humanist consolidation, not
 *   the sibling regimes, and the sibling arrangements appear nowhere in the
 *   metrics. Claim and metrics are authored independently: the regime
 *   presents itself as mere fidelity to antiquity, while the metrics record
 *   enforced exclusion wrapped around a functioning core. Family links run to
 *   the continuity and hybrid readings, which carry the same coordination
 *   function with different victim structures.
 *
 * KEY AGENTS:
 *   - classical_philologists: primary beneficiary-administrator (organized/identity_locked) — reconstruct the standard and police usage; careers and authority ride on exclusive adjudication
 *   - humanist_educators: founding agenda-setter (institutional/constrained) — built the rupture doctrine into curricula; collected patronage and displaced scholastic rivals
 *   - critical_edition_publishers: commercial beneficiary (organized/mobile) — sell the apparatus the standard makes necessary; exit is a catalog decision
 *   - medieval_scribes_and_clerks: historical target (powerless/trapped) — transmission work graded as corruption across a temporal gap they cannot answer
 *   - medievalist_historians: modern target (moderate/constrained) — defend the legitimacy of their sources' language against the ancient benchmark
 *   - latin_language_students: burden-bearing entrants (powerless/constrained) — examined against the reconstructed form; corrected for attested medieval usage
 *   - defenders_of_ecclesiastical_latin: excluded voice (powerful/trapped) — custodians of the continuous tradition the doctrine condemns wholesale
 *   - classical_examining_bodies: administrative enforcer (institutional/constrained) — certify conformity; collect order, not the doctrine's gains
 *   - historiographers_of_language: analytical observer (analytical/analytical) — document both traditions and the contingency of the classical anchor
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin__discontinuity_reading, 0.62).
domain_priors:suppression_score(correct_latin__discontinuity_reading, 0.45).
domain_priors:theater_ratio(correct_latin__discontinuity_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin__discontinuity_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(correct_latin__discontinuity_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(correct_latin__discontinuity_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin__discontinuity_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(correct_latin__discontinuity_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin__discontinuity_reading, tangled_rope).
narrative_ontology:human_readable(correct_latin__discontinuity_reading, "Discontinuity Reading of Correct Latin: Ancient Texts as Sole Standard, Medieval Usage Disqualified").
narrative_ontology:topic_domain(correct_latin__discontinuity_reading, "historical linguistics/philology/intellectual history").

domain_priors:requires_active_enforcement(correct_latin__discontinuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin__discontinuity_reading, '5f1aafa6-af99-4e1f-a9bd-5e75760b4ede').
narrative_ontology:cs_kernel_codification('5f1aafa6-af99-4e1f-a9bd-5e75760b4ede', fixed_text).
narrative_ontology:cs_authority_grounding('5f1aafa6-af99-4e1f-a9bd-5e75760b4ede', lineage).
narrative_ontology:cs_interpretation_layer_present('5f1aafa6-af99-4e1f-a9bd-5e75760b4ede').
narrative_ontology:cs_reading_relation('5f1aafa6-af99-4e1f-a9bd-5e75760b4ede', correct_latin__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('5f1aafa6-af99-4e1f-a9bd-5e75760b4ede', correct_latin__hybrid_reading, forecloses).
narrative_ontology:cs_axiom('5f1aafa6-af99-4e1f-a9bd-5e75760b4ede', foundational, antiquity_terminates_legitimate_transmission).
narrative_ontology:cs_axiom_status(antiquity_terminates_legitimate_transmission, holdable).
narrative_ontology:cs_axiom_grounding('5f1aafa6-af99-4e1f-a9bd-5e75760b4ede', antiquity_terminates_legitimate_transmission, conventional).
narrative_ontology:cs_axiom('5f1aafa6-af99-4e1f-a9bd-5e75760b4ede', foundational, medieval_forms_are_corruption_not_evolution).
narrative_ontology:cs_axiom_status(medieval_forms_are_corruption_not_evolution, holdable).
narrative_ontology:cs_axiom_grounding('5f1aafa6-af99-4e1f-a9bd-5e75760b4ede', medieval_forms_are_corruption_not_evolution, empirically_contingent).
narrative_ontology:cs_axiom('5f1aafa6-af99-4e1f-a9bd-5e75760b4ede', secondary, reconstruction_from_texts_alone_restores_correctness).
narrative_ontology:cs_axiom_status(reconstruction_from_texts_alone_restores_correctness, holdable).
narrative_ontology:cs_axiom_grounding('5f1aafa6-af99-4e1f-a9bd-5e75760b4ede', reconstruction_from_texts_alone_restores_correctness, instrumental).
narrative_ontology:cs_reference_frame('5f1aafa6-af99-4e1f-a9bd-5e75760b4ede', textual_classical_exemplar).
narrative_ontology:cs_drift_state('5f1aafa6-af99-4e1f-a9bd-5e75760b4ede', post_medieval_latin_rehabilitation, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('5f1aafa6-af99-4e1f-a9bd-5e75760b4ede', '').
narrative_ontology:cs_kernel_id(correct_latin__discontinuity_reading, correct_latin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin__discontinuity_reading, classical_philologists).
narrative_ontology:constraint_beneficiary(correct_latin__discontinuity_reading, humanist_educators).
narrative_ontology:constraint_beneficiary(correct_latin__discontinuity_reading, critical_edition_publishers).
narrative_ontology:constraint_victim(correct_latin__discontinuity_reading, medieval_scribes_and_clerks).
narrative_ontology:constraint_victim(correct_latin__discontinuity_reading, medievalist_historians).
narrative_ontology:constraint_victim(correct_latin__discontinuity_reading, latin_language_students).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(correct_latin__discontinuity_reading, latin_language_students).
narrative_ontology:constraint_vindicates(correct_latin__discontinuity_reading, classical_golden_age_doctrine).
narrative_ontology:constraint_vindicates(correct_latin__discontinuity_reading, textual_primacy_of_ancient_witnesses).
narrative_ontology:constraint_vindicates(correct_latin__discontinuity_reading, medieval_latinity_as_decline_narrative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Edit, emend, and annotate the ancient corpus; referee journals; train doctoral students in reconstruction technique. Prestige, chairs, and publication markets flow to whoever can adjudicate classical usage authoritatively. Departure would mean renouncing the premise their training, portfolios, and institutions are built on, so exit is rare even when the standard itself is questioned.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, classical_philologists, beneficiary,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(correct_latin__discontinuity_reading, classical_philologists, agenda_setter).

% Renaissance and Counter-Reformation schoolmasters who wrote the grammars, composed the imitation exercises, and set curricula making ancient authors the sole models. They won patronage by displacing scholastic rivals and embedded the doctrine so deeply in institutional routine that successors inherited it as settled method.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, humanist_educators, agenda_setter,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(correct_latin__discontinuity_reading, humanist_educators, beneficiary).

% Sell critical editions, commentaries, dictionaries, and school texts; every cohort of students and scholars needs the apparatus anew. Catalogs can be redirected to other subjects, so the commercial tie is opportunistic rather than constitutive.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, critical_edition_publishers, beneficiary,
    organized, biographical, mobile, global).

% Copied, adapted, and composed the Latin that carried law, liturgy, and learning for a thousand years. Once the rupture doctrine took hold, their skilled work was regraded as barbarism by readers they never met; no channel exists for contesting a verdict delivered across eight centuries.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, medieval_scribes_and_clerks, payer,
    powerless, generational, trapped, continental).

% Study charters, chronicles, and correspondence written in post-classical Latin. Their sources' language is routinely graded defective against the ancient benchmark, forcing a standing defense of legitimacy that colleagues working on classical texts never face; retraining into another specialty would forfeit decades of paleographic skill.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, medievalist_historians, payer,
    moderate, biographical, constrained, global).

% Learn grammar and composition aimed exclusively at the ancient models; examiners mark medieval constructions as errors even where abundantly attested in the tradition. They receive a coherent target and access to the ancient corpus in exchange, but bear the whole correction burden and absorb the lesson that the intervening millennium was failure.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, latin_language_students, payer,
    powerless, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(correct_latin__discontinuity_reading, latin_language_students, beneficiary).

% Guard a liturgical and documentary Latin descended continuously from late antiquity. The rupture doctrine condemns their usage wholesale, yet the fora where usage norms are set — philological congresses, curriculum boards, classical journals — hold no seat for them; they answer in sermons, encyclicals, and liturgiological scholarship the standard-setters rarely read.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, defenders_of_ecclesiastical_latin, excluded,
    powerful, generational, trapped, global).

% Set syllabi, commission set texts, and mark compositions and unseen translation against the reconstructed norm; certification is where the standard touches millions of lives. They collect orderly comparability of qualifications rather than fees tied to the doctrine; revising the norm would require renegotiation across universities, ministries, and publishers simultaneously.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, classical_examining_bodies, agenda_setter,
    institutional, generational, constrained, national).

% Trace Latin's arc from archaic inscriptions to digital corpora. From this distance the periodic redefinitions of correctness — Augustan canonization, Carolingian reform, humanist rupture, twentieth-century rehabilitation — appear as successive settlements among rival constituencies, and the current settlement's dependence on one of them becomes visible.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, historiographers_of_language, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(correct_latin__discontinuity_reading, classical_philologists).
narrative_ontology:fixing_cost_class(correct_latin__discontinuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single textually anchored target form for a language no longer acquired natively, so that editing ancient works, certifying student attainment, citing across centuries, and composing new Latin all key to one shared reference instead of drifting apart.
% TRANSFER_FUNCTION: Moves linguistic authority from the living transmission chain, whose products are reclassified as error, to the corps trained to reconstruct the ancient form; moves students' study time into mastering that reconstruction; moves publishing revenue, chairs, and journal space toward classical over medieval specializations.
% ABSENT_VOICES: Defenders of ecclesiastical and medieval Latin would object that the doctrine voids a millennium of competent practice, but hold no seat on curriculum boards or philological congresses; medieval scribes cannot answer verdicts delivered posthumously; students subject to the marking scheme sit on no examining body.
% DISAPPEARANCE_RATIONALE: Overnight removal would force immediate reorganization: examining bodies would need a new marking norm, editors a new criterion for emending versus retaining transmitted readings, teachers a new target form, and the classical profession its justification for exclusive adjudication. Medieval and ecclesiastical usage would regain standing as legitimate development rather than defect, and the boundary between error and variant, currently drawn at antiquity's edge, would have to be redrawn from somewhere else.
% FOUNDING_PROBLEM: After Latin ceased to be anyone's mother tongue, writers and teachers had no native intuition to distinguish defensible usage from accumulated scribal variation; the humanists needed a criterion sharp enough to restore eloquence, and fixed it to the recovered texts of antiquity.
% FOUNDING_PROBLEM_CORROBORATION: Historians of classical scholarship, writing outside the benefiting parties, corroborate that the founding problem was real: transmission variance was massive and a criterion was genuinely needed once native regulation ended. What no one outside the benefiting parties attests is that the problem required declaring the intervening millennium corrupt — medievalist philologists and liturgical authorities explicitly testify that continuity-based and hybrid solutions meet the same need without the exclusion. The necessity of the rupture is asserted only from within the beneficiary set.
narrative_ontology:disappearance_verdict(correct_latin__discontinuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(correct_latin__discontinuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin__discontinuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(correct_latin__discontinuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(correct_latin__discontinuity_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(correct_latin__discontinuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(correct_latin__discontinuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(correct_latin__discontinuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.62: the transfer is durable and asymmetric — authority and resources move from the transmitted tradition to the reconstruction profession — but bounded by a real service, since editing and teaching do need the anchor. Suppression 0.45 is authored as a raw structural property and deliberately left unscaled: silent editorial emendation, examination marking, and curricular exclusion coerce without armies; any scaling by directionality and scope is the engine's arithmetic, not the story's. Theater 0.38: a growing share of activity is ritualized purism — display composition and stylistic gatekeeping — that would outlive the function's erosion. Accessibility_collapse 0.55: alternatives persisted continuously (monastic scriptoria, liturgy, medievalist philology) yet stayed marginal wherever credentials are issued. Resistance 0.60: anti-Ciceronian currents, liturgical defense, and the twentieth-century rehabilitation of medieval Latinity met the doctrine head-on. All three series share one seven-point grid (1450–2000) with endpoints matching the scalars. The suppression series is authored because enforcement capacity itself is the traced dynamic — built up through early-modern schooling, peaking with nineteenth-century professionalization, decaying since mid-century — which is exactly the enforcement-history case the scalar rule reserves for series.
 *
 * PERSPECTIVAL GAP:
 *   From the philologist seat the structure reads as disciplined scholarship: the standard simply is what rigor looks like, and enforcement is quality control. From the student and medievalist seats it reads as a tribunal where their inheritance is presumed guilty until re-formed in an extinct idiom. The publisher seat experiences neither — a revenue line with a cheap exit — while the examining-body seat experiences administration rather than gain. The divergence tracks exit optionality and identity fusion more than formal power: the least powerful seats are not the most locked, and the best-positioned beneficiary walks away freely.
 *
 * DIRECTIONALITY LOGIC:
 *   The declarations drive the derivation: classical_philologists, humanist_educators, and critical_edition_publishers sit near the subsidized end; medieval_scribes_and_clerks, medievalist_historians, and latin_language_students near the target end, with students damped slightly by their incidental benefit. Two overrides correct relationships the structural data cannot reach. Institutional (classical_examining_bodies) is pinned at d=0.40 because a power-atom fallback would guess blind: these bodies collect procedural order, not the doctrine's gains, placing them modestly on the beneficiary side of symmetric. Powerful (defenders_of_ecclesiastical_latin) is pinned at d=0.70 because, as an excluded seat, they transact nothing inside the regime yet the doctrine's rhetoric condemns their tradition wholesale — loading them toward the target end despite formal absence from it.
 *
 * MANDATROPHY ANALYSIS:
 *   Calling the regime a snare would erase the standardization function even its critics rely on: editing, examining, and cross-era citation need one fixed reference for a non-native literary language, and that need survives rejection of the doctrine. Calling it a rope would erase what pays for the apparatus: disqualifying a millennium of transmitted practice is what manufactures the scarcity of authorized adjudicators and thus the gains. The tangled-rope claim keeps both halves visible. On obsolescence: the generic problem — a dead literary language needs a criterion — remains live wherever Latin is taught or edited, so the mandate is not simply dead; what is contested is whether this particular solution, total rupture, is still the right one, which is why the founding-problem status is contested rather than resolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'Which account of transmitted Latin''s authority does the operative standard rest on — this discontinuity reading, or one of its siblings?',
    'Institutional analysis of whose testimony counts when usage norms are set: if only textual reconstruction counts, this reading governs; if living-practice transmission carries weight, a sibling governs.',
    'Sibling adoption dissolves this regime''s victim structure: the continuity reading legitimates medieval practice outright, removing the corruption verdict and the reconstruction monopoly; the hybrid reading narrows exclusion to resisted reforms — changing both epsilon and type.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: this constraint is one reading of kernel correct_latin; siblings would restructure the beneficiary/victim sets entirely.').

omega_variable(
    corruption_or_regulated_register,
    'Is medieval Latin actually ruleless deviation, or a systematically regulated learned register with internal norms of its own?',
    'Systematic grammatical comparison across large medieval corpora: medieval Latin philology has already documented consistent syntactic and lexical norms and a diglossia structure rather than chaos.',
    'If regulated, the corruption premise fails on its own empirical ground; the exclusion stands exposed as pure status allocation, feeding the axiom_overriding drift declared in cs_structure and pressuring reclassification of the whole regime.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(corruption_or_regulated_register, empirical, 'Whether the factual predicate of the rupture doctrine (deviation versus development) holds.').

omega_variable(
    classical_anchor_contingency,
    'Does the standard''s authority depend on the classical period being naturally normative, or on contingent canonization of Ciceronian taste by Renaissance preference?',
    'Comparative analysis of rival anchors (Silver Age, patristic, pan-medieval benchmarks) and whether the regime''s operations survive substituting the anchor while keeping the textual method.',
    'If the anchor is contingent, the regime''s quasi-natural framing of classical perfection fails and its persistence rests wholly on enforcement — blocking any natural-law-adjacent reading and concentrating classification weight on the extraction side.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(classical_anchor_contingency, conceptual, 'Contingency of the golden-age anchor beneath the doctrine of textual primacy.').

omega_variable(
    enforcement_decay_or_retrenchment,
    'Does the falling suppression trajectory after 1950 reflect genuine decay of the regime''s coercive capacity, or retrenchment into fortified redoubts such as elite curricula and high-stakes examinations?',
    'Track enforcement density per Latin-using institution, examination-board rule changes, and journal rejection patterns from 1950 to the present.',
    'Decay predicts drift toward inertial, theatrically maintained survival; retrenchment predicts a smaller but harder regime — opposite lifecycle forecasts hang on this resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_decay_or_retrenchment, empirical, 'Whether the late-interval relaxation is attrition or concentration of enforcement.').

omega_variable(
    student_burden_allocation,
    'Is the burden placed on learners the unavoidable price of reaching the ancient corpus, or an imposed barrier that filters access while yielding no compensating competence?',
    'Compare attainment and attrition under rupture-framed versus continuity-framed pedagogy, and survey what failed students actually lose access to.',
    'If barrier-shaped, harms extend diffusely beyond any capturing seat, raising the effective burden on the powerless seats beyond what professional gain-flow explains.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(student_burden_allocation, conceptual, 'Coordination-cost versus access-barrier character of the learner''s share.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin__discontinuity_reading, 1450, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(corr_tr_t1450, correct_latin__discontinuity_reading, theater_ratio, 1450, 0.12).
narrative_ontology:measurement_basis(corr_tr_t1450, observed).
narrative_ontology:measurement(corr_tr_t1550, correct_latin__discontinuity_reading, theater_ratio, 1550, 0.18).
narrative_ontology:measurement_basis(corr_tr_t1550, observed).
narrative_ontology:measurement(corr_tr_t1650, correct_latin__discontinuity_reading, theater_ratio, 1650, 0.24).
narrative_ontology:measurement_basis(corr_tr_t1650, observed).
narrative_ontology:measurement(corr_tr_t1750, correct_latin__discontinuity_reading, theater_ratio, 1750, 0.28).
narrative_ontology:measurement_basis(corr_tr_t1750, observed).
narrative_ontology:measurement(corr_tr_t1850, correct_latin__discontinuity_reading, theater_ratio, 1850, 0.34).
narrative_ontology:measurement_basis(corr_tr_t1850, observed).
narrative_ontology:measurement(corr_tr_t1950, correct_latin__discontinuity_reading, theater_ratio, 1950, 0.37).
narrative_ontology:measurement_basis(corr_tr_t1950, observed).
narrative_ontology:measurement(corr_tr_t2000, correct_latin__discontinuity_reading, theater_ratio, 2000, 0.38).
narrative_ontology:measurement_basis(corr_tr_t2000, observed).

% Extraction over time
narrative_ontology:measurement(corr_be_t1450, correct_latin__discontinuity_reading, base_extractiveness, 1450, 0.48).
narrative_ontology:measurement_basis(corr_be_t1450, observed).
narrative_ontology:measurement(corr_be_t1550, correct_latin__discontinuity_reading, base_extractiveness, 1550, 0.56).
narrative_ontology:measurement_basis(corr_be_t1550, observed).
narrative_ontology:measurement(corr_be_t1650, correct_latin__discontinuity_reading, base_extractiveness, 1650, 0.6).
narrative_ontology:measurement_basis(corr_be_t1650, observed).
narrative_ontology:measurement(corr_be_t1750, correct_latin__discontinuity_reading, base_extractiveness, 1750, 0.64).
narrative_ontology:measurement_basis(corr_be_t1750, observed).
narrative_ontology:measurement(corr_be_t1850, correct_latin__discontinuity_reading, base_extractiveness, 1850, 0.7).
narrative_ontology:measurement_basis(corr_be_t1850, observed).
narrative_ontology:measurement(corr_be_t1950, correct_latin__discontinuity_reading, base_extractiveness, 1950, 0.66).
narrative_ontology:measurement_basis(corr_be_t1950, observed).
narrative_ontology:measurement(corr_be_t2000, correct_latin__discontinuity_reading, base_extractiveness, 2000, 0.62).
narrative_ontology:measurement_basis(corr_be_t2000, observed).

% Suppression requirement over time
narrative_ontology:measurement(corr_su_t1450, correct_latin__discontinuity_reading, suppression_requirement, 1450, 0.4).
narrative_ontology:measurement_basis(corr_su_t1450, observed).
narrative_ontology:measurement(corr_su_t1550, correct_latin__discontinuity_reading, suppression_requirement, 1550, 0.54).
narrative_ontology:measurement_basis(corr_su_t1550, observed).
narrative_ontology:measurement(corr_su_t1650, correct_latin__discontinuity_reading, suppression_requirement, 1650, 0.6).
narrative_ontology:measurement_basis(corr_su_t1650, observed).
narrative_ontology:measurement(corr_su_t1750, correct_latin__discontinuity_reading, suppression_requirement, 1750, 0.62).
narrative_ontology:measurement_basis(corr_su_t1750, observed).
narrative_ontology:measurement(corr_su_t1850, correct_latin__discontinuity_reading, suppression_requirement, 1850, 0.64).
narrative_ontology:measurement_basis(corr_su_t1850, observed).
narrative_ontology:measurement(corr_su_t1950, correct_latin__discontinuity_reading, suppression_requirement, 1950, 0.52).
narrative_ontology:measurement_basis(corr_su_t1950, observed).
narrative_ontology:measurement(corr_su_t2000, correct_latin__discontinuity_reading, suppression_requirement, 2000, 0.45).
narrative_ontology:measurement_basis(corr_su_t2000, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin__discontinuity_reading, information_standard).
narrative_ontology:affects_constraint(correct_latin__discontinuity_reading, correct_latin__continuity_reading).
narrative_ontology:affects_constraint(correct_latin__discontinuity_reading, correct_latin__hybrid_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'correct Latin' decomposes, per epsilon-invariance, into three structurally distinct normative regimes: this discontinuity reading (validity terminates with antiquity; maximal exclusion of transmitted practice), the continuity reading (practice transmits validity; no victim set), and the hybrid reading (partial continuity with targeted reform; intermediate victim set). Each is authored as its own story with its own epsilon, beneficiaries, and victims; they share one coordination function (a fixed reference for a non-native literary language) while distributing its costs differently. This story links outward to both siblings; reciprocal links belong in their files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(correct_latin__discontinuity_reading, institutional, 0.4).
constraint_indexing:directionality_override(correct_latin__discontinuity_reading, powerful, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
