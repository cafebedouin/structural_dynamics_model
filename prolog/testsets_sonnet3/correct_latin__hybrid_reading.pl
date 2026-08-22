% ============================================================================
% CONSTRAINT STORY: correct_latin__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_correct_latin__hybrid_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: correct_latin__hybrid_reading
 *   human_readable: Correct Latin as Medieval-Transmitted Form, Correctable by Textual Evidence (Hybrid Reading)
 *   domain: historical_linguistics/philology/intellectual_history
 *
 * SUMMARY:
 *   Renaissance and early humanist philologists, faced with critiques that
 *   institutional medieval Latin had degraded from its classical models,
 *   developed a corrective apparatus: keep the inherited grammatical
 *   infrastructure that let Latin function as a living administrative and
 *   liturgical language, but systematically correct surface features against
 *   newly recovered or better-collated classical manuscripts. The apparatus
 *   positions itself as moderate and evidence-based — neither preserving
 *   'corrupt' medieval forms wholesale nor demanding total classical
 *   reconstruction — but it concentrates the authority to decide which forms
 *   are 'errors' in the hands of a scholarly class, and it imposes retraining
 *   costs on those trained under the prior medieval-continuity norm without
 *   offering them a role in setting the new corrected standard.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin__hybrid_reading, 0.42).
domain_priors:suppression_score(correct_latin__hybrid_reading, 0.38).
domain_priors:theater_ratio(correct_latin__hybrid_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin__hybrid_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(correct_latin__hybrid_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(correct_latin__hybrid_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin__hybrid_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(correct_latin__hybrid_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(correct_latin__hybrid_reading, "Correct Latin as Medieval-Transmitted Form, Correctable by Textual Evidence (Hybrid Reading)").
narrative_ontology:topic_domain(correct_latin__hybrid_reading, "historical_linguistics/philology/intellectual_history").

domain_priors:requires_active_enforcement(correct_latin__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin__hybrid_reading, '83edee85-3eec-4edc-bde5-aaa7c246357f').
narrative_ontology:cs_kernel_codification('83edee85-3eec-4edc-bde5-aaa7c246357f', distributed).
narrative_ontology:cs_authority_grounding('83edee85-3eec-4edc-bde5-aaa7c246357f', expertise).
narrative_ontology:cs_interpretation_layer_present('83edee85-3eec-4edc-bde5-aaa7c246357f').
narrative_ontology:cs_reading_relation('83edee85-3eec-4edc-bde5-aaa7c246357f', correct_latin__continuity_reading, influences).
narrative_ontology:cs_reading_relation('83edee85-3eec-4edc-bde5-aaa7c246357f', correct_latin__discontinuity_reading, influences).
narrative_ontology:cs_axiom('83edee85-3eec-4edc-bde5-aaa7c246357f', foundational, medieval_core_partially_legitimate).
narrative_ontology:cs_axiom_status(medieval_core_partially_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('83edee85-3eec-4edc-bde5-aaa7c246357f', medieval_core_partially_legitimate, conventional).
narrative_ontology:cs_axiom('83edee85-3eec-4edc-bde5-aaa7c246357f', foundational, textual_evidence_corrects_but_does_not_reoccupy).
narrative_ontology:cs_axiom_status(textual_evidence_corrects_but_does_not_reoccupy, holdable).
narrative_ontology:cs_axiom_grounding('83edee85-3eec-4edc-bde5-aaa7c246357f', textual_evidence_corrects_but_does_not_reoccupy, instrumental).
narrative_ontology:cs_reference_frame('83edee85-3eec-4edc-bde5-aaa7c246357f', humanist_corrective_standard).
narrative_ontology:cs_drift_state('83edee85-3eec-4edc-bde5-aaa7c246357f', post_manuscript_collation_expansion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('83edee85-3eec-4edc-bde5-aaa7c246357f', '').
narrative_ontology:cs_kernel_id(correct_latin__hybrid_reading, correct_latin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin__hybrid_reading, philological_reform_scholars).
narrative_ontology:constraint_beneficiary(correct_latin__hybrid_reading, humanist_pedagogues).
narrative_ontology:constraint_beneficiary(correct_latin__hybrid_reading, church_latin_institutions).
narrative_ontology:constraint_victim(correct_latin__hybrid_reading, medieval_trained_clerics).
narrative_ontology:constraint_victim(correct_latin__hybrid_reading, vernacular_educated_students).
narrative_ontology:constraint_victim(correct_latin__hybrid_reading, provincial_latin_teachers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Compare medieval manuscript readings against recovered classical exemplars and issue corrections to orthography, vocabulary, and select constructions while leaving the inherited grammatical core intact. Their authority to adjudicate which medieval forms stand and which are corrected is itself the constraint; they occupy the position of arbiter and are the primary beneficiaries of that role's prestige and institutional placement.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, philological_reform_scholars, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(correct_latin__hybrid_reading, philological_reform_scholars, beneficiary).

% Teach the corrected hybrid standard in schools and universities, building careers and curricula around the reformed textbook Latin. They benefit from the reform's prestige value and from positioning themselves as custodians of a purified but still-accessible tradition, distinct from both stubborn traditionalists and radical classicizers.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, humanist_pedagogues, beneficiary,
    organized, generational, mobile, continental).

% Depend on Latin continuity for liturgical and administrative function across centuries; adopt the hybrid standard because it preserves usable continuity with existing practice while absorbing enough classical correction to answer humanist criticism of 'barbarous' church Latin. They administer the standard's application in their own institutions.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, church_latin_institutions, beneficiary,
    institutional, civilizational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(correct_latin__hybrid_reading, church_latin_institutions, agenda_setter).

% Learned Latin through inherited medieval pedagogy and use it competently for its actual functions (liturgy, notarial work, correspondence). Under the hybrid standard, forms they were trained in are selectively flagged as errors requiring correction, devaluing their existing competence without offering them a clear path to full requalification short of retraining under the new textual authority.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, medieval_trained_clerics, payer,
    moderate, biographical, constrained, regional).

% Enter Latin instruction fresh and must absorb a moving target: grammatical core drawn from medieval-continuity teaching materials, but vocabulary and orthography subject to ongoing textual correction from newly recovered manuscripts. They bear the pedagogical cost of a standard defined by scholarly consensus they have no access to or say in.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, vernacular_educated_students, payer,
    powerless, biographical, trapped, regional).

% Teach Latin outside the major humanist centers, at a distance from the manuscript-comparison work driving corrections. They must periodically relearn and re-teach 'corrected' forms handed down from metropolitan scholarship, absorbing retraining costs each time the correction frontier moves, without participating in setting it.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, provincial_latin_teachers, payer,
    moderate, biographical, constrained, regional).

% The surviving classical manuscript corpus that grounds the correction process. It does not act; it is cited as the evidentiary basis by which reform scholars justify particular corrections against medieval-transmitted forms.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, manuscript_textual_evidence, observer,
    analytical, civilizational, analytical, continental).
narrative_ontology:stakeholder_non_agent(correct_latin__hybrid_reading, manuscript_textual_evidence).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single teachable Latin standard that preserves the accumulated grammatical infrastructure of a millennium of continuous practice (texts, pedagogy, institutional use) while incrementally aligning surface features (spelling, word choice, some syntax) with recovered classical exemplars, avoiding both the instability of unconstrained medieval variation and the pedagogical shock of wholesale classical reconstruction.
% TRANSFER_FUNCTION: Moves interpretive and corrective authority from those competent in inherited medieval practice to those credentialed in textual/manuscript scholarship; moves retraining costs onto teachers and students distant from the centers where corrections are adjudicated, while concentrating prestige and gatekeeping position with the scholars who determine which medieval forms stand and which are corrected.
% ABSENT_VOICES: Medieval-trained clerics and provincial teachers whose competence is partially devalued have no institutional standing in the textual-critical process that reclassifies their forms as errors; their objection would be that their Latin worked for its purposes and the correction imposes cost without functional necessity, but they are not seated at the philological tribunal that issues the corrections.
% DISAPPEARANCE_RATIONALE: If the hybrid correction apparatus disappeared, medieval-continuity practice would simply stand uncorrected in daily use (nothing changes for most working clerics), reform scholars would lose their adjudicating function and associated prestige, and humanist pedagogy would have to justify its curriculum on other grounds — the standard's disappearance rearranges who has authority over 'correct' Latin, even though most actual usage would be unaffected.
% FOUNDING_PROBLEM: Medieval Latin had drifted enough from classical models, and varied enough regionally, that humanist critics could plausibly call institutional Latin 'barbarous,' threatening the prestige and international legibility of Latin as the shared learned language; the hybrid reform was built to answer that charge without discarding the working continuity that let Latin still function as a living administrative and liturgical language.
% FOUNDING_PROBLEM_CORROBORATION: Reform scholars and humanist pedagogues attest the founding problem (barbarized Latin threatening prestige) as still partly live, citing continued orthographic and lexical variation. Independent historians of education and manuscript studies outside the reforming institutions note that much of the 'correction' targeted forms that caused no actual communicative failure, and that the correction frontier has kept moving in ways that serve scholarly gatekeeping as much as any functional problem — corroboration for a 'solved but persists' reading comes from this outside historiographical literature, not from the reforming parties themselves.
narrative_ontology:disappearance_verdict(correct_latin__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(correct_latin__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(correct_latin__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(correct_latin__hybrid_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(correct_latin__hybrid_reading_tests).
:- end_tests(correct_latin__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.42 at interval end) rather than high: the coordination function is real — a single evolving standard that most practitioners could still largely use — but there is a genuine and growing rent captured by the class empowered to adjudicate corrections, and a genuine cost imposed on those whose prior training is partially devalued each time the correction frontier advances. Suppression is moderate (0.38): there is no criminal enforcement, but institutional credentialing (who counts as teaching 'correct' Latin) increasingly requires conformity to the corrected standard, and this pressure has hardened over the measured interval as textual scholarship accumulated more corrective claims. Theater ratio is modest but rising (0.28): some correction activity increasingly serves scholarly prestige and gatekeeping rather than any functional communicative failure in the corrected forms.
 *
 * DIRECTIONALITY LOGIC:
 *   Reform scholars and the humanist pedagogues who teach their standard are the structural beneficiaries — they set the correction agenda and gain prestige and institutional position from occupying the arbiter role (low d). Church Latin institutions are dual-positioned: real beneficiaries of the continuity the hybrid standard preserves, but also administer its application, giving them partial agenda-setting standing. Medieval-trained clerics, vernacular students, and provincial teachers are the targets: their prior competence is selectively devalued, they bear retraining costs, and they have no seat in the correction process (high d). The manuscript evidence itself is a non-agent observer — it grounds the corrections cited but does not act or collect.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid reading resists mislabeling in both directions the sibling readings would produce: it does not treat the entire correction apparatus as pure extraction (which would erase the genuine coordination value of a single teachable, evolving standard, as a pure-snare reading of 'humanist tyranny over medieval Latin' would), nor does it treat correction as costless technical improvement with no distributional effect (which would erase the real retraining burden the correction imposes on those distant from the adjudicating centers, as an uncritical discontinuity reading might). Tangled rope captures this: a genuine coordination function (shared living standard) coexists with asymmetric extraction (adjudicating authority and its rents concentrate upward; retraining costs concentrate downward) requiring active enforcement (institutional credentialing pressure) to hold.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disagreement_location,
    'Where exactly does the hybrid reading''s correction line fall relative to the continuity and discontinuity readings, and is that line itself contested within humanist scholarship?',
    'Compare specific correction decisions (e.g., particular orthographic or lexical rulings) across scholars who self-identify with different degrees of medieval legitimacy — the disagreement is located in exactly which surface features get corrected versus grandfathered as legitimately medieval.',
    'If the correction line is itself unstable or driven by scholarly reputation competition rather than principled textual evidence, the hybrid reading''s claim to be a principled middle path (versus a contested compromise dressed as evidence-based moderation) weakens, pushing the classification toward snare; if the line is genuinely evidence-stable, the tangled_rope coordination function is more secure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'This constraint is one reading (hybrid_reading) of the correct_latin kernel; the sibling continuity_reading would treat the entire correction apparatus as illegitimate imposition, and the sibling discontinuity_reading would treat the retained medieval grammatical core as itself an error requiring further correction. The disagreement between all three readings is located precisely at how much weight textual evidence should carry against continuous practice — this reading''s answer (partial, corrective, not full reoccupation) is a specific structural choice, not a neutral synthesis.').

omega_variable(
    correction_frontier_endogeneity,
    'Does the set of features flagged for correction expand because textual evidence genuinely accumulates, or because the correcting institution''s authority is partly self-perpetuating (new corrections justify continued scholarly gatekeeping)?',
    'Track whether corrections issued in later periods rely on newly discovered manuscript evidence versus re-litigation of previously settled questions; a pattern of re-litigation without new evidence would indicate frontier expansion driven by institutional incentive rather than evidence accumulation.',
    'Endogenous frontier expansion would support the rising theater_ratio and suppression_requirement trajectories as symptoms of extraction rather than genuine coordination improvement, strengthening the tangled_rope classification''s extraction pole over its coordination pole.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(correction_frontier_endogeneity, empirical, 'Whether the correction apparatus''s growth reflects real evidentiary accumulation or self-perpetuating institutional authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin__hybrid_reading, 0, 400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(corr_tr_t0, correct_latin__hybrid_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(corr_tr_t80, correct_latin__hybrid_reading, theater_ratio, 80, 0.15).
narrative_ontology:measurement(corr_tr_t160, correct_latin__hybrid_reading, theater_ratio, 160, 0.2).
narrative_ontology:measurement(corr_tr_t240, correct_latin__hybrid_reading, theater_ratio, 240, 0.24).
narrative_ontology:measurement(corr_tr_t320, correct_latin__hybrid_reading, theater_ratio, 320, 0.26).
narrative_ontology:measurement(corr_tr_t400, correct_latin__hybrid_reading, theater_ratio, 400, 0.28).

% Extraction over time
narrative_ontology:measurement(corr_be_t0, correct_latin__hybrid_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(corr_be_t80, correct_latin__hybrid_reading, base_extractiveness, 80, 0.3).
narrative_ontology:measurement(corr_be_t160, correct_latin__hybrid_reading, base_extractiveness, 160, 0.36).
narrative_ontology:measurement(corr_be_t240, correct_latin__hybrid_reading, base_extractiveness, 240, 0.39).
narrative_ontology:measurement(corr_be_t320, correct_latin__hybrid_reading, base_extractiveness, 320, 0.41).
narrative_ontology:measurement(corr_be_t400, correct_latin__hybrid_reading, base_extractiveness, 400, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(corr_su_t0, correct_latin__hybrid_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(corr_su_t80, correct_latin__hybrid_reading, suppression_requirement, 80, 0.26).
narrative_ontology:measurement(corr_su_t160, correct_latin__hybrid_reading, suppression_requirement, 160, 0.31).
narrative_ontology:measurement(corr_su_t240, correct_latin__hybrid_reading, suppression_requirement, 240, 0.34).
narrative_ontology:measurement(corr_su_t320, correct_latin__hybrid_reading, suppression_requirement, 320, 0.36).
narrative_ontology:measurement(corr_su_t400, correct_latin__hybrid_reading, suppression_requirement, 400, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin__hybrid_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(correct_latin__hybrid_reading, 0.1).
narrative_ontology:affects_constraint(correct_latin__hybrid_reading, correct_latin__continuity_reading).
narrative_ontology:affects_constraint(correct_latin__hybrid_reading, correct_latin__discontinuity_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the correct_latin kernel. continuity_reading treats medieval-transmitted Latin as fully legitimate evolved Classical Latin (minimal or no correction owed to living practice); discontinuity_reading treats medieval Latin as corrupt deviation requiring reconstruction from ancient texts (correction owed everywhere the medieval form departs from classical exemplars). hybrid_reading (this file) occupies the structural middle: grammatical core accepted as continuous, surface features (orthography, vocabulary, select syntax) subject to targeted textual correction. Each reading has its own ε, its own beneficiary/victim structure, and its own claimed type — they are not measurements of one underlying constraint but three structurally distinct constraints sharing a contested kernel. The hybrid reading's correction activity creates downstream pressure on the other two: its partial-legitimacy stance narrows the space in which pure continuity claims can be asserted uncorrected, and its refusal of full reoccupation limits how far discontinuity-driven reconstruction claims can extend without contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
