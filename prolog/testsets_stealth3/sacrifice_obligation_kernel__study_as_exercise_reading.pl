% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_kernel__study_as_exercise_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_kernel__study_as_exercise_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: sacrifice_obligation_kernel__study_as_exercise_reading
 *   human_readable: Study-as-Exercise Reading of the Sacrificial Obligation
 *   domain: religious/halakhic/commitment-system
 *
 * SUMMARY:
 *   Rabbinic Judaism grounds a durable interpretive settlement in the
 *   Talmudic dictum (Menahot 110a, attributed to R. Yochanan) that whoever
 *   engages in the laws of burnt offerings is as if he had brought one: under
 *   current conditions — no standing Temple — intellectual engagement with
 *   the sacrificial corpus constitutes the genuine exercise of the
 *   sacrificial commandment, not merely preparation for a future performance.
 *   This story instantiates that settlement as ONE reading of the
 *   sacrifice_obligation_kernel. Rule-1 discipline holds throughout: the
 *   sibling readings (performance_only, messianic_suspension,
 *   symbolic_archive) are separate constraints in separate files, linked by
 *   network edges and routed through omegas, never folded into this story's
 *   classification. The ε referent is the standing study-as-exercise
 *   arrangement itself, assessed by this reading's own lights — never the
 *   arrangement a sibling reading would install. Claim and metrics are
 *   authored independently: the claimed type states the structure as this
 *   reading understands it; the metrics describe observed operation; any
 *   computed divergence is the signal, not an error to reconcile.
 *
 * KEY AGENTS:
 *   - - rabbinic_authority: Steward-interpreter (institutional/identity_locked) — defines and transmits what counts as occupying the commandment; principal collector of interpretive standing
 *   - - studying_communities: Fulfillment recipients (organized/constrained) — occupy the commandment through study; pay only attention and time
 *   - - yeshiva_institutions: Program administrators (organized/constrained) — schedule, certify, and fund the study the definition designates as exercising; genuinely dual-positioned
 *   - - karaite_communities: Excluded dissenters (moderate/trapped) — deny the interpretive chain's authority to redefine the commandment; outside the conversation
 *   - - historically_excluded_women_students: Excluded claimants (powerless/trapped) — historically barred from the designated fulfilling activity
 *   - - halakhic_historians: Analytical observers (analytical/analytical) — document the transformation without standing inside the halakhic frame
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_kernel__study_as_exercise_reading, 0.04).
domain_priors:suppression_score(sacrifice_obligation_kernel__study_as_exercise_reading, 0.1).
domain_priors:theater_ratio(sacrifice_obligation_kernel__study_as_exercise_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, extractiveness, 0.04).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_kernel__study_as_exercise_reading, rope).
narrative_ontology:human_readable(sacrifice_obligation_kernel__study_as_exercise_reading, "Study-as-Exercise Reading of the Sacrificial Obligation").
narrative_ontology:topic_domain(sacrifice_obligation_kernel__study_as_exercise_reading, "religious/halakhic/commitment-system").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_kernel__study_as_exercise_reading, '2aa7c4c0-a7a4-491c-87f1-3710d10159d5').
narrative_ontology:cs_kernel_codification('2aa7c4c0-a7a4-491c-87f1-3710d10159d5', fixed_text).
narrative_ontology:cs_authority_grounding('2aa7c4c0-a7a4-491c-87f1-3710d10159d5', lineage).
narrative_ontology:cs_interpretation_layer_present('2aa7c4c0-a7a4-491c-87f1-3710d10159d5').
narrative_ontology:cs_reading_relation('2aa7c4c0-a7a4-491c-87f1-3710d10159d5', sacrifice_obligation_kernel__performance_only_reading, coexists_with).
narrative_ontology:cs_reading_relation('2aa7c4c0-a7a4-491c-87f1-3710d10159d5', sacrifice_obligation_kernel__messianic_suspension_reading, coexists_with).
narrative_ontology:cs_reading_relation('2aa7c4c0-a7a4-491c-87f1-3710d10159d5', sacrifice_obligation_kernel__symbolic_archive_reading, coexists_with).
narrative_ontology:cs_axiom('2aa7c4c0-a7a4-491c-87f1-3710d10159d5', foundational, study_constitutes_genuine_offering_equivalent).
narrative_ontology:cs_axiom_status(study_constitutes_genuine_offering_equivalent, holdable).
narrative_ontology:cs_axiom_grounding('2aa7c4c0-a7a4-491c-87f1-3710d10159d5', study_constitutes_genuine_offering_equivalent, theological).
narrative_ontology:cs_axiom('2aa7c4c0-a7a4-491c-87f1-3710d10159d5', secondary, obligation_demands_current_occupation_not_readiness).
narrative_ontology:cs_axiom_status(obligation_demands_current_occupation_not_readiness, holdable).
narrative_ontology:cs_axiom_grounding('2aa7c4c0-a7a4-491c-87f1-3710d10159d5', obligation_demands_current_occupation_not_readiness, deontological).
narrative_ontology:cs_reference_frame('2aa7c4c0-a7a4-491c-87f1-3710d10159d5', study_occupied_obligation_baseline).
narrative_ontology:cs_drift_state('2aa7c4c0-a7a4-491c-87f1-3710d10159d5', contemporary_diaspora_practice, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('2aa7c4c0-a7a4-491c-87f1-3710d10159d5', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_kernel__study_as_exercise_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__study_as_exercise_reading, studying_communities).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__study_as_exercise_reading, rabbinic_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__study_as_exercise_reading, yeshiva_institutions).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__study_as_exercise_reading, study_equivalence_talmudic_dictum).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__study_as_exercise_reading, obligation_occupation_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and transmits the Talmudic dictum equating engagement with sacrificial law to bringing the offering itself; decides, through responsa, codes, and curricula, what counts as occupying the commandment under present conditions; trains each generation of interpreters inside the same transmission chain. Gains interpretive standing and institutional continuity from the arrangement. If fulfillment were defined by some other authority or criterion, its distinctive adjudication role over this commandment would lapse. Leaving the arrangement would mean repudiating the tradition that constitutes its own self-understanding, not switching to an equivalent alternative.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, rabbinic_authority, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_kernel__study_as_exercise_reading, rabbinic_authority, beneficiary).

% Observant laypeople and students who engage the sacrificial corpus — through yeshiva tractates, daily recitation of the offering passages in the liturgy, and structured study cycles — and thereby count as having exercised the commandment. They receive fulfillment-status and continuity of practice without transferring money, goods, or labor to anyone. The cost they bear is attention and study hours. Stopping carries social cost inside observant communities, but no external barrier prevents it.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, studying_communities, beneficiary,
    organized, biographical, constrained, global).

% Academies and schools that schedule, fund, examine, and credential the study of sacrificial law, converting the doctrine into curricula, examination tracks, and teaching posts. Enrollment, endowment support, and institutional purpose are tied to the commandment remaining exercisable through study. A rival definition of fulfillment would strand curricular investment and certified expertise built around the current definition.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, yeshiva_institutions, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_kernel__study_as_exercise_reading, yeshiva_institutions, agenda_setter).

% Reject the oral interpretive chain altogether, and with it any rabbinic authority to determine how a written-Torah commandment is fulfilled. From their seat, fulfillment is being defined by the interested interpreters themselves. They have no standing inside the conversation in which the doctrine circulates and no procedural path into it; their objection lives in polemical literature outside the rabbinic frame.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, karaite_communities, excluded,
    moderate, generational, trapped, regional).

% For most of the tradition's recorded span, the advanced study that the doctrine designates as the fulfilling act was overwhelmingly reserved for men; women were taught other portions of the law. Their prospective objection — that access to the designated fulfilling activity was rationed by gender, so the doctrine's universality was partial in practice — is largely absent from the sources that fixed the doctrine. Access has widened in many contemporary communities, but the historical distribution shaped who could occupy the commandment and on what terms.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, historically_excluded_women_students, excluded,
    powerless, biographical, trapped, global).

% Academic scholars documenting how post-destruction rabbinic Judaism converted an altar-centered cult into a text-and-study-centered practice: the origins of the equivalence dictum, its competitors, its liturgical and curricular carriers, and its adaptive function across two millennia. They hold no position inside the halakhic conversation and collect nothing from the arrangement's operation.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, halakhic_historians, observer,
    analytical, civilizational, analytical, global).

narrative_ontology:fixing_cost_class(sacrifice_obligation_kernel__study_as_exercise_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Keeps a Temple-dependent commandment exercisable for a diaspora community with no altar: the arrangement fixes intellectual engagement with the sacrificial corpus as the current legitimate form of the obligation, so daily practice, liturgical recitation, and the legal study of sacrifices continue as living observance rather than historical memory.
% TRANSFER_FUNCTION: Moves hours of study and attention from community members into the sacrificial legal corpus, and returns recognition: demonstrated mastery confers standing and teaching authority within the community. No money, goods, or labor pass from any party to any other through the arrangement itself.
% ABSENT_VOICES: Karaite communities, who deny the interpretive chain any authority to redefine a written-Torah commandment, are outside the conversation entirely. Historically, women were largely barred from the advanced study the doctrine designates as fulfilling; their objection — that access to the fulfilling act was rationed by gender — is barely recorded in the sources that settled the doctrine. Both absences are commentary-grade: they mark seats the consensus formed without, not votes it ignored.
% DISAPPEARANCE_RATIONALE: If the study-as-exercise definition vanished overnight, observant communities would confront a binding commandment they can neither perform nor currently occupy: daily offering-recitations and yeshiva tractates would lose their status as fulfillment and become mere preparation or memory. Either distress at permanent non-observance or migration to a successor definition — readiness, archive, or renewed performance claims — would follow, and liturgy, curriculum, and the felt continuity between Sinai and present practice would reorganize around whichever successor won.
% FOUNDING_PROBLEM: After 70 CE the Jerusalem Temple lay destroyed and the sacrificial commandments — the center of biblical worship — became unperformable. The sages faced the problem of how Israel continues to observe obligations it cannot perform, and how the detailed sacrificial law stays operative law rather than becoming a relic.
% FOUNDING_PROBLEM_CORROBORATION: Academic historians of early rabbinic Judaism — seated outside every beneficiary party — attest the post-destruction adaptation problem and the emergence of study-substitution solutions to it. Karaite polemicists, though opponents of the rabbinic answer, concede the same underlying problem: the commandments became unperformable and something had to be said about them. Independent corroboration of the founding problem exists; what remains disputed is the rabbinic solution, not the problem.
narrative_ontology:disappearance_verdict(sacrifice_obligation_kernel__study_as_exercise_reading, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_kernel__study_as_exercise_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_kernel__study_as_exercise_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sacrifice_obligation_kernel__study_as_exercise_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_kernel__study_as_exercise_reading, 0.04, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_kernel__study_as_exercise_reading_tests).
:- end_tests(sacrifice_obligation_kernel__study_as_exercise_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored near zero (0.04 at interval end) because nothing is transferred from anyone: study costs attention, and fulfillment flows back to the studier at no charge to third parties. The residual 0.02-to-0.04 band reflects interpretive-standing accrual to the teaching class, addressed by the subfloor omega rather than asserted away. Suppression is 0.10 as raw unscaled structure: nothing blocks entry, exit, or rival belief; the residue is ordinary communal expectation. Theater is low (0.12) because the designated activity — study — substantively occurs; the slow rise across the grid tracks the growth of quick liturgical recitation of offering passages alongside deep study. Accessibility_collapse (0.68) is moderate-high: within practice, once the definition is accepted there is no alternative way to occupy the obligation, but the rival definitions remain intellectually live, so alternatives are narrowed rather than extinguished. Resistance is low (0.15): the definition has been mainstream inside rabbinic Judaism since late antiquity, and outsiders are indifferent or opposed on grounds that predate this particular reading. Measurement series run on ONE shared grid (t = 0, 20, 40, 60, 80, 100; one unit ≈ one generation, t0 at the early codification of the equivalence dictum) with both tracked metrics authored at every point; suppression_requirement series is deliberately unauthored because the enforcement picture is static — the scalar suppression already carries it, and the guidance forbids inventing an enforcement-dynamics narrative the story does not have.
 *
 * PERSPECTIVAL GAP:
 *   There is no payer seat, so the divergence the engine computes runs along a different axis than the usual target/beneficiary split. The stewarding seat (rabbinic_authority) experiences the arrangement as the unbroken continuation of revealed law it administers — the definition IS its inheritance. The excluded seats experience the same structure incompatibly: karaite_communities read it as the interpreters grading their own homework; historically_excluded_women_students read it as a fulfilling activity from which they were rationed out. The analytical seat sees a successful two-millennium adaptive transformation of a ruined cult into a portable practice. One structure, three irreconcilable experiences — computed from the structural data, not adjudicated by the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   studying_communities derive d near 0.0 from their beneficiary declaration — they receive fulfillment and transfer nothing. rabbinic_authority derives near-beneficiary from the same declaration, but its adjudication role exists only under THIS definition of fulfillment: it holds a stake in the constraint's specific shape beyond passive subsidy, which the plain beneficiary derivation understates. The directionality override sets the institutional seat to d = 0.15 — still firmly on the beneficiary side, encoding interpretive-standing interest. yeshiva_institutions sit near 0.05 as program beneficiaries. No seat approaches the target end; aggregate effective extraction stays at or below the identity_coordination floor (0.08), matching the zero-transfer structure the story declares.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — occupying an obligation that cannot be performed — is live as long as the obligation is held binding and the altar is absent; no mandate has outlived its function, so mandatrophy_resolved is not declared. The rope classification guards against both characteristic mislabelings. A snare reading would require a victim set, but nothing is transferred from anyone; the arrangement coordinates without collecting. A mountain reading would require naturality, but the definition is an authored, revisable interpretation whose persistence tracks communal acceptance and teaching, not physical law — emerges_naturally is false and no false-summit beneficiaries are declared to smuggle it back in. The nearest genuine drift risk is toward tangled_rope via interpretive-standing rents concentrating in the teaching class; the subfloor_extraction_status_rent and interpretive_monopoly_benignty omegas carry that watch.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint is the study_as_exercise_reading of the sacrifice_obligation_kernel: what would each sibling reading — performance_only, messianic_suspension, symbolic_archive — change structurally if it became the operative definition instead?',
    'Track which definition of fulfillment commanding authorities actually teach and apply under changed conditions (resumed sacrifice, large-scale disengagement); the operative definition in responsa and curricula, not stated allegiance, resolves which reading is live.',
    'performance_only recreates an unfulfillable obligation borne as distress rather than extraction; suspension converts occupation into readiness-maintenance with a waiting posture; the archive reading dissolves the halakhic claim and, with it, rabbinic adjudication of this commandment entirely. Each outcome changes epsilon, the beneficiary structure, and the classification of this seat.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer structure: one of four live readings; the disagreement is located at what counts as occupying the obligation under non-performance conditions.').

omega_variable(
    interpretive_monopoly_benignty,
    'Is rabbinic authority''s interpretive control over what counts as fulfillment a neutral coordination artifact, or does it close the fulfillment definition in ways that primarily serve the interpreters?',
    'Comparative institutional history: fulfillment-path openness in communities with competing interpretive authorities (Rabbanite versus Karaite trajectories); measure whether definitional revisions historically tracked communal welfare or interpreter interests.',
    'If self-serving closure dominates, the arrangement drifts toward a hybrid coordination/extraction profile with rabbinic authority as partial extractor; if the definition stays open and welfare-tracking, the pure-coordination classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_monopoly_benignty, empirical, 'Whether the interpretive closure serves the coordinated community or the coordinating class.').

omega_variable(
    subfloor_extraction_status_rent,
    'Authored epsilon (0.04) sits below the identity_coordination Boltzmann floor (0.08): is the residual extraction inherent coordination cost, or genuine status-rent accruing to the interpretive class?',
    'Longitudinal comparison of authority accrual — credentials, positions, honorifics flowing to the teaching class — against measurable community welfare across periods of doctrinal stability.',
    'Genuine status-rent would lift effective extraction above the floor and flag excess extraction for review; a coordination-cost reading leaves the rope certification untouched.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subfloor_extraction_status_rent, empirical, 'Sub-floor residual: coordination cost or concentrated interpretive rent.').

omega_variable(
    gender_access_rationing,
    'Did the arrangement operate universally, or did access rationing by gender mean only part of the population could perform the designated fulfilling activity — leaving a suppressed historical claimant set?',
    'Historical records of women''s access to advanced sacrificial-law study across periods and regions; compare formal doctrine (open to all obligated) against documented practice (who actually studied).',
    'Confirmed rationing raises the historical suppression estimate and supplies a victim-shaped seat for past windows of the constraint, shifting per-era classifications even if the present-day arrangement remains victim-free.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(gender_access_rationing, empirical, 'Historical access rationing versus doctrinal universality of the fulfilling activity.').

omega_variable(
    cs_framing_alternative_lineage_kernel,
    'The cs_structure frames THIS constraint as the interpretive ruling operating inside a lineage-authority system; a coherent alternative framing treats the lineage-authority structure itself as the constraint under analysis. Which framing governs the classification?',
    'Scope test: if the analysis question is ''what fulfills the sacrificial commandment,'' the ruling is the constraint and this story stands; if it is ''what sustains rabbinic legitimacy,'' the authority structure is the constraint and this story''s epsilon becomes one component of a larger extraction profile.',
    'Under the alternative framing, classification migrates to the authority structure''s own profile — potentially hybrid coordination/extraction via interpretive-monopoly dynamics — and this story''s near-zero epsilon would no longer describe the whole arrangement under evaluation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_alternative_lineage_kernel, conceptual, 'Framing under-determination: ruling-as-constraint versus lineage-authority-as-constraint; the story is scoped to the former on the grounds that the fulfillment definition, not the authority chain, is the arrangement under contest.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_kernel__study_as_exercise_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(study_exercise_reading_tr_t0, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(study_exercise_reading_tr_t0, observed).
narrative_ontology:measurement(study_exercise_reading_tr_t20, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 20, 0.07).
narrative_ontology:measurement_basis(study_exercise_reading_tr_t20, observed).
narrative_ontology:measurement(study_exercise_reading_tr_t40, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 40, 0.09).
narrative_ontology:measurement_basis(study_exercise_reading_tr_t40, observed).
narrative_ontology:measurement(study_exercise_reading_tr_t60, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 60, 0.1).
narrative_ontology:measurement_basis(study_exercise_reading_tr_t60, observed).
narrative_ontology:measurement(study_exercise_reading_tr_t80, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 80, 0.11).
narrative_ontology:measurement_basis(study_exercise_reading_tr_t80, observed).
narrative_ontology:measurement(study_exercise_reading_tr_t100, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 100, 0.12).
narrative_ontology:measurement_basis(study_exercise_reading_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(study_exercise_reading_be_t0, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 0, 0.02).
narrative_ontology:measurement_basis(study_exercise_reading_be_t0, observed).
narrative_ontology:measurement(study_exercise_reading_be_t20, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 20, 0.02).
narrative_ontology:measurement_basis(study_exercise_reading_be_t20, observed).
narrative_ontology:measurement(study_exercise_reading_be_t40, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 40, 0.03).
narrative_ontology:measurement_basis(study_exercise_reading_be_t40, observed).
narrative_ontology:measurement(study_exercise_reading_be_t60, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 60, 0.03).
narrative_ontology:measurement_basis(study_exercise_reading_be_t60, observed).
narrative_ontology:measurement(study_exercise_reading_be_t80, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 80, 0.04).
narrative_ontology:measurement_basis(study_exercise_reading_be_t80, observed).
narrative_ontology:measurement(study_exercise_reading_be_t100, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 100, 0.04).
narrative_ontology:measurement_basis(study_exercise_reading_be_t100, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(sacrifice_obligation_kernel__study_as_exercise_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_kernel__study_as_exercise_reading, identity_coordination).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__study_as_exercise_reading, performance_only_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__study_as_exercise_reading, messianic_suspension_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__study_as_exercise_reading, symbolic_archive_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the sacrificial obligation' decomposes into four structurally distinct readings with different ε values, beneficiary structures, and victim sets — exactly the ε-invariance decomposition the framework requires. Upstream within the family: the Talmudic equivalence dictum carries the highest internal-transmission confidence and is cited in opposite directions by this reading (study equals offering) and performance_only (study merely prepares); the archive reading draws on the same corpus while denying it any halakhic force. Each member links to the others through affects_constraints; no member averages over the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sacrifice_obligation_kernel__study_as_exercise_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
