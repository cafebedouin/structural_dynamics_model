% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_kernel__performance_only_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_kernel__performance_only_reading, []).

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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: sacrifice_obligation_kernel__performance_only_reading
 *   human_readable: Sacrifice Obligation — Performance-Only Reading (Study Preparatory, Not Discharging)
 *   domain: religious law / halakhic authority / commitment-system dynamics
 *
 * SUMMARY:
 *   This story instantiates the performance_only_reading of the sacrifice
 *   obligation kernel: the commandments of animal sacrifice remain binding,
 *   their discharge condition is physical performance in the restored
 *   service, and study of the sacrificial laws — however meritorious — is
 *   preparation, not fulfillment. The standing arrangement under contest is a
 *   community bound for roughly 1,900 years by commandments it cannot
 *   perform, administered by a legal tradition that maintains the
 *   obligation's force, the liturgy that rehearses the service daily, and the
 *   study-curriculum that trains for a performance that never comes. The
 *   epsilon referent is that standing arrangement assessed by this reading's
 *   own lights: the obligation is real, the incapacity is real, and the gap
 *   between them is the arrangement's defining fact. The claim/metrics split
 *   is deliberate: the reading itself claims faithful custodianship of a live
 *   command, while the authored metrics describe a heavily extracting,
 *   increasingly theatrical, inertially maintained arrangement — the engine
 *   measures that divergence rather than reconciling it. Sibling readings
 *   (study_as_exercise, messianic_suspension, symbolic_archive) are separate
 *   constraint stories linked through the network edges; their existence is
 *   noted here only as family structure, not folded into this file's
 *   classification.
 *
 * KEY AGENTS:
 *   - - halakhic_authority_structure: Administering seat (institutional/identity_locked) — transmits and defends the discharge rule; could change it only at the cost of repudiating its own authority; captures no rent
 *   - - commanded_jewish_people: Primary bearer (organized/identity_locked) — bound, unable to perform, obligation forwarded generationally
 *   - - priestly_lineage_descendants: Vocational bearer (organized/identity_locked) — priestly identity maintained in abeyance
 *   - - temple_restoration_activists: Maximal-acceptance bearers (moderate/constrained) — treat the unfulfilled obligation as grievance to end, not endure
 *   - - study_discharge_advocates: Overruled internal voice (organized/identity_locked) — hold a discharge reading the operative ruling rejects
 *   - - comparative_liturgists: Analytical observer (analytical/analytical) — documents the practice-norm gap from outside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_kernel__performance_only_reading, 0.74).
domain_priors:suppression_score(sacrifice_obligation_kernel__performance_only_reading, 0.4).
domain_priors:theater_ratio(sacrifice_obligation_kernel__performance_only_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, extractiveness, 0.74).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_kernel__performance_only_reading, piton).
narrative_ontology:human_readable(sacrifice_obligation_kernel__performance_only_reading, "Sacrifice Obligation — Performance-Only Reading (Study Preparatory, Not Discharging)").
narrative_ontology:topic_domain(sacrifice_obligation_kernel__performance_only_reading, "religious law / halakhic authority / commitment-system dynamics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_kernel__performance_only_reading, '497d06cb-8f44-42ea-835d-e251fa4ae38d').
narrative_ontology:cs_kernel_codification('497d06cb-8f44-42ea-835d-e251fa4ae38d', fixed_text).
narrative_ontology:cs_authority_grounding('497d06cb-8f44-42ea-835d-e251fa4ae38d', lineage).
narrative_ontology:cs_interpretation_layer_present('497d06cb-8f44-42ea-835d-e251fa4ae38d').
narrative_ontology:cs_reading_relation('497d06cb-8f44-42ea-835d-e251fa4ae38d', sacrifice_obligation_kernel__study_as_exercise_reading, forecloses).
narrative_ontology:cs_reading_relation('497d06cb-8f44-42ea-835d-e251fa4ae38d', sacrifice_obligation_kernel__messianic_suspension_reading, forecloses).
narrative_ontology:cs_reading_relation('497d06cb-8f44-42ea-835d-e251fa4ae38d', sacrifice_obligation_kernel__symbolic_archive_reading, coexists_with).
narrative_ontology:cs_axiom('497d06cb-8f44-42ea-835d-e251fa4ae38d', foundational, obligation_binding_independent_of_capacity).
narrative_ontology:cs_axiom_status(obligation_binding_independent_of_capacity, holdable).
narrative_ontology:cs_axiom_grounding('497d06cb-8f44-42ea-835d-e251fa4ae38d', obligation_binding_independent_of_capacity, deontological).
narrative_ontology:cs_axiom('497d06cb-8f44-42ea-835d-e251fa4ae38d', foundational, physical_performance_exhausts_discharge).
narrative_ontology:cs_axiom_status(physical_performance_exhausts_discharge, holdable).
narrative_ontology:cs_axiom_grounding('497d06cb-8f44-42ea-835d-e251fa4ae38d', physical_performance_exhausts_discharge, conventional).
narrative_ontology:cs_reference_frame('497d06cb-8f44-42ea-835d-e251fa4ae38d', sinaitic_performance_commandment).
narrative_ontology:cs_drift_state('497d06cb-8f44-42ea-835d-e251fa4ae38d', post_destruction_diaspora_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('497d06cb-8f44-42ea-835d-e251fa4ae38d', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_kernel__performance_only_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_victim(sacrifice_obligation_kernel__performance_only_reading, commanded_jewish_people).
narrative_ontology:constraint_victim(sacrifice_obligation_kernel__performance_only_reading, priestly_lineage_descendants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(sacrifice_obligation_kernel__performance_only_reading, temple_restoration_activists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Transmits and adjudicates the ruling that the sacrifice commandments remain binding and are discharged only by physical performance, with study of the laws serving as preparation rather than fulfillment. Codifies the discharge rule across successive legal compilations, maintains liturgy and curricula oriented toward eventual restoration, and defends the ruling against rival discharge proposals arising inside the tradition. It could in principle reinterpret the discharge rule, but doing so would require repudiating its own chain of transmitted rulings. It collects no material rent from the unfulfilled obligation and carries the ongoing burden of answering the discharge challenge.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__performance_only_reading, halakhic_authority_structure, agenda_setter,
    institutional, civilizational, identity_locked, global).

% Bound by commandments they have been unable to perform for roughly 1,900 years. Each generation studies the sacrificial laws as preparation, recites daily petitions for the restoration of the service, and marks the absence liturgically. The undischarged obligation passes intact to children and converts alike. Leaving the covenant community would mean leaving the peoplehood itself, which for the overwhelming majority is not a live option; the obligation therefore travels with them wherever they settle.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__performance_only_reading, commanded_jewish_people, payer,
    organized, generational, identity_locked, global).

% Inherit priestly status tied to a service that cannot currently occur. Some communities maintain purity practices and genealogical records in anticipation of resumed service; others carry the lineage as an unused inheritance. Their vocational identity is constituted by duties in abeyance — honored, named, and publicly recalled in liturgy, but unexercisable.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__performance_only_reading, priestly_lineage_descendants, payer,
    organized, generational, identity_locked, global).

% A small movement that accepts the performance premise maximally and treats the unfulfilled obligation as an urgent grievance to be ended rather than endured. They pursue political advocacy, liturgical innovation, and practical preparation aimed at enabling actual performance. Their identification with the performance premise is total, which closes off the exit taken by co-religionists who simply bracket the question.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__performance_only_reading, temple_restoration_activists, payer,
    moderate, biographical, constrained, regional).

% Streams inside the tradition — from the amoraic teaching attributed to R. Yochanan that studying the laws of a burnt offering is as if one offered it, through later homiletic and devotional currents — hold that intellectual engagement genuinely occupies the commandment. The operative ruling rejects their discharge claim, so their proposed resolution carries no weight in the arrangement this story describes. They remain inside the covenant and cannot exit without forfeiting the tradition they interpret.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__performance_only_reading, study_discharge_advocates, excluded,
    organized, generational, identity_locked, global).

% Document how dispersed communities have maintained sacrificial petition, calendrical memory, and readiness-practice across two millennia without any performance occurring. Trace the widening distance between the codified norm and lived practice. Hold no position on the ruling's content and bear none of its burdens.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__performance_only_reading, comparative_liturgists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_obligation_kernel__performance_only_reading, diffuse).
narrative_ontology:fixing_cost_class(sacrifice_obligation_kernel__performance_only_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a single, transmissible standard of what the covenant requires — which commandments bind, what counts as performing them, and what discharges them — across roughly 120 generations in the absence of the performance infrastructure the commandments presuppose. It prevents privatized discharge (each person deciding for themselves that study, intention, or metaphor suffices) and preserves operational knowledge of the service against the possibility of restoration.
% TRANSFER_FUNCTION: Moves interpretive labor, study time, and liturgical attention from each generation of the commanded community toward a perpetually deferred performance, and forwards the undischarged obligation itself from parents to children. Nothing material flows to any agent: the transferred items are compliance-debt, attention, and preparation-work, receivable only by a restored service that has not existed.
% ABSENT_VOICES: The study-discharge advocates are present in the tradition but structurally unheard — their resolution is rejected by the operative ruling and enters this arrangement only as a rejected minority position. Secularized descendants of the commanded community, for whom the entire obligation-apparatus is unintelligible, are outside the conversation altogether and would object to its premises rather than its details. Both groups pair with the excluded stakeholder seat and the observer seat respectively.
% DISAPPEARANCE_RATIONALE: If the performance-only ruling vanished overnight, the discharge question would reopen immediately: communities adopting the study-as-exercise position would consider the commandment occupied at once, liturgical restoration petitions would lose their object, the readiness-curricula would reframe as cultural study, and restoration politics would deflate. Liturgy, education, priestly identity-maintenance, and messianic politics all hang on the deficit staying open — the arrangements rearrange around whichever successor account of the obligation wins.
% FOUNDING_PROBLEM: How a command-community preserves the bindingness and legibility of a performance-commandment through prolonged incapacity — neither falsifying the command by declaring substitutes sufficient, nor abandoning it — while keeping the community oriented to a uniform standard and prepared for renewed capacity.
% FOUNDING_PROBLEM_CORROBORATION: No beneficiary set exists to self-attest, so corroboration comes from the record itself: the continuous treatment of sacrificial law as practically operative in successive legal compilations (Mishnah through the later codes), the unbroken daily recitation of restoration petitions across geographically separated communities, and academic liturgical historiography documenting that continuity from outside the tradition. The sibling readings dispute the status — discharge and archive readings effectively declare the problem resolved or dissolved — and that dispute is recorded in the omega variables rather than averaged away here.
narrative_ontology:disappearance_verdict(sacrifice_obligation_kernel__performance_only_reading, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_kernel__performance_only_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_kernel__performance_only_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sacrifice_obligation_kernel__performance_only_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_kernel__performance_only_reading, 0.74, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_kernel__performance_only_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sacrifice_obligation_kernel__performance_only_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sacrifice_obligation_kernel__performance_only_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.74 at interval end) because the arrangement imposes a standing, undischarged claim on every member of the community and forwards it intact to each generation — a compound deficit with no settlement mechanism short of restoration. Suppression is moderate (0.40) and mostly internalized rather than structural: no barrier prevents an individual from privately adopting a discharge reading, but covenantal identity fusion makes exit equivalent to leaving the peoplehood, and the operative ruling strips the internal alternative of official effect. Roughly two-thirds of the suppressive force is internalized identity, one-third structural (social and institutional cost of deviation). Theater rises monotonically (0.25 to 0.48) as living memory of actual performance faded and embodied rehearsal — daily recitation of the service's order, festival reenactments of what would be brought — became the arrangement's main physical form; the ratio is authored honestly for a piton profile without being tuned to certify one. Accessibility_collapse is low (0.35): the rival readings remain articulable and openly held, so understanding the arrangement does not close alternatives. Resistance is moderate (0.50): the perennial intra-traditional pull toward discharge readings plus modern departures from the obligation-apparatus altogether. The temporal series run on one shared nine-point grid. The extractiveness series oscillates twice (dips at t=24 and t=48) superposed on upward drift: the dips correspond to periods when discharge and spiritualizing readings gained institutional ground, the recoveries to codifying reassertions that restabilized the performance-only norm. The oscillation is hermeneutic competition, not intermittent reinforcement — no seat profits from cycling it. The suppression_requirement series traces the enforcement arc independently: a codification ratchet hardening interpretive enforcement through the middle of the interval, then decay as pluralism made rival readings openly tenable and enforcement capacity shifted from coercion to custodial repetition.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the administering seat compute differently from the same structure. From the commanded community's position the arrangement is a live, unpayable claim carried under identity lock — the engine should compute near-full-target extraction for that seat. From the administering seat the same arrangement is custodial fidelity: the tradition maintains what it believes it received, bears the defensive burden against discharge proposals, and collects nothing — a much lower effective extraction despite identical formal power. The activist seat accepts the claim's validity entirely and experiences only the unavailability as the injury. The overruled discharge advocates experience the arrangement as a door held shut on a resolution they can see. The engine computes these divergences from the structural data; the authored claim adjudicates nothing.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations are deliberately empty: no actor collects from the unfulfilled obligation — the extracted value (compliance-debt, attention, preparation-labor) dissipates into liturgy and study rather than accruing to any seat, which is the receipt surface's 'diffuse' assertion made after checking every named seat. Victims are declared for the commanded community and the priestly lineage, driving their derived directionality toward the full-target end, amplified by identity_locked exit. Two overrides correct derivations the structural data alone cannot produce. First, the administering seat (institutional): a naive derivation risks reading administrators as near-beneficiaries; the override to d=0.40 records its actual position — mildly subsidized by the legitimation the open question provides, but far from capture, since it bears maintenance and defense costs and receives no material flow. Second, the activist seat (moderate): victim-role derivation would place it near full-target, but its total endorsement of the performance premise converts the deficit into purpose; the override to d=0.60 records a target that fights for the constraint's activation rather than against its existence.
 *
 * MANDATROPHY ANALYSIS:
 *   The piton claim rests on the cost-asymmetry test, not the theater ratio: the administering seat could change the arrangement — adopting a discharge reading or formalizing suspension is interpretively available — but the cost to it (repudiating its own transmitted rulings, fracturing the community, conceding nineteen centuries of framing) exceeds the diffuse burden it bears, while no seat profits enough to defend the arrangement as extraction and no single bearer is hurt enough to force the fix. Mislabeling risks run both ways. Reading the arrangement as a snare fails for lack of a capturer: with no beneficiary set, the extraction has no recipient, and the persistence mechanism is inertia plus identity, not coercive enforcement (the suppression series ends low and falling). Reading it as a rope fails because the arrangement does not serve the bearers' alternatives — it forecloses the discharge resolutions large parts of the tradition find compelling. On the R5 interview, the founding problem is live under this reading's own lights and the disappearance verdict is world_rearranges, so the mismatch consumer finds no dead-mandate flag here; the sibling readings that declare the mandate dead or dissolved carry that contention in their own files, and the disagreement is routed through the kernel_reading_indexicality omega rather than averaged into this story's status.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading of the sacrifice_obligation_kernel (performance_only_reading). What changes structurally if a sibling reading becomes operative?',
    'Cross-reading comparison of the four family stories'' victim sets, epsilon referents, and computed types: study_as_exercise empties the victim set and collapses epsilon toward the coordination floor; messianic_suspension removes present force and shifts the arrangement toward anticipatory scaffolding; symbolic_archive closes the corpus and ends the obligation''s career entirely.',
    'Every metric in this story is indexed to the performance-only reading. Under a sibling reading the same historical material yields a different constraint with different victims and a different type; classifications computed from this file do not transfer across readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Indexical dependence of epsilon and victim structure on reading selection within the kernel.').

omega_variable(
    discharge_rule_warrant,
    'Is the nondischarge ruling (study prepares but does not fulfill) a principled entailment of action-commandment semantics, or a contingent interpretive choice that the rival study-as-offering dictum shows to be revisable?',
    'Systematic textual-historical analysis of discharge precedents across classes of commandments: if action-commandments uniformly resist intellectual substitution on principled grounds, the ruling is structural; if the precedent record is genuinely divided, the ruling is one enforceable option among available readings.',
    'If contingent, the arrangement''s persistence is attributable to institutional choice, strengthening extraction attribution to the administering seat; if principled, the command-capacity gap is irreducible within the framework and the extraction is nobody''s doing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discharge_rule_warrant, empirical, 'Whether the discharge rule is principled or revisably conventional.').

omega_variable(
    victim_status_voluntariness,
    'Are the commanded-but-unable victims of an imposed standing cost, or voluntary obligees whose unmet obligation is a self-chosen covenantal commitment they would not trade away?',
    'Compare communities retaining the performance reading under identical incapacity with those adopting discharge or suspension readings: measure deficit-correlates such as liturgical mourning intensity, restoration-politics engagement, and stated willingness to retain the obligation absent any prospect of performance.',
    'If the voluntariness framing holds, effective extraction drops sharply and the arrangement reads as costly identity coordination the members sustain; if imposition holds, the diffuse-burden dynamics strengthen and the payer seat computes nearer full-target.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_status_voluntariness, conceptual, 'Imposed-cost versus chosen-commitment framing of the community''s position.').

omega_variable(
    restoration_expectation_decay,
    'Does the arrangement''s persistence depend on credible restoration expectation, and what happens to the undischarged obligation if that expectation fades below a threshold?',
    'Longitudinal attitude and practice data across communities with varying restoration salience: track whether declining expectation produces adoption of discharge readings, conversion of the obligation into pure inherited inertia, or intensified revival movements.',
    'Fading expectation converts anticipation into pure inertia (deepening the atrophied-function profile) or forces the community across a reading boundary; sustained expectation keeps the arrangement in its anticipatory mode indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(restoration_expectation_decay, empirical, 'Dependence of persistence on live restoration expectation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_kernel__performance_only_reading, 0, 64).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacrifice_performance_reading_tr_t0, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(sacrifice_performance_reading_tr_t8, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(sacrifice_performance_reading_tr_t16, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 16, 0.32).
narrative_ontology:measurement(sacrifice_performance_reading_tr_t24, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 24, 0.35).
narrative_ontology:measurement(sacrifice_performance_reading_tr_t32, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 32, 0.38).
narrative_ontology:measurement(sacrifice_performance_reading_tr_t40, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement(sacrifice_performance_reading_tr_t48, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 48, 0.44).
narrative_ontology:measurement(sacrifice_performance_reading_tr_t56, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 56, 0.46).
narrative_ontology:measurement(sacrifice_performance_reading_tr_t64, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 64, 0.48).

% Extraction over time
narrative_ontology:measurement(sacrifice_performance_reading_be_t0, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(sacrifice_performance_reading_be_t8, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 8, 0.61).
narrative_ontology:measurement(sacrifice_performance_reading_be_t16, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 16, 0.65).
narrative_ontology:measurement(sacrifice_performance_reading_be_t24, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 24, 0.6).
narrative_ontology:measurement(sacrifice_performance_reading_be_t32, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 32, 0.64).
narrative_ontology:measurement(sacrifice_performance_reading_be_t40, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 40, 0.69).
narrative_ontology:measurement(sacrifice_performance_reading_be_t48, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 48, 0.65).
narrative_ontology:measurement(sacrifice_performance_reading_be_t56, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 56, 0.7).
narrative_ontology:measurement(sacrifice_performance_reading_be_t64, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 64, 0.74).

% Suppression requirement over time
narrative_ontology:measurement(sacrifice_performance_reading_su_t0, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(sacrifice_performance_reading_su_t8, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 8, 0.48).
narrative_ontology:measurement(sacrifice_performance_reading_su_t16, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 16, 0.55).
narrative_ontology:measurement(sacrifice_performance_reading_su_t24, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 24, 0.58).
narrative_ontology:measurement(sacrifice_performance_reading_su_t32, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 32, 0.52).
narrative_ontology:measurement(sacrifice_performance_reading_su_t40, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 40, 0.47).
narrative_ontology:measurement(sacrifice_performance_reading_su_t48, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 48, 0.44).
narrative_ontology:measurement(sacrifice_performance_reading_su_t56, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 56, 0.42).
narrative_ontology:measurement(sacrifice_performance_reading_su_t64, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 64, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_kernel__performance_only_reading, identity_coordination).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__performance_only_reading, sacrifice_obligation_kernel__study_as_exercise_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__performance_only_reading, sacrifice_obligation_kernel__messianic_suspension_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__performance_only_reading, sacrifice_obligation_kernel__symbolic_archive_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'the sacrifice obligation' conflates four structurally distinct claims that share one kernel and one historical referent (the standing post-destruction arrangement) but carry different reading-indexed epsilon values, different victim sets, and different types. This story holds the maximal-claim reading; the study_as_exercise and messianic_suspension stories hold discharge-altering readings with reduced or emptied victim sets; the symbolic_archive story holds a register shift with no halakhic claim at all. Upstream/downstream: the performance-only reading is the baseline from which the others deviate, and each sibling story cites this arrangement as the position it modifies — hence the edges run from this file to all three siblings. No epsilon is averaged across readings; each file classifies its own reading cleanly.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sacrifice_obligation_kernel__performance_only_reading, institutional, 0.4).
constraint_indexing:directionality_override(sacrifice_obligation_kernel__performance_only_reading, moderate, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
