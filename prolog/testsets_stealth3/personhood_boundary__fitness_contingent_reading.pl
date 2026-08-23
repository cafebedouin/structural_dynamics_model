% ============================================================================
% CONSTRAINT STORY: personhood_boundary__fitness_contingent_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_personhood_boundary__fitness_contingent_reading, []).

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
 *   constraint_id: personhood_boundary__fitness_contingent_reading
 *   human_readable: Fitness-Gated Personhood Boundary (Demonstrated-Capacity Reading)
 *   domain: moral philosophy/commitment systems/historical ethics
 *
 * SUMMARY:
 *   A polity conditions moral and legal personhood on demonstrated fitness:
 *   newborns — and, in the arrangement's mature forms, institutionalized
 *   adults — acquire standing only by passing an examination administered by
 *   the governing authority, and entities that fail, or have not yet been
 *   presented, lack standing, with lethal or custodial consequences. The
 *   claimed type is authored independently of the metrics: the arrangement is
 *   claimed as a snare because whatever rationing narrative travels with it,
 *   its persistence depends on coercively maintained exclusion of
 *   identifiably named classes and its returns concentrate in the examining
 *   authority. This file instantiates one reading of the personhood_boundary
 *   kernel; reading-specific uncertainties are routed to omega variables, and
 *   sibling readings live in their own files linked through the network.
 *
 * KEY AGENTS:
 *   - regime_authority: Primary beneficiary and agenda-setter (institutional/arbitrage) — writes the fitness statute, defines thresholds, collects legitimacy and savings
 *   - examination_boards: Executing beneficiary with agenda-setting reach (powerful/constrained) — conducts examinations, issues unappealable verdicts
 *   - hygiene_professional_class: Secondary beneficiary (powerful/mobile) — supplies the scientific vocabulary, staffing, and careers premised on the boundary
 *   - pre_fitness_newborns: Primary target (powerless/trapped) — await or fail the examination; failure terminates recognition and life
 *   - disabled_persons_under_review: Extended target (powerless/trapped) — live under recurring reclassification risk inside confinement institutions
 *   - parents_of_examinees: Coerced intermediary (moderate/constrained) — must present children and comply; partial relief benefit binds some to the machinery
 *   - moral_dissenters_clergy: Excluded objectors (organized/constrained) — barred from panels, penalized for sheltering the condemned
 *   - contemporary_bioethics_community: Analytical observer (analytical/analytical) — audits the record; holds no enforcement seat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(personhood_boundary__fitness_contingent_reading, 0.82).
domain_priors:suppression_score(personhood_boundary__fitness_contingent_reading, 0.85).
domain_priors:theater_ratio(personhood_boundary__fitness_contingent_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(personhood_boundary__fitness_contingent_reading, snare).
narrative_ontology:human_readable(personhood_boundary__fitness_contingent_reading, "Fitness-Gated Personhood Boundary (Demonstrated-Capacity Reading)").
narrative_ontology:topic_domain(personhood_boundary__fitness_contingent_reading, "moral philosophy/commitment systems/historical ethics").

domain_priors:requires_active_enforcement(personhood_boundary__fitness_contingent_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(personhood_boundary__fitness_contingent_reading, '88de4afd-3b99-45d0-a575-8fd3dc952f06').
narrative_ontology:cs_kernel_codification('88de4afd-3b99-45d0-a575-8fd3dc952f06', formalized).
narrative_ontology:cs_authority_grounding('88de4afd-3b99-45d0-a575-8fd3dc952f06', extraction).
narrative_ontology:cs_interpretation_layer_present('88de4afd-3b99-45d0-a575-8fd3dc952f06').
narrative_ontology:cs_reading_relation('88de4afd-3b99-45d0-a575-8fd3dc952f06', personhood_boundary__birth_threshold_reading, forecloses).
narrative_ontology:cs_reading_relation('88de4afd-3b99-45d0-a575-8fd3dc952f06', personhood_boundary__potential_based_reading, forecloses).
narrative_ontology:cs_axiom('88de4afd-3b99-45d0-a575-8fd3dc952f06', foundational, standing_requires_demonstrated_fitness).
narrative_ontology:cs_axiom_status(standing_requires_demonstrated_fitness, holdable).
narrative_ontology:cs_axiom_grounding('88de4afd-3b99-45d0-a575-8fd3dc952f06', standing_requires_demonstrated_fitness, deontological).
narrative_ontology:cs_axiom('88de4afd-3b99-45d0-a575-8fd3dc952f06', secondary, polity_may_withhold_standing_pending_examination).
narrative_ontology:cs_axiom_status(polity_may_withhold_standing_pending_examination, holdable).
narrative_ontology:cs_axiom_grounding('88de4afd-3b99-45d0-a575-8fd3dc952f06', polity_may_withhold_standing_pending_examination, instrumental).
narrative_ontology:cs_reference_frame('88de4afd-3b99-45d0-a575-8fd3dc952f06', fitness_gated_membership_polity).
narrative_ontology:cs_drift_state('88de4afd-3b99-45d0-a575-8fd3dc952f06', post_nuremberg_bioethics_era, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('88de4afd-3b99-45d0-a575-8fd3dc952f06', '').
narrative_ontology:cs_kernel_id(personhood_boundary__fitness_contingent_reading, personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(personhood_boundary__fitness_contingent_reading, regime_authority).
narrative_ontology:constraint_beneficiary(personhood_boundary__fitness_contingent_reading, examination_boards).
narrative_ontology:constraint_beneficiary(personhood_boundary__fitness_contingent_reading, hygiene_professional_class).
narrative_ontology:constraint_victim(personhood_boundary__fitness_contingent_reading, pre_fitness_newborns).
narrative_ontology:constraint_victim(personhood_boundary__fitness_contingent_reading, disabled_persons_under_review).
narrative_ontology:constraint_victim(personhood_boundary__fitness_contingent_reading, parents_of_examinees).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(personhood_boundary__fitness_contingent_reading, parents_of_examinees).
narrative_ontology:constraint_vindicates(personhood_boundary__fitness_contingent_reading, population_quality_improvement_doctrine).
narrative_ontology:constraint_vindicates(personhood_boundary__fitness_contingent_reading, medical_authority_over_life_verdicts).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacts the personhood statute, defines the fitness threshold, funds and empowers the examination boards, and penalizes concealment and open dissent. Collects the arrangement's principal returns: legitimacy as steward of communal health, budgetary authority over the care apparatus, and the resources released by withholding recognition. Stands above the rule it administers and can widen or narrow the reviewed class as political conditions shift.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, regime_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Conduct the inspections and questionnaire reviews, issue verdicts on which newborns enter the community, and certify the standing of persons under recurring review. Their verdicts are effectively unappealable and their authority over life-defining decisions is unmatched. Individually they cannot refuse service without career and legal jeopardy; collectively they run the day-to-day machinery of the boundary.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, examination_boards, beneficiary,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(personhood_boundary__fitness_contingent_reading, examination_boards, agenda_setter).

% Supply the scientific vocabulary, diagnostic criteria, and staffing on which the examination rests. Careers, journals, institutes, and professional prestige grow around the boundary's administration. Individual members can emigrate or retreat into ordinary clinical practice, but their status premium and research funding depend on the boundary remaining authoritative.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, hygiene_professional_class, beneficiary,
    powerful, biographical, mobile, national).

% Await or undergo the examination with no standing to invoke and no one authorized to speak for their continuation. Failure to pass, or non-presentation at all, ends recognition and, in the arrangement's operative form, ends life. There is no exit: the examination is the only door into moral and legal community, and it is held by others.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, pre_fitness_newborns, payer,
    powerless, immediate, trapped, regional).

% Live recognized only provisionally where the regime extends review beyond infancy. Reclassification at any reassessment can withdraw care, legal protection, and standing itself. Confinement institutions house the reviewable class, which removes both physical mobility and the social contact through which advocacy could form.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, disabled_persons_under_review, payer,
    powerless, biographical, trapped, national).

% Must present newborns for examination and comply with the verdict. Concealing a child invites penalty, loss of communal standing, and suspicion. Some households experience genuine relief from care burdens and come to defend the arrangement; others comply in anguish. Their position binds them to the machinery they also suffer under.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, parents_of_examinees, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(personhood_boundary__fitness_contingent_reading, parents_of_examinees, beneficiary).

% Traditions holding personhood to be unconditional at birth or conception object that no polity holds the authority to adjudicate who counts as a person. Historically they were barred from examination panels, fined or imprisoned for sheltering children marked for exposure, and their objection sat entirely outside the examination's stated terms of reference.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, moral_dissenters_clergy, excluded,
    organized, civilizational, constrained, continental).

% Audits the documentary record, reconstructs the warrant structures behind each historical variant of the boundary, and traces the post-war repudiation of the examination regimes. Holds no enforcement seat and collects nothing from the arrangement's operation or cessation.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, contemporary_bioethics_community, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(personhood_boundary__fitness_contingent_reading, regime_authority).
narrative_ontology:fixing_cost_class(personhood_boundary__fitness_contingent_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides the polity a single public criterion for which newborns and dependent members receive care investment and full recognition, replacing per-household discretion with an administered examination and a shared threshold.
% TRANSFER_FUNCTION: Moves survival, care resources, and legal-moral recognition away from entities that fail, or have not yet passed, the fitness examination, toward the examining authority and the compliant community; moves kill-or-keep decision authority from parents to examination boards.
% ABSENT_VOICES: The examined themselves — pre-fitness newborns and persons slated for review — cannot speak, and the boundary is precisely what denies them representatives. Religious and moral dissenters who denied the state's authority to adjudicate standing were barred from the panels that exercised it. The persons who would have existed absent the practice are structurally voiceless.
% DISAPPEARANCE_RATIONALE: Households would regain sole decision authority over their newborns (or the question would dissolve under unconditional standing), the examination professions would lose their gatekeeping function and much of their claimed expertise, confinement institutions would empty, and the polity's self-understanding as a fitness-sorted community would reorganize around unconditional membership.
% FOUNDING_PROBLEM: In its subsistence-era form: which newborns can a community at the margin of starvation afford to raise, and who decides? In its modern form: how can a state improve the 'quality' of its population and shed the cost of dependent care?
% FOUNDING_PROBLEM_CORROBORATION: Demographic and economic histories attest that industrial-era societies bore no subsistence constraint comparable to the ancient one, and cross-cultural comparison shows equally poor societies that never adopted an examination gate — bracketing pure necessity. The Nuremberg Doctors' Trial record contains prosecution and independent medical testimony that the modern programs served ideological rather than survival ends. No corroborating source outside the benefiting parties attests that the modern founding problem remained live; only the administering authorities and their professional clients assert it.
narrative_ontology:disappearance_verdict(personhood_boundary__fitness_contingent_reading, world_rearranges).
narrative_ontology:founding_problem_status(personhood_boundary__fitness_contingent_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(personhood_boundary__fitness_contingent_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(personhood_boundary__fitness_contingent_reading, 'none', 1).
narrative_ontology:epsilon_provenance(personhood_boundary__fitness_contingent_reading, 0.82, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(personhood_boundary__fitness_contingent_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(personhood_boundary__fitness_contingent_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(personhood_boundary__fitness_contingent_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claimed type and metrics are authored independently. The claim is snare: the arrangement's persistence depends on coercively enforced exclusion with identifiable casualties, and the rationing story functions as cover once its material premise lapses. The metrics describe its documented modern-phase operation. Extractiveness is high (0.82) because the boundary converts an entire class's continuation into the administrator's discretionary grant. Suppression (0.85) is authored as a raw structural property and is deliberately not scaled by power or scope — only extractiveness is scaled downstream; the enforcement machinery must punish concealment, silence dissent, and confine the reviewable class. Theater ratio 0.30: the examinations do sort real people, but as the founding scarcity problem receded the pseudo-medical legitimation layer grew — panels reviewing paper questionnaires without examining anyone, statistics produced for publicity — a rising share of activity defending the boundary rather than performing it. Accessibility_collapse 0.62: within an enforcing regime the alternative (unconditional standing) is criminalized, but concealment networks, flight, and cross-border variation kept partial alternatives alive. Resistance 0.55: family concealment, clerical defiance, and finally external military defeat ended the peak regimes. The measurement series share one time grid (t=0,5,10,15,20,25,30) and ratchet monotonically — no cycle — modeling enforcement intensification and extraction accumulation as the modern programs matured. Same-power divergence: examination boards (constrained) and the wider professional class (mobile) hold equal nominal power with different stakes; boards fuse their authority to the verdict machinery and cannot refuse service without ruin, while journal clinicians keep outside options — identity fusion with the hygiene mission binds both, but asymmetrically. Coalition check: the primary targets cannot form coalitions by construction — the boundary denies them the very agency coalition requires — and the confinement machinery worked to prevent external coalitions (families, clergy) from forming on their behalf; the historical abolition vector ran through precisely such external coalitions plus conquest.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently. From the regime and board seats the arrangement presents as stewardship: a necessary public criterion, competently administered, protecting communal resources — a coordination-shaped experience built on the founding-problem narrative. From the parents' seat it is coerced complicity: compliance under penalty threaded with occasional genuine relief, which is why that seat carries two roles. From the targets' seat — the newborn awaiting verdict, the confined person under review — the arrangement is terminal: no appeal, no exit, no standing to object. From the excluded dissenters' seat it is an usurpation of an authority no polity legitimately holds: adjudicating who counts as a person. The engine derives these per-seat types from the power, exit, and role data; this commentary does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries sit at the beneficiary end: the boundary subsidizes the regime authority, boards, and professional class with legitimacy, authority, and careers, and the regime's arbitrage-grade control over its own rule places it nearest d=0. Declared targets sit at the full-target end amplified by trapped exit: the examination is the only door to membership, so no arbitrage or mobility exists for pre_fitness_newborns or disabled_persons_under_review. Parents derive high d from victim listing, damped somewhat by the declared relief benefit — coerced intermediaries near 0.7. The excluded dissenters experience suppression without transfer: they pay in liberty and conscience rather than appearing in the receipt stream. National scope scales verification difficulty upward for the targets — concealment is detectable at scale — while the regime's own compliance with its self-authored rule is unverifiable by design.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy lens guards both directions here. Forward: the founding scarcity problem of the subsistence era is dead in the industrial phase — the arrangement persisted and intensified after its material justification expired, which is exactly the signature the founding_problem_status x disappearance_verdict mismatch flags for capture/zombie investigation; the modern form maintains itself because the regime collects from the boundary, not because anyone still needs the rationing it performs theatrically. Reverse: crediting the genuine ancient coordination content — harsh-environment viability triage under real scarcity — must not launder the modern form; the same colloquial label spans a dead mandate wearing a live one's clothes. The theater trajectory makes the substitution legible: as necessity departed, performative legitimation filled the vacuum and extraction rose rather than fell. The constraint is not a piton despite its theatrical layer, because capture is concentrated — the regime authority demonstrably collects — and the administrator bears almost none of the cost of an arrangement only it could dissolve.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation,
    'This constraint instantiates the fitness_contingent_reading of the personhood_boundary kernel: do the victim set, enforcement surface, and authority structure authored here hold for this reading alone, independent of the sibling readings?',
    'Compile the three reading-files sharing the kernel and compare computed classifications over shared structural slots; divergent per-seat types across files confirm reading-indexed separation rather than one observable-dependent constraint.',
    'If sibling readings were merged into this file, the victim set would oscillate between empty (birth threshold) and partially populated (potential based), destroying epsilon invariance; kept separate, each reading carries one stable epsilon.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Committer-frame routing: this file is one reading of the personhood_boundary kernel, not the kernel whole.').

omega_variable(
    sibling_birth_threshold_delta,
    'What structurally changes if the birth_threshold_reading replaces this one?',
    'Adopt the sibling''s axiom set — standing attaches at birth unconditionally; the examination machinery loses its object, the victim set empties for all born humans, and the state''s exclusion authority lapses entirely.',
    'The surviving arrangement would classify as inertial residue rather than active exclusion: enforcement machinery without targets is pure vestige, and the regime''s collected legitimacy evaporates with its gatekeeping function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_birth_threshold_delta, conceptual, 'Structural delta under the birth-threshold sibling reading.').

omega_variable(
    sibling_potential_based_delta,
    'What structurally changes if the potential_based_reading replaces this one?',
    'Adopt the sibling''s axiom set — standing attaches via potential for rational agency: healthy newborns re-enter the protected set immediately, leaving only severe-disability cases excluded, and the examination converts from demonstration-testing to prognosis-assessment.',
    'The victim set contracts sharply and extractiveness drops but does not vanish; the enforcement burden migrates from observation of demonstrated capacity to diagnostic prediction, changing suppression mechanics from surveillance of behavior to contested prognostication.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_potential_based_delta, conceptual, 'Structural delta under the potential-based sibling reading.').

omega_variable(
    disagreement_locus_warrant,
    'Where exactly do the three readings of the kernel disagree?',
    'Locate the disputed element: the warrant for moral standing — a birth event (birth_threshold_reading), potential capacity (potential_based_reading), or demonstrated fitness (this reading). Each warrant fixes where in an entity''s lifespan the boundary falls.',
    'Whichever warrant prevails redetermines the temporal boundaries of the victim set; the readings disagree about the criterion itself, not about enforcement machinery or beneficiary identity, which is why the files share enforcement-shape vocabulary but not victim sets.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(disagreement_locus_warrant, conceptual, 'The kernel contest is located in the warrant for standing, not in the administration.').

omega_variable(
    fitness_threshold_political_adjustability,
    'Is the fitness threshold a medically determinate line or a politically adjustable parameter?',
    'Historical audit of threshold revisions: peak regimes repeatedly widened the categories of the excluded when fiscal or ideological pressure rose, extending review from children to adults and from institution populations to broader classes, suggesting adjustment tracked politics rather than medicine.',
    'If the threshold is politically adjustable, the examination is gatekeeping power dressed as diagnosis and the exclusionary reading hardens; if determinate in principle, residual coordination content survives and the assessment splits accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fitness_threshold_political_adjustability, empirical, 'Determinacy of the fitness threshold under political pressure.').

omega_variable(
    scarcity_necessity_history,
    'Was fitness-gated exclusion ever genuinely resource-necessary, or was it ideological across its whole history?',
    'Cross-cultural and demographic bracketing: equally poor societies without any examination gate, and wealthy societies that retained one, jointly bound out pure necessity; subsistence-era mortality data indicate whether ancient triage ever tracked survival arithmetic rather than custom.',
    'If exclusion was never necessary, the coordination function is wholly nominal and the constraint admits no coordination credit in any phase; if it was necessary in subsistence antiquity, the early-phase record deserves coordination acknowledgment that the modern phase forfeited entirely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scarcity_necessity_history, empirical, 'Whether the founding scarcity problem ever actually bound the communities adopting the gate.').

omega_variable(
    parental_compliance_internalization,
    'Is parental compliance driven by structural coercion, internalized acceptance of the regime''s valuations, or both, and in what proportion?',
    'Post-regime testimony and refusal rates under decriminalization: if concealment remained rare even after penalties lapsed, internalization carried the load; if refusals surged once punishment ended, structure carried it.',
    'If substantially internalized, the arrangement''s suppression outlives its enforcement machinery — rescuing the condemned remains deviant and the scalar suppression measure understates the arrangement''s grip on the intermediary seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(parental_compliance_internalization, empirical, 'Structural versus internalized suppression in parental compliance with verdicts.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(personhood_boundary__fitness_contingent_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pers_tr_t0, personhood_boundary__fitness_contingent_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(pers_tr_t5, personhood_boundary__fitness_contingent_reading, theater_ratio, 5, 0.14).
narrative_ontology:measurement(pers_tr_t10, personhood_boundary__fitness_contingent_reading, theater_ratio, 10, 0.17).
narrative_ontology:measurement(pers_tr_t15, personhood_boundary__fitness_contingent_reading, theater_ratio, 15, 0.21).
narrative_ontology:measurement(pers_tr_t20, personhood_boundary__fitness_contingent_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(pers_tr_t25, personhood_boundary__fitness_contingent_reading, theater_ratio, 25, 0.28).
narrative_ontology:measurement(pers_tr_t30, personhood_boundary__fitness_contingent_reading, theater_ratio, 30, 0.3).

% Extraction over time
narrative_ontology:measurement(pers_be_t0, personhood_boundary__fitness_contingent_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(pers_be_t5, personhood_boundary__fitness_contingent_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(pers_be_t10, personhood_boundary__fitness_contingent_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(pers_be_t15, personhood_boundary__fitness_contingent_reading, base_extractiveness, 15, 0.68).
narrative_ontology:measurement(pers_be_t20, personhood_boundary__fitness_contingent_reading, base_extractiveness, 20, 0.74).
narrative_ontology:measurement(pers_be_t25, personhood_boundary__fitness_contingent_reading, base_extractiveness, 25, 0.79).
narrative_ontology:measurement(pers_be_t30, personhood_boundary__fitness_contingent_reading, base_extractiveness, 30, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(pers_su_t0, personhood_boundary__fitness_contingent_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(pers_su_t5, personhood_boundary__fitness_contingent_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(pers_su_t10, personhood_boundary__fitness_contingent_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(pers_su_t15, personhood_boundary__fitness_contingent_reading, suppression_requirement, 15, 0.73).
narrative_ontology:measurement(pers_su_t20, personhood_boundary__fitness_contingent_reading, suppression_requirement, 20, 0.78).
narrative_ontology:measurement(pers_su_t25, personhood_boundary__fitness_contingent_reading, suppression_requirement, 25, 0.82).
narrative_ontology:measurement(pers_su_t30, personhood_boundary__fitness_contingent_reading, suppression_requirement, 30, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(personhood_boundary__fitness_contingent_reading, identity_coordination).
narrative_ontology:affects_constraint(personhood_boundary__fitness_contingent_reading, personhood_boundary__birth_threshold_reading).
narrative_ontology:affects_constraint(personhood_boundary__fitness_contingent_reading, personhood_boundary__potential_based_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'personhood boundary': one kernel, three structurally distinct readings, each a separate file with its own epsilon, victim set, and enforcement surface. This file carries the fitness_contingent_reading only. The birth-threshold sibling is upstream (broadest protected set, negligible extraction); the potential-based sibling sits between (protected set narrowed by severe-disability carve-outs); this reading is downstream-most and most extractive, because its warrant — demonstrated fitness — yields the narrowest protected set and licenses lethal administration. Upstream claims are routinely cited as precedent by downstream defenses, which is why the edges run from this file to both siblings as well as expecting reciprocal links. No file in the family hedges epsilon across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
