% ============================================================================
% CONSTRAINT STORY: sacrifice_commandment__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_commandment__performance_only, []).

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
 *   constraint_id: sacrifice_commandment__performance_only
 *   human_readable: Post-Temple Sacrificial Precepts: Binding-but-Suspended (Performance-Only Reading)
 *   domain: religious/halakhic/commitment-system
 *
 * SUMMARY:
 *   Since the destruction of the Second Temple, the halakhic system has
 *   maintained the sacrificial precepts as binding obligations that cannot be
 *   performed. This story instantiates the performance_only reading of that
 *   arrangement: fulfillment of a sacrificial commandment is constituted by
 *   physical execution, so in the Temple's absence the commandments are
 *   suspended, not discharged by any substitute. Assessed by this reading's
 *   own lights, the standing arrangement extracts heavily: roughly nineteen
 *   centuries of elite scholarly attention have been directed at procedure no
 *   one can execute, attention that on this reading yields no fulfillment and
 *   is diverted from living law. The arrangement nonetheless retains a
 *   genuine coordination function — obligation-continuity, liturgical unity,
 *   refusal of fictional substitution — which is why the claim is
 *   tangled_rope rather than snare. Claim and metrics are authored
 *   independently: the claimed type states the structure this reading
 *   believes true; the metrics state what this reading believes descriptively
 *   operative. Family note: the colloquial label 'the sacrificial
 *   commandments after the destruction' decomposes into three structurally
 *   distinct constraints with different epsilon values — this story (high:
 *   labor with no discharge), study_as_performance (low: the same labor
 *   becomes fulfillment), and archive_maintenance (moderate: preparatory
 *   investment with an implicit sunset at restoration). They are linked via
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - - rabbinic_authority_structure: Agenda-setter (institutional/arbitrage) — rules the precepts binding-but-unperformable and administers the apparatus that keeps them present
 *   - - talmudic_scholars: Primary payer (organized/identity_locked) — lifetime attention directed at unperformable procedure
 *   - - yeshiva_academy_system: Beneficiary (institutional/arbitrage) — converts the study labor into enrollments, posts, and institutional continuity
 *   - - temple_restoration_movement: Beneficiary (organized/identity_locked) — holds the restoration mandate the suspended precepts warrant
 *   - - observant_laity: Payer with secondary beneficiary position (moderate/constrained) — carries the daily liturgical weight, receives identity continuity
 *   - - practical_halakha_advocates: Excluded (organized/constrained) — would reallocate curriculum to living law, holds no curriculum seat
 *   - - academic_historians_of_religion: Analytical observer (analytical/analytical) — documents the apparatus without a fulfillment stake
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_commandment__performance_only, 0.78).
domain_priors:suppression_score(sacrifice_commandment__performance_only, 0.65).
domain_priors:theater_ratio(sacrifice_commandment__performance_only, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, extractiveness, 0.78).
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_commandment__performance_only, tangled_rope).
narrative_ontology:human_readable(sacrifice_commandment__performance_only, "Post-Temple Sacrificial Precepts: Binding-but-Suspended (Performance-Only Reading)").
narrative_ontology:topic_domain(sacrifice_commandment__performance_only, "religious/halakhic/commitment-system").

domain_priors:requires_active_enforcement(sacrifice_commandment__performance_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_commandment__performance_only, '98c9ccc0-796e-4936-a303-e8d795617ff5').
narrative_ontology:cs_kernel_codification('98c9ccc0-796e-4936-a303-e8d795617ff5', fixed_text).
narrative_ontology:cs_authority_grounding('98c9ccc0-796e-4936-a303-e8d795617ff5', lineage).
narrative_ontology:cs_interpretation_layer_present('98c9ccc0-796e-4936-a303-e8d795617ff5').
narrative_ontology:cs_reading_relation('98c9ccc0-796e-4936-a303-e8d795617ff5', sacrifice_commandment__study_as_performance, forecloses).
narrative_ontology:cs_reading_relation('98c9ccc0-796e-4936-a303-e8d795617ff5', sacrifice_commandment__archive_maintenance, coexists_with).
narrative_ontology:cs_axiom('98c9ccc0-796e-4936-a303-e8d795617ff5', foundational, physical_execution_constitutive_of_fulfillment).
narrative_ontology:cs_axiom_status(physical_execution_constitutive_of_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('98c9ccc0-796e-4936-a303-e8d795617ff5', physical_execution_constitutive_of_fulfillment, deontological).
narrative_ontology:cs_axiom('98c9ccc0-796e-4936-a303-e8d795617ff5', secondary, no_substitution_in_absence_of_temple).
narrative_ontology:cs_axiom_status(no_substitution_in_absence_of_temple, holdable).
narrative_ontology:cs_axiom_grounding('98c9ccc0-796e-4936-a303-e8d795617ff5', no_substitution_in_absence_of_temple, deontological).
narrative_ontology:cs_reference_frame('98c9ccc0-796e-4936-a303-e8d795617ff5', temple_cult_execution_baseline).
narrative_ontology:cs_drift_state('98c9ccc0-796e-4936-a303-e8d795617ff5', post_destruction_exile_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('98c9ccc0-796e-4936-a303-e8d795617ff5', '').
narrative_ontology:cs_kernel_id(sacrifice_commandment__performance_only, sacrifice_commandment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_commandment__performance_only, rabbinic_authority_structure).
narrative_ontology:constraint_beneficiary(sacrifice_commandment__performance_only, yeshiva_academy_system).
narrative_ontology:constraint_beneficiary(sacrifice_commandment__performance_only, temple_restoration_movement).
narrative_ontology:constraint_victim(sacrifice_commandment__performance_only, talmudic_scholars).
narrative_ontology:constraint_victim(sacrifice_commandment__performance_only, observant_laity).
narrative_ontology:constraint_victim(sacrifice_commandment__performance_only, practical_halakha_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sacrifice_commandment__performance_only, observant_laity).
narrative_ontology:constraint_vindicates(sacrifice_commandment__performance_only, obligation_continuity_after_destruction).
narrative_ontology:constraint_vindicates(sacrifice_commandment__performance_only, anti_substitution_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adjudicates the status of the sacrificial precepts after the Temple's destruction, ruling them binding but incapable of performance. Maintains the liturgy and curriculum that keep the sacrificial corpus present in daily communal life, and holds sole authority to rule on when and how the precepts could resume. Its standing as arbiter of the suspended obligation depends on the obligation remaining open.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, rabbinic_authority_structure, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_commandment__performance_only, rabbinic_authority_structure, beneficiary).

% Spend years of training and a lifetime of study hours mastering the orders of Temple service and offering procedure. Under this reading none of that study discharges a commandment, yet curriculum requirements, ordination expectations, and communal honor structures keep the material compulsory. Leaving the field means forfeiting scholarly standing and, often, religious belonging.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, talmudic_scholars, payer,
    organized, biographical, identity_locked, global).

% Operates the academies where sacrificial law is taught. The permanently open, permanently unperformable corpus guarantees inexhaustible subject matter: enrollments, faculty posts, publishing programs, and institutional prestige all draw on it. The attention students and teachers spend is the raw material the academies convert into institutional continuity.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, yeshiva_academy_system, beneficiary,
    institutional, generational, arbitrage, global).

% Small, dedicated groups that prepare vessels, garments, site plans, and priestly lineages for a rebuilt Temple. The unfulfilled, still-binding sacrificial precepts are the legal and moral warrant for the project; if the precepts were considered discharged, lapsed, or fulfilled by other means, the movement's mandate would lapse with them.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, temple_restoration_movement, beneficiary,
    organized, civilizational, identity_locked, global).

% Recite the sacrificial passages in the daily prayer service, fund the academies, and inherit a liturgy that keeps the offerings verbally present every morning. They receive communal identity and continuity from the practice while carrying its time and attention costs; stepping outside the liturgical frame generally means stepping outside the community.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, observant_laity, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_commandment__performance_only, observant_laity, beneficiary).

% Teachers, decisors, and communal leaders who argue that curriculum hours and elite attention should go to applicable law: contracts, damages, family status, medical ethics. They hold no seats on the curriculum-setting bodies, which are staffed by the coalition that preserves the sacrificial corpus's centrality.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, practical_halakha_advocates, excluded,
    organized, biographical, constrained, global).

% Document and compare the post-destruction sacrificial apparatus across traditions: its consolidation at Yavneh, its curricular entrenchment in the Geonic and medieval periods, its liturgical diffusion into the daily prayer book. They take no position on fulfillment and bear none of the costs.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, academic_historians_of_religion, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_commandment__performance_only, yeshiva_academy_system).
narrative_ontology:fixing_cost_class(sacrifice_commandment__performance_only, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves the continuity of a legal-obligation system through catastrophic disruption: keeps the commandment inventory intact and binding, prevents ad hoc abandonment or quiet substitution, maintains a shared liturgical memory of the cult, and holds the community oriented toward a single recognized condition (restoration) under which the precepts could resume.
% TRANSFER_FUNCTION: Moves scholarly attention (and lay liturgical attention) away from practically applicable law and toward the procedural law of an unperformable cult; moves institutional resources (enrollment, endowment, curricular hours, publishing capacity) into sustaining that study; concentrates interpretive authority over the suspended obligation in the rabbinic structure.
% ABSENT_VOICES: Practical-halakha advocates have no seat on curriculum-setting bodies and would reallocate elite attention toward living law; the lay communities that fund the system have no vote on how curricular hours are allocated; historically, voices proposing that study of sacrifices be formally downgraded once the texts were secured were marginalized rather than answered.
% DISAPPEARANCE_RATIONALE: If the binding-but-suspended structure vanished overnight, yeshiva curricula would reallocate toward applicable law within a generation, the daily liturgy would shed its sacrificial sections or reframe them as history, restoration movements would lose their legal warrant, and the rabbinic structure would lose its arbitration role over an obligation no longer open. The post-destruction economy of sacred attention is organized around this structure and would reorganize without it.
% FOUNDING_PROBLEM: After 70 CE the tradition faced a covenantal obligation system whose central cult had been destroyed: how to keep the sacrificial precepts binding, the technical knowledge intact, and the community oriented toward restoration, when performance was impossible and despair or substitution threatened the obligation's continuity.
% FOUNDING_PROBLEM_CORROBORATION: Academic historians of religion corroborate the founding problem's existence and its Yavnean-era consolidation from outside the benefiting parties, while disputing its current status: the textual-preservation goal is demonstrably complete (the corpus is comprehensive), supporting the internal critics' claim that the problem is dead; restoration movements and parts of the rabbinic establishment attest it is live, citing anticipated rebuilding. No neutral party attests liveness; liveness is asserted only from within the beneficiary set.
narrative_ontology:disappearance_verdict(sacrifice_commandment__performance_only, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_commandment__performance_only, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_commandment__performance_only, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sacrifice_commandment__performance_only, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_commandment__performance_only, 0.78, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_commandment__performance_only_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sacrifice_commandment__performance_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sacrifice_commandment__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78 at interval end) because the arrangement consumes scarce expert attention on material that, under this reading, discharges nothing; the opportunity cost compounds as living-law domains compete for the same finite scholarly labor pool. Suppression (0.65) is structural and unscaled: curricular mandates, ordination expectations, and communal sanction close off wholesale redirection of attention, though partial specialization elsewhere remains possible, so alternatives are narrowed rather than eliminated. Theater_ratio (0.52) reflects the large share of activity that rehearses impossibility — daily verbal re-enactment of offerings, hypothetical reconstruction of services no one will run — against the genuine technical scholarship that remains real intellectual work. Accessibility_collapse (0.55) is moderate: a scholar who grasps the situation can still specialize in applicable law, at real career and communal cost. Resistance (0.5) is substantive: the sibling readings are themselves intra-traditional resistance to this arrangement's accounting, and modern curricula show visible strain toward electives. The temporal series run on one shared grid (70, 250, 500, 800, 1100, 1400, 1700, 2026) with every tracked metric authored at every point. Enforcement rose steeply during the Yavnean-through-Geonic consolidation, plateaued through the medieval codifications, and has softened in the modern era as electives expand — while extraction stayed high because the accumulated apparatus persists regardless of enforcement intensity.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setting/beneficiary seats compute differently from the same structure. From the rabbinic_authority_structure and yeshiva_academy_system positions the arrangement is a functioning continuity machine they built and administer: the corpus stays intact, the community stays oriented, the academies stay full. From the talmudic_scholars position the identical structure operates as a lifetime tax on attention paid to an account that accepts no deposits — nineteen centuries of labor with no discharge. Observant_laity straddle the divide: the liturgy costs them daily attention while supplying the identity frame that makes the cost invisible. The engine computes this per-seat divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Talmudic_scholars are declared victims with identity_locked exit, placing them near the full-target end: trapped or identity-locked targets amplify effective extraction. Observant_laity are dual-positioned (payer with secondary beneficiary), landing near symmetric — genuine identity benefit, diffuse attention cost. Yeshiva_academy_system and rabbinic_authority_structure are declared beneficiaries with arbitrage-grade exit, damping their effective extraction toward subsidy: they can reinterpret the doctrine (the sibling readings are precisely such reinterpretations) and collect either way. Temple_restoration_movement carries a directionality_override (d=0.10): the automatic derivation weighs identity_lock toward the target end, but structurally they are pure mandate-beneficiaries — the suspended, unfulfilled status of the precepts is the entirety of their warrant, and they bear almost none of the attention cost. Practical_halakha_advocates are excluded rather than coordinated: their objection is real but unseated, which is part of what the enforcement machinery maintains.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preserve cult knowledge and obligation-continuity through exile — is textually complete: the corpus is comprehensive and has been for a millennium. Yet the apparatus persists at full scale, and the parties dispute whether the problem is dead (critics: preservation achieved, continuation is inertia plus identity) or live (restoration parties: the obligation awaits resumption). The mismatch consumer should read founding_problem_status=contested against disappearance_verdict=world_rearranges: the world would visibly rearrange if the structure vanished, so whatever drives its persistence is not mere vestige — but the contested status flags that a substantial share of its current operation may be mandate-outliving-function. Classification discipline cuts both ways: labeling the whole arrangement a snare erases the genuine coordination function (obligation-continuity, anti-substitution integrity) that even this reading's own framework relies on to keep the commandment honestly open rather than quietly abolished; labeling it a rope erases the asymmetric, actively enforced attention extraction the record documents. Tangled_rope holds both halves. If the identity frame of the scholarly class broke — if consensus held that Kodashim mastery is optional — reallocation would follow within a generation, which is the signature of enforced rather than functional persistence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the sacrifice_commandment kernel. Would instantiating a sibling reading change the classification?',
    'Compare the three linked stories directly: under study_as_performance the identical study labor becomes discharge and epsilon falls toward coordination cost; under archive_maintenance it becomes preparatory investment with an implicit sunset at restoration. The disagreement resolves only by settling whether physical execution is constitutive of fulfillment.',
    'If study_as_performance were adopted, this story''s high-extraction verdict dissolves — the same labor fulfills, and the arrangement approaches rope. If archive_maintenance were adopted, extraction moderates and a sunset condition appears. The performance_only reading is the maximal-extraction instantiation of the kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: this story is the performance_only reading; sibling readings re-price the same labor.').

omega_variable(
    counterfactual_attention_allocation,
    'Is scholarly attention genuinely diverted from living law, or would the marginal labor absorbed by Kodashim study have gone to leisure or non-halakhic pursuits absent the arrangement?',
    'Compare career-length output profiles of scholars in institutions with heavy versus light Kodashim curricular requirements, controlling for cohort size and total study hours; or exploit curricular reforms as natural experiments in reallocation.',
    'If the attention is genuinely diverted, the victim declaration stands and extraction is real; if the labor market would hold slack regardless, effective extraction drops and the arrangement looks closer to a costless identity practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_attention_allocation, empirical, 'Whether the opportunity-cost victim claim survives counterfactual allocation analysis.').

omega_variable(
    suspension_vs_tacit_abolition,
    'Is ''suspended'' a stable normative category, or does indefinite suspension functionally equal tacit abolition maintained by ritual remembrance?',
    'Examine whether the tradition treats the suspended precepts as live law (e.g., rulings that would apply upon restoration, liability language, continued novelty in the corpus) or as commemorated history; test whether any practical legal consequence still flows from their bindingness.',
    'If suspension is functionally abolition-with-remembrance, the extraction reading weakens — nothing is being withheld from anyone, and the apparatus is memorial rather than obligational. If the precepts remain live law awaiting conditions, the extraction reading strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suspension_vs_tacit_abolition, conceptual, 'Whether indefinite suspension is a live obligation state or a dignified name for discontinuation.').

omega_variable(
    restoration_conversion_question,
    'If the Temple were restored, would the study apparatus convert into performance support (extraction collapsing to coordination cost), or would new contests (priestly genealogy, animal-welfare objections, purity logistics) replace the current ones?',
    'Historical analogy from prior restorations (Second Temple resumption after the Babylonian exile) and from contemporary restoration-movement planning documents; neither is decisive.',
    'If conversion would be smooth, the arrangement''s extraction is contingent on impossibility and the constraint is transitional in nature; if new contests would emerge, the extraction is structural to the obligation form itself and persists across regimes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(restoration_conversion_question, empirical, 'Whether the measured extraction is impossibility-contingent or structural to the commandment form.').

omega_variable(
    suppression_structural_vs_internalized,
    'How much of the measured suppression is structural (curricular mandates, ordination gates, communal sanction) versus internalized (piety that equates neglect of any Torah portion with disrespect, identity fused with whole-Torah mastery)?',
    'Observe voluntary allocation in settings where structural requirements are relaxed (elective tracks, autonomous study societies): if scholars freed from requirements still allocate similarly, the internalized share is high; if allocation shifts promptly to living law, the structural share dominates.',
    'If internalization dominates, effective suppression exceeds the structural measure and persists after any curricular reform — reform alone would not release the attention. If structural mechanisms dominate, curricular change is sufficient remedy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Composition of suppression between external enforcement and fused identity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_commandment__performance_only, 70, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t70, sacrifice_commandment__performance_only, theater_ratio, 70, 0.2).
narrative_ontology:measurement_basis(sacr_tr_t70, observed).
narrative_ontology:measurement(sacr_tr_t250, sacrifice_commandment__performance_only, theater_ratio, 250, 0.3).
narrative_ontology:measurement_basis(sacr_tr_t250, observed).
narrative_ontology:measurement(sacr_tr_t500, sacrifice_commandment__performance_only, theater_ratio, 500, 0.38).
narrative_ontology:measurement_basis(sacr_tr_t500, observed).
narrative_ontology:measurement(sacr_tr_t800, sacrifice_commandment__performance_only, theater_ratio, 800, 0.44).
narrative_ontology:measurement_basis(sacr_tr_t800, observed).
narrative_ontology:measurement(sacr_tr_t1100, sacrifice_commandment__performance_only, theater_ratio, 1100, 0.5).
narrative_ontology:measurement_basis(sacr_tr_t1100, observed).
narrative_ontology:measurement(sacr_tr_t1400, sacrifice_commandment__performance_only, theater_ratio, 1400, 0.54).
narrative_ontology:measurement_basis(sacr_tr_t1400, observed).
narrative_ontology:measurement(sacr_tr_t1700, sacrifice_commandment__performance_only, theater_ratio, 1700, 0.56).
narrative_ontology:measurement_basis(sacr_tr_t1700, observed).
narrative_ontology:measurement(sacr_tr_t2026, sacrifice_commandment__performance_only, theater_ratio, 2026, 0.52).
narrative_ontology:measurement_basis(sacr_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(sacr_be_t70, sacrifice_commandment__performance_only, base_extractiveness, 70, 0.45).
narrative_ontology:measurement_basis(sacr_be_t70, observed).
narrative_ontology:measurement(sacr_be_t250, sacrifice_commandment__performance_only, base_extractiveness, 250, 0.58).
narrative_ontology:measurement_basis(sacr_be_t250, observed).
narrative_ontology:measurement(sacr_be_t500, sacrifice_commandment__performance_only, base_extractiveness, 500, 0.66).
narrative_ontology:measurement_basis(sacr_be_t500, observed).
narrative_ontology:measurement(sacr_be_t800, sacrifice_commandment__performance_only, base_extractiveness, 800, 0.72).
narrative_ontology:measurement_basis(sacr_be_t800, observed).
narrative_ontology:measurement(sacr_be_t1100, sacrifice_commandment__performance_only, base_extractiveness, 1100, 0.76).
narrative_ontology:measurement_basis(sacr_be_t1100, observed).
narrative_ontology:measurement(sacr_be_t1400, sacrifice_commandment__performance_only, base_extractiveness, 1400, 0.78).
narrative_ontology:measurement_basis(sacr_be_t1400, observed).
narrative_ontology:measurement(sacr_be_t1700, sacrifice_commandment__performance_only, base_extractiveness, 1700, 0.77).
narrative_ontology:measurement_basis(sacr_be_t1700, observed).
narrative_ontology:measurement(sacr_be_t2026, sacrifice_commandment__performance_only, base_extractiveness, 2026, 0.78).
narrative_ontology:measurement_basis(sacr_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t70, sacrifice_commandment__performance_only, suppression_requirement, 70, 0.35).
narrative_ontology:measurement_basis(sacr_su_t70, observed).
narrative_ontology:measurement(sacr_su_t250, sacrifice_commandment__performance_only, suppression_requirement, 250, 0.55).
narrative_ontology:measurement_basis(sacr_su_t250, observed).
narrative_ontology:measurement(sacr_su_t500, sacrifice_commandment__performance_only, suppression_requirement, 500, 0.68).
narrative_ontology:measurement_basis(sacr_su_t500, observed).
narrative_ontology:measurement(sacr_su_t800, sacrifice_commandment__performance_only, suppression_requirement, 800, 0.74).
narrative_ontology:measurement_basis(sacr_su_t800, observed).
narrative_ontology:measurement(sacr_su_t1100, sacrifice_commandment__performance_only, suppression_requirement, 1100, 0.76).
narrative_ontology:measurement_basis(sacr_su_t1100, observed).
narrative_ontology:measurement(sacr_su_t1400, sacrifice_commandment__performance_only, suppression_requirement, 1400, 0.75).
narrative_ontology:measurement_basis(sacr_su_t1400, observed).
narrative_ontology:measurement(sacr_su_t1700, sacrifice_commandment__performance_only, suppression_requirement, 1700, 0.72).
narrative_ontology:measurement_basis(sacr_su_t1700, observed).
narrative_ontology:measurement(sacr_su_t2026, sacrifice_commandment__performance_only, suppression_requirement, 2026, 0.65).
narrative_ontology:measurement_basis(sacr_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_commandment__performance_only, identity_coordination).
narrative_ontology:affects_constraint(sacrifice_commandment__performance_only, sacrifice_commandment__study_as_performance).
narrative_ontology:affects_constraint(sacrifice_commandment__performance_only, sacrifice_commandment__archive_maintenance).

% DUAL FORMULATION NOTE:
% The colloquial label 'the sacrificial commandments after the destruction' decomposes into three constraint stories per the epsilon-invariance principle: performance_only (this story — execution constitutive, precepts suspended, high epsilon over nineteen centuries of undischarged labor), study_as_performance (study itself discharges — low epsilon, the labor becomes fulfillment), and archive_maintenance (study preserves knowledge for restoration — moderate epsilon, instrumental preparation with an implicit sunset at rebuilding). Each has its own stable epsilon, beneficiary/victim structure, and classification; they are linked here and in their own files. Upstream/downstream: the performance_only reading is textually prior and is cited by the other two as the baseline from which their substitutions deviate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sacrifice_commandment__performance_only, organized, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
