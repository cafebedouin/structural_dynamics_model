% ============================================================================
% CONSTRAINT STORY: personhood_boundary_kernel_flat_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_personhood_boundary_kernel_flat_control, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:flat_control_of/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: personhood_boundary_kernel_flat_control
 *   human_readable: The Determinate Personhood Threshold Commitment
 *   domain: moral_philosophy/metaethics/law
 *
 * SUMMARY:
 *   This story treats as ONE constraint the shared background commitment —
 *   held across the political spectrum, by courts, legislatures, and much of
 *   lay moral discourse — that there is a single, fixed, discoverable fact
 *   about when personhood/rights-holder status begins, from which the
 *   permissibility of abortion can be mechanically derived. This is
 *   deliberately authored flat: it is not decomposed into a
 *   conception-reading, a viability-reading, a birth-reading, or a
 *   gradualist-reading as separate constraints. Those readings, where they
 *   exist, would be siblings under a kernel; here the substrate itself — the
 *   commitment that SOME such fixed fact exists and settles the matter — is
 *   the single object under analysis. The contestation that would otherwise
 *   be distributed across reading-siblings is instead carried by perspectival
 *   divergence across stakeholder seats (state authorities vs. pregnant
 *   people vs. philosophers) and by omega variables naming the open
 *   metaethical questions the commitment leaves unresolved.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(personhood_boundary_kernel_flat_control, 0.58).
domain_priors:suppression_score(personhood_boundary_kernel_flat_control, 0.72).
domain_priors:theater_ratio(personhood_boundary_kernel_flat_control, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(personhood_boundary_kernel_flat_control, extractiveness, 0.58).
narrative_ontology:constraint_metric(personhood_boundary_kernel_flat_control, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(personhood_boundary_kernel_flat_control, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(personhood_boundary_kernel_flat_control, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(personhood_boundary_kernel_flat_control, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(personhood_boundary_kernel_flat_control, tangled_rope).
narrative_ontology:human_readable(personhood_boundary_kernel_flat_control, "The Determinate Personhood Threshold Commitment").
narrative_ontology:topic_domain(personhood_boundary_kernel_flat_control, "moral_philosophy/metaethics/law").

domain_priors:requires_active_enforcement(personhood_boundary_kernel_flat_control).

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(personhood_boundary_kernel_flat_control, personhood_boundary_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(personhood_boundary_kernel_flat_control, state_prosecutorial_authorities).
narrative_ontology:constraint_beneficiary(personhood_boundary_kernel_flat_control, advocacy_organizations_on_both_sides).
narrative_ontology:constraint_beneficiary(personhood_boundary_kernel_flat_control, religious_institutions_with_doctrinal_stakes).
narrative_ontology:constraint_beneficiary(personhood_boundary_kernel_flat_control, legal_professionals_specializing_in_reproductive_law).
narrative_ontology:constraint_victim(personhood_boundary_kernel_flat_control, pregnant_people_facing_criminal_liability).
narrative_ontology:constraint_victim(personhood_boundary_kernel_flat_control, clinicians_providing_reproductive_care).
narrative_ontology:constraint_victim(personhood_boundary_kernel_flat_control, people_seeking_time_sensitive_reproductive_healthcare).
narrative_ontology:constraint_vindicates(personhood_boundary_kernel_flat_control, moral_status_is_binary_and_threshold_based).
narrative_ontology:constraint_vindicates(personhood_boundary_kernel_flat_control, legal_permissibility_must_track_a_single_metaphysical_fact).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers and enforces whichever determinate threshold the jurisdiction has adopted (conception, viability, birth, or some statutory line), prosecuting or declining to prosecute based on where the line is drawn. Its authority depends on the premise that a single correct line exists to be enforced, rather than on the line being a contested policy choice; it collects legitimacy and enforcement power from the appearance that its rulings track a fact rather than a decision.
narrative_ontology:constraint_stakeholder(personhood_boundary_kernel_flat_control, state_prosecutorial_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Bears direct legal and physical consequences of wherever the threshold is currently drawn in their jurisdiction, including criminal liability, forced continuation of pregnancy, or loss of access to care, depending on the line's location and enforcement intensity. Cannot exit the jurisdiction's threshold-commitment without geographic relocation, which is often financially or logistically foreclosed.
narrative_ontology:constraint_stakeholder(personhood_boundary_kernel_flat_control, pregnant_people_facing_criminal_liability, payer,
    powerless, immediate, trapped, national).

% Must calibrate every clinical decision against a legally enforced threshold whose philosophical determinacy they may not believe in, facing licensure loss, civil liability, or criminal prosecution if their judgment about a borderline case diverges from the state's chosen line. Exit means leaving the profession or relocating practice to a different threshold jurisdiction.
narrative_ontology:constraint_stakeholder(personhood_boundary_kernel_flat_control, clinicians_providing_reproductive_care, payer,
    moderate, biographical, constrained, national).

% Raises funds, builds membership, and secures political influence by campaigning for their preferred determinate line (conception, viability, birth) as though it were the discoverable moral fact rather than one framing among several. Both pro-life and pro-choice organizational infrastructures derive resources and durability from the premise that a single correct answer exists and is currently being gotten wrong by the other side.
narrative_ontology:constraint_stakeholder(personhood_boundary_kernel_flat_control, advocacy_organizations_on_both_sides, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(personhood_boundary_kernel_flat_control, advocacy_organizations_on_both_sides, agenda_setter).

% Derives doctrinal authority and moral-teaching legitimacy from asserting a specific, fixed answer to the personhood question (typically conception), which in turn grounds broader claims to moral expertise on adjacent questions. The institution's continued relevance in public moral discourse partly depends on the boundary being treated as a determinate fact its tradition has correctly identified.
narrative_ontology:constraint_stakeholder(personhood_boundary_kernel_flat_control, religious_institutions_with_doctrinal_stakes, beneficiary,
    institutional, civilizational, analytical, global).

% Builds career specialization and billable expertise around litigating and interpreting exactly where the legally operative threshold sits and how borderline cases should be resolved. Their professional value is partly a function of the boundary's contestedness and its treatment as a matter for adjudication rather than acknowledged indeterminacy.
narrative_ontology:constraint_stakeholder(personhood_boundary_kernel_flat_control, legal_professionals_specializing_in_reproductive_law, beneficiary,
    moderate, biographical, mobile, national).

% Needs timely medical decisions, but timeliness itself is hostage to litigating or legislating where the determinate line falls; delay imposed by the search for or defense of a fixed answer can convert a low-risk procedure into a high-risk one. Has essentially no capacity to exit the jurisdiction's chosen threshold under time pressure.
narrative_ontology:constraint_stakeholder(personhood_boundary_kernel_flat_control, people_seeking_time_sensitive_reproductive_healthcare, payer,
    powerless, immediate, trapped, national).

% Studies the structure of the personhood debate itself, including the widespread disagreement among serious moral theories (capacity-based, potentiality-based, relational, gradualist) about whether a sharp threshold exists at all. Notes that gradualist and vague-boundary positions are live, well-defended options in the literature, which is in tension with the political and legal system's operational requirement for a single bright line.
narrative_ontology:constraint_stakeholder(personhood_boundary_kernel_flat_control, moral_philosophers_and_bioethicists, observer,
    analytical, civilizational, analytical, global).

% Holds that moral status plausibly increases gradually and that no non-arbitrary sharp line exists, a position that would dissolve the coordinating premise both major political camps rely on. This position is intellectually live but structurally excluded from legal and legislative debate, which requires a determinate line for statutes and court rulings to operate at all.
narrative_ontology:constraint_stakeholder(personhood_boundary_kernel_flat_control, gradualist_and_indeterminacy_theorists, excluded,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides courts, legislatures, and clinical practice with a single operational line so that criminal law, medical protocols, and insurance systems can function without adjudicating metaphysics case by case; without SOME operational line, the legal system cannot process abortion-related cases at all.
% TRANSFER_FUNCTION: Moves decisional burden, criminal and civil liability, and access to timely care from the state (which would otherwise have to build a system tolerant of moral indeterminacy) onto pregnant people and clinicians, who must comply with whichever line is currently enforced regardless of their own considered views; moves political and financial resources toward advocacy organizations and doctrinal institutions that campaign on the premise of a discoverable fact.
% ABSENT_VOICES: Gradualist and indeterminacy theorists, who represent a substantial strand of serious moral philosophy, are structurally excluded from legislative and judicial process because those processes require operational determinacy; their exclusion is not incidental but load-bearing, since a legal system built on acknowledged gradualism could not easily assign binary criminal liability.
% DISAPPEARANCE_RATIONALE: If the shared commitment to a fixed determinate answer vanished — replaced by open acknowledgment that moral status admits of degrees or is irreducibly contested — criminal abortion statutes premised on bright-line personhood would lose their metaphysical warrant, advocacy organizations on both sides would lose a central organizing premise, doctrinal claims to settled moral authority would weaken, and legal systems would likely shift toward graduated, context-sensitive frameworks (as some jurisdictions already approximate via gestational-limit schemes that function pragmatically without claiming metaphysical certainty).
% FOUNDING_PROBLEM: Legal and moral systems needed *some* way to adjudicate competing claims about fetal and maternal interests, and adjudication historically required a determinate rule rather than an acknowledged spectrum, since criminal law in particular demands bright lines for liability.
% FOUNDING_PROBLEM_CORROBORATION: Practicing bioethicists and philosophers of law outside the advocacy apparatus (writing in venues not funded by either pro-life or pro-choice organizational infrastructure) attest that the demand for a single determinate line is a legal-administrative necessity rather than a demonstrated metaphysical discovery; no independent party outside the beneficiary set — neither advocacy organizations nor doctrinal institutions — corroborates that the specific line each favors has been established as fact rather than chosen as policy.
narrative_ontology:disappearance_verdict(personhood_boundary_kernel_flat_control, world_rearranges).
narrative_ontology:founding_problem_status(personhood_boundary_kernel_flat_control, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(personhood_boundary_kernel_flat_control, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-22',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(personhood_boundary_kernel_flat_control, 'none', 1).
narrative_ontology:epsilon_provenance(personhood_boundary_kernel_flat_control, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(personhood_boundary_kernel_flat_control_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(personhood_boundary_kernel_flat_control, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(personhood_boundary_kernel_flat_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects that the commitment does real coordinating work (courts and clinics need an operational rule to function) but also channels resources, legitimacy, and liability asymmetrically: advocacy organizations and doctrinal institutions gain durable political and financial infrastructure from treating the question as fact-like, while pregnant people and clinicians bear the liability and access costs of compliance with whichever line the state adopts. Suppression (0.72) is high because the legal system does not tolerate acknowledged indeterminacy — gradualist positions are philosophically live but administratively excluded, and dissent from the operative line carries real legal consequence, not merely social disapproval. Theater ratio (0.40) reflects that a meaningful share of the debate's intensity is invested in defending the DETERMINACY premise itself (that a fact-like answer exists to be found) rather than in resolving the underlying moral disagreement, since most serious positions in the literature (capacity views, potentiality views, relational views, gradualism) do not actually converge on a sharp threshold. Accessibility collapse is moderate-low (0.35) because, unlike a genuine natural law, alternative framings (gradualism, indeterminacy, capacities-based sliding scales) remain live and articulate in the philosophical literature even though they are excluded from legal operation. Resistance is high (0.85) because the commitment is actively and continuously contested by serious moral philosophy, by cross-jurisdictional legal variation, and by the empirical fact that societies disagree sharply and persistently about where the line should fall — a genuine natural law would not generate this much sustained, well-credentialed disagreement.
 *
 * PERSPECTIVAL GAP:
 *   From the state authority's seat, the commitment looks like a coordination mechanism enabling consistent, non-arbitrary law — a rope. From the pregnant person's seat under criminal threat, the same commitment looks like enforced extraction of compliance with a metaphysical claim they may reasonably reject — closer to a tangled rope or snare, depending on enforcement intensity. From the analytical philosopher's seat, the commitment looks like a piton-adjacent structure: an inherited framework (the demand for a bright line) that persists institutionally long after serious moral theory has moved toward acknowledging gradualism or irreducible indeterminacy as live, credible positions. The engine computing divergent per-seat classifications from these structural facts is the intended output — this story does not adjudicate which seat is 'right.'
 *
 * DIRECTIONALITY LOGIC:
 *   State prosecutorial authorities and doctrinal/advocacy institutions sit near the beneficiary end: they derive enforcement legitimacy, political resources, or moral authority from the premise that a determinate answer exists and that they have correctly identified (or are correctly pursuing) it. Pregnant people facing liability and people needing time-sensitive care sit at the target end: they are trapped by jurisdiction, bear the sharpest costs of wherever the line currently sits, and cannot exit the commitment itself (only, at high cost, the specific jurisdiction). Clinicians occupy an intermediate position — professionally constrained rather than fully trapped, since relocation of practice is possible but costly. Legal professionals specializing in this area are structural beneficiaries of the boundary's persistent contestedness, independent of which side of the substantive debate they take.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — legal systems needing an operational rule to assign criminal and civil liability — remains partially live (courts still need SOME rule to function), which prevents flatly calling this pure zombie mandatrophy. But the corroboration gap is significant: the specific determinacy claim (that a FACT, not a policy choice, is being tracked) is corroborated by no source outside the beneficiary set. This is the tangled-rope signature: real coordination function (operational rules are needed) coexisting with asymmetric extraction (the metaphysical framing that inflates political stakes and forecloses gradualist alternatives serves advocacy and doctrinal beneficiaries disproportionately) — not a pure snare (the coordination need is genuine) and not a pure rope (the extraction and suppression of alternatives are real and load-bearing).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sharp_threshold_metaphysical_fact_or_political_construction,
    'Is there in fact a determinate, discoverable moral fact about when rights-holder status begins, or is the demand for a sharp threshold itself a legal-administrative artifact imposed on an irreducibly gradual or contested moral phenomenon?',
    'This is likely not empirically resolvable through further data; it depends on which metaethical framework (moral realism with sharp natural kinds vs. anti-realism, vagueness theory, or gradualist moral-status views) is correct, which is itself contested among specialists with no consensus resolution mechanism in view.',
    'If a genuine sharp fact exists and is discoverable, the commitment functions closer to a mountain (an irreducible feature of moral reality that legal systems are merely trying to track) and current extraction/suppression readings would need revision downward. If no such fact exists, the commitment is closer to a tangled rope or snare wearing metaphysical justification as cover, and the extraction reading is conservative rather than overstated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sharp_threshold_metaphysical_fact_or_political_construction, conceptual, 'Core ambiguity: natural moral fact vs. constructed administrative necessity.').

omega_variable(
    gradualism_administrative_feasibility,
    'Could a legal system actually operate on an acknowledged gradualist or vague-boundary basis (e.g., graduated liability scaling with gestational stage, similar to some existing statutory schemes) without collapsing into unworkable case-by-case metaphysical adjudication?',
    'Comparative study of jurisdictions using gestational-limit or graduated frameworks rather than sharp personhood declarations, assessing whether they function coherently without invoking a determinate personhood fact.',
    'If graduated frameworks function well in practice, this weakens the claim that a sharp determinate line is a coordination necessity, shifting the classification toward snare/tangled_rope (the determinacy demand is less necessary than claimed). If graduated frameworks prove unworkable, the coordination function claim is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gradualism_administrative_feasibility, empirical, 'Whether the coordination function actually requires a bright line or merely a workable rule.').

omega_variable(
    suppression_internalized_or_structural,
    'For pregnant people and clinicians complying with the enforced threshold, is the suppression they experience primarily structural (criminal statute, licensure risk) or partly internalized (moral belief in the enforced line, shaped by upbringing, religious formation, or social environment, such that some comply not merely under duress but from genuine internalized conviction)?',
    'Survey and interview data distinguishing compliance motivated by legal threat from compliance motivated by genuinely held belief in the enforced threshold, ideally tracking whether stated beliefs shift when legal threat is removed (e.g., comparing behavior/attitudes across jurisdictions with differing enforcement intensity).',
    'If suppression is substantially internalized, effective suppression is higher than the structural measure alone suggests, since affected agents carry the constraint''s logic with them even absent enforcement; if primarily structural, removing legal threat would be sufficient to restore full agency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalized_or_structural, empirical, 'Structural vs. internalized suppression mechanism among those complying with the enforced threshold.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(personhood_boundary_kernel_flat_control, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pers_tr_t0, personhood_boundary_kernel_flat_control, theater_ratio, 0, 0.28).
narrative_ontology:measurement(pers_tr_t10, personhood_boundary_kernel_flat_control, theater_ratio, 10, 0.31).
narrative_ontology:measurement(pers_tr_t20, personhood_boundary_kernel_flat_control, theater_ratio, 20, 0.34).
narrative_ontology:measurement(pers_tr_t30, personhood_boundary_kernel_flat_control, theater_ratio, 30, 0.36).
narrative_ontology:measurement(pers_tr_t40, personhood_boundary_kernel_flat_control, theater_ratio, 40, 0.38).
narrative_ontology:measurement(pers_tr_t50, personhood_boundary_kernel_flat_control, theater_ratio, 50, 0.4).

% Extraction over time
narrative_ontology:measurement(pers_be_t0, personhood_boundary_kernel_flat_control, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(pers_be_t10, personhood_boundary_kernel_flat_control, base_extractiveness, 10, 0.47).
narrative_ontology:measurement(pers_be_t20, personhood_boundary_kernel_flat_control, base_extractiveness, 20, 0.51).
narrative_ontology:measurement(pers_be_t30, personhood_boundary_kernel_flat_control, base_extractiveness, 30, 0.55).
narrative_ontology:measurement(pers_be_t40, personhood_boundary_kernel_flat_control, base_extractiveness, 40, 0.57).
narrative_ontology:measurement(pers_be_t50, personhood_boundary_kernel_flat_control, base_extractiveness, 50, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(pers_su_t0, personhood_boundary_kernel_flat_control, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(pers_su_t10, personhood_boundary_kernel_flat_control, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(pers_su_t20, personhood_boundary_kernel_flat_control, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(pers_su_t30, personhood_boundary_kernel_flat_control, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(pers_su_t40, personhood_boundary_kernel_flat_control, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(pers_su_t50, personhood_boundary_kernel_flat_control, suppression_requirement, 50, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(personhood_boundary_kernel_flat_control, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This story is authored FLAT per the construction-perturbation control: it represents the substrate commitment as a single constraint, not decomposed into conception/viability/birth/gradualist reading-siblings. No affects_constraints links are declared because no sibling reading files exist in this construction; a companion kernel-decomposed version of this same substrate would link readings via affects_constraints and would carry cs_structure.reading_relations/axioms, which this flat control deliberately omits.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
