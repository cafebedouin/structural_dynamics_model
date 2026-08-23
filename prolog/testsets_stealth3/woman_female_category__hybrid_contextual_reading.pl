% ============================================================================
% CONSTRAINT STORY: woman_female_category__hybrid_contextual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_woman_female_category__hybrid_contextual_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: woman_female_category__hybrid_contextual_reading
 *   human_readable: Context-Partitioned Dual-Criteria Regime for the Woman/Female Category (Hybrid Contextual Reading)
 *   domain: political_philosophy/bioethics/gender_studies/law
 *
 * SUMMARY:
 *   This story instantiates the hybrid contextual reading of the contested
 *   woman/female-category kernel: membership is determined by biological sex
 *   in embodiment-governed contexts (clinical protocols, competitive sport
 *   categories, custodial and safety assignments) and by gender identity in
 *   recognition-governed contexts (documents, social recognition, most
 *   civil-status law). The arrangement is treated strictly as this reading's
 *   own constraint: a standing, enforced partition telling every classifier
 *   which criterion to apply in which room, sparing institutions the
 *   adjudication of the underlying dispute. Per the epsilon-invariance
 *   principle, the colloquial label decomposes into three structurally
 *   distinct constraints, this partition regime plus two universal regimes,
 *   linked by network edges; each carries its own epsilon, beneficiaries, and
 *   victims, and no hedging across readings occurs here. The epsilon authored
 *   below refers to the standing partition arrangement as this reading itself
 *   assesses it: losses distributed across constituencies rather than
 *   concentrated, moderate in aggregate. Claim and metrics are independent
 *   authored facts: the type claimed is what this reading believes
 *   structurally true of the arrangement, the metrics are what it believes
 *   descriptively true of its operation, and the engine computes per-seat
 *   classifications from the structural data. KEY AGENTS (by structural
 *   relationship): - institutional_classifiers: Agenda-setter and primary
 *   beneficiary (institutional/arbitrage) — administers allocations, collects
 *   conflict-avoidance - binary_aligned_individuals: Stability beneficiary
 *   (moderate/mobile) — sorted identically under either criterion -
 *   issue_advocacy_organizations: Secondary beneficiary of sustained contest
 *   (organized/mobile) - trans_people: Payer in embodiment-governed rooms
 *   (moderate/constrained) — partial winner in recognition-governed rooms -
 *   sex_based_rights_advocates: Payer in recognition-governed rooms
 *   (organized/constrained) — partial winner in embodiment-governed rooms -
 *   intersex_people: Acute payer with no resolving room (powerless/trapped) -
 *   nonbinary_people: Excluded voice — presupposed out by the partition's
 *   binarity (powerless/trapped) - bioethics_law_scholars: Analytical
 *   observer (analytical/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_female_category__hybrid_contextual_reading, 0.52).
domain_priors:suppression_score(woman_female_category__hybrid_contextual_reading, 0.6).
domain_priors:theater_ratio(woman_female_category__hybrid_contextual_reading, 0.36).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, theater_ratio, 0.36).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_female_category__hybrid_contextual_reading, tangled_rope).
narrative_ontology:human_readable(woman_female_category__hybrid_contextual_reading, "Context-Partitioned Dual-Criteria Regime for the Woman/Female Category (Hybrid Contextual Reading)").
narrative_ontology:topic_domain(woman_female_category__hybrid_contextual_reading, "political_philosophy/bioethics/gender_studies/law").

domain_priors:requires_active_enforcement(woman_female_category__hybrid_contextual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_female_category__hybrid_contextual_reading, '6405fa2e-2794-450b-89c3-0518ff9d1830').
narrative_ontology:cs_kernel_codification('6405fa2e-2794-450b-89c3-0518ff9d1830', distributed).
narrative_ontology:cs_authority_grounding('6405fa2e-2794-450b-89c3-0518ff9d1830', distributed).
narrative_ontology:cs_reading_relation('6405fa2e-2794-450b-89c3-0518ff9d1830', woman_female_category__sex_biology_reading, coexists_with).
narrative_ontology:cs_reading_relation('6405fa2e-2794-450b-89c3-0518ff9d1830', woman_female_category__gender_identity_reading, coexists_with).
narrative_ontology:cs_axiom('6405fa2e-2794-450b-89c3-0518ff9d1830', foundational, context_function_determines_category_criteria).
narrative_ontology:cs_axiom_status(context_function_determines_category_criteria, holdable).
narrative_ontology:cs_axiom_grounding('6405fa2e-2794-450b-89c3-0518ff9d1830', context_function_determines_category_criteria, instrumental).
narrative_ontology:cs_axiom('6405fa2e-2794-450b-89c3-0518ff9d1830', secondary, plural_criteria_avoid_adjudication_stalemate).
narrative_ontology:cs_axiom_status(plural_criteria_avoid_adjudication_stalemate, holdable).
narrative_ontology:cs_axiom_grounding('6405fa2e-2794-450b-89c3-0518ff9d1830', plural_criteria_avoid_adjudication_stalemate, conventional).
narrative_ontology:cs_reference_frame('6405fa2e-2794-450b-89c3-0518ff9d1830', context_partitioned_dual_criteria).
narrative_ontology:cs_drift_state('6405fa2e-2794-450b-89c3-0518ff9d1830', contemporary_culture_war_intensification, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('6405fa2e-2794-450b-89c3-0518ff9d1830', '').
narrative_ontology:cs_kernel_id(woman_female_category__hybrid_contextual_reading, woman_female_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_female_category__hybrid_contextual_reading, institutional_classifiers).
narrative_ontology:constraint_beneficiary(woman_female_category__hybrid_contextual_reading, binary_aligned_individuals).
narrative_ontology:constraint_beneficiary(woman_female_category__hybrid_contextual_reading, issue_advocacy_organizations).
narrative_ontology:constraint_victim(woman_female_category__hybrid_contextual_reading, trans_people).
narrative_ontology:constraint_victim(woman_female_category__hybrid_contextual_reading, sex_based_rights_advocates).
narrative_ontology:constraint_victim(woman_female_category__hybrid_contextual_reading, intersex_people).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Run the classification machinery: sporting federations set eligibility rules, health systems choose which marker enters which record, registries decide what documents assert, courts adjudicate challenges to the allocations. Each published allocation buys the institution freedom from adjudicating the underlying dispute, and the class can shift allocations between contexts as litigation and public pressure demand. Their exposure is reputational and legal, not categorical: no rule they administer reclassifies them.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, institutional_classifiers, agenda_setter,
    institutional, generational, arbitrage, global).

% People whose bodily development and lived identity point the same way under either criterion. Whichever rule a given room applies, they are sorted identically; they carry essentially none of the boundary cost and receive whatever stability the arrangement provides. Their exit from any particular institution is ordinary mobility.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, binary_aligned_individuals, beneficiary,
    moderate, biographical, mobile, global).

% Campaign organizations on both flanks whose funding, membership, and media presence are renewed by each new allocation fight. Partial victories in some rooms and defeats in others keep both the grievance and the donor cycle alive; a final settlement of the underlying dispute would retire their core product.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, issue_advocacy_organizations, beneficiary,
    organized, biographical, mobile, national).

% Live on the losing side of whichever room applies the embodiment criterion: eligibility screening in sport, custodial assignment, and some medical protocols that assume binary developmental categories. In identity-governed rooms the same people obtain documents and social recognition aligned with identity. They cannot stop being classified; withdrawing from any single institution removes that exposure but not the system-wide sorting. Legal and mutual-aid networks exist and litigate, with reach varying sharply by jurisdiction.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, trans_people, payer,
    moderate, biographical, constrained, global).

% Campaign for embodiment-based classification to hold everywhere: single-sex provisions, sex-disaggregated statistics, forensic and clinical categories tied to developmental biology. Each identity-governed adoption erodes the data or provision they defend; each embodiment-governed allocation is a partial win. Like the mirror constituency, they cannot exit the classification system, and their strongest redoubts are precisely the rooms the identity side contests hardest.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, sex_based_rights_advocates, payer,
    organized, generational, constrained, national).

% Born with developmental variation that neither binary criterion cleanly sorts. In embodiment-governed rooms they face forced or early assignment, including infant intervention, eligibility panel judgment, and administrator-chosen document markers. Identity-governed rooms change little for them, since the binary paperwork persists regardless. No room in the arrangement resolves their classification rather than assigning it.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, intersex_people, payer,
    powerless, generational, trapped, global).

% Fall outside the two-term partition the entire arrangement presupposes; every allocation rule on offer sorts them badly or not at all. They are rarely seated in the consultations where context boundaries are drawn, though those boundaries determine what they are called in every room.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, nonbinary_people, excluded,
    powerless, biographical, trapped, global).

% Analyze the allocation regime from outside it: map which criterion governs which room, trace litigation patterns, compare jurisdictions. Published assessments of coherence and fairness feed back into court opinions and legislative review. Hold no stake in any particular sorting.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, bioethics_law_scholars, observer,
    analytical, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(woman_female_category__hybrid_contextual_reading, institutional_classifiers).
narrative_ontology:fixing_cost_class(woman_female_category__hybrid_contextual_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives every classifier a workable answer without settling the kernel dispute: each domain runs the criterion its function appears to require, and contest energy is absorbed by giving each constituency wins in some rooms. Records, rosters, and registries keep operating while the culture fights.
% TRANSFER_FUNCTION: Moves category access and definitional authority room by room. Embodiment-governed rooms transfer recognition away from trans people and assign intersex people administratively; identity-governed rooms transfer recognition toward declared identity and away from sex-based provisions and statistics. Adjudication costs move from institutions to the constituencies, who litigate allocation by allocation.
% ABSENT_VOICES: Nonbinary people are presupposed out by the binarity the partition enforces and rarely seated where boundaries are drawn. Intersex people are decided about in eligibility panels and clinical protocols they seldom help set. Context-spanning individuals, such as trans athletes and detained people, have their treatment fixed in rooms they enter only as subjects. They are outside consultation structures and appear mainly as litigants after rules ship.
% DISAPPEARANCE_RATIONALE: If the partition vanished overnight, every registry, federation, clinic, and custodial system must immediately adopt a single criterion or adjudicate case by case. Documents, eligibility rules, and data pipelines churn; both flanks escalate to impose their universal rule on the vacated rooms; sport eligibility, custody assignment, and statistical collection are contested in every jurisdiction simultaneously.
% FOUNDING_PROBLEM: Institutions needed operable classification rules during a period when two universal definitional regimes had made the category politically unadjudicable; the partition was built so administration could proceed while the wider dispute burned without resolution.
% FOUNDING_PROBLEM_CORROBORATION: No one outside the contest attests the founding problem is solved. Appellate opinions in several jurisdictions observe that the arrangement now generates the very disputes it was built to contain; peer-reviewed bioethics and legal scholarship documents the shift from containment to contest-generation; and both payer constituencies independently attest that the problem has mutated rather than abated. The institutional beneficiaries alone attest that the original containment function still works.
narrative_ontology:disappearance_verdict(woman_female_category__hybrid_contextual_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_female_category__hybrid_contextual_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_female_category__hybrid_contextual_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(woman_female_category__hybrid_contextual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_female_category__hybrid_contextual_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_female_category__hybrid_contextual_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(woman_female_category__hybrid_contextual_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(woman_female_category__hybrid_contextual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.52 at interval end) because the partition distributes rather than concentrates loss: each constituency concedes rooms, and no seat captures anything like the full surplus, but both payer constituencies and intersex people bear real, recurring costs, and the burden of adjudication is shifted onto them. Suppression (0.60) reflects the enforcement the partition needs, eligibility screening, document-marker regimes, protocol defaults, plus a distinctly epistemic mode: the arrangement suppresses the kernel question itself by ensuring no institution ever has to answer it. Suppression is authored as a raw structural property; the engine scales only extractiveness by directionality and scope. Theater ratio (0.36) is moderate and rising: balancing statements, framework documents, and consultation exercises proliferate faster than substantive re-adjudication, a Goodhart signature worth watching, though core enforcement remains functional rather than ceremonial. Accessibility collapse is low (0.30) because the alternatives, either universal sibling reading, or case-by-case adjudication, remain fully visible and actively pursued; nothing about understanding the partition forecloses exit routes. Resistance is high (0.74): both flanks contest allocations continuously through litigation, legislation, and professional-body campaigns, which is precisely why enforcement requirements climb over the interval. The three measurement series run on one shared time grid (t = 0, 3, 6, 9, 12, 15) with every metric authored at every point; trajectories are monotonic hardening, not cyclical, reflecting accumulation of allocations, enforcement infrastructure, and symbolic maintenance rather than oscillation.
 *
 * PERSPECTIVAL GAP:
 *   Seats should compute differently. From the agenda_setter seat the arrangement is managed pluralism it built and can steer: low effective extraction, since the constraint subsidizes the institution with peace. From the trans_people seat the salient fact is that the rooms governing their highest-stakes exposures run on a criterion that excludes them, so the same structure computes as exclusionary machinery despite their wins elsewhere. From the sex_based_rights_advocate seat the mirror image holds: recognition-governed rooms read as erosion of provisions and data they are obligated to defend. This is also the same-level lateral case: two organized constituencies of comparable nominal power experience the constraint in opposed directions, differentiated not by global standing but by which criterion governs the rooms salient to their lives, and by which rooms they can afford to litigate in. On identity-lock: trans_people are authored constrained rather than identity_locked because exposure is imposed by external classification that no one can decline; identity fusion raises the stakes of the contest and explains its persistence, but exit from the classification system is unavailable to every seat alike, fused or not. The engine computes these divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive the derivation. institutional_classifiers collect the arrangement's peace and hold arbitrage-grade exit, the ability to re-allocate contexts under pressure, placing them nearest the beneficiary end. binary_aligned_individuals are subsidized by stability and mobile, sitting at or beyond symmetry on the beneficiary side. issue_advocacy_organizations draw sustaining returns from perpetual contest. trans_people and sex_based_rights_advocates are declared victims and derive high directionalities tempered by their partial wins in the opposite context, landing them mid-to-high rather than near-full-target; intersex_people, trapped with no offsetting room, sit nearest the full-target end of any seat. No directionality overrides are authored: the schema keys overrides by power atom, and this story's conflicted atoms would misfire, since the moderate atom hosts both a beneficiary (binary_aligned_individuals) and a payer (trans_people), and the organized atom likewise hosts a beneficiary and a payer. Blanket atom-level correction would corrupt one seat to fix another, so the derivation stands with its residual imprecision documented here.
 *
 * MANDATROPHY ANALYSIS:
 *   Reading the partition as pure extraction would erase the genuine coordination it performs: institutions do need operable rules, and the binary-aligned majority genuinely receives stable, consistent classification. Reading it as pure coordination would erase the distributed extraction: both constituencies pay in the rooms that matter most to them, intersex people are assigned rather than served, and the enforcement bill grows yearly. The tangled_rope claim preserves both halves, coordination function plus asymmetric, actively enforced extraction. Mandatrophy is not declared resolved: the founding problem's status is contested, and the mismatch-relevant signal is that institutions attest containment while every outside seat attests mutation. The rising theater ratio tracks drift toward symbolic balancing; if allocation boundaries continue migrating while enforcement becomes increasingly performative, the structure carries piton risk in a later interval, an administrator-maintained shell whose fixing cost has grown prohibitive relative to any seat's willingness to bear the resulting culture war. The prohibitive fixing-cost cell combined with a named capturer seat keeps this on the captured side of the receipt surface rather than the neglect side.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading of kernel woman_female_category. Which determination rule governs the category, and how would adopting either universal sibling restructure this constraint''s victim set?',
    'Political or legal resolution of the kernel dispute, or cross-story comparison once the two sibling stories (woman_female_category__sex_biology_reading, woman_female_category__gender_identity_reading) are compiled with their own epsilon values and stakeholder surfaces.',
    'Adopting the biological reading moves trans_people into the victim set in every context and collapses the distributed loss profile into concentrated exclusion; adopting the identity reading moves sex_based_rights_advocates into the victim set everywhere. The moderate distributed extraction authored here exists only under the partition; the location of the disagreement is the scope quantifier, universal versus context-relative determination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: this story instantiates the hybrid contextual reading of a three-reading kernel; sibling readings restructure the victim set wholesale.').

omega_variable(
    partition_boundary_migration,
    'Are the context boundaries of the partition stable, or does each allocation migrate, with fairness-of-contest logic creeping into custody and medicine, and recognition logic creeping into eligibility?',
    'Longitudinal coding of allocation decisions across jurisdictions, tracking which rooms each criterion governs over time and which flank''s arguments recur in adjacent rooms.',
    'Boundary migration converts the balanced-loss profile into progressive capture by whichever flank wins neighboring rooms, drifting practice toward one universal reading without any formal adoption of it; the victim set would concentrate accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_boundary_migration, empirical, 'Whether the load-bearing partition itself drifts, shifting who pays.').

omega_variable(
    intersex_assignment_unresolved,
    'Can any binary criterion govern intersex people in embodiment-governed contexts without arbitrary or coercive assignment?',
    'Clinical-ethics outcome data comparing deferred-assignment protocols with current practice, plus litigation records on eligibility panels and document-marker defaults.',
    'If no such governance is possible, the partition''s coordination claim fails outright for intersex people: their extraction is unconditional rather than context-bound, and they belong in the victim set of every context rather than half of them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intersex_assignment_unresolved, empirical, 'Whether the hybrid leaves intersex classification permanently assigned rather than resolved.').

omega_variable(
    acquiescence_structure,
    'Is constituency acquiescence in the rooms where their reading is subordinated structural, reflecting lost leverage there, or internalized, reflecting accepted conditional belonging?',
    'Attitude tracking among affected constituencies before and after favorable rulings elsewhere: if subordination in one room is rapidly repriced after wins in another, the acquiescence was structural; if it persists despite changed leverage, an internalized component is carrying it.',
    'An internalized component raises durable suppression above what the structural measure captures, since targets carry the subordination across rooms; a purely structural component predicts fast reversal when allocations flip.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(acquiescence_structure, empirical, 'Structural versus internalized component of constituency acquiescence under the partition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_female_category__hybrid_contextual_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wfc_hybrid_ctx_tr_t0, woman_female_category__hybrid_contextual_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(wfc_hybrid_ctx_tr_t3, woman_female_category__hybrid_contextual_reading, theater_ratio, 3, 0.22).
narrative_ontology:measurement(wfc_hybrid_ctx_tr_t6, woman_female_category__hybrid_contextual_reading, theater_ratio, 6, 0.26).
narrative_ontology:measurement(wfc_hybrid_ctx_tr_t9, woman_female_category__hybrid_contextual_reading, theater_ratio, 9, 0.3).
narrative_ontology:measurement(wfc_hybrid_ctx_tr_t12, woman_female_category__hybrid_contextual_reading, theater_ratio, 12, 0.33).
narrative_ontology:measurement(wfc_hybrid_ctx_tr_t15, woman_female_category__hybrid_contextual_reading, theater_ratio, 15, 0.36).

% Extraction over time
narrative_ontology:measurement(wfc_hybrid_ctx_be_t0, woman_female_category__hybrid_contextual_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(wfc_hybrid_ctx_be_t3, woman_female_category__hybrid_contextual_reading, base_extractiveness, 3, 0.38).
narrative_ontology:measurement(wfc_hybrid_ctx_be_t6, woman_female_category__hybrid_contextual_reading, base_extractiveness, 6, 0.43).
narrative_ontology:measurement(wfc_hybrid_ctx_be_t9, woman_female_category__hybrid_contextual_reading, base_extractiveness, 9, 0.47).
narrative_ontology:measurement(wfc_hybrid_ctx_be_t12, woman_female_category__hybrid_contextual_reading, base_extractiveness, 12, 0.5).
narrative_ontology:measurement(wfc_hybrid_ctx_be_t15, woman_female_category__hybrid_contextual_reading, base_extractiveness, 15, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(wfc_hybrid_ctx_su_t0, woman_female_category__hybrid_contextual_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(wfc_hybrid_ctx_su_t3, woman_female_category__hybrid_contextual_reading, suppression_requirement, 3, 0.45).
narrative_ontology:measurement(wfc_hybrid_ctx_su_t6, woman_female_category__hybrid_contextual_reading, suppression_requirement, 6, 0.5).
narrative_ontology:measurement(wfc_hybrid_ctx_su_t9, woman_female_category__hybrid_contextual_reading, suppression_requirement, 9, 0.54).
narrative_ontology:measurement(wfc_hybrid_ctx_su_t12, woman_female_category__hybrid_contextual_reading, suppression_requirement, 12, 0.58).
narrative_ontology:measurement(wfc_hybrid_ctx_su_t15, woman_female_category__hybrid_contextual_reading, suppression_requirement, 15, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_female_category__hybrid_contextual_reading, identity_coordination).
narrative_ontology:affects_constraint(woman_female_category__hybrid_contextual_reading, woman_female_category__sex_biology_reading).
narrative_ontology:affects_constraint(woman_female_category__hybrid_contextual_reading, woman_female_category__gender_identity_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of the colloquial label 'the definition of woman/female'. The single natural-language concept covers three structurally distinct claims with distinct epsilon values: universal biological determination (low contest inside its own frame, concentrated victim set), universal identity determination (mirror-image structure), and this context-partitioned regime (distributed victims, moderate extraction, enforcement-intensive). The upstream siblings supply the criteria this reading allocates; this reading influences both by partially realizing each as a sub-rule while blocking either from becoming universal. Each story links the others via network edges per the epsilon-invariance principle; no single story hedges across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
