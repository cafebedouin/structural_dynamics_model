% ============================================================================
% CONSTRAINT STORY: constitutional_authority_boundary__parliamentary_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_authority_boundary__parliamentary_primacy_reading, []).

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
 *   constraint_id: constitutional_authority_boundary__parliamentary_primacy_reading
 *   human_readable: Parliamentary Primacy: Legislative Final Authority over Constitutional Meaning
 *   domain: constitutional law/political philosophy/institutional design
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel
 *   'constitutional authority boundary': the parliamentary-primacy
 *   settlement, in which the constitutional text — where one exists at all —
 *   is subordinate to the elected legislature, which retains final authority
 *   to define constitutional meaning through ordinary or entrenched
 *   legislation. The arrangement is assessed here by its own lights, as the
 *   reading itself assesses it: a low-rent coordination settlement that vests
 *   final interpretive authority in the accountable branch. The claim/metric
 *   gap is deliberate and structural: the reading CLAIMS a functioning
 *   coordination rope, while the authored metrics describe its actual
 *   operation, including the real costs borne by the judiciary and by
 *   minorities whose protections lack entrenchment. Sibling readings
 *   (judicial supremacy; coordinate construction) are separate constraints
 *   with their own stories, linked through the network and through
 *   cs_structure.reading_relations; they are not folded into this file.
 *
 * KEY AGENTS:
 *   - elected_legislatures: Agenda-setter and primary beneficiary (institutional/arbitrage) — holds and administers final interpretive authority and could restructure the settlement at will
 *   - governing_parliamentary_majorities: Immediate beneficiary (powerful/mobile) — exercises the authority between elections and answers at the next one
 *   - constitutional_courts_judiciary: Primary target (institutional/identity_locked) — bears the settlement's principal cost: no binding final word on constitutional questions
 *   - unentrenched_rights_minorities: Secondary target (powerless/trapped) — carries contingent exposure to majoritarian definition of their protections
 *   - national_electorates: Net beneficiary with diffuse costs (organized/constrained) — holds the indirect lever through elections
 *   - opposition_parliamentarians: Rotating payer/beneficiary (organized/mobile) — outvoted today, potentially the pen-holder next term
 *   - entrenched_charter_advocates: Excluded voice (moderate/constrained) — would entrench rights beyond majority reach but sits outside the settlement's definition machinery
 *   - academic_constitutional_theorists: Analytical observer (analytical/analytical) — maps the settlement comparatively without bearing its costs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_authority_boundary__parliamentary_primacy_reading, 0.22).
domain_priors:suppression_score(constitutional_authority_boundary__parliamentary_primacy_reading, 0.28).
domain_priors:theater_ratio(constitutional_authority_boundary__parliamentary_primacy_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_authority_boundary__parliamentary_primacy_reading, rope).
narrative_ontology:human_readable(constitutional_authority_boundary__parliamentary_primacy_reading, "Parliamentary Primacy: Legislative Final Authority over Constitutional Meaning").
narrative_ontology:topic_domain(constitutional_authority_boundary__parliamentary_primacy_reading, "constitutional law/political philosophy/institutional design").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_authority_boundary__parliamentary_primacy_reading, '4e243714-7e6b-4a91-a04e-57eb19ed8d93').
narrative_ontology:cs_kernel_codification('4e243714-7e6b-4a91-a04e-57eb19ed8d93', implicit).
narrative_ontology:cs_authority_grounding('4e243714-7e6b-4a91-a04e-57eb19ed8d93', practice).
narrative_ontology:cs_interpretation_layer_present('4e243714-7e6b-4a91-a04e-57eb19ed8d93').
narrative_ontology:cs_reading_relation('4e243714-7e6b-4a91-a04e-57eb19ed8d93', constitutional_authority_boundary__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('4e243714-7e6b-4a91-a04e-57eb19ed8d93', constitutional_authority_boundary__coordinate_construction_reading, forecloses).
narrative_ontology:cs_axiom('4e243714-7e6b-4a91-a04e-57eb19ed8d93', foundational, elected_branch_holds_final_interpretive_authority).
narrative_ontology:cs_axiom_status(elected_branch_holds_final_interpretive_authority, holdable).
narrative_ontology:cs_axiom_grounding('4e243714-7e6b-4a91-a04e-57eb19ed8d93', elected_branch_holds_final_interpretive_authority, deontological).
narrative_ontology:cs_axiom('4e243714-7e6b-4a91-a04e-57eb19ed8d93', secondary, judicial_review_is_advisory_or_overridable).
narrative_ontology:cs_axiom_status(judicial_review_is_advisory_or_overridable, holdable).
narrative_ontology:cs_axiom_grounding('4e243714-7e6b-4a91-a04e-57eb19ed8d93', judicial_review_is_advisory_or_overridable, conventional).
narrative_ontology:cs_reference_frame('4e243714-7e6b-4a91-a04e-57eb19ed8d93', dicean_parliamentary_omnicompetence).
narrative_ontology:cs_drift_state('4e243714-7e6b-4a91-a04e-57eb19ed8d93', contemporary_rights_devolution_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('4e243714-7e6b-4a91-a04e-57eb19ed8d93', '').
narrative_ontology:cs_kernel_id(constitutional_authority_boundary__parliamentary_primacy_reading, constitutional_authority_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__parliamentary_primacy_reading, elected_legislatures).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__parliamentary_primacy_reading, governing_parliamentary_majorities).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__parliamentary_primacy_reading, national_electorates).
narrative_ontology:constraint_victim(constitutional_authority_boundary__parliamentary_primacy_reading, constitutional_courts_judiciary).
narrative_ontology:constraint_victim(constitutional_authority_boundary__parliamentary_primacy_reading, unentrenched_rights_minorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__parliamentary_primacy_reading, opposition_parliamentarians).
narrative_ontology:constraint_victim(constitutional_authority_boundary__parliamentary_primacy_reading, national_electorates).
narrative_ontology:constraint_victim(constitutional_authority_boundary__parliamentary_primacy_reading, opposition_parliamentarians).
narrative_ontology:constraint_vindicates(constitutional_authority_boundary__parliamentary_primacy_reading, parliamentary_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(constitutional_authority_boundary__parliamentary_primacy_reading, popular_sovereignty_through_representation).
narrative_ontology:constraint_vindicates(constitutional_authority_boundary__parliamentary_primacy_reading, political_constitutionalism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Makes and unmakes law, including law about constitutional meaning. Defines the procedures of its own authority, can amend or repeal any earlier statute, and answers to no external interpreter for the content of its enactments. Its finality is renewed each session by the simple act of legislating; it could restructure the entire settlement by ordinary statute if a majority wished.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, elected_legislatures, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(constitutional_authority_boundary__parliamentary_primacy_reading, elected_legislatures, beneficiary).

% Hold working control of the chamber for the life of one parliament. During that window they decide what the constitution means in practice — which rights statutes confer, what limits bind the executive — and their products cannot be struck down by any court. Their authority evaporates at the next election, which is also the mechanism by which they answer for how they used it.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, governing_parliamentary_majorities, beneficiary,
    powerful, biographical, mobile, national).

% Adjudicate ordinary disputes and interpret statutes, and may flag that legislation conflicts with constitutional principles or rights instruments, but their flags do not bind the chamber: a majority can legislate past any ruling. Senior judges publicly defend the settlement even while noting its edges. Leaving the role means leaving the bench; staying means administering a finality that belongs to someone else.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, constitutional_courts_judiciary, payer,
    institutional, generational, identity_locked, national).

% Depend for the protection of their rights on statutes that a future majority can amend or repeal by simple vote. They gain recourse through elections, lobbying, and the courts' interpretive leeway, but hold no guarantee that outranks the next parliament's priorities. Emigration is the only full exit from the jurisdiction's constitutional order, at prohibitive personal cost.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, unentrenched_rights_minorities, payer,
    powerless, generational, trapped, national).

% Choose the chamber that defines constitutional meaning, and so hold the ultimate lever indirectly. They receive accountable government whose acts trace to a vote, and they carry the diffuse consequence when a majority they later regret defines rights narrowly. Between elections their influence is limited to persuasion.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, national_electorates, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_authority_boundary__parliamentary_primacy_reading, national_electorates, payer).

% Sit in the chamber but out of power: today's outvoted minority drafts amendments that fail and watches constitutional meaning get defined without them, with the standing expectation that electoral rotation may hand them the pen next term. Their exit runs through the same elections that discipline the majority.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, opposition_parliamentarians, payer,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(constitutional_authority_boundary__parliamentary_primacy_reading, opposition_parliamentarians, beneficiary).

% Campaign for bills of rights that future parliaments could not casually repeal. Their preferred instrument — entrenchment that binds successors — has no assured foothold in a system where each chamber's authority is renewed by ordinary majority, so they operate as petitioners to the very body whose powers they would trim.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, entrenched_charter_advocates, excluded,
    moderate, generational, constrained, national).

% Map and compare how different polities settle the question of final constitutional authority, publish critiques and defenses of the legislative-finality settlement, and supply the vocabulary in which judges and ministers argue about it. They hold no vote and bear no cost; their leverage is entirely persuasive.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, academic_constitutional_theorists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_authority_boundary__parliamentary_primacy_reading, governing_parliamentary_majorities).
narrative_ontology:fixing_cost_class(constitutional_authority_boundary__parliamentary_primacy_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, determinate answer to the question every multi-branch polity faces — when institutions disagree about what the constitution requires, who decides — by vesting final decision in the elected chamber. This avoids inter-branch deadlock, gives every constitutional controversy a terminus, and keeps the locus of decision traceable to a recent election.
% TRANSFER_FUNCTION: Moves final interpretive authority over constitutional questions from courts, and from the constitutional text considered as an independent limit, to the elected legislature: concretely, statutes acquire immunity from judicial invalidation, and the courts' constitutional pronouncements become advisory signals that a majority may legislate past.
% ABSENT_VOICES: Legal constitutionalists and entrenched-charter advocates would object that the room contains no seat for those whose rights depend on limits that outlast a majority; future minorities and future electorates are structurally absent because the settlement lets present majorities define meaning for successors. They are outside because the room is the chamber itself, and the settlement defines admission.
% DISAPPEARANCE_RATIONALE: If legislative finality vanished overnight, every statute's validity would become open to judicial challenge, pending litigation would surge, and the executive's authority would hang on unresolved appeals until a new arbiter — courts, a constituent assembly, or negotiated convention — emerged. The settlement's beneficiaries and bearers alike would have to renegotiate their positions; nothing about the surrounding society dictates the replacement.
% FOUNDING_PROBLEM: After the overthrow of royal prerogative as a source of unaccountable lawmaking, the founding problem was twofold: ensure that ultimate lawmaking authority rested with the elected representatives of the nation rather than the crown or unelected offices, and provide a determinate, non-violent method for resolving disagreements between institutions about the limits of public power.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians and comparative-politics scholarship corroborate both halves of the genealogy from outside the beneficiary set, and the judiciary — a seat that bears the settlement's costs — repeatedly affirms the historical account in its own judgments while noting the settlement's modern edges. No serious participant disputes that the anti-prerogative problem existed; the dispute concerns only whether its solution still serves.
narrative_ontology:disappearance_verdict(constitutional_authority_boundary__parliamentary_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_authority_boundary__parliamentary_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_authority_boundary__parliamentary_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_authority_boundary__parliamentary_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_authority_boundary__parliamentary_primacy_reading, 0.22, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_authority_boundary__parliamentary_primacy_reading_tests).
:- end_tests(constitutional_authority_boundary__parliamentary_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is low (0.22 at interval end): the settlement's costs — the judiciary's foreclosed final word, minorities' unentrenched exposure — are real but modest, partly offset by accountability benefits, and assessed by the reading's own lights the arrangement collects little rent. Suppression (0.28) is structural-doctrinal rather than coercive: no enforcement machinery exists, and the closure of the strong-judicial-review alternative operates through accepted doctrine, not force; insiders nonetheless cannot reach that alternative from inside the system, which keeps suppression above zero. Theater is low (0.15): the settlement is mostly functional, with a rising but still minor share of ceremonial sovereignty-invocation as its substantive content thins. Accessibility collapse (0.48) reflects the insider/comparative split: for participants inside the order, alternatives collapse substantially (a court cannot bootstrap binding review into existence), but other polities run different settlements and the chamber itself could legislate a new one. Resistance (0.2) is limited to occasional judicial dicta questioning the frame's absoluteness and a persistent academic critique; no sustained active resistance exists. The measurement series run on one shared time grid (t=0,20,40,60,80,100 over the settlement's modern century, roughly the universal-franchise era to the present) so every tracked metric is authored at every examined point; the mild rise-and-settle in extractiveness tracks the human-rights/devolution era's added friction and the subsequent post-referendum reassertion of legislative finality. No suppression_requirement series is authored: enforcement capacity is static across the interval — convention, not machinery, sustains the settlement — and that stability is carried by the scalar instead.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute sharply different types from identical structural data. From the chamber's position the settlement is self-government: final authority resting where elections can reach it. From the bench's position the same structure is subordination: a career spent administering a finality that belongs to another branch. From the minority seat it is contingency: protection held at the pleasure of transient majorities. Two of these seats share the institutional power atom yet sit at opposite ends of directionality — the differentiation comes from exit options, not global standing: the chamber holds arbitrage-grade exit (it writes the rule), while the bench is identity-locked into a role constituted by fidelity to enacted law.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to real structural relationships: the chamber and its transient majorities collect interpretive authority directly (d near the beneficiary end), and the electorate collects accountability (mixed beneficiary/payer, near symmetric). Victim declarations map to the two seats that bear costs through the structure: the judiciary, whose binding-review capacity is what the settlement takes, and minorities, whose protections rank below the next majority's priorities. One directionality override is declared: the derivation chain would drive the powerless minority seat to near-full-target (victim + trapped exit), but that overshoots — minorities also receive diffuse accountability benefits and have historically advanced their position through the very legislative channel the settlement empowers, so d is overridden down to 0.65, encoding partial-target status. The judiciary needs no override: victim + identity_locked correctly lands it near the full-target end, and its elevated per-seat effective extraction against a low aggregate epsilon is exactly the seat divergence this story exists to record.
 *
 * MANDATROPHY ANALYSIS:
 *   The analysis guards against two mislabels. Read from the judiciary's seat alone, the settlement resembles pure taking — one branch's authority confiscated for another's benefit. Read through its own ceremonial register, it masquerades as an immutable feature of democracy itself. Neither holds: the settlement solves a live coordination problem (anti-prerogative originally, inter-branch deadlock generally), its founding problem remains live per the R5 interview, and no mandatrophy is declared — the legacy boolean is left unset and the finding lives in the six_questions fields, which supersede it. The piton signature is likewise distant: the administrator (the chamber) bears trivial cost from the settlement and could change it cheaply, but the function it performs is not atrophied — statutes are still immune from invalidation, which is the settlement doing exactly what it was built to do.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the kernel constitutional_authority_boundary — the parliamentary_primacy_reading. What would change structurally if a sibling reading were instantiated instead?',
    'Comparative instantiation: the sibling stories (judicial_supremacy_reading, coordinate_construction_reading) are authored separately; their beneficiary/victim sets and epsilon values are compared against this file''s to locate the disagreement.',
    'Under the judicial-supremacy sibling the judiciary becomes the primary beneficiary and the legislature the target, with epsilon rising accordingly; under the coordinate sibling no single seat holds final authority and the beneficiary/victim structure dissolves into distributed competence. The disagreement is located precisely in WHO holds final interpretive authority — every other structural difference follows from that.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: this story is one reading of a three-way-contested kernel; sibling readings would invert or dissolve the beneficiary/victim structure.').

omega_variable(
    constructed_vs_natural_settlement,
    'Is legislative finality a discovered necessity of majoritarian democracy, or a constructed choice that comparative evidence shows to be replaceable?',
    'Comparative constitutional performance data across polities running judicial-supremacy and coordinate settlements: if rights protection, stability, and democratic satisfaction track no advantage for legislative finality, the necessity defense fails.',
    'If constructed, the settlement''s appeal to inevitability weakens and reform arguments gain standing; if necessary, part of the measured suppression reflects the price of the coordination itself rather than a defended preference.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constructed_vs_natural_settlement, empirical, 'Whether the settlement is a natural feature of democracy or one contingent design among viable alternatives.').

omega_variable(
    minority_protection_direction,
    'Does the settlement protect minorities better than entrenched alternatives (the accountability hypothesis) or expose them more (the counter-majoritarian hazard)?',
    'Cross-polity outcome comparison of minority-rights trajectories under legislative-finality versus entrenched-review settlements, controlling for wealth and culture.',
    'If accountability dominates, the minority seat''s directionality drops toward beneficiary and aggregate epsilon falls further; if exposure dominates, the seat is a genuine full target and a tangled-rope reading of the settlement becomes structurally plausible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_protection_direction, empirical, 'Sign of the minority seat''s net position under the settlement — the largest single uncertainty in the directionality picture.').

omega_variable(
    entrenchment_possibility_paradox,
    'Can a sovereign legislature genuinely entrench anything against its successors, or does the self-binding paradox reduce ''entrenched legislation'' to ordinary law with ceremonial reinforcement?',
    'Doctrinal analysis of attempted entrenchments in legislative-finality systems and whether any survived a determined majority; comparative study of referendum-locked provisions.',
    'If entrenchment is impossible, the settlement is purely majoritarian and the minority seat''s exposure is total; if entrenchment works, a scaffold-like element enters the structure and the minority seat''s exit improves.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(entrenchment_possibility_paradox, conceptual, 'Whether the settlement''s own terms permit limits that outlast the entrenching majority.').

omega_variable(
    dicean_frame_erosion,
    'Is the classical frame of unlimited legislative competence eroding in practice — through rights-instrument pressure, devolution, and judicial willingness to identify constitutional principles courts will enforce against the executive — or does the frame remain intact?',
    'Track whether any court in a legislative-finality polity asserts binding review over primary legislation, and whether legislative practice begins to self-limit in anticipation of judicial response.',
    'Continued erosion would move the drift state toward severe, pull the settlement toward a coordinate configuration, and eventually dissolve this reading''s reference frame; confirmed stability would validate the low-drift characterization and the rope classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dicean_frame_erosion, empirical, 'Trajectory of the settlement''s reference frame under contemporary practice — the story''s principal temporal uncertainty.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_authority_boundary__parliamentary_primacy_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(cons_tr_t0, observed).
narrative_ontology:measurement(cons_tr_t20, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 20, 0.11).
narrative_ontology:measurement_basis(cons_tr_t20, observed).
narrative_ontology:measurement(cons_tr_t40, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 40, 0.13).
narrative_ontology:measurement_basis(cons_tr_t40, observed).
narrative_ontology:measurement(cons_tr_t60, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 60, 0.15).
narrative_ontology:measurement_basis(cons_tr_t60, observed).
narrative_ontology:measurement(cons_tr_t80, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 80, 0.18).
narrative_ontology:measurement_basis(cons_tr_t80, observed).
narrative_ontology:measurement(cons_tr_t100, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 100, 0.15).
narrative_ontology:measurement_basis(cons_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(cons_be_t0, observed).
narrative_ontology:measurement(cons_be_t20, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 20, 0.16).
narrative_ontology:measurement_basis(cons_be_t20, observed).
narrative_ontology:measurement(cons_be_t40, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 40, 0.19).
narrative_ontology:measurement_basis(cons_be_t40, observed).
narrative_ontology:measurement(cons_be_t60, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 60, 0.21).
narrative_ontology:measurement_basis(cons_be_t60, observed).
narrative_ontology:measurement(cons_be_t80, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 80, 0.24).
narrative_ontology:measurement_basis(cons_be_t80, observed).
narrative_ontology:measurement(cons_be_t100, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 100, 0.22).
narrative_ontology:measurement_basis(cons_be_t100, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(constitutional_authority_boundary__parliamentary_primacy_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_authority_boundary__parliamentary_primacy_reading, resource_allocation).
narrative_ontology:affects_constraint(constitutional_authority_boundary__parliamentary_primacy_reading, constitutional_authority_boundary__judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_authority_boundary__parliamentary_primacy_reading, constitutional_authority_boundary__coordinate_construction_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial notion 'constitutional authority' decomposes into three structurally distinct settlements of the same kernel (constitutional_authority_boundary). This story authors the parliamentary-primacy member: legislature as final arbiter, low epsilon, judiciary and unentrenched minorities as cost-bearing seats. The judicial-supremacy sibling inverts the beneficiary/victim structure (courts collect, legislatures pay) and carries materially higher epsilon; the coordinate-construction sibling dissolves the single-arbiter structure entirely, distributing interpretive competence with no final seat. The members are linked pairwise through network.affects_constraints because each is cited as the alternative to the others in constitutional argument; the upstream member in empirical-prevalence terms is this one (most polities historically ran some variant of legislative finality), which the siblings implicitly measure themselves against.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_authority_boundary__parliamentary_primacy_reading, powerless, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
