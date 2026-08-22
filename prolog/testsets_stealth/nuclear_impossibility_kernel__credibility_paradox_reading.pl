% ============================================================================
% CONSTRAINT STORY: nuclear_impossibility_kernel__credibility_paradox_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nuclear_impossibility_kernel__credibility_paradox_reading, []).

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
 *   constraint_id: nuclear_impossibility_kernel__credibility_paradox_reading
 *   human_readable: Nuclear Deterrence Credibility-Maintenance Regime (Credibility Paradox Reading)
 *   domain: strategic_studies/international_relations
 *
 * SUMMARY:
 *   Since 1945 the nuclear powers have maintained a standing apparatus whose
 *   function is to keep the threat of nuclear use believable enough to
 *   restrain rivals: declaratory doctrine, force posture, escalation
 *   planning, extended-deterrence commitments, and the exercise and
 *   modernization cycles that refresh believability. This story instantiates
 *   the credibility_paradox_reading of the nuclear_impossibility_kernel: the
 *   use-threat is inherently incredible because executing it guarantees
 *   mutual destruction, so the apparatus must be continuously rebuilt and
 *   defended, 'unthinkability' is maintained rhetoric rather than structural
 *   fact, and war remains reachable through escalation ladders that planners
 *   labor to keep climbable and controlled. The epsilon referent is the
 *   standing credibility-maintenance arrangement itself, assessed by this
 *   reading's own lights — not the disarmament arrangement any reading might
 *   endorse. Sibling readings (structural_contraction_reading,
 *   rational_dropout_reading) are separate constraint stories linked through
 *   the network section; their structural deltas are recorded in the omega
 *   variables. Claim and metrics are authored independently: the claim states
 *   tangled_rope as this reading's structural truth — a real coordination
 *   function (great-power war prevention) fused with asymmetric extraction
 *   and mandatory active enforcement — while the metrics describe the
 *   arrangement's observed operation without tuning toward any predicted
 *   engine output. KEY AGENTS (by structural relationship): -
 *   strategic_establishments: Agenda-setter and primary beneficiary
 *   (institutional/identity_locked) — authors doctrine, operates the forces,
 *   collects the budgets - nuclear_weapon_states_governments: Beneficiary
 *   with payer burden (institutional/constrained) — hold the weapons and the
 *   status - extended_deterrence_protectorates: Secondary beneficiary
 *   (powerful/constrained) — discounted security under another power's
 *   umbrella - deterrence_taxpayers: Primary payer (moderate/trapped) —
 *   compulsory funding, no exit - frontline_theater_populations: Primary
 *   victim (moderate/constrained) — designated theaters of limited exchange,
 *   never consulted - disarmament_advocacy_movements: Excluded voice
 *   (organized/constrained) — barred from planning channels -
 *   deterrence_theorists_community: Analytical observer
 *   (analytical/analytical) — models the paradox, referees the readings
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nuclear_impossibility_kernel__credibility_paradox_reading, 0.62).
domain_priors:suppression_score(nuclear_impossibility_kernel__credibility_paradox_reading, 0.61).
domain_priors:theater_ratio(nuclear_impossibility_kernel__credibility_paradox_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nuclear_impossibility_kernel__credibility_paradox_reading, tangled_rope).
narrative_ontology:human_readable(nuclear_impossibility_kernel__credibility_paradox_reading, "Nuclear Deterrence Credibility-Maintenance Regime (Credibility Paradox Reading)").
narrative_ontology:topic_domain(nuclear_impossibility_kernel__credibility_paradox_reading, "strategic_studies/international_relations").

domain_priors:requires_active_enforcement(nuclear_impossibility_kernel__credibility_paradox_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nuclear_impossibility_kernel__credibility_paradox_reading, '0cbec3ad-4f81-4252-a9ce-d3d25ffedbf1').
narrative_ontology:cs_kernel_codification('0cbec3ad-4f81-4252-a9ce-d3d25ffedbf1', distributed).
narrative_ontology:cs_authority_grounding('0cbec3ad-4f81-4252-a9ce-d3d25ffedbf1', expertise).
narrative_ontology:cs_interpretation_layer_present('0cbec3ad-4f81-4252-a9ce-d3d25ffedbf1').
narrative_ontology:cs_reading_relation('0cbec3ad-4f81-4252-a9ce-d3d25ffedbf1', nuclear_impossibility_kernel__structural_contraction_reading, influences).
narrative_ontology:cs_reading_relation('0cbec3ad-4f81-4252-a9ce-d3d25ffedbf1', nuclear_impossibility_kernel__rational_dropout_reading, coexists_with).
narrative_ontology:cs_axiom('0cbec3ad-4f81-4252-a9ce-d3d25ffedbf1', foundational, deterrence_requires_believed_use_threat).
narrative_ontology:cs_axiom_status(deterrence_requires_believed_use_threat, holdable).
narrative_ontology:cs_axiom_grounding('0cbec3ad-4f81-4252-a9ce-d3d25ffedbf1', deterrence_requires_believed_use_threat, instrumental).
narrative_ontology:cs_axiom('0cbec3ad-4f81-4252-a9ce-d3d25ffedbf1', foundational, use_barrier_is_maintained_rhetoric_not_structure).
narrative_ontology:cs_axiom_status(use_barrier_is_maintained_rhetoric_not_structure, holdable).
narrative_ontology:cs_axiom_grounding('0cbec3ad-4f81-4252-a9ce-d3d25ffedbf1', use_barrier_is_maintained_rhetoric_not_structure, empirically_contingent).
narrative_ontology:cs_axiom('0cbec3ad-4f81-4252-a9ce-d3d25ffedbf1', secondary, escalation_ladders_preserve_war_reachability).
narrative_ontology:cs_axiom_status(escalation_ladders_preserve_war_reachability, holdable).
narrative_ontology:cs_axiom_grounding('0cbec3ad-4f81-4252-a9ce-d3d25ffedbf1', escalation_ladders_preserve_war_reachability, empirically_contingent).
narrative_ontology:cs_reference_frame('0cbec3ad-4f81-4252-a9ce-d3d25ffedbf1', credible_use_threat_precondition).
narrative_ontology:cs_drift_state('0cbec3ad-4f81-4252-a9ce-d3d25ffedbf1', contemporary_second_nuclear_age, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('0cbec3ad-4f81-4252-a9ce-d3d25ffedbf1', '').
narrative_ontology:cs_kernel_id(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_impossibility_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__credibility_paradox_reading, strategic_establishments).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_weapon_states_governments).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__credibility_paradox_reading, extended_deterrence_protectorates).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__credibility_paradox_reading, deterrence_taxpayers).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__credibility_paradox_reading, frontline_theater_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_weapon_states_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design, operate, and narrate the arsenal: strategic commands run the forces, national laboratories maintain the stockpile, contractors build delivery systems, and all three author the declaratory statements and exercise schedules that keep the use-threat believable. Budgets, promotion ladders, and institutional purpose track the credibility mission; winding the mission down would mean dismantling organizations whose identity is the mission.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, strategic_establishments, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(nuclear_impossibility_kernel__credibility_paradox_reading, strategic_establishments, beneficiary).

% Hold the weapons and the great-power status that travels with them. They finance modernization cycles and accept the alliance entanglements the umbrella requires, because unilateral renunciation would leave them exposed to rivals who offer no reciprocal assurance. Their security claim and their largest discretionary budget line are the same object.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_weapon_states_governments, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_weapon_states_governments, payer).

% Allied states that host forward-based weapons and rely on another power's retaliation promise instead of fielding their own. They receive security below its full production cost, but basing agreements place them on target lists, and leaving the umbrella means choosing between indigenous armament and uncovered vulnerability.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, extended_deterrence_protectorates, beneficiary,
    powerful, biographical, constrained, regional).

% Fund the machinery through general revenue across decades. They cannot decline the appropriation, rarely see the trade-offs behind specific programs, and absorb the opportunity cost of every modernization cycle against domestic spending.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, deterrence_taxpayers, payer,
    moderate, immediate, trapped, national).

% Live in the regions that war plans designate as the corridor of limited exchange — Central Europe in the earlier decades, East Asian littorals and the eastern European plain in current planning. They carry the highest stakes in any breakdown of restraint, were never consulted about hosting the theater, and cannot relocate away from geography chosen by planners elsewhere.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, frontline_theater_populations, payer,
    moderate, biographical, constrained, regional).

% Organize for abolition and deep cuts through treaties, courts, and mass mobilization. They hold legal and moral arguments that the posture-setting councils do not admit to their tables; their access runs through public opinion and occasional diplomatic conferences rather than planning channels.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, disarmament_advocacy_movements, excluded,
    organized, generational, constrained, global).

% Model the dilemma, publish the canonical treatments, staff the advisory pipelines, and referee disputes among rival interpretations of what the weapons did to war. They collect no rents from the arrangement and bear none of its direct costs; their seat is analytic.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, deterrence_theorists_community, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nuclear_impossibility_kernel__credibility_paradox_reading, strategic_establishments).
narrative_ontology:fixing_cost_class(nuclear_impossibility_kernel__credibility_paradox_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a believed use-threat sufficient to prevent direct military conflict between nuclear-armed rivals: the credibility apparatus — declaratory policy, force posture, escalation planning, extended commitments — coordinates great-power restraint by making each side's retaliation believable.
% TRANSFER_FUNCTION: Moves fiscal resources from general tax bases to strategic weapons complexes, commands, and contractors; moves existential risk onto frontline populations whose regions are the designated theaters of limited exchange; confers great-power status on nuclear-armed elites.
% ABSENT_VOICES: Disarmament advocates and non-nuclear states outside alliance umbrellas are structurally marginalized in strategic planning; frontline populations were never consulted about serving as the presumed battlefield. Their objection — that stability purchased with their territory is not their bargain — has no seat in the councils that set posture.
% DISAPPEARANCE_RATIONALE: If the credibility-maintenance apparatus vanished overnight, alliances built on extended deterrence would dissolve or rearm, nuclear states would race either to rebuild or to negotiate arsenals away, and great-power military planning would reorganize around conventional balance — the dependence web is exactly what the stakeholder surface names.
% FOUNDING_PROBLEM: After 1945, rival ideological blocs armed with fission weapons faced the problem of avoiding a third world war without conceding core interests — how to stabilize permanent great-power rivalry between armed camps.
% FOUNDING_PROBLEM_CORROBORATION: Declassified crisis archives (Executive Committee recordings, Able Archer inquiry materials) and historians of the Cold War writing outside the benefiting parties attest both the original danger and the recurring proximity of use; non-nuclear states' diplomatic testimony at review conferences attests that the persistence-of-rivalry framing is disputed. Corroboration exists for the genealogy itself; no external source settles whether the founding problem remains live, which is why status is authored as contested.
narrative_ontology:disappearance_verdict(nuclear_impossibility_kernel__credibility_paradox_reading, world_rearranges).
narrative_ontology:founding_problem_status(nuclear_impossibility_kernel__credibility_paradox_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nuclear_impossibility_kernel__credibility_paradox_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(nuclear_impossibility_kernel__credibility_paradox_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nuclear_impossibility_kernel__credibility_paradox_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nuclear_impossibility_kernel__credibility_paradox_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nuclear_impossibility_kernel__credibility_paradox_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nuclear_impossibility_kernel__credibility_paradox_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.62 because the apparatus transfers a large, recurring share of discretionary state expenditure to a concentrated professional complex while its core deliverable — non-use — is attributable jointly to the apparatus and to factors it does not control (see the deterrence-versus-luck omega). Suppression is authored at 0.61 as a raw structural property, unscaled by power or scope: secrecy regimes, career gatekeeping, and the marginalization of abolitionist argument inside planning channels suppress alternatives, while the broader public retains electoral and protest channels, keeping suppression below pure-extraction grade. Theater_ratio 0.45 reflects the widening gap between declaratory 'unthinkability' rhetoric and the operational pursuit of usable options — roughly half of maintenance activity signals rather than functions, per this reading's own account. Accessibility_collapse 0.38: alternatives (deep cuts, minimum deterrence, conventional defense, abolition) remain visibly argued and institutionally represented, so understanding the arrangement does not close the option space. Resistance 0.58: organized treaty movements, litigation, and mass mobilization meet the apparatus continuously. The measurement series share one nine-point grid (1945-2025) across all three tracked metrics. The trajectories oscillate rather than drift monotonically: crisis, buildup, detente, relaxation, accumulation repeats across the Cold War and recurs in the second nuclear age. The oscillation is partly the extraction mechanism itself — each crisis re-legitimates the apparatus and resets budget baselines (intermittent reinforcement), so the calm-phase dips do not indicate benignity. End-state values equal the base_properties scalars; mid-series variation documents the cycle.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from identical structural data. From the strategic_establishments seat the arrangement is indispensable stewardship: the paradox is the job, and skepticism is irresponsibility. From the nuclear_weapon_states seat it is a status asset worth its budget line. From the protectorate seat it is discounted security carrying a basing liability. From the taxpayer and frontline-population seats it is compulsory payment for a deliverable they cannot verify and a risk they never consented to. The frontline seat additionally notes that coalition potential is structurally weak: prospective theaters sit in different countries inside divergent alliances, preventing the cross-border coalition that shared exposure would otherwise suggest. The observer seat sees a self-referential machine: the apparatus exists to sustain belief in a threat whose incredibility is the reason the apparatus exists.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: strategic_establishments sit nearest the beneficiary pole (they author the rules and collect the budgets); nuclear_weapon_states_governments and extended_deterrence_protectorates sit low-to-moderate (net security gain against partial payment and basing exposure). Victim declarations drive high directionality: deterrence_taxpayers sit near the target pole (compulsory payment, no exit from taxation), and frontline_theater_populations sit nearest the full-target end — they bear the existential tail risk of the apparatus failing, were never consulted, and cannot relocate off geography selected by planners elsewhere. Because the engine scales effective extraction by directionality and scope while leaving suppression unscaled, the global-scope beneficiary seats see dampened effective extraction while the trapped regional victim seats see amplified effective extraction — the structural asymmetry that produces seat divergence. No directionality overrides are authored: the beneficiary/victim declarations plus exit options already yield the correct qualitative placement for every seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — stabilizing armed bloc rivalry after 1945 — is contested rather than dead: great-power rivalry has returned in a new configuration, so the arrangement cannot be flagged as a zombie mandate, but neither can the original problem be certified live in its 1947 form. Holding founding_problem_status at contested keeps the status-by-verdict consumer honest. The tangled_rope claim performs the anti-mislabeling work in both directions: a pure-extraction reading would erase the genuine war-prevention coordination that even this reading's critics concede, while a pure-coordination reading would erase the concentrated receipt of gains (named in the receipt surface) and the enforced marginalization of alternatives. The classification also blocks a decayed-inertia reading: the apparatus is not maintained by performance alone — it is actively rebuilt each cycle, which is why theater_ratio stays below the inertial band despite substantial rhetorical overlay.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint instantiates the credibility_paradox_reading of nuclear_impossibility_kernel; how would classification change if the structural_contraction_reading or rational_dropout_reading were adopted instead?',
    'Author the sibling readings as separate constraint stories with their own epsilon, stakeholders, and metrics, then compare computed types across the family.',
    'If structural contraction is right, the use-barrier is a fixed limit and the credibility-maintenance apparatus is largely superfluous signaling; if rational dropout is right, the regime is a stable coordination device and measured extraction is mostly coordination cost. This reading''s instability claim supports the tangled_rope computation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: one of three live readings of the nuclear-impossibility kernel; sibling deltas recorded here rather than folded into this story.').

omega_variable(
    rhetorical_versus_structural_barrier,
    'Is the barrier to nuclear use genuinely structural (guaranteed retaliation under any attack) or maintained rhetoric that maturing counterforce and damage-limitation technology could dissolve?',
    'Comparative analysis of counterforce capability growth against observed crisis behavior; declassified deliberation records and exercise data on first-use decision points.',
    'If the barrier is rhetorical, this reading''s instability claim strengthens and the arrangement trends toward pure extraction as usable options mature; if structural, the maintenance apparatus is mostly performance and the theater_ratio is understated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rhetorical_versus_structural_barrier, empirical, 'Whether ''unthinkability'' is structural fact or maintained rhetoric — the load-bearing ambiguity of this reading.').

omega_variable(
    escalation_control_validity,
    'Does escalation actually remain controllable once nuclear use begins, or is the climbable-and-controllable escalation ladder a planner fiction?',
    'Historical near-miss reconstruction (Cuban missile crisis communications failures, Able Archer 83, the Norwegian rocket incident) combined with red-team wargaming of limited-exchange scenarios.',
    'Uncontrollable escalation collapses this reading''s ''war remains reachable'' delta toward the structural-contraction position; demonstrably controllable escalation validates the instability claim and raises the stakes of credibility maintenance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(escalation_control_validity, empirical, 'Whether escalation ladders hold under use — the empirical hinge between this reading and its structural sibling.').

omega_variable(
    deterrence_versus_luck_attribution,
    'Has the credibility apparatus prevented great-power war, or have documented near-misses shown survival to be luck the apparatus did not purchase?',
    'Counterfactual analysis of crisis archives separating deterrence effects from chance events (malfunctions, misinterpretations, unauthorized actions).',
    'Luck attribution strips the coordination function of genuineness, pushing effective extraction upward and the classification toward pure extraction; robust deterrence attribution secures the coordination half of the hybrid.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_versus_luck_attribution, empirical, 'Attribution of non-use to the apparatus versus chance — determines whether the coordination function is real.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nuclear_impossibility_kernel__credibility_paradox_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nucl_tr_t1945, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 1945, 0.12).
narrative_ontology:measurement(nucl_tr_t1955, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 1955, 0.2).
narrative_ontology:measurement(nucl_tr_t1965, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 1965, 0.28).
narrative_ontology:measurement(nucl_tr_t1975, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 1975, 0.33).
narrative_ontology:measurement(nucl_tr_t1985, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 1985, 0.36).
narrative_ontology:measurement(nucl_tr_t1995, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 1995, 0.44).
narrative_ontology:measurement(nucl_tr_t2005, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 2005, 0.43).
narrative_ontology:measurement(nucl_tr_t2015, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 2015, 0.41).
narrative_ontology:measurement(nucl_tr_t2025, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 2025, 0.45).

% Extraction over time
narrative_ontology:measurement(nucl_be_t1945, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 1945, 0.38).
narrative_ontology:measurement(nucl_be_t1955, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 1955, 0.5).
narrative_ontology:measurement(nucl_be_t1965, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 1965, 0.6).
narrative_ontology:measurement(nucl_be_t1975, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 1975, 0.58).
narrative_ontology:measurement(nucl_be_t1985, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 1985, 0.64).
narrative_ontology:measurement(nucl_be_t1995, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 1995, 0.46).
narrative_ontology:measurement(nucl_be_t2005, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 2005, 0.5).
narrative_ontology:measurement(nucl_be_t2015, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 2015, 0.55).
narrative_ontology:measurement(nucl_be_t2025, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 2025, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(nucl_su_t1945, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 1945, 0.35).
narrative_ontology:measurement(nucl_su_t1955, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 1955, 0.55).
narrative_ontology:measurement(nucl_su_t1965, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 1965, 0.65).
narrative_ontology:measurement(nucl_su_t1975, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 1975, 0.58).
narrative_ontology:measurement(nucl_su_t1985, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 1985, 0.66).
narrative_ontology:measurement(nucl_su_t1995, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 1995, 0.48).
narrative_ontology:measurement(nucl_su_t2005, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 2005, 0.5).
narrative_ontology:measurement(nucl_su_t2015, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 2015, 0.54).
narrative_ontology:measurement(nucl_su_t2025, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 2025, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nuclear_impossibility_kernel__credibility_paradox_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__credibility_paradox_reading, structural_contraction_reading).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__credibility_paradox_reading, rational_dropout_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'nuclear weapons made war impossible' conflates three structurally distinct claims with different epsilon values and different beneficiary structures. This story authors the credibility-paradox claim (instability, maintained rhetoric, reachable war). The structural-contraction and rational-dropout claims are separate stories; each carries its own epsilon, stakeholders, and classification, and all three link through affects_constraints. This reading is upstream in influence: its instability premise generates the counterforce and limited-war programs that put empirical pressure on the structural-contraction premise and supply the cost-side volatility the rational-dropout reading prices.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
