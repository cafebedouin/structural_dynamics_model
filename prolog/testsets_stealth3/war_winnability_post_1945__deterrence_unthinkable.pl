% ============================================================================
% CONSTRAINT STORY: war_winnability_post_1945__deterrence_unthinkable
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_winnability_post_1945__deterrence_unthinkable, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: war_winnability_post_1945__deterrence_unthinkable
 *   human_readable: Post-1945 Categorical Unwinnability of Great-Power Total War (Deterrence-Unthinkable Reading)
 *   domain: strategic studies / nuclear deterrence theory / international relations
 *
 * SUMMARY:
 *   After 1945, the dominant strategic settlement held that great-power total
 *   war had become categorically unwinnable and that planning for victory was
 *   therefore incoherent - a category error rather than a bad idea. Strategy
 *   reorganized around averting war rather than fighting it (Brodie's dictum
 *   that the chief purpose of military establishments must become averting
 *   the wars they exist to fight). This story instantiates the
 *   deterrence_unthinkable reading of the war_winnability_post_1945 kernel:
 *   the foreclosure is treated here as REAL and OPERATIONAL - winnability
 *   exited the reachable planning space entirely. The claim/metric gap is
 *   deliberate: the reading CLAIMS a natural-law-like categorical foreclosure
 *   (mountain), while the authored metrics describe an arrangement with named
 *   beneficiaries, named payers, and active doctrinal enforcement - the
 *   divergence is the false-summit signal the corpus exists to take, not an
 *   error to reconcile. CONSTRAINT FAMILY: this story is one of three
 *   readings of the same kernel, linked via network.affects_constraints. The
 *   siblings instantiate different constraints with different epsilon:
 *   countervailing_thinkable (winnability constrained but alive; lower
 *   epsilon, smaller victim set) and rhetorical_contraction (contraction
 *   discursive only; extraction relocated to speech space). This reading sits
 *   upstream of rhetorical_contraction in one sense (its categorical claim is
 *   what made victory-talk unsayable) and in direct logical conflict with it
 *   in another (this reading asserts operational foreclosure; the sibling
 *   denies it). KEY AGENTS (by structural relationship): -
 *   civilian_populations_of_great_powers: Primary beneficiary
 *   (moderate/trapped) - receives the no-war good - political_leaderships:
 *   Secondary beneficiary (powerful/constrained) - decision stability and
 *   legitimating narrative - deterrence_intellectual_establishment: Secondary
 *   beneficiary and receipt seat (institutional/identity_locked) - collects
 *   the doctrine's discursive rents - military_establishments: Primary target
 *   (institutional/trapped) - bears mission foreclosure -
 *   war_planning_professions: Secondary target (moderate/constrained) - craft
 *   rendered unspeakable in open doctrine - counterforce_advocates: Excluded
 *   voice (moderate/constrained) - inside institutions, outside the
 *   conversation - international_relations_historians: Analytical observer
 *   (analytical/analytical)
 *
 * KEY AGENTS:
 *   - civilian_populations_of_great_powers: primary beneficiary (moderate/trapped) - receives the non-occurrence of great-power total war, bears residual accident and escalation risk without voice in employment doctrine
 *   - political_leaderships: secondary beneficiary (powerful/constrained) - governs inside the settlement; catastrophic options removed from their desks, stabilizing narrative supplied, exit would trigger punishable instability
 *   - deterrence_intellectual_establishment: secondary beneficiary and receipt seat (institutional/identity_locked) - administers the settlement through think tanks, war colleges, and advisory circuits; careers fused to the premise
 *   - military_establishments: primary target (institutional/trapped) - profession of arms whose defining object is foreclosed; maintains forces whose use is officially unthinkable
 *   - war_planning_professions: secondary target (moderate/constrained) - operational researchers and war gamers whose product survives only in classified channels
 *   - counterforce_advocates: excluded voice (moderate/constrained) - argues limited victory remains thinkable; barred from doctrinal authority
 *   - international_relations_historians: analytical observer (analytical/analytical) - traces whether the foreclosure was forced by physics or chosen by benefited coalitions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_winnability_post_1945__deterrence_unthinkable, 0.38).
domain_priors:suppression_score(war_winnability_post_1945__deterrence_unthinkable, 0.55).
domain_priors:theater_ratio(war_winnability_post_1945__deterrence_unthinkable, 0.37).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, extractiveness, 0.38).
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 0.37).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_winnability_post_1945__deterrence_unthinkable, mountain).
narrative_ontology:human_readable(war_winnability_post_1945__deterrence_unthinkable, "Post-1945 Categorical Unwinnability of Great-Power Total War (Deterrence-Unthinkable Reading)").
narrative_ontology:topic_domain(war_winnability_post_1945__deterrence_unthinkable, "strategic studies / nuclear deterrence theory / international relations").

domain_priors:requires_active_enforcement(war_winnability_post_1945__deterrence_unthinkable).
domain_priors:emerges_naturally(war_winnability_post_1945__deterrence_unthinkable).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_winnability_post_1945__deterrence_unthinkable, '862da1c2-f5be-4988-a6c8-be6a2d1fd7bd').
narrative_ontology:cs_kernel_codification('862da1c2-f5be-4988-a6c8-be6a2d1fd7bd', formalized).
narrative_ontology:cs_authority_grounding('862da1c2-f5be-4988-a6c8-be6a2d1fd7bd', extraction).
narrative_ontology:cs_interpretation_layer_present('862da1c2-f5be-4988-a6c8-be6a2d1fd7bd').
narrative_ontology:cs_reading_relation('862da1c2-f5be-4988-a6c8-be6a2d1fd7bd', war_winnability_post_1945__countervailing_thinkable, coexists_with).
narrative_ontology:cs_reading_relation('862da1c2-f5be-4988-a6c8-be6a2d1fd7bd', war_winnability_post_1945__rhetorical_contraction, forecloses).
narrative_ontology:cs_axiom('862da1c2-f5be-4988-a6c8-be6a2d1fd7bd', foundational, victory_planning_is_category_error).
narrative_ontology:cs_axiom_status(victory_planning_is_category_error, holdable).
narrative_ontology:cs_axiom_grounding('862da1c2-f5be-4988-a6c8-be6a2d1fd7bd', victory_planning_is_category_error, empirically_contingent).
narrative_ontology:cs_axiom('862da1c2-f5be-4988-a6c8-be6a2d1fd7bd', secondary, war_averting_chief_military_purpose).
narrative_ontology:cs_axiom_status(war_averting_chief_military_purpose, holdable).
narrative_ontology:cs_axiom_grounding('862da1c2-f5be-4988-a6c8-be6a2d1fd7bd', war_averting_chief_military_purpose, instrumental).
narrative_ontology:cs_reference_frame('862da1c2-f5be-4988-a6c8-be6a2d1fd7bd', categorical_unwinnability_settlement).
narrative_ontology:cs_drift_state('862da1c2-f5be-4988-a6c8-be6a2d1fd7bd', contemporary_multipolar_revisionism, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('862da1c2-f5be-4988-a6c8-be6a2d1fd7bd', '').
narrative_ontology:cs_kernel_id(war_winnability_post_1945__deterrence_unthinkable, war_winnability_post_1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__deterrence_unthinkable, civilian_populations_of_great_powers).
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__deterrence_unthinkable, political_leaderships).
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__deterrence_unthinkable, deterrence_intellectual_establishment).
narrative_ontology:constraint_victim(war_winnability_post_1945__deterrence_unthinkable, military_establishments).
narrative_ontology:constraint_victim(war_winnability_post_1945__deterrence_unthinkable, war_planning_professions).
narrative_ontology:constraint_vindicates(war_winnability_post_1945__deterrence_unthinkable, nuclear_revolution_theory).
narrative_ontology:constraint_vindicates(war_winnability_post_1945__deterrence_unthinkable, mutual_assured_destruction_logic).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive the arrangement's core output - the non-occurrence of great-power total war - while bearing its residual risks: accident and escalation exposure, the tax burden of arsenal maintenance, and civil-defense obligations they did not choose. They have no direct voice in targeting doctrine or employment policy; their protection is administered on their behalf. Exit is unavailable: citizenship and geography fix their exposure to whatever posture their governments sustain.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, civilian_populations_of_great_powers, beneficiary,
    moderate, generational, trapped, global).

% Govern inside the settlement: the foreclosure of victory planning removes catastrophic options from their desks and supplies a stabilizing public narrative of deterrence and peace through strength. They inherit the management burden - crisis signaling, posture reviews, alliance reassurance - and cannot step outside the framework without triggering instability their publics would punish. Their tenures depend on the non-occurrence the arrangement delivers.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, political_leaderships, beneficiary,
    powerful, biographical, constrained, national).

% Founded and staffs the discipline that administers the settlement: think tanks, university security-studies programs, defense-advisory circuits, war-college faculties. Collects status, funding, and advisory access by producing the theories - assured destruction, escalation management, arms control - through which official thought runs. Members' careers and self-concept are constituted by the premise that victory planning is incoherent; rehabilitating winnability would depreciate their accumulated intellectual capital, and leaving the framework means leaving the profession.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, deterrence_intellectual_establishment, beneficiary,
    institutional, generational, identity_locked, global).

% The profession of arms organized, until 1945, around fighting and winning wars finds its defining object foreclosed: its chief purpose becomes averting the war it exists to fight. It maintains forces whose employment is officially unthinkable, trains for operations that must never occur, and absorbs budget competition justified by 'war is obsolete' rhetoric while remaining the custodian of the arsenal that makes the rhetoric true. It cannot exit: the state compels its existence, and no pre-nuclear professionalism remains available to return to.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, military_establishments, payer,
    institutional, generational, trapped, global).

% Operational researchers, war gamers, and logistics planners whose craft assumed war as a solvable problem. Open doctrine renders their core product unspeakable; careers migrate into classified contingency work, into deterrence modeling, or out of the field entirely. Their skills stay legible inside the closed channel even as the open channel denies their object exists.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, war_planning_professions, payer,
    moderate, biographical, constrained, national).

% Strategists who argue that limited nuclear war remains thinkable and winnable through counterforce targeting and escalation control. Within the settlement's frame their position reads as professional irresponsibility; they publish in minority venues, file dissenting annexes, and are barred from doctrinal authority. They are inside the institutions but outside the conversation that sets doctrine.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, counterforce_advocates, excluded,
    moderate, biographical, constrained, national).

% Trace the settlement across declassified planning documents, service histories, and adversary archives; assess whether the foreclosure was forced by weapons physics or chosen by coalitions that benefited from its stability. They neither collect from nor pay into the arrangement; their assessments feed later doctrinal revisions and the historical record on which future settlements will be judged.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, international_relations_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(war_winnability_post_1945__deterrence_unthinkable, deterrence_intellectual_establishment).
narrative_ontology:fixing_cost_class(war_winnability_post_1945__deterrence_unthinkable, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of great-power war avoidance: once both sides hold forces that make total war mutually suicidal, the foreclosure of victory planning coordinates expectations so that neither side invests in war-winning capabilities, gambles on first strikes, or treats crisis bargaining as a prelude to war. It aligns all great-power elites on the same operating assumption - that the war will not be fought - and stabilizes the reciprocal restraint that follows.
% TRANSFER_FUNCTION: Moves strategic authority and resources from war-fighting institutions (military establishments, planning professions) to deterrence-administering institutions (defense intellectuals, centralized command structures, arms-control bureaucracies); moves the option of total war out of every actor's choice set; moves public attention and democratic oversight away from war-preparation questions by rendering them technically unutterable.
% ABSENT_VOICES: Counterforce_advocates and the limited-war tradition would object that the foreclosure forecloses prematurely - that declaring war unthinkable strips leaders of the analytical tools needed to manage crises short of it, and that a settlement which cannot say how a war would be fought also cannot say how one would be avoided. They sit outside doctrinal authority, in minority journals and dissenting annexes. Ordinary civilians bear the residual risk of accident and escalation with no seat in employment-policy deliberations at all.
% DISAPPEARANCE_RATIONALE: If the foreclosure vanished overnight - if great-power elites came to regard total war as winnable and resumed victory planning - force postures would shift toward damage limitation and counterforce, arms racing would intensify around first-strike incentives, crisis bargaining would harden as each side priced in war-fighting options, and the probability of catastrophic miscalculation would rise sharply. Every named seat's situation would rearrange: the intellectual establishment's premise would collapse, the military professions would recover their telos at the price of living on a hair trigger, and civilian populations would lose the non-occurrence they currently receive without choosing it.
% FOUNDING_PROBLEM: Built to solve the problem that crystallized between 1945 and the late 1950s: states now possessed arsenals capable of destroying each other's societies, military organizations were trained for a mission that promised universal suicide, and no doctrine existed for how such states should orient toward war. The settlement's answer was to declare victory incoherent and redirect strategy to prevention.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting set: payer-side institutional records (service histories, war-college curricula, declassified planning documents such as NSC-68 and the SIOP studies) attest that the founding problem drove the reorganization; former officials' memoirs and independent diplomatic historiography confirm it; adversary-side archives (Soviet General Staff materials) show a parallel foreclosure reached independently. No attesting source depends on the settlement's continuation for its standing.
narrative_ontology:disappearance_verdict(war_winnability_post_1945__deterrence_unthinkable, world_rearranges).
narrative_ontology:founding_problem_status(war_winnability_post_1945__deterrence_unthinkable, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_winnability_post_1945__deterrence_unthinkable, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(war_winnability_post_1945__deterrence_unthinkable, 'none', 1).
narrative_ontology:epsilon_provenance(war_winnability_post_1945__deterrence_unthinkable, 0.38, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_winnability_post_1945__deterrence_unthinkable_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, ExtMetricName, E),
    domain_priors:suppression_score(war_winnability_post_1945__deterrence_unthinkable, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(war_winnability_post_1945__deterrence_unthinkable),
    narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(war_winnability_post_1945__deterrence_unthinkable_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.38 at interval end): the arrangement's principal charge - surrendering total war as an option - is one every governed party accepts given the alternative, but real transfers ride on it: mission coherence taken from military professions, planning careers displaced into classified channels, epistemic authority concentrated in a small deterrence-expert class, and allied autonomy narrowed. Suppression (0.55) reflects active enforcement of the foreclosure - budget lines, clearance-gated planning, promotion gates, and the professional treatment of victory-planning as irresponsibility - roughly 60% structural and 40% internalized (professionals police themselves even where enforcement relaxes; see the suppression omega). Theater (0.37) captures the growing share of declaratory ritual - assured-destruction pronouncements, posture reviews, no-first-use debates - that performs the settlement while practice quietly diverges beneath it. Accessibility_collapse (0.62) is high but sub-mountain: within the reading's frame the alternative collapses almost completely once the premise is granted, yet operational alternatives demonstrably persisted in classified form, which is precisely the seam the rhetorical_contraction sibling mines. Resistance (0.48) records the durable minority tradition - counterforce schools, escalation-ladder theorists, services defending conventional missions - that never won doctrinal authority and never went extinct.
 *   
 *   The temporal series run on one shared grid (t=0,15,30,45,60,75, mapping approximately to 1945-2020). Base extractiveness rose as the settlement consolidated (services reorganized, planning professions displaced), peaked at the height of assured-destruction orthodoxy, eased as post-Cold-War pluralization reopened debate, and ticked back up with renewed multipolar rivalry. Suppression_requirement is authored because enforcement capacity is the traced dynamic here: it built steeply through the settlement's consolidation (0.25 to 0.62), partially relaxed after the Cold War (0.50), and re-narrowed recently (0.55) as employment policy receded behind renewed secrecy. Theater rises monotonically as declaratory performance accumulates atop a stable operational core. All points are observed historical judgments, not projections.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary seats should compute different types from identical structural data. From military_establishments the settlement operates as a foreclosure imposed from outside: the profession's telos revoked, its training redirected to operations that must never occur, its budget defended in a rhetoric that calls war obsolete while demanding perpetual readiness for it. From civilian_populations_of_great_powers the same structure operates as salvation - the non-occurrence of civilization-ending war - experienced not as a constraint at all but as the background condition of life. Political_leaderships experience it as both: freedom from catastrophic temptation and captivity to a management burden they cannot lay down. The sharpest divergence is between the two institutional-power seats: military_establishments (trapped - the state compels their existence and no pre-nuclear professionalism remains to return to) and deterrence_intellectual_establishment (identity_locked - the fusion here is professional-institutional: careers, institutions, and self-concept constituted by the doctrine's premise, so that rehabilitating winnability would depreciate accumulated intellectual capital and exit would mean leaving the profession). If that identity frame broke, the establishment's seat would migrate from beneficiary toward payer, and the settlement's enforcement coalition would narrow to leaderships alone.
 *
 * DIRECTIONALITY LOGIC:
 *   Civilian populations sit nearest the beneficiary pole (d near 0.0): they receive the arrangement's core output and bear only diffuse residual risk. Political_leaderships derive low d as beneficiaries, slightly raised by the crisis-management burden they carry. The deterrence_intellectual_establishment derives very low d - full beneficiary further damped by identity_locked exit, which anchors them at the subsidized end. Military_establishments derive high d as trapped payers: the foreclosure takes their mission and offers no exit, so effective extraction is amplified toward the full-target end. War_planning_professions derive similarly high d with somewhat less amplification (constrained rather than trapped exit - skills remain legible in classified channels). Counterforce_advocates are excluded rather than coordinated: their exclusion is part of what the enforcement machinery maintains. Spatial scope is global, which scales effective extraction modestly upward for the targets (verification of doctrinal compliance across alliances and services is hard); suppression, by contrast, enters the computation unscaled - it is a raw structural property of the settlement's enforcement machinery. No directionality overrides are used: the beneficiary/victim declarations plus exit atoms already produce the correct relationships for every seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification apparatus earns its keep twice here. First, the categorical claim ('planning for victory is incoherent') is dressed as natural law - the way gravity is dressed - and a naive reading would certify a mountain and stop asking questions. Declaring the beneficiaries and payers routes the story through false-summit evaluation: if the foreclosure is a constructed settlement maintained by actors who collect from its stability, the mountain claim is a false summit and the engine reclassifies accordingly. Second, the mandatrophy interview shows the founding problem - avoiding existential great-power war while arsenals persist - is still LIVE, corroborated from outside the benefiting set by payer-side institutional records (service histories, war-college curricula) and by independent historiography and adversary archives. Live founding problem plus world_rearranges verdict means no zombie flag: the settlement has not outlived its function. What HAS atrophied is tracked by the rhetorical_contraction sibling - the declaratory function increasingly performs a foreclosure that practice no longer fully honors - and that drift is visible in this story's rising theater_ratio rather than mislabeled here as mandate death.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constructed_vs_natural_foreclosure,
    'Is the categorical unwinnability of great-power total war a physical/logical limit that would hold regardless of who defends it or enforces it, or a constructed doctrine maintained because identifiable actors benefit from its settlement?',
    'Compare strategic outcomes across arsenal sizes and damage-limitation technologies: if counterforce accuracy, missile defense, or deep arsenal reductions reopen a coherent victory calculus, the foreclosure tracks force structure (constructed) rather than weapons physics (natural).',
    'If constructed, the false-summit signature fires and the constraint reclassifies toward tangled_rope with named beneficiaries; if natural, mountain certification stands and the beneficiary declarations are incidental to the physics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constructed_vs_natural_foreclosure, empirical, 'Natural-law versus constructed status of the winnability foreclosure').

omega_variable(
    kernel_reading_structural_delta,
    'This constraint instantiates the deterrence_unthinkable reading of kernel war_winnability_post_1945; what would the sibling readings (countervailing_thinkable, rhetorical_contraction) change structurally?',
    'Author the sibling stories and compare: countervailing_thinkable restores a coherent military mission through limited counterforce victory (shrinking the victim set and lowering epsilon); rhetorical_contraction relocates the foreclosure from planning space to speech space (moving victims from planners as such to open-doctrine professionals while operational planning continues).',
    'Adopting a sibling changes beneficiaries, victims, and classification; the disagreement is located in whether the foreclosure reaches operational planning or only declaratory doctrine.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer structure: this story is one reading of a three-reading kernel').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression of victory planning structural (career gates, budget control, clearance regimes, doctrinal gatekeeping) or internalized (professionals self-censor victory talk as inherently irresponsible)?',
    'Post-Cold-War discourse trajectory: if victory-planning arguments revived freely once external enforcement eased in the 1990s, the internalized component is small; if the taboo persisted after the enforcement machinery relaxed, it is internalized.',
    'Internalized suppression raises effective suppression above the structural measure and persists after enforcement removal, shifting the constraint''s computed classification toward harder extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized mechanism split of doctrinal suppression').

omega_variable(
    arsenal_contingency_of_naturalness,
    'The foreclosure''s naturalness is conditional on maintained second-strike arsenals: does the constraint persist as a property of weapons physics, or of the continuing political choice to keep arsenals at catastrophic scale?',
    'Track deep-reduction and treaty-lapse scenarios: if arsenals fall below damage-limitation thresholds and winnability debates reopen in official doctrine, the mountain dissolves into ordinary strategic choice.',
    'If arsenals shrink past the threshold, the constraint drifts from mountain toward rope/scaffold territory; classification and epsilon follow force structure rather than physics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(arsenal_contingency_of_naturalness, empirical, 'Whether the constraint''s persistence rides on maintained arsenals rather than physics alone').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_winnability_post_1945__deterrence_unthinkable, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t0, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(war__tr_t0, observed).
narrative_ontology:measurement(war__tr_t15, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 15, 0.18).
narrative_ontology:measurement_basis(war__tr_t15, observed).
narrative_ontology:measurement(war__tr_t30, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 30, 0.28).
narrative_ontology:measurement_basis(war__tr_t30, observed).
narrative_ontology:measurement(war__tr_t45, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 45, 0.3).
narrative_ontology:measurement_basis(war__tr_t45, observed).
narrative_ontology:measurement(war__tr_t60, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 60, 0.34).
narrative_ontology:measurement_basis(war__tr_t60, observed).
narrative_ontology:measurement(war__tr_t75, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 75, 0.37).
narrative_ontology:measurement_basis(war__tr_t75, observed).

% Extraction over time
narrative_ontology:measurement(war__be_t0, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 0, 0.2).
narrative_ontology:measurement_basis(war__be_t0, observed).
narrative_ontology:measurement(war__be_t15, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 15, 0.35).
narrative_ontology:measurement_basis(war__be_t15, observed).
narrative_ontology:measurement(war__be_t30, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 30, 0.42).
narrative_ontology:measurement_basis(war__be_t30, observed).
narrative_ontology:measurement(war__be_t45, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 45, 0.4).
narrative_ontology:measurement_basis(war__be_t45, observed).
narrative_ontology:measurement(war__be_t60, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 60, 0.36).
narrative_ontology:measurement_basis(war__be_t60, observed).
narrative_ontology:measurement(war__be_t75, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 75, 0.38).
narrative_ontology:measurement_basis(war__be_t75, observed).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t0, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 0, 0.25).
narrative_ontology:measurement_basis(war__su_t0, observed).
narrative_ontology:measurement(war__su_t15, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 15, 0.45).
narrative_ontology:measurement_basis(war__su_t15, observed).
narrative_ontology:measurement(war__su_t30, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 30, 0.62).
narrative_ontology:measurement_basis(war__su_t30, observed).
narrative_ontology:measurement(war__su_t45, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 45, 0.58).
narrative_ontology:measurement_basis(war__su_t45, observed).
narrative_ontology:measurement(war__su_t60, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 60, 0.5).
narrative_ontology:measurement_basis(war__su_t60, observed).
narrative_ontology:measurement(war__su_t75, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 75, 0.55).
narrative_ontology:measurement_basis(war__su_t75, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_winnability_post_1945__deterrence_unthinkable, enforcement_mechanism).
narrative_ontology:affects_constraint(war_winnability_post_1945__deterrence_unthinkable, war_winnability_post_1945__countervailing_thinkable).
narrative_ontology:affects_constraint(war_winnability_post_1945__deterrence_unthinkable, war_winnability_post_1945__rhetorical_contraction).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'nuclear revolution' / 'war is unwinnable now'. The label conflates three structurally distinct claims: (1) deterrence_unthinkable (this story) - categorical operational foreclosure, epsilon moderate, victims are military professions; (2) countervailing_thinkable - constrained-but-alive winnability, epsilon lower, victim set smaller; (3) rhetorical_contraction - discursive-only contraction, epsilon relocated onto speech-space professionals. Each gets its own epsilon, beneficiaries, and classification; all three link via network.affects_constraints. This reading influences the rhetorical sibling upstream (its categorical claim is what made victory-talk unsayable) and stands in logical conflict with it on the operative question.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
