% ============================================================================
% CONSTRAINT STORY: qwerty_persistence_mechanism__lock_in_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence_mechanism__lock_in_reading, []).

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
 *   constraint_id: qwerty_persistence_mechanism__lock_in_reading
 *   human_readable: QWERTY Layout Persistence (Path-Dependent Lock-In Reading)
 *   domain: economic_history/technology/path_dependence
 *
 * SUMMARY:
 *   The QWERTY key layout, standardized with the 1873 Remington contract,
 *   remains the universal keyboard mapping more than 150 years later. This
 *   story instantiates the LOCK-IN READING of the contested kernel
 *   qwerty_persistence_mechanism: the layout persists through path-dependent
 *   coordination failure despite technical inferiority — each typist,
 *   manufacturer, and employer rationally stays given that everyone else
 *   stays, and the aggregation of individually rational choices produces a
 *   collectively suboptimal equilibrium that no actor chose and no actor
 *   captures from. The colloquial label 'QWERTY persists' decomposes, per the
 *   epsilon-invariance principle, into three structurally distinct claims:
 *   this file (lock-in: spontaneous coordination failure, no capturer),
 *   naturalization_reading (the layout is genuinely adequate and rivals
 *   lapsed fairly), and beneficiary_extraction_reading (incumbents actively
 *   defend it for rents). Each is a separate constraint with its own epsilon,
 *   beneficiaries, and type, linked via network.affects_constraints.
 *   Epsilon's referent here is the standing QWERTY arrangement as this
 *   reading assesses it — diffuse efficiency losses borne by text producers —
 *   never the hypothetical Dvorak world this reading does not endorse. The
 *   claimed type (piton) and the authored metrics are independent facts: the
 *   claim states what this reading believes is structurally true; the metrics
 *   state what is descriptively true of the arrangement's operation.
 *
 * KEY AGENTS:
 *   - keyboard_standards_bodies: agenda setter (institutional/constrained) — ratifies the inherited layout in published standards; could revise but has no mandate or mechanism to compel adoption, so documentation follows practice
 *   - keyboard_manufacturers: rider-beneficiary (organized/constrained) — collects standardized tooling and predictable demand from the shared mapping without administering it
 *   - high_volume_text_workers: primary cost bearer (powerless/trapped) — transcriptionists, data-entry operators, writers, and programmers absorbing the layout's speed and error costs every working hour
 *   - general_keyboard_users: mass dual-positioned participant (moderate/mobile) — receives the universal-standard good, carries small unnoticed losses, and constitutes the mutual expectation that blocks alternatives
 *   - employers_of_text_workers: dual-positioned payer/beneficiary (powerful/constrained) — buys pre-trained labor and loses output to the same layout's inefficiencies; cannot coordinate retraining alone
 *   - alternative_layout_advocates: excluded promoter (powerless/trapped) — Dvorak/Colemak designers with no seat in any standards, procurement, or curriculum forum
 *   - ergonomics_researchers: analytical observer (analytical/analytical) — produces the contested performance evidence the sibling readings quarrel over
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_mechanism__lock_in_reading, 0.4).
domain_priors:suppression_score(qwerty_persistence_mechanism__lock_in_reading, 0.35).
domain_priors:theater_ratio(qwerty_persistence_mechanism__lock_in_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_mechanism__lock_in_reading, piton).
narrative_ontology:human_readable(qwerty_persistence_mechanism__lock_in_reading, "QWERTY Layout Persistence (Path-Dependent Lock-In Reading)").
narrative_ontology:topic_domain(qwerty_persistence_mechanism__lock_in_reading, "economic_history/technology/path_dependence").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_mechanism__lock_in_reading, '77448531-ce66-402e-bf42-d8bd6b7f21f9').
narrative_ontology:cs_kernel_codification('77448531-ce66-402e-bf42-d8bd6b7f21f9', distributed).
narrative_ontology:cs_authority_grounding('77448531-ce66-402e-bf42-d8bd6b7f21f9', expertise).
narrative_ontology:cs_interpretation_layer_present('77448531-ce66-402e-bf42-d8bd6b7f21f9').
narrative_ontology:cs_reading_relation('77448531-ce66-402e-bf42-d8bd6b7f21f9', qwerty_persistence_mechanism__naturalization_reading, coexists_with).
narrative_ontology:cs_reading_relation('77448531-ce66-402e-bf42-d8bd6b7f21f9', qwerty_persistence_mechanism__beneficiary_extraction_reading, coexists_with).
narrative_ontology:cs_axiom('77448531-ce66-402e-bf42-d8bd6b7f21f9', foundational, material_technical_inferiority_persists).
narrative_ontology:cs_axiom_status(material_technical_inferiority_persists, holdable).
narrative_ontology:cs_axiom_grounding('77448531-ce66-402e-bf42-d8bd6b7f21f9', material_technical_inferiority_persists, empirically_contingent).
narrative_ontology:cs_axiom('77448531-ce66-402e-bf42-d8bd6b7f21f9', foundational, spontaneous_coordination_failure_suffices).
narrative_ontology:cs_axiom_status(spontaneous_coordination_failure_suffices, holdable).
narrative_ontology:cs_axiom_grounding('77448531-ce66-402e-bf42-d8bd6b7f21f9', spontaneous_coordination_failure_suffices, empirically_contingent).
narrative_ontology:cs_reference_frame('77448531-ce66-402e-bf42-d8bd6b7f21f9', suboptimal_path_dependent_equilibrium).
narrative_ontology:cs_drift_state('77448531-ce66-402e-bf42-d8bd6b7f21f9', post_fable_of_the_keys_critique, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('77448531-ce66-402e-bf42-d8bd6b7f21f9', '').
narrative_ontology:cs_kernel_id(qwerty_persistence_mechanism__lock_in_reading, qwerty_persistence_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__lock_in_reading, keyboard_manufacturers).
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__lock_in_reading, general_keyboard_users).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__lock_in_reading, high_volume_text_workers).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__lock_in_reading, alternative_layout_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__lock_in_reading, employers_of_text_workers).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__lock_in_reading, general_keyboard_users).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__lock_in_reading, employers_of_text_workers).
narrative_ontology:constraint_vindicates(qwerty_persistence_mechanism__lock_in_reading, path_dependence_hypothesis).
narrative_ontology:constraint_vindicates(qwerty_persistence_mechanism__lock_in_reading, increasing_returns_market_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain the published keyboard-layout standards that document the existing mapping. They ratify what the market already does rather than direct it; revising the standard would strand every trained typist and every manufacturer's tooling, and the bodies have no mandate or mechanism to compel adoption of a revision. Their publications follow practice more than they lead it.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, keyboard_standards_bodies, agenda_setter,
    institutional, generational, constrained, global).

% Build keyboards and integrated devices to the inherited layout because that is what every buyer already knows how to use. Standardized tooling and predictable replacement demand flow from the shared mapping. Shipping an alternative layout means educating a market that sees no reason to move; some firms hedge with switchable or dual-legended products, but the mainline business rides the inherited standard.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, keyboard_manufacturers, beneficiary,
    organized, biographical, constrained, global).

% Transcriptionists, data-entry operators, writers, and programmers whose income depends on sustained text throughput. They absorb the layout's speed and error costs every working hour. Retraining personally would mean weeks or months below full speed while every shared machine, colleague, and temporary worker around them stays on the old mapping — a cost no individual can recover.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, high_volume_text_workers, payer,
    powerless, biographical, trapped, global).

% Hundreds of millions of casual users who learned the mapping in school or by absorption and can sit down at any device in the world and type. They carry small individual efficiency losses they rarely notice, and they are the mass whose expectations make any alternative layout commercially unattractive. Their own switching costs are low, which is precisely why their staying is decisive.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, general_keyboard_users, beneficiary,
    moderate, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(qwerty_persistence_mechanism__lock_in_reading, general_keyboard_users, payer).

% Firms that buy keyboards, hire pre-trained typists, and absorb the productivity shortfall of their heaviest text producers. They benefit from a labor market that arrives already trained on one layout, and they lose output to the same layout's inefficiencies. Coordinating a workforce-wide retraining is a public-good problem inside the firm: the pioneer pays the disruption while the benefit accrues industry-wide.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, employers_of_text_workers, payer,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(qwerty_persistence_mechanism__lock_in_reading, employers_of_text_workers, beneficiary).

% Designers and communities behind alternative mappings such as Dvorak and Colemak who have spent decades documenting efficiency and comfort gains. They hold no seat in any standards body, procurement process, or curriculum board; their proposals die not by rejection but by never reaching a decision point, because adoption requires simultaneous movement by manufacturers, schools, and employers that no forum convenes.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, alternative_layout_advocates, excluded,
    powerless, generational, trapped, global).

% Academic specialists who measure typing performance, error rates, and musculoskeletal outcomes across layouts. They produce the contested evidence base that the rival explanations of the layout's dominance quarrel over, and they hold no stake in which mapping ships.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, ergonomics_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qwerty_persistence_mechanism__lock_in_reading, diffuse).
narrative_ontology:fixing_cost_class(qwerty_persistence_mechanism__lock_in_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides one shared key-to-symbol mapping so that any trained typist can operate any keyboard and any manufacturer can build one hardware layout for the entire market — solving the skill-to-hardware matching problem across hundreds of millions of users without negotiation.
% TRANSFER_FUNCTION: Moves the burden of adaptation forward onto each new cohort of typists, who must absorb the inherited layout's learning costs, and concentrates the efficiency shortfall on the highest-volume text producers; the standardization surplus is distributed widely and collected by no one.
% ABSENT_VOICES: Alternative-layout designers, ergonomics researchers skeptical of the inherited mapping, and keyboard purchasers at large have no seat in any standards, procurement, or curriculum forum. The layout persists by default rather than by decision, so the objection never finds a room to be raised in — the unanimity around the standard reflects the absence of any venue where it could be contested.
% DISAPPEARANCE_RATIONALE: If the QWERTY mapping vanished overnight, every keyboard on earth would be blank or illegible to its users; text entry would halt until a successor mapping propagated through manufacturing, retail, and retraining. Hardware tooling, school curricula, hiring expectations, and skill certification would all rearrange around whichever successor reached critical mass first — the arrangement's disappearance forces a wholesale re-coordination, not a return to a prior state.
% FOUNDING_PROBLEM: Mechanical typebar jamming on 1870s Sholes typewriters: adjacent frequently-used letter pairs struck in quick succession would collide and seize the mechanism, and the layout (on the canonical account) separated common pairs so typebars could settle between strokes.
% FOUNDING_PROBLEM_CORROBORATION: Historians of technology and the Sholes patent record corroborate the jamming-era origin from outside any beneficiary set; the problem's death is attested by the physical extinction of typebar mechanisms, which no party disputes. Attestation that the founding problem is gone therefore rests on engineering history rather than on the arrangement's beneficiaries — though the omega jamming_origin_story_accuracy records that the origin narrative itself is historiographically contested, and the sibling readings dispute whether the layout ever optimally served even that problem.
narrative_ontology:disappearance_verdict(qwerty_persistence_mechanism__lock_in_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence_mechanism__lock_in_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_mechanism__lock_in_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(qwerty_persistence_mechanism__lock_in_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence_mechanism__lock_in_reading, 0.4, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence_mechanism__lock_in_reading_tests).
:- end_tests(qwerty_persistence_mechanism__lock_in_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.40 at interval end): the arrangement imposes real, measurable efficiency losses on text producers, but nothing is collected — the losses dissipate as unrecovered time, which caps epsilon well below extractive arrangements where a seat receives the flow. Suppression is moderate-low (0.35) and reflects a structural property of the arrangement, not an enforcement budget: alternative layouts remain legally and technically available, but exit is individually irrational because every shared machine, colleague, and hiring pipeline stays on the inherited mapping. No suppression_requirement time series is authored because there is no enforcement machinery whose buildup or decay this story tracks — the static scalar carries the whole picture, per the alignment rule. Theater ratio is honestly low (0.15): nobody performs QWERTY compliance ceremonially; the arrangement persists inertially, and the slight rise tracks growing folklore and advocacy activity around the layout rather than functional maintenance. Accessibility collapse is moderate (0.40): understanding the situation does not collapse alternatives — software layouts are one settings-panel away — but social cost keeps them unexercised. Resistance is low (0.20): episodic advocacy campaigns and isolated employer experiments, never a sustained movement, because the diffuse cost structure gives no bearer enough stake to fight. The piton claim rests on the cost-asymmetry test, not on theatricality: the administrators of the standard could change it, but the fix (retraining the global installed base) costs vastly more than any of them bears, and the benefit of fixing is uncapturable — which is also why the theater ratio stays low while the arrangement stays stuck.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute divergent types from identical structural data. From the high_volume_text_worker seat the arrangement is a daily tax with no viable personal exit — an imposed cost experienced as extraction. From the keyboard_manufacturer seat it is stable demand and tooling amortization — experienced as ordinary commercial order. From the keyboard_standards_body seat it is a settled document requiring no action. From the ergonomics_researcher seat it is a textbook coordination failure visible in full. The engine computes these per-seat classifications from power, exit, and directional position; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   High_volume_text_workers are declared victims with trapped exit: they bear the concentrated efficiency cost and cannot leave, placing them near the full-target end. Alternative_layout_advocates are declared victims of a different kind — their alternative cannot reach critical mass, so the standard's dominance is the cost they bear. Keyboard_manufacturers and general_keyboard_users are declared beneficiaries: the former collect tooling standardization, the latter the universal-interoperability good, placing them toward the beneficiary end; general_keyboard_users carry a secondary payer position reflecting their small diffuse losses. Employers sit mid-range: they pay productivity shortfalls and collect a pre-trained labor pool. Suppression, as a raw structural property, is not scaled by power or scope — only extractiveness is scaled, by directionality and the global scope's verification difficulty. One directionality override is authored: the institutional seat (keyboard_standards_bodies) is pinned at d=0.5 because the derivation chain has no neutral-administrator category — the body neither collects nor pays, and the canonical fallback for institutional actors could skew it toward quasi-beneficiary; the override records the administratively-neutral relationship the structural data cannot express.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing mechanical typebar jamming on 1870s Sholes machines — is dead: no typebar mechanism has been manufactured in generations, and no party disputes the machinery is gone. The R5 interview records founding_problem_status=dead against disappearance_verdict=world_rearranges, and that mismatch is exactly the signal the piton classification predicts: the arrangement outlived its mandate and now persists on inertia alone. The classification prevents two opposite misreadings. Reading it as a snare fails because no seat captures the flow — gain_flow is authored 'diffuse' as an affirmative claim after checking every named seat; the efficiency losses accrue to no one. Reading it as a rope fails because participants are not net beneficiaries relative to feasible alternatives — the coordination good is real, but the reading's core premise is that a superior coordination point was reachable and was missed. The piton label locates the pathology where this reading says it is: in the cost-asymmetry between administrators who could change the standard and a fix whose costs are concentrated on whoever moves first while its benefits are diffuse and uncapturable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_allocation,
    'Is QWERTY''s persistence best explained by spontaneous coordination failure despite inferiority (this reading), by genuine adequacy under fair competition (naturalization_reading), or by active incumbent defense of training investments and market position (beneficiary_extraction_reading)?',
    'Comparative structural evidence: documented episodes of manufacturers or incumbents suppressing alternative layouts favor the extraction reading; small measured switching costs combined with parity in head-to-head trials favor the naturalization reading; material measured efficiency gaps with no identifiable suppressor and repeated failures of voluntary transition favor this reading.',
    'Adopting the extraction reading converts this story into a tangled_rope with named rent-collectors and sharply higher effective extraction; adopting the naturalization reading collapses epsilon toward the information_standard coordination floor and reclassifies toward rope; only this reading sustains the piton classification with diffuse gains.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_allocation, conceptual, 'Which of three rival explanations of QWERTY persistence this constraint instantiates; this file is the lock-in instantiation.').

omega_variable(
    dvorak_efficiency_gap_magnitude,
    'How large is the real efficiency gap between QWERTY and optimized layouts such as Dvorak or Colemak for professional text entry?',
    'Controlled longitudinal retraining studies of working professionals with productivity and error telemetry, insulated from advocacy sponsorship on either side.',
    'A near-zero gap collapses this reading''s epsilon toward the coordination-cost floor and effectively hands the kernel to naturalization_reading; a material gap confirms the inferiority premise this reading rests on and keeps the elevated extractiveness trajectory live.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dvorak_efficiency_gap_magnitude, empirical, 'Magnitude of the technical-inferiority premise underlying the lock-in account.').

omega_variable(
    switching_cost_composition,
    'Is the transition barrier composed mainly of retraining cost, hardware and tooling inertia, or pure mutual expectation — and would a coordinated employer-and-schools transition program break it?',
    'Natural experiments: regional school-system adoptions of alternative layouts and large-employer pilot conversions, tracked over a decade for spillover and reversal.',
    'If a coordinated program plausibly breaks the lock, the arrangement is better read as convertible through a deliberate transition (scaffold-shaped remedy) than as a terminal inertial residue; if the barrier is irreducibly mutual expectation, the inertial classification stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(switching_cost_composition, empirical, 'Composition of the transition barrier and feasibility of coordinated escape.').

omega_variable(
    jamming_origin_story_accuracy,
    'Was QWERTY actually designed to solve typebar jamming on 1870s mechanical typewriters, or is the jamming rationale a retrospective reconstruction?',
    'Sholes patent and correspondence archives, reconstruction of early prototype mechanics, and historiographic review of the design-decision record.',
    'If the founding-problem narrative is itself retrospective, the genealogy shifts from ''dead problem'' to ''possibly never the operative problem,'' strengthening the case that the arrangement persists without ever having had the function its story claims.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(jamming_origin_story_accuracy, empirical, 'Historical accuracy of the typebar-jamming founding narrative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_mechanism__lock_in_reading, 1873, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwerty_lockin_tr_t1873, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 1873, 0.02).
narrative_ontology:measurement(qwerty_lockin_tr_t1910, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 1910, 0.05).
narrative_ontology:measurement(qwerty_lockin_tr_t1940, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 1940, 0.08).
narrative_ontology:measurement(qwerty_lockin_tr_t1970, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(qwerty_lockin_tr_t1990, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 1990, 0.12).
narrative_ontology:measurement(qwerty_lockin_tr_t2010, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 2010, 0.14).
narrative_ontology:measurement(qwerty_lockin_tr_t2026, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 2026, 0.15).

% Extraction over time
narrative_ontology:measurement(qwerty_lockin_be_t1873, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 1873, 0.05).
narrative_ontology:measurement(qwerty_lockin_be_t1910, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 1910, 0.15).
narrative_ontology:measurement(qwerty_lockin_be_t1940, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 1940, 0.22).
narrative_ontology:measurement(qwerty_lockin_be_t1970, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 1970, 0.3).
narrative_ontology:measurement(qwerty_lockin_be_t1990, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 1990, 0.35).
narrative_ontology:measurement(qwerty_lockin_be_t2010, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 2010, 0.38).
narrative_ontology:measurement(qwerty_lockin_be_t2026, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 2026, 0.4).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(qwerty_persistence_mechanism__lock_in_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_mechanism__lock_in_reading, information_standard).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__lock_in_reading, qwerty_persistence_mechanism__naturalization_reading).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__lock_in_reading, qwerty_persistence_mechanism__beneficiary_extraction_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'QWERTY persists' decomposes into three structurally distinct claims per the epsilon-invariance principle. naturalization_reading holds the lowest epsilon (adequacy; rivals lapsed fairly) and classifies toward rope; this lock_in_reading holds moderate epsilon (real inferiority, no capturer) and claims piton; beneficiary_extraction_reading holds the highest epsilon (active incumbent defense, named rent-collectors) and classifies toward tangled_rope. The readings are mutually linked via affects_constraints because they cite one another: the naturalization critique (small measured gaps) is deployed against this reading, and documented suppression episodes would transfer weight from this reading to the extraction reading. Each file carries its own beneficiaries, victims, and claimed type; no story hedges epsilon across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(qwerty_persistence_mechanism__lock_in_reading, institutional, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
