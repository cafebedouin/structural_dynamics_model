% ============================================================================
% CONSTRAINT STORY: qwerty_persistence__lapsed_alternatives_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence__lapsed_alternatives_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: qwerty_persistence__lapsed_alternatives_reading
 *   human_readable: QWERTY Coordination Equilibrium (Lapsed Alternatives Reading)
 *   domain: technology_history/industrial_standards/path_dependence
 *
 * SUMMARY:
 *   The QWERTY letter arrangement has dominated keyboard design for over a
 *   century. This story authors the lapsed_alternatives_reading of that
 *   persistence: the arrangement holds because each typist, manufacturer,
 *   educator, and platform vendor does best by matching what everyone else
 *   already uses, and rival layouts such as Dvorak and Colemak — though
 *   freely available, preinstalled in major operating systems, and teachable
 *   — never accumulate enough simultaneous adopters to become
 *   self-sustaining. On this reading there is no defender, no enforcer, and
 *   no collector: the arrangement is a solved coordination problem whose
 *   solution outlived its original reason (mechanical typebar jamming) and
 *   persists on interoperability value alone. The sibling reading of the same
 *   kernel is authored in a separate file and linked through the network
 *   section; this file does not adjudicate between them. KEY AGENTS (by
 *   structural relationship): - touch_typists: primary coordinated population
 *   (moderate/constrained) — learn once, use everywhere; bear the entire
 *   private switching cost if defecting - keyboard_manufacturers: passive
 *   beneficiary (organized/arbitrage) — produce to dominant demand, tooling
 *   largely layout-agnostic - software_platform_vendors: passive beneficiary
 *   (institutional/mobile) — ship the default plus alternatives at trivial
 *   marginal support cost - typing_educators: incidental beneficiary
 *   (moderate/constrained) — curricula and certifications built around the
 *   dominant layout - alternative_layout_advocates: cost-bearing minority
 *   (moderate/mobile) — demonstrate exit is feasible; their failure to
 *   cascade is this reading's central datum -
 *   occupational_health_researchers: excluded voice (moderate/analytical) —
 *   argue the equilibrium entrenches avoidable strain costs; hold no
 *   procurement seat - path_dependence_economists: analytical observer
 *   (analytical/generational) — evaluate coordination versus defense accounts
 *   of the persistence
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence__lapsed_alternatives_reading, 0.08).
domain_priors:suppression_score(qwerty_persistence__lapsed_alternatives_reading, 0.05).
domain_priors:theater_ratio(qwerty_persistence__lapsed_alternatives_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence__lapsed_alternatives_reading, rope).
narrative_ontology:human_readable(qwerty_persistence__lapsed_alternatives_reading, "QWERTY Coordination Equilibrium (Lapsed Alternatives Reading)").
narrative_ontology:topic_domain(qwerty_persistence__lapsed_alternatives_reading, "technology_history/industrial_standards/path_dependence").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence__lapsed_alternatives_reading, '03d2cd3c-095c-4366-be97-d0b9b138e12b').
narrative_ontology:cs_kernel_codification('03d2cd3c-095c-4366-be97-d0b9b138e12b', distributed).
narrative_ontology:cs_authority_grounding('03d2cd3c-095c-4366-be97-d0b9b138e12b', practice).
narrative_ontology:cs_interpretation_layer_present('03d2cd3c-095c-4366-be97-d0b9b138e12b').
narrative_ontology:cs_reading_relation('03d2cd3c-095c-4366-be97-d0b9b138e12b', qwerty_persistence__incumbent_preservation_reading, coexists_with).
narrative_ontology:cs_axiom('03d2cd3c-095c-4366-be97-d0b9b138e12b', foundational, coordination_value_sustains_persistence).
narrative_ontology:cs_axiom_status(coordination_value_sustains_persistence, holdable).
narrative_ontology:cs_axiom_grounding('03d2cd3c-095c-4366-be97-d0b9b138e12b', coordination_value_sustains_persistence, empirically_contingent).
narrative_ontology:cs_axiom('03d2cd3c-095c-4366-be97-d0b9b138e12b', secondary, non_adoption_reveals_insufficient_net_benefit).
narrative_ontology:cs_axiom_status(non_adoption_reveals_insufficient_net_benefit, holdable).
narrative_ontology:cs_axiom_grounding('03d2cd3c-095c-4366-be97-d0b9b138e12b', non_adoption_reveals_insufficient_net_benefit, empirically_contingent).
narrative_ontology:cs_reference_frame('03d2cd3c-095c-4366-be97-d0b9b138e12b', coordination_equilibrium_standard).
narrative_ontology:cs_drift_state('03d2cd3c-095c-4366-be97-d0b9b138e12b', contemporary_input_diversification, gap(stable, minor, true)).
narrative_ontology:cs_created_at('03d2cd3c-095c-4366-be97-d0b9b138e12b', '').
narrative_ontology:cs_kernel_id(qwerty_persistence__lapsed_alternatives_reading, qwerty_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence__lapsed_alternatives_reading, touch_typists).
narrative_ontology:constraint_beneficiary(qwerty_persistence__lapsed_alternatives_reading, keyboard_manufacturers).
narrative_ontology:constraint_beneficiary(qwerty_persistence__lapsed_alternatives_reading, software_platform_vendors).
narrative_ontology:constraint_beneficiary(qwerty_persistence__lapsed_alternatives_reading, typing_educators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(qwerty_persistence__lapsed_alternatives_reading, alternative_layout_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Learn one letter arrangement during schooling and reuse that skill on every keyboard they encounter for the rest of their working lives. If they adopt a different layout they must retrain over weeks, temporarily lose speed, and cope with unfamiliar machines at shared desks, kiosks, and colleagues' computers. Most never attempt the change; a minority do and report the transition period as the main obstacle.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, touch_typists, beneficiary,
    moderate, biographical, constrained, global).

% Build keyboards predominantly in the dominant arrangement because that is what buyers expect; keycap tooling and firmware accommodate other arrangements at modest cost, and some models ship with switchable legends. They neither promote nor defend any particular arrangement — they follow the demand curve. If orders for an alternative layout appeared at scale, production lines would retool within a product cycle.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, keyboard_manufacturers, beneficiary,
    organized, generational, arbitrage, global).

% Ship the dominant layout as the factory default on every device and include alternative layouts as settings options at negligible engineering cost. The default choice tracks what incoming users already know; changing the default would generate support burdens and user complaints disproportionate to any efficiency argument.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, software_platform_vendors, beneficiary,
    institutional, generational, mobile, global).

% Teach courses, run certification tests, and publish instructional materials keyed to the dominant arrangement. Adopting a different layout would strand curricula, require retraining instructors, and disconnect graduates from employer expectations, so program updates follow the installed base rather than lead it.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, typing_educators, beneficiary,
    moderate, biographical, constrained, global).

% Use and promote layouts they judge faster or more comfortable, maintain communities and tutorials, and demonstrate that switching is physically feasible. Each adopter pays the full retraining cost personally and accepts thinner compatibility — fewer preconfigured machines, occasional awkwardness on shared hardware — and their numbers stay too small to shift defaults or manufacturer product lines.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, alternative_layout_advocates, payer,
    moderate, biographical, mobile, global).

% Study musculoskeletal strain in keyboard work and argue that letter arrangement affects finger travel and posture. They publish recommendations favoring alternative arrangements but hold no seat in hardware procurement, curriculum committees, or product default decisions, so their findings enter the conversation only as citations.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, occupational_health_researchers, excluded,
    moderate, generational, analytical, global).

% Analyze why the arrangement persists and whether its persistence reflects efficiency or historical accident, using adoption data, switching-cost estimates, and the contested early efficiency studies. They take no part in the arrangement beyond writing about it.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, path_dependence_economists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qwerty_persistence__lapsed_alternatives_reading, diffuse).
narrative_ontology:fixing_cost_class(qwerty_persistence__lapsed_alternatives_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a single shared letter arrangement so that typing skill, hardware, software defaults, and instructional materials interoperate across employers, devices, and generations without renegotiation.
% TRANSFER_FUNCTION: Moves retraining time and compatibility friction onto any individual who defects to a different layout, while the resulting interoperability savings accrue to the whole installed base; nothing of value is transferred to any administering party because none administers it.
% ABSENT_VOICES: Occupational-health researchers and keyboard workers with strain injuries would object that the equilibrium entrenches an arrangement with documented ergonomic costs, but they hold no seat where defaults, curricula, or product lines are decided; alternative-layout advocates speak openly yet carry no decisive weight anywhere the default is set.
% DISAPPEARANCE_RATIONALE: Hundreds of millions of trained typists would wake to keyboards they could not use; keycap legends, firmware defaults, school curricula, and hiring expectations would all scramble until a replacement convention crystallized, destroying the accumulated training investment in the interim.
% FOUNDING_PROBLEM: Mechanical typebar jamming in early typewriters: striking nearby keys in rapid succession made adjacent typebars collide, so the arrangement separated common letter pairs; a fast-typing sales demonstration was also part of the design brief.
% FOUNDING_PROBLEM_CORROBORATION: Technology historians and the engineering literature on Sholes's prototypes attest the jamming problem, and its obsolescence is attested by the plain physical fact that no modern input device contains typebars; patent records and museum collections corroborate independently of any party that benefits from the arrangement today.
narrative_ontology:disappearance_verdict(qwerty_persistence__lapsed_alternatives_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence__lapsed_alternatives_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence__lapsed_alternatives_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(qwerty_persistence__lapsed_alternatives_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence__lapsed_alternatives_reading, 0.08, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence__lapsed_alternatives_reading_tests).
:- end_tests(qwerty_persistence__lapsed_alternatives_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is low (0.08) because the only burden the arrangement imposes is the switching cost facing a would-be defector — no party collects anything from its operation. Suppression is near zero (0.05): rival layouts ship in every major operating system, are legal, documented, and teachable; nothing coerces conformity beyond the network effect itself, and suppression is authored as a raw structural property, unscaled by power or scope. Theater is near zero (0.05): maintenance consists of manufacturing to specification and teaching the layout — functional activity throughout. Accessibility_collapse is low (0.15), and this is the profile's distinctive feature: understanding that the arrangement is arbitrary does not close off alternatives — they remain fully accessible and simply fail to propagate, unlike a natural law whose alternatives collapse on comprehension. Resistance is low (0.10): no constituency organizes against the arrangement; advocacy energy flows toward promoting rivals, not attacking the incumbent. The claimed type is rope on structural grounds — a genuine collective-action solution with net beneficiaries and no suppressed exits — authored independently of these metric values. The temporal series share one grid (t = years since approximately 1905, points every 20 years): base_extractiveness creeps upward as the installed base grows and raises each defector's switching cost; theater_ratio drifts negligibly as origin lore accumulates cultural weight. There are no cyclical dynamics: the process is monotone slow drift, not oscillation, so no intermittent-reinforcement mechanism is implicated.
 *
 * PERSPECTIVAL GAP:
 *   From the touch typist's seat the arrangement is invisible infrastructure — learned once, never thought about again; a per-seat classification computed there should come out benign. From the advocate's seat the same arrangement is a wall: their preferred layout cannot catch on, and the switching cost they must personally pay feels imposed by everyone else's staying put. From the manufacturer's seat it is barely a constraint at all — a demand forecast. One structure, three different lived experiences; the engine computes that divergence from the power, horizon, and exit data rather than from this claim. Across readings, the identical historical record supports a benign classification in this file and a materially more extractive one in the sibling file — that divergence is the kernel contest and is routed to the omega variable, not reconciled here.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries declared are the coordinated population and its suppliers; no victims are declared because this reading finds no asymmetric extraction — the costs the arrangement imposes are the symmetric price of interoperability, borne by anyone who departs and by no one who conforms. Alternative-layout advocates derive the highest directionality among the seated agents: they visibly bear switching costs, but by choice and without coercion, which keeps their effective burden modest. Gains accrue diffusely to every participant in the form of interoperability, so no seat captures them — hence the diffuse receipt declaration. Global spatial scope raises verification difficulty at the margin, but with base extraction this low the scope amplification has very little to amplify.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — mechanical typebar collision — disappeared with the electromechanical keyboard, so the genealogy interview returns founding_problem_status dead while the disappearance verdict returns world_rearranges; the mismatch flag will fire, and this analysis is its resolution. The arrangement did not atrophy into performance: its original function was replaced by a successor function (interoperability) that is fully live, cheaply maintained, and experienced as valuable by every seated agent — theater_ratio stays near zero across the whole interval precisely because nothing theatrical is needed to hold it. A degraded constraint persists because fixing costs more than anyone affected bears; this arrangement persists because it works. The honest residual risk is the reverse of atrophy: if text input migrates to voice or heavy prediction, the coordination function itself could lapse quickly, and the arrangement would decay without defenders — the signature of a live coordination good, not a preserved shell.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading of the qwerty_persistence kernel: does the arrangement persist through coordination value alone (this reading, lapsed_alternatives_reading), or through active defense of capital investments by incumbents (sibling reading, incumbent_preservation_reading)?',
    'Counterfactual and archival tests: the sibling reading predicts documented episodes of manufacturers, employers, or standards bodies suppressing or refusing to support alternative layouts; this reading predicts adoption dynamics fit tipping-model curves with no suppression necessary. Locate or fail to locate such episodes in trade press, corporate archives, and procurement records.',
    'If the sibling reading is right, epsilon rises substantially, beneficiaries concentrate among incumbent equipment holders, and the classification shifts toward tangled_rope or snare; if this reading holds, the classification stays rope with diffuse gains and no victim set.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, empirical, 'Which persistence mechanism — coordination equilibrium or incumbent defense — actually operates for the QWERTY arrangement.').

omega_variable(
    dvorak_advantage_magnitude,
    'How large is the real efficiency and ergonomic advantage of alternative layouts over the dominant arrangement?',
    'Preregistered controlled trials with modern text-entry tasks, independent of the contested mid-century studies (Dvorak''s own Navy trials versus the later reanalysis of them).',
    'A large verified advantage means the switching-cost burden blocks a substantial welfare gain, pushing epsilon upward and creating pressure toward tangled_rope; a negligible advantage confirms the current low epsilon and the near-optimality of the standing arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dvorak_advantage_magnitude, empirical, 'Size of the forgone benefit that switching costs are holding back.').

omega_variable(
    switching_cost_symmetry,
    'Are switching costs symmetric across user populations, as this reading expects, or concentrated on identifiable groups (multilingual typists, users of assistive input methods, shared-equipment workers)?',
    'Segmented cost accounting: survey retraining time, compatibility friction, and equipment availability across user segments rather than averaging over the whole installed base.',
    'Symmetric costs confirm the empty victim set and the rope classification; concentrated costs would introduce a de facto victim group and push toward tangled_rope even without any capturing seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(switching_cost_symmetry, empirical, 'Whether the coordination cost burden is evenly shared or silently concentrated.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence__lapsed_alternatives_reading, 0, 120).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t0, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 0, 0.02).
narrative_ontology:measurement_basis(qwer_tr_t0, observed).
narrative_ontology:measurement(qwer_tr_t20, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 20, 0.02).
narrative_ontology:measurement_basis(qwer_tr_t20, observed).
narrative_ontology:measurement(qwer_tr_t40, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 40, 0.03).
narrative_ontology:measurement_basis(qwer_tr_t40, observed).
narrative_ontology:measurement(qwer_tr_t60, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 60, 0.03).
narrative_ontology:measurement_basis(qwer_tr_t60, observed).
narrative_ontology:measurement(qwer_tr_t80, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 80, 0.04).
narrative_ontology:measurement_basis(qwer_tr_t80, observed).
narrative_ontology:measurement(qwer_tr_t100, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 100, 0.04).
narrative_ontology:measurement_basis(qwer_tr_t100, observed).
narrative_ontology:measurement(qwer_tr_t120, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 120, 0.05).
narrative_ontology:measurement_basis(qwer_tr_t120, observed).

% Extraction over time
narrative_ontology:measurement(qwer_be_t0, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 0, 0.04).
narrative_ontology:measurement_basis(qwer_be_t0, observed).
narrative_ontology:measurement(qwer_be_t20, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 20, 0.05).
narrative_ontology:measurement_basis(qwer_be_t20, observed).
narrative_ontology:measurement(qwer_be_t40, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 40, 0.06).
narrative_ontology:measurement_basis(qwer_be_t40, observed).
narrative_ontology:measurement(qwer_be_t60, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 60, 0.07).
narrative_ontology:measurement_basis(qwer_be_t60, observed).
narrative_ontology:measurement(qwer_be_t80, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 80, 0.07).
narrative_ontology:measurement_basis(qwer_be_t80, observed).
narrative_ontology:measurement(qwer_be_t100, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 100, 0.08).
narrative_ontology:measurement_basis(qwer_be_t100, observed).
narrative_ontology:measurement(qwer_be_t120, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 120, 0.08).
narrative_ontology:measurement_basis(qwer_be_t120, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(qwerty_persistence__lapsed_alternatives_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence__lapsed_alternatives_reading, information_standard).
narrative_ontology:affects_constraint(qwerty_persistence__lapsed_alternatives_reading, incumbent_preservation_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'QWERTY persistence' conflates two structurally distinct claims, decomposed per the epsilon-invariance principle into a two-file constraint family. This file (lapsed_alternatives_reading) authors the coordination-equilibrium claim: epsilon approximately 0.08 set by switching costs alone, diffuse beneficiaries, no victims, no enforcement machinery. The sibling file (incumbent_preservation_reading) authors the defense claim: materially higher epsilon, concentrated beneficiaries among incumbent equipment interests, and alleged suppression episodes as load-bearing evidence. The analytical link runs in both directions: coordination dynamics are the null hypothesis against which defense evidence is tested, and alleged defense episodes are the anomalies the coordination account must absorb. Each file keeps a single stable epsilon; neither hedges across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
