% ============================================================================
% CONSTRAINT STORY: qwerty_persistence_mechanism__lock_in_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: qwerty_persistence_mechanism__lock_in_reading
 *   human_readable: QWERTY Keyboard Standard as Coordination Failure (Lock-In Reading)
 *   domain: economic_history/technology_studies/path_dependence
 *
 * SUMMARY:
 *   QWERTY is the canonical case study in path-dependence economics. This
 *   story instantiates the LOCK-IN READING of the kernel: the layout persists
 *   not because any actor benefits from suppressing alternatives (the
 *   beneficiary_extraction_reading) and not because it turned out to be
 *   genuinely adequate under fair competition (the naturalization_reading),
 *   but because network externalities created a coordination equilibrium that
 *   is individually rational to maintain and collectively costly to leave,
 *   with no concentrated party positioned to capture rents from that gap. The
 *   mechanism is Arthur/David-style increasing-returns lock-in: early
 *   adoption advantage compounds through training and hardware sunk costs
 *   until switching costs exceed switching benefits for every individual
 *   actor, even though a coordinated switch would benefit the population in
 *   aggregate.
 *
 * KEY AGENTS:
 *   - keyboard_manufacturers_incumbent_tooling: incidental beneficiary of standard stability, not its architect
 *   - typing_instruction_industry: incidental beneficiary, teaches whatever the standard is
 *   - typists_general_population: diffuse victims of an efficiency gap no one profits from closing
 *   - would_be_alternative_layout_adopters: excluded by coordination math, not by gatekeeping
 *   - device_and_os_platform_vendors: administers the default but does not prohibit alternatives
 *   - economic_historians: analytical observers of the contested mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_mechanism__lock_in_reading, 0.18).
domain_priors:suppression_score(qwerty_persistence_mechanism__lock_in_reading, 0.12).
domain_priors:theater_ratio(qwerty_persistence_mechanism__lock_in_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_mechanism__lock_in_reading, tangled_rope).
narrative_ontology:human_readable(qwerty_persistence_mechanism__lock_in_reading, "QWERTY Keyboard Standard as Coordination Failure (Lock-In Reading)").
narrative_ontology:topic_domain(qwerty_persistence_mechanism__lock_in_reading, "economic_history/technology_studies/path_dependence").

domain_priors:requires_active_enforcement(qwerty_persistence_mechanism__lock_in_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_mechanism__lock_in_reading, '38fb4e4f-5298-420c-be3d-f0098c7a74d8').
narrative_ontology:cs_kernel_codification('38fb4e4f-5298-420c-be3d-f0098c7a74d8', distributed).
narrative_ontology:cs_authority_grounding('38fb4e4f-5298-420c-be3d-f0098c7a74d8', distributed).
narrative_ontology:cs_reading_relation('38fb4e4f-5298-420c-be3d-f0098c7a74d8', qwerty_persistence_mechanism__naturalization_reading, coexists_with).
narrative_ontology:cs_reading_relation('38fb4e4f-5298-420c-be3d-f0098c7a74d8', qwerty_persistence_mechanism__beneficiary_extraction_reading, coexists_with).
narrative_ontology:cs_axiom('38fb4e4f-5298-420c-be3d-f0098c7a74d8', foundational, network_lock_in_without_concentrated_capture).
narrative_ontology:cs_axiom_status(network_lock_in_without_concentrated_capture, holdable).
narrative_ontology:cs_axiom_grounding('38fb4e4f-5298-420c-be3d-f0098c7a74d8', network_lock_in_without_concentrated_capture, empirically_contingent).
narrative_ontology:cs_axiom('38fb4e4f-5298-420c-be3d-f0098c7a74d8', secondary, coordination_threshold_exceeds_individual_switching_incentive).
narrative_ontology:cs_axiom_status(coordination_threshold_exceeds_individual_switching_incentive, holdable).
narrative_ontology:cs_axiom_grounding('38fb4e4f-5298-420c-be3d-f0098c7a74d8', coordination_threshold_exceeds_individual_switching_incentive, empirically_contingent).
narrative_ontology:cs_reference_frame('38fb4e4f-5298-420c-be3d-f0098c7a74d8', increasing_returns_network_equilibrium).
narrative_ontology:cs_drift_state('38fb4e4f-5298-420c-be3d-f0098c7a74d8', contemporary_digital_keyboard_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('38fb4e4f-5298-420c-be3d-f0098c7a74d8', '').
narrative_ontology:cs_kernel_id(qwerty_persistence_mechanism__lock_in_reading, qwerty_persistence_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__lock_in_reading, keyboard_manufacturers_incumbent_tooling).
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__lock_in_reading, typing_instruction_industry).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__lock_in_reading, typists_general_population).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__lock_in_reading, would_be_alternative_layout_adopters).
narrative_ontology:constraint_vindicates(qwerty_persistence_mechanism__lock_in_reading, network_externality_lock_in_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Manufacture keyboards, firmware, and typing curricula built around QWERTY. They did not design the lock-in and would face real retooling costs if a coordinated switch occurred, but their existing capital stock and product lines are structurally favored by the standard remaining fixed. They collect no rent from suppressing alternatives — they simply face a coordination problem identical to everyone else's, resolved in their favor by inertia.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, keyboard_manufacturers_incumbent_tooling, beneficiary,
    organized, generational, constrained, global).

% Schools, typing-course publishers, and certification bodies teach QWERTY because it is what employers and devices expect. They benefit from stability of the standard but did not construct or defend it against alternatives through coercive means; a coordinated shift would simply require them to retool curricula.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, typing_instruction_industry, beneficiary,
    moderate, generational, constrained, national).

% Learn and use a layout that imposes marginally higher finger travel, repetitive strain risk, and slower peak typing speed than documented alternatives (e.g., Dvorak). No individual typist can benefit from switching alone because every device, every job posting, and every other typist assumes QWERTY; the switching cost is borne entirely by whoever moves first, so nobody moves. They are victims of a diffuse collective-action failure, not of any actor's extraction.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, typists_general_population, payer,
    powerless, biographical, trapped, global).

% Individuals and small communities who have tried alternative layouts bear a permanent bilingual-keyboard tax (cognitive switching cost, incompatible muscle memory, social friction on shared devices) without ever tipping the network toward the alternative, because the coordination threshold required for a network switch is far beyond what isolated adopters can generate.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, would_be_alternative_layout_adopters, excluded,
    powerless, biographical, trapped, global).

% Ship QWERTY as the default layout on every consumer device and operating system because doing otherwise would fragment their user base and support burden. They administer the standard by default-setting, not by prohibition — alternative layouts are typically available as a menu option, technically accessible but socially and practically dead on arrival because of the network's weight.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, device_and_os_platform_vendors, agenda_setter,
    institutional, generational, mobile, global).

% Study QWERTY as the canonical path-dependence case study, debating (this being one contested reading among several) whether the persistence reflects genuine coordination failure, manufacturer capture, or eventual adequacy. They have no stake in the outcome beyond correctly characterizing the mechanism.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, economic_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qwerty_persistence_mechanism__lock_in_reading, diffuse).
narrative_ontology:fixing_cost_class(qwerty_persistence_mechanism__lock_in_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A single, universally shared keyboard layout lets any typist use any device and any employer hire any typist without layout-specific retraining — a genuine, valuable coordination good realized by everyone converging on the same standard, whichever standard it is.
% TRANSFER_FUNCTION: The arrangement does not transfer value from an identifiable payer to an identifiable receiver; it imposes a diffuse efficiency loss (extra keystrokes, marginally higher strain, foregone speed) on the entire population of typists relative to a counterfactual optimal-layout equilibrium, with no corresponding concentrated gain captured by any actor.
% ABSENT_VOICES: Proponents of alternative layouts (Dvorak, Colemak) are not organizationally excluded by any gatekeeper — they are excluded by the coordination math itself: the switching threshold requires a critical mass no individual or firm can unilaterally supply, so their voice exists but cannot move the equilibrium.
% DISAPPEARANCE_RATIONALE: If QWERTY vanished overnight with no coordinated replacement, the immediate result would be chaos disproportionate to the mechanism's actual inefficiency — every keyboard, every trained typist, every documented interface would be simultaneously invalidated. But if a coordinated, well-resourced replacement were installed in its place (the counterfactual this reading cares about), the world would rearrange only modestly: typing speeds would rise a few percent, strain injuries would fall somewhat, and no concentrated interest would lose a revenue stream, because under this reading nobody is collecting rent from the status quo.
% FOUNDING_PROBLEM: Early typewriters jammed when adjacent-key strikers were struck in rapid succession; QWERTY's letter arrangement was selected to slow typists down and separate commonly paired letters, solving a mechanical problem that stopped existing once typewriters were replaced by electric and then electronic keyboards.
% FOUNDING_PROBLEM_CORROBORATION: Historians of technology (outside any manufacturer or instructional body) and human-factors researchers studying keyboard ergonomics attest that the jamming problem QWERTY solved disappeared with mechanical typewriters decades ago; the layout's persistence is corroborated by these external observers as a coordination artifact rather than a live functional requirement, though the SAME observers are split on whether the persistence should be read as failure, adequacy, or capture — which is exactly the kernel this story is one reading of.
narrative_ontology:disappearance_verdict(qwerty_persistence_mechanism__lock_in_reading, contested).
narrative_ontology:founding_problem_status(qwerty_persistence_mechanism__lock_in_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_mechanism__lock_in_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(qwerty_persistence_mechanism__lock_in_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence_mechanism__lock_in_reading, 0.18, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored low (0.18 at interval end) and rising only modestly, because under this reading there is no rent extraction — the cost is an efficiency loss distributed across the entire population, not a transfer captured anywhere. Suppression is low (0.12): nothing actively forbids alternative layouts; the barrier is coordination cost, not coercion. Theater ratio is very low (0.08): almost no performative maintenance exists because there is no institution whose job is to defend QWERTY against challengers — the standard persists by default inertia, not active guardianship. Accessibility collapse is authored high (0.72) despite low suppression: alternatives are technically available on every device's settings menu, yet practically dead because of network-threshold economics, which is a distinct mechanism from coercive foreclosure. Resistance is moderate (0.35): a persistent minority (ergonomics advocates, alternative-layout communities) continues to campaign, but cannot generate enough force to tip the equilibrium.
 *
 * PERSPECTIVAL GAP:
 *   From the coordination-function seat (device vendors, manufacturers) the arrangement reads as a working standard requiring no active decision; from the payer seat (typists bearing marginal inefficiency) it reads as a mildly suboptimal but unavoidable status quo. Neither seat perceives coercion or capture under this reading — the divergence is efficiency-loss vs. convenience, not extraction vs. rent-collection, which is the structural delta distinguishing this reading from beneficiary_extraction_reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries here are best read as incidental, not extractive: manufacturers and instructors benefit from stability the way anyone benefits from a stable convention, without having engineered or defended it against a challenger. The engine's directionality derivation would ordinarily push beneficiaries toward the subsidized end and victims toward the extracted end; this reading's structural claim is that the gap between those ends is real but SMALL, because nobody is actively pulling on the extraction lever — the beneficiary group would be indifferent, not resistant, to a coordinated switch, distinguishing this from a captured arrangement.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (mechanical typewriter jamming) is dead, yet the arrangement persists — a classic mandatrophy signature. But this reading resolves the signature WITHOUT invoking capture: the arrangement persists because the coordination problem of switching is harder to solve than the coordination problem of adopting was in the first place (network effects compound asymmetrically), not because any party maintains it for private benefit. This distinguishes lock-in mandatrophy (nobody's fault, hard to fix) from extraction mandatrophy (somebody's fault, actively defended).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    lock_in_vs_naturalization_boundary,
    'Is the observed persistence gap between QWERTY and alternative layouts (e.g., Dvorak) large enough to constitute genuine collective suboptimality, or have decades of QWERTY-specific ergonomic refinement (keyboard shape, key travel, software autocorrect) closed the gap such that QWERTY has become adequate on its own terms?',
    'Controlled comparative studies of trained typists on both layouts using modern hardware, controlling for training time invested; meta-analysis of the contested Liebowitz-Margolis critique of the original David (1985) QWERTY narrative.',
    'If the efficiency gap is negligible under modern conditions, this reading collapses toward naturalization_reading (ε should be near-zero, not 0.18); if the gap remains substantial and documented, this reading''s ε is conservative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lock_in_vs_naturalization_boundary, empirical, 'Whether the technical inferiority premise underlying the lock-in reading still holds empirically.').

omega_variable(
    lock_in_vs_extraction_boundary,
    'Do keyboard manufacturers, typing-instruction publishers, or platform vendors take any ACTIVE steps (lobbying, default-setting decisions calibrated to suppress switching costs of rivals, patent behavior) to defend QWERTY beyond passive default-shipping, or is their relationship to the standard purely incidental?',
    'Historical review of standards-body records, patent filings, and vendor lobbying disclosures for any documented effort to suppress alternative layout adoption beyond ordinary default-setting.',
    'Evidence of active defense would shift this constraint toward the beneficiary_extraction_reading (higher suppression, concentrated beneficiary, requires_active_enforcement in a coercive sense); absence of such evidence supports this reading''s diffuse, no-capture framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lock_in_vs_extraction_boundary, empirical, 'Whether incumbent beneficiaries are passive or active in maintaining the lock-in.').

omega_variable(
    coordination_threshold_measurability,
    'Can the actual coordination threshold required for a population-scale layout switch be estimated, or is ''the switching cost exceeds the switching benefit for every individual'' an untestable structural claim that could equally describe a naturalized-adequate standard?',
    'Model the network-effect dynamics using historical analogues of successful standard switches (e.g., metric conversion, right-hand traffic conversions) to estimate whether QWERTY''s threshold is unusually high or ordinary.',
    'If QWERTY''s switching threshold is not unusual relative to other successfully-converted standards, the ''trapped by coordination failure'' framing weakens relative to ''nobody has bothered because the gain is marginal'' (naturalization-adjacent).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_threshold_measurability, conceptual, 'Whether the lock-in claim is empirically falsifiable or a structural default explanation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_mechanism__lock_in_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t0, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 0, 0.02).
narrative_ontology:measurement(qwer_tr_t20, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 20, 0.03).
narrative_ontology:measurement(qwer_tr_t40, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 40, 0.04).
narrative_ontology:measurement(qwer_tr_t60, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 60, 0.05).
narrative_ontology:measurement(qwer_tr_t80, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 80, 0.07).
narrative_ontology:measurement(qwer_tr_t100, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 100, 0.08).

% Extraction over time
narrative_ontology:measurement(qwer_be_t0, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(qwer_be_t20, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 20, 0.08).
narrative_ontology:measurement(qwer_be_t40, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 40, 0.11).
narrative_ontology:measurement(qwer_be_t60, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 60, 0.14).
narrative_ontology:measurement(qwer_be_t80, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 80, 0.16).
narrative_ontology:measurement(qwer_be_t100, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 100, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(qwerty_persistence_mechanism__lock_in_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_mechanism__lock_in_reading, information_standard).
narrative_ontology:boltzmann_floor_override(qwerty_persistence_mechanism__lock_in_reading, 0.03).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__lock_in_reading, qwerty_persistence_mechanism__naturalization_reading).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__lock_in_reading, qwerty_persistence_mechanism__beneficiary_extraction_reading).

% DUAL FORMULATION NOTE:
% This is one of three sibling constraint stories decomposing the colloquial 'why does QWERTY persist' question, per the ε-invariance principle. naturalization_reading claims near-zero ε (adequate standard, fair competition). beneficiary_extraction_reading claims higher ε with an identifiable capturing beneficiary and active suppression. This lock_in_reading occupies the middle: real but diffuse social cost (ε=0.18), no concentrated beneficiary, low suppression, high accessibility_collapse driven by coordination math rather than coercion. All three share the kernel_id qwerty_persistence_mechanism and are linked bidirectionally for contamination/coupling analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
