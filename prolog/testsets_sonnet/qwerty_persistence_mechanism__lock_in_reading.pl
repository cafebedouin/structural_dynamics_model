% ============================================================================
% CONSTRAINT STORY: qwerty_persistence_mechanism__lock_in_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: QWERTY Keyboard Layout — Path-Dependent Coordination Lock-In
 *   domain: economic_history/technology_studies/path_dependence
 *
 * SUMMARY:
 *   This story instantiates the lock-in reading of the QWERTY persistence
 *   kernel: the standard economic-history account in which a keyboard layout,
 *   once adopted at scale, becomes practically un-displaceable through pure
 *   network-coordination dynamics, independent of whether any party actively
 *   benefits from suppressing alternatives or whether the layout is genuinely
 *   technically inferior. Under this reading, no manufacturer, standards
 *   body, or interest group deliberately maintains QWERTY against a superior
 *   alternative for extraction (that is the beneficiary_extraction_reading, a
 *   separate constraint); nor is QWERTY simply the fairly-won best available
 *   option (that is the naturalization_reading, also separate). This
 *   reading's distinctive claim is a market failure without a beneficiary:
 *   the social cost of a locally-suboptimal-but-not-catastrophically-bad
 *   standard is real and diffused, but it is not captured as rent by anyone,
 *   and the persistence mechanism is genuinely just coordination cost — every
 *   individual's rational non-switching, aggregated, produces a collectively
 *   worse-than-achievable outcome with no villain.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_mechanism__lock_in_reading, 0.18).
domain_priors:suppression_score(qwerty_persistence_mechanism__lock_in_reading, 0.12).
domain_priors:theater_ratio(qwerty_persistence_mechanism__lock_in_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_mechanism__lock_in_reading, piton).
narrative_ontology:human_readable(qwerty_persistence_mechanism__lock_in_reading, "QWERTY Keyboard Layout — Path-Dependent Coordination Lock-In").
narrative_ontology:topic_domain(qwerty_persistence_mechanism__lock_in_reading, "economic_history/technology_studies/path_dependence").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_mechanism__lock_in_reading, '2194ae19-9d23-4753-be3d-d1d5691e50a0').
narrative_ontology:cs_kernel_codification('2194ae19-9d23-4753-be3d-d1d5691e50a0', implicit).
narrative_ontology:cs_authority_grounding('2194ae19-9d23-4753-be3d-d1d5691e50a0', distributed).
narrative_ontology:cs_reading_relation('2194ae19-9d23-4753-be3d-d1d5691e50a0', qwerty_persistence_mechanism__naturalization_reading, coexists_with).
narrative_ontology:cs_reading_relation('2194ae19-9d23-4753-be3d-d1d5691e50a0', qwerty_persistence_mechanism__beneficiary_extraction_reading, coexists_with).
narrative_ontology:cs_axiom('2194ae19-9d23-4753-be3d-d1d5691e50a0', foundational, coordination_cost_without_capture).
narrative_ontology:cs_axiom_status(coordination_cost_without_capture, holdable).
narrative_ontology:cs_axiom_grounding('2194ae19-9d23-4753-be3d-d1d5691e50a0', coordination_cost_without_capture, empirically_contingent).
narrative_ontology:cs_axiom('2194ae19-9d23-4753-be3d-d1d5691e50a0', secondary, collective_suboptimality_absent_villain).
narrative_ontology:cs_axiom_status(collective_suboptimality_absent_villain, holdable).
narrative_ontology:cs_axiom_grounding('2194ae19-9d23-4753-be3d-d1d5691e50a0', collective_suboptimality_absent_villain, empirically_contingent).
narrative_ontology:cs_reference_frame('2194ae19-9d23-4753-be3d-d1d5691e50a0', mechanical_typewriter_jam_avoidance_standard).
narrative_ontology:cs_drift_state('2194ae19-9d23-4753-be3d-d1d5691e50a0', contemporary_digital_keyboard_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2194ae19-9d23-4753-be3d-d1d5691e50a0', '').
narrative_ontology:cs_kernel_id(qwerty_persistence_mechanism__lock_in_reading, qwerty_persistence_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__lock_in_reading, keyboard_manufacturers_incumbent_tooling).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__lock_in_reading, typists_general_population).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__lock_in_reading, software_localization_industry).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Learn QWERTY as children or new typists because it is the only layout anyone around them uses, on every keyboard they encounter. Bear the (modest, contested) ergonomic and typing-speed cost of a layout not optimized for finger travel, but switching individually gains nothing since every other keyboard they will ever touch — work, school, public terminals — remains QWERTY. No one profits from their cost; it is simply diffused across everyone who types.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, typists_general_population, payer,
    powerless, biographical, trapped, global).

% Builds input methods, shortcuts, and training materials around QWERTY's physical arrangement across every language and platform. Carries the accumulated engineering cost of designing around a layout no one chose for its merits, but has no incentive to unilaterally support an alternative that almost no user base has adopted.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, software_localization_industry, payer,
    moderate, generational, constrained, global).

% Continue producing QWERTY hardware because tooling, supply chains, and consumer expectation are already built around it. Benefit incidentally from not having to retool or re-market, but do not actively suppress alternatives (alternative layouts like Dvorak or Colemak are freely sold and software-supported) — they simply have no reason to bear first-mover cost when demand for switching is near zero.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, keyboard_manufacturers_incumbent_tooling, beneficiary,
    organized, generational, mobile, global).

% Designed and promoted layouts (Dvorak, Colemak) claiming ergonomic and speed advantages. Are not blocked by law or coercion — anyone can install these layouts today at zero monetary cost — but face a coordination wall: individual adoption yields little benefit because the surrounding world (keyboards, muscle memory of collaborators, typing tests, employer expectations) remains QWERTY. Their voice is present in enthusiast communities but absent from mainstream standard-setting.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, alternative_layout_designers, excluded,
    powerless, biographical, mobile, global).

% Study QWERTY as the canonical path-dependence case study, debating whether the standard economic story (technical inferiority locked in by network effects) survives empirical scrutiny (the Liebowitz-Margolis critique) or whether the lock-in account itself is the more defensible reading despite the empirical fight over typing-speed data.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, economic_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A shared keyboard layout solves a genuine coordination problem: every typist, manufacturer, typing-instruction system, and piece of software benefits from a single common standard rather than fragmentation across incompatible layouts.
% TRANSFER_FUNCTION: No deliberate transfer of value from one party to another occurs. What moves is a diffuse efficiency cost — foregone typing speed and ergonomic comfort — spread thinly across the entire population of typists, with no corresponding concentrated gain captured by any single actor.
% ABSENT_VOICES: Alternative-layout advocates and ergonomics researchers are not silenced by any authority, but they lack a coordination mechanism to organize a mass switch; their technically-grounded objections circulate in niche communities without a lever to move the standard.
% DISAPPEARANCE_RATIONALE: If QWERTY vanished overnight and had to be re-selected from scratch, the coordination problem would resolve to whatever layout achieved critical mass fastest — plausibly QWERTY again, given how close current empirical estimates place the ergonomic gap. The lock-in reading holds the world would NOT dramatically improve if QWERTY disappeared (the switching cost of relearning would likely exceed captured gains for most individuals), but it WOULD rearrange in the sense that a coordination failure — persistence of a locally-suboptimal-but-not-clearly-inferior standard absent any mechanism for collective re-optimization — would be resolved rather than perpetually latent.
% FOUNDING_PROBLEM: Early typewriters jammed when adjacent-key strikes occurred in rapid succession; the layout was arranged partly to separate common letter pairs and slow the collision-prone striking pattern (the actual QWERTY design history is itself contested and less deliberately anti-speed than folklore holds).
% FOUNDING_PROBLEM_CORROBORATION: Mechanical typewriter engineers and historians of technology (outside any interest group that benefits from QWERTY's persistence) attest that the jamming problem QWERTY partially addressed disappeared with electric and then electronic keyboards decades ago; the layout's continuation past that point is attested by economic historians (David 1985; the QWERTY/Dvorak literature) as owing to installed-base coordination costs rather than to any surviving technical necessity.
narrative_ontology:disappearance_verdict(qwerty_persistence_mechanism__lock_in_reading, contested).
narrative_ontology:founding_problem_status(qwerty_persistence_mechanism__lock_in_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_mechanism__lock_in_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored low (0.18 at present) because under this reading no party siphons value from the arrangement — the cost is a diffuse efficiency loss, not a transfer. Suppression is low (0.12): nothing coercive blocks a typist or firm from switching to Dvorak or Colemak; the barrier is coordination cost, not enforcement. Theater ratio is low (0.10): keyboard standardization involves essentially no performative maintenance — no one is staging QWERTY's defense, it simply persists by default. Accessibility collapse is high (0.72) despite low suppression: alternatives are legally and technically available but functionally foreclosed by the installed-base coordination problem — this is the signature of lock-in as distinct from coercion. Resistance is moderate (0.35): a real community of alternative-layout advocates exists and periodically gains attention, but does not constitute organized resistance capable of shifting the equilibrium.
 *
 * PERSPECTIVAL GAP:
 *   From the analytical/historian seat, the constraint looks like a textbook coordination-failure market failure: individually rational non-switching aggregates into collective suboptimality. From the individual typist's seat, there is no felt extraction at all — simply 'this is how keyboards are.' The gap between the diffuse collective cost (real, per the lock-in reading) and the absence of any individually-experienced injury is exactly what keeps this reading from computing as a snare or tangled_rope: there is a victim class in aggregate but no identifiable extraction mechanism transferring value to a beneficiary.
 *
 * DIRECTIONALITY LOGIC:
 *   Typists and the localization industry are payers under this reading not because anyone extracts from them, but because they bear the diffuse cost of a suboptimal coordination equilibrium with no exit that individually pays off. Manufacturers are named beneficiaries only in the weak, incidental sense of avoiding retooling costs — they do not actively maintain QWERTY against superior alternatives (that active-maintenance claim belongs to the sibling beneficiary_extraction_reading, not this one); the directionality here should sit closer to symmetric than a true extraction case would produce, reflecting that this reading treats the manufacturer's position as passive non-switching rather than deliberate rent protection.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (mechanical key-jamming) is dead, and the arrangement plainly outlived it — but under the lock-in reading this is not a mandatrophy story in the beneficiary sense (no one benefits from the mandate's obsolescence going unaddressed). It is closer to genuine institutional piton: the coordination structure that once solved a real problem now persists purely through switching-cost inertia, with diffuse cost and no concentrated capturer, which is consistent with a piton read rather than snare or tangled_rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_inferiority_of_qwerty,
    'Is QWERTY actually measurably inferior in typing speed/ergonomics to alternatives like Dvorak, net of the confound that most comparative studies were conducted or funded by parties with an interest in the outcome (including the original Dvorak-sponsored studies)?',
    'Independent, pre-registered controlled studies comparing skilled typists trained from scratch on each layout, controlling for motivation and Hawthorne effects; the Liebowitz-Margolis reanalysis of the historical record is the central existing challenge to the inferiority premise.',
    'If QWERTY is not measurably inferior, this lock-in reading collapses toward the naturalization_reading — there is no efficiency loss to attribute to coordination failure. If a real gap exists, the lock-in reading is empirically supported as this story assumes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(empirical_inferiority_of_qwerty, empirical, 'Whether the technical-inferiority premise the lock-in reading depends on actually holds up empirically.').

omega_variable(
    coordination_failure_vs_active_maintenance,
    'Is QWERTY''s persistence better modeled as passive coordination failure (no one steering it) or as at least partly actively maintained by keyboard manufacturers and typing-certification bodies protecting sunk training and tooling investments?',
    'Historical/archival research into whether manufacturers or typing-education institutions ever lobbied against alternative-layout standardization efforts, versus simply never adopting them for lack of demand.',
    'Evidence of active lobbying or deliberate suppression would shift this constraint toward the sibling beneficiary_extraction_reading, with a concentrated beneficiary and higher extraction/suppression scores than authored here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_failure_vs_active_maintenance, empirical, 'Whether persistence is genuinely passive coordination failure or partly active incumbent maintenance — the boundary between this reading and its sibling.').

omega_variable(
    cs_framing_kernel_vs_market_mechanism,
    'Is QWERTY persistence better modeled as a commitment-system kernel (a standard an authority structure defends) or as a pure decentralized market coordination mechanism with no authority at all?',
    'Assess whether any standards body (ANSI, ISO) treats QWERTY as a codified, actively-adjudicated standard versus a de facto convention with no adjudicating authority.',
    'If a genuine standards-body kernel exists, cs_structure fields would shift toward formalized/distributed rather than the implicit/distributed framing used here; if purely decentralized, the cs_structure block may not be warranted at all for this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_kernel_vs_market_mechanism, conceptual, 'Whether this reading warrants CS framing at all, and if so under what kernel_codification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_mechanism__lock_in_reading, 1873, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t1873, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 1873, 0.02).
narrative_ontology:measurement(qwer_tr_t1910, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 1910, 0.03).
narrative_ontology:measurement(qwer_tr_t1950, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 1950, 0.04).
narrative_ontology:measurement(qwer_tr_t1980, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 1980, 0.06).
narrative_ontology:measurement(qwer_tr_t2000, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 2000, 0.08).
narrative_ontology:measurement(qwer_tr_t2024, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(qwer_be_t1873, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 1873, 0.05).
narrative_ontology:measurement(qwer_be_t1910, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 1910, 0.08).
narrative_ontology:measurement(qwer_be_t1950, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 1950, 0.1).
narrative_ontology:measurement(qwer_be_t1980, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 1980, 0.14).
narrative_ontology:measurement(qwer_be_t2000, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 2000, 0.16).
narrative_ontology:measurement(qwer_be_t2024, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 2024, 0.18).

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
% Three sibling readings of the qwerty_persistence_mechanism kernel exist as separate constraint stories, each with its own ε, beneficiary/victim structure, and claimed type per the ε-invariance principle: this lock_in_reading (piton-leaning, no concentrated beneficiary, diffuse collective cost, extractiveness ~0.18), naturalization_reading (expected mountain/rope-leaning, denies any inferiority or lock-in exists), and beneficiary_extraction_reading (expected tangled_rope/snare-leaning, names manufacturers as active extractors, higher extractiveness and suppression). The three are linked bidirectionally via affects_constraints because they are structurally rival explanations of the same observable persistence pattern — evidence bearing on one reading's empirical premises directly bears on the others' plausibility.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
