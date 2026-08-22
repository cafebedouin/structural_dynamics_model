% ============================================================================
% CONSTRAINT STORY: qwerty_persistence__incumbent_preservation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence__incumbent_preservation_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: qwerty_persistence__incumbent_preservation_reading
 *   human_readable: QWERTY Keyboard Standard — Incumbent Preservation Reading
 *   domain: technology_history/industrial_standards/path_dependence
 *
 * SUMMARY:
 *   This story reads the QWERTY keyboard layout's persistence through the
 *   incumbent-preservation lens: standards bodies, manufacturers, and
 *   training institutions actively maintain the layout not because it still
 *   solves the coordination problem it was built for (mechanical typebar
 *   jamming, obsolete since electric typewriters) but because their capital
 *   investments — tooling, curricula, certifications, and personal typing
 *   skill — only retain value if the standard holds. The sibling reading
 *   (lapsed_alternatives_reading) treats the same historical episode as a
 *   story of coordination value and failed critical mass for alternatives;
 *   this reading treats it as active, ongoing defensive suppression of
 *   superior alternatives to protect sunk-cost beneficiaries. Both are
 *   readings of the same kernel — QWERTY's persistence — but this reading
 *   authors a materially higher extraction figure because it locates agency
 *   and intent in the incumbent beneficiaries rather than treating the
 *   outcome as a passive network-effect failure.
 *
 * KEY AGENTS:
 *   - keyboard_manufacturers: primary beneficiary and agenda-setter (organized/arbitrage) — protects tooling and supply-chain capital
 *   - trained_typists: beneficiary (moderate/constrained) — protects sunk skill investment
 *   - typing_training_institutions: beneficiary and agenda-setter (institutional/arbitrage) — protects curriculum and certification capital
 *   - alternative_layout_adopters and efficiency_seeking_typists: primary targets (powerless/trapped-constrained) — bear incompatibility and ergonomic costs
 *   - layout_innovators: excluded voice (powerless/trapped) — no path to market
 *   - standards_historians: analytical observer — documents rationale drift
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence__incumbent_preservation_reading, 0.61).
domain_priors:suppression_score(qwerty_persistence__incumbent_preservation_reading, 0.58).
domain_priors:theater_ratio(qwerty_persistence__incumbent_preservation_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, extractiveness, 0.61).
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence__incumbent_preservation_reading, tangled_rope).
narrative_ontology:human_readable(qwerty_persistence__incumbent_preservation_reading, "QWERTY Keyboard Standard — Incumbent Preservation Reading").
narrative_ontology:topic_domain(qwerty_persistence__incumbent_preservation_reading, "technology_history/industrial_standards/path_dependence").

domain_priors:requires_active_enforcement(qwerty_persistence__incumbent_preservation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence__incumbent_preservation_reading, 'e65b18bf-868a-4793-8812-4287ad9e71f6').
narrative_ontology:cs_kernel_codification('e65b18bf-868a-4793-8812-4287ad9e71f6', implicit).
narrative_ontology:cs_authority_grounding('e65b18bf-868a-4793-8812-4287ad9e71f6', practice).
narrative_ontology:cs_interpretation_layer_present('e65b18bf-868a-4793-8812-4287ad9e71f6').
narrative_ontology:cs_reading_relation('e65b18bf-868a-4793-8812-4287ad9e71f6', qwerty_persistence__lapsed_alternatives_reading, coexists_with).
narrative_ontology:cs_axiom('e65b18bf-868a-4793-8812-4287ad9e71f6', foundational, incumbent_capital_protection_drives_standard_persistence).
narrative_ontology:cs_axiom_status(incumbent_capital_protection_drives_standard_persistence, holdable).
narrative_ontology:cs_axiom_grounding('e65b18bf-868a-4793-8812-4287ad9e71f6', incumbent_capital_protection_drives_standard_persistence, empirically_contingent).
narrative_ontology:cs_axiom('e65b18bf-868a-4793-8812-4287ad9e71f6', secondary, alternative_suppression_is_deliberate_not_incidental).
narrative_ontology:cs_axiom_status(alternative_suppression_is_deliberate_not_incidental, holdable).
narrative_ontology:cs_axiom_grounding('e65b18bf-868a-4793-8812-4287ad9e71f6', alternative_suppression_is_deliberate_not_incidental, empirically_contingent).
narrative_ontology:cs_reference_frame('e65b18bf-868a-4793-8812-4287ad9e71f6', mechanical_jam_avoidance_standard).
narrative_ontology:cs_drift_state('e65b18bf-868a-4793-8812-4287ad9e71f6', digital_keyboard_era, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('e65b18bf-868a-4793-8812-4287ad9e71f6', '').
narrative_ontology:cs_kernel_id(qwerty_persistence__incumbent_preservation_reading, qwerty_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence__incumbent_preservation_reading, keyboard_manufacturers).
narrative_ontology:constraint_beneficiary(qwerty_persistence__incumbent_preservation_reading, trained_typists).
narrative_ontology:constraint_beneficiary(qwerty_persistence__incumbent_preservation_reading, typing_training_institutions).
narrative_ontology:constraint_beneficiary(qwerty_persistence__incumbent_preservation_reading, office_equipment_vendors).
narrative_ontology:constraint_victim(qwerty_persistence__incumbent_preservation_reading, alternative_layout_adopters).
narrative_ontology:constraint_victim(qwerty_persistence__incumbent_preservation_reading, efficiency_seeking_typists).
narrative_ontology:constraint_victim(qwerty_persistence__incumbent_preservation_reading, layout_innovators).
narrative_ontology:constraint_victim(qwerty_persistence__incumbent_preservation_reading, new_typing_learners).
narrative_ontology:constraint_vindicates(qwerty_persistence__incumbent_preservation_reading, installed_base_advantage_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Have sunk decades of tooling, supply-chain, and manufacturing capital into QWERTY-layout production. Lobby standards bodies, resist retooling for alternative layouts, and price alternative-layout devices as premium/niche products, which suppresses their uptake. Their capital investment is protected precisely because switching costs fall on everyone else.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, keyboard_manufacturers, beneficiary,
    organized, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(qwerty_persistence__incumbent_preservation_reading, keyboard_manufacturers, agenda_setter).

% Have invested years building muscle-memory fluency on QWERTY. Their skill is a sunk asset that only retains value if the standard persists; they have professional and economic incentive to resist any transition, even one that would benefit new entrants.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, trained_typists, beneficiary,
    moderate, biographical, constrained, national).

% Curricula, certification exams, and instructor training are all built around QWERTY. Institutions actively resist layout curriculum changes because retooling instructional material and retraining instructors is costly and yields no near-term institutional benefit; they lobby to keep QWERTY as the certified standard.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, typing_training_institutions, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(qwerty_persistence__incumbent_preservation_reading, typing_training_institutions, agenda_setter).

% Individuals or small groups who adopt or would adopt an alternative layout bear the full cost of incompatibility: unavailable hardware, unrecognized certifications, colleagues and shared devices defaulting back to QWERTY, and social friction from being the outlier. Their exit is nominally available but practically foreclosed by the ecosystem around them.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, alternative_layout_adopters, payer,
    powerless, biographical, trapped, national).

% Would benefit from a layout with lower finger travel and fewer repetitive strain injuries but cannot unilaterally switch without abandoning shared equipment, employer-mandated tools, and transferable typing credentials. They absorb the ergonomic and productivity cost of a suboptimal standard that persists for reasons unrelated to typing efficiency.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, efficiency_seeking_typists, payer,
    powerless, biographical, constrained, national).

% Designers of alternative layouts (Dvorak-style and successors) have no path to market without manufacturer buy-in, training-institution certification, or critical mass of independent adopters — all three of which are controlled by parties invested in the incumbent standard. Their voice is structurally absent from the standards-setting process.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, layout_innovators, excluded,
    powerless, biographical, trapped, national).

% Learn QWERTY by default because it is what is taught, sold, and certified — not because it was independently evaluated as superior. They pay the cost of learning a layout optimized for a mechanical constraint (jam avoidance in lever-arm typewriters) that no longer exists in their own equipment.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, new_typing_learners, payer,
    powerless, biographical, constrained, national).

% Study why QWERTY persisted after the mechanical jam-avoidance rationale became obsolete with electric and digital keyboards. Document the divergence between the standard's original coordination rationale and its current enforcement rationale.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, standards_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qwerty_persistence__incumbent_preservation_reading, keyboard_manufacturers).
narrative_ontology:fixing_cost_class(qwerty_persistence__incumbent_preservation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A single, universally-taught keyboard layout lets typists move between employers, devices, and training programs without relearning finger placement, and lets manufacturers mass-produce one physical layout instead of many.
% TRANSFER_FUNCTION: Moves adaptation cost from incumbent manufacturers, trained typists, and training institutions (who keep their sunk capital and skills valuable) onto anyone trying to adopt a more efficient alternative layout, who must bear incompatibility costs alone.
% ABSENT_VOICES: Layout innovators and prospective alternative-layout communities have no seat in the standards process; they would argue the original mechanical rationale for QWERTY (jam avoidance on lever-arm typewriters) disappeared decades ago and the standard now persists purely to protect sunk investments, not to solve any live coordination problem.
% DISAPPEARANCE_RATIONALE: Manufacturers, trained typists, and training institutions would say the world rearranges badly — retraining costs, incompatible fleets of hardware, and devalued certifications. Alternative-layout advocates and efficiency-seekers would say the world barely changes for the average new learner, since a fresh learner has no sunk cost either way, and argue any disruption is a one-generation transition cost that would pay for itself in aggregate typing efficiency and reduced repetitive strain injury.
% FOUNDING_PROBLEM: Early mechanical typewriters jammed when adjacent-lettered typebars were struck in quick succession; QWERTY was arranged in part to slow typists down and separate common letter pairs mechanically.
% FOUNDING_PROBLEM_CORROBORATION: Independent typewriter historians and ergonomics researchers outside the manufacturing and training-institution beneficiary set attest that the jam-avoidance rationale became moot with the shift to electric typewriters and later digital keyboards, where no mechanical jamming is possible; the standard's persistence past that point is attributed by these outside sources to installed-base defense rather than continued coordination necessity.
narrative_ontology:disappearance_verdict(qwerty_persistence__incumbent_preservation_reading, contested).
narrative_ontology:founding_problem_status(qwerty_persistence__incumbent_preservation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence__incumbent_preservation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(qwerty_persistence__incumbent_preservation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence__incumbent_preservation_reading, 0.61, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence__incumbent_preservation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(qwerty_persistence__incumbent_preservation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(qwerty_persistence__incumbent_preservation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises over the interval (0.12 to 0.61) as the original mechanical rationale disappears (electric typewriters, then digital keyboards) while the standard's enforcement mechanisms — manufacturing defaults, certification requirements, training curricula — persist and harden. Theater ratio also rises (0.05 to 0.42): more of the justificatory language shifts from 'this prevents jamming' (a claim that becomes literally false) toward 'this is what everyone already knows' (a coordination-value claim that increasingly functions as cover for sunk-cost defense). Suppression requirement rises in parallel (0.15 to 0.58) as maintaining the standard requires increasingly deliberate manufacturer and institutional coordination once the natural mechanical rationale that made the standard self-enforcing is gone.
 *
 * PERSPECTIVAL GAP:
 *   From the manufacturer and training-institution seats, the standard looks like ordinary, necessary coordination — 'this is simply how keyboards are made and taught.' From the alternative-layout adopter and efficiency-seeker seats, the same arrangement looks like active, enforced lock-in maintained well past its functional justification. The engine should compute these as structurally different seat classifications from the same underlying data, which is the point of the tangled_rope classification: both the coordination story and the extraction story are true, for different parties, simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   Manufacturers, trained typists, and training institutions are declared beneficiaries because the standard's persistence directly protects assets they hold — tooling, skill, and curriculum respectively — regardless of whether the standard remains functionally optimal. Alternative-layout adopters, efficiency seekers, layout innovators, and new learners are declared victims because they bear the switching costs, ergonomic costs, or exclusion costs that the beneficiary defense imposes. The directionality derivation should place manufacturers and institutions near the full-beneficiary end (arbitrage exit, organized/institutional power) and alternative-adopters near the full-target end (trapped exit, powerless).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (mechanical jam avoidance) is dead, but the arrangement persists — this is the textbook mandatrophy signature. Classifying this reading as tangled_rope rather than snare or piton preserves the fact that a real coordination function (interoperability of skills and hardware across employers and devices) still operates today, even though the reason IT is QWERTY specifically rather than any other converged layout is beneficiary defense, not present-day optimality. Calling it a pure snare would erase the genuine coordination value new learners get from a universal standard; calling it a pure rope would erase the fact that incumbents actively suppress alternatives rather than merely benefiting passively from network effects — which is precisely what distinguishes this reading from the lapsed_alternatives_reading sibling.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    agency_vs_network_effect_ambiguity,
    'Is QWERTY''s persistence better explained by active, agentic beneficiary defense (manufacturers and institutions deliberately preserving dominance) or by a passive network-effect equilibrium where no single party need act with intent for the standard to lock in?',
    'Historical evidence of coordinated resistance — trade association lobbying records, deliberate marketing suppression of alternative-layout devices, certification-body decisions to exclude alternative layouts — versus evidence that adoption of alternatives simply never reached a tipping point despite no active opposition.',
    'If agentic defense is well-evidenced, this reading (tangled_rope, higher epsilon, defined victim set) is the structurally accurate account. If the historical record shows mainly passive network effects with no deliberate suppression, the lapsed_alternatives_reading (lower epsilon, closer to rope) is the more accurate account and this reading overstates intent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(agency_vs_network_effect_ambiguity, conceptual, 'Whether QWERTY persistence reflects active beneficiary defense or passive coordination failure — the exact structural fork between this reading and its sibling.').

omega_variable(
    efficiency_gap_magnitude,
    'How large is the actual typing-efficiency and ergonomic gap between QWERTY and leading alternative layouts, once modern digital-era conditions (no mechanical jamming) are accounted for?',
    'Controlled comparative studies of typing speed, error rate, and repetitive strain injury incidence across matched cohorts trained on QWERTY versus alternative layouts, correcting for training-time differences.',
    'A large, well-established efficiency gap strengthens the victim-side extraction claim (efficiency_seeking_typists are meaningfully harmed). A negligible or contested gap weakens the extraction claim and shifts weight toward the coordination-value account.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(efficiency_gap_magnitude, empirical, 'Whether the alleged efficiency cost borne by QWERTY users versus alternative-layout users is empirically substantial.').

omega_variable(
    reading_selection_evidentiary_basis,
    'What specific historical or documentary signal justified authoring this episode under the incumbent_preservation_reading rather than the lapsed_alternatives_reading?',
    'This reading was selected based on documented instances of manufacturer and standards-body resistance to Dvorak-layout adoption (including contested efficiency studies commissioned by parties with a stake in the outcome) as evidence of active defense rather than passive lapse.',
    'If archival research overturns this evidentiary basis and shows no deliberate suppression activity, this reading''s tangled_rope classification and elevated epsilon lose their structural grounding and the lapsed_alternatives_reading becomes the better-supported account of the same kernel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_selection_evidentiary_basis, conceptual, 'Documents which framing signal (evidence of active defense vs. absence of coordinated resistance) guided selection of this reading over its sibling, per the CS-framing under-determination guidance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence__incumbent_preservation_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t0, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(qwer_tr_t20, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(qwer_tr_t40, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement(qwer_tr_t60, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 60, 0.32).
narrative_ontology:measurement(qwer_tr_t80, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 80, 0.38).
narrative_ontology:measurement(qwer_tr_t100, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 100, 0.42).

% Extraction over time
narrative_ontology:measurement(qwer_be_t0, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(qwer_be_t20, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 20, 0.28).
narrative_ontology:measurement(qwer_be_t40, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 40, 0.45).
narrative_ontology:measurement(qwer_be_t60, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 60, 0.53).
narrative_ontology:measurement(qwer_be_t80, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 80, 0.58).
narrative_ontology:measurement(qwer_be_t100, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 100, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(qwer_su_t0, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(qwer_su_t20, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 20, 0.3).
narrative_ontology:measurement(qwer_su_t40, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 40, 0.44).
narrative_ontology:measurement(qwer_su_t60, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 60, 0.5).
narrative_ontology:measurement(qwer_su_t80, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 80, 0.55).
narrative_ontology:measurement(qwer_su_t100, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 100, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence__incumbent_preservation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(qwerty_persistence__incumbent_preservation_reading, 0.08).
narrative_ontology:affects_constraint(qwerty_persistence__incumbent_preservation_reading, qwerty_persistence__lapsed_alternatives_reading).

% DUAL FORMULATION NOTE:
% This constraint and qwerty_persistence__lapsed_alternatives_reading are two readings of the same kernel (qwerty_persistence). This reading authors the incumbent-preservation account: active beneficiary defense, tangled_rope classification, terminal epsilon 0.61, with a defined victim set (alternative-layout adopters, efficiency seekers, layout innovators, new learners). The sibling authors the coordination-value account: passive network-effect lock-in, a classification closer to rope, and materially lower epsilon, with no defensive-suppression framing and likely no victim set at all. The two files must never be merged or averaged; per the ε-invariance principle, they are structurally distinct constraints sharing one historical kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
