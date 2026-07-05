% ============================================================================
% CONSTRAINT STORY: qwerty_persistence__incumbent_preservation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:measurement_basis/2,
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
 *   The QWERTY keyboard layout was designed in the 1870s to address a genuine
 *   mechanical problem in early typewriters — typebar jamming from rapid
 *   adjacent keystrokes. That problem disappeared with the electric and
 *   electronic typewriter, yet QWERTY persisted and hardened into the global
 *   standard. This reading (incumbent_preservation_reading) holds that
 *   persistence past the mechanical era is best explained by active
 *   beneficiary defense: manufacturers with sunk tooling capital, typing
 *   institutions with accredited curricula, and trained typists with
 *   skill-specific human capital all actively preserved the standard against
 *   documented ergonomic and speed alternatives (most famously the Dvorak
 *   Simplified Keyboard), rather than the standard persisting purely because
 *   coordination value outweighed switching costs. A sibling reading
 *   (lapsed_alternatives_reading, a separate constraint story) holds that
 *   persistence is adequately explained by coordination failure of
 *   alternatives reaching critical mass, without requiring active
 *   suppression. This story instantiates ONLY the incumbent-preservation
 *   reading as a clean, ε-invariant constraint — the coordination-lapse story
 *   is not blended in here.
 *
 * KEY AGENTS:
 *   - keyboard_manufacturers: primary beneficiary and agenda_setter (institutional/arbitrage) — sunk capital defended via non-adoption of alternatives
 *   - trained_touch_typists: beneficiary (organized/identity_locked) — skill-specific human capital tied to the standard
 *   - typing_training_institutions: beneficiary (organized/constrained) — accredited curricula built on QWERTY
 *   - alternative_layout_adopters and efficiency_seeking_typists: primary victims (powerless/trapped, constrained) — bear stranded investment and foregone efficiency
 *   - dvorak_layout_advocates: excluded voice — documented alternative superiority, structurally shut out of standards bodies
 *   - economic_historians: analytical observer — documents the lock-in mechanism
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
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence__incumbent_preservation_reading, tangled_rope).
narrative_ontology:human_readable(qwerty_persistence__incumbent_preservation_reading, "QWERTY Keyboard Standard — Incumbent Preservation Reading").
narrative_ontology:topic_domain(qwerty_persistence__incumbent_preservation_reading, "technology_history/industrial_standards/path_dependence").

domain_priors:requires_active_enforcement(qwerty_persistence__incumbent_preservation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence__incumbent_preservation_reading, '913ebd7a-ac2d-4123-af8a-a7bd69cbebd6').
narrative_ontology:cs_kernel_codification('913ebd7a-ac2d-4123-af8a-a7bd69cbebd6', distributed).
narrative_ontology:cs_authority_grounding('913ebd7a-ac2d-4123-af8a-a7bd69cbebd6', extraction).
narrative_ontology:cs_interpretation_layer_present('913ebd7a-ac2d-4123-af8a-a7bd69cbebd6').
narrative_ontology:cs_reading_relation('913ebd7a-ac2d-4123-af8a-a7bd69cbebd6', qwerty_persistence__lapsed_alternatives_reading, coexists_with).
narrative_ontology:cs_axiom('913ebd7a-ac2d-4123-af8a-a7bd69cbebd6', foundational, beneficiary_capital_defense_drives_persistence).
narrative_ontology:cs_axiom_status(beneficiary_capital_defense_drives_persistence, holdable).
narrative_ontology:cs_axiom_grounding('913ebd7a-ac2d-4123-af8a-a7bd69cbebd6', beneficiary_capital_defense_drives_persistence, empirically_contingent).
narrative_ontology:cs_axiom('913ebd7a-ac2d-4123-af8a-a7bd69cbebd6', secondary, standards_bodies_captured_by_installed_base_interests).
narrative_ontology:cs_axiom_status(standards_bodies_captured_by_installed_base_interests, holdable).
narrative_ontology:cs_axiom_grounding('913ebd7a-ac2d-4123-af8a-a7bd69cbebd6', standards_bodies_captured_by_installed_base_interests, empirically_contingent).
narrative_ontology:cs_reference_frame('913ebd7a-ac2d-4123-af8a-a7bd69cbebd6', mechanical_jamming_prevention_standard).
narrative_ontology:cs_drift_state('913ebd7a-ac2d-4123-af8a-a7bd69cbebd6', post_electronic_keyboard_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('913ebd7a-ac2d-4123-af8a-a7bd69cbebd6', '').
narrative_ontology:cs_kernel_id(qwerty_persistence__incumbent_preservation_reading, qwerty_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence__incumbent_preservation_reading, keyboard_manufacturers).
narrative_ontology:constraint_beneficiary(qwerty_persistence__incumbent_preservation_reading, trained_touch_typists).
narrative_ontology:constraint_beneficiary(qwerty_persistence__incumbent_preservation_reading, typing_training_institutions).
narrative_ontology:constraint_beneficiary(qwerty_persistence__incumbent_preservation_reading, office_equipment_suppliers).
narrative_ontology:constraint_victim(qwerty_persistence__incumbent_preservation_reading, alternative_layout_adopters).
narrative_ontology:constraint_victim(qwerty_persistence__incumbent_preservation_reading, efficiency_seeking_typists).
narrative_ontology:constraint_victim(qwerty_persistence__incumbent_preservation_reading, dvorak_layout_advocates).
narrative_ontology:constraint_victim(qwerty_persistence__incumbent_preservation_reading, new_entrant_manufacturers).
narrative_ontology:constraint_vindicates(qwerty_persistence__incumbent_preservation_reading, installed_base_lock_in_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Manufacture typewriters and later keyboards to the QWERTY layout, having sunk tooling, supply-chain, and marketing capital into it. They actively lobby against layout changes, refuse to stock alternative-layout devices at scale, and frame QWERTY as a settled standard rather than a contingent choice they benefit from defending. Their exit from the standard would strand existing capital investment.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, keyboard_manufacturers, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(qwerty_persistence__incumbent_preservation_reading, keyboard_manufacturers, beneficiary).

% Have invested years developing muscle memory and professional credentials (typing speed certifications, secretarial qualifications) tied to QWERTY. Their labor-market value depends on the standard staying fixed; switching to an alternative layout would erase a hard-won skill asset. They defend the standard as a professional identity, not merely a habit.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, trained_touch_typists, beneficiary,
    organized, biographical, identity_locked, national).

% Business schools, secretarial colleges, and certification bodies built curricula, textbooks, and testing infrastructure around QWERTY. Switching standards would require rebuilding accredited coursework and would devalue existing certifications they issue and sell.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, typing_training_institutions, beneficiary,
    organized, generational, constrained, national).

% Sell and service QWERTY-standardized equipment to offices; interoperability with the installed base is their core value proposition. They benefit from network effects that punish any office adopting a nonstandard layout.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, office_equipment_suppliers, beneficiary,
    powerful, generational, arbitrage, national).

% Individuals or small offices that adopted or wanted to adopt more ergonomic or faster layouts (e.g., Dvorak) bear compatibility costs, hiring friction (no pool of trained typists), and social costs of being seen as idiosyncratic. Their equipment and training investments are stranded outside the dominant network.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, alternative_layout_adopters, payer,
    powerless, biographical, trapped, national).

% Individual typists who could achieve higher speed and lower repetitive-strain injury risk with an alternative layout but face retraining costs, employer skepticism, and lack of institutional support. They pay in foregone efficiency and physical strain to remain compatible with the dominant standard.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, efficiency_seeking_typists, payer,
    powerless, biographical, constrained, national).

% Engineers and efficiency researchers who developed and promoted alternative layouts with documented performance advantages. They are structurally excluded from standards bodies, procurement decisions, and training curricula dominated by incumbent interests; their evidence is acknowledged but rarely acted upon.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, dvorak_layout_advocates, excluded,
    moderate, biographical, trapped, national).

% Would-be manufacturers of alternative-layout devices face a market where compatibility with the trained labor pool and existing office fleets makes QWERTY devices the only commercially viable product. They pay in foreclosed market opportunity and cannot achieve scale to compete on device merits alone.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, new_entrant_manufacturers, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(qwerty_persistence__incumbent_preservation_reading, new_entrant_manufacturers, excluded).

% Study QWERTY as a canonical case of path dependence and lock-in; they document the beneficiary-defense mechanisms, the disputed empirical basis for Dvorak's claimed superiority, and the institutional actors who benefit from standard persistence.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, economic_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qwerty_persistence__incumbent_preservation_reading, keyboard_manufacturers).
narrative_ontology:fixing_cost_class(qwerty_persistence__incumbent_preservation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A single, universally taught keyboard layout allows typists to be hired interchangeably across offices and manufacturers to produce compatible equipment without per-customer customization — genuine coordination value exists in having ANY common standard.
% TRANSFER_FUNCTION: The arrangement moves retraining costs, ergonomic injury risk, and foreclosed efficiency gains from incumbent manufacturers, trained typists, and training institutions onto would-be adopters of alternative layouts and new entrant manufacturers who cannot achieve compatible scale.
% ABSENT_VOICES: Dvorak layout advocates and ergonomics researchers documenting repetitive strain injury costs are largely absent from standards-setting and procurement decisions; their technical arguments are acknowledged in retrospective histories but were not represented in the manufacturer- and institution-dominated bodies that could have revisited the standard.
% DISAPPEARANCE_RATIONALE: Incumbent beneficiaries (manufacturers, training institutions, trained typists) would experience the world rearranging catastrophically — stranded capital, devalued certifications, erased skill premiums. Efficiency-seeking typists and alternative-layout advocates would argue the world barely changes for end users, since modern touch typists retrain in weeks and software-level layout switching is nearly costless today; the dispute over how much would actually rearrange is itself part of the constraint's contested status.
% FOUNDING_PROBLEM: Early typewriter mechanisms jammed when adjacent typebars were struck in rapid succession; QWERTY was arranged in part to slow typists down and separate commonly paired letters, solving a genuine mechanical constraint of 19th-century typewriter hardware.
% FOUNDING_PROBLEM_CORROBORATION: Mechanical engineers and typewriter historians outside the manufacturer and training-institution beneficiary set attest that typebar-jamming ceased to be a relevant constraint once electric typewriters and later electronic keyboards eliminated mechanical typebars entirely — a transition completed by the mid-20th century, decades before the standard's persistence in question.
narrative_ontology:disappearance_verdict(qwerty_persistence__incumbent_preservation_reading, contested).
narrative_ontology:founding_problem_status(qwerty_persistence__incumbent_preservation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence__incumbent_preservation_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extractiveness rises from 0.18 at the mechanical-era founding (when the coordination function was genuinely load-bearing and near-costless to defend) to 0.61 by the modern endpoint, tracking the accumulation of defensive institutional investment after the founding mechanical problem (typebar jamming) had already been solved by electric typewriters. Theater ratio rises in parallel (0.05 to 0.42) as the 'QWERTY is optimal' or 'QWERTY is now unchangeable' justification increasingly substitutes for the original engineering rationale, which no longer applies. Suppression_requirement rises from 0.10 to 0.58, reflecting a shift from passive network effects (early) to active institutional defense — procurement exclusion, curriculum lock-in, and dismissal of ergonomic evidence (late) — that this reading identifies as the persistence mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Manufacturers, trained typists, and training institutions are declared beneficiaries because each holds an asset (tooling, skill capital, accredited curriculum) whose value depends specifically on QWERTY remaining the standard — the engine should derive low d for these seats. Alternative-layout adopters, efficiency-seeking typists, and new entrant manufacturers are declared victims because they bear the switching costs, foregone efficiency, and market foreclosure that the beneficiary defense imposes — the engine should derive high d for these seats. Dvorak advocates are excluded rather than victimized in the direct extraction sense; their cost is exclusion from the standards conversation, not direct transfer.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (mechanical typebar jamming) is dead — solved decades ago by electric and electronic keyboards that removed the physical typebar entirely. Yet the arrangement persists and has intensified its defensive posture (rising suppression_requirement) well past the point the founding problem disappeared. This is a canonical mandatrophy signature: a coordination mechanism whose original justification has lapsed, now sustained primarily by beneficiary defense of the resulting capital and human-capital investments — precisely the incumbent_preservation_reading's core claim, as distinguished from the lapsed_alternatives_reading which would attribute persistence to Dvorak-style alternatives failing to reach critical mass through no one's active suppression.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    active_suppression_vs_switching_cost_economics,
    'Is the persistence of QWERTY past the mechanical-jamming era best explained by active beneficiary suppression of alternatives, or by ordinary switching-cost economics in which no party need actively suppress anything for the incumbent standard to persist?',
    'Archival evidence of manufacturer lobbying, training-institution curriculum decisions, and standards-body deliberations that explicitly weighed and rejected Dvorak or other alternatives despite documented performance data, versus evidence that alternatives were merely never adopted at sufficient scale absent any active opposition.',
    'If active suppression evidence dominates, this reading (incumbent_preservation) is the structurally accurate account and the constraint is properly tangled_rope with defined victims. If switching-cost economics dominate without active suppression, the sibling lapsed_alternatives_reading better describes the mechanism and this reading''s victim/suppression framing overstates the case.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(active_suppression_vs_switching_cost_economics, empirical, 'Whether QWERTY persistence involves active beneficiary suppression or passive coordination-failure of alternatives.').

omega_variable(
    dvorak_superiority_evidence_quality,
    'How robust is the empirical evidence that Dvorak or other alternative layouts offer meaningfully superior speed or ergonomic outcomes over QWERTY, once controlling for training effects and researcher bias in the original comparative studies?',
    'Independent modern replication of layout-comparison studies controlling for typist selection and training investment, distinct from the original mid-20th-century Navy studies conducted by a Dvorak-affiliated researcher.',
    'If alternative-layout superiority is weak or unreplicated, the victim framing (efficiency_seeking_typists, alternative_layout_adopters as bearing real foregone efficiency) is overstated and the constraint''s extraction claim weakens toward a genuine rope with negligible foregone value. If robust, the extraction framing is well-supported.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dvorak_superiority_evidence_quality, empirical, 'Whether claimed alternative-layout performance advantages are empirically robust.').

omega_variable(
    founding_problem_dissolution_timing,
    'Precisely when did the founding mechanical problem (typebar jamming) become irrelevant, and does defensive institutional behavior demonstrably postdate that dissolution rather than predate or coincide with it?',
    'Cross-reference manufacturing transition dates (mechanical to electric to electronic keyboards) against documented instances of standards-body or institutional resistance to layout change.',
    'If defensive behavior clearly postdates the mechanical problem''s dissolution, mandatrophy is well-established. If defensive behavior substantially predates or coincides with the mechanical era, the extraction narrative is weaker and the standard may have always carried some beneficiary-defense component alongside genuine coordination value.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_dissolution_timing, empirical, 'Timing relationship between founding-problem dissolution and onset of defensive institutional behavior.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence__incumbent_preservation_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t0, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(qwer_tr_t0, observed).
narrative_ontology:measurement(qwer_tr_t20, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement_basis(qwer_tr_t20, observed).
narrative_ontology:measurement(qwer_tr_t40, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 40, 0.25).
narrative_ontology:measurement_basis(qwer_tr_t40, observed).
narrative_ontology:measurement(qwer_tr_t60, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 60, 0.33).
narrative_ontology:measurement_basis(qwer_tr_t60, observed).
narrative_ontology:measurement(qwer_tr_t80, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 80, 0.39).
narrative_ontology:measurement_basis(qwer_tr_t80, observed).
narrative_ontology:measurement(qwer_tr_t100, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 100, 0.42).
narrative_ontology:measurement_basis(qwer_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(qwer_be_t0, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(qwer_be_t0, observed).
narrative_ontology:measurement(qwer_be_t20, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 20, 0.32).
narrative_ontology:measurement_basis(qwer_be_t20, observed).
narrative_ontology:measurement(qwer_be_t40, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 40, 0.45).
narrative_ontology:measurement_basis(qwer_be_t40, observed).
narrative_ontology:measurement(qwer_be_t60, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 60, 0.54).
narrative_ontology:measurement_basis(qwer_be_t60, observed).
narrative_ontology:measurement(qwer_be_t80, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 80, 0.59).
narrative_ontology:measurement_basis(qwer_be_t80, observed).
narrative_ontology:measurement(qwer_be_t100, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 100, 0.61).
narrative_ontology:measurement_basis(qwer_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(qwer_su_t0, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement_basis(qwer_su_t0, observed).
narrative_ontology:measurement(qwer_su_t20, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 20, 0.22).
narrative_ontology:measurement_basis(qwer_su_t20, observed).
narrative_ontology:measurement(qwer_su_t40, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 40, 0.34).
narrative_ontology:measurement_basis(qwer_su_t40, observed).
narrative_ontology:measurement(qwer_su_t60, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 60, 0.45).
narrative_ontology:measurement_basis(qwer_su_t60, observed).
narrative_ontology:measurement(qwer_su_t80, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 80, 0.53).
narrative_ontology:measurement_basis(qwer_su_t80, observed).
narrative_ontology:measurement(qwer_su_t100, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 100, 0.58).
narrative_ontology:measurement_basis(qwer_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence__incumbent_preservation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(qwerty_persistence__incumbent_preservation_reading, 0.1).
narrative_ontology:affects_constraint(qwerty_persistence__incumbent_preservation_reading, lapsed_alternatives_reading).

% DUAL FORMULATION NOTE:
% This story and lapsed_alternatives_reading are two readings of the same qwerty_persistence kernel, decomposed per the ε-invariance principle rather than modeled as one constraint with a measurement parameter. incumbent_preservation_reading claims tangled_rope with rising defensive extraction (ε=0.61 at interval end, suppression_requirement rising to 0.58); lapsed_alternatives_reading is expected to claim a coordination-dominant type (rope or scaffold-adjacent) with lower extraction, attributing persistence to critical-mass failure rather than active suppression. Both stories share the same historical substrate (the QWERTY standard) but assign structurally different ε and different victim sets, which is exactly the disambiguation the ε-invariance test requires rather than blending the two accounts into one story with a hidden observable parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
