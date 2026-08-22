% ============================================================================
% CONSTRAINT STORY: qwerty_persistence__lapsed_alternatives_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   constraint_id: qwerty_persistence__lapsed_alternatives_reading
 *   human_readable: QWERTY Keyboard Layout Persistence — Coordination/Switching-Cost Reading
 *   domain: technology_history/industrial_standards/path_dependence
 *
 * SUMMARY:
 *   This story instantiates the 'lapsed alternatives' reading of the QWERTY
 *   persistence kernel: the keyboard layout persists not because any party
 *   actively suppresses competitors, but because coordination value —
 *   everyone benefiting from everyone else using the same standard — makes
 *   switching costly for all parties symmetrically, and alternative layouts
 *   (Dvorak, Colemak) simply never accumulated the critical mass of
 *   independent adopters needed to make individual switching worthwhile.
 *   Under this reading there is no concentrated beneficiary extracting from a
 *   victim class; keyboard manufacturers, software vendors, and typists are
 *   ALL coordination beneficiaries, and the sidelining of alternative
 *   designers is a mass-action failure, not a suppression campaign. This is
 *   one of two readings of the same kernel — the sibling
 *   incumbent_preservation_reading holds instead that manufacturers and
 *   vendors actively defend QWERTY to protect sunk capital investment in
 *   tooling, training infrastructure, and habituated user bases, which would
 *   locate a beneficiary class deliberately blocking a
 *   switching-cost-reducing alternative. The two readings share the
 *   observable (QWERTY has persisted for 150 years despite documented
 *   alternatives) but author entirely different epsilon, beneficiary
 *   structure, and coordination/extraction balance.
 *
 * KEY AGENTS:
 *   - typists_and_typing_learners: primary coordination beneficiaries — skills transfer only because everyone uses the same layout
 *   - keyboard_manufacturers: coordination beneficiaries with mobile exit — would retool without resistance if demand shifted
 *   - software_and_os_vendors: coordination beneficiaries with mobile exit — support alternatives already at negligible cost
 *   - alternative_layout_designers: excluded not by suppression but by failure to reach adoption critical mass
 *   - typing_educators_and_standards_bodies: agenda-setters who track prevailing practice rather than defend it
 *   - economic_historians: analytical observers adjudicating between this reading and the incumbent-preservation sibling
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence__lapsed_alternatives_reading, 0.18).
domain_priors:suppression_score(qwerty_persistence__lapsed_alternatives_reading, 0.12).
domain_priors:theater_ratio(qwerty_persistence__lapsed_alternatives_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence__lapsed_alternatives_reading, rope).
narrative_ontology:human_readable(qwerty_persistence__lapsed_alternatives_reading, "QWERTY Keyboard Layout Persistence — Coordination/Switching-Cost Reading").
narrative_ontology:topic_domain(qwerty_persistence__lapsed_alternatives_reading, "technology_history/industrial_standards/path_dependence").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence__lapsed_alternatives_reading, '2d95d8e2-39a5-4479-bf88-7b48a4154401').
narrative_ontology:cs_kernel_codification('2d95d8e2-39a5-4479-bf88-7b48a4154401', distributed).
narrative_ontology:cs_authority_grounding('2d95d8e2-39a5-4479-bf88-7b48a4154401', distributed).
narrative_ontology:cs_reading_relation('2d95d8e2-39a5-4479-bf88-7b48a4154401', qwerty_persistence__incumbent_preservation_reading, coexists_with).
narrative_ontology:cs_axiom('2d95d8e2-39a5-4479-bf88-7b48a4154401', foundational, persistence_explained_by_symmetric_coordination_cost).
narrative_ontology:cs_axiom_status(persistence_explained_by_symmetric_coordination_cost, holdable).
narrative_ontology:cs_axiom_grounding('2d95d8e2-39a5-4479-bf88-7b48a4154401', persistence_explained_by_symmetric_coordination_cost, empirically_contingent).
narrative_ontology:cs_axiom('2d95d8e2-39a5-4479-bf88-7b48a4154401', secondary, no_concentrated_beneficiary_captures_switching_cost).
narrative_ontology:cs_axiom_status(no_concentrated_beneficiary_captures_switching_cost, holdable).
narrative_ontology:cs_axiom_grounding('2d95d8e2-39a5-4479-bf88-7b48a4154401', no_concentrated_beneficiary_captures_switching_cost, empirically_contingent).
narrative_ontology:cs_reference_frame('2d95d8e2-39a5-4479-bf88-7b48a4154401', mechanical_jam_prevention_original_design).
narrative_ontology:cs_drift_state('2d95d8e2-39a5-4479-bf88-7b48a4154401', contemporary_electronic_keyboard_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('2d95d8e2-39a5-4479-bf88-7b48a4154401', '').
narrative_ontology:cs_kernel_id(qwerty_persistence__lapsed_alternatives_reading, qwerty_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence__lapsed_alternatives_reading, typists_and_typing_learners).
narrative_ontology:constraint_beneficiary(qwerty_persistence__lapsed_alternatives_reading, keyboard_manufacturers).
narrative_ontology:constraint_beneficiary(qwerty_persistence__lapsed_alternatives_reading, software_and_os_vendors).
narrative_ontology:constraint_vindicates(qwerty_persistence__lapsed_alternatives_reading, network_effect_lock_in_theory).
narrative_ontology:constraint_vindicates(qwerty_persistence__lapsed_alternatives_reading, critical_mass_adoption_threshold_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Learn QWERTY because every keyboard, school curriculum, and typing tutor teaches it. Benefit from a single universal layout: skills transfer across every device and job without retraining. Switching to an alternative layout alone would mean relearning muscle memory with no one else to type with or hand a device to.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, typists_and_typing_learners, beneficiary,
    powerless, biographical, constrained, global).

% Manufacture QWERTY as the default because it is what every buyer expects and every supply chain is tooled for. Could produce alternative layouts (and do, as niche options) at modest retooling cost, and switching demand toward an alternative would be followed without resistance if it appeared.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, keyboard_manufacturers, beneficiary,
    organized, generational, mobile, global).

% Ship QWERTY as the default input mapping because it matches near-universal user expectation and physical hardware. Support alternative layouts as a settings option at negligible cost; they have no structural stake in preventing an alternative from spreading if users demanded it.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, software_and_os_vendors, beneficiary,
    institutional, generational, mobile, global).

% Designed and promoted alternative layouts (Dvorak, Colemak, and others) on ergonomic or efficiency grounds. Their proposals remain available and technically supported everywhere, but never crossed the adoption threshold needed for a second party to make learning one worthwhile — the exclusion here is failure to reach critical mass, not a barrier erected against them.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, alternative_layout_designers, excluded,
    powerless, biographical, arbitrage, global).

% Set curricula and certification standards around QWERTY because it is the layout learners will actually use afterward. Would teach an alternative layout if it became the thing employers and devices expected; the choice tracks prevailing practice rather than defending it.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, typing_educators_and_standards_bodies, agenda_setter,
    organized, generational, mobile, national).

% Study why QWERTY persisted despite documented ergonomic alternatives. Assess whether the persistence is best explained by coordination value (this reading) or by active incumbent defense of sunk capital (the sibling reading).
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, economic_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qwerty_persistence__lapsed_alternatives_reading, diffuse).
narrative_ontology:fixing_cost_class(qwerty_persistence__lapsed_alternatives_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Everyone who types benefits from every other typist, manufacturer, and software vendor using the same key mapping — a shared layout lets skills, hardware, and training transfer freely across people and devices. This is the coordination problem QWERTY persistence solves: pick one standard, everyone benefits from everyone else having picked it too.
% TRANSFER_FUNCTION: Nothing is extracted and transferred from one party to another under this reading; the arrangement's cost is the shared switching cost every participant would pay to move to any alternative, borne symmetrically rather than transferred to a subordinate party.
% ABSENT_VOICES: Alternative-layout designers and advocates (Dvorak proponents, Colemak communities) are structurally sidelined, but not by suppression — by the coordination math itself: no one benefits from switching alone, so their technically-superior proposals never accumulate the critical mass of adopters needed to make switching individually rational.
% DISAPPEARANCE_RATIONALE: Under this reading, if QWERTY 'disappeared' (all keyboards reset to blank and users had to choose fresh), coordination would simply reconstitute around whichever layout reached critical mass first — the world would not obviously be better or worse, just re-coordinated. This is contested against the sibling incumbent-preservation reading, which holds that removing entrenched capital interests would let genuinely superior alternatives win, meaning the world WOULD rearrange for the better.
% FOUNDING_PROBLEM: Early typewriter mechanisms jammed when adjacent typebars were struck in quick succession; QWERTY was arranged partly to slow common letter-pairs and reduce jams, and this arrangement was then standardized so trained typists, textbooks, and machines could be interoperable.
% FOUNDING_PROBLEM_CORROBORATION: Mechanical typewriter engineers and typewriter-history scholars (outside any current beneficiary group — keyboard manufacturers and software vendors today have no stake in the jam-prevention rationale) attest the original mechanical-jamming problem vanished with electric and electronic keyboards decades ago; under THIS reading, the arrangement's continued existence is fully accounted for by the coordination value of a shared standard, independent of the original mechanical rationale — it is not a zombie extraction, it is a coordination equilibrium that outlived its founding cause for an unrelated (and legitimate) reason.
narrative_ontology:disappearance_verdict(qwerty_persistence__lapsed_alternatives_reading, contested).
narrative_ontology:founding_problem_status(qwerty_persistence__lapsed_alternatives_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence__lapsed_alternatives_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(qwerty_persistence__lapsed_alternatives_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence__lapsed_alternatives_reading, 0.18, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored low (0.18) and rises only slowly over the interval, because under this reading the entire cost of the arrangement is the shared, symmetric switching cost every party would bear to coordinate on a different standard — there is no party collecting rent from another's inability to exit. Suppression is low (0.12): nothing coercive prevents an individual from learning Dvorak; the constraint is emergent from adoption-network math, not enforcement. Theater ratio stays low and flat (0.08 by interval end) because there is minimal performative maintenance — no one is staging defense of QWERTY, they are simply using what everyone else uses. Accessibility collapse is moderately high (0.62) reflecting the genuine difficulty of coordinating a mass switch even though the alternative is technically available on every device today — this is coordination failure, not suppression. Resistance is low (0.2): almost no one actively resists switching in principle; they simply lack a coordinated partner to switch with.
 *
 * PERSPECTIVAL GAP:
 *   Every stakeholder here computes close to the same seat-type under this reading — typists, manufacturers, and vendors are ALL beneficiaries of the coordination equilibrium, which is the structural signature this reading predicts (Rope, no concentrated victim). The sibling reading would instead produce sharp seat divergence: manufacturers and vendors as agenda-setting beneficiaries defending sunk capital, and would-be switchers as bearing an artificially inflated (rather than merely coordination-natural) switching cost.
 *
 * DIRECTIONALITY LOGIC:
 *   Under this reading, directionality sits near symmetric-to-beneficiary for every named agent: typists benefit from universal skill transfer, manufacturers and vendors benefit from stable, predictable demand, and even alternative-layout designers are not structurally extracted from — their proposals remain live and legally/technically unblocked, they simply never won the coordination game. No victim group is declared because this reading holds that the cost (switching friction) is symmetric across all participants rather than transferred from a payer class to a beneficiary class.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading directly addresses the mandatrophy question the kernel poses: has the QWERTY arrangement outlived its founding function (jam prevention on mechanical typewriters) while persisting for an illegitimate reason? This reading's answer is NO — the founding problem is dead, but the arrangement's CURRENT persistence is fully and legitimately accounted for by an independent, still-live coordination function (network value of a shared standard), not by zombie defense of the original mechanical rationale or by rent extraction. This is what distinguishes a Rope-classified lapsed-standard-transition story from a Piton: a Piton persists by inertia despite no one being served; this reading holds QWERTY persists because coordination value is a genuine, present-tense benefit to all parties, independent of history.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_incumbent_defense_ambiguity,
    'Is QWERTY''s 150-year persistence best explained by pure coordination value (this reading) or by active incumbent defense of sunk capital investment in manufacturing, training infrastructure, and habituated user bases (the incumbent_preservation_reading)?',
    'Historical analysis of whether keyboard manufacturers or software vendors ever took affirmative action to block, discredit, or raise switching costs for alternative layouts beyond passive non-adoption — e.g., patent enforcement against alternative-layout hardware, refusal to support alternative layouts in software, lobbying against typing-curriculum changes. Absence of such affirmative blocking action favors this reading; presence of it favors the sibling.',
    'If affirmative incumbent blocking action is found, this reading''s core premise (symmetric coordination cost, no concentrated beneficiary/victim split) would be undermined and the sibling incumbent_preservation_reading would better fit the evidence — the constraint would need reclassification toward Tangled Rope or Snare with a declared victim set.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_incumbent_defense_ambiguity, conceptual, 'Whether QWERTY persistence is a coordination equilibrium or the product of active incumbent defense — the central kernel-level ambiguity between the two sibling readings.').

omega_variable(
    critical_mass_threshold_measurability,
    'What adoption share would an alternative layout actually need to reach before individual switching becomes rational, and has any alternative come close to that threshold?',
    'Network-effect modeling using observed adoption curves for Dvorak/Colemak among self-selected efficiency-motivated typists, compared against modeled critical-mass thresholds for keyboard-layout coordination games.',
    'If empirical adoption never approached even a fraction of the modeled threshold, that supports the lapsed-alternatives reading (the coordination failure was structural and inevitable, not artificially blocked). If adoption approached the threshold and then was suppressed by identifiable action, that would support the sibling reading instead.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(critical_mass_threshold_measurability, empirical, 'Whether observed alternative-layout adoption ever approached a measurable critical-mass threshold.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence__lapsed_alternatives_reading, 0, 140).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t0, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 0, 0.02).
narrative_ontology:measurement(qwer_tr_t20, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 20, 0.03).
narrative_ontology:measurement(qwer_tr_t50, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 50, 0.05).
narrative_ontology:measurement(qwer_tr_t80, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 80, 0.06).
narrative_ontology:measurement(qwer_tr_t110, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 110, 0.07).
narrative_ontology:measurement(qwer_tr_t140, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 140, 0.08).

% Extraction over time
narrative_ontology:measurement(qwer_be_t0, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(qwer_be_t20, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 20, 0.08).
narrative_ontology:measurement(qwer_be_t50, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 50, 0.12).
narrative_ontology:measurement(qwer_be_t80, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 80, 0.15).
narrative_ontology:measurement(qwer_be_t110, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 110, 0.17).
narrative_ontology:measurement(qwer_be_t140, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 140, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(qwerty_persistence__lapsed_alternatives_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence__lapsed_alternatives_reading, information_standard).
narrative_ontology:boltzmann_floor_override(qwerty_persistence__lapsed_alternatives_reading, 0.03).
narrative_ontology:affects_constraint(qwerty_persistence__lapsed_alternatives_reading, qwerty_persistence__incumbent_preservation_reading).

% DUAL FORMULATION NOTE:
% This story and qwerty_persistence__incumbent_preservation_reading are sibling readings of a single kernel (qwerty_persistence). Both describe the same observable historical record (QWERTY's persistence despite documented alternatives) but author structurally distinct claims: this reading holds extraction is low and symmetric (pure coordination cost, epsilon=0.18, no victim set), while the incumbent-preservation reading holds extraction is concentrated and actively defended (a beneficiary class protecting sunk capital against a payer class bearing artificially inflated switching costs, expected higher epsilon). Per the epsilon-invariance principle, these are two constraints, not one constraint measured two ways — decomposed into separate files and linked here.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
