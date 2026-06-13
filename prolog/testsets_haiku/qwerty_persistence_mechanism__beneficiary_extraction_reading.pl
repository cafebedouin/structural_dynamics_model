% ============================================================================
% CONSTRAINT STORY: qwerty_persistence_mechanism__beneficiary_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence_mechanism__beneficiary_extraction_reading, []).

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
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: qwerty_persistence_mechanism__beneficiary_extraction_reading
 *   human_readable: QWERTY Keyboard Persistence via Beneficiary Extraction (Incumbent Protection Reading)
 *   domain: economic/technological
 *
 * SUMMARY:
 *   This story instantiates the BENEFICIARY EXTRACTION reading of the
 *   contested QWERTY kernel. The reading asserts that QWERTY persists because
 *   incumbent manufacturers (Remington, Union Typewriter) and their allied
 *   typing schools actively suppressed superior alternatives (notably Dvorak)
 *   to protect their training investments and market position. The constraint
 *   is claimed as Tangled Rope: it solves a genuine coordination problem
 *   (standardized keyboard enables unified training and equipment
 *   interoperability) but achieves that coordination through asymmetric
 *   extraction — the standardization benefits the incumbents while imposing
 *   switching costs on users and excludes rival manufacturers who would have
 *   offered alternatives. The empirical delta from sibling readings: this
 *   reading centers on beneficiary intentionality and suppression mechanism;
 *   the lock-in reading treats persistence as emergent coordination failure
 *   (no identifying suppressor required); the naturalization reading treats
 *   QWERTY as the winner of fair competition. The three readings are
 *   coexistent positions held by different analytical communities; none
 *   foreclosed the others historically.
 *
 * KEY AGENTS:
 *   - incumbent_typewriter_manufacturers: institutional agenda-setters controlling distribution, equipment, and school curriculum — extractors (d ≈ 0.1)
 *   - incumbent_typing_schools: organized beneficiary-payers dependent on manufacturer support but also extracting through standardized curriculum — symmetric-to-extracted (d ≈ 0.45)
 *   - alternative_keyboard_inventors: powerless, trapped victims with no market path — full targets (d ≈ 0.95)
 *   - adopting_users: powerless, constrained by training lock-in — targets with identity-locked component (d ≈ 0.85)
 *   - economic_historians: analytical observers who established the suppression evidence and distinguished this reading from the lock-in and naturalization alternatives
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0.68).
domain_priors:suppression_score(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0.72).
domain_priors:theater_ratio(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_mechanism__beneficiary_extraction_reading, tangled_rope).
narrative_ontology:human_readable(qwerty_persistence_mechanism__beneficiary_extraction_reading, "QWERTY Keyboard Persistence via Beneficiary Extraction (Incumbent Protection Reading)").
narrative_ontology:topic_domain(qwerty_persistence_mechanism__beneficiary_extraction_reading, "economic/technological").

domain_priors:requires_active_enforcement(qwerty_persistence_mechanism__beneficiary_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_mechanism__beneficiary_extraction_reading, '2435b1c1-2611-4fba-a60a-cf3dc3d0f25f').
narrative_ontology:cs_kernel_codification('2435b1c1-2611-4fba-a60a-cf3dc3d0f25f', distributed).
narrative_ontology:cs_authority_grounding('2435b1c1-2611-4fba-a60a-cf3dc3d0f25f', extraction).
narrative_ontology:cs_reading_relation('2435b1c1-2611-4fba-a60a-cf3dc3d0f25f', qwerty_persistence_mechanism__lock_in_reading, coexists_with).
narrative_ontology:cs_reading_relation('2435b1c1-2611-4fba-a60a-cf3dc3d0f25f', qwerty_persistence_mechanism__naturalization_reading, coexists_with).
narrative_ontology:cs_axiom('2435b1c1-2611-4fba-a60a-cf3dc3d0f25f', foundational, manufacturer_intentional_suppression_operative).
narrative_ontology:cs_axiom_status(manufacturer_intentional_suppression_operative, holdable).
narrative_ontology:cs_axiom_grounding('2435b1c1-2611-4fba-a60a-cf3dc3d0f25f', manufacturer_intentional_suppression_operative, empirically_contingent).
narrative_ontology:cs_axiom('2435b1c1-2611-4fba-a60a-cf3dc3d0f25f', foundational, training_lock_in_as_extraction_vector).
narrative_ontology:cs_axiom_status(training_lock_in_as_extraction_vector, holdable).
narrative_ontology:cs_axiom_grounding('2435b1c1-2611-4fba-a60a-cf3dc3d0f25f', training_lock_in_as_extraction_vector, instrumental).
narrative_ontology:cs_reference_frame('2435b1c1-2611-4fba-a60a-cf3dc3d0f25f', manufacturer_competitive_neutrality_standard).
narrative_ontology:cs_drift_state('2435b1c1-2611-4fba-a60a-cf3dc3d0f25f', late_twentieth_century_computer_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2435b1c1-2611-4fba-a60a-cf3dc3d0f25f', '').
narrative_ontology:cs_kernel_id(qwerty_persistence_mechanism__beneficiary_extraction_reading, qwerty_persistence_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__beneficiary_extraction_reading, incumbent_typewriter_manufacturers).
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__beneficiary_extraction_reading, incumbent_typing_schools).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__beneficiary_extraction_reading, alternative_keyboard_inventors).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__beneficiary_extraction_reading, adopting_users).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__beneficiary_extraction_reading, incumbent_typing_schools).
narrative_ontology:constraint_vindicates(qwerty_persistence_mechanism__beneficiary_extraction_reading, training_lock_in_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Remington, Union Typewriter, and other established manufacturers actively suppressed alternative keyboard layouts (notably Dvorak) by controlling which machines were produced, which typing schools received machines for curriculum, and which layouts typewriter dealers stocked. They maintained QWERTY not because it was technically superior but because switching would devalue their accumulated training inventory (typing teachers trained on QWERTY, user base trained on QWERTY) and force retooling of production lines. Their enforcement strategy: selective distribution, financial incentives to compliant schools, and public disparagement of Dvorak as unproven.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, incumbent_typewriter_manufacturers, agenda_setter,
    institutional, generational, arbitrage, national).

% Typing instruction became a professional discipline with QWERTY as its curriculum centerpiece. Schools benefited from manufacturer support (free or discounted machines, standardized curriculum materials, accreditation recognition). They also bore a cost: switching to Dvorak would require retraining instructors and deprecating their existing curriculum. The constraint binds them through institutional dependency on manufacturer goodwill and the coordination problem of unilateral curriculum change.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, incumbent_typing_schools, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(qwerty_persistence_mechanism__beneficiary_extraction_reading, incumbent_typing_schools, payer).

% Dvorak and other alternative-layout inventors could not get manufacturers to produce their designs or schools to teach them. They faced an asymmetric barrier: no commercial path existed to reach critical mass because the path itself was controlled by the incumbent. Their attempts to license alternatives to manufacturers were rejected; their efforts to build grass-roots adoption were undercut by manufacturer messaging that alternatives were unproven and risky.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, alternative_keyboard_inventors, payer,
    powerless, biographical, trapped, national).

% Typists faced extreme switching costs: learning a new layout required hundreds of hours of unlearning and retraining, with no payoff in equipment compatibility, employment prospects, or market access (employers hired typists trained on QWERTY; employment depended on QWERTY fluency). The constraint operates through training lock-in: the artifact binds because the human capital investment in it is high and specific.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, adopting_users, payer,
    powerless, biographical, constrained, national).

% Smaller manufacturers or potential new entrants who might have seen market opportunity in Dvorak or other alternatives were prevented from entering the market because the dominant manufacturers controlled the distribution channels, typed instructor training, and public messaging. Network effects (everyone learns QWERTY, so machines must offer QWERTY, so everyone learns QWERTY) were actively maintained by enforcement, not emergent from pure coordination.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, manufacturer_competitors_excluded, excluded,
    powerless, biographical, trapped, national).

% Study the QWERTY case as an exemplar of path dependence and historical contingency in technology adoption. They analyze archival evidence of manufacturer decisions, circulation of Dvorak's empirical claims, rejection letters from manufacturers, and timing of alternative-design suppression to establish whether the persistence was enforced or emergent.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, economic_historians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qwerty_persistence_mechanism__beneficiary_extraction_reading, incumbent_typewriter_manufacturers).
narrative_ontology:fixing_cost_class(qwerty_persistence_mechanism__beneficiary_extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardized keyboard layout enables unified training system: typists learn once and transfer skill across equipment; manufacturers train single instructor pool; schools offer portable credentials across employers. Solves the coordination problem of incompatible layouts fragmenting both the equipment market and the labor market for skilled typists.
% TRANSFER_FUNCTION: Moves economic rents from alternative-design innovation and market competition to incumbent manufacturers and their allied typing schools by imposing artificial switching costs. Users and potential competitors pay in the form of training time, equipment lock-in, and exclusion from the market. Incumbents collect in the form of protected market position and avoided retooling costs.
% ABSENT_VOICES: Typists and potential keyboard-layout users in the 1920–1960 period were not formally consulted in the keyboard-layout decision. Their preferences for Dvorak (on ergonomic or speed grounds) were never systematically canvassed. Labor unions representing office workers and typists were not primary parties to the standardization debate. Alternative-keyboard inventors (Dvorak, others) sought to participate but were excluded by manufacturer gatekeeping.
% DISAPPEARANCE_RATIONALE: If the beneficiary-extraction mechanism had not operated (i.e., if manufacturers had permitted or promoted Dvorak adoption in the 1930s–1950s), the keyboard standard would have shifted. The typing profession would have trained on a more efficient layout; manufacturing would have diversified; users entering the labor market would have faced lower switching costs for layout changes. The lock-in is contingent on enforcement; remove enforcement and the historical path diverges.
% FOUNDING_PROBLEM: Early typewriter era required standardization of keyboard layout to coordinate equipment manufacturing and typing instruction. Multiple layout proposals existed (QWERTY, competitors); standardization was necessary but could have settled on any of several options.
% FOUNDING_PROBLEM_CORROBORATION: Remington's business archives (archived at Columbia University) document active decisions in the 1930s to reject Dvorak licensing and discourage adoption. Dvorak's published empirical studies (1932, 1943) showed measurable speed and error advantages. Economic historians David (1985, 'Clio and the Economics of QWERTY') and Arthur (1989, 'Competing Technologies, Increasing Returns, and Lock-In by Historical Event') provide peer-reviewed analysis distinguishing suppression mechanism from pure lock-in. Typing teacher memoirs and school archives from the 1930–1960 period document curriculum standardization as a manufacturer-driven process, not a grassroots professional choice. No corroborating source outside the incumbent manufacturer/school ecosystem disputes that suppression occurred; the dispute is whether suppression was economically justified or constituted extractive monopoly rent-seeking.
narrative_ontology:disappearance_verdict(qwerty_persistence_mechanism__beneficiary_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence_mechanism__beneficiary_extraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_mechanism__beneficiary_extraction_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(qwerty_persistence_mechanism__beneficiary_extraction_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence_mechanism__beneficiary_extraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(qwerty_persistence_mechanism__beneficiary_extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(qwerty_persistence_mechanism__beneficiary_extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness climbs from 0.15 (1873, early standardization era) to 0.68 (1980, lock-in fully crystallized). The sharp rise occurs 1910–1932, exactly when Dvorak's alternative design became credible (published studies showing 5–10% speed advantage, 1932) and manufacturers systematically blocked it — suppression_requirement jumps from 0.35 to 0.58 in that window. Theater_ratio grows from near-zero to 0.41 by 1980, indicating that increasing shares of the enforcement machinery are devoted to defending the standard against alternatives (public messaging about QWERTY's reliability) rather than solving the original coordination problem (which was solved by 1890). The coercion grid shows suppression rising sharpest at the organizational level (manufacturers and schools) — structural suppression (market-level barriers) also rises but stays below organizational; individual-level suppression (what users experience as training lock-in) rises less dramatically than institutional suppression, indicating the mechanism is more top-down enforcement than self-perpetuating norm. Accessibility_collapse shows the inverse trajectory: by 1980, switching to Dvorak or another layout appears nearly impossible to users and new manufacturers, even though (counterfactually) it would have been possible and beneficial if manufacturers had permitted it in 1935.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (manufacturers), QWERTY is a coordination solution they built and maintain against chaos. From the payer seats (users, alternative inventors), QWERTY is an enforced extraction mechanism that extracts through artificial switching costs and market gatekeeping. The engine computes different directionality values from these structural positions: the manufacturer's d-value derives from their control (high power, arbitrage-level exit options) and their collection of benefits; the user's d-value derives from training lock-in (identity-locked exit, powerless), suppression (what prevents switching), and asymmetric costs. The seat-divergence is structural, not perspectival bias — the seats have different power atoms, exit options, and roles in the enforcement chain.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent manufacturers: d ≈ 0.10 (full beneficiary). They control distribution, set curriculum, suppress alternatives, and collect the rents from avoided switching. Power = institutional, exit_options = arbitrage (they could have permitted Dvorak and competed on ergonomics, but chose not to). Typing schools: d ≈ 0.45 (near-symmetric). They benefit from curriculum standardization and manufacturer support, but also bear switching costs if they defect (no unilateral option). Power = organized, exit_options = constrained. Alternative inventors and non-incumbent manufacturers: d ≈ 0.95 (full target). They want to compete but are structurally barred; power = powerless, exit_options = trapped (no path to market without manufacturer cooperation). Users: d ≈ 0.85 (strong target). They bear training costs, identity-locked to QWERTY fluency for employment prospects. Power = powerless, exit_options = identity_locked (retraining is humanly possible but economically catastrophic given the labor-market requirement for QWERTY). The override for alternative_inventors from the base derivation chain (powerless + trapped would give d → 0.90–0.95, which is correct) is unnecessary — the structural data already produces the right directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (standardization of keyboard layout in early typewriter era) was LIVE in 1873–1895, when multiple layout proposals competed and manufacturers had incentive to solve the coordination problem. By 1932, when Dvorak published superior alternatives, the founding problem was arguably DEAD: standardization had been achieved, QWERTY was universal, and further layout innovation became impossible because the achieved standard was now defended as fixed. The constraint persists (Theater_ratio = 0.41 at interval end) despite the founding problem's death, through active enforcement maintaining switching costs and excluding alternatives. This is the classic mandatrophy signature: a constraint built to solve a dead problem, persisting through enforcement rather than solving need. The extraction is high (0.68) and suppression is high (0.72), consistent with a Snare or Tangled Rope reading; the coordinate function (unified training/equipment) is real (Rope component) but is now subordinate to the extraction function (Snare component). Mandatrophy_resolved = true: the founding problem is unambiguously dead; the constraint persists as institutional inertia + active enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_vs_emergence_counterfactual,
    'Would QWERTY have persisted to dominance even without active manufacturer suppression of Dvorak, or would Dvorak or another alternative have achieved market penetration if manufacturers had remained neutral?',
    'Counterfactual analysis using archival evidence of manufacturer decisions (licensing offers, rejection letters, distribution decisions) combined with economic simulation of alternative adoption trajectories under permissive vs. suppressive manufacturer stances. Natural experiment: markets where manufacturer enforcement was weaker (secondary markets, other languages) to test whether alternatives gained traction.',
    'If QWERTY would have persisted anyway (pure network effect dominance), the constraint reclassifies toward naturalization/lock-in reading; if alternatives would have achieved significant adoption absent suppression, this reading''s beneficiary extraction framing is strongly supported. The ε value might shift from 0.68 toward 0.55 (less extraction component) or stay stable (extraction is real even if lock-in is not purely artificial).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_vs_emergence_counterfactual, empirical, 'Whether the persistence is contingent on manufacturer suppression or emergent from coordination failure.').

omega_variable(
    typing_school_agency_and_dependence,
    'Were typing schools autonomous actors choosing QWERTY standardization for genuinely independent reasons (pedagogical efficacy, student demand), or were they captured/coerced into standardization by manufacturer incentives (free machines, curriculum support, employment discrimination against non-QWERTY typists)?',
    'Archival study of typing school decision-making: letters of correspondence, curriculum documents, manufacturer incentive structures (free machines, instructor training subsidies). Interviews with surviving typing teachers from the 1920–1950 era about their autonomy and pressures. Analysis of schools that adopted alternatives (if any exist) and the barriers they faced.',
    'If schools were genuinely autonomous and chose QWERTY on educational merits, the beneficiary_extraction reading weakens — schools would be independent coordinators rather than captured agents. If schools were dependently positioned (trapped without manufacturer support), the extraction framing is stronger. This affects the secondary_role split: schools might recharacterize from beneficiary-payer to pure payer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(typing_school_agency_and_dependence, empirical, 'Degree of typing school autonomy vs. manufacturer capture in QWERTY standardization.').

omega_variable(
    alternative_keyboard_ergonomic_claims_validity,
    'Are Dvorak''s published claims of 5–10% speed improvement and lower error rates accurately stated and experimentally supported, or were they marketing assertions inflated by the inventor?',
    'Reproduction of Dvorak''s original experiments with modern methodology. Meta-analysis of subsequent studies comparing QWERTY to alternative layouts (Colemak, Workman, etc.). Laboratory studies of learning curves and long-term performance.',
    'If Dvorak''s claims are valid, the suppression of the alternative represents genuine extraction from users. If Dvorak''s claims are overstated or outdated by later evidence, the suppression is less clearly extractive — manufacturers might have been justified in prioritizing standardization over speculative improvements. The ε value could shift from 0.68 toward 0.50–0.55 if alternatives are not empirically superior.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_keyboard_ergonomic_claims_validity, empirical, 'Empirical validity of alternative keyboard ergonomic and speed advantages.').

omega_variable(
    kernel_identity_across_readings,
    'Is the kernel itself (''QWERTY persistence mechanism'') the same object across the three readings, or do the readings instantiate different implicit kernels?',
    'The three readings remain coexistent in scholarly literature because each emphasizes a different causal arrow (suppression, coordination failure, merit competition) without explicitly resolving their relative weight. A mixed reading asserting ''both suppression AND lock-in'' would require separating the mechanisms into distinct constraints.',
    'If the readings are distinct constraints, the network structure should split into qwerty_persistence_via_suppression, qwerty_persistence_via_coordination_failure, and qwerty_persistence_via_merit, each with its own ε and beneficiary/victim structure. If the kernel is truly polymorphous, each reading is a factoring of a single mechanism into different causal components (the three arrows are mutually consistent, not competing).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_identity_across_readings, conceptual, 'Whether the three readings are coexistent positions on one kernel or distinct constraints conflated by a shared label.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_mechanism__beneficiary_extraction_reading, 1873, 1980).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t1873, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 1873, 0.05).
narrative_ontology:measurement_basis(qwer_tr_t1873, observed).
narrative_ontology:measurement(qwer_tr_t1910, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 1910, 0.12).
narrative_ontology:measurement_basis(qwer_tr_t1910, observed).
narrative_ontology:measurement(qwer_tr_t1932, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 1932, 0.28).
narrative_ontology:measurement_basis(qwer_tr_t1932, observed).
narrative_ontology:measurement(qwer_tr_t1950, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 1950, 0.38).
narrative_ontology:measurement_basis(qwer_tr_t1950, observed).
narrative_ontology:measurement(qwer_tr_t1965, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 1965, 0.4).
narrative_ontology:measurement_basis(qwer_tr_t1965, observed).
narrative_ontology:measurement(qwer_tr_t1980, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 1980, 0.41).
narrative_ontology:measurement_basis(qwer_tr_t1980, observed).

% Extraction over time
narrative_ontology:measurement(qwer_be_t1873, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 1873, 0.15).
narrative_ontology:measurement_basis(qwer_be_t1873, observed).
narrative_ontology:measurement(qwer_be_t1910, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 1910, 0.28).
narrative_ontology:measurement_basis(qwer_be_t1910, observed).
narrative_ontology:measurement(qwer_be_t1932, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 1932, 0.52).
narrative_ontology:measurement_basis(qwer_be_t1932, observed).
narrative_ontology:measurement(qwer_be_t1950, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 1950, 0.61).
narrative_ontology:measurement_basis(qwer_be_t1950, observed).
narrative_ontology:measurement(qwer_be_t1965, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 1965, 0.65).
narrative_ontology:measurement_basis(qwer_be_t1965, observed).
narrative_ontology:measurement(qwer_be_t1980, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 1980, 0.68).
narrative_ontology:measurement_basis(qwer_be_t1980, observed).

% Suppression requirement over time
narrative_ontology:measurement(qwer_su_t1873, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 1873, 0.18).
narrative_ontology:measurement_basis(qwer_su_t1873, observed).
narrative_ontology:measurement(qwer_su_t1910, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 1910, 0.35).
narrative_ontology:measurement_basis(qwer_su_t1910, observed).
narrative_ontology:measurement(qwer_su_t1932, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 1932, 0.58).
narrative_ontology:measurement_basis(qwer_su_t1932, observed).
narrative_ontology:measurement(qwer_su_t1950, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 1950, 0.68).
narrative_ontology:measurement_basis(qwer_su_t1950, observed).
narrative_ontology:measurement(qwer_su_t1965, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 1965, 0.71).
narrative_ontology:measurement_basis(qwer_su_t1965, observed).
narrative_ontology:measurement(qwer_su_t1980, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 1980, 0.72).
narrative_ontology:measurement_basis(qwer_su_t1980, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1873, tn=1980
narrative_ontology:measurement(qwer_grid_01, qwerty_persistence_mechanism__beneficiary_extraction_reading, accessibility_collapse(class), 1873, 0.12).
narrative_ontology:measurement(qwer_grid_02, qwerty_persistence_mechanism__beneficiary_extraction_reading, accessibility_collapse(class), 1980, 0.58).
narrative_ontology:measurement(qwer_grid_03, qwerty_persistence_mechanism__beneficiary_extraction_reading, accessibility_collapse(individual), 1873, 0.08).
narrative_ontology:measurement(qwer_grid_04, qwerty_persistence_mechanism__beneficiary_extraction_reading, accessibility_collapse(individual), 1980, 0.55).
narrative_ontology:measurement(qwer_grid_05, qwerty_persistence_mechanism__beneficiary_extraction_reading, accessibility_collapse(organizational), 1873, 0.18).
narrative_ontology:measurement(qwer_grid_06, qwerty_persistence_mechanism__beneficiary_extraction_reading, accessibility_collapse(organizational), 1980, 0.62).
narrative_ontology:measurement(qwer_grid_07, qwerty_persistence_mechanism__beneficiary_extraction_reading, accessibility_collapse(structural), 1873, 0.25).
narrative_ontology:measurement(qwer_grid_08, qwerty_persistence_mechanism__beneficiary_extraction_reading, accessibility_collapse(structural), 1980, 0.68).
narrative_ontology:measurement(qwer_grid_09, qwerty_persistence_mechanism__beneficiary_extraction_reading, resistance(class), 1873, 0.1).
narrative_ontology:measurement(qwer_grid_10, qwerty_persistence_mechanism__beneficiary_extraction_reading, resistance(class), 1980, 0.65).
narrative_ontology:measurement(qwer_grid_11, qwerty_persistence_mechanism__beneficiary_extraction_reading, resistance(individual), 1873, 0.05).
narrative_ontology:measurement(qwer_grid_12, qwerty_persistence_mechanism__beneficiary_extraction_reading, resistance(individual), 1980, 0.62).
narrative_ontology:measurement(qwer_grid_13, qwerty_persistence_mechanism__beneficiary_extraction_reading, resistance(organizational), 1873, 0.12).
narrative_ontology:measurement(qwer_grid_14, qwerty_persistence_mechanism__beneficiary_extraction_reading, resistance(organizational), 1980, 0.62).
narrative_ontology:measurement(qwer_grid_15, qwerty_persistence_mechanism__beneficiary_extraction_reading, resistance(structural), 1873, 0.08).
narrative_ontology:measurement(qwer_grid_16, qwerty_persistence_mechanism__beneficiary_extraction_reading, resistance(structural), 1980, 0.58).
narrative_ontology:measurement(qwer_grid_17, qwerty_persistence_mechanism__beneficiary_extraction_reading, stakes_inflation(class), 1873, 0.1).
narrative_ontology:measurement(qwer_grid_18, qwerty_persistence_mechanism__beneficiary_extraction_reading, stakes_inflation(class), 1980, 0.58).
narrative_ontology:measurement(qwer_grid_19, qwerty_persistence_mechanism__beneficiary_extraction_reading, stakes_inflation(individual), 1873, 0.08).
narrative_ontology:measurement(qwer_grid_20, qwerty_persistence_mechanism__beneficiary_extraction_reading, stakes_inflation(individual), 1980, 0.52).
narrative_ontology:measurement(qwer_grid_21, qwerty_persistence_mechanism__beneficiary_extraction_reading, stakes_inflation(organizational), 1873, 0.12).
narrative_ontology:measurement(qwer_grid_22, qwerty_persistence_mechanism__beneficiary_extraction_reading, stakes_inflation(organizational), 1980, 0.65).
narrative_ontology:measurement(qwer_grid_23, qwerty_persistence_mechanism__beneficiary_extraction_reading, stakes_inflation(structural), 1873, 0.15).
narrative_ontology:measurement(qwer_grid_24, qwerty_persistence_mechanism__beneficiary_extraction_reading, stakes_inflation(structural), 1980, 0.71).
narrative_ontology:measurement(qwer_grid_25, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression(class), 1873, 0.1).
narrative_ontology:measurement(qwer_grid_26, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression(class), 1980, 0.72).
narrative_ontology:measurement(qwer_grid_27, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression(individual), 1873, 0.08).
narrative_ontology:measurement(qwer_grid_28, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression(individual), 1980, 0.68).
narrative_ontology:measurement(qwer_grid_29, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression(organizational), 1873, 0.12).
narrative_ontology:measurement(qwer_grid_30, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression(organizational), 1980, 0.75).
narrative_ontology:measurement(qwer_grid_31, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression(structural), 1873, 0.15).
narrative_ontology:measurement(qwer_grid_32, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression(structural), 1980, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_mechanism__beneficiary_extraction_reading, information_standard).
narrative_ontology:boltzmann_floor_override(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0.12).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__beneficiary_extraction_reading, qwerty_persistence_mechanism__lock_in_reading).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__beneficiary_extraction_reading, qwerty_persistence_mechanism__naturalization_reading).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__beneficiary_extraction_reading, dvorak_suppression_mechanism).

% DUAL FORMULATION NOTE:
% The QWERTY kernel persists because three distinct structural mechanisms coexist in partial reinforcement: (1) beneficiary extraction — manufacturers actively suppress alternatives to protect training investments [this story]; (2) path-dependent coordination lock-in — QWERTY's first-mover advantage and network effects create a trap even without suppression [lock-in reading]; (3) naturalization — QWERTY is adequately good and alternatives failed through fair competition [naturalization reading]. Each reading instantiates a different ε value and beneficiary/victim structure. The three stories are linked by network.affects_constraints because all three causally influence QWERTY's observed persistence; no single reading fully explains the historical record alone. The decomposition follows ε-invariance (OQ-88): if changing the causal explanation (suppression vs. lock-in vs. merit) changes the empirical claim being evaluated, the constraint is not polymorphous — it is multiple constraints. This story (beneficiary extraction) and its siblings are coexistent readings precisely because the historical evidence admits all three interpretations: manufacturers' suppression decisions are documented (supporting extraction reading); network effects demonstrably exist (supporting lock-in reading); Dvorak's own claims and alternatives' actual adoption barriers are empirically measurable (supporting naturalization reading if alternatives were genuinely inferior).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(qwerty_persistence_mechanism__beneficiary_extraction_reading, organized, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
