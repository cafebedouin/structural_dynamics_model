% ============================================================================
% CONSTRAINT STORY: qwerty_persistence__incumbent_preservation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    narrative_ontology:affects_constraint/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: qwerty_persistence__incumbent_preservation_reading
 *   human_readable: QWERTY Keyboard Layout Persistence (Incumbent Preservation Reading)
 *   domain: technology_history/industrial_standards/path_dependence
 *
 * SUMMARY:
 *   This constraint describes the persistence of the QWERTY keyboard layout,
 *   specifically from the perspective that its dominance is actively
 *   preserved by incumbent beneficiaries to protect their capital investments
 *   and established market positions. It is one reading of the broader
 *   'QWERTY persistence' kernel. The constraint functions as a Tangled Rope,
 *   providing a coordination function (universal layout) while simultaneously
 *   extracting costs from those who would prefer or benefit from
 *   alternatives, through active suppression and defense by incumbents.
 *
 * KEY AGENTS:
 *   - qwerty_keyboard_manufacturers: Primary beneficiary (institutional/arbitrage) — actively defends the standard.
 *   - trained_typists: Beneficiary (moderate/identity_locked) — benefits from existing skill, resists change.
 *   - typing_training_institutions: Beneficiary (organized/constrained) — benefits from stable curriculum.
 *   - alternative_keyboard_manufacturers: Victim (powerful/constrained) — suppressed by QWERTY's dominance.
 *   - efficiency_seeking_users: Victim (moderate/constrained) — bears costs of suboptimal layout.
 *   - ergonomics_researchers: Victim (analytical/analytical) — provides evidence of QWERTY's suboptimality, but their findings are suppressed.
 *   - competition_authorities: Observer (institutional/analytical) — investigates market dominance and anti-competitive practices.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence__incumbent_preservation_reading, 0.65).
domain_priors:suppression_score(qwerty_persistence__incumbent_preservation_reading, 0.7).
domain_priors:theater_ratio(qwerty_persistence__incumbent_preservation_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence__incumbent_preservation_reading, tangled_rope).
narrative_ontology:human_readable(qwerty_persistence__incumbent_preservation_reading, "QWERTY Keyboard Layout Persistence (Incumbent Preservation Reading)").
narrative_ontology:topic_domain(qwerty_persistence__incumbent_preservation_reading, "technology_history/industrial_standards/path_dependence").

domain_priors:requires_active_enforcement(qwerty_persistence__incumbent_preservation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence__incumbent_preservation_reading, '2ccf30c9-71cf-474f-9706-3c4d9e75f313').
narrative_ontology:cs_kernel_codification('2ccf30c9-71cf-474f-9706-3c4d9e75f313', implicit).
narrative_ontology:cs_authority_grounding('2ccf30c9-71cf-474f-9706-3c4d9e75f313', extraction).
narrative_ontology:cs_interpretation_layer_present('2ccf30c9-71cf-474f-9706-3c4d9e75f313').
narrative_ontology:cs_reading_relation('2ccf30c9-71cf-474f-9706-3c4d9e75f313', qwerty_persistence__lapsed_alternatives_reading, coexists_with).
narrative_ontology:cs_axiom('2ccf30c9-71cf-474f-9706-3c4d9e75f313', foundational, incumbent_investment_protection_is_paramount).
narrative_ontology:cs_axiom_status(incumbent_investment_protection_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('2ccf30c9-71cf-474f-9706-3c4d9e75f313', incumbent_investment_protection_is_paramount, conventional).
narrative_ontology:cs_axiom('2ccf30c9-71cf-474f-9706-3c4d9e75f313', secondary, market_dominance_justifies_status_quo).
narrative_ontology:cs_axiom_status(market_dominance_justifies_status_quo, holdable).
narrative_ontology:cs_axiom_grounding('2ccf30c9-71cf-474f-9706-3c4d9e75f313', market_dominance_justifies_status_quo, conventional).
narrative_ontology:cs_reference_frame('2ccf30c9-71cf-474f-9706-3c4d9e75f313', qwerty_as_unquestioned_standard).
narrative_ontology:cs_drift_state('2ccf30c9-71cf-474f-9706-3c4d9e75f313', contemporary_digital_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2ccf30c9-71cf-474f-9706-3c4d9e75f313', '').
narrative_ontology:cs_kernel_id(qwerty_persistence__incumbent_preservation_reading, qwerty_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence__incumbent_preservation_reading, qwerty_keyboard_manufacturers).
narrative_ontology:constraint_beneficiary(qwerty_persistence__incumbent_preservation_reading, trained_typists).
narrative_ontology:constraint_beneficiary(qwerty_persistence__incumbent_preservation_reading, typing_training_institutions).
narrative_ontology:constraint_victim(qwerty_persistence__incumbent_preservation_reading, alternative_keyboard_manufacturers).
narrative_ontology:constraint_victim(qwerty_persistence__incumbent_preservation_reading, efficiency_seeking_users).
narrative_ontology:constraint_victim(qwerty_persistence__incumbent_preservation_reading, ergonomics_researchers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Companies that produce QWERTY keyboards and related hardware. They have significant capital investments in QWERTY-specific tooling and supply chains. They actively defend the standard through lobbying, marketing, and sometimes legal action against alternatives, benefiting from the stability of the market.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, qwerty_keyboard_manufacturers, agenda_setter,
    institutional, generational, arbitrage, global).

% Individuals who have invested time and effort in learning the QWERTY layout. Their existing skill set makes them resistant to adopting new layouts, as the switching cost (re-learning) is high. They benefit from the ubiquity of QWERTY, which ensures their skills are always applicable.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, trained_typists, beneficiary,
    moderate, biographical, identity_locked, global).

% Schools and programs that teach typing. Their curriculum is built around QWERTY, and a shift to an alternative layout would require significant investment in new materials, teacher training, and a justification for the change to students and parents. They benefit from the stability of the QWERTY standard.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, typing_training_institutions, beneficiary,
    organized, generational, constrained, national).

% Companies that design and produce alternative keyboard layouts (e.g., Dvorak, Colemak) claiming ergonomic or efficiency benefits. They face immense barriers to market entry and adoption due to QWERTY's entrenched position and active defense by incumbents. They bear the cost of suppressed innovation and limited market access.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, alternative_keyboard_manufacturers, payer,
    powerful, biographical, constrained, global).

% Users who are aware of and desire more efficient or ergonomic keyboard layouts but are constrained by the ubiquity of QWERTY hardware, software support, and the social cost of using a non-standard layout. They bear the cost of suboptimal typing speed, increased strain, or the high switching cost to alternatives.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, efficiency_seeking_users, payer,
    powerless, immediate, constrained, global).

% Academics and scientists who study human-computer interaction and ergonomics, often publishing research demonstrating the inefficiencies and health drawbacks of the QWERTY layout. Their findings are often downplayed or ignored by incumbents, and their proposed solutions struggle to gain adoption despite scientific merit.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, ergonomics_researchers, excluded,
    analytical, generational, analytical, global).

% Government bodies tasked with ensuring fair competition. They may investigate QWERTY's dominance if evidence of anti-competitive practices by incumbents emerges, but the diffuse nature of the 'standard' makes direct intervention challenging.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, competition_authorities, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qwerty_persistence__incumbent_preservation_reading, qwerty_keyboard_manufacturers).
narrative_ontology:fixing_cost_class(qwerty_persistence__incumbent_preservation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a universal, predictable keyboard layout that allows anyone to use any keyboard without retraining, facilitating communication and data entry across diverse hardware and software platforms.
% TRANSFER_FUNCTION: Transfers market dominance and sustained revenue to QWERTY keyboard manufacturers and related industries, while imposing costs (suboptimal efficiency, ergonomic strain, suppressed innovation) on users and alternative manufacturers.
% ABSENT_VOICES: Alternative keyboard designers and manufacturers, as well as ergonomics researchers, are largely excluded from the 'standard-setting' conversation, which is dominated by incumbents. They would advocate for open standards and evidence-based design.
% DISAPPEARANCE_RATIONALE: If QWERTY vanished overnight, the immediate chaos would be immense, but over time, the market would likely converge on more efficient or ergonomic layouts, driven by user demand and technological innovation. The entire keyboard manufacturing, software interface, and typing education industries would undergo a massive, disruptive reorganization.
% FOUNDING_PROBLEM: The original problem was to create a mechanical typewriter layout that prevented key jams and allowed for rapid typing, given the technological constraints of the late 19th century.
% FOUNDING_PROBLEM_CORROBORATION: The original mechanical problem is long dead due to advances in technology. Ergonomics researchers and alternative manufacturers widely corroborate that QWERTY is suboptimal for modern typing. QWERTY manufacturers, however, contest this, citing user familiarity and the cost of switching as ongoing 'problems' that QWERTY 'solves' by maintaining stability.
narrative_ontology:disappearance_verdict(qwerty_persistence__incumbent_preservation_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence__incumbent_preservation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence__incumbent_preservation_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(qwerty_persistence__incumbent_preservation_reading, 'none', 1).

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
 *   The extractiveness (0.65) reflects the ongoing costs borne by users and alternative manufacturers due to QWERTY's suboptimal design and the active suppression of alternatives. Suppression (0.7) is high because incumbents actively lobby, market, and sometimes litigate to maintain QWERTY's position, making it difficult for alternatives to gain traction. The theater ratio (0.2) is relatively low, as the coordination function (a universal standard) is real, but a significant portion of the 'maintenance' is defensive rather than purely functional. The increasing extractiveness and suppression over time reflect the hardening of QWERTY's dominance as capital investments in its production and training deepened.
 *
 * PERSPECTIVAL GAP:
 *   QWERTY manufacturers and trained typists perceive the layout as a beneficial, stable standard (closer to a Rope or even a Mountain of coordination). In contrast, alternative manufacturers and efficiency-seeking users experience it as an extractive and suppressive force, actively maintained against their interests (a Snare or Tangled Rope). The engine's per-seat classification will reflect this divergence based on the declared roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   QWERTY keyboard manufacturers, trained typists, and typing training institutions are beneficiaries (low d) as they profit from or are locked into the existing standard. Alternative keyboard manufacturers, efficiency-seeking users, and ergonomics researchers are victims (high d) as they bear the costs of QWERTY's persistence and the suppression of superior alternatives. Competition authorities are observers (d=0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling QWERTY's persistence as purely 'natural' network effects (a Rope or Mountain) by highlighting the active role of incumbent defense. It distinguishes between genuine coordination benefits and the extractive costs imposed by the active preservation of a suboptimal standard, which is a key aspect of mandatrophy where the original 'mandate' (standardization) is co-opted for rent-seeking.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is QWERTY''s persistence primarily due to incumbent defense (this reading) or the natural lapse of alternatives (lapsed_alternatives_reading)?',
    'Historical analysis of lobbying efforts, patent defense, and marketing spend by QWERTY incumbents versus independent assessment of alternative layout adoption barriers (e.g., network effects without active suppression).',
    'If incumbent defense is primary, the constraint is a Tangled Rope with higher extraction and suppression. If alternative lapse is primary, it''s closer to a Rope or even a Mountain of coordination, with lower extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, empirical, 'Ambiguity between incumbent preservation and natural lapse as drivers of QWERTY persistence.').

omega_variable(
    defensive_suppression_cost,
    'What proportion of ''coordination costs'' are actually defensive suppression costs incurred by incumbents to maintain QWERTY''s dominance?',
    'Detailed accounting of R&D, marketing, and legal expenditures by QWERTY manufacturers specifically aimed at discrediting or blocking alternative layouts, compared to genuine standardization costs.',
    'A higher proportion of defensive suppression costs would increase the effective extractiveness and suppression metrics, reinforcing the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(defensive_suppression_cost, empirical, 'Distinguishing genuine coordination costs from defensive suppression costs.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence__incumbent_preservation_reading, 1873, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t1873, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 1873, 0.05).
narrative_ontology:measurement(qwer_tr_t1930, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 1930, 0.1).
narrative_ontology:measurement(qwer_tr_t1970, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(qwer_tr_t2020, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 2020, 0.2).

% Extraction over time
narrative_ontology:measurement(qwer_be_t1873, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 1873, 0.1).
narrative_ontology:measurement(qwer_be_t1930, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 1930, 0.4).
narrative_ontology:measurement(qwer_be_t1970, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 1970, 0.6).
narrative_ontology:measurement(qwer_be_t2020, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 2020, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(qwer_su_t1873, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 1873, 0.1).
narrative_ontology:measurement(qwer_su_t1930, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 1930, 0.3).
narrative_ontology:measurement(qwer_su_t1970, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 1970, 0.6).
narrative_ontology:measurement(qwer_su_t2020, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 2020, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence__incumbent_preservation_reading, information_standard).
narrative_ontology:affects_constraint(qwerty_persistence__incumbent_preservation_reading, qwerty_persistence__lapsed_alternatives_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'QWERTY persistence' kernel, focusing on incumbent defense. The 'lapsed_alternatives_reading' focuses on network effects and the natural failure of alternatives to reach critical mass.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
