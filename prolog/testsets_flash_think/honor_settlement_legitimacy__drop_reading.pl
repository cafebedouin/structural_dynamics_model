% ============================================================================
% CONSTRAINT STORY: honor_settlement_legitimacy__drop_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_settlement_legitimacy__drop_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: honor_settlement_legitimacy__drop_reading
 *   human_readable: Legal and Social Prohibition of Dueling (Drop Reading)
 *   domain: historical_sociology/legal_history/cultural_anthropology
 *
 * SUMMARY:
 *   This constraint story, the 'drop_reading' of the
 *   honor_settlement_legitimacy kernel, describes the legal and social
 *   prohibition of dueling from the perspective that dueling, while largely
 *   suppressed, persisted as a fringe practice among residual honor culture
 *   adherents. It emphasizes that honor culture remained a live option in
 *   specific geographic and social niches, and thus the constraint against
 *   dueling was not universally effective in eliminating it from the
 *   normative repertoire, but rather pushed it underground.
 *
 * KEY AGENTS:
 *   - state_legal_system: Agenda setter (institutional/mobile) — enforces the prohibition
 *   - broader_society: Beneficiary (organized/mobile) — benefits from reduced violence
 *   - honor_culture_adherents: Payer/Excluded (powerless/identity_locked) — bears costs, maintains alternative norms
 *   - duelists: Payer (powerless/trapped) — bears direct legal and social costs
 *   - cultural_historians: Observer (analytical/analytical) — analyzes the phenomenon
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_settlement_legitimacy__drop_reading, 0.78).
domain_priors:suppression_score(honor_settlement_legitimacy__drop_reading, 0.85).
domain_priors:theater_ratio(honor_settlement_legitimacy__drop_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_settlement_legitimacy__drop_reading, snare).
narrative_ontology:human_readable(honor_settlement_legitimacy__drop_reading, "Legal and Social Prohibition of Dueling (Drop Reading)").
narrative_ontology:topic_domain(honor_settlement_legitimacy__drop_reading, "historical_sociology/legal_history/cultural_anthropology").

domain_priors:requires_active_enforcement(honor_settlement_legitimacy__drop_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_settlement_legitimacy__drop_reading, '8c2e87a6-a113-40d3-ba41-ed68bfbb583d').
narrative_ontology:cs_kernel_codification('8c2e87a6-a113-40d3-ba41-ed68bfbb583d', formalized).
narrative_ontology:cs_authority_grounding('8c2e87a6-a113-40d3-ba41-ed68bfbb583d', lineage).
narrative_ontology:cs_interpretation_layer_present('8c2e87a6-a113-40d3-ba41-ed68bfbb583d').
narrative_ontology:cs_reading_relation('8c2e87a6-a113-40d3-ba41-ed68bfbb583d', honor_settlement_legitimacy__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('8c2e87a6-a113-40d3-ba41-ed68bfbb583d', honor_settlement_legitimacy__composite_reading, coexists_with).
narrative_ontology:cs_axiom('8c2e87a6-a113-40d3-ba41-ed68bfbb583d', foundational, honor_is_socially_constructed_and_resilient).
narrative_ontology:cs_axiom_status(honor_is_socially_constructed_and_resilient, holdable).
narrative_ontology:cs_axiom_grounding('8c2e87a6-a113-40d3-ba41-ed68bfbb583d', honor_is_socially_constructed_and_resilient, conventional).
narrative_ontology:cs_axiom('8c2e87a6-a113-40d3-ba41-ed68bfbb583d', foundational, state_monopoly_on_violence_is_contingent_and_contested).
narrative_ontology:cs_axiom_status(state_monopoly_on_violence_is_contingent_and_contested, holdable).
narrative_ontology:cs_axiom_grounding('8c2e87a6-a113-40d3-ba41-ed68bfbb583d', state_monopoly_on_violence_is_contingent_and_contested, empirically_contingent).
narrative_ontology:cs_reference_frame('8c2e87a6-a113-40d3-ba41-ed68bfbb583d', state_monopoly_on_violence_norm).
narrative_ontology:cs_drift_state('8c2e87a6-a113-40d3-ba41-ed68bfbb583d', post_enlightenment_legal_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('8c2e87a6-a113-40d3-ba41-ed68bfbb583d', '').
narrative_ontology:cs_kernel_id(honor_settlement_legitimacy__drop_reading, honor_settlement_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__drop_reading, state_legal_system).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__drop_reading, broader_society).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__drop_reading, honor_culture_adherents).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__drop_reading, duelists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enforces laws prohibiting dueling, maintaining its monopoly on violence. Benefits from the erosion of alternative dispute resolution mechanisms.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, state_legal_system, agenda_setter,
    institutional, generational, mobile, national).

% Benefits from reduced private violence and a more stable social order, accepting state authority in dispute resolution. Experiences the constraint as a coordination mechanism.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, broader_society, beneficiary,
    organized, biographical, mobile, national).

% Individuals and groups who continue to adhere to traditional honor codes that legitimize dueling. They face legal penalties and social ostracization for upholding their values, making exit from the honor code difficult due to identity fusion.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, honor_culture_adherents, payer,
    powerless, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(honor_settlement_legitimacy__drop_reading, honor_culture_adherents, excluded).

% Individuals who engage in dueling, often from honor-bound communities. They face severe legal consequences and social stigma, with no legitimate exit from the legal prohibition if caught.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, duelists, payer,
    powerless, immediate, trapped, local).

% Study the historical evolution of dueling, honor codes, and state power. They analyze the persistence of dueling as a fringe practice and its implications for social theory.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, cultural_historians, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish and maintain the state's monopoly on violence, preventing private individuals from resorting to lethal combat for dispute resolution and thereby ensuring public order.
% TRANSFER_FUNCTION: Transfers the legitimate right to use violence from individuals to the state. It extracts the freedom of individuals to settle honor disputes through dueling, imposing legal and social costs on those who attempt to do so.
% ABSENT_VOICES: Those who believe in the inherent right to personal combat for honor defense, or those who view dueling as a legitimate, albeit dangerous, form of social arbitration. Their voices are suppressed by legal prohibition and social stigmatization.
% DISAPPEARANCE_RATIONALE: If the legal and social prohibition on dueling vanished overnight, it is plausible that dueling would re-emerge, particularly within residual honor-bound communities or subcultures, leading to a reorganization of dispute resolution practices and potentially challenging state authority over violence.
% FOUNDING_PROBLEM: Uncontrolled private violence, challenges to state authority over justice, and social instability arising from honor disputes that escalated into lethal combat.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, historians of state formation, and sociologists of violence corroborate that the problem of maintaining state monopoly on violence is ongoing, even if dueling is no longer the primary threat. The persistence of fringe dueling practices, as described in this reading, serves as a minor but real challenge to this monopoly.
narrative_ontology:disappearance_verdict(honor_settlement_legitimacy__drop_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_settlement_legitimacy__drop_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_settlement_legitimacy__drop_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(honor_settlement_legitimacy__drop_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_settlement_legitimacy__drop_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_settlement_legitimacy__drop_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(honor_settlement_legitimacy__drop_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(honor_settlement_legitimacy__drop_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Snare because it actively extracts from a specific group (honor_culture_adherents, duelists) by suppressing their preferred method of honor settlement, while providing coordination benefits to the broader society. Extractiveness is high (0.78) for the victims, reflecting the severe legal and social penalties. Suppression is very high (0.85) due to active legal enforcement and social stigmatization. Theater ratio is moderate (0.4) as enforcement was real for those caught, but the overall public performance of suppression might have exceeded the actual prevalence of dueling as it became fringe. Accessibility collapse is moderate (0.65) because while dueling was largely eliminated from mainstream society, it remained an option for those deeply embedded in honor cultures. Resistance is moderate (0.45), reflecting the continued, albeit clandestine, practice of dueling by adherents.
 *
 * PERSPECTIVAL GAP:
 *   The state legal system and broader society experience this constraint as a legitimate and beneficial coordination mechanism that reduces violence. In contrast, honor culture adherents and duelists experience it as a highly extractive and suppressive force that denies them a culturally sanctioned means of dispute resolution and identity maintenance. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The state legal system is a clear beneficiary, consolidating its power and maintaining order. Broader society also benefits from reduced violence. Honor culture adherents and duelists are the primary targets, facing legal and social penalties for their adherence to alternative norms. Their 'identity_locked' and 'trapped' exit options amplify the effective extraction they experience.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Snare prevents mislabeling the prohibition as a pure Rope, which would ignore the significant extraction from honor culture adherents. While the founding problem (uncontrolled private violence) remains live, the persistence of dueling as a fringe practice suggests that the constraint's function shifted from broad societal control to targeted suppression of a subculture, maintaining its extractive nature for that group.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately representing the ''drop_reading'' of the ''honor_settlement_legitimacy'' kernel?',
    'Comparison with other readings of the same kernel, ensuring that the structural delta (persistence of honor culture/dueling as fringe) is consistently captured and distinct from other readings'' claims.',
    'If not, the analysis of the kernel''s overall evolution would be incomplete or distorted, potentially misrepresenting the pathways of cultural change.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Verifies the fidelity of this constraint to its declared kernel reading.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of dueling structural (legal penalties, social ostracization) or internalized (cognitive shift making dueling unthinkable for most)?',
    'Post-exit suppression trajectory: if dueling re-emerges in contexts where structural barriers are removed, it suggests suppression was primarily structural. If it does not, internalization is stronger. For honor culture adherents, it is primarily structural.',
    'If internalized for the broader society, the constraint''s effective suppression is higher than the structural measure suggests, as the prohibition is self-enforcing. For adherents, it remains structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for dueling.').

omega_variable(
    honor_culture_resilience_measurement,
    'How can the ''live option'' status of honor culture in specific niches be quantitatively measured?',
    'Sociological studies of subcultures, ethnographic research, analysis of legal records for dueling incidents, and cultural artifact analysis (e.g., literature, personal correspondence) within identified niches.',
    'A higher measured resilience of honor culture would strengthen the ''drop_reading'' and suggest that the constraint against dueling faced more persistent, albeit localized, resistance than other readings might imply.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(honor_culture_resilience_measurement, empirical, 'Quantifying the persistence of honor culture as a ''live option''.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_settlement_legitimacy__drop_reading, 1850, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1850, honor_settlement_legitimacy__drop_reading, theater_ratio, 1850, 0.3).
narrative_ontology:measurement(hono_tr_t1875, honor_settlement_legitimacy__drop_reading, theater_ratio, 1875, 0.35).
narrative_ontology:measurement(hono_tr_t1900, honor_settlement_legitimacy__drop_reading, theater_ratio, 1900, 0.4).
narrative_ontology:measurement(hono_tr_t1925, honor_settlement_legitimacy__drop_reading, theater_ratio, 1925, 0.45).
narrative_ontology:measurement(hono_tr_t1950, honor_settlement_legitimacy__drop_reading, theater_ratio, 1950, 0.4).

% Extraction over time
narrative_ontology:measurement(hono_be_t1850, honor_settlement_legitimacy__drop_reading, base_extractiveness, 1850, 0.8).
narrative_ontology:measurement(hono_be_t1875, honor_settlement_legitimacy__drop_reading, base_extractiveness, 1875, 0.79).
narrative_ontology:measurement(hono_be_t1900, honor_settlement_legitimacy__drop_reading, base_extractiveness, 1900, 0.78).
narrative_ontology:measurement(hono_be_t1925, honor_settlement_legitimacy__drop_reading, base_extractiveness, 1925, 0.77).
narrative_ontology:measurement(hono_be_t1950, honor_settlement_legitimacy__drop_reading, base_extractiveness, 1950, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1850, honor_settlement_legitimacy__drop_reading, suppression_requirement, 1850, 0.9).
narrative_ontology:measurement(hono_su_t1875, honor_settlement_legitimacy__drop_reading, suppression_requirement, 1875, 0.88).
narrative_ontology:measurement(hono_su_t1900, honor_settlement_legitimacy__drop_reading, suppression_requirement, 1900, 0.86).
narrative_ontology:measurement(hono_su_t1925, honor_settlement_legitimacy__drop_reading, suppression_requirement, 1925, 0.84).
narrative_ontology:measurement(hono_su_t1950, honor_settlement_legitimacy__drop_reading, suppression_requirement, 1950, 0.85).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1850, tn=1950
narrative_ontology:measurement(hono_grid_01, honor_settlement_legitimacy__drop_reading, accessibility_collapse(class), 1850, 0.65).
narrative_ontology:measurement(hono_grid_02, honor_settlement_legitimacy__drop_reading, accessibility_collapse(class), 1950, 0.55).
narrative_ontology:measurement(hono_grid_03, honor_settlement_legitimacy__drop_reading, accessibility_collapse(individual), 1850, 0.8).
narrative_ontology:measurement(hono_grid_04, honor_settlement_legitimacy__drop_reading, accessibility_collapse(individual), 1950, 0.75).
narrative_ontology:measurement(hono_grid_05, honor_settlement_legitimacy__drop_reading, accessibility_collapse(organizational), 1850, 0.7).
narrative_ontology:measurement(hono_grid_06, honor_settlement_legitimacy__drop_reading, accessibility_collapse(organizational), 1950, 0.6).
narrative_ontology:measurement(hono_grid_07, honor_settlement_legitimacy__drop_reading, accessibility_collapse(structural), 1850, 0.75).
narrative_ontology:measurement(hono_grid_08, honor_settlement_legitimacy__drop_reading, accessibility_collapse(structural), 1950, 0.65).
narrative_ontology:measurement(hono_grid_09, honor_settlement_legitimacy__drop_reading, resistance(class), 1850, 0.35).
narrative_ontology:measurement(hono_grid_10, honor_settlement_legitimacy__drop_reading, resistance(class), 1950, 0.25).
narrative_ontology:measurement(hono_grid_11, honor_settlement_legitimacy__drop_reading, resistance(individual), 1850, 0.5).
narrative_ontology:measurement(hono_grid_12, honor_settlement_legitimacy__drop_reading, resistance(individual), 1950, 0.45).
narrative_ontology:measurement(hono_grid_13, honor_settlement_legitimacy__drop_reading, resistance(organizational), 1850, 0.3).
narrative_ontology:measurement(hono_grid_14, honor_settlement_legitimacy__drop_reading, resistance(organizational), 1950, 0.2).
narrative_ontology:measurement(hono_grid_15, honor_settlement_legitimacy__drop_reading, resistance(structural), 1850, 0.4).
narrative_ontology:measurement(hono_grid_16, honor_settlement_legitimacy__drop_reading, resistance(structural), 1950, 0.3).
narrative_ontology:measurement(hono_grid_17, honor_settlement_legitimacy__drop_reading, stakes_inflation(class), 1850, 0.75).
narrative_ontology:measurement(hono_grid_18, honor_settlement_legitimacy__drop_reading, stakes_inflation(class), 1950, 0.65).
narrative_ontology:measurement(hono_grid_19, honor_settlement_legitimacy__drop_reading, stakes_inflation(individual), 1850, 0.9).
narrative_ontology:measurement(hono_grid_20, honor_settlement_legitimacy__drop_reading, stakes_inflation(individual), 1950, 0.85).
narrative_ontology:measurement(hono_grid_21, honor_settlement_legitimacy__drop_reading, stakes_inflation(organizational), 1850, 0.8).
narrative_ontology:measurement(hono_grid_22, honor_settlement_legitimacy__drop_reading, stakes_inflation(organizational), 1950, 0.7).
narrative_ontology:measurement(hono_grid_23, honor_settlement_legitimacy__drop_reading, stakes_inflation(structural), 1850, 0.85).
narrative_ontology:measurement(hono_grid_24, honor_settlement_legitimacy__drop_reading, stakes_inflation(structural), 1950, 0.75).
narrative_ontology:measurement(hono_grid_25, honor_settlement_legitimacy__drop_reading, suppression(class), 1850, 0.8).
narrative_ontology:measurement(hono_grid_26, honor_settlement_legitimacy__drop_reading, suppression(class), 1950, 0.75).
narrative_ontology:measurement(hono_grid_27, honor_settlement_legitimacy__drop_reading, suppression(individual), 1850, 0.95).
narrative_ontology:measurement(hono_grid_28, honor_settlement_legitimacy__drop_reading, suppression(individual), 1950, 0.9).
narrative_ontology:measurement(hono_grid_29, honor_settlement_legitimacy__drop_reading, suppression(organizational), 1850, 0.85).
narrative_ontology:measurement(hono_grid_30, honor_settlement_legitimacy__drop_reading, suppression(organizational), 1950, 0.8).
narrative_ontology:measurement(hono_grid_31, honor_settlement_legitimacy__drop_reading, suppression(structural), 1850, 0.9).
narrative_ontology:measurement(hono_grid_32, honor_settlement_legitimacy__drop_reading, suppression(structural), 1950, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_settlement_legitimacy__drop_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__drop_reading, honor_settlement_legitimacy__contraction_reading).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__drop_reading, honor_settlement_legitimacy__composite_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'honor_settlement_legitimacy' kernel. This 'drop_reading' emphasizes the persistence of dueling as a fringe practice, contrasting with the 'contraction_reading' (dueling became unthinkable) and the 'composite_reading' (decline due to multiple reinforcing mechanisms).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
