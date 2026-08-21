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
 *   human_readable: Prohibition of Dueling (Fringe Persistence Reading)
 *   domain: historical_sociology/legal_history/cultural_anthropology
 *
 * SUMMARY:
 *   This constraint story instantiates the 'drop_reading' of the
 *   'honor_settlement_legitimacy' kernel. From this perspective, the legal
 *   and social prohibition against dueling, while largely successful in the
 *   broader society, did not completely eradicate the practice. Instead,
 *   dueling persisted as a fringe activity among residual adherents of honor
 *   culture, particularly in specific geographic or social niches. The
 *   constraint is thus a Tangled Rope: it coordinates social order for the
 *   majority but continues to extract significantly from a minority whose
 *   identity is tied to older honor codes.
 *
 * KEY AGENTS:
 *   - state_legal_system: Primary agenda_setter (institutional/arbitrage) — enforces the prohibition.
 *   - broader_society: Primary beneficiary (organized/mobile) — benefits from reduced violence.
 *   - honor_culture_adherents: Primary payer (powerless/identity_locked) — bears the costs of suppression.
 *   - cultural_historians: Analytical observer (analytical/analytical) — analyzes the phenomenon.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_settlement_legitimacy__drop_reading, 0.65).
domain_priors:suppression_score(honor_settlement_legitimacy__drop_reading, 0.75).
domain_priors:theater_ratio(honor_settlement_legitimacy__drop_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_settlement_legitimacy__drop_reading, tangled_rope).
narrative_ontology:human_readable(honor_settlement_legitimacy__drop_reading, "Prohibition of Dueling (Fringe Persistence Reading)").
narrative_ontology:topic_domain(honor_settlement_legitimacy__drop_reading, "historical_sociology/legal_history/cultural_anthropology").

domain_priors:requires_active_enforcement(honor_settlement_legitimacy__drop_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_settlement_legitimacy__drop_reading, '42165df3-bd79-4be1-944f-67734d91d4b5').
narrative_ontology:cs_kernel_codification('42165df3-bd79-4be1-944f-67734d91d4b5', formalized).
narrative_ontology:cs_authority_grounding('42165df3-bd79-4be1-944f-67734d91d4b5', lineage).
narrative_ontology:cs_interpretation_layer_present('42165df3-bd79-4be1-944f-67734d91d4b5').
narrative_ontology:cs_reading_relation('42165df3-bd79-4be1-944f-67734d91d4b5', honor_settlement_legitimacy__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('42165df3-bd79-4be1-944f-67734d91d4b5', honor_settlement_legitimacy__composite_reading, coexists_with).
narrative_ontology:cs_axiom('42165df3-bd79-4be1-944f-67734d91d4b5', foundational, private_violence_illegitimate).
narrative_ontology:cs_axiom_status(private_violence_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('42165df3-bd79-4be1-944f-67734d91d4b5', private_violence_illegitimate, deontological).
narrative_ontology:cs_axiom('42165df3-bd79-4be1-944f-67734d91d4b5', foundational, honor_is_social_not_physical).
narrative_ontology:cs_axiom_status(honor_is_social_not_physical, holdable).
narrative_ontology:cs_axiom_grounding('42165df3-bd79-4be1-944f-67734d91d4b5', honor_is_social_not_physical, conventional).
narrative_ontology:cs_reference_frame('42165df3-bd79-4be1-944f-67734d91d4b5', state_monopoly_on_violence_ideal).
narrative_ontology:cs_drift_state('42165df3-bd79-4be1-944f-67734d91d4b5', late_19th_early_20th_century, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('42165df3-bd79-4be1-944f-67734d91d4b5', '').
narrative_ontology:cs_kernel_id(honor_settlement_legitimacy__drop_reading, honor_settlement_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__drop_reading, state_legal_system).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__drop_reading, broader_society).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__drop_reading, honor_culture_adherents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enforces laws prohibiting dueling, aiming to establish and maintain a monopoly on legitimate violence. It benefits from increased social order and reduced challenges to its authority.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, state_legal_system, agenda_setter,
    institutional, generational, arbitrage, national).

% Benefits from the reduction of private violence and the establishment of more predictable legal means of dispute resolution. Generally accepts the new social norm against dueling.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, broader_society, beneficiary,
    organized, biographical, mobile, national).

% Adhere to traditional honor codes that may still sanction dueling as a means of settling grievances. They face legal penalties, social ostracization, and the erosion of their cultural practices, making the constraint highly extractive for them. Their identity is tied to these practices.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, honor_culture_adherents, payer,
    powerless, biographical, identity_locked, local).

% Analyze the historical evolution of honor cultures and the legal/social mechanisms that led to the decline of dueling, noting its persistence as a fringe practice in certain niches.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, cultural_historians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish the state's monopoly on violence, replacing private dueling with legal and institutional mechanisms for dispute resolution, thereby promoting public order and safety.
% TRANSFER_FUNCTION: Transfers the right to settle grievances by violence from individuals to the state, and transfers social legitimacy from honor-based dueling to formal legal processes. It extracts social capital and freedom of action from those who adhere to traditional honor codes.
% ABSENT_VOICES: Individuals and groups from earlier periods who viewed dueling as an essential, legitimate component of honor and social standing, whose perspectives are now marginalized or actively suppressed by the dominant legal and social norms.
% DISAPPEARANCE_RATIONALE: If the prohibition on dueling and its associated social stigma vanished, the state's monopoly on violence would be challenged. While widespread dueling might not immediately return, the underlying honor culture could re-emerge in specific social contexts, leading to a reorganization of dispute resolution mechanisms in those niches.
% FOUNDING_PROBLEM: The widespread social disruption, violence, and challenges to state authority caused by a culture where private dueling was a legitimate and common means of dispute resolution, leading to instability and loss of life.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and sociologists corroborate the historical problem of dueling and its impact on social order, noting the state's consistent efforts to suppress private violence. While dueling itself is rare, the broader problem of maintaining state monopoly on violence remains live.
narrative_ontology:disappearance_verdict(honor_settlement_legitimacy__drop_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_settlement_legitimacy__drop_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_settlement_legitimacy__drop_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(honor_settlement_legitimacy__drop_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_settlement_legitimacy__drop_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is moderate-high (0.65) because while dueling was rare, the consequences for those who engaged in it were severe, representing a high cost to honor culture adherents. Suppression is high (0.75) due to active legal enforcement and social stigmatization, which was effective enough to marginalize the practice but not eliminate it. Theater ratio is moderate (0.40) as some enforcement activity served to reinforce the dominant norm, even if actual duels were infrequent. Accessibility collapse is moderate (0.60) because while dueling as a legitimate option collapsed for most, it remained a culturally available, albeit risky, option for a fringe group. Resistance is moderate (0.55) reflecting the continued, albeit limited, adherence to dueling practices.
 *
 * PERSPECTIVAL GAP:
 *   The state and broader society experience this constraint as a successful coordination mechanism that brought order and reduced violence. For honor culture adherents, however, the same constraint is experienced as an oppressive force that criminalizes their deeply held cultural practices and extracts severe penalties for non-compliance. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The state legal system and broader society are beneficiaries, as they gain from the reduction of private violence and the consolidation of state authority. Honor culture adherents are targets, as they face legal and social penalties for maintaining their traditional practices. Their 'identity_locked' exit option reflects the deep cultural commitment that makes abandoning dueling a profound personal and social cost.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate to suppress private violence remains live, as the problem of maintaining state monopoly on violence is ongoing. However, the specific manifestation of 'dueling' as a widespread threat has largely atrophied for the majority. For the fringe adherents, the constraint is still actively extractive, preventing it from fully degrading into a Piton. The 'contested' status of the founding problem reflects this dual reality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately identified as the ''drop_reading'' of the ''honor_settlement_legitimacy'' kernel?',
    'Comparative analysis with ''contraction_reading'' and ''composite_reading'' to assess which best describes the observed historical persistence of dueling as a fringe practice versus its complete cognitive disappearance or overdetermined decline.',
    'If an alternative reading is more accurate, the structural properties (extractiveness, suppression, stakeholder roles) and classification of this constraint would need to be re-evaluated to align with that reading''s core premise.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Confirms this constraint as a specific reading of the honor settlement kernel.').

omega_variable(
    fringe_practice_quantification,
    'What was the actual prevalence and social impact of dueling as a ''fringe practice'' during the specified interval?',
    'Detailed historical sociological research, including analysis of court records, personal correspondence, and journalistic accounts to quantify instances of dueling and their social consequences.',
    'Higher prevalence would increase the measured extractiveness and suppression, potentially shifting the classification towards a Snare. Lower prevalence would reduce these metrics, potentially moving it closer to a Piton or a more benign Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fringe_practice_quantification, empirical, 'Quantifies the extent of dueling''s persistence.').

omega_variable(
    suppression_mechanism_ambiguity,
    'To what extent was the suppression of dueling structural (legal penalties) versus internalized (social stigma and changing cultural values)?',
    'Analysis of post-legal-reform social attitudes and the persistence of honor-based violence in contexts where legal enforcement was weak. If honor-based violence persisted even without strong legal barriers, internalized suppression was less effective.',
    'If suppression was primarily structural, its removal would likely lead to a resurgence of dueling. If largely internalized, the constraint''s effective suppression is higher than the structural measure suggests, as the norm is carried by individuals.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for dueling.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_settlement_legitimacy__drop_reading, 1850, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1850, honor_settlement_legitimacy__drop_reading, theater_ratio, 1850, 0.35).
narrative_ontology:measurement(hono_tr_t1870, honor_settlement_legitimacy__drop_reading, theater_ratio, 1870, 0.38).
narrative_ontology:measurement(hono_tr_t1890, honor_settlement_legitimacy__drop_reading, theater_ratio, 1890, 0.4).
narrative_ontology:measurement(hono_tr_t1910, honor_settlement_legitimacy__drop_reading, theater_ratio, 1910, 0.42).
narrative_ontology:measurement(hono_tr_t1930, honor_settlement_legitimacy__drop_reading, theater_ratio, 1930, 0.41).
narrative_ontology:measurement(hono_tr_t1950, honor_settlement_legitimacy__drop_reading, theater_ratio, 1950, 0.4).

% Extraction over time
narrative_ontology:measurement(hono_be_t1850, honor_settlement_legitimacy__drop_reading, base_extractiveness, 1850, 0.6).
narrative_ontology:measurement(hono_be_t1870, honor_settlement_legitimacy__drop_reading, base_extractiveness, 1870, 0.63).
narrative_ontology:measurement(hono_be_t1890, honor_settlement_legitimacy__drop_reading, base_extractiveness, 1890, 0.65).
narrative_ontology:measurement(hono_be_t1910, honor_settlement_legitimacy__drop_reading, base_extractiveness, 1910, 0.66).
narrative_ontology:measurement(hono_be_t1930, honor_settlement_legitimacy__drop_reading, base_extractiveness, 1930, 0.65).
narrative_ontology:measurement(hono_be_t1950, honor_settlement_legitimacy__drop_reading, base_extractiveness, 1950, 0.64).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1850, honor_settlement_legitimacy__drop_reading, suppression_requirement, 1850, 0.7).
narrative_ontology:measurement(hono_su_t1870, honor_settlement_legitimacy__drop_reading, suppression_requirement, 1870, 0.73).
narrative_ontology:measurement(hono_su_t1890, honor_settlement_legitimacy__drop_reading, suppression_requirement, 1890, 0.75).
narrative_ontology:measurement(hono_su_t1910, honor_settlement_legitimacy__drop_reading, suppression_requirement, 1910, 0.76).
narrative_ontology:measurement(hono_su_t1930, honor_settlement_legitimacy__drop_reading, suppression_requirement, 1930, 0.75).
narrative_ontology:measurement(hono_su_t1950, honor_settlement_legitimacy__drop_reading, suppression_requirement, 1950, 0.74).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_settlement_legitimacy__drop_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__drop_reading, state_monopoly_on_violence).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__drop_reading, social_honor_codes).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__drop_reading, honor_settlement_legitimacy__contraction_reading).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__drop_reading, honor_settlement_legitimacy__composite_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'honor_settlement_legitimacy' kernel, focusing on the persistence of dueling as a fringe practice. It is linked to its sibling readings, 'contraction_reading' and 'composite_reading', which offer alternative explanations for dueling's decline.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
