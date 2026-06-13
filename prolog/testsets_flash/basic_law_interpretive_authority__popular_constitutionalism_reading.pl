% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_authority__popular_constitutionalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_authority__popular_constitutionalism_reading, []).

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
 *   constraint_id: basic_law_interpretive_authority__popular_constitutionalism_reading
 *   human_readable: Popular Constitutionalism: Ongoing Democratic Interpretation of Basic Law
 *   domain: constitutional_law/political_theory/institutional_design
 *
 * SUMMARY:
 *   This constraint describes the 'popular constitutionalism' reading of
 *   basic law interpretive authority, where constitutional meaning is
 *   understood to emerge from ongoing democratic contestation rather than
 *   being finally adjudicated by any single institutional body. This reading
 *   emphasizes the role of public opinion, social movements, and political
 *   processes in shaping the constitution's meaning, distributing
 *   interpretive power across multiple sites and preventing its capture by a
 *   single elite. The constraint is claimed as a Rope because it facilitates
 *   coordination around a dynamic, publicly-driven process of meaning-making,
 *   with relatively low extraction and suppression, though it entails costs
 *   of gridlock and slower resolution.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_authority__popular_constitutionalism_reading, 0.3).
domain_priors:suppression_score(basic_law_interpretive_authority__popular_constitutionalism_reading, 0.2).
domain_priors:theater_ratio(basic_law_interpretive_authority__popular_constitutionalism_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__popular_constitutionalism_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__popular_constitutionalism_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__popular_constitutionalism_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_authority__popular_constitutionalism_reading, rope).
narrative_ontology:human_readable(basic_law_interpretive_authority__popular_constitutionalism_reading, "Popular Constitutionalism: Ongoing Democratic Interpretation of Basic Law").
narrative_ontology:topic_domain(basic_law_interpretive_authority__popular_constitutionalism_reading, "constitutional_law/political_theory/institutional_design").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_authority__popular_constitutionalism_reading, 'b777277d-2468-468d-87df-c019e7409cd4').
narrative_ontology:cs_kernel_codification('b777277d-2468-468d-87df-c019e7409cd4', distributed).
narrative_ontology:cs_authority_grounding('b777277d-2468-468d-87df-c019e7409cd4', distributed).
narrative_ontology:cs_reading_relation('b777277d-2468-468d-87df-c019e7409cd4', basic_law_interpretive_authority__judicial_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('b777277d-2468-468d-87df-c019e7409cd4', basic_law_interpretive_authority__parliamentary_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('b777277d-2468-468d-87df-c019e7409cd4', foundational, popular_sovereignty_is_continuous).
narrative_ontology:cs_axiom_status(popular_sovereignty_is_continuous, holdable).
narrative_ontology:cs_axiom_grounding('b777277d-2468-468d-87df-c019e7409cd4', popular_sovereignty_is_continuous, deontological).
narrative_ontology:cs_axiom('b777277d-2468-468d-87df-c019e7409cd4', foundational, no_institutional_terminal_authority).
narrative_ontology:cs_axiom_status(no_institutional_terminal_authority, holdable).
narrative_ontology:cs_axiom_grounding('b777277d-2468-468d-87df-c019e7409cd4', no_institutional_terminal_authority, conventional).
narrative_ontology:cs_reference_frame('b777277d-2468-468d-87df-c019e7409cd4', founding_era_democratic_deliberation).
narrative_ontology:cs_drift_state('b777277d-2468-468d-87df-c019e7409cd4', contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('b777277d-2468-468d-87df-c019e7409cd4', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_authority__popular_constitutionalism_reading, basic_law_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__popular_constitutionalism_reading, the_electorate).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__popular_constitutionalism_reading, political_movements).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__popular_constitutionalism_reading, judiciary).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__popular_constitutionalism_reading, legislature).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from retaining ultimate interpretive authority over the constitution, ensuring that fundamental law reflects contemporary popular will. Bears the costs of potential instability and slower resolution of constitutional disputes.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, the_electorate, beneficiary,
    organized, generational, constrained, national).

% Gains legitimacy and influence by mobilizing public opinion to shape constitutional meaning, rather than being constrained by judicial or legislative fiat. Invests heavily in public discourse and advocacy.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, political_movements, beneficiary,
    moderate, biographical, mobile, national).

% Loses its claim to terminal interpretive authority, reducing its institutional power and prestige. Must defer to popular constitutional understandings, even if they conflict with legal precedent or judicial philosophy. Its identity as the 'final arbiter' is challenged.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, judiciary, payer,
    institutional, generational, identity_locked, national).

% Does not gain terminal interpretive authority, as its decisions are still subject to popular contestation. Must engage in continuous public debate to sustain constitutional interpretations, rather than relying solely on its democratic mandate.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, legislature, payer,
    institutional, generational, constrained, national).

% Analyzes the dynamics of popular constitutionalism, its historical manifestations, and its implications for institutional design and democratic theory. Provides critical commentary on the ongoing interpretive process.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, constitutional_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the ongoing process of constitutional meaning-making by ensuring that no single institution holds terminal authority, thereby requiring continuous public engagement and deliberation to legitimize constitutional interpretations.
% TRANSFER_FUNCTION: Transfers interpretive authority from specialized institutional bodies (judiciary, legislature) to the broader democratic public, distributing the costs of constitutional gridlock and the benefits of popular legitimacy across multiple sites of contestation.
% ABSENT_VOICES: Those who advocate for a fixed, immutable constitutional meaning, often appealing to original intent or natural law, are marginalized in a system of popular constitutionalism. They would argue for stability and predictability over ongoing contestation.
% DISAPPEARANCE_RATIONALE: If popular constitutionalism vanished, interpretive authority would likely consolidate in either the judiciary or the legislature, fundamentally altering the balance of power, the nature of constitutional debate, and the role of public opinion in shaping fundamental law. The entire institutional landscape would shift.
% FOUNDING_PROBLEM: The problem of ensuring that fundamental law remains responsive to the evolving values and needs of a democratic society, preventing constitutional meaning from becoming ossified or captured by a narrow elite.
% FOUNDING_PROBLEM_CORROBORATION: Political theorists, historians of constitutional development, and social movements consistently attest to the ongoing tension between institutional stability and democratic responsiveness, corroborating the live status of this founding problem from outside the direct beneficiaries.
narrative_ontology:disappearance_verdict(basic_law_interpretive_authority__popular_constitutionalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_authority__popular_constitutionalism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_authority__popular_constitutionalism_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(basic_law_interpretive_authority__popular_constitutionalism_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_authority__popular_constitutionalism_reading_tests).
:- end_tests(basic_law_interpretive_authority__popular_constitutionalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.3) because no single party captures significant rents from this interpretive process; the 'cost' is primarily the distributed effort of ongoing public engagement. Suppression is low (0.2) as the core tenet is to resist institutional suppression of popular interpretive claims. Theater ratio is low (0.1) because the process is genuinely about contestation, not performance. Accessibility collapse is moderate (0.4) as alternatives (judicial or legislative supremacy) are always present as counter-arguments, but this reading actively resists their dominance. Resistance is high (0.7) because this reading is constantly resisted by those who seek more stable, institutionally-adjudicated constitutional meaning.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the electorate and political movements, this constraint is a genuine Rope, enabling democratic self-governance. From the perspective of the judiciary and legislature, it imposes a cost by denying them final interpretive authority, potentially making it feel more like a Snare or Tangled Rope, as their institutional power is curtailed.
 *
 * DIRECTIONALITY LOGIC:
 *   The electorate and political movements are beneficiaries (d near 0.0) as they gain direct influence over constitutional meaning. The judiciary and legislature are payers (d near 1.0) as they lose their claim to terminal authority, which is a significant institutional cost. Constitutional scholars are observers (d near 0.5) as they analyze the process without directly benefiting or paying in the same way.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint inherently resists mandatrophy by embedding ongoing contestation as its core function. Its 'mandate' is to prevent any single interpretation from becoming ossified or detached from popular will. If it were to become a Piton, it would imply that the democratic contestation itself had become purely performative, with real interpretive power residing elsewhere, which would fundamentally contradict its nature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    stability_vs_responsiveness_tradeoff,
    'What is the optimal balance between constitutional stability (predictability, rule of law) and democratic responsiveness (popular will, evolving values) in a system of popular constitutionalism?',
    'Empirical studies comparing constitutional amendment rates, judicial review outcomes, and public opinion shifts across different constitutional systems over long time horizons.',
    'If the costs of instability (e.g., frequent constitutional crises, erosion of minority rights) are found to outweigh the benefits of responsiveness, this reading''s extractiveness (in terms of social friction) would be re-evaluated upward, potentially shifting its classification towards a Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stability_vs_responsiveness_tradeoff, empirical, 'The inherent tension between stability and responsiveness in popular constitutionalism.').

omega_variable(
    institutional_capture_risk,
    'Does the absence of terminal institutional adjudication in popular constitutionalism increase the risk of constitutional meaning being captured by powerful political factions or media narratives, rather than genuine popular deliberation?',
    'Case studies of constitutional moments where popular interpretation was demonstrably swayed by well-resourced political campaigns or biased media, leading to outcomes that did not reflect broad, informed public consensus.',
    'If such capture is frequent and effective, the suppression metric would be re-evaluated upward (as alternative, uncaptured interpretations are suppressed), and the extractiveness would increase (as specific factions benefit from manipulating meaning), potentially reclassifying it as a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_capture_risk, empirical, 'Risk of capture by political factions in the absence of institutional checks.').

omega_variable(
    reading_framing_underdetermination,
    'Is this constraint truly a ''popular constitutionalism'' reading, or is it a ''judicial supremacy'' reading with a strong democratic-rhetoric overlay?',
    'Analyze the actual outcomes of constitutional disputes: if judicial decisions consistently override popular sentiment without significant political consequence, the ''popular constitutionalism'' framing is merely rhetorical cover for judicial supremacy.',
    'If the latter, the constraint would be reclassified as ''judicial_supremacy_reading'', with a higher extractiveness (from judicial power) and suppression (of popular will), likely shifting its type to a Tangled Rope or Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_framing_underdetermination, conceptual, 'Ambiguity between genuine popular constitutionalism and rhetorical cover for judicial power.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_authority__popular_constitutionalism_reading, 1787, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t1787, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 1787, 0.05).
narrative_ontology:measurement(basi_tr_t1850, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 1850, 0.08).
narrative_ontology:measurement(basi_tr_t1900, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 1900, 0.1).
narrative_ontology:measurement(basi_tr_t1950, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 1950, 0.09).
narrative_ontology:measurement(basi_tr_t2000, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(basi_tr_t2024, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(basi_be_t1787, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 1787, 0.2).
narrative_ontology:measurement(basi_be_t1850, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 1850, 0.25).
narrative_ontology:measurement(basi_be_t1900, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 1900, 0.28).
narrative_ontology:measurement(basi_be_t1950, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 1950, 0.3).
narrative_ontology:measurement(basi_be_t2000, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 2000, 0.29).
narrative_ontology:measurement(basi_be_t2024, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 2024, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t1787, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 1787, 0.15).
narrative_ontology:measurement(basi_su_t1850, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 1850, 0.18).
narrative_ontology:measurement(basi_su_t1900, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 1900, 0.2).
narrative_ontology:measurement(basi_su_t1950, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 1950, 0.19).
narrative_ontology:measurement(basi_su_t2000, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 2000, 0.2).
narrative_ontology:measurement(basi_su_t2024, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 2024, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_authority__popular_constitutionalism_reading, identity_coordination).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__popular_constitutionalism_reading, judicial_supremacy_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__popular_constitutionalism_reading, parliamentary_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'basic_law_interpretive_authority' kernel. Each reading represents a different structural claim about where constitutional meaning resides and how it is adjudicated.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
