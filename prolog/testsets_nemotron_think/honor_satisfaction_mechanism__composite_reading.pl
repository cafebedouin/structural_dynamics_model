% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_mechanism__composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_mechanism__composite_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: honor_satisfaction_mechanism__composite_reading
 *   human_readable: Honor Satisfaction Mechanism — Composite Reading (State Monopoly, Bourgeois Norms, Insurance, Category-Shift)
 *   domain: historical_sociology/legal_history/normative_systems
 *
 * SUMMARY:
 *   The composite reading treats honor satisfaction as a constraint that
 *   operated through four simultaneous, partially overlapping mechanisms
 *   across 1500–1900: (1) the state's progressive monopoly on legitimate
 *   violence (criminalizing dueling, creating courts), (2) bourgeois
 *   commercial norms (reputation, credit, chambers of commerce) that offered
 *   non-violent satisfaction for merchants, (3) financial/insurance
 *   innovations (fidelity bonds, credit reporting, reputation insurance) that
 *   monetized trust, and (4) a category-shift that recoded 'honor' into
 *   'reputation,' 'creditworthiness,' and 'social capital' — making the old
 *   violent forms cognitively illegible. The constraint did not decline
 *   monotonically; it was actively displaced by competing coordination
 *   systems that extracted differently. The composite reading refuses the
 *   single-mechanism narratives of the decline_reading (frequency decay) and
 *   contraction_reading (cognitive unthinkability), arguing instead that
 *   multiple extractive pressures — state, market, finance, epistemic —
 *   operated independently and together eroded the constraint's coherence
 *   while each successor mechanism inherited its extraction logic.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_mechanism__composite_reading, 0.68).
domain_priors:suppression_score(honor_satisfaction_mechanism__composite_reading, 0.72).
domain_priors:theater_ratio(honor_satisfaction_mechanism__composite_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_mechanism__composite_reading, tangled_rope).
narrative_ontology:human_readable(honor_satisfaction_mechanism__composite_reading, "Honor Satisfaction Mechanism — Composite Reading (State Monopoly, Bourgeois Norms, Insurance, Category-Shift)").
narrative_ontology:topic_domain(honor_satisfaction_mechanism__composite_reading, "historical_sociology/legal_history/normative_systems").

domain_priors:requires_active_enforcement(honor_satisfaction_mechanism__composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_mechanism__composite_reading, 'fec1bf5a-836c-4cc8-aefe-2258104fe874').
narrative_ontology:cs_kernel_codification('fec1bf5a-836c-4cc8-aefe-2258104fe874', distributed).
narrative_ontology:cs_authority_grounding('fec1bf5a-836c-4cc8-aefe-2258104fe874', practice).
narrative_ontology:cs_interpretation_layer_present('fec1bf5a-836c-4cc8-aefe-2258104fe874').
narrative_ontology:cs_reading_relation('fec1bf5a-836c-4cc8-aefe-2258104fe874', honor_satisfaction_mechanism__decline_reading, coexists_with).
narrative_ontology:cs_reading_relation('fec1bf5a-836c-4cc8-aefe-2258104fe874', honor_satisfaction_mechanism__contraction_reading, influences).
narrative_ontology:cs_axiom('fec1bf5a-836c-4cc8-aefe-2258104fe874', foundational, honor_satisfaction_is_always_composite).
narrative_ontology:cs_axiom_status(honor_satisfaction_is_always_composite, holdable).
narrative_ontology:cs_axiom_grounding('fec1bf5a-836c-4cc8-aefe-2258104fe874', honor_satisfaction_is_always_composite, empirically_contingent).
narrative_ontology:cs_axiom('fec1bf5a-836c-4cc8-aefe-2258104fe874', foundational, displacement_mechanisms_operate_independently).
narrative_ontology:cs_axiom_status(displacement_mechanisms_operate_independently, holdable).
narrative_ontology:cs_axiom_grounding('fec1bf5a-836c-4cc8-aefe-2258104fe874', displacement_mechanisms_operate_independently, empirically_contingent).
narrative_ontology:cs_reference_frame('fec1bf5a-836c-4cc8-aefe-2258104fe874', early_modern_honor_order).
narrative_ontology:cs_drift_state('fec1bf5a-836c-4cc8-aefe-2258104fe874', long_nineteenth_century, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fec1bf5a-836c-4cc8-aefe-2258104fe874', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_mechanism__composite_reading, honor_satisfaction_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__composite_reading, aristocracy_nobility).
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__composite_reading, state_legal_authorities).
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__composite_reading, bourgeoisie_merchants).
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__composite_reading, insurance_financial_institutions).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__composite_reading, commoners_excluded_from_honor).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__composite_reading, women_excluded_from_honor).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__composite_reading, dueling_participants_killed_injured).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__composite_reading, marginalized_groups_denied_satisfaction).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__composite_reading, aristocracy_nobility).
narrative_ontology:constraint_vindicates(honor_satisfaction_mechanism__composite_reading, state_monopoly_on_violence_doctrine).
narrative_ontology:constraint_vindicates(honor_satisfaction_mechanism__composite_reading, commercial_reputation_as_honor_substitute).
narrative_ontology:constraint_vindicates(honor_satisfaction_mechanism__composite_reading, financial_risk_pooling_for_reputation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The honor system defined their social identity and political legitimacy. They benefited from exclusive access to satisfaction (duels, courts of honor) but paid with their lives — dueling mortality was concentrated in this class. Exit meant abandoning the identity that constituted their class position; the code was internalized as self-concept.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, aristocracy_nobility, beneficiary,
    powerful, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_mechanism__composite_reading, aristocracy_nobility, payer).

% Monopolized legitimate violence through courts and criminal law, progressively criminalizing dueling while offering legal satisfaction (libel, assault courts). They extracted compliance and legitimacy from suppressing private violence, but also coordinated social order. Could reform the system but faced resistance from nobility and practical limits of enforcement.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, state_legal_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Developed commercial reputation, credit networks, and contractual enforcement as functional substitutes for aristocratic honor. Gained access to satisfaction without lethal risk; their exit from the honor system was upward — they built a parallel system that eventually superseded it. Benefited from state courts but also created private ordering (chambers of commerce, exchanges).
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, bourgeoisie_merchants, beneficiary,
    organized, biographical, mobile, national).

% Created financial instruments (reputation insurance, fidelity bonds, credit reporting) that monetized honor-satisfaction functions. Extracted rents from the transition by selling certainty where honor once demanded violence. Their interest was in the persistence of reputation-as-asset, not in the older violent forms.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, insurance_financial_institutions, beneficiary,
    organized, generational, arbitrage, national).

% Had no standing in the honor system — could not give or receive satisfaction, were subject to aristocratic violence without recourse, and later faced bourgeois legal exclusion (debtors' prison, reputation blacklisting). The constraint extracted labor and deference without offering protection. Exit was structurally blocked by class position.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, commoners_excluded_from_honor, payer,
    powerless, biographical, trapped, local).

% Formally excluded from the honor system (could not duel, could not sue for honor in most jurisdictions), yet bore its costs — reputational ruin, social death, violence from male relatives enforcing family honor. The constraint extracted compliance through gendered honor codes while denying them agency. Exit meant total social exclusion.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, women_excluded_from_honor, payer,
    powerless, biographical, trapped, local).

% The direct physical victims of the honor mechanism — aristocrats and officers who died or were maimed in duels they could not refuse without social death. Their participation was coerced by identity_locked pressure; the cost was existential and irreversible. The coordination function (dispute resolution) existed but the extraction was lethal.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, dueling_participants_killed_injured, payer,
    moderate, immediate, constrained, local).

% Jews, minorities, colonial subjects, and other groups systematically denied standing in any satisfaction mechanism — state courts, honor courts, commercial reputation systems. Would have objected to all readings of the kernel; their absence is structural, not incidental. The constraint's persistence depended on their exclusion.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, marginalized_groups_denied_satisfaction, excluded,
    powerless, generational, trapped, national).

% Analyze the mechanism from outside the system. See the full structural arc: state monopoly displacing private violence, bourgeois norms displacing aristocratic honor, financialization displacing both, category-shift recoding honor as reputation/credit. No stake in any reading; the constraint is an object of study.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, historical_sociologists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a structured, culturally legible pathway for resolving status disputes and enforcing commitments without state intervention — duels, courts of honor, and later commercial arbitration replaced feuding and private vengeance with bounded, ritualized confrontation.
% TRANSFER_FUNCTION: Moved the risk of violent death and social destruction onto duelists (disproportionately aristocratic men), moved the monopoly rent of legitimate violence to the state, moved the coordination surplus to bourgeois merchants (who got cheaper dispute resolution), and moved financialized reputation-rents to insurance institutions.
% ABSENT_VOICES: Commoners, women, Jews, colonial subjects, and the enslaved — structurally excluded from all satisfaction mechanisms. They would have objected to the violence of dueling, the class bias of state courts, the commercial exclusion of bourgeois reputation, and the financialization of identity. Their absence is not accidental; the mechanism's coherence required their silence.
% DISAPPEARANCE_RATIONALE: If the honor satisfaction mechanism vanished overnight (no duels, no honor courts, no commercial reputation, no credit scores), the social order would not revert to pre-honor chaos — it would reorganize around whatever satisfaction mechanisms exist at that moment (state law, social media reputation, algorithmic scoring, gang enforcement). The function is persistent; the form is contingent. The world rearranges because the coordination problem (how to settle disputes without unlimited violence) is permanent.
% FOUNDING_PROBLEM: How to resolve status disputes and enforce commitments in a society where the state lacks capacity to monopolize violence, where kinship feuds threaten social order, and where commercial exchange requires trust between strangers.
% FOUNDING_PROBLEM_CORROBORATION: State-centric historians (Weber, Elias) attest the founding problem was state monopoly formation — the mechanism was a transitional scaffold. Bourdieu-influenced sociologists attest it was symbolic capital reproduction — the problem is live (status competition persists). Economic historians (Greif, Milgrom) attest it was commercial coordination — the problem was solved by reputation mechanisms that replaced honor. No single corroboration outside the beneficiary sets; the founding problem is multiply realized and the readings disagree on which realization counts.
narrative_ontology:disappearance_verdict(honor_satisfaction_mechanism__composite_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_satisfaction_mechanism__composite_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_mechanism__composite_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(honor_satisfaction_mechanism__composite_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_mechanism__composite_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_mechanism__composite_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(honor_satisfaction_mechanism__composite_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(honor_satisfaction_mechanism__composite_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.68: The mechanism extracted lethal risk from aristocrats, total exclusion from commoners/women/minorities, and monopoly rents for state/merchants/insurers — but also provided genuine coordination (dispute resolution, trust in trade). Suppression 0.72: Active enforcement by state (anti-dueling laws), social pressure (honor codes), and market exclusion (credit blacklists) maintained the constraint; alternatives were suppressed, not just disadvantaged. Theater 0.45 rising: Early period had functional duels/courts; later period saw performative duels (first blood, delope) and ceremonial honor courts while real satisfaction migrated to courts and markets. Accessibility collapse 0.62: Once you understood the honor code, alternatives (legal petition, commercial arbitration) were cognitively and structurally difficult — but not impossible, as bourgeois exit shows. Resistance 0.58: Significant resistance from nobility (dueling persisted illegally), from excluded groups (petitions, riots, alternative mutual aid), and from merchants building parallel systems.
 *
 * PERSPECTIVAL GAP:
 *   The aristocracy experiences the constraint as identity-constituting coordination (they are the honor system); the state experiences it as a monopoly to be enforced and then replaced; the bourgeoisie experiences it as a legacy system to be arbitraged and superseded; the excluded experience it as pure extraction with no coordination benefit. The engine computes per-seat types from these structural positions: for the aristocracy it may compute as rope (coordination they depend on); for the state as tangled_rope (monopoly coordination + extraction); for the excluded as snare (pure extraction); for observers as mountain (the coordination problem is permanent). The divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Aristocracy: identity_locked beneficiaries who also pay with their lives (d ~ 0.55 — net beneficiary but high cost). State: agenda_setter with arbitrage exit (d ~ 0.15 — structural beneficiary). Bourgeoisie: mobile beneficiaries who built exit (d ~ 0.2). Insurers: arbitrage beneficiaries (d ~ 0.1). Commoners/women/minorities: trapped payers (d ~ 0.95). Duelists: constrained payers facing existential cost (d ~ 0.85). The directionality spread is wide because the constraint operated differently at each social level — not a single mechanism but a stacked apparatus.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's original mandate (resolve disputes without state capacity, prevent feuding) was live in 1500. By 1900, state courts, commercial arbitration, and financial instruments had solved the coordination problem more efficiently. The constraint persisted as theater (ceremonial duels, honor courts) and as extracted rents (state monopoly, credit systems, insurance) — a classic mandatrophy where the function atrophied but the structure remained, repurposed by each successor mechanism. The composite reading catches this by showing multiple independent extractive pressures, not a single decay curve.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_composite_vs_decline,
    'Does the composite reading''s multi-mechanism account foreclose the decline_reading''s frequency-decay narrative, or do they coexist as different granularities of the same historical process?',
    'Test whether frequency data for dueling (decline_reading''s observable) can be fully explained by the displacement mechanisms the composite reading identifies (state criminalization, bourgeois exit, financialization). If residual decline remains unexplained, the readings coexist; if fully explained, composite forecloses decline.',
    'If composite forecloses decline, the kernel''s contest reduces to composite vs. contraction. If they coexist, the kernel has three live readings with different causal granularities — the engine must track all three as separate constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_composite_vs_decline, conceptual, 'Whether multi-mechanism displacement explains away frequency decay or leaves it as an independent dynamic.').

omega_variable(
    kernel_reading_composite_vs_contraction,
    'Does the category-shift mechanism (honor → reputation/credit) in the composite reading logically entail the contraction_reading''s claim that dueling became cognitively unthinkable, or is cognitive unthinkability a separate epistemic break?',
    'Analyze whether the semantic shift in honor vocabulary (tracked in discourse corpora) correlates with the institutional displacement mechanisms, or whether there is an autonomous cultural break (e.g., sentimentalism, humanitarianism) that made violence illegible as honor.',
    'If category-shift entails contraction, the composite reading influences contraction (downstream pressure). If contraction is autonomous, they coexist — the kernel has two independent displacement logics (institutional and epistemic).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_composite_vs_contraction, conceptual, 'Whether institutional displacement causes cognitive contraction or they are independent.').

omega_variable(
    extraction_coordination_boundary_per_mechanism,
    'For each of the four mechanisms (state monopoly, bourgeois norms, insurance, category-shift), what is the independent ε — how much did each extract vs. coordinate?',
    'Disaggregate the composite constraint into four sub-constraints per the ε-invariance principle. Measure state monopoly''s extraction (monopoly rent vs. order coordination), bourgeois norms'' extraction (exclusionary reputation vs. trade coordination), insurance''s extraction (financial rents vs. risk pooling), category-shift''s extraction (epistemic capture vs. conceptual clarification).',
    'If any sub-mechanism is mountain (ε ≈ 0) or snare (ε high, no coordination), the composite reading''s tangled_rope claim for the whole is an aggregation artifact. The kernel may need four constraint stories, not one composite.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_coordination_boundary_per_mechanism, empirical, 'Whether the composite constraint''s ε is an average of structurally distinct sub-constraints.').

omega_variable(
    excluded_groups_coalition_potential,
    'Could the structurally excluded groups (commoners, women, minorities) have formed a coalition capable of disrupting the honor satisfaction mechanism, or was their exit_options=''trapped'' structurally irreducible?',
    'Historical counterfactual: examine moments of potential coalition (e.g., 1848 revolutions, abolitionist-feminist alliances, Jewish emancipation movements) — did honor satisfaction appear on their collective agenda? If never, trapped is structural; if yes but failed, coalition power existed but was defeated.',
    'If coalition was possible, the constraint''s suppression is lower than measured (resistance was organizable). If structurally irreducible, suppression 0.72 is accurate and the constraint is a snare for these groups regardless of coordination for others.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(excluded_groups_coalition_potential, empirical, 'Whether the excluded''s powerlessness was structural or contingent on failed organization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_mechanism__composite_reading, 1500, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(honor_sat_comp_tr_t1500, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1500, 0.15).
narrative_ontology:measurement(honor_sat_comp_tr_t1550, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1550, 0.18).
narrative_ontology:measurement(honor_sat_comp_tr_t1600, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1600, 0.22).
narrative_ontology:measurement(honor_sat_comp_tr_t1650, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1650, 0.28).
narrative_ontology:measurement(honor_sat_comp_tr_t1700, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1700, 0.35).
narrative_ontology:measurement(honor_sat_comp_tr_t1750, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1750, 0.38).
narrative_ontology:measurement(honor_sat_comp_tr_t1800, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1800, 0.42).
narrative_ontology:measurement(honor_sat_comp_tr_t1850, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1850, 0.44).
narrative_ontology:measurement(honor_sat_comp_tr_t1900, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1900, 0.45).

% Extraction over time
narrative_ontology:measurement(honor_sat_comp_be_t1500, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1500, 0.45).
narrative_ontology:measurement(honor_sat_comp_be_t1550, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1550, 0.48).
narrative_ontology:measurement(honor_sat_comp_be_t1600, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1600, 0.52).
narrative_ontology:measurement(honor_sat_comp_be_t1650, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1650, 0.58).
narrative_ontology:measurement(honor_sat_comp_be_t1700, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1700, 0.62).
narrative_ontology:measurement(honor_sat_comp_be_t1750, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1750, 0.65).
narrative_ontology:measurement(honor_sat_comp_be_t1800, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1800, 0.67).
narrative_ontology:measurement(honor_sat_comp_be_t1850, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1850, 0.68).
narrative_ontology:measurement(honor_sat_comp_be_t1900, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1900, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(honor_sat_comp_su_t1500, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1500, 0.55).
narrative_ontology:measurement(honor_sat_comp_su_t1550, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1550, 0.58).
narrative_ontology:measurement(honor_sat_comp_su_t1600, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1600, 0.62).
narrative_ontology:measurement(honor_sat_comp_su_t1650, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1650, 0.65).
narrative_ontology:measurement(honor_sat_comp_su_t1700, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1700, 0.68).
narrative_ontology:measurement(honor_sat_comp_su_t1750, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1750, 0.7).
narrative_ontology:measurement(honor_sat_comp_su_t1800, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1800, 0.71).
narrative_ontology:measurement(honor_sat_comp_su_t1850, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1850, 0.72).
narrative_ontology:measurement(honor_sat_comp_su_t1900, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1900, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_mechanism__composite_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(honor_satisfaction_mechanism__composite_reading, 0.1).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__composite_reading, honor_satisfaction_mechanism__decline_reading).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__composite_reading, honor_satisfaction_mechanism__contraction_reading).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__composite_reading, state_monopoly_on_violence).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__composite_reading, commercial_reputation_system).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__composite_reading, credit_reporting_infrastructure).

% DUAL FORMULATION NOTE:
% The honor_satisfaction_mechanism kernel decomposes into three constraint stories (composite_reading, decline_reading, contraction_reading) linked by network.affects_constraints. The composite reading treats the mechanism as a multi-mechanism apparatus; the decline reading treats it as a frequency-decay process; the contraction reading treats it as a cognitive-category collapse. Each has distinct ε, stakeholders, and temporal profiles. The composite reading influences both siblings by providing the displacement mechanisms that explain their observables.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(honor_satisfaction_mechanism__composite_reading, powerful, 0.55).
constraint_indexing:directionality_override(honor_satisfaction_mechanism__composite_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
