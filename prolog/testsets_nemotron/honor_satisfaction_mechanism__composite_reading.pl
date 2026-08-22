% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_mechanism__composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
 *   human_readable: Composite Honor Satisfaction Mechanism (1750-1850)
 *   domain: historical_sociology/legal_history/normative_systems
 *
 * SUMMARY:
 *   The honor satisfaction mechanism in Europe 1750-1850 was not a single
 *   constraint but a composite of four distinct mechanisms operating
 *   simultaneously: (1) state monopoly on violence criminalizing dueling, (2)
 *   bourgeois norms replacing personal honor with professional reputation and
 *   contract, (3) insurance actuarial logic treating honor violence as
 *   uninsurable risk, (4) category-shift recategorizing dueling from 'honor
 *   satisfaction' to 'murder/manslaughter.' Each mechanism extracted from the
 *   aristocratic/gentry honor system while providing partial coordination
 *   functions. The composite constraint is a tangled rope: it coordinates
 *   dispute resolution through new institutional channels while extracting
 *   status, bodily safety, and cultural autonomy from the old honor-bound
 *   classes. The structural delta is erosion via multiple independent
 *   extractive pressures plus recategorization — not a single causal line.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_mechanism__composite_reading, 0.68).
domain_priors:suppression_score(honor_satisfaction_mechanism__composite_reading, 0.72).
domain_priors:theater_ratio(honor_satisfaction_mechanism__composite_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_mechanism__composite_reading, tangled_rope).
narrative_ontology:human_readable(honor_satisfaction_mechanism__composite_reading, "Composite Honor Satisfaction Mechanism (1750-1850)").
narrative_ontology:topic_domain(honor_satisfaction_mechanism__composite_reading, "historical_sociology/legal_history/normative_systems").

domain_priors:requires_active_enforcement(honor_satisfaction_mechanism__composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_mechanism__composite_reading, '10b396b6-e3c1-4017-9a01-06f53a89e735').
narrative_ontology:cs_kernel_codification('10b396b6-e3c1-4017-9a01-06f53a89e735', distributed).
narrative_ontology:cs_authority_grounding('10b396b6-e3c1-4017-9a01-06f53a89e735', distributed).
narrative_ontology:cs_reading_relation('10b396b6-e3c1-4017-9a01-06f53a89e735', honor_satisfaction_mechanism__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('10b396b6-e3c1-4017-9a01-06f53a89e735', honor_satisfaction_mechanism__decline_reading, coexists_with).
narrative_ontology:cs_axiom('10b396b6-e3c1-4017-9a01-06f53a89e735', foundational, honor_satisfaction_is_multi_mechanism).
narrative_ontology:cs_axiom_status(honor_satisfaction_is_multi_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('10b396b6-e3c1-4017-9a01-06f53a89e735', honor_satisfaction_is_multi_mechanism, empirically_contingent).
narrative_ontology:cs_axiom('10b396b6-e3c1-4017-9a01-06f53a89e735', foundational, extraction_pressures_are_independent_and_compounding).
narrative_ontology:cs_axiom_status(extraction_pressures_are_independent_and_compounding, holdable).
narrative_ontology:cs_axiom_grounding('10b396b6-e3c1-4017-9a01-06f53a89e735', extraction_pressures_are_independent_and_compounding, empirically_contingent).
narrative_ontology:cs_axiom('10b396b6-e3c1-4017-9a01-06f53a89e735', secondary, recategorization_is_extraction_lock_in).
narrative_ontology:cs_axiom_status(recategorization_is_extraction_lock_in, holdable).
narrative_ontology:cs_axiom_grounding('10b396b6-e3c1-4017-9a01-06f53a89e735', recategorization_is_extraction_lock_in, conventional).
narrative_ontology:cs_reference_frame('10b396b6-e3c1-4017-9a01-06f53a89e735', pre_state_monopoly_honor_order).
narrative_ontology:cs_drift_state('10b396b6-e3c1-4017-9a01-06f53a89e735', post_napoleonic_settlement, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('10b396b6-e3c1-4017-9a01-06f53a89e735', '2026-08-05T14:30:00Z').
narrative_ontology:cs_kernel_id(honor_satisfaction_mechanism__composite_reading, honor_satisfaction_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__composite_reading, state_sovereignty_apparatus).
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__composite_reading, bourgeois_professional_classes).
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__composite_reading, insurance_underwriters).
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__composite_reading, legal_profession).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__composite_reading, aristocratic_officer_corps).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__composite_reading, honor_bound_gentry).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__composite_reading, military_subalterns).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__composite_reading, dueling_participants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claims monopoly on legitimate violence and honor adjudication through courts-martial, civilian courts, and police powers. Enforces anti-dueling edicts to consolidate state authority over dispute resolution. Benefits from eliminating private violence that challenges state sovereignty.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, state_sovereignty_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Merchants, lawyers, doctors, and civil servants who gain status and legal protection by displacing aristocratic honor culture with contract law and professional reputation. Their honor satisfaction comes through courts and professional bodies, not personal combat.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, bourgeois_professional_classes, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_mechanism__composite_reading, bourgeois_professional_classes, agenda_setter).

% Life and fire insurance companies that treat dueling as an uninsurable actuarial risk. They lobby for legal prohibition because duelists invalidate risk pools and create moral hazard. Their commercial interest aligns with state suppression of private violence.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, insurance_underwriters, beneficiary,
    organized, biographical, mobile, national).

% Lawyers and judges who replace honor courts with formal litigation. They control the new procedural apparatus for satisfaction (libel suits, assault charges, satisfaction via apology). Their professional monopoly on dispute resolution expands as dueling declines.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, legal_profession, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_mechanism__composite_reading, legal_profession, agenda_setter).

% Military aristocrats for whom dueling is constitutive of officer identity and command authority. State prohibition threatens their professional honor and internal cohesion. They cannot exit the constraint without abandoning their self-conception as a warrior caste.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, aristocratic_officer_corps, payer,
    powerful, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_mechanism__composite_reading, aristocratic_officer_corps, payer).

% Landowning gentry whose social standing depends on personal readiness to defend honor through combat. Legal suppression makes them vulnerable to insult without recognized recourse. Their identity is fused with the duel as the ultimate proof of gentlemanly status.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, honor_bound_gentry, payer,
    moderate, biographical, identity_locked, regional).

% Junior officers and enlisted men pressured by regimental honor culture to fight duels they cannot refuse without career destruction. They bear the physical risk while senior officers and the state extract the symbolic capital.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, military_subalterns, payer,
    powerless, immediate, trapped, local).

% Individuals caught in specific honor disputes who face prosecution if they duel and social death if they don't. The constraint extracts their liberty and safety while offering no coherent alternative satisfaction mechanism during the transition period.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, dueling_participants, payer,
    powerless, immediate, constrained, local).

% Analyzes the mechanism-shift as a structural recategorization of honor satisfaction from personal violence to institutional procedures. Sees four simultaneous extraction pressures rather than a single causal line.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, historical_sociologist, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a decentralized, class-bound mechanism for settling status disputes and maintaining group cohesion among elites without state intervention — a private ordering system for honor that operated alongside and sometimes against formal law.
% TRANSFER_FUNCTION: Moved the capacity to define and enforce honor satisfaction from personalized violence (dueling) to four institutional channels: state courts (criminal prosecution), bourgeois courts (civil libel/defamation), insurance actuarial tables (risk exclusion), and professional bodies (reputational sanctions). Extraction flowed as: aristocratic bodies/risk -> state sovereignty + bourgeois legal fees + insurance premiums + professional gatekeeping rents.
% ABSENT_VOICES: Women of the aristocratic and gentry classes whose honor was defended/violated through male proxies but who had no standing in any satisfaction mechanism. Colonial subjects and non-Europeans excluded from the honor system entirely but subject to its violence. The urban poor who faced analogous violence without the dignity of 'honor' recognition.
% DISAPPEARANCE_RATIONALE: If the composite mechanism vanished in 1800, the aristocratic officer corps would revert to open dueling, the state would lose its violence monopoly claim, insurance markets would face unpriced mortality risks, and the legal profession would lose a major jurisdictional expansion. The entire settlement of honor disputes would reorganize around personal combat.
% FOUNDING_PROBLEM: How to settle status disputes among armed elites in a way that maintains social order without requiring a strong central state — the duel was the private law of the gentleman when public law was weak or partial.
% FOUNDING_PROBLEM_CORROBORATION: Confirmed by military historians (e.g., Kiernan, The Duel in European History) and legal historians (e.g., Shoemaker, The London Mob) outside the benefiting parties: the founding problem (weak state, armed elite) was resolved by state consolidation and disarmament of elites by 1830, yet the anti-dueling apparatus persisted and intensified.
narrative_ontology:disappearance_verdict(honor_satisfaction_mechanism__composite_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_satisfaction_mechanism__composite_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_mechanism__composite_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
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
 *   Extraction (0.68) reflects the compounding effect: state extracts sovereignty compliance, bourgeoisie extracts legal fees and status monopoly, insurance extracts risk-free pools, legal profession extracts jurisdictional rent. Suppression (0.72) is high because all four mechanisms actively prohibit the old form (dueling) while the new forms are not yet fully functional for all classes. Theater ratio (0.58) rises over time as the 'honor' justification for suppression becomes increasingly performative — the real work is sovereignty and market rationalization. Accessibility collapse (0.65) is moderate: alternatives (courts, insurance, professional bodies) exist but are class-gated. Resistance (0.55) is significant but fragmented across the four mechanisms.
 *
 * PERSPECTIVAL GAP:
 *   From the state/bourgeois seats, the mechanism is a rope (genuine coordination of dispute resolution). From the aristocratic officer seat, it is a snare (pure extraction of their honor capacity). From the gentry seat, it is a tangled rope (coordination exists but they are excluded from it). The engine computes this divergence from the structural data — the composite reading's claim of 'tangled rope' reflects the aggregate structure, not any single seat.
 *
 * DIRECTIONALITY LOGIC:
 *   State and bourgeois/professional/insurance beneficiaries sit at d≈0.1-0.2 (constraint subsidizes them). Aristocratic officers are identity-locked at d≈0.9 — the constraint attacks their constitutive practice. Gentry are identity-locked at d≈0.85. Subalterns are trapped at d≈0.95. Dueling participants are constrained at d≈0.8. The analytical observer sits at d=0.5. The four beneficiary groups have different power atoms but converge on low directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (weak state, armed elite dispute resolution) died by 1830, but the constraint persisted and intensified — classic mandatrophy. The composite mechanism's coordination function (state courts, bourgeois law, insurance, professional bodies) was live and strengthening, but the extraction from honor-bound classes continued past functional necessity. The recategorization (dueling → crime) is the mandatrophy signature: a category-shift that locks in extraction after the coordination problem is solved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mechanism_independence,
    'Are the four mechanisms (state, bourgeois, insurance, category-shift) structurally independent extraction pressures, or do they form a single causal chain where one drives the others?',
    'Counterfactual analysis: if any one mechanism were removed (e.g., no insurance actuarial pressure), would the others still produce the observed erosion trajectory? Compare jurisdictions with different mechanism combinations (e.g., Prussia vs. England vs. France).',
    'If independent, the composite reading''s multi-mechanism structure is validated and ε=0.68 reflects compounding extraction. If a single chain, the constraint should be decomposed into a primary driver with downstream effects, reducing ε for the composite.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mechanism_independence, conceptual, 'Whether the four mechanisms are independent extractive pressures or a single causal chain.').

omega_variable(
    coordination_vs_extraction_boundary,
    'At what point does the coordination function (state courts, bourgeois law, insurance, professional bodies) become self-sustaining without extraction from the honor-bound classes?',
    'Track the institutional capacity of each new mechanism over time: when do courts handle honor disputes without referencing the old code? When does professional reputation fully replace personal honor? When does insurance cover all mortality risks without honor exclusions?',
    'If coordination becomes self-sustaining before 1850, the rising theater_ratio and extractiveness after that point are pure mandatrophy. If coordination never fully replaces the old system for all classes, the extraction is structural to the transition itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_vs_extraction_boundary, empirical, 'Temporal boundary between genuine coordination and pure extraction in the composite mechanism.').

omega_variable(
    identity_lock_mechanism,
    'Is the identity_locked exit option for aristocratic officers and gentry a genuine structural trap (no alternative honor satisfaction exists) or an ideological commitment (they refuse the alternatives that exist)?',
    'Examine whether officers/gentry who accepted the new mechanisms (court litigation, professional reputation) suffered status loss or were integrated. Track biographical trajectories of ''converts'' vs. ''holdouts''.',
    'If structural trap, directionality d→1.0 is correct and extraction is maximized. If ideological commitment, d should be lower (~0.6-0.7) and the constraint is less extractive than measured — the victims partly construct their own victimhood.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, conceptual, 'Whether identity_lock reflects structural absence of alternatives or ideological refusal of existing alternatives.').

omega_variable(
    composite_reading_commitment,
    'Does this composite reading''s core premise (four independent mechanisms) logically foreclose the contraction_reading (cognitive unthinkability) or decline_reading (demographic fade), or do they coexist as complementary explanations?',
    'Test whether a single historical actor could hold the composite reading AND the contraction reading simultaneously without contradiction. If the composite requires multiple mechanisms, does that contradict the contraction reading''s claim of a single cognitive shift?',
    'If forecloses, the readings are mutually exclusive kernel interpretations. If coexists_with, they are compatible frames emphasizing different causal layers. If influences, the composite reading''s multi-mechanism account creates pressure on the contraction reading''s cognitive primacy claim.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(composite_reading_commitment, conceptual, 'Structural relationship between composite reading and sibling kernel readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_mechanism__composite_reading, 1750, 1850).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hsm_composite_tr_t1750, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1750, 0.25).
narrative_ontology:measurement(hsm_composite_tr_t1775, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1775, 0.35).
narrative_ontology:measurement(hsm_composite_tr_t1800, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1800, 0.45).
narrative_ontology:measurement(hsm_composite_tr_t1825, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1825, 0.52).
narrative_ontology:measurement(hsm_composite_tr_t1840, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1840, 0.56).
narrative_ontology:measurement(hsm_composite_tr_t1850, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1850, 0.58).

% Extraction over time
narrative_ontology:measurement(hsm_composite_be_t1750, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1750, 0.35).
narrative_ontology:measurement(hsm_composite_be_t1775, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1775, 0.42).
narrative_ontology:measurement(hsm_composite_be_t1800, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1800, 0.55).
narrative_ontology:measurement(hsm_composite_be_t1825, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1825, 0.62).
narrative_ontology:measurement(hsm_composite_be_t1840, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1840, 0.66).
narrative_ontology:measurement(hsm_composite_be_t1850, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1850, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(hsm_composite_su_t1750, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1750, 0.4).
narrative_ontology:measurement(hsm_composite_su_t1775, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1775, 0.5).
narrative_ontology:measurement(hsm_composite_su_t1800, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1800, 0.62).
narrative_ontology:measurement(hsm_composite_su_t1825, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1825, 0.68).
narrative_ontology:measurement(hsm_composite_su_t1840, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1840, 0.71).
narrative_ontology:measurement(hsm_composite_su_t1850, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1850, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_mechanism__composite_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(honor_satisfaction_mechanism__composite_reading, 0.12).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__composite_reading, state_monopoly_violence_consolidation).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__composite_reading, bourgeois_legal_profession_formation).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__composite_reading, insurance_actuarial_rationalization).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__composite_reading, criminal_category_recodification_1750_1850).

% DUAL FORMULATION NOTE:
% Part of the honor_satisfaction_mechanism constraint family with contraction_reading and decline_reading. This composite reading decomposes the kernel into four simultaneous mechanisms with independent extraction pressures. The contraction_reading treats the shift as a single cognitive category-change; the decline_reading treats it as demographic attrition. All three readings share the same referent (the historical erosion of dueling) but author different ε values and different beneficiary/victim structures per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(honor_satisfaction_mechanism__composite_reading, powerful, 0.85).
constraint_indexing:directionality_override(honor_satisfaction_mechanism__composite_reading, moderate, 0.82).
constraint_indexing:directionality_override(honor_satisfaction_mechanism__composite_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
