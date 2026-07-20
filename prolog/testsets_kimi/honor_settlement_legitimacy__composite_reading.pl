% ============================================================================
% CONSTRAINT STORY: honor_settlement_legitimacy__composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_settlement_legitimacy__composite_reading, []).

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
 *   constraint_id: honor_settlement_legitimacy__composite_reading
 *   human_readable: Aristocratic Dueling Code for Honor Settlement (Composite Decline Reading)
 *   domain: historical_sociology/legal_history/cultural_anthropology
 *
 * SUMMARY:
 *   The aristocratic dueling code required gentlemen to settle serious
 *   insults through personal combat, enforced by social ostracism and status
 *   forfeiture. The composite reading of the honor_settlement_legitimacy
 *   kernel holds that dueling's decline from roughly 1750 to 1900 was
 *   overdetermined: cultural contraction (the code becoming cognitively
 *   unthinkable) formed the dominant edge, but was reinforced by independent
 *   material and institutional mechanismsâstate legal monopolization,
 *   professional bourgeois ascendance, and military bureaucratizationâthat
 *   would have suppressed the practice even without cultural change. This
 *   constraint story treats the dueling code itself as the constraint,
 *   tracking its lifecycle drift from active tangled-rope enforcement toward
 *   piton-like theatrical maintenance before effective disappearance.
 *
 * KEY AGENTS:
 *   - aristocratic_estate (agenda_setter/powerful/identity_locked): Maintains the dueling code and enforces it through social ostracism; fuses aristocratic identity with honor violence.
 *   - military_officer_class (beneficiary-payer/organized/constrained): Institutionalizes dueling for corps cohesion but bears mortality costs.
 *   - challenged_gentlemen (payer/moderate/trapped): Bear the direct physical and legal costs of compelled combat.
 *   - state_legal_apparatus (observer/institutional/analytical): Prosecutes and ultimately dismantles the legal conditions sustaining the code.
 *   - bourgeois_reformers (excluded/organized/mobile): Campaign against dueling from outside the honor group.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_settlement_legitimacy__composite_reading, 0.15).
domain_priors:suppression_score(honor_settlement_legitimacy__composite_reading, 0.2).
domain_priors:theater_ratio(honor_settlement_legitimacy__composite_reading, 0.85).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, theater_ratio, 0.85).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_settlement_legitimacy__composite_reading, tangled_rope).
narrative_ontology:human_readable(honor_settlement_legitimacy__composite_reading, "Aristocratic Dueling Code for Honor Settlement (Composite Decline Reading)").
narrative_ontology:topic_domain(honor_settlement_legitimacy__composite_reading, "historical_sociology/legal_history/cultural_anthropology").

domain_priors:requires_active_enforcement(honor_settlement_legitimacy__composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_settlement_legitimacy__composite_reading, '6cd87dda-eb37-439b-84b4-3f15216c3c94').
narrative_ontology:cs_kernel_codification('6cd87dda-eb37-439b-84b4-3f15216c3c94', fixed_text).
narrative_ontology:cs_authority_grounding('6cd87dda-eb37-439b-84b4-3f15216c3c94', practice).
narrative_ontology:cs_interpretation_layer_present('6cd87dda-eb37-439b-84b4-3f15216c3c94').
narrative_ontology:cs_reading_relation('6cd87dda-eb37-439b-84b4-3f15216c3c94', honor_settlement_legitimacy__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('6cd87dda-eb37-439b-84b4-3f15216c3c94', honor_settlement_legitimacy__drop_reading, coexists_with).
narrative_ontology:cs_axiom('6cd87dda-eb37-439b-84b4-3f15216c3c94', foundational, cultural_unthinkability_is_necessary_but_insufficient).
narrative_ontology:cs_axiom_status(cultural_unthinkability_is_necessary_but_insufficient, holdable).
narrative_ontology:cs_axiom_grounding('6cd87dda-eb37-439b-84b4-3f15216c3c94', cultural_unthinkability_is_necessary_but_insufficient, empirically_contingent).
narrative_ontology:cs_axiom('6cd87dda-eb37-439b-84b4-3f15216c3c94', secondary, material_and_legal_mechanisms_reinforce_symbolic_change).
narrative_ontology:cs_axiom_status(material_and_legal_mechanisms_reinforce_symbolic_change, holdable).
narrative_ontology:cs_axiom_grounding('6cd87dda-eb37-439b-84b4-3f15216c3c94', material_and_legal_mechanisms_reinforce_symbolic_change, empirically_contingent).
narrative_ontology:cs_reference_frame('6cd87dda-eb37-439b-84b4-3f15216c3c94', aristocratic_honor_practice).
narrative_ontology:cs_drift_state('6cd87dda-eb37-439b-84b4-3f15216c3c94', post_napoleonic_state_formation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6cd87dda-eb37-439b-84b4-3f15216c3c94', '').
narrative_ontology:cs_kernel_id(honor_settlement_legitimacy__composite_reading, honor_settlement_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__composite_reading, aristocratic_estate).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__composite_reading, military_officer_class).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__composite_reading, challenged_gentlemen).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__composite_reading, military_officer_class).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the code of honor through social sanction, sets the terms for insult and satisfaction, and enforces compliance via ostracism. Its collective identity is fused with the dueling ritual as a boundary marker against bourgeois and commoner classes.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, aristocratic_estate, agenda_setter,
    powerful, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(honor_settlement_legitimacy__composite_reading, aristocratic_estate, beneficiary).

% Institutionalizes dueling within the officer corps as a rite of courage and fraternity; benefits from internal cohesion and external prestige but bears the mortality and injury costs of the practice.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, military_officer_class, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(honor_settlement_legitimacy__composite_reading, military_officer_class, payer).

% Any gentleman who receives a challenge must fight or face social death and status forfeiture; bears the direct physical, legal, and psychological costs of the duel, with no legitimate institutional escape within the honor group.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, challenged_gentlemen, payer,
    moderate, biographical, trapped, regional).

% Criminalizes dueling and prosecutes fatalities, yet historically tolerates or selectively enforces the code among the aristocracy; increasingly dismantles the legal and social conditions that sustain the practice over the interval.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, state_legal_apparatus, observer,
    institutional, generational, analytical, national).

% Advocate for legal suppression of dueling and the substitution of court-based dispute resolution; structurally excluded from the honor group's internal negotiations but increasingly shape public opinion and state policy.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, bourgeois_reformers, excluded,
    organized, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a ritualized, bilateral mechanism for settling honor disputes among armed aristocrats, preventing open-ended vendettas, ambushes, and status free-for-all by formalizing violence into a rule-bound, survivable contest.
% TRANSFER_FUNCTION: Moves mortal risk and bodily injury from the challenged gentleman to the duelist pair, while transferring social status and boundary maintenance to the aristocratic estate and officer class; the collective extracts cohesion at the expense of the individual duelist's safety.
% ABSENT_VOICES: Women are entirely excluded from the honor code and its settlement mechanisms; bourgeois reformers and religious critics are structurally absent from the seconds' negotiations but campaign publicly against the practice; lower commoners lack standing to challenge or refuse.
% DISAPPEARANCE_RATIONALE: If the dueling code vanished overnight in 1750, aristocratic status hierarchy would lose a key boundary mechanism, honor disputes would shift to courts and patronage networks, and military officer bonding would require new rituals; the social world would rearrange around alternative legitimacy forms, as it historically did.
% FOUNDING_PROBLEM: Unregulated honor violence among armed aristocrats produced feuds, ambushes, and perpetual insecurity of person; the code was built to formalize and limit this violence to consensual, rule-bound combat.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and state formation theorists outside the aristocratic beneficiary class attest that the monopoly of violence migrated to state courts and criminal law by the mid-19th century; Enlightenment sociologists and bourgeois reformers corroborate that the problem of aristocratic violence was subsumed under modern legal frameworks. The aristocratic estate itself denies the problem is solved, asserting the code remains necessary for masculine virtue.
narrative_ontology:disappearance_verdict(honor_settlement_legitimacy__composite_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_settlement_legitimacy__composite_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_settlement_legitimacy__composite_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(honor_settlement_legitimacy__composite_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_settlement_legitimacy__composite_reading, 0.15, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_settlement_legitimacy__composite_reading_tests).
:- end_tests(honor_settlement_legitimacy__composite_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness declines from 0.80 to 0.15 over the interval as state law, bourgeois norms, and military bureaucracy independently undermine the code's enforceability. Suppression requirement tracks this decay (0.88 to 0.20) because the constraint's persistence depends on active social enforcement that erodes as alternatives become thinkable. Theater_ratio rises monotonically from 0.20 to 0.85: late duels become increasingly performative, ritualized, and legally shielded, while the code's functional role in status maintenance hollows out. Accessibility_collapse is low at end-state (0.20) because courts, social apology, and professional mediation are widely available by 1900. Resistance is low at end-state (0.15) because the constraint has lost both defenders and active challengers; it is a ghost code by the interval's end. The claim/metric divergence is intentional: the constraint is claimed as tangled_rope because its structural nature throughout the interval is hybrid coordination-extraction, but the end-state metrics honestly capture its near-total atrophy.
 *
 * PERSPECTIVAL GAP:
 *   The aristocratic seat experiences the code as necessary coordination preventing feudal chaos and maintaining masculine virtue; the challenged gentleman seat experiences compelled mortal risk with no legitimate exit. The composite reading adds that material/institutional seats (state prosecutors, bourgeois reformers) experience the constraint as an obsolete practice to be dismantled rather than maintained. The engine computes this divergence from the structural data: identity_locked exit for the estate amplifies its subsidy, while trapped exit for the gentleman amplifies his extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The aristocratic estate is the primary structural beneficiary (low d), capturing status boundary maintenance and collective identity cohesion. The military officer class is a mixed seat (intermediate d) because it extracts group solidarity while paying mortality costs. The challenged gentleman is the primary target (high d), bearing the physical and legal costs of the duel. State and bourgeois observers sit outside the extraction flow, with analytical exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâunregulated aristocratic violenceâwas solved by state formation and legal modernization well before the interval's end. The constraint persisted beyond its functional necessity, transitioning from an active tangled rope to a piton-like theatrical remnant. The composite reading prevents mislabeling the decline as purely cultural (contraction) or purely material by documenting convergent causation; it also prevents mislabeling the early code as pure extraction (snare) because the code genuinely coordinated honor disputes that would otherwise have produced vendettas. The temporal measurements capture this mandatrophy trajectory explicitly.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    composite_vs_contraction_causality,
    'Does the composite reading''s claim of material reinforcement add independent explanatory power beyond the contraction reading''s cultural framework transformation, or are the material mechanisms epiphenomenal to cultural change?',
    'Archival research isolating legal and military institutional pressures in regions with divergent state-formation timelines (e.g., German states vs. England) to test whether dueling declined even where bourgeois cultural hegemony was weaker.',
    'If material factors are epiphenomenal, the composite reading collapses toward the contraction reading and epsilon should shift downward; if independent, the composite reading is structurally validated as a distinct constraint with multicausal overdetermination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(composite_vs_contraction_causality, empirical, 'Whether material mechanisms in the composite reading are independent or epiphenomenal.').

omega_variable(
    fringe_persistence_scope,
    'Does the drop reading''s fringe persistence invalidate the composite reading''s overdetermination claim, or does residual dueling constitute a structurally distinct constraint disconnected from the aristocratic mainstream?',
    'Ethnographic and archival measurement of dueling incidence among residual subcultures (military academies, colonial outposts) versus the pre-1800 aristocratic mainstream to assess structural continuity.',
    'If fringe practice is structurally continuous with the old code, the composite reading overstates the decline; if discontinuous, the drop reading describes a separate constraint and composite holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fringe_persistence_scope, empirical, 'Whether fringe dueling persistence undermines the composite decline claim.').

omega_variable(
    kernel_reading_indeterminacy,
    'Is the composite reading distinguished from its siblings by empirical scope, or by normative commitment to multicausal historical methodology?',
    'Historiographical analysis of whether composite and contraction readings are empirically separable or merely differently weighted interpretations of the same archival record.',
    'If methodologically indistinct, the kernel may not admit structurally distinct readings and the constraint family should be collapsed; if distinct, the epsilon values and stakeholder structures should diverge measurably across readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Whether the kernel readings are structurally distinct or merely weighted variants.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_settlement_legitimacy__composite_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t0, honor_settlement_legitimacy__composite_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(hono_tr_t20, honor_settlement_legitimacy__composite_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(hono_tr_t40, honor_settlement_legitimacy__composite_reading, theater_ratio, 40, 0.45).
narrative_ontology:measurement(hono_tr_t60, honor_settlement_legitimacy__composite_reading, theater_ratio, 60, 0.6).
narrative_ontology:measurement(hono_tr_t80, honor_settlement_legitimacy__composite_reading, theater_ratio, 80, 0.72).
narrative_ontology:measurement(hono_tr_t100, honor_settlement_legitimacy__composite_reading, theater_ratio, 100, 0.85).

% Extraction over time
narrative_ontology:measurement(hono_be_t0, honor_settlement_legitimacy__composite_reading, base_extractiveness, 0, 0.8).
narrative_ontology:measurement(hono_be_t20, honor_settlement_legitimacy__composite_reading, base_extractiveness, 20, 0.72).
narrative_ontology:measurement(hono_be_t40, honor_settlement_legitimacy__composite_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(hono_be_t60, honor_settlement_legitimacy__composite_reading, base_extractiveness, 60, 0.42).
narrative_ontology:measurement(hono_be_t80, honor_settlement_legitimacy__composite_reading, base_extractiveness, 80, 0.28).
narrative_ontology:measurement(hono_be_t100, honor_settlement_legitimacy__composite_reading, base_extractiveness, 100, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t0, honor_settlement_legitimacy__composite_reading, suppression_requirement, 0, 0.88).
narrative_ontology:measurement(hono_su_t20, honor_settlement_legitimacy__composite_reading, suppression_requirement, 20, 0.8).
narrative_ontology:measurement(hono_su_t40, honor_settlement_legitimacy__composite_reading, suppression_requirement, 40, 0.68).
narrative_ontology:measurement(hono_su_t60, honor_settlement_legitimacy__composite_reading, suppression_requirement, 60, 0.52).
narrative_ontology:measurement(hono_su_t80, honor_settlement_legitimacy__composite_reading, suppression_requirement, 80, 0.35).
narrative_ontology:measurement(hono_su_t100, honor_settlement_legitimacy__composite_reading, suppression_requirement, 100, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_settlement_legitimacy__composite_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__composite_reading, honor_settlement_legitimacy__contraction_reading).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__composite_reading, honor_settlement_legitimacy__drop_reading).

% DUAL FORMULATION NOTE:
% This constraint is the composite_reading of the honor_settlement_legitimacy kernel. The kernel conflates three structurally distinct historiographical claims about dueling's decline: composite (multicausal overdetermination), contraction (pure cultural unthinkability), and drop (fringe persistence). Each reading instantiates a different constraint with different epsilon, stakeholder structure, and classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
