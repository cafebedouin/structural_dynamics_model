% ============================================================================
% CONSTRAINT STORY: balfour_mandate_instruments__mandatory_interpretive_discretion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_balfour_mandate_instruments__mandatory_interpretive_discretion, []).

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
 *   constraint_id: balfour_mandate_instruments__mandatory_interpretive_discretion
 *   human_readable: British Mandatory Power Interpretive Discretion Over Palestine Mandate
 *   domain: international_law/colonial_administration
 *
 * SUMMARY:
 *   The British mandatory power over Palestine (1920-1948) operated under a
 *   League of Nations mandate that required balancing two incompatible
 *   obligations: facilitating a 'national home' for the Jewish people and
 *   protecting the rights of existing Arab communities. This constraint story
 *   models the operational mechanism that resolved this contradiction in
 *   practice: British interpretive discretion over the mandate text,
 *   exercised without external review or binding appeal. The mandate provided
 *   no hierarchy between its conflicting clauses and no mechanism for
 *   international arbitration of disputes about their meaning. Both Arab
 *   Palestinian and Zionist Jewish communities faced strategic uncertainty as
 *   British policy oscillated across successive reinterpretations (1920 land
 *   regime → 1922 Churchill White Paper limiting immigration → 1930 Passfield
 *   White Paper → 1939 White Paper sharply restricting Jewish land purchase
 *   and immigration). Each shift was presented as a legitimate reading of the
 *   same mandate text; neither community could anchor claims to fixed meaning
 *   or invoke external arbitration. This constraint is ONE reading of the
 *   contested kernel 'balfour_mandate_instruments': it models interpretive
 *   discretion itself as the system, not as a means to coordinate between
 *   readings. The sibling readings (jewish_national_home_primacy,
 *   dual_obligation_indigenous_rights) model the substantive outcomes each
 *   community expected; this reading models the mechanism that allowed
 *   Britain to deliver different outcomes at different times while
 *   maintaining legal authority.
 *
 * KEY AGENTS:
 *   - British colonial administrators: institutional power, agenda-setter, beneficiary from policy flexibility and divide-and-rule strategy
 *   - Arab Palestinian communities: organized power, trapped exit, victims of policy shifts that progressively undermine self-governance expectations
 *   - Zionist Jewish communities: moderate-to-powerful, trapped exit, victims of policy restraint despite apparent mandate alignment (1939 White Paper)
 *   - League of Nations: institutional observer, analytically positioned but enforcement-capacity absent
 *   - Alternative interpretive authorities: excluded by design—cannot adjudicate their own claims about mandate meaning
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(balfour_mandate_instruments__mandatory_interpretive_discretion, 0.68).
domain_priors:suppression_score(balfour_mandate_instruments__mandatory_interpretive_discretion, 0.72).
domain_priors:theater_ratio(balfour_mandate_instruments__mandatory_interpretive_discretion, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, extractiveness, 0.68).
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(balfour_mandate_instruments__mandatory_interpretive_discretion, snare).
narrative_ontology:human_readable(balfour_mandate_instruments__mandatory_interpretive_discretion, "British Mandatory Power Interpretive Discretion Over Palestine Mandate").
narrative_ontology:topic_domain(balfour_mandate_instruments__mandatory_interpretive_discretion, "international_law/colonial_administration").

domain_priors:requires_active_enforcement(balfour_mandate_instruments__mandatory_interpretive_discretion).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(balfour_mandate_instruments__mandatory_interpretive_discretion, '2e804b7e-c7c9-43e2-9fb4-a5c509d303d5').
narrative_ontology:cs_kernel_codification('2e804b7e-c7c9-43e2-9fb4-a5c509d303d5', fixed_text).
narrative_ontology:cs_authority_grounding('2e804b7e-c7c9-43e2-9fb4-a5c509d303d5', extraction).
narrative_ontology:cs_reading_relation('2e804b7e-c7c9-43e2-9fb4-a5c509d303d5', balfour_mandate_instruments__jewish_national_home_primacy, influences).
narrative_ontology:cs_reading_relation('2e804b7e-c7c9-43e2-9fb4-a5c509d303d5', balfour_mandate_instruments__dual_obligation_indigenous_rights, influences).
narrative_ontology:cs_axiom('2e804b7e-c7c9-43e2-9fb4-a5c509d303d5', foundational, mandatory_power_unilateral_interpretation_authority).
narrative_ontology:cs_axiom_status(mandatory_power_unilateral_interpretation_authority, holdable).
narrative_ontology:cs_axiom_grounding('2e804b7e-c7c9-43e2-9fb4-a5c509d303d5', mandatory_power_unilateral_interpretation_authority, conventional).
narrative_ontology:cs_axiom('2e804b7e-c7c9-43e2-9fb4-a5c509d303d5', foundational, mandate_text_internal_contradiction_allows_discretion).
narrative_ontology:cs_axiom_status(mandate_text_internal_contradiction_allows_discretion, holdable).
narrative_ontology:cs_axiom_grounding('2e804b7e-c7c9-43e2-9fb4-a5c509d303d5', mandate_text_internal_contradiction_allows_discretion, empirically_contingent).
narrative_ontology:cs_reference_frame('2e804b7e-c7c9-43e2-9fb4-a5c509d303d5', league_mandate_unilateral_authority_framework).
narrative_ontology:cs_drift_state('2e804b7e-c7c9-43e2-9fb4-a5c509d303d5', mandate_termination_1948, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('2e804b7e-c7c9-43e2-9fb4-a5c509d303d5', '').
narrative_ontology:cs_kernel_id(balfour_mandate_instruments__mandatory_interpretive_discretion, balfour_mandate_instruments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__mandatory_interpretive_discretion, british_colonial_administrators).
narrative_ontology:constraint_victim(balfour_mandate_instruments__mandatory_interpretive_discretion, arab_palestinian_communities).
narrative_ontology:constraint_victim(balfour_mandate_instruments__mandatory_interpretive_discretion, zionist_jewish_communities).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(balfour_mandate_instruments__mandatory_interpretive_discretion, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(balfour_mandate_instruments__mandatory_interpretive_discretion, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(balfour_mandate_instruments__mandatory_interpretive_discretion_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(balfour_mandate_instruments__mandatory_interpretive_discretion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(balfour_mandate_instruments__mandatory_interpretive_discretion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68 at interval end) is moderate-high because the constraint's operation systematically disadvantages both victim communities: neither can root their claims in the mandate text itself; both depend on British discretion. The measurement series shows a rise from 0.48 to 0.61 over the first 12 time-points (corresponding to 1920–1930, the period of escalating White Papers and contradictory policy), then plateau at ~0.67–0.69. The plateau reflects a structural equilibrium: British discretion is now fully operationalized; no community can credibly appeal beyond it. Suppression (0.72) is high because maintaining this discretionary system requires active suppression of alternative interpretive authorities and appeals to external review. Both Arab and Jewish leadership petitioned the League; petitions were received and filed, but the League's advisory opinions had no enforcement power and British policy continued unreviewed. Theater (0.58) reflects the gap between the mandate's stated coordination purpose (balancing conflicting obligations) and its actual operation (British discretion resolving uncertainty in Britain's favor). The ratio rises to 0.58 by time-point 12 as the gap between written mandate and operational policy becomes obvious to both communities; it plateaus there because the performance of legitimate interpretation (publishing White Papers, citing mandate clauses, staging League consultations) becomes routinized rather than intensifying.
 *
 * PERSPECTIVAL GAP:
 *   From the British administrative seat, this is a rope: a coordination mechanism for managing an insoluble contradiction by granting expert discretion to a neutral arbiter (the framing Britain presented). From the Arab Palestinian seat and the Zionist Jewish seat, this is a snare: a mechanism that concentrates power to extract compliance and policy deference while precluding appeal. The engine's per-seat computation should differentiate: the beneficiary seat experiences low χ (favorable outcomes, policy flexibility, no accountability), while the victim seats experience high χ (strategic uncertainty, path-dependent lock-in, suppressed appeal mechanisms). The authored metrics (high suppression, moderate theater) reflect the victim-seat experience; the computed per-seat divergence is the measurement this story enables.
 *
 * DIRECTIONALITY LOGIC:
 *   British administrators are the beneficiary (d near 0.0–0.2): they collect policy flexibility, divide-and-rule advantages, and freedom from external review. Arab Palestinian communities are targets (d near 0.8–1.0): they face escalating policy shifts that undermine initial expectations and cannot appeal. Zionist Jewish communities are also targets (d near 0.7–0.9) despite nominal mandate alignment: the 1939 White Paper demonstrates that even aligned communities remain trapped by dependence on British discretion. The League of Nations observers have analytical exit (d = 0.5 by default, no strategic exposure). Alternative interpretive authorities (excluded stakeholders) are trapped by design—they cannot exit the system because they have no voice in it; their exclusion is the mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exhibits no mandatrophy: the founding problem (irresolvable contradiction in the mandate text) remains live and structurally unresolved throughout the interval. The constraint does not become vestigial or performative; it is actively operationalized (suppression requirement rises and holds at 0.72). However, the constraint's persistence depends on a specific arrangement of power: British institutional dominance + League of Nations enforcement capacity absent + absence of alternative interpretive authorities. If external review mechanisms were activated (e.g., the League enforced advisory opinions) or if the communities jointly authored interpretations, this constraint's operational form would dissolve. It is NOT a piton (which persists by inertia); it is a live snare maintained by active suppression of alternatives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mandate_text_genuine_ambiguity,
    'Is the mandate text genuinely ambiguous (two incompatible but textually supportable readings), or is one reading the correct interpretation and the other a cover for extractive policy?',
    'Comparative analysis of mandate instruments for other League territories; historical documentation of drafters'' intent; textual analysis of competing clause hierarchies. A genuine ambiguity would show up in contemporary legal scholarship divided across professional consensus lines; a cover story would show post-hoc rationalization of shifting policy.',
    'If genuinely ambiguous, the constraint is more a rope-with-asymmetry (both readings textually viable, one party controls adjudication). If a cover story, the constraint is purely a snare (textual meaning is subordinated to institutional preference). The measured suppression and theater values suggest a cover story, but the ambiguity omega documents the alternative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_text_genuine_ambiguity, empirical, 'Whether mandate text ambiguity is structural or pretextual.').

omega_variable(
    british_discretion_scope_constraint,
    'How much of the measured extraction flows from British interpretive discretion itself, versus from the imbalance of power between Britain and the two communities? Could the same mandate text, adjudicated by a neutral third party, produce different outcomes?',
    'Counterfactual analysis: construct what an international arbitration of the mandate''s conflicting clauses would have produced, given the communities'' respective negotiating positions and the text''s structural ambiguities. Compare to actual British policy outcomes.',
    'If the discretion is the primary mechanism, then granting interpretive authority to an alternative (neutral, joint, or external) body would substantially alter the outcome. If power imbalance dominates (whichever arbiter controls the discretion will favor the most powerful party), then substituting the arbiter does not solve the underlying asymmetry.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(british_discretion_scope_constraint, conceptual, 'Whether interpretive mechanism or power imbalance is the primary extraction driver.').

omega_variable(
    sibling_reading_foreclosure,
    'Does this reading of interpretive discretion (mandatory_interpretive_discretion) foreclose the sibling readings (jewish_national_home_primacy, dual_obligation_indigenous_rights), or do all three readings coexist at different sites (Britain''s institutional framing, Zionist expectation, Arab expectation) without logical contradiction?',
    'Examination of the mandate text: does it logically permit all three readings simultaneously, or does one reading''s core premise rule out another''s? The reading_relations section of cs_structure carries the engine-facing declaration; this omega documents the textual basis for the choice.',
    'If the readings coexist (different parties held different readings without logical contradiction), the kernel is structurally indeterminate—a site of genuine pluralism where discretion permitted by textual ambiguity. If one forecloses another, the textual basis for that foreclosure should be named; the mandatory power''s discretion is then obscuring a settled reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Logical compatibility of the three kernel readings.').

omega_variable(
    identity_lock_mechanism,
    'For both Arab Palestinian and Zionist Jewish communities, to what degree is their continued engagement with the mandate system an identity-locked exit (they cannot imagine withdrawing from the negotiation even when the terms disadvantage them), versus a genuinely constrained exit (material or legal barriers prevent withdrawal)?',
    'Comparative analysis of how each community behaved when the mandate system faced existential crises (the 1929 Hebron riots, the 1936-1939 Arab Revolt, the 1948 war). Did either community attempt to exit negotiation entirely and pursue alternatives? What prevented or enabled such exit?',
    'If the exit is primarily identity-locked (both communities claim historical rights to the territory that preclude exit even under adverse terms), the suppression is partially internalized—the communities carry it with them even if external structures were removed. If constrained (material vulnerability, isolation from allies), the suppression is structural and removal of the external constraint would enable exit. The measured suppression value (0.72) does not distinguish mechanism; the omega documents which operates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Identity-locked versus structurally constrained exit mechanisms.').

omega_variable(
    committer_reading_contention,
    'This reading models interpretive discretion itself as the constraint. The sibling readings model what each community expected the mandate to require. Are these readings capturing a genuine disagreement about mandate meaning (each reading is a coherent interpretation, and the disagreement is which interpretation is correct), or is this reading modeling a different category of constraint entirely—not about interpretation but about power to control interpretation?',
    'Textual analysis of mandate instruments and contemporaneous legal scholarship. If the sibling readings represent competing but structurally coherent legal interpretations, the readings are intra-kernel. If this reading models CONTROL OF INTERPRETATION rather than a substantive interpretation, it may be modeling a different constraint category (institutional power, not kernel reading).',
    'If this is a true kernel reading, it should coexist-with or influence the siblings. If it is modeling institutional control rather than interpretation, the relationship to the siblings is parasitic—this reading explains how Britain sustained contradictory positions without losing authority, but does not itself interpret the kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_reading_contention, conceptual, 'Whether this reading is a substantive interpretation of mandate meaning or a meta-level analysis of control of interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(balfour_mandate_instruments__mandatory_interpretive_discretion, 0, 28).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(balf_tr_t0, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 0, 0.42).
narrative_ontology:measurement(balf_tr_t4, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 4, 0.48).
narrative_ontology:measurement(balf_tr_t8, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 8, 0.54).
narrative_ontology:measurement(balf_tr_t12, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 12, 0.58).
narrative_ontology:measurement(balf_tr_t16, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 16, 0.59).
narrative_ontology:measurement(balf_tr_t20, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 20, 0.58).
narrative_ontology:measurement(balf_tr_t28, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 28, 0.58).

% Extraction over time
narrative_ontology:measurement(balf_be_t0, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(balf_be_t4, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 4, 0.54).
narrative_ontology:measurement(balf_be_t8, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 8, 0.61).
narrative_ontology:measurement(balf_be_t12, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 12, 0.66).
narrative_ontology:measurement(balf_be_t16, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 16, 0.67).
narrative_ontology:measurement(balf_be_t20, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 20, 0.69).
narrative_ontology:measurement(balf_be_t28, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 28, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(balf_su_t0, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 0, 0.54).
narrative_ontology:measurement(balf_su_t4, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 4, 0.6).
narrative_ontology:measurement(balf_su_t8, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 8, 0.66).
narrative_ontology:measurement(balf_su_t12, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 12, 0.7).
narrative_ontology:measurement(balf_su_t16, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 16, 0.71).
narrative_ontology:measurement(balf_su_t20, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 20, 0.72).
narrative_ontology:measurement(balf_su_t28, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 28, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(balfour_mandate_instruments__mandatory_interpretive_discretion, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(balfour_mandate_instruments__mandatory_interpretive_discretion, 0.18).
narrative_ontology:affects_constraint(balfour_mandate_instruments__mandatory_interpretive_discretion, balfour_mandate_instruments__jewish_national_home_primacy).
narrative_ontology:affects_constraint(balfour_mandate_instruments__mandatory_interpretive_discretion, balfour_mandate_instruments__dual_obligation_indigenous_rights).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'balfour_mandate_instruments' kernel. The jewish_national_home_primacy reading models the outcome preferred by Zionist communities (demographic/territorial transformation, Jewish institutional supremacy). The dual_obligation_indigenous_rights reading models the outcome preferred by Arab Palestinian communities (equal or superior protection of existing rights and self-determination). This reading (mandatory_interpretive_discretion) models the MECHANISM that allowed Britain to deliver different outcomes at different times while maintaining legal authority: unreviewed interpretive discretion over the mandate text. The three readings instantiate different ε values: primacy and dual_obligation are moderate-snare (substantive extraction of land/resources/political control), while mandatory_interpretive_discretion is a moderate-snare of a different mechanism (extraction of interpretive authority itself, which enables the substantive extractions the sibling readings model). The family structure: mandatory_interpretive_discretion influences both siblings because the mechanism of discretion determines which substantive interpretation can be operationalized at any moment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(balfour_mandate_instruments__mandatory_interpretive_discretion, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
