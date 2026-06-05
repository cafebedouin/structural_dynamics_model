% ============================================================================
% CONSTRAINT STORY: dueling_disappearance_mechanism__institutional_displacement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dueling_disappearance_mechanism__institutional_displacement_reading, []).

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
 *   constraint_id: dueling_disappearance_mechanism__institutional_displacement_reading
 *   human_readable: Dueling as Dispute-Resolution Coordination (Institutional Displacement Reading)
 *   domain: legal_history/cultural_anthropology/institutional_sociology
 *
 * SUMMARY:
 *   This constraint story instantiates the institutional_displacement_reading
 *   of dueling's disappearance from Western legal practice. Under this
 *   reading, dueling was a coordination mechanism on dispute resolution that
 *   persisted as long as formal institutions (courts, banking infrastructure,
 *   libel law) did not provide adequate, accessible alternatives. As courts
 *   became more accessible, banking and commercial law emerged to handle
 *   credit disputes, and libel statutes created remedies for reputation
 *   harms, dueling lost its coordination function. Participants voluntarily
 *   shifted to institutional alternatives — not because honor-culture became
 *   unthinkable (the contraction_reading hypothesis), but because
 *   institutions solved the same coordination problem more efficiently.
 *   Dueling persists in this reading not as a sign of cultural failure but as
 *   available-but-disfavored option in institutional gaps: isolated
 *   communities with court access delays, military cohorts where
 *   institutional dispute resolution is awkward, aristocratic enclaves where
 *   institutional alternatives carry reputational cost. The constraint type
 *   is Rope throughout: a coordination mechanism on how disputes are
 *   resolved, with multiple competing solutions (dueling, courts, banking
 *   arbitration, libel litigation). Decline is substitution, not foreclosure.
 *
 * KEY AGENTS:
 *   - Honor-culture participants (gentry, military, merchants with reputation-dependent credit): Primary beneficiary (powerful/arbitrage) — dueling coordinates dispute resolution with low friction in pre-institutional periods; shift to institutional alternatives when available
 *   - Formalized institutions (courts, banking law, libel courts): Primary beneficiary (institutional/arbitrage) — gain coordination function and adoption once infrastructure is in place
 *   - Merchants relying on credit reputation: Moderate power (mobile exit) — benefit from both dueling (before banking) and institutional dispute resolution (after banking emerges); voluntary shifters
 *   - Frontier/isolated populations: Moderate power (constrained exit) — retain dueling longer because institutional access is genuinely scarce; not victims, but delayed adopters
 *   - Military establishments: Powerful (arbitrage) — maintain dueling code longer through institutional inertia; code persists as Piton (performative ritual) rather than functional coordination
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dueling_disappearance_mechanism__institutional_displacement_reading, 0.12).
domain_priors:suppression_score(dueling_disappearance_mechanism__institutional_displacement_reading, 0.08).
domain_priors:theater_ratio(dueling_disappearance_mechanism__institutional_displacement_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dueling_disappearance_mechanism__institutional_displacement_reading, rope).
narrative_ontology:human_readable(dueling_disappearance_mechanism__institutional_displacement_reading, "Dueling as Dispute-Resolution Coordination (Institutional Displacement Reading)").
narrative_ontology:topic_domain(dueling_disappearance_mechanism__institutional_displacement_reading, "legal_history/cultural_anthropology/institutional_sociology").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dueling_disappearance_mechanism__institutional_displacement_reading, '834c528b-f951-45af-9f91-f080375de2ef').
narrative_ontology:cs_kernel_codification('834c528b-f951-45af-9f91-f080375de2ef', distributed).
narrative_ontology:cs_authority_grounding('834c528b-f951-45af-9f91-f080375de2ef', distributed).
narrative_ontology:cs_reading_relation('834c528b-f951-45af-9f91-f080375de2ef', dueling_disappearance_mechanism__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('834c528b-f951-45af-9f91-f080375de2ef', dueling_disappearance_mechanism__overdetermined_composite_reading, influences).
narrative_ontology:cs_axiom('834c528b-f951-45af-9f91-f080375de2ef', foundational, dueling_as_functional_coordination).
narrative_ontology:cs_axiom_status(dueling_as_functional_coordination, holdable).
narrative_ontology:cs_axiom_grounding('834c528b-f951-45af-9f91-f080375de2ef', dueling_as_functional_coordination, empirically_contingent).
narrative_ontology:cs_axiom('834c528b-f951-45af-9f91-f080375de2ef', foundational, institutional_gap_persistence).
narrative_ontology:cs_axiom_status(institutional_gap_persistence, holdable).
narrative_ontology:cs_axiom_grounding('834c528b-f951-45af-9f91-f080375de2ef', institutional_gap_persistence, empirically_contingent).
narrative_ontology:cs_reference_frame('834c528b-f951-45af-9f91-f080375de2ef', honor_culture_functional_dispute_resolution).
narrative_ontology:cs_drift_state('834c528b-f951-45af-9f91-f080375de2ef', institutional_modernization_complete, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('834c528b-f951-45af-9f91-f080375de2ef', '').
narrative_ontology:cs_kernel_id(dueling_disappearance_mechanism__institutional_displacement_reading, dueling_disappearance_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__institutional_displacement_reading, honor_culture_participants).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__institutional_displacement_reading, merchants_outside_formal_credit).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__institutional_displacement_reading, gentry_without_court_access).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: REGIONAL GENTRY WITH ARBITRAGE (ROPE) — Before formalized court access and banking infrastructure, dueling coordinates honor-based dispute resolution with genuine benefit: it is available when formal courts are distant or unaffordable. Low suppression, low extraction, high coordination value. Arbitrage exit option reflects ability to use courts when they become accessible.
constraint_indexing:constraint_classification(dueling_disappearance_mechanism__institutional_displacement_reading, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 2: MERCHANT CLASS (ROPE, INSTITUTIONAL SHIFT) — As banking and commercial courts emerge, merchants shift voluntarily to institutional dispute resolution. Dueling remains available but loses coordination value — it no longer solves the actual problem (credit disputes, contract breaches) that merchants face. Mobile exit (can choose institutions over dueling) and no experienced extraction. The shift is Rope → Rope (coordination remains, but mechanism changes).
constraint_indexing:constraint_classification(dueling_disappearance_mechanism__institutional_displacement_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 3: RESIDUAL DUELING CODE (PITON) — By mid-to-late 19th century, dueling persists as a formal practice among military and aristocratic classes, but its functional value has been displaced by institutions. High theater ratio (ritual and formality without coordination necessity), low extractiveness (the practice maintains itself through cultural performance rather than genuine dispute resolution). Piton classification reflects institutional inertia — the code survives through honor-culture repetition despite alternatives making it functionally obsolete.
constraint_indexing:constraint_classification(dueling_disappearance_mechanism__institutional_displacement_reading, piton,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: INSTITUTIONAL FRAMEWORK (ROPE) — Courts, banking law, libel litigation, and commercial arbitration solve the coordination problem that dueling previously addressed: how to resolve disputes of honor, property, and reputation when informal mechanisms fail. These institutions benefit from adoption (lower transaction costs, predictable outcomes, scalability) and coordinate rational actors. Low extraction, high coordination. This is not a Snare — the institutions deliver genuine problem-solving that dueling also delivered, but with lower cost and broader applicability.
constraint_indexing:constraint_classification(dueling_disappearance_mechanism__institutional_displacement_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: LEGAL PROHIBITION (SCAFFOLD) — Formal prohibition of dueling in law codes (e.g., Prussian criminal code, U.S. state legislation) creates an explicit sunset: dueling becomes illegal, not merely disfavored. This is Scaffold because it has a formal enforcement sunset clause (the legal prohibition itself) and a genuine coordination function (codifying the choice of institutional alternatives). Low extractiveness, explicit sunset mechanism.
constraint_indexing:constraint_classification(dueling_disappearance_mechanism__institutional_displacement_reading, scaffold,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (ROPE) — From a civilizational perspective, this reading treats dueling as a coordination mechanism on dispute resolution that declined through institutional substitution, not cultural foreclosure. Courts, banking law, and libel statutes solved the same coordination problem (how to handle disputes of honor and property) with lower friction. Dueling was never inherently unthinkable — it remains available in institutional gaps (e.g., isolated communities, military cultures in certain periods) and resurfaces when institutions fail. The analytical observer sees Rope throughout: coordination mechanism → institutional substitution → Rope (new mechanism).
constraint_indexing:constraint_classification(dueling_disappearance_mechanism__institutional_displacement_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dueling_disappearance_mechanism__institutional_displacement_reading_tests).

test(piton_threshold) :-
    domain_priors:theater_ratio(dueling_disappearance_mechanism__institutional_displacement_reading, TR),
    TR >= 0.70.

:- end_tests(dueling_disappearance_mechanism__institutional_displacement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Very low. This reading assumes dueling was a genuine coordination mechanism that solved a real problem (dispute resolution in honor cultures). Low extraction because both duelers benefit from the coordination (compared to lawlessness or private vendetta), and both choose the mechanism voluntarily. The mechanism is not exploitative — it is consensual and reciprocal. Suppression (0.08): Very low. Participants are not trapped; they voluntarily shift to alternatives when institutions emerge. No barriers prevent exit except time lag (institutional access must exist before it can substitute). Theater ratio (0.35, rising to 0.60): Moderate initially, rising over time. In early periods, dueling is substantially functional (low theater) — it genuinely resolves disputes that no institution handles. As institutions mature and replace dueling's function, dueling becomes increasingly performative — the ritual persists (high theater by t100) but the coordination necessity declines. The residual dueling code among military and aristocratic classes is mostly performative (Piton perspective). This measured rise in theater ratio as the mechanism declines reflects the Goodhart drift: the practice persists through cultural inertia after the function it solved has been replaced.
 *
 * PERSPECTIVAL GAP:
 *   The institutional_displacement_reading generates a perspectival gap between early-stage beneficiaries (gentry with dueling access and no institutional alternatives) and late-stage observers (legal historians who see dueling as culturally obsolete). However, this is not a Snare-vs-Rope gap (beneficiary vs victim). It is a gap between rational actors in different institutional contexts: those in institutional gaps rationally prefer dueling; those with institutional access rationally prefer courts. The Piton perspective (military code keepers) shows the temporal gap: dueling persists as performative ritual after its function is solved, maintained through cultural inertia rather than coordination necessity. This gap is diagnostic of the reading: if dueling persisted because the cultural axiom of honor became unthinkable, it would disappear entirely. If dueling persisted because institutional alternatives were unavailable, it would persist in institutional gaps. The empirical residue should show institutional-gap persistence, not universal cultural foreclosure.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality varies by perspective and time period. Early in the interval (t0), dueling participants experience low extraction (d ≈ 0.15) because they genuinely benefit from the coordination mechanism and face no systemic barriers — it is Rope. As institutions emerge (t50), merchants and accessible populations shift voluntarily to institutional alternatives (d rises slightly as coordination necessity declines, but remains Rope because shift is voluntary). Late in the interval (t100), residual dueling among military and aristocratic cohorts experiences higher theater and slight extraction creep (d ≈ 0.25-0.30) as the practice becomes performative — it is Piton rather than Rope. The analytical observer (d ≈ 0.50, institutional context) sees the entire trajectory as institutional competition: Rope vs Rope vs Piton, not Rope vs Snare. This reading explicitly rejects the victim framing: there are no victims of the institutional displacement, only voluntary adopters of superior coordination mechanisms.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading avoids mandatrophy by maintaining Rope classification throughout. The constraint is a coordination mechanism (dueling) that is out-competed by alternative mechanisms (institutions), not a mechanism that extractively locks participants. The Piton perspective (residual dueling code) shows degradation but not extraction — the practice persists through inertia and cultural performance, not because anyone benefits from the extraction. This reading's ε (0.12) reflects pure coordination: low extraction, low suppression, no victim set. The measurement trajectory shows theater rising as function declines — characteristic of Piton emergence from Rope — but the mechanism type does not degrade to Snare because there is no extraction mechanism sustaining it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    institutional_sufficiency_threshold,
    'At what level of institutional access does dueling cease to provide a coordination benefit? Does threshold differ by geographic region, social class, or dispute type?',
    'Historical mapping of court availability, accessibility costs, and procedural duration against dueling frequency by region and time period; correlation of institutional access expansion with dueling decline rates',
    'If threshold is low (institutional access alone drives decline): institutional_displacement_reading is strongly supported. If threshold is high or region-dependent: contraction_reading (cultural shift) may be better explanation for persistent variation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_sufficiency_threshold, empirical, 'Institutional access threshold for dueling decline').

omega_variable(
    dueling_persistence_in_institutional_gaps,
    'Does dueling persist in populations with low institutional access (frontier communities, military cohorts, isolated gentry) longer than in urbanized, court-accessible populations?',
    'Longitudinal comparison of dueling frequency in institutional-access-varied populations (colonial America vs European cities, military vs civilian, frontier vs settled); time-lagged analysis of court establishment and dueling decline',
    'If gap persistence is strong: supports institutional_displacement_reading. If persistence is weak or independent of institutional access: suggests cultural/legal factors dominate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dueling_persistence_in_institutional_gaps, empirical, 'Whether dueling persists longer in institutional gaps').

omega_variable(
    institutional_substitution_completeness,
    'Do formalized institutions (courts, banking, libel law) completely substitute dueling''s coordination function, or do institutional alternatives leave some dispute categories unresolved?',
    'Analysis of dispute types that dueling addressed (honor, property, reputation, credit) and which institutions now handle them; identification of any residual dispute categories for which no institution provides clear jurisdiction or remedy',
    'If substitution is complete: dueling decline is predictable from institutional availability alone. If gaps remain: dueling may persist as a functional mechanism in those categories, supporting overdetermined_composite_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_substitution_completeness, empirical, 'Completeness of institutional substitution for dueling').

omega_variable(
    kernel_reading_contest_framing,
    'Is dueling''s decline best explained as institutional substitution (institutional_displacement_reading), cultural axiom change (contraction_reading), or overdetermined causation (composite_reading)?',
    'This omega documents the contestation within the kernel. Institutional_displacement_reading assumes dueling was a functional coordination mechanism that competitors (courts, banking, libel law) out-competed. Contraction_reading assumes honor-culture axioms became unthinkable. Composite_reading assumes all three acted. Empirical resolution depends on whether dueling persists in institutional gaps (supports displacement), whether cultural meaning-shift precedes institutional adoption (supports contraction), or whether causation is genuinely independent (supports composite).',
    'This reading''s ε (0.12) and claimed_type (rope) depend on the assumption that dueling was primarily a coordination mechanism. If contraction_reading is correct, dueling is better modeled as snare or piton (cultural lock, degraded practice). If composite_reading is correct, multiple constraints should be decomposed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest_framing, conceptual, 'Kernel contest: which reading best explains dueling''s decline?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dueling_disappearance_mechanism__institutional_displacement_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dueling_disp_theater_t0, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(dueling_disp_theater_t50, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 50, 0.35).
narrative_ontology:measurement(dueling_disp_theater_t100, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 100, 0.6).

% Extraction over time
narrative_ontology:measurement(dueling_disp_extract_t0, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(dueling_disp_extract_t50, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 50, 0.1).
narrative_ontology:measurement(dueling_disp_extract_t100, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 100, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dueling_disappearance_mechanism__institutional_displacement_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__institutional_displacement_reading, dueling_disappearance_mechanism__contraction_reading).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__institutional_displacement_reading, dueling_disappearance_mechanism__overdetermined_composite_reading).

% DUAL FORMULATION NOTE:
% Dueling's historical decline is contested across three incompatible constraint readings: institutional_displacement_reading (this story, ε=0.12, Rope), contraction_reading (cultural axiom shift, expected ε=0.25-0.35, Piton or Snare), and overdetermined_composite_reading (multiple independent mechanisms, expected ε=0.35-0.50, Tangled Rope). Each reading models the same historical phenomenon (dueling's decline) as a structurally distinct constraint with different ε, different beneficiary/victim structure, and different terminal classification. The three stories are siblings (linked via network.affects_constraints), not observations of a single constraint from different angles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
