% ============================================================================
% CONSTRAINT STORY: ulysses_chp14
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ulysses_oxen_1904, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ulysses_chp14
 *   human_readable: The Gestation Mountain (Holles Street)
 *   domain: biological/linguistic/medical
 *
 * SUMMARY:
 *   Chapter 14 of Ulysses (the Oxen of the Sun episode) unfolds at the
 *   National Maternity Hospital (Holles Street, Dublin) where Mrs. Mina
 *   Purefoy labors and delivers a child. Joyce structures the chapter as a
 *   linguistic and stylistic recapitulation of gestation: nine subsections
 *   representing the nine months of pregnancy, each written in a different
 *   literary register and language, culminating in the delivery and
 *   parturition motif. The constraint is biological: the nine-month gestation
 *   cycle, the labor process, the inevitable delivery. But the constraint is
 *   also *structural to the narrative*: it organizes the literary experiment.
 *   The Gestation Mountain is thus both a natural law and a narrative
 *   principle. From the perspective of the laboring woman, it is an immutable
 *   biological process with no exit. From the perspective of the hospital, it
 *   is a natural phenomenon that medicine supports but does not create. From
 *   the perspective of the analytical embryologist, it is a cascade of
 *   developmental necessities with zero degrees of freedom. From the
 *   perspective of Joyce's narrative technique, it is a coordination
 *   mechanism that enables literary expression without asymmetric extraction
 *   — the constraint serves the narrative collaboration, not any single
 *   agent's benefit.
 *
 * KEY AGENTS:
 *   - Mrs. Mina Purefoy: Primary subject (powerless/trapped) — undergoes the biological process; no exit; maximum accessibility collapse
 *   - The Laboring Body: The constraint's locus (biological/embodied) — the physiological cascade of gestation and parturition
 *   - The Medical Practitioners (National Maternity Hospital): Observers and interventionists (institutional/arbitrage) — support the process; aligned with the constraint; no extraction
 *   - The Fetus/Offspring: Beneficiary of the gestation process (no agency) — develops through the constraint but not an agent in the Deferential Realism sense
 *   - The Narrative (Joyce's Chapter 14): The literary constraint (analytical/generational) — uses gestation as organizational scaffold; pure coordination, no extraction
 *   - The Analytical Observer: Civilizational perspective (analytical/analytical) — recognizes the constraint as a universal natural law of mammalian reproduction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ulysses_chp14, 0.18).
domain_priors:suppression_score(ulysses_chp14, 0.04).
domain_priors:theater_ratio(ulysses_chp14, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ulysses_chp14, extractiveness, 0.18).
narrative_ontology:constraint_metric(ulysses_chp14, suppression_requirement, 0.04).
narrative_ontology:constraint_metric(ulysses_chp14, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ulysses_chp14, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(ulysses_chp14, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ulysses_chp14, mountain).
narrative_ontology:human_readable(ulysses_chp14, "The Gestation Mountain (Holles Street)").
narrative_ontology:topic_domain(ulysses_chp14, "biological/linguistic/medical").

domain_priors:emerges_naturally(ulysses_chp14).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE LABORING WOMAN (MOUNTAIN) — Parturition is a biological limit. The woman's body enacts the gestation constraint with no alternative pathway. The process is embedded in mammalian physiology — an irreducible natural law. Nine months of gestation, hours of labor, delivery as a threshold event. No exit, no negotiation, no coordination mechanism can eliminate the fundamental temporal and physical requirements. Maximum accessibility collapse (biological clock cannot be circumvented) and minimal resistance (the process is as inevitable as gravity).
constraint_indexing:constraint_classification(ulysses_chp14, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE MATERNITY HOSPITAL (MOUNTAIN) — From the institutional perspective, gestation is a natural process that the hospital observes, supports, and occasionally intervenes in — but does not create or extract from. The hospital exists because the constraint exists. The constraint is prior to any institutional arrangement. Labor support, antisepsis, anesthesia, emergency intervention: these are adaptations to an immutable biological reality, not negotiations with it. The hospital has some arbitrage (discretion over intervention protocols) but the underlying constraint (pregnancy duration, delivery as threshold) is invariant. Emergence is natural; resistance is low (the institution is aligned with the process, not opposing it).
constraint_indexing:constraint_classification(ulysses_chp14, mountain,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (MOUNTAIN) — Gestation is a mathematical and biological necessity. The developmental clock is encoded in embryonic induction cascades, hormonal signaling, and cell differentiation programs. The nine-month duration reflects gestational constraints: sufficient time for organ formation, CNS development, fetal viability. These are not cultural conventions or institutional arrangements — they are consequences of mammalian developmental biology. The constraint is universal across all human pregnancies and across species with similar gestational strategies. Complete accessibility collapse: there is no alternative framework for human reproduction. Zero degrees of freedom. This is a mountain by the strictest definition.
constraint_indexing:constraint_classification(ulysses_chp14, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: THE NARRATIVE/LINGUISTIC OBSERVER (ROPE) — From the perspective of Ulysses Chapter 14, gestation is a *narrative principle*, not merely a biological fact. Joyce uses the constraint to coordinate multiple narrative voices, literary styles, and thematic registers — one for each month of pregnancy, culminating in parturition as a stylistic threshold. The constraint here functions as *pure coordination*: it enables the literary structure without extracting from any agent. The gestation cycle is the scaffolding for narrative collaboration. This perspective sees the constraint as a Rope: generative, structuring, non-extractive. No suppression, no asymmetric benefit. The constraint organizes discourse.
constraint_indexing:constraint_classification(ulysses_chp14, rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 5: THE MEDICAL PRACTITIONER (MOUNTAIN) — The doctor at Holles Street encounters the gestation constraint as an immutable boundary condition. Pregnancy presents with a fixed timeline; labor follows predictable (though variable) stages; delivery is the point of maximal uncertainty and intervention. The practitioner has tactical discretion (forceps vs. expectant management, anesthesia protocols) but strategic powerlessness over the underlying constraint. The body's clock cannot be negotiated. The practitioner sees emergence_naturally = true: the process is prior to medicine. Accessibility collapse is high: there is no alternative biological pathway known to medicine. Resistance is low: medical practice aligns with, rather than opposes, the natural process.
constraint_indexing:constraint_classification(ulysses_chp14, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 6: THE EMBRYOLOGICAL OBSERVER (MOUNTAIN) — Gestation unfolds as a cascade of developmental checkpoints: implantation, gastrulation, organogenesis, CNS development, organ maturation. Each phase requires time; each has minimal tolerance for acceleration or compression. The embryological constraint is a mountain: the sequence is invariant, the timeline is fixed within narrow bounds, accessibility to alternative pathways is zero. No coordinating agent controls this process. It emerges from physical chemistry and cell biology. Suppression is zero (nothing external is suppressing the process). Extractiveness is zero (no agent is benefiting asymmetrically — the process serves the offspring). This is the purest form of mountain classification.
constraint_indexing:constraint_classification(ulysses_chp14, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ulysses_chp14_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ulysses_chp14, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ulysses_chp14, TypeOther, context(agent_power(analytical), _, _, _)),
    TypePowerless \= TypeOther.

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(ulysses_chp14, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ulysses_chp14, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(ulysses_chp14, ExtMetricName, E),
    domain_priors:suppression_score(ulysses_chp14, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(ulysses_chp14),
    narrative_ontology:constraint_metric(ulysses_chp14, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(ulysses_chp14, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(ulysses_chp14_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Very low. The gestation constraint is not extractive — it is a natural process. The coefficient reflects only that the process has directionality (toward the offspring), not that any agent is extracting from another. The biological flow favors fetal development; this is not extraction, it is process. Suppression (0.04): Minimal. The process faces no external suppression — it unfolds according to its own internal logic. Barriers exist (medical complications, environmental factors) but are not *suppressive* in the DR sense (coercive prevention of alternatives). Theater ratio (0.15): Very low. Gestation is not performative. The biological process is what it is; medical theater (hospital rituals, labor support protocols) exists *around* the constraint, not as a substitute for it. Claimed type: Mountain. The constraint meets all mountain thresholds: ε ≤ 0.25, suppression ≤ 0.05, emerges_naturally = true, accessibility_collapse ≥ 0.85, resistance ≤ 0.15.
 *
 * PERSPECTIVAL GAP:
 *   The gap between biological and narrative perspectives reflects the constraint's dual status. The biological mountain is invariant: gestation cannot be circumvented, redesigned, or negotiated. The narrative rope is a literary choice: Joyce's use of the gestation cycle as an organizational scaffold is contingent on the author's artistic decision, not on the biological constraint itself. Both perspectives are correct; they inhabit different observational domains (biology vs. literary technique). The engine's classification will register all perspectives as mountain except the narrative observer's rope, revealing that the constraint's structure is invariant in nature but available as a narrative resource.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) in the gestation constraint is unusual: there is no asymmetric extraction in the economic or political sense. The beneficiary (the developing fetus/offspring) has no agency and bears no alternative costs. The pregnant person bears the biological cost but receives no compensatory benefit from the constraint itself — any emotional or social benefit comes from external value systems, not from the gestation structure. The constraint is not extractive; it is procreative. Directionality flows toward the offspring, but this is biological necessity, not institutional leverage. All perspectives derive d from the recognition that this is a natural law, not a constructed scheme, so d-values are baseline: powerless agents see maximum experienced constraint (but not as extraction), institutional agents see alignment (arbitrage is compatibility with the natural process), analytical agents see structural inevitability (d = analytical standard).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy here is resolved by recognizing that gestation is NOT a coordination problem masquerading as a natural law, nor a natural law masquerading as coordination. It is genuinely both: a natural law (from the biological perspective) and a coordination resource (from the narrative perspective). The mandatrophy question — 'Is this a mountain being called a rope, or a rope being called a mountain?' — has a negative answer: it is neither confusion. The constraint is a true mountain when evaluated biologically; it is a true rope when evaluated narratively. The perspectival difference does not indicate misclassification but rather highlights that the constraint inhabits multiple structural domains simultaneously. The literary use of gestation (Rope) is *downstream of* the biological constraint (Mountain), not competitive with it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technological_circumvention_horizon,
    'Could artificial gestation (ectogenesis) eliminate the biological constraint, reclassifying it from mountain to constructed protocol?',
    'Longitudinal tracking of ectogenesis technology development; assessment of whether fetal development outside the womb can replicate all constraints of in-utero gestation or whether new constraints (nutrient delivery, immune tolerance, neuro-sensory signaling) are discovered to be equally immutable.',
    'If ectogenesis becomes functionally equivalent: the constraint shifts from natural law to engineered system. Classification becomes technology-dependent (mountain under biological reproduction, rope or tangled_rope under ectogenesis). If ectogenesis reveals deeper constraints: classification remains mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_circumvention_horizon, empirical, 'Whether ectogenesis technology can fully circumvent gestational timing constraints').

omega_variable(
    narrative_vs_biological_constraint_identity,
    'Is the ''gestation'' constraint in Chapter 14 identical to the biological constraint, or does the literary constraint decompose into a separate constraint with different structural properties?',
    'Structural analysis of whether Joyce''s nine-month narrative cycle tracks the biological pregnancy duration and delivery, or whether the literary structure could be instantiated with different biological timings. Comparison with other literary works using cyclical/temporal constraints.',
    'If identical: single constraint (mountain from biological perspectives, rope from narrative perspective). If decomposed: two distinct constraints (biological mountain, narrative/coordination rope) linked by network.affects_constraints. The ε-invariance principle suggests they should be separate if the literary constraint''s structure could survive alternative biological timings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(narrative_vs_biological_constraint_identity, conceptual, 'Whether narrative and biological constraints are structurally identical or decomposed').

omega_variable(
    labor_extraction_asymmetry,
    'Does the biological constraint of gestation inherently extract asymmetric benefit to the fetus/offspring versus cost to the pregnant person, or is this asymmetry a societal interpretation rather than structural?',
    'Analysis of metabolic cost distribution during pregnancy; assessment of whether the asymmetry persists in absence of social/institutional interpretation or is culturally constructed. Comparative analysis across reproductive systems (viviparous vs. oviparous).',
    'If inherent: mountain classification remains (asymmetry is not extraction, just directionality). If constructed: constraint might decompose into biological mountain (pregnancy as such) and social/institutional snare (extraction via enforced motherhood).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(labor_extraction_asymmetry, conceptual, 'Whether gestation''s cost-benefit asymmetry is inherent or socially constructed').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ulysses_chp14, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ulys_tr_t0, ulysses_chp14, theater_ratio, 0, 0.12).
narrative_ontology:measurement(ulys_tr_t4, ulysses_chp14, theater_ratio, 4, 0.14).
narrative_ontology:measurement(ulys_tr_t9, ulysses_chp14, theater_ratio, 9, 0.15).

% Extraction over time
narrative_ontology:measurement(ulys_be_t0, ulysses_chp14, base_extractiveness, 0, 0.16).
narrative_ontology:measurement(ulys_be_t4, ulysses_chp14, base_extractiveness, 4, 0.17).
narrative_ontology:measurement(ulys_be_t9, ulysses_chp14, base_extractiveness, 9, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ulysses_chp14, global_infrastructure).

% DUAL FORMULATION NOTE:
% The Gestation Mountain represents a natural law (biological constraint on reproduction) that is also available as a narrative/linguistic organizing principle in Joyce's Chapter 14. The biological constraint and the literary constraint are structurally linked but do not decompose into separate constraint stories under the ε-invariance principle: both have ε ≈ 0.18 (representing minimal extractiveness, pure processual directionality). The dual perspectives (mountain from biology, rope from narrative technique) are both valid readings of the same underlying constraint structure, not evidence of decomposition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
