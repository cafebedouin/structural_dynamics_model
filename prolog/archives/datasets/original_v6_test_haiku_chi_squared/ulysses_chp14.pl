% ============================================================================
% CONSTRAINT STORY: ulysses_chp14
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ulysses_chp14, []).

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
 *   Chapter 14 of Ulysses ('Oxen of the Sun') stages the 'proliferent
 *   continuance' of human life at the National Maternity Hospital on Holles
 *   Street, Dublin. The constraint is biological gestation and parturition —
 *   the nine-month development cycle of human reproduction and the inevitable
 *   event of labor. This constraint is a mountain: it emerges naturally from
 *   developmental biology, appears across all human societies without
 *   variation, and admits no alternatives or exits. The chapter's linguistic
 *   baroque (Joyce's style evolving through historical phases of English
 *   prose) mirrors the biological constraint by modeling how language itself
 *   'develops' and 'matures.' Yet the two dimensions — biological development
 *   and linguistic development — are structurally distinct constraints,
 *   conflated in the narrative but separable analytically. The base
 *   properties reflect the biological constraint: extractiveness (0.12) is
 *   minimal because no agent 'extracts' from gestation itself; it simply
 *   occurs. Suppression (0.02) is negligible because there are no alternative
 *   reproductive pathways to suppress. Theater ratio (0.15, rising to 0.15 by
 *   end) reflects that childbirth is inherently functional — the biological
 *   process leaves little room for performative overlay, though institutional
 *   context (Holles Street) may introduce theatrical elements not inherent to
 *   biology itself. The mountain classification is stable across all four
 *   perspectives because the biological constraint is invariant: a laboring
 *   woman, an obstetrician, a maternity institution, and a civilizational
 *   observer all confront the same irreducible timeline and process.
 *
 * KEY AGENTS:
 *   - The Laboring Woman: Primary locus of the constraint (powerless/trapped) — experiencing gestation and parturition as biological necessity
 *   - The Obstetrician: Institutional facilitator (moderate/trapped) — constrained to work within biological limits despite medical knowledge
 *   - Holles Street Maternity Hospital: Institutional container (institutional/analytical) — structured entirely around accepting and managing biological reproduction
 *   - The Analytical Observer (Joyce): Civilizational view (analytical/analytical) — observing how biological constraint maps onto linguistic and literary form
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ulysses_chp14, 0.12).
domain_priors:suppression_score(ulysses_chp14, 0.02).
domain_priors:theater_ratio(ulysses_chp14, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ulysses_chp14, extractiveness, 0.12).
narrative_ontology:constraint_metric(ulysses_chp14, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(ulysses_chp14, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ulysses_chp14, accessibility_collapse, 0.92).
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

% PERSPECTIVE 1: THE LABORING WOMAN (MOUNTAIN) — Childbirth is an inescapable biological event. The pregnant body's nine-month gestation and parturition are constraint-bound processes — no exit options exist. The woman is the locus of the constraint itself. The physical/biological necessity of gestation transcends all social contexts. d≈0.90, but f(d) applies only to extraction mechanisms, not to natural law itself. The mountain classification holds independent of observer position.
constraint_indexing:constraint_classification(ulysses_chp14, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE MEDICAL ATTENDANT (MOUNTAIN) — Regardless of skill, knowledge, or institutional authority, the obstetrician must work within the biological constraints of human reproduction. Labor patterns, fetal development, maternal physiology follow laws that cannot be transcended by medical technique alone. The attendant is constrained by the same natural law, though positioned as facilitator rather than experiencer. d≈0.70 (observer of but not primary subject), but the mountain persists.
constraint_indexing:constraint_classification(ulysses_chp14, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: THE OBSTETRICAL INSTITUTION (MOUNTAIN) — Holles Street maternity hospital exists to manage and support biological reproduction, not to escape its constraints. The institution's entire function is predicated on accepting the immutability of gestation, labor, and parturition. From the civilizational view, the constraint appears as an unchanging structure of biological reality across all human societies and across history. Theater_ratio is low (0.15) because the biological processes are intrinsically functional, not performative.
constraint_indexing:constraint_classification(ulysses_chp14, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (MOUNTAIN) — From the civilizational and universal scope, human gestation is a biological constant: 280 days of fetal development, followed by labor, followed by parturition. This timeline is a feature of mammalian reproduction, not a cultural or institutional variable. The constraint emerges naturally from developmental biology and has appeared in every human society without exception or meaningful variation. Extractiveness ≤0.12 reflects that no agent 'extracts' from the biological process itself — it simply occurs. Suppression ≤0.02 because there are no alternatives to suppress; the constraint is not constraining between options but rather constitutive of a single necessary process.
constraint_indexing:constraint_classification(ulysses_chp14, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ulysses_chp14_tests).

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
 *   Extractiveness (0.12): Gestation is a biological process, not a mechanism of extraction. No agent 'profits' from pregnancy in the sense that one entity's gain corresponds to another's loss. The value 0.12 (not zero) reflects minimal institutional overhead: Holles Street hospital extracts a small amount of resources and authority in managing the biological process, but this is not inherent to the biological constraint itself. The biological constraint alone has extractiveness ≈ 0.05; institutional mediation adds marginal overhead. Suppression (0.02): There are no alternative reproductive pathways to suppress. The constraint is not 'suppressing' competing options; it is defining a single necessary process. Value 0.02 reflects measurement noise and minor variations in individual physiology, but no meaningful suppression of alternatives. Theater ratio (0.15, stable): Childbirth is intrinsically functional. Institutional context (hospital protocols, medical supervision) introduces some performative elements, but the core biological process — labor and delivery — cannot be substantially theatrical. The ratio increases slightly from 0.08 to 0.15 over the gestation period, reflecting increasing institutional presence and documentation as birth approaches, but remains low because the biological process drives the institutional response, not vice versa. Accessibility collapse (0.92): Human gestation has a fixed duration (~280 days) and a mandatory endpoint (parturition). No technology currently exists to significantly alter this timeline or eliminate the process. 0.92 represents high confidence that alternatives are inaccessible, but not absolute (future ectogenesis might change this). Resistance (0.08): The constraint is not resisted because it is not experienced as coercive. The laboring woman does not resist pregnancy as such; the biological constraint is accepted as a necessary condition of human reproduction. 0.08 reflects minor physiological variations and psychological resistance in some cases, but the constraint is generally accepted as natural. Claimed type (Mountain): The structural properties satisfy all mountain gates: extractiveness ≤ 0.25, suppression ≤ 0.05, emerges_naturally=true, accessibility_collapse ≥ 0.85, resistance ≤ 0.15. The constraint meets every criterion for natural law classification.
 *
 * PERSPECTIVAL GAP:
 *   Remarkably, this constraint shows NO perspectival gap. All four perspectives classify as Mountain. The laboring woman experiences gestation as inescapable (Mountain). The obstetrician must work within biological limits (Mountain). The institution is defined by accepting the biological constraint (Mountain). The analytical observer sees gestation as a civilizational constant (Mountain). This uniformity is a signature of true natural law: the constraint's classification is invariant across all possible observation positions because the underlying structure is invariant. The absence of perspectival gap is evidence of the mountain classification's validity. If perspectives diverged — e.g., if the beneficiary saw Rope while the victim saw Snare — this would indicate a false summit: the constraint would be institutional or cultural, not natural.
 *
 * DIRECTIONALITY LOGIC:
 *   Because this is a mountain constraint, directionality (d) and effective extraction (χ) are not primary analytical tools. The constraint does not 'extract' in the sense of transferring resources asymmetrically from one agent to another. Instead, it is a constitutive constraint: all agents are subject to the same biological process. The laboring woman is the locus of the constraint (d≈0.90, but this measures her centrality to the process, not her victimization by extraction). The obstetrician observes from outside (d≈0.70). The institution manages but does not escape (d≈0.05 as institutional beneficiary, but this small value reflects that the institution has no real 'escape route' either — it exists to serve the constraint, not to profit from it). All directionality values are secondary to the mountain classification; they do not override or modify the natural law structure.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    linguistic_vs_biological_constraint,
    'Is Joyce''s ''Oxen of the Sun'' primarily modeling a biological constraint (gestation) or a linguistic constraint (the development of English language and literary form)?',
    'Structural analysis of the constraint''s extractiveness across biological, linguistic, and narratological domains. Does the constraint derive its force from biology or from Joyce''s literary project?',
    'If primarily biological: mountain classification holds universally. If primarily linguistic: extractiveness rises to 0.30-0.40 (literary choice, not natural law), and the constraint may decompose into separate stories for biological gestation vs literary gestation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(linguistic_vs_biological_constraint, conceptual, 'Whether the constraint is fundamentally biological or literary-linguistic in nature').

omega_variable(
    accessibility_collapse_measurement,
    'What constitutes ''accessibility collapse'' (≥0.85) for a biological process? How do we measure the impossibility of altering fundamental reproductive biology?',
    'Historical analysis of attempted technological interventions (artificial gestation, parthenogenesis, developmental acceleration). Quantification of how many alternative pathways to human reproduction exist. Current accessibility_collapse=0.92 assumes near-zero alternatives; this is empirically robust for biological timelines but depends on technological boundaries.',
    'If future technology enables truly alternative reproductive pathways (ectogenesis, accelerated development): accessibility_collapse drops below 0.85, and the constraint may shift from Mountain to Rope or Scaffold.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(accessibility_collapse_measurement, empirical, 'Measurement of accessibility collapse for biological reproduction constraints').

omega_variable(
    naturalization_vs_institutional_mediation,
    'Is the constraint ''natural law'' (biology) or does institutional presence (Holles Street hospital) constitute a secondary constraint layer that extracts from the biological process?',
    'Comparative analysis of childbirth outcomes, extraction of resources, and institutional narratives across contexts: home birth, hospital birth, medicalized vs midwife-led care. Does institutional mediation add extractiveness above the base biological constraint?',
    'If institutional: two separate constraints should be decomposed — biological gestation (Mountain) and medical gatekeeping (Tangled Rope or Snare). If purely natural: single mountain story. Current story treats institutional context as transparent to the biological constraint; this may be a false naturalization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalization_vs_institutional_mediation, conceptual, 'Whether the constraint is purely natural or mediated by institutional extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ulysses_chp14, 0, 280).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ulysses_tr_t0, ulysses_chp14, theater_ratio, 0, 0.08).
narrative_ontology:measurement(ulysses_tr_t140, ulysses_chp14, theater_ratio, 140, 0.12).
narrative_ontology:measurement(ulysses_tr_t280, ulysses_chp14, theater_ratio, 280, 0.15).

% Extraction over time
narrative_ontology:measurement(ulysses_be_t0, ulysses_chp14, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(ulysses_be_t140, ulysses_chp14, base_extractiveness, 140, 0.12).
narrative_ontology:measurement(ulysses_be_t280, ulysses_chp14, base_extractiveness, 280, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ulysses_chp14, global_infrastructure).

% DUAL FORMULATION NOTE:
% The Gestation Mountain may decompose into two separate constraints if institutional extraction is analyzed separately from biological development. Future story: 'Holles Street Mediation' (ε≈0.30, Tangled Rope) modeling institutional gatekeeping of biological reproduction. Current story treats institutional context as transparent and focuses solely on the biological constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
