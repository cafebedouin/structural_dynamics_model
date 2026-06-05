% ============================================================================
% CONSTRAINT STORY: castration_longevity_choice
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_castration_longevity_choice, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: castration_longevity_choice
 *   human_readable: The Castration-Longevity Trade-off
 *   domain: biological/social/medical
 *
 * SUMMARY:
 *   The castration-longevity trade-off is a constraint that operates at the
 *   intersection of biology, medical autonomy, and gerontological research.
 *   The hypothesis that male sex hormones decrease lifespan has deep
 *   historical roots (eunuch longevity reports, animal studies of
 *   testosterone and mortality) and contemporary support (androgen
 *   suppression in aging, evolutionary trade-off theory). Yet the constraint
 *   exhibits profound structural ambiguity: Is the trade-off an immutable law
 *   of mammalian biology (Mountain), a legitimate biological choice that
 *   benefits longevity research (Rope), a mixed coordination-extraction
 *   hybrid requiring institutional enforcement to suppress elective
 *   castration (Tangled Rope), a temporary barrier being overcome by
 *   pharmacological alternatives (Scaffold), a degraded taboo maintained by
 *   inertia (Piton), or pure extraction of reproductive autonomy (Snare)? The
 *   same biological fact — testosterone's effect on lifespan — is classified
 *   differently by every observer. The constraint's theater ratio (0.61)
 *   reflects the institutional performance of 'medical necessity' and
 *   'patient protection' rhetoric around the castration taboo, obscuring
 *   genuine scientific uncertainty and epistemic suppression.
 *
 * KEY AGENTS:
 *   - Longevity Researchers: Primary beneficiaries (institutional/arbitrage) — benefit from castration hypothesis as research vector; can pivot to alternative interventions
 *   - Male Reproductive Autonomy: Primary victim (powerless/trapped) — cannot exit biological sex hormone effects; castration is irreversible
 *   - Historical Eunuch Communities: Secondary victim (moderate/constrained) — experienced cultural/religious pressure; some benefited from social status
 *   - Medical Ethics Framework: Organized agents (organized/constrained) — see constraint as temporary, working to build non-destructive alternatives
 *   - Medical Taboo Institution: Institutional actor (institutional/arbitrage) — maintains prohibition on elective castration; gatekeeps access and suppresses research
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional suppression as biological law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(castration_longevity_choice, 0.52).
domain_priors:suppression_score(castration_longevity_choice, 0.68).
domain_priors:theater_ratio(castration_longevity_choice, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(castration_longevity_choice, extractiveness, 0.52).
narrative_ontology:constraint_metric(castration_longevity_choice, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(castration_longevity_choice, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(castration_longevity_choice, tangled_rope).
narrative_ontology:human_readable(castration_longevity_choice, "The Castration-Longevity Trade-off").
narrative_ontology:topic_domain(castration_longevity_choice, "biological/social/medical").

domain_priors:requires_active_enforcement(castration_longevity_choice).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(castration_longevity_choice, longevity_researchers).
narrative_ontology:constraint_beneficiary(castration_longevity_choice, biogerontology_field).
narrative_ontology:constraint_beneficiary(castration_longevity_choice, medical_interventionists).
narrative_ontology:constraint_victim(castration_longevity_choice, male_reproductive_autonomy).
narrative_ontology:constraint_victim(castration_longevity_choice, sexual_function_bearers).
narrative_ontology:constraint_victim(castration_longevity_choice, informed_consent_structure).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MALE REPRODUCTIVE AUTONOMY (SNARE) — Cannot exit the biological fact that sex hormones affect lifespan; bears irreversible consequence of castration. No alternative path to longevity benefit without hormonal sacrifice. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.66.
constraint_indexing:constraint_classification(castration_longevity_choice, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: HISTORICAL EUNUCH PRACTITIONERS (TANGLED ROPE) — Constrained by cultural/religious context and lack of alternative prestige pathways, but some benefited from social status, career access, and (allegedly) longevity. d≈0.65, f(d)≈0.95, σ=0.9 → χ≈0.50.
constraint_indexing:constraint_classification(castration_longevity_choice, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LONGEVITY RESEARCH ESTABLISHMENT (ROPE) — Benefits from the castration-longevity hypothesis as a research vector; can arbitrage between multiple intervention modalities (caloric restriction, metformin, senolytic drugs). Experiences constraint as coordination: the hormone-lifespan nexus provides a unified framework for understanding aging. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.06.
constraint_indexing:constraint_classification(castration_longevity_choice, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MEDICAL ETHICS FRAMEWORK (SCAFFOLD) — Organized agents (IRBs, ethics committees, informed consent doctrine) see castration-for-longevity as a temporary constraint resolvable by building non-destructive alternatives: hormone antagonists, selective androgen receptor modulators, gene therapy. d≈0.42, f(d)≈0.42, σ=1.2 → χ≈0.27. Sunset logic: as pharmacological alternatives mature, irreversible castration becomes unnecessary.
constraint_indexing:constraint_classification(castration_longevity_choice, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: MEDICAL TABOO (PITON) — The institutional prohibition on elective castration persists through inertia and cultural conservatism despite weak functional justification. The taboo was adaptive when alternatives didn't exist; now it constrains research and patient choice theatrically. theater_ratio=0.61. Maintained by professional gatekeeping and narrative control, not by coherent safety rationale.
constraint_indexing:constraint_classification(castration_longevity_choice, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, the trade-off between reproductive function and longevity is framed as an immutable biological law: evolution optimized for reproduction, not lifespan extension; hormones that enable reproduction reduce post-reproductive lifespan. This perspective naturalizes the constraint as inherent to mammalian biology. However, structural data (ε=0.52, suppression=0.68, theater=0.61) contradicts mountain classification — the engine will compute this as a false summit, revealing that the 'natural law' framing obscures contingent institutional, technological, and epistemic choices.
constraint_indexing:constraint_classification(castration_longevity_choice, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(castration_longevity_choice_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(castration_longevity_choice, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(castration_longevity_choice, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(castration_longevity_choice, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(castration_longevity_choice, TR),
    TR >= 0.70.

:- end_tests(castration_longevity_choice_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts reproductive autonomy from potential male beneficiaries of longevity-extending interventions. However, the extraction is not maximal because (1) longevity benefit is real and may be legitimately chosen, (2) uncertainty about magnitude reduces confident extraction, and (3) alternative interventions are emerging. The 0.52 value reflects genuine mixed nature — real trade-off, but not pure predation. Suppression (0.68): High. Multiple suppressors: medical institutional gatekeeping (elective castration is prohibited/stigmatized), epistemic suppression (longevity benefit magnitude is understudied due to taboo), social taboo (reproductive autonomy is heavily weighted culturally), and informed consent barriers (honest discussion of trade-off is rare). Theater ratio (0.61): Moderate-high. The medical prohibition on elective castration is partly theatrical: framed as patient protection, but actually reflects institutional conservatism and cultural reproductive normativity rather than coherent safety rationale. Pharmacological alternatives (SARMs, hormone antagonists) are available but underexplored as longevity interventions, indicating that the taboo suppresses research more than it protects patients.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates complete perspectival disagreement. The male reproductive autonomy agent sees pure extraction (Snare) — irreversible loss of sexual function for uncertain longevity benefit, suppressed by institutional taboo. The eunuch tradition practitioners saw mixed coordination and extraction (Tangled Rope) — career and status access but constrained by cultural pressure. The longevity research establishment sees coordination (Rope) — the castration-longevity nexus as a research framework enabling broader aging interventions. The medical ethics framework sees a temporary problem (Scaffold) — castration unnecessary once pharmacological alternatives mature. The medical taboo institution sees a degraded ritual (Piton) — the prohibition persists through inertia, not function. The analytical observer risks seeing natural law (Mountain) — reproductive-longevity trade-off as evolution's immutable design — but the structural data reveals this as a false summit naturalizing contingent institutional choices.
 *
 * DIRECTIONALITY LOGIC:
 *   Male reproductive autonomy: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction — reproductive function is irreversible loss with no exit path. Eunuch tradition practitioners: Victim + constrained → d≈0.65, f(d)≈0.95. High extraction but mitigated by cultural status benefits. Longevity researchers: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary — can pivot to multiple intervention modalities. Medical ethics framework: Organized + constrained → d≈0.42, f(d)≈0.42. Moderate extraction but framework has agency in building alternatives. Medical taboo: Institutional + arbitrage → d≈0.05, f(d)≈-0.10. Piton classification from theater gate (0.61 ≥ 0.70 threshold not met, but significant performative component). Analytical observer: analytical → d≈0.72, f(d)≈1.15. Mountain classification is perspectival (observer naturalizes constraint); false summit.
 *
 * MANDATROPHY ANALYSIS:
 *   CRITICAL AMBIGUITY: The mandatrophy cannot be fully resolved without answering the five omegas. Current state (extractiveness=0.52) is borderline — not clearly snare (would require ε≥0.66) or rope (would require χ≤0.35 from all perspectives). The Tangled Rope classification is justified because: (1) genuine coordination function exists (longevity research benefits from hormone-lifespan nexus), (2) asymmetric extraction exists (reproductive autonomy is extracted from some beneficiaries), (3) active enforcement is required (medical institutions enforce taboo). However, if omega_hormone_lifespan_causality resolves as 'behavioral correlation only' (not direct hormonal causation), the constraint's biological foundation collapses and it becomes purely social/institutional (likely Snare or Piton). If omega_alternative_intervention_sufficiency resolves as 'yes, pharmacological alternatives work', the scaffold sunset becomes real and the constraint degrades to Piton. If omega_informed_consent_authenticity resolves as 'no, suppression prevents authentic consent', the constraint shifts toward Snare. The current Tangled Rope is robust only under the hypothesis that castration provides genuine longevity benefit AND authentic consent is epistemically possible AND institutional enforcement is justified. This triply-conditional structure is the mandatrophy itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hormone_lifespan_causality,
    'Do testosterone and other male sex hormones causally reduce lifespan, or do they correlate with behaviors/pathologies that reduce lifespan?',
    'Longitudinal studies with hormonal manipulation independent of behavior; animal models with isolated hormone/behavior decoupling; Mendelian randomization on genetic variants affecting hormone levels',
    'If causal at hormonal level: castration is a real lever (constraint is genuine biological tradeoff). If behavioral/ecological: castration removes behavioral drivers of mortality without addressing root causes (constraint is partly social construction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hormone_lifespan_causality, empirical, 'Whether testosterone directly causes lifespan reduction or correlates via behavior').

omega_variable(
    alternative_intervention_sufficiency,
    'Can selective androgen receptor modulators (SARMs), hormone antagonists, or gene therapy achieve longevity benefits without reproductive sacrifice?',
    'Clinical trials comparing castration vs pharmacological antagonism vs gene therapy on longevity outcomes; mechanistic studies of hormone signaling selectivity',
    'If alternatives are effective: castration is rendered unnecessary (scaffold sunset is real). If alternatives fail: castration may be the only biological lever (constraint is harder and more extractive).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_intervention_sufficiency, empirical, 'Whether pharmacological alternatives can replicate castration longevity benefits').

omega_variable(
    informed_consent_authenticity,
    'Can informed consent for elective castration be authentic given medical institutional suppression, social taboo, and uncertainty about magnitude of longevity benefit?',
    'Analysis of how medical institutions frame castration-for-longevity; comparison of informed consent documents across jurisdictions; survey of patient decision-making independence',
    'If authentic consent is possible: victim status is reduced, constraint becomes rope. If suppression prevents authenticity: constraint is snare for victims (trapped by epistemic/institutional barriers).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(informed_consent_authenticity, conceptual, 'Whether authentic informed consent for elective castration is possible under current institutional conditions').

omega_variable(
    longevity_quantification_precision,
    'What is the precise magnitude of lifespan extension from castration in humans (not historical estimates, but clinical estimates if available)?',
    'Meta-analysis of historical eunuch records; controlled animal models with translational parameters; biomarker studies of aging acceleration in androgen-rich vs androgen-depleted groups',
    'If extension < 5 years: extraction cost (reproductive loss) may outweigh benefit. If extension > 15 years: trade-off becomes more structurally compelling.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(longevity_quantification_precision, empirical, 'Precise quantification of castration-induced lifespan extension in humans').

omega_variable(
    autonomy_weighting_cultural_variance,
    'How should medical ethics weight reproductive autonomy vs longevity choice across cultures with different valuations of reproductive function?',
    'Cross-cultural ethics analysis; empirical study of reproductive autonomy valuations; comparative medical policy analysis',
    'If autonomy is paramount globally: constraint is universally extractive (snare from reproductive autonomy perspective). If longevity choice is paramount: constraint is universally coordinating (rope from longevity perspective).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(autonomy_weighting_cultural_variance, preference, 'Cross-cultural weighting of reproductive autonomy vs longevity extension').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(castration_longevity_choice, 0, 1000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cast_long_tr_t0, castration_longevity_choice, theater_ratio, 0, 0.35).
narrative_ontology:measurement(cast_long_tr_t500, castration_longevity_choice, theater_ratio, 500, 0.52).
narrative_ontology:measurement(cast_long_tr_t1000, castration_longevity_choice, theater_ratio, 1000, 0.61).

% Extraction over time
narrative_ontology:measurement(cast_long_be_t0, castration_longevity_choice, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(cast_long_be_t500, castration_longevity_choice, base_extractiveness, 500, 0.42).
narrative_ontology:measurement(cast_long_be_t1000, castration_longevity_choice, base_extractiveness, 1000, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(castration_longevity_choice, resource_allocation).
narrative_ontology:affects_constraint(castration_longevity_choice, hormone_replacement_therapy_access).
narrative_ontology:affects_constraint(castration_longevity_choice, transgender_medical_autonomy).
narrative_ontology:affects_constraint(castration_longevity_choice, reproductive_autonomy_framework).

% DUAL FORMULATION NOTE:
% The castration-longevity trade-off decomposes into biological (hormone-lifespan causality), epistemic (magnitude and mechanism), and institutional (medical taboo enforcement) constraints. The network links reflect structural dependencies: HRT access determines whether castration is irreversible or reversible (affecting directionality); transgender medical autonomy shares the same institutional gatekeeping; reproductive autonomy is the primary victim structural role.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(castration_longevity_choice, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
