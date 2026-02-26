% ============================================================================
% CONSTRAINT STORY: agg1_genetic_determinism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_agg1_genetic_determinism, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: agg1_genetic_determinism
 *   human_readable: The institutional claim that the AGG-1 gene deterministically causes aggression.
 *   domain: social_science/genetics/policy
 *
 * SUMMARY:
 *   This constraint is not the AGG-1 gene itself, but the institutional claim
 *   that it deterministically causes aggression. This is a social construct
 *   built upon a sliver of complex, non-deterministic biological reality. The
 *   claim functions to simplify a multifaceted social problem into a simple
 *   biological one, enabling certain institutional actors to extract
 *   resources (funding, legal authority, social control) while imposing
 *   severe costs (stigma, discrimination, loss of liberty) on a
 *   biologically-defined subgroup. The high suppression score reflects how
 *   this simplistic narrative actively crowds out more nuanced,
 *   evidence-based approaches that focus on gene-environment interactions and
 *   social factors.
 *
 * KEY AGENTS:
 *   - Individuals with the gene variant: Primary victims (powerless/trapped) — bear the costs of stigma and discrimination.
 *   - Punitive legal systems: Primary beneficiaries (institutional/arbitrage) — gain a 'scientific' tool for social control.
 *   - Sensationalist media outlets: Beneficiaries (organized/mobile) — profit from a simple, dramatic narrative.
 *   - Nuanced genetic researchers: Secondary victims (moderate/constrained) — their complex findings are suppressed or distorted by the dominant narrative.
 *   - The general public: Tertiary victims (organized/mobile) — their understanding is distorted, leading to support for poor policy.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(agg1_genetic_determinism, 0.75).
domain_priors:suppression_score(agg1_genetic_determinism, 0.8).
domain_priors:theater_ratio(agg1_genetic_determinism, 0.72).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(agg1_genetic_determinism, extractiveness, 0.75).
narrative_ontology:constraint_metric(agg1_genetic_determinism, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(agg1_genetic_determinism, theater_ratio, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(agg1_genetic_determinism, snare).
narrative_ontology:human_readable(agg1_genetic_determinism, "The institutional claim that the AGG-1 gene deterministically causes aggression.").
narrative_ontology:topic_domain(agg1_genetic_determinism, "social_science/genetics/policy").

domain_priors:requires_active_enforcement(agg1_genetic_determinism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(agg1_genetic_determinism, sensationalist_media_outlets).
narrative_ontology:constraint_beneficiary(agg1_genetic_determinism, private_security_and_pharma).
narrative_ontology:constraint_beneficiary(agg1_genetic_determinism, punitive_legal_systems).
narrative_ontology:constraint_victim(agg1_genetic_determinism, individuals_with_gene_variant).
narrative_ontology:constraint_victim(agg1_genetic_determinism, evidence_based_social_policy).
narrative_ontology:constraint_victim(agg1_genetic_determinism, public_scientific_literacy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE LABELED INDIVIDUAL (SNARE) — An individual identified with the AGG-1 variant is trapped by a biological label they cannot change or escape. They face social stigma, potential legal discrimination, and psychological burden with no exit. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈1.07.
constraint_indexing:constraint_classification(agg1_genetic_determinism, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PUNITIVE LEGAL SYSTEM (ROPE) — From this perspective, the claim is a pure coordination tool. It provides a simple, 'scientific' justification for harsher sentencing, increased surveillance, and absolving the state of addressing complex socio-economic causes of crime. d≈0.05, f(d)≈-0.12, σ=1.0 → χ≈-0.09. The negative extraction indicates a net subsidy.
constraint_indexing:constraint_classification(agg1_genetic_determinism, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE NUANCED GENETIC RESEARCHER (TANGLED ROPE) — This agent sees both the genuine (but small) coordination function of identifying a genetic correlate and the massive, harmful extraction built upon the deterministic misinterpretation. They are constrained by funding and publication systems that reward simple, dramatic narratives. d≈0.75 (as victim), f(d)≈1.10, σ=1.2 → χ≈0.99. The classification is Tangled Rope, reflecting the mixed function, though the effective extraction is high enough to border on Snare.
constraint_indexing:constraint_classification(agg1_genetic_determinism, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: THE GENERAL PUBLIC (PITON) — The public primarily interacts with a degraded, performative version of the scientific claim via media. The 'warrior gene' narrative persists through institutional inertia and its narrative simplicity, long after its scientific function has been superseded by more complex models. theater_ratio=0.72 satisfies the piton gate (≥0.70).
constraint_indexing:constraint_classification(agg1_genetic_determinism, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: THE ANALYTICAL OBSERVER (SNARE) — The observer sees the full structure: a scientifically weak claim is used to create a powerful extractive mechanism. The so-called coordination function is based on a falsehood and serves only to enable the extraction. High base extraction (ε=0.75) and suppression (0.80) confirm the Snare classification. d≈0.73, f(d)≈1.15, σ=1.2 → χ≈1.04.
constraint_indexing:constraint_classification(agg1_genetic_determinism, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 6: THE BIOLOGICAL DETERMINIST VIEW (MOUNTAIN) — This perspective misinterprets the institutional claim as a natural law ('biology is destiny'). It frames the genetic link as an unchangeable feature of reality. The engine will flag this as a false summit, as the base properties (ε=0.75, suppression=0.80, requires_active_enforcement=true) are antithetical to a Mountain classification.
constraint_indexing:constraint_classification(agg1_genetic_determinism, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(agg1_genetic_determinism_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(agg1_genetic_determinism, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(agg1_genetic_determinism, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(agg1_genetic_determinism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(agg1_genetic_determinism, TR),
    TR >= 0.70.

:- end_tests(agg1_genetic_determinism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.75) is high because the claim reallocates immense social resources and moral authority based on a gross oversimplification of science, with tangible harm to labeled individuals. Suppression (0.80) is high because the deterministic 'warrior gene' narrative is highly memorable and institutionally sticky, making it extremely difficult for more complex, accurate models of gene-environment interaction to gain traction in policy and public discourse. Theater Ratio (0.72) is high because the public-facing version of the claim is almost entirely performative, a story about biological destiny that has little connection to the functional reality of a minor statistical correlation.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. The legal system sees a useful coordination tool (Rope). The labeled individual experiences an inescapable trap (Snare). The researcher who knows the nuance sees a system of mixed incentives and harmful oversimplification (Tangled Rope). The public consumes a degraded, inertial narrative (Piton). The determinist sees a law of nature (Mountain, a false summit). This demonstrates how a single set of facts can generate the full spectrum of classifications depending on the observer's structural relationship to the claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (legal system, media) have arbitrage or mobile exit options, leading to low 'd' values and a perception of coordination (Rope) or net benefit. Victims (labeled individuals) are trapped, leading to a high 'd' value (d≈0.95) and maximum effective extraction (Snare). Constrained actors like researchers fall in between, experiencing the structure as a mix of coordination and extraction (Tangled Rope). The directionality derivation correctly maps these structural positions to the divergent classifications.
 *
 * MANDATROPHY ANALYSIS:
 *   This case resolves the mandatrophy by illustrating that a constraint's identity is the full set of its perspectival classifications. Asking whether genetic determinism is 'really' a Rope or a Snare is the wrong question. It is a Rope *for the judge*, a Snare *for the defendant*, and a Piton *for the public*. The analytical observer's classification of Snare is a claim about the total structure's dynamics, which are dominated by non-consensual extraction justified by a performative, suppressive narrative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interaction_effect_size,
    'Is the true effect size of gene-environment interaction large enough to completely invalidate any policy based on the gene alone?',
    'Large-scale longitudinal studies correlating AGG-1 variants, detailed environmental histories, and aggression outcomes.',
    'If the interaction effect accounts for >95% of the variance attributed to the gene, the claim collapses from a Snare to a pure Piton (all theater, no function). If the main effect is non-trivial, it remains a Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(interaction_effect_size, empirical, 'Quantifying the gene-environment interaction effect size for AGG-1.').

omega_variable(
    rhetorical_intent,
    'Is the ''deterministic'' claim a deliberate rhetorical strategy for social control or a genuine, widespread misunderstanding of complex science?',
    'Archival analysis of policy memos, media talking points, and private communications from key institutional actors.',
    'If deliberate, it confirms the Snare structure. If a genuine misunderstanding, it suggests the structure is a highly extractive Tangled Rope that emerged without a master plan.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(rhetorical_intent, conceptual, 'Distinguishing between deliberate strategy and emergent misunderstanding.').

omega_variable(
    legal_admissibility_threshold,
    'What level of statistical correlation is deemed sufficient for a genetic marker to be admissible as evidence in legal proceedings?',
    'Comparative analysis of legal standards (e.g., Daubert standard in the U.S.) and their application to behavioral genetics.',
    'This is a policy choice that determines the ''sharpness'' of the Snare. A low threshold enables more extraction; a high threshold defangs the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legal_admissibility_threshold, preference, 'The policy-dependent threshold for legal admissibility of genetic evidence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(agg1_genetic_determinism, 1995, 2015).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(agg1_tr_t0, agg1_genetic_determinism, theater_ratio, 0, 0.4).
narrative_ontology:measurement(agg1_tr_t10, agg1_genetic_determinism, theater_ratio, 10, 0.6).
narrative_ontology:measurement(agg1_tr_t20, agg1_genetic_determinism, theater_ratio, 20, 0.72).

% Extraction over time
narrative_ontology:measurement(agg1_be_t0, agg1_genetic_determinism, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(agg1_be_t10, agg1_genetic_determinism, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(agg1_be_t20, agg1_genetic_determinism, base_extractiveness, 20, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(agg1_genetic_determinism, information_standard).
narrative_ontology:affects_constraint(agg1_genetic_determinism, criminal_justice_sentencing_guidelines).
narrative_ontology:affects_constraint(agg1_genetic_determinism, public_education_science_curriculum).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
