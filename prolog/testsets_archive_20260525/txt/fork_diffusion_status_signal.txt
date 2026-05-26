% ============================================================================
% CONSTRAINT STORY: fork_diffusion_status_signal
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fork_diffusion_status_signal, []).

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
 *   constraint_id: fork_diffusion_status_signal
 *   human_readable: Fork Diffusion as Status Signal and Dental Reconfiguration Constraint
 *   domain: anthropology/technology_adoption/cultural_evolution
 *
 * SUMMARY:
 *   Fork adoption in Europe during the 16th-18th centuries represents a
 *   unique constraint case where a status-signaling mechanism became embedded
 *   in physical morphology through generational behavioral change. Beginning
 *   as an elite practice in Renaissance Italy and spreading northward through
 *   aristocratic courts, fork use gradually became enforced social
 *   expectation, then internalized cultural norm, and finally a
 *   path-dependent reality with measurable biological consequences: the
 *   emergence of the modern overbite dental pattern as jaw mechanics adapted
 *   to fork-based eating. This constraint tests a theoretical edge
 *   case—whether path-naturalization can occur through pure disuse of
 *   alternatives without active extraction, or whether beneficiaries
 *   necessarily persist once the path becomes locked. The data suggests a
 *   trajectory from high extraction (elite monopoly on refined status through
 *   fork use) toward lower extraction (universal adoption, loss of status
 *   differentiation) toward institutional theater (modern etiquette rules
 *   divorced from original function) toward potential natural law fallacy
 *   (dental morphology appearing immutable despite its recent contingent
 *   origin). The constraint exhibits all six DR types from different
 *   structural positions, with the critical diagnostic question being whether
 *   the false summit (natural law appearance) masks an extraction mechanism
 *   that has evolved rather than disappeared.
 *
 * KEY AGENTS:
 *   - Aristocratic Elite: Primary beneficiary (institutional/arbitrage) — captures status differentiation and cultural authority through fork monopoly
 *   - Lower Class Populations: Primary victim (powerless/trapped) — face social exclusion and employment barriers for refusing conformity
 *   - Merchant Class: Secondary actor (moderate/constrained) — constrained by status signaling requirements but benefits from cutlery production and service opportunities
 *   - Culinary Professionals: Organized victim (organized/constrained) — both benefit from complex service protocols and face rigid enforcement hierarchy
 *   - Dental Morphology Stability: Collective victim (powerless/trapped) — abstract biological property bearing the accumulated cost of eating pattern change
 *   - Etiquette Ritual System: Institutional piton (institutional/arbitrage) — maintains theater of fork rules through inertia, not active function
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent status choice as inevitable biological law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fork_diffusion_status_signal, 0.38).
domain_priors:suppression_score(fork_diffusion_status_signal, 0.62).
domain_priors:theater_ratio(fork_diffusion_status_signal, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fork_diffusion_status_signal, extractiveness, 0.38).
narrative_ontology:constraint_metric(fork_diffusion_status_signal, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(fork_diffusion_status_signal, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fork_diffusion_status_signal, tangled_rope).
narrative_ontology:human_readable(fork_diffusion_status_signal, "Fork Diffusion as Status Signal and Dental Reconfiguration Constraint").
narrative_ontology:topic_domain(fork_diffusion_status_signal, "anthropology/technology_adoption/cultural_evolution").

domain_priors:requires_active_enforcement(fork_diffusion_status_signal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fork_diffusion_status_signal, aristocratic_classes).
narrative_ontology:constraint_beneficiary(fork_diffusion_status_signal, commercial_cutlery_producers).
narrative_ontology:constraint_victim(fork_diffusion_status_signal, lower_class_populations).
narrative_ontology:constraint_victim(fork_diffusion_status_signal, dental_morphology_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

constraint_indexing:constraint_classification(fork_diffusion_status_signal, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

constraint_indexing:constraint_classification(fork_diffusion_status_signal, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

constraint_indexing:constraint_classification(fork_diffusion_status_signal, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

constraint_indexing:constraint_classification(fork_diffusion_status_signal, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

constraint_indexing:constraint_classification(fork_diffusion_status_signal, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

constraint_indexing:constraint_classification(fork_diffusion_status_signal, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fork_diffusion_status_signal_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fork_diffusion_status_signal, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fork_diffusion_status_signal, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(fork_diffusion_status_signal, TR),
    TR >= 0.70.

:- end_tests(fork_diffusion_status_signal_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate, declining over interval. Initial extractiveness was high (0.52 at T=0) when fork use was elite monopoly—the constraint extracted status differential and enforced class hierarchy. As universal adoption occurred by T=200, the status differentiation value declined, reducing extraction to 0.38. However, extraction never reached zero because merchants and culinary professionals continue to benefit from fork-centered commercial activity and service coordination, even as the original elite extraction mechanism loses force. This trajectory suggests the constraint evolves from snare → tangled_rope → piton rather than disappearing. Suppression (0.62): High and persistent. The suppression mechanism shifted from legal/employment-based (early period: sumptuary laws, domestic service requirements) to social/cultural (later period: stigma against 'uncivilized' eating). The scalar value remains high because alternatives (knife-and-fingers eating) became increasingly unavailable as universal adoption created infrastructure lock-in: all cutlery sets include forks, all dining spaces presume fork use, all social training begins with fork instruction. Exit options remained genuinely suppressed throughout. Theater ratio (0.68, rising from 0.35): Dramatic increase reflects the progressive disconnection between function and ritual. In the early period (T=0), fork use had genuine functional value for multi-course service coordination—theater was low. By T=200, fork placement rules and usage etiquette had become purely performative, divorced from any efficiency function. Modern fork usage (18 rules for a single utensil) serves only status signaling and social boundary maintenance, not functional eating advantage. The rising theater ratio is the diagnostic signature of piton emergence.
 *
 * PERSPECTIVAL GAP:
 *   The critical diagnostic gap is between perspectives that experience fork adoption as enforced constraint (lower class: snare) and perspectives that experience it as natural law (analytical observer: mountain). The engine's false summit detector identifies the gap: beneficiary group exists (aristocratic elite), yet the mountain perspective naturalizes the outcome. This gap reveals how status-signaling mechanisms become embedded in biological morphology and subsequently naturalized as inevitable physical laws. The mechanism is: (1) elite innovation creates status boundary, (2) universal adoption erases original extraction value, (3) path-dependent infrastructure locks in conformity, (4) biological consequences accumulate, (5) natural law appearance emerges from disuse of alternatives, (6) observers risk treating the contingent outcome as immutable law.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from beneficiary/victim declarations and exit options. Aristocratic beneficiaries with arbitrage exit (can always signal status through other means) experience low d ~0.15, producing negative effective extraction χ—the constraint subsidizes them. Lower class victims with trapped exit (forced conformity for employment) experience high d ~0.95, producing maximum f(d) ~1.42, amplifying experienced extraction. Merchants with constrained exit (can avoid fork adoption at career cost) experience moderate d ~0.65, producing balanced f(d) ~1.00, reflecting their mixed position. The dental morphology victim is powerless/trapped/universal, producing canonical d ~1.00, but as an abstract collective good, the constraint's extraction manifests as biological cost rather than economic extraction. The piton perspective experiences low χ because theater_ratio high (0.68) dampens the classification—institutional actors maintaining ritual see it as low-extraction maintenance of tradition, not extraction mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION STRUCTURE: This constraint resolves mandatrophy by decomposing across time and structural position. At T=0 (1500), the constraint is clearly snare/extraction: elite monopoly on fork status creates high suppression and asymmetric benefit. At T=200 (1700), the constraint approaches rope/piton: universal adoption erases the status differential, and fork etiquette becomes performative theater. The mandatrophy question—'Is this coordination (rope) or extraction (snare)?'—has a time-dependent answer: it WAS extraction, it IS theater, it appears to BE natural law. The false summit signature fires because the mountain perspective naturalizes what was originally a contingent elite choice. The resolution is not 'pick one type' but 'map the temporal trajectory': snare (early period, elite monopoly) → tangled_rope (middle period, universal adoption with infrastructure benefits) → piton (late period, theater divorced from function) → false mountain (observational risk: naturalizing path-dependent outcome as inevitable law).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    beneficiary_intentionality_ambiguity,
    'Did aristocratic elites deliberately engineer fork adoption as a status-signaling mechanism, or did they opportunistically adopt it once it emerged, subsequently naturalizing it as refined behavior?',
    'Historical analysis of adopter testimonies, correspondence, and earliest etiquette texts; identification of explicit intentionality statements vs. post-hoc rationalizations; comparison with similar status-signaling technologies',
    'If intentional design: constraint is clearly snare/tangled_rope with deliberate extraction. If opportunistic adoption: constraint blurs into emergent social coordination with extraction as secondary effect—classification shifts toward rope with embedded status function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_intentionality_ambiguity, empirical, 'Degree of deliberate engineering vs. emergent adoption of fork status signal').

omega_variable(
    dental_morphology_causation_threshold,
    'Is the dental morphology shift (overbite emergence) a necessary and direct consequence of fork-based eating mechanics, or a contingent outcome dependent on generational accumulation, nutritional factors, and genetic drift?',
    'Comparative dental morphology analysis across fork-adopting vs. non-adopting populations controlling for genetics and nutrition; biomechanical studies of bite force distribution under different eating tool configurations; archaeological evidence of overbite prevalence before and after fork adoption',
    'If necessary: the constraint contains a genuine natural law component—the dental change is immutable once eating patterns are established. If contingent: the constraint is purely social; dental changes are side effects, not inherent laws.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dental_morphology_causation_threshold, empirical, 'Causation linking fork use to overbite emergence').

omega_variable(
    lower_class_adoption_voluntary_threshold,
    'To what degree did lower classes adopt fork use voluntarily to gain status mobility vs. through enforced conformity to maintain employment and social position?',
    'Historical records of sumptuary law enforcement, workplace dining rules, domestic service requirements; analysis of adoption rates in controlled vs. uncontrolled populations; comparison with voluntary adoption of other status markers',
    'If mostly voluntary: constraint is coordination problem with status benefit (rope). If mostly enforced: constraint is extraction mechanism masked as cultural refinement (snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lower_class_adoption_voluntary_threshold, empirical, 'Degree of voluntary vs. coerced fork adoption in lower classes').

omega_variable(
    false_summit_natural_law_claim,
    'Does the mountain classification (dental morphology as natural law) represent genuine biomechanical inevitability or naturalization of a contingent institutional constraint that benefits identifiable actors?',
    'Decomposition of the constraint into two questions: (1) Is eating tool change → bite configuration change a law of biomechanics? (2) Was fork adoption a natural discovery or a status-signaling choice by elites? If (1) yes but (2) is human choice, the constraint is not a mountain despite the natural law component.',
    'If mountain is legitimate: constraint is immutable natural law, no extraction component. If false summit: constraint is tangled rope with embedded natural law component serving extraction ends. Triggers false_summit_mountain signature in engine.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_law_claim, conceptual, 'Whether mountain classification naturalizes contingent institutional choice as physical law').

omega_variable(
    path_naturalization_without_beneficiary_extraction,
    'Can a constraint persist and become naturalized purely through disuse of alternatives, without active extraction by identifiable beneficiaries? Or do beneficiaries necessarily emerge once path-dependence creates asymmetric distributions?',
    'Theoretical: examine whether path-dependent coordination can produce appearance of natural law without beneficiary group maintaining the path. Empirical: test via comparison with constraints where beneficiary group is absent or has dissolved but constraint persists (e.g., vestigial organs, abandoned technologies).',
    'If beneficiary extraction is necessary: tangled_rope classification is correct; false summit signature fires. If paths can naturalize through pure disuse: constraint may be rope or scaffold evolving into piton, without snare/extraction component—reclassify upward.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(path_naturalization_without_beneficiary_extraction, conceptual, 'Whether path-naturalization requires active beneficiary extraction or emerges from disuse alone').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fork_diffusion_status_signal, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fork_theater_1500, fork_diffusion_status_signal, theater_ratio, 0, 0.35).
narrative_ontology:measurement(fork_theater_1600, fork_diffusion_status_signal, theater_ratio, 100, 0.58).
narrative_ontology:measurement(fork_theater_1700, fork_diffusion_status_signal, theater_ratio, 200, 0.68).

% Extraction over time
narrative_ontology:measurement(fork_extract_1500, fork_diffusion_status_signal, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(fork_extract_1600, fork_diffusion_status_signal, base_extractiveness, 100, 0.42).
narrative_ontology:measurement(fork_extract_1700, fork_diffusion_status_signal, base_extractiveness, 200, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fork_diffusion_status_signal, attachment_coordination).
narrative_ontology:affects_constraint(fork_diffusion_status_signal, sumptuary_law_enforcement).
narrative_ontology:affects_constraint(fork_diffusion_status_signal, table_service_standardization).

% DUAL FORMULATION NOTE:
% Fork diffusion as status signal is decomposed from two related constraints with distinct ε values: (1) sumptuary_law_enforcement (ε=0.68, snare) — the legal/employment-based suppression of alternative eating methods; (2) table_service_standardization (ε=0.32, rope) — the genuine coordination problem of multi-course service protocols. The current story (fork_diffusion_status_signal, ε=0.38) represents the intermediate mixed constraint where status signaling and service coordination are entangled. The false summit question applies to whether the dental morphology change is natural law or contingent outcome of this mixed constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fork_diffusion_status_signal, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
