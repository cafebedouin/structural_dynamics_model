% ============================================================================
% CONSTRAINT STORY: fda_component_efficacy_standard
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fda_component_efficacy_standard, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: fda_component_efficacy_standard
 *   human_readable: FDA's Component-Level Efficacy Standard for Combination Vaccines
 *   domain: technological/political/public_health
 *
 * SUMMARY:
 *   The FDA's requirement that combination vaccines demonstrate efficacy for
 *   each component separately creates a structural constraint with competing
 *   coordination and extraction functions. On the coordination side,
 *   component-level efficacy data enables rigorous safety assurance and
 *   provides comparable evidence across manufacturers. On the extraction
 *   side, the standard creates approval barriers that protect incumbent
 *   manufacturers with pre-approved components while delaying access to
 *   potentially superior novel combinations, particularly harming low-income
 *   countries where efficient multi-disease vaccination is critical. The
 *   constraint is a genuine tangled rope: the coordination function (safety
 *   verification) is real and valuable, but is entangled with extractive
 *   effects (market protection, approval delays) that create real public
 *   health costs measured in preventable deaths. The theater ratio (0.64)
 *   reflects that component-level testing increasingly consists of regulatory
 *   compliance work that confirms what immunological mechanism already
 *   predicts rather than discovering unexpected interactions. The constraint
 *   has become more extractive over the interval as immunological
 *   understanding has advanced but regulatory requirements have not adapted —
 *   modern immunologists can predict most vaccine interactions from first
 *   principles, yet the standard persists through institutional inertia.
 *
 * KEY AGENTS:
 *   - Emerging Vaccine Developers: Primary victims (powerless/trapped) — must replicate component-level efficacy trials for each novel combination, creating 5-7 year delays and hundreds of millions in R&D costs
 *   - Public Health Systems in Low-Income Countries: Secondary victims (moderate/constrained) — need efficient multi-disease vaccines but cannot access novel combinations until component-level trials complete in wealthy markets
 *   - Incumbent Vaccine Manufacturers: Primary beneficiaries (institutional/arbitrage) — protected from rapid competitive innovation by high regulatory barriers; their approved combinations face minimal new competition
 *   - FDA Regulatory Authority: Institutional arbiter (institutional/constrained) — enforces component-level standard as safety requirement but also maintains regulatory authority through standard definition; constrained by Congressional mandate and existing precedent
 *   - Component Testing Infrastructure: Institutional system (institutional/constrained) — clinical trial networks, contract research organizations, and regulatory consultants whose business models depend on component-level trial requirements
 *   - Analytical Observer: Global immunology perspective (analytical/analytical) — can measure net public health costs of approval delays against safety gains, revealing true extraction magnitude
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fda_component_efficacy_standard, 0.52).
domain_priors:suppression_score(fda_component_efficacy_standard, 0.68).
domain_priors:theater_ratio(fda_component_efficacy_standard, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fda_component_efficacy_standard, extractiveness, 0.52).
narrative_ontology:constraint_metric(fda_component_efficacy_standard, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(fda_component_efficacy_standard, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fda_component_efficacy_standard, tangled_rope).
narrative_ontology:human_readable(fda_component_efficacy_standard, "FDA's Component-Level Efficacy Standard for Combination Vaccines").
narrative_ontology:topic_domain(fda_component_efficacy_standard, "technological/political/public_health").

domain_priors:requires_active_enforcement(fda_component_efficacy_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fda_component_efficacy_standard, incumbent_vaccine_manufacturers).
narrative_ontology:constraint_beneficiary(fda_component_efficacy_standard, fda_regulatory_authority).
narrative_ontology:constraint_victim(fda_component_efficacy_standard, vaccine_developers_with_novel_combinations).
narrative_ontology:constraint_victim(fda_component_efficacy_standard, public_health_access_equity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EMERGING VACCINE DEVELOPER (SNARE) — Cannot exit the FDA approval pathway; must demonstrate component-level efficacy for novel combination formulations even when epidemiological data supports the combination. Trapped by regulatory monopoly. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.72.
constraint_indexing:constraint_classification(fda_component_efficacy_standard, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PUBLIC HEALTH SYSTEMS IN LOW-INCOME COUNTRIES (SNARE) — Constrained by FDA standard adoption in international markets and WHO prequalification processes that reference FDA approvals. Needs efficient combination vaccines but cannot bypass component-level efficacy requirement. High suppression due to resource constraints. d≈0.88, f(d)≈1.32, σ=1.1 → χ≈0.75.
constraint_indexing:constraint_classification(fda_component_efficacy_standard, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: INCUMBENT VACCINE MANUFACTURERS (ROPE) — Benefit from high regulatory barriers that protect their existing approved combinations. Component-level standard functions as coordination mechanism for their market position: ensures no competitor can rapidly iterate combination strategies without replicating each component trial. d≈0.08, f(d)≈-0.11, σ=1.0 → χ≈-0.06. Net beneficiary through protection.
constraint_indexing:constraint_classification(fda_component_efficacy_standard, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: FDA REGULATORY AUTHORITY (TANGLED ROPE) — Coordination function: standardized component-level testing ensures reproducible efficacy data across manufacturers. Extraction function: the standard creates dependency on FDA for approval pathway definition; FDA maintains authority over what constitutes 'sufficient' evidence. Constrained by Congressional mandate to protect public health but also embedded in existing approval precedents. d≈0.48, f(d)≈0.60, σ=1.0 → χ≈0.31. Hybrid role: enforcer of genuine safety requirement + beneficiary of regulatory authority consolidation.
constraint_indexing:constraint_classification(fda_component_efficacy_standard, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: COMPONENT EFFICACY TESTING INFRASTRUCTURE (PITON) — Historical origin: component-level testing made sense when vaccines were developed sequentially and combined empirically. Modern immunology can predict interactions from first principles; component-level testing is now largely performative verification rather than discovery mechanism. Theater ratio (0.64) reflects that extensive component-level trials continue despite diminishing epistemic return. d≈0.45, f(d)≈0.50, σ=1.0 → χ≈0.32. Maintained through institutional inertia despite reduced functional necessity.
constraint_indexing:constraint_classification(fda_component_efficacy_standard, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From global immunology perspective, the constraint is both coordinating AND extractive. Coordination: component-level efficacy data enables international vaccine surveillance and comparison. Extraction: the standard delays access to potentially superior combination vaccines, creating mortality and morbidity costs in populations waiting for approved formulations. Theater component (0.64) reflects regulatory theater in early-phase work. d≈0.62, f(d)≈0.80, σ=1.2 → χ≈0.50. Hybrid classification stable across perspectives — indicates genuine tangled rope structure, not perspectival artifact.
constraint_indexing:constraint_classification(fda_component_efficacy_standard, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fda_component_efficacy_standard_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fda_component_efficacy_standard, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fda_component_efficacy_standard, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(fda_component_efficacy_standard, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(fda_component_efficacy_standard, TR),
    TR >= 0.70.

:- end_tests(fda_component_efficacy_standard_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The standard creates measurable approval delays (5-7 years per combination) and development costs (200-300M per novel combination). However, extractiveness is not maximal (>0.70) because: (a) the coordination function is genuine — component-level data does provide safety value; (b) incumbent manufacturers also face component-level requirements for new combinations; (c) alternatives exist (European, Japanese regulatory pathways) though with higher political/trade friction. The 0.52 value reflects substantive extraction entangled with legitimate coordination. Suppression (0.68): High. Multiple suppression mechanisms: (a) regulatory monopoly — only FDA pathway provides access to U.S. market; (b) international precedent — WHO prequalification references FDA approvals, amplifying FDA standard globally; (c) resource barriers — component-level trials require $200M+ capital, inaccessible to most developers; (d) knowledge asymmetry — FDA owns interpretation of what constitutes 'sufficient' component-level evidence; (e) publication bias — failed component trials less likely published, reducing external scrutiny. Theater ratio (0.64): Moderate-high. Components of theater: (a) safety verification theater — component trials confirm predicted immunological interactions ~80% of the time; (b) regulatory compliance theater — trial design optimized for FDA approval rather than public health question; (c) precedent theater — requirements persist because previous approvals followed this path, not because new data justify it. Early period (0.35) reflects genuine innovation in vaccine combination strategies when component-level testing revealed unexpected interactions. Modern period (0.64) reflects that immunology now predicts most interactions — testing confirms rather than discovers.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximal perspectival disagreement. The emerging developer sees pure extraction (Snare): trapped in a regulatory monopoly, forced to replicate costly trials despite epidemiological evidence of combination safety. The incumbent manufacturer sees coordination (Rope): the standard ensures all competitors meet equal evidence standards and protects against unsafe rapid iteration. The FDA sees its own role as tangled (Tangled Rope): genuine safety function entangled with regulatory authority maintenance. The piton perspective reveals degraded function: modern immunology makes component-level testing largely confirmatory rather than discovery work. The analytical observer sees the strongest tangled rope signature: global public health gains from novel combinations (measurable in prevented mortality) are offset against safety verification benefits, with extraction costs outweighing coordination benefits by approximately 3-5x in low-income country contexts. This perspectival gap is not a measurement problem — it reflects genuine structural asymmetry: the beneficiaries (incumbents, FDA authority) experience coordination; the victims (emerging developers, low-income populations) experience extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Emerging vaccine developer: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction from the developer's perspective — no exit option other than abandoning novel combinations. Incumbent manufacturer: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11. Arbitrage exit (can develop in EU, Japan markets) reduces their d; they benefit from the standard's protection. FDA authority: Institutional + constrained → d≈0.48, f(d)≈0.60. Constrained exit (bound by Congressional mandate and precedent) pushes d toward center; they are neither pure beneficiary nor pure victim, but rather an institutional arbiter whose authority depends on the standard persisting. Public health system: Victim + constrained → d≈0.88, f(d)≈1.32. High extraction but not maximum because some alternative vaccines exist (less efficient, higher cost). Analytical observer: analytical → d≈0.62, f(d)≈0.80. Positioned between victims and beneficiaries because observer can measure net outcomes; global scope amplifies harm (σ=1.2) from approval delays in low-income contexts.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves the coordination-vs-extraction ambiguity by demonstrating that a genuine coordination function (safety verification) and a genuine extraction function (market protection) are structurally inseparable within the current regulatory framework. The mandatrophy is not resolved by choosing 'actually this is rope' or 'actually this is snare' — the tangled rope classification is the correct answer. The constraint cannot be reformulated as pure coordination (Rope) because the approval delays create real mortality/morbidity costs that outweigh safety benefits. The constraint cannot be reformulated as pure extraction (Snare) because component-level efficacy data genuinely reduces approval risks and enables international regulatory harmonization. The path to mandatrophy resolution is NOT to reclassify the constraint but to decompose it: (1) a safety-verification sub-constraint (component-level efficacy requirement for genuinely novel immunological interactions) that should be Rope or Scaffold; (2) a market-protection sub-constraint (blanket requirement for all combinations regardless of prior component data and immunological predictability) that should be recognized as extractive. The current FDA standard conflates these, which is why tangled rope is the correct classification. Resolving the mandatrophy requires distinguishing these functions and creating a path where novel combinations with independent component approvals and strong mechanistic rationales can use expedited combination-efficacy studies (epidemiological equivalence pathways) rather than full component-level trials. This would preserve safety coordination while removing extraction. Such pathways are being piloted (the 'accelerated pathway for combination vaccines' proposal is a scaffold precursor), which confirms the decomposition logic.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interaction_predictability_threshold,
    'At what level of immunological understanding does component-level efficacy testing become redundant rather than necessary for safety assurance?',
    'Comparative analysis: trials where component efficacy data matched immunological predictions vs. cases where interactions were unpredictable; correlation between mechanistic understanding and actual combination outcomes',
    'If threshold crossed: component-level standard becomes theater (piton classification strengthens). If interactions remain unpredictable: standard retains genuine safety function (tangled rope strengthens).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(interaction_predictability_threshold, empirical, 'Whether modern immunology can predict combination vaccine interactions without empirical component testing').

omega_variable(
    mortality_cost_of_delay,
    'What is the total disease burden (in DALYs prevented vs. prevented) of delaying access to novel combination vaccines while component-level efficacy trials complete?',
    'Epidemiological modeling comparing: mortality/morbidity from diseases preventable by delayed combinations vs. safety gains from component-level testing; historical case studies of combination vaccines where delays cost documented lives',
    'If delay cost > safety benefit by factor of 10x: extraction dominance over coordination becomes undeniable (snare from all perspectives). If parity: true tangled rope structure confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mortality_cost_of_delay, empirical, 'Net public health cost of FDA approval timeline delays for combination vaccines').

omega_variable(
    equivalence_pathway_feasibility,
    'Could an ''epidemiological efficacy equivalence'' standard replace component-level testing for combinations of independently-proven components with known safety profiles?',
    'Pilot pathway design: comparison of resources needed for component-level trials vs. combination-level surveillance with safety thresholds; historical analysis of combination vaccines approved via alternative pathways in other jurisdictions',
    'If feasible: scaffold sunset clause becomes real — component-level standard could sunset for well-characterized components. If infeasible: standard persists, piton/tangled rope perspectives persist.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(equivalence_pathway_feasibility, conceptual, 'Whether alternative approval pathways could functionally replace component-level efficacy standard').

omega_variable(
    regulatory_capture_extent,
    'To what degree does incumbent manufacturer influence over FDA advisory committees shape component-level standard persistence?',
    'Analysis of advisory committee composition and voting patterns; comparison of FDA guidance before/after advisory committee membership changes; international regulatory comparison (EMA, PMDA standards)',
    'If high capture: extractive intentionality confirmed (snare/tangled rope extraction component strengthens). If low: standard reflects genuine safety consensus (tangled rope coordination component justified).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_extent, empirical, 'Extent of regulatory capture in FDA component-level efficacy standard').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fda_component_efficacy_standard, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fda_combo_tr_t0, fda_component_efficacy_standard, theater_ratio, 0, 0.35).
narrative_ontology:measurement(fda_combo_tr_t10, fda_component_efficacy_standard, theater_ratio, 10, 0.48).
narrative_ontology:measurement(fda_combo_tr_t20, fda_component_efficacy_standard, theater_ratio, 20, 0.64).

% Extraction over time
narrative_ontology:measurement(fda_combo_be_t0, fda_component_efficacy_standard, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(fda_combo_be_t10, fda_component_efficacy_standard, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(fda_combo_be_t20, fda_component_efficacy_standard, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fda_component_efficacy_standard, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fda_component_efficacy_standard, 0.35).
narrative_ontology:affects_constraint(fda_component_efficacy_standard, vaccine_development_timeline_bottleneck).
narrative_ontology:affects_constraint(fda_component_efficacy_standard, pharmaceutical_manufacturing_regulatory_capture).
narrative_ontology:affects_constraint(fda_component_efficacy_standard, global_vaccine_equity_access_constraint).

% DUAL FORMULATION NOTE:
% The component-level efficacy standard is downstream of the broader FDA regulatory authority constraint but represents a distinct structural constraint at the vaccine-combination design level. The upstream constraint (FDA authority itself) has ε≈0.15 (Mountain-adjacent) reflecting constitutional/statutory foundations; the component-level standard has ε≈0.52 (Tangled Rope) reflecting contingent technical/market decisions. They are linked because changes to FDA authority precedents propagate downward to how component-level standards are interpreted and enforced.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fda_component_efficacy_standard, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
