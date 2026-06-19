% ============================================================================
% CONSTRAINT STORY: nad_precursor_bioavailability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nad_precursor_bioavailability, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: nad_precursor_bioavailability
 *   human_readable: NAD+ Precursor Bioavailability Constraint
 *   domain: biomedical/aging_biology/nutritional_biochemistry
 *
 * SUMMARY:
 *   NAD+ precursor supplements (NR and NMN) are marketed as anti-aging
 *   interventions based on research showing age-related NAD+ decline and
 *   preclinical studies in animal models. Human trials consistently
 *   demonstrate circulating NAD+ elevation but minimal functional improvement
 *   in cognition, vascular function, or physical performance. The constraint
 *   operates through the gap between a measurable biomarker (blood NAD+) and
 *   the functional outcomes consumers seek. Manufacturers and influencers
 *   emphasize the biomarker data while eliding the functional outcome gap.
 *   The claim/metric independence is maintained: the constraint is claimed as
 *   snare (pure extraction with coordination cover story) while metrics
 *   describe the actual extractive operation and the enforcement required to
 *   maintain information asymmetry.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nad_precursor_bioavailability, 0.68).
domain_priors:suppression_score(nad_precursor_bioavailability, 0.71).
domain_priors:theater_ratio(nad_precursor_bioavailability, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nad_precursor_bioavailability, extractiveness, 0.68).
narrative_ontology:constraint_metric(nad_precursor_bioavailability, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(nad_precursor_bioavailability, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nad_precursor_bioavailability, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(nad_precursor_bioavailability, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nad_precursor_bioavailability, snare).
narrative_ontology:human_readable(nad_precursor_bioavailability, "NAD+ Precursor Bioavailability Constraint").
narrative_ontology:topic_domain(nad_precursor_bioavailability, "biomedical/aging_biology/nutritional_biochemistry").

domain_priors:requires_active_enforcement(nad_precursor_bioavailability).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nad_precursor_bioavailability, supplement_manufacturers).
narrative_ontology:constraint_beneficiary(nad_precursor_bioavailability, longevity_influencers).
narrative_ontology:constraint_victim(nad_precursor_bioavailability, consumers_expecting_functional_improvement).
narrative_ontology:constraint_victim(nad_precursor_bioavailability, aging_adults_with_limited_resources).
narrative_ontology:constraint_vindicates(nad_precursor_bioavailability, nad_decline_aging_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Manufacture and market NR (nicotinamide riboside) and NMN (nicotinamide mononucleotide) supplements at premium prices, citing published research showing circulating NAD+ elevation. Frame the products as anti-aging interventions targeting mitochondrial function and cellular energy. Control messaging through direct-to-consumer marketing, influencer partnerships, and selective citation of biomarker studies. Can pivot to alternative longevity compounds if market pressure mounts.
narrative_ontology:constraint_stakeholder(nad_precursor_bioavailability, supplement_manufacturers, agenda_setter,
    powerful, biographical, mobile, global).

% Purchase NAD+ precursors expecting cognitive enhancement, improved vascular function, or increased physical performance based on marketing claims and circulating NAD+ data. Pay premium prices for months or years. Functional outcomes remain subjective or absent; blood tests may show NAD+ elevation but no corresponding improvement in the outcomes that motivated purchase. Exit requires acknowledging sunk costs and abandoning hope in a widely-promoted intervention.
narrative_ontology:constraint_stakeholder(nad_precursor_bioavailability, consumers_expecting_functional_improvement, payer,
    moderate, biographical, constrained, global).

% Allocate limited discretionary income to NAD+ precursors based on fear of cognitive decline and promises of healthspan extension. Cannot afford comprehensive health interventions; the supplement represents a significant monthly expense. Lack resources to independently verify claims or access alternative interventions. Trapped by asymmetric information and the psychological cost of admitting the expenditure was ineffective.
narrative_ontology:constraint_stakeholder(nad_precursor_bioavailability, aging_adults_with_limited_resources, payer,
    powerless, immediate, trapped, national).

% Promote NAD+ precursors through affiliate relationships, sponsored content, and personal testimonials. Benefit from commission structures and audience growth. Cite circulating NAD+ studies as evidence while eliding the functional outcome gap. Can shift to promoting alternative compounds if NAD+ precursors lose credibility without reputational cost.
narrative_ontology:constraint_stakeholder(nad_precursor_bioavailability, longevity_influencers, beneficiary,
    organized, biographical, mobile, global).

% Conduct controlled trials measuring both circulating NAD+ and functional outcomes. Publish findings showing consistent biomarker elevation but minimal functional improvement in humans. Observe the gap between their published data and commercial claims. Some receive industry funding for biomarker studies; others pursue independent mechanistic research on tissue-specific NAD+ metabolism.
narrative_ontology:constraint_stakeholder(nad_precursor_bioavailability, aging_biology_researchers, observer,
    institutional, generational, analytical, global).

% Classify NAD+ precursors as dietary supplements rather than drugs, exempting them from efficacy requirements. Monitor for safety signals but do not require demonstration of functional benefit. Could reclassify if sufficient adverse events accumulate or if manufacturers make explicit disease claims, but current regulatory framework treats biomarker elevation as sufficient for market access.
narrative_ontology:constraint_stakeholder(nad_precursor_bioavailability, regulatory_authorities, observer,
    institutional, generational, analytical, national).

% Study tissue-specific NAD+ compartmentalization and the demand-signal mechanisms that regulate mitochondrial NAD+ utilization. Their research suggests circulating NAD+ elevation does not reliably translate to functional mitochondrial NAD+ availability without corresponding demand signals. This mechanistic understanding is largely absent from commercial messaging and consumer understanding.
narrative_ontology:constraint_stakeholder(nad_precursor_bioavailability, mitochondrial_biologists, excluded,
    moderate, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nad_precursor_bioavailability, supplement_manufacturers).
narrative_ontology:fixing_cost_class(nad_precursor_bioavailability, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a measurable intervention targeting a validated aging biomarker (NAD+ decline) with a clear mechanism of action (precursor supplementation) and objective verification (blood NAD+ testing).
% TRANSFER_FUNCTION: Moves money from consumers seeking healthspan extension to supplement manufacturers and affiliated marketers, in exchange for compounds that elevate circulating NAD+ without reliably producing the functional improvements that motivated purchase.
% ABSENT_VOICES: Mitochondrial biologists studying tissue-specific NAD+ compartmentalization and demand-signal requirements are structurally excluded from the commercial conversation. Their mechanistic understanding of why circulating NAD+ elevation may not translate to functional benefit is not represented in marketing materials or consumer decision-making.
% DISAPPEARANCE_RATIONALE: If the constraint vanished overnight and consumers had full access to the functional outcome data, the premium NAD+ precursor market would collapse within months. Consumers would reallocate spending to interventions with demonstrated functional benefits. Manufacturers would pivot to alternative longevity compounds. The research community would refocus on understanding tissue-specific NAD+ metabolism rather than measuring circulating levels.
% FOUNDING_PROBLEM: NAD+ levels decline with age across multiple tissues, and this decline correlates with mitochondrial dysfunction, cellular senescence, and age-related pathology in animal models. Early preclinical studies showed NAD+ precursor supplementation could reverse some age-related deficits in mice.
% FOUNDING_PROBLEM_CORROBORATION: Supplement manufacturers and some aging biology researchers attest the problem is live and the intervention is effective, citing circulating NAD+ elevation and selected animal studies. Independent researchers conducting human trials with functional endpoints, mitochondrial biologists studying compartmentalization, and systematic reviews of human trial data attest that circulating NAD+ elevation does not reliably translate to functional improvement in humans. Multiple published meta-analyses from researchers without industry ties support the functional-outcome-gap reading.
narrative_ontology:disappearance_verdict(nad_precursor_bioavailability, world_rearranges).
narrative_ontology:founding_problem_status(nad_precursor_bioavailability, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nad_precursor_bioavailability, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-18',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-4-5-20250929', 'unspecified').
narrative_ontology:story_seed(nad_precursor_bioavailability, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nad_precursor_bioavailability_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nad_precursor_bioavailability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nad_precursor_bioavailability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is substantial (0.68) because consumers pay premium prices for months or years without receiving the functional benefits that motivated purchase. The transfer is sustained by information asymmetry: circulating NAD+ data is widely cited while functional outcome data remains in specialist literature. Suppression is high (0.71) because the constraint's persistence depends on actively maintaining this information asymmetry through selective citation, influencer marketing, and regulatory classification that exempts supplements from efficacy requirements. Theater ratio is moderate (0.42): the biomarker elevation is real and the preclinical research is genuine, but an increasing share of the commercial apparatus exists to defend the biomarker-as-proxy-for-benefit framing rather than to deliver functional improvement. Accessibility collapse is moderate (0.48) because alternative interventions exist but require more effort or different framing. Resistance is substantial (0.58) as independent researchers publish functional outcome data and some consumers publicly document their null results.
 *
 * PERSPECTIVAL GAP:
 *   From the manufacturer seat, the constraint operates as legitimate commerce: they sell a compound that demonstrably raises circulating NAD+, cite published research, and make no explicit disease claims. From the constrained consumer seats, the same structure operates as extraction: they pay for functional improvement that does not materialize, sustained by selective presentation of biomarker data. The engine computes this divergence from the structural positions; the authored claim does not adjudicate between these framings.
 *
 * DIRECTIONALITY LOGIC:
 *   Supplement manufacturers are the primary beneficiaries (d near 0.1): they collect revenue, control messaging, and can exit to alternative compounds without loss. Longevity influencers are secondary beneficiaries (d near 0.2): they profit from affiliate relationships and audience growth with minimal accountability for outcomes. Consumers expecting functional improvement are the primary targets (d near 0.85): they bear the financial cost and opportunity cost of foregone effective interventions, with constrained exit due to sunk costs and hope. Aging adults with limited resources are maximally extracted (d near 0.95): trapped by information asymmetry and resource constraints. Researchers and regulators occupy analytical positions (d near 0.5): they observe the structure but are not directly extracted from or benefiting.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is not mislabeled coordination. The coordination function (providing a measurable intervention targeting a validated biomarker) is real but minimal relative to the extraction. The functional outcome gap is not a coordination failure but the structural feature that enables extraction: if consumers had full access to functional outcome data, they would not purchase at current prices or volumes. The constraint persists not because it solves a coordination problem but because information asymmetry and regulatory classification suppress the functional outcome signal.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tissue_specific_nad_compartmentalization,
    'Does circulating NAD+ elevation reliably translate to functional NAD+ availability in the tissue compartments (mitochondria, nucleus) where NAD+-dependent processes occur?',
    'Tissue biopsy studies measuring compartment-specific NAD+ levels and NAD+-dependent enzyme activity in humans supplementing with precursors, correlated with functional outcomes. Mechanistic studies of NAD+ transport across mitochondrial membranes and demand-signal requirements.',
    'If compartmentalization prevents functional NAD+ delivery, the entire precursor supplementation approach is mechanistically flawed and the constraint is pure extraction. If compartmentalization is overcome by higher doses or specific formulations, some functional benefit may be achievable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tissue_specific_nad_compartmentalization, empirical, 'Whether circulating NAD+ reaches functional compartments in human tissues.').

omega_variable(
    demand_signal_requirement,
    'Is NAD+ availability the rate-limiting factor for mitochondrial function in aging humans, or is the limitation in demand signals (exercise, metabolic stress) that drive NAD+ utilization?',
    'Controlled trials comparing NAD+ precursors alone versus precursors combined with interventions that increase metabolic demand (exercise, caloric restriction). Mechanistic studies of AMPK and SIRT1 activation requirements.',
    'If demand signals are rate-limiting, NAD+ precursors without corresponding lifestyle interventions will remain ineffective regardless of bioavailability. This would explain the functional outcome gap and suggest the constraint is extraction riding on a misidentified bottleneck.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(demand_signal_requirement, empirical, 'Whether NAD+ supply or demand signals limit mitochondrial function in aging.').

omega_variable(
    biomarker_proxy_validity,
    'Is circulating NAD+ a valid proxy for the functional outcomes consumers seek, or is the biomarker-outcome correlation weak enough that elevation without functional benefit is the expected result?',
    'Systematic review of human trials measuring both circulating NAD+ and functional outcomes. Meta-analysis of correlation strength between biomarker change and functional change across studies.',
    'If the biomarker-outcome correlation is weak, the commercial framing is fundamentally misleading and the constraint is extraction by design. If the correlation is strong but functional benefits require longer duration or higher doses, the constraint may be coordination with implementation failures.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(biomarker_proxy_validity, empirical, 'Whether circulating NAD+ predicts functional improvement in humans.').

omega_variable(
    regulatory_classification_lock_in,
    'Does the dietary supplement classification create a structural incentive to market biomarker elevation rather than pursue drug development with functional endpoints?',
    'Economic analysis of development costs and market access timelines for supplement versus drug pathways. Historical analysis of compounds that transitioned from supplement to drug classification.',
    'If supplement classification enables profitable marketing without efficacy demonstration, manufacturers have no incentive to pursue functional outcome trials and the constraint is sustained by regulatory arbitrage. If drug development is economically viable, the current state represents a transitional market failure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_classification_lock_in, conceptual, 'Whether regulatory structure incentivizes biomarker marketing over functional development.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nad_precursor_bioavailability, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nad__tr_t0, nad_precursor_bioavailability, theater_ratio, 0, 0.25).
narrative_ontology:measurement(nad__tr_t3, nad_precursor_bioavailability, theater_ratio, 3, 0.29).
narrative_ontology:measurement(nad__tr_t6, nad_precursor_bioavailability, theater_ratio, 6, 0.33).
narrative_ontology:measurement(nad__tr_t9, nad_precursor_bioavailability, theater_ratio, 9, 0.37).
narrative_ontology:measurement(nad__tr_t12, nad_precursor_bioavailability, theater_ratio, 12, 0.4).
narrative_ontology:measurement(nad__tr_t15, nad_precursor_bioavailability, theater_ratio, 15, 0.42).

% Extraction over time
narrative_ontology:measurement(nad__be_t0, nad_precursor_bioavailability, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(nad__be_t3, nad_precursor_bioavailability, base_extractiveness, 3, 0.54).
narrative_ontology:measurement(nad__be_t6, nad_precursor_bioavailability, base_extractiveness, 6, 0.59).
narrative_ontology:measurement(nad__be_t9, nad_precursor_bioavailability, base_extractiveness, 9, 0.63).
narrative_ontology:measurement(nad__be_t12, nad_precursor_bioavailability, base_extractiveness, 12, 0.66).
narrative_ontology:measurement(nad__be_t15, nad_precursor_bioavailability, base_extractiveness, 15, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(nad__su_t0, nad_precursor_bioavailability, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(nad__su_t3, nad_precursor_bioavailability, suppression_requirement, 3, 0.58).
narrative_ontology:measurement(nad__su_t6, nad_precursor_bioavailability, suppression_requirement, 6, 0.63).
narrative_ontology:measurement(nad__su_t9, nad_precursor_bioavailability, suppression_requirement, 9, 0.67).
narrative_ontology:measurement(nad__su_t12, nad_precursor_bioavailability, suppression_requirement, 12, 0.69).
narrative_ontology:measurement(nad__su_t15, nad_precursor_bioavailability, suppression_requirement, 15, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nad_precursor_bioavailability, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is downstream of mitochondrial_demand_signal_deficiency. The upstream constraint describes the mechanistic gap (NAD+ supply without demand signals); this constraint describes the commercial extraction enabled by that gap. They form a constraint family where the mechanistic understanding (tangled_rope) enables the commercial structure (snare).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
