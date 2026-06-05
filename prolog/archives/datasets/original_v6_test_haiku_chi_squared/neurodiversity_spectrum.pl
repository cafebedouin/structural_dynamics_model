% ============================================================================
% CONSTRAINT STORY: neurodiversity_spectrum
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_neurodiversity_spectrum, []).

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
 *   constraint_id: neurodiversity_spectrum
 *   human_readable: The Social/Medical Model of the Neurodiversity Spectrum
 *   domain: social/medical/healthcare
 *
 * SUMMARY:
 *   The neurodiversity spectrum represents a modern reframing of
 *   neurodevelopmental variation from discrete pathology categories (DSM-IV)
 *   to continuous dimensional variation (DSM-5, ICD-11). Originating in
 *   autism self-advocacy and ADHD communities, the spectrum model asserts
 *   that neurotypes (autism, ADHD, dyslexia, etc.) exist on continua rather
 *   than as binary normal/pathological states, and that many neurodivergent
 *   traits confer strengths alongside challenges. This constraint exhibits a
 *   fundamental structural tension: the spectrum model operates within and
 *   depends upon the same medical-institutional framework (diagnostic
 *   systems, insurance gatekeeping, pharmaceutical regulation) that it claims
 *   to transcend. While the social model of neurodiversity genuinely
 *   validates alternative neurotypes and rejects deficit-only framing, it
 *   remains locked into diagnostic gating for access to accommodations,
 *   funding, and support. The theater ratio has risen from 0.35 to 0.68 over
 *   15 years as the spectrum language has become progressively more
 *   performative: institutions adopt spectrum terminology (inclusive hiring,
 *   neurodiversity initiatives) while maintaining categorical diagnostic
 *   thresholds for access to accommodations. Meanwhile, extractiveness has
 *   climbed from 0.32 to 0.58 as pharmaceutical interests have expanded ADHD
 *   stimulant markets under the banner of spectrum validity, and diagnostic
 *   industries have capitalized on spectrum expansion.
 *
 * KEY AGENTS:
 *   - High-support-needs individuals (powerless/trapped): Primary victims. Trapped by diagnostic gatekeeping and funding tied to formal diagnosis. Cannot access services without clinical label and institutional validation.
 *   - Self-diagnosed and undiagnosed neurodivergent adults (moderate/constrained): Secondary victims. Constrained by diagnostic costs ($2K–$5K), long waitlists (18–36 months), and stigma. Benefit from social model's identity validation but remain excluded from formal support systems.
 *   - Neurodiversity advocacy organizations (institutional/arbitrage): Primary beneficiaries. Gain legitimacy, funding, and platform from spectrum-as-identity framing. Experience the constraint as pure coordination of neurodivergent voices.
 *   - Educational inclusion movement (organized/constrained): Secondary beneficiary with sunset pathway. Organized agents (inclusive schools, UDL advocates) see spectrum model as temporary scaffold toward post-diagnostic inclusion; sunset: universal design replaces diagnosis-dependent accommodations.
 *   - DSM/diagnostic institutional framework (institutional/arbitrage): Institutional actor maintaining performative spectrum language while preserving categorical gating. Sees its own processes as increasingly degraded (piton perspective).
 *   - Pharmaceutical and diagnostic industries (powerful/arbitrage): Powerful beneficiary and coordinator. Benefits from spectrum expansion (broadens market for ADHD medications, diagnostic devices); constrains alternatives (non-pharmaceutical interventions, self-identification pathways).
 *   - Analytical observer (analytical/analytical): Risks naturalizing contingent institutional arrangements as inherent neuroscience
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(neurodiversity_spectrum, 0.58).
domain_priors:suppression_score(neurodiversity_spectrum, 0.62).
domain_priors:theater_ratio(neurodiversity_spectrum, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(neurodiversity_spectrum, extractiveness, 0.58).
narrative_ontology:constraint_metric(neurodiversity_spectrum, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(neurodiversity_spectrum, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(neurodiversity_spectrum, tangled_rope).
narrative_ontology:human_readable(neurodiversity_spectrum, "The Social/Medical Model of the Neurodiversity Spectrum").
narrative_ontology:topic_domain(neurodiversity_spectrum, "social/medical/healthcare").

domain_priors:requires_active_enforcement(neurodiversity_spectrum).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(neurodiversity_spectrum, disability_service_providers).
narrative_ontology:constraint_beneficiary(neurodiversity_spectrum, pharmaceutical_and_diagnostic_industries).
narrative_ontology:constraint_beneficiary(neurodiversity_spectrum, neurodivergent_advocates_institutional).
narrative_ontology:constraint_victim(neurodiversity_spectrum, high_support_needs_individuals).
narrative_ontology:constraint_victim(neurodiversity_spectrum, economically_marginalized_neurodivergent).
narrative_ontology:constraint_victim(neurodiversity_spectrum, non_diagnosed_neurodivergent_adults).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HIGH-SUPPORT-NEEDS INDIVIDUAL (SNARE) — Trapped by diagnostic gatekeeping, funding tied to medical model labels, and institutional structures requiring formal diagnoses for accommodations. Cannot exit the diagnostic system without losing access to support. d≈0.98, f(d)≈1.45, σ=1.2 → χ≈1.00.
constraint_indexing:constraint_classification(neurodiversity_spectrum, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SELF-DIAGNOSED/UNDIAGNOSED NEURODIVERGENT ADULT (TANGLED ROPE) — Constrained by cost of clinical diagnosis (avg $2,000–$5,000 USD), long waitlists (18–36 months in many jurisdictions), and stigma. Benefits from social model's validation of alternative neurotypes and self-identification practices. d≈0.72, f(d)≈1.12, σ=1.0 → χ≈0.65.
constraint_indexing:constraint_classification(neurodiversity_spectrum, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: NEURODIVERSITY ADVOCACY ORGANIZATION (ROPE) — Benefits from the social model framework through institutional legitimacy, funding, and platform. Experiences the constraint as pure coordination: communicating neurodiversity's strengths enables peer recognition and policy reform. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.07.
constraint_indexing:constraint_classification(neurodiversity_spectrum, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: EDUCATIONAL INCLUSION MOVEMENT (SCAFFOLD) — Organized agents (inclusive schools, universal design for learning advocates) see the spectrum model as temporary scaffolding toward a future where neurodiversity is normalized without diagnostic gatekeeping. Currently constrained by special education infrastructure tied to medical model labels; sunset path: universal design replaces diagnosis-dependent accommodations in 10–20 years. d≈0.38, f(d)≈0.38, σ=1.0 → χ≈0.22.
constraint_indexing:constraint_classification(neurodiversity_spectrum, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: DSM INSTITUTIONAL FRAMEWORK (PITON) — The DSM-5's spectrum model (particularly for autism) is largely performative: it maintains diagnostic utility for insurance/funding purposes while paying lip service to dimensional variation. The spectrum replaces discrete categories with continuous scales, but still requires categorical diagnostic thresholds for access to services. theater_ratio=0.68 reflects this tension: the spectrum language is progressive but the underlying gating mechanism is unchanged. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.04.
constraint_indexing:constraint_classification(neurodiversity_spectrum, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: PHARMACEUTICAL AND DIAGNOSTIC INDUSTRY (TANGLED ROPE) — Powerful actor that both coordinates medical knowledge production and extracts rent through diagnostic device licensing, medication marketing, and test availability. Benefits from spectrum expansion (broadens patient population for treatments like stimulant medication for ADHD). Constrains alternatives (e.g., non-pharmaceutical interventions, self-identification pathways without clinical validation) through regulatory capture and evidence standards. d≈0.25, f(d)≈0.15, σ=1.2 → χ≈0.10.
constraint_indexing:constraint_classification(neurodiversity_spectrum, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a neuroscience perspective, neurodiversity is an irreducible biological fact: human brains vary across multiple cognitive dimensions (processing speed, working memory capacity, sensory sensitivity, attention regulation), and these variations are not pathological but distributive. The spectrum model reflects this natural variation. However, the structural data (ε=0.58, suppression=0.62, theater=0.68) contradicts the mountain classification — the 'neurodiversity is natural' framing naturalizes contingent medical/institutional arrangements.
constraint_indexing:constraint_classification(neurodiversity_spectrum, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(neurodiversity_spectrum_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(neurodiversity_spectrum, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(neurodiversity_spectrum, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(neurodiversity_spectrum, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(neurodiversity_spectrum, TR),
    TR >= 0.70.

:- end_tests(neurodiversity_spectrum_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts from high-support-needs individuals through diagnostic gatekeeping (access to accommodations requires formal diagnosis, which requires wealth and time). It also extracts from self-identified neurodivergent adults who cannot access services without clinical validation. However, the extraction is not maximal because: (a) advocacy organizations have successfully created alternative community support structures (online communities, peer mentorship); (b) some jurisdictions have loosened diagnostic requirements; (c) the pharmaceutical industry's extraction is more complex — it both enables and constrains access. The measured 0.58 reflects the increasing rent-seeking (pharmaceutical marketing, diagnostic expansion) layered atop the coordination function (medical model validation). Suppression (0.62): High. Significant barriers include: (a) diagnostic gatekeeping (cost, waitlists, clinician expertise gaps); (b) institutional requirement for formal diagnosis to access accommodations; (c) stigma and risk of pathologization; (d) regulatory capture by pharmaceutical interests shaping diagnostic criteria; (e) false choice between affirming neurodiversity and accessing medical support. However, suppression is not maximal (not 0.80+) because: (a) self-identification communities exist; (b) some accommodations (workplace flexibility, educational modifications) are becoming uncoupled from diagnosis; (c) policy momentum is shifting toward universal design reducing diagnosis-dependency. Theater ratio (0.68): High and rising. The spectrum model language is progressively performative: organizations adopt 'neuro-inclusive' branding without changing diagnostic gating; DSM-5 uses spectrum language while maintaining categorical thresholds; pharmaceutical companies market ADHD medications using neurodiversity-affirming language while expanding market scope. The theater ratio reflects the growing gap between the social model's egalitarian framing and the medical model's extraction mechanism.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a fundamental perspectival inversion: the high-support-needs individual (powerless/trapped) sees a Snare — locked into the diagnostic system with no exit. The advocacy organization (institutional/arbitrage) sees a Rope — the spectrum model validates neurodiversity and enables peer coordination. The industry (powerful/arbitrage) sees a Tangled Rope — they both coordinate medical knowledge production and extract through market expansion. The educational inclusion movement (organized/constrained) sees a Scaffold with a real sunset — universal design is an alternative pathway that will eventually reduce diagnosis-dependency. The DSM framework (institutional/arbitrage) sees its own Piton — the spectrum language is performative while the underlying gating mechanism persists. The analytical observer (analytical/analytical) risks seeing a Mountain (neurodiversity is natural variation) that naturalizes the contingent institutional arrangements. The perspectival gap reveals that the constraint's classification depends entirely on the actor's structural position relative to diagnostic gating and pharmaceutical markets.
 *
 * DIRECTIONALITY LOGIC:
 *   High-support-needs individuals: Victims + trapped → d≈0.98, f(d)≈1.45. Trapped by diagnostic requirements for service access; cannot exit without losing support. Maximum extraction exposure. Self-identified/undiagnosed adults: Victims + constrained → d≈0.72, f(d)≈1.12. Constrained by diagnostic costs and waitlists; can partially exit through self-identification communities but cannot access formal systems. Moderate-high extraction. Neurodiversity advocates (institutional): Beneficiaries + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary through platform and legitimacy; can arbitrage between communities. Educational inclusion movement (organized): Partially constrained by infrastructure tied to diagnosis; partially beneficiary through resources directed to inclusive education. d≈0.38, f(d)≈0.38. DSM framework (institutional): Beneficiary + arbitrage through institutional legitimacy and funding; piton classification from theater gate, not high chi. Pharmaceutical industry (powerful): Beneficiary + arbitrage through market expansion; constrains alternatives through regulatory capture. d≈0.25, f(d)≈0.15. Analytical observer: d≈0.72, f(d)≈1.15. Mountain classification risks naturalizing the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in the neurodiversity spectrum is a contest between two truth claims: (1) 'Neurodiversity is natural variation, not pathology' (social model, suggests Mountain or Rope), and (2) 'The medical and pharmaceutical institutions have captured neurodiversity language for market expansion and diagnostic rent-seeking' (critical political economy view, suggests Snare or Tangled Rope). The JSON resolves this by: (a) accepting both claims as structurally real — the spectrum IS a natural fact of neurological variation, AND the institutional apparatus uses spectrum language to maintain gatekeeping; (b) disaggregating by perspective — the advocacy organization's rope experience and the high-support-needs individual's snare experience are both valid observations of the same structural phenomenon from different positions; (c) identifying the falsifiable distinction: if diagnostic gatekeeping and pharmaceutical extraction were fully decoupled from the spectrum model (e.g., through universal accommodations not requiring diagnosis, or pharmaceutical regulation breaking capture), the constraint would collapse into pure Rope from all perspectives. Until that decoupling occurs, the constraint remains a Tangled Rope at the institutional level and a Snare at the powerless level. The analytical observer's mountain is a false summit — the 'neurodiversity is natural' framing, while true, does not justify the institutional arrangements.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    diagnostic_vs_continuum_boundary,
    'Is the spectrum model a genuine continuum of neurotype variation or a relabeled categorical diagnostic threshold?',
    'Empirical analysis of DSM-5 autism spectrum disorder diagnostic criteria: do clinicians use dimensional scores or revert to categorical diagnosis gates? Study discontinuity in diagnostic practices across the supposed spectrum.',
    'If genuine continuum: spectrum model is Mountain (natural variation, no extraction). If categorical threshold disguised as continuum: model is Piton (performative spectrum language concealing gating mechanism).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(diagnostic_vs_continuum_boundary, empirical, 'Whether the spectrum is a true continuum or a categorical gate disguised as continuous').

omega_variable(
    self_identification_validity_threshold,
    'What level of self-identification validity (without clinical diagnosis) would justify access to accommodations and support services?',
    'Comparative study of outcomes for self-identified vs clinically diagnosed individuals; meta-analysis of false-positive rates in self-identification for specific neurotypes (e.g., ADHD, autism); longitudinal follow-up of undiagnosed individuals seeking accommodations.',
    'If self-identification validity is high (>85% specificity): social model gatekeeping can be substantially reduced, lowering suppression to ~0.30, reclassifying constraint from Snare/Tangled Rope to Rope. If low (<65%): medical model gatekeeping is justified, maintaining current suppression and snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(self_identification_validity_threshold, empirical, 'Threshold for self-identification validity in neurodiversity assessment').

omega_variable(
    universal_design_sufficiency,
    'Can universal design for learning (UDL) and environmental accommodations fully replace diagnosis-dependent support, or do some individuals require diagnosis-indexed interventions?',
    'Randomized controlled trials comparing outcomes under UDL alone vs UDL plus diagnosis-indexed clinical interventions; longitudinal tracking of workplace/educational outcomes with and without diagnostic labels.',
    'If UDL suffices: scaffold sunset is real and near (5–10 years). If diagnosis-indexed interventions outperform: medical model gating persists; scaffold timeline extends (20+ years) or collapses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_design_sufficiency, empirical, 'Whether universal design alone can replace diagnosis-dependent accommodations').

omega_variable(
    extraction_vs_coordination_asymmetry,
    'Is the pharmaceutical and diagnostic industry''s benefit from spectrum expansion a legitimate coordination incentive (better diagnostic tools enable better treatment) or asymmetric extraction (market expansion regardless of population benefit)?',
    'Analysis of pharmaceutical marketing spend vs evidence production; comparison of medication efficacy improvements vs marketing budget growth; study of incentives for off-label prescriptions for ADHD stimulants in high-income populations vs low-income populations.',
    'If coordination: industry role should be integrated into rope or scaffold framework. If extraction: confirms tangled rope or snare classification from industry perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_coordination_asymmetry, empirical, 'Whether pharmaceutical expansion represents coordination or extraction').

omega_variable(
    support_access_equity_distribution,
    'Do medical model gatekeeping and diagnostic cost create inequitable access (high-support-needs individuals in low-income regions go undiagnosed and unsupported)?',
    'Cross-national comparison of diagnosis rates by income quintile and region; longitudinal tracking of support access for diagnosed vs undiagnosed populations with similar support needs; analysis of waitlist duration by SES.',
    'If severe inequity (>3x support access gap between high/low income): snare classification from low-income perspective is confirmed; suppression minimum 0.65. If minimal inequity: model is more rope-like, suppression lowers toward 0.40.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(support_access_equity_distribution, empirical, 'Whether medical gatekeeping creates inequitable support access by SES').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(neurodiversity_spectrum, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(neuro_tr_t0, neurodiversity_spectrum, theater_ratio, 0, 0.35).
narrative_ontology:measurement(neuro_tr_t7, neurodiversity_spectrum, theater_ratio, 7, 0.52).
narrative_ontology:measurement(neuro_tr_t15, neurodiversity_spectrum, theater_ratio, 15, 0.68).

% Extraction over time
narrative_ontology:measurement(neuro_be_t0, neurodiversity_spectrum, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(neuro_be_t7, neurodiversity_spectrum, base_extractiveness, 7, 0.45).
narrative_ontology:measurement(neuro_be_t15, neurodiversity_spectrum, base_extractiveness, 15, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(neurodiversity_spectrum, resource_allocation).
narrative_ontology:affects_constraint(neurodiversity_spectrum, special_education_gatekeeping).
narrative_ontology:affects_constraint(neurodiversity_spectrum, adhd_medication_market_expansion).
narrative_ontology:affects_constraint(neurodiversity_spectrum, diagnostic_cost_barriers).

% DUAL FORMULATION NOTE:
% The neurodiversity spectrum decomposes into three structurally distinct constraints: (1) special_education_gatekeeping (ε≈0.35, national/institutional, Rope with snare from powerless perspective) — the funding structure requiring formal diagnosis; (2) adhd_medication_market_expansion (ε≈0.65, global/pharmaceutical, Snare with rope from pharmaceutical perspective) — the rent-seeking expansion of stimulant medication markets under neurodiversity framing; (3) diagnostic_cost_barriers (ε≈0.48, national, Tangled Rope) — the direct extraction through diagnostic pricing and waitlists. The present story is the overarching coordination/extraction hybrid linking all three. Each sibling story has its own ε and perspectives; this story models them as a family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(neurodiversity_spectrum, powerful, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
