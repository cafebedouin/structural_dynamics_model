% ============================================================================
% CONSTRAINT STORY: germline_regulation_threshold_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_germline_regulation_threshold_2026, []).

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
 *   constraint_id: germline_regulation_threshold_2026
 *   human_readable: International Germline Editing Regulatory Threshold
 *   domain: political/technological
 *
 * SUMMARY:
 *   The international regulatory moratorium on human germline editing
 *   represents a complex constraint that operates simultaneously as
 *   legitimate coordination, temporary scaffolding with an implicit sunset,
 *   degraded institutional ritual, and asymmetric extraction of research
 *   opportunity. The constraint crystallized around the 2015 NIH moratorium
 *   and WHO statements, formalized through national legislation (US, EU,
 *   Canada, Australia) and international consensus statements, yet operates
 *   imperfectly across jurisdictions with Singapore, UK, and China
 *   maintaining licensing frameworks for 'responsible research.' The tension
 *   arises between precautionary ethics (preventing unknown risks to future
 *   populations) and therapeutic opportunity (eliminating genetic disease
 *   from the human germline). Base extractiveness has risen from 0.35 to 0.52
 *   over the measurement interval as off-target editing rates have improved,
 *   making the research prohibition increasingly costly in foregone
 *   therapeutic capability. Theater ratio has risen from 0.48 to 0.64 as
 *   regulatory statements accumulate while actual enforcement diverges across
 *   jurisdictions—the constraint maintains consensus performance through
 *   repeated international statements while the underlying mechanism
 *   (enforced restriction on research) becomes theatrically maintained rather
 *   than functionally effective.
 *
 * KEY AGENTS:
 *   - Germline Editing Researchers: Primary victim (powerless/trapped) — career paths blocked; no appeal mechanism for therapeutic applications. Cannot exit globally applicable restrictions.
 *   - Disease-Bearing Families: Primary victim (moderate/constrained) — blocked from access to potential germline therapies; constrained by regulatory prohibitions and waiting lists.
 *   - Incumbent Pharmaceutical Firms: Primary beneficiary (institutional/arbitrage) — moratorium protects somatic therapy markets and R&D focus; captures first-mover advantage in validated therapeutic pathways.
 *   - Precautionary Ethics Community: Secondary beneficiary (organized/mobile) — moratorium aligns with risk-averse governance philosophy; maintains influence over research agenda.
 *   - Regulatory Apparatus (WHO, national agencies): Tertiary beneficiary/performer (institutional/arbitrage) — moratorium concentrates governance authority; maintains coordination narrative.
 *   - Emerging Research Community: Secondary victim (organized/mobile) — young researchers in permissive jurisdictions benefit from regulatory clarity but face global reputational constraints.
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing institutional risk allocation as inherent technological constraint.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(germline_regulation_threshold_2026, 0.52).
domain_priors:suppression_score(germline_regulation_threshold_2026, 0.68).
domain_priors:theater_ratio(germline_regulation_threshold_2026, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(germline_regulation_threshold_2026, extractiveness, 0.52).
narrative_ontology:constraint_metric(germline_regulation_threshold_2026, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(germline_regulation_threshold_2026, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(germline_regulation_threshold_2026, tangled_rope).
narrative_ontology:human_readable(germline_regulation_threshold_2026, "International Germline Editing Regulatory Threshold").
narrative_ontology:topic_domain(germline_regulation_threshold_2026, "political/technological").

domain_priors:requires_active_enforcement(germline_regulation_threshold_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(germline_regulation_threshold_2026, incumbent_biotech_firms).
narrative_ontology:constraint_beneficiary(germline_regulation_threshold_2026, regulatory_bodies).
narrative_ontology:constraint_beneficiary(germline_regulation_threshold_2026, precautionary_ethics_community).
narrative_ontology:constraint_victim(germline_regulation_threshold_2026, germline_editing_researchers).
narrative_ontology:constraint_victim(germline_regulation_threshold_2026, disease_elimination_capability).
narrative_ontology:constraint_victim(germline_regulation_threshold_2026, patients_with_genetic_disease).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GERMLINE RESEARCHER (SNARE) — Scientists in restrictive jurisdictions cannot pursue legitimate disease-elimination research. Career path blocked. No appeal mechanism for therapeutic applications. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.86.
constraint_indexing:constraint_classification(germline_regulation_threshold_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DISEASE-BEARING FAMILY (TANGLED ROPE) — Constrained by regulatory prohibitions and organ donation waiting lists. Indirectly benefits from disease elimination research when it occurs, but blocked from access to germline therapies. d≈0.78, f(d)≈1.12, σ=1.2 → χ≈0.59.
constraint_indexing:constraint_classification(germline_regulation_threshold_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: INCUMBENT PHARMACEUTICAL FIRMS (ROPE) — Moratorium protects somatic therapy markets and prevents germline competition. Experiences constraint as coordination of R&D focus and market structure. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.06.
constraint_indexing:constraint_classification(germline_regulation_threshold_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: BIOETHICS & PRECAUTIONARY COALITION (SCAFFOLD) — Organized precautionary community sees moratorium as temporary coordination: somatic therapies mature, off-target effects reduce, regulatory frameworks evolve. Sunset implicit in the phrase 'until safety established.' d≈0.35, f(d)≈0.35, σ=1.2 → χ≈0.22.
constraint_indexing:constraint_classification(germline_regulation_threshold_2026, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY APPARATUS (PITON) — WHO moratoria, national bans, and international statements on germline editing persist through performative reaffirmation despite declining enforcement capacity and jurisdictional arbitrage (Singapore, UK, China operating under different regimes). theater_ratio≈0.64: regulations exist but are not uniformly enforced. Regulatory system sees itself as degraded — maintaining consensus ritual while actual science operates in regulatory gaps. d≈0.10, f(d)≈-0.08, σ=1.2 → χ≈-0.05.
constraint_indexing:constraint_classification(germline_regulation_threshold_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: EMERGING RESEARCH COMMUNITY (TANGLED ROPE) — Organized young researchers in permissive jurisdictions (UK HFEA licensing, Singapore frameworks) benefit from regulatory clarity and publication access, but constrained globally by reputational and funding restrictions. d≈0.52, f(d)≈0.68, σ=1.2 → χ≈0.43.
constraint_indexing:constraint_classification(germline_regulation_threshold_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From civilizational scale, some constraints on high-risk germline interventions reflect irreducible tradeoffs between innovation speed and population genetic safety. Off-target effects, mosaicism, unknown long-term effects are inherent to current technology — not institutional artifacts. However, structural data (ε=0.52, suppression=0.68) contradicts the mountain classification, revealing this as a false summit: the constraint naturalizes what is partly contingent institutional risk allocation.
constraint_indexing:constraint_classification(germline_regulation_threshold_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(germline_regulation_threshold_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(germline_regulation_threshold_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(germline_regulation_threshold_2026, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(germline_regulation_threshold_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(germline_regulation_threshold_2026, TR),
    TR >= 0.70.

:- end_tests(germline_regulation_threshold_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts significant research opportunity—germline editing capability that would eliminate heritable genetic disease. The extraction is not total (somatic therapies remain available, and some jurisdictions permit licensed research) but represents a meaningful delay in disease elimination capability. The rise from 0.35 to 0.52 reflects improving off-target editing rates, which make the research prohibition increasingly costly in foregone therapeutic benefit. Suppression (0.68): High. Multiple barriers prevent exit: international consensus statements, national legislation, funding restrictions (NIH/EU prohibit germline research funding), publication bias against germline claims in high-impact journals, and reputational costs for researchers pursuing germline work. Yet suppression is not absolute—Singapore and UK licensing create regulatory arbitrage, and preprints/lower-tier journals accept germline research claims. Theater ratio (0.64): Moderate-high. International statements on germline editing moratoria (WHO 2021, Nuffield 2018, National Academies 2020) perform consensus while actual enforcement diverges. The constraint maintains ritual reaffirmation (theater) despite jurisdictional gaps. The rise from 0.48 to 0.64 reflects increasing gap between consensus statements and operational reality across different regulatory regimes.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits a dramatic perspectival gap. The precautionary ethics community (beneficiary) sees a Scaffold—temporary coordination until safety is established, with an implicit sunset as off-target rates improve. The incumbent pharmaceutical industry sees a Rope—coordination of R&D focus that enables somatic therapy markets to mature. The regulatory apparatus sees a Piton—performative consensus statements masking jurisdictional fragmentation and declining enforcement capacity. The germline researcher sees a Snare—arbitrary career prohibition with no appeal mechanism for therapeutic applications. The disease-bearing family sees a Tangled Rope—the constraint both protects them (preventing unknown risks) and harms them (blocking germline disease elimination). The analytical observer risks seeing a Mountain—off-target effects and mosaicism as inherent technological constraints—but the structural data reveals this as a false summit naturalizing what is partly institutional risk allocation. The perspectival gaps reflect genuine disagreement about whether the constraint is temporary (scaffold) or permanent (mountain), whether it is coordinating (rope) or extracting (snare), and whether the boundary between therapeutic and enhancement is stable or unstable.
 *
 * DIRECTIONALITY LOGIC:
 *   Germline researchers: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction—researchers cannot exit global restrictions. Disease-bearing families: Victim + constrained → d≈0.78, f(d)≈1.12. High extraction—therapeutic access is blocked but benefit from research progress when it occurs. Incumbent pharmaceutical firms: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary—can exit into somatic therapy markets and capture regulatory priority. Precautionary ethics community: Beneficiary + mobile → d≈0.35, f(d)≈0.35. Moderate beneficiary—can mobilize through ethics bodies and media, but constrained by scientific momentum toward permissive jurisdictions. Regulatory apparatus: Institutional + arbitrage → d≈0.10, f(d)≈-0.08. Piton classification from theater gate, not from directionality—maintains consensus performance while losing enforcement capacity. Emerging research community: Victim + mobile (organized) → d≈0.52, f(d)≈0.68. Moderate extraction—organized researchers can exit via permissive jurisdictions but face reputational constraints.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy resolves by observing that the constraint legitimately exhibits properties of Tangled Rope at the global level (coordination of precautionary research norms + asymmetric extraction of research opportunity) but decomposes into different types when jurisdictional perspective is specified. The regulatory apparatus maintains the fiction of global moratorium (Piton) while the actual constraint operates as Scaffold (with implicit sunset as safety improves) in permissive jurisdictions and Snare (with no appeals mechanism) in restrictive ones. The false natural law arises when the analytical observer claims off-target editing effects are immutable technical constraints rather than institutional choices about acceptable risk levels. The constraint resolves mandatrophy by acknowledging that 'germline regulation' is not a single constraint but a family of constraints operating at different jurisdictional levels: (1) the precautionary coordination function (Rope/Scaffold at global consensus level), (2) the research extraction mechanism (Snare at researcher level), (3) the therapeutic blocking mechanism (Snare at patient level), and (4) the regulatory theater maintenance (Piton at institutional level). The fact that all six types appear from different structural positions confirms that the indexical classification is working correctly—the constraint is genuinely different from different vantage points.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    therapeutic_vs_enhancement_boundary,
    'Where is the boundary between therapeutic germline intervention and enhancement? Can it be enforced internationally?',
    'Jurisdictional case studies; analysis of approved somatic therapies and their germline analogs; tracking of regulatory drift in UK, Singapore, China frameworks',
    'If boundary is clear and enforceable: moratorium is Mountain or legitimate Rope. If boundary is blurred or unenforceable: moratorium is Snare (arbitrary power allocation). If enhancement pressure grows: Piton (ritual enforcement without mechanism).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(therapeutic_vs_enhancement_boundary, conceptual, 'Therapeutic-enhancement boundary definition and enforceability').

omega_variable(
    off_target_risk_trajectory,
    'What is the actual off-target editing rate trajectory? Will it reach clinically acceptable levels within 5-10 years or require 20+ years?',
    'High-fidelity long-term animal model studies; prospective off-target prediction algorithms; comparative analysis with somatic therapy safety thresholds',
    'If 5-10 years: scaffold sunset is credible, moratorium is temporary coordination. If 20+ years: moratorium operates as longer-term extraction (research delay). If trajectory stalls: moratorium may become permanent snare for researchers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(off_target_risk_trajectory, empirical, 'Off-target editing safety trajectory').

omega_variable(
    regulatory_arbitrage_equilibrium,
    'Will permissive jurisdictions (Singapore, UK, China) create a stable equilibrium of differential research standards, or will competitive pressure force global harmonization toward the most permissive regime?',
    'Tracking of researcher migration patterns; analysis of publication and patent flows; international coordination agreements over next 5-10 years',
    'If equilibrium holds: piton classification correct (moratorium persists in some jurisdictions while others regulate permissively). If harmonization toward permissive: snare breaks (constraint weakens globally). If harmonization toward restrictive: scaffold sunset fails (moratorium hardens).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_arbitrage_equilibrium, empirical, 'Regulatory arbitrage equilibrium dynamics').

omega_variable(
    population_preference_shift,
    'Will public demand for disease elimination capability outpace precautionary ethics consensus as somatic germline therapies prove safe in pilot populations?',
    'International polling on germline intervention acceptance; tracking of patient advocacy coalitions; analysis of media framing shifts in 5-year windows',
    'If demand rises sharply: moratorium shifts from Rope/Scaffold (consensus) to Snare (imposed against preference). If demand stays stable: moratorium retains Rope characteristics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(population_preference_shift, preference, 'Public preference shifts on germline intervention').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(germline_regulation_threshold_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(germline_tr_t0, germline_regulation_threshold_2026, theater_ratio, 0, 0.48).
narrative_ontology:measurement(germline_tr_t5, germline_regulation_threshold_2026, theater_ratio, 5, 0.58).
narrative_ontology:measurement(germline_tr_t10, germline_regulation_threshold_2026, theater_ratio, 10, 0.64).

% Extraction over time
narrative_ontology:measurement(germline_be_t0, germline_regulation_threshold_2026, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(germline_be_t5, germline_regulation_threshold_2026, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(germline_be_t10, germline_regulation_threshold_2026, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(germline_regulation_threshold_2026, enforcement_mechanism).
narrative_ontology:affects_constraint(germline_regulation_threshold_2026, crispr_accuracy_threshold).
narrative_ontology:affects_constraint(germline_regulation_threshold_2026, off_target_effect_characterization).
narrative_ontology:affects_constraint(germline_regulation_threshold_2026, heritable_disease_elimination_capability).

% DUAL FORMULATION NOTE:
% The germline editing moratorium decomposes into multiple structural constraints when viewed at different jurisdictional scales and agent positions. The upstream constraint is the technical off-target editing limitation (constraint_id: off_target_effect_characterization, ε≈0.15, Mountain at technical level), which justifies precautionary moratorium. The moratorium itself (this constraint, ε=0.52, Tangled Rope) then extracts research opportunity asymmetrically across jurisdictions. The downstream constraint is the disease elimination capability gap (constraint_id: heritable_disease_elimination_capability, ε≈0.65, Snare for disease-bearing populations), which arises from the moratorium's asymmetric blocking of therapeutic research.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(germline_regulation_threshold_2026, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
