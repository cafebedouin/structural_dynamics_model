% ============================================================================
% CONSTRAINT STORY: regulatory_capture_tech_platforms
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_regulatory_capture_tech_platforms, []).

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
 *   constraint_id: regulatory_capture_tech_platforms
 *   human_readable: Regulatory Capture of Technology Platforms by Market Dominance
 *   domain: political_economy/technology_regulation
 *
 * SUMMARY:
 *   Regulatory capture of technology platforms represents a hybrid
 *   coordination-extraction constraint where dominant firms have
 *   systematically captured the regulatory agencies designed to constrain
 *   them. Unlike classical utility monopolies where capture was primarily
 *   about suppressing price regulation, tech platform capture operates across
 *   multiple regulatory domains (antitrust, data privacy, content moderation,
 *   labor standards) with asymmetric technical expertise and jurisdictional
 *   arbitrage. The constraint exhibits all six types depending on observer
 *   position: users experience it as a snare (trapped by network effects),
 *   regulatory agencies as tangled rope (coordinating with platforms while
 *   being captured), platforms themselves as rope (experiencing beneficial
 *   coordination), international coordination bodies as temporary scaffold
 *   with sunset logic, legacy antitrust frameworks as degraded piton, and
 *   civilizational observers risk naturalizing what is actually a contingent
 *   jurisdictional design failure as an immutable law. The theater ratio
 *   (0.68) reflects that formal enforcement activities (antitrust suits,
 *   privacy fines) are substantial in appearance but produce minimal friction
 *   for market leaders due to obsolete regulatory tools and captured
 *   enforcement priorities.
 *
 * KEY AGENTS:
 *   - Dominant Platforms (Meta, Google, Amazon, Apple, Microsoft): Primary beneficiary (institutional/arbitrage) — capture regulatory agencies; arbitrage across jurisdictions; extract regulatory leniency through technical expertise asymmetry and revolving-door influence
 *   - Regulatory Agencies (FTC, FCC, SEC, EU DMA enforcers, national data protection authorities): Mixed target/coordinator (moderate/constrained) — constrained by technical complexity, budget scarcity, and career incentives; also coordinate with platforms on legitimate technical issues; experience capture as asymmetric extraction
 *   - Platform Users and Competitive Entrants: Primary victim (powerless/trapped) — trapped by network effects; cannot exit without total loss of market access; regulation designed to protect them is captured by the firms they depend on
 *   - International Coordination Bodies (EU Digital Markets Act, OECD, multi-stakeholder frameworks): Organized reform actors (organized/constrained) — building alternative regulatory pathways with sunset logic; represent attempt to escape jurisdictional arbitrage
 *   - Legacy Antitrust Enforcers: Institutional degradation (institutional/arbitrage) — maintain pre-digital enforcement doctrines through inertia; enforcement activity is performative rather than functionally constraining; see own regulatory tools as obsolete
 *   - Analytical Observer: Civilizational/universal (analytical/analytical) — risks naturalizing jurisdictional design choice (national regulation) as immutable law of economics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(regulatory_capture_tech_platforms, 0.58).
domain_priors:suppression_score(regulatory_capture_tech_platforms, 0.65).
domain_priors:theater_ratio(regulatory_capture_tech_platforms, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(regulatory_capture_tech_platforms, extractiveness, 0.58).
narrative_ontology:constraint_metric(regulatory_capture_tech_platforms, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(regulatory_capture_tech_platforms, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(regulatory_capture_tech_platforms, tangled_rope).
narrative_ontology:human_readable(regulatory_capture_tech_platforms, "Regulatory Capture of Technology Platforms by Market Dominance").
narrative_ontology:topic_domain(regulatory_capture_tech_platforms, "political_economy/technology_regulation").

domain_priors:requires_active_enforcement(regulatory_capture_tech_platforms).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(regulatory_capture_tech_platforms, dominant_platforms).
narrative_ontology:constraint_victim(regulatory_capture_tech_platforms, regulatory_agencies).
narrative_ontology:constraint_victim(regulatory_capture_tech_platforms, competitive_market_participants).
narrative_ontology:constraint_victim(regulatory_capture_tech_platforms, platform_users).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PLATFORM USERS AND COMPETITORS (SNARE) — Trapped by network effects and switching costs. No viable alternatives; cannot exit without total loss of market access. Regulation designed to protect them is captured by the very firms they depend on. Bears full extraction cost with no alternatives.
constraint_indexing:constraint_classification(regulatory_capture_tech_platforms, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: REGULATORY AGENCIES (TANGLED ROPE) — Constrained by resource scarcity, technical complexity, and revolving-door employment incentives. Also benefit from coordinating with platforms (technical expertise, market intelligence, coordination on emerging threats). Active enforcement required but asymmetric extraction embedded in the coordination relationship.
constraint_indexing:constraint_classification(regulatory_capture_tech_platforms, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DOMINANT PLATFORMS (ROPE) — Arbitrage options: can relocate headquarters, relocate operations, fragment into subsidiary structures, or lobby for favorable regulatory regimes in competing jurisdictions. Experience the constraint as pure coordination: engagement with regulators enables policy alignment and market stability. Net beneficiary.
constraint_indexing:constraint_classification(regulatory_capture_tech_platforms, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INTERNATIONAL COORDINATION BODIES (SCAFFOLD) — Organizations like the Digital Markets Act, international data governance frameworks, and multi-stakeholder forums represent emergent coordination structures with sunset logic. These are temporary scaffolding designed to establish norms and enforcement capacity that will eventually mature into decentralized standard-setting and distributed scrutiny. Organized actors see exit pathways emerging.
constraint_indexing:constraint_classification(regulatory_capture_tech_platforms, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY ANTITRUST FRAMEWORKS (PITON) — Pre-digital antitrust doctrine (price-based tests, consumer harm theories) persists despite loss of functional relevance. Regulatory agencies maintain these frameworks through inertia. Theater ratio high: enforcement activities against tech platforms appear rigorous but produce minimal friction due to remedies designed around obsolete harm categories. The ritual of antitrust enforcement persists; the function has atrophied.
constraint_indexing:constraint_classification(regulatory_capture_tech_platforms, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN CLAIM) — From a civilizational/universal perspective, regulatory capture of tech platforms appears inherent to scale asymmetry: firms operating at continental/global scale necessarily command more resources and expertise than regulatory agencies designed at national scope. This structural mismatch appears as an immutable law. However, the structural data contradicts mountain classification — the engine will identify this as a false summit, revealing that what appears natural (scale asymmetry) is actually contingent (regulatory design choices, jurisdictional coordination capacity, enforcement budget allocation).
constraint_indexing:constraint_classification(regulatory_capture_tech_platforms, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(regulatory_capture_tech_platforms_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(regulatory_capture_tech_platforms, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(regulatory_capture_tech_platforms, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(regulatory_capture_tech_platforms, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(regulatory_capture_tech_platforms, TR),
    TR >= 0.70.

:- end_tests(regulatory_capture_tech_platforms_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate, increasing over the interval. Initially (t=0, ε=0.35) the constraint operated as looser coordination — platforms and agencies were negotiating emerging issues with relatively balanced information asymmetries. Over 10 years, extractiveness increased to 0.58 as platforms accumulated market dominance, agencies lost technical hiring capacity (revolving-door brain drain), and regulatory gaps accumulated (jurisdictional arbitrage, emerging business models outpacing rules). The increase reflects not a change in the structural relationship but a shift in the leverage balance. Suppression (0.65): Moderate-high. Multiple non-overlapping barriers: regulatory complexity (platforms must navigate 100+ jurisdictions), switching costs for users (account data, social graph, payment infrastructure), lack of viable alternatives (no competitor at comparable scale), and resource asymmetry (platforms spend 10x more on regulatory engagement than agencies spend on enforcement). However, suppression is not total — some jurisdictions (EU) have built enforcement capacity; some users migrate to alternative platforms; some staff remain in agencies despite low compensation. Theater ratio (0.68): High and increasing. Formal enforcement activities (FTC antitrust suits, privacy fines totaling billions) are substantial in appearance. However, outcomes are weak: fines are minor (< 5% of annual revenue for market leaders), behavioral remedies are cosmetic (checkbox privacy controls), structural remedies are rare (no breakup orders actually executed). The ritual of enforcement persists; the actual friction on business models has declined. Theater increased from 0.52 (t=0) to 0.68 (t=10) as the gap between enforcement theater and actual constraint on behavior widened.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how a single structural phenomenon (platform dominance + regulatory mismatch) produces contradictory classifications from different observation positions. The beneficiary (platforms) sees rope and experiences it as beneficial coordination. The trapped victim (users) sees snare and experiences maximum extraction. The constrained coordinator (agencies) sees tangled rope with mixed benefits and costs. The organized reform effort (international bodies) sees scaffold with realistic sunset logic. The legacy system sees piton (enforcement theater). The civilizational observer risks seeing mountain (inevitable scale mismatch) but the falseness of this summit is diagnostic: it reveals that what appears 'natural' or 'inevitable' is actually contingent on regulatory design choices. The perspectival gap is not a bug in the classification system — it is the diagnostic signal that reveals the extraction mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's experienced extractiveness chi is computed from their base extraction ε (0.58), directionality d (derived from beneficiary/victim status + exit options), and scope modifier σ(S). Platforms as beneficiaries with arbitrage options derive low d → f(d) ≈ -0.12 → low or negative chi. Users as trapped victims derive high d → f(d) ≈ 1.42 → high chi. Agencies as constrained victims derive moderate-high d → f(d) ≈ 1.0 → moderate chi. The derived directionality values reflect real asymmetries: platforms genuinely benefit; users genuinely bear extraction; agencies experience mixed coordination and constraint. The tangled rope classification at the agency perspective requires three conditions: (a) beneficiary present (platforms benefit), (b) victim present (users and possibly agencies themselves), (c) active enforcement required (yes — regulation of markets requires active authority). All three conditions are met.
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC EXEMPLAR OF PERSPECTIVAL PLURALISM: This constraint resolves mandatrophy by showing that no single classification is 'correct' — the presheaf of perspectives IS the answer. The analytical observer's mountain (scale asymmetry is inevitable) is revealed as a false summit because the structural data shows contingency (regulatory design, jurisdiction coordination, enforcement budget are all choice variables). The beneficiary's rope is their genuine structural experience. The victim's snare is their genuine structural reality. The coordinator's tangled rope captures the actual mixed relationship. The reform body's scaffold is a real structural feature (sunset logic is present). The legacy system's piton is a real observation (theater ratio is high and increasing). Mandatrophy is resolved not by choosing among these but by recognizing that all are structurally valid relative to their observation positions. The constraint illustration reveals why single-position analysis fails: observing from the beneficiary's position produces rope; observing from the victim's position produces snare; the gap between them IS the extraction mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capture_mechanism_directionality,
    'Is regulatory capture primarily extractive (platforms extracting regulatory leniency) or primarily coordination (platforms and agencies mutually optimizing policy)?',
    'Comparative analysis of rule outcomes: regulatory agency proposals vs actual rules; agency budget evolution and staffing (capture-driven under-resourcing vs coordination-driven strategic focus); exit behavior of senior staff (revolving door patterns)',
    'If primarily extractive: snare classification deepens; beneficiaries list becomes more restricted. If primarily coordination: rope classification for agencies gains strength; requires recalibrating directionality upward for agency perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capture_mechanism_directionality, empirical, 'Directionality of regulatory capture: extractive vs coordinative').

omega_variable(
    suppression_internalization_degree,
    'How much of measured suppression (0.65) is structural (regulatory barriers, cost of compliance, jurisdictional fragmentation) vs internalized (agencies have adopted platform-friendly framing, lost capacity to imagine alternatives)?',
    'Post-reform suppression trajectory analysis; institutional memory assessment (can agencies articulate alternative regulatory models?); comparative analysis across jurisdictions with different capture intensity',
    'If mostly structural: barrier removal through policy reform is possible. If heavily internalized: regulatory agencies have developed institutional identity fusion with platform ecosystem; requires identity-frame disruption rather than just policy change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_degree, empirical, 'Proportion of suppression that is structural vs internalized').

omega_variable(
    international_coordination_viability,
    'Can distributed international regulatory coordination (EU, UK, proposed multilateral frameworks) actually escape platform arbitrage, or do platforms exploit jurisdictional gaps faster than coordination mechanisms mature?',
    'Real-time tracking of regulatory arbitrage opportunities vs coordination closure rate; analysis of platform responses to regulatory action (relocation, fragmentation, subsidiary restructuring); maturation timeline of international standards',
    'If coordination viable: scaffold sunset is realistic; extractiveness will decline over 10-20 year horizon. If platforms exploit faster than coordination matures: scaffold is aspirational theater; extractiveness remains high and may increase.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(international_coordination_viability, empirical, 'Whether international coordination can outpace platform arbitrage').

omega_variable(
    user_agency_countervailing_power,
    'Do user-level countervailing mechanisms (data portability, interoperability, migration tools) shift exit_options from trapped to constrained, or do network effects re-trap users in practice?',
    'Empirical tracking of user exit rates post-interoperability mandate; cost-of-migration analysis including switching costs + relationship loss + tool incompatibility; user preference surveys on exit behavior under different friction regimes',
    'If users become genuinely mobile: victim classification weakens; chi values decrease. If network effects persist: trapped status persists regardless of formal portability; suppression remains high.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(user_agency_countervailing_power, empirical, 'Whether data portability shifts user exit options from trapped to constrained').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(regulatory_capture_tech_platforms, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(regcap_tr_t0, regulatory_capture_tech_platforms, theater_ratio, 0, 0.52).
narrative_ontology:measurement(regcap_tr_t5, regulatory_capture_tech_platforms, theater_ratio, 5, 0.62).
narrative_ontology:measurement(regcap_tr_t10, regulatory_capture_tech_platforms, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(regcap_be_t0, regulatory_capture_tech_platforms, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(regcap_be_t5, regulatory_capture_tech_platforms, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(regcap_be_t10, regulatory_capture_tech_platforms, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(regulatory_capture_tech_platforms, enforcement_mechanism).
narrative_ontology:affects_constraint(regulatory_capture_tech_platforms, platform_algorithmic_opacity).
narrative_ontology:affects_constraint(regulatory_capture_tech_platforms, data_portability_interoperability).
narrative_ontology:affects_constraint(regulatory_capture_tech_platforms, content_moderation_accountability).

% DUAL FORMULATION NOTE:
% Regulatory capture operates at multiple constraint levels: (1) at the enforcement mechanism level (this story) where agencies are captured; (2) at the specific policy level where individual rules are written to favor incumbents (separate constraint stories for data privacy rules, antitrust remedies, content moderation standards). This story tracks the meta-level capture of enforcement capacity itself. The downstream stories decompose specific policy domains where capture manifests. All are linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(regulatory_capture_tech_platforms, institutional, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
