% ============================================================================
% CONSTRAINT STORY: earth_similarity_index_governance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_earth_similarity_index_governance, []).

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
 *   constraint_id: earth_similarity_index_governance
 *   human_readable: Earth Similarity Index Governance and Exoplanet Habitability Classification
 *   domain: astrobiology/planetary_science/research_governance
 *
 * SUMMARY:
 *   The Earth Similarity Index (ESI), developed by the Planetary Habitability
 *   Laboratory at the University of Puerto Rico, ranks exoplanets by
 *   similarity to Earth across four parameters: mean radius, bulk density,
 *   escape velocity, and surface temperature. Since its introduction in 2011,
 *   ESI has become the de facto standard in astrobiology for prioritizing
 *   exoplanet observation targets and framing habitability narratives for the
 *   public and funding bodies. This constraint exhibits a core tension: ESI
 *   provides genuine coordination (aggregates multiple habitability
 *   dimensions enabling comparison across thousands of targets) but
 *   simultaneously concentrates research attention and funding through
 *   institutional authority, creating path dependence that may constrain
 *   exploration of alternative habitability frameworks. The constraint's
 *   extractiveness (0.52) reflects that while the framework performs a
 *   coordination function, its monopoly status enables extraction of research
 *   attention and agenda-setting power. The theater ratio (0.58) indicates
 *   that institutional maintenance of the index increasingly relies on
 *   citation authority and brand recognition rather than demonstrated
 *   superiority over emerging alternatives. Multiple institutional
 *   perspectives reveal different experienced extractiveness: exoplanet teams
 *   see pure coordination (Rope), funding bodies see mixed coordination and
 *   lock-in (Tangled Rope), alternative index developers see a temporary
 *   constraint with an exit path (Scaffold), while the broader biosignature
 *   field experiences extraction disguised as objective ranking (Snare).
 *
 * KEY AGENTS:
 *   - Exoplanet Discovery Teams: Primary beneficiary (institutional/arbitrage) — ESI elevates research visibility, enables competitive proposal positioning, provides citation authority for habitability claims. Can exit to competing indices at minimal cost.
 *   - Astrobiology Funding Bodies: Secondary beneficiary with lock-in (organized/mobile) — ESI coordinates international coordination and public communication, but commitment to ESI-ranked target portfolios creates path dependence. Can exit but faces coordination costs.
 *   - Biosignature Research Community: Primary victim (powerless/trapped) — Locked into ESI framework through citation dependency and funding mechanisms. Cannot meaningfully exit without losing credibility in peer evaluation.
 *   - Emerging Research Programs: Secondary victim (moderate/constrained) — Early-career researchers face constrained mobility due to ESI's gatekeeping effect on resource allocation. Can exit through alternative methods at significant career cost.
 *   - Planetary Habitability Laboratory: Institutional maintainer (institutional/arbitrage) — Maintains index authority through ongoing updates and publication. Benefits from institutional identity fusion with ESI.
 *   - Alternative Index Developers: Potential disruptors (powerful/mobile) — Well-funded researchers developing competing frameworks see ESI monopoly as temporary. High agency in shaping future habitability governance.
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — Risks naturalizing the ESI system as inherent to comparative planetary science rather than contingent institutional arrangement.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(earth_similarity_index_governance, 0.52).
domain_priors:suppression_score(earth_similarity_index_governance, 0.48).
domain_priors:theater_ratio(earth_similarity_index_governance, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(earth_similarity_index_governance, extractiveness, 0.52).
narrative_ontology:constraint_metric(earth_similarity_index_governance, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(earth_similarity_index_governance, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(earth_similarity_index_governance, tangled_rope).
narrative_ontology:human_readable(earth_similarity_index_governance, "Earth Similarity Index Governance and Exoplanet Habitability Classification").
narrative_ontology:topic_domain(earth_similarity_index_governance, "astrobiology/planetary_science/research_governance").

domain_priors:requires_active_enforcement(earth_similarity_index_governance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(earth_similarity_index_governance, exoplanet_discovery_teams).
narrative_ontology:constraint_beneficiary(earth_similarity_index_governance, astrobiology_funding_bodies).
narrative_ontology:constraint_victim(earth_similarity_index_governance, biosignature_research_credibility).
narrative_ontology:constraint_victim(earth_similarity_index_governance, emerging_research_programs).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: BIOSIGNATURE RESEARCH CREDIBILITY (SNARE) — The broader biosignature detection field is locked into the ESI framework through citation dependency and funding mechanisms. Cannot meaningfully exit without losing credibility in peer evaluation. Constrained to justify research through ESI-ranked targets. Maximum extraction from invisible constraint — the indexing system appears objective but determines which research gets resources.
constraint_indexing:constraint_classification(earth_similarity_index_governance, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EMERGING RESEARCH PROGRAMS (TANGLED ROPE) — Early-career researchers and new research groups face constrained mobility. The ESI framework coordinates legitimate comparative astrobiology (genuine benefit) but also extracts through prioritization mechanisms that favor established teams with high-ESI target portfolios. Can exit through alternative methods (e.g., biosignature-first approach) at significant career cost. Mixed extraction and coordination.
constraint_indexing:constraint_classification(earth_similarity_index_governance, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EXOPLANET DISCOVERY TEAMS (ROPE) — Primary beneficiaries experience the ESI framework as coordination: the index aggregates multiple habitability criteria (water, radiation, distance from star) enabling comparison across thousands of discovered exoplanets. Net benefit through citations, mission proposal justification, and research profile elevation. Can exit to alternative indices (PHL Habitable Zone Index variants) at low cost (arbitrage). Experienced as pure coordination benefit.
constraint_indexing:constraint_classification(earth_similarity_index_governance, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ASTROBIOLOGY FUNDING BODIES (TANGLED ROPE) — NASA, ESA, and equivalent agencies benefit from ESI as a coordination mechanism (allocates resources to high-potential targets) but also extract value through standardization. The framework creates path dependence: committed investment in ESI-ranked target observations makes agencies reluctant to adopt competing frameworks. Mobile exit possible (agencies can fund non-ESI targets) but entails coordination costs with international partners and public communication challenges. Active enforcement required to maintain ESI primacy in funding decisions.
constraint_indexing:constraint_classification(earth_similarity_index_governance, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: PLANETARY HABITABILITY LAB (PITON) — The PHL (original ESI authors) maintains the index through ongoing data updates and publication rituals. The institutional maintenance of ESI appears functional but increasingly performative: the index is cited as if it determines research direction (it does), but alternative habitability frameworks exist and serve the same coordination function with lower institutional overhead. The original laboratory sees ESI as their institutional identity, creating inertia even as the functional role has been partly displaced by competing indices. Theater ratio elevated because the index's authority depends partly on tradition and citation patterns rather than demonstrable superior predictive power for biosignature discovery.
constraint_indexing:constraint_classification(earth_similarity_index_governance, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ALTERNATIVE INDEX DEVELOPERS (SCAFFOLD) — Researchers developing competing habitability frameworks (ESI variants, machine-learning approaches, biosignature-weighted metrics) see the ESI monopoly as temporary. The architectural question ('what criteria predict biosignature presence?') is genuinely open, and multiple indices are being tested simultaneously. High power (developed by well-funded groups), mobile exit (can pivot to accepted indices or develop new metrics), and a sunset horizon: as biosignature data accumulates and validates/invalidates specific criteria, weaker indices fade and stronger frameworks emerge. Effective extraction is low because these agents see the constraint as temporary and have agency to shape its outcome.
constraint_indexing:constraint_classification(earth_similarity_index_governance, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, ranking exoplanets by Earth similarity is an inherent requirement of comparative planetary science: any study of 5000+ exoplanets requires some metric to organize investigation. The indexing problem itself is immutable — absent a ranking, the field cannot proceed. However, THIS specific indexing system (Earth Similarity Index by PHL) is contingent. The mountain classification reveals a frame shift: the necessity is the indexing problem, not the ESI solution. The engine's false summit detector identifies that the constraint is actually institutional (ESI's specific authority) not natural law (the indexing requirement).
constraint_indexing:constraint_classification(earth_similarity_index_governance, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(earth_similarity_index_governance_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(earth_similarity_index_governance, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(earth_similarity_index_governance, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(earth_similarity_index_governance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(earth_similarity_index_governance, TR),
    TR >= 0.70.

:- end_tests(earth_similarity_index_governance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The ESI framework coordinates genuine astrophysical comparison (water availability, stellar radiation, planetary mass/composition) enabling systematic study of 5000+ exoplanets. However, the index's monopoly status extracts research agenda-setting power: the concentration of observation resources on high-ESI targets limits exploration of targets selected by alternative criteria (extremophile habitats, targets with unique geochemistry, rare binary systems). The growth trajectory (0.28→0.52 over ten years) reflects increasing institutional lock-in as alternative indices emerge but fail to displace ESI. Suppression (0.48): Moderate. Barriers to alternative index adoption include: citation momentum (ESI-ranked targets appear in thousands of publications), integration into proposal systems (major funding agencies list ESI scores in target selection), and public communication (ESI scores are media-friendly and understandable). But suppression is not total—alternative metrics exist and are being actively developed. Theater ratio (0.58): Moderate-high. The index's authority increasingly depends on institutional brand and citation patterns rather than demonstrated predictive superiority. ESI's continued dominance despite emergence of competing frameworks indicates performative maintenance. As biosignature data accumulates from JWST observations, the index's real predictive power can be measured directly; preliminary data suggests alternative metrics may equal or exceed ESI for certain target classes, yet ESI maintains research agenda dominance through institutional momentum.
 *
 * PERSPECTIVAL GAP:
 *   The most significant perspectival gap lies between the exoplanet discovery teams (pure beneficiary, Rope classification) and the broader biosignature research community (pure victim, Snare classification) that share identical spatial scope (global) but opposite structural relationships to the constraint. The gap reveals how institutional authority distributes extraction: those who proposed and maintain the index experience coordination, while those who must justify research through the index experience constraint. The scaffold perspective (alternative developers) is empirically important—it indicates a sunset mechanism if and only if the alternative indices gain demonstrable performance superiority and institutional adoption, but it currently represents aspirational rather than structural status.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary directionality derives from exoplanet teams and funding bodies that gain citation advantage, visibility, and research profile elevation from ESI's canonical status. These agents experience low or negative d values (they are net extractors), producing low or negative χ via f(d). Their institutional power and arbitrage exit options amplify their beneficiary status. Victim directionality derives from biosignature research programs and emerging researchers whose access to resources depends on ESI ranking and citation legitimacy. Trapped or constrained exit options (career dependency on established frameworks) produce high d values, increasing experienced χ. The piton classification emerges from the theater gate, not from direct χ—the index's institutional maintenance depends increasingly on performative maintenance (citation authority, brand recognition) rather than demonstrated functional superiority. Funding bodies occupy a middle position with strong power (organized) but mobile exit (capable of funding non-ESI targets), producing moderate d that generates the tangled_rope classification.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids mandatrophy by refusing to collapse all perspectives into a single 'true' type. The mandatrophy would arise if we claimed that 'ESI is really just pure coordination' (Rope everywhere) or 'ESI is really just extraction' (Snare everywhere). Instead, the constraint's structure generates legitimately different types from different positions. The beneficiary sees coordination; the victim sees extraction; the alternative developer sees a sunset mechanism; the institutional maintainer sees their identity-fusion (approaching Piton); the analytical observer can mistake institutional contingency for natural law (false summit). The mandatrophy is resolved by accepting that ESI performs both coordination and extraction functions simultaneously, and that agents experience different aspects of that duality based on their structural position. The institutional lock-in omega (institutional momentum vs. performance-driven authority) is the critical resolution point: if lock-in dominates, more perspectives classify as Snare or Piton; if performance dominates, more perspectives classify as Rope, and the alternative-index scaffold has lower credibility.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    esi_predictive_validity,
    'Does the Earth Similarity Index actually predict biosignature detectability better than alternative habitability metrics or simpler proxies (e.g., water presence alone)?',
    'Longitudinal tracking of biosignature confirmation rates for high-ESI targets vs. targets selected by competing indices; measurement of prediction accuracy as real biosignature data accumulates from JWST and next-generation missions',
    'If ESI outperforms: the framework''s authority is justified and extraction is minimized (coordinates genuine advantage). If alternatives are equal or superior: ESI''s monopoly is purely institutional (extraction increases, classification shifts toward Snare from more perspectives).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(esi_predictive_validity, empirical, 'Whether ESI predictively outperforms competing habitability metrics').

omega_variable(
    alternative_index_adoption_trajectory,
    'Are emerging habitability indices (machine-learning trained on biosignature analogues, biosignature-weighted variants, context-specific metrics for extremophiles) actually displacing ESI in funding and publication, or is the ESI monopoly strengthening?',
    'Citation analysis over 5-year windows; tracking of exoplanet target selection in major mission proposals (JWST programs, future flagship missions); survey of early-career researcher index adoption',
    'If alternatives are displacing: the scaffold perspective is structural (sunset is real, extraction is temporary). If ESI monopoly strengthens: the constraint is locking in institutional inertia (extraction increases, Snare classification becomes dominant).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_index_adoption_trajectory, empirical, 'Whether alternative habitability indices are displacing ESI in practice').

omega_variable(
    biosignature_discovery_bias_mechanism,
    'Does the concentration of observations on high-ESI targets create a self-fulfilling prophecy (biosignatures more likely to be detected where we look hardest) or does it genuinely improve detection probability through better target selection?',
    'Comparison of biosignature detection rates (biosignature-positive confirmed observations / total observations) for high-ESI vs. low-ESI targets observed with comparable instrumentation; separation of observation bias from target selection quality',
    'If self-fulfilling prophecy: ESI drives resources to certain targets but doesn''t improve baseline detection probability (extraction increases, coordination value decreases). If genuine improvement: ESI coordinates legitimate advantage (coordination value maintained, extraction justified).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(biosignature_discovery_bias_mechanism, empirical, 'Whether ESI improves biosignature detection or creates observation bias').

omega_variable(
    institutional_lock_in_mechanism,
    'To what extent is ESI''s authority maintained by citation momentum and institutional identity (PHL brand, established user base, integration into proposal systems) versus actual demonstrated superior performance?',
    'Network analysis of citation patterns (citations for methodological justification vs. citations for authority/default); interview data from funding bodies on index selection processes; analysis of proposal success rates for high-ESI vs. alternative-index targets controlling for target properties',
    'If citation momentum dominates: institutional lock-in is a primary extraction mechanism (theater increases, classification approaches Piton). If performance-driven: the index''s authority is justified (extraction decreases, Rope classification dominates).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_lock_in_mechanism, conceptual, 'Whether ESI authority is driven by performance or institutional momentum').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(earth_similarity_index_governance, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(esi_tr_t0, earth_similarity_index_governance, theater_ratio, 0, 0.38).
narrative_ontology:measurement(esi_tr_t5, earth_similarity_index_governance, theater_ratio, 5, 0.48).
narrative_ontology:measurement(esi_tr_t10, earth_similarity_index_governance, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(esi_be_t0, earth_similarity_index_governance, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(esi_be_t5, earth_similarity_index_governance, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(esi_be_t10, earth_similarity_index_governance, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(earth_similarity_index_governance, information_standard).
narrative_ontology:affects_constraint(earth_similarity_index_governance, biosignature_research_targeting).
narrative_ontology:affects_constraint(earth_similarity_index_governance, habitable_zone_boundary_definition).

% DUAL FORMULATION NOTE:
% ESI governance operates at the institutional level (index maintenance, adoption decisions, funding prioritization) and at the research level (target selection, publication authority). The institutional story (this constraint) is upstream of the research-level story on biosignature targeting priorities. The two are linked: institutional monopoly of ESI drives research-level resource concentration.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(earth_similarity_index_governance, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
