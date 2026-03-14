% ============================================================================
% CONSTRAINT STORY: paleoclimate_proxy_substitution
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_paleoclimate_proxy_substitution, []).

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
 *   constraint_id: paleoclimate_proxy_substitution
 *   human_readable: Paleoclimate Proxy Substitution and Data Dominance
 *   domain: paleoclimatology/Earth_science
 *
 * SUMMARY:
 *   Paleoclimate proxy substitution creates a structural constraint in Earth
 *   science research where certain methods for reconstructing past climate
 *   (ice cores, instrumental records) have acquired dominance through
 *   historical accident, funding concentration, and institutional
 *   entrenchment rather than through demonstrated epistemological
 *   superiority. The constraint operates between ice-core-dominant research
 *   groups (primary beneficiaries), alternative proxy communities (primary
 *   victims), and the knowledge commons itself (epistemic victim). The
 *   extractiveness has increased over the measurement interval (0.28 → 0.52)
 *   as ice-core drilling has become more technically sophisticated and
 *   expensive, raising barriers to entry for competing methods. The theater
 *   ratio has also increased (0.42 → 0.68) as paleoclimate consensus
 *   documents (IPCC, consensus syntheses) encode proxy dominance through
 *   performative authority rather than through direct empirical adjudication.
 *   A multi-proxy integration movement is building alternative frameworks
 *   that could sunset this dominance by demonstrating superior skill through
 *   ensemble methods, but these initiatives remain constrained by funding
 *   structures that privilege traditional proxies.
 *
 * KEY AGENTS:
 *   - Ice-Core Laboratories (Greenland, Antarctica): Institutional beneficiaries (institutional/arbitrage) — capture majority of paleoclimate funding, set data standards, define methodological authority
 *   - Alternative Proxy Communities (tree-ring, coral, speleothem researchers): Primary victims (powerless/trapped) — face funding disadvantage, publication bias, methodological subordination with no structural exit
 *   - Paleoclimate Temporal Coverage (the knowledge commons): Secondary victim (powerless/trapped) — gaps in coverage (Antarctic interglacials, short-term variability) cannot be solved by dominant proxy framework; bears cost via systematic bias
 *   - Mid-Sized Regional Proxy Programs: Mixed position (organized/constrained) — experience coordination benefits from shared standards but extraction through resource disadvantage
 *   - PAGES 2k / Multi-Proxy Integration Coalition: Organized agent (organized/constrained) — building alternative frameworks with sunset logic; perceives exit path through methodological maturation
 *   - Paleoclimate Assessment Consensus Bodies (IPCC, syntheses): Institutional actor (institutional/arbitrage) — maintains proxy dominance narratives through performative consensus; benefits from simplified single-proxy messaging
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing institutional dominance as epistemological necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(paleoclimate_proxy_substitution, 0.52).
domain_priors:suppression_score(paleoclimate_proxy_substitution, 0.65).
domain_priors:theater_ratio(paleoclimate_proxy_substitution, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(paleoclimate_proxy_substitution, extractiveness, 0.52).
narrative_ontology:constraint_metric(paleoclimate_proxy_substitution, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(paleoclimate_proxy_substitution, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(paleoclimate_proxy_substitution, tangled_rope).
narrative_ontology:human_readable(paleoclimate_proxy_substitution, "Paleoclimate Proxy Substitution and Data Dominance").
narrative_ontology:topic_domain(paleoclimate_proxy_substitution, "paleoclimatology/Earth_science").

domain_priors:requires_active_enforcement(paleoclimate_proxy_substitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(paleoclimate_proxy_substitution, proxy_dominant_research_groups).
narrative_ontology:constraint_beneficiary(paleoclimate_proxy_substitution, ice_core_laboratories).
narrative_ontology:constraint_victim(paleoclimate_proxy_substitution, alternative_proxy_communities).
narrative_ontology:constraint_victim(paleoclimate_proxy_substitution, paleoclimate_temporal_coverage).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ALTERNATIVE PROXY COMMUNITY (SNARE) — Tree rings, speleothems, coral records, and sediment-based proxies cannot exit the dominance of ice-core and instrumental-record prioritization. Funding, publication space, and citation weight concentrate on dominant proxies. Alternative proxies bear full extractive cost: method development underfunded, results published in lower-tier venues, interpreted through the lens of dominant proxies rather than trusted independently. No coordination benefit — pure subordination.
constraint_indexing:constraint_classification(paleoclimate_proxy_substitution, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PALEOCLIMATE TEMPORAL COVERAGE (SNARE) — The exclusive reliance on certain proxies creates blind spots: ice cores have gaps (Antarctic interglacials, orbital scale transitions); instrumental records only span 170 years; tree rings diverge from temperature in recent centuries. These gaps cannot be solved by the dominant proxy framework itself. Alternative proxies could fill gaps but are suppressed. The knowledge commons bears extraction via systematic bias and reduced temporal coverage — maximum experienced extraction with no escape except methodological revolution.
constraint_indexing:constraint_classification(paleoclimate_proxy_substitution, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: MODERATE PROXY RESEARCH PROGRAMS (TANGLED ROPE) — Regional tree-ring networks, stalagmite chronologies, and marine sediment teams experience genuine coordination benefits (shared calibration standards, climate model benchmarking, paleoclimate datasets) alongside extraction (funding disadvantage vs ice-core labs, pressure to validate dominance, methodological constraints). Exit is expensive (retraining, equipment investment, publication lag) but possible. Mixed structure: coordination that enables but also constrains.
constraint_indexing:constraint_classification(paleoclimate_proxy_substitution, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ICE-CORE LABORATORIES (ROPE) — Primary beneficiaries experiencing the constraint as pure coordination. Ice cores from Greenland and Antarctica directly preserve atmospheric isotopes, trapped gases, and dust — causally precedent to other proxies. High-precision dating, unambiguous climate signal interpretation, and century-to-million-year records create a genuine coordination advantage. These groups see other proxies as supporting or validating ice-core signals, not as competitors. Net beneficiary — extraction flows toward this agent via funding concentration, publication prominence, and the authority to set paleoclimate 'truth.'
constraint_indexing:constraint_classification(paleoclimate_proxy_substitution, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: MULTI-PROXY INTEGRATION COALITION (SCAFFOLD) — Organizations like PAGES 2k, PMIP, and community data archives are building frameworks that treat alternative proxies as structurally coequal rather than subordinate. Organized effort to develop shared calibration metrics, joint uncertainty quantification, and ensemble paleoclimate products. The sunset logic: as multi-proxy integration matures and demonstrates improved skill (better match to instrumental records, reduced systematic biases), the dominance of any single proxy framework loses justification. Estimated sunset: 15-20 years as computational paleoclimatology advances. Low effective extraction because the coalition has collective agency and perceives an exit path.
constraint_indexing:constraint_classification(paleoclimate_proxy_substitution, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: PALEOCLIMATE CONSENSUS NARRATIVES (PITON) — Assessment reports (IPCC, paleoclimate syntheses) encode proxy dominance as established fact through performative consensus-building: ice-core records are presented as the 'ground truth' against which other proxies are evaluated. The narrative persists through institutional inertia and authority citation chains despite limited empirical justification for the hierarchy. Theater ratio is high — the consensus ritual reinforces proxy dominance through rhetorical weight rather than epistemological superiority. The institutional system sees its own narrative as degraded but maintains it because alternatives haven't fully displaced it.
constraint_indexing:constraint_classification(paleoclimate_proxy_substitution, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some proxy selection bias is inherent to paleoclimatology: certain archives (ice cores, tree rings, corals) preserve climate signals more directly than others, and this difference is a natural constraint of geochemistry and dendrochronology. No human institution can escape the fact that some proxies are more reliable than others. However, the structural data contradicts this natural law framing — the dominance hierarchy is not determined by signal quality alone but by funding, publishing power, and historical accident. The engine will flag this as a false summit: naturalization of institutional preference.
constraint_indexing:constraint_classification(paleoclimate_proxy_substitution, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(paleoclimate_proxy_substitution_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(paleoclimate_proxy_substitution, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(paleoclimate_proxy_substitution, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(paleoclimate_proxy_substitution, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(paleoclimate_proxy_substitution, TR),
    TR >= 0.70.

:- end_tests(paleoclimate_proxy_substitution_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high and increasing. Ice-core laboratories capture disproportionate resources, data-setting authority, and publication prominence. However, the extraction is partially justified by genuine technical advantage (direct preservation of atmospheric composition, precise dating, unambiguous climate signal). The increase over time reflects capital-intensification: drilling, isotope analysis, and ice-sheet modeling have become increasingly expensive, raising barriers to entry. The constraint is not pure exploitation but coordination overlaid with asymmetric rent-capture. Suppression (0.65): High. Barriers include funding concentration (ice-core drilling programs receive 30–50% of paleoclimate research budgets), publication bias (high-impact journals overweight ice-core findings), career structure (postdoc training concentrated in ice-core labs), and methodological gatekeeping (ice-core standards used to evaluate alternative proxies rather than multiple proxies evaluated independently). Theater ratio (0.68): High and increasing. Paleoclimate consensus narratives cite ice-core records as 'ground truth' through rhetorical weight and authority chains rather than through direct empirical validation. Consensus documents (IPCC) present proxy hierarchy as established fact despite limited blind-test comparison. The theater has increased as assessment processes have formalized, creating more opportunities for consensus narratives to entrench.
 *
 * PERSPECTIVAL GAP:
 *   The largest perspectival gaps reveal extraction mechanisms. Ice-core labs see Rope (coordination) — we are solving the legitimate problem of reconstructing paleoclimate with maximum precision and temporal coverage. Alternative proxies see Snare (pure extraction) — we are systematically subordinated and cannot exit. The analytical observer at the civilizational scale risks seeing Mountain (natural law) — some proxies are inherently superior — but the structural data reveals this as false: the hierarchy is enforced through institutional mechanisms (funding allocation, publication bias, consensus gatekeeping) that are contingent. The piton perspective (paleoclimate consensus narratives) reveals the performative nature of the dominance: the consensus ritual reproduces proxy hierarchy through rhetorical authority, not through independent empirical adjudication. The scaffold perspective (multi-proxy integration) reveals an organized response with genuine exit logic: as computational paleoclimatology advances and as funding for collaborative data infrastructure improves, single-proxy dominance becomes unjustifiable.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality pipeline computes each agent's relationship to the extraction flow. Ice-core laboratories are beneficiaries with arbitrage options (can exit by pursuing other methods but choose not to because extraction flows toward them via funding). Alternative proxy communities are victims with trapped exit (cannot shift to dominant methods because capital and training are sunk, cannot compete fairly because resource allocation is asymmetric). The powerless classification for alternative proxies is not descriptive of absolute power but constraint-relative: within the paleoclimate funding and publishing ecosystem, these communities cannot exit or negotiate. Mid-sized programs are organized agents with constrained exit (can organize research consortia, share equipment, publish in alternative venues, but face career and resource costs). The organizational capacity differentiates them from trapped agents — they have agency within the constraint even if the constraint's extraction remains asymmetric.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION MECHANISM: The constraint resolves the mandatrophy by revealing that proxy substitution is neither pure coordination (Rope) nor pure extraction (Snare) but a hybrid (Tangled Rope) with a clear sunset clause (Scaffold). The genuine coordination function is climate signal reconstruction — alternative proxies do contribute to paleoclimate knowledge and enable broader spatial-temporal coverage. But the coordination is overlaid with extraction: the contribution of alternative proxies is systematically undervalued, their methods are judged by ice-core standards rather than evaluated independently, and resource allocation is driven by historical precedent and equipment investment rather than by cost-effectiveness analysis. The mandatrophy dissolves when the multi-proxy integration movement demonstrates through blind tests that ensemble reconstructions match or exceed ice-core-dominated reconstructions in skill against independent instrumental benchmarks. At that point, the 'natural law' framing of proxy hierarchy collapses, and the constraint transitions from Snare/Piton toward Rope (true coordination with fair weighting) or dissolves entirely if multi-proxy methods reduce the need for hierarchical substitution. The theater ratio is the leading indicator: as multi-proxy integration matures, theater should decline (consensus narratives become harder to maintain when alternative methods show superior skill), and extractiveness should decline (resource allocation becomes responsive to demonstrated performance rather than locked into historical patterns).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proxy_signal_quality_measure,
    'What metric objectively ranks proxy signal quality against instrumental data? Is such ranking independent of the proxy chosen as reference?',
    'Cross-validation analysis: compare skill scores when each major proxy (ice cores, tree rings, corals, sediments) is used as reference; identify circular reasoning in current hierarchies; develop model-independent quality metrics',
    'If quality is truly ranked by signal properties: ice-core dominance may be justified and not extraction. If ranking depends on reference choice: dominance is conventional, not epistemic, and represents pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proxy_signal_quality_measure, empirical, 'Whether proxy signal quality hierarchy is empirically justified or reference-dependent').

omega_variable(
    alternative_proxy_cost_effectiveness,
    'What is the cost-benefit profile of alternative proxies (tree rings, corals, speleothems) vs ice-core drilling, accounting for temporal coverage, spatial resolution, and funding investment per recovered year of climate data?',
    'Systematic analysis of funding allocation vs data yield; comparison of field campaign costs; accounting for underfunding bias in cost estimates',
    'If alternatives are cost-ineffective at scale: ice-core dominance follows rational resource allocation (Rope). If alternatives are cost-competitive: suppression is driven by convention and funding lock-in (Snare/Tangled Rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_proxy_cost_effectiveness, empirical, 'Cost-effectiveness of alternative proxies relative to ice cores').

omega_variable(
    multi_proxy_skill_convergence,
    'Do ensemble paleoclimate reconstructions using multiple proxies with equal weighting outperform ice-core-dominated single-proxy reconstructions in validation against instrumental records?',
    'Comparative skill testing: RE (reconstruction efficiency), RMSE (root mean square error), correlation with instrumental benchmarks; track skill trends as multi-proxy methods mature',
    'If ensemble skill exceeds dominant-proxy skill: scaffold sunset is empirically grounded, and proxy substitution is a temporary coordination problem. If dominant-proxy skill remains superior: substitution suppression is epistemically justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(multi_proxy_skill_convergence, empirical, 'Whether multi-proxy ensemble skill exceeds dominant-proxy skill').

omega_variable(
    glacial_interglacial_proxy_fidelity,
    'For glacial-interglacial cycles and other orbital-timescale climate changes, do alternative proxies (benthic foraminifera, pollen, alkenones) reproduce ice-core signals independently, or do alternative records show systematic divergence that reveals ice-core bias?',
    'Comparative phase-lag analysis, amplitude comparison, spectral coherence testing across proxy types for the same time intervals; phase reconstruction of orbital forcing',
    'If independent agreement: ice-core dominance reflects genuine signal fidelity (Rope). If systematic divergence: ice-core signal may be compressed or filtered by specific archive properties, and dominance reflects historical artifact (Snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(glacial_interglacial_proxy_fidelity, empirical, 'Whether alternative proxies independently reproduce ice-core signals or show systematic divergence').

omega_variable(
    career_incentive_structural_lock,
    'Does the career path structure (postdocs trained on dominant proxies, labs equipped for dominant methods, publication bias toward dominant results) create path dependence that would persist even if alternative proxies showed superior skill?',
    'Interviews with early-career researchers; analysis of training program curricula; historical case studies of paradigm shifts in paleoclimate methods; comparison with other fields (e.g., paleontology) where method pluralism coexists',
    'If career lock is structural: extraction persists regardless of skill differences, and only generational turnover or external funding shock can break it (Snare/Piton). If lock is responsive to evidence: skill improvements can shift resource allocation (coordination problem solvable by demonstration).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(career_incentive_structural_lock, empirical, 'Whether career structure creates path dependence independent of proxy skill').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(paleoclimate_proxy_substitution, 0, 23).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(paleoclim_tr_t0, paleoclimate_proxy_substitution, theater_ratio, 0, 0.42).
narrative_ontology:measurement(paleoclim_tr_t8, paleoclimate_proxy_substitution, theater_ratio, 8, 0.58).
narrative_ontology:measurement(paleoclim_tr_t15, paleoclimate_proxy_substitution, theater_ratio, 15, 0.68).
narrative_ontology:measurement(paleoclim_tr_t23, paleoclimate_proxy_substitution, theater_ratio, 23, 0.62).

% Extraction over time
narrative_ontology:measurement(paleoclim_be_t0, paleoclimate_proxy_substitution, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(paleoclim_be_t8, paleoclimate_proxy_substitution, base_extractiveness, 8, 0.4).
narrative_ontology:measurement(paleoclim_be_t15, paleoclimate_proxy_substitution, base_extractiveness, 15, 0.52).
narrative_ontology:measurement(paleoclim_be_t23, paleoclimate_proxy_substitution, base_extractiveness, 23, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(paleoclimate_proxy_substitution, information_standard).
narrative_ontology:affects_constraint(paleoclimate_proxy_substitution, paleoclimate_attribution_confidence).
narrative_ontology:affects_constraint(paleoclimate_proxy_substitution, abrupt_climate_change_resolution).

% DUAL FORMULATION NOTE:
% Paleoclimate proxy substitution is upstream of specific paleoclimate claims (e.g., Last Glacial Maximum temperature, Holocene variability magnitude). Each downstream claim inherits the extraction and theater ratio of the proxy substitution constraint. Stories for specific paleoclimate claims should reference this constraint and note how proxy dominance affects confidence bounds.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(paleoclimate_proxy_substitution, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
