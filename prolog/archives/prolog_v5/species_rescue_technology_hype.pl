% ============================================================================
% CONSTRAINT STORY: species_rescue_technology_hype
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_species_rescue_technology_hype, []).

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
 *   constraint_id: species_rescue_technology_hype
 *   human_readable: Species Rescue Technology Hype Cycle
 *   domain: conservation/biotechnology
 *
 * SUMMARY:
 *   Species rescue technology hype represents a structural constraint where
 *   the coordination mechanism (channeling capital and attention toward
 *   conservation through charismatic technological narratives) is inseparable
 *   from an extraction mechanism (redirecting resources away from proven
 *   habitat protection toward speculative interventions with lower real-world
 *   impact). The constraint operates at multiple scales: funding
 *   organizations experience it as enabling their expansion; biotech
 *   companies arbitrage between hype cycles; academic researchers benefit
 *   from high-profile publication opportunities; field conservation NGOs
 *   internalize technology-forward framing as part of organizational
 *   identity; donors experience information asymmetry that suppresses
 *   skepticism; and species themselves experience delayed recovery as
 *   conservation resources flow toward genome editing and synthetic biology
 *   rather than anti-poaching enforcement and habitat protection. The theater
 *   ratio has increased from 0.48 to 0.73 over fifteen years, reflecting the
 *   growing gap between narrative commitments (we will de-extinct the
 *   passenger pigeon, we will engineer climate-resistant corals) and
 *   implemented practice (habitat degradation accelerates despite record
 *   conservation funding). This is a diagnostic case of how coordination
 *   narratives can be captured by extraction mechanisms: the underlying
 *   story—that technology will solve conservation—is coherent and appealing
 *   to funders; it mobilizes capital that might otherwise remain dormant; but
 *   it simultaneously channels that capital away from interventions with
 *   proven efficacy.
 *
 * KEY AGENTS:
 *   - Biotech and Synthetic Biology Companies: Primary beneficiaries (institutional/arbitrage) — capture venture funding, market narratives, and talent pipeline. Can arbitrage across multiple hype cycles.
 *   - Academic Research Institutions: Primary beneficiaries (organized/arbitrage) — researchers gain funding, prestige, and career advancement through high-profile species rescue narratives.
 *   - Conservation Marketing Organizations: Secondary beneficiaries (organized/arbitrage) — funding-dependent nonprofits benefit from public salience of hype narratives; experience constraint as enabling fundraising.
 *   - Endangered Species and Ecosystems: Primary victims (powerless/trapped) — experience delayed recovery as resources redirect toward speculative tech rather than habitat protection.
 *   - Field Conservation Organizations: Secondary victims (powerless/identity_locked) — structurally mobile (could prioritize habitat protection) but identity-fused with technology narratives. Self-identity as conservation innovators prevents recognition of extraction.
 *   - Donor Funding Base: Mixed (moderate/constrained) — constrained by information asymmetry; experiences both coordination (capital mobilized) and extraction (misdirection).
 *   - Policy and Governance Institutions: Complicit maintainers (institutional/constrained) — theater increases; actual enforcement of habitat law atrophies. Piton classification.
 *   - Analytical Observer: Structural witness (analytical/analytical) — sees tangled hybrid mechanism that cannot cleanly separate benefit from cost.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(species_rescue_technology_hype, 0.58).
domain_priors:suppression_score(species_rescue_technology_hype, 0.62).
domain_priors:theater_ratio(species_rescue_technology_hype, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(species_rescue_technology_hype, extractiveness, 0.58).
narrative_ontology:constraint_metric(species_rescue_technology_hype, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(species_rescue_technology_hype, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(species_rescue_technology_hype, tangled_rope).
narrative_ontology:human_readable(species_rescue_technology_hype, "Species Rescue Technology Hype Cycle").
narrative_ontology:topic_domain(species_rescue_technology_hype, "conservation/biotechnology").

domain_priors:requires_active_enforcement(species_rescue_technology_hype).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(species_rescue_technology_hype, biotech_companies).
narrative_ontology:constraint_beneficiary(species_rescue_technology_hype, academic_researchers).
narrative_ontology:constraint_beneficiary(species_rescue_technology_hype, conservation_marketing_orgs).
narrative_ontology:constraint_victim(species_rescue_technology_hype, field_conservation_funding).
narrative_ontology:constraint_victim(species_rescue_technology_hype, habitat_protection_capacity).
narrative_ontology:constraint_victim(species_rescue_technology_hype, endangered_species_actual_recovery).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ENDANGERED SPECIES (SNARE) — Trapped within a system that redirects scarce conservation resources toward speculative technological interventions rather than proven habitat protection. Actual species recovery declines even as hype-driven funding increases. No exit option; bears full cost of delayed and diverted resources. Maximum extraction experienced.
constraint_indexing:constraint_classification(species_rescue_technology_hype, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: FIELD CONSERVATION ORGS (SNARE via identity_locked) — Structurally mobile: could shift funding back to habitat protection, pursue alternative conservation models, or challenge tech narratives. But identity-locked into a conservationist framing that celebrates technological solutions as inevitable progress. Identity fusion with 'cutting-edge' framing prevents recognition that they are reproducing the extraction mechanism. Organizational identity becomes dependent on appearing technologically sophisticated. Exit would require abandoning 'conservation innovator' self-concept.
constraint_indexing:constraint_classification(species_rescue_technology_hype, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 3: DONOR FUNDING BASE (TANGLED ROPE) — Constrained by information asymmetry and desire to believe in solutions. Coordination benefit exists: donors genuinely want to fund effective conservation, and hype channels enthusiasm and capital that might otherwise be dormant. But extraction occurs through misallocation: funds flow to charismatic tech rather than to unsexy habitat work. Constrained exit: donors face costs of reputation damage if they defund high-profile initiatives, but retain some agency in portfolio rebalancing.
constraint_indexing:constraint_classification(species_rescue_technology_hype, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: BIOTECH COMPANIES (ROPE) — Net beneficiaries experiencing the constraint as pure coordination. Hype enables venture funding, attracts talent, and creates market narratives for genome editing, de-extinction, and synthetic biology applications. Companies can arbitrage between hype cycles: move to next charismatic species or next technological narrative when current funding dries up. Extraction flows toward this agent; they experience the constraint as a coordination mechanism enabling their expansion.
constraint_indexing:constraint_classification(species_rescue_technology_hype, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ACADEMIC RESEARCHERS (ROPE) — Organized beneficiaries with arbitrage options. Hype enables grant funding, career advancement through high-profile publications, and access to technology partnerships. Researchers experience the constraint as enabling: the hype cycle funds labs, attracts students, and opens career pathways. Can arbitrage by shifting research focus across hype cycles. Net benefit through career prestige and resource access.
constraint_indexing:constraint_classification(species_rescue_technology_hype, rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: POLICY AND GOVERNANCE (PITON) — Conservation policy rhetoric has internalized 'technology will solve this' framing despite decades of evidence that habitat protection is the primary driver of species recovery. Policy institutions maintain the appearance of technological engagement while actual enforcement of habitat law declines. Theater dominates: species action plans reference genome sequencing and gene drives; actual enforcement of wetland protection statutes atrophies. Piton because the primary function (habitat conservation) has degraded while theatrical commitment (technology narratives) increases.
constraint_indexing:constraint_classification(species_rescue_technology_hype, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — The constraint exhibits genuine coordination: technology funding mobilizes capital and attention that would otherwise be dormant; researchers collaborating across institutions build genuine knowledge infrastructure. But extraction is structurally embedded: the coordination mechanism simultaneously channels resources away from proven interventions (habitat protection, anti-poaching enforcement, anti-fragmentation policy). Benef fit and cost cannot be decoupled; the mechanism is hybrid.
constraint_indexing:constraint_classification(species_rescue_technology_hype, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(species_rescue_technology_hype_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(species_rescue_technology_hype, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(species_rescue_technology_hype, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(species_rescue_technology_hype, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(species_rescue_technology_hype, TR),
    TR >= 0.70.

:- end_tests(species_rescue_technology_hype_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts resources from habitat protection (proven efficacy, unsexy, difficult) toward speculative technology (charismatic, narrative-rich, fundable). The extraction accelerated from 0.35 to 0.58 over the measurement interval, driven by: (1) increasing biotech venture appetite for conservation narratives, (2) academic incentives for novel research over replication of habitat-protection work, (3) donor preference for solutions-oriented narratives. Suppression (0.62): Moderate-high. Multiple barriers prevent exit or redirection: (a) information asymmetry — most donors cannot distinguish speculative from proven interventions; (b) identity lock — organizations have internalized technology narratives; (c) career lock — researchers have built expertise and reputation in hype-aligned domains; (d) policy capture — governance institutions have rhetorically committed to technology pathways and face costs of reversal. Theater ratio (0.68 rising to 0.73): High and increasing. The gap between narrative investment and functional impact widens. Species action plans increasingly reference genome editing and synthetic biology; actual resource allocation to these domains is rising while endangered species continue to decline. The theater-to-function ratio indicates that much conservation discourse is performative.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is maximal. Biotech companies see a pure coordination mechanism (Rope) — hype funds their growth. Academic researchers see career opportunity (Rope) — prestige flows toward novel tech research. Conservation NGOs experience capture via identity-lock (Snare) — they cannot exit because their self-concept depends on appearing innovative. Field practitioners see pure extraction (Snare) — they have evidence that habitat protection works and watch funding flow elsewhere. Policy institutions see their own degradation (Piton) — public commitment to technology narratives alongside atrophying habitat enforcement. Donors see coordination with some misdirection (Tangled Rope) — they benefit from feeling part of a solution while bearing allocation inefficiency. The analytical observer sees a hybrid mechanism (Tangled Rope) where the coordination and extraction cannot be separated: the very narrative that mobilizes capital simultaneously misdirects it.
 *
 * DIRECTIONALITY LOGIC:
 *   Biotech companies and academic institutions experience low d values (beneficiaries with arbitrage options) — they extract via the coordination mechanism but can exit by shifting hype focus. They experience χ as beneficial. Field conservation organizations experience high d (victims despite structural mobility) because their identity frame prevents recognizing exit options. Endangered species experience maximum d (trapped, powerless) — they bear extraction with no alternative. Donors experience intermediate d (constrained by information asymmetry) — they benefit from the coordination narrative (feeling they are funding solutions) but bear extraction through misdirection cost. Policy institutions experience constrained d (cannot reverse course without political cost) — the theater has become institutionalized. The analytical observer recognizes that d differs radically across positions despite observing the same structural phenomenon.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint is genuinely tangled: both rope and snare elements are structurally real. The coordination benefit (hype mobilizes dormant capital; technology research builds knowledge infrastructure) is not theater — it is real. But the extraction mechanism (redirection from proven interventions) is equally real. The false summit is the claim that technology is a substitute for habitat protection rather than a complement. Most beneficiaries (biotech, academia) experience the constraint as enabling their growth (rope), while most victims (species, habitat systems, field practitioners) experience it as pure extraction (snare). The constraint cannot be resolved by choosing one type — it requires managing the hybrid. Mandatrophy resolution requires acknowledging that funding hype and resource misdirection are inseparable faces of the same mechanism. No unitary classification captures the full reality; instead, the presheaf of perspectives at different (P,T,E,S) positions constitutes the complete structural description.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technology_readiness_ambiguity,
    'Are speculative technologies (de-extinction, synthetic populations, gene drives) genuinely intermediate solutions, or are they funding-absorbing dead-ends that will never achieve operational efficacy at scale?',
    '10-20 year longitudinal tracking: compare promised timelines and cost projections at hype peak vs actual implementation outcomes. Track what fraction of announced technologies achieve real-world deployment and species-level impact.',
    'If genuinely intermediate: tangled_rope classification confirmed; extraction coexists with real benefit. If dead-ends: extraction is pure (snare), and the constraint is Goodhart fraud — theater pretending to be function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_readiness_ambiguity, empirical, 'Whether speculative technologies will achieve operational efficacy or remain perpetual dead-ends').

omega_variable(
    counterfactual_funding_redirect,
    'What fraction of biotech/academic species conservation funding would redirect to habitat protection if hype narratives collapsed, vs what would simply evaporate?',
    'Natural experiment analysis: examination of donor behavior when hype cycles collapse (e.g., decline of de-extinction interest post-2024). Surveys of conservation program directors about funding reallocation under scenarios without biotech hype.',
    'If high redirect fraction: constraint is allocation distortion (victims still receive benefits, just misdirected). If most funding evaporates: constraint is net-negative for victims, and suppression of habitat alternatives is real extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_funding_redirect, empirical, 'Counterfactual funding behavior absent hype narratives').

omega_variable(
    field_organizational_identity_fusion,
    'Are field conservation organizations genuinely identity-locked into technology narratives, or do they strategically deploy hype language while maintaining private skepticism about efficacy?',
    'Discourse analysis of private communications (board minutes, internal strategy docs) vs public-facing messaging. Interviews with program directors about stated vs actual conservation priorities when hype pressure is absent.',
    'If genuinely identity-locked: perspective 2 classification (snare via identity_locked) confirmed; unlock requires identity shift. If strategic: organizations have agency and suppression is lower than measured; reclassify to constrained exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(field_organizational_identity_fusion, conceptual, 'Degree of genuine identity fusion vs strategic deployment of hype language').

omega_variable(
    policy_theater_intentionality,
    'Do policy makers genuinely believe technology will substitute for habitat protection, or do they use technology narratives cynically to avoid implementing politically costly habitat law enforcement?',
    'Institutional discourse analysis: compare rhetoric about technology in public hearings vs private policy documents. Longitudinal tracking of budget allocations for habitat enforcement relative to technology investment.',
    'If genuine belief: policy institutions are captured by hype narratives (piton classification valid). If cynical deployment: constraint is deliberately obfuscatory extraction, and classification may shift to snare at the policy level.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_theater_intentionality, conceptual, 'Whether policy theater reflects genuine belief or cynical misdirection').

omega_variable(
    habitat_decline_attribution,
    'What fraction of observed accelerating species decline attributable to resource misdirection via hype cycles vs to exogenous factors (climate, land use, pollution) beyond conservation control?',
    'Species recovery model comparison: simulations of counterfactual funding allocation (habitat protection vs current mix) vs observed outcomes. Control for confounding drivers; isolate conservation-controllable variance.',
    'If high attribution to misdirection: constraint causes measurable harm (victims bear real extraction cost). If low: hype may be epiphenomenal to dominant decline drivers, and suppression metrics should be recalibrated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(habitat_decline_attribution, empirical, 'Attribution of species decline to hype-driven resource misdirection').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(species_rescue_technology_hype, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(srtech_tr_t0, species_rescue_technology_hype, theater_ratio, 0, 0.48).
narrative_ontology:measurement(srtech_tr_t5, species_rescue_technology_hype, theater_ratio, 5, 0.58).
narrative_ontology:measurement(srtech_tr_t10, species_rescue_technology_hype, theater_ratio, 10, 0.68).
narrative_ontology:measurement(srtech_tr_t15, species_rescue_technology_hype, theater_ratio, 15, 0.73).

% Extraction over time
narrative_ontology:measurement(srtech_be_t0, species_rescue_technology_hype, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(srtech_be_t5, species_rescue_technology_hype, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(srtech_be_t10, species_rescue_technology_hype, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(srtech_be_t15, species_rescue_technology_hype, base_extractiveness, 15, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(species_rescue_technology_hype, resource_allocation).
narrative_ontology:affects_constraint(species_rescue_technology_hype, habitat_protection_enforcement).
narrative_ontology:affects_constraint(species_rescue_technology_hype, academic_publication_bias_conservation).
narrative_ontology:affects_constraint(species_rescue_technology_hype, venture_capital_nature_commodification).

% DUAL FORMULATION NOTE:
% This constraint is part of a larger constraint family around conservation funding allocation mechanisms. Upstream: venture_capital_nature_commodification (how biotech financing enters conservation discourse). Downstream: habitat_protection_enforcement (how hype narratives suppress investment in unglamorous but proven interventions). Each story in the family has distinct ε and perspectival structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(species_rescue_technology_hype, institutional, 0.38).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
