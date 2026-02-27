% ============================================================================
% CONSTRAINT STORY: polar_bear_biobanking
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_polar_bear_biobanking, []).

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
 *   constraint_id: polar_bear_biobanking
 *   human_readable: Polar Bear Genetic Biobanking as a Climate Change Hedge
 *   domain: technological/environmental/political
 *
 * SUMMARY:
 *   Polar bear biobanking represents the creation of a genetic archive and
 *   de-extinction capability as a response to climate-driven population
 *   collapse. The policy frames extinction as inevitable (or at least
 *   probable) and proposes technological insurance: if wild polar bears
 *   disappear, their genetic legacy can be stored and potentially reanimated
 *   via synthetic reproduction in a future technological regime. This
 *   constraint exhibits a structural pattern of extraction masked by
 *   conservation rhetoric. The beneficiaries — biotechnology firms and
 *   conservation marketing agencies — gain IP rights, funding flows, and
 *   narrative authority. The victims — Arctic indigenous communities and
 *   habitat restoration efforts — experience suppression of their
 *   alternatives: indigenous management authority is displaced by biobank
 *   protocols, and mitigation/adaptation funding is diverted to biotech
 *   infrastructure. The constraint's theater ratio has increased from 0.55 to
 *   0.81 over the 10-year interval, reflecting the growing gap between public
 *   messaging (cutting-edge science preserving species) and actual function
 *   (speculative technology with low near-term probability of success). The
 *   extractiveness has risen in tandem (0.32 → 0.52), indicating that over
 *   time, more of the constraint's operational logic has shifted toward
 *   rent-seeking (biotech IP, funding capture) and away from genuine
 *   coordination. The constraint is a Snare from the perspective of those
 *   excluded from biobanking decisions (indigenous communities, habitat
 *   defenders) and a Rope from the perspective of beneficiaries who frame it
 *   as pure coordination.
 *
 * KEY AGENTS:
 *   - Arctic Indigenous Communities: Primary victim (powerless/trapped) — biobanking displaces traditional management authority; subsistence and cultural rights are subordinated to genetic preservation protocols
 *   - Habitat Restoration Efforts: Primary victim (powerless/trapped) — diverted funding and political attention; de-extinction narrative positions mitigation as inadequate, making restoration work seem less urgent
 *   - Biotechnology Firms: Primary beneficiary (institutional/arbitrage) — capture IP licensing revenue, research funding, and de-extinction technology monopoly
 *   - Conservation Marketing Agencies: Secondary beneficiary (organized/arbitrage) — fundraising narrative of technologically-salvaged species is highly compelling; institutional survival depends on visible action
 *   - Climate-Vulnerable Arctic States: Moderate/constrained — caught between pressure to act on climate/extinction and desire to avoid costly mitigation; biobanking provides appearance of action without commitments
 *   - Conservation Institutions (Piton): Institutional actor (institutional/analytical) — maintain biobanking infrastructure through grant cycles and annual reports; genuine de-extinction research is underfunded relative to storage/sequencing maintenance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(polar_bear_biobanking, 0.52).
domain_priors:suppression_score(polar_bear_biobanking, 0.68).
domain_priors:theater_ratio(polar_bear_biobanking, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(polar_bear_biobanking, extractiveness, 0.52).
narrative_ontology:constraint_metric(polar_bear_biobanking, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(polar_bear_biobanking, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(polar_bear_biobanking, snare).
narrative_ontology:human_readable(polar_bear_biobanking, "Polar Bear Genetic Biobanking as a Climate Change Hedge").
narrative_ontology:topic_domain(polar_bear_biobanking, "technological/environmental/political").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(polar_bear_biobanking, biotechnology_firms).
narrative_ontology:constraint_beneficiary(polar_bear_biobanking, conservation_marketing_agencies).
narrative_ontology:constraint_victim(polar_bear_biobanking, arctic_indigenous_communities).
narrative_ontology:constraint_victim(polar_bear_biobanking, habitat_restoration_efforts).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ARCTIC INDIGENOUS COMMUNITIES (SNARE) — Trapped in declining polar bear populations and habitat loss; biobanking becomes a substitute for addressing root causes (climate mitigation, habitat protection). Indigenous management authority is displaced by biotechnology protocols. Cannot exit without abandoning subsistence rights and cultural practices. d≈0.92, f(d)≈1.39, σ=0.9 → χ≈0.65.
constraint_indexing:constraint_classification(polar_bear_biobanking, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: HABITAT RESTORATION EFFORTS (SNARE) — Trapped: biobanking diverts funding, political will, and institutional attention from the only proven mechanism to preserve living polar bears (sea ice protection, emissions reduction). De-extinction frames the crisis as solvable by technology rather than mitigation. d≈0.90, f(d)≈1.36, σ=1.2 → χ≈0.71.
constraint_indexing:constraint_classification(polar_bear_biobanking, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: CLIMATE-VULNERABLE ARCTIC STATES (TANGLED ROPE) — Constrained by climate impacts and political pressure to 'do something' visible. Biobanking provides a coordination signal (we care, we're taking action) while avoiding costly mitigation commitments. Benefits from the appearance of conservation action; victims to the extent it displaces real adaptation funding. d≈0.58, f(d)≈0.77, σ=1.0 → χ≈0.40.
constraint_indexing:constraint_classification(polar_bear_biobanking, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: BIOTECHNOLOGY FIRMS (ROPE) — Primary beneficiaries. Biobanking creates IP opportunities, de-extinction licensing revenue, and funding streams. Experience the constraint as pure coordination: collecting and storing genetic samples solves collective action problems in species preservation research. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.06. Net beneficiary; negative effective extraction.
constraint_indexing:constraint_classification(polar_bear_biobanking, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: CONSERVATION MARKETING AGENCIES (ROPE) — Secondary beneficiaries. Biobanking is a powerful narrative: 'We're preventing extinction through cutting-edge science.' High fundraising appeal. Coordination function: creates a shared conservation goal. d≈0.10, f(d)≈-0.08, σ=1.2 → χ≈-0.05.
constraint_indexing:constraint_classification(polar_bear_biobanking, rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: CONSERVATION INSTITUTIONS (PITON) — Biobanking persists through institutional inertia and theatrical preservation of the appearance of action. The mechanism is substantially performative: de-extinction remains speculative and under-resourced compared to actual biobank maintenance. theater_ratio=0.81 reflects that most activity is public communication and grant reporting, not functional biology. d≈0.45, f(d)≈0.52, σ=1.2 → χ≈0.22.
constraint_indexing:constraint_classification(polar_bear_biobanking, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — False summit. The framing 'extinction is inevitable, so preservation-by-biobanking is our only option' naturalizes climate change and habitat loss as immutable. But the structural data (ε=0.52, suppression=0.68, theater=0.81) reveals this is a contingent institutional arrangement, not a law of nature. The analytical engine will flag this as false naturalization.
constraint_indexing:constraint_classification(polar_bear_biobanking, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(polar_bear_biobanking_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(polar_bear_biobanking, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(polar_bear_biobanking, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(polar_bear_biobanking, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(polar_bear_biobanking, TR),
    TR >= 0.70.

:- end_tests(polar_bear_biobanking_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts value from habitat restoration and indigenous authority. The extraction is masked by conservation framing (we are 'saving' polar bears), but the net effect is that funds and political priority flow away from proven preservation mechanisms (sea ice protection, emissions reduction, indigenous co-management) toward speculative technology. The 0.52 value reflects that the extraction is real but not absolute — genuine research funding exists within the biobanking ecosystem, and some ecosystem services (genetic data for conservation biology) have non-trivial value. Suppression (0.68): High. Significant barriers to choosing alternative pathways: (1) Climate change appears inevitable in policy discourse, making de-extinction seem necessary. (2) Indigenous management authority is suppressed through institutional protocols that require genetic/biotechnical expertise. (3) Habitat restoration is framed as too slow and too late. (4) Technology optimism narrative suppresses critical questions about feasibility and displacement. Theater ratio (0.81): Very high and increasing. Most activity is performative: public announcements of samples collected, genetic sequencing milestones, promises of future de-extinction capability. Actual de-extinction research is minimal and speculative. The ratio has risen from 0.55 to 0.81 over 10 years because institutional attention has shifted to marketing and grant writing rather than advancing the technical capability.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiaries (biotech firms, conservation NGOs) see the constraint as a Rope: pure coordination that solves the collective action problem of preserving genetic diversity. The victims (indigenous communities, habitat defenders) see it as a Snare: an extraction mechanism that suppresses their authority and diverts resources. Arctic states see a mixed Tangled Rope: they benefit from the appearance of action (funding, international credibility) while bearing some victim costs (divided conservation investment). Conservation institutions see it as a Piton: a degraded ritual maintained by institutional inertia and grant cycles. The analytical observer risks seeing a Mountain (extinction is inevitable, biobanking is our only option) but the structural data reveals this as false naturalization — the climate crisis is human-caused and could be addressed by mitigation; de-extinction is not inevitable because habitat protection is still possible.
 *
 * DIRECTIONALITY LOGIC:
 *   Arctic indigenous communities: Victim + trapped → d≈0.92, f(d)≈1.39. Maximum extraction. Habitat restoration: Victim + trapped → d≈0.90, f(d)≈1.36. Near-maximum extraction. Biotechnology firms: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary; derived d is low because arbitrage exit means they have mobility. Conservation marketing: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.08. Net beneficiary. Arctic states: Both beneficiary (appearance of action) and victim (cost of divided conservation budget) + constrained → d≈0.58, f(d)≈0.77. Mixed extraction. Conservation institutions maintain the constraint through institutional inertia; piton classification reflects theater (performative maintenance), not high directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE OR NATURAL CONSERVATION NECESSITY? The mandatrophy is resolved by establishing that de-extinction is not inevitable because habitat preservation is still possible. If the constraint were truly a response to inevitable extinction (Mountain), then biobanking would be a rational insurance mechanism. But climate change is anthropogenic and addressable — the real choice is between (a) mitigation + habitat protection + indigenous co-management, or (b) biobanking + de-extinction tech as a substitute. The extractiveness comes from the displacement of (a) by (b). The engine detects this by observing that the analytical observer's Mountain perspective (extinction is inevitable) is a false summit: the structural data (high suppression of alternatives, increasing theater ratio, clear beneficiary/victim split) indicates contingent institutional arrangements, not natural law. The snare classification is confirmed by the high d values for indigenous communities and habitat efforts, and the negative d values for beneficiaries.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    de_extinction_technical_feasibility,
    'Is mammalian de-extinction from ancient/degraded DNA technically feasible within the next 50 years, or is it science fiction?',
    'Tracking progress in de-extinction pilot projects (woolly mammoth genome reconstruction, proxy animal cloning); expert assessment of cloning success rates for polar bears specifically; comparison with actual polar bear breeding programs',
    'If feasible: biobanking is insurance (Rope coordination logic strengthens). If infeasible: biobanking is pure theater (Snare/Piton logic dominates; extraction is masked by technological optimism).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(de_extinction_technical_feasibility, empirical, 'Whether de-extinction is technically achievable').

omega_variable(
    funding_displacement_magnitude,
    'To what extent does biobanking funding displace direct habitat protection and climate mitigation funding?',
    'Comparative analysis of funding flows: aggregate conservation budgets pre- and post-biobank policy; tracking of funding reallocation within institutions adopting biobanking; opportunity cost analysis of researcher time',
    'If displacement > 20%: snare/extraction logic is empirically verified. If displacement negligible: biobanking is additive coordination, not extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(funding_displacement_magnitude, empirical, 'Degree of funding displacement from habitat protection').

omega_variable(
    indigenous_authority_erosion,
    'Does biobanking displace or subordinate indigenous management authority over polar bear populations and genetic resources?',
    'Institutional audit: comparison of indigenous decision-making authority in biobanking-present vs biobanking-absent Arctic jurisdictions; analysis of Nagoya Protocol compliance and benefit-sharing agreements; interviews with indigenous management bodies',
    'If authority is displaced: biobanking is extractive relative to indigenous communities (confirmed snare victim status). If indigenous authority is preserved: the power asymmetry is constrained.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(indigenous_authority_erosion, empirical, 'Whether biobanking displaces indigenous management authority').

omega_variable(
    moral_hazard_climate_action,
    'Does biobanking create a moral hazard: does the promise of de-extinction reduce political urgency for climate mitigation?',
    'Comparative analysis of climate policy intensity before/after biobanking announcements; media analysis: tracking of ''backup plan'' framing in climate policy debates; interviews with policy makers on how biobanking affects their assessments of necessary emissions reductions',
    'If moral hazard is significant: biobanking acts as suppression mechanism (reduces political alternatives for habitat protection). If negligible: biobanking is true additive action.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(moral_hazard_climate_action, empirical, 'Whether biobanking reduces climate mitigation urgency').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(polar_bear_biobanking, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pbbank_tr_t0, polar_bear_biobanking, theater_ratio, 0, 0.55).
narrative_ontology:measurement(pbbank_tr_t5, polar_bear_biobanking, theater_ratio, 5, 0.7).
narrative_ontology:measurement(pbbank_tr_t10, polar_bear_biobanking, theater_ratio, 10, 0.81).

% Extraction over time
narrative_ontology:measurement(pbbank_be_t0, polar_bear_biobanking, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(pbbank_be_t5, polar_bear_biobanking, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(pbbank_be_t10, polar_bear_biobanking, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(polar_bear_biobanking, global_infrastructure).
narrative_ontology:affects_constraint(polar_bear_biobanking, arctic_sea_ice_sovereignty).
narrative_ontology:affects_constraint(polar_bear_biobanking, indigenous_genetic_rights_biopiracy).

% DUAL FORMULATION NOTE:
% Polar bear biobanking is downstream of climate change policy failures (arctic_sea_ice_sovereignty covers the root cause). It also instantiates a specific instance of indigenous genetic rights extraction (indigenous_genetic_rights_biopiracy is the broader pattern). The three constraints form a family: climate failure enables extinction risk → extinction risk justifies biobanking → biobanking extracts indigenous genetic authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(polar_bear_biobanking, moderate, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
