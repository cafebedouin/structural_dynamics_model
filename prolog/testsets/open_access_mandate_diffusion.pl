% ============================================================================
% CONSTRAINT STORY: open_access_mandate_diffusion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_open_access_mandate_diffusion, []).

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
 *   constraint_id: open_access_mandate_diffusion
 *   human_readable: Open Access Mandate Diffusion Across Academic Institutions
 *   domain: academic_publishing/institutional_policy
 *
 * SUMMARY:
 *   Open Access mandates represent a coordinated institutional effort by
 *   funding agencies and research libraries to break publisher subscription
 *   monopolies and enable public access to publicly-funded research. The
 *   constraint exhibits Tangled Rope structure: genuine coordination function
 *   (enabling public access, reducing library budget pressure) combined with
 *   asymmetric extraction (publishers lose revenue, emerging-economy
 *   institutions face new author-side cost barriers, monograph disciplines
 *   face temporary disadvantage). The mandate diffusion shows initial
 *   extractiveness increase (as APC models proliferate and enforcement costs
 *   mount) with theater ratio rising as institutional compliance becomes
 *   performative (repository deposits that no one reads). Different agents
 *   experience the same structural change completely differently: funding
 *   agencies see pure coordination benefit; publishers see extraction via
 *   policy mandate; research universities see mixed coordination and
 *   enforcement burden; emerging economies see cost displacement rather than
 *   cost reduction.
 *
 * KEY AGENTS:
 *   - Funding Agency (NIH, EU): Primary beneficiary (institutional/arbitrage) — enforces OA mandate without bearing transition costs; gains verification and impact measurement capabilities
 *   - Journal Publisher: Primary victim (powerless/trapped) — loses subscription revenue model with no viable exit; forced to transition to APC or hybrid models; suppression through collective institutional bargaining
 *   - Research University Librarian: Secondary actor (moderate/constrained) — manages complex dual mandate and transition costs; experiences genuine coordination benefit alongside extraction and enforcement burden
 *   - Early-Career OA Researcher: Secondary beneficiary (powerful/mobile) — benefits from increased research visibility; faces minimal suppression; can choose optimal publication venue
 *   - Monograph-Dependent Discipline: Secondary victim (organized/constrained) — faces temporary extraction as APC models are optimized for journals; waiting for open monograph infrastructure
 *   - Emerging Economy Institution: Secondary victim (moderate/constrained) — benefits from access but faces author-side cost barriers and mandate enforcement without funding support
 *   - Institutional Repository System: Institutional actor (institutional/arbitrage) — maintains degraded function through inertia despite decline in actual use
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent publication system changes as inevitable information-technology evolution
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(open_access_mandate_diffusion, 0.52).
domain_priors:suppression_score(open_access_mandate_diffusion, 0.48).
domain_priors:theater_ratio(open_access_mandate_diffusion, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(open_access_mandate_diffusion, extractiveness, 0.52).
narrative_ontology:constraint_metric(open_access_mandate_diffusion, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(open_access_mandate_diffusion, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(open_access_mandate_diffusion, tangled_rope).
narrative_ontology:human_readable(open_access_mandate_diffusion, "Open Access Mandate Diffusion Across Academic Institutions").
narrative_ontology:topic_domain(open_access_mandate_diffusion, "academic_publishing/institutional_policy").

domain_priors:requires_active_enforcement(open_access_mandate_diffusion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(open_access_mandate_diffusion, academic_researchers).
narrative_ontology:constraint_beneficiary(open_access_mandate_diffusion, public_access_beneficiaries).
narrative_ontology:constraint_beneficiary(open_access_mandate_diffusion, funding_agencies).
narrative_ontology:constraint_victim(open_access_mandate_diffusion, journal_publishers).
narrative_ontology:constraint_victim(open_access_mandate_diffusion, subscription_dependent_libraries).
narrative_ontology:constraint_victim(open_access_mandate_diffusion, emerging_institutions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: JOURNAL PUBLISHER (SNARE) — Trapped in declining subscription revenue with no viable exit. Open access mandates eliminate the primary revenue model while enforcement is externalized to institutions. Publisher faces extraction of their rent-seeking model through policy mandate; cannot abandon journal operations without catastrophic sunk cost loss. Maximum suppression through institutional coalitions (JISC, Projekt DEAL) that negotiate collectively, eliminating individual negotiation capacity.
constraint_indexing:constraint_classification(open_access_mandate_diffusion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: RESEARCH UNIVERSITY LIBRARIAN (TANGLED ROPE) — Constrained by dual mandate: serve open access policy goals while maintaining current journal access for researchers. Experiences genuine coordination benefit (OA mandate reduces library budget pressure over time) alongside extraction (must phase out subscriptions while OA uptake is incomplete, creating service gaps). Significant active enforcement burden — must negotiate transition agreements, manage hybrid publishing, and communicate policy to faculty. Exit options constrained by institutional policy commitment and career dependence on policy compliance.
constraint_indexing:constraint_classification(open_access_mandate_diffusion, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FUNDING AGENCY (ROPE) — Benefits from OA mandate enforcement as a coordination mechanism: ensures research output is publicly accessible, enabling verification and impact measurement. Agency uses mandate as enforcement lever without bearing transition costs. Experiences the constraint as pure coordination — they set the policy and receive compliance without negotiation or sacrifice. Maximum beneficiary position with arbitrage options (can shift to other publisher relationships or fund alternative platforms).
constraint_indexing:constraint_classification(open_access_mandate_diffusion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: EARLY-CAREER OA RESEARCHER (ROPE) — Benefits from OA mandate as pure coordination benefit: research is immediately accessible globally, increasing citations and impact without additional effort. Faces minimal suppression — can publish in OA journals or deposit in repositories without significant cost. Mobile exit options (can choose journals, platforms, or repositories without career penalty). Experiences mandate as enabling their own dissemination goals.
constraint_indexing:constraint_classification(open_access_mandate_diffusion, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: MONOGRAPH-DEPENDENT DISCIPLINE (SCAFFOLD) — Humanities and social sciences have slower OA adoption because monographs (not journal articles) are primary publication vehicle. OA mandates create temporary extraction: scholars must maintain dual publishing pathways while OA monograph infrastructure matures. Constraint has sunset logic: university presses are developing open monograph publishing models (Knowledge Unlatched, MUSE, Direct to Open). As infrastructure matures over 5-10 years, extraction declines. Organized resistance (AAUP advocacy) has delayed but not blocked mandate diffusion. Classified as Scaffold because enforcement is declining in strength as alternatives emerge.
constraint_indexing:constraint_classification(open_access_mandate_diffusion, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: LEGACY INSTITUTIONAL REPOSITORY (PITON) — Universities invested heavily in IR infrastructure (DSpace, Fedora instances) for OA deposit. These systems now see declining use as researchers prefer preprint servers (arXiv, bioRxiv) and platform-native OA (Gold OA in journals). The institutional repository persists through inertia — libraries maintain systems to demonstrate OA commitment, but actual research discovery and dissemination happens elsewhere. Theater ratio is high: compliance checking against repositories is performative (IRs are rarely the primary dissemination channel). Piton classification reflects institutional sunk costs maintaining degraded function.
constraint_indexing:constraint_classification(open_access_mandate_diffusion, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: EMERGING ECONOMY INSTITUTION (TANGLED ROPE) — Small, resource-constrained institutions in developing nations benefit from OA access (no subscription fees, research is discoverable) but face extraction costs: OA publishing models shift costs to author-side (APCs), creating barriers for researchers with limited funding. OA mandates from Northern funding agencies enforce policy without accounting for author-side barriers. Constrained exit — must comply with funder mandates to access grants but cannot afford APC-based publishing. Forced to route research through low-cost OA journals or preprints. Active enforcement through grant compliance audits.
constraint_indexing:constraint_classification(open_access_mandate_diffusion, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / INEVITABLE DIFFUSION VIEW (MOUNTAIN) — From civilizational scale, OA mandate diffusion appears inevitable: digital technology makes exclusionary publishing economically irrational; funders have incentive to demand open access; institutional coordination through policy mandates is structural to how modern knowledge governance works. The constraint appears as an immutable natural law of information systems: openness is the lower-entropy state for digital goods. However, this perspective risks false summitry — the persistence of subscription models and author-side extraction via APCs reveals contingent institutional arrangements, not physical law.
constraint_indexing:constraint_classification(open_access_mandate_diffusion, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(open_access_mandate_diffusion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(open_access_mandate_diffusion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(open_access_mandate_diffusion, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(open_access_mandate_diffusion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(open_access_mandate_diffusion, TR),
    TR >= 0.70.

:- end_tests(open_access_mandate_diffusion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, rising over the interval. Initial OA mandates appeared low-extraction (pure policy coordination) but as APC publishing matured and author-side costs became visible, effective extraction increased. The measure captures both subscription extraction (now partly displaced) and APC extraction (now partly centralized). Suppression (0.48): Moderate. Institutional barriers to non-compliance are significant (grant restrictions, evaluation pressure) but not total — researchers can still publish outside OA if accepting career costs. Collective bargaining (Projekt DEAL, JISC) reduces publisher negotiation power but doesn't eliminate choice entirely for some institutions. Theater ratio (0.61): Moderate-high. Significant performative content in repository compliance checking (metrics that count deposits regardless of actual discovery) and APC model implementation (claiming OA while shifting costs). Open access advocates dispute theater measurements, but institutional practice shows substantial gap between compliance and actual researcher behavior change.
 *
 * PERSPECTIVAL GAP:
 *   The mandate exhibits extreme perspectival divergence. Funding agencies see pure coordination (Rope) and claim inevitability (Mountain). Publishers see extraction (Snare) with no exit. Research universities experience mixed effects (Tangled Rope). Monograph-dependent disciplines see temporary harm with eventual relief (Scaffold). Early-career OA researchers see enabling coordination (Rope). Emerging economies see cost displacement (Tangled Rope). The institutional repository system persists as degraded ritual (Piton). These are not measurement errors or framing differences — they reflect genuinely different structural positions within the mandate system. The perspectival gap is the constraint itself.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) varies dramatically by agent power and exit options. Funding agencies (institutional/arbitrage) derive low d (0.15) — they set policy and receive compliance without cost. Publishers (powerless/trapped) derive high d (0.95) — they bear extraction without exit options. Research universities (moderate/constrained) derive moderate-high d (0.65) — they experience both beneficiary and victim roles. The mandate's enforcement capacity (active enforcement flag) is critical: without collective institutional backing, individual publishers retain negotiation power. With collective enforcement, publisher d approaches maximum. Emerging-economy institutions have paradoxical directionality: they benefit from access (low d as beneficiary) but bear author-side costs (high d as victim). The aggregate d for these institutions may be neutral or negative (paying to access + paying to publish) despite policy framing them as beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves potential mandatrophy confusion by showing that OA mandates are GENUINELY both coordination mechanisms AND extraction structures. They coordinate research access (first time in history that publicly-funded research is universally accessible) AND extract value from publishers (eliminate subscription model rent-seeking) AND create new extraction (displace costs to authors, disadvantage institutions without APC funding). The classification as Tangled Rope is not a compromise between Rope and Snare — it's the accurate structural description. Some agents genuinely benefit (funders, open-access researchers), some genuinely bear costs (publishers, emerging-economy authors), some experience both (libraries, universities). The mandate's enforcement creates the Tangled Rope structure: without active institutional enforcement, the coordination would be voluntary (pure Rope). With active enforcement, extraction becomes possible (Tangled Rope emerges). The theater ratio reflects performative compliance: institutions appear to comply with OA policy while actual researcher behavior changes more slowly than aggregate metrics suggest.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    apc_extraction_displacement,
    'Does OA mandate diffusion reduce publishing extraction or merely displace it from subscription to author-side APC models?',
    'Longitudinal cost analysis: total author+institution spending on publishing before vs after OA transition; comparison of aggregate extraction under subscription vs APC models; analysis of who bears cost (wealthy institutions vs early-career researchers vs emerging economies)',
    'If costs are merely displaced: OA mandate is Snare (extraction redistributed rather than reduced). If aggregate costs decrease: mandate is Tangled Rope or Scaffold. If wealthy institutions capture APC publishing while emerging economies lose access: mandate reverses beneficiary/victim roles geographically.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(apc_extraction_displacement, empirical, 'Whether OA mandates reduce or displace publishing extraction').

omega_variable(
    mandate_enforcement_sustainability,
    'Can centralized OA mandate enforcement (Projekt DEAL, JISC, funder policies) be sustained indefinitely without institutional coalition fracture?',
    'Historical analysis of institutional exit from collective bargaining: which institutions break ranks; under what cost pressures; whether enforcement coalitions hold or collapse under publisher counter-offers',
    'If coalitions sustain: Scaffold classification holds (temporary enforcement). If coalitions fracture: mandate diffusion stalls and reverts to Tangled Rope (ongoing extraction persists). If enforcement strengthens: approaches Mountain (irreversible institutional change).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_enforcement_sustainability, empirical, 'Sustainability of centralized OA mandate enforcement coalitions').

omega_variable(
    researcher_behavior_lock_in,
    'Do early-career researchers become lock-in dependent on OA publication for career advancement, creating new extraction mechanism if OA journals gain gatekeeping power?',
    'Career trajectory analysis: correlation between OA publication and funding/promotion outcomes; monitoring for prestige concentration in OA journals (if top-tier journals become predominantly OA, prestige asymmetry reverses)',
    'If OA journals develop gatekeeper power: OA mandate creates new Snare (extraction via OA prestige filters replaces subscription extraction). If career value remains distributed: Rope classification holds (pure coordination benefit).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(researcher_behavior_lock_in, empirical, 'Potential gatekeeping emergence in OA journal tier structure').

omega_variable(
    monograph_publishing_parallel_path,
    'Will open monograph infrastructure mature fast enough to prevent permanent structural disadvantage for monograph-dependent disciplines?',
    'Tracking infrastructure adoption rates (Knowledge Unlatched, Direct to Open, MUSE); cost trajectory for OA monograph publishing; correlation between infrastructure maturation and monograph scholar OA adoption',
    'If infrastructure matures rapidly (3-5 years): Scaffold sunset is real, and generational discipline inequality is temporary. If infrastructure stalls: monograph disciplines remain under Tangled Rope extraction indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(monograph_publishing_parallel_path, empirical, 'Timeline for open monograph infrastructure maturation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(open_access_mandate_diffusion, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(oamd_tr_t0, open_access_mandate_diffusion, theater_ratio, 0, 0.42).
narrative_ontology:measurement(oamd_tr_t3, open_access_mandate_diffusion, theater_ratio, 3, 0.55).
narrative_ontology:measurement(oamd_tr_t6, open_access_mandate_diffusion, theater_ratio, 6, 0.63).
narrative_ontology:measurement(oamd_tr_t10, open_access_mandate_diffusion, theater_ratio, 10, 0.61).

% Extraction over time
narrative_ontology:measurement(oamd_be_t0, open_access_mandate_diffusion, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(oamd_be_t3, open_access_mandate_diffusion, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(oamd_be_t6, open_access_mandate_diffusion, base_extractiveness, 6, 0.54).
narrative_ontology:measurement(oamd_be_t10, open_access_mandate_diffusion, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(open_access_mandate_diffusion, information_standard).
narrative_ontology:affects_constraint(open_access_mandate_diffusion, scholarly_journal_subscription_model).
narrative_ontology:affects_constraint(open_access_mandate_diffusion, research_evaluation_metrics_gaming).
narrative_ontology:affects_constraint(open_access_mandate_diffusion, global_knowledge_inequality).

% DUAL FORMULATION NOTE:
% OA mandate diffusion should be decomposed into two structurally distinct constraints: (1) subscription_model_disruption (ε≈0.65, Snare for publishers) and (2) apc_publishing_emergence (ε≈0.48, Tangled Rope as emerging author-side extraction). Current story treats them as unified constraint affecting all agents symmetrically. Future work should separate these with distinct beneficiary/victim profiles and interval measurements tracking the cost displacement trajectory.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(open_access_mandate_diffusion, institutional, 0.15).
constraint_indexing:directionality_override(open_access_mandate_diffusion, moderate, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
