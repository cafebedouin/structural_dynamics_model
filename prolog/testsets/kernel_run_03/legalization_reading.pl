% ============================================================================
% CONSTRAINT STORY: legalization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legalization_reading, []).

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
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: legalization_reading
 *   human_readable: Legalization Reading: Drug Control Authority as Third-Party Protection and Harm Reduction
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   The legalization reading frames substance-control authority as justified
 *   by protecting third parties and marginalized communities from
 *   black-market violence and racialized enforcement, treating drug use
 *   itself as an individual choice outside the scope of state constraint.
 *   This reading contests the prohibition reading (which treats drug use as
 *   requiring state intervention to prevent harms to the user themselves) and
 *   coexists with the harm-reduction reading (which treats drug use as
 *   requiring public-health intervention without criminalization). The
 *   legalization reading's core structural claim is that prohibition
 *   enforcement apparatus creates MORE total harm than the drug use it
 *   purports to address — $40+ billion annual transfer to criminal supply
 *   chains, mass incarceration concentrated on marginalized communities,
 *   product adulteration from unregulated supply, violence from supply-chain
 *   competition. The constraint's extractiveness is moderate-to-high (0.58)
 *   because the mechanism for achieving third-party protection
 *   (criminalization of supply, enforcement of prohibition) extracts
 *   significant costs from prosecuted users and small-scale distributors,
 *   disrupts community cohesion through selective enforcement, and diverts
 *   state capacity from treatment infrastructure. However, extractiveness is
 *   lower than the prohibition reading would assign because the legalization
 *   reading explicitly treats harm reduction (not punishment) as the goal,
 *   and proposes mechanisms (legal market competition, regulatory
 *   substitution, treatment access) that could eliminate the extraction
 *   mechanism entirely. Theater ratio declines over the measurement interval
 *   (0.50 → 0.35) because legalization infrastructure reduces performative
 *   legal ritual — police enforcement against small-scale distribution is the
 *   theatrical element, whereas treatment access and regulated market access
 *   are functional.
 *
 * KEY AGENTS:
 *   - Marginalized Communities: Primary beneficiary AND victim (moderate/constrained) — benefit from reduced black-market violence and reduced enforcement targeting; bear extraction through visibility of harm and service-delivery transition gaps
 *   - Third-Party Protectees (Families, Communities Harmed by Black Market): Primary beneficiary and constraint justification (powerless/trapped) — protected from black-market violence but experience collateral enforcement costs
 *   - Prosecuted Users and Street-Level Distributors: Primary victim (powerless/trapped) — bear full cost of criminalization; non-participants in supply-chain violence but subject to enforcement
 *   - Public Health System and Harm Reduction Infrastructure: Institutional beneficiary (institutional/arbitrage) — controls implementation of treatment access and demand-reduction mechanisms
 *   - Law Enforcement, Prosecutors, Prison System: Institutional actor with path dependency (institutional/arbitrage) — possesses enforcement authority but faces institutional inertia in transitioning to harm-reduction alignment
 *   - International Supply-Chain Actors (Production, Cartels, Distribution): Organized targets (organized/constrained) — extraction through legal market competition and regulatory exclusion
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional harm-reduction policy as immutable law of drug control
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legalization_reading, 0.58).
domain_priors:suppression_score(legalization_reading, 0.72).
domain_priors:theater_ratio(legalization_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legalization_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(legalization_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(legalization_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legalization_reading, tangled_rope).
narrative_ontology:human_readable(legalization_reading, "Legalization Reading: Drug Control Authority as Third-Party Protection and Harm Reduction").
narrative_ontology:topic_domain(legalization_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(legalization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legalization_reading, '1f788088-84fb-476d-8654-d247c2c2af91').
narrative_ontology:cs_created_at('1f788088-84fb-476d-8654-d247c2c2af91', '').
narrative_ontology:cs_kernel_codification('1f788088-84fb-476d-8654-d247c2c2af91', formalized).
narrative_ontology:cs_authority_grounding('1f788088-84fb-476d-8654-d247c2c2af91', extraction).
narrative_ontology:cs_interpretation_layer_present('1f788088-84fb-476d-8654-d247c2c2af91').
narrative_ontology:cs_kernel_id(legalization_reading, substance_control_authority).
narrative_ontology:cs_reading_relation('1f788088-84fb-476d-8654-d247c2c2af91', prohibition_reading, coexists_with).
narrative_ontology:cs_reading_relation('1f788088-84fb-476d-8654-d247c2c2af91', harm_reduction_reading, influences).
narrative_ontology:cs_axiom('1f788088-84fb-476d-8654-d247c2c2af91', foundational, drug_use_outside_state_constraint_scope).
narrative_ontology:cs_axiom_status(drug_use_outside_state_constraint_scope, holdable).
narrative_ontology:cs_axiom_grounding('1f788088-84fb-476d-8654-d247c2c2af91', drug_use_outside_state_constraint_scope, deontological).
narrative_ontology:cs_axiom('1f788088-84fb-476d-8654-d247c2c2af91', foundational, prohibition_enforcement_creates_more_harm_than_drug_use).
narrative_ontology:cs_axiom_status(prohibition_enforcement_creates_more_harm_than_drug_use, holdable).
narrative_ontology:cs_axiom_grounding('1f788088-84fb-476d-8654-d247c2c2af91', prohibition_enforcement_creates_more_harm_than_drug_use, empirically_contingent).
narrative_ontology:cs_reference_frame('1f788088-84fb-476d-8654-d247c2c2af91', legal_market_substitution_authority).
narrative_ontology:cs_drift_state('1f788088-84fb-476d-8654-d247c2c2af91', contemporary_post_cannabis_legalization, gap(authority_erosion, substantial, true)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legalization_reading, marginalized_communities).
narrative_ontology:constraint_beneficiary(legalization_reading, third_party_protectees).
narrative_ontology:constraint_beneficiary(legalization_reading, public_health_system).
narrative_ontology:constraint_victim(legalization_reading, prosecuted_users_and_small_distributors).
narrative_ontology:constraint_victim(legalization_reading, enforcement_apparatus_itself).
narrative_ontology:constraint_victim(legalization_reading, state_capacity_diversion).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PROSECUTED USER OR STREET DISTRIBUTOR (SNARE) — Trapped by enforcement mechanisms that criminalize drug use as default response. Bears full cost of incarceration, employment barriers, and family separation. No meaningful exit option; high suppression through criminal justice coercion. Experiences extraction as pure penal harm with minimal coordination function.
constraint_indexing:constraint_classification(legalization_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MARGINALIZED COMMUNITIES (TANGLED ROPE) — Constrained by geography, economic circumstance, and policing intensity. Experience the legalization reading as genuine conflict resolution: reduced black market violence, lower enforcement targeting (as criminalization declines), access to treatment infrastructure instead of incarceration. BUT extraction persists through the transition mechanism itself — communities bear the visibility of harm during decriminalization phase, experience withdrawal of punitive infrastructure before treatment infrastructure fully replaces it. Mixed coordination (harm reduction function) and extraction (temporal mismatch in service delivery).
constraint_indexing:constraint_classification(legalization_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: PUBLIC HEALTH SYSTEM (ROPE) — Institutional beneficiary with exit options (can choose enforcement model, treatment allocation, prevention focus). Legalization reading shifts the constraint to pure coordination: the system's function is to minimize drug-related harms through treatment access, safe supply mechanisms, and demand reduction. Low or no experienced extraction because institutional actor controls the harm-reduction infrastructure that justifies the constraint. Net beneficiary with agency.
constraint_indexing:constraint_classification(legalization_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: THIRD-PARTY PROTECTEES (SNARE) — Trapped in geographies where black market violence creates legitimate harm: overdose deaths, property crime from supply-chain disputes, product adulteration. The legalization reading frames them as primary constraint beneficiaries — the constraint's entire justification is protecting them from black-market extraction. But they experience extraction through the enforcement apparatus itself: heavy-handed policing, collateral detention of family members, ecosystem disruption during transition to legalization. The constraint claims to protect them but extracts costs in the process.
constraint_indexing:constraint_classification(legalization_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 5: ENFORCEMENT APPARATUS (PITON) — Institutional actors with ostensible arbitrage options but path-dependent on enforcement model. From legalization reading perspective, these institutions have become degraded — the enforcement infrastructure persists through inertia despite shifting toward harm reduction as the stated goal. Theater ratio rises here: police maintain drug-enforcement operations even as policy intent shifts to treatment. Continued resource allocation to enforcement despite reduced functional justification. Piton classification reflects institutional inertia protecting bureaucratic territory.
constraint_indexing:constraint_classification(legalization_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: INTERNATIONAL SUPPLY-CHAIN ACTORS (TANGLED ROPE) — Organized but constrained by regulatory environment, enforcement cooperation, and interdiction capacity. Legalization reading frames them as primary extraction targets: the constraint's function includes dismantling profit motives for illegal supply chains through legal market competition and substitution. These actors experience both coordination (with black-market competitors) and extraction (margin erosion, regulatory exclusion). Effective extraction χ is moderate-to-high despite organized power because the regulatory system is explicitly designed to extract rents (taxation) and eliminate competitors (through legality requirements).
constraint_indexing:constraint_classification(legalization_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL-LAW VIEW (MOUNTAIN) — Civilizational perspective treats drug-policy authority as grounded in an immutable harm-minimization principle: the state's role is to minimize total drug-related harm (overdose, violence, adulteration, ecosystem disruption) regardless of mechanism. This perspective sees legalization as the natural law outcome of a correct harm calculus. However, false-summit detection will trigger: identifiable beneficiaries (marginalized communities, public health) and extractive costs (prosecuted users, enforcement apparatus path-dependency) reveal this as a constructed institutional arrangement, not natural law.
constraint_indexing:constraint_classification(legalization_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legalization_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(legalization_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(legalization_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(legalization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(legalization_reading, TR),
    TR >= 0.70.

:- end_tests(legalization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The legalization reading's justification mechanism — replacing criminal supply-chain violence with legal market + regulatory management — appears low-extraction because it treats drug use as outside state constraint scope. However, extractiveness remains substantial because (1) the legalization mechanism still requires enforcement (against remaining illegal supply, against non-compliance with legal frameworks), (2) prosecuted users and small distributors bear significant transition costs during the shift from prohibition to legalization, (3) the state assumes new extraction function through legal-market taxation and regulatory control of demand. The extractiveness is lower than prohibition reading because the legalization reading explicitly constrains enforcement scope and provides treatment alternative, but higher than Rope classification because enforcement costs and transition harms are real. Suppression (0.72): High. The suppression metric captures barriers to exit from the constraint: (1) geographic dependence — communities dependent on supply networks have no exit from drug-supply dynamics; (2) path dependency of enforcement apparatus — law enforcement and prosecutorial institutions have structural incentives toward enforcement maximization; (3) policy lock-in — once legalization framework is adopted, transitioning back to prohibition is politically and institutionally difficult; (4) international supply-chain coordination costs — moving supply to legal channels requires coordinated regulatory change across jurisdictions. Theater ratio (0.35): Low. Legalization reading explicitly privileges functional harm-reduction mechanisms (treatment access, legal market access, third-party protection) over performative legal ritual. The measurement trajectory (0.50 → 0.35) reflects declining theater as legalization infrastructure matures and enforcement theater (arrests of street distributors, prosecution of possession cases) is replaced by treatment referral and regulatory compliance monitoring. Claimed type (Tangled Rope): The constraint has both genuine coordination function (harm reduction for third parties through black-market elimination) AND asymmetric extraction (prosecuted users and enforcement apparatus bear costs of transition). Active enforcement is required — legalization reading does NOT advocate for removal of state authority, but rather redirection of that authority from criminalization toward harm management.
 *
 * PERSPECTIVAL GAP:
 *   The legalization reading produces perspectival gaps among institutional contexts. The public health system (Perspective 3) sees pure Rope — coordination mechanism with no extraction because the health system controls the harm-reduction infrastructure. Law enforcement (Perspective 5) sees Piton — degraded ritual in which enforcement apparatus continues despite declining functional justification as policy intent shifts toward treatment. Prosecuted users (Perspective 1) see Snare — pure extraction with no coordination benefit because criminalization has been identified as harmful rather than protective. Marginalized communities (Perspective 2) see Tangled Rope — the legalization reading treats them as primary beneficiaries (harm reduction, reduced enforcement targeting) but extracts costs through transition mechanism (visibility of harm during decriminalization, service-delivery gaps). The analytical observer risks seeing Mountain — treating the legalization reading's harm-minimization principle as a natural law of drug policy — but structural data reveals this as a false summit: the principle is a chosen framing that benefits public health institutions and marginalized communities while extracting costs from prosecuted users and enforcement apparatus.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for each perspective is derived from agent power, exit options, and beneficiary/victim status. Prosecuted users (powerless/trapped) have high d (≈0.95) because they are victims with no exit: they bear the full cost of criminalization and experience extraction whether the policy is prohibition (criminal liability) or legalization (enforcement asymptotically approaches zero but never reaches it due to residual illegal supply). Public health system (institutional/arbitrage) has low d (≈0.15) because it is a beneficiary with exit options: the system controls harm-reduction infrastructure and experiences net benefit from legalization reading's adoption. Marginalized communities (moderate/constrained) have moderate d (≈0.55) because they are simultaneously beneficiaries (protection from black-market violence) and victims (transition costs, enforcement visibility): the legalization reading treats them as primary protectees but extraction persists through the mechanism itself. International supply-chain actors (organized/constrained) have high d (≈0.75) because they are pure targets: the legalization reading explicitly aims to eliminate their market by legal substitution and regulation. Law enforcement (institutional/arbitrage) has moderate-high d (≈0.65) from legalization reading perspective because while they possess institutional power, the reading treats them as path-dependent on enforcement maximization (inertia rather than true arbitrage).
 *
 * MANDATROPHY ANALYSIS:
 *   The legalization reading resolves mandatrophy by treating the constraint as Tangled Rope from the base analytical perspective: it has both coordination function (black-market harm reduction) and asymmetric extraction (prosecuted users, transition costs, enforcement apparatus path dependency). The mandatrophy question 'is this coordination or extraction?' receives answer: both. The coordination function is real — legalization can demonstrably reduce black-market violence and reduce the scale of enforcement coercion compared to prohibition. The extraction is also real — moving to legalization extracts significant costs from the prosecuted population and from law-enforcement institutions, and creates new extraction mechanisms through legal-market regulation and taxation. The legalization reading does not resolve mandatrophy by denying one dimension; it resolves by assigning higher weight to third-party protection and marginalized community harm-reduction than to prosecuted-user costs. This is a normative choice, not an empirical one. Different reading would weight these differently: the prohibition reading would assign higher weight to prevention of drug use (denying the coordination function); the harm-reduction reading would assign equal weight to prosecuted users and third parties (balancing extraction and coordination). The mandatrophy resolution is perspectival: the legalization reading achieves Tangled Rope by clearly identifying who benefits (marginalized communities, third parties, public health institutions) and who bears costs (prosecuted users, law enforcement turf), and defending that tradeoff through the harm-minimization principle.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    black_market_elimination_feasibility,
    'Does legalization actually eliminate black markets or create dual markets (legal + residual illegal)?',
    'Post-legalization outcome analysis: market share measurement, price parity studies, product-quality parity between legal and residual illegal supply. Historical comparison across cannabis legalization jurisdictions, Portuguese decriminalization experience, and alcohol post-Prohibition.',
    'If black markets eliminated: legalization reading''s core harm-reduction mechanism is validated; constraint classification stable as Tangled Rope. If dual markets persist: extraction mechanism shifts from production violence to supply-chain competition; may reclassify toward Snare if legal market cannot undercut illegal pricing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(black_market_elimination_feasibility, empirical, 'Whether legalization eliminates black markets or creates dual-market equilibrium').

omega_variable(
    enforcement_apparatus_path_dependency,
    'Can law-enforcement and prosecutorial institutions actually transition toward harm-reduction focus, or does bureaucratic inertia preserve enforcement-maximization incentives?',
    'Post-legalization resource allocation analysis: budget shifts from enforcement to treatment/prevention, prosecution rate changes for possession vs distribution, officer retraining and retention data, institutional behavior consistency with stated policy.',
    'If transition successful: enforcement apparatus becomes Rope (pure coordination, no extraction). If transition fails: apparatus becomes Piton (degraded ritual) or Snare (continued coercive extraction). Classification of perspectives 5 may shift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_apparatus_path_dependency, empirical, 'Whether enforcement institutions can transition to harm-reduction alignment').

omega_variable(
    marginalized_community_extraction_parity,
    'Does the legalization transition distribute extraction cost symmetrically or do marginalized communities bear disproportionate transition costs (visibility of harm, service delivery gaps)?',
    'Comparative outcome measurement during transition: incarceration rates by race/class, treatment access timelines by geography, overdose mortality during policy implementation, community economic opportunity within legal market.',
    'If symmetric: legalization reading''s third-party protection mechanism is operationalized; Perspective 2 shifts toward Rope. If asymmetric: marginalized communities experience continued extraction despite nominally beneficiary status; Perspective 2 remains Tangled Rope or reclassifies toward Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(marginalized_community_extraction_parity, empirical, 'Whether transition costs are distributed equitably or concentrated on marginalized communities').

omega_variable(
    drug_use_individual_choice_vs_constraint,
    'Does the legalization reading treat drug use as an individual choice outside state constraint scope, or does it constitute a new constraint (public health management of demand)?',
    'Policy implementation analysis: regulatory scope of legal market, advertising restrictions, age restrictions, consumption-site restrictions, treatment mandate depth. Comparison with pure decriminalization (no legal market) vs legalization (state-managed market).',
    'If pure choice: constraint scope narrows to black-market elimination and protection of third parties; legalization reading''s victim set remains prosecuted users + enforcement apparatus. If new constraint: state management of demand creates extraction mechanism targeting users through taxation, restriction, monitoring; victim set expands; may reclassify toward Snare from Perspective 1.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(drug_use_individual_choice_vs_constraint, conceptual, 'Whether legalization treats drug use as individual choice or new state-constraint domain').

omega_variable(
    kernel_reading_contest_location,
    'Where is the fundamental disagreement between legalization and prohibition readings located — empirical (what harms result?) or normative (what ends justify what costs)?',
    'Rhetorical analysis of prohibition vs legalization advocacy: identification of refutable empirical claims vs normative axioms that ground each position. Do disagreements resolve with better data or do they reflect different value commitments?',
    'If empirical: harm-minimization principle is shared; readings differ on mechanism; potential for resolution through evidence. If normative: readings reflect irreducible value differences (autonomy vs order, individual vs collective); coexistence required; no resolution possible.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Whether kernel contest is empirical or normative').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legalization_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legal_tr_t0, legalization_reading, theater_ratio, 0, 0.5).
narrative_ontology:measurement(legal_tr_t5, legalization_reading, theater_ratio, 5, 0.38).
narrative_ontology:measurement(legal_tr_t10, legalization_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(legal_tr_t15, legalization_reading, theater_ratio, 15, 0.33).

% Extraction over time
narrative_ontology:measurement(legal_be_t0, legalization_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(legal_be_t5, legalization_reading, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(legal_be_t10, legalization_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(legal_be_t15, legalization_reading, base_extractiveness, 15, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legalization_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(legalization_reading, prohibition_reading).
narrative_ontology:affects_constraint(legalization_reading, harm_reduction_reading).
narrative_ontology:affects_constraint(legalization_reading, black_market_supply_chain_structure).

% DUAL FORMULATION NOTE:
% The legalization reading is one of three constraint stories derived from the contested kernel 'substance_control_authority'. The sibling constraints 'prohibition_reading' and 'harm_reduction_reading' have different ε values reflecting different empirical claims about harm mechanisms. The legalization reading (ε=0.58) treats legalization as the optimal mechanism; prohibition reading would assign higher ε to the same structural mechanism (viewing it as extraction disguised as harm reduction); harm-reduction reading would assign comparable ε but with different beneficiary/victim mapping (treating prosecuted users as co-beneficiaries of harm reduction rather than sole victims). All three readings share the same institutional actors and structural options but differ in authority justification and scope.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legalization_reading, institutional, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
