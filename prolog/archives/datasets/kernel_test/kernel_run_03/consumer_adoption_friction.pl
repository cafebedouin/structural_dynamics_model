% ============================================================================
% CONSTRAINT STORY: consumer_adoption_friction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_consumer_adoption_friction, []).

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
 *   constraint_id: consumer_adoption_friction
 *   human_readable: Consumer Adoption Friction in Digital Money Infrastructure
 *   domain: monetary_economics/financial_infrastructure/technology_governance
 *
 * SUMMARY:
 *   The transition from cash and check-based payment systems to
 *   digital/electronic money in developed economies created a structural gap
 *   between when digital payment became conceptually recognized and
 *   institutionally permissible (1970s–1980s regulatory frameworks) and when
 *   consumers actually held and used digital money at scale (1990s–2010s).
 *   This adoption friction—the prolonged lag between technical feasibility
 *   and mass adoption—can be understood as a natural coordination problem
 *   inherent to monetary system transitions, or as an engineered extraction
 *   mechanism that benefited incumbent payment processors (Visa, Mastercard,
 *   ACH operators) and banking institutions while imposing costs on early
 *   adopters, unbanked populations, and alternative payment networks. The
 *   constraint exhibits characteristics of all six DR types depending on
 *   perspective. The temporal measurements show increasing extractiveness
 *   (0.35→0.52) and rising theater ratio (0.48→0.64) over the 40-year
 *   interval, indicating that performative infrastructure (PCI-DSS
 *   compliance, settlement protocols, legacy standard maintenance) grew as
 *   digital alternatives became technically viable. The core analytical
 *   question is whether the friction is a natural feature of monetary system
 *   emergence (mountain) or a contingent institutional arrangement engineered
 *   by beneficiaries to extend their competitive advantage
 *   (tangled_rope/snare). The regulatory response—open banking directives,
 *   interoperability mandates, central bank digital currency
 *   initiatives—frames adoption friction as a temporary coordination problem
 *   with a regulatory sunset (scaffold), but the persistence of legacy
 *   standards and the continued evolution of incumbent lock-in suggest the
 *   problem may be regenerating rather than resolving.
 *
 * KEY AGENTS:
 *   - Incumbent Payment Processors (Visa, Mastercard, ACH operators): Primary beneficiaries (institutional/arbitrage) — extended competitive moat through adoption friction; controlled transition timeline; maintained merchant lock-in during digital migration
 *   - Banking Institutions: Secondary beneficiary (institutional/arbitrage) — coordinated customer migration at own pace; retained deposit-account centrality in payment flows
 *   - Unbanked Populations: Primary victim (powerless/trapped) — excluded from digital payment participation; forced to bear transaction costs of cash-substitute services; no exit from the constraint
 *   - Early Adopters & Small Merchants: Secondary victim (moderate/constrained) — bore costs of learning new systems, network building, compatibility challenges; faced switching costs that favored incumbents
 *   - Alternative Payment Networks (cryptocurrency, mobile money, alternative clearing): Tertiary victim (powerful/constrained) — faced regulatory barriers and network effect disadvantage during critical adoption window; constrained by incumbent control of merchant infrastructure
 *   - Financial Regulators & Central Banks: Organized agents (organized/constrained) — eventually recognized adoption friction as coordination problem; deployed regulatory fixes (open banking, interoperability mandates, CBDC); possess sunset logic for constraint resolution
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing engineered lock-in as inevitable feature of monetary systems
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(consumer_adoption_friction, 0.52).
domain_priors:suppression_score(consumer_adoption_friction, 0.58).
domain_priors:theater_ratio(consumer_adoption_friction, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(consumer_adoption_friction, extractiveness, 0.52).
narrative_ontology:constraint_metric(consumer_adoption_friction, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(consumer_adoption_friction, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(consumer_adoption_friction, tangled_rope).
narrative_ontology:human_readable(consumer_adoption_friction, "Consumer Adoption Friction in Digital Money Infrastructure").
narrative_ontology:topic_domain(consumer_adoption_friction, "monetary_economics/financial_infrastructure/technology_governance").

domain_priors:requires_active_enforcement(consumer_adoption_friction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(consumer_adoption_friction, incumbent_payment_processors).
narrative_ontology:constraint_beneficiary(consumer_adoption_friction, banking_institutions).
narrative_ontology:constraint_beneficiary(consumer_adoption_friction, central_banks).
narrative_ontology:constraint_victim(consumer_adoption_friction, early_adopters).
narrative_ontology:constraint_victim(consumer_adoption_friction, unbanked_populations).
narrative_ontology:constraint_victim(consumer_adoption_friction, alternative_payment_networks).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNBANKED POPULATION (SNARE) — Trapped by infrastructure barriers (no account access, merchant compatibility unavailable, legacy system dependency). Cannot exit the constraint without abandoning all digital payment participation. Maximum experienced extraction through exclusion costs and forced reliance on expensive cash-substitute services.
constraint_indexing:constraint_classification(consumer_adoption_friction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: EARLY ADOPTERS & SMALL MERCHANTS (TANGLED ROPE) — Constrained by network effects (adoption friction raises switching costs, but network growth benefits adopters). Bear coordination costs (learning, network building) alongside extraction (prolonged infrastructure transition, compatibility tax). Genuine coordination function exists (building payment networks) but overlaid with asymmetric extraction favoring incumbents.
constraint_indexing:constraint_classification(consumer_adoption_friction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INCUMBENT PAYMENT PROCESSORS (ROPE) — Benefit from adoption friction as a coordination mechanism that prevents destabilization of their network effects. Experience the constraint as pure coordination: friction extends their competitive moat, maintains merchant lock-in, and enables controlled transition to digital standards on their terms. Low experienced extraction — they are the beneficiaries.
constraint_indexing:constraint_classification(consumer_adoption_friction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: FINANCIAL REGULATORS & CENTRAL BANKS (SCAFFOLD) — Organize coordination of standards (PCI-DSS, open banking directives, CBDC frameworks) to manage transition. See adoption friction as a temporary problem with regulatory sunset: managed migration pathways, interoperability mandates, and central bank digital currency infrastructure are building alternatives that bypass incumbent lock-in. Suppression declines as regulation increases.
constraint_indexing:constraint_classification(consumer_adoption_friction, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY PAYMENT RITUALS & STANDARDS (PITON) — Performative compliance with outdated standards (card magnetic stripe retention, settlement protocols, clearing house requirements). These mechanisms persisted decades beyond their functional necessity due to institutional inertia. Theater ratio measures the proportion of payment infrastructure labor devoted to legacy compatibility. Standards persist because alternatives haven't fully displaced them, not because they solve problems. Degraded constraint maintained through regulatory conservatism.
constraint_indexing:constraint_classification(consumer_adoption_friction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, some adoption lag is inherent to monetary infrastructure: new payment systems always face the cold-start problem of network effects, and the temporal gap between conceptual viability and consumer adoption is a structural feature of how money emerges. However, this perspective naturalizes what is actually a contingent institutional arrangement: adoption friction is engineered through standards-setting, regulatory design, and incumbent control, not an immutable property of monetary systems.
constraint_indexing:constraint_classification(consumer_adoption_friction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(consumer_adoption_friction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(consumer_adoption_friction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(consumer_adoption_friction, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(consumer_adoption_friction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(consumer_adoption_friction, TR),
    TR >= 0.70.

:- end_tests(consumer_adoption_friction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Incumbent payment processors captured substantial benefits during the 40-year transition — extended market dominance, increased transaction volume as digital payments grew, maintained pricing power through network effects and switching costs. But the extraction is not maximal (not ≥0.66 for snare) because genuine coordination functions existed: these incumbents did build and maintain the digital payment infrastructure that enabled the transition. The extractiveness reflects the asymmetry between coordination costs (borne by early adopters and merchants) and coordination benefits (captured by incumbents). Suppression (0.58): Moderate-high. Multiple barriers to faster adoption: (1) Merchant terminal infrastructure fragmentation — incompatible systems required merchants to deploy multiple technologies; (2) Banking system fragmentation — interbank clearing protocols were designed by incumbents to maintain their intermediary role; (3) Consumer familiarity and trust — digital payment felt riskier than cash; (4) Regulatory conservatism — payment system regulators favored incumbent-led transition over disruptive alternatives. These barriers are structural and institutional rather than physical, making them partly addressable through regulatory intervention (EU PSD2, open banking mandates have reduced some barriers). Theater ratio (0.64): Moderate-high. Significant performative content in payment infrastructure maintenance: PCI-DSS compliance is primarily a ritual (security theater with real costs but limited actual breach prevention), settlement protocols are legacy ceremonies that could be automated more efficiently, and regulatory review cycles devoted to incumbent-preferred standards rather than innovation. The theater ratio increased over time as digital alternatives became technically viable but regulatory inertia prevented displacement — the gap between what the system does and what it needs to do expanded. This rising theater is diagnostic of constraint degradation (piton signal) overlaid on ongoing extraction.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a critical perspectival gap between beneficiary and victim experiences. The incumbent processors see coordination (rope) — they genuinely are solving the problem of building digital payment infrastructure, and they experience the constraint as a network effect mechanism that prevents destabilization. Early adopters and merchants see hybrid coordination-extraction (tangled_rope) — the infrastructure is genuinely useful (coordination function) but they bear disproportionate transition costs while incumbents capture the value. The unbanked see pure extraction (snare) — they are excluded from the digital payment ecosystem entirely, bearing the costs of cash-substitute services without access to the benefits. The regulatory perspective sees a temporary problem (scaffold) — regulatory interventions (open banking, interoperability mandates) are systematically reducing adoption friction, suggesting a sunset is plausible. The legacy payment system perspective sees institutional degradation (piton) — settlement protocols, magnetic stripe standards, and clearing house requirements persist purely through inertia, not function. The civilizational analytical perspective risks naturalizing this as immutable (mountain) — the cold-start problem in monetary systems is genuine — but the structural data reveals the naturalness as contingent on specific institutional arrangements (standard-setting authority, regulatory structure, incumbent control of merchant networks). The false summit signature would trigger here: the mountain classification is a naturalization of what is actually a tangled rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective derives its directionality from the agent's structural position relative to extraction flow. Beneficiaries (incumbents, banks, central banks) occupy positions where the constraint extends their power and options — d ≈ 0.05–0.20, yielding negative or low f(d), meaning they experience low or negative effective extraction. The institutional/arbitrage combination produces the lowest experienced extraction because these agents have high exit capacity (can deploy capital elsewhere) and occupy positions where the constraint subsidizes them. Victims (unbanked, early adopters, alternative networks) occupy positions where the constraint restricts their options and extracts resources — d ≈ 0.75–0.95, yielding high f(d) ≈ 1.2–1.4, meaning they experience high effective extraction despite moderate base extractiveness. The powerless/trapped combination produces maximum experienced extraction because these agents have zero exit capacity. Organized regulators occupy an intermediate position (constrained exit, but with leverage to reshape the constraint) — d ≈ 0.50–0.60, yielding mid-range f(d) ≈ 0.70, producing the scaffold perspective. The analytical/civilizational perspective is perspectival on whether the constraint is natural (mountain) or constructed (tangled_rope/snare) — the analytical observer risks naturalizing because the cold-start problem in monetary systems is real, but the engineering of that problem through standards-setting and regulatory design is also real.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that the perspectival gap is not ambiguity about types but genuine structural difference in experienced extraction. The rope perspective (beneficiary) and the snare perspective (victim) are both accurate descriptions of the constraint from their respective positions — they are not disagreeing about facts, they are experiencing different extraction flows. The constraint is simultaneously rope, tangled_rope, snare, scaffold, and piton from different perspectives because those perspectives locate different agents in different structural positions relative to the same extraction mechanism. The mandatrophy is resolved by the multi-perspective presheaf: the constraint does not have a single type, it has a perspectival classification that varies with observer position. The misleading classification would be claiming this is 'rope' (ignoring victim experience) or 'snare' (ignoring genuine coordination function) or 'mountain' (naturalizing institutional arrangements). The tangled_rope claim_type reflects the analytical perspective's balanced assessment that genuine coordination overlays asymmetric extraction — neither function dominates, making this the base classification. The scaffold perspective (organized regulators with sunset logic) reflects active mitigation underway through regulatory reform, reducing but not eliminating the extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_emergence_reading,
    'Is ''emergence of digital money'' a single constraint about adoption lag, or a contested kernel with different readings of what counts as emergence: conceptual thinkability vs. operational first-hold vs. functional displacement of predecessors?',
    'Decompose the empirical timeline: (1) When did digital money become conceptually recognized by regulators and economists? (2) When did first operational systems exist and enable holdings? (3) When did digital money displace cash as the marginal transaction medium? If these dates differ by > 5 years and each reflects a structurally distinct constraint, the ''emergence'' is best modeled as a constraint family rather than a single story.',
    'If single constraint: unified extraction narrative about incumbent lock-in delaying adoption. If constraint family: separate stories for regulatory recognition (likely rope/scaffold), technical feasibility (likely mountain/rope), and consumer adoption (tangled_rope/snare), each with different ε and different beneficiary/victim structures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_emergence_reading, conceptual, 'Whether emergence is a single constraint or a contested kernel with multiple readings').

omega_variable(
    incumbent_lock_in_vs_natural_friction,
    'To what extent is adoption friction an intentional outcome of incumbent payment processor lock-in versus a natural consequence of coordination costs in monetary system transitions?',
    'Historical analysis of standards-setting decisions: (1) Documented resistance to interoperability standards by incumbents. (2) Comparative analysis of jurisdictions with active regulatory interoperability mandates (EU PSD2, India UPI) vs. jurisdictions with incumbent-led standards. (3) Cost-benefit analysis of actual network effects: do real switching costs exceed estimated technological transition costs?',
    'If primarily lock-in: classification shifts toward snare from more perspectives (extraction is deliberately engineered). If primarily natural friction: classification shifts toward rope/scaffold (friction is coordination cost, not extraction). Current balanced assessment reflects genuine uncertainty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incumbent_lock_in_vs_natural_friction, empirical, 'Incumbent lock-in versus natural monetary transition friction').

omega_variable(
    alternative_payment_network_feasibility,
    'Could alternative payment networks (crypto, mobile money, alternative clearing systems) have achieved faster consumer adoption without regulatory barriers, or do network effects make incumbent coordination unavoidable?',
    'Historical counterfactual analysis: (1) Adoption rates of unregulated alternative systems in permissive jurisdictions (El Salvador crypto, Kenya M-Pesa pre-regulation). (2) Speed of adoption vs. incumbent payment system transition rates in same jurisdictions. (3) Technical analysis of whether alternative networks provide equivalent or superior functionality.',
    'If alternatives could have achieved faster adoption without regulatory barriers: extraction narrative strengthens (regulation serves incumbent interests). If network effects make incumbents inevitable: friction is largely natural (scaffold/rope perspectives strengthen). This shapes whether the constraint is primarily extraction or primarily coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_payment_network_feasibility, empirical, 'Whether unregulated alternatives could have displaced incumbents faster').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is measured suppression (0.58) primarily structural (merchant terminal incompatibility, banking infrastructure fragmentation, regulatory requirements) or internalized (consumer belief that incumbent systems are the only ''safe'' or ''legitimate'' payment method)?',
    'Post-barrier-removal analysis: regions where regulatory mandates (EU PSD2, GDPR) reduce technical barriers — do adoption rates rise proportionally? If yes, suppression is primarily structural. If no: suppression includes internalized barriers (trust, familiarity, sunk-cost framing) that persist after technical removal.',
    'If primarily structural: policy interventions (interoperability mandates, open banking) can reduce suppression and friction. If internalized: suppression persists even with technical barriers removed — the constraint''s extraction mechanism is partially cognitive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Structural versus internalized suppression in adoption friction').

omega_variable(
    central_bank_digital_currency_foreclosure,
    'Will CBDC infrastructure, once deployed globally, foreclose the alternative payment network reading of this constraint by centralizing digital money issuance?',
    'Monitoring CBDC deployment: (1) Is CBDC architecture designed for interoperability with alternative systems, or proprietary/restricted? (2) Do regulatory frameworks encourage or suppress alternative payment networks in CBDC era? (3) Do CBDCs reduce or increase adoption friction relative to incumbent private payment systems?',
    'If CBDC foreclosed alternatives: the scaffold perspective''s sunset (alternative networks solve the problem) becomes false. Constraint shifts from scaffold (temporary) to piton (degraded incumbent system preserved under CBDC canopy) or snare (central bank controls payment layer directly). If CBDC enables alternatives: scaffold perspective confirmed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(central_bank_digital_currency_foreclosure, empirical, 'Whether CBDC deployment will foreclose or enable alternative payment networks').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(consumer_adoption_friction, 1980, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(caf_tr_t0, consumer_adoption_friction, theater_ratio, 0, 0.48).
narrative_ontology:measurement(caf_tr_t10, consumer_adoption_friction, theater_ratio, 10, 0.58).
narrative_ontology:measurement(caf_tr_t20, consumer_adoption_friction, theater_ratio, 20, 0.64).

% Extraction over time
narrative_ontology:measurement(caf_be_t0, consumer_adoption_friction, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(caf_be_t10, consumer_adoption_friction, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(caf_be_t20, consumer_adoption_friction, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(consumer_adoption_friction, resource_allocation).
narrative_ontology:affects_constraint(consumer_adoption_friction, payment_network_effects).
narrative_ontology:affects_constraint(consumer_adoption_friction, merchant_terminal_fragmentation).
narrative_ontology:affects_constraint(consumer_adoption_friction, banking_system_interoperability).

% DUAL FORMULATION NOTE:
% Consumer adoption friction is downstream of three distinct infrastructure constraints: (1) payment_network_effects (classic cold-start problem — coordination function, low extraction), (2) merchant_terminal_fragmentation (incumbent control of merchant interface — extraction mechanism), (3) banking_system_interoperability (regulatory design favoring incumbent intermediation — extraction mechanism). Each has different ε and different beneficiary/victim structures. This story models the combined effect at the consumer adoption layer; the upstream constraints model the infrastructure mechanisms separately.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(consumer_adoption_friction, powerful, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
