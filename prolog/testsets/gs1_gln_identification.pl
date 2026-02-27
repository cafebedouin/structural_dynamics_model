% ============================================================================
% CONSTRAINT STORY: gs1_gln_identification
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gs1_gln_identification, []).

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
 *   constraint_id: gs1_gln_identification
 *   human_readable: Global Location Number (GLN) Standard
 *   domain: technological/economic
 *
 * SUMMARY:
 *   The Global Location Number (GLN) is a 13-digit identification standard
 *   maintained by GS1 (Global Standards 1, formerly known as the Uniform Code
 *   Council). Introduced in 1974 as the Universal Product Code (UPC) and
 *   evolved into a comprehensive suite of identification standards, GLN
 *   identifies physical locations (warehouses, hospital rooms, distribution
 *   centers) and legal entities (corporations, branches, departments). The
 *   constraint exhibits a hybrid structure: GLN provides genuine coordination
 *   value for global supply chains and regulatory compliance, but also
 *   creates structural extraction mechanisms through mandatory membership
 *   fees, lock-in to GS1's governance decisions, and suppression of
 *   alternative identification standards. The constraint operates at the
 *   intersection of technology (identification infrastructure), economics
 *   (supply chain efficiency), and regulation (FDA, EMA, customs authorities
 *   mandate GLN for pharmaceutical traceability and cross-border commerce).
 *   The perspectival analysis reveals why the same standard appears as pure
 *   coordination to beneficiaries with arbitrage options, pure extraction to
 *   trapped actors, and degraded ritual to legacy systems. The constraint's
 *   extractiveness has grown from 0.18 (1974: genuine coordination without
 *   significant monopoly position) to 0.38 (2024: consolidated global
 *   monopoly with mandatory regulatory adoption).
 *
 * KEY AGENTS:
 *   - GS1 Organization: Primary beneficiary (institutional/arbitrage) — controls standard evolution, derives revenue from membership and registration fees, has unilateral power to modify GLN specification
 *   - Multinational Logistics Providers: Secondary beneficiary (institutional/arbitrage) — benefit from global interoperability without incurring switching costs; can maintain parallel proprietary systems if needed
 *   - Small Enterprises (Unregistered): Primary victim (powerless/trapped) — cannot access modern supply chains without GLN registration; no alternative identification system with equivalent market acceptance
 *   - Healthcare Systems (Developing Regions): Secondary victim (moderate/constrained) — benefit from traceability coordination but constrained by mandatory fee structures and administrative overhead not designed for low-resource settings
 *   - Regulatory Authorities (FDA, EMA, Customs): Organized semi-beneficiary (organized/constrained) — benefit from standardized location identification for enforcement but constrained by dependency on GS1's technical governance and unable to modify standard for jurisdiction-specific needs
 *   - Open Standards Coalition: Organized aspirational actor (organized/constrained) — developing blockchain and distributed ledger alternatives; sees GLN monopoly as temporary
 *   - Legacy Enterprise Systems: Institutional maintainers (institutional/arbitrage) — nominally use GLN but with significant theatrical compliance overhead; switching costs prohibitive despite availability of more accurate locational technologies
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gs1_gln_identification, 0.38).
domain_priors:suppression_score(gs1_gln_identification, 0.52).
domain_priors:theater_ratio(gs1_gln_identification, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gs1_gln_identification, extractiveness, 0.38).
narrative_ontology:constraint_metric(gs1_gln_identification, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(gs1_gln_identification, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gs1_gln_identification, tangled_rope).
narrative_ontology:human_readable(gs1_gln_identification, "Global Location Number (GLN) Standard").
narrative_ontology:topic_domain(gs1_gln_identification, "technological/economic").

domain_priors:requires_active_enforcement(gs1_gln_identification).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gs1_gln_identification, gs1_organization).
narrative_ontology:constraint_beneficiary(gs1_gln_identification, large_retailers).
narrative_ontology:constraint_beneficiary(gs1_gln_identification, multinational_logistics_providers).
narrative_ontology:constraint_victim(gs1_gln_identification, small_enterprises).
narrative_ontology:constraint_victim(gs1_gln_identification, healthcare_systems_in_developing_regions).
narrative_ontology:constraint_victim(gs1_gln_identification, supply_chain_transparency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNREGISTERED SMALL MERCHANT (SNARE) — A small vendor or distributor in a developing region cannot participate in modern supply chains without GLN registration and GS1 fees. Exit is blocked: without GLN, access to major retail networks is impossible. Suppression is structural: GS1 licensing is mandatory for electronic commerce in regulated sectors. Maximum extraction experienced by those with no alternative identification systems and no resources to absorb licensing costs.
constraint_indexing:constraint_classification(gs1_gln_identification, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: REGIONAL HEALTHCARE PROVIDER (TANGLED ROPE) — Benefits from GLN standardization: interoperable inventory tracking, medication traceability, regulatory compliance documentation. But constrained by mandatory GS1 membership fees, complex administrative overhead for location updates, and dependency on GS1's monopolistic standard. Some exit options exist (proprietary hospital systems, alternative identifiers) but incur switching costs and regulatory friction. Mixed experience: genuine coordination value plus moderate extraction.
constraint_indexing:constraint_classification(gs1_gln_identification, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MULTINATIONAL LOGISTICS PROVIDER (ROPE) — Primary beneficiary. GLN enables global asset tracking, regulatory compliance across jurisdictions, and standardized data interchange with supply chain partners. Can switch to proprietary systems if desired but finds GLN's universal adoption economically advantageous. Benefits from coordination without experiencing extraction — the standard solves their fundamental problem of locating goods and locations across heterogeneous systems.
constraint_indexing:constraint_classification(gs1_gln_identification, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY AUTHORITY (TANGLED ROPE) — Organized agents (FDA, EMA, customs authorities) benefit from GLN's standardization for enforcement and auditing. But constrained by dependency on GS1's governance structure for standard updates, limited ability to modify the standard for jurisdiction-specific needs, and vulnerability to GS1's commercial decisions. Some agency through industry consortia, but structural asymmetry: regulators adopt GS1 standards but do not control them. Both coordination function and asymmetric extraction.
constraint_indexing:constraint_classification(gs1_gln_identification, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: GS1 ORGANIZATION (ROPE) — Primary beneficiary and de facto enforcer. GLN is GS1's flagship identification system; revenue streams from membership fees, GLN registration, and data services are entirely enabled by the standard's universal adoption. Can unilaterally modify the standard; faces minimal exit pressure because no effective alternative exists at equivalent scale. Experiences constraint as pure coordination — a standard that solves everyone's interoperability problem and funds the organization maintaining it.
constraint_indexing:constraint_classification(gs1_gln_identification, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: OPEN STANDARDS COALITION (SCAFFOLD) — Organized advocates for interoperable, non-proprietary identification standards (blockchain-based supply chain protocols, open UID systems, EPCIS alternative implementations) see GLN's dominance as a temporary coordination monopoly with an emerging sunset. As decentralized ledgers and open-source supply chain protocols mature, GLN's enforceability diminishes. Low effective extraction from this perspective because the coalition has agency and sees a clear exit path — adoption of alternative standards is already beginning in niche supply chains (local food networks, pharmaceutical traceability via blockchain).
constraint_indexing:constraint_classification(gs1_gln_identification, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: LEGACY ENTERPRISE SYSTEM ADMINISTRATOR (PITON) — Institutional actors maintaining older EDI and XML-based data interchange systems that nominally use GLN but encode significant theatrical compliance overhead. GLN's actual verification of physical location information is often minimal — many warehouses register GLN numbers but the identifier maps to outdated or inaccurate location data. The standard persists through institutional inertia: GLN integration is embedded in decades-old supply chain software, switching costs are prohibitive, and regulatory expectations enforce continued use despite the existence of more accurate locational technologies (GPS, cell tower triangulation, RFID). Theater ratio elevated by the gap between GLN's theoretical role (precise location identification) and its actual function (administrative reference number for legacy systems).
constraint_indexing:constraint_classification(gs1_gln_identification, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / INFORMATION THEORY VIEW (MOUNTAIN) — From a civilizational/universal perspective, some form of standardized location identification is an immutable requirement for global supply chain coordination. The constraint appears as a natural law: any distributed system requiring location-aware asset tracking needs a globally agreed namespace. GLN is the instantiation, but the need itself is structural to logistics at scale. However, this naturalizes what is actually a contingent institutional design: alternative namespaces (URIs with hierarchical encoding, blockchain-based distributed identity, region-specific numbering systems) could serve the same function. The engine's false summit detector identifies this as naturalization of a particular technological choice rather than a law of logistics.
constraint_indexing:constraint_classification(gs1_gln_identification, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gs1_gln_identification_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(gs1_gln_identification, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gs1_gln_identification, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(gs1_gln_identification, TR),
    TR >= 0.70.

:- end_tests(gs1_gln_identification_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. GLN extracts through mandatory membership fees ($1,500-$15,000 annually depending on organization size and jurisdiction), per-GLN registration costs, and data service fees. However, extraction is not severe (would be 0.46+) because: (1) genuine coordination value reduces relative extraction burden for users who benefit from global interoperability, (2) some alternative systems exist (proprietary identifiers, regional standards) providing partial exit options, and (3) multinational actors can distribute costs across operations. The value of 0.38 reflects that extraction is real but mediated by genuine coordination function. Suppression (0.52): Moderate-high. Suppression mechanisms include: regulatory mandates (FDA traceability rules make GLN quasi-mandatory for pharmaceuticals), institutional lock-in (supply chain software integration costs), and the absence of viable alternatives at equivalent scale. However, suppression is not maximal (would be 0.60+) because: (1) organizations can technically use proprietary identifiers alongside GLN, (2) blockchain alternatives are emerging, and (3) some regional systems (China's internal logistics standards, India's informal supply chains) operate outside GLN. Theater ratio (0.48): Moderate. GLN's stated purpose is precise location identification, but actual implementation often involves: (1) location data that becomes outdated quickly (enterprises maintain separate real-time locational databases), (2) administrative use where the GLN functions as a compliance token rather than a locational identifier, and (3) enterprise systems that encode GLN in legacy EDI protocols with minimal real-time accuracy benefits. Theater has increased over the interval as enterprises added GLN to existing systems without redesigning for genuine real-time location transparency.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap across the eight perspectives is substantial and reveals the constraint's true hybrid nature. GS1 and multinational logistics providers see pure coordination (Rope) — the standard solves their fundamental problem of global interoperability at minimal perceived cost. Unregistered small merchants and developing-region enterprises see pure extraction (Snare) — they are trapped outside modern supply chains without absorbing mandatory costs. Healthcare regulators and regional authorities see mixed coordination and extraction (Tangled Rope) — they benefit from standardization for enforcement but constrained by inability to control standard evolution. Legacy enterprise systems see degraded ritual (Piton) — GLN integration persists through institutional inertia despite the existence of more accurate locational technologies. The open standards coalition sees a temporary monopoly with emerging alternatives (Scaffold) — blockchain and distributed ledger systems are building replacement pathways. The analytical observer risks naturalizing this as immutable (Mountain) — 'global supply chains inherently require a unified identification standard' — but the structural evidence reveals this as a particular instantiation of a more general need. Alternative namespaces could serve the same function with different extraction profiles.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from each agent's structural position relative to the GLN constraint. GS1 Organization (institutional power + arbitrage exit) experiences d ≈ 0.05 — full beneficiary with ability to exit (but no incentive, as GLN is their primary revenue source). Their f(d) ≈ -0.12 produces negative effective extraction (coordination is net benefit). Multinational Logistics Providers (institutional + arbitrage) experience similar d ≈ 0.15 — beneficiary with exit options (could use proprietary systems) but benefits from universal adoption outweigh switching costs. Small enterprises (powerless + trapped) experience high d ≈ 0.95 — no exit options without sacrificing supply chain access, forced to absorb membership and registration costs. Regulatory authorities (organized + constrained) experience d ≈ 0.50 — symmetric position: benefit from standardization but constrained by inability to modify standard. This differentiation produces the perspectival gap: beneficiaries experience Rope, trapped actors experience Snare, organized constrained actors experience Tangled Rope. The analytical observer's mountain perspective is challenged by the structural evidence of lock-in and extracted rents, revealing it as a false summit (naturalization of a contingent monopoly).
 *
 * MANDATROPHY ANALYSIS:
 *   GLN resolves the mandatrophy by demonstrating how a coordination standard can accumulate extraction mechanisms without losing its coordination function. The constraint is genuinely Tangled Rope: it provides both coordination value (solved global supply chain interoperability) and asymmetric extraction (mandatory fees, lock-in, suppression of alternatives). The mandatrophy question is: 'Is GLN a coordination mechanism that has accumulated extraction, or an extraction mechanism that maintains a coordination facade?' The answer from the structural data: both, from different perspectives. GS1 and logistics multinationals see pure coordination (Rope). Small enterprises and developing-region healthcare systems see the extraction (Snare). The constraint's extractiveness has grown over 50 years not because the coordination function degraded, but because GS1's monopoly position strengthened, allowing them to extract more rent from a standard that users cannot exit (lock-in). The theater ratio's increase reflects that GLN's actual benefit in real-time location accuracy has not kept pace with alternative technologies (GPS, RFID, blockchain), but GLN persists because the switching costs from 50 years of institutional embedding are prohibitive. This is the classic pattern of Tangled Rope evolution: a coordination mechanism that works becomes an extraction mechanism that persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    data_accuracy_vs_compliance_theater,
    'Is GLN''s primary value in actual location data accuracy or in providing a standardized compliance token that enables regulatory auditing regardless of underlying data quality?',
    'Audit of GLN-registered location data against ground truth (GPS, facility records); comparison of enforcement outcomes under GLN-based vs direct-verification regulatory frameworks',
    'If accuracy primary: GLN functions as genuine coordination (Rope from more perspectives). If compliance theater primary: GLN functions as extraction mechanism (Snare from regulator perspective, Piton from enterprise perspective).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(data_accuracy_vs_compliance_theater, empirical, 'Whether GLN provides data accuracy or primarily serves as compliance token').

omega_variable(
    monopoly_lock_in_vs_network_effect_necessity,
    'Does GS1''s market dominance derive from genuine network effects (universal adoption becoming more valuable the more widespread it is) or from switching cost lock-in (early adoption creates path dependency preventing alternative standards)?',
    'Historical analysis of GLN adoption barriers and technical switching costs; case study of regions attempting to implement alternative identification standards; comparison of switching costs between GLN and open-source alternatives',
    'If network effects: GLN is a natural coordination monopoly (Rope perspective justified). If lock-in: GLN is a maintained Snare (victims trapped by switching costs rather than technical necessity).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(monopoly_lock_in_vs_network_effect_necessity, empirical, 'Whether GLN dominance reflects network effects or switching cost lock-in').

omega_variable(
    blockchain_alternative_maturity,
    'Are decentralized, blockchain-based location identification systems (distributed ledger supply chain protocols, NFT-based asset tracking) technically and economically viable alternatives that could replace GLN in the next 10-20 years?',
    'Technical feasibility assessment of blockchain-based location identification at scale; cost comparison analysis; pilot program results in early-adopter supply chains (pharmaceutical, food safety)',
    'If viable: scaffold perspective is correct — GLN''s sunset is real and predictable. If not viable: scaffold is aspirational, and GLN maintains structural monopoly indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(blockchain_alternative_maturity, empirical, 'Whether blockchain alternatives can provide viable replacement for GLN').

omega_variable(
    healthcare_specific_regulatory_capture,
    'In healthcare supply chains, does GLN''s mandatory adoption (enforced by FDA and EMA traceability regulations) represent a genuine regulatory requirement or a regulatory capture scenario where GS1 lobbying has embedded proprietary standards into law?',
    'Analysis of regulatory history: when and why GLN became mandatory in pharmaceutical traceability regulations; comparison with alternative standards considered during regulation drafting; interview evidence from regulatory agency decision-makers',
    'If genuine requirement: healthcare perspective remains Tangled Rope (mixed coordination and extraction). If capture: healthcare victim status (Snare) is more accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(healthcare_specific_regulatory_capture, conceptual, 'Whether healthcare GLN mandates reflect regulation or regulatory capture').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gs1_gln_identification, 1974, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gln_tr_t0, gs1_gln_identification, theater_ratio, 0, 0.32).
narrative_ontology:measurement(gln_tr_t15, gs1_gln_identification, theater_ratio, 15, 0.4).
narrative_ontology:measurement(gln_tr_t30, gs1_gln_identification, theater_ratio, 30, 0.48).

% Extraction over time
narrative_ontology:measurement(gln_be_t0, gs1_gln_identification, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(gln_be_t15, gs1_gln_identification, base_extractiveness, 15, 0.28).
narrative_ontology:measurement(gln_be_t30, gs1_gln_identification, base_extractiveness, 30, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gs1_gln_identification, information_standard).
narrative_ontology:affects_constraint(gs1_gln_identification, barcode_standardization).
narrative_ontology:affects_constraint(gs1_gln_identification, pharmaceutical_supply_chain_traceability).
narrative_ontology:affects_constraint(gs1_gln_identification, epcis_interoperability).

% DUAL FORMULATION NOTE:
% GLN is the instantiation layer of a broader constraint family: the need for standardized identification in global logistics is a primary constraint (lower extractiveness, higher coordination value); GLN's specific monopolistic governance by GS1 is a secondary constraint (higher extractiveness, enforced through lock-in). These could be modeled as separate stories: gs1_identification_need (the coordination problem) and gs1_gln_monopoly (the extraction mechanism). This story integrates both because the coordination need and the monopolistic implementation cannot be cleanly separated in practice — exit from GLN means losing access to the coordination value it provides.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gs1_gln_identification, institutional, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
