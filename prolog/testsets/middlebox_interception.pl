% ============================================================================
% CONSTRAINT STORY: middlebox_interception
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_middlebox_interception, []).

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
 *   constraint_id: middlebox_interception
 *   human_readable: Middlebox Interception in Network Infrastructure
 *   domain: network_security/infrastructure
 *
 * SUMMARY:
 *   Middlebox interception represents a structural tension between legitimate
 *   network infrastructure needs and systematic user surveillance.
 *   Middleboxes — intermediary devices deployed across internet
 *   infrastructure to intercept, inspect, and modify traffic — are deployed
 *   by network operators ostensibly for DDoS mitigation, caching, and traffic
 *   engineering. However, these same devices enable content censorship,
 *   behavioral tracking, and law enforcement surveillance. The constraint
 *   exhibits hybrid coordination-extraction dynamics: from operators'
 *   perspective, middleboxes provide genuine network management coordination;
 *   from users' perspective, the same devices perform extraction without
 *   consent. The extractiveness has increased from 0.35 to 0.52 over the
 *   measurement interval as: (1) encryption technologies (TLS 1.3) have
 *   reduced middlebox content access, forcing operators to intensify metadata
 *   analysis and traffic pattern extraction; (2) surveillance capabilities
 *   have expanded beyond DDoS mitigation into behavioral targeting and
 *   censorship; (3) regulatory pressure (law enforcement mandates for backend
 *   access) has increased institutional deployment of interception
 *   infrastructure. The theater ratio has risen from 0.42 to 0.58, reflecting
 *   that operators increasingly justify surveillance capabilities as
 *   necessary byproducts of network management, when in fact the interception
 *   mechanism is a primary institutional goal. The suppression requirement
 *   has risen from 0.48 to 0.65, indicating that maintaining user ignorance
 *   of interception and preventing exit through encryption adoption now
 *   requires active enforcement — blocking VPN traffic, throttling encrypted
 *   protocols, and legal prohibition of privacy tools.
 *
 * KEY AGENTS:
 *   - End Users: Primary victims (powerless/trapped) — bear extraction of behavioral tracking and content filtering; have no technical or economic means to avoid interception
 *   - Network Operators: Primary beneficiaries (institutional/arbitrage) — gain operational control, cost reduction, and optional law enforcement access; frame interception as necessary management byproduct
 *   - Content Delivery Providers: Secondary beneficiaries (institutional/arbitrage) — benefit from traffic inspection for caching and optimization; coordinate with operators
 *   - Law Enforcement Agencies: Tertiary beneficiaries (powerful/arbitrage) — extract surveillance data from middleboxes for criminal investigation and political surveillance
 *   - Encryption Standards Bodies: Organized resistance (organized/constrained) — attempt to prevent middlebox access through protocol design (TLS 1.3), but face political pressure to accommodate backdoors
 *   - Dissidents in Censorious Jurisdictions: Maximal victims (moderate/constrained) — experience pure extraction: surveillance, censorship, and arrest risk with no offsetting benefit
 *   - Analytical Observer: Risk of false summit (analytical/analytical) — tempted to naturalize middleboxes as inevitable feature of packet-switched networks, obscuring that they represent institutional choice for centralized control
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(middlebox_interception, 0.52).
domain_priors:suppression_score(middlebox_interception, 0.65).
domain_priors:theater_ratio(middlebox_interception, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(middlebox_interception, extractiveness, 0.52).
narrative_ontology:constraint_metric(middlebox_interception, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(middlebox_interception, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(middlebox_interception, tangled_rope).
narrative_ontology:human_readable(middlebox_interception, "Middlebox Interception in Network Infrastructure").
narrative_ontology:topic_domain(middlebox_interception, "network_security/infrastructure").

domain_priors:requires_active_enforcement(middlebox_interception).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(middlebox_interception, network_operators).
narrative_ontology:constraint_beneficiary(middlebox_interception, content_delivery_providers).
narrative_ontology:constraint_beneficiary(middlebox_interception, law_enforcement_agencies).
narrative_ontology:constraint_victim(middlebox_interception, end_users).
narrative_ontology:constraint_victim(middlebox_interception, encrypted_communication_systems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: END USER (SNARE) — Structurally trapped. Traffic routing through middleboxes is transparent and unavoidable — users have no technical means to detect or circumvent interception without abandoning internet access. Suppression is maximal: no alternative routes exist, no negotiation is possible, and the interception mechanism is deliberately obscured. The user bears extraction (data inspection, behavioral tracking, content filtering) with minimal benefit.
constraint_indexing:constraint_classification(middlebox_interception, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ENCRYPTED APPLICATION USER (TANGLED ROPE) — Constrained but not trapped. End-to-end encrypted protocols (TLS, HTTPS) prevent middlebox content inspection, but the constraint persists through traffic pattern analysis, metadata extraction, and connection throttling. Users pay adoption costs (incompatibility with some services, performance penalties) to maintain privacy. Both coordination and extraction coexist: the infrastructure requires middleboxes for DDoS mitigation and traffic engineering (users benefit indirectly), but those same middleboxes enable behavioral tracking and censorship (users are victimized).
constraint_indexing:constraint_classification(middlebox_interception, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: NETWORK OPERATOR (ROPE) — Experiences middleboxes as pure coordination mechanism. Middleboxes solve genuine infrastructure problems: DDoS mitigation, traffic engineering, cache optimization, and congestion management. Operators deploy middleboxes to improve network quality and reduce infrastructure costs. The interception capability is secondary — operators frame it as necessary byproduct of traffic management, not primary function. They have arbitrage options: deploy alternative traffic engineering (SDN, in-band signaling) at higher cost.
constraint_indexing:constraint_classification(middlebox_interception, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CONTENT DELIVERY PROVIDER (ROPE) — Middlebox interception enables content optimization and caching coordination. CDNs benefit from traffic inspection to identify cacheable content, optimize delivery paths, and apply content-specific compression. They frame middlebox cooperation as coordination, not extraction. Alternative implementations (in-network caching coordination via HTTP headers, explicit CDN peering) exist but require endpoint participation. Low extraction experienced because CDN and operator interests align.
constraint_indexing:constraint_classification(middlebox_interception, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ENCRYPTION STANDARDS BODY (TANGLED ROPE) — Organized actors (IETF, TLS Working Group) experience middlebox interception as both coordination constraint and extraction threat. TLS specifications must account for middlebox behavior (TLS 1.3 was partly designed to prevent middlebox inspection). The standardization process itself becomes a site of contention: should protocols explicitly prevent middlebox access (benefiting users) or accommodate it (benefiting operators/law enforcement)? Constrained by institutional consensus requirements and political pressure. The constraint requires active enforcement — standards must be enforced against implementers who add backdoors for interception.
constraint_indexing:constraint_classification(middlebox_interception, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: DISSIDENT IN CENSORIOUS JURISDICTION (SNARE) — For users in countries deploying middleboxes for political censorship, the constraint approaches pure extraction. Middleboxes perform content filtering, identity tracking, and targeted blocking. Exit options are partially constrained (VPN use is illegal, detected, and throttled). The victim bears maximum extraction — behavioral surveillance, censorship, and arrest risk — with no offsetting benefit. The institutional actors (government agencies) are pure beneficiaries.
constraint_indexing:constraint_classification(middlebox_interception, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: LAW ENFORCEMENT AGENCY (TANGLED ROPE) — Experiences middleboxes as an interception access mechanism with legitimate (criminal investigation) and illegitimate (political surveillance, rights violations) uses. Powerful actors (governments) can demand backend access to middlebox data without user consent. The constraint requires active enforcement of access controls and warrant procedures (often violated in practice). From this perspective, the coordination function is law enforcement coordination; the extraction is political surveillance; the beneficiary is the powerful agency; the victim is the dissident population.
constraint_indexing:constraint_classification(middlebox_interception, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURALIZED VIEW (MOUNTAIN) — From a civilizational timescale, interception appears as an immutable feature of packet-switched networks. The claim is that intermediary devices are necessary for routing, and inspection is a necessary feature of routing hardware. However, this naturalizes a contingent architectural choice. End-to-end encryption, in-band signaling, and explicit consent protocols represent alternatives; they require institutional adoption, not technological impossibility. The mountain classification reveals itself as false summit — the constraint is enforceable institutional architecture, not immutable law.
constraint_indexing:constraint_classification(middlebox_interception, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(middlebox_interception_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(middlebox_interception, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(middlebox_interception, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(middlebox_interception, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(middlebox_interception_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Middlebox interception extracts user data (behavioral patterns, communications metadata, browsing history) without consent. The extraction is not total (end-to-end encryption prevents content access) but systematic (metadata analysis remains possible). The increasing trajectory (0.35→0.52) reflects that encryption has closed content channels, forcing extraction to intensify via metadata and behavioral analysis. Suppression (0.65): High. Users have limited practical exit options. Encryption provides partial protection at adoption cost (VPN blocking, performance penalties, compatibility problems). In many jurisdictions, circumvention tools are legally prohibited. Institutional barriers are substantial: network routing is a natural monopoly; users cannot choose alternative providers. The rising trajectory (0.48→0.65) reflects increased enforcement against circumvention techniques. Theater ratio (0.58): Moderate-high. Operators justify middleboxes primarily as network management tools (traffic engineering, DDoS mitigation, caching). These coordination functions are real but represent only part of the actual deployment driver. The growing theater (0.42→0.58) reflects that surveillance capabilities are increasingly embedded in the management justification — operators must perform theater to legitimize interception infrastructure.
 *
 * PERSPECTIVAL GAP:
 *   The eight-way perspectival split reveals how the same infrastructure constraint exhibits fundamentally different structural patterns depending on who is measuring. The end user sees pure predation (Snare): interception is unavoidable, non-consensual, and extractive with no offsetting benefit. The network operator sees infrastructure coordination (Rope): middleboxes solve real management problems (DDoS, congestion, caching) that benefit the entire network and users indirectly. Both agents are correct in their observations — the gap is not disagreement but rather different causal roles. The operator's benefit (operational control, cost savings) coexists with the user's cost (surveillance, censorship). This is the definition of Tangled Rope from the perspective of organized agents (encryption standards bodies, ISPs transitioning to encryption-friendly architectures) who see both functions clearly and perceive extraction as an extractable component of the coordination mechanism. The analytical observer at civilizational timescale risks naturalizing the constraint as Mountain — 'intermediaries are necessary, therefore interception is necessary, therefore users must accept surveillance' — but this reasoning smuggles in institutional choices (centralized routing control, operator surveillance capabilities) as if they were technical laws. The false summit detector will flag this.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's experienced extractiveness (χ) is derived from base extractiveness (ε=0.52), directionality (d from beneficiary/victim + exit options), and scope (σ(S) for global scope ≈ 1.2). End users: victims + trapped exit → d≈0.95 → f(d)≈1.42 → χ ≈ 0.52×1.42×1.2 ≈ 0.89 (perceived as near-total extraction). Network operators: beneficiaries + arbitrage exit → d≈0.05 → f(d)≈-0.12 → χ ≈ 0.52×(-0.12)×1.2 ≈ -0.07 (perceived as subsidy/coordination). Encrypted users: victims + constrained exit → d≈0.68 → f(d)≈0.95 → χ ≈ 0.52×0.95×1.2 ≈ 0.59 (perceived as moderate extraction). The gap between beneficiary χ (negative) and victim χ (0.89) is maximal, reflecting that the constraint extracts in one direction while the institution frames it as coordination.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by exhibiting genuine Tangled Rope structure: both coordination and extraction are structurally real, not illusion. Network operators genuinely solve real problems (DDoS mitigation, traffic management). Users genuinely suffer surveillance and censorship. These are not contradictory — they coexist in the same mechanism. The theater ratio (0.58) indicates that some performative justification exists (operators sometimes invoke management benefits that are secondary to surveillance goals), but not enough to classify as pure Piton. The measurements show extraction increasing even as encryption technology (TLS 1.3) has removed easy content access, indicating operators are intensifying surveillance via metadata and behavioral analysis — this is extraction being actively engineered, not coordination cost. The suppression trajectory rising from 0.48 to 0.65 indicates that as user exit options improve (encryption adoption), the institutional enforcement required to suppress those options increases. This is the Tangled Rope signature: active enforcement + persistent victim group + real coordination function + asymmetric benefit distribution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    middlebox_necessity_vs_choice,
    'Are middleboxes technically necessary for network management, or do they represent an institutional choice to centralize control?',
    'Comparative analysis of network architectures: compare cost/complexity of middlebox-dependent vs. decentralized management (SDN, in-band signaling, explicit CDN coordination). Measure performance metrics and operational overhead for alternative implementations.',
    'If necessary: constraint reclassifies toward Rope for operators and Mountain for users (technical immutability). If choice: constraint reclassifies toward Snare for users (institutional extraction enforced through monopoly on routing infrastructure).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(middlebox_necessity_vs_choice, empirical, 'Whether middleboxes are technically necessary or institutionally chosen').

omega_variable(
    interception_capability_necessity,
    'Can middleboxes perform legitimate network management (DDoS mitigation, traffic shaping) without content inspection and user surveillance capability?',
    'Protocol analysis: identify minimum information requirements for each management function. Compare TLS 1.3 (metadata-only access) to earlier versions (full content access). Measure whether metadata alone (packet size, timing, frequency) suffices for management tasks.',
    'If inspection unnecessary: extractiveness drops significantly; constraint becomes Rope for operators. If inspection required: extractiveness remains high; extraction and coordination are genuinely coupled.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interception_capability_necessity, empirical, 'Whether content inspection is technically required for network management').

omega_variable(
    encryption_adoption_rate_threshold,
    'What adoption rate of end-to-end encryption technologies (TLS 1.3, encrypted DNS, VPN) breaks the middlebox interception constraint''s suppression mechanism?',
    'Time-series analysis of encryption adoption rates, correlation with middlebox effectiveness metrics (detection rates, content access success). Identify inflection point where middlebox-dependent revenue or control mechanisms fail.',
    'If threshold < 50% adoption: middleboxes remain effective for majority; suppression stays high. If threshold > 70% adoption: middleboxes become ineffective; suppression collapses; constraint reclassifies toward Rope or Piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(encryption_adoption_rate_threshold, empirical, 'Encryption adoption threshold that breaks middlebox suppression').

omega_variable(
    interception_for_surveillance_vs_management,
    'How much of middlebox deployment is justified by legitimate network management versus unauthorized surveillance and censorship?',
    'Jurisdictional analysis: survey middlebox deployment across regions with different legal/surveillance regimes. Correlate deployment intensity with documented human rights violations and censorship practices. Distinguish operators'' stated purpose (management) from actual use patterns (surveillance).',
    'If management-dominant (>70%): constraint is Tangled Rope with legitimate coordination component. If surveillance-dominant: constraint is Snare with thin management cover; extractiveness and suppression both rise substantially.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interception_for_surveillance_vs_management, empirical, 'Proportion of middlebox deployment driven by surveillance versus management').

omega_variable(
    false_summit_natural_law_status,
    'Is the inevitability of middlebox interception a natural law (immutable technical property) or a false summit (naturalized institutional choice)?',
    'Historical comparison: evaluate network designs that eliminate middleboxes (end-to-end encryption, fully decentralized routing, in-band coordination). Assess adoption barriers (institutional, economic, regulatory) versus technical impossibility. Identify who benefits from naturalizing the constraint as law versus choice.',
    'If natural law: Mountain classification holds; users must accept tradeoff. If false summit: Snare classification applies; the constraint is an enforcement mechanism for surveillance/control by powerful actors; policy alternatives exist.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_law_status, conceptual, 'Whether middlebox necessity is natural law or naturalized institutional choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(middlebox_interception, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mbox_tr_t0, middlebox_interception, theater_ratio, 0, 0.42).
narrative_ontology:measurement(mbox_tr_t5, middlebox_interception, theater_ratio, 5, 0.55).
narrative_ontology:measurement(mbox_tr_t10, middlebox_interception, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(mbox_be_t0, middlebox_interception, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(mbox_be_t5, middlebox_interception, base_extractiveness, 5, 0.47).
narrative_ontology:measurement(mbox_be_t10, middlebox_interception, base_extractiveness, 10, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(mbox_su_t0, middlebox_interception, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(mbox_su_t5, middlebox_interception, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(mbox_su_t10, middlebox_interception, suppression_requirement, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(middlebox_interception, enforcement_mechanism).
narrative_ontology:affects_constraint(middlebox_interception, end_to_end_encryption_adoption).
narrative_ontology:affects_constraint(middlebox_interception, network_operator_regulatory_capture).
narrative_ontology:affects_constraint(middlebox_interception, privacy_tool_prohibition).

% DUAL FORMULATION NOTE:
% Middlebox interception decomposes into three structurally distinct constraints: (1) middlebox_interception (this story) — the coordination/extraction hybrid at infrastructure layer; (2) end_to_end_encryption_adoption — users' technical response, forms a competing constraint with lower extractiveness; (3) network_operator_regulatory_capture — the institutional dynamics where operators extract regulatory benefit from government surveillance demands. The middlebox constraint influences both downstream constraints; both downstream constraints feed back to make middleboxes harder to deploy and more extraction-dependent.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(middlebox_interception, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
