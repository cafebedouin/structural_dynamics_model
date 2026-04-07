% ============================================================================
% CONSTRAINT STORY: regulatory_capture_logistics_sector
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_regulatory_capture_logistics_sector, []).

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
 *   constraint_id: regulatory_capture_logistics_sector
 *   human_readable: Regulatory Capture in the Logistics Sector
 *   domain: economic_policy/regulatory_governance
 *
 * SUMMARY:
 *   Regulatory capture in the logistics sector creates a structural
 *   constraint where incumbent carriers have systematically shaped
 *   transportation regulations to raise barriers against new market entrants
 *   while claiming to pursue safety and coordination. The constraint exhibits
 *   genuine coordination mechanisms (fuel surcharge transparency,
 *   cross-border permit standards, intermodal safety certifications) that are
 *   authentic public goods, layered with extractive mechanisms (selective
 *   licensing, compliance-cost asymmetries, market-allocation quotas) that
 *   primarily protect incumbent profit margins. The regulatory agency itself
 *   is captured not primarily through corruption but through professional
 *   identity-lock: transport ministry staff have built entire careers within
 *   the regulatory framework, creating a feedback loop where preserving the
 *   framework becomes an organizational survival instinct. The constraint's
 *   extractiveness has increased over the measurement interval (0.42 to 0.58)
 *   as regulatory complexity has accumulated, while theater ratio has also
 *   increased (0.55 to 0.68) indicating that many regulations have drifted
 *   toward pure compliance theater rather than functional coordination. The
 *   gig-economy disruptors (Uber Freight, digital logistics platforms)
 *   represent a structural sunset mechanism: digital platforms are building
 *   alternative coordination (GPS tracking, algorithmic dispatch, automated
 *   insurance underwriting) that bypasses traditional regulatory
 *   infrastructure. If these platforms mature and achieve regulatory
 *   legitimacy, traditional licensing regimes may become functionally
 *   obsolete within 10-15 years.
 *
 * KEY AGENTS:
 *   - Incumbent Carriers (FedEx, DHL, UPS regional divisions): Primary beneficiary (institutional/arbitrage) — capture regulatory benefits directly through influence over fuel surcharge rules, capacity allocation, and licensing thresholds; experience regulations as coordination tools that serve their interests
 *   - Small Logistics Operators: Primary victim (powerless/trapped) — face prohibitive licensing costs, insurance requirements written to favor large operators, and permit allocation that de facto excludes new entrants; no viable exit from the industry without abandonment
 *   - Regional Competitors: Secondary victim (moderate/constrained) — can operate but at high regulatory compliance cost; benefit from some coordination (safety standards) but bear extraction through asymmetric overhead
 *   - Transport Ministry / Regulatory Agency: Institutional actor (institutional/constrained, identity_locked) — gatekeepers of the regulatory system; structurally mobile (could rewrite regulations) but identity-locked through professional culture and career path embeddedness; see themselves as neutral coordinators rather than capture instruments
 *   - Gig-Economy Platforms: Organized agents (organized/mobile) — building alternative coordination pathways outside traditional regulatory regime; represent structural sunset to incumbent regulatory capture as platforms mature and prove alternative coordination methods work
 *   - Consumer Welfare / Logistics Market: Victim (powerless/trapped) — abstract collective good bearing cost of reduced competition, innovation suppression, and higher shipping costs; no organized voice in regulatory process
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(regulatory_capture_logistics_sector, 0.58).
domain_priors:suppression_score(regulatory_capture_logistics_sector, 0.62).
domain_priors:theater_ratio(regulatory_capture_logistics_sector, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(regulatory_capture_logistics_sector, extractiveness, 0.58).
narrative_ontology:constraint_metric(regulatory_capture_logistics_sector, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(regulatory_capture_logistics_sector, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(regulatory_capture_logistics_sector, tangled_rope).
narrative_ontology:human_readable(regulatory_capture_logistics_sector, "Regulatory Capture in the Logistics Sector").
narrative_ontology:topic_domain(regulatory_capture_logistics_sector, "economic_policy/regulatory_governance").

domain_priors:requires_active_enforcement(regulatory_capture_logistics_sector).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(regulatory_capture_logistics_sector, incumbent_carriers).
narrative_ontology:constraint_beneficiary(regulatory_capture_logistics_sector, freight_forwarding_oligopoly).
narrative_ontology:constraint_victim(regulatory_capture_logistics_sector, new_market_entrants).
narrative_ontology:constraint_victim(regulatory_capture_logistics_sector, small_operators).
narrative_ontology:constraint_victim(regulatory_capture_logistics_sector, consumer_welfare).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALL LOGISTICS OPERATOR (SNARE) — Trapped by licensing barriers, insurance requirements, and fuel surcharge regulations written by incumbent carriers through regulatory capture. No viable exit without abandoning the industry. Bears maximum extraction via compliance costs and market-exclusion mechanisms embedded in regulations.
constraint_indexing:constraint_classification(regulatory_capture_logistics_sector, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REGIONAL COMPETITOR (TANGLED ROPE) — Structurally mobile but constrained by regulatory compliance costs and coordination requirements (fuel reporting, safety certifications, cross-border permits). Benefits from coordination mechanisms (safety standards, interoperability agreements) but bears extraction through asymmetric regulatory overhead that favors incumbents with compliance infrastructure.
constraint_indexing:constraint_classification(regulatory_capture_logistics_sector, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INCUMBENT CARRIER (ROPE) — Experiences regulations as coordination mechanism: fuel surcharge protocols, intermodal standards, capacity allocation rules enable predictable market operations. Benefits from high-barrier entry regulations written collaboratively with transport ministry. Net beneficiary position — extraction runs toward this actor.
constraint_indexing:constraint_classification(regulatory_capture_logistics_sector, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REGULATORY AGENCY (TANGLED ROPE, IDENTITY_LOCKED) — The regulator is structurally mobile (could in principle rewrite regulations to lower barriers) but identity-locked: its professional identity is constituted through the regulatory framework it inherited and maintains. Agency staff career paths are embedded in the incumbent-carrier relationship. Sees itself as implementing stable, predictable rules rather than protecting incumbents. Genuine coordination function exists (safety, intermodal coordination) alongside extractive asymmetry imposed on entrants.
constraint_indexing:constraint_classification(regulatory_capture_logistics_sector, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: NATIONAL LOGISTICS FRAMEWORK (PITON) — The regulatory structure (multi-generational licensing regimes, permit allocation, safety certification requirements) is substantially performative at the civilizational horizon. Original coordination purpose (preventing predatory pricing, ensuring safety standards) has been displaced by rent-seeking theater. The framework persists through institutional inertia despite alternatives (dynamic pricing, digital credentials, decentralized compliance verification) existing outside the formal system. Theater ratio reflects that much regulatory overhead is symbolic compliance rather than safety-functional.
constraint_indexing:constraint_classification(regulatory_capture_logistics_sector, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: GIG-ECONOMY DISRUPTORS (SCAFFOLD) — Organized alternative service providers (Uber Freight, digital logistics platforms, autonomous routing systems) see regulatory capture as a temporary coordination failure with structural sunset. These platforms are building parallel verification and coordination systems (GPS tracking, automated insurance, algorithmic dispatch) that bypass traditional permit regimes. As digital platforms mature, traditional licensing loses functional value. Sunset clause is real: within 10-15 years, digital supply chain verification may make traditional licensing bureaus structurally obsolete.
constraint_indexing:constraint_classification(regulatory_capture_logistics_sector, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (FALSE MOUNTAIN) — From the analytical/civilizational perspective, the constraint risks appearing as natural law: 'Complex logistics systems require coordination, which inevitably creates incumbent advantage.' This perspective naturalizes what is actually a contingent regulatory choice. The false summit will be detected by the engine's structure-checking gates: the beneficiary/victim declarations reveal the asymmetry is institutional, not inherent.
constraint_indexing:constraint_classification(regulatory_capture_logistics_sector, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(regulatory_capture_logistics_sector_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(regulatory_capture_logistics_sector, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(regulatory_capture_logistics_sector, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(regulatory_capture_logistics_sector, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(regulatory_capture_logistics_sector, TR),
    TR >= 0.70.

:- end_tests(regulatory_capture_logistics_sector_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The incumbent carriers extract through regulatory barriers that maintain high margins in a market that would otherwise be competitive. The extractiveness is not as severe as a pure snare (0.75+) because genuine coordination functions exist — safety standards, intermodal compatibility, fuel efficiency reporting. But the coordination is substantially exceeded by rent extraction: licenses are rationed beyond safety requirements, compliance costs are designed to be fixed (burdening small operators disproportionately), and permit allocation mechanisms preserve market share for incumbents. The extractiveness trajectory (0.42→0.58) reflects regulatory accumulation: each new regulation adds compliance overhead; few are ever removed. Suppression (0.62): Moderate-high. Barriers to entry are substantial: licensing costs ($10k-$50k per vehicle depending on jurisdiction), insurance ($5k-$15k annually), and compliance labor. But suppression is not absolute: some new entrants do emerge, primarily through scale (regional operations that amortize overhead) or evasion (informal sectors). Gig platforms are structurally reducing suppression through alternative coordination (digital insurance, algorithmic dispatch). Theater ratio (0.68): High and rising. Much regulatory overhead is performative: safety certifications that repeat information already present in vehicle registries, fuel surcharge reporting that mirrors market prices, permit allocation that follows predictable administrative rules rather than dynamic need assessment. The theater has increased as regulations have accumulated without consolidation — regulatory bodies maintain legacy rules even after their functional purpose expires. Digital platforms operate at theater ratio ~0.20 (automated compliance, no ritual gatekeepers), creating pressure for traditional framework to either reform or be bypassed.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between incumbent carrier (Rope) and small operator (Snare) is maximal — the same constraint classifies as pure coordination from one perspective and pure extraction from the other. This gap is not measurement error; it is structural truth. The constraint genuinely coordinates (ensures safety standards, enables intermodal efficiency) for actors with the scale to participate in the coordinating system, while simultaneously extracting from actors that lack that scale. The captured regulator's identity-lock is critical to understanding persistence: the regulator is not malicious but is captured through professional culture. The agency staff have organized their careers around the regulatory framework; they see regulatory complexity as job-security and regulatory reform as organizational threat. Shifting their classification from Tangled Rope to Rope (if regulations were simplified) would require internal cultural revolution, not just new rules. The gig-platform perspective reveals a structural exit: once alternative coordination systems prove reliable (which they are doing), the traditional regulatory regime's claim to necessity collapses. This is why the scaffold classification is credible — the sunset is not dependent on regulatory reform but on technological displacement.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective derives from the agent's structural position relative to the constraint. Incumbent carriers (institutional/arbitrage) have low d (≈0.15) because they are beneficiaries with mobile exit options — they could operate in unregulated markets but choose not to because the regulatory regime protects their margins. Small operators (powerless/trapped) have high d (≈0.92) because they are victims with no exit — they cannot leave logistics without career abandonment. Regional competitors (moderate/constrained) have intermediate d (≈0.65) because they are partially trapped by compliance costs but have some scale to absorb overhead. The captured regulator (institutional/identity_locked) has intermediate d (≈0.58) because identity-lock makes exit costly psychologically even if structurally possible — leaving would require reconstructing professional identity. The analytical perspective has d≈0.72 (observer of the asymmetry). The sigmoid function f(d) translates these d values into experienced extractiveness χ: low-d beneficiaries experience negative or weak extraction (the constraint subsidizes them); high-d victims experience maximum extraction (the constraint extracts from them). The directionality derivation reveals why beneficiaries and victims classify differently: they occupy opposite positions in the extraction flow.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in this constraint is NOT resolved. The classification claim is Tangled Rope: the constraint has both genuine coordination function (safety, intermodal standards) and asymmetric extraction (barrier maintenance, rent collection). The risk is mis-classification as pure Rope (pure coordination) by regulators who see only the coordination machinery, or as pure Snare (pure extraction) by entrants who see only the barriers. The indexical framework prevents this by requiring multiple perspectives and explicit beneficiary/victim declarations. The mandatrophy is managed by declaring both beneficiaries (incumbent carriers benefit from coordination + protection) and victims (new entrants bear extraction without coordination benefit). The analytics derive directionality from this declaration, producing perspectival disagreement (carrier sees Rope, entrant sees Snare, analyst sees Tangled Rope). This is the correct analytical outcome: the same constraint is both coordination and extraction depending on your structural position. Full mandatrophy resolution would require either: (a) demonstrating that the coordination could be achieved with lower extraction (moving toward Rope), or (b) demonstrating that the extraction is not necessary for coordination (moving toward Snare). The current evidence suggests a genuine hybrid: some extraction is necessary to fund regulatory infrastructure; much is excess rent protection. The tangled rope classification captures this ambiguity appropriately.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    regulatory_capture_definition_boundary,
    'Where is the line between legitimate incumbent knowledge-advantage and extractive regulatory capture?',
    'Comparative analysis of entry barriers across jurisdictions with different regulatory models (EU digital transport licensing vs. US trucking deregulation vs. China central allocation); measurement of compliance cost ratios (incumbent vs. entrant)',
    'If entry barriers are largely knowledge/infrastructure capital: capture is moderate (Rope with extraction overlay = Tangled Rope). If entry barriers are primarily regulatory/artificial: capture is severe (Snare from entrant perspective). Classification hinges on this resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_definition_boundary, empirical, 'Definition boundary between legitimate incumbent advantage and regulatory capture').

omega_variable(
    safety_coordination_vs_rent_extraction,
    'How much of the regulatory overhead (licensing, certifications, permits) serves genuine safety coordination vs. rent extraction?',
    'Safety incident analysis comparing jurisdictions with high vs. low regulatory density; identification of regulations with safety correlation vs. regulations with only incumbent-protection correlation',
    'If safety function dominates: classification shifts toward Rope (coordination-primary). If rent extraction dominates: classification shifts toward Snare (extraction-primary). Current assumption is mixed (Tangled Rope = both present); resolution determines the weighting.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(safety_coordination_vs_rent_extraction, empirical, 'Safety function vs. rent extraction in regulatory overhead').

omega_variable(
    digital_disruption_timeline_credibility,
    'Is the scaffold perspective''s sunset timeline (10-15 years for digital platforms to obsolete traditional licensing) empirically grounded or aspirational?',
    'Tracking digital logistics market penetration rates, regulatory response speed to digital innovations, and whether traditional licensing bodies are actually losing functional authority or adapting to co-govern digital platforms',
    'If timeline is real: scaffold perspective is structural, not wishful. Extractiveness will decline as platforms mature. If timeline is extended or regulatory bodies adapt to capture platforms too: scaffold is aspirational, and the constraint may persist or mutate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(digital_disruption_timeline_credibility, empirical, 'Credibility of digital platform sunset timeline for regulatory capture').

omega_variable(
    agency_identity_lock_mechanism,
    'Is the regulatory agency''s constraint-maintenance truly driven by identity-locked professionalization, or is it driven by explicit political capture (corrupt incentives, revolving-door lobbying)?',
    'Analysis of agency staff career trajectories, post-employment industry positions, lobbying expenditure patterns, and internal agency culture documentation (leaked emails, interviews); distinction between structural incentive-lock vs. corruption',
    'If identity-locked: the constraint is maintainable through internal cultural shift (new training paradigm, new career incentives). If corruption-driven: requires external enforcement or political intervention. Classification type may shift from Tangled Rope to Snare if corruption predominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(agency_identity_lock_mechanism, empirical, 'Whether agency constraint is identity-locked or corruption-driven').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(regulatory_capture_logistics_sector, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rcls_tr_t0, regulatory_capture_logistics_sector, theater_ratio, 0, 0.55).
narrative_ontology:measurement(rcls_tr_t5, regulatory_capture_logistics_sector, theater_ratio, 5, 0.62).
narrative_ontology:measurement(rcls_tr_t10, regulatory_capture_logistics_sector, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(rcls_be_t0, regulatory_capture_logistics_sector, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(rcls_be_t5, regulatory_capture_logistics_sector, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(rcls_be_t10, regulatory_capture_logistics_sector, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(regulatory_capture_logistics_sector, enforcement_mechanism).
narrative_ontology:affects_constraint(regulatory_capture_logistics_sector, supply_chain_resilience_externality).
narrative_ontology:affects_constraint(regulatory_capture_logistics_sector, last_mile_delivery_labor_extraction).

% DUAL FORMULATION NOTE:
% Regulatory capture in logistics is upstream of both supply-chain vulnerability (reliance on incumbent carriers creates bottlenecks) and labor extraction in last-mile delivery (regulatory barriers protect incumbent employment models against competition that might improve worker conditions). The network links show contamination: capture in the regulatory domain propagates to reduced competition and innovation in service delivery.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(regulatory_capture_logistics_sector, institutional, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
