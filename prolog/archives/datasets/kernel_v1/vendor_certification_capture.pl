% ============================================================================
% CONSTRAINT STORY: vendor_certification_capture
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vendor_certification_capture, []).

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
 *   constraint_id: vendor_certification_capture
 *   human_readable: Vendor Certification Capture in Supply Chain Governance
 *   domain: economic_governance/regulatory_capture
 *
 * SUMMARY:
 *   Vendor certification capture operates as a private-sector regulatory
 *   mechanism where incumbent vendors extract rents through control of
 *   certification standards and audit bodies. Unlike traditional government
 *   regulatory capture, the constraint exhibits no direct state enforcement —
 *   its power derives from supply chain network effects and informational
 *   asymmetries. Incumbent vendors participate in standard-setting
 *   committees, fund audit operations, and maintain board representation in
 *   certification bodies. New entrants face asymmetrically high compliance
 *   costs, opaque audit processes, and appeals mechanisms controlled by the
 *   bodies that set standards. The constraint exhibits all hallmarks of
 *   Tangled Rope: genuine coordination function (certification does
 *   communicate quality specifications and enable supply chain integration)
 *   coupled with asymmetric extraction (incumbents benefit from protection;
 *   entrants bear burdens). The theater_ratio (0.68) reflects increasing
 *   performative content: audit processes have expanded in bureaucratic
 *   complexity while their actual verification power has stagnated,
 *   particularly as product complexity has outpaced auditor expertise. The
 *   upward trend in extractiveness (0.35 → 0.58) over the interval reflects
 *   accumulating barriers to entry as incumbents have refined capture
 *   mechanisms and incumbents have consolidated control over multiple
 *   certification bodies. Suppression (0.65) reflects high structural
 *   barriers: emerging competitors face certification costs, timeline delays,
 *   and reputational risk if rejected, with limited appeals. The constraint
 *   is characterized by perspectival heterogeneity: incumbents perceive pure
 *   coordination (Rope), emerging competitors perceive pure extraction
 *   (Snare), mid-market suppliers perceive mixed dynamics (Tangled Rope), and
 *   organized coalitions building open standards perceive a temporary regime
 *   with a sunset (Scaffold).
 *
 * KEY AGENTS:
 *   - Incumbent Vendors: Primary beneficiary (institutional/arbitrage) — capture certification bodies; benefit from supply chain lock-in and barrier to entry; experience the constraint as coordination with minimal extraction overhead
 *   - Certification Bodies: Secondary beneficiary (institutional/arbitrage) — funded by incumbent audit fees; perceive themselves as neutral standard-setters; revenue dependence structurally aligns incentives with incumbents
 *   - Emerging Competitors: Primary victim (powerless/trapped) — face asymmetrically high certification costs; opaque audit standards; limited appeals; no meaningful exit options
 *   - Supply Chain Integrity: Secondary victim (powerless/trapped) — epistemic commons bears cost of standards drift toward incumbent convenience; no mechanism for non-participating actors to challenge captured standards
 *   - Mid-Market Suppliers: Mixed position (moderate/constrained) — some benefit from incumbent protection (reduced local competition); others constrained by standards they cannot influence; can exit at significant cost
 *   - Large Buyers (Oligopsonic Pressure): Powerful position (powerful/arbitrage) — sufficient purchasing power to demand alternative verification; can enforce de-certification; constrained by supplier bottlenecks created by incumbent capture
 *   - Open Standards Coalition: Organized position (organized/constrained) — building alternatives (blockchain verification, third-party audits, multi-stakeholder consortia); perceive captured regime as temporary with sunset pathway; face network effect barriers
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vendor_certification_capture, 0.58).
domain_priors:suppression_score(vendor_certification_capture, 0.65).
domain_priors:theater_ratio(vendor_certification_capture, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vendor_certification_capture, extractiveness, 0.58).
narrative_ontology:constraint_metric(vendor_certification_capture, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(vendor_certification_capture, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vendor_certification_capture, tangled_rope).
narrative_ontology:human_readable(vendor_certification_capture, "Vendor Certification Capture in Supply Chain Governance").
narrative_ontology:topic_domain(vendor_certification_capture, "economic_governance/regulatory_capture").

domain_priors:requires_active_enforcement(vendor_certification_capture).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vendor_certification_capture, incumbent_vendors).
narrative_ontology:constraint_beneficiary(vendor_certification_capture, certification_bodies).
narrative_ontology:constraint_victim(vendor_certification_capture, emerging_competitors).
narrative_ontology:constraint_victim(vendor_certification_capture, supply_chain_integrity).
narrative_ontology:constraint_victim(vendor_certification_capture, downstream_consumers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EMERGING COMPETITOR (SNARE) — New market entrants face certification standards designed by and for incumbents. Compliance costs are asymmetrically high; audit processes are opaque; appeals mechanisms are controlled by the same bodies that set standards. No meaningful exit: must certify to access markets or stay out entirely. This is pure extraction from the perspectival position of the powerless competitor with no alternative pathways.
constraint_indexing:constraint_classification(vendor_certification_capture, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SUPPLY CHAIN INTEGRITY (SNARE) — The epistemic commons (collective trust in standards) bears the cost of captured standards that prioritize incumbent protection over actual quality verification. Standards drift toward incumbent convenience rather than genuine risk detection. No mechanism for non-participating actors (actual consumers, future entrants) to challenge standards. Trapped by the authority of the certification regime itself.
constraint_indexing:constraint_classification(vendor_certification_capture, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: MID-MARKET SUPPLIERS (TANGLED ROPE) — Medium-sized suppliers have partial agency. They can challenge standards through internal committee processes or threaten to exit, but at significant cost (delistings, reputation damage). The certification system does provide genuine value (market access, quality coordination) alongside the extraction. Some suppliers benefit from incumbent protection (reduced local competition), creating coalition fragmentation.
constraint_indexing:constraint_classification(vendor_certification_capture, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: INCUMBENT VENDORS (ROPE) — The primary beneficiary experiences the constraint as pure coordination: standards communicate quality specifications, enable supply chain integration, and establish shared practices. Incumbents have arbitrage options (self-certification, own audit bodies, ecosystem lock-in via API/format standards). They perceive the certification regime as solving a genuine collective action problem with minimal overhead from their perspective.
constraint_indexing:constraint_classification(vendor_certification_capture, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: CERTIFICATION BODIES (ROPE) — Funded by audit fees and dues from incumbent members, certification bodies experience the constraint as coordination. They provide genuine verification services and maintain technical standards. The institutional structure (incumbent board membership, audit fee revenue) aligns their incentives with incumbents, but they perceive themselves as neutral standard-setters, not as capture agents.
constraint_indexing:constraint_classification(vendor_certification_capture, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: LARGE BUYERS (TANGLED ROPE) — Mega-retailers (Walmart, Amazon, major OEMs) have enough purchasing power to demand alternative verification or private audits. They experience the constraint as mixed: certification bodies coordinate essential standards while also extracting audit fees, and the incumbent favoritism creates supply bottlenecks that raise input costs. These buyers can enforce de-certification or fund competing standards, making them constrained rather than trapped.
constraint_indexing:constraint_classification(vendor_certification_capture, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: OPEN STANDARDS COALITION (SCAFFOLD) — Emerging alternatives (third-party audit networks, blockchain-based verification, industry consortia with non-incumbent leadership, government reference standards) are building parallel certification pathways. These organized actors see the captured regime as temporary, with a sunset: distributed verification, open-source audit protocols, and multi-stakeholder governance models are eroding the incumbent-controlled bodies' monopoly. The constraint is classified as scaffold from this perspective because the organized coalition has agency and perceives a transition pathway.
constraint_indexing:constraint_classification(vendor_certification_capture, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: LEGACY CERTIFICATION REGIME (PITON) — At civilizational scale, the constraint shows signs of degradation. The original coordination function (assurance of minimum quality) persists, but the regime increasingly runs on theater: expensive audits with limited verification power, bureaucratic compliance that doesn't track actual product quality, and institutional inertia (firms maintain certifications because 'everyone has them'). The theater_ratio (0.68) reflects this: substantial performative content maintaining the structure despite declining functional value.
constraint_indexing:constraint_classification(vendor_certification_capture, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, information asymmetries in supply chains create an inherent need for credible signaling. Certification is a natural institutional response to the akismos problem (buyer cannot verify seller's quality claims). This perspective risks naturalizing the captured certification regime as an inevitable feature of markets. The engine's false summit detector will identify this as naturalization of a contingent institutional arrangement — the need for certification does not entail incumbent capture.
constraint_indexing:constraint_classification(vendor_certification_capture, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vendor_certification_capture_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(vendor_certification_capture, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(vendor_certification_capture, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(vendor_certification_capture, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(vendor_certification_capture, TR),
    TR >= 0.70.

:- end_tests(vendor_certification_capture_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, rising. The constraint extracts significant rents from new entrants (certification costs, timeline delays, risk of rejection) while providing genuine coordination benefits to the system overall. The rising trajectory (0.35 → 0.58) reflects incumbents refining capture mechanisms and consolidating control over multiple certification bodies. This is not maximum extraction (a Snare would show ≥0.66) because genuine coordination value exists alongside the extraction — smaller suppliers do benefit from access to certified supply chains. But the 23-point increase over the interval reflects drift toward pure extraction as incumbents have shifted standards to favor their technical capabilities and cost structures. Suppression (0.65): Moderate-high. New entrants face multiple structural barriers: high certification costs, complex audit processes, timeline delays (certification can require 6-18 months), publication of audit results in company-controlled databases, appeals processes controlled by the same bodies setting standards, reputational risk of failed certification, and network effects (buyers assume uncertified suppliers are lower quality). But suppression is not total (not 0.85+) because some emerging competitors do achieve certification, and large buyers can create alternative verification pathways. Theater ratio (0.68): High, rising. Audit processes have become increasingly ceremonial: auditors allocate time to checklist compliance and documentation review rather than actual risk verification; complex products outpace auditor technical expertise; standards have accumulated bureaucratic requirements (ISO procedures, documentation formats) without corresponding improvement in defect detection; incumbent firms with insider knowledge navigate audits efficiently while newer firms face capricious interpretations. The rising trend (0.42 → 0.68) reflects expansion of audit scope without increase in verification power — adding theater to maintain authority as real verification capability stagnates.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates classic regulatory capture dynamics but with an inverted institutional structure: the regulatory body (certification organization) is private, funded by those it regulates (incumbents), and controlled by them through board membership. The perspectival gap reveals how the same structural arrangement appears as coordination to beneficiaries and extraction to victims. Incumbents see legitimate standard-setting and quality assurance (Rope) — they perceive the certification regime as solving a genuine supply chain coordination problem with minimal overhead from their position. Emerging competitors see pure extraction (Snare) — standards are designed for incumbent convenience, audit processes opaque, appeals controlled by the capturing body. The analytical observer risks seeing an immutable natural law (Mountain) — information asymmetries require credible signaling, so certification is inherent to markets. But this naturalizes the captured regime. Certification is necessary; incumbent control of certification is not. The false summit classification reveals how the 'necessity of certification' is conflated with 'necessity of incumbent-controlled certification.' The scaffold perspective (open standards coalition) perceives the regime as temporary with a sunset pathway, identifying genuine alternatives (blockchain verification, independent audit networks) that could decouple coordination from extraction. The piton perspective reveals regime degradation: the original coordination function persists through institutional inertia and network effects, but the theater_ratio shows increasing performative content divorced from verification function.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is computed from the agent's structural relationship to the constraint. Incumbent vendors as beneficiaries with arbitrage options (can maintain own certification bodies, ecosystem lock-in through format/API standards, self-attestation) derive d ≈ 0.10 (mostly beneficiary, weak target position), producing low/negative effective extraction chi from their perspective — they experience Rope. Emerging competitors as victims with trapped exit (must certify or exit market) derive d ≈ 0.92 (mostly target), producing high chi → Snare from their perspective. Certification bodies as institutional actors with arbitrage (can migrate to different funding models in theory, but constrained by incumbent revenue dependence) derive d ≈ 0.35-0.45 (moderate target position), producing Rope from their perspective due to their perceived neutral role. Large buyers as powerful actors with arbitrage (can demand private audits, fund alternatives, or de-certify) derive d ≈ 0.30 (partial beneficiary through lower input costs in the short term, but victims of supply bottlenecks), producing constrained Tangled Rope. The perspectival gap reflects real structural differences in exit capacity, not mere subjective disagreement: incumbent vendors genuinely have arbitrage options that emerging competitors lack.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing how the same structural mechanism (incumbent control of certification bodies) produces different classifications from different agent perspectives because agents occupy structurally different positions relative to the extraction flow. The constraint is genuinely Tangled Rope at base (ε=0.58): coordination benefits are real (standards do enable supply chain integration) and asymmetric extraction is real (standards favor incumbents). The mandatrophy arises from asking: 'Is this primarily coordination or primarily extraction?' The answer is: it is both, and the proportion depends on your position in the supply chain. For incumbents, coordination dominates (Rope). For new entrants, extraction dominates (Snare). For the field epistemic commons, extraction dominates entirely (Snare). For organized coalitions with exit paths, the regime becomes temporary (Scaffold). The constraint is not mislabeled as Tangled Rope — the base metrics (ε=0.58, suppression=0.65, requires_active_enforcement=true, beneficiaries and victims both present) satisfy all gates. What resolves mandatrophy is recognizing that the perspectival heterogeneity IS the constraint's structural reality, not an artifact of measurement ambiguity. The engine's multi-perspective architecture is designed exactly for cases like this: one structural arrangement, six legitimate classifications from different observatories.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    incumbent_participation_versus_governance_capture,
    'Does incumbent participation in standard-setting committees constitute legitimate expertise contribution or structural capture?',
    'Analysis of standard evolution: comparison of standards set with incumbent participation vs. standards set by independent technical bodies; measurement of divergence between technical requirements and incumbent technical capabilities; exit velocity of emerging competitors post-standard revision.',
    'If legitimate expertise: constraint reclassifies toward Rope from multiple perspectives (genuine coordination). If structural capture: constraint classifies as Snare/Tangled Rope (extraction masked as coordination). The distinction determines whether the beneficiary perspective (incumbents) is accurately capturing their structural role or whether they are extracting through the appearance of coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(incumbent_participation_versus_governance_capture, empirical, 'Whether incumbent participation is expertise contribution or structural capture mechanism').

omega_variable(
    audit_cost_allocation_rationale,
    'Are audit costs structurally unavoidable coordination overhead or redistributive mechanisms designed to burden new entrants?',
    'Cost decomposition: auditor time allocation analysis showing which audit activities produce genuine verification vs. ritual compliance; comparison of audit costs across equivalent-scale enterprises with and without incumbent backing; correlation between audit cost and actual defect detection rates.',
    'If unavoidable overhead: suppression lowers (0.40-0.50 range) and constraint reclassifies toward Rope. If redistributive: suppression confirmed (≥0.65) and constraint holds Snare/Tangled Rope. Determines whether the suppression metric accurately reflects the capturing mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(audit_cost_allocation_rationale, empirical, 'Whether audit costs are coordination overhead or redistributive burden mechanism').

omega_variable(
    standard_technical_necessity_variance,
    'Do certified standards correlate with actual supply chain risk mitigation outcomes, or do standards drift toward incumbent convenience independent of risk reduction?',
    'Outcome analysis: comparison of defect rates, recall frequency, and supply chain disruption between certified and non-certified suppliers; measurement of standard requirement drift over time in relation to incumbent technical evolution; analysis of whether standards rejected by emerging competitors show higher real-world failure rates.',
    'If standards track real risk: constraint exhibits genuine coordination function and reclassifies toward Rope from many perspectives (extraction overstated). If standards drift toward incumbent convenience: constraint is pure extraction masked as coordination (Snare confirmed). Determines the core legitimacy of the entire certification regime.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(standard_technical_necessity_variance, empirical, 'Whether standards correlate with actual risk mitigation or drift toward incumbent convenience').

omega_variable(
    certification_body_independence_capture_mechanism,
    'Can certification bodies remain neutral standard-setters when funded primarily by audit fees from incumbent members, or does revenue dependence structure inevitable capture?',
    'Governance analysis: measurement of voting power distribution in standard-setting committees; analysis of fee revenue concentration among top vendors; comparison of standard difficulty/cost between different certification bodies with different revenue structures; case studies of standards rejected by bodies dependent vs. independent from incumbent funding.',
    'If bodies can remain neutral: Rope classification from institutional perspective is justified; constraint reclassifies as pure coordination. If revenue dependence is structurally capturing: institution perspective is partial (constrained by hidden incentives); constraint remains Tangled Rope/Snare. Determines whether the certification body is an independent actor or a captured intermediary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(certification_body_independence_capture_mechanism, empirical, 'Whether certification body revenue structure enables independence or structures capture').

omega_variable(
    emerging_alternative_pathway_viability,
    'Are blockchain-based verification, third-party audit networks, and multi-stakeholder consortia genuine alternatives with lower extraction, or do they reproduce the same capture dynamics at scale?',
    'Comparative analysis of alternative systems: measurement of entry barriers and certification costs in emerging pathways; identification of incumbent infiltration into ''alternative'' governance bodies; timeline projection for market share migration; analysis of whether alternatives reduce extraction or merely offer temporary bypass.',
    'If alternatives are viable: Scaffold classification is structural (sunset pathway exists) and constraint will eventually reclassify as temporary. If alternatives are captured or unviable: Scaffold is aspirational (theater, not function); constraint entrenchment is deeper than current metrics suggest; universe of perspectives must expand to show false exits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emerging_alternative_pathway_viability, empirical, 'Whether emerging alternatives are genuine exits or reproduce capture dynamics').

omega_variable(
    network_effects_lock_in_strength,
    'How strongly do network effects lock supply chains into incumbent-controlled certification regimes, and can coordination benefits be decoupled from incumbent extraction?',
    'Switching cost analysis: measurement of costs for a supply chain to migrate from incumbent certification to alternative; identification of whether switching costs derive from genuine coordination loss or from incumbent market control; case studies of successful migration to competing certification regimes.',
    'If network effects are weak: constraint is more temporary than current classification suggests; emerging pathways have higher viability; Scaffold classifications are more accurate. If network effects are strong: lock-in is structural; emerging pathways face higher barriers; Piton (degradation without exit) may become dominant over time. Determines whether the constraint''s temporal trajectory is toward resolution or deepening.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_effects_lock_in_strength, empirical, 'Lock-in strength and decoupling viability of coordination from extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vendor_certification_capture, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vcc_tr_t0, vendor_certification_capture, theater_ratio, 0, 0.42).
narrative_ontology:measurement(vcc_tr_t5, vendor_certification_capture, theater_ratio, 5, 0.56).
narrative_ontology:measurement(vcc_tr_t10, vendor_certification_capture, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(vcc_be_t0, vendor_certification_capture, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(vcc_be_t5, vendor_certification_capture, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(vcc_be_t10, vendor_certification_capture, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(vcc_su_t0, vendor_certification_capture, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(vcc_su_t5, vendor_certification_capture, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(vcc_su_t10, vendor_certification_capture, suppression_requirement, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vendor_certification_capture, resource_allocation).
narrative_ontology:affects_constraint(vendor_certification_capture, supply_chain_asymmetric_information).
narrative_ontology:affects_constraint(vendor_certification_capture, incumbent_firm_ecosystem_lock_in).
narrative_ontology:affects_constraint(vendor_certification_capture, small_supplier_market_exit).

% DUAL FORMULATION NOTE:
% Vendor certification capture is downstream of the fundamental asymmetric information problem in supply chains (buyers cannot verify seller quality claims) but represents a distinct structural constraint. The upstream asymmetric information constraint has ε ≈ 0.08 (Mountain: natural feature of market exchange). The certification capture constraint has ε ≈ 0.58 (Tangled Rope: contingent institutional arrangement that could be designed differently). The constraint family is composed of: (1) supply_chain_asymmetric_information (Mountain, ε≈0.08), (2) vendor_certification_capture (Tangled Rope, ε≈0.58), and (3) emerging alternatives pathway (Scaffold, ε≈0.20). All three stories should be linked via network.affects_constraints to show that certification capture emerges from the combination of asymmetric information (necessary base condition) and incumbent-captured governance structures (contingent institutional design).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vendor_certification_capture, institutional, 0.38).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
