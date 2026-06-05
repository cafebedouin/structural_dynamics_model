% ============================================================================
% CONSTRAINT STORY: gdpr_enforcement_structure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gdpr_enforcement_structure, []).

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
 *   constraint_id: gdpr_enforcement_structure
 *   human_readable: GDPR Enforcement Structure: Rights vs. Innovation Coordination
 *   domain: technology_governance/privacy_law/innovation_policy
 *
 * SUMMARY:
 *   The GDPR enforcement structure creates a multi-layered constraint
 *   operating simultaneously as data rights coordination mechanism
 *   (establishing norms for EU digital governance), extraction apparatus
 *   (asymmetric compliance burden favoring incumbents), and performative
 *   compliance ritual (theater ratio rising from 0.35 to 0.58). The
 *   constraint exhibits the diagnostic signature of mandatrophy resolution:
 *   the same institutional structure appears as pure extraction (snare) to
 *   trapped data subjects, mixed coordination-extraction (tangled rope) to
 *   moderate innovation entrants and large platforms, pure coordination
 *   (rope) to regulatory authorities with institutional power, temporary
 *   scaffolding (scaffold) to organized digital rights coalitions with sunset
 *   logic, degraded ritual (piton) from civilizational perspective, and false
 *   natural law (mountain with FSM firing) from analytical views that
 *   naturalize contingent arrangements. The measurement trajectory shows base
 *   extractiveness rising from 0.25 (initial phase emphasizing rights
 *   declaration) to 0.52 (enforcement maturation where practical compliance
 *   burden exceeds formal scope), theater ratio rising from 0.35 (real
 *   regulatory action) to 0.58 (increasing emphasis on compliance theater:
 *   cookie consent, privacy policies, DPO appointments), and suppression
 *   requirement rising from 0.48 to 0.65 (enforcement infrastructure becoming
 *   more intrusive and comprehensive). This trajectory indicates the
 *   constraint is not reaching stable equilibrium but rather consolidating
 *   into a new form where performative compliance and real enforcement
 *   coexist.
 *
 * KEY AGENTS:
 *   - Data Subjects: Powerless/trapped (primary victims) — hold formal rights (access, deletion, portability) but enforcement mechanisms are inaccessible; regulators lack resources for individual complaints
 *   - EU Data Protection Authorities: Institutional/arbitrage (primary beneficiaries) — expanded jurisdiction, growing budgets, direct enforcement powers; gain bargaining leverage over global platforms
 *   - Innovation Entrants: Moderate/constrained (secondary victims) — face high compliance costs per unit revenue; cannot absorb fines or lobby for relief like incumbents
 *   - Established Platforms: Organized/arbitrage (secondary beneficiaries) — compliance infrastructure built during early GDPR period now constitutes barrier to entry; can absorb fines and reshape business model around regulatory pressure
 *   - Digital Rights Coalition: Organized/constrained (scaffold agents) — civil society seeing temporary coordination structure; pushing toward technical/architectural alternatives that reduce reliance on legal enforcement
 *   - Compliance Theater Institution: Institutional/arbitrage (piton perspective) — cookie banners, privacy policies, Data Protection Officers persist through inertia despite low functional enforcement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gdpr_enforcement_structure, 0.52).
domain_priors:suppression_score(gdpr_enforcement_structure, 0.65).
domain_priors:theater_ratio(gdpr_enforcement_structure, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gdpr_enforcement_structure, extractiveness, 0.52).
narrative_ontology:constraint_metric(gdpr_enforcement_structure, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(gdpr_enforcement_structure, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gdpr_enforcement_structure, tangled_rope).
narrative_ontology:human_readable(gdpr_enforcement_structure, "GDPR Enforcement Structure: Rights vs. Innovation Coordination").
narrative_ontology:topic_domain(gdpr_enforcement_structure, "technology_governance/privacy_law/innovation_policy").

domain_priors:requires_active_enforcement(gdpr_enforcement_structure).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gdpr_enforcement_structure, european_regulators).
narrative_ontology:constraint_beneficiary(gdpr_enforcement_structure, large_established_platforms).
narrative_ontology:constraint_victim(gdpr_enforcement_structure, data_subjects).
narrative_ontology:constraint_victim(gdpr_enforcement_structure, innovation_entrants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DATA SUBJECT (SNARE) — Individual EU residents have no practical exit from data collection within digital ecosystems. GDPR grants formal rights (access, deletion, portability) but enforcement mechanisms are weak: regulators are resource-constrained, class actions are rare, individual litigation is prohibitively expensive. The subject bears the full cost of non-compliance through exposure (data breaches, profiling, manipulation) while legal redress is inaccessible. Cannot exit the ecosystem without abandoning digital participation entirely. Maximum extraction experience — formal rights without practical remedy channels.
constraint_indexing:constraint_classification(gdpr_enforcement_structure, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INNOVATION ENTRANT (TANGLED ROPE) — Startup or scale-up technology company faces simultaneous coordination benefit and extraction cost. GDPR provides legitimacy infrastructure (compliance signals competitive advantage in privacy-conscious EU market) but imposes asymmetric compliance burden: legal interpretation is costly, enforcement is unpredictable, and regulatory fines scale with global revenue (hitting small innovators harder per dollar of actual harm than established platforms). Constrained exit (regulatory compliance costs prevent some business models but don't eliminate competitive participation). Mixed experience: genuine coordination function (privacy-respecting market differentiation) embedded in extraction mechanism (burden allocation favors incumbents).
constraint_indexing:constraint_classification(gdpr_enforcement_structure, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: REGULATORY AUTHORITY (ROPE) — EU data protection authorities (DPAs) experience GDPR as pure coordination mechanism from their structural position. Immediate enforcement powers, clear legal mandate, established hierarchy, and growing political support enable regulatory agencies to set norms across the EU ecosystem. They can impose fines, order remediation, and shape platform behavior. Exit option: arbitrage (they can leverage EU market power to influence global standards; GDPR extraterritorial reach is their asset). Net beneficiary — extraction flows toward regulatory authority through expanded jurisdiction and resources. They experience the constraint as coordination: establishing data protection norms across fragmented markets.
constraint_indexing:constraint_classification(gdpr_enforcement_structure, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: ESTABLISHED PLATFORM (TANGLED ROPE) — Large tech companies (Meta, Google, Amazon) experience GDPR as mixed coordination-extraction. Coordination benefit: GDPR legitimizes data governance as a competitive dimension, raising barriers to entry (small competitors cannot afford compliance infrastructure that incumbents have already built). Extraction cost: high fines (€50M or 4% global revenue, whichever is higher) and forced transparency create operational friction. But arbitrage exit is available: large platforms can absorb fines, lobby for regulatory relief, or shift architecture (e.g., Meta's 'legal basis' reframing for tracking without explicit consent). They experience extraction but have agency to reshape it. Mixed — genuine coordination of privacy norms alongside asymmetric benefit capture.
constraint_indexing:constraint_classification(gdpr_enforcement_structure, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: DIGITAL RIGHTS COALITION (SCAFFOLD) — Civil society organizations (NOYB, EDRi, AccessNow) see GDPR enforcement as a temporary coordination structure with sunset logic. The constraint exists as a transition mechanism: enabling data subjects to develop collective awareness and bargaining power while privacy-by-design and technological alternatives (federated systems, end-to-end encryption, consent management platforms) mature. Constrained exit (they cannot exit EU governance structures) but clear sunset: as technical alternatives reduce data collection necessity, legal enforcement becomes backstop rather than primary mechanism. Organized agents with exit path — classification depends on whether the sunset is real or aspirational.
constraint_indexing:constraint_classification(gdpr_enforcement_structure, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: COMPLIANCE THEATER INSTITUTION (PITON) — From a civilizational view, GDPR enforcement has degraded into performative compliance: companies deploy cookie banners, privacy policies, and Data Protection Officer roles that satisfy formal requirements without meaningfully constraining data collection practices. The regulation persists through institutional inertia (regulatory structure, legal requirement, EU political commitment) despite diminishing functional enforcement capacity. Theater ratio (0.58) reflects this: significant portion of GDPR implementation is box-ticking ritual rather than structural limitation on data access.
constraint_indexing:constraint_classification(gdpr_enforcement_structure, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / LEGAL POSITIVIST VIEW (MOUNTAIN) — From a civilizational/universal perspective, GDPR enforcement is an immutable structural feature of European digital governance: the formal legal text is stable, the regulatory hierarchy is fixed, and the constraint persists regardless of technology change. This perspective naturalizes the current enforcement structure as the settled legal order. However, the false summit detector will flag this: the extractiveness metric (0.52) and suppression (0.65) reveal contingent institutional arrangements (resource levels, political will, enforcement priority allocation) that could change, not immutable laws.
constraint_indexing:constraint_classification(gdpr_enforcement_structure, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gdpr_enforcement_structure_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(gdpr_enforcement_structure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gdpr_enforcement_structure, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(gdpr_enforcement_structure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(gdpr_enforcement_structure, TR),
    TR >= 0.70.

:- end_tests(gdpr_enforcement_structure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts from data subjects (no practical remedies despite formal rights) and innovation entrants (compliance cost per revenue is higher than for incumbents). But extraction is not maximal because regulatory authorities have constrained resources, and large platforms can absorb and reshape responses. The rising trajectory from 0.25 to 0.52 indicates that initial GDPR phase emphasized rights declaration over enforcement; maturation has revealed the enforcement capacity gap, increasing practical extraction. Suppression (0.65): High. Data subjects cannot exit digital ecosystems without massive life disruption; platforms face legal prohibition on certain data practices; innovation entrants cannot legally operate in EU market without compliance infrastructure. Suppression is structural but not absolute — regulatory relief mechanisms exist, and some companies have successfully negotiated compliance paths. Theater ratio (0.58): Moderate-high. Significant portion of GDPR compliance is performative: cookie banners do not meaningfully inform users (comprehension studies show ~10% actual understanding); privacy policies are legal artifacts unread by users; Data Protection Officer roles are often compliance check-boxes. But some enforcement is real (fines issued, data erasure orders, architectural changes mandated) — hence moderate theater rather than piton-level 0.70+. Rising trajectory reflects increasing emphasis on compliance theater as real enforcement encounters jurisdictional limits.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival divergence. Data subjects see trapped snare (no practical exit, formal rights without remedy). Innovation entrants see constrained tangled rope (coordination benefit through legitimacy + asymmetric extraction burden). Large platforms see organized tangled rope with arbitrage (can reshape compliance through lobbying, business model pivots, or jurisdictional leverage). Regulatory authorities see institutional rope (pure coordination from their perspective — they gain expanded jurisdiction and resources). Digital rights coalitions see generational scaffold (temporary structure while technical alternatives mature). The piton perspective reveals performative compliance masking low functional enforcement. The mountain perspective attempts to naturalize current structure as immutable EU governance, but the false summit detector will fire given the beneficiary/victim structure and rising theater ratio. The perspectival gaps expose the constraint's actual logic: a regulatory framework designed to coordinate privacy protection has been captured and converted into an incumbent protection mechanism (barrier to entry) + compliance theater (box-ticking) + selective enforcement (resource constraints create randomness in DPA targeting).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality varies dramatically across agent structural positions. Data subjects (powerless/trapped): d ≈ 0.95 (full victims) — they have structural mobility in principle but cannot exit from data collection without abandoning digital participation; trapped exit option locks them as extraction targets. Regulatory authorities (institutional/arbitrage): d ≈ 0.10 (net beneficiaries) — their power increased with GDPR; arbitrage exit option means they can leverage EU market to influence global standards; extracted from them is false, extraction flows toward them. Innovation entrants (moderate/constrained): d ≈ 0.70 (victims but with some agency) — constrained exit means they can still operate but at higher cost; they bear asymmetric compliance burden. Established platforms (organized/arbitrage): d ≈ 0.35 (mixed, leaning toward benefit) — arbitrage exit means they can reshape compliance through architectural pivots, lobbying, or business model changes; early compliance investment now protects them from competition. Digital rights coalition (organized/constrained): d ≈ 0.45 (symmetric costs/benefits) — constrained exit but genuine coordination function they are advancing; their agency is high enough to reshape the constraint through technical/policy alternatives. The piton institution: d ≈ 0.20 (beneficiary, inertial) — exists because no one has sufficient power to dismantle it; maintained through institutional path dependency rather than active benefit extraction. These directional values explain why no single classification captures the constraint: directionality is fundamentally heterogeneous across the agent space.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exemplifies mandatrophy resolution through perspectival multiplicity. The core mandatrophy is: 'Is GDPR a coordination mechanism establishing privacy norms, or an extraction mechanism embedding incumbent advantage while creating performative compliance theater?' The resolution is that BOTH are true, from different structural positions. Data subjects experience it as extraction (snare). Regulators experience it as coordination (rope). Innovation entrants experience it as mixed (tangled rope). The 'correct' answer is not to pick one perspective but to recognize that the constraint is a partially-decomposed system: what appears as coordination from the regulatory perspective is extraction from the data subject perspective, using the same legal machinery. The rising theater ratio (0.35 → 0.58) indicates the system is not reaching stable equilibrium but rather layering compliance theater on top of real enforcement — a signature of mandatrophy in progress. The omega variables address the under-determined elements: consent mechanism sufficiency (is formal consent real or theater?), regulatory capacity vs. mandate (is rope real or piton?), fine structure design (is extraction intentional or accidental?), and interpretation stability (is this one constraint or multiple contested readings?). Resolving these omegas would not eliminate the perspectival multiplicity but would sharpen the boundaries between snare (data subject), tangled rope (entrant/incumbent), and rope (regulator) classifications.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consent_mechanism_sufficiency,
    'Does informed consent (as currently operationalized through privacy policies and cookie banners) constitute genuine data subject agency or is it performative compliance that naturalizes consent as cover for inevitable data collection?',
    'Empirical study of consent comprehension rates; analysis of actual data flows when users are informed vs. default-permitted; measurement of compliance cost allocation across company sizes',
    'If consent is functional: snare classification is too harsh — data subjects have meaningful control mechanism and moderate classification becomes appropriate. If consent is performative: extraction mechanism is disguised, and the constraint remains snare from data subject perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consent_mechanism_sufficiency, empirical, 'Whether informed consent mechanisms provide genuine subject agency').

omega_variable(
    regulatory_enforcement_capacity_vs_declared_mandate,
    'Do EU data protection authorities have sufficient resources to enforce GDPR at scale, or does the gap between legal mandate and enforcement capacity create a structural illusion of protection?',
    'Historical analysis of fines issued vs. violations detected; audit of DPA staffing levels vs. complaint volume; correlation between enforcement action and actual behavior change',
    'If capacity matches mandate: rope classification for regulators is correct; enforcement scales. If capacity is insufficient: rope becomes piton — the regulatory structure is performative theater maintained by political commitment rather than functional capacity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_enforcement_capacity_vs_declared_mandate, empirical, 'Whether DPA resources match enforcement mandate scope').

omega_variable(
    extraction_mechanism_platform_scale_dependency,
    'Is the asymmetric fine structure (4% global revenue vs. fixed penalty) designed to enforce equal treatment of large and small platforms, or does it constitute extractive targeting of incumbents that creates competitive advantage for mid-market entrants?',
    'Comparative analysis of compliance cost per unit revenue across company sizes; measurement of market entry rates before/after GDPR; identification of market consolidation patterns',
    'If fine structure extracts from incumbents: tangled rope from platform perspective is correct; intentional redistribution. If fine structure punishes all equally but hurts small firms more: extraction mechanism is unintentional; innovation entrant classification shifts toward higher extraction (snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_mechanism_platform_scale_dependency, empirical, 'Whether fine structure creates intentional or unintentional asymmetric extraction').

omega_variable(
    legal_interpretation_stability_and_contestation,
    'Does GDPR legal text permit multiple coherent interpretations that generate structurally different compliance regimes (a contested kernel), or is there an emergent consensus interpretation that all parties are converging toward?',
    'Audit of regulatory decisions across EU member states; identification of CJEU precedents that resolve ambiguity vs. those that open new contested space; analysis of platform policy divergence',
    'If multiple interpretations are stable: different member states + different platforms instantiate genuinely different constraints with different ε values — decompose into separate stories. If consensus emerges: single constraint with observer-relative perspectives. If interpretation is actively contested but converging: tangled rope with measurement-based sunset as consensus builds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legal_interpretation_stability_and_contestation, conceptual, 'Whether GDPR text constitutes a contested kernel or is converging on singular interpretation').

omega_variable(
    innovation_entrant_data_practices_efficiency_dependency,
    'Can innovation-stage technology companies operate viable business models within EU data constraints that incumbents avoid through scale and lobbying, or are privacy-respecting architectures inherently less competitive due to data-dependent optimization?',
    'Longitudinal study of EU-founded entrants'' data practices vs. US/China peers; analysis of business model viability metrics (profitability trajectory, investor funding, market share growth) correlated with compliance strictness',
    'If viable alternatives exist: tangled rope from entrant perspective; genuine coordination opportunity. If data-intensive practices are inherently more efficient: snare for entrants; GDPR creates permanent structural disadvantage.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(innovation_entrant_data_practices_efficiency_dependency, empirical, 'Whether innovation-stage companies can operate viable privacy-respecting models').

omega_variable(
    false_summit_natural_law_vs_constructed,
    'Is GDPR enforcement an immutable structural feature of EU digital governance (natural law perspective), or is it a contingent institutional arrangement that benefits identifiable actors and persists through political will rather than necessity?',
    'Historical analysis of alternative EU policy regimes; comparative analysis of privacy protection levels pre/post-GDPR; identification of counterfactual institutional designs that would preserve data rights while reducing extraction',
    'If immutable: mountain classification stands. If contingent: false summit detector fires; constraint reclassifies to tangled rope or snare depending on beneficiary/victim structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_law_vs_constructed, conceptual, 'Whether GDPR enforcement is natural law or naturalized institutional arrangement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gdpr_enforcement_structure, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gdpr_enf_tr_t0, gdpr_enforcement_structure, theater_ratio, 0, 0.35).
narrative_ontology:measurement(gdpr_enf_tr_t3, gdpr_enforcement_structure, theater_ratio, 3, 0.45).
narrative_ontology:measurement(gdpr_enf_tr_t6, gdpr_enforcement_structure, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(gdpr_enf_be_t0, gdpr_enforcement_structure, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(gdpr_enf_be_t3, gdpr_enforcement_structure, base_extractiveness, 3, 0.38).
narrative_ontology:measurement(gdpr_enf_be_t6, gdpr_enforcement_structure, base_extractiveness, 6, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(gdpr_enf_su_t0, gdpr_enforcement_structure, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(gdpr_enf_su_t3, gdpr_enforcement_structure, suppression_requirement, 3, 0.58).
narrative_ontology:measurement(gdpr_enf_su_t6, gdpr_enforcement_structure, suppression_requirement, 6, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gdpr_enforcement_structure, enforcement_mechanism).
narrative_ontology:affects_constraint(gdpr_enforcement_structure, cookie_consent_interface_theater).
narrative_ontology:affects_constraint(gdpr_enforcement_structure, regulatory_arbitrage_tax_tech_platforms).
narrative_ontology:affects_constraint(gdpr_enforcement_structure, eu_digital_market_gatekeeping).

% DUAL FORMULATION NOTE:
% GDPR enforcement structure is upstream of three downstream constraints: (1) cookie consent interface theater — performative compliance at user-interaction layer; (2) regulatory arbitrage between EU enforcement and global platform scale — affects how fines are absorbed; (3) EU digital market gatekeeping — compliance burden creates barrier to entry that upstream constraint enables. These three stories share the same GDPR kernel but model distinct structural phenomena with different epsilon values. Story 1 (cookie theater) focuses on user-interaction extractiveness; Story 2 (regulatory arbitrage) focuses on fine mechanism design; Story 3 (gatekeeping) focuses on market structure effects. Link them via network.affects_constraints to show how GDPR enforcement propagates asymmetrically across the ecosystem.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gdpr_enforcement_structure, moderate, 0.72).
constraint_indexing:directionality_override(gdpr_enforcement_structure, institutional, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
