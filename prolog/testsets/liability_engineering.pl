% ============================================================================
% CONSTRAINT STORY: liability_engineering
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_liability_engineering, []).

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
 *   constraint_id: liability_engineering
 *   human_readable: Liability Engineering: Structural Extraction via Legal Architecture
 *   domain: legal_economics/corporate_governance
 *
 * SUMMARY:
 *   Liability engineering is the structural practice of architecting
 *   corporate and legal forms to minimize victim recovery while maintaining
 *   the appearance of a functioning legal system. The constraint operates
 *   through nested architectural choices: corporate subsidiaries isolate
 *   parent assets from operational liability; limited liability rules cap
 *   exposure; insurance structures cap recovery further; bankruptcy law
 *   provides exit mechanisms for accumulated liabilities; regulatory gaps
 *   create arbitrage opportunities for sophisticated actors. The constraint
 *   exhibits a classic Tangled Rope structure: a genuine coordination
 *   function (predictable liability rules enable capital formation and
 *   efficient risk allocation) is inseparable from an extraction function
 *   (these same rules systematically limit victim recovery). The tension
 *   arises from competing principles: limited liability is necessary for
 *   capital markets to function at scale (nobody would invest in firms with
 *   unlimited personal liability); but unlimited liability-capping protects
 *   corporate wealth while leaving victims uncompensated. The analytical
 *   question is whether this trade-off is fundamental (inseparable) or
 *   contingent (remediable through alternative architecture). The rising
 *   extractiveness and theater measurements suggest that liability
 *   engineering has become more sophisticated over time — the legal
 *   architecture that was once incidental to corporate organization has
 *   become increasingly intentional, with dedicated strategies (shell
 *   companies, insurance arbitrage, litigation financing structures) designed
 *   specifically to minimize victim recovery.
 *
 * KEY AGENTS:
 *   - Injured Parties / Environmental Victims: Primary victims (powerless/trapped) — have no exit from harm state; face deliberately fragmented liability structures designed to minimize recovery
 *   - Plaintiff Attorneys / Regulators: Moderate power (moderate/constrained) — operate within legal system designed to obstruct their recovery efforts; have procedural rights but limited enforcement capacity
 *   - Corporate Parent Entities: Primary beneficiaries (institutional/arbitrage) — primary beneficiaries of liability isolation; experience the constraint as pure coordination and capital protection
 *   - Subsidiary / Shell Companies: Functional instruments (institutional/arbitrage) — structured to isolate liability; often lack significant assets or operations of their own
 *   - Insurance Industry: Secondary beneficiary (institutional/arbitrage) — monetizes liability allocation through insurance products; caps and fragments recovery mechanisms
 *   - Regulatory State: Organized constraint enforcer (organized/constrained) — theoretically responsible for oversight but often outmatched by corporate legal sophistication; experiences genuine coordination needs (capital predictability) alongside inability to enforce against sophisticated avoidance
 *   - Legal Services Industry: Secondary beneficiary (institutional/arbitrage) — grows with complexity; liability engineering increases demand for legal services
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees constraint as structurally hybrid and difficult to decompose
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(liability_engineering, 0.58).
domain_priors:suppression_score(liability_engineering, 0.68).
domain_priors:theater_ratio(liability_engineering, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(liability_engineering, extractiveness, 0.58).
narrative_ontology:constraint_metric(liability_engineering, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(liability_engineering, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(liability_engineering, tangled_rope).
narrative_ontology:human_readable(liability_engineering, "Liability Engineering: Structural Extraction via Legal Architecture").
narrative_ontology:topic_domain(liability_engineering, "legal_economics/corporate_governance").

domain_priors:requires_active_enforcement(liability_engineering).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(liability_engineering, corporate_parent_entities).
narrative_ontology:constraint_beneficiary(liability_engineering, institutional_investors).
narrative_ontology:constraint_beneficiary(liability_engineering, legal_services_industry).
narrative_ontology:constraint_victim(liability_engineering, injured_parties).
narrative_ontology:constraint_victim(liability_engineering, affected_workers).
narrative_ontology:constraint_victim(liability_engineering, environmental_remediation_burden).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INJURED PARTY (SNARE) — Cannot exit the constraint. Once harmed, victim faces fragmented legal liability structure designed to minimize payout: subsidiary bankruptcy shields parent; shell company structures distribute liability; insurance cap-out leaves residual harm uncompensated. High experienced extraction, no alternatives, maximum suppression.
constraint_indexing:constraint_classification(liability_engineering, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: PLAINTIFF ATTORNEY / REGULATOR (TANGLED ROPE) — Moderate power, constrained exit. Must navigate the liability structure (coordination function: legal process exists and is accessible) but faces deliberate architectural obstruction (extraction: fragmented entities make recovery harder). Experiences genuine coordination benefit (procedural rights) alongside asymmetric extraction (costs of pursuing liability across multiple corporate shells, insurance structures, and jurisdictional boundaries).
constraint_indexing:constraint_classification(liability_engineering, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CORPORATE PARENT ENTITY (ROPE) — Primary beneficiary (institutional/arbitrage). Experiences the constraint as pure coordination: liability structures enable efficient risk allocation, subsidiary isolation protects consolidated balance sheets, and insurance mechanisms provide predictability. Net beneficiary — coordination function is genuine (capital allocation works), extraction runs toward this agent.
constraint_indexing:constraint_classification(liability_engineering, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY STATE (TANGLED ROPE) — Organized power (can set standards, inspect, enforce), constrained exit (cannot unilaterally dissolve corporate forms). Sees the constraint as hybrid: genuine coordination function (bankruptcy and liability rules provide predictability for capital markets) alongside systematic extraction (regulatory capture, enforcement resource limitations, corporate lobbying power exceeds enforcement capacity). Active enforcement is required to maintain the constraint; coordination and extraction are inseparable.
constraint_indexing:constraint_classification(liability_engineering, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LEGAL FORMALISM THEATER (PITON) — Theater ratio high (0.55): the formal legal apparatus (corporate veil, limited liability, bankruptcy law) performs the function of 'fair allocation' and 'incentive alignment' but is substantially captured by beneficiaries. The ritual of litigation, discovery, settlement (theater) masks the structural outcome (liability engineering prevents full recovery). Maintained through institutional inertia — the corporate form persists because institutional actors depend on its predictability, not because the fairness justifications still hold.
constraint_indexing:constraint_classification(liability_engineering, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — Civilizational view. Liability engineering is a hybrid constraint: the coordination function (legal predictability, capital allocation, risk distribution) is genuine and valuable; the extraction function (victim liability capping, parent protection, regulatory arbitrage) is also structurally embedded and not separable. Both functions arise from the same architectural choices (subsidiary isolation, insurance limits, liability walls). The constraint cannot be 'fixed' by removing the extraction without dismantling the coordination — these are two sides of the same legal architecture.
constraint_indexing:constraint_classification(liability_engineering, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(liability_engineering_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(liability_engineering, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(liability_engineering, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(liability_engineering, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(liability_engineering, TR),
    TR >= 0.70.

:- end_tests(liability_engineering_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint systematically reduces victim recovery while maintaining corporate asset protection. However, extraction is not total — victims do recover partial damages through litigation, settlements are sometimes substantial, and regulatory pressure occasionally forces improvements. The measurement arc shows extractiveness increasing from 0.35 to 0.58, reflecting the sophistication of liability engineering strategies over 30 years. Suppression (0.68): High. Multiple barriers prevent victim exit: (1) legal obstacles (limited liability doctrine, subsidiary shielding, insurance caps), (2) resource barriers (litigation costs, legal complexity), (3) informational asymmetry (victims don't know about corporate structure until after harm), (4) temporal barriers (recovery timelines extend decades; victims die before settlement). Theater ratio (0.55): Moderate. The legal system performs fairness theater (discovery, litigation, settlement negotiations) while engineering predictable outcomes favoring beneficiaries. The theater is not total — genuine randomness exists in litigation outcomes — but the structure tilts systematically toward liability minimization. Claimed type (Tangled Rope): Justified by presence of both coordination function (capital predictability) and asymmetric extraction (liability capping), with active enforcement required to maintain structure.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap is the tension between beneficiary and victim experience of the same constraint. Corporate parents (beneficiary/arbitrage) experience the constraint as enabling coordination (predictable risk allocation) with negative experienced extraction (they benefit). Injured parties (victim/trapped) experience the same constraint as pure extraction (liability capping, recovery obstruction) with maximum experienced extraction (they are harmed). The gap reveals that the constraint's coordination function is real but unevenly distributed — it creates predictability for capital markets while creating unpredictability and suppression for victims. The analytical observer bridges this gap by recognizing that both perspectives are structurally accurate but attached to different parts of the system. The constraint genuinely coordinates capital allocation AND systematically extracts from victims — these are not contradictory; they are inseparable aspects of the same architecture.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) ranges from 0.05 (corporate parent as full beneficiary) to 0.95 (injured party as full target). The corporate parent experiences low d because they benefit from liability isolation and have arbitrage options (exit the jurisdiction, restructure corporate forms). The injured party experiences high d because they bear full harm costs and have no exit options — trapping d at maximum values. The plaintiff attorney and regulatory state experience intermediate d values reflecting constrained exit (they can engage in litigation and enforcement but face resource limitations and structural obstacles). The legal industry experiences low d as secondary beneficiary — profits from complexity. The directionality pipeline derives d from power atom + exit options + beneficiary/victim status; the sigmoid f(d) then amplifies experienced extractiveness for high-d agents (injured parties, victims) while dampening it for low-d agents (corporate parents, beneficiaries).
 *
 * MANDATROPHY ANALYSIS:
 *   LIABILITY ENGINEERING RESOLVES MANDATROPHY BY SHOWING THE CONSTRAINT IS CONSTITUTIVELY HYBRID: The mandatrophy arises from the question 'Is this coordination or extraction?' The answer is 'both, inseparably.' Removing the extraction mechanisms (subsidiary liability shielding, insurance caps, recovery capping) would destroy the coordination function (capital formation and market predictability depend on limited liability doctrine). Conversely, removing coordination would require replacing the entire corporate legal architecture. The constraint is Tangled Rope not because it has two separable functions (one good, one bad) but because these functions arise from the same architectural choices and cannot be surgically separated. This resolves the mandatrophy: the classification is not 'true type revealed' but rather 'hybrid constraint where both functions are structurally necessary to each other.' The analytical observer's role is to name this hybrid nature clearly — neither pretending it is pure coordination (beneficiary perspective) nor pure extraction (victim perspective), but acknowledging that the legal architecture simultaneously enables and obstructs.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_extraction_separability,
    'Can liability law be restructured to preserve coordination benefits (capital allocation, predictability) while eliminating extraction mechanisms (victim liability caps, subsidiary shielding)?',
    'Comparative legal analysis of alternative liability architectures (unlimited parent liability, full piercing, strict proportional recovery). Test whether these alternatives maintain market function and capital formation.',
    'If separable: constraint reclassifies from Tangled Rope to pure Rope/Scaffold at analytical and regulatory levels. If inseparable: current structure is Pareto-optimal on coordination dimensions and cannot be improved without trade-off.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_separability, conceptual, 'Whether liability coordination and extraction functions are structurally separable').

omega_variable(
    regulatory_capture_extent,
    'What proportion of liability engineering''s protective effect derives from genuine legal principle vs. capture-driven enforcement failure?',
    'Jurisdictional variance analysis: compare liability recovery rates and corporate asset protection across high-enforcement (Nordic, German, Canadian) vs. capture-prone (US, UK, offshore) jurisdictions. Controlled for tort law design.',
    'If primarily capture: intervention on enforcement can reduce extraction without changing legal architecture. If primarily principle: only legal reform can address extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_capture_extent, empirical, 'Extent of regulatory capture in liability engineering enforcement').

omega_variable(
    alternative_risk_allocation,
    'What alternative mechanisms (mandatory insurance, escrow bonds, parent guarantees, strict proportional liability) could preserve predictability for capital markets while improving victim recovery?',
    'Legal and economic modeling of alternative architectures; pilot testing in specific liability-prone industries (pharmaceuticals, chemical manufacturing, oil & gas).',
    'If viable alternatives exist: current structure is contingent, not necessary. If no viable alternatives: current structure represents a binding constraint on victim recovery.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_risk_allocation, conceptual, 'Viability of alternative risk allocation mechanisms').

omega_variable(
    victim_coalition_power,
    'Can aggregated victim organizations overcome the powerless/trapped classification through collective action and legislative power?',
    'Historical analysis of tort reform movements, mass litigation campaigns, regulatory shifts following victim coalitions (asbestos, tobacco, opioids). Measure success rate and institutional barriers to coalition formation.',
    'If yes: powerless classification may upgrade to organized at generational timescale. If no: structural barriers to victim coordination are themselves part of the extraction mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_coalition_power, empirical, 'Whether victim coalitions can overcome powerless/trapped classification').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(liability_engineering, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(liab_tr_t0, liability_engineering, theater_ratio, 0, 0.4).
narrative_ontology:measurement(liab_tr_t15, liability_engineering, theater_ratio, 15, 0.48).
narrative_ontology:measurement(liab_tr_t30, liability_engineering, theater_ratio, 30, 0.55).

% Extraction over time
narrative_ontology:measurement(liab_be_t0, liability_engineering, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(liab_be_t15, liability_engineering, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(liab_be_t30, liability_engineering, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(liability_engineering, enforcement_mechanism).
narrative_ontology:affects_constraint(liability_engineering, tort_law_reform).
narrative_ontology:affects_constraint(liability_engineering, corporate_limited_liability).
narrative_ontology:affects_constraint(liability_engineering, insurance_regulation).
narrative_ontology:affects_constraint(liability_engineering, bankruptcy_law).

% DUAL FORMULATION NOTE:
% Liability engineering comprises multiple linked constraints: the corporate limited liability doctrine (base coordination), subsidiary isolation practices (tactical extraction), insurance cap structures (secondary extraction), and bankruptcy law (tertiary extraction). This story treats liability engineering holistically; decomposition into separate stories for each mechanism would be appropriate for higher-resolution analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(liability_engineering, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
