% ============================================================================
% CONSTRAINT STORY: civil_rights_litigation_barrier
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_civil_rights_litigation_barrier, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: civil_rights_litigation_barrier
 *   human_readable: Civil Rights Litigation Barrier
 *   domain: legal/social_justice
 *
 * SUMMARY:
 *   The civil rights litigation barrier creates a structural tension between
 *   the right to legal redress and the material capacity to pursue it. This
 *   constraint exhibits the full range of DR types from different structural
 *   positions. A claimant bearing the full cost of litigation to vindicate a
 *   constitutional right experiences pure extraction (Snare). A civil rights
 *   organization that must strategically triage limited resources experiences
 *   mixed coordination and extraction (Tangled Rope). The legal establishment
 *   that profits from fee structures experiences coordination mechanisms that
 *   stabilize their revenue (Rope). Institutional defendants experience
 *   gatekeeping that protects them disproportionately while screening
 *   frivolous claims (Tangled Rope from their position). Organized reform
 *   coalitions see emerging exit pathways through legal aid, pro bono, and
 *   alternative dispute resolution (Scaffold with sunset). An analytical
 *   observer might naturalize cost barriers as inherent to adversarial
 *   justice (Mountain), but the structural data reveals this as false summit
 *   — the barriers are contingent institutional arrangements. The constraint
 *   shows modest theater (0.45) because litigation rules, while imperfect,
 *   provide genuine due process protection. The extractiveness trajectory
 *   (0.42→0.58 over 30 years) reflects accumulation of cost barriers: rising
 *   attorney fees, increasing case complexity, legal aid funding stagnation,
 *   and normalization of private funding dependence.
 *
 * KEY AGENTS:
 *   - Civil Rights Claimants: Primary victim (powerless/trapped) — face $50k-$500k+ litigation costs over 3-7 year timescale with no alternative dispute resolution path
 *   - Marginalized Communities: Diffuse victim (powerless/trapped) — systemic underenforcement of civil rights when claims are gatekept by cost
 *   - Civil Rights Organizations: Secondary victim/partial beneficiary (moderate/constrained) — capacity-constrained, must triage cases; also benefit from precedent and donor attention
 *   - Legal Establishment: Primary beneficiary (institutional/arbitrage) — profits from fee structures; controls litigation rules; can select profitable cases
 *   - Institutional Defendants: Secondary beneficiary (institutional/constrained) — protected by gatekeeping; also benefit from due process rules; constrained by liability exposure
 *   - Reform Coalition: Organized counterpower (organized/mobile) — legal aid advocates, law school clinics, contingency bar building alternative pathways
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks naturalizing cost barriers as inherent to justice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(civil_rights_litigation_barrier, 0.58).
domain_priors:suppression_score(civil_rights_litigation_barrier, 0.68).
domain_priors:theater_ratio(civil_rights_litigation_barrier, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(civil_rights_litigation_barrier, extractiveness, 0.58).
narrative_ontology:constraint_metric(civil_rights_litigation_barrier, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(civil_rights_litigation_barrier, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(civil_rights_litigation_barrier, tangled_rope).
narrative_ontology:human_readable(civil_rights_litigation_barrier, "Civil Rights Litigation Barrier").
narrative_ontology:topic_domain(civil_rights_litigation_barrier, "legal/social_justice").

domain_priors:requires_active_enforcement(civil_rights_litigation_barrier).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(civil_rights_litigation_barrier, institutional_defendants).
narrative_ontology:constraint_beneficiary(civil_rights_litigation_barrier, legal_establishment).
narrative_ontology:constraint_victim(civil_rights_litigation_barrier, civil_rights_claimants).
narrative_ontology:constraint_victim(civil_rights_litigation_barrier, marginalized_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CIVIL RIGHTS CLAIMANT (SNARE) — Trapped by cost barriers (legal fees $50k-$500k+), time barriers (litigation 3-7 years), and knowledge barriers (legal expertise required). No meaningful exit option from justice system. Bears full extraction cost. Cannot organize collectively without legal resources. Maximum experienced constraint as pure extraction mechanism.
constraint_indexing:constraint_classification(civil_rights_litigation_barrier, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CIVIL RIGHTS ORGANIZATION (TANGLED ROPE) — Constrained by funding limitations and capacity rationing. Must litigate strategically, selecting high-impact cases. Also benefits from litigation precedent ecosystem and donor attention to high-profile cases. Genuine coordination function (precedent-setting benefits all future claimants) alongside asymmetric extraction (organization bears litigation costs that individual claimants cannot). Medium experienced extraction.
constraint_indexing:constraint_classification(civil_rights_litigation_barrier, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LEGAL ESTABLISHMENT (ROPE) — Benefits from fee structures, billable hours, and institutional prestige. Experiences constraint as coordination mechanism: litigation rules standardize dispute resolution and create predictable fee structures. Net beneficiary — extraction flows toward this institution, not away. Low experienced extraction because arbitrage options abundant (can refuse cases, select profitable ones, adjust billing rates).
constraint_indexing:constraint_classification(civil_rights_litigation_barrier, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INSTITUTIONAL DEFENDANT (TANGLED ROPE) — Constrained by liability exposure and reputational risk, but also benefits from litigation gatekeeping (high barriers screen out frivolous claims and protect due process). Experiences genuine coordination function (litigation rules prevent arbitrary accusations) alongside asymmetric benefit (barriers disproportionately protect defendants vs plaintiffs). Moderate experienced extraction in their favor.
constraint_indexing:constraint_classification(civil_rights_litigation_barrier, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: REFORM COALITION (SCAFFOLD) — Organized groups (civil rights coalitions, law school clinics, contingency bar) see litigation barriers as a temporary coordination failure with emerging solutions: legal aid expansion, class action mechanics, contingency fee normalization, and pro bono requirements. These alternatives are building exit pathways. Sunset horizon: 15-25 years as legal aid infrastructure matures and alternative dispute resolution mechanisms scale.
constraint_indexing:constraint_classification(civil_rights_litigation_barrier, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational view, some litigation cost is inherent to adversarial justice: complex claims require expertise, time, and resources to adjudicate fairly. The cost gap between plaintiffs and defendants is seen as a structural feature of due process itself. However, the structural data contradicts this mountain classification — the engine will identify false summit, revealing that cost barriers are contingent institutional arrangements, not laws of nature.
constraint_indexing:constraint_classification(civil_rights_litigation_barrier, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(civil_rights_litigation_barrier_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(civil_rights_litigation_barrier, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(civil_rights_litigation_barrier, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(civil_rights_litigation_barrier, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(civil_rights_litigation_barrier_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Civil rights claimants face documented cost barriers ($50k-$500k litigation expenses, 3-7 year timescale) that systematically exclude low-income plaintiffs. The barrier is not purely extractive — litigation rules do provide genuine due process protection and prevent frivolous claims. But the barrier's distribution is asymmetric: wealthy defendants can absorb costs; individual claimants cannot. The trajectory shows accumulation (0.42→0.58) as attorney fees rise faster than inflation and case complexity increases. Suppression (0.68): High. Multiple reinforcing barriers: (1) Cost barrier (legal fees, discovery expenses), (2) Knowledge barrier (requires expert navigation of complex rules), (3) Time barrier (litigation takes 3-7 years; claimants cannot afford to wait), (4) Institutional bias (judges and court systems often reflect defendant institutional interests), (5) Publication bias (precedent-setting cases get litigated; routine cases settle cheaply or go unpursued). Theater ratio (0.45): Moderate. Litigation procedures provide genuine due process (adversarial testing, discovery rules, written records). But theater elements exist: (a) Discovery disputes become performative cost-escalation games, (b) Summary judgment motions function partly as gatekeeping theater, (c) Judicial opinions are sometimes performative legitimation of predetermined institutional interests. Theater is lower than in peer review (verification_bottleneck = 0.72) because litigation has genuine functional elements; theater is higher than in pure coordination because gatekeeping serves extractive purposes.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is stark. Civil rights claimants experience pure extraction (Snare) — the system blocks redress access. Civil rights organizations experience mixed coordination/extraction (Tangled Rope) — they solve coordination problems (precedent, norm-setting) while extracting through case selection. Legal establishment experiences coordination (Rope) — litigation rules create predictable, profitable structures. Institutional defendants experience asymmetric protection (Tangled Rope from their position) — due process rules protect them while barriers protect them further. Reform coalitions see temporary barriers with exit pathways (Scaffold) — legal aid, pro bono, contingency fees, and alternative dispute resolution are scaling. The analytical observer's natural law (Mountain) is revealed as false summit: cost barriers are contingent institutional choices (funding legal aid, simplifying procedures, normalizing contingency fees) not inherent features of justice.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by each agent's structural relationship to the barrier. Civil rights claimants are trapped victims — they bear maximum extraction cost with no exit options (d ≈ 0.95 → f(d) ≈ 1.42). Civil rights organizations are constrained but partially benefited — they navigate cost barriers strategically and benefit from litigation precedent (d ≈ 0.55 → f(d) ≈ 0.75). Legal establishment are beneficiaries with arbitrage — they control fee structures and can select profitable cases (d ≈ 0.10 → f(d) ≈ -0.01). Institutional defendants are beneficiaries but constrained by liability — they benefit from gatekeeping but face exposure (d ≈ 0.30 → f(d) ≈ 0.15). Scope modifier (national σ = 1.0) applies to all perspectives equally. The beneficiary/victim declarations drive the derivation chain: beneficiaries (legal establishment, institutional defendants) get low d → low or negative χ; victims (civil rights claimants, marginalized communities) get high d → high χ. This produces the perspectival gap: the beneficiary sees coordination (Rope), the victim sees extraction (Snare), the organized counterpower sees sunset (Scaffold).
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVES MANDATROPHY: The constraint distinguishes genuine coordination (due process protection, frivolous claim screening) from pure extraction (cost gatekeeping). The mandatory classification prevents false labeling of pure extraction as coordination. Civil rights litigation barriers have a genuine coordination function — adversarial testing does improve claim quality, written records do create accountability. But this coordination function does NOT require the current cost barriers. The legal aid, pro bono, and contingency fee alternatives provide the same coordination function with lower extraction. The mandatrophy is resolved by recognizing that extractiveness (0.58) exceeds what coordination function requires (estimated 0.15-0.20). The delta (≈0.38) is pure extractive rent-seeking. The tangled_rope classification certifies that both components are real: (1) genuine coordination component, (2) genuine extraction component. This prevents mislabeling as pure extraction (Snare) OR pure coordination (Rope).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    litigation_cost_threshold,
    'What litigation cost level distinguishes fair compensation for lawyer expertise from extractive gatekeeping?',
    'Comparative analysis of legal cost structures across jurisdictions; international comparison of civil rights litigation barriers; correlation between legal cost and civil rights outcome prevalence',
    'If threshold < $20k: most complex claims gatekept as unaffordable. If threshold > $100k: only wealthy plaintiffs pursue valid claims, creating systematic underenforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(litigation_cost_threshold, empirical, 'Litigation cost threshold for distinguishing expertise compensation from gatekeeping').

omega_variable(
    contingency_fee_coverage,
    'Do contingency fee arrangements adequately substitute for plaintiff cost barriers, or do they create lawyer moral hazard (selecting only high-monetary-damage cases)?',
    'Analysis of case selection patterns under contingency vs hourly billing; comparison of civil rights outcomes (injunctive relief, precedent value) vs monetary damages; plaintiff satisfaction surveys',
    'If contingency is adequate: many claims can proceed without cost gatekeeping (constraint moves toward Rope). If moral hazard is severe: civil rights claims without monetary damages remain trapped (constraint remains Snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(contingency_fee_coverage, empirical, 'Whether contingency fees resolve plaintiff cost barriers or create lawyer selection bias').

omega_variable(
    legal_aid_capacity_sufficiency,
    'Can legal aid systems scale to meet demand, or are funding constraints structural (perpetual underresourcing relative to need)?',
    'Time-series analysis of legal aid funding vs civil rights claim volume; international benchmarking of legal aid spending as percentage of GDP; wait time data for legal aid intake',
    'If scale is possible: scaffold sunset clause is genuine (constraint will degrade toward Rope/Piton). If structural underresourcing: legal aid is perpetual performance theater (constraint remains Snare/Tangled Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legal_aid_capacity_sufficiency, empirical, 'Whether legal aid capacity can meet demand or is structurally underfunded').

omega_variable(
    knowledge_barrier_substitutability,
    'Can simplified legal procedures or AI-assisted document preparation substitute for expert attorney knowledge, or are complex civil rights claims inherently knowledge-intensive?',
    'Analysis of self-represented litigant outcomes in civil rights cases; comparison of success rates with vs without counsel; feasibility studies of simplified procedures for specific claim types',
    'If substitution is possible: knowledge barrier can be reduced independent of cost barrier (constraint components decompose). If inherent complexity: knowledge and cost barriers are inseparable (constraint remains tangled).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(knowledge_barrier_substitutability, empirical, 'Whether knowledge barriers can be decoupled from cost barriers through procedural simplification').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(civil_rights_litigation_barrier, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(civrights_tr_t0, civil_rights_litigation_barrier, theater_ratio, 0, 0.38).
narrative_ontology:measurement(civrights_tr_t15, civil_rights_litigation_barrier, theater_ratio, 15, 0.42).
narrative_ontology:measurement(civrights_tr_t30, civil_rights_litigation_barrier, theater_ratio, 30, 0.45).

% Extraction over time
narrative_ontology:measurement(civrights_be_t0, civil_rights_litigation_barrier, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(civrights_be_t15, civil_rights_litigation_barrier, base_extractiveness, 15, 0.5).
narrative_ontology:measurement(civrights_be_t30, civil_rights_litigation_barrier, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(civil_rights_litigation_barrier, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(civil_rights_litigation_barrier, 0.12).
narrative_ontology:affects_constraint(civil_rights_litigation_barrier, criminal_legal_aid_shortage).
narrative_ontology:affects_constraint(civil_rights_litigation_barrier, employment_discrimination_underenforcement).
narrative_ontology:affects_constraint(civil_rights_litigation_barrier, housing_discrimination_remedy_gap).

% DUAL FORMULATION NOTE:
% Civil rights litigation barrier is upstream of specific domain-based constraints (employment discrimination, housing discrimination, criminal justice). Each domain has claim-specific extractiveness values; the litigation barrier represents the structural mechanism that prevents enforcement across all domains.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(civil_rights_litigation_barrier, institutional, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
