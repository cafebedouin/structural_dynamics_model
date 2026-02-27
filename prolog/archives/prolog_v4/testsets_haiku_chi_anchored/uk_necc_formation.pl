% ============================================================================
% CONSTRAINT STORY: uk_necc_formation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_uk_necc_formation, []).

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
 *   constraint_id: uk_necc_formation
 *   human_readable: UK National Economic Crime Centre (NECC) Formation
 *   domain: political/economic
 *
 * SUMMARY:
 *   The UK National Economic Crime Centre (NECC) is a newly established law
 *   enforcement agency tasked with combating fraud, money laundering, and
 *   kleptocracy. Modeled on the American FBI, it represents a policy choice
 *   to concentrate economic crime enforcement capacity in a single national
 *   institution. The constraint structure involves genuine coordination
 *   benefits — suppressing organized crime improves financial system
 *   legitimacy and protects government revenue — but also real extraction
 *   mechanisms: the financial sector bears disproportionate compliance costs,
 *   surveillance expands beyond criminal targeting to include legitimate
 *   actors, and institutional theater inflates the claimed capacity relative
 *   to actual new functional capability. The NECC exhibits all core DR types
 *   from different perspectives: organized crime operators see pure
 *   suppression (Snare); the financial sector sees mixed coordination and
 *   extraction (Tangled Rope); government revenue authorities see net
 *   coordination benefit (Rope); international standards coalitions see a
 *   temporary measure with a sunset (Scaffold); the institutional law
 *   enforcement system sees a performative rebranding (Piton); and the
 *   civilizational analytical observer risks naturalizing a contingent policy
 *   choice as an immutable requirement (false Mountain).
 *
 * KEY AGENTS:
 *   - Government Revenue Authority: Primary beneficiary (institutional/arbitrage) — captures coordination benefit through improved tax compliance and legitimacy
 *   - Organized Crime Operators: Primary victims (powerless/trapped) — face direct suppression and operational constraint via enhanced enforcement
 *   - Financial Privacy Sector: Secondary victims (moderate/constrained) — bear compliance burden and expanded surveillance while also benefiting from crime suppression
 *   - International Financial Integrity Coalition: Organized actors (organized/constrained) — FATF, UN, UK-EU data-sharing protocols; see NECC as temporary until standards mature
 *   - UK Law Enforcement Institutional System: Institutional actor (institutional/arbitrage) — maintains theater through rebranding and resource reallocation; benefits from institutional prestige
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing a political choice as inherent necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(uk_necc_formation, 0.52).
domain_priors:suppression_score(uk_necc_formation, 0.65).
domain_priors:theater_ratio(uk_necc_formation, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(uk_necc_formation, extractiveness, 0.52).
narrative_ontology:constraint_metric(uk_necc_formation, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(uk_necc_formation, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(uk_necc_formation, tangled_rope).
narrative_ontology:human_readable(uk_necc_formation, "UK National Economic Crime Centre (NECC) Formation").
narrative_ontology:topic_domain(uk_necc_formation, "political/economic").

domain_priors:requires_active_enforcement(uk_necc_formation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(uk_necc_formation, government_revenue_authority).
narrative_ontology:constraint_beneficiary(uk_necc_formation, financial_system_legitimacy).
narrative_ontology:constraint_beneficiary(uk_necc_formation, law_enforcement_institutional_authority).
narrative_ontology:constraint_victim(uk_necc_formation, financial_privacy_sector).
narrative_ontology:constraint_victim(uk_necc_formation, organized_crime_operational_capacity).
narrative_ontology:constraint_victim(uk_necc_formation, regulatory_arbitrage_actors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ORGANIZED CRIME OPERATOR (SNARE) — Faces direct suppression via enhanced detection and enforcement capacity. No legitimate exit from NECC jurisdiction; trapped by territorial scope and legal mandate. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.72. Pure extraction mechanism: operational capability restricted, surveillance increased, prosecution risk amplified.
constraint_indexing:constraint_classification(uk_necc_formation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FINANCIAL PRIVACY SECTOR (TANGLED ROPE) — Experiences both coordination benefit (NECC targets bad actors, improving legitimate sector reputation) and extraction (compliance burden, reporting obligations, operational scrutiny). d≈0.68, f(d)≈1.05, σ=1.0 → χ≈0.55. Constrained exit: regulatory requirements force participation; coordination function is real but asymmetric enforcement falls disproportionately on non-state actors.
constraint_indexing:constraint_classification(uk_necc_formation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: GOVERNMENT REVENUE AUTHORITY (ROPE) — Primary beneficiary. NECC improves tax compliance, reduces kleptocratic asset laundering (which diverts tax revenue), and strengthens institutional legitimacy through visible crime-fighting. d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.05. Net beneficiary with low extraction cost. Can arbitrage NECC resources for revenue recovery and use institutional coordination capacity.
constraint_indexing:constraint_classification(uk_necc_formation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INTERNATIONAL FINANCIAL INTEGRITY COALITION (SCAFFOLD) — NECC is a temporary capacity-building measure (modeled on FBI) with explicit sunset logic: cooperative international standards (FATF recommendations, UN conventions, UK-EU post-Brexit alignment) are intended to make centralized national enforcement less necessary as global frameworks mature. d≈0.35, f(d)≈0.35, σ=1.2 → χ≈0.24. Sunset rationale: as international AML/CFT standards harden and cross-border intelligence-sharing protocols become automatic, unilateral UK enforcement becomes redundant. Estimated sunset: 10-15 years as FATF-mutual evaluation and cross-border data directives mature.
constraint_indexing:constraint_classification(uk_necc_formation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: UK LAW ENFORCEMENT INSTITUTIONAL SYSTEM (PITON) — NECC's founding involves substantial institutional theater: policy documents emphasize 'world-class' capabilities and 'cutting-edge' intelligence fusion, but the actual functionality overlaps significantly with existing NCA (National Crime Agency), HMRC, and PwC forensic capabilities. theater_ratio=0.58 reflects moderate theater (policy framing exceeds actual new capacity). The constraint persists through institutional inertia and reputational signaling rather than genuine functional gap. Institutional arbitrage: NECC allows government to claim crime-fighting success while resource constraints limit actual enforcement.
constraint_indexing:constraint_classification(uk_necc_formation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, economic crime enforcement is framed as an immutable requirement of modern capitalism: economies cannot function without anti-fraud/AML mechanisms. This perspective naturalizes NECC as a necessary institutional law. However, the base properties (ε=0.52, suppression=0.65, theater=0.58) contradict the mountain classification — this is a false summit. The 'necessity' naturalizes what is actually a contingent policy choice: other models (decentralized industry self-regulation, international standards without national enforcement, market reputation mechanisms) exist but are politically suppressed. The analytical observer must recognize that framing an institutional choice as a natural law masks the actual power relationships.
constraint_indexing:constraint_classification(uk_necc_formation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(uk_necc_formation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(uk_necc_formation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(uk_necc_formation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(uk_necc_formation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(uk_necc_formation, TR),
    TR >= 0.70.

:- end_tests(uk_necc_formation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The NECC imposes compliance obligations on the financial sector (KYC, reporting, monitoring) that exceed the actual crime suppression benefit realized by those institutions. The burden is not as severe as full snare-level extraction (0.66+) because there is genuine coordination benefit — suppressing organized crime does legitimize the financial system. But the distribution is asymmetric: regulated financial institutions bear most costs; organized criminals bear enforcement costs but remain partially insulated through offshore channels and regulatory arbitrage. The initial extractiveness (0.38) reflects the policy conception — mostly coordination. The rise to 0.52 reflects actual implementation showing higher compliance burden and lower crime suppression than promised. Suppression (0.65): High. Multiple suppression mechanisms: legal barriers to financial privacy (KYC enforcement), mandatory reporting obligations, surveillance expansion, career barriers for actors in underground economy. But suppression is not total (0.80+) because organized crime has partial workarounds (cryptocurrency, offshore jurisdictions, informal networks). Theater ratio (0.58): Moderate. NECC founding rhetoric emphasizes 'world-class' and 'cutting-edge' capabilities, but functional overlap with NCA and HMRC is substantial. However, theater is not dominant (0.70+) because the institution does have real enforcement capacity. The ratio reflects the gap between policy framing (new transformative capability) and actual incremental expansion.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits the full range of DR classification. Organized crime operators see pure extraction (Snare) — they face suppression with no offsetting benefit and no exit option. The financial sector sees mixed coordination and extraction (Tangled Rope) — they benefit from crime suppression (improves their market) but bear compliance costs. Government revenue authorities see net coordination (Rope) — NECC improves tax compliance and financial system legitimacy. International standards coalitions see a temporary scaffolding (Scaffold) — NECC is meant to be superseded by international frameworks. The law enforcement system sees a performative ritual (Piton) — NECC is institutional theater that provides legitimacy without fundamental functional change. The civilizational observer risks seeing natural necessity (Mountain) — but the structural data reveals this as a naturalized political choice. The perspectival gap between the beneficiary (revenue authority) and victims (crime operators, privacy sector) is stark: they experience the same institution as solving a coordination problem vs. imposing pure extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Government revenue authority: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary with minimal extraction cost. Organized crime operators: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction — cannot exit UK territory or legal jurisdiction. Financial privacy sector: Mixed (beneficiary through crime suppression + victim through compliance burden) + constrained → d≈0.68, f(d)≈1.05. Significant extraction but moderated by coordination benefit. International coalition: Organized + constrained → d≈0.35, f(d)≈0.35. Low extraction; has collective agency and sees path forward through standards convergence. Law enforcement system: Institutional + arbitrage → d≈0.08, f(d)≈-0.10. Piton classification comes from theater gate (0.58), not directionality. Analytical observer: analytical → d≈0.72, f(d)≈1.15. Mountain classification is perspectival and vulnerable to false summit detection.
 *
 * MANDATROPHY ANALYSIS:
 *   CONSTRAINT RESOLUTION: The NECC formation avoids mandatrophy by explicitly acknowledging the coordination-extraction hybrid. Base properties show moderate extractiveness (0.52, not 0.66+), indicating this is not a pure snare disguised as coordination. The scaffold perspective confirms that the extraction is intentionally temporary (sunset via international standards maturation). The theater ratio (0.58) is high enough to signal institutional theater but not so high (0.70+) as to collapse into pure piton. The key distinction is BENEFICIARY/VICTIM ASYMMETRY: government revenue authority and financial system legitimacy benefit; organized crime and privacy-oriented actors bear costs. This asymmetry justifies tangled_rope classification. The piton perspective (institutional theater) is real but secondary — the constraint's primary function is coordination (crime suppression) with real institutional enforcement, not pure performative maintenance. The false mountain (civilizational 'necessity') is a perspectival trap that the schema avoids by requiring explicit beneficiary/victim declaration. NECC is a political choice, not a law of nature, because it has clear beneficiaries and victims — if it were a natural law, no one would benefit and no one could exit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    crime_displacement_vs_suppression,
    'Does NECC-enhanced UK enforcement suppress organized crime capacity or merely displace criminal activity to lower-enforcement jurisdictions?',
    'Cross-national econometric analysis of organized crime metrics post-NECC formation; comparison of UK crime rate changes vs. neighboring jurisdictions; asset recovery statistics correlated with displacement indicators',
    'If net suppression: NECC is a genuine public good (Rope classification more justified). If displacement-dominant: NECC is extractive performance theater (Snare classification more justified from field perspective).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(crime_displacement_vs_suppression, empirical, 'Whether NECC suppresses organized crime or displaces it').

omega_variable(
    compliance_burden_distribution,
    'Do NECC compliance obligations fall equally on legitimate financial sector and underground economy, or disproportionately on regulated institutions?',
    'Analysis of Know Your Customer (KYC) implementation costs vs. enforcement action rates; comparison of compliance burden per financial institution vs. prosecutions per organized crime operation',
    'If equally distributed: tangled rope classification confirmed. If disproportionate on regulated sector: extraction mechanism is hidden (higher actual ε; should be Snare from financial sector perspective).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(compliance_burden_distribution, empirical, 'Distribution of NECC compliance burden').

omega_variable(
    international_standards_convergence,
    'Will FATF/UN/UK-EU cooperative frameworks mature to the point where unilateral UK enforcement becomes redundant (confirming scaffold sunset)?',
    'Tracking of international AML/CFT standard adoption rates; cross-border intelligence-sharing protocol maturation; EU-UK post-Brexit data adequacy rulings; FATF mutual evaluation outcomes',
    'If convergence occurs: scaffold thesis is correct and NECC has an engineered sunset. If convergence stalls: NECC becomes permanent and reclassifies as tangled rope or snare depending on extraction evolution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(international_standards_convergence, conceptual, 'Whether international standards will make NECC enforcement redundant').

omega_variable(
    institutional_resource_capture,
    'Will NECC resources be systematically redirected from economic crime toward political priorities (e.g., protest suppression, activist surveillance)?',
    'Historical analysis of law enforcement mission creep; tracking of NECC resource allocation changes post-formation; comparison with FBI resource drift in US; monitoring of parliamentary oversight effectiveness',
    'If resource capture occurs: institutional arbitrage is the true beneficiary (higher actual d for government, revealing piton classification). If oversight holds: tangled rope classification confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_resource_capture, empirical, 'Whether NECC resources will be captured for non-economic-crime purposes').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(uk_necc_formation, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(necc_tr_t0, uk_necc_formation, theater_ratio, 0, 0.42).
narrative_ontology:measurement(necc_tr_t3, uk_necc_formation, theater_ratio, 3, 0.5).
narrative_ontology:measurement(necc_tr_t6, uk_necc_formation, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(necc_be_t0, uk_necc_formation, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(necc_be_t3, uk_necc_formation, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(necc_be_t6, uk_necc_formation, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(uk_necc_formation, enforcement_mechanism).
narrative_ontology:affects_constraint(uk_necc_formation, uk_regulatory_arbitrage_finance).
narrative_ontology:affects_constraint(uk_necc_formation, international_aml_cft_standards).
narrative_ontology:affects_constraint(uk_necc_formation, organized_crime_operational_capacity).

% DUAL FORMULATION NOTE:
% NECC formation is downstream of the international AML/CFT standards (FATF, UN conventions) but represents a unilateral national enforcement choice that may become redundant if cross-border standards mature. The upstream constraint (international standards) has ε≈0.15 (Mountain: standards are technical requirements); NECC formation has ε=0.52 (Tangled Rope: policy choice with real extraction costs). This decomposition shows how a constraint family can span from natural requirement (international standards) to contingent political implementation (NECC).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(uk_necc_formation, moderate, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
