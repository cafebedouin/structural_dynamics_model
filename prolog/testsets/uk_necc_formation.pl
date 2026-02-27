% ============================================================================
% CONSTRAINT STORY: uk_necc_formation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
 *   domain: political/economic/law_enforcement
 *
 * SUMMARY:
 *   The UK National Economic Crime Centre (NECC) represents a major
 *   institutional consolidation in law enforcement, modelled on the FBI,
 *   designed to combat economic crimes including fraud, money laundering, and
 *   kleptocracy. The NECC formation exemplifies the tension between genuine
 *   coordination (fragmented law enforcement creating evasion opportunities)
 *   and extractive expansion (centralized institutional authority creating
 *   surveillance overhead and scope creep). The constraint manifests as
 *   Tangled Rope at the baseline: legitimate coordination benefits (unified
 *   enforcement against distributed criminal networks) coexist with
 *   asymmetric extraction (enforcement targeting some populations more than
 *   others, compliance costs borne by regulated institutions, potential scope
 *   creep into political investigation). The theater_ratio has risen from
 *   0.42 to 0.65 over the formation interval, indicating increasing emphasis
 *   on visible institutional performance (prosecutions, asset seizures,
 *   organizational announcements) relative to measured crime reduction
 *   outcomes. The extractiveness has grown from 0.38 to 0.52 as the NECC's
 *   scope has broadened and operational authority has expanded. This dual
 *   movement — rising theater and rising extractiveness — is diagnostic of
 *   institutional scope creep: the NECC may have begun as a genuine
 *   coordination mechanism but is accumulating extractive properties as it
 *   consolidates authority.
 *
 * KEY AGENTS:
 *   - UK Government Executive: Primary beneficiary (institutional/arbitrage) — gains centralized law enforcement control and institutional capacity. Can exit by reallocating resources or delegating authority.
 *   - Financial Crime Targets (Money Launderers, Economic Fraudsters): Primary victims (powerless/trapped) — face concentrated investigative and prosecutorial authority with no exit option within UK jurisdiction.
 *   - Legitimate Financial Institutions: Secondary beneficiary and victim (moderate/constrained) — benefit from reduced economic crime but bear compliance costs and surveillance overhead. Constrained by cooperation requirements.
 *   - Law Enforcement Institutional (NCA, Regional Squads): Reorganized under NECC — treated as resources rather than autonomous actors. Potential piton dynamic: existing institutional structures maintained through reorganization rather than eliminated.
 *   - Financial Compliance and RegTech Sector: Institutional beneficiary (organized/constrained) — benefits from increased compliance demand but sees NECC as temporary institutional form as international standards evolve.
 *   - International Law Enforcement Community (FBI, EUROPOL, INTERPOL): Collaborative beneficiary and potential director (organized/constrained) — benefits from UK coordination but may influence NECC priorities through intelligence requests.
 *   - Political Dissidents and Civil Society Groups: Implicit potential victims (powerless/constrained) — at risk of scope creep into NECC investigative authority (omega variable).
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing institutional consolidation as necessary natural law rather than recognizing it as a contingent political choice.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(uk_necc_formation, 0.52).
domain_priors:suppression_score(uk_necc_formation, 0.48).
domain_priors:theater_ratio(uk_necc_formation, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(uk_necc_formation, extractiveness, 0.52).
narrative_ontology:constraint_metric(uk_necc_formation, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(uk_necc_formation, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(uk_necc_formation, tangled_rope).
narrative_ontology:human_readable(uk_necc_formation, "UK National Economic Crime Centre (NECC) Formation").
narrative_ontology:topic_domain(uk_necc_formation, "political/economic/law_enforcement").

domain_priors:requires_active_enforcement(uk_necc_formation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(uk_necc_formation, uk_government_executive).
narrative_ontology:constraint_beneficiary(uk_necc_formation, financial_compliance_sector).
narrative_ontology:constraint_beneficiary(uk_necc_formation, law_enforcement_institutional).
narrative_ontology:constraint_victim(uk_necc_formation, financial_crime_targets).
narrative_ontology:constraint_victim(uk_necc_formation, jurisdictional_autonomy_regional).
narrative_ontology:constraint_victim(uk_necc_formation, operational_transparency_constraint).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TARGETED MONEY LAUNDERER (SNARE) — Faces centralized, well-resourced enforcement agency with investigative powers. No exit option; trapped within UK jurisdiction. Maximum extraction through prosecution, asset seizure, and criminal sanction. The NECC concentrates enforcement power against distributed illicit networks with limited coordination capacity.
constraint_indexing:constraint_classification(uk_necc_formation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LEGITIMATE FINANCIAL INSTITUTION (TANGLED ROPE) — Benefits from reduced economic crime (coordination function: cleaner financial system, reduced contagion risk), but also bears compliance costs and surveillance overhead. Constrained by regulatory requirements to cooperate with NECC. Mixed extraction and coordination — institutional participation is both protection and burden.
constraint_indexing:constraint_classification(uk_necc_formation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: UK GOVERNMENT EXECUTIVE (ROPE) — Primary beneficiary. NECC enables centralized control of economic crime enforcement, improves law and order enforcement capacity, and projects institutional competence. Net coordination benefit: solves collective action problem of fragmented enforcement (NCA, Regional Crime Squads, etc.). Arbitrage position: can exit the constraint by delegating authority or changing resource allocation.
constraint_indexing:constraint_classification(uk_necc_formation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: FINANCIAL COMPLIANCE AND REGTECH SECTOR (SCAFFOLD) — Benefits from NECC through increased compliance demand, regulatory clarity, and market expansion. However, sees NECC as a temporary institutional form: international standards (FATF, AML/CFT) are evolving toward decentralized compliance (blockchain identity, distributed ledger clearing). As international norms mature, NECC's centralized model becomes less essential. Theater_ratio is moderate — visible enforcement activity (prosecutions, asset seizures) performs political function while actual economic crime requires ongoing operational evolution.
constraint_indexing:constraint_classification(uk_necc_formation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: TRADITIONAL LAW ENFORCEMENT BUREAUCRACY (PITON) — Regional and specialized police forces (NCA legacy, regional organized crime units) are reorganized under NECC. The constraint is maintained through institutional inertia — the existing law enforcement apparatus resists disintermediation, and NECC becomes a consolidation vehicle for preserving hierarchical control. Theater is high: visible organizational restructuring and press announcements about 'new American-style FBI model' perform institutional competence while actual investigative capacity may not increase proportionally.
constraint_indexing:constraint_classification(uk_necc_formation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: INTERNATIONAL INTELLIGENCE AND LAW ENFORCEMENT COMMUNITY (TANGLED ROPE) — Benefits from UK coordination (improved information sharing, operational cooperation with FBI, EUROPOL, etc.) but also constrained by interagency coordination requirements and intelligence-sharing protocols. NECC acts as a coordination device (rope function) for international law enforcement cooperation, but also extracts institutional autonomy from partner agencies through centralized UK gatekeeping.
constraint_indexing:constraint_classification(uk_necc_formation, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/analytical perspective, some centralization of law enforcement authority is inherent to modern nation-state functioning: complex organized crime requires coordination across jurisdictions, and distributed enforcement is mathematically subject to evasion. From this view, NECC emergence appears as a natural institutional response to the inherent coordination problem of policing in a networked economy. However, the structural data (0.52 extractiveness, 0.48 suppression, beneficiaries + victims) contradicts mountain classification — this is a false summit. The 'natural necessity' framing obscures contingent choices about institutional design, accountability mechanisms, and resource allocation.
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
 *   Extractiveness (0.52): Moderate-high. The NECC consolidates enforcement authority over economic crime targets (high extraction), imposes compliance costs on financial institutions (moderate extraction), and creates centralized institutional gatekeeping for international law enforcement coordination (moderate extraction). However, extraction is not total (Snare-level ≥0.66) because legitimate crime reduction is a genuine outcome and some institutional actors (government executive, compliance sector) experience genuine benefits. The rising trajectory (0.38 → 0.52) reflects expanding scope and authority consolidation. Suppression (0.48): Moderate. Economic crime targets face high barriers to exit (trapped within jurisdiction), but alternative enforcement modalities (international cooperation, decentralized investigation) exist and are not completely suppressed. Compliance costs for financial institutions create suppression (limited choice in cooperation), but this is policy-imposed rather than inherent. Theater_ratio (0.58): Moderate-high. NECC prosecutions and asset seizures are heavily publicized and perform institutional legitimacy. However, the visibility may exceed the actual crime reduction impact — the theater has increased from 0.42 as the institution has matured and emphasized public-facing enforcement activity. Claimed type: Tangled Rope. The NECC has genuine coordination benefits (unified enforcement against distributed criminal networks) and asymmetric extraction (benefits flow disproportionately to government executive and compliance sector, costs distributed across crime targets and financial institutions). Requires active enforcement (true): NECC is actively built and enforced, not a passive coordination mechanism.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same institutional structure — a centralized law enforcement agency — can appear as coordination (Rope) from the beneficiary's perspective, extraction (Snare) from the target's perspective, and hybrid (Tangled Rope) from the institution implementing it. The NECC performs a genuine coordination function (solving fragmented enforcement) but simultaneously enables extraction (concentrating power, creating scope creep risk). The theater_ratio rise (0.42 → 0.65) indicates increasing emphasis on visible institutional performance, suggesting that the NECC is accumulating piton properties — performative institutional maintenance beyond functional necessity. The international perspective reveals that NECC may be simultaneously Rope (for international cooperation) and Snare (if UK gatekeeping extracts value from partner intelligence). The traditional law enforcement perspective (Piton) recognizes that organizational restructuring may preserve rather than transform institutional power dynamics, maintaining existing hierarchies under new labeling.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality derives from their structural position relative to the extraction/coordination flow. The government executive benefits from NECC authority (beneficiary position) and can exit by reallocating resources (arbitrage options) — derives low d (approximately 0.15), experiences negative or minimal extraction, sees Rope classification. The crime target bears costs (victim position) and cannot exit UK jurisdiction (trapped options) — derives high d (approximately 0.90), experiences maximum effective extraction, sees Snare classification. The financial institution is a beneficiary (reduced crime contagion) but constrained (compliance requirements, cooperation obligations), deriving moderate d (approximately 0.50), experiencing moderate effective extraction, seeing Tangled Rope. The international law enforcement community is a beneficiary (shared intelligence, coordination gains) but constrained by institutional gatekeeping (depends on NECC for UK access), deriving moderate d (approximately 0.55), experiencing moderate effective extraction. The traditional law enforcement bureaucracy is formally reorganized (appears as beneficiary from preservation of hierarchy) but constrained by new NECC authority structure, deriving moderate d with institutional inertia dynamics. The analytical observer, by definition, occupies a position of maximum uncertainty about the true extraction flow — they see both coordination and extraction possibilities, deriving d near 0.50, at the inflection point of the sigmoid function where f(d) ≈ 0.65. No directionality overrides are declared; the structural derivation captures the essential relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is NOT YET RESOLVED. The NECC exhibits genuine coordination properties (unified enforcement, crime reduction) and genuine extraction properties (compliance costs, centralized gatekeeping, scope creep risk). The classification as Tangled Rope is defensible from the baseline properties, but the rising extractiveness (0.38 → 0.52) and rising theater (0.42 → 0.65) create risk of classification migration toward Snare as institutional scope expands. The unresolved omegas (extraction vs. coordination threshold, scope creep, international sovereignty, performance measurement, sector capture) all point toward a contingent future: if enforcement remains crime-focused and accountability mechanisms prevent scope creep, the Tangled Rope classification stabilizes. If extractiveness continues rising and theater increases further, the true type may migrate toward Snare at the analytical level, with the Rope classification (government executive perspective) revealing itself as a beneficiary illusion that obscures broader extraction. The mandatrophy resolution requires longitudinal observation of the omega variables — particularly whether prosecution targets remain economic crimes (coordination function) or expand into political dissent (extraction function) and whether the compliance burden on financial institutions increases proportionally to crime reduction benefits.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extraction_vs_coordination_threshold,
    'What fraction of NECC''s enforcement activity represents genuine economic crime reduction vs. asset capture and institutional expansion?',
    'Longitudinal tracking of prosecution outcomes (conviction rates, asset recovery legitimacy), cost-benefit analysis of enforcement activity relative to GDP impact of economic crime, comparison to pre-NECC baseline enforcement metrics',
    'If >70% of activity produces genuine crime reduction: NECC classifies as Rope from more perspectives. If <50% represents outcome-driven enforcement: NECC is primarily extractive (Snare from regulator perspective, Tangled Rope from victim perspective).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_coordination_threshold, empirical, 'Whether NECC enforcement represents crime reduction or institutional asset capture').

omega_variable(
    accountability_and_scope_creep,
    'Does NECC''s centralized investigative authority create scope creep into political investigation, dissent suppression, or regulatory capture by financial institutions?',
    'Longitudinal tracking of NECC investigation scope; audit of prosecution target distribution (organized crime vs. white-collar vs. political); analysis of regulatory requests from financial institutions relative to actual crime targets',
    'If scope creep confirmed: suppression increases, victimhood expands beyond economic criminals to include political dissent or regulatory targets. Classification shifts toward Snare from broader perspectives.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(accountability_and_scope_creep, empirical, 'Whether centralized NECC authority produces institutional scope creep').

omega_variable(
    international_coordination_vs_uk_sovereignty,
    'Does NECC''s role as UK gateway to international law enforcement (FBI, EUROPOL, INTERPOL) enhance UK law enforcement capacity or subordinate UK institutional autonomy to foreign intelligence priorities?',
    'Analysis of NECC''s relationship to Five Eyes intelligence sharing; audit of investigation triggers (UK-initiated vs. international request-driven); comparison of UK to international prosecution priorities',
    'If UK-initiated investigations dominate: NECC enhances UK institutional capacity (rope/scaffold perspective). If international request-driven: NECC becomes a conduit for foreign extraction (snare perspective, victims = UK subjects under foreign intelligence demand).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_coordination_vs_uk_sovereignty, empirical, 'Whether NECC enhances UK autonomy or subordinates it to international intelligence').

omega_variable(
    performance_measurement_and_theater,
    'How much of NECC''s visible activity (public prosecutions, asset seizures, press announcements) is driven by genuine crime reduction objectives vs. performance metrics and institutional legitimacy theater?',
    'Analysis of prosecution case selection and public visibility; correlation between press coverage intensity and actual economic crime impact; examination of resource allocation toward high-visibility vs. high-impact cases',
    'If high theater ratio persists (>0.65): NECC classification shifts toward Piton across more perspectives. If theater declines over time: theater_ratio falls below 0.50, supporting Tangled Rope or Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performance_measurement_and_theater, empirical, 'What fraction of NECC activity is performative vs. functionally driven').

omega_variable(
    institutional_capture_by_regulated_sector,
    'Does NECC''s dependence on private financial sector intelligence (bank reporting, compliance data) create regulatory capture dynamics where financial institutions shape NECC priorities?',
    'Audit of NECC investigation targets relative to financial sector risk profiles; analysis of funding and resource allocation decisions influenced by financial sector lobbying; study of revolving door between NECC leadership and private compliance roles',
    'If capture confirmed: beneficiaries include financial_compliance_sector more than crime reduction itself. Victims expand to include small-business fraud targets and individual debtors (lower-priority in institutional capture scenarios). Classification shifts toward Snare for non-financial-sector crime victims.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_capture_by_regulated_sector, empirical, 'Whether NECC is captured by financial sector interests').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(uk_necc_formation, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(necc_theater_t0, uk_necc_formation, theater_ratio, 0, 0.42).
narrative_ontology:measurement(necc_theater_t3, uk_necc_formation, theater_ratio, 3, 0.58).
narrative_ontology:measurement(necc_theater_t5, uk_necc_formation, theater_ratio, 5, 0.65).

% Extraction over time
narrative_ontology:measurement(necc_extract_t0, uk_necc_formation, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(necc_extract_t3, uk_necc_formation, base_extractiveness, 3, 0.47).
narrative_ontology:measurement(necc_extract_t5, uk_necc_formation, base_extractiveness, 5, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(uk_necc_formation, enforcement_mechanism).
narrative_ontology:affects_constraint(uk_necc_formation, financial_surveillance_infrastructure).
narrative_ontology:affects_constraint(uk_necc_formation, uk_regulatory_capture_banking).
narrative_ontology:affects_constraint(uk_necc_formation, international_law_enforcement_gatekeeping).

% DUAL FORMULATION NOTE:
% The NECC formation is downstream of fragmented UK law enforcement structure and upstream of specific enforcement outcomes. The coordination benefit (solving fragmentation) is genuine but distinct from the extraction risk (centralized authority). If future analysis reveals that NECC primarily serves as institutional gatekeeping for international law enforcement rather than domestic crime reduction, this story should be decomposed into two constraints: (1) UK_internal_crime_coordination (lower extractiveness, Rope or Scaffold), (2) UK_international_law_enforcement_gatekeeping (higher extractiveness, potential Snare). Network relationships indicate structural influence on financial surveillance infrastructure, regulatory dynamics with banking sector, and international law enforcement coordination patterns.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
