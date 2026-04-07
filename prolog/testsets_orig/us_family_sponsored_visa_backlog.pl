% ============================================================================
% CONSTRAINT STORY: us_family_sponsored_visa_backlog
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_family_sponsored_visa_backlog, []).

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
 *   constraint_id: us_family_sponsored_visa_backlog
 *   human_readable: U.S. Family-Sponsored Visa Backlog
 *   domain: immigration_policy/family_separation
 *
 * SUMMARY:
 *   The U.S. family-sponsored visa backlog creates a structural constraint on
 *   family reunification that operates through policy-driven delays rather
 *   than inherent coordination costs. As of 2024, millions of
 *   family-sponsored visa applicants wait 5-15+ years for processing,
 *   creating indefinite family separation. The constraint exhibits
 *   characteristics of tangled rope (genuine coordination function — the visa
 *   system does attempt to match family members — combined with extraction
 *   function — the delays protect labor market interests and justify
 *   enforcement infrastructure) while appearing as snare to powerless
 *   separated agents and rope to protected labor market sectors. The backlog
 *   is not a natural law of immigration; countries like Canada process family
 *   visas in 1-3 years, revealing that the US delay is policy-driven through
 *   per-country visa caps, visa category allocations, and staffing decisions.
 *   The theater ratio reflects that the system invokes family reunification
 *   as a principle while systematically preventing it — the gap between
 *   stated norm and actual performance has widened over the measurement
 *   interval.
 *
 * KEY AGENTS:
 *   - Separated Family Members Abroad: Primary victims (powerless/trapped) — bear separation costs with no exit options; cannot work in US during indefinite wait
 *   - U.S. Family Sponsors (Petitioners): Mixed position (moderate/constrained) — benefit from US stability but bear separation and legal costs; experience both coordination and extraction
 *   - Protected Labor Market Sectors: Primary beneficiaries (institutional/arbitrage) — benefit from labor supply reduction via delayed family visas; experience pure coordination benefit
 *   - Immigration Enforcement Apparatus: Secondary beneficiary (powerful/mobile) — justifies budget and personnel through queue management; maintains extraction mechanism
 *   - Immigration Reform Coalition: Organized opposition (organized/constrained) — perceives scaffold with sunset; advocates for legislative reform to process acceleration
 *   - U.S. Government (State/Labor/DHS): Institutional enforcer (institutional/arbitrage) — maintains policy-driven delays as tool for labor market protection and border sovereignty
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent policy choices as immutable features of state sovereignty
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_family_sponsored_visa_backlog, 0.58).
domain_priors:suppression_score(us_family_sponsored_visa_backlog, 0.68).
domain_priors:theater_ratio(us_family_sponsored_visa_backlog, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_family_sponsored_visa_backlog, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_family_sponsored_visa_backlog, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(us_family_sponsored_visa_backlog, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_family_sponsored_visa_backlog, tangled_rope).
narrative_ontology:human_readable(us_family_sponsored_visa_backlog, "U.S. Family-Sponsored Visa Backlog").
narrative_ontology:topic_domain(us_family_sponsored_visa_backlog, "immigration_policy/family_separation").

domain_priors:requires_active_enforcement(us_family_sponsored_visa_backlog).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_family_sponsored_visa_backlog, us_labor_market_protected_workers).
narrative_ontology:constraint_beneficiary(us_family_sponsored_visa_backlog, immigration_enforcement_apparatus).
narrative_ontology:constraint_victim(us_family_sponsored_visa_backlog, separated_family_members).
narrative_ontology:constraint_victim(us_family_sponsored_visa_backlog, visa_applicants_in_queue).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SEPARATED FAMILY MEMBER (SNARE) — Trapped in indefinite separation. No alternative visa pathways for most family relationships. Cannot exit by switching to employment-based visa (sponsorship requires employer, not family). Faces maximum suppression: legal barriers (visa category caps), economic barriers (cannot work in US during wait), and emotional costs of separation. Experiences pure extraction — the US captures labor market protection benefit while the family member bears the separation cost. Zero coordination benefit perceived — the delay serves no purpose for the separated party. Maximum experienced extraction.
constraint_indexing:constraint_classification(us_family_sponsored_visa_backlog, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: US FAMILY SPONSOR (TANGLED ROPE) — Benefits from the constraint (labor market protection through delayed visa issuance) and bears costs (separation from family, emotional burden, legal fees). Has constrained exit options: can sponsor but cannot accelerate process; cannot switch to different family member without restarting. Experiences genuine coordination function (the visa system does coordinate family reunification, even if slowly) alongside extraction (the system delays reunification to protect labor market, extracting emotional cost). Mixed experience — partial benefit from US labor market stability, significant cost from separation.
constraint_indexing:constraint_classification(us_family_sponsored_visa_backlog, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PROTECTED LABOR MARKET SECTOR (ROPE) — Benefits from the backlog as coordination mechanism that reduces labor supply pressure. The delay functions as a coordination tool: it prevents wage depression in protected sectors by slowing family reunification of lower-skilled workers. Experiences the constraint as pure coordination with minimal coercion — the sector does not need to actively suppress the visa queue; the system naturally produces the protective effect. Low experienced extraction because the benefit is passive coordination, not active rent-seeking.
constraint_indexing:constraint_classification(us_family_sponsored_visa_backlog, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: IMMIGRATION ENFORCEMENT APPARATUS (TANGLED ROPE) — Powerful institutional actor with mixed interests. Benefits from the backlog as justification for budget, personnel, and enforcement infrastructure (prevents argument that visa processing could be fully automated or eliminated). Bears costs of managing queue (administrative burden), political pressure from advocates (emotional leverage from separated families), and legal challenges. Has mobile exit options (could restructure entirely, though politically costly). Experiences genuine coordination function (manages the visa system) alongside extraction (maintains backlog as institutional preservation mechanism). The apparatus is both solution provider and beneficiary of the problem's persistence.
constraint_indexing:constraint_classification(us_family_sponsored_visa_backlog, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: IMMIGRATION REFORM COALITION (SCAFFOLD) — Organized agents (advocacy groups, family separation organizations, some legislators) perceive the backlog as a temporary policy failure with a sunset. The coalition sees clear exit pathway: legislative reform (increase visa cap numbers, streamline processing, eliminate per-country caps). Experiences the constraint as coordination problem (matching family members across borders) with artificially created extraction (policy-driven delays). Has constrained exit options but perceives a structural solution. Theater ratio is moderate for this perspective: the coalition's advocacy work is partially performative (generating political pressure) but also functional (building coalition, drafting reform proposals). Classifies as scaffold because the sunset is perceived as achievable within a generational timeframe (10-20 years of advocacy pressure) and the suppression would decline once processing accelerates.
constraint_indexing:constraint_classification(us_family_sponsored_visa_backlog, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: FAMILY REUNIFICATION NORM (PITON) — The US legal framework enshrines family reunification as a principle (INA foundation, constitutional family liberty interests). But the principle is substantially performative: stated in law yet systematically undermined by visa caps and allocation mechanisms. The norm persists through institutional inertia and rhetorical invocation ('family is the cornerstone of immigration') despite low functional compliance. Theater ratio is high: the system claims to honor family reunification while structurally preventing it. The norm is degraded — maintained because it provides political legitimacy, not because it operates effectively.
constraint_indexing:constraint_classification(us_family_sponsored_visa_backlog, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FALSE NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal analytical perspective, one might naturalize the backlog as an immutable feature of border control: 'managing immigration inevitably produces delays; this is inherent to sovereignty.' This perspective treats the backlog as emerging naturally from the structural requirement to verify identities and screen applicants. However, the empirical data contradicts this: processing times and backlogs vary dramatically across countries and visa categories, revealing that the delay is policy-driven, not inherent. The engine will flag this as a false summit — the naturalization disguises contingent institutional choices (per-country caps, visa category allocations, staffing decisions) as immutable laws of state sovereignty.
constraint_indexing:constraint_classification(us_family_sponsored_visa_backlog, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_family_sponsored_visa_backlog_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_family_sponsored_visa_backlog, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_family_sponsored_visa_backlog, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_family_sponsored_visa_backlog, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_family_sponsored_visa_backlog, TR),
    TR >= 0.70.

:- end_tests(us_family_sponsored_visa_backlog_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The backlog transfers costs (separation, delay, legal fees) from protected labor market to separated families. However, the extraction is not as severe as a pure snare (0.70+) because the US also provides a genuine coordination service — the visa system does eventually reunify families, even if slowly. The 0.58 reflects that the primary function (family coordination) exists alongside significant extraction (delay as labor market protection). The value has increased from 0.42 to 0.58 across the interval, indicating that visa processing has slowed relative to application growth, and the extraction mechanism has deepened. Suppression (0.68): High. Separated families face legal barriers (visa category caps, per-country limits), economic barriers (cannot work in US during wait, costs of maintaining two households), and emotional barriers (separation duration normalizes as permanent). Suppression has remained relatively stable across the interval — the structural barriers are systemic, not declining. Theater ratio (0.55): Moderate. The system invokes family reunification as foundational principle ('family is the cornerstone of US immigration policy') while systematically delaying it. The gap between rhetoric and practice is substantial but not total — some families are reunified, and the system does process cases, just very slowly. Theater has slightly increased as the gap between stated commitment and actual processing times has widened.
 *
 * PERSPECTIVAL GAP:
 *   The gap between snare and rope perspectives reveals the extraction mechanism: the separated family member sees pure extraction because the delays serve no purpose for them (family coordination is the stated purpose but delayed) and all costs fall on them (separation). The protected sector sees pure coordination because they benefit from the side effect of delay (wage protection) without experiencing coercion (the benefit flows to them passively). Both perspectives are empirically accurate from their positions — the gap itself is the diagnostic signal that this is tangled rope, not rope or snare alone.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations map to real structural relationships: labor market sectors benefit through reduced wage pressure (arbitrage exit, low d); separated families bear costs with no exit (trapped exit, high d); enforcement apparatus benefits through queue-justified budgets and personnel (arbitrage exit, moderate-low d); family sponsors occupy mixed position (constrained exit, moderate d). These directionality values feed into f(d) sigmoid, which scales effective extraction. Trapped agents with high d experience maximum multiplier; arbitrage beneficiaries with low d experience subsidy effect (negative extraction). No directionality overrides needed — the derivation chain from structural positions produces accurate d values.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVES MANDATROPHY: The constraint avoids mislabeling pure coordination as extraction by explicitly declaring both coordination function (visa system does reunify families, though slowly) and extraction function (delays protect labor market, justify apparatus budgets). The tangled rope classification captures both mechanisms simultaneously. The snare perspective shows what pure extraction looks like from the separated family's view; the rope perspective shows what pure coordination looks like from the protected sector's view. The mandatrophy is resolved by accepting that the constraint has BOTH genuine coordination (family matching) and genuine extraction (labor protection), not one disguised as the other. The challenge is that rhetoric naturalizes this mix: policy-makers present labor protection as incidental to family coordination, when structural analysis reveals the labor protection is the primary function and family coordination is the constraint being extracted from.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    labor_market_protection_threshold,
    'At what visa issuance rate does meaningful labor market protection occur, and at what rate does it disappear?',
    'Econometric analysis of wage pressure correlation with visa processing speed; controlled comparison across countries and time periods with different visa caps and processing rates',
    'If threshold is low (achieved at high visa issuance rates): backlog provides minimal actual protection, and the snare classification dominates. If threshold is high (requires sustained low issuance): the tangled rope classification with genuine extraction is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_market_protection_threshold, empirical, 'Labor market protection effectiveness threshold').

omega_variable(
    legislative_reform_feasibility,
    'Is legislative reform to increase visa caps and accelerate processing politically achievable within 10-20 years, or is the backlog structurally permanent?',
    'Political capacity analysis; historical precedent for major immigration law reform; coalition strength tracking; measurement of legislative window dynamics',
    'If reform is achievable: scaffold classification is justified, and the constraint has a real sunset. If reform is politically frozen: the constraint is more like a snare or piton than a scaffold, and the extraction has permanence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legislative_reform_feasibility, preference, 'Political feasibility of legislative reform').

omega_variable(
    suppression_mechanism_internalization,
    'To what degree have separated families internalized the backlog as inevitable, versus maintaining active resistance and hope for change?',
    'Qualitative research on family member narratives; measurement of advocacy participation rates; longitudinal tracking of psychological adaptation vs active organizing',
    'If internalized: suppression acts as a structural lock even if legal barriers hypothetically weakened. If resistance remains active: the suppression is structural but not internalized, and reform pressure persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Internalization of backlog as inevitable versus active resistance').

omega_variable(
    enforcement_apparatus_institutional_dependence,
    'How deeply is the immigration enforcement apparatus''s organizational structure dependent on maintaining the backlog for budgetary and personnel justification?',
    'Organizational analysis of USCIS/NVC staffing models, budget allocation tied to queue size, automation-resistance mechanisms, political testimony dependencies',
    'If highly dependent: the apparatus has strong extractive incentive to maintain the backlog, suggesting snare or tangled rope. If loosely dependent: apparatus could transition to processed-case metrics without resistance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_apparatus_institutional_dependence, empirical, 'Institutional dependence of enforcement apparatus on backlog').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_family_sponsored_visa_backlog, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fsvb_tr_t0, us_family_sponsored_visa_backlog, theater_ratio, 0, 0.48).
narrative_ontology:measurement(fsvb_tr_t5, us_family_sponsored_visa_backlog, theater_ratio, 5, 0.52).
narrative_ontology:measurement(fsvb_tr_t10, us_family_sponsored_visa_backlog, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(fsvb_be_t0, us_family_sponsored_visa_backlog, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(fsvb_be_t5, us_family_sponsored_visa_backlog, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(fsvb_be_t10, us_family_sponsored_visa_backlog, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_family_sponsored_visa_backlog, resource_allocation).
narrative_ontology:affects_constraint(us_family_sponsored_visa_backlog, us_employment_based_visa_specialization).
narrative_ontology:affects_constraint(us_family_sponsored_visa_backlog, family_separation_psychological_impact).

% DUAL FORMULATION NOTE:
% The family visa backlog is distinct from individual visa category constraints but affects those constraints through resource allocation (visa number pools are zero-sum across categories). The resource allocation coordination type applies — the constraint coordinates allocation of limited visa slots across multiple categories and countries.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_family_sponsored_visa_backlog, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
