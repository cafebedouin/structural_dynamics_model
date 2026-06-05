% ============================================================================
% CONSTRAINT STORY: professional_licensing_barriers
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_professional_licensing_barriers, []).

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
 *   constraint_id: professional_licensing_barriers
 *   human_readable: Professional Licensing Barriers as Asymmetric Extraction
 *   domain: labor/regulation/professional_gatekeeping
 *
 * SUMMARY:
 *   Professional licensing creates a structural constraint that exhibits
 *   genuine coordination benefits (credential verification, consumer
 *   protection, quality assurance) inseparably bound to systematic extraction
 *   (supply restriction, wage maintenance, barrier elevation). The constraint
 *   is a paradigmatic Tangled Rope: incumbent practitioners and licensing
 *   boards benefit from both the coordination function and the resulting
 *   supply scarcity. Prospective entrants and low-income consumers bear the
 *   costs of both restricted supply and artificially elevated service prices.
 *   The constraint's evolution shows increasing theater ratio (0.42 → 0.58)
 *   as information technology reduces the original information-asymmetry
 *   problems licensing was designed to solve, yet licensing intensity has
 *   risen, not fallen — suggesting that quality assurance justification is
 *   increasingly theatrical while extraction function persists. The
 *   measurements span 30 years (baseline=1994, midpoint=2009, current=2024)
 *   capturing the period of accelerating information asymmetry reduction
 *   (internet expansion) and simultaneous licensing intensification
 *   (increased education requirements, exam rigor, credential bundling).
 *
 * KEY AGENTS:
 *   - Prospective Entrants: Powerless/trapped (national scope) — face multi-year education requirements, expensive licensing exams, geographic restrictions, renewal fees. Bear suppression (legal barriers + market entry costs) without coordination benefit.
 *   - Low-Income Consumers: Moderate/constrained (regional scope) — face artificially inflated service prices due to restricted supply. Geographic or economic barriers prevent accessing services; unlicensed alternatives are legally prohibited.
 *   - Incumbent Licensed Practitioners: Institutional/arbitrage (national scope) — primary beneficiaries. Benefit from reduced competition, wage premiums, professional network enforcement, and consumer trust signals.
 *   - Licensing Boards: Organized/mobile (national scope) — officially coordinate quality standards but structurally captured by incumbent practitioners. Board membership typically restricted to license holders; regulatory capture is structural, not exceptional.
 *   - Licensing Reform Coalition: Organized/constrained (national scope) — consumer advocates, occupational licensing reform groups, low-income service organizations. Have political traction in some states but face incumbent opposition; constrained by institutional entrenchment.
 *   - Analytical Observer: Analytical/analytical (global scope) — risks naturalizing a specific institutional arrangement as inevitable.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(professional_licensing_barriers, 0.52).
domain_priors:suppression_score(professional_licensing_barriers, 0.68).
domain_priors:theater_ratio(professional_licensing_barriers, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(professional_licensing_barriers, extractiveness, 0.52).
narrative_ontology:constraint_metric(professional_licensing_barriers, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(professional_licensing_barriers, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(professional_licensing_barriers, tangled_rope).
narrative_ontology:human_readable(professional_licensing_barriers, "Professional Licensing Barriers as Asymmetric Extraction").
narrative_ontology:topic_domain(professional_licensing_barriers, "labor/regulation/professional_gatekeeping").

domain_priors:requires_active_enforcement(professional_licensing_barriers).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(professional_licensing_barriers, incumbent_licensed_practitioners).
narrative_ontology:constraint_beneficiary(professional_licensing_barriers, licensing_boards).
narrative_ontology:constraint_victim(professional_licensing_barriers, prospective_entrants).
narrative_ontology:constraint_victim(professional_licensing_barriers, low_income_consumers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PROSPECTIVE ENTRANT (SNARE) — Faces insurmountable barriers to entry: expensive licensing exams, extended education requirements, residency restrictions, grandfathering clauses. Cannot exit without abandoning career aspirations. No coordination benefit accrues to this agent — they are purely victimized by supply restriction.
constraint_indexing:constraint_classification(professional_licensing_barriers, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LOW-INCOME CONSUMER (SNARE) — Constrained by artificially inflated prices resulting from restricted supply of licensed practitioners. Cannot access affordable services in many markets; licensing reduces competition and raises consumer cost. Suppression is high because alternatives (unlicensed practitioners, self-service) are legally prohibited or heavily stigmatized.
constraint_indexing:constraint_classification(professional_licensing_barriers, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INCUMBENT LICENSED PRACTITIONER (ROPE) — Experiences the constraint as coordination: licensing assures consumers of quality, builds consumer trust, enables professional networks. Benefits from reduced competition and wage premiums. Has arbitrage options (geographic relocation, credential reciprocity, specialization). Net beneficiary — extraction runs toward this agent; they perceive the constraint as solving a legitimate coordination problem (credential verification, consumer protection) and capture the benefits.
constraint_indexing:constraint_classification(professional_licensing_barriers, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: LICENSING BOARD (TANGLED ROPE) — Has a genuine coordination function (setting standards, verifying credentials, disciplining incompetent practitioners) but also enforces supply restrictions that benefit incumbents. Board members are often incumbent practitioners (conflicts of interest). Mobile in principle (can relax standards) but constrained by incumbent pressure and regulatory capture. The board itself has become a mechanism for asymmetric extraction even as it performs a legitimate coordination function.
constraint_indexing:constraint_classification(professional_licensing_barriers, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: HISTORICAL LEGITIMACY NARRATIVE (PITON) — The original justification for professional licensing (consumer protection in opaque service markets) has atrophied as information asymmetries have declined (internet reviews, price comparison, professional standardization). Yet licensing persists through institutional inertia. Theater ratio is elevated because much regulatory activity now performs legitimacy-maintenance rather than actual quality assurance — continuing education requirements, examination rigor, and disciplinary procedures persist more as ritual than as effective mechanisms. The functional constraint (ensuring practitioner quality) has degraded while the extractive constraint (supply restriction) remains.
constraint_indexing:constraint_classification(professional_licensing_barriers, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: LICENSING REFORM MOVEMENT (SCAFFOLD) — Organized actors (consumer advocates, occupational licensing reform coalitions, lower-income service organizations) see licensing barriers as a temporary coordination failure with a sunset path. Reciprocity agreements, competency-based assessments replacing time-in-grade, federal portability, and tiered licensing (apprenticeship pathways) represent structural solutions. The constraint is temporary because the coordination function (quality assurance) can be provided through alternative mechanisms (third-party certification, reputation systems, outcome-based accountability) with lower extraction. Sunset logic: as alternative verification mechanisms mature, licensing's monopoly on credential verification weakens.
constraint_indexing:constraint_classification(professional_licensing_barriers, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the constraint appears immutable: opaque service markets always require credentialing mechanisms; information asymmetries are inherent to professional services; some form of licensure is inevitable. But this perspective risks naturalizing a specific institutional arrangement. The engine's false summit detector will flag this as a false mountain — the apparent inevitability conceals structural choices (who sets standards? who benefits from supply restriction? what alternatives to licensing exist?) that are not laws of nature but contingent policy decisions.
constraint_indexing:constraint_classification(professional_licensing_barriers, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(professional_licensing_barriers_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(professional_licensing_barriers, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(professional_licensing_barriers, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(professional_licensing_barriers, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(professional_licensing_barriers, TR),
    TR >= 0.70.

:- end_tests(professional_licensing_barriers_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint restricts entry (reducing supply) and increases prices (transferring wealth from consumers to incumbents), but some extraction is justified by genuine quality assurance function. The 0.52 value reflects that extraction is significant but not maximal — a snare with pure extraction would score ≥0.66. The measurement trajectory (0.38 → 0.52) shows rising extraction despite falling information asymmetry, suggesting increasing supply-restriction pressure. Suppression (0.68): High. Significant legal barriers (examination requirements, education mandates, geographic restrictions, renewal fees) reduce alternatives and prevent entry. Suppression is not total because some alternatives exist (unlicensed practitioners, self-service, regulated alternatives like nurse practitioners), but legal/market barriers are substantial. Theater ratio (0.58): Moderate-high. Initial theater ratio (0.42) reflects that licensing had functional quality-assurance content in 1994 when information asymmetries were severe. Current theater ratio (0.58) reflects that continuing education, examination rigor, and disciplinary procedures increasingly perform legitimacy-maintenance rather than quality assurance — many requirements persist despite declining information failures. This is the clearest evidence that the constraint has shifted from coordination-dominant to extraction-dominant over the 30-year window.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows the full range of classification from fundamentally different structural positions. Incumbents see Rope (pure coordination benefiting all) because they internalize the supply restriction as legitimate and experience the wage premium as earned through quality verification. Low-income consumers see Snare (pure extraction with no benefit) because they cannot access services at all or only at inflated prices. Prospective entrants see Snare (barriers to entry with no immediate benefit, though some may later become incumbents and flip perspectives). Licensing boards see Tangled Rope in their immediate context (genuine quality-assurance work) but might see Piton from a longer horizon (the quality work is increasingly performative). Reform movements see Scaffold (the coordination function is real but can be achieved through alternative mechanisms with lower extraction). The analytical observer risks seeing Mountain (information asymmetries in professional services are inevitable) but the structural data reveals this as false naturalization — the specific institutional arrangement (state-granted monopoly on credentialing) is a policy choice, not a natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim status and exit options. Incumbents (beneficiaries + arbitrage) have d ≈ 0.15 (low extraction toward them). Prospective entrants (victims + trapped) have d ≈ 0.95 (maximum extraction target). Low-income consumers (victims + constrained) have d ≈ 0.85 (high extraction target with some options). Licensing boards occupy an interesting structural position: officially they are institutional actors with arbitrage options (can relax standards), but actually they are captured institutional actors (cannot deviate from incumbent preferences without facing political pressure and board replacement). The override would elevate board d from the derived ~0.20 to ~0.35-0.40 to capture this captured institutional position — the board is not a pure beneficiary even though it nominally has institutional power. This is regulatory capture in action: structural power (ability to set standards) is exercised on behalf of incumbents, not independently.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that professional licensing exhibits all six types simultaneously because the constraint has multiple structural dimensions that different observers weight differently. The Tangled Rope classification at the claimed_type level reflects the dominant structural fact: genuine coordination (quality assurance) and genuine extraction (supply restriction) are inseparable in the current institutional arrangement. The Snare classifications from powerless and moderate perspectives reflect that these agents experience only the extraction; they cannot capture the coordination benefit. The Rope classification from incumbent perspective reflects that incumbents experience primarily the coordination benefit (consumer trust, professional legitimacy) and capture the extraction benefit (wage premium, supply protection). The Piton classification from the historical narrative perspective reflects that the coordination function (information asymmetry reduction) has atrophied while the extraction function (supply restriction) persists, maintained through institutional theater. The Scaffold perspective reflects that the constraint's extraction function can be separated from its coordination function through alternative institutional designs (tiered licensing, outcome-based accountability, reciprocity agreements). The false Mountain at the analytical level reflects the risk of naturalizing a specific institutional arrangement as inevitable. All classifications are structurally valid — the mandatrophy is resolved by the ε-invariance principle: the constraint has one ε (0.52) and one definite structure (Tangled Rope), but multiple legitimate perspectival readings of that structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    information_asymmetry_reduction,
    'Do modern information systems (reputation platforms, price transparency, outcome tracking) actually reduce the market information failures that licensing was designed to remedy?',
    'Empirical comparison: consumer harm rates in lightly-licensed markets (cosmetics, personal training) vs heavily-licensed markets (law, medicine) controlling for service complexity; international comparison of licensing stringency vs consumer protection outcomes',
    'If information systems sufficiently reduce asymmetries: licensing extraction becomes unjustifiable and the constraint reclassifies toward Snare across more perspectives. If information systems remain inadequate: licensing coordination function remains essential and justification for some supply restriction holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(information_asymmetry_reduction, empirical, 'Whether digital information systems reduce opaque service market failures').

omega_variable(
    alternative_credentialing_feasibility,
    'Can tiered licensing (apprenticeship→journeyperson→master), industry-recognized certification, or outcome-based accountability replicate licensing''s coordination function with lower extraction?',
    'Case study analysis of hybrid licensing models; comparison of quality assurance effectiveness in reformed vs traditional licensing regimes; tracking of consumer outcomes under alternative credential systems',
    'If alternatives are feasible: supply restriction has no coordination justification, and the constraint is pure Snare from all perspectives except incumbents. If alternatives fail quality gates: some licensing function is irreducible, supporting Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_credentialing_feasibility, empirical, 'Whether alternative credentialing mechanisms can replace licensing').

omega_variable(
    incumbent_capture_degree,
    'What fraction of licensing board decisions reflect incumbent interest in supply restriction vs. genuine quality assurance or consumer protection?',
    'Analysis of regulatory decisions correlated with incumbent income effects; tracking of exam difficulty evolution; comparing jurisdictions with reformist vs incumbent-controlled boards',
    'If capture is high (>70%): constraint is structurally Snare even if coordination function nominally exists. If capture is low (<30%): constraint legitimately remains Tangled Rope with meaningful coordination component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incumbent_capture_degree, empirical, 'Proportion of licensing decisions driven by incumbent interest vs quality assurance').

omega_variable(
    geographic_wage_premium_origin,
    'How much of the wage premium for licensed practitioners is attributable to genuine quality vs. supply restriction?',
    'Jurisdictional comparison: wage premiums in states with strict licensing vs states with reciprocity/reduced barriers, controlling for practitioner selection and service complexity; comparison of wage premiums to quality outcome differences',
    'If most premium is from supply restriction (>60%): extraction is the primary function. If most premium reflects quality (>60%): coordination is primary and some extraction is justified cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geographic_wage_premium_origin, empirical, 'Attribution of wage premiums to quality vs supply restriction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(professional_licensing_barriers, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prof_lic_tr_t0, professional_licensing_barriers, theater_ratio, 0, 0.42).
narrative_ontology:measurement(prof_lic_tr_t15, professional_licensing_barriers, theater_ratio, 15, 0.52).
narrative_ontology:measurement(prof_lic_tr_t30, professional_licensing_barriers, theater_ratio, 30, 0.58).

% Extraction over time
narrative_ontology:measurement(prof_lic_be_t0, professional_licensing_barriers, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(prof_lic_be_t15, professional_licensing_barriers, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(prof_lic_be_t30, professional_licensing_barriers, base_extractiveness, 30, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(prof_lic_su_t0, professional_licensing_barriers, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(prof_lic_su_t15, professional_licensing_barriers, suppression_requirement, 15, 0.65).
narrative_ontology:measurement(prof_lic_su_t30, professional_licensing_barriers, suppression_requirement, 30, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(professional_licensing_barriers, enforcement_mechanism).
narrative_ontology:affects_constraint(professional_licensing_barriers, occupational_wage_inequality).
narrative_ontology:affects_constraint(professional_licensing_barriers, geographic_service_supply_inequality).

% DUAL FORMULATION NOTE:
% Professional licensing connects to wage inequality (occupational licensing enforces wage premiums for incumbents) and geographic service availability (supply restriction concentrates services in wealthy urban areas). Each downstream constraint has its own ε reflecting different observables — licensing barriers create wage premiums (separate constraint) and geographic supply gaps (separate constraint). This story models the licensing mechanism itself; the others model its effects.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(professional_licensing_barriers, organized, 0.38).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
