% ============================================================================
% CONSTRAINT STORY: jordan_microfinance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jordan_microfinance, []).

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
 *   constraint_id: jordan_microfinance
 *   human_readable: Ideological Gating of Microfinance in Jordan
 *   domain: economic/development_finance
 *
 * SUMMARY:
 *   The Jordan microfinance constraint exemplifies ideological gating of
 *   capital access. A USAID-backed microfinance program (Finca) in Jordan
 *   offers loans to impoverished borrowers on the condition that they attend
 *   mandatory educational sessions. The program combines genuine access to
 *   credit (a coordination benefit) with coercive ideological compliance
 *   (extraction). The structure creates a perspectival gap: development
 *   institutions perceive coordination and capacity-building; borrowers
 *   perceive a mandatory gate; the analytical observer perceives tangled rope
 *   — mixed coordination and extraction without clear separation. The
 *   extractiveness of 0.58 reflects moderate-to-high coercion: borrowers
 *   cannot access capital without compliance, yet they gain genuine credit
 *   access. The theater ratio of 0.64 reflects that the education sessions
 *   function partly as legitimate financial literacy (function) and partly as
 *   ideological/cultural conditioning (theater), with the proportion shifting
 *   over time as program maturity reveals which components drive borrower
 *   outcomes and which are performative.
 *
 * KEY AGENTS:
 *   - Impoverished Borrowers: Primary victims (powerless/trapped) — cannot access credit without attending mandatory education; no alternative credit sources available at comparable cost
 *   - Informal Economy Sector: Secondary victims (moderate/constrained) — gains access to capital but under ideological compliance conditions; loses autonomy over learning content
 *   - USAID Development Apparatus: Primary beneficiary (institutional/arbitrage) — leverages microfinance to advance development ideology and U.S. policy alignment; funds the program
 *   - Finca International: Primary beneficiary (institutional/arbitrage) — operates program, captures social-impact positioning, aligns with institutional mission
 *   - Jordanian Financial Authority: Secondary actor (institutional/constrained) — constrained by USAID funding dependencies; maintains program compliance frameworks
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jordan_microfinance, 0.58).
domain_priors:suppression_score(jordan_microfinance, 0.68).
domain_priors:theater_ratio(jordan_microfinance, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jordan_microfinance, extractiveness, 0.58).
narrative_ontology:constraint_metric(jordan_microfinance, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(jordan_microfinance, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jordan_microfinance, snare).
narrative_ontology:human_readable(jordan_microfinance, "Ideological Gating of Microfinance in Jordan").
narrative_ontology:topic_domain(jordan_microfinance, "economic/development_finance").

domain_priors:requires_active_enforcement(jordan_microfinance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jordan_microfinance, usaid_development_agenda).
narrative_ontology:constraint_beneficiary(jordan_microfinance, finca_institutional_model).
narrative_ontology:constraint_victim(jordan_microfinance, impoverished_borrowers).
narrative_ontology:constraint_victim(jordan_microfinance, informal_economy_access).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: IMPOVERISHED BORROWER (SNARE) — Trapped. Without access to formal credit, the borrower's only route to capital requires attending ideological education sessions. Exit options are severely constrained: informal lending at higher rates, or no credit at all. The mandatory education is a coercive gate, not a coordination benefit. Maximum experienced extraction — no alternative pathway exists.
constraint_indexing:constraint_classification(jordan_microfinance, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: INFORMAL ECONOMY SECTOR (TANGLED ROPE) — Constrained. The microfinance program offers access to formalized credit (genuine coordination benefit) but gates that access through ideological compliance. The sector benefits from capital availability but bears the cost of ideological enforcement. Mixed coordination (access) and extraction (gating).
constraint_indexing:constraint_classification(jordan_microfinance, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: USAID DEVELOPMENT APPARATUS (ROPE) — Arbitrage exits available; operates globally. The mandatory education sessions serve USAID's stated development goals (financial literacy, democratic values, entrepreneurship norms aligned with U.S. policy). The constraint functions as coordination from this perspective: disseminating development ideology enables USAID to measure program success and justify funding. Net institutional beneficiary.
constraint_indexing:constraint_classification(jordan_microfinance, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: FINCA INTERNATIONAL (ROPE) — Arbitrage exits; operates globally with multiple country portfolios. The mandatory education sessions align with Finca's institutional model (human-centered development, social performance metrics). The constraint enables Finca to differentiate its product (education + credit) and capture social-impact positioning. Net institutional beneficiary with genuine coordination function.
constraint_indexing:constraint_classification(jordan_microfinance, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: JORDANIAN REGULATORY/FINANCIAL AUTHORITY (PITON) — Constrained by external funding dependencies and U.S. policy alignment requirements. The mandatory education requirement appears in regulatory frameworks and donor agreements but is maintained largely through institutional inertia and theater. The authority cannot easily remove it (would anger USAID) nor effectively enforce it (borrowers and lenders develop workarounds). Theater ratio high — the compliance ritual persists despite degraded function.
constraint_indexing:constraint_classification(jordan_microfinance, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational scope, the constraint is revealed as extractive ideology gating (snare for borrowers) wrapped in development coordination language (rope for institutions). The structure is: USAID + Finca benefit from ideological compliance; borrowers pay the coercive gate; the informal economy gains access but loses autonomy. This is not a mountain (not natural law) nor a pure rope (beneficiaries outweigh victims). It is tangled rope: genuine coordination (credit access) with asymmetric extraction (ideological compliance tax).
constraint_indexing:constraint_classification(jordan_microfinance, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jordan_microfinance_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(jordan_microfinance, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(jordan_microfinance, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(jordan_microfinance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(jordan_microfinance, TR),
    TR >= 0.70.

:- end_tests(jordan_microfinance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The microfinance program gates access to capital behind mandatory education attendance. For borrowers with no realistic alternatives, this is coercive — the education is not a choice but a price of entry. The extractiveness is not extreme (0.70+) because the underlying credit product is genuine: borrowers do receive loans on reasonable terms, and the education (however ideologically freighted) conveys some real financial literacy. The trajectory from 0.42 to 0.58 reflects initial program optimism (lower theater, more genuine coordination framing) shifting to maturation (higher theater as the ideological component becomes more visible). Suppression (0.68): High. Borrowers cannot realistically refuse the education and still access credit. Informal lenders exist but at higher cost/terms. The mandatory attendance and the threat of exclusion from the program create suppression of exit options. Suppression is not maximal (0.85+) because some borrowers do have informal alternatives, even if costly. Theater ratio (0.64): Moderate-high. The education sessions combine functional financial literacy (genuine benefit) with ideological/cultural conditioning (development messaging, U.S. values alignment, entrepreneurship norms aligned with donor priorities). The theater has increased from 0.48 to 0.64 as program maturity has revealed that much of the education is performative — compliance is measured by attendance, not outcomes; content is standardized across diverse borrower contexts; and many borrowers already possess the 'taught' financial literacy from informal economy experience.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits a stark perspectival gap between institutional and individual actors. USAID and Finca perceive the education requirement as a coordination benefit (development capacity-building, social performance differentiation). The impoverished borrower perceives a mandatory gate that blocks access without providing real new knowledge (since informal economy participants already understand basic financial management). The analytical observer at civilizational scope perceives tangled rope: the program genuinely expands credit access (coordination function) but taxes that access with ideological compliance (extraction asymmetry). The theater ratio reveals that much of the 'education' is performative — the real function is gating, and the performance is the 'development' framing that justifies the gate. The Jordanian regulatory authority perceives inertial compliance (Piton) — the requirement is maintained because it satisfies donors, not because it drives borrower or lender behavior.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's experienced extractiveness is computed from their structural position. USAID and Finca have arbitrage exits (they operate globally with multiple portfolios) and benefit from the ideological compliance gate, yielding low d values → negative or low χ → Rope classification. Borrowers are trapped (no realistic exit) and bear the cost of mandatory attendance, yielding high d → high f(d) → high χ → Snare classification. The informal economy sector is constrained (it can exit the program but faces cost, since informal lending is more expensive) and experiences mixed benefits/costs, yielding moderate d → moderate χ → Tangled Rope classification. The regulatory authority is constrained by USAID funding dependency and experiences the requirement as inertial, yielding moderate-high d but high theater → Piton classification.
 *
 * MANDATROPHY ANALYSIS:
 *   Snare classification is confirmed for the borrower perspective. The constraint extracts (mandatory education gate) with no genuine coordination function for the borrower — they already possess informal financial knowledge, and attendance is compulsory, not voluntary. The extraction is sustained by suppressing alternatives (informal lending is costlier) and naturalizing the gate as 'development' (theater). However, at the analytical/civilizational level, the constraint is Tangled Rope: there IS a genuine coordination function (credit access to the informal economy) alongside the extraction (ideological gating). The mandatrophy is resolved by recognizing that the constraint operates differently at different scales. For the individual borrower, it is coercive extraction. For the sector/institution, it is mixed coordination-extraction. For development policy, it is ideological alignment via capital conditionality. No single type fully captures the structure — the presheaf of perspectives reveals that the constraint's function varies by observational position.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    education_content_fungibility,
    'Are the mandatory education sessions functionally necessary for borrower financial literacy, or are they primarily vehicles for ideological/cultural conditioning?',
    'Comparison of borrower outcomes (default rates, business success, repeat borrowing) between cohorts receiving identical credit terms with vs. without mandatory education; analysis of education curriculum content for ideological vs. technical components',
    'If functionally necessary: constraint is coordination-dominant (Rope/Tangled Rope). If primarily ideological: constraint is extraction-dominant (Snare). This distinction determines whether the education gate is justified or coercive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(education_content_fungibility, empirical, 'Whether education sessions are functionally necessary or ideologically conditional').

omega_variable(
    exit_option_availability,
    'Do impoverished borrowers in Jordan have realistic alternative sources of credit (informal lenders, family networks, government schemes) that make the microfinance gate a true choice rather than coerced participation?',
    'Survey of borrower exit options and cost differentials; ethnographic documentation of informal lending rates, terms, and accessibility in target communities; analysis of borrower composition (would they have accessed credit without the microfinance program?)',
    'If alternatives exist at comparable cost: exit_options upgrade to ''constrained'' or ''mobile'', lowering d and χ, potentially reclassifying as Tangled Rope even for the borrower perspective. If alternatives don''t exist or are far costlier: confinement is confirmed (d → high), strengthening Snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_option_availability, empirical, 'Availability and cost of alternative credit sources').

omega_variable(
    usaid_ideology_alignment,
    'To what extent is the mandatory education an explicit requirement of USAID funding, versus an institutional choice by Finca that USAID merely accepts or endorses?',
    'Analysis of USAID grant agreements, Finca partnership contracts, and program documentation; interviews with program administrators; historical comparison with Finca programs in non-USAID-funded contexts',
    'If USAID-mandated: the constraint is a direct U.S. policy lever, making it systemic (Snare classification robust). If Finca-chosen (even if USAID-aligned): the constraint is institutional strategy, potentially opening negotiation paths. Distribution of culpability affects mandatrophy resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(usaid_ideology_alignment, empirical, 'Whether education requirement is USAID-mandated or institutionally chosen').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jordan_microfinance, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jmf_tr_t0, jordan_microfinance, theater_ratio, 0, 0.48).
narrative_ontology:measurement(jmf_tr_t5, jordan_microfinance, theater_ratio, 5, 0.58).
narrative_ontology:measurement(jmf_tr_t10, jordan_microfinance, theater_ratio, 10, 0.64).

% Extraction over time
narrative_ontology:measurement(jmf_be_t0, jordan_microfinance, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(jmf_be_t5, jordan_microfinance, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(jmf_be_t10, jordan_microfinance, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jordan_microfinance, resource_allocation).
narrative_ontology:affects_constraint(jordan_microfinance, conditional_cash_transfer_gates).
narrative_ontology:affects_constraint(jordan_microfinance, development_ideology_implementation).

% DUAL FORMULATION NOTE:
% The Jordan microfinance constraint decomposes into two structurally distinct claims: (1) genuine credit access to the informal economy (ε ≈ 0.15, Rope), and (2) ideological gating of that access (ε ≈ 0.65, Snare). These are linked by the program design but represent different constraints. The story prioritizes the combined view (Tangled Rope at analytical scope, Snare at borrower scope) to preserve the structural insight that 'development' programs often combine real benefits with hidden extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jordan_microfinance, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
