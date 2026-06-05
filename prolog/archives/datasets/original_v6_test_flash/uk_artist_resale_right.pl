% ============================================================================
% CONSTRAINT STORY: uk_artist_resale_right
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_uk_artist_resale_right, []).

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
 *   constraint_id: uk_artist_resale_right
 *   human_readable: UK Artist's Resale Right (ARR) Legislation
 *   domain: economic/legal
 *
 * SUMMARY:
 *   The UK Artist's Resale Right (ARR), enacted in 2006, is a legal mandate
 *   requiring a royalty payment to the original artist (or their estate) upon
 *   the resale of their work by an art market professional. This aims to
 *   provide artists with a share of the increasing value of their work over
 *   time, but also imposes costs on art dealers and auction houses.
 *
 * KEY AGENTS:
 *   - UK Artists and Estates: Primary beneficiaries, receiving royalties from resales.
 *   - Art Dealers and Auction Houses: Bear the costs of collecting and remitting royalties.
 *   - European Commission: Originating body of the legislation; enforcement now a piton post-Brexit.
 *   - Analytical Observer: Assesses the overall economic and cultural impact of the ARR.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(uk_artist_resale_right, 0.35).
domain_priors:suppression_score(uk_artist_resale_right, 0.25).
domain_priors:theater_ratio(uk_artist_resale_right, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(uk_artist_resale_right, extractiveness, 0.35).
narrative_ontology:constraint_metric(uk_artist_resale_right, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(uk_artist_resale_right, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(uk_artist_resale_right, tangled_rope).
narrative_ontology:human_readable(uk_artist_resale_right, "UK Artist's Resale Right (ARR) Legislation").
narrative_ontology:topic_domain(uk_artist_resale_right, "economic/legal").

domain_priors:requires_active_enforcement(uk_artist_resale_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(uk_artist_resale_right, uk_artists).
narrative_ontology:constraint_beneficiary(uk_artist_resale_right, artist_estates).
narrative_ontology:constraint_victim(uk_artist_resale_right, art_dealers).
narrative_ontology:constraint_victim(uk_artist_resale_right, auction_houses).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The ARR provides a small but consistent income stream, especially for established artists and their estates, who can arbitrage their artistic capital.
constraint_indexing:constraint_classification(uk_artist_resale_right, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% Art market professionals are constrained by the legal requirement to collect and remit the resale royalties, increasing transaction costs and potentially impacting pricing. They also benefit from increased market transparency and perceived fairness.
constraint_indexing:constraint_classification(uk_artist_resale_right, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% The ARR originated in the EU. Now that the UK has left the EU, the legislation acts as a piton, continuing due to inertia and potential negative impacts on the art market if repealed. The EC's role is now largely performative.
constraint_indexing:constraint_classification(uk_artist_resale_right, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(continental))).

% The ARR represents a complex interplay of economic incentives, artistic recognition, and market regulation. It attempts to balance the rights of artists with the efficiency of the art market but introduces extraction in the form of transaction costs and potential disincentives for trading certain artworks.
constraint_indexing:constraint_classification(uk_artist_resale_right, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(uk_artist_resale_right_tests).

test(piton_threshold) :-
    domain_priors:theater_ratio(uk_artist_resale_right, TR),
    TR >= 0.70.

:- end_tests(uk_artist_resale_right_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness: 0.35 - Represents the royalty extracted from art sales. Suppression: 0.25 - Reflects the limited ability of art dealers to avoid complying with the ARR. Theater Ratio: 0.15 - The ARR has a low theater ratio, as it is primarily a functional mechanism for royalty collection, with minimal performative aspects.
 *
 * PERSPECTIVAL GAP:
 *   Artists and their estates benefit from the royalty payments, viewing the ARR as a rope. Art dealers and auction houses experience it as a tangled rope due to the increased transaction costs and administrative burden. The European Commission's perspective is now that of a piton because of Brexit. The analytical observer sees the balance between artistic rights and market efficiency as creating a tangled rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Artists benefit and have some power to advocate for the right. Art dealers are negatively affected and are constrained, but can pass costs on, hence their directionality falls between target and symmetric. The analytical observer sees the overall picture.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    market_impact_quantification,
    'What is the actual impact of the ARR on art market prices and trading volumes, and is this impact disproportionately borne by certain segments of the market?',
    'Econometric analysis of art market data, controlling for other factors influencing prices and volumes.',
    'If the impact is substantial and negative, the ARR may be reclassified as a snare. If the impact is negligible, it may be reclassified as a rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_impact_quantification, empirical, 'Quantification of ARR''s art market impact').

omega_variable(
    administrative_burden_assessment,
    'How significant is the administrative burden on art dealers and auction houses to comply with the ARR, and could this burden be reduced through technological or regulatory improvements?',
    'Surveys and interviews with art market professionals, coupled with cost-benefit analysis of potential regulatory reforms.',
    'If the administrative burden is high, it reinforces the tangled rope classification. If the burden is low, it could shift the perspective of art dealers towards a more rope-like classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(administrative_burden_assessment, empirical, 'Assessment of ARR''s administrative burden').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(uk_artist_resale_right, 2006, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uk_a_tr_t2006, uk_artist_resale_right, theater_ratio, 2006, 0.05).
narrative_ontology:measurement(uk_a_tr_t2015, uk_artist_resale_right, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(uk_a_tr_t2024, uk_artist_resale_right, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(uk_a_be_t2006, uk_artist_resale_right, base_extractiveness, 2006, 0.3).
narrative_ontology:measurement(uk_a_be_t2015, uk_artist_resale_right, base_extractiveness, 2015, 0.33).
narrative_ontology:measurement(uk_a_be_t2024, uk_artist_resale_right, base_extractiveness, 2024, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(uk_artist_resale_right, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
