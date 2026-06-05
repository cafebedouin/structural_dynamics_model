:- module(constraint_access_arbitrage, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:interval/3,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */
/**
 * SUMMARY:
 *   constraint_id: access_arbitrage
 *   human_readable: Access Arbitrage: Reporters Pay in Framing for Institutional Access
 *
 * Reporters at the NYT require access to elite institutions (The White House,
 * The Fed). This access is an arbitrage: the institution grants a quote,
 * and the reporter grants a framing that uses the institution's own
 * lexicon. To break the framing is to lose the access.
 */

/* ==========================================================================
   2. BASE PROPERTIES
   ========================================================================== */
domain_priors:base_extractiveness(access_arbitrage, 0.45).
domain_priors:suppression_score(access_arbitrage, 0.50).
domain_priors:theater_ratio(access_arbitrage, 0.35).

narrative_ontology:constraint_metric(access_arbitrage, extractiveness, 0.45).
narrative_ontology:constraint_metric(access_arbitrage, suppression_requirement, 0.50).
narrative_ontology:constraint_metric(access_arbitrage, theater_ratio, 0.35).

narrative_ontology:constraint_claim(access_arbitrage, tangled_rope).
narrative_ontology:human_readable(access_arbitrage, "Access Arbitrage: Reporters Pay in Framing for Institutional Access").
domain_priors:requires_active_enforcement(access_arbitrage).

narrative_ontology:constraint_beneficiary(access_arbitrage, government_institutions).
narrative_ontology:constraint_victim(access_arbitrage, news_readers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS
   ========================================================================== */
% Beneficiary (Institution): Sees it as a Rope for "orderly information release."
constraint_indexing:constraint_classification(access_arbitrage, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% Target (Reader): Sees it as a Tangled Rope—they get info, but it's pre-processed.
constraint_indexing:constraint_classification(access_arbitrage, tangled_rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

/* ==========================================================================
   4. INTEGRATION HOOKS
   ========================================================================== */
narrative_ontology:interval(access_arbitrage, 0, 10).
narrative_ontology:affects_constraint(access_arbitrage, theatrical_neutrality_snare).
