% ============================================================================
% CONSTRAINT STORY: med_diet_consensus_2026
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_med_diet_consensus_2026, []).

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
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:omega_variable/3,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: med_diet_consensus_2026
 *   human_readable: Mediterranean Diet Scientific Hegemony
 *   domain: health/scientific/economic
 *
 * SUMMARY:
 *   This constraint models the socio-economic and scientific consensus that
 *   establishes the Mediterranean diet as the "gold standard" of nutrition.
 *   While based on positive health outcomes, the consensus functions as a
 *   Tangled Rope: it provides a genuine coordination benefit for public health
 *   while simultaneously creating economic extraction for low-income populations
 *   and suppressing alternative dietary patterns by classifying them as "fads"
 *   or "sub-optimal." This influences insurance, public health policy, and
 *   social status.
 *
 * KEY AGENTS (by structural relationship):
 *   - Low-Income Populations: Primary target (powerless/trapped) — bears economic extraction from high food costs and social pressure.
 *   - Public Health Bodies & Scientists: Primary beneficiary (institutional/mobile) — benefits from a clear, evidence-backed standard for policy and research.
 *   - Processed Food Industry: Secondary victim (organized/constrained) — loses market share and legitimacy.
 *   - Analytical Observer: Sees the full structure of coordination and asymmetric extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Rationale: The economic cost of "required" foods (olive oil, fresh fish, nuts)
% for low-income groups, plus the social cost of non-compliance, represents
% significant extraction.
domain_priors:base_extractiveness(med_diet_consensus_2026, 0.48).

% Rationale: The scientific consensus actively delegitimizes and suppresses
% alternative dietary models ("fads," "beaten in RCTs"), creating high barriers
% to their adoption in mainstream medicine and public health.
domain_priors:suppression_score(med_diet_consensus_2026, 0.65).

% Rationale: The costs and benefits are real; the constraint is not primarily
% performative, though it has status-signaling elements.
domain_priors:theater_ratio(med_diet_consensus_2026, 0.17).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(med_diet_consensus_2026, extractiveness, 0.48).
narrative_ontology:constraint_metric(med_diet_consensus_2026, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(med_diet_consensus_2026, theater_ratio, 0.17).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(med_diet_consensus_2026, tangled_rope).

% --- Binary flags ---
% Rationale: The consensus is actively maintained by journals, universities,
% public health bodies, and media, not a law of nature.
domain_priors:requires_active_enforcement(med_diet_consensus_2026).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(med_diet_consensus_2026, global_healthcare_systems).
narrative_ontology:constraint_beneficiary(med_diet_consensus_2026, public_health_scientists).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(med_diet_consensus_2026, low_income_populations).
narrative_ontology:constraint_victim(med_diet_consensus_2026, processed_food_industry).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE)
% For those who cannot afford fresh fish, nuts, and high-quality olive oil,
% the "Gold Standard" acts as a Snare. It is a normative requirement for
% health that is economically inaccessible, creating systemic exclusion.
% Engine derives d ≈ 0.95 → high χ.
constraint_indexing:constraint_classification(med_diet_consensus_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% The scientist or public health body sees the diet as a "Rope"—a beneficial
% coordination mechanism grounded in RCT data. It is a tool to improve public
% health outcomes, with low perceived extraction.
% Engine derives d ≈ 0.15 → low/negative χ.
constraint_indexing:constraint_classification(med_diet_consensus_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analyst sees the complete structure: a genuine coordination function
% (health benefits) tangled with significant asymmetric extraction (economic
% costs for the poor) and active suppression of alternatives.
% Engine derives d ≈ 0.72 → moderate/high χ.
constraint_indexing:constraint_classification(med_diet_consensus_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(med_diet_consensus_2026_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target (Snare) and beneficiary (Rope).
    constraint_indexing:constraint_classification(med_diet_consensus_2026, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(med_diet_consensus_2026, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(med_diet_consensus_2026, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_properties) :-
    % Verify the structural requirements for a Tangled Rope are met.
    domain_priors:requires_active_enforcement(med_diet_consensus_2026),
    narrative_ontology:constraint_beneficiary(med_diet_consensus_2026, _),
    narrative_ontology:constraint_victim(med_diet_consensus_2026, _).

:- end_tests(med_diet_consensus_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The original file conflated the biological basis for the diet (a potential
 *   Mountain) with the socio-economic policy consensus built upon it. This
 *   regeneration focuses strictly on the policy consensus, which is a human-
 *   constructed constraint. The metrics were adjusted to reflect a Tangled
 *   Rope structure: base_extractiveness (0.48) captures the high economic cost
 *   for the poor, and suppression_score (0.65) reflects the active
 *   delegitimization of alternative diets by the scientific establishment.
 *   The constraint requires active enforcement via journals, grants, and public
 *   health messaging to maintain its hegemony.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For institutional beneficiaries (scientists, health bodies),
 *   the consensus is a pure coordination Rope that simplifies their work and
 *   improves outcomes. For powerless victims (low-income populations), it is a
 *   Snare of social judgment and economic burden, where they are blamed for
 *   failing to adhere to a standard they cannot afford. The analytical observer
 *   sees both sides and classifies it as a Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: `global_healthcare_systems` and `public_health_scientists`
 *     benefit from a simplified, evidence-based public health message that
 *     reduces long-term healthcare costs and provides clear research direction.
 *   - Victims: `low_income_populations` bear the direct cost of expensive
 *     staples, while the `processed_food_industry` bears the cost of
 *     delegitimization and lost market share. This asymmetric cost/benefit
 *     structure is the core of the Tangled Rope.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly identifies that a constraint with a genuine,
 *   data-backed coordination function can still be highly extractive and
 *   coercive. It avoids mislabeling the consensus as a pure Rope (ignoring
 *   the economic victims) or a pure Snare (ignoring the real health benefits).
 *   The Tangled Rope classification captures this duality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_med_diet_genetic_universality,
    "Does the Mediterranean diet benefit all genetic haplotypes equally, or is the 'gold standard' biased toward European metabolic history?",
    "Genome-wide association studies across diverse global populations (beyond the Seven Countries Study)",
    "If biased: Snare for non-European populations. If universal: Strengthens Rope/coordination aspect.",
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(omega_med_diet_genetic_universality, empirical, "Universality of diet benefits across different genetic populations.").

omega_variable(
    omega_med_diet_supply_chain_collapse,
    "Will climate change render Mediterranean staples (olive oil, specific fish) so scarce that the diet becomes an elitist Snare for all but the wealthy?",
    "Monitoring agricultural yields in the Mediterranean basin vs. global demand over next 20 years",
    "If scarce: Degrades from Tangled Rope to a pure Snare. If adaptable: Remains a Tangled Rope.",
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(omega_med_diet_supply_chain_collapse, empirical, "Impact of climate change on the economic accessibility of diet staples.").

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(med_diet_consensus_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Base extraction > 0.46 requires temporal data.
% Models the intensification of the consensus from 1999 (initial RCTs) to 2026.
% As the consensus solidified, the economic premium (extraction) on "approved"
% foods increased.
narrative_ontology:measurement(med_diet_ex_t0, med_diet_consensus_2026, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(med_diet_ex_t5, med_diet_consensus_2026, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(med_diet_ex_t10, med_diet_consensus_2026, base_extractiveness, 10, 0.48).

% Theater ratio remained low and stable; the constraint's function is substantive.
narrative_ontology:measurement(med_diet_tr_t0, med_diet_consensus_2026, theater_ratio, 0, 0.15).
narrative_ontology:measurement(med_diet_tr_t5, med_diet_consensus_2026, theater_ratio, 5, 0.16).
narrative_ontology:measurement(med_diet_tr_t10, med_diet_consensus_2026, theater_ratio, 10, 0.17).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% The consensus acts as a standard for information (what is "healthy").
narrative_ontology:coordination_type(med_diet_consensus_2026, information_standard).

% --- Network Decomposition (Constraint Families) ---
% DUAL FORMULATION NOTE:
% This constraint (the policy consensus) is one of two stories decomposed from
% the colloquial label "the Mediterranean diet". The other is the underlying
% biological reality.
% Decomposed because ε differs across observables (ε-invariance principle).
% Related stories:
%   - med_diet_biology (ε≈0.05, Mountain) - The physiological response to nutrients.
%   - med_diet_consensus_2026 (ε=0.48, Tangled Rope) - This file.
%
% narrative_ontology:affects_constraint(med_diet_biology, med_diet_consensus_2026).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed. The structural derivation from beneficiary/victim
% declarations and exit options accurately models the directionality.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */