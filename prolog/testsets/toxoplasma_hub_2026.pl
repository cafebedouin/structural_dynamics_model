% ============================================================================
% CONSTRAINT STORY: toxoplasma_hub_2026
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_toxoplasma_hub_2026, []).

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
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: toxoplasma_hub_2026
 *   human_readable: The Toxoplasma Cyst as an Active Hub
 *   domain: biological/medical
 *
 * SUMMARY:
 *   Single-cell RNA sequencing has revealed that Toxoplasma gondii cysts are not
 *   dormant but are active metabolic hubs. They house at least five distinct
 *   bradyzoite subtypes geared toward survival, spread, or reactivation. This
 *   biological complexity creates a "Sovereignty Gap" in modern medicine, as
 *   current therapies only target fast-replicating forms, leaving the active
 *   hubs inside neurons untouchable and capable of extracting host resources.
 *
 * KEY AGENTS (by structural relationship):
 *   - infected_human_hosts: Primary target (powerless/trapped) — bears extraction via neurological alteration.
 *   - toxoplasma_gondii_parasite: Primary beneficiary (institutional/mobile) — benefits from the cyst's protective and resource-extracting functions.
 *   - medical_researchers: Analytical observer — sees the full structure of the host-parasite interaction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Extraction is high. Cysts extract neuronal integrity by altering
% extracellular vesicle (EV) signaling and glutamate regulation.
domain_priors:base_extractiveness(toxoplasma_hub_2026, 0.78).
% Suppression is near-total. The protective cyst wall suppresses all
% existing medical treatments and immune system elimination.
domain_priors:suppression_score(toxoplasma_hub_2026, 0.85).
% Theater ratio is moderate. The "dormant" life cycle model was a
% theatrical oversimplification now challenged by functional data.
domain_priors:theater_ratio(toxoplasma_hub_2026, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(toxoplasma_hub_2026, extractiveness, 0.78).
narrative_ontology:constraint_metric(toxoplasma_hub_2026, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(toxoplasma_hub_2026, theater_ratio, 0.42).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(toxoplasma_hub_2026, snare).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(toxoplasma_hub_2026, toxoplasma_gondii_parasite).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(toxoplasma_hub_2026, infected_human_hosts).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   Do not add measurement_basis, beneficiary/victim, or other metadata.
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% PERSPECTIVE 1: THE INFECTED HOST (SNARE)
% The host is trapped by the cyst, which extracts neurological resources and
% cannot be removed by current medicine or the immune system.
constraint_indexing:constraint_classification(toxoplasma_hub_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE PARASITE (ROPE)
% From the parasite's perspective, the cyst is a pure coordination mechanism
% for long-term survival, resource management, and eventual transmission.
constraint_indexing:constraint_classification(toxoplasma_hub_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (SNARE)
% An analyst sees the high, asymmetric extraction (ε=0.78) and near-total
% suppression of alternatives (suppression=0.85), classifying it as a
% biological snare, not a fundamental law (mountain).
constraint_indexing:constraint_classification(toxoplasma_hub_2026, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(toxoplasma_hub_2026_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(toxoplasma_hub_2026, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(toxoplasma_hub_2026, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget \= TypeBeneficiary.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(toxoplasma_hub_2026, ExtMetricName, E),
    (E =< 0.25 ; E >= 0.46). % Mountain or high-extraction Snare/Tangled.

test(analytical_claim_consistency) :-
    narrative_ontology:constraint_claim(toxoplasma_hub_2026, ClaimedType),
    constraint_indexing:constraint_classification(toxoplasma_hub_2026, AnalyticalType, context(agent_power(analytical), _, _, _)),
    ClaimedType == AnalyticalType.

:- end_tests(toxoplasma_hub_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (0.78): High. The cyst is not passive but actively
 *     modulates host neuron function, altering glutamate regulation and
 *     extracellular vesicle signaling for its own benefit. This represents a
 *     significant extraction of neurological integrity and resources.
 *   - Suppression Score (0.85): Very high. The cyst's structure makes it
 *     impervious to both the host immune system and all current antimicrobial
 *     therapies, which only target the active, replicating form of the parasite.
 *     There are no viable exit options for the host.
 *   - Theater Ratio (0.42): Moderate. The previous scientific consensus of a
 *     "dormant" cyst was a form of theater (an oversimplified model) that has
 *     been dismantled by new evidence of its metabolic and functional activity.
 *
 * PERSPECTIVAL GAP:
 *   The gap is a classic host-parasite dynamic. The infected host (powerless,
 *   trapped) experiences the cyst as a Snare: an inescapable trap that extracts
 *   resources and degrades autonomy. The parasite (institutional, mobile)
 *   experiences the same structure as a Rope: a perfect coordination tool for
 *   its own long-term survival and propagation. The analytical observer sides
 *   with the host's classification, recognizing the high asymmetric extraction
 *   as the defining feature, making it a Snare.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `toxoplasma_gondii_parasite`. The parasite is the sole
 *     beneficiary. The cyst structure ensures its persistence and protection.
 *   - Victim: `infected_human_hosts`. The host bears all costs through the
 *     appropriation of neuronal resources and potential long-term neurological
 *     sequelae. The directionality is unambiguously from host to parasite.
 *
 * MANDATROPHY ANALYSIS:
 *   [RESOLVED MANDATROPHY] The high extraction score (0.78) is validated by
 *   the clear, one-way flow of resources from host to parasite. This is not a
 *   complex coordination problem with high overhead; it is a direct,
 *   asymmetric biological extraction. Classifying this as a Snare from the
 *   analytical perspective correctly identifies the nature of the constraint and
 *   avoids the mandatrophy of mislabeling a parasitic relationship as a form
 *   of difficult but necessary coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_toxoplasma_hub_2026,
    'Are the observed neurological alterations a side-effect of cyst presence, or an evolved manipulative function to aid transmission?',
    'Longitudinal studies correlating specific bradyzoite subtype populations with host behavioral changes and transmission success rates.',
    'If side-effect -> Snare (collateral extraction). If manipulative function -> Snare (instrumental extraction). The classification is stable, but the intentionality of the extraction mechanism remains uncertain.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(toxoplasma_hub_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection. The interval represents the shift in
% scientific understanding from the "dormant cyst" model to the "active hub" model.

% Theater ratio drops as the "dormant" myth is replaced by "active hub" data.
narrative_ontology:measurement(tx_tr_t0, toxoplasma_hub_2026, theater_ratio, 0, 0.70).
narrative_ontology:measurement(tx_tr_t5, toxoplasma_hub_2026, theater_ratio, 5, 0.56).
narrative_ontology:measurement(tx_tr_t10, toxoplasma_hub_2026, theater_ratio, 10, 0.42).

% Extraction rises as the long-term impact on glutamate regulation is quantified.
narrative_ontology:measurement(tx_ex_t0, toxoplasma_hub_2026, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(tx_ex_t5, toxoplasma_hub_2026, base_extractiveness, 5, 0.57).
narrative_ontology:measurement(tx_ex_t10, toxoplasma_hub_2026, base_extractiveness, 10, 0.78).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% No Boltzmann or network data is applicable for this biological constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed. The structural derivation from beneficiary/victim
% status accurately models the host-parasite relationship.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */