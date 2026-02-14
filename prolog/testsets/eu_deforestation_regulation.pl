% ============================================================================
% CONSTRAINT STORY: eu_deforestation_regulation
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-25
% ============================================================================

:- module(constraint_eu_deforestation_regulation, []).

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
    domain_priors:emerges_naturally/1.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: eu_deforestation_regulation
 *   human_readable: EU Deforestation Regulation (EUDR)
 *   domain: economic/political
 *
 * SUMMARY:
 *   The EU Deforestation Regulation (EUDR) prohibits the import of commodities
 *   linked to deforestation. It aims to reduce the EU's contribution to
 *   global deforestation by requiring companies to prove their products are
 *   deforestation-free.
 *
 * KEY AGENTS (by structural relationship):
 *   - Smallholder farmers in developing nations: Primary target (powerless/trapped) — bears extraction
 *   - EU consumers and environmental groups: Primary beneficiary (organized/mobile) — benefits from constraint
 *   - EU importers and traders: Secondary actor (institutional/constrained)
 *   - Analytical observer: Analytical observer (analytical/analytical) — sees full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_deforestation_regulation, 0.48).
domain_priors:suppression_score(eu_deforestation_regulation, 0.70).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(eu_deforestation_regulation, 0.30).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_deforestation_regulation, extractiveness, 0.48).
narrative_ontology:constraint_metric(eu_deforestation_regulation, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(eu_deforestation_regulation, theater_ratio, 0.30).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(eu_deforestation_regulation, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(eu_deforestation_regulation). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
% Without these, the engine falls back to generic power-atom assumptions.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(eu_deforestation_regulation, eu_consumers_environmental_groups).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(eu_deforestation_regulation, smallholder_farmers_developing_nations).
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three)
%   Scaffold:     beneficiary + (has_sunset_clause OR no enforcement)
%   Snare:        victim required; beneficiary optional

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

% PERSPECTIVE 1: SMALLHOLDER FARMERS (SNARE)
% Smallholder farmers lack the resources for traceability and may be excluded
% from EU markets.
constraint_indexing:constraint_classification(eu_deforestation_regulation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EU CONSUMERS AND ENVIRONMENTAL GROUPS (ROPE)
% EU consumers benefit from reduced deforestation and more sustainable products.
constraint_indexing:constraint_classification(eu_deforestation_regulation, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Requires beneficiary + victim + requires_active_enforcement
constraint_indexing:constraint_classification(eu_deforestation_regulation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVES (declare when applicable) ---
% EU Importers and Traders
constraint_indexing:constraint_classification(eu_deforestation_regulation, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_deforestation_regulation_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(eu_deforestation_regulation, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(eu_deforestation_regulation, TypeBeneficiary, context(agent_power(organized), _, _, _)),
    TypeTarget \= TypeBeneficiary.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(eu_deforestation_regulation, ExtMetricName, E),
    E >= 0.46. % high-extraction Tangled Rope

:- end_tests(eu_deforestation_regulation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The EUDR is classified as a Tangled Rope due to its combination of
 *   coordination (reducing deforestation) and asymmetric extraction (burden
 *   on smallholder farmers). The base extractiveness is set at 0.48,
 *   reflecting the significant costs borne by producers who may struggle to
 *   comply with the new requirements. The suppression score is high (0.70)
 *   because the regulation restricts access to the EU market for
 *   non-compliant goods, limiting alternatives.
 *
 * PERSPECTIVAL GAP:
 *   Smallholder farmers perceive the EUDR as a Snare because they lack the
 *   resources to comply, effectively trapping them outside the EU market. EU
 *   consumers and environmental groups see it as a Rope because it
 *   coordinates action to reduce deforestation.
 *
 * DIRECTIONALITY LOGIC:
 *   EU consumers and environmental groups benefit from reduced deforestation,
 *   while smallholder farmers bear the costs of compliance or exclusion.
 *   EU importers and traders are somewhat constrained, as they must ensure
 *   their supply chains meet the new standards, but they also benefit from
 *   a more level playing field and reduced reputational risk.
 *
 * INTER-INSTITUTIONAL DYNAMICS (if applicable):
 *   EU importers face a choice between sourcing from areas at lower risk
 *   of deforestation (constrained) or adopting the standards necessary to sell
 *   into EU markets.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the EUDR as pure extraction by
 *   recognizing its coordination function in reducing deforestation. It avoids
 *   mislabeling it as pure coordination by acknowledging the asymmetric burden
 *   on smallholder farmers. The regulation aims to coordinate action against
 *   deforestation, but its implementation and impact create extraction for
 *   specific groups.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_eudr,
    'Will the EUDR effectively reduce global deforestation, or will it simply shift deforestation to other regions?',
    'Monitoring deforestation rates in both EU-supplier countries and non-EU-supplier countries over time.',
    'If it reduces global deforestation, it is a more successful Tangled Rope. If it merely shifts deforestation, it is closer to a Snare.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(eu_deforestation_regulation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection (metric_substitution,
% extraction_accumulation) by providing measurements at multiple time points.
%
% Required for high-extraction constraints (base_extractiveness > 0.46).
% Use at least 3 time points (T=0, midpoint, T=end) for each tracked metric.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(eu_deforestation_regulation_tr_t0, eu_deforestation_regulation, theater_ratio, 0, 0.10).
narrative_ontology:measurement(eu_deforestation_regulation_tr_t5, eu_deforestation_regulation, theater_ratio, 5, 0.20).
narrative_ontology:measurement(eu_deforestation_regulation_tr_t10, eu_deforestation_regulation, theater_ratio, 10, 0.30).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(eu_deforestation_regulation_ex_t0, eu_deforestation_regulation, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(eu_deforestation_regulation_ex_t5, eu_deforestation_regulation, base_extractiveness, 5, 0.44).
narrative_ontology:measurement(eu_deforestation_regulation_ex_t10, eu_deforestation_regulation, base_extractiveness, 10, 0.48).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(eu_deforestation_regulation, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% Use ONLY when the automatic derivation (beneficiary/victim + exit → d)
% would produce an inaccurate directionality value. The derivation chain
% priority is: override > structural > canonical fallback.
%
% Format: directionality_override(ConstraintID, PowerAtom, D_Value)
%   D_Value in [0.0, 1.0]: 0.0 = full beneficiary, 1.0 = full target
%
% Common override scenarios:
%   - Regulatory capture: institution that appears to benefit but is
%     actually partly captured → override d upward (0.25-0.40)
%   - Indirect beneficiary: agent in victim group who actually benefits
%     through secondary effects → override d downward
%   - Asymmetric institutional: two institutional actors that the
%     derivation can't distinguish → override to differentiate
%
% Example (uncomment if needed):
% constraint_indexing:directionality_override(eu_deforestation_regulation, institutional, 0.30).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */