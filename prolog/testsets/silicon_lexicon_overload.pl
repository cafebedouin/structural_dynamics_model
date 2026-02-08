% ============================================================================
% CONSTRAINT STORY: silicon_lexicon_overload
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-22
% ============================================================================

:- module(constraint_silicon_lexicon_overload, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

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
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: silicon_lexicon_overload
 * human_readable: The Silicon Lexicon (Corporate Tech-Speak)
 * domain: linguistic/technological/social
 * * SUMMARY:
 * The specialized jargon of the tech industry (e.g., "bandwidth," "alignment," "synergy") has escaped its original context and become a mandatory social and professional protocol in many corporate environments. It functions as a powerful tool that standardizes communication for efficiency but also extracts human nuance, rebranding complex emotional and social states as machine-like inputs and outputs. Failure to adopt this lexicon results in professional marginalization.
 * * KEY AGENTS:
 * - Employees: Subject (Powerless)
 * - Corporate Management: Beneficiary (Institutional)
 * - Linguistic Analyst: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(silicon_lexicon_overload, 0.80). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(silicon_lexicon_overload, 0.70).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(silicon_lexicon_overload, 0.10).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(silicon_lexicon_overload, extractiveness, 0.80).
narrative_ontology:constraint_metric(silicon_lexicon_overload, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(silicon_lexicon_overload, theater_ratio, 0.10).

% Constraint self-claim (what does the constraint claim to be?)
% Values: natural_law, coordination, constructed, enforcement
narrative_ontology:constraint_claim(silicon_lexicon_overload, tangled_rope).

% Binary flags
domain_priors:requires_active_enforcement(silicon_lexicon_overload). % Required for Tangled Rope

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
% Both are required for Tangled Rope. Coordination is also required for Scaffold.
narrative_ontology:constraint_beneficiary(silicon_lexicon_overload, corporate_management).
narrative_ontology:constraint_victim(silicon_lexicon_overload, employees).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For an employee, the lexicon is a coercive trap. Being told you lack "bandwidth"
% for a family emergency is a direct application of the lexicon as an extractive tool,
% translating human needs into a language of system failure.
constraint_indexing:constraint_classification(silicon_lexicon_overload, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% For management, the lexicon is a pure coordination tool. It creates a high-speed,
% low-friction communication protocol to align thousands of individuals towards a
% singular goal, appearing as essential infrastructure for efficiency.
constraint_indexing:constraint_classification(silicon_lexicon_overload, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analyst sees both functions. The lexicon is a Tangled Rope: it provides a
% genuine coordination benefit (Rope aspect) but achieves it through coercive
% suppression of other linguistic modes, enabling asymmetric extraction of value
% from employees (Snare aspect).
constraint_indexing:constraint_classification(silicon_lexicon_overload, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(silicon_lexicon_overload_tests).

test(perspectival_gap_rope_snare) :-
    % Verify the core perspectival gap between the powerless and institutional views.
    constraint_indexing:constraint_classification(silicon_lexicon_overload, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(silicon_lexicon_overload, rope, context(agent_power(institutional), _, _, _)).

test(analytical_resolution_is_tangled_rope) :-
    % Verify the analytical observer correctly identifies the hybrid nature.
    constraint_indexing:constraint_classification(silicon_lexicon_overload, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_requirements_met) :-
    % A Tangled Rope requires enforcement, a beneficiary (for coordination), and a victim (for extraction).
    domain_priors:requires_active_enforcement(silicon_lexicon_overload),
    narrative_ontology:constraint_beneficiary(silicon_lexicon_overload, _),
    narrative_ontology:constraint_victim(silicon_lexicon_overload, _).

:- end_tests(silicon_lexicon_overload_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The base extractiveness (0.80) and suppression (0.70) are high because the lexicon isn't just a set of words; it's a cognitive framework that actively devalues and displaces other ways of knowing and communicating (e.g., emotional, intuitive, poetic). It extracts value by forcing human experience into a machine-readable format optimized for corporate productivity metrics.
 * The Perspectival Gap is stark: what is a tool of efficient coordination (Rope) for management is a tool of dehumanizing coercion (Snare) for employees.
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * The high extraction score (0.80) creates a Mandatrophy risk, where the system might incorrectly label a useful coordination tool as pure extraction. This is resolved by the Analytical Observer's classification of the constraint as a 'Tangled Rope'. This classification formally acknowledges that the constraint possesses BOTH a genuine coordination function (the Rope aspect valued by beneficiaries) AND a severe, asymmetric extractive function (the Snare aspect felt by victims). The system does not have to choose; it recognizes the hybrid nature, resolving the paradox.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_silicon_lexicon_overload,
    'Is the lexicon a deliberately engineered control system (Snare) or a naturally emergent linguistic phenomenon of a technological society that was later co-opted (Mountain -> Snare)?',
    'Historical linguistic analysis tracking the origin of key jargon from technical papers into corporate mission statements and HR policy.',
    'If engineered, it is a tool of social control. If emergent and co-opted, it reflects a deeper cultural shift where society voluntarily adopts machine-like cognition.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(silicon_lexicon_overload, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data for this high-extraction constraint shows its intensification.
% Initially a niche jargon (low extraction), it became a global corporate standard
% (high extraction) over the interval. Theater remains low as it is highly functional.

% Theater ratio over time (metric_substitution detection):
narrative_ontology:measurement(slo_tr_t0, silicon_lexicon_overload, theater_ratio, 0, 0.05).
narrative_ontology:measurement(slo_tr_t5, silicon_lexicon_overload, theater_ratio, 5, 0.10).
narrative_ontology:measurement(slo_tr_t10, silicon_lexicon_overload, theater_ratio, 10, 0.10).

% Extraction over time (extraction_accumulation detection):
narrative_ontology:measurement(slo_ex_t0, silicon_lexicon_overload, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(slo_ex_t5, silicon_lexicon_overload, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(slo_ex_t10, silicon_lexicon_overload, base_extractiveness, 10, 0.80).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The lexicon is a standard for information exchange.
narrative_ontology:coordination_type(silicon_lexicon_overload, information_standard).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */