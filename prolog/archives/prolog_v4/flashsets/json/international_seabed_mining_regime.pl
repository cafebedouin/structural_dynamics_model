% ============================================================================
% CONSTRAINT STORY: international_seabed_mining_regime
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_international_seabed_mining_regime, []).

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
 *   constraint_id: international_seabed_mining_regime
 *   human_readable: International Seabed Mining Regime
 *   domain: geopolitical
 *
 * SUMMARY:
 *   The International Seabed Mining Regime, overseen by the International
 *   Seabed Authority (ISA), aims to regulate the extraction of mineral
 *   resources from the international seabed. This regime attempts to balance
 *   the potential economic benefits of mining with the need to protect the
 *   fragile deep-sea environment. However, the regime faces challenges
 *   related to enforcement, environmental impact assessment, and equitable
 *   distribution of benefits.
 *
 * KEY AGENTS:
 *   - ISA Member States: Institutional actors with mixed incentives (revenue vs. environment)
 *   - Mining Contractors: Direct beneficiaries seeking resource extraction
 *   - Deep Sea Ecosystems: Victims directly impacted by mining activities
 *   - Future Generations: Victims who will bear the long-term environmental consequences
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(international_seabed_mining_regime, 0.6).
domain_priors:suppression_score(international_seabed_mining_regime, 0.4).
domain_priors:theater_ratio(international_seabed_mining_regime, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(international_seabed_mining_regime, extractiveness, 0.6).
narrative_ontology:constraint_metric(international_seabed_mining_regime, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(international_seabed_mining_regime, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(international_seabed_mining_regime, tangled_rope).
narrative_ontology:human_readable(international_seabed_mining_regime, "International Seabed Mining Regime").
narrative_ontology:topic_domain(international_seabed_mining_regime, "geopolitical").

domain_priors:requires_active_enforcement(international_seabed_mining_regime).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(international_seabed_mining_regime, isa_member_states).
narrative_ontology:constraint_beneficiary(international_seabed_mining_regime, mining_contractors).
narrative_ontology:constraint_victim(international_seabed_mining_regime, deep_sea_ecosystems).
narrative_ontology:constraint_victim(international_seabed_mining_regime, future_generations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective 1: Deep Sea Ecosystems (Snare) - Unable to represent themselves or exit the situation. The ecosystems are directly impacted by the mining activities and have no means of recourse. They bear the full cost of the regime.
constraint_indexing:constraint_classification(international_seabed_mining_regime, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Perspective 2: ISA Member States (Tangled Rope) - Benefit from potential revenue sharing and asserted resource management, but are also constrained by the ISA's regulatory framework and reputational risk. They benefit from resource extraction but also bear costs of environmental damage. Exit is constrained due to treaty obligations but member states have influence through representation.
constraint_indexing:constraint_classification(international_seabed_mining_regime, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% Perspective 3: Mining Contractors (Tangled Rope) - Benefit directly from resource extraction activities authorized by the ISA, but are also subject to the ISA's regulations and potential liabilities. They benefit economically but face regulatory constraints. Mobile due to the potential to shift operations to different jurisdictions or resources.
constraint_indexing:constraint_classification(international_seabed_mining_regime, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(global))).

% Perspective 4: International Law of the Sea (Piton) - The overarching legal framework is in place, but its effectiveness is degraded by the lack of fully defined regulations and enforcement mechanisms for seabed mining. It aims to manage resources but operates with limited functional oversight, resulting in theatrical compliance.
constraint_indexing:constraint_classification(international_seabed_mining_regime, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% Perspective 5: Analytical Observer (Tangled Rope) - Sees the entire system as a mix of coordination and extraction. The ISA aims to coordinate resource management, but also enables extraction that causes environmental damage and benefits certain actors more than others. Exit through analysis but no direct influence.
constraint_indexing:constraint_classification(international_seabed_mining_regime, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% Perspective 6: Future Generations (Snare) - Cannot participate in current decision-making and will bear the long-term environmental consequences of seabed mining. They have no agency and cannot exit the situation.
constraint_indexing:constraint_classification(international_seabed_mining_regime, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(international_seabed_mining_regime_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(international_seabed_mining_regime, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(international_seabed_mining_regime, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(international_seabed_mining_regime, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(international_seabed_mining_regime, TR),
    TR >= 0.70.

:- end_tests(international_seabed_mining_regime_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.60): High. The regime enables extraction of seabed minerals, which benefits mining contractors and potentially ISA member states. The deep-sea ecosystems and future generations bear the environmental costs. Suppression (0.40): Moderate. The ISA's regulatory framework and enforcement mechanisms limit mining activities to some extent, but they are not fully effective in preventing environmental damage. The power asymmetry between member states contributes to medium suppression. Theater Ratio (0.75): High. The regime involves performative activities, such as environmental impact assessments and stakeholder consultations, and a substantial focus on the functional aspects of resource extraction. The increase over time reflects the growing awareness of environmental concerns and the need for more visible efforts to address them.
 *
 * PERSPECTIVAL GAP:
 *   Different stakeholders perceive the regime differently. Mining contractors see it as a means to access valuable resources, while environmental groups view it as a threat to the fragile deep-sea ecosystem. ISA member states face a balancing act between economic benefits and environmental protection. The analytical observer recognizes the inherent tensions and trade-offs within the regime.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (mining contractors, ISA member states) experience lower effective extraction because they benefit from the regime. Victims (deep-sea ecosystems, future generations) experience higher effective extraction because they bear the environmental costs. The ISA member states, while benefiting from revenue, are also constrained by the need to comply with the regime's regulations, impacting their d value. The engine computes 'd' from beneficiary/victim + exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The system is classified as Tangled Rope because it exhibits both coordination and extraction. The ISA aims to coordinate resource management (coordination), but the regime also enables environmental damage and benefits certain actors more than others (extraction). The mandatrophy is resolved by recognizing the mixed nature of the regime and the different perspectives of stakeholders.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    environmental_impact_threshold,
    'What level of environmental damage is acceptable in exchange for access to seabed minerals?',
    'Scientific research on the long-term effects of seabed mining on deep-sea ecosystems.',
    'If the threshold is high, more mining will occur with greater environmental damage. If the threshold is low, less mining will occur but access to needed minerals may be limited.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(environmental_impact_threshold, empirical, 'Acceptable level of environmental damage.').

omega_variable(
    enforcement_mechanism_effectiveness,
    'How effective are the ISA''s enforcement mechanisms in ensuring compliance with environmental regulations?',
    'Audits and inspections of mining operations, tracking of compliance records, and independent assessments of environmental monitoring data.',
    'If enforcement is effective, environmental damage will be minimized. If enforcement is weak, greater environmental damage is likely to occur.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_mechanism_effectiveness, empirical, 'Effectiveness of ISA''s enforcement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(international_seabed_mining_regime, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inte_tr_t0, international_seabed_mining_regime, theater_ratio, 0, 0.2).
narrative_ontology:measurement(inte_tr_t10, international_seabed_mining_regime, theater_ratio, 10, 0.5).
narrative_ontology:measurement(inte_tr_t20, international_seabed_mining_regime, theater_ratio, 20, 0.75).

% Extraction over time
narrative_ontology:measurement(inte_be_t0, international_seabed_mining_regime, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(inte_be_t10, international_seabed_mining_regime, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(inte_be_t20, international_seabed_mining_regime, base_extractiveness, 20, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(international_seabed_mining_regime, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
