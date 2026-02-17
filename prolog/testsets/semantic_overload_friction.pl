% ============================================================================
% CONSTRAINT STORY: semantic_overload_friction
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-23
% ============================================================================

:- module(constraint_semantic_overload_friction, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: semantic_overload_friction
 * human_readable: The Semantic Saturation Threshold
 * domain: technological/social
 * * SUMMARY:
 * This constraint represents the "Friction of Jargon." As a specialized
 * domain (e.g., law, tech, or academia) matures, its language becomes
 * increasingly dense. While this facilitates precise internal coordination
 * (Rope), it acts as a massive barrier to entry and a source of confusion
 * (Snare) for external subjects, effectively extracting cognitive labor or forcing
 * reliance on intermediaries.
 * * KEY AGENTS:
 * - The Layman: Subject (Powerless). Unable to decode the "Mandarin" language.
 * - The Professional Guild: Beneficiary (Institutional). Protects territory
 * through linguistic complexity.
 * - The LLM/Translator: Auditor (Analytical). Quantifies the information
 * density vs. comprehension gap.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% [RESOLVED MANDATROPHY]: Extraction (0.55) is high because the "Tax"
% is paid in cognitive labor, time, and the cost of hiring intermediaries.
% The system extracts compliance by making the rules unintelligible to those they govern.
domain_priors:base_extractiveness(semantic_overload_friction, 0.55).
domain_priors:suppression_score(semantic_overload_friction, 0.90).   % High: No alternative language exists to participate in the domain.
domain_priors:theater_ratio(semantic_overload_friction, 0.45).       % Moderate: Some jargon is functional; some is performative.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(semantic_overload_friction, extractiveness, 0.55).
narrative_ontology:constraint_metric(semantic_overload_friction, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(semantic_overload_friction, theater_ratio, 0.45).

% Constraint self-claim (what does the constraint claim to be?)
narrative_ontology:constraint_claim(semantic_overload_friction, tangled_rope).
narrative_ontology:human_readable(semantic_overload_friction, "The Semantic Saturation Threshold").

% Binary flags
domain_priors:requires_active_enforcement(semantic_overload_friction). % Enforcement is social/professional (e.g., peer review, certification).

% Structural property derivation hooks for Tangled Rope / Scaffold
narrative_ontology:constraint_beneficiary(semantic_overload_friction, professional_guild).
narrative_ontology:constraint_victim(semantic_overload_friction, layman).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE LAYMAN (SNARE)
% For the non-specialist, the language is a Snare. They are trapped by
% rules they cannot understand, requiring them to hire intermediaries.
constraint_indexing:constraint_classification(semantic_overload_friction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE GUILD MEMBER (ROPE)
% To the professional, the jargon is a Rope. It is a high-bandwidth
% coordination mechanism that allows for precision and shared identity.
constraint_indexing:constraint_classification(semantic_overload_friction, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE LINGUISTIC AUDITOR (TANGLED ROPE)
% Analytically, the system is a Tangled Rope. The very tool used for
% coordination (specialized terms) is the same tool used for extraction
% (gatekeeping).
constraint_indexing:constraint_classification(semantic_overload_friction, tangled_rope,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: THE REFORMER (SCAFFOLD)
% During a "Plain Language" reform, the complex terminology is treated
% as a Scaffold—a legacy support system to be dismantled over time.
constraint_indexing:constraint_classification(semantic_overload_friction, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))) :-
    narrative_ontology:has_sunset_clause(semantic_overload_friction).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(semantic_overload_friction_tests).

test(perspectival_gap) :-
    % Verify the tension between the insider (Rope) and the outsider (Snare).
    constraint_indexing:constraint_classification(semantic_overload_friction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(semantic_overload_friction, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional,
    TypePowerless == snare,
    TypeInstitutional == rope.

test(analytical_classification_is_tangled_rope) :-
    % Verify that high-extraction language is flagged as a Tangled Rope.
    constraint_indexing:constraint_classification(semantic_overload_friction, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_requirements_met) :-
    % Verify that all structural requirements for a Tangled Rope are declared.
    narrative_ontology:constraint_beneficiary(semantic_overload_friction, _),
    narrative_ontology:constraint_victim(semantic_overload_friction, _),
    domain_priors:requires_active_enforcement(semantic_overload_friction).

:- end_tests(semantic_overload_friction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * Semantic Overload is a "Hidden Snare." Because language is usually seen
 * as a public good (Rope), its use as a tool of exclusion (base ε = 0.55)
 * often goes unchallenged. The Perspectival Gap is stark: insiders view the
 * "precision" as the benefit, while outsiders experience the "obscurity" as
 * the cost, paying in cognitive load or fees to intermediaries. Suppression
 * is high (0.90) because there is no alternative lexicon to participate in
 * the professional domain.
 *
 * MANDATROPHY ANALYSIS:
 * The Tangled Rope classification is critical here. It prevents the system
 * from making a binary choice between "pure coordination" (Rope) and "pure
 * extraction" (Snare). It correctly identifies that the *same mechanism*
 * serves both functions simultaneously, which is the defining characteristic
 * of sophisticated gatekeeping. The temporal data shows this evolving from
 * a primarily coordinative tool to a more extractive one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_jargon_utility,
    'Is the linguistic complexity genuinely required for technical precision, or is it primarily performative gatekeeping?',
    'Information-theoretic analysis comparing semantic density against predictive accuracy or functional outcomes in the domain.',
    'If purely performative, the Tangled Rope classification degrades towards Piton as the theater_ratio increases.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing and temporal analysis.
narrative_ontology:interval(semantic_overload_friction, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% The constraint intensifies over time as a domain matures. Initially, new
% terms are functional (low extraction/theater). Later, they become layered
% with social meaning, increasing both performative aspects and the barrier to entry.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(sof_tr_t0, semantic_overload_friction, theater_ratio, 0, 0.10).
narrative_ontology:measurement(sof_tr_t5, semantic_overload_friction, theater_ratio, 5, 0.30).
narrative_ontology:measurement(sof_tr_t10, semantic_overload_friction, theater_ratio, 10, 0.45).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(sof_ex_t0, semantic_overload_friction, base_extractiveness, 0, 0.20).
narrative_ontology:measurement(sof_ex_t5, semantic_overload_friction, base_extractiveness, 5, 0.40).
narrative_ontology:measurement(sof_ex_t10, semantic_overload_friction, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Jargon is a specialized information standard for a sub-group.
narrative_ontology:coordination_type(semantic_overload_friction, information_standard).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */