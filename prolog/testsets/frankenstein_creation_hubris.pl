% ============================================================================
% CONSTRAINT STORY: frankenstein_creation_hubris
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_frankenstein_creation_hubris, []).

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
 * * constraint_id: frankenstein_creation_hubris
 * human_readable: The Creator's Burden
 * domain: technological/social
 * * SUMMARY:
 * Victor Frankenstein animates a sentient being but immediately abandons it
 * out of horror. The constraint is the irreversible moral and physical
 * obligation the creator has to the creation. Victor's rejection of this
 * bond triggers a catastrophic cycle of vengeance that extracts his life,
 * sanity, and loved ones, while trapping the Creature in a state of social
 * damnation.
 * * KEY AGENTS:
 * - The Creature: Subject (Powerless)
 * - The Scientific Community: Beneficiary (Institutional)
 * - Victor Frankenstein: A powerful agent trapped by his own creation.
 * - The Analytical Observer: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
% The creation extracts the life, sanity, and loved ones of the creator, and
% extracts any possibility of social existence from the creature.
domain_priors:base_extractiveness(frankenstein_creation_hubris, 0.90).
% The secret of the creation is suppressed by Victor’s ego and fear,
% preventing any intervention or alternative resolution.
domain_priors:suppression_score(frankenstein_creation_hubris, 0.85).
% The actions are functional and consequential, not performative.
domain_priors:theater_ratio(frankenstein_creation_hubris, 0.10).

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(frankenstein_creation_hubris, extractiveness, 0.90).
narrative_ontology:constraint_metric(frankenstein_creation_hubris, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(frankenstein_creation_hubris, theater_ratio, 0.10).

% Constraint self-claim (what does the constraint claim to be?)
% Victor's act is framed as a constructed effort for scientific progress.
narrative_ontology:constraint_claim(frankenstein_creation_hubris, tangled_rope).

% Binary flags
% The constraint is enforced by the Creature's physical capabilities and
% Victor's psychological guilt.
domain_priors:requires_active_enforcement(frankenstein_creation_hubris).

% Structural property derivation hooks:
% The story, as a cautionary tale, benefits the coordination of scientific ethics.
narrative_ontology:constraint_beneficiary(frankenstein_creation_hubris, scientific_community).
% The direct victims are Victor, his family, and the Creature itself.
narrative_ontology:constraint_victim(frankenstein_creation_hubris, frankenstein_lineage).
narrative_ontology:constraint_victim(frankenstein_creation_hubris, the_creature).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE CREATURE (MOUNTAIN)
% For the Creature, social rejection based on his appearance is an immutable
% law of nature. He cannot negotiate it or escape it. It is a fixed reality.
constraint_indexing:constraint_classification(frankenstein_creation_hubris, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE SCIENTIFIC COMMUNITY (ROPE)
% As a beneficiary, the scientific community uses the cautionary tale as a
% 'Rope' to coordinate ethical behavior and establish research boundaries.
% The tragedy is repurposed into a functional tool for social good.
constraint_indexing:constraint_classification(frankenstein_creation_hubris, rope,
    context(agent_power(institutional),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Analytically, the act of creation has a coordination function (advancing
% science) but also involves immense asymmetric extraction (destroying its
% creator and victims) and requires active enforcement (the Creature's revenge).
% This hybrid nature defines it as a Tangled Rope.
constraint_indexing:constraint_classification(frankenstein_creation_hubris, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: VICTOR FRANKENSTEIN (SNARE)
% For Victor, his creation becomes a 'Snare'. It is a coercive trap of his
% own making that systematically extracts his peace, his loved ones, and
% ultimately his life, with no possibility of escape.
constraint_indexing:constraint_classification(frankenstein_creation_hubris, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(frankenstein_creation_hubris_tests).

test(perspectival_gap_subject_vs_beneficiary) :-
    % Verify the gap between the Creature (powerless) and the Scientific Community (institutional).
    constraint_indexing:constraint_classification(frankenstein_creation_hubris, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(frankenstein_creation_hubris, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    assertion(TypePowerless == mountain),
    assertion(TypeInstitutional == rope),
    assertion(TypePowerless \= TypeInstitutional).

test(analytical_classification_is_tangled_rope) :-
    % Verify the analytical observer correctly identifies the hybrid nature.
    constraint_indexing:constraint_classification(frankenstein_creation_hubris, TypeAnalytical, context(agent_power(analytical), _, _, _)),
    assertion(TypeAnalytical == tangled_rope).

test(high_extraction_and_suppression) :-
    % Verify the base metrics meet the Snare/Tangled Rope thresholds.
    domain_priors:base_extractiveness(frankenstein_creation_hubris, E),
    domain_priors:suppression_score(frankenstein_creation_hubris, S),
    assertion(E >= 0.46),
    assertion(S >= 0.40).

:- end_tests(frankenstein_creation_hubris_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE: [RESOLVED MANDATROPHY]
 * The core of this story is the profound perspectival gap between the agents.
 * The Creature experiences its existence as a 'Mountain' of immutable social law.
 * The scientific community, benefiting from the lesson, sees it as a 'Rope' for
 * ethical coordination. Victor, the architect, is caught in a 'Snare' of his
 * own making.
 *
 * * MANDATROPHY ANALYSIS:
 * An analysis that only saw the extraction would label this a pure Snare. The
 * Tangled Rope classification from the analytical perspective is crucial. It
 * acknowledges that even catastrophic acts of hubris can be co-opted by
 * institutions to serve a genuine, albeit post-hoc, coordination function
 * (in this case, establishing ethical boundaries for future science). This
 * prevents misclassifying the entire structure as purely extractive and captures
 * its complex, hybrid nature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_frankenstein_creation_hubris,
    'Is the Creature''s tragedy one of inherent nature (an inescapable Mountain of biology) or failed nurture (a Snare of social abandonment)?',
    'Comparative analysis of the text against modern developmental psychology models on attachment and social ostracization.',
    'If nature: The tragedy is deterministic and inevitable. If nurture: The tragedy is a result of moral failure and was avoidable.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(frankenstein_creation_hubris, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% The constraint's extraction intensifies as the Creature's quest for
% vengeance escalates from abandonment to murder. Theater remains low.
%
% Theater ratio over time (metric_substitution detection):
narrative_ontology:measurement(frankenstein_creation_hubris_tr_t0, frankenstein_creation_hubris, theater_ratio, 0, 0.10).
narrative_ontology:measurement(frankenstein_creation_hubris_tr_t5, frankenstein_creation_hubris, theater_ratio, 5, 0.10).
narrative_ontology:measurement(frankenstein_creation_hubris_tr_t10, frankenstein_creation_hubris, theater_ratio, 10, 0.10).

% Extraction over time (extraction_accumulation detection):
% T=0: The moment of creation/abandonment. Extraction is latent but high.
narrative_ontology:measurement(frankenstein_creation_hubris_ex_t0, frankenstein_creation_hubris, base_extractiveness, 0, 0.50).
% T=5: After the murder of William. The cycle of revenge is active.
narrative_ontology:measurement(frankenstein_creation_hubris_ex_t5, frankenstein_creation_hubris, base_extractiveness, 5, 0.80).
% T=10: The final confrontation in the Arctic. Extraction is total.
narrative_ontology:measurement(frankenstein_creation_hubris_ex_t10, frankenstein_creation_hubris, base_extractiveness, 10, 0.90).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The story functions as an enforcement mechanism for an ethical boundary.
narrative_ontology:coordination_type(frankenstein_creation_hubris, enforcement_mechanism).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */