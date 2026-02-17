% ============================================================================
% CONSTRAINT STORY: ancient_grudge_verona
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-22
% ============================================================================

:- module(constraint_ancient_grudge_verona, []).

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
 * * constraint_id: ancient_grudge_verona
 * human_readable: The Montague-Capulet Feud
 * domain: social/political
 * * SUMMARY:
 * An inherited, transgenerational conflict ("ancient grudge") that mandates
 * spontaneous violence between two noble houses in Renaissance Verona. This
 * constraint overrides civil law and personal desire, functioning as the
 * primary filter for all social interaction and ultimately extracting the
 * lives of the houses' heirs.
 * * KEY AGENTS:
 * - Romeo & Juliet: Subjects (Powerless), whose lives are entirely circumscribed by the feud.
 * - Tybalt & Lord Capulet: Beneficiaries (Powerful/Institutional), who enforce the feud to maintain household honor and identity.
 * - Prince Escalus & Friar Lawrence: Auditors (Institutional/Analytical), who attempt to manage or resolve the feud from outside its core logic.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(ancient_grudge_verona, 0.80). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(ancient_grudge_verona, 0.70).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(ancient_grudge_verona, 0.65).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(ancient_grudge_verona, extractiveness, 0.80).
narrative_ontology:constraint_metric(ancient_grudge_verona, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(ancient_grudge_verona, theater_ratio, 0.65).

% Constraint self-claim (what does the constraint claim to be?)
% Values: natural_law, coordination, constructed, enforcement
% The feud is presented by its participants as an unchangeable, natural fact of their world.
narrative_ontology:constraint_claim(ancient_grudge_verona, tangled_rope).
narrative_ontology:human_readable(ancient_grudge_verona, "The Montague-Capulet Feud").

% Binary flags
domain_priors:requires_active_enforcement(ancient_grudge_verona). % Required for Tangled Rope

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
% Both are required for Tangled Rope.
narrative_ontology:constraint_beneficiary(ancient_grudge_verona, veronese_noble_houses).
narrative_ontology:constraint_victim(ancient_grudge_verona, house_heirs).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (ROMEO & JULIET) - MOUNTAIN
% From their perspective, the feud is an immutable law of nature, a "star-crossed"
% fate they cannot escape. The high extraction feels absolute.
% χ = 0.80 * π(powerless:1.5) * σ(regional:0.9) = 1.08
constraint_indexing:constraint_classification(ancient_grudge_verona, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: THE ENFORCER (TYBALT) - TANGLED ROPE
% For an enforcer like Tybalt, the feud is a coordination mechanism for honor
% and identity (Rope aspect) that also requires violent enforcement and produces
% victims (Snare aspect). He is a willing participant.
% χ = 0.80 * π(powerful:0.6) * σ(local:0.8) = 0.384
constraint_indexing:constraint_classification(ancient_grudge_verona, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 3: THE SOVEREIGN (PRINCE ESCALUS) - SNARE
% From the perspective of the state, the feud is a purely destructive, coercive
% trap that undermines civil order and extracts the lives of his subjects. It has
% no redeeming coordination value for the city as a whole.
% χ = 0.80 * π(institutional:-0.2) * σ(regional:0.9) = -0.144 (a net cost)
constraint_indexing:constraint_classification(ancient_grudge_verona, snare,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER - TANGLED ROPE
% The analytical view recognizes both the genuine coordination function (household
% identity, honor) and the highly coercive, asymmetric extraction (death of heirs).
% This dual nature is the definition of a Tangled Rope.
% χ = 0.80 * π(analytical:1.15) * σ(global:1.2) = 1.104
constraint_indexing:constraint_classification(ancient_grudge_verona, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ancient_grudge_verona_tests).

test(perspectival_gap_subject_vs_enforcer) :-
    % Verify the gap between the powerless victims (Mountain) and powerful enforcers (Tangled Rope).
    constraint_indexing:constraint_classification(ancient_grudge_verona, mountain, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ancient_grudge_verona, tangled_rope, context(agent_power(powerful), _, _, _)),
    constraint_indexing:constraint_classification(ancient_grudge_verona, snare, context(agent_power(institutional), _, _, _)).

test(threshold_validation_high_extraction) :-
    % This constraint must be classified as high-extraction.
    narrative_ontology:constraint_metric(ancient_grudge_verona, extractiveness, E),
    E >= 0.46.

test(tangled_rope_structural_requirements) :-
    % Verify that the necessary structural properties for a Tangled Rope are declared.
    domain_priors:requires_active_enforcement(ancient_grudge_verona),
    narrative_ontology:constraint_beneficiary(ancient_grudge_verona, _),
    narrative_ontology:constraint_victim(ancient_grudge_verona, _).

:- end_tests(ancient_grudge_verona_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The base extractiveness of 0.80 represents the total capture of biological
 * surplus (the lives of the heirs) by the transgenerational feud. The high
 * suppression score of 0.70 reflects the lack of viable alternatives; any
 * attempt at peace or reconciliation is actively suppressed by enforcers like
 * Tybalt.
 *
 * The Perspectival Gap is profound:
 * - For Romeo and Juliet (powerless), the feud is an inescapable Mountain of fate.
 * - For Tybalt (powerful), it's a Tangled Rope of honor coordination and violent enforcement.
 * - For Prince Escalus (institutional), it's a Snare that destroys civic peace.
 *
 * This divergence prevents a monolithic classification and reveals the constraint's
 * complex social function.
 *
 * * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * The high extraction (0.80) could be misread as a pure Snare (Mandatrophy).
 * However, the Tangled Rope classification from the analytical and enforcer
 * perspectives correctly identifies that the feud possesses a genuine, albeit
 * destructive, coordination function: the maintenance of household identity and
 * honor. This coordination is what gives the constraint its stability and
 * prevents it from being a simple predatory trap. The system is predatory, but
 * its persistence is owed to its coordination role for the beneficiaries.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_ancient_grudge_verona_1,
    'Is the feud a Mountain of ingrained human tribalism or a constructed Snare that could have been dismantled by effective governance?',
    'Comparative historical analysis of other Renaissance city-states that successfully suppressed noble feuds.',
    'If Mountain, the tragedy is inevitable. If Snare, the tragedy is a failure of the Prince''s policy.',
    confidence_without_resolution(medium)
).

omega_variable(
    omega_ancient_grudge_verona_2,
    'Is the 0.80 extraction a functional necessity for household sovereignty in a weak state, or a performative choice for tribal ego?',
    'Audit of household resource allocation vs. peace mediation efforts in the historical record.',
    'If necessity, the feud has a stronger coordination claim. If performative, it is closer to a pure Snare.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(ancient_grudge_verona, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection (metric_substitution,
% extraction_accumulation) by providing measurements at multiple time points.
% Required for high-extraction constraints (base_extractiveness > 0.46).

% Theater ratio: Rising from functional household defense (0.20) to
% inertial "Honor Theater" (0.65) that mandates lethal brawls over trivial insults.
narrative_ontology:measurement(feud_tr_t0, ancient_grudge_verona, theater_ratio, 0, 0.20).
narrative_ontology:measurement(feud_tr_t5, ancient_grudge_verona, theater_ratio, 5, 0.45).
narrative_ontology:measurement(feud_tr_t10, ancient_grudge_verona, theater_ratio, 10, 0.65).

% Extraction: Progressive liquidation of the "fatal loins" (heirs) as the
% "new mutiny" escalates from civil brawls to total biological extraction.
narrative_ontology:measurement(feud_ex_t0, ancient_grudge_verona, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(feud_ex_t5, ancient_grudge_verona, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(feud_ex_t10, ancient_grudge_verona, base_extractiveness, 10, 0.80).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The feud functions as a mechanism to enforce tribal honor and identity.
narrative_ontology:coordination_type(ancient_grudge_verona, enforcement_mechanism).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */