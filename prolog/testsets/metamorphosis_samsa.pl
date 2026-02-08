% ============================================================================
% CONSTRAINT STORY: metamorphosis_samsa
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-22
% ============================================================================

:- module(constraint_metamorphosis_samsa, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

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
 * * constraint_id: metamorphosis_samsa
 * human_readable: The Samsa Family's Debt Bondage
 * domain: economic/social/biological
 * * SUMMARY:
 * Gregor Samsa, a traveling salesman, wakes up transformed into a "horrible vermin."
 * This biological event makes it impossible for him to continue working, which was
 * the sole means of paying off his parents' debt to his employer. The constraint
 * is the totalizing nature of this debt servitude, where the failure of the
 * biological "machine" (Gregor's body) is treated as a contractual violation,
 * leading to his dehumanization and eventual disposal by his family.
 * * KEY AGENTS:
 * - Gregor Samsa: The subject, whose labor is extracted (Powerless).
 * - The Firm / Chief Clerk: The beneficiary of the labor and enforcer of the debt (Institutional).
 * - The Samsa Family: Initially victims alongside Gregor, they become beneficiaries of his removal.
 * - Analytical Observer: A systems auditor viewing the entire socio-economic arrangement.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(metamorphosis_samsa, 0.80). % High: Gregor's entire life force is extracted to service a debt he did not incur.
domain_priors:suppression_score(metamorphosis_samsa, 0.70).   % High: Alternatives (medical care, communication, empathy) are rapidly suppressed.
domain_priors:theater_ratio(metamorphosis_samsa, 0.10).       % Low: The family's actions are brutally functional, not performative.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(metamorphosis_samsa, extractiveness, 0.80).
narrative_ontology:constraint_metric(metamorphosis_samsa, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(metamorphosis_samsa, theater_ratio, 0.10).

% Constraint self-claim (what does the constraint claim to be?)
% The firm presents the debt as a standard business arrangement (coordination),
% but its function is pure coercive extraction (enforcement).
narrative_ontology:constraint_claim(metamorphosis_samsa, snare).

% Binary flags
domain_priors:requires_active_enforcement(metamorphosis_samsa). % The family must lock the door; the Clerk threatens legal action.

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(metamorphosis_samsa, the_firm).
narrative_ontology:constraint_beneficiary(metamorphosis_samsa, the_samsa_family_post_gregor).
narrative_ontology:constraint_victim(metamorphosis_samsa, gregor_samsa).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: GREGOR SAMSA (THE SUBJECT)
% For Gregor, the transformation is an absolute, unchangeable physical fact.
% It has zero degrees of freedom, making it a Mountain from his trapped perspective.
% His powerlessness (π=1.5) and local scope (σ=0.8) yield χ = 0.8 * 1.5 * 0.8 = 0.96.
constraint_indexing:constraint_classification(metamorphosis_samsa, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE CHIEF CLERK (THE BENEFICIARY/ENFORCER)
% The Clerk sees Gregor's state not as a biological tragedy but as a breach of
% a coercive labor contract. It is a Snare designed to extract labor, and
% Gregor's failure to comply is met with threats.
% Institutional power (π=-0.2) makes extraction feel negligible or even beneficial.
% χ = 0.8 * -0.2 * 1.0 = -0.16 (perceived as a net benefit/necessary cost).
% The system classifies as Snare based on base metrics, despite the negative chi.
constraint_indexing:constraint_classification(metamorphosis_samsa, snare,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% The analyst sees the entire structure: a high-extraction, high-suppression
% system that uses debt to enforce labor. The biological event is merely the
% catalyst that reveals the underlying Snare.
% Analytical power (π=1.15) and global scope (σ=1.2) yield χ = 0.8 * 1.15 * 1.2 = 1.104.
constraint_indexing:constraint_classification(metamorphosis_samsa, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(metamorphosis_samsa_tests).

test(perspectival_gap_mountain_vs_snare) :-
    % Verify the core Mandatrophy gap: the subject sees a Mountain, the observer sees a Snare.
    constraint_indexing:constraint_classification(metamorphosis_samsa, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(metamorphosis_samsa, TypeAnalytical, context(agent_power(analytical), _, _, _)),
    TypePowerless == mountain,
    TypeAnalytical == snare,
    TypePowerless \= TypeAnalytical.

test(high_extraction_and_suppression_scores) :-
    % Verify the base metrics meet the Snare threshold.
    domain_priors:base_extractiveness(metamorphosis_samsa, E),
    domain_priors:suppression_score(metamorphosis_samsa, S),
    E >= 0.46,
    S >= 0.60.

:- end_tests(metamorphosis_samsa_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The core of this story is the perspectival gap between an event experienced as
 * an immutable biological horror (Mountain) and one analyzed as the failure point
 * of a coercive economic system (Snare). The base extractiveness is set to 0.80
 * to reflect the totality of Gregor's servitude; his life has been completely
 * consumed by the debt. The suppression score of 0.70 reflects the family's and
 * firm's rapid abandonment of any alternative other than isolating and eventually
 * eliminating the non-productive "unit."
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * This file resolves the Mandatrophy problem by demonstrating that what appears
 * as a Mountain to the powerless subject (Gregor) is correctly identified as a
 * high-extraction Snare by the analytical observer. The system does not mistake
 * Gregor's inability to overcome his condition for the constraint being a natural
 * law. Instead, it correctly identifies the *socio-economic reaction* to his
 * condition as the constructed Snare. The biological change is the catalyst, but
 * the debt bondage is the constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_metamorphosis_samsa_1,
    'Is the transformation a literal biological event or a psychological/allegorical one?',
    'This is textually undecidable and a central point of literary debate. No in-narrative data can resolve it.',
    'If literal, the Mountain perspective is grounded in physics. If allegorical, the entire constraint is a social Snare manifesting physically.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(metamorphosis_samsa, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% The debt servitude was a long-standing condition. The story's interval
% begins with the transformation, but the metrics were already at a crisis point.
% We model them as consistently high throughout the narrative period.

% Theater ratio over time (consistently low and functional):
narrative_ontology:measurement(ms_tr_t0, metamorphosis_samsa, theater_ratio, 0, 0.10).
narrative_ontology:measurement(ms_tr_t5, metamorphosis_samsa, theater_ratio, 5, 0.10).
narrative_ontology:measurement(ms_tr_t10, metamorphosis_samsa, theater_ratio, 10, 0.10).

% Extraction over time (consistently high):
narrative_ontology:measurement(ms_ex_t0, metamorphosis_samsa, base_extractiveness, 0, 0.80).
narrative_ontology:measurement(ms_ex_t5, metamorphosis_samsa, base_extractiveness, 5, 0.80).
narrative_ontology:measurement(ms_ex_t10, metamorphosis_samsa, base_extractiveness, 10, 0.80).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% This constraint is primarily extractive and does not represent a complex
% coordination mechanism, so no Boltzmann data is declared.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */