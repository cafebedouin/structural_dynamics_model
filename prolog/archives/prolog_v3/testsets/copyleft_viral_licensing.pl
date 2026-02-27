% ============================================================================
% CONSTRAINT STORY: copyleft_viral_licensing
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% ============================================================================

:- module(constraint_copyleft_viral_licensing, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: copyleft_viral_licensing
 *   human_readable: Copyleft Viral Licensing (e.g., GPL)
 *   domain: technological/legal
 *
 * SUMMARY:
 *   Copyleft is a legal mechanism using copyright law to ensure a work and its
 *   derivatives remain free (as in freedom). Unlike permissive licenses, it
 *   requires that any modified versions of the software must also be licensed
 *   under the same reciprocal terms. This "viral" or "share-alike" property
 *   creates a self-expanding commons of free software.
 *
 * KEY AGENTS (by structural relationship):
 *   - Proprietary Software Vendors: Primary target (institutional/trapped) — bears the "cost" of forced transparency if they incorporate copyleft code.
 *   - Open Source Developer Community: Primary beneficiary (moderate/mobile) — gains access to a growing commons of software and the guarantee their contributions won't be privatized.
 *   - Free Software Foundations: Architect/Enforcer (organized/constrained) — actively enforces the license terms to protect the commons.
 *   - Analytical Observer: Sees the full structure as a coordination mechanism.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(copyleft_viral_licensing, 0.05).
domain_priors:suppression_score(copyleft_viral_licensing, 0.20).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(copyleft_viral_licensing, 0.10).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(copyleft_viral_licensing, extractiveness, 0.05).
narrative_ontology:constraint_metric(copyleft_viral_licensing, suppression_requirement, 0.20).
narrative_ontology:constraint_metric(copyleft_viral_licensing, theater_ratio, 0.10).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(copyleft_viral_licensing, rope).
narrative_ontology:human_readable(copyleft_viral_licensing, "Copyleft Viral Licensing (e.g., GPL)").
narrative_ontology:topic_domain(copyleft_viral_licensing, "technological/legal").

% --- Binary flags ---
domain_priors:requires_active_enforcement(copyleft_viral_licensing).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(copyleft_viral_licensing, open_source_developer_community).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(copyleft_viral_licensing, proprietary_software_vendors).

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

% PERSPECTIVE 1: THE PROPRIETARY DEVELOPER (THE "VICTIM")
% This agent perceives the license as a threat. While narratively a "Snare",
% its structural metrics (ε=0.05, suppression=0.20) do not meet the Snare
% thresholds. It is a Rope, but a costly one from this perspective. The
% engine derives a high d value (d≈0.95) from victim status + trapped exit,
% leading to a positive χ, but one that is still too low for a Snare classification.
constraint_indexing:constraint_classification(copyleft_viral_licensing, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE OPEN SOURCE DEVELOPER (THE "BENEFICIARY")
% For a contributor to the commons, Copyleft is a pure Rope. It coordinates
% mass collaboration by removing the fear of "Proprietary Capture". The
% engine derives a low d value from beneficiary status + mobile exit,
% leading to a low/negative χ, representing a net subsidy.
constraint_indexing:constraint_classification(copyleft_viral_licensing, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE END USER
% For a non-developer end user, the license is an invisible, enabling Rope.
% It provides access to powerful software at zero monetary cost. As a
% beneficiary with limited ability to change the terms, they experience it
% as a stable coordination mechanism.
constraint_indexing:constraint_classification(copyleft_viral_licensing, rope,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER
% The analyst sees the full structure: a low-extraction, moderate-suppression
% mechanism that uses legal enforcement to create a coordination commons.
% The classification is unambiguously Rope, aligning with the constraint_claim.
constraint_indexing:constraint_classification(copyleft_viral_licensing, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(copyleft_viral_licensing_tests).

test(uniform_classification_is_rope) :-
    % Verify that despite different perspectives, the structural classification
    % is uniformly Rope due to its low base extractiveness.
    constraint_indexing:constraint_classification(copyleft_viral_licensing, TypeInstitutional,
        context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(copyleft_viral_licensing, TypeModerate,
        context(agent_power(moderate), _, _, _)),
    constraint_indexing:constraint_classification(copyleft_viral_licensing, TypeAnalytical,
        context(agent_power(analytical), _, _, _)),
    TypeInstitutional == rope,
    TypeModerate == rope,
    TypeAnalytical == rope.

test(claim_matches_analytical_view) :-
    % The system's overall claim must match the analytical perspective.
    narrative_ontology:constraint_claim(copyleft_viral_licensing, ClaimedType),
    constraint_indexing:constraint_classification(copyleft_viral_licensing, AnalyticalType,
        context(agent_power(analytical), _, _, _)),
    ClaimedType == AnalyticalType.

:- end_tests(copyleft_viral_licensing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.05): Extremely low. The constraint's purpose is to
 *     prevent extraction (privatization of a commons), not enable it. The only
 *     "extraction" is the reciprocal requirement to share back, which is the
 *     core coordination function.
 *   - Suppression (0.20): Low. The license terms are explicit and public. While
 *     corporations may internally suppress its use to avoid legal obligations,
 *     the constraint itself does not suppress alternatives like permissive
 *     licenses; they coexist.
 *   - Theater Ratio (0.10): Low. The license is actively and functionally
 *     enforced by entities like the Free Software Foundation. Its purpose is
 *     instrumental, not performative.
 *
 * PERSPECTIVAL GAP:
 *   The key insight is that the perspectival gap is NOT in the classification
 *   type (it is a Rope from all perspectives) but in the effective
 *   extractiveness (χ). For a beneficiary (open source developer), χ is low or
 *   negative. For a "victim" (proprietary vendor), victim status and trapped
 *   exit options derive a high directionality (d), making χ positive and
 *   non-trivial. They experience it as a costly, coercive Rope, which they
 *   narratively frame as a "Snare", even though it does not meet the
 *   structural requirements for a Snare (ε ≥ 0.46, suppression ≥ 0.60).
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `open_source_developer_community`. They receive the benefit
 *     of a guaranteed commons and protection against their work being privatized.
 *     This derives a low `d` value.
 *   - Victim: `proprietary_software_vendors`. They bear the cost of being forced
 *     to release their own source code if they incorporate copylefted work.
 *     This derives a high `d` value.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a classic example of using a legal system (copyright,
 *   which can be a Snare) to enforce a coordination mechanism (a Rope). The
 *   low ε score correctly identifies its primary function as coordination,
 *   preventing the "viral" enforcement mechanism from being misclassified as
 *   pure extraction. The analysis correctly separates the perceived cost for
 *   one group from the overall structural function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% The primary uncertainty revolves around the long-term efficacy of copyleft
% in the age of cloud computing and Software-as-a-Service (SaaS), where code
% execution is centralized and distribution (the trigger for GPL) may not occur.

omega_variable(
    omega_copyleft_saas_loophole,
    'Will the "SaaS Loophole" render traditional copyleft (like GPL) ineffective for cloud services, and will stronger versions (like AGPL) become the new standard?',
    'Track adoption rates of AGPL vs. GPL in new major open source projects and monitor legal precedents related to software interaction over networks.',
    'If AGPL becomes standard, the Rope''s coordination extends to the cloud. If not, the commons may be effectively privatized via SaaS wrappers, turning the original Rope into a Piton for server-side software.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(omega_copyleft_saas_loophole, empirical, 'The future efficacy of copyleft licenses in a SaaS-dominated software ecosystem.').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(copyleft_viral_licensing, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Copyleft licensing has been a stable legal mechanism since its inception.
% The metrics are modeled as flat, showing no significant drift over the
% primary interval of its relevance (post-1985).
% Base extractiveness is low, so this section is for completeness.

% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(copyleft_viral_licensing_tr_t0, copyleft_viral_licensing, theater_ratio, 0, 0.10).
narrative_ontology:measurement(copyleft_viral_licensing_tr_t5, copyleft_viral_licensing, theater_ratio, 5, 0.10).
narrative_ontology:measurement(copyleft_viral_licensing_tr_t10, copyleft_viral_licensing, theater_ratio, 10, 0.10).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(copyleft_viral_licensing_ex_t0, copyleft_viral_licensing, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(copyleft_viral_licensing_ex_t5, copyleft_viral_licensing, base_extractiveness, 5, 0.05).
narrative_ontology:measurement(copyleft_viral_licensing_ex_t10, copyleft_viral_licensing, base_extractiveness, 10, 0.05).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Copyleft is a legal framework that enforces a coordination scheme.
narrative_ontology:coordination_type(copyleft_viral_licensing, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this constraint. The structural derivation
% from beneficiary/victim groups and exit options correctly captures the
% directionality for all key agents.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */