% ============================================================================
% CONSTRAINT STORY: academic_peer_review_gatekeeping_u4_exp_r2
% ============================================================================
% Version: 7.1 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-16
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_academic_peer_review_gatekeeping_u4_exp_r2, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: academic_peer_review_gatekeeping_u4_exp_r2
 *   human_readable: Academic Peer Review and Journal Gatekeeping
 *   domain: economic/social/technological
 *
 * SUMMARY:
 *   The academic publishing system operates by having researchers, typically
 *   funded by public or university grants, provide their research articles,
 *   peer review labor, and editorial services to publishers for free. These
 *   publishers, often for-profit corporations, then sell subscriptions to the
 *   compiled journals back to university libraries at extremely high costs,
 *   effectively making the academic community pay to access its own work. The
 *   system is maintained by the 'publish or perish' culture of academia,
 *   where publication in high-prestige journals is essential for career
 *   advancement (tenure, grants).
 *
 * KEY AGENTS:
 *   - Junior Professors: Primary victims (powerless/trapped) - Must provide free labor and generate articles to secure a career.
 *   - For-Profit Journal Publishers: Primary beneficiaries (institutional/arbitrage) - Capture immense value from free labor and monopolistic pricing.
 *   - University Libraries/Consortia: Constrained victims (organized/constrained) - Forced to pay exorbitant fees to provide essential resources for their researchers.
 *   - Senior Academics: Enforcers/Beneficiaries (powerful/mobile) - Act as editors and reviewers, upholding the prestige system from which they benefit.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(academic_peer_review_gatekeeping_u4_exp_r2, 0.68).
domain_priors:suppression_score(academic_peer_review_gatekeeping_u4_exp_r2, 0.72).
domain_priors:theater_ratio(academic_peer_review_gatekeeping_u4_exp_r2, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u4_exp_r2, extractiveness, 0.68).
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u4_exp_r2, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u4_exp_r2, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(academic_peer_review_gatekeeping_u4_exp_r2, tangled_rope).
narrative_ontology:human_readable(academic_peer_review_gatekeeping_u4_exp_r2, "Academic Peer Review and Journal Gatekeeping").
narrative_ontology:topic_domain(academic_peer_review_gatekeeping_u4_exp_r2, "economic/social/technological").

domain_priors:requires_active_enforcement(academic_peer_review_gatekeeping_u4_exp_r2).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(academic_peer_review_gatekeeping_u4_exp_r2, for_profit_journal_publishers).
narrative_ontology:constraint_beneficiary(academic_peer_review_gatekeeping_u4_exp_r2, senior_academics_as_editors).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u4_exp_r2, junior_professors).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u4_exp_r2, university_libraries).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u4_exp_r2, the_general_public).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% For the junior academic, the 'publish or perish' mandate makes this a coercive system with no viable exit, classifying it as a Snare.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u4_exp_r2, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% From the publisher's perspective, this is a highly efficient Rope that coordinates free expert labor to create a high-margin product.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u4_exp_r2, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Library consortia see both the coordination function (access to vetted research) and the severe extraction (unsustainable subscription costs), classifying it as a Tangled Rope.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u4_exp_r2, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% The system possesses a genuine coordination function (peer review) but is dominated by asymmetric extraction, making it a canonical Tangled Rope.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u4_exp_r2, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(academic_peer_review_gatekeeping_u4_exp_r2_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u4_exp_r2, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u4_exp_r2, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(academic_peer_review_gatekeeping_u4_exp_r2, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(academic_peer_review_gatekeeping_u4_exp_r2_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (ε=0.68) is high, reflecting the direct conversion of free, expert labor into profit. The suppression score (0.72) is also high because alternatives like pre-print servers or institutional repositories lack the 'prestige' required for career advancement, effectively locking researchers into the established system. The system requires active enforcement through tenure committees and funding bodies that use journal impact factor as a primary metric of success.
 *
 * PERSPECTIVAL GAP:
 *   A significant gap exists. The junior researcher, trapped by career incentives, experiences the system as a coercive Snare. The publisher, which profits from coordinating this activity, views it as a legitimate and efficient Rope. The library consortium, which must negotiate with the publisher, sees both the coordination function and the crippling extraction, identifying it as a Tangled Rope. This gap is the core of the system's durability.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is unambiguous. Value flows from researchers (labor) and their institutions (subscription fees) to the publishers. Publishers are the beneficiaries. Researchers, universities, and the public (who fund the research and are often denied access) are the victims. Senior academics who serve as editors gain prestige, acting as both beneficiaries of the system's status hierarchy and enforcers of its norms.
 *
 * MANDATROPHY ANALYSIS:
 *   The Tangled Rope classification is critical for avoiding mandatrophy. Labeling the system a pure Snare would ignore the genuine, if now overshadowed, coordination function of peer review in quality control. Labeling it a Rope, as publishers do, would be a gross misrepresentation that ignores the massive, non-consensual extraction. The Tangled Rope classification correctly identifies that a system with a valid coordination purpose has become predominantly extractive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    quality_vs_rent_seeking,
    'Is the high cost of journal access a necessary byproduct of ensuring high-quality peer review and editorial standards, or is it primarily rent-seeking behavior enabled by a captured market?',
    'Comparative analysis of operational costs and profit margins between for-profit publishers and non-profit/open-access platforms with equivalent quality metrics (e.g., citation rates, retraction rates).',
    'If necessary byproduct, re-classifies towards Rope and lowers ε. If rent-seeking, solidifies Snare/Tangled Rope classification and increases ε.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quality_vs_rent_seeking, empirical, 'Distinguishing between necessary quality control costs and monopolistic rent-seeking.').

omega_variable(
    prestige_decoupling,
    'Can the prestige signal of top-tier journals be decoupled from the for-profit publishing model and attached to alternative, open-access platforms?',
    'Tracking the adoption of alternative metrics (e.g., DORA) by tenure committees and funding agencies.',
    'If prestige can be decoupled, the suppression metric would decrease significantly, potentially allowing the constraint to be resolved into a Rope. If not, it remains a durable Tangled Rope or Snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(prestige_decoupling, conceptual, 'Whether the prestige signal can be separated from the extractive business model.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(academic_peer_review_gatekeeping_u4_exp_r2, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acad_tr_t1970, academic_peer_review_gatekeeping_u4_exp_r2, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(acad_tr_t1997, academic_peer_review_gatekeeping_u4_exp_r2, theater_ratio, 1997, 0.3).
narrative_ontology:measurement(acad_tr_t2024, academic_peer_review_gatekeeping_u4_exp_r2, theater_ratio, 2024, 0.45).

% Extraction over time
narrative_ontology:measurement(acad_be_t1970, academic_peer_review_gatekeeping_u4_exp_r2, base_extractiveness, 1970, 0.2).
narrative_ontology:measurement(acad_be_t1997, academic_peer_review_gatekeeping_u4_exp_r2, base_extractiveness, 1997, 0.45).
narrative_ontology:measurement(acad_be_t2024, academic_peer_review_gatekeeping_u4_exp_r2, base_extractiveness, 2024, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(academic_peer_review_gatekeeping_u4_exp_r2, information_standard).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping_u4_exp_r2, university_tenure_system).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping_u4_exp_r2, scientific_funding_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
