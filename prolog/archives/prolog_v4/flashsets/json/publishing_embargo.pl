% ============================================================================
% CONSTRAINT STORY: publishing_embargo
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_publishing_embargo, []).

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
 *   constraint_id: publishing_embargo
 *   human_readable: Academic Publishing Embargo
 *   domain: social
 *
 * SUMMARY:
 *   Academic publishing embargoes restrict researchers from publicly sharing
 *   their findings before the official publication date in a peer-reviewed
 *   journal. This system has both benefits and drawbacks, creating a tangled
 *   rope constraint. It aims to incentivize high-quality research and provide
 *   a period of exclusivity for journal publishers, but it also delays the
 *   dissemination of knowledge and can disadvantage early-career researchers.
 *
 * KEY AGENTS:
 *   - Journal Publishers: Beneficiaries of the exclusivity period for subscription revenue.
 *   - Senior Researchers: Beneficiaries who control timing and prestige of publications.
 *   - Early Career Researchers: Victims who need to publish to advance careers.
 *   - Knowledge Seekers: Victims of delayed information access.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(publishing_embargo, 0.5).
domain_priors:suppression_score(publishing_embargo, 0.6).
domain_priors:theater_ratio(publishing_embargo, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(publishing_embargo, extractiveness, 0.5).
narrative_ontology:constraint_metric(publishing_embargo, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(publishing_embargo, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(publishing_embargo, tangled_rope).
narrative_ontology:human_readable(publishing_embargo, "Academic Publishing Embargo").
narrative_ontology:topic_domain(publishing_embargo, "social").

domain_priors:requires_active_enforcement(publishing_embargo).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(publishing_embargo, journal_publishers).
narrative_ontology:constraint_beneficiary(publishing_embargo, senior_researchers).
narrative_ontology:constraint_victim(publishing_embargo, early_career_researchers).
narrative_ontology:constraint_victim(publishing_embargo, knowledge_seekers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Early career researchers are often trapped by the need to publish in high-impact journals to advance their careers, making them vulnerable to the embargo. They bear the cost of delayed dissemination of their work.
constraint_indexing:constraint_classification(publishing_embargo, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(global))).

% Senior researchers benefit from the embargo system as it allows them to maintain control over the dissemination of their research findings, enhancing their reputation and influence. However, they are also constrained by the system, as they must adhere to the embargo rules.
constraint_indexing:constraint_classification(publishing_embargo, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Journal publishers benefit from the embargo system as it allows them to maintain exclusivity over the research findings, which drives subscriptions and advertising revenue. They have arbitrage options through alternative publishing models.
constraint_indexing:constraint_classification(publishing_embargo, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Knowledge seekers (students, independent researchers, the informed public) are often unaware of and cannot avoid the effects of publication embargoes, especially in fields like medicine where important research is delayed from reaching them.
constraint_indexing:constraint_classification(publishing_embargo, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Analytical observers may see the embargo as a tangled rope, serving some coordination function for quality control and revenue generation while simultaneously extracting from researchers and the public by delaying access to information.
constraint_indexing:constraint_classification(publishing_embargo, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(publishing_embargo_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(publishing_embargo, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(publishing_embargo, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(publishing_embargo, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(publishing_embargo_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate because while the embargo does extract from the public and researchers by delaying access, it also serves to incentivize publication and quality control. Suppression is high because there are limited alternatives for researchers to share findings quickly without jeopardizing publication.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap exists because journal publishers and some senior researchers benefit from the embargo, which drives subscriptions and prestige. However, early-career researchers and the public are harmed by the delay in access to information.
 *
 * DIRECTIONALITY LOGIC:
 *   Journal publishers are beneficiaries and have arbitrage exit options through different business models. Senior researchers are beneficiaries and are constrained by norms. Early career researchers are victims trapped by career requirements. Knowledge seekers are victims trapped by lack of direct access.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    open_access_mandates_impact,
    'To what extent will open access mandates diminish the power of publishing embargoes?',
    'Track the adoption rates of open access policies and their impact on journal subscriptions and research dissemination timelines.',
    'If open access prevails, embargoes may weaken, shifting the system towards a rope or scaffold. If not, the tangled rope/snare persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(open_access_mandates_impact, empirical, 'Impact of open access mandates on publishing embargoes.').

omega_variable(
    peer_review_alternatives,
    'Are there viable alternative peer review models that do not rely on embargoes for quality control and exclusivity?',
    'Evaluate the effectiveness of pre-print servers and post-publication peer review systems.',
    'Successful alternatives could transform the embargo from a tangled rope/snare into a piton or scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(peer_review_alternatives, conceptual, 'Viability of alternative peer review models.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(publishing_embargo, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(publ_tr_t0, publishing_embargo, theater_ratio, 0, 0.2).
narrative_ontology:measurement(publ_tr_t5, publishing_embargo, theater_ratio, 5, 0.3).
narrative_ontology:measurement(publ_tr_t10, publishing_embargo, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(publ_be_t0, publishing_embargo, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(publ_be_t5, publishing_embargo, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(publ_be_t10, publishing_embargo, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(publishing_embargo, information_standard).
narrative_ontology:affects_constraint(publishing_embargo, peer_review_process).
narrative_ontology:affects_constraint(publishing_embargo, research_funding_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
