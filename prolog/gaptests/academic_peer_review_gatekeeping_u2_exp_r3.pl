% ============================================================================
% CONSTRAINT STORY: academic_peer_review_gatekeeping_u2_exp_r3
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_academic_peer_review_gatekeeping_u2_exp_r3, []).

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
 *   constraint_id: academic_peer_review_gatekeeping_u2_exp_r3
 *   human_readable: Academic Peer Review and Journal Gatekeeping
 *   domain: economic/social/technological
 *
 * SUMMARY:
 *   The academic publishing model functions as a system of gatekeeping where
 *   researchers, primarily at public institutions, provide free labor
 *   (research, writing, peer review) to for-profit publishers. These
 *   publishers then erect paywalls and sell access to this collective work
 *   back to the same institutions at extremely high prices. The system is
 *   perpetuated by the 'publish or perish' culture, where career advancement
 *   is tied to publication in a small number of 'high-impact' journals
 *   controlled by these publishers.
 *
 * KEY AGENTS:
 *   - Junior Professors: Primary victims (powerless/trapped) - must participate to gain tenure.
 *   - For-Profit Journal Publishers: Primary beneficiaries (institutional/arbitrage) - capture immense value from free labor and subscription fees.
 *   - Research Institutions / Library Consortia: Secondary victims and analytical observers (organized/constrained) - forced to pay escalating costs while also enforcing the system's norms.
 *   - General Public: Tertiary victims (powerless/trapped) - denied access to publicly funded research.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(academic_peer_review_gatekeeping_u2_exp_r3, 0.62).
domain_priors:suppression_score(academic_peer_review_gatekeeping_u2_exp_r3, 0.75).
domain_priors:theater_ratio(academic_peer_review_gatekeeping_u2_exp_r3, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u2_exp_r3, extractiveness, 0.62).
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u2_exp_r3, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u2_exp_r3, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(academic_peer_review_gatekeeping_u2_exp_r3, tangled_rope).
narrative_ontology:human_readable(academic_peer_review_gatekeeping_u2_exp_r3, "Academic Peer Review and Journal Gatekeeping").
narrative_ontology:topic_domain(academic_peer_review_gatekeeping_u2_exp_r3, "economic/social/technological").

domain_priors:requires_active_enforcement(academic_peer_review_gatekeeping_u2_exp_r3).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(academic_peer_review_gatekeeping_u2_exp_r3, for_profit_journal_publishers).
narrative_ontology:constraint_beneficiary(academic_peer_review_gatekeeping_u2_exp_r3, tenured_senior_academics).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u2_exp_r3, junior_professors).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u2_exp_r3, research_institutions).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u2_exp_r3, general_public).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% For a junior academic, 'publish or perish' in high-impact journals is a career necessity. The system extracts free labor (writing, reviewing) with no viable alternative for securing tenure, making it a Snare.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_exp_r3, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% From the publisher's perspective, the system is a mechanism for coordinating quality control and disseminating prestigious research. They see their role as essential curation, classifying the constraint as a beneficial Rope.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_exp_r3, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% An analytical observer, such as a library consortium managing budgets, sees both the coordination function (prestige as a sorting metric) and the severe extraction (exorbitant subscription fees for publicly-funded research). This dual nature defines it as a Tangled Rope.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_exp_r3, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

% University administrations are both victims (paying high subscription fees) and enforcers (using journal prestige in tenure decisions). Their exit is constrained by the need to attract and retain top talent, locking them into the system.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_exp_r3, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(academic_peer_review_gatekeeping_u2_exp_r3_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_exp_r3, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_exp_r3, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(academic_peer_review_gatekeeping_u2_exp_r3, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(academic_peer_review_gatekeeping_u2_exp_r3_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (ε=0.62) reflects the massive value transfer from public and non-profit sectors to a few large publishers. The suppression score (0.75) is high due to the institutionalization of journal prestige in hiring, promotion, and funding decisions, creating a powerful lock-in effect with few viable alternatives for career-minded academics.
 *
 * PERSPECTIVAL GAP:
 *   A significant gap exists. Junior academics experience the system as a coercive Snare due to career pressures. Publishers frame it as a valuable Rope that coordinates and validates scientific quality. Analytical observers like library consortia see the reality: a Tangled Rope that performs a genuine (if debatable) coordination function while enabling asymmetric, rent-seeking extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is clear: value flows from researchers and their institutions (the victims) to the publishers (the beneficiaries). Researchers provide uncompensated labor and their institutions pay to access the product of that labor. Senior tenured academics can be secondary beneficiaries, as they act as gatekeepers (editors) and derive status from the prestige system they oversee.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a Tangled Rope is crucial for avoiding mandatrophy. A pure Snare classification would fail to explain why so many intelligent people willingly participate; it misses the genuine coordination function that prestige, however flawed, provides. A pure Rope classification would be a gross misrepresentation, ignoring the billions of dollars extracted from the public and non-profit sectors. The Tangled Rope classification correctly identifies that a system can have both a coordination function and be highly extractive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prestige_as_quality_signal,
    'Is the prestige signal from high-impact journals a genuine, irreplaceable coordination function for scientific quality, or a manufactured scarcity that primarily serves to justify extraction?',
    'Comparative analysis of long-term citation impact and reproducibility rates between research published in high-cost prestige journals versus well-regarded open-access platforms.',
    'If prestige is found to be a weak or theatrical proxy for quality, the constraint's coordination function is minimal, and it collapses into a pure Snare. If it is a necessary signal, it remains a Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prestige_as_quality_signal, empirical, 'Whether journal prestige is a necessary quality signal or a theatrical justification for extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(academic_peer_review_gatekeeping_u2_exp_r3, 1975, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acad_tr_t1975, academic_peer_review_gatekeeping_u2_exp_r3, theater_ratio, 1975, 0.15).
narrative_ontology:measurement(acad_tr_t2000, academic_peer_review_gatekeeping_u2_exp_r3, theater_ratio, 2000, 0.3).
narrative_ontology:measurement(acad_tr_t2025, academic_peer_review_gatekeeping_u2_exp_r3, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(acad_be_t1975, academic_peer_review_gatekeeping_u2_exp_r3, base_extractiveness, 1975, 0.25).
narrative_ontology:measurement(acad_be_t2000, academic_peer_review_gatekeeping_u2_exp_r3, base_extractiveness, 2000, 0.5).
narrative_ontology:measurement(acad_be_t2025, academic_peer_review_gatekeeping_u2_exp_r3, base_extractiveness, 2025, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(academic_peer_review_gatekeeping_u2_exp_r3, information_standard).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping_u2_exp_r3, university_funding_models).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping_u2_exp_r3, scientific_reproducibility_crisis).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
