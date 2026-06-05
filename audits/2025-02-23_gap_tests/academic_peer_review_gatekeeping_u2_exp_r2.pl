% ============================================================================
% CONSTRAINT STORY: academic_peer_review_gatekeeping_u2_exp_r2
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2023-10-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_academic_peer_review_gatekeeping_u2_exp_r2, []).

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
 *   constraint_id: academic_peer_review_gatekeeping_u2_exp_r2
 *   human_readable: Academic Peer Review and Journal Gatekeeping
 *   domain: economic/social/technological
 *
 * SUMMARY:
 *   The academic publishing model is a system where researchers, typically
 *   funded by public or university grants, produce research, write articles,
 *   and perform peer review for free. For-profit publishers then package this
 *   free labor and sell it back to university libraries at extremely high
 *   subscription costs, while also restricting public access. This creates a
 *   conflict between the academic mission of knowledge dissemination and the
 *   publisher's profit motive.
 *
 * KEY AGENTS:
 *   - Junior Professors: Primary victims (powerless/trapped) - Must publish in high-prestige journals to secure tenure and career advancement.
 *   - For-Profit Journal Publishers: Primary beneficiaries (institutional/arbitrage) - Capture immense value from the free labor of academics and high subscription fees from their institutions.
 *   - University Libraries/Consortia: Analytical observers/victims (analytical/constrained) - Bear the direct financial costs and negotiate with publishers, but are pressured by faculty to maintain subscriptions.
 *   - Senior Professors/Editors: Enforcers/participants (powerful/mobile) - Uphold the system's standards through editorial work, lending it their prestige while being aware of its flaws.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(academic_peer_review_gatekeeping_u2_exp_r2, 0.68).
domain_priors:suppression_score(academic_peer_review_gatekeeping_u2_exp_r2, 0.8).
domain_priors:theater_ratio(academic_peer_review_gatekeeping_u2_exp_r2, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u2_exp_r2, extractiveness, 0.68).
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u2_exp_r2, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u2_exp_r2, theater_ratio, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(academic_peer_review_gatekeeping_u2_exp_r2, tangled_rope).
narrative_ontology:human_readable(academic_peer_review_gatekeeping_u2_exp_r2, "Academic Peer Review and Journal Gatekeeping").
narrative_ontology:topic_domain(academic_peer_review_gatekeeping_u2_exp_r2, "economic/social/technological").

domain_priors:requires_active_enforcement(academic_peer_review_gatekeeping_u2_exp_r2).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(academic_peer_review_gatekeeping_u2_exp_r2, for_profit_journal_publishers).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u2_exp_r2, junior_professors).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u2_exp_r2, university_libraries).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u2_exp_r2, general_public).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% For a junior academic, publishing in prestigious, paywalled journals is often non-negotiable for career survival ('publish or perish'). They provide free labor (research, writing, reviewing) and are trapped by the system's credentialing function.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_exp_r2, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% From the publisher's perspective, this is an extremely efficient coordination mechanism. It leverages academic norms to acquire high-value content and review labor for free, which it then packages and sells at a high margin. For them, it is pure coordination for profit.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_exp_r2, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Library consortia see the full picture: the system coordinates scholarly communication (a Rope function) but does so with enormous, asymmetric extraction (a Snare function). They analyze the costs but are constrained by faculty demand for access, leading to the Tangled Rope classification.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_exp_r2, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% A tenured professor acting as an editor participates in the coordination function, believing they are upholding standards. However, they are also aware of the extractive business model and their own complicity, making it a Tangled Rope. Their exit options are better but still constrained by the prestige economy.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_exp_r2, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(academic_peer_review_gatekeeping_u2_exp_r2_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_exp_r2, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_exp_r2, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(academic_peer_review_gatekeeping_u2_exp_r2, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(academic_peer_review_gatekeeping_u2_exp_r2_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.68) is high due to the business model of acquiring labor and content for free and selling the product at monopoly prices. The suppression (0.80) is extremely high because the 'publish or perish' culture, tenure requirements, and university ranking systems create a near-total lack of viable alternatives for career-minded academics. The theater ratio (0.60) reflects the focus on 'impact factor' and journal branding over the actual function of scientific communication.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For the publisher, it's a brilliant Rope that coordinates academic output for profit. For the junior researcher, it's a Snare that extracts their labor under duress. For the library system, which sees both the coordination function and the crippling costs, it is a clear Tangled Rope. This gap is sustained by the diffusion of costs (paid by libraries) and benefits (accrued by publishers) away from the individuals performing the labor (researchers).
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is unambiguous. Value flows from researchers (labor) and their institutions (cash via libraries) to the publishers. Publishers are the sole structural beneficiaries. Researchers, their institutions, and the public who cannot access the research are the victims.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this system as a pure Snare would miss the fact that it does, however imperfectly, perform a coordination function: it standardizes, archives, and disseminates research. Classifying it as a Rope would ignore the colossal and asymmetric extraction. The Tangled Rope classification is essential to capture this duality, where a genuine coordination need has been captured and transformed into a highly extractive enterprise.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prestige_as_quality_signal,
    'Is the prestige conferred by high-impact journals a genuine signal of quality (a coordination good) or an artificial scarcity manufactured by publishers (an extractive mechanism)?',
    'Large-scale analysis comparing the long-term impact and reproducibility of research from top-tier paywalled journals versus high-quality, rigorously reviewed open-access alternatives.',
    'If prestige is a robust proxy for quality, the system retains a strong Tangled Rope character. If it is primarily manufactured scarcity, the system collapses into a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prestige_as_quality_signal, empirical, 'Whether journal prestige is a real quality signal or manufactured scarcity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(academic_peer_review_gatekeeping_u2_exp_r2, 1980, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acad_tr_t1980, academic_peer_review_gatekeeping_u2_exp_r2, theater_ratio, 1980, 0.2).
narrative_ontology:measurement(acad_tr_t2000, academic_peer_review_gatekeeping_u2_exp_r2, theater_ratio, 2000, 0.45).
narrative_ontology:measurement(acad_tr_t2020, academic_peer_review_gatekeeping_u2_exp_r2, theater_ratio, 2020, 0.6).

% Extraction over time
narrative_ontology:measurement(acad_be_t1980, academic_peer_review_gatekeeping_u2_exp_r2, base_extractiveness, 1980, 0.25).
narrative_ontology:measurement(acad_be_t2000, academic_peer_review_gatekeeping_u2_exp_r2, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement(acad_be_t2020, academic_peer_review_gatekeeping_u2_exp_r2, base_extractiveness, 2020, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(academic_peer_review_gatekeeping_u2_exp_r2, information_standard).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping_u2_exp_r2, university_funding_models).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping_u2_exp_r2, scientific_reproducibility_crisis).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping_u2_exp_r2, tenure_and_promotion_systems).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
