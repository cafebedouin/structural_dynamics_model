% ============================================================================
% CONSTRAINT STORY: academic_peer_review_gatekeeping_u4_exp_r3
% ============================================================================
% Version: 7.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_academic_peer_review_gatekeeping_u4_exp_r3, []).

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
 *   constraint_id: academic_peer_review_gatekeeping_u4_exp_r3
 *   human_readable: Academic Peer Review and Journal Gatekeeping
 *   domain: economic/social/technological
 *
 * SUMMARY:
 *   The academic peer review system is a mechanism where researchers provide
 *   free labor (writing, reviewing, editing) to for-profit publishers. These
 *   publishers then erect paywalls and sell access to the research—often
 *   funded by public grants—back to the researchers' own institutions at
 *   extremely high costs. The system is maintained by a prestige economy
 *   where career advancement (i.e., tenure) is tied to publishing in
 *   high-impact, exclusive journals.
 *
 * KEY AGENTS:
 *   - Junior Professors: Primary victims (powerless/trapped) - must provide free labor and publish in prestige journals to secure their careers.
 *   - For-Profit Journal Publishers: Primary beneficiaries (institutional/arbitrage) - capture immense value from free labor and institutional subscriptions.
 *   - Research Institutions / Library Consortia: Secondary victims (institutional/constrained) - forced to pay escalating fees to access research their own faculty produced.
 *   - General Public: Tertiary victims (powerless/trapped) - fund the research through taxes but are denied access.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(academic_peer_review_gatekeeping_u4_exp_r3, 0.68).
domain_priors:suppression_score(academic_peer_review_gatekeeping_u4_exp_r3, 0.75).
domain_priors:theater_ratio(academic_peer_review_gatekeeping_u4_exp_r3, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u4_exp_r3, extractiveness, 0.68).
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u4_exp_r3, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u4_exp_r3, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(academic_peer_review_gatekeeping_u4_exp_r3, tangled_rope).
narrative_ontology:human_readable(academic_peer_review_gatekeeping_u4_exp_r3, "Academic Peer Review and Journal Gatekeeping").
narrative_ontology:topic_domain(academic_peer_review_gatekeeping_u4_exp_r3, "economic/social/technological").

domain_priors:requires_active_enforcement(academic_peer_review_gatekeeping_u4_exp_r3).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(academic_peer_review_gatekeeping_u4_exp_r3, for_profit_journal_publishers).
narrative_ontology:constraint_beneficiary(academic_peer_review_gatekeeping_u4_exp_r3, senior_academic_gatekeepers).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u4_exp_r3, junior_professors).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u4_exp_r3, research_institutions).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u4_exp_r3, general_public).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: The untenured academic who must publish in high-impact, costly journals to secure a career, providing free labor (research, writing, reviewing) into a system that extracts from their institution.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u4_exp_r3, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: The publisher who views the system as an efficient coordination mechanism to vet, brand, and disseminate research, generating revenue that supports this ecosystem.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u4_exp_r3, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: The university or library consortium that must pay exorbitant subscription fees ('The Big Deal') to access research produced by its own faculty, trapped by the need to provide resources for its community.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u4_exp_r3, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: The outside observer who recognizes both the genuine coordination function (quality signaling, community organization) and the severe, asymmetric extraction of value from publicly-funded labor.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u4_exp_r3, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(academic_peer_review_gatekeeping_u4_exp_r3_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u4_exp_r3, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u4_exp_r3, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(academic_peer_review_gatekeeping_u4_exp_r3, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(academic_peer_review_gatekeeping_u4_exp_r3_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high due to the direct conversion of free, skilled labor and public funds into private profit. Suppression (0.75) is severe because the 'publish or perish' mandate and the journal prestige hierarchy create powerful lock-in, making alternative platforms (pre-print servers, open-access journals) appear career-limiting for junior academics, despite their functional superiority in dissemination.
 *
 * PERSPECTIVAL GAP:
 *   A significant gap exists. The Junior Professor, trapped by career incentives, experiences the system as a Snare. The Publisher, benefiting from the arrangement, frames it as a necessary Rope for coordinating scientific validation. The Analyst, observing both the coordination function and the extreme value extraction, classifies it as a Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Value flows from researchers (labor) and their institutions (subscription fees, which are often from public funds) to the shareholders of publishing companies. The beneficiaries are the publishers who own the journals. The victims are the researchers who perform the labor for free and the institutions and public who pay for the final product.
 *
 * MANDATROPHY ANALYSIS:
 *   The system is often defended as a pure coordination mechanism (Rope) for ensuring quality control. This mislabels the constraint. The Tangled Rope classification is critical as it correctly identifies that a genuine coordination function (organizing peer review) has been tightly coupled with a highly extractive business model. It prevents the system's beneficiaries from hiding the asymmetric extraction behind the veil of 'maintaining standards'.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    quality_control_necessity,
    'Is the publisher-centric model with high barriers and costs a necessary structure for maintaining scientific rigor, or is it a rent-seeking artifact that could be replaced by more efficient, open models without a loss of quality?',
    'Large-scale comparative study of research quality, retraction rates, and scientific impact between top-tier subscription journals and high-quality, peer-governed open access platforms over a decade.',
    'If the model is proven necessary for quality, the constraint leans towards a high-cost Rope. If it is primarily rent-seeking, it is a pure Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quality_control_necessity, empirical, 'Ambiguity between the system's claimed quality control function and its observed rent-seeking behavior.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(academic_peer_review_gatekeeping_u4_exp_r3, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acad_tr_t1980, academic_peer_review_gatekeeping_u4_exp_r3, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(acad_tr_t2002, academic_peer_review_gatekeeping_u4_exp_r3, theater_ratio, 2002, 0.3).
narrative_ontology:measurement(acad_tr_t2024, academic_peer_review_gatekeeping_u4_exp_r3, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(acad_be_t1980, academic_peer_review_gatekeeping_u4_exp_r3, base_extractiveness, 1980, 0.25).
narrative_ontology:measurement(acad_be_t2002, academic_peer_review_gatekeeping_u4_exp_r3, base_extractiveness, 2002, 0.55).
narrative_ontology:measurement(acad_be_t2024, academic_peer_review_gatekeeping_u4_exp_r3, base_extractiveness, 2024, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(academic_peer_review_gatekeeping_u4_exp_r3, information_standard).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping_u4_exp_r3, university_tenure_system).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping_u4_exp_r3, public_research_funding_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
