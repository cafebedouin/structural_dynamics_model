% ============================================================================
% CONSTRAINT STORY: family_law_authority__christian_canonical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_family_law_authority__christian_canonical_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: family_law_authority__christian_canonical_reading
 *   human_readable: Christian Canonical Marriage (Catholic/Protestant)
 *   domain: religious/social/legal
 *
 * SUMMARY:
 *   This constraint models Christian canonical marriage, encompassing both
 *   the Catholic view of marriage as an indissoluble sacrament and the varied
 *   Protestant views of marriage as a sacred covenant, often allowing for
 *   divorce under certain conditions. It is one reading of the broader
 *   'family_law_authority' kernel. The constraint's persistence relies on
 *   ecclesiastical authority and the identity-locked nature of many
 *   adherents, who face significant social and spiritual costs for
 *   non-compliance. While providing genuine coordination for adherents, it
 *   also extracts from those who diverge from its strictures, particularly
 *   regarding divorce or recognition of non-canonical unions.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__christian_canonical_reading, 0.45).
domain_priors:suppression_score(family_law_authority__christian_canonical_reading, 0.6).
domain_priors:theater_ratio(family_law_authority__christian_canonical_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__christian_canonical_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__christian_canonical_reading, "Christian Canonical Marriage (Catholic/Protestant)").
narrative_ontology:topic_domain(family_law_authority__christian_canonical_reading, "religious/social/legal").

domain_priors:requires_active_enforcement(family_law_authority__christian_canonical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__christian_canonical_reading, 'a72d065a-cf62-4661-9b6a-4a1a371d410b').
narrative_ontology:cs_kernel_codification('a72d065a-cf62-4661-9b6a-4a1a371d410b', formalized).
narrative_ontology:cs_authority_grounding('a72d065a-cf62-4661-9b6a-4a1a371d410b', lineage).
narrative_ontology:cs_interpretation_layer_present('a72d065a-cf62-4661-9b6a-4a1a371d410b').
narrative_ontology:cs_reading_relation('a72d065a-cf62-4661-9b6a-4a1a371d410b', family_law_authority__hindu_dharmashastra_reading, coexists_with).
narrative_ontology:cs_reading_relation('a72d065a-cf62-4661-9b6a-4a1a371d410b', family_law_authority__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('a72d065a-cf62-4661-9b6a-4a1a371d410b', family_law_authority__parsi_zoroastrian_reading, coexists_with).
narrative_ontology:cs_reading_relation('a72d065a-cf62-4661-9b6a-4a1a371d410b', family_law_authority__secular_contractual_reading, coexists_with).
narrative_ontology:cs_axiom('a72d065a-cf62-4661-9b6a-4a1a371d410b', foundational, marriage_divinely_instituted).
narrative_ontology:cs_axiom_status(marriage_divinely_instituted, holdable).
narrative_ontology:cs_axiom_grounding('a72d065a-cf62-4661-9b6a-4a1a371d410b', marriage_divinely_instituted, theological).
narrative_ontology:cs_axiom('a72d065a-cf62-4661-9b6a-4a1a371d410b', foundational, ecclesiastical_authority_over_validity).
narrative_ontology:cs_axiom_status(ecclesiastical_authority_over_validity, holdable).
narrative_ontology:cs_axiom_grounding('a72d065a-cf62-4661-9b6a-4a1a371d410b', ecclesiastical_authority_over_validity, conventional).
narrative_ontology:cs_reference_frame('a72d065a-cf62-4661-9b6a-4a1a371d410b', traditional_christian_doctrine).
narrative_ontology:cs_drift_state('a72d065a-cf62-4661-9b6a-4a1a371d410b', contemporary_secular_society, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('a72d065a-cf62-4661-9b6a-4a1a371d410b', '').
narrative_ontology:cs_kernel_id(family_law_authority__christian_canonical_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__christian_canonical_reading, ecclesiastical_authorities).
narrative_ontology:constraint_beneficiary(family_law_authority__christian_canonical_reading, adherents_seeking_stability).
narrative_ontology:constraint_victim(family_law_authority__christian_canonical_reading, adherents_seeking_divorce).
narrative_ontology:constraint_victim(family_law_authority__christian_canonical_reading, non_adherents_seeking_recognition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Catholic authorities define marriage as an indissoluble sacrament; Protestant denominations vary but generally hold marriage as a sacred covenant. They administer rites, interpret doctrine, and adjudicate validity, often influencing civil recognition. They benefit from the stability and moral authority derived from upholding these norms.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, ecclesiastical_authorities, agenda_setter,
    institutional, generational, constrained, global).

% Benefit from the clear moral framework, community support, and perceived spiritual benefits of a religiously sanctioned marriage. Their identity is often deeply intertwined with their faith, making exit from the canonical framework difficult even if civil options exist.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, adherents_seeking_stability, beneficiary,
    moderate, biographical, identity_locked, local).

% For Catholics, divorce is not recognized, leading to spiritual and social exclusion if they remarry civilly. For some Protestants, divorce is permitted but carries social stigma. They bear the cost of non-compliance with canonical rules, often facing emotional and social pressure.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, adherents_seeking_divorce, payer,
    powerless, immediate, identity_locked, local).

% Individuals who do not adhere to Christian canonical views but seek recognition for their relationships (e.g., same-sex couples in contexts where religious definitions influence civil law) may find their unions unrecognized or devalued by the broader social influence of this constraint.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, non_adherents_seeking_recognition, payer,
    powerless, biographical, constrained, national).

% State legal systems often interact with religious definitions of marriage, sometimes recognizing them, sometimes diverging. They observe the social and legal implications of canonical rules, particularly regarding divorce, inheritance, and child custody, and may legislate to either accommodate or supersede religious definitions.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, secular_legal_systems, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates social and spiritual life around a shared understanding of marriage as a sacred institution, providing a framework for family formation, moral conduct, and community cohesion within Christian traditions.
% TRANSFER_FUNCTION: Transfers moral authority over marital status and family life from individuals to ecclesiastical bodies, in exchange for spiritual guidance, community belonging, and a stable social framework.
% ABSENT_VOICES: Individuals who advocate for purely secular, contract-based marriage, or those from other religious traditions, are often excluded from the internal discourse of Christian canonical marriage, and their perspectives on marital validity or dissolution are not considered within this framework.
% DISAPPEARANCE_RATIONALE: If Christian canonical marriage vanished overnight, the social fabric of many communities would be profoundly altered. Ecclesiastical authority over family life would collapse, leading to a redefinition of marital norms, family structures, and spiritual obligations for millions of adherents, and a significant shift in the relationship between religious institutions and civil law.
% FOUNDING_PROBLEM: To establish a divinely ordained and stable institution for procreation, companionship, and the spiritual formation of families, preventing social chaos and moral decay.
% FOUNDING_PROBLEM_CORROBORATION: Ecclesiastical authorities and many adherents attest that the founding problem of maintaining moral order and family stability is still live, citing contemporary challenges to traditional family structures. Secular legal scholars and sociologists, from outside the benefiting parties, corroborate the ongoing social function of religious marriage in providing community and moral guidance, even while disputing its exclusive claims or specific doctrines.
narrative_ontology:disappearance_verdict(family_law_authority__christian_canonical_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__christian_canonical_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__christian_canonical_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(family_law_authority__christian_canonical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(family_law_authority__christian_canonical_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(family_law_authority__christian_canonical_reading_tests).
:- end_tests(family_law_authority__christian_canonical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) reflects the costs borne by adherents whose personal circumstances conflict with canonical rules (e.g., seeking divorce) and the social pressure on non-adherents. Suppression (0.60) is moderate-to-high, stemming from the strong social and spiritual sanctions for non-compliance, particularly within tightly-knit religious communities, and the identity-locked nature of many adherents. Theater ratio is low (0.10) as the constraint's functions (spiritual guidance, community cohesion) are genuinely active, not merely performative. The metrics show relative stability over the interval, indicating a persistent, institutionally maintained structure.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of ecclesiastical authorities and many adherents, this constraint is a foundational Rope, providing essential spiritual and social coordination. From the perspective of those seeking divorce or non-adherents, it operates as a Snare, imposing significant costs and limiting autonomy. The engine's per-seat classification will reflect this divergence based on the declared power, exit options, and beneficiary/victim status of each stakeholder.
 *
 * DIRECTIONALITY LOGIC:
 *   Ecclesiastical authorities are clear beneficiaries (d near 0.0) as they derive legitimacy and influence from upholding these norms. Adherents seeking stability are also beneficiaries (d near 0.2-0.3), gaining community and moral clarity. Adherents seeking divorce and non-adherents seeking recognition are targets (d near 0.7-0.8), bearing the costs of non-compliance or non-recognition. Secular legal systems act as observers, analyzing the constraint's impact without being directly subject to its internal enforcement.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    internalized_vs_structural_suppression,
    'Is the suppression experienced by adherents primarily structural (ecclesiastical sanctions, social exclusion) or internalized (guilt, fear of spiritual consequences)?',
    'Post-exit trajectory analysis: if spiritual/social distress persists after formal disengagement from the church, it suggests internalized suppression. Surveys on ex-adherents'' psychological well-being.',
    'If internalized, the effective suppression is higher than the structural measure suggests, as individuals carry the constraint''s force with them. This would amplify the ''snare'' aspect for affected individuals.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_vs_structural_suppression, empirical, 'Distinguishing the source of suppressive force on adherents.').

omega_variable(
    civil_vs_canonical_recognition_divergence,
    'To what extent does the civil recognition of marriage diverge from the canonical definition, and how does this affect adherents'' options?',
    'Comparative legal analysis across jurisdictions with varying church-state separation, and ethnographic studies of adherents navigating dual legal systems.',
    'Greater divergence between civil and canonical law increases the ''constrained'' exit options for adherents, potentially reducing the effective suppression of the canonical constraint by offering a viable alternative, though often with social costs. Less divergence amplifies the constraint''s power.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(civil_vs_canonical_recognition_divergence, empirical, 'Impact of civil law on the canonical constraint''s effective reach.').

omega_variable(
    sacramental_vs_covenantal_framing_impact,
    'Does the Catholic sacramental (indissoluble) framing of marriage lead to significantly higher extraction and suppression for adherents compared to Protestant covenantal (divorce-permitting) framings?',
    'Comparative analysis of divorce rates, social stigma, and psychological distress among Catholic vs. Protestant adherents in similar cultural contexts.',
    'If the sacramental framing imposes significantly higher costs, the ''christian_canonical_reading'' might need to be decomposed into distinct Catholic and Protestant sub-readings, each with its own ε and suppression values, as the structural differences would be too great for a single constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sacramental_vs_covenantal_framing_impact, conceptual, 'Whether Catholic and Protestant framings constitute distinct constraints.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__christian_canonical_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fami_tr_t0, family_law_authority__christian_canonical_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(fami_tr_t10, family_law_authority__christian_canonical_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(fami_tr_t20, family_law_authority__christian_canonical_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(fami_tr_t30, family_law_authority__christian_canonical_reading, theater_ratio, 30, 0.1).
narrative_ontology:measurement(fami_tr_t40, family_law_authority__christian_canonical_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(fami_tr_t50, family_law_authority__christian_canonical_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(fami_be_t0, family_law_authority__christian_canonical_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(fami_be_t10, family_law_authority__christian_canonical_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(fami_be_t20, family_law_authority__christian_canonical_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(fami_be_t30, family_law_authority__christian_canonical_reading, base_extractiveness, 30, 0.44).
narrative_ontology:measurement(fami_be_t40, family_law_authority__christian_canonical_reading, base_extractiveness, 40, 0.43).
narrative_ontology:measurement(fami_be_t50, family_law_authority__christian_canonical_reading, base_extractiveness, 50, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(fami_su_t0, family_law_authority__christian_canonical_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(fami_su_t10, family_law_authority__christian_canonical_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(fami_su_t20, family_law_authority__christian_canonical_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(fami_su_t30, family_law_authority__christian_canonical_reading, suppression_requirement, 30, 0.58).
narrative_ontology:measurement(fami_su_t40, family_law_authority__christian_canonical_reading, suppression_requirement, 40, 0.59).
narrative_ontology:measurement(fami_su_t50, family_law_authority__christian_canonical_reading, suppression_requirement, 50, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__christian_canonical_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
