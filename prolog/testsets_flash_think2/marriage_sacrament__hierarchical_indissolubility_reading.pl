% ============================================================================
% CONSTRAINT STORY: marriage_sacrament__hierarchical_indissolubility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_sacrament__hierarchical_indissolubility_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: marriage_sacrament__hierarchical_indissolubility_reading
 *   human_readable: Marriage as Ontological Reality: Hierarchical Indissolubility Reading
 *   domain: religious_doctrine/canon_law/political_sociology
 *
 * SUMMARY:
 *   This constraint describes the Catholic Church's doctrine of marriage as
 *   an ontological reality, requiring hierarchical adjudication and asserting
 *   indissolubility as constitutive rather than aspirational. This reading
 *   emphasizes strict adherence to canonical norms, leading to the exclusion
 *   of divorced and remarried Catholics from full sacramental participation
 *   and imposing significant burdens through the annulment process. It is one
 *   reading of the broader 'marriage_sacrament' kernel, distinct from more
 *   pastoral interpretations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_sacrament__hierarchical_indissolubility_reading, 0.8).
domain_priors:suppression_score(marriage_sacrament__hierarchical_indissolubility_reading, 0.85).
domain_priors:theater_ratio(marriage_sacrament__hierarchical_indissolubility_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_sacrament__hierarchical_indissolubility_reading, tangled_rope).
narrative_ontology:human_readable(marriage_sacrament__hierarchical_indissolubility_reading, "Marriage as Ontological Reality: Hierarchical Indissolubility Reading").
narrative_ontology:topic_domain(marriage_sacrament__hierarchical_indissolubility_reading, "religious_doctrine/canon_law/political_sociology").

domain_priors:requires_active_enforcement(marriage_sacrament__hierarchical_indissolubility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_sacrament__hierarchical_indissolubility_reading, 'c3ed3d91-6fa5-4552-a1ea-82d19b81764f').
narrative_ontology:cs_kernel_codification('c3ed3d91-6fa5-4552-a1ea-82d19b81764f', formalized).
narrative_ontology:cs_authority_grounding('c3ed3d91-6fa5-4552-a1ea-82d19b81764f', lineage).
narrative_ontology:cs_interpretation_layer_present('c3ed3d91-6fa5-4552-a1ea-82d19b81764f').
narrative_ontology:cs_reading_relation('c3ed3d91-6fa5-4552-a1ea-82d19b81764f', marriage_sacrament__civic_pastoral_reading, coexists_with).
narrative_ontology:cs_axiom('c3ed3d91-6fa5-4552-a1ea-82d19b81764f', foundational, marriage_is_ontological_indissoluble).
narrative_ontology:cs_axiom_status(marriage_is_ontological_indissoluble, holdable).
narrative_ontology:cs_axiom_grounding('c3ed3d91-6fa5-4552-a1ea-82d19b81764f', marriage_is_ontological_indissoluble, deontological).
narrative_ontology:cs_reference_frame('c3ed3d91-6fa5-4552-a1ea-82d19b81764f', tridentine_sacramental_theology).
narrative_ontology:cs_drift_state('c3ed3d91-6fa5-4552-a1ea-82d19b81764f', post_vatican_ii_era, gap(stable, minor, false)).
narrative_ontology:cs_created_at('c3ed3d91-6fa5-4552-a1ea-82d19b81764f', '').
narrative_ontology:cs_kernel_id(marriage_sacrament__hierarchical_indissolubility_reading, marriage_sacrament).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_sacrament__hierarchical_indissolubility_reading, catholic_hierarchy).
narrative_ontology:constraint_beneficiary(marriage_sacrament__hierarchical_indissolubility_reading, doctrinal_conservatives).
narrative_ontology:constraint_victim(marriage_sacrament__hierarchical_indissolubility_reading, divorced_remarried_catholics).
narrative_ontology:constraint_victim(marriage_sacrament__hierarchical_indissolubility_reading, catholics_seeking_annulment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets, interprets, and enforces the doctrine of marriage, including its indissolubility and the processes for annulment. Benefits from maintaining doctrinal purity and institutional control over sacramental life.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, catholic_hierarchy, agenda_setter,
    institutional, generational, arbitrage, global).

% Adhere strictly to the hierarchical interpretation of marriage, benefiting from the clarity and perceived stability of the doctrine. They often reinforce its enforcement through social and theological pressure.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, doctrinal_conservatives, beneficiary,
    organized, generational, constrained, global).

% Are excluded from full sacramental participation (e.g., Eucharist) unless they obtain an annulment or live celibately. They bear significant spiritual and social costs, often feeling alienated from the Church they identify with.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, divorced_remarried_catholics, payer,
    powerless, biographical, identity_locked, global).

% Undergo a lengthy, costly, and often emotionally taxing annulment process, subject to the judgment of ecclesiastical tribunals. They bear the procedural burdens and uncertainty, with no guarantee of a favorable outcome.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, catholics_seeking_annulment, payer,
    moderate, biographical, constrained, global).

% Advocate for a more compassionate and pastorally sensitive approach to marriage, emphasizing human experience and individual discernment. They lack formal authority within the hierarchical structure and their views are often marginalized.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, civic_pastoral_advocates, excluded,
    organized, generational, constrained, global).

% Study the theological, canonical, and sociological implications of the doctrine and its enforcement. They analyze its consistency, historical development, and impact on adherents, often from a critical distance.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, analytical_theologians, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_sacrament__hierarchical_indissolubility_reading, catholic_hierarchy).
narrative_ontology:fixing_cost_class(marriage_sacrament__hierarchical_indissolubility_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To maintain doctrinal consistency and institutional unity regarding the nature of marriage as a sacrament, ensuring its perceived integrity against external pressures and internal deviations.
% TRANSFER_FUNCTION: Transfers spiritual authority, doctrinal purity, and institutional stability to the Catholic hierarchy and its conservative adherents, while transferring exclusion, spiritual burden, and procedural costs to divorced/remarried Catholics and those seeking annulment.
% ABSENT_VOICES: Many divorced and remarried Catholics who have left the Church due to the constraint's impact, as well as theologians and pastoral workers who advocate for a more inclusive approach but are not formally recognized in decision-making bodies.
% DISAPPEARANCE_RATIONALE: If the hierarchical enforcement of indissolubility vanished overnight, the Catholic Church's sacramental theology, canon law, and pastoral practice would undergo a profound reordering. Millions of adherents' spiritual lives, institutional authority, and internal cohesion would be dramatically impacted, leading to a reorganization of marital norms and practices within the Church.
% FOUNDING_PROBLEM: To define and protect the sacred, indissoluble nature of marriage as instituted by Christ, ensuring its integrity against secularization, individualistic interpretations, and the perceived erosion of traditional family structures.
% FOUNDING_PROBLEM_CORROBORATION: The Catholic hierarchy and doctrinal conservatives attest that the problem of defending marriage's sacred nature remains live, citing ongoing societal challenges to traditional definitions. Critics, including some theologians and pastoral advocates, argue that while doctrinal defense is important, the constraint's primary function has shifted to enforcing institutional power and excluding non-conforming members, rather than genuinely addressing the founding problem in a pastoral way. Independent sociological studies also highlight the pastoral challenges created by the current enforcement.
narrative_ontology:disappearance_verdict(marriage_sacrament__hierarchical_indissolubility_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_sacrament__hierarchical_indissolubility_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_sacrament__hierarchical_indissolubility_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(marriage_sacrament__hierarchical_indissolubility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_sacrament__hierarchical_indissolubility_reading, 0.8, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_sacrament__hierarchical_indissolubility_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_sacrament__hierarchical_indissolubility_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_sacrament__hierarchical_indissolubility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.8) due to the severe spiritual and social costs imposed on those who do not conform to the strict interpretation, including exclusion from sacraments and the arduous annulment process. Suppression is also high (0.85) because the institutional authority of the Church, backed by canon law and social pressure, effectively limits alternatives for Catholics seeking full participation. Theater ratio is low (0.1) as the enforcement mechanisms (e.g., tribunals, denial of sacraments) are genuinely applied and have real consequences, not merely performative. Accessibility collapse is high (0.75) because the path to full sacramental participation for divorced/remarried Catholics within this framework is severely restricted. Resistance is moderate (0.4) as there is internal dissent and advocacy for change, but it faces strong institutional reaffirmation of the doctrine.
 *
 * PERSPECTIVAL GAP:
 *   The Catholic hierarchy and doctrinal conservatives experience this constraint as a necessary defense of sacred truth and institutional integrity, providing clarity and stability. For divorced and remarried Catholics, the same structure operates as a source of profound exclusion and spiritual pain, imposing significant burdens and limiting their participation in the faith community. The engine will compute these divergent experiences from the declared structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   The Catholic hierarchy and doctrinal conservatives are beneficiaries, gaining institutional stability, doctrinal purity, and reinforced authority. Divorced and remarried Catholics, along with those navigating the annulment process, are the primary targets, bearing the costs of exclusion, stigma, and procedural burdens. Civic pastoral advocates are excluded, as their alternative interpretations are not formally recognized or integrated into the hierarchical decision-making.
 *
 * MANDATROPHY ANALYSIS:
 *   From the perspective of this reading, the mandate to uphold indissolubility is perpetually live and essential to the Church's identity, thus mandatrophy is not acknowledged. However, from an external or critical perspective, the constraint's rigid enforcement could be seen as having outlived its pastoral function, becoming an extractive mechanism that prioritizes institutional control over the spiritual well-being of its members. The 'founding_problem_status' being 'contested' reflects this tension.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_truth_vs_institutional_construct,
    'Is the indissolubility of marriage, as enforced by this reading, a divinely revealed theological truth or an institutional construct designed to maintain hierarchical authority and doctrinal uniformity?',
    'Historical-critical theological analysis of the development of marriage doctrine, combined with sociological studies of its impact on institutional power dynamics versus spiritual formation.',
    'If primarily an institutional construct, the constraint''s extractiveness and suppression would be re-evaluated as purely instrumental for power, rather than a necessary consequence of theological truth, potentially reclassifying it closer to a Snare. If a theological truth, the extraction is framed as a necessary cost of adherence to divine law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_truth_vs_institutional_construct, conceptual, 'Ambiguity between theological truth and institutional construct.').

omega_variable(
    exclusion_as_necessity_vs_extraction,
    'Is the exclusion of divorced and remarried Catholics from sacraments a necessary consequence of the doctrine of indissolubility, or an extractive enforcement mechanism that could be mitigated without undermining the core theological claim?',
    'Theological and canonical proposals for alternative pastoral solutions (e.g., ''internal forum'' solutions, changes in annulment criteria) that are implemented and evaluated for their impact on doctrinal integrity and pastoral inclusion.',
    'If mitigation is possible without doctrinal compromise, the current exclusion would be seen as an unnecessarily high form of extraction. If not, the extraction is deemed intrinsic to the doctrine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exclusion_as_necessity_vs_extraction, empirical, 'Whether exclusion is a necessary consequence or an avoidable extraction.').

omega_variable(
    annulment_as_truth_seeking_vs_barrier,
    'Is the annulment process primarily a truth-seeking mechanism to determine marital nullity, or does it function as a prohibitive barrier to re-entry into full sacramental life, imposing undue costs and delays?',
    'Empirical study of annulment process outcomes, costs, and timelines across different dioceses, compared with stated canonical goals and pastoral needs. Analysis of reforms (e.g., Pope Francis''s ''Mitis Iudex Dominus Iesus'') and their actual impact.',
    'If primarily a barrier, the annulment process contributes significantly to the constraint''s extractiveness and suppression. If genuinely truth-seeking and accessible, its extractive component is lower.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(annulment_as_truth_seeking_vs_barrier, empirical, 'Annulment process as truth-seeking or prohibitive barrier.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_sacrament__hierarchical_indissolubility_reading, 1960, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1960, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 1960, 0.1).
narrative_ontology:measurement(marr_tr_t1975, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 1975, 0.09).
narrative_ontology:measurement(marr_tr_t1990, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 1990, 0.08).
narrative_ontology:measurement(marr_tr_t2005, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 2005, 0.09).
narrative_ontology:measurement(marr_tr_t2024, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(marr_be_t1960, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 1960, 0.7).
narrative_ontology:measurement(marr_be_t1975, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 1975, 0.72).
narrative_ontology:measurement(marr_be_t1990, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 1990, 0.75).
narrative_ontology:measurement(marr_be_t2005, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 2005, 0.78).
narrative_ontology:measurement(marr_be_t2024, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 2024, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1960, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 1960, 0.75).
narrative_ontology:measurement(marr_su_t1975, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 1975, 0.78).
narrative_ontology:measurement(marr_su_t1990, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 1990, 0.8).
narrative_ontology:measurement(marr_su_t2005, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 2005, 0.83).
narrative_ontology:measurement(marr_su_t2024, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 2024, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_sacrament__hierarchical_indissolubility_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_sacrament__hierarchical_indissolubility_reading, catholic_eucharistic_discipline).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
