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
 *   human_readable: Marriage Sacrament: Hierarchical Indissolubility Reading
 *   domain: religious_doctrine/canon_law/political_sociology
 *
 * SUMMARY:
 *   This constraint represents the 'hierarchical indissolubility' reading of
 *   the marriage sacrament within the Catholic Church. It asserts that
 *   marriage is an ontological reality, divinely instituted and indissoluble,
 *   requiring strict hierarchical adjudication and enforcement. This reading
 *   leads to the exclusion of divorced and civilly remarried Catholics from
 *   full sacramental life and imposes a demanding annulment process. The high
 *   extractiveness and suppression reflect the significant costs borne by
 *   those who do not conform to this strict interpretation, and the
 *   institutional power required to maintain it.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_sacrament__hierarchical_indissolubility_reading, 0.85).
domain_priors:suppression_score(marriage_sacrament__hierarchical_indissolubility_reading, 0.92).
domain_priors:theater_ratio(marriage_sacrament__hierarchical_indissolubility_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_sacrament__hierarchical_indissolubility_reading, snare).
narrative_ontology:human_readable(marriage_sacrament__hierarchical_indissolubility_reading, "Marriage Sacrament: Hierarchical Indissolubility Reading").
narrative_ontology:topic_domain(marriage_sacrament__hierarchical_indissolubility_reading, "religious_doctrine/canon_law/political_sociology").

domain_priors:requires_active_enforcement(marriage_sacrament__hierarchical_indissolubility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_sacrament__hierarchical_indissolubility_reading, 'b705baba-6536-436a-a5b8-850ff6c0017d').
narrative_ontology:cs_kernel_codification('b705baba-6536-436a-a5b8-850ff6c0017d', fixed_text).
narrative_ontology:cs_authority_grounding('b705baba-6536-436a-a5b8-850ff6c0017d', lineage).
narrative_ontology:cs_interpretation_layer_present('b705baba-6536-436a-a5b8-850ff6c0017d').
narrative_ontology:cs_reading_relation('b705baba-6536-436a-a5b8-850ff6c0017d', marriage_sacrament__civic_pastoral_reading, coexists_with).
narrative_ontology:cs_axiom('b705baba-6536-436a-a5b8-850ff6c0017d', foundational, marriage_ontologically_indissoluble).
narrative_ontology:cs_axiom_status(marriage_ontologically_indissoluble, holdable).
narrative_ontology:cs_axiom_grounding('b705baba-6536-436a-a5b8-850ff6c0017d', marriage_ontologically_indissoluble, theological).
narrative_ontology:cs_axiom('b705baba-6536-436a-a5b8-850ff6c0017d', foundational, ecclesiastical_hierarchy_sole_arbiter).
narrative_ontology:cs_axiom_status(ecclesiastical_hierarchy_sole_arbiter, holdable).
narrative_ontology:cs_axiom_grounding('b705baba-6536-436a-a5b8-850ff6c0017d', ecclesiastical_hierarchy_sole_arbiter, conventional).
narrative_ontology:cs_reference_frame('b705baba-6536-436a-a5b8-850ff6c0017d', tridentine_doctrinal_purity).
narrative_ontology:cs_drift_state('b705baba-6536-436a-a5b8-850ff6c0017d', contemporary_pastoral_challenges, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('b705baba-6536-436a-a5b8-850ff6c0017d', '').
narrative_ontology:cs_kernel_id(marriage_sacrament__hierarchical_indissolubility_reading, marriage_sacrament).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_sacrament__hierarchical_indissolubility_reading, ecclesiastical_hierarchy).
narrative_ontology:constraint_beneficiary(marriage_sacrament__hierarchical_indissolubility_reading, traditionalist_catholics).
narrative_ontology:constraint_victim(marriage_sacrament__hierarchical_indissolubility_reading, divorced_remarried_catholics).
narrative_ontology:constraint_victim(marriage_sacrament__hierarchical_indissolubility_reading, catholics_seeking_annulment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines, interprets, and enforces the doctrine of marriage indissolubility. Administers the annulment process and determines sacramental access. Benefits from maintaining doctrinal purity and institutional authority.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, ecclesiastical_hierarchy, agenda_setter,
    institutional, generational, arbitrage, global).

% Are excluded from full sacramental participation (e.g., Eucharist) unless their prior marriage is annulled. Bear the social and spiritual cost of this exclusion. Their identity as Catholics makes leaving the Church a profound loss.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, divorced_remarried_catholics, payer,
    powerless, biographical, identity_locked, local).

% Must navigate a lengthy, costly, and often emotionally taxing tribunal process to have their prior marriage declared null. Bear the financial and psychological costs, with no guarantee of a favorable outcome.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, catholics_seeking_annulment, payer,
    moderate, immediate, constrained, local).

% Benefit from the strict enforcement of indissolubility, which aligns with their theological convictions and reinforces the perceived stability and authority of the Church. Their adherence to this reading is a source of identity and community.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, traditionalist_catholics, beneficiary,
    organized, generational, mobile, global).

% Are on the front lines of applying this doctrine, often mediating between the hierarchy's strictures and the lived realities of their parishioners. They observe the impact of the constraint but have limited power to alter its fundamental interpretation.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, pastoral_clergy, observer,
    moderate, biographical, constrained, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_sacrament__hierarchical_indissolubility_reading, ecclesiastical_hierarchy).
narrative_ontology:fixing_cost_class(marriage_sacrament__hierarchical_indissolubility_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a universal, unchanging definition of marriage within the Church, providing a clear framework for moral and sacramental life, and reinforcing the Church's authority as the arbiter of divine law.
% TRANSFER_FUNCTION: Transfers spiritual and social capital (sacramental access, full community standing) from divorced/remarried Catholics to the ecclesiastical hierarchy, which controls the conditions of access and benefits from the reinforcement of its doctrinal authority.
% ABSENT_VOICES: Many Catholics who have divorced and remarried outside the Church's recognition, or those who have left the Church due to its stance, are effectively silenced. They would advocate for a more compassionate, less juridical approach to marriage and remarriage, but their perspectives are not formally integrated into the hierarchical adjudication process.
% DISAPPEARANCE_RATIONALE: If the hierarchical enforcement of indissolubility vanished overnight, the Church's authority over marriage would be fundamentally altered. Divorced and remarried Catholics would likely seek full sacramental participation without annulment, leading to a significant shift in pastoral practice and a challenge to the traditional understanding of marriage within the Church.
% FOUNDING_PROBLEM: To uphold the sanctity and permanence of marriage as a divine institution, reflecting Christ's union with the Church, and to provide a stable moral framework for family life.
% FOUNDING_PROBLEM_CORROBORATION: The ecclesiastical hierarchy and traditionalist Catholics attest that the problem of maintaining marriage's sanctity and permanence is still live, citing societal trends towards divorce and secularization. Critics, including some theologians and pastoral clergy, argue that while the ideal remains, the current juridical enforcement creates more pastoral problems than it solves, suggesting the 'problem' has shifted from doctrinal purity to pastoral care.
narrative_ontology:disappearance_verdict(marriage_sacrament__hierarchical_indissolubility_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_sacrament__hierarchical_indissolubility_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_sacrament__hierarchical_indissolubility_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(marriage_sacrament__hierarchical_indissolubility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_sacrament__hierarchical_indissolubility_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.85) due to the severe spiritual and social costs imposed on divorced/remarried Catholics, including exclusion from the Eucharist and the burden of the annulment process. Suppression is also very high (0.92) because the Church's authority is absolute in this domain, with no legitimate internal alternatives for those who wish to remain Catholic and remarry. Accessibility collapse is high (0.8) as the only 'exit' for full sacramental participation is the annulment process, which is tightly controlled and often prohibitive. Resistance is moderate (0.4) but largely internal or pastoral, not directly challenging the core doctrine. Theater ratio is low (0.1) as the enforcement is genuine and directly tied to core doctrinal claims, not merely performative.
 *
 * PERSPECTIVAL GAP:
 *   The ecclesiastical hierarchy perceives this constraint as a necessary defense of divine truth and institutional integrity, a 'rope' for spiritual guidance. Divorced/remarried Catholics experience it as a 'snare' that traps them between their lived reality and their desire for full participation in their faith. The engine's classification as 'snare' from the victim's seat captures this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The ecclesiastical hierarchy and traditionalist Catholics are beneficiaries, as this reading reinforces their authority and theological convictions. Divorced/remarried Catholics and those seeking annulment are clear victims, bearing the direct costs of exclusion and the annulment process. Pastoral clergy are observers, mediating the constraint's impact without directly benefiting or being victimized by its core mechanism. The 'identity_locked' exit option for divorced/remarried Catholics reflects the profound difficulty of leaving the Church for those whose identity is deeply intertwined with their Catholic faith.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pastoral_vs_juridical_priority,
    'Is the primary purpose of the Church''s marriage doctrine juridical enforcement of an ontological reality, or pastoral care for individuals in complex marital situations?',
    'Analysis of papal pronouncements and synodal documents over time: a shift towards prioritizing pastoral discernment and mercy would indicate a re-framing of the constraint''s core purpose.',
    'If pastoral care is prioritized, the constraint''s extractiveness and suppression would likely decrease, potentially reclassifying it towards a Tangled Rope or even a Rope, as the focus shifts from exclusion to integration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pastoral_vs_juridical_priority, conceptual, 'Ambiguity in the Church''s primary emphasis regarding marriage doctrine.').

omega_variable(
    annulment_process_efficacy,
    'Does the annulment process genuinely discern the nullity of a marriage from its inception, or does it function as a de facto ''Catholic divorce'' for those with resources and persistence?',
    'Empirical study of annulment outcomes, processing times, and costs across different dioceses, correlated with socio-economic factors of petitioners.',
    'If the process is found to be disproportionately accessible to the wealthy or to function as a de facto divorce, the ''theater_ratio'' would increase, and the ''extractiveness'' would be seen as more arbitrary, strengthening the Snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(annulment_process_efficacy, empirical, 'Effectiveness and equity of the annulment process.').

omega_variable(
    kernel_reading_divergence,
    'How fundamentally does this ''hierarchical indissolubility'' reading diverge from the ''civic_pastoral'' reading in its practical application and impact on individuals?',
    'Comparative case studies of pastoral practice in dioceses adhering to each reading, focusing on sacramental access for divorced/remarried Catholics and the role of conscience.',
    'If the divergence is found to be substantial and irreconcilable in practice, it reinforces the need for separate constraint stories. If practical convergence is observed despite doctrinal differences, it suggests the kernel itself may be more flexible than either reading claims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'Structural and practical differences between sibling readings of the marriage sacrament kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_sacrament__hierarchical_indissolubility_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(marr_tr_t10, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(marr_tr_t20, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(marr_tr_t30, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 30, 0.1).
narrative_ontology:measurement(marr_tr_t40, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(marr_tr_t50, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 0, 0.8).
narrative_ontology:measurement(marr_be_t10, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 10, 0.82).
narrative_ontology:measurement(marr_be_t20, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 20, 0.83).
narrative_ontology:measurement(marr_be_t30, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 30, 0.84).
narrative_ontology:measurement(marr_be_t40, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 40, 0.85).
narrative_ontology:measurement(marr_be_t50, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 50, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 0, 0.88).
narrative_ontology:measurement(marr_su_t10, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 10, 0.89).
narrative_ontology:measurement(marr_su_t20, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 20, 0.9).
narrative_ontology:measurement(marr_su_t30, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 30, 0.91).
narrative_ontology:measurement(marr_su_t40, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 40, 0.92).
narrative_ontology:measurement(marr_su_t50, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 50, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
