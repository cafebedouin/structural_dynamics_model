% ============================================================================
% CONSTRAINT STORY: jati_practice_norm__orthodox_textual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jati_practice_norm__orthodox_textual_reading, []).

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
 *   constraint_id: jati_practice_norm__orthodox_textual_reading
 *   human_readable: Jati Boundaries (Orthodox Textual Reading)
 *   domain: social_anthropology/religious_studies/political_economy
 *
 * SUMMARY:
 *   This constraint is the 'orthodox_textual_reading' of the
 *   'jati_practice_norm' kernel. It posits that jati boundaries are fixed by
 *   scriptural varna frameworks, and deviation constitutes ritual pollution.
 *   This reading emphasizes the divine origin and immutable nature of the
 *   hierarchy. Sibling readings include 'localized_practice_reading'
 *   (emphasizing local negotiation and fluidity) and
 *   'colonial_census_reading' (emphasizing external reification by
 *   administrative apparatus).
 *
 * KEY AGENTS:
 *   - brahmin_priesthood: Primary agenda_setter (institutional/arbitrage) — benefits from maintaining the system.
 *   - upper_varna_jatis: Primary beneficiary (powerful/mobile) — benefits from social status and economic advantages.
 *   - lower_varna_jatis: Primary payer (powerless/identity_locked) — bears the burden of ritual pollution and blocked mobility.
 *   - dalits: Primary payer (powerless/trapped) — bears the most severe forms of exclusion.
 *   - traditional_authority_structures: Secondary agenda_setter (institutional/constrained) — enforces norms locally.
 *   - social_reformers: Analytical observer (organized/analytical) — critiques the system.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jati_practice_norm__orthodox_textual_reading, 0.85).
domain_priors:suppression_score(jati_practice_norm__orthodox_textual_reading, 0.9).
domain_priors:theater_ratio(jati_practice_norm__orthodox_textual_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jati_practice_norm__orthodox_textual_reading, snare).
narrative_ontology:human_readable(jati_practice_norm__orthodox_textual_reading, "Jati Boundaries (Orthodox Textual Reading)").
narrative_ontology:topic_domain(jati_practice_norm__orthodox_textual_reading, "social_anthropology/religious_studies/political_economy").

domain_priors:requires_active_enforcement(jati_practice_norm__orthodox_textual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jati_practice_norm__orthodox_textual_reading, '23a318f3-ed4c-425e-b10a-1dba86f74e80').
narrative_ontology:cs_kernel_codification('23a318f3-ed4c-425e-b10a-1dba86f74e80', fixed_text).
narrative_ontology:cs_authority_grounding('23a318f3-ed4c-425e-b10a-1dba86f74e80', lineage).
narrative_ontology:cs_interpretation_layer_present('23a318f3-ed4c-425e-b10a-1dba86f74e80').
narrative_ontology:cs_reading_relation('23a318f3-ed4c-425e-b10a-1dba86f74e80', jati_practice_norm__localized_practice_reading, forecloses).
narrative_ontology:cs_reading_relation('23a318f3-ed4c-425e-b10a-1dba86f74e80', jati_practice_norm__colonial_census_reading, coexists_with).
narrative_ontology:cs_axiom('23a318f3-ed4c-425e-b10a-1dba86f74e80', foundational, varna_is_divinely_ordained).
narrative_ontology:cs_axiom_status(varna_is_divinely_ordained, holdable).
narrative_ontology:cs_axiom_grounding('23a318f3-ed4c-425e-b10a-1dba86f74e80', varna_is_divinely_ordained, theological).
narrative_ontology:cs_axiom('23a318f3-ed4c-425e-b10a-1dba86f74e80', foundational, ritual_purity_hierarchy_is_immutable).
narrative_ontology:cs_axiom_status(ritual_purity_hierarchy_is_immutable, holdable).
narrative_ontology:cs_axiom_grounding('23a318f3-ed4c-425e-b10a-1dba86f74e80', ritual_purity_hierarchy_is_immutable, deontological).
narrative_ontology:cs_reference_frame('23a318f3-ed4c-425e-b10a-1dba86f74e80', divinely_ordained_varna_hierarchy).
narrative_ontology:cs_drift_state('23a318f3-ed4c-425e-b10a-1dba86f74e80', contemporary_secular_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('23a318f3-ed4c-425e-b10a-1dba86f74e80', '').
narrative_ontology:cs_kernel_id(jati_practice_norm__orthodox_textual_reading, jati_practice_norm).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jati_practice_norm__orthodox_textual_reading, brahmin_priesthood).
narrative_ontology:constraint_beneficiary(jati_practice_norm__orthodox_textual_reading, upper_varna_jatis).
narrative_ontology:constraint_beneficiary(jati_practice_norm__orthodox_textual_reading, traditional_authority_structures).
narrative_ontology:constraint_victim(jati_practice_norm__orthodox_textual_reading, lower_varna_jatis).
narrative_ontology:constraint_victim(jati_practice_norm__orthodox_textual_reading, dalits).
narrative_ontology:constraint_victim(jati_practice_norm__orthodox_textual_reading, deviant_individuals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and enforces scriptural texts, defining and maintaining the varna-jati hierarchy. Benefits from ritual authority, social deference, and economic support derived from this position.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, brahmin_priesthood, agenda_setter,
    institutional, generational, arbitrage, regional).

% Benefit from higher social status, ritual purity, and preferential access to resources, education, and occupations. Their position is legitimized by the scriptural framework.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, upper_varna_jatis, beneficiary,
    powerful, generational, mobile, regional).

% Bear the burden of ritual impurity, restricted social interaction, and assigned occupations often deemed polluting. Their mobility is severely blocked, and deviation leads to social ostracism.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, lower_varna_jatis, payer,
    powerless, biographical, identity_locked, local).

% Experience the most extreme forms of exclusion, discrimination, and violence, often considered outside the varna system entirely. Their situation is one of profound social and economic entrapment.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, dalits, payer,
    powerless, biographical, trapped, local).

% Individuals who attempt to cross jati boundaries, marry outside their group, or reject traditional occupations. They face severe social sanctions, ostracism, and loss of community support.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, deviant_individuals, payer,
    powerless, immediate, constrained, local).

% Village councils, community elders, and religious bodies that actively enforce jati norms through social pressure, fines, and excommunication, ensuring compliance with the scriptural framework.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, traditional_authority_structures, agenda_setter,
    institutional, generational, constrained, local).

% Advocate for the abolition or reform of the jati system, challenging its scriptural legitimacy and highlighting its discriminatory practices. They face significant resistance from traditional authorities.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, social_reformers, observer,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jati_practice_norm__orthodox_textual_reading, brahmin_priesthood).
narrative_ontology:fixing_cost_class(jati_practice_norm__orthodox_textual_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a rigid social order, division of labor, and ritual purity hierarchy, providing a clear (though unequal) framework for social interaction, marriage patterns, and religious practice based on scriptural interpretations.
% TRANSFER_FUNCTION: Transfers social status, ritual purity, economic opportunity, and political power from lower varna jatis and Dalits to upper varna jatis and the Brahmin priesthood, justified by a divinely ordained hierarchy.
% ABSENT_VOICES: Dalits and lower varna jatis are structurally excluded from the discourse that defines and legitimizes the system. Their historical and ongoing objections are suppressed by social sanction, economic dependency, and lack of institutional power.
% DISAPPEARANCE_RATIONALE: If the scripturally derived jati boundaries and their enforcement vanished overnight, the entire social, economic, and religious fabric of many communities would undergo a fundamental and chaotic reorganization. Labor, social status, marriage patterns, and religious authority would all need to be redefined, leading to widespread social upheaval.
% FOUNDING_PROBLEM: To maintain ritual purity, social order, and a divinely ordained hierarchy, preventing chaos, spiritual degradation, and the mixing of varna (social categories) as prescribed by sacred texts.
% FOUNDING_PROBLEM_CORROBORATION: The Brahmin priesthood and upper varna jatis assert that the problem of maintaining dharma and social order is still live and the system is essential. Social reformers, human rights organizations, and independent historical analyses from outside the benefiting parties argue that the founding problem is largely a justification for maintaining an extractive system, and that the original problem (if it ever existed as claimed) is long dead or fundamentally reinterpreted.
narrative_ontology:disappearance_verdict(jati_practice_norm__orthodox_textual_reading, world_rearranges).
narrative_ontology:founding_problem_status(jati_practice_norm__orthodox_textual_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jati_practice_norm__orthodox_textual_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(jati_practice_norm__orthodox_textual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jati_practice_norm__orthodox_textual_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jati_practice_norm__orthodox_textual_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jati_practice_norm__orthodox_textual_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jati_practice_norm__orthodox_textual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) due to the systemic transfer of resources, status, and opportunities from lower to upper jatis. Suppression is very high (0.90) because the system relies on severe social sanctions, ritual ostracism, and blocked mobility to prevent deviation and maintain the hierarchy. Theater ratio is moderate (0.40) as there is a genuine belief in ritual purity and scriptural authority, but also a performative aspect to maintaining social control and justifying the extractive structure. Accessibility collapse is high (0.88) as alternatives to the prescribed social order are severely limited and punished.
 *
 * PERSPECTIVAL GAP:
 *   The Brahmin priesthood and upper varna jatis perceive this as a divinely ordained, necessary social order that ensures dharma and stability. Lower varna jatis and Dalits experience it as a deeply oppressive and extractive system that denies them basic human dignity and opportunity. The engine's per-seat classification will reflect this divergence, with beneficiaries seeing a 'rope' or 'mountain' and victims experiencing a 'snare'.
 *
 * DIRECTIONALITY LOGIC:
 *   The Brahmin priesthood and upper varna jatis are clear beneficiaries (low d) as they gain status, power, and resources. Lower varna jatis, Dalits, and deviant individuals are clear targets (high d) as they bear the costs of exclusion, discrimination, and blocked mobility. Traditional authority structures act as agenda-setters, enforcing the system from which they also derive legitimacy and some benefit. Social reformers are analytical observers, outside the direct flow of extraction but working to dismantle it.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a 'snare' prevents mislabeling this as a 'rope' or 'mountain'. While a coordination function (social order, division of labor) is claimed, the high extractiveness, severe suppression, and identifiable victims demonstrate that the coordination story serves as cover for a system of asymmetric extraction. The 'contested' status of the founding problem further supports this, indicating that the original mandate is no longer universally accepted as legitimate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scriptural_interpretation_ambiguity,
    'To what extent is the ''fixed scriptural varna framework'' truly immutable, or is it subject to diverse interpretations and historical re-readings?',
    'Comparative textual analysis of different scriptural commentaries across historical periods, and ethnographic studies of diverse interpretive traditions within contemporary Hinduism.',
    'If the framework is found to be highly interpretive, it weakens the ''fixed_text'' codification and ''lineage'' authority grounding, potentially reclassifying the constraint as more ''distributed'' or ''implicit'' in its kernel, and shifting its type towards a ''tangled_rope'' or ''piton'' if the extraction is found to be less directly tied to immutable text.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scriptural_interpretation_ambiguity, conceptual, 'Ambiguity in the immutability and interpretation of foundational religious texts.').

omega_variable(
    ritual_pollution_belief_vs_control,
    'To what extent is the concept of ''ritual pollution'' a genuine, deeply held religious belief, versus a social construct primarily serving as a mechanism for social control and maintaining hierarchy?',
    'Sociological and psychological studies exploring the lived experience and internal beliefs of individuals across different jatis, alongside historical analysis of the evolution of purity norms in relation to power structures.',
    'If primarily a tool for social control, the suppression metric''s ''internalized'' component would be higher, and the constraint''s extractiveness would be seen as more deliberate and less ''naturalized'' by belief, reinforcing its ''snare'' classification. If a genuine belief, it complicates the analysis of agency and consent within the system.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ritual_pollution_belief_vs_control, empirical, 'Distinguishing genuine religious belief from social control in the concept of ritual pollution.').

omega_variable(
    historical_origin_vs_claimed_origin,
    'What is the actual historical origin and evolution of jati boundaries, and how does it diverge from the claimed fixed scriptural varna framework?',
    'Archaeological, linguistic, and historical research tracing the development of social stratification in ancient and medieval India, comparing it with the narrative presented in religious texts.',
    'A significant divergence would undermine the ''lineage'' authority grounding and ''fixed_text'' kernel codification, potentially reclassifying the constraint as a ''tangled_rope'' or ''snare'' that has successfully naturalized a constructed history, rather than genuinely emerging from a fixed, ancient source.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_origin_vs_claimed_origin, empirical, 'Discrepancy between the historical and claimed origins of the jati system.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jati_practice_norm__orthodox_textual_reading, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jati_tr_t0, jati_practice_norm__orthodox_textual_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(jati_tr_t400, jati_practice_norm__orthodox_textual_reading, theater_ratio, 400, 0.35).
narrative_ontology:measurement(jati_tr_t800, jati_practice_norm__orthodox_textual_reading, theater_ratio, 800, 0.4).
narrative_ontology:measurement(jati_tr_t1200, jati_practice_norm__orthodox_textual_reading, theater_ratio, 1200, 0.42).
narrative_ontology:measurement(jati_tr_t1600, jati_practice_norm__orthodox_textual_reading, theater_ratio, 1600, 0.4).
narrative_ontology:measurement(jati_tr_t2000, jati_practice_norm__orthodox_textual_reading, theater_ratio, 2000, 0.4).

% Extraction over time
narrative_ontology:measurement(jati_be_t0, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(jati_be_t400, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 400, 0.75).
narrative_ontology:measurement(jati_be_t800, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 800, 0.8).
narrative_ontology:measurement(jati_be_t1200, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 1200, 0.83).
narrative_ontology:measurement(jati_be_t1600, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 1600, 0.86).
narrative_ontology:measurement(jati_be_t2000, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 2000, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(jati_su_t0, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(jati_su_t400, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 400, 0.8).
narrative_ontology:measurement(jati_su_t800, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 800, 0.85).
narrative_ontology:measurement(jati_su_t1200, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 1200, 0.88).
narrative_ontology:measurement(jati_su_t1600, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 1600, 0.9).
narrative_ontology:measurement(jati_su_t2000, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 2000, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jati_practice_norm__orthodox_textual_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
