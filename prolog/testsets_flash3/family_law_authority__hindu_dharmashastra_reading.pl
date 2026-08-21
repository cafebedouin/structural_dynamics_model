% ============================================================================
% CONSTRAINT STORY: family_law_authority__hindu_dharmashastra_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_family_law_authority__hindu_dharmashastra_reading, []).

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
 *   constraint_id: family_law_authority__hindu_dharmashastra_reading
 *   human_readable: Hindu Dharmashastra Reading of Marriage as Sacramental Samskara
 *   domain: comparative_law/political_theory/religious_governance
 *
 * SUMMARY:
 *   This constraint models the Hindu Dharmashastra reading of marriage as a
 *   sacramental samskara (sacred rite) in India, particularly in the period
 *   before the Hindu Marriage Act of 1955. It emphasizes indissolubility,
 *   caste endogamy, joint family property rules, and the wife's role as a
 *   ritual participant rather than an autonomous contractor. This reading is
 *   one of several competing interpretations of family law authority in
 *   India, each forming a distinct constraint.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__hindu_dharmashastra_reading, 0.65).
domain_priors:suppression_score(family_law_authority__hindu_dharmashastra_reading, 0.7).
domain_priors:theater_ratio(family_law_authority__hindu_dharmashastra_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__hindu_dharmashastra_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__hindu_dharmashastra_reading, "Hindu Dharmashastra Reading of Marriage as Sacramental Samskara").
narrative_ontology:topic_domain(family_law_authority__hindu_dharmashastra_reading, "comparative_law/political_theory/religious_governance").

domain_priors:requires_active_enforcement(family_law_authority__hindu_dharmashastra_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__hindu_dharmashastra_reading, 'a0feb8ca-24d4-4ff4-8673-4fab00ed2d4b').
narrative_ontology:cs_kernel_codification('a0feb8ca-24d4-4ff4-8673-4fab00ed2d4b', formalized).
narrative_ontology:cs_authority_grounding('a0feb8ca-24d4-4ff4-8673-4fab00ed2d4b', lineage).
narrative_ontology:cs_interpretation_layer_present('a0feb8ca-24d4-4ff4-8673-4fab00ed2d4b').
narrative_ontology:cs_reading_relation('a0feb8ca-24d4-4ff4-8673-4fab00ed2d4b', family_law_authority__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('a0feb8ca-24d4-4ff4-8673-4fab00ed2d4b', family_law_authority__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('a0feb8ca-24d4-4ff4-8673-4fab00ed2d4b', family_law_authority__parsi_zoroastrian_reading, coexists_with).
narrative_ontology:cs_reading_relation('a0feb8ca-24d4-4ff4-8673-4fab00ed2d4b', family_law_authority__secular_contractual_reading, influences).
narrative_ontology:cs_axiom('a0feb8ca-24d4-4ff4-8673-4fab00ed2d4b', foundational, marriage_as_indissoluble_samskara).
narrative_ontology:cs_axiom_status(marriage_as_indissoluble_samskara, holdable).
narrative_ontology:cs_axiom_grounding('a0feb8ca-24d4-4ff4-8673-4fab00ed2d4b', marriage_as_indissoluble_samskara, theological).
narrative_ontology:cs_axiom('a0feb8ca-24d4-4ff4-8673-4fab00ed2d4b', foundational, caste_endogamy_as_dharmic_duty).
narrative_ontology:cs_axiom_status(caste_endogamy_as_dharmic_duty, holdable).
narrative_ontology:cs_axiom_grounding('a0feb8ca-24d4-4ff4-8673-4fab00ed2d4b', caste_endogamy_as_dharmic_duty, conventional).
narrative_ontology:cs_reference_frame('a0feb8ca-24d4-4ff4-8673-4fab00ed2d4b', traditional_dharmic_order).
narrative_ontology:cs_drift_state('a0feb8ca-24d4-4ff4-8673-4fab00ed2d4b', post_1955_legal_reforms, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a0feb8ca-24d4-4ff4-8673-4fab00ed2d4b', '').
narrative_ontology:cs_kernel_id(family_law_authority__hindu_dharmashastra_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__hindu_dharmashastra_reading, joint_family_elders).
narrative_ontology:constraint_beneficiary(family_law_authority__hindu_dharmashastra_reading, caste_community_leaders).
narrative_ontology:constraint_victim(family_law_authority__hindu_dharmashastra_reading, hindu_women).
narrative_ontology:constraint_victim(family_law_authority__hindu_dharmashastra_reading, lower_caste_individuals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(family_law_authority__hindu_dharmashastra_reading, hindu_men).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and enforce dharmic texts and customary practices related to marriage, property, and family roles. They benefit from the stability and continuity of the joint family structure and the social capital derived from adherence to tradition.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, joint_family_elders, agenda_setter,
    institutional, generational, constrained, local).

% Uphold caste endogamy norms and other community-specific marriage customs. They derive authority from maintaining the social order and purity of the caste, often imposing social sanctions for non-compliance.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, caste_community_leaders, agenda_setter,
    organized, generational, constrained, regional).

% Are primarily defined by their role as ritual participants and bearers of lineage, with limited autonomy in marital choice, property rights, and divorce (especially pre-1955). Their identity is deeply intertwined with their marital and family status, making exit extremely costly socially and economically.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, hindu_women, payer,
    powerless, biographical, identity_locked, local).

% Face severe social and economic penalties for violating caste endogamy norms, including ostracization and violence. Their options are limited by deeply entrenched social hierarchies and lack of institutional support for inter-caste marriage.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, lower_caste_individuals, payer,
    powerless, biographical, trapped, local).

% Benefit from the stability of the joint family, the perpetuation of lineage, and the ritual participation of their wives. They have more agency in marital decisions and property matters than women, but are still bound by caste and family expectations.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, hindu_men, beneficiary,
    moderate, biographical, constrained, local).

% Interprets and applies modern Indian family law (e.g., Hindu Marriage Act of 1955) which has codified and reformed many aspects of Dharmashastra, introducing divorce rights and equalizing property. They observe the tension between traditional practices and statutory law.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, secular_state_judiciary, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(family_law_authority__hindu_dharmashastra_reading, joint_family_elders).
narrative_ontology:fixing_cost_class(family_law_authority__hindu_dharmashastra_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates social order, lineage continuity, and ritual purity within Hindu communities by defining marriage as an indissoluble sacred bond (samskara) and regulating family structure and caste relations.
% TRANSFER_FUNCTION: Transfers social status, ritual obligations, and property rights according to dharmic principles, primarily from women and lower-caste individuals to joint family elders and caste community leaders.
% ABSENT_VOICES: Individuals seeking inter-caste or inter-religious marriages, women seeking autonomy in marital decisions or divorce, and those advocating for individual contractual rights over sacramental obligations are structurally excluded from the traditional interpretive framework. Their voices are heard in secular legal forums but not within the Dharmashastra's authority structure.
% DISAPPEARANCE_RATIONALE: If the Dharmashastra's authority over marriage vanished overnight, the social fabric of many Hindu communities would undergo profound reorganization. Caste endogamy would weaken, individual autonomy in marriage would increase, and the joint family system would face significant challenges, leading to a shift towards more contractual and individualistic marital arrangements.
% FOUNDING_PROBLEM: To establish a stable social and cosmic order (dharma) by regulating the most fundamental social unit (the family) and ensuring the proper performance of rituals and perpetuation of lineage.
% FOUNDING_PROBLEM_CORROBORATION: Traditionalists and religious scholars attest the problem is still live, emphasizing the need for dharma and social stability. Secular legal scholars and human rights advocates attest that while social order is important, the specific mechanisms of Dharmashastra (e.g., indissolubility, caste endogamy) are outdated and often harmful, with the problem of social order now addressed by modern statutory law. Independent sociological studies document the ongoing tension and reform efforts.
narrative_ontology:disappearance_verdict(family_law_authority__hindu_dharmashastra_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__hindu_dharmashastra_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__hindu_dharmashastra_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(family_law_authority__hindu_dharmashastra_reading, 'none', 1).
narrative_ontology:epsilon_provenance(family_law_authority__hindu_dharmashastra_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(family_law_authority__hindu_dharmashastra_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(family_law_authority__hindu_dharmashastra_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(family_law_authority__hindu_dharmashastra_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.65) due to the significant limitations placed on individual autonomy, particularly for women and lower-caste individuals, regarding marital choice, divorce, and property rights. Suppression (0.70) is also high, maintained through social ostracization, community pressure, and the deep intertwining of identity with traditional roles. Theater ratio is low (0.20) as the rituals and practices are genuinely held as sacred and functional within the traditional framework, though their justification is increasingly contested by secular legal frameworks. The slight decrease in suppression towards 1955 reflects growing social reform movements and the anticipation of statutory changes.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of joint family elders and caste leaders, this constraint is a necessary framework for social and cosmic order, a 'rope' of tradition. From the perspective of Hindu women and lower-caste individuals, it operates as a 'snare' of enforced roles and limited rights. The engine's per-seat classification will reflect this divergence based on the declared power, exit options, and beneficiary/victim status of each stakeholder.
 *
 * DIRECTIONALITY LOGIC:
 *   Joint family elders and caste community leaders are beneficiaries and agenda-setters, as they derive authority and social capital from upholding these traditions. Hindu women and lower-caste individuals are victims, bearing the primary costs of restricted autonomy and social sanctions. Hindu men are beneficiaries of lineage continuity and ritual stability, though also constrained by tradition. The secular state judiciary acts as an observer, analyzing the constraint's operation against modern legal principles.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sacramental_vs_contractual_nature,
    'Is marriage fundamentally a sacramental samskara (indissoluble sacred rite) or a civil contract between autonomous individuals?',
    'Legal reform and judicial precedent: the Hindu Marriage Act of 1955 partially resolved this by introducing divorce and codifying rights, but the underlying conceptual tension persists in social practice.',
    'If primarily sacramental, indissolubility and ritual roles are inherent; if contractual, individual autonomy and exit options are paramount, reclassifying the constraint towards a ''rope'' or ''scaffold'' for individual choice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sacramental_vs_contractual_nature, conceptual, 'Conceptual ambiguity regarding the fundamental nature of marriage.').

omega_variable(
    caste_endogamy_enforcement_mechanism,
    'To what extent is caste endogamy enforced through internalized norms versus overt social sanctions and violence?',
    'Sociological studies on inter-caste marriage outcomes, legal enforcement of anti-discrimination laws, and shifts in community acceptance over time.',
    'If primarily internalized, suppression is higher and more difficult to dislodge, even with legal reforms. If overt, legal and social interventions can more directly reduce suppression and increase accessibility to alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(caste_endogamy_enforcement_mechanism, empirical, 'Structural vs. internalized suppression mechanism for caste endogamy.').

omega_variable(
    mandatrophy_of_indissolubility,
    'Has the original mandate for sacramental indissolubility outlived its function in contemporary society, given changing social norms and legal reforms?',
    'Analysis of divorce rates, women''s economic independence, and public opinion surveys on marital expectations. The Hindu Marriage Act of 1955 already introduced divorce, indicating a partial resolution.',
    'If the mandate is dead, the persistence of indissolubility norms (where they still exist socially) would reclassify the constraint towards a ''piton'' or ''snare'' sustained by inertia or concentrated extraction, rather than genuine coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandatrophy_of_indissolubility, empirical, 'Whether the mandate for indissolubility has atrophied.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__hindu_dharmashastra_reading, 1900, 1955).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fami_tr_t1900, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 1900, 0.1).
narrative_ontology:measurement(fami_tr_t1910, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 1910, 0.12).
narrative_ontology:measurement(fami_tr_t1920, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 1920, 0.15).
narrative_ontology:measurement(fami_tr_t1930, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 1930, 0.18).
narrative_ontology:measurement(fami_tr_t1940, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 1940, 0.2).
narrative_ontology:measurement(fami_tr_t1955, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 1955, 0.2).

% Extraction over time
narrative_ontology:measurement(fami_be_t1900, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 1900, 0.6).
narrative_ontology:measurement(fami_be_t1910, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 1910, 0.62).
narrative_ontology:measurement(fami_be_t1920, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 1920, 0.64).
narrative_ontology:measurement(fami_be_t1930, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 1930, 0.66).
narrative_ontology:measurement(fami_be_t1940, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 1940, 0.67).
narrative_ontology:measurement(fami_be_t1955, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 1955, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(fami_su_t1900, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 1900, 0.75).
narrative_ontology:measurement(fami_su_t1910, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 1910, 0.73).
narrative_ontology:measurement(fami_su_t1920, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 1920, 0.71).
narrative_ontology:measurement(fami_su_t1930, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 1930, 0.69).
narrative_ontology:measurement(fami_su_t1940, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 1940, 0.68).
narrative_ontology:measurement(fami_su_t1955, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 1955, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__hindu_dharmashastra_reading, identity_coordination).
narrative_ontology:affects_constraint(family_law_authority__hindu_dharmashastra_reading, family_law_authority__muslim_shariat_reading).
narrative_ontology:affects_constraint(family_law_authority__hindu_dharmashastra_reading, family_law_authority__christian_canonical_reading).
narrative_ontology:affects_constraint(family_law_authority__hindu_dharmashastra_reading, family_law_authority__parsi_zoroastrian_reading).
narrative_ontology:affects_constraint(family_law_authority__hindu_dharmashastra_reading, family_law_authority__secular_contractual_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'family_law_authority' kernel. Its structural properties, beneficiaries, and victims differ significantly from other readings (e.g., Muslim Shariat, Christian Canonical, Parsi Zoroastrian, or Secular Contractual), necessitating separate constraint stories. This reading emphasizes sacramental indissolubility and caste endogamy, which are distinct from the contractual or ecclesiastical framings of other traditions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
