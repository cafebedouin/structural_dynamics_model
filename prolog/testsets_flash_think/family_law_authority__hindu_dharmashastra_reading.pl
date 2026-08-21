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
 *   human_readable: Hindu Marriage as Sacramental Samskara (Dharmashastra Reading)
 *   domain: religious_governance/social_norms/family_law
 *
 * SUMMARY:
 *   This constraint describes the traditional Hindu understanding of marriage
 *   as a sacramental samskara (rite of passage) governed by Dharmashastra
 *   texts and customary practice, particularly as it operated prior to the
 *   Hindu Marriage Act of 1955. It is one reading of the broader
 *   'family_law_authority' kernel. This reading emphasizes indissolubility,
 *   caste endogamy, joint family property rules, and the wife's role as a
 *   ritual participant rather than an autonomous contractor. The claimed type
 *   is 'tangled_rope' because it provides a clear social coordination
 *   function but also involves significant asymmetric extraction and requires
 *   active social and religious enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__hindu_dharmashastra_reading, 0.8).
domain_priors:suppression_score(family_law_authority__hindu_dharmashastra_reading, 0.85).
domain_priors:theater_ratio(family_law_authority__hindu_dharmashastra_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__hindu_dharmashastra_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__hindu_dharmashastra_reading, "Hindu Marriage as Sacramental Samskara (Dharmashastra Reading)").
narrative_ontology:topic_domain(family_law_authority__hindu_dharmashastra_reading, "religious_governance/social_norms/family_law").

domain_priors:requires_active_enforcement(family_law_authority__hindu_dharmashastra_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__hindu_dharmashastra_reading, '6f3b9157-e2af-4947-9c6f-d6ea4ba53f05').
narrative_ontology:cs_kernel_codification('6f3b9157-e2af-4947-9c6f-d6ea4ba53f05', fixed_text).
narrative_ontology:cs_authority_grounding('6f3b9157-e2af-4947-9c6f-d6ea4ba53f05', lineage).
narrative_ontology:cs_interpretation_layer_present('6f3b9157-e2af-4947-9c6f-d6ea4ba53f05').
narrative_ontology:cs_reading_relation('6f3b9157-e2af-4947-9c6f-d6ea4ba53f05', family_law_authority__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('6f3b9157-e2af-4947-9c6f-d6ea4ba53f05', family_law_authority__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('6f3b9157-e2af-4947-9c6f-d6ea4ba53f05', family_law_authority__parsi_zoroastrian_reading, coexists_with).
narrative_ontology:cs_reading_relation('6f3b9157-e2af-4947-9c6f-d6ea4ba53f05', family_law_authority__secular_contractual_reading, forecloses).
narrative_ontology:cs_axiom('6f3b9157-e2af-4947-9c6f-d6ea4ba53f05', foundational, marriage_as_indissoluble_samskara).
narrative_ontology:cs_axiom_status(marriage_as_indissoluble_samskara, holdable).
narrative_ontology:cs_axiom_grounding('6f3b9157-e2af-4947-9c6f-d6ea4ba53f05', marriage_as_indissoluble_samskara, theological).
narrative_ontology:cs_axiom('6f3b9157-e2af-4947-9c6f-d6ea4ba53f05', foundational, caste_endogamy_as_dharma).
narrative_ontology:cs_axiom_status(caste_endogamy_as_dharma, holdable).
narrative_ontology:cs_axiom_grounding('6f3b9157-e2af-4947-9c6f-d6ea4ba53f05', caste_endogamy_as_dharma, conventional).
narrative_ontology:cs_reference_frame('6f3b9157-e2af-4947-9c6f-d6ea4ba53f05', traditional_dharmic_marriage_ideal).
narrative_ontology:cs_drift_state('6f3b9157-e2af-4947-9c6f-d6ea4ba53f05', post_independence_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('6f3b9157-e2af-4947-9c6f-d6ea4ba53f05', '').
narrative_ontology:cs_kernel_id(family_law_authority__hindu_dharmashastra_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__hindu_dharmashastra_reading, dharmic_scholars_and_priests).
narrative_ontology:constraint_beneficiary(family_law_authority__hindu_dharmashastra_reading, dominant_caste_men).
narrative_ontology:constraint_beneficiary(family_law_authority__hindu_dharmashastra_reading, joint_family_elders).
narrative_ontology:constraint_victim(family_law_authority__hindu_dharmashastra_reading, hindu_women).
narrative_ontology:constraint_victim(family_law_authority__hindu_dharmashastra_reading, lower_caste_individuals).
narrative_ontology:constraint_victim(family_law_authority__hindu_dharmashastra_reading, inter_caste_couples).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and transmit dharmic texts, officiate marriage rituals, and enforce customary practices. They derive authority and social standing from maintaining the traditional order.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, dharmic_scholars_and_priests, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from patriarchal property rules, social status derived from lineage, and the prescribed roles of women within the family structure. They are largely unconstrained by the indissoluble nature of marriage.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, dominant_caste_men, beneficiary,
    powerful, biographical, mobile, local).

% Enforce caste endogamy, manage joint family property, and dictate marital arrangements. Their authority within the family is upheld by the dharmic framework.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, joint_family_elders, agenda_setter,
    powerful, biographical, constrained, local).

% Bear the costs of sacramental indissolubility (pre-1955), lack of autonomy in marital choice, exclusion from direct property inheritance, and prescribed ritual roles that limit their agency. Their identity is often fused with their marital and familial status.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, hindu_women, payer,
    powerless, biographical, identity_locked, local).

% Subject to caste endogamy norms, which limit marital choices and reinforce social hierarchies. They face social ostracism and lack of recognition if they transgress these norms.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, lower_caste_individuals, payer,
    powerless, generational, trapped, local).

% Are often socially ostracized and their unions may not be fully recognized or legitimized by traditional authorities, despite individual desires. They are structurally excluded from the 'ideal' dharmic marriage.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, inter_caste_couples, excluded,
    powerless, biographical, trapped, local).

% Observes and, post-1955, actively legislates on family matters, often in tension with traditional dharmic interpretations. From the perspective of this reading, it is an external force that challenges its authority.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, secular_legal_system_india, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(family_law_authority__hindu_dharmashastra_reading, dominant_caste_men).
narrative_ontology:fixing_cost_class(family_law_authority__hindu_dharmashastra_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes social order, family lineage, and religious continuity by prescribing marital rites, roles, and property arrangements, ensuring the continuation of dharma and community structure.
% TRANSFER_FUNCTION: Transfers social status, ritual obligations, and property rights, primarily from women and lower castes to men and dominant families, maintaining existing social hierarchies.
% ABSENT_VOICES: Autonomous women's advocates, caste reformers, and secular law reformers would object to the lack of individual autonomy, gender inequality, and caste-based discrimination inherent in the traditional framework. They are often marginalized or actively suppressed within the traditional discourse.
% DISAPPEARANCE_RATIONALE: If the traditional dharmic understanding of marriage and its enforcement vanished overnight, the entire social and religious fabric of Hindu society, particularly family structures, property inheritance, and caste relations, would undergo a profound and immediate reorganization, leading to widespread social disruption and new forms of family formation.
% FOUNDING_PROBLEM: To ensure social stability, ritual purity, and the continuation of family lines and dharma through religiously sanctioned and socially regulated unions, preventing chaos and the mixing of varnas (social classes).
% FOUNDING_PROBLEM_CORROBORATION: The problem's status is primarily attested as 'live' by dharmic scholars, priests, and joint family elders, who emphasize the ongoing need for religious and social order. However, secular legal scholars and social reformers attest that the specific problems the constraint was built to solve are either dead or can be addressed through more equitable, secular means; their testimony is often found in legislative debates and academic critiques.
narrative_ontology:disappearance_verdict(family_law_authority__hindu_dharmashastra_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__hindu_dharmashastra_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__hindu_dharmashastra_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(family_law_authority__hindu_dharmashastra_reading, 'none', 1).
narrative_ontology:epsilon_provenance(family_law_authority__hindu_dharmashastra_reading, 0.8, 'gemini-2.5-flash', 'none', direct).

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
 *   The high extractiveness (0.8) stems from the indissoluble nature of marriage, the exclusion of women from direct property rights, and the rigid gender and caste roles enforced by religious and social norms. Suppression (0.85) is high due to social ostracism, religious injunctions, and the lack of legal alternatives for those who wished to deviate from prescribed practices. The low theater ratio (0.2) reflects that the rituals and customs were genuinely functional in maintaining the social and religious order, not merely performative. Accessibility collapse is high (0.75) as alternatives were severely limited, especially for women and lower castes. Resistance is moderate (0.4) as overt challenges were often met with severe social consequences, though quiet subversion likely existed.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of its adherents (dharmic scholars, dominant caste men, elders), this constraint is a sacred, divinely ordained order essential for social and religious harmony. From the perspective of its victims (women, lower castes, inter-caste couples), it is an oppressive system that limits their freedom, agency, and rights. The engine's computation of a 'tangled_rope' classification from the authored metrics reflects this structural asymmetry, despite the internal 'mountain' or 'rope' claim by its beneficiaries.
 *
 * DIRECTIONALITY LOGIC:
 *   Dharmic scholars, priests, and joint family elders are beneficiaries and agenda-setters, deriving authority and social capital from upholding the system. Dominant caste men are primary beneficiaries, gaining property rights, social status, and control within the patriarchal structure. Hindu women and lower caste individuals are the primary payers/victims, bearing the costs of limited autonomy, property exclusion, and social restrictions. Inter-caste couples are excluded, facing non-recognition and ostracism. The secular legal system acts as an observer, eventually becoming a challenger to this traditional authority.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate to maintain social and religious order is deeply intertwined with its extractive mechanisms. The 'sacramental' nature of marriage makes claims of mandatrophy difficult to assert from within the framework, as its function is seen as timeless and divinely ordained. However, the high extraction and suppression, coupled with the contested status of its founding problem, suggest that the coordination function serves as a cover for maintaining existing power structures. The post-1955 legal reforms highlight how external forces can expose and challenge this entanglement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sacramental_vs_contractual_nature,
    'Is marriage fundamentally a religious sacrament (samskara) or a civil contract between autonomous individuals?',
    'Conceptual analysis of legal and theological frameworks, and empirical observation of how individuals and institutions prioritize religious vs. secular definitions in practice.',
    'If primarily a sacrament, the constraint''s religious grounding is central, making challenges to its ''naturalness'' difficult. If primarily a contract, its extractive elements are more easily challenged as violations of individual rights, potentially reclassifying it towards a Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sacramental_vs_contractual_nature, conceptual, 'Ambiguity in the fundamental nature of marriage.').

omega_variable(
    impact_of_legal_reform,
    'How did the Hindu Marriage Act of 1955 (introducing divorce, monogamy, etc.) alter the structural nature of the Dharmashastra reading, even if not its ideal?',
    'Empirical study of post-1955 legal cases, social practices, and the evolving interpretations by dharmic authorities regarding the enforceability and social acceptance of traditional norms.',
    'If the Act significantly eroded the practical enforceability of traditional norms, the constraint''s effective suppression and extractiveness would decrease, potentially shifting its classification towards a Piton or a less extractive Tangled Rope. If traditional norms persisted strongly despite legal changes, the Act''s impact on this reading''s structural properties was limited.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(impact_of_legal_reform, empirical, 'Structural impact of secular legal reforms on religious law.').

omega_variable(
    internalized_vs_structural_suppression,
    'To what extent is the suppression experienced by Hindu women and lower castes internalized (e.g., through religious upbringing and social conditioning) versus structural (e.g., lack of legal recourse, social ostracism)?',
    'Sociological studies examining post-exit trajectories and psychological impacts of individuals who attempt to defy traditional norms, as well as analysis of the persistence of traditional beliefs even after legal barriers are removed.',
    'If suppression is largely internalized, the constraint''s effective suppression is higher than external measures suggest, as individuals carry the constraint with them. If primarily structural, legal and social reforms would have a more immediate and direct impact on reducing suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_vs_structural_suppression, empirical, 'Mechanism of suppression (internalized vs. structural).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__hindu_dharmashastra_reading, 1900, 1955).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fami_tr_t1900, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 1900, 0.18).
narrative_ontology:measurement(fami_tr_t1910, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 1910, 0.19).
narrative_ontology:measurement(fami_tr_t1920, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 1920, 0.2).
narrative_ontology:measurement(fami_tr_t1930, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 1930, 0.2).
narrative_ontology:measurement(fami_tr_t1940, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 1940, 0.21).
narrative_ontology:measurement(fami_tr_t1955, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 1955, 0.2).

% Extraction over time
narrative_ontology:measurement(fami_be_t1900, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 1900, 0.78).
narrative_ontology:measurement(fami_be_t1910, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 1910, 0.79).
narrative_ontology:measurement(fami_be_t1920, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 1920, 0.8).
narrative_ontology:measurement(fami_be_t1930, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 1930, 0.8).
narrative_ontology:measurement(fami_be_t1940, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 1940, 0.81).
narrative_ontology:measurement(fami_be_t1955, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 1955, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(fami_su_t1900, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 1900, 0.83).
narrative_ontology:measurement(fami_su_t1910, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 1910, 0.84).
narrative_ontology:measurement(fami_su_t1920, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 1920, 0.85).
narrative_ontology:measurement(fami_su_t1930, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 1930, 0.85).
narrative_ontology:measurement(fami_su_t1940, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 1940, 0.86).
narrative_ontology:measurement(fami_su_t1955, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 1955, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__hindu_dharmashastra_reading, identity_coordination).
narrative_ontology:affects_constraint(family_law_authority__hindu_dharmashastra_reading, caste_system_norms).
narrative_ontology:affects_constraint(family_law_authority__hindu_dharmashastra_reading, hindu_succession_law).
narrative_ontology:affects_constraint(family_law_authority__hindu_dharmashastra_reading, gendered_property_rights).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
