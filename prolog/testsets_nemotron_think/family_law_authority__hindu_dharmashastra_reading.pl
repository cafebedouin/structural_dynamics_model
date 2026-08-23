% ============================================================================
% CONSTRAINT STORY: family_law_authority__hindu_dharmashastra_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: family_law_authority__hindu_dharmashastra_reading
 *   human_readable: Hindu Dharmashastra Marriage as Sacramental Samskara (Pre-1955)
 *   domain: religious_governance/family_law/comparative_law
 *
 * SUMMARY:
 *   The Hindu dharmashastra reading of family law authority treats marriage
 *   as a sacramental samskara (sacrament) — indissoluble, caste-endogamous,
 *   embedded in joint family property rules, with the wife as ritual
 *   participant (saha-dharmini) rather than autonomous contractor. This
 *   reading governed Hindu personal law until the 1955 Hindu Marriage Act
 *   introduced divorce and the 1956 Succession Act reformed property rights.
 *   The constraint exhibits strong coordination functions (property
 *   transmission, ritual continuity, caste order) alongside asymmetric
 *   extraction (gender hierarchy, caste hierarchy, generational hierarchy).
 *   Active enforcement came from religious authorities, family councils,
 *   social ostracism, and colonial courts that codified 'Anglo-Hindu law' by
 *   freezing custom. The claimed type is tangled_rope — genuine coordination
 *   inextricably bound with extraction — and the metrics reflect high
 *   extractiveness and suppression with near-total accessibility collapse for
 *   victims.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__hindu_dharmashastra_reading, 0.78).
domain_priors:suppression_score(family_law_authority__hindu_dharmashastra_reading, 0.82).
domain_priors:theater_ratio(family_law_authority__hindu_dharmashastra_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__hindu_dharmashastra_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__hindu_dharmashastra_reading, "Hindu Dharmashastra Marriage as Sacramental Samskara (Pre-1955)").
narrative_ontology:topic_domain(family_law_authority__hindu_dharmashastra_reading, "religious_governance/family_law/comparative_law").

domain_priors:requires_active_enforcement(family_law_authority__hindu_dharmashastra_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__hindu_dharmashastra_reading, '483fc744-473b-4709-9aa8-988d20926856').
narrative_ontology:cs_kernel_codification('483fc744-473b-4709-9aa8-988d20926856', fixed_text).
narrative_ontology:cs_authority_grounding('483fc744-473b-4709-9aa8-988d20926856', lineage).
narrative_ontology:cs_interpretation_layer_present('483fc744-473b-4709-9aa8-988d20926856').
narrative_ontology:cs_reading_relation('483fc744-473b-4709-9aa8-988d20926856', family_law_authority__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('483fc744-473b-4709-9aa8-988d20926856', family_law_authority__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('483fc744-473b-4709-9aa8-988d20926856', family_law_authority__parsi_zoroastrian_reading, coexists_with).
narrative_ontology:cs_reading_relation('483fc744-473b-4709-9aa8-988d20926856', family_law_authority__secular_contractual_reading, influences).
narrative_ontology:cs_axiom('483fc744-473b-4709-9aa8-988d20926856', foundational, marriage_as_sacramental_samskara).
narrative_ontology:cs_axiom_status(marriage_as_sacramental_samskara, holdable).
narrative_ontology:cs_axiom_grounding('483fc744-473b-4709-9aa8-988d20926856', marriage_as_sacramental_samskara, theological).
narrative_ontology:cs_axiom('483fc744-473b-4709-9aa8-988d20926856', foundational, caste_endogamy_as_dharmic_obligation).
narrative_ontology:cs_axiom_status(caste_endogamy_as_dharmic_obligation, holdable).
narrative_ontology:cs_axiom_grounding('483fc744-473b-4709-9aa8-988d20926856', caste_endogamy_as_dharmic_obligation, theological).
narrative_ontology:cs_axiom('483fc744-473b-4709-9aa8-988d20926856', foundational, wife_as_ritual_participant_not_contractor).
narrative_ontology:cs_axiom_status(wife_as_ritual_participant_not_contractor, holdable).
narrative_ontology:cs_axiom_grounding('483fc744-473b-4709-9aa8-988d20926856', wife_as_ritual_participant_not_contractor, theological).
narrative_ontology:cs_axiom('483fc744-473b-4709-9aa8-988d20926856', foundational, indissolubility_of_marriage).
narrative_ontology:cs_axiom_status(indissolubility_of_marriage, holdable).
narrative_ontology:cs_axiom_grounding('483fc744-473b-4709-9aa8-988d20926856', indissolubility_of_marriage, theological).
narrative_ontology:cs_axiom('483fc744-473b-4709-9aa8-988d20926856', secondary, joint_family_property_by_birthright).
narrative_ontology:cs_axiom_status(joint_family_property_by_birthright, holdable).
narrative_ontology:cs_axiom_grounding('483fc744-473b-4709-9aa8-988d20926856', joint_family_property_by_birthright, conventional).
narrative_ontology:cs_reference_frame('483fc744-473b-4709-9aa8-988d20926856', classical_dharmashastra_order).
narrative_ontology:cs_drift_state('483fc744-473b-4709-9aa8-988d20926856', post_hindu_code_bills_1955_56, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('483fc744-473b-4709-9aa8-988d20926856', '').
narrative_ontology:cs_kernel_id(family_law_authority__hindu_dharmashastra_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__hindu_dharmashastra_reading, upper_caste_householders).
narrative_ontology:constraint_beneficiary(family_law_authority__hindu_dharmashastra_reading, joint_family_karta).
narrative_ontology:constraint_beneficiary(family_law_authority__hindu_dharmashastra_reading, religious_authorities_pandits).
narrative_ontology:constraint_beneficiary(family_law_authority__hindu_dharmashastra_reading, husband_patriarch).
narrative_ontology:constraint_victim(family_law_authority__hindu_dharmashastra_reading, wives_ritual_participants).
narrative_ontology:constraint_victim(family_law_authority__hindu_dharmashastra_reading, lower_caste_individuals).
narrative_ontology:constraint_victim(family_law_authority__hindu_dharmashastra_reading, inter_caste_couples).
narrative_ontology:constraint_victim(family_law_authority__hindu_dharmashastra_reading, junior_coparceners).
narrative_ontology:constraint_victim(family_law_authority__hindu_dharmashastra_reading, widows_seeking_remarriage).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(family_law_authority__hindu_dharmashastra_reading, wife_ritual_participant).
narrative_ontology:constraint_vindicates(family_law_authority__hindu_dharmashastra_reading, marriage_as_sacramental_samskara).
narrative_ontology:constraint_vindicates(family_law_authority__hindu_dharmashastra_reading, caste_endogamy_as_dharmic_order).
narrative_ontology:constraint_vindicates(family_law_authority__hindu_dharmashastra_reading, joint_family_property_regime).
narrative_ontology:constraint_vindicates(family_law_authority__hindu_dharmashastra_reading, indissolubility_of_marriage).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds authority over family ritual, property management, and wife's role. Benefits from wife's ritual labor and joint family property control. Exit from the constraint means loss of religious status, family authority, and property rights; socially constrained by caste and religious identity.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, husband_patriarch, agenda_setter,
    powerful, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(family_law_authority__hindu_dharmashastra_reading, husband_patriarch, beneficiary).

% Marriage is sacramental indissoluble bond; role defined as ritual participant (saha-dharmini) not autonomous contractor. No independent property rights in joint family; dependent on husband's family for maintenance. Exit (divorce, separation) is religiously impossible and socially fatal — identity fused with marital role. Bears costs of indissolubility, caste endogamy restrictions, and lack of autonomy.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, wife_ritual_participant, payer,
    powerless, biographical, identity_locked, local).

% Caste endogamy preserves ritual purity, social capital, and property within caste group. Benefits from exclusion of lower castes from marriage alliances and the joint family system that consolidates wealth. Can navigate within caste networks; exit from endogamy norms means loss of caste status but mobility within caste is high.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, upper_caste_householders, beneficiary,
    organized, generational, mobile, regional).

% Excluded from marriage alliances with upper castes by dharmic prescription; confined to endogamous pools that reinforce occupational and ritual hierarchy. Constraint extracts marriage market access and ritual dignity. Exit from caste endogamy is structurally blocked by upper caste enforcement and internalized hierarchy; conversion or migration are rare and costly exits.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, lower_caste_individuals, payer,
    powerless, generational, trapped, regional).

% Marriage across caste lines is prohibited by dharmashastra; such unions face social ostracism, violence, and legal non-recognition pre-1955. The constraint extracts the possibility of chosen partnership. Exit options are nearly zero — separation enforced by family/community, or flight with total social rupture.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, inter_caste_couples, payer,
    powerless, immediate, trapped, local).

% Manages joint family property under Mitakshara or Dayabhaga rules; authority derived from seniority and gender. Benefits from consolidated property control and labor of junior members. Can manipulate partition rules; exit (partition) is possible but costly and reduces power — arbitrage-grade exit within the system.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, joint_family_karta, agenda_setter,
    powerful, generational, arbitrage, local).
narrative_ontology:stakeholder_secondary_role(family_law_authority__hindu_dharmashastra_reading, joint_family_karta, beneficiary).

% Have birth-right in joint family property but no management power until partition. Labor and earnings absorbed into joint pool. Constraint extracts labor and delays independent property control. Exit via partition is legally available but socially discouraged and economically risky.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, junior_coparceners, payer,
    moderate, biographical, constrained, local).

% Pre-1955, widow remarriage prohibited for upper castes by dharmashastra (except in some regional customs). Constraint extracts life autonomy, sexuality, and social participation. Identity as 'widow' is ritually fixed; exit via remarriage is religiously forbidden and socially punished. Dependent on natal or marital family for survival.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, widows_seeking_remarriage, payer,
    powerless, biographical, identity_locked, local).

% Interpret dharmashastra texts, officiate marriages, adjudicate caste and marriage disputes. Authority grounded in textual lineage and ritual knowledge. Benefit from monopoly on sacramental validation and dispute resolution. Can move between patronage networks; institutional role provides mobility.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, religious_authorities_pandits, agenda_setter,
    institutional, generational, mobile, regional).

% Brahmo Samaj, Arya Samaj, and other reformers challenged indissolubility, caste endogamy, and widow remarriage bans from within Hindu tradition. Structurally excluded from orthodox authority; their voices suppressed by orthodox pandits and colonial courts deferring to 'custom'. Exit from orthodox framework into reformist or secular frameworks is possible but costly.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, reformist_hindu_intellectuals, excluded,
    moderate, biographical, mobile, national).

% Anglo-Hindu law (colonial courts applying dharmashastra as 'personal law') codified and froze certain interpretations. Observed and administered the constraint but did not originate it. Their role was to adjudicate disputes using pandit testimony, creating a feedback loop that rigidified custom.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, colonial_legal_officials, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates intergenerational property transmission (joint family), ritual continuity (samskara), caste boundary maintenance (endogamy), and gendered division of ritual labor. Solves the problem of social reproduction in a hierarchical, agrarian society by fixing roles, property rules, and marriage pools.
% TRANSFER_FUNCTION: Moves ritual labor, reproductive labor, and property control from wives and junior members to husband/karta; moves marriage market access and ritual dignity from lower castes to upper castes; moves authority over family disputes from individuals to religious elders; moves autonomy from all parties to the sacramental framework.
% ABSENT_VOICES: Women's autonomous voices (no independent legal personality), lower caste perspectives on endogamy (enforced by upper caste power), inter-caste couples (structurally excluded), children's interests (subsumed under family), reformist Hindus (marginalized by orthodox-colonial alliance). These voices are absent from the dharmashastra textual tradition and the colonial courts that administered it.
% DISAPPEARANCE_RATIONALE: If the sacramental indissoluble marriage with caste endogamy and joint family property vanished overnight, the entire social architecture of pre-1955 Hindu society would reorganize: property would individualize, caste boundaries would become permeable, women would gain marital autonomy, and the ritual economy of the family would collapse. The 1955 Hindu Marriage Act and 1956 Succession Act were precisely this rearrangement — partial, contested, and incomplete.
% FOUNDING_PROBLEM: How to maintain social order, ritual purity, and intergenerational property transmission in a stratified agrarian society without centralized state administration of family life. The dharmashastra provided a self-enforcing, textually grounded framework that aligned religious duty (dharma) with social function.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (agrarian social order without state family law) is dead — the Indian state now administers family law (Hindu Marriage Act 1955, Succession Act 1956). Corroboration: B.R. Ambedkar's parliamentary debates on the Hindu Code Bill, the Law Commission reports, and the fact that the Indian state explicitly replaced dharmashastra with statutory law. Orthodox pandits contested this; the state's own legislative record confirms the founding problem was deemed solved by state law.
narrative_ontology:disappearance_verdict(family_law_authority__hindu_dharmashastra_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__hindu_dharmashastra_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__hindu_dharmashastra_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(family_law_authority__hindu_dharmashastra_reading, 'none', 1).
narrative_ontology:epsilon_provenance(family_law_authority__hindu_dharmashastra_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.78) is high because the constraint extracts autonomy, labor, property control, and marriage choice from wives, lower castes, junior coparceners, and widows — and this extraction is structural, not incidental. Suppression (0.82) is very high because exit is blocked by religious doctrine (indissolubility), social enforcement (caste ostracism), identity fusion (wife as ritual participant), and legal non-recognition of alternatives. Theater ratio (0.45) reflects that ritual performance (saptapadi, kanyadaan) is real coordination but increasingly performative as property and gender tensions grow. Accessibility collapse (0.88) is near-total pre-1955: divorce, inter-caste marriage, widow remarriage, and nuclear family property were structurally unavailable. Resistance (0.55) is moderate — reform movements existed but were marginalized until state intervention.
 *
 * PERSPECTIVAL GAP:
 *   From the karta/pandit seat, this is a rope: genuine coordination of property, ritual, and caste order with minimal coercion (participants 'voluntarily' follow dharma). From the wife/lower caste seat, this is a snare: indissolubility and endogamy are cover for extraction, enforced by identity fusion and social death. The engine computes this divergence from structural data — the same constraint is rope for the powerful and snare for the powerless.
 *
 * DIRECTIONALITY LOGIC:
 *   Husband/karta sits near beneficiary end (d ~ 0.15): collects ritual labor, property control, and authority; constrained exit but high power. Wife is identity-locked target (d ~ 0.95): fused identity, no exit, bears extraction. Lower caste individuals are trapped (d ~ 0.9): structural exclusion, internalized hierarchy. Junior coparceners are constrained payers (d ~ 0.7): delayed autonomy, partition possible but costly. Religious authorities are agenda-setters with mobility (d ~ 0.2): interpretive monopoly, institutional power. Colonial officials are analytical observers (d ~ 0.5): administer but don't originate.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (agrarian social reproduction without state family law) is dead — solved by state legislation. Yet the constraint persists in customary practice, cultural expectations, and partial legal survivals (e.g., joint family property rules under Mitakshara coparcenary until 2005 amendment). This is mandatrophy: the mandate (dharmic social order) outlived its function (agrarian reproduction), but the constraint persists through cultural inertia and identity politics. The 1955/1956 Acts were the mandatrophy resolution attempt — incomplete because they retained joint family property and did not fully secularize marriage.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_kernel_reading_identity,
    'Is this constraint a single reading of the family_law_authority kernel, or does it represent the kernel itself?',
    'Structural decomposition: if changing the observable (e.g., measuring extraction by gender vs. caste vs. generation) changes epsilon, the label ''Hindu marriage'' conflates multiple constraints. Test by writing separate stories for gender extraction, caste extraction, and property coordination within the same textual tradition.',
    'If multiple constraints, each gets its own epsilon and classification; the kernel becomes a family of linked stories via network.affects_constraints. If single constraint, epsilon is stable across observables.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_kernel_reading_identity, conceptual, 'Whether the dharmashastra reading is one epsilon-invariant constraint or a conflation of multiple constraints (gender, caste, property) under one label.').

omega_variable(
    customary_vs_textual_authority,
    'Does the constraint''s authority derive from dharmashastra texts or from customary practice that the texts merely rationalize?',
    'Historical analysis of regional variation: if practice varies widely while texts are constant, custom drives the constraint; if practice tracks textual interpretation, texts drive it. Colonial court records (privy council decisions) show the interaction.',
    'If custom-driven, the constraint is more adaptive (rope-like); if text-driven, more rigid (snare-like). Affects claimed_type and emergence_naturally assessment.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(customary_vs_textual_authority, empirical, 'Text vs. custom as the real source of the constraint''s binding force.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (caste ostracism, legal non-recognition, economic dependency) or internalized (identity fusion, dharmic self-concept, ritual necessity)?',
    'Post-exit suppression trajectory: track individuals who exited (conversion, migration, reformist marriage) — if suppression persists after structural barriers removed, internalized component is significant.',
    'If internalized, effective suppression is higher than structural measure suggests; the constraint travels with the agent. Affects omega weighting in classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in interpersonal/religious constraints.').

omega_variable(
    joint_family_coordination_extraction_boundary,
    'Is the joint family property system a genuine coordination mechanism (risk pooling, economies of scale) or an extraction mechanism (karta control, junior member disempowerment)?',
    'Counterfactual: compare economic outcomes of partitioned vs. joint families in similar agro-ecological zones; test whether partition improves welfare of junior members without collapsing risk pooling.',
    'If genuine coordination, the property rule is rope-like; if extraction, snare-like. This boundary determines whether the tangled_rope classification holds or splits.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(joint_family_coordination_extraction_boundary, conceptual, 'Whether joint family property coordination justifies its extraction or masks it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__hindu_dharmashastra_reading, 1800, 1955).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fla_hindu_dharmashastra_tr_t1800, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 1800, 0.35).
narrative_ontology:measurement(fla_hindu_dharmashastra_tr_t1850, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 1850, 0.4).
narrative_ontology:measurement(fla_hindu_dharmashastra_tr_t1900, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 1900, 0.45).
narrative_ontology:measurement(fla_hindu_dharmashastra_tr_t1920, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 1920, 0.5).
narrative_ontology:measurement(fla_hindu_dharmashastra_tr_t1940, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 1940, 0.48).
narrative_ontology:measurement(fla_hindu_dharmashastra_tr_t1955, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 1955, 0.45).

% Extraction over time
narrative_ontology:measurement(fla_hindu_dharmashastra_be_t1800, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 1800, 0.72).
narrative_ontology:measurement(fla_hindu_dharmashastra_be_t1850, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 1850, 0.75).
narrative_ontology:measurement(fla_hindu_dharmashastra_be_t1900, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 1900, 0.78).
narrative_ontology:measurement(fla_hindu_dharmashastra_be_t1920, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 1920, 0.8).
narrative_ontology:measurement(fla_hindu_dharmashastra_be_t1940, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 1940, 0.79).
narrative_ontology:measurement(fla_hindu_dharmashastra_be_t1955, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 1955, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(fla_hindu_dharmashastra_su_t1800, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 1800, 0.75).
narrative_ontology:measurement(fla_hindu_dharmashastra_su_t1850, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 1850, 0.78).
narrative_ontology:measurement(fla_hindu_dharmashastra_su_t1900, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 1900, 0.82).
narrative_ontology:measurement(fla_hindu_dharmashastra_su_t1920, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 1920, 0.85).
narrative_ontology:measurement(fla_hindu_dharmashastra_su_t1940, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 1940, 0.83).
narrative_ontology:measurement(fla_hindu_dharmashastra_su_t1955, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 1955, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__hindu_dharmashastra_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(family_law_authority__hindu_dharmashastra_reading, 0.08).
narrative_ontology:affects_constraint(family_law_authority__hindu_dharmashastra_reading, family_law_authority__muslim_shariat_reading).
narrative_ontology:affects_constraint(family_law_authority__hindu_dharmashastra_reading, family_law_authority__christian_canonical_reading).
narrative_ontology:affects_constraint(family_law_authority__hindu_dharmashastra_reading, family_law_authority__parsi_zoroastrian_reading).
narrative_ontology:affects_constraint(family_law_authority__hindu_dharmashastra_reading, family_law_authority__secular_contractual_reading).
narrative_ontology:affects_constraint(family_law_authority__hindu_dharmashastra_reading, hindu_succession_mitakshara_coparcenary).
narrative_ontology:affects_constraint(family_law_authority__hindu_dharmashastra_reading, hindu_adoption_maintenance_act_1956).

% DUAL FORMULATION NOTE:
% The family_law_authority kernel decomposes into five readings (hindu_dharmashastra, muslim_shariat, christian_canonical, parsi_zoroastrian, secular_contractual) with different epsilon values and victim/beneficiary structures. This reading (hindu_dharmashastra) has the highest extractiveness (0.78) due to caste-gender-generational intersectionality. The secular_contractual_reading (post-1955 statutory law) influences this reading by overriding indissolubility and modifying property rules, but the dharmashastra reading persists in customary practice and cultural authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(family_law_authority__hindu_dharmashastra_reading, institutional, 0.15).
constraint_indexing:directionality_override(family_law_authority__hindu_dharmashastra_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
