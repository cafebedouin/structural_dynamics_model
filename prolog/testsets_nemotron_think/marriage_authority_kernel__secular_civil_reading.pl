% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel__secular_civil_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority_kernel__secular_civil_reading, []).

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
 *   constraint_id: marriage_authority_kernel__secular_civil_reading
 *   human_readable: Secular Civil Marriage Authority (Special Marriage Act 1954)
 *   domain: comparative_law/constitutional_pluralism/religious_governance
 *
 * SUMMARY:
 *   The Special Marriage Act 1954 establishes a secular civil marriage regime
 *   in India, grounded in constitutional individual rights (Articles 14, 15,
 *   21). It operates alongside religious personal laws (Hindu, Muslim,
 *   Christian, Parsi) as an optional parallel system. The Act enables
 *   inter-religious marriage without conversion and provides gender-equitable
 *   divorce, maintenance, and inheritance provisions. Civil courts
 *   adjudicate. However, individuals who choose civil marriage over community
 *   law face severe social costs — ostracism, family rupture, loss of
 *   community support — imposed by religious authorities, not the Act itself.
 *   This creates a tangled rope: genuine coordination (legal recognition,
 *   equality) coexists with asymmetric extraction (social costs borne by the
 *   most vulnerable users). The constraint is claimed as a rope
 *   (coordination) but operates as a tangled rope due to the
 *   community-imposed exit costs that the Act's existence structurally
 *   enables.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__secular_civil_reading, 0.28).
domain_priors:suppression_score(marriage_authority_kernel__secular_civil_reading, 0.15).
domain_priors:theater_ratio(marriage_authority_kernel__secular_civil_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__secular_civil_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel__secular_civil_reading, "Secular Civil Marriage Authority (Special Marriage Act 1954)").
narrative_ontology:topic_domain(marriage_authority_kernel__secular_civil_reading, "comparative_law/constitutional_pluralism/religious_governance").

domain_priors:requires_active_enforcement(marriage_authority_kernel__secular_civil_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__secular_civil_reading, '20850804-de85-476b-84a2-fe1a13c8a8db').
narrative_ontology:cs_kernel_codification('20850804-de85-476b-84a2-fe1a13c8a8db', formalized).
narrative_ontology:cs_authority_grounding('20850804-de85-476b-84a2-fe1a13c8a8db', lineage).
narrative_ontology:cs_interpretation_layer_present('20850804-de85-476b-84a2-fe1a13c8a8db').
narrative_ontology:cs_reading_relation('20850804-de85-476b-84a2-fe1a13c8a8db', marriage_authority_kernel__hindu_codified_reading, coexists_with).
narrative_ontology:cs_reading_relation('20850804-de85-476b-84a2-fe1a13c8a8db', marriage_authority_kernel__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('20850804-de85-476b-84a2-fe1a13c8a8db', marriage_authority_kernel__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('20850804-de85-476b-84a2-fe1a13c8a8db', marriage_authority_kernel__parsi_communal_reading, coexists_with).
narrative_ontology:cs_axiom('20850804-de85-476b-84a2-fe1a13c8a8db', foundational, marriage_authority_derives_from_constitutional_rights).
narrative_ontology:cs_axiom_status(marriage_authority_derives_from_constitutional_rights, holdable).
narrative_ontology:cs_axiom_grounding('20850804-de85-476b-84a2-fe1a13c8a8db', marriage_authority_derives_from_constitutional_rights, conventional).
narrative_ontology:cs_axiom('20850804-de85-476b-84a2-fe1a13c8a8db', foundational, gender_equality_supersedes_community_custom_in_marriage).
narrative_ontology:cs_axiom_status(gender_equality_supersedes_community_custom_in_marriage, holdable).
narrative_ontology:cs_axiom_grounding('20850804-de85-476b-84a2-fe1a13c8a8db', gender_equality_supersedes_community_custom_in_marriage, deontological).
narrative_ontology:cs_reference_frame('20850804-de85-476b-84a2-fe1a13c8a8db', constitutional_individual_rights_framework).
narrative_ontology:cs_drift_state('20850804-de85-476b-84a2-fe1a13c8a8db', contemporary_constitutional_jurisprudence, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('20850804-de85-476b-84a2-fe1a13c8a8db', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__secular_civil_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__secular_civil_reading, inter_religious_couples).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__secular_civil_reading, women_seeking_gender_equitable_marriage).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__secular_civil_reading, individuals_choosing_civil_over_community_law).
narrative_ontology:constraint_victim(marriage_authority_kernel__secular_civil_reading, individuals_bearing_social_costs_of_community_exit).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__secular_civil_reading, constitutional_individual_rights_supremacy).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__secular_civil_reading, gender_equality_in_marriage_law).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__secular_civil_reading, state_neutrality_in_personal_law).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacted the Special Marriage Act 1954 as a constitutional mandate; civil courts administer it. The state bears enforcement costs (court infrastructure, legal aid) but collects no direct revenue from marriages. Its interest is constitutional compliance and uniform civil law.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, indian_state_parliament_courts, agenda_setter,
    institutional, generational, analytical, national).

% Cannot marry under religious personal laws without conversion; the Act provides their only legal path to marriage without abandoning faith. They gain legal recognition, inheritance rights, and child legitimacy. Exit from the Act means no legal marriage; exit from community means social ostracism.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, inter_religious_couples, beneficiary,
    powerless, biographical, constrained, local).

% Religious personal laws often disadvantage women in divorce, maintenance, custody, inheritance. The Act provides gender-neutral provisions. They gain substantive equality but face familial and community pressure for bypassing religious law. Their exit options are limited by economic dependence and social embeddedness.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, women_seeking_gender_equitable_marriage, beneficiary,
    powerless, biographical, constrained, local).

% Choosing civil marriage under the Act triggers social sanctions: family ostracism, community exclusion, loss of religious-community support networks, sometimes violence. These costs are imposed by community authorities, not the Act itself, but the Act's existence makes the choice possible and thus the costs structurally attach to its use.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, individuals_bearing_social_costs_of_community_exit, payer,
    powerless, biographical, identity_locked, local).

% Lose jurisdictional authority over members who choose civil marriage. They cannot prevent the Act's operation but enforce social costs on defectors. They would object to the Act's existence and its constitutional grounding but are not formal parties to its administration.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, religious_community_authorities, excluded,
    organized, generational, trapped, national).

% Adjudicate constitutional challenges to personal laws; interpret the Act's provisions; debate Uniform Civil Code implications. They see the full structure of legal pluralism and the Act's place within it. They neither collect nor pay.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, constitutional_courts_legal_scholars, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legally recognized marriage framework that does not require religious conversion or adherence to gender-unequal personal law, enabling inter-religious marriage and gender-equitable terms for all citizens regardless of community affiliation.
% TRANSFER_FUNCTION: Transfers jurisdictional authority over marriage from religious community structures to civil courts; transfers the social costs of community exit onto individuals who choose the civil route (ostracism, loss of community support); transfers the benefit of gender-equitable legal terms to women who would otherwise be subject to unequal personal law.
% ABSENT_VOICES: Minority religious communities that view the Act as state encroachment on communal autonomy; women in highly conservative communities who cannot even access knowledge of the Act; children of inter-religious civil marriages who navigate identity conflicts without institutional support.
% DISAPPEARANCE_RATIONALE: If the Special Marriage Act vanished, inter-religious couples would lose their only non-conversion legal marriage path; women in unequal personal laws would lose a statutory equality floor; civil courts would lose a major personal law docket; religious authorities would regain de facto monopoly over marriage for their members. The legal landscape of Indian pluralism would fundamentally shift.
% FOUNDING_PROBLEM: Post-independence India needed a marriage law that honored constitutional guarantees of equality and non-discrimination while respecting religious diversity. The Act was built to solve: (1) no legal path for inter-religious marriage without conversion, (2) gender inequality embedded in all major personal laws, (3) the constitutional mandate for a Uniform Civil Code (Article 44) requiring at least one optional civil code.
% FOUNDING_PROBLEM_CORROBORATION: The Constituent Assembly debates (outside beneficiary parties) record the dual intent: equality floor and UCC stepping stone. The Law Commission reports (2018, 2024) attest the gender-equality problem persists in personal laws. Religious leadership bodies (AIMPLB, Hindu Mahasabha, etc.) contest that the problem was ever the state's to solve — they attest the founding problem is dead or illegitimate.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__secular_civil_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__secular_civil_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__secular_civil_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_authority_kernel__secular_civil_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority_kernel__secular_civil_reading, 0.28, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority_kernel__secular_civil_reading_tests).
:- end_tests(marriage_authority_kernel__secular_civil_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28) is moderate: the Act itself imposes minimal direct costs (court fees, procedural delay) but the social costs of community exit are high and structurally linked to its use. Suppression (0.15) is low: the Act does not suppress religious personal laws; they remain fully operational. Theater ratio (0.12) is low: the Act functions as intended, though rising political rhetoric around UCC adds performative layer. Accessibility collapse (0.35) is moderate: alternatives (personal laws) remain fully available and widely used. Resistance (0.42) is moderate: religious authorities resist the Act's legitimacy and pressure members not to use it, but cannot legally block it.
 *
 * PERSPECTIVAL GAP:
 *   From the state's seat, the Act is a rope: pure coordination fulfilling constitutional duty. From the user seats (inter-religious couples, women), it is a tangled rope: they gain legal equality but pay social extraction. From religious authorities' seat, it is a snare: it extracts their jurisdictional authority and community cohesion. The engine computes these divergences from the declared roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   The state (agenda_setter) is a structural beneficiary in the constitutional sense (fulfills mandate) but bears enforcement costs — d near symmetric. Inter-religious couples and women seeking equity are beneficiaries who also pay social costs — their directionality is complex: low d for legal benefits, high d for social costs. The engine will compute per-seat χ from this structural split. Religious authorities are excluded — their exclusion is the mechanism that generates the social costs on users. Constitutional courts are analytical observers.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (inter-religious marriage access, gender equality floor, UCC stepping stone) remains contested. The Act has not atrophied — usage grows slowly but steadily. However, the political project of UCC has shifted from the Act as a voluntary model to a mandatory replacement, changing the constraint's telos. The mandatrophy question is whether the Act still serves its original coordination function or has become a political instrument.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the marriage_authority_kernel a single constraint with multiple readings, or are the five readings structurally distinct constraints that merely share a label?',
    'Apply ε-invariance test: if measuring extractiveness/suppression under each reading yields stably different values that cannot be reconciled by observer position alone, they are distinct constraints. The secular reading''s ε (0.28) differs from the muslim_shariat_reading''s likely higher ε (gender-unequal provisions, community enforcement).',
    'If distinct constraints, each gets its own classification and the kernel is a colloquial label, not a structural unit. If one constraint, the readings are observer perspectives on a single arrangement — but the ε-invariance principle forbids observer-relative ε.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether the kernel label obscures structurally distinct constraints per ε-invariance principle.').

omega_variable(
    social_cost_attribution,
    'Are the social costs of community exit (ostracism, violence) attributable to the Special Marriage Act as a constraint, or to the religious community authorities as a separate constraint?',
    'Counterfactual: if the Act were repealed, would the social costs disappear? No — they would attach to any inter-religious or non-conforming marriage. The costs are imposed by community authorities, not the Act. But the Act''s existence creates the choice that triggers them.',
    'If costs are external, the Act''s ε is lower (rope-like). If costs are structurally internal to the Act''s operation, ε is higher (tangled_rope). Affects classification and directionality for user seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(social_cost_attribution, conceptual, 'Whether community-imposed social costs count as the Act''s extraction.').

omega_variable(
    coordination_genuineness,
    'Does the Act genuinely coordinate (solve a collective action problem for inter-religious couples) or does it primarily serve as a symbolic equality floor that few can practically access due to social costs?',
    'Empirical: track usage rates, demographic profiles of users, and outcome data (marriage stability, economic outcomes, social integration) over time. Compare with jurisdictions lacking such an option.',
    'If coordination is genuine and accessible, rope/tangled_rope classification holds. If mostly symbolic, the Act drifts toward piton (theatrical maintenance of equality claim).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_genuineness, empirical, 'Whether the Act''s coordination function is substantively realized or largely aspirational.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__secular_civil_reading, 1954, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marriage_authority_kernel__secular_civil_reading_tr_t1954, marriage_authority_kernel__secular_civil_reading, theater_ratio, 1954, 0.05).
narrative_ontology:measurement(marriage_authority_kernel__secular_civil_reading_tr_t1976, marriage_authority_kernel__secular_civil_reading, theater_ratio, 1976, 0.07).
narrative_ontology:measurement(marriage_authority_kernel__secular_civil_reading_tr_t1985, marriage_authority_kernel__secular_civil_reading, theater_ratio, 1985, 0.09).
narrative_ontology:measurement(marriage_authority_kernel__secular_civil_reading_tr_t2000, marriage_authority_kernel__secular_civil_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(marriage_authority_kernel__secular_civil_reading_tr_t2010, marriage_authority_kernel__secular_civil_reading, theater_ratio, 2010, 0.11).
narrative_ontology:measurement(marriage_authority_kernel__secular_civil_reading_tr_t2024, marriage_authority_kernel__secular_civil_reading, theater_ratio, 2024, 0.12).

% Extraction over time
narrative_ontology:measurement(marriage_authority_kernel__secular_civil_reading_be_t1954, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 1954, 0.15).
narrative_ontology:measurement(marriage_authority_kernel__secular_civil_reading_be_t1976, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 1976, 0.18).
narrative_ontology:measurement(marriage_authority_kernel__secular_civil_reading_be_t1985, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 1985, 0.22).
narrative_ontology:measurement(marriage_authority_kernel__secular_civil_reading_be_t2000, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 2000, 0.25).
narrative_ontology:measurement(marriage_authority_kernel__secular_civil_reading_be_t2010, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 2010, 0.27).
narrative_ontology:measurement(marriage_authority_kernel__secular_civil_reading_be_t2024, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 2024, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(marriage_authority_kernel__secular_civil_reading_su_t1954, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 1954, 0.08).
narrative_ontology:measurement(marriage_authority_kernel__secular_civil_reading_su_t1976, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 1976, 0.1).
narrative_ontology:measurement(marriage_authority_kernel__secular_civil_reading_su_t1985, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 1985, 0.12).
narrative_ontology:measurement(marriage_authority_kernel__secular_civil_reading_su_t2000, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 2000, 0.13).
narrative_ontology:measurement(marriage_authority_kernel__secular_civil_reading_su_t2010, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 2010, 0.14).
narrative_ontology:measurement(marriage_authority_kernel__secular_civil_reading_su_t2024, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 2024, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__secular_civil_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(marriage_authority_kernel__secular_civil_reading, 0.1).
narrative_ontology:affects_constraint(marriage_authority_kernel__secular_civil_reading, marriage_authority_kernel__hindu_codified_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__secular_civil_reading, marriage_authority_kernel__muslim_shariat_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__secular_civil_reading, marriage_authority_kernel__christian_canonical_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__secular_civil_reading, marriage_authority_kernel__parsi_communal_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of five readings of the marriage_authority_kernel. The kernel decomposes into five constraint stories because each reading has a distinct ε (extractiveness profile), distinct beneficiary/victim structure, and distinct enforcement mechanism. The secular reading has the lowest ε and highest gender equity; the muslim_shariat_reading likely has the highest ε due to asymmetric gender provisions and community enforcement. They are linked via affects_constraints because the secular reading's existence as an exit option structurally pressures the personal law systems to reform (influences relation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_authority_kernel__secular_civil_reading, powerless, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
