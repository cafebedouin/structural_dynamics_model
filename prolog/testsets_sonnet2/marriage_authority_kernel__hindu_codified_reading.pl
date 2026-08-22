% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel__hindu_codified_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority_kernel__hindu_codified_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: marriage_authority_kernel__hindu_codified_reading
 *   human_readable: Hindu Marriage Act (1955) Authority as Interpreted by Civil Courts
 *   domain: comparative_law/constitutional_pluralism/religious_governance
 *
 * SUMMARY:
 *   This story instantiates the Hindu-codified reading of the contested
 *   marriage-authority kernel in Indian law: the claim that legitimate
 *   marriage/family law authority for Hindus (statutorily including
 *   Buddhists, Jains, and Sikhs) derives from the Hindu Marriage Act 1955 as
 *   interpreted by civil courts, rather than from unwritten custom, caste
 *   council adjudication, or a religiously-neutral civil code. The reading
 *   combines a genuine coordination achievement (a single litigable statute
 *   replacing fragmented custom, including introducing divorce and
 *   inheritance rights that many customary regimes lacked) with asymmetric
 *   extraction (fault-based procedural burdens and uneven enforcement that
 *   fall disproportionately on women seeking exit, and courts' patchy
 *   handling of inter-caste marriages the Act formally validates but does not
 *   socially secure). This is Tangled Rope: the coordination function is real
 *   and the enforcement apparatus (civil court jurisdiction, mandatory
 *   registration and litigation pathways) is also real, but it is not the
 *   same as the secular_civil_reading (Special Marriage Act) or the
 *   muslim_shariat_reading, each of which has its own ε, its own
 *   beneficiary/victim structure, and its own classification as a separate
 *   constraint story.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__hindu_codified_reading, 0.42).
domain_priors:suppression_score(marriage_authority_kernel__hindu_codified_reading, 0.48).
domain_priors:theater_ratio(marriage_authority_kernel__hindu_codified_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__hindu_codified_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel__hindu_codified_reading, "Hindu Marriage Act (1955) Authority as Interpreted by Civil Courts").
narrative_ontology:topic_domain(marriage_authority_kernel__hindu_codified_reading, "comparative_law/constitutional_pluralism/religious_governance").

domain_priors:requires_active_enforcement(marriage_authority_kernel__hindu_codified_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__hindu_codified_reading, 'e5411689-88d0-4c20-8204-c7370394225b').
narrative_ontology:cs_kernel_codification('e5411689-88d0-4c20-8204-c7370394225b', formalized).
narrative_ontology:cs_authority_grounding('e5411689-88d0-4c20-8204-c7370394225b', extraction).
narrative_ontology:cs_interpretation_layer_present('e5411689-88d0-4c20-8204-c7370394225b').
narrative_ontology:cs_reading_relation('e5411689-88d0-4c20-8204-c7370394225b', marriage_authority_kernel__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('e5411689-88d0-4c20-8204-c7370394225b', marriage_authority_kernel__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('e5411689-88d0-4c20-8204-c7370394225b', marriage_authority_kernel__parsi_communal_reading, coexists_with).
narrative_ontology:cs_reading_relation('e5411689-88d0-4c20-8204-c7370394225b', marriage_authority_kernel__secular_civil_reading, influences).
narrative_ontology:cs_axiom('e5411689-88d0-4c20-8204-c7370394225b', foundational, state_codification_supersedes_customary_law).
narrative_ontology:cs_axiom_status(state_codification_supersedes_customary_law, holdable).
narrative_ontology:cs_axiom_grounding('e5411689-88d0-4c20-8204-c7370394225b', state_codification_supersedes_customary_law, conventional).
narrative_ontology:cs_axiom('e5411689-88d0-4c20-8204-c7370394225b', foundational, community_indexed_law_compatible_with_equity_reform).
narrative_ontology:cs_axiom_status(community_indexed_law_compatible_with_equity_reform, holdable).
narrative_ontology:cs_axiom_grounding('e5411689-88d0-4c20-8204-c7370394225b', community_indexed_law_compatible_with_equity_reform, instrumental).
narrative_ontology:cs_reference_frame('e5411689-88d0-4c20-8204-c7370394225b', post_1955_codified_uniformity).
narrative_ontology:cs_drift_state('e5411689-88d0-4c20-8204-c7370394225b', contemporary_family_court_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e5411689-88d0-4c20-8204-c7370394225b', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__hindu_codified_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__hindu_codified_reading, state_judiciary).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__hindu_codified_reading, hindu_male_householders).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__hindu_codified_reading, codification_reform_lobby).
narrative_ontology:constraint_victim(marriage_authority_kernel__hindu_codified_reading, hindu_women_seeking_divorce).
narrative_ontology:constraint_victim(marriage_authority_kernel__hindu_codified_reading, inter_caste_couples).
narrative_ontology:constraint_victim(marriage_authority_kernel__hindu_codified_reading, non_normative_family_forms).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__hindu_codified_reading, state_competence_to_codify_personal_law).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__hindu_codified_reading, uniform_community_law_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Civil courts interpret and enforce the Hindu Marriage Act 1955, adjudicating marriage validity, divorce, maintenance, and custody for anyone the state classifies as Hindu (a category the Act itself defines broadly, including Buddhists, Jains, and Sikhs). Courts hold final interpretive authority over what counts as valid Hindu marriage practice, superseding local custom where it conflicts with the codified text.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, state_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Benefit from a codified, litigable framework that replaced more variable customary arrangements with predictable statutory categories (grounds for divorce, maintenance formulas, inheritance triggers) that in practice have historically favored existing property and custodial arrangements. Can exit into the secular civil code via inter-religious marriage registration, but doing so forfeits community standing and inheritance certainty.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, hindu_male_householders, beneficiary,
    moderate, biographical, constrained, national).

% Must litigate divorce, maintenance, and custody through a civil court system applying the codified Act's fault-based grounds and evidentiary burdens, which are slower and more adversarial than either the reformist promise of codification or the secular civil code's provisions. Community and family pressure to avoid litigation, combined with limited independent economic resources, makes formal exit from the marriage itself, not just the legal regime, costly. Cannot access the more individual-rights-oriented Special Marriage Act framework without renouncing the social and religious standing that codified Hindu marriage confers.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, hindu_women_seeking_divorce, payer,
    powerless, biographical, trapped, national).

% The Act formally permits inter-caste marriage within the broad statutory Hindu category, but registration, family recognition, and inheritance disputes are litigated in courts that apply community-inflected precedent unevenly across states. Their marriages are legally valid but socially and administratively contested, forcing repeated recourse to the same courts that are supposed to have settled the question by codification.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, inter_caste_couples, payer,
    powerless, biographical, constrained, regional).

% Mid-20th-century reformers who campaigned for codification as a vehicle for gender-equity improvements (monogamy, divorce rights, inheritance reform) over customary practice. Their vindication is the existence of the Act itself; they benefit reputationally and politically from pointing to codification as a completed reform, even where courts' interpretation has not delivered the full equity the reform promised.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, codification_reform_lobby, beneficiary,
    organized, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__hindu_codified_reading, codification_reform_lobby, observer).

% Cohabiting couples, same-sex partners, and those in relationship forms the Act does not contemplate are simply outside its categories. They are not victims of an adverse ruling so much as absent from the framework's vocabulary entirely; their objections rarely reach the courts that administer the kernel.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, non_normative_family_forms, excluded,
    powerless, biographical, trapped, national).

% Argue from outside the Hindu-specific framework that community-indexed personal law of any kind, including this reading, should yield to a single secular code. They observe and critique the kernel's community-bounded structure without being subject to it themselves.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, uniform_civil_code_advocates, observer,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides Hindus (statutorily defined to include Buddhists, Jains, and Sikhs) with a single, litigable, predictable body of marriage, divorce, and inheritance rules, replacing the prior patchwork of regional and caste-specific customary law with one statute interpreted uniformly by the civil judiciary.
% TRANSFER_FUNCTION: Moves interpretive and adjudicative authority from community elders, caste councils, and religious custom to state civil courts; in individual disputes, moves economic and custodial outcomes toward whichever party the court's application of fault-based, precedent-laden statutory categories favors, which in aggregate has tended to favor householders with property and social standing over dependents seeking exit.
% ABSENT_VOICES: Cohabiting and same-sex partners, and other family forms the Act does not recognize, have no voice in a framework organized entirely around statutorily defined Hindu marriage; uniform civil code advocates are heard in public debate but not seated within the kernel's own adjudicative structure.
% DISAPPEARANCE_RATIONALE: If the Hindu Marriage Act's civil-court authority vanished overnight, hundreds of millions of marriages, divorces, and inheritance claims would lose their governing statute; disputes would revert to a mix of uncodified custom, competing personal-law claims, and pressure toward the secular Special Marriage Act, producing years of jurisdictional confusion, especially for pending divorce and maintenance cases.
% FOUNDING_PROBLEM: Pre-1955, Hindu marriage and divorce were governed by fragmented, often unwritten regional and caste custom with no divorce provision in most traditions, leaving courts to adjudicate case by case with no settled statutory text and leaving women with essentially no exit right from marriage.
% FOUNDING_PROBLEM_CORROBORATION: The codification reform lobby and the state judiciary attest the founding problem (absence of divorce rights, customary fragmentation) is substantially solved by the Act's existence. Independent family-law scholarship and women's rights litigators outside the reform lobby attest the founding problem persists in modified form: fault-based grounds, protracted litigation timelines, and uneven maintenance enforcement mean the exit right the Act promised remains practically unavailable to many of the women it was written to protect.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__hindu_codified_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__hindu_codified_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__hindu_codified_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_authority_kernel__hindu_codified_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority_kernel__hindu_codified_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority_kernel__hindu_codified_reading_tests).
:- end_tests(marriage_authority_kernel__hindu_codified_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) and rising slowly: the Act's coordination value (predictability, statutory divorce rights) is durable, but litigation costs and fault-based procedural burdens have compounded as case law accretes without matching statutory reform, meaning women's practical exit costs have not fallen even as the doctrine matures. Suppression is moderate (0.48): there is no criminal enforcement of marriage itself, but there is active judicial and administrative machinery (mandatory registration in many states, court-only divorce, jurisdictional exclusivity over Hindu-classified marriages) that forecloses informal or customary alternative adjudication. Theater ratio is low but rising (0.22 by 2024) reflecting growing performative elements — ceremonial court mediation requirements, counseling mandates — that some family-court reform literature treats as delay mechanisms rather than genuine dispute resolution aids.
 *
 * DIRECTIONALITY LOGIC:
 *   State judiciary sits as agenda_setter/beneficiary: it holds and expands interpretive authority, and its institutional legitimacy is partly constituted by administering this kernel. Hindu male householders derive a structural benefit from statutory predictability that has historically tracked existing property/custodial arrangements — this is not identical to bad faith, but the derivation correctly assigns them low d. Hindu women seeking divorce and inter-caste couples are the structural targets: trapped or constrained exit, powerless in the courtroom relative to institutional and moderate-power counterparties, bearing the transfer function's costs directly. The codification reform lobby occupies an unusual beneficiary seat — they benefit reputationally from the Act's existence as a completed reform even where its operation diverges from their original equity aims, which is exactly the kind of divergence Mandatrophy analysis exists to surface.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (absence of divorce rights, fragmented custom, no statutory recourse for women) was substantially live in 1955 and is only partially dead today — divorce rights now exist in statute, but the practical exit costs the Act was meant to eliminate persist in modified, procedural form. Classifying this as tangled_rope rather than snare or mountain prevents two errors: treating the entire apparatus as pure extraction (which would erase its genuine 1955 coordination achievement and the real rights it introduced) and treating it as settled natural law (which is the reform lobby's own preferred frame, and exactly the frame FSM-style analysis should be suspicious of when the same actors both wrote the codification and now cite its existence as proof of resolved equity).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    codification_as_reform_vs_control,
    'Was the 1955 codification primarily a genuine gender-equity reform that happened to also centralize state authority, or was state centralization the primary goal with equity language as legitimating cover?',
    'Legislative history analysis of the 1955 parliamentary debates, comparing the equity provisions actually enacted against equity provisions proposed and dropped, and cross-referencing with contemporaneous state interest in displacing caste council jurisdiction.',
    'If centralization was primary, the coordination story is substantially cover and the constraint sits closer to snare; if equity was primary and centralization was incidental, tangled_rope is the more defensible read, with extraction understood as an artifact of incomplete implementation rather than original design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(codification_as_reform_vs_control, conceptual, 'Whether 1955 codification''s genealogy is reform-led or state-control-led.').

omega_variable(
    hindu_category_boundary_construction,
    'Is the statutory category ''Hindu'' (as used by the Act, including Buddhists, Jains, Sikhs) a natural description of a pre-existing community, or a constructed administrative category that itself performs extraction by binding groups into a framework they did not choose?',
    'Historical and legal analysis of Sikh, Jain, and Buddhist community objections to inclusion under the Act at the time of passage and in subsequent litigation seeking separate personal-law status.',
    'If the category is substantially constructed rather than natural, the beneficiary/victim structure authored here undercounts a victim group (non-Hindu communities administratively classified as Hindu) whose objections have historically been treated as a footnote rather than a structural exclusion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hindu_category_boundary_construction, conceptual, 'Whether the statutory Hindu category is a natural or constructed boundary.').

omega_variable(
    cross_reading_forum_shopping,
    'To what extent do individuals strategically convert or claim alternate religious identity specifically to access a more favorable sibling reading (e.g., converting to access the secular Special Marriage Act, or Christian personal law''s different divorce grounds)?',
    'Empirical study of conversion-adjacent marriage/divorce filings and their correlation with pending litigation under the Hindu Marriage Act.',
    'High forum-shopping rates would indicate the kernel''s community-bounded readings function partly as a market of differently-priced exit options, materially affecting how trapped the hindu_women_seeking_divorce stakeholder group actually is relative to its authored exit_options value here.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cross_reading_forum_shopping, empirical, 'Whether strategic identity-switching undermines the trapped exit-option assessment for this reading''s payers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__hindu_codified_reading, 1955, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1955, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 1955, 0.1).
narrative_ontology:measurement(marr_tr_t1970, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 1970, 0.14).
narrative_ontology:measurement(marr_tr_t1985, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 1985, 0.16).
narrative_ontology:measurement(marr_tr_t2000, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(marr_tr_t2012, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 2012, 0.2).
narrative_ontology:measurement(marr_tr_t2024, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 2024, 0.22).

% Extraction over time
narrative_ontology:measurement(marr_be_t1955, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 1955, 0.28).
narrative_ontology:measurement(marr_be_t1970, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 1970, 0.32).
narrative_ontology:measurement(marr_be_t1985, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 1985, 0.35).
narrative_ontology:measurement(marr_be_t2000, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 2000, 0.38).
narrative_ontology:measurement(marr_be_t2012, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 2012, 0.4).
narrative_ontology:measurement(marr_be_t2024, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1955, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 1955, 0.4).
narrative_ontology:measurement(marr_su_t1970, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 1970, 0.42).
narrative_ontology:measurement(marr_su_t1985, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 1985, 0.44).
narrative_ontology:measurement(marr_su_t2000, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 2000, 0.46).
narrative_ontology:measurement(marr_su_t2012, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 2012, 0.47).
narrative_ontology:measurement(marr_su_t2024, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 2024, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__hindu_codified_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, muslim_shariat_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, christian_canonical_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, parsi_communal_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, secular_civil_reading).

% DUAL FORMULATION NOTE:
% This story is one of five siblings decomposing the natural-language concept 'Indian marriage/family law authority' into structurally distinct constraints per the ε-invariance principle. Each community-indexed reading (Hindu, Muslim, Christian, Parsi) plus the secular civil reading has its own kernel-adjudicating institution, its own beneficiary/victim structure, and its own ε — they are not the same constraint viewed under different observables. The hindu_codified_reading is linked to all four siblings because a change in any sibling's legitimacy or enforcement capacity (e.g., a Uniform Civil Code enactment absorbing the secular_civil_reading's domain) would directly alter this reading's relative attractiveness and the exit options available to its payer stakeholders.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
