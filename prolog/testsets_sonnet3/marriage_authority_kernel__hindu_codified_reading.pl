% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel__hindu_codified_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: marriage_authority_kernel__hindu_codified_reading
 *   human_readable: Hindu Marriage Act 1955 as Codified Reading of Marriage Authority Kernel
 *   domain: comparative_law/religious_governance
 *
 * SUMMARY:
 *   This constraint models the Hindu codified reading of the marriage
 *   authority kernel: the claim that legitimate marriage/family law for
 *   Hindus derives from the Hindu Marriage Act 1955, interpreted and enforced
 *   by secular civil courts rather than community religious functionaries.
 *   Amendments since 1955 (particularly on maintenance, divorce grounds, and
 *   the 2005 Hindu Succession Amendment on daughters' coparcenary rights)
 *   have gradually narrowed the statute's original gender asymmetry, which
 *   the declining extractiveness trajectory reflects. The reading remains
 *   structurally tangled rather than a pure rope: it genuinely coordinates
 *   (uniform rules, predictable adjudication, replacing fragmented custom)
 *   but the coordination rides on a persistent asymmetry in divorce,
 *   maintenance enforcement, and inter-faith marriage access that requires
 *   the state's continuing enforcement apparatus to hold in place.
 *
 * KEY AGENTS:
 *   - male_hindu_spouses: structural beneficiary of historical statutory defaults
 *   - state_family_court_apparatus: agenda-setting interpretive authority
 *   - hindu_wives_seeking_divorce: primary payer, trapped exit
 *   - inter_faith_couples: payer bearing community-boundary friction cost
 *   - legal_reform_advocates: excluded voice pressing for uniform civil code
 *   - comparative_law_scholars: analytical observer across the kernel's five readings
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
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__hindu_codified_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel__hindu_codified_reading, "Hindu Marriage Act 1955 as Codified Reading of Marriage Authority Kernel").
narrative_ontology:topic_domain(marriage_authority_kernel__hindu_codified_reading, "comparative_law/religious_governance").

domain_priors:requires_active_enforcement(marriage_authority_kernel__hindu_codified_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__hindu_codified_reading, 'bf56ceae-03f9-4bfe-9f87-1137de3530c0').
narrative_ontology:cs_kernel_codification('bf56ceae-03f9-4bfe-9f87-1137de3530c0', formalized).
narrative_ontology:cs_authority_grounding('bf56ceae-03f9-4bfe-9f87-1137de3530c0', extraction).
narrative_ontology:cs_interpretation_layer_present('bf56ceae-03f9-4bfe-9f87-1137de3530c0').
narrative_ontology:cs_reading_relation('bf56ceae-03f9-4bfe-9f87-1137de3530c0', marriage_authority_kernel__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('bf56ceae-03f9-4bfe-9f87-1137de3530c0', marriage_authority_kernel__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('bf56ceae-03f9-4bfe-9f87-1137de3530c0', marriage_authority_kernel__parsi_communal_reading, coexists_with).
narrative_ontology:cs_reading_relation('bf56ceae-03f9-4bfe-9f87-1137de3530c0', marriage_authority_kernel__secular_civil_reading, influences).
narrative_ontology:cs_axiom('bf56ceae-03f9-4bfe-9f87-1137de3530c0', foundational, state_codification_supersedes_uncodified_community_custom).
narrative_ontology:cs_axiom_status(state_codification_supersedes_uncodified_community_custom, holdable).
narrative_ontology:cs_axiom_grounding('bf56ceae-03f9-4bfe-9f87-1137de3530c0', state_codification_supersedes_uncodified_community_custom, conventional).
narrative_ontology:cs_axiom('bf56ceae-03f9-4bfe-9f87-1137de3530c0', foundational, civil_courts_hold_final_interpretive_authority_over_religious_personal_law).
narrative_ontology:cs_axiom_status(civil_courts_hold_final_interpretive_authority_over_religious_personal_law, holdable).
narrative_ontology:cs_axiom_grounding('bf56ceae-03f9-4bfe-9f87-1137de3530c0', civil_courts_hold_final_interpretive_authority_over_religious_personal_law, conventional).
narrative_ontology:cs_axiom('bf56ceae-03f9-4bfe-9f87-1137de3530c0', secondary, communal_differentiation_in_personal_law_is_constitutionally_legitimate).
narrative_ontology:cs_axiom_status(communal_differentiation_in_personal_law_is_constitutionally_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('bf56ceae-03f9-4bfe-9f87-1137de3530c0', communal_differentiation_in_personal_law_is_constitutionally_legitimate, instrumental).
narrative_ontology:cs_reference_frame('bf56ceae-03f9-4bfe-9f87-1137de3530c0', colonial_era_uncodified_hindu_custom).
narrative_ontology:cs_drift_state('bf56ceae-03f9-4bfe-9f87-1137de3530c0', post_2005_succession_amendment_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('bf56ceae-03f9-4bfe-9f87-1137de3530c0', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__hindu_codified_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__hindu_codified_reading, male_hindu_spouses).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__hindu_codified_reading, state_family_court_apparatus).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__hindu_codified_reading, hindu_community_institutional_continuity).
narrative_ontology:constraint_victim(marriage_authority_kernel__hindu_codified_reading, hindu_wives_seeking_divorce).
narrative_ontology:constraint_victim(marriage_authority_kernel__hindu_codified_reading, inter_faith_couples).
narrative_ontology:constraint_victim(marriage_authority_kernel__hindu_codified_reading, hindu_women_in_maintenance_disputes).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__hindu_codified_reading, codification_produces_legal_certainty).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__hindu_codified_reading, civil_court_adjudication_secures_uniformity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate within a codified statute whose historical defaults on property, guardianship, and maintenance were drafted around a male-headed household norm; retains structural advantage in litigation over marital property and child custody even as amendments have narrowed the gap.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, male_hindu_spouses, beneficiary,
    moderate, biographical, constrained, national).

% Civil courts hold interpretive monopoly over the Act, adjudicating validity, divorce, maintenance, and succession questions. Courts administer the statute, resolve ambiguities through precedent, and can expand or narrow its reach through interpretation without touching the codified text itself.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, state_family_court_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Religious and community bodies benefit from state validation of Hindu personal law as a distinct, codified sphere — this secures continued recognition of caste-endogamous marriage patterns and community-specific succession norms without requiring assimilation into a uniform civil code.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, hindu_community_institutional_continuity, beneficiary,
    organized, civilizational, arbitrage, national).

% Face statutory grounds for divorce, restitution of conjugal rights provisions, and maintenance formulas that route through years of civil litigation; rural and low-income wives without independent income face the steepest asymmetry, since exit requires proving fault-based grounds and mounting a maintenance claim inside the same court system that also protects the marriage's presumption of validity.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, hindu_wives_seeking_divorce, payer,
    powerless, biographical, trapped, regional).

% A marriage where one party is Hindu and one is not falls outside clean coverage of the Act, forcing couples toward conversion (to bring the marriage under the Act), the Special Marriage Act's mandatory notice period, or contested validity litigation — the codified reading's community boundary becomes a friction cost imposed specifically on couples who cross it.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, inter_faith_couples, payer,
    powerless, biographical, constrained, national).

% Maintenance under Section 25 and related provisions is discretionary and litigated case-by-case; without independent assets, a woman seeking maintenance depends entirely on a court's assessment of the husband's disclosed income, and enforcement of maintenance orders against non-compliant husbands is chronically weak.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, hindu_women_in_maintenance_disputes, payer,
    powerless, biographical, trapped, regional).

% Feminist legal scholars and reform bodies have long argued for a uniform civil code that would eliminate the community-specific patchwork entirely; their position is heard in law commission reports and academic literature but has not displaced the codified-community structure, which persists through legislative inertia and political sensitivity around personal law reform.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, legal_reform_advocates, excluded,
    organized, generational, constrained, national).

% Study the Hindu Marriage Act as one operative reading within India's plural personal law system, comparing its gender-equity outcomes against the Muslim, Christian, Parsi, and secular civil readings without holding a stake in any single community's arrangement.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, comparative_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority_kernel__hindu_codified_reading, diffuse).
narrative_ontology:fixing_cost_class(marriage_authority_kernel__hindu_codified_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, predictable statutory framework for marriage validity, divorce, succession, and maintenance within the Hindu community, replacing a fragmented landscape of regional custom and uncodified textual interpretation with one civil-court-administered code applicable uniformly across India.
% TRANSFER_FUNCTION: Moves adjudicative authority from community religious functionaries to state civil courts, and moves bargaining leverage in marital breakdown disputes toward the party (historically and still disproportionately the husband) favored by the statute's property, guardianship, and fault-based divorce defaults, at the cost of women's exit speed and inter-faith couples' access.
% ABSENT_VOICES: Uniform civil code advocates and intra-community reform voices (particularly among lower-caste and tribal Hindus whose customary marriage practices predate and diverge from the codified norm) are not parties to the statute's interpretation; the codification treats 'Hindu' as a single legal category that flattens substantial internal variation.
% DISAPPEARANCE_RATIONALE: If the Hindu Marriage Act and its civil-court interpretive apparatus vanished, marriage validity, divorce, and succession for the Hindu-majority population would revert to uncodified custom and colonial-era case law, family courts would lose their primary statutory basis for a large share of their caseload, and the entire community-differentiated personal-law architecture (of which this is one pillar) would need a replacement — most plausibly accelerating pressure toward the Special Marriage Act or a uniform civil code.
% FOUNDING_PROBLEM: Pre-1955, Hindu marriage and succession were governed by fragmented regional and textual traditions (Mitakshara, Dayabhaga, local custom) with no uniform statutory basis, producing unpredictable outcomes in cross-regional disputes and leaving reformist goals (raising marriage age, permitting divorce, restricting polygamy) unenforceable without legislative codification.
% FOUNDING_PROBLEM_CORROBORATION: The Law Commission of India's periodic reports on family law reform (an institutional voice outside the Hindu religious establishment) attest that codification succeeded at producing uniform, litigable divorce and marriage-validity rules, but separately attest that maintenance enforcement and gender-equity gaps remain live and unresolved seventy years on — corroboration exists but is split: procedural uniformity is dead-as-a-problem, gender-equity is a live problem the codification did not close.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__hindu_codified_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__hindu_codified_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__hindu_codified_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness at 0.42 (declining from 0.55 in 1955) reflects a statute whose gender-asymmetric defaults have been substantially, though incompletely, corrected by amendment and judicial interpretation over seventy years. Suppression at 0.48 reflects moderate but real friction — divorce and maintenance require litigation, and inter-faith couples face structural friction crossing the community boundary — but this is markedly lower than the muslim_shariat_reading's suppression profile (which this story does not author, per Rule 1) because civil courts, not community religious authorities, hold final adjudicative power and are subject to constitutional review. Theater ratio rising slightly (0.10 to 0.22) tracks growing procedural formalism in family court litigation without a corresponding function increase.
 *
 * DIRECTIONALITY LOGIC:
 *   State_family_court_apparatus sits at the agenda-setting seat: institutional power, analytical exit (courts are not themselves subject to the constraint), civilizational time horizon. Male_hindu_spouses and hindu_community_institutional_continuity are beneficiaries — the former through statutory defaults, the latter through continued state recognition of a distinct communal legal sphere. Hindu_wives_seeking_divorce and hindu_women_in_maintenance_disputes are targets: trapped exit options, powerless power atom, and the story's central asymmetry — divorce and maintenance access differ sharply by economic independence. Inter_faith_couples are a distinct victim class: their cost is not gender asymmetry but community-boundary friction specific to the codified reading's community-scoped design.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (fragmented uncodified custom, unenforceable reform) is genuinely dead as a coordination problem — codification succeeded at producing a uniform, litigable statute. But the mandate has not simply outlived its function; it has partially transformed into a persistence mechanism for community-differentiated personal law itself, which is now defended by hindu_community_institutional_continuity independent of whether uniform codification could equally be achieved under a single civil code. This is why the story claims tangled_rope rather than mountain or pure rope: real coordination coexists with an enforcement-dependent asymmetry that a uniform civil code reading would eliminate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    codification_as_coordination_vs_communal_boundary_maintenance,
    'Is the Hindu Marriage Act''s persistence best explained as ongoing coordination value (uniform, predictable adjudication that genuinely serves Hindu litigants) or as boundary-maintenance for a communally-differentiated personal law system that could be replaced by a uniform civil code without loss of coordination function?',
    'Compare litigant outcomes and satisfaction under the Hindu Marriage Act against outcomes under the Special Marriage Act for structurally similar disputes (divorce grounds, maintenance, succession); if outcomes converge, the community-specific coordination value is low and boundary-maintenance is the better explanation.',
    'If boundary-maintenance dominates, the tangled_rope classification is a conservative reading and the constraint may be closer to a snare wearing a coordination justification; if coordination value is substantial and community-specific, tangled_rope remains the accurate reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(codification_as_coordination_vs_communal_boundary_maintenance, conceptual, 'Whether codification''s persistence reflects genuine ongoing coordination value or communal boundary-maintenance.').

omega_variable(
    kernel_reading_location_of_gender_asymmetry,
    'Where exactly within the marriage_authority_kernel''s readings does the residual gender asymmetry live — in the codified statutory text itself, in civil-court interpretive practice, or in the enforcement gap between maintenance orders and actual compliance?',
    'Disaggregate maintenance-order issuance rates from maintenance-order compliance rates in reported family court data; a large gap between issuance and compliance would locate the asymmetry in enforcement rather than in the statute or its interpretation.',
    'If the asymmetry is primarily an enforcement gap rather than a statutory or interpretive one, the effective extraction experienced by hindu_wives_seeking_divorce and hindu_women_in_maintenance_disputes is structurally closer to a piton (enforcement apparatus that has atrophied for this specific function) layered onto an otherwise functioning tangled rope, rather than extraction built into the codified reading itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_location_of_gender_asymmetry, empirical, 'Whether residual gender asymmetry is located in statute, interpretation, or enforcement gap.').

omega_variable(
    communal_category_flattening_ambiguity,
    'Does treating ''Hindu'' as a single legal category under the Act obscure genuine sub-communal variation (caste-specific customary marriage practices, tribal Hindu practices exempted or contested under the Act) in a way that produces hidden victims not captured in this story''s victim declarations?',
    'Review case law on Section 2 exceptions (Scheduled Tribes) and customary-marriage-validity litigation to determine how much sub-communal variation the codified category actually absorbs versus displaces.',
    'If substantial sub-communal variation is displaced rather than absorbed, additional victim groups (e.g. tribal Hindu communities with customary marriage practices) should be added in a revision, which would shift the extraction and suppression profile.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(communal_category_flattening_ambiguity, empirical, 'Whether the single ''Hindu'' legal category obscures unaddressed sub-communal victim groups.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__hindu_codified_reading, 1955, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1955, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 1955, 0.1).
narrative_ontology:measurement(marr_tr_t1970, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 1970, 0.13).
narrative_ontology:measurement(marr_tr_t1985, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 1985, 0.16).
narrative_ontology:measurement(marr_tr_t2000, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(marr_tr_t2015, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 2015, 0.2).
narrative_ontology:measurement(marr_tr_t2025, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 2025, 0.22).

% Extraction over time
narrative_ontology:measurement(marr_be_t1955, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 1955, 0.55).
narrative_ontology:measurement(marr_be_t1970, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 1970, 0.5).
narrative_ontology:measurement(marr_be_t1985, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 1985, 0.48).
narrative_ontology:measurement(marr_be_t2000, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 2000, 0.45).
narrative_ontology:measurement(marr_be_t2015, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 2015, 0.43).
narrative_ontology:measurement(marr_be_t2025, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 2025, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1955, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 1955, 0.6).
narrative_ontology:measurement(marr_su_t1970, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 1970, 0.56).
narrative_ontology:measurement(marr_su_t1985, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 1985, 0.53).
narrative_ontology:measurement(marr_su_t2000, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 2000, 0.51).
narrative_ontology:measurement(marr_su_t2015, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 2015, 0.49).
narrative_ontology:measurement(marr_su_t2025, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 2025, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__hindu_codified_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(marriage_authority_kernel__hindu_codified_reading, 0.12).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, marriage_authority_kernel__muslim_shariat_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, marriage_authority_kernel__christian_canonical_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, marriage_authority_kernel__parsi_communal_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, marriage_authority_kernel__secular_civil_reading).

% DUAL FORMULATION NOTE:
% This story is one of five sibling readings of the marriage_authority_kernel, each authored as a separate constraint per the ε-invariance principle (a single 'personal law system' label conflates five structurally distinct authority claims with different beneficiary/victim structures and different ε values). The hindu_codified_reading is linked bidirectionally to all four siblings; disagreement is located in the axioms and reading_relations fields of cs_structure, not folded into any single story's classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
