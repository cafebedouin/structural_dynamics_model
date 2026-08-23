% ============================================================================
% CONSTRAINT STORY: family_law_authority__secular_contractual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-25
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_family_law_authority__secular_contractual_reading, []).

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
 *   constraint_id: family_law_authority__secular_contractual_reading
 *   human_readable: Secular Civil Marriage Contract
 *   domain: legal/family/social
 *
 * SUMMARY:
 *   The secular contractual reading of family law authority treats marriage
 *   as a civil contract between autonomous individuals, valid solely through
 *   state registration. It emerged in the 19th century as states sought
 *   uniform marriage codes that could operate across religious pluralism and
 *   guarantee gender-equal rights. The reading has expanded over time to
 *   include interfaith couples (always permitted in principle) and, more
 *   recently, same-sex couples. It coexists with religious marriage systems:
 *   most jurisdictions allow religious solemnization to satisfy the civil
 *   registration requirement, but the civil code's validity criteria are
 *   independent of religious doctrine.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__secular_contractual_reading, 0.25).
domain_priors:suppression_score(family_law_authority__secular_contractual_reading, 0.2).
domain_priors:theater_ratio(family_law_authority__secular_contractual_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__secular_contractual_reading, rope).
narrative_ontology:human_readable(family_law_authority__secular_contractual_reading, "Secular Civil Marriage Contract").
narrative_ontology:topic_domain(family_law_authority__secular_contractual_reading, "legal/family/social").

domain_priors:requires_active_enforcement(family_law_authority__secular_contractual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__secular_contractual_reading, '0f8dc741-d4ad-47a3-ba41-3c4f7719538e').
narrative_ontology:cs_kernel_codification('0f8dc741-d4ad-47a3-ba41-3c4f7719538e', formalized).
narrative_ontology:cs_authority_grounding('0f8dc741-d4ad-47a3-ba41-3c4f7719538e', lineage).
narrative_ontology:cs_interpretation_layer_present('0f8dc741-d4ad-47a3-ba41-3c4f7719538e').
narrative_ontology:cs_reading_relation('0f8dc741-d4ad-47a3-ba41-3c4f7719538e', family_law_authority__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('0f8dc741-d4ad-47a3-ba41-3c4f7719538e', family_law_authority__hindu_dharmashastra_reading, coexists_with).
narrative_ontology:cs_reading_relation('0f8dc741-d4ad-47a3-ba41-3c4f7719538e', family_law_authority__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('0f8dc741-d4ad-47a3-ba41-3c4f7719538e', family_law_authority__parsi_zoroastrian_reading, coexists_with).
narrative_ontology:cs_axiom('0f8dc741-d4ad-47a3-ba41-3c4f7719538e', foundational, marriage_is_civil_contract_between_autonomous_individuals).
narrative_ontology:cs_axiom_status(marriage_is_civil_contract_between_autonomous_individuals, holdable).
narrative_ontology:cs_axiom_grounding('0f8dc741-d4ad-47a3-ba41-3c4f7719538e', marriage_is_civil_contract_between_autonomous_individuals, conventional).
narrative_ontology:cs_axiom('0f8dc741-d4ad-47a3-ba41-3c4f7719538e', secondary, state_registration_sufficient_for_legal_validity).
narrative_ontology:cs_axiom_status(state_registration_sufficient_for_legal_validity, holdable).
narrative_ontology:cs_axiom_grounding('0f8dc741-d4ad-47a3-ba41-3c4f7719538e', state_registration_sufficient_for_legal_validity, conventional).
narrative_ontology:cs_axiom('0f8dc741-d4ad-47a3-ba41-3c4f7719538e', secondary, gender_symmetry_in_marital_rights).
narrative_ontology:cs_axiom_status(gender_symmetry_in_marital_rights, holdable).
narrative_ontology:cs_axiom_grounding('0f8dc741-d4ad-47a3-ba41-3c4f7719538e', gender_symmetry_in_marital_rights, deontological).
narrative_ontology:cs_reference_frame('0f8dc741-d4ad-47a3-ba41-3c4f7719538e', secular_contractual_marriage_model).
narrative_ontology:cs_drift_state('0f8dc741-d4ad-47a3-ba41-3c4f7719538e', contemporary_human_rights_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('0f8dc741-d4ad-47a3-ba41-3c4f7719538e', '').
narrative_ontology:cs_kernel_id(family_law_authority__secular_contractual_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__secular_contractual_reading, secular_couples).
narrative_ontology:constraint_beneficiary(family_law_authority__secular_contractual_reading, interfaith_couples).
narrative_ontology:constraint_beneficiary(family_law_authority__secular_contractual_reading, lgbtq_couples).
narrative_ontology:constraint_beneficiary(family_law_authority__secular_contractual_reading, state_administration).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(family_law_authority__secular_contractual_reading, legal_profession).
narrative_ontology:constraint_victim(family_law_authority__secular_contractual_reading, children_of_marriage).
narrative_ontology:constraint_vindicates(family_law_authority__secular_contractual_reading, civil_contractual_model_of_marriage).
narrative_ontology:constraint_vindicates(family_law_authority__secular_contractual_reading, gender_symmetric_legal_rights).
narrative_ontology:constraint_vindicates(family_law_authority__secular_contractual_reading, state_registration_as_validity_criterion).
narrative_ontology:constraint_vindicates(family_law_authority__secular_contractual_reading, interfaith_marriage_permissibility).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacts and administers the civil marriage code: sets eligibility criteria, registration procedures, default property regimes, and dissolution rules. Collects licensing fees. Maintains the registry as the authoritative record. Can amend the code legislatively.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, state_administration, agenda_setter,
    institutional, generational, arbitrage, national).

% Couples who marry without religious ceremony. Gain immediate legal recognition, default property and inheritance rights, medical decision-making authority, tax benefits, and a standardized dissolution process. Can opt out by not marrying or by using private contracts (costlier, less comprehensive).
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, secular_couples, beneficiary,
    organized, biographical, mobile, national).

% Couples from different religious backgrounds. Civil marriage is often the only path to legal recognition without conversion or religious compromise. Religious authorities may refuse to solemnize; the civil route bypasses that gate. Exit to religious-only marriage is blocked by doctrinal barriers.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, interfaith_couples, beneficiary,
    moderate, biographical, constrained, national).

% Same-sex and gender-diverse couples. In jurisdictions where civil marriage is gender-neutral, this is the primary route to legal recognition. Religious marriage is largely unavailable. Exit options are limited to jurisdictions without recognition or to private contractual approximations.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, lgbtq_couples, beneficiary,
    moderate, biographical, constrained, national).

% Churches, temples, mosques, and other religious bodies that maintain their own marriage rites and rules. They are not bound by the civil code's gender symmetry or interfaith permissibility. They can solemnize marriages that also receive civil recognition if they comply with registration requirements, but their internal validity criteria remain distinct.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, religious_institutions, excluded,
    powerful, generational, arbitrage, national).

% Children born or adopted into civil marriages. Bear the consequences of the default property and custody rules upon dissolution. Have no say in the marital contract but are structurally subject to its terms. Exit is impossible until majority.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, children_of_marriage, payer,
    powerless, biographical, trapped, national).

% Lawyers, notaries, and judges who administer marriage licenses, draft prenuptial agreements, and handle dissolution proceedings. The civil code creates a steady stream of standardized work. They benefit from the system's complexity and dispute volume.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, legal_profession, beneficiary,
    organized, biographical, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a uniform, religion-neutral legal framework for recognizing intimate partnerships: establishes default property regimes, inheritance rights, medical decision-making authority, tax status, and a standardized dissolution process without requiring religious assent or gender-asymmetric rules.
% TRANSFER_FUNCTION: Moves legal recognition and a bundle of default rights from the state to the marrying couple; the state collects licensing fees and retains regulatory control over eligibility and dissolution. The transfer is bidirectional: couples gain rights, the state gains administrative coherence and revenue.
% ABSENT_VOICES: Religious traditionalists who hold that marriage is inherently sacramental and cannot be reduced to a civil contract; polygamous communities whose marital form is excluded by the dyadic model; anarchist or anti-statist groups who reject state registration of intimate life as such. These voices are structurally excluded because the civil code's legitimacy rests on state authority, not their consent.
% DISAPPEARANCE_RATIONALE: If civil marriage vanished overnight, couples would lose default legal protections: no automatic inheritance, no spousal medical authority, no tax filing status, no standardized divorce process. Religious marriages would fill the gap for believers, but interfaith, secular, and LGBTQ+ couples would have only private contracts — costlier, less comprehensive, and unevenly enforced. The state would lose its primary mechanism for tracking household composition.
% FOUNDING_PROBLEM: Nineteenth-century European and post-colonial states needed a uniform marriage law that could operate across religious differences, guarantee gender-equal rights, and serve administrative coherence (census, taxation, inheritance). Religious codes were plural, gender-asymmetric, and inapplicable to interfaith or non-believing couples.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians (e.g., Grossbard-Shechtman on marriage markets), human rights bodies (UN Human Rights Committee General Comment 28), and interfaith advocacy organizations attest that the founding problem — uniform recognition across religious difference with gender symmetry — remains live. The state's own legislative revisions (same-sex marriage, gender-neutral language) confirm the problem persists in new forms.
narrative_ontology:disappearance_verdict(family_law_authority__secular_contractual_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__secular_contractual_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__secular_contractual_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(family_law_authority__secular_contractual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(family_law_authority__secular_contractual_reading, 0.25, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(family_law_authority__secular_contractual_reading_tests).
:- end_tests(family_law_authority__secular_contractual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low and declining (0.35→0.15): licensing fees are nominal, and the state's regulatory control serves administrative coherence more than revenue. Suppression is low and declining (0.4→0.15): religious and customary alternatives persist, and exit to cohabitation or private contract is legally available (though less comprehensive). Theater ratio is very low (0.15→0.05): the system performs its coordination function with minimal performative overhead. Accessibility collapse is moderate (0.35): the civil route is highly accessible, but the bundle of rights it confers is difficult to replicate privately. Resistance is moderate (0.35): religious conservatives contest the gender symmetry and same-sex inclusion, but the civil code's legitimacy is broadly accepted.
 *
 * PERSPECTIVAL GAP:
 *   From the state_administration seat, the constraint is pure coordination (rope). From the children_of_marriage seat, it imposes binding default rules they never chose (payer experience). From religious_institutions, it is a parallel system they can ignore or engage with selectively (excluded seat). The engine computes these divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The state_administration is the agenda_setter with institutional power and arbitrage-grade exit (it can amend the code). Secular, interfaith, and LGBTQ+ couples are beneficiaries with organized/moderate power and mobile-to-constrained exit. Children are powerless payers with trapped exit — they bear the consequences of dissolution rules without consent. Religious institutions are excluded but powerful, with arbitrage exit (they operate their own parallel systems). The legal profession benefits as a secondary beneficiary.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (uniform recognition across religious difference with gender symmetry) remains live. The constraint has not atrophied; it has expanded its beneficiary set. No mandatrophy resolution is declared.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    christian_assumptions_in_civil_code,
    'Does the secular civil marriage code smuggle in Christian assumptions (monogamy, dyadic structure, indissolubility norms, consanguinity rules) despite claiming neutrality?',
    'Comparative legal genealogy tracing the provenance of each default rule (e.g., monogamy requirement, prohibited degrees, divorce grounds) to its historical source. Cross-jurisdictional analysis of codes with different religious histories.',
    'If the code''s default rules are historically Christian, the ''neutral coordination'' claim is undermined; the constraint may function as a Christian-normative imposition on non-Christian populations, raising extraction for those groups.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(christian_assumptions_in_civil_code, empirical, 'Whether the civil code''s neutral facade conceals Christian-normative defaults.').

omega_variable(
    registration_as_control_vs_coordination,
    'Is the state registration requirement a genuine coordination mechanism (preventing bigamy, protecting third parties) or a mechanism of state control over intimate life?',
    'Counterfactual analysis: jurisdictions with minimal registration (e.g., common-law marriage recognition) vs. strict registration. Measure bigamy rates, third-party reliance costs, and state surveillance capacity.',
    'If registration is primarily control, suppression is understated; the constraint extracts compliance from couples who would prefer informal recognition. If coordination, the current suppression metric is accurate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(registration_as_control_vs_coordination, conceptual, 'Whether state registration serves coordination or control.').

omega_variable(
    civil_religious_marriage_boundary,
    'Does the availability of civil marriage undermine religious marriage systems (by providing an exit option) or stabilize them (by offloading couples who would otherwise pressure religious authorities for reform)?',
    'Longitudinal study of religious marriage rates and internal reform movements before/after civil marriage introduction. Compare jurisdictions with/without civil marriage.',
    'If civil marriage undermines religious systems, it extracts from religious institutions by draining adherents. If it stabilizes them, it functions as a release valve that preserves religious autonomy. Changes the network.affects_constraints mapping.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(civil_religious_marriage_boundary, empirical, 'Direction of structural influence between civil and religious marriage systems.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__secular_contractual_reading, 1800, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fami_tr_t1800, family_law_authority__secular_contractual_reading, theater_ratio, 1800, 0.15).
narrative_ontology:measurement(fami_tr_t1850, family_law_authority__secular_contractual_reading, theater_ratio, 1850, 0.12).
narrative_ontology:measurement(fami_tr_t1900, family_law_authority__secular_contractual_reading, theater_ratio, 1900, 0.1).
narrative_ontology:measurement(fami_tr_t1950, family_law_authority__secular_contractual_reading, theater_ratio, 1950, 0.08).
narrative_ontology:measurement(fami_tr_t2000, family_law_authority__secular_contractual_reading, theater_ratio, 2000, 0.07).
narrative_ontology:measurement(fami_tr_t2024, family_law_authority__secular_contractual_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(fami_be_t1800, family_law_authority__secular_contractual_reading, base_extractiveness, 1800, 0.35).
narrative_ontology:measurement(fami_be_t1850, family_law_authority__secular_contractual_reading, base_extractiveness, 1850, 0.3).
narrative_ontology:measurement(fami_be_t1900, family_law_authority__secular_contractual_reading, base_extractiveness, 1900, 0.25).
narrative_ontology:measurement(fami_be_t1950, family_law_authority__secular_contractual_reading, base_extractiveness, 1950, 0.2).
narrative_ontology:measurement(fami_be_t2000, family_law_authority__secular_contractual_reading, base_extractiveness, 2000, 0.18).
narrative_ontology:measurement(fami_be_t2024, family_law_authority__secular_contractual_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(fami_su_t1800, family_law_authority__secular_contractual_reading, suppression_requirement, 1800, 0.4).
narrative_ontology:measurement(fami_su_t1850, family_law_authority__secular_contractual_reading, suppression_requirement, 1850, 0.35).
narrative_ontology:measurement(fami_su_t1900, family_law_authority__secular_contractual_reading, suppression_requirement, 1900, 0.3).
narrative_ontology:measurement(fami_su_t1950, family_law_authority__secular_contractual_reading, suppression_requirement, 1950, 0.25).
narrative_ontology:measurement(fami_su_t2000, family_law_authority__secular_contractual_reading, suppression_requirement, 2000, 0.2).
narrative_ontology:measurement(fami_su_t2024, family_law_authority__secular_contractual_reading, suppression_requirement, 2024, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__secular_contractual_reading, information_standard).
narrative_ontology:boltzmann_floor_override(family_law_authority__secular_contractual_reading, 0.02).
narrative_ontology:affects_constraint(family_law_authority__secular_contractual_reading, family_law_authority__hindu_dharmashastra_reading).
narrative_ontology:affects_constraint(family_law_authority__secular_contractual_reading, family_law_authority__muslim_shariat_reading).
narrative_ontology:affects_constraint(family_law_authority__secular_contractual_reading, family_law_authority__christian_canonical_reading).
narrative_ontology:affects_constraint(family_law_authority__secular_contractual_reading, family_law_authority__parsi_zoroastrian_reading).

% DUAL FORMULATION NOTE:
% The family_law_authority kernel decomposes into five readings. This reading (secular_contractual) provides the state-administered baseline that the religious readings either supplement, replace, or reject. The religious readings typically require their own validity criteria PLUS civil registration for state recognition, making civil registration a downstream dependency. This reading influences the siblings by setting the state-recognition floor they must meet.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
