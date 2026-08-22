% ============================================================================
% CONSTRAINT STORY: family_law_authority__secular_contractual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: Marriage as Secular Civil Contract Under State Law
 *   domain: comparative_law/political_theory/religious_governance
 *
 * SUMMARY:
 *   This constraint story models the secular_contractual_reading of the
 *   family_law_authority kernel: marriage as a civil contract validated
 *   exclusively by state registration, entered by autonomous individuals with
 *   gender-symmetric rights, permitting interfaith unions and requiring no
 *   religious test. It operates in comparative legal systems (exemplified by
 *   the Indian Special Marriage Act, French civil code, and Turkish secular
 *   family law) where the state claims monopoly over marital validity. The
 *   reading coexists with sibling religious readings in pluralist
 *   jurisdictions but contests their authority by asserting state
 *   registration as the sole criterion.
 *
 * KEY AGENTS:
 *   - civil_registrar (institutional/agenda_setter): Sets and administers the rules of valid marriage, registration procedures, and dissolution grounds.
 *   - secular_spouses (moderate/beneficiary): Enter marriage for legal protections without religious requirements.
 *   - interfaith_partners (moderate/beneficiary): Rely on the secular route to marry across religious boundaries.
 *   - religious_communities (organized/payer): Bear the cost of displaced communal jurisdiction over marriage.
 *   - religious_judges_clergy (institutional/payer): Lose legal standing as marital validators and arbiters.
 *   - womens_rights_advocates (organized/beneficiary): Use the secular framework to secure gender-symmetric rights unavailable under patriarchal religious personal laws.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__secular_contractual_reading, 0.45).
domain_priors:suppression_score(family_law_authority__secular_contractual_reading, 0.55).
domain_priors:theater_ratio(family_law_authority__secular_contractual_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__secular_contractual_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__secular_contractual_reading, "Marriage as Secular Civil Contract Under State Law").
narrative_ontology:topic_domain(family_law_authority__secular_contractual_reading, "comparative_law/political_theory/religious_governance").

domain_priors:requires_active_enforcement(family_law_authority__secular_contractual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__secular_contractual_reading, '6e9f4d3d-529c-42d2-9ef3-588055472468').
narrative_ontology:cs_kernel_codification('6e9f4d3d-529c-42d2-9ef3-588055472468', formalized).
narrative_ontology:cs_authority_grounding('6e9f4d3d-529c-42d2-9ef3-588055472468', lineage).
narrative_ontology:cs_interpretation_layer_present('6e9f4d3d-529c-42d2-9ef3-588055472468').
narrative_ontology:cs_reading_relation('6e9f4d3d-529c-42d2-9ef3-588055472468', family_law_authority__hindu_dharmashastra_reading, coexists_with).
narrative_ontology:cs_reading_relation('6e9f4d3d-529c-42d2-9ef3-588055472468', family_law_authority__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('6e9f4d3d-529c-42d2-9ef3-588055472468', family_law_authority__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('6e9f4d3d-529c-42d2-9ef3-588055472468', family_law_authority__parsi_zoroastrian_reading, coexists_with).
narrative_ontology:cs_axiom('6e9f4d3d-529c-42d2-9ef3-588055472468', foundational, state_registration_sole_validity_criterion).
narrative_ontology:cs_axiom_status(state_registration_sole_validity_criterion, holdable).
narrative_ontology:cs_axiom_grounding('6e9f4d3d-529c-42d2-9ef3-588055472468', state_registration_sole_validity_criterion, conventional).
narrative_ontology:cs_axiom('6e9f4d3d-529c-42d2-9ef3-588055472468', foundational, gender_symmetric_autonomous_consent).
narrative_ontology:cs_axiom_status(gender_symmetric_autonomous_consent, holdable).
narrative_ontology:cs_axiom_grounding('6e9f4d3d-529c-42d2-9ef3-588055472468', gender_symmetric_autonomous_consent, deontological).
narrative_ontology:cs_reference_frame('6e9f4d3d-529c-42d2-9ef3-588055472468', secular_legal_positivism).
narrative_ontology:cs_drift_state('6e9f4d3d-529c-42d2-9ef3-588055472468', contemporary_pluralist_challenge, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6e9f4d3d-529c-42d2-9ef3-588055472468', '').
narrative_ontology:cs_kernel_id(family_law_authority__secular_contractual_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__secular_contractual_reading, secular_spouses).
narrative_ontology:constraint_beneficiary(family_law_authority__secular_contractual_reading, interfaith_partners).
narrative_ontology:constraint_beneficiary(family_law_authority__secular_contractual_reading, womens_rights_advocates).
narrative_ontology:constraint_victim(family_law_authority__secular_contractual_reading, religious_communities).
narrative_ontology:constraint_victim(family_law_authority__secular_contractual_reading, religious_judges_clergy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers marriage registration, verifies consent and capacity, issues certificates, and adjudicates validity. Defines the sole legal path to marital status and the conditions for dissolution. Cannot exit the state mandate but operates its enforcement machinery.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, civil_registrar, agenda_setter,
    institutional, generational, constrained, national).

% Enter marriage for inheritance, tax, medical proxy, and child custody protections without submitting to religious tests. Their legal benefits are contingent on state paperwork and compliance with civil formalities.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, secular_spouses, beneficiary,
    moderate, biographical, constrained, national).

% Rely on the secular contractual channel to marry across religious boundaries without conversion or community approval. Without this route, their union would lack legal validity in many pluralist jurisdictions.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, interfaith_partners, beneficiary,
    moderate, biographical, constrained, national).

% Perform religious marriage ceremonies that no longer confer legal status automatically. Members must undergo separate civil registration, diluting communal authority over family formation and dissolution.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, religious_communities, payer,
    organized, generational, constrained, national).

% Lose historical jurisdiction over marital validity and divorce adjudication. Their rulings bind only the conscience, not the state; they must defer to civil courts for legal enforcement.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, religious_judges_clergy, payer,
    institutional, generational, constrained, national).

% Use the secular framework to secure gender-symmetric property, inheritance, and divorce rights that are often asymmetrical under patriarchal religious personal laws. They lobby to expand and enforce the civil contract's equality promises.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, womens_rights_advocates, beneficiary,
    organized, biographical, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a uniform, predictable legal framework for property, inheritance, medical proxy, and child custody between spouses, eliminating the transaction costs of private contracting for each incident and standardizing dissolution procedures.
% TRANSFER_FUNCTION: Moves jurisdictional authority over marital validity, registration, and dissolution from religious communities and customary institutions to state registrars and courts; moves legal protections and status benefits to registered couples conditioned on state compliance.
% ABSENT_VOICES: Religious minorities seeking recognized communal legal autonomy and traditional authorities who would govern marriage under dharmashastra, shariat, or canonical law are structurally subordinated; they would argue for plural jurisdiction but are excluded from the validity framework.
% DISAPPEARANCE_RATIONALE: If the secular civil contract vanished overnight, inheritance claims, tax statuses, medical proxies, and divorce jurisdictions would fall into legal uncertainty; religious communities would reclaim marital jurisdiction, interfaith couples would lose legal shelter, and family courts would lose their primary docket anchor.
% FOUNDING_PROBLEM: Pre-modern and colonial legal pluralism created overlapping, inconsistent marital jurisdictions with uncertain property and inheritance outcomes, especially disadvantaging women and interfaith unions.
% FOUNDING_PROBLEM_CORROBORATION: Secular legal historians and feminist jurists attest the problem of legal uncertainty and gender asymmetry under plural personal law. Religious authorities dispute that pluralism was a problem, arguing that communal harmony existed before state intervention; they corroborate the persistence of their own frameworks but not the necessity of the secular replacement.
narrative_ontology:disappearance_verdict(family_law_authority__secular_contractual_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__secular_contractual_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__secular_contractual_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(family_law_authority__secular_contractual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(family_law_authority__secular_contractual_reading, 0.45, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.45) reflects the jurisdictional extraction the state performs by monopolizing legal validity; this is not monetary rent but compliance and authority. Suppression (0.55) captures the legal invalidity imposed on religious-only marriages, collapsing alternative validity structures for those seeking state-recognized status. Theater ratio (0.25) is relatively low because the coordination function (property, inheritance, medical proxy) is genuine and operational, though some performative compliance exists around registration rituals. Accessibility collapse (0.70) is high because once legal benefits are tied to registration, non-state alternatives effectively disappear for pragmatic purposes. Resistance (0.50) reflects ongoing religious and political mobilization against secular family law in many jurisdictions.
 *
 * PERSPECTIVAL GAP:
 *   From the registrar's seat, the constraint is necessary coordination providing legal certainty and gender equality; from the religious community seat, the same structure is jurisdictional displacement and cultural extraction. The engine computes this divergence from the structural data â the same legal form reads as liberation or imposition depending on whether the agent's authority is expanded or collapsed by it.
 *
 * DIRECTIONALITY LOGIC:
 *   The civil registrar and the state legal system sit near the beneficiary end (low d): they collect jurisdictional authority and administrative control. Secular spouses, interfaith partners, and women's rights advocates are net beneficiaries of the legal protections (low-to-moderate d). Religious communities and clergy are the structural targets (high d): they lose legal authority and must undergo state procedures to gain recognition for their members. The engine will compute higher effective extraction for the religious seats and lower (possibly negative, i.e., subsidy) for the secular beneficiary seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â legal uncertainty under plural jurisdiction â was genuine, and the secular contractual reading still coordinates real benefits (property clarity, medical proxy, interfaith shelter). However, the arrangement has outlived its purely transitional rationale in some jurisdictions, persisting as a steady-state jurisdictional monopoly. The Tangled Rope classification captures that the coordination is real but asymmetric: some are coordinated (secular individuals) while others pay (religious communities). A Scaffold reading would require a sunset clause, which is absent; a Snare reading would deny the coordination function, which is real. Piton is inappropriate because the function is not atrophied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    secular_validity_as_coordination_or_extraction,
    'Is state registration as the sole validity criterion a genuine coordination necessity for legal certainty, or does it extract jurisdictional authority from religious communities that could otherwise administer valid marriage law?',
    'Comparative analysis of jurisdictions with legal pluralism (recognized religious personal law) versus unitary civil systems: measuring legal certainty outcomes, women''s rights enforcement, and interfaith access in both models.',
    'If pluralism delivers equivalent coordination, the sole-validity claim is jurisdictional extraction (Snare-like); if unitary civil registration is necessary for the coordination benefits claimed, the extraction is the price of coordination (Tangled Rope confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secular_validity_as_coordination_or_extraction, conceptual, 'Whether sole state validity is coordination floor or jurisdictional extraction').

omega_variable(
    kernel_reading_boundary,
    'This constraint is the secular_contractual_reading of family_law_authority. Would adopting any sibling reading (e.g., hindu_dharmashastra_reading) as the sole framework change the beneficiary/victim structure, and which specific structural element would shift?',
    'Cross-reading structural comparison: the sibling readings differ on the authority that validates marriage (state registrar vs. religious authority), which would swap the agenda_setter and redistribute the payer/beneficiary seats.',
    'The agenda_setter would shift from state to religious authority; the payer seat would shift from religious communities to secular individuals and women seeking gender symmetry; the coordination function would remain but its enforcement mechanism would change.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Structural delta between this reading and sibling kernel readings').

omega_variable(
    gender_symmetry_practice_gap,
    'Does the secular contractual reading''s formal gender symmetry survive into practice, or do patriarchal social norms reconstitute asymmetry within the civil form?',
    'Empirical study of property and divorce outcomes in secular civil marriages across jurisdictions, controlling for education and class.',
    'If practice gaps are large, the coordination benefit is partially theatrical and the constraint extracts performative compliance without delivering promised symmetry; if gaps are small, the formal symmetry is operationally real.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gender_symmetry_practice_gap, empirical, 'Whether formal gender symmetry holds in lived outcomes').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__secular_contractual_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fla_secular_tr_t0, family_law_authority__secular_contractual_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(fla_secular_tr_t14, family_law_authority__secular_contractual_reading, theater_ratio, 14, 0.18).
narrative_ontology:measurement(fla_secular_tr_t28, family_law_authority__secular_contractual_reading, theater_ratio, 28, 0.2).
narrative_ontology:measurement(fla_secular_tr_t42, family_law_authority__secular_contractual_reading, theater_ratio, 42, 0.22).
narrative_ontology:measurement(fla_secular_tr_t56, family_law_authority__secular_contractual_reading, theater_ratio, 56, 0.24).
narrative_ontology:measurement(fla_secular_tr_t70, family_law_authority__secular_contractual_reading, theater_ratio, 70, 0.25).

% Extraction over time
narrative_ontology:measurement(fla_secular_be_t0, family_law_authority__secular_contractual_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(fla_secular_be_t14, family_law_authority__secular_contractual_reading, base_extractiveness, 14, 0.38).
narrative_ontology:measurement(fla_secular_be_t28, family_law_authority__secular_contractual_reading, base_extractiveness, 28, 0.4).
narrative_ontology:measurement(fla_secular_be_t42, family_law_authority__secular_contractual_reading, base_extractiveness, 42, 0.42).
narrative_ontology:measurement(fla_secular_be_t56, family_law_authority__secular_contractual_reading, base_extractiveness, 56, 0.43).
narrative_ontology:measurement(fla_secular_be_t70, family_law_authority__secular_contractual_reading, base_extractiveness, 70, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(fla_secular_su_t0, family_law_authority__secular_contractual_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(fla_secular_su_t14, family_law_authority__secular_contractual_reading, suppression_requirement, 14, 0.45).
narrative_ontology:measurement(fla_secular_su_t28, family_law_authority__secular_contractual_reading, suppression_requirement, 28, 0.5).
narrative_ontology:measurement(fla_secular_su_t42, family_law_authority__secular_contractual_reading, suppression_requirement, 42, 0.53).
narrative_ontology:measurement(fla_secular_su_t56, family_law_authority__secular_contractual_reading, suppression_requirement, 56, 0.55).
narrative_ontology:measurement(fla_secular_su_t70, family_law_authority__secular_contractual_reading, suppression_requirement, 70, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__secular_contractual_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(family_law_authority__secular_contractual_reading, 0.08).
narrative_ontology:affects_constraint(family_law_authority__secular_contractual_reading, family_law_authority__hindu_dharmashastra_reading).
narrative_ontology:affects_constraint(family_law_authority__secular_contractual_reading, family_law_authority__muslim_shariat_reading).
narrative_ontology:affects_constraint(family_law_authority__secular_contractual_reading, family_law_authority__christian_canonical_reading).
narrative_ontology:affects_constraint(family_law_authority__secular_contractual_reading, family_law_authority__parsi_zoroastrian_reading).

% DUAL FORMULATION NOTE:
% This constraint and its siblings decompose the natural-language label 'family law authority' into structurally distinct claims about who validates marriage. The secular reading asserts state monopoly; the religious readings assert communal/sacramental authority. Their epsilon values differ because the extraction profile of a state monopoly (jurisdictional) differs from that of a religious authority (communal boundary maintenance). They form a constraint family linked by shared regulatory domain and mutual exclusivity claims over marital validity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
