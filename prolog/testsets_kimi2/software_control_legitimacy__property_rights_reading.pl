% ============================================================================
% CONSTRAINT STORY: software_control_legitimacy__property_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_control_legitimacy__property_rights_reading, []).

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
 *   constraint_id: software_control_legitimacy__property_rights_reading
 *   human_readable: Software Control as Property Right
 *   domain: software_engineering/political_economy
 *
 * SUMMARY:
 *   This constraint story captures the property_rights_reading of the
 *   contested software_control_legitimacy kernel. Under this reading, creator
 *   control over software is grounded in property rights tradition and
 *   justified by the need to protect investment and sustain commercial
 *   markets. The constraint operates through copyright, licensing, and
 *   technical protection measures that restrict use, modification, and
 *   distribution. It generates a genuine coordination functionâa market for
 *   complex softwareâbut asymmetrically extracts from users and FOSS
 *   advocates by suppressing unrestricted models. Sibling readings
 *   (freedom_imperative, pragmatic_openness, commons) assign different
 *   structural roles to the same kernel; per the Îµ-invariance principle,
 *   they are modeled as separate constraints.
 *
 * KEY AGENTS:
 *   - software_vendors: Primary beneficiary (powerful/mobile) â captures returns from enforced scarcity
 *   - investors: Secondary beneficiary (powerful/mobile) â allocates capital assuming enforceable control
 *   - foss_advocates: Primary target (moderate/constrained) â bears suppression of commons models
 *   - end_users: Secondary target (organized/constrained) â forfeits autonomy over computing environment
 *   - legislatures_and_courts: Agenda setter (institutional/analytical) â administers the legal framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_control_legitimacy__property_rights_reading, 0.55).
domain_priors:suppression_score(software_control_legitimacy__property_rights_reading, 0.62).
domain_priors:theater_ratio(software_control_legitimacy__property_rights_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_control_legitimacy__property_rights_reading, tangled_rope).
narrative_ontology:human_readable(software_control_legitimacy__property_rights_reading, "Software Control as Property Right").
narrative_ontology:topic_domain(software_control_legitimacy__property_rights_reading, "software_engineering/political_economy").

domain_priors:requires_active_enforcement(software_control_legitimacy__property_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_control_legitimacy__property_rights_reading, '69062fe8-6867-4434-889a-3046ea632187').
narrative_ontology:cs_kernel_codification('69062fe8-6867-4434-889a-3046ea632187', formalized).
narrative_ontology:cs_authority_grounding('69062fe8-6867-4434-889a-3046ea632187', lineage).
narrative_ontology:cs_interpretation_layer_present('69062fe8-6867-4434-889a-3046ea632187').
narrative_ontology:cs_reading_relation('69062fe8-6867-4434-889a-3046ea632187', software_control_legitimacy__freedom_imperative_reading, forecloses).
narrative_ontology:cs_reading_relation('69062fe8-6867-4434-889a-3046ea632187', software_control_legitimacy__pragmatic_openness_reading, coexists_with).
narrative_ontology:cs_reading_relation('69062fe8-6867-4434-889a-3046ea632187', software_control_legitimacy__commons_reading, influences).
narrative_ontology:cs_axiom('69062fe8-6867-4434-889a-3046ea632187', foundational, software_control_as_property_right).
narrative_ontology:cs_axiom_status(software_control_as_property_right, holdable).
narrative_ontology:cs_axiom_grounding('69062fe8-6867-4434-889a-3046ea632187', software_control_as_property_right, conventional).
narrative_ontology:cs_reference_frame('69062fe8-6867-4434-889a-3046ea632187', classical_property_rights_framework).
narrative_ontology:cs_drift_state('69062fe8-6867-4434-889a-3046ea632187', digital_replication_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('69062fe8-6867-4434-889a-3046ea632187', '').
narrative_ontology:cs_kernel_id(software_control_legitimacy__property_rights_reading, software_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_control_legitimacy__property_rights_reading, software_vendors).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__property_rights_reading, investors).
narrative_ontology:constraint_victim(software_control_legitimacy__property_rights_reading, foss_advocates).
narrative_ontology:constraint_victim(software_control_legitimacy__property_rights_reading, end_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop and distribute software under licenses that restrict copying, modification, and redistribution. Revenue depends on legal enforcement of these restrictions. They benefit from a market structure where control over digital artifacts can be monetized through scarcity.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, software_vendors, beneficiary,
    powerful, biographical, mobile, global).

% Provide capital to software ventures expecting that legal control over source code and distribution will protect market position and generate returns. Their investment models assume enforceable exclusion from competitors and users.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, investors, beneficiary,
    powerful, biographical, mobile, global).

% Develop and promote software under licenses permitting free use, modification, and sharing. They bear costs because the dominant property-rights framework treats unrestricted distribution as illegal, marginalizing commons-based models and restricting user freedoms.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, foss_advocates, payer,
    moderate, generational, constrained, global).

% Access and use software under terms that prohibit modification, reverse engineering, and sharing. They pay licensing fees and forfeit control over their computing environment. Alternatives exist but require abandoning mainstream tools and support ecosystems.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, end_users, payer,
    organized, biographical, constrained, global).

% Draft, amend, and adjudicate copyright, patent, and contract laws that define the scope of permissible software control. They set the rules that transform property claims into enforceable restrictions, operating within international treaty frameworks and domestic policy pressures.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, legislatures_and_courts, agenda_setter,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables a market for complex software by ensuring creators can capture returns on investment, theoretically incentivizing production that might not occur if digital goods were immediately commons.
% TRANSFER_FUNCTION: Transfers exclusive control over digital artifacts and their usage conditions from the public and user base to creators and vendors, enforced by copyright law and contract.
% ABSENT_VOICES: FOSS advocates who view unrestricted software freedom as an ethical baseline are structurally marginalized in policy debates framed around property rights. End-users who modify software for personal need are absent from licensing negotiations.
% DISAPPEARANCE_RATIONALE: Without enforceable software control, proprietary licensing collapses, investment shifts away from enclosed distribution models, and the software industry reorganizes around service, support, and commons-based funding.
% FOUNDING_PROBLEM: How to incentivize costly software development when digital goods are non-rival and trivially copyable.
% FOUNDING_PROBLEM_CORROBORATION: Software vendors and industry associations attest the problem is live. Independent economists and FOSS advocates contest it, pointing to thriving open-source ecosystems and alternative incentive structures. No consensus outside the benefiting parties.
narrative_ontology:disappearance_verdict(software_control_legitimacy__property_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_control_legitimacy__property_rights_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_control_legitimacy__property_rights_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(software_control_legitimacy__property_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_control_legitimacy__property_rights_reading, 0.55, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_control_legitimacy__property_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(software_control_legitimacy__property_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(software_control_legitimacy__property_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55) is moderate: the constraint genuinely enables a market for software that might be underproduced without investment protection, but the restriction of user freedoms and suppression of FOSS alternatives constitutes meaningful extraction. Suppression (0.62) is moderate-to-high: persistence depends on active legal and technical enforcement (DMCA, EULAs, DRM) to prevent copying and modification. Theater ratio (0.35) reflects growing performative enforcementâDRM that is routinely bypassed, licensing language that exceeds practical controlâwithout negating the real coordination function. Accessibility collapse (0.45) is incomplete: FOSS alternatives exist but face structural barriers (network effects, compatibility, funding). Resistance (0.55) reflects the sustained FOSS movement and routine user noncompliance. Temporal measurements show extraction and enforcement ratcheting upward from 1980â2030 as digital distribution matured, then plateauing as alternative models partially displaced pure proprietary reliance.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (legislatures) experiences the constraint as a policy instrument balancing innovation incentives against access costs. The beneficiary seats (vendors, investors) experience it as necessary market infrastructure. The payer seats (FOSS advocates, end users) experience it as an imposed restriction that denies autonomy and channels resources toward enclosed models. The engine computes this divergence from structural data: beneficiaries have mobile exit and global scope; payers are constrained with high directionality toward the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (software_vendors, investors) have low directionality: the constraint subsidizes their business models by creating legally enforceable scarcity. FOSS advocates have high directionality: the constraint directly targets their preferred model for suppression. End users have intermediate-high directionality: they are the object of restrictive terms but also receive some coordination benefit (access to funded software). The structural derivation from roles and exit options produces the spread.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resists classification as pure rope because the coordination function (investment protection) is inseparable from asymmetric extraction (freedom restriction). It resists classification as pure snare because the commercial software market is a real coordination achievement and many participants voluntarily transact. The Tangled Rope classification captures the hybrid: genuine coordination held in place by active enforcement that simultaneously suppresses alternatives and extracts from payers.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint instantiates the property_rights_reading of the software_control_legitimacy kernel. Would adopting the freedom_imperative or commons readings reclassify the constraint as a snare or dissolve it entirely?',
    'Comparative analysis of the sibling constraint stories generated from the same kernel.',
    'If the freedom-imperative reading is structurally valid, this constraint''s victim set expands and its coordination function evaporates, raising epsilon toward snare territory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Kernel reading contest and structural alternative').

omega_variable(
    property_rights_incentive_efficacy,
    'Do enforceable software property rights actually increase the quantity or quality of software production relative to commons-based alternatives?',
    'Econometric comparison of innovation rates, quality metrics, and developer livelihoods across proprietary and open-source ecosystems.',
    'If proprietary rights show no marginal innovation benefit, the coordination story is cover for extraction, supporting reclassification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(property_rights_incentive_efficacy, empirical, 'Empirical basis for the investment-incentive coordination claim').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_control_legitimacy__property_rights_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, software_control_legitimacy__property_rights_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(soft_tr_t10, software_control_legitimacy__property_rights_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(soft_tr_t20, software_control_legitimacy__property_rights_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(soft_tr_t30, software_control_legitimacy__property_rights_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement(soft_tr_t40, software_control_legitimacy__property_rights_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement(soft_tr_t50, software_control_legitimacy__property_rights_reading, theater_ratio, 50, 0.42).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, software_control_legitimacy__property_rights_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(soft_be_t10, software_control_legitimacy__property_rights_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(soft_be_t20, software_control_legitimacy__property_rights_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(soft_be_t30, software_control_legitimacy__property_rights_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(soft_be_t40, software_control_legitimacy__property_rights_reading, base_extractiveness, 40, 0.55).
narrative_ontology:measurement(soft_be_t50, software_control_legitimacy__property_rights_reading, base_extractiveness, 50, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t0, software_control_legitimacy__property_rights_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(soft_su_t10, software_control_legitimacy__property_rights_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(soft_su_t20, software_control_legitimacy__property_rights_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(soft_su_t30, software_control_legitimacy__property_rights_reading, suppression_requirement, 30, 0.65).
narrative_ontology:measurement(soft_su_t40, software_control_legitimacy__property_rights_reading, suppression_requirement, 40, 0.68).
narrative_ontology:measurement(soft_su_t50, software_control_legitimacy__property_rights_reading, suppression_requirement, 50, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_control_legitimacy__property_rights_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
