% ============================================================================
% CONSTRAINT STORY: software_control_legitimacy__freedom_imperative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_control_legitimacy__freedom_imperative_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: software_control_legitimacy__freedom_imperative_reading
 *   human_readable: Freedom Imperative Reading: Proprietary Software as Categorical Denial of User Control
 *   domain: software_engineering/political_economy_of_technology/intellectual_property
 *
 * SUMMARY:
 *   This story instantiates the freedom_imperative_reading of the
 *   software_control_legitimacy kernel: the claim that user control over
 *   computing is a fundamental right, such that any proprietary software
 *   categorically denies that right regardless of how it was licensed,
 *   priced, or adopted. This is a distinct constraint from the
 *   pragmatic_openness_reading (which treats openness as a methodology with
 *   legitimate proprietary alternatives), the property_rights_reading (which
 *   treats restriction as a legitimate exercise of creator authority), and
 *   the commons_reading (which treats the question as one of negotiated
 *   collective governance rather than an absolute rights claim). Each reading
 *   has a different beneficiary/victim structure and a different epsilon;
 *   this file does not average or hedge across them. Here, ALL proprietary
 *   software enters the victim-adjacent structure (vendors, their enterprise
 *   customers, and their under-NDA developers all bear the categorical
 *   illegitimacy verdict), and 'users as rights-holders' plus organized
 *   free-software advocacy bodies are the structural beneficiaries of the
 *   moral and mobilizational capital the reading generates.
 *
 * KEY AGENTS:
 *   - software_users_as_rights_holders: designated rights-holders whose consent to proprietary terms is not treated as curing the illegitimacy
 *   - proprietary_software_vendors: categorically illegitimate under this reading regardless of conduct
 *   - closed_source_dependent_enterprise_customers: their considered preference is recast as harm
 *   - commercial_software_developers_under_nda: individually implicated by employment structure
 *   - free_software_foundation_and_affiliated_advocacy_bodies: sets and enforces the substantive definition of legitimacy, and is the seat that most directly captures the reading's mobilizational and moral capital
 *   - independent_software_ecosystem_observers: analytical seat with no stake in the legitimacy claim itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_control_legitimacy__freedom_imperative_reading, 0.71).
domain_priors:suppression_score(software_control_legitimacy__freedom_imperative_reading, 0.58).
domain_priors:theater_ratio(software_control_legitimacy__freedom_imperative_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_control_legitimacy__freedom_imperative_reading, tangled_rope).
narrative_ontology:human_readable(software_control_legitimacy__freedom_imperative_reading, "Freedom Imperative Reading: Proprietary Software as Categorical Denial of User Control").
narrative_ontology:topic_domain(software_control_legitimacy__freedom_imperative_reading, "software_engineering/political_economy_of_technology/intellectual_property").

domain_priors:requires_active_enforcement(software_control_legitimacy__freedom_imperative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_control_legitimacy__freedom_imperative_reading, '7ed5d21a-d9d6-4b89-a656-cb0eb7949021').
narrative_ontology:cs_kernel_codification('7ed5d21a-d9d6-4b89-a656-cb0eb7949021', distributed).
narrative_ontology:cs_authority_grounding('7ed5d21a-d9d6-4b89-a656-cb0eb7949021', practice).
narrative_ontology:cs_interpretation_layer_present('7ed5d21a-d9d6-4b89-a656-cb0eb7949021').
narrative_ontology:cs_reading_relation('7ed5d21a-d9d6-4b89-a656-cb0eb7949021', software_control_legitimacy__pragmatic_openness_reading, influences).
narrative_ontology:cs_reading_relation('7ed5d21a-d9d6-4b89-a656-cb0eb7949021', software_control_legitimacy__property_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('7ed5d21a-d9d6-4b89-a656-cb0eb7949021', software_control_legitimacy__commons_reading, influences).
narrative_ontology:cs_axiom('7ed5d21a-d9d6-4b89-a656-cb0eb7949021', foundational, user_control_is_inalienable_right).
narrative_ontology:cs_axiom_status(user_control_is_inalienable_right, holdable).
narrative_ontology:cs_axiom_grounding('7ed5d21a-d9d6-4b89-a656-cb0eb7949021', user_control_is_inalienable_right, deontological).
narrative_ontology:cs_axiom('7ed5d21a-d9d6-4b89-a656-cb0eb7949021', foundational, consent_to_proprietary_terms_does_not_cure_violation).
narrative_ontology:cs_axiom_status(consent_to_proprietary_terms_does_not_cure_violation, holdable).
narrative_ontology:cs_axiom_grounding('7ed5d21a-d9d6-4b89-a656-cb0eb7949021', consent_to_proprietary_terms_does_not_cure_violation, deontological).
narrative_ontology:cs_axiom('7ed5d21a-d9d6-4b89-a656-cb0eb7949021', secondary, software_quality_is_secondary_to_freedom_status).
narrative_ontology:cs_axiom_status(software_quality_is_secondary_to_freedom_status, holdable).
narrative_ontology:cs_axiom_grounding('7ed5d21a-d9d6-4b89-a656-cb0eb7949021', software_quality_is_secondary_to_freedom_status, conventional).
narrative_ontology:cs_reference_frame('7ed5d21a-d9d6-4b89-a656-cb0eb7949021', four_freedoms_founding_charter).
narrative_ontology:cs_drift_state('7ed5d21a-d9d6-4b89-a656-cb0eb7949021', cloud_and_saas_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('7ed5d21a-d9d6-4b89-a656-cb0eb7949021', '').
narrative_ontology:cs_kernel_id(software_control_legitimacy__freedom_imperative_reading, software_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_control_legitimacy__freedom_imperative_reading, software_users_as_rights_holders).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__freedom_imperative_reading, free_software_foundation_and_affiliated_advocacy_bodies).
narrative_ontology:constraint_victim(software_control_legitimacy__freedom_imperative_reading, proprietary_software_vendors).
narrative_ontology:constraint_victim(software_control_legitimacy__freedom_imperative_reading, closed_source_dependent_enterprise_customers).
narrative_ontology:constraint_victim(software_control_legitimacy__freedom_imperative_reading, commercial_software_developers_under_nda).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__freedom_imperative_reading, closed_source_dependent_enterprise_customers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Framed under this reading as holders of an inherent moral entitlement to inspect, modify, and redistribute the software running their own machines. Where they run proprietary software they are, under this reading, being denied a freedom they are owed regardless of any license they accepted; the reading treats their consent to proprietary terms as not curing the underlying illegitimacy.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, software_users_as_rights_holders, beneficiary,
    organized, generational, constrained, global).

% Build businesses on withholding source code and controlling modification and redistribution, typically to fund development, protect trade secrets, and sustain a commercial model. Under this reading their entire operating model is categorized as an ethics violation regardless of price, quality, or the voluntariness of the transaction with users; they cannot become legitimate without abandoning closed source.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, proprietary_software_vendors, payer,
    powerful, biographical, constrained, global).

% Enterprises that have selected proprietary tools for support guarantees, vendor accountability, or integration reasons. Under this reading, their considered choice is recast as a symptom of the freedom violation rather than a legitimate preference; migrating away from entrenched proprietary stacks carries real switching costs, which the reading treats as evidence of the harm rather than as a tradeoff the customer accepted.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, closed_source_dependent_enterprise_customers, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(software_control_legitimacy__freedom_imperative_reading, closed_source_dependent_enterprise_customers, beneficiary).

% Individual engineers who write closed-source code as employees, often under contractual obligations to keep source proprietary. Under this reading their labor is participating in an ethically illegitimate structure regardless of their own views on the matter or their employment necessity; the reading offers them no path to legitimacy short of refusing such employment.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, commercial_software_developers_under_nda, payer,
    moderate, biographical, constrained, national).

% Articulate and enforce the four-freedoms framework (run, study, modify, share) as the operative definition of legitimate software, publish license-compliance rulings, campaign against proprietary formats and DRM, and organize boycotts and public pressure against vendors deemed non-compliant. They set the substantive content of what counts as ethical software under this reading and derive organizational relevance, funding, and moral standing from maintaining the categorical position.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, free_software_foundation_and_affiliated_advocacy_bodies, agenda_setter,
    organized, civilizational, mobile, global).
narrative_ontology:stakeholder_secondary_role(software_control_legitimacy__freedom_imperative_reading, free_software_foundation_and_affiliated_advocacy_bodies, beneficiary).

% Historians and economists of technology who track licensing trends, adoption patterns, and welfare outcomes across proprietary and open models without a stake in either camp's legitimacy claim. They can compare outcomes but are not parties to the ethical dispute the reading stakes out.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, independent_software_ecosystem_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_control_legitimacy__freedom_imperative_reading, free_software_foundation_and_affiliated_advocacy_bodies).
narrative_ontology:fixing_cost_class(software_control_legitimacy__freedom_imperative_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The reading coordinates a movement of developers, users, and advocacy organizations around a shared standard of software legitimacy (the four freedoms), enabling collective action — license drafting, compliance campaigns, boycotts, alternative tooling — that no single actor could mount alone.
% TRANSFER_FUNCTION: Moves moral and reputational standing away from proprietary vendors and their customers and developers, and toward free-software advocacy organizations and the users the reading designates as rights-holders; also moves developer labor and user migration effort toward free-software alternatives under moral pressure.
% ABSENT_VOICES: Users who have knowingly and repeatedly chosen proprietary software for reasons the reading does not recognize as legitimate (support, liability, integration, convenience) are treated as victims of a denial rather than as parties whose stated preference should count; their own account of their choice is structurally excluded from the reading's evaluation.
% DISAPPEARANCE_RATIONALE: If the freedom-imperative framing vanished, the free software movement's specific moral vocabulary and campaign infrastructure (FSF, GPL enforcement culture, 'software freedom' as a rights claim) would lose its foundational justification, and advocacy bodies built around it would need to fall back to a pragmatic or commons framing to retain relevance. Proprietary software itself would be materially unaffected, since it does not depend on this reading being accepted, which is why the verdict is contested rather than a clean rearrangement claim in either direction.
% FOUNDING_PROBLEM: Early hacker culture at institutions like MIT experienced a shift from open, modifiable lab software toward vendor lock-in and non-disclosure agreements that prevented users and fellow developers from fixing, understanding, or sharing improvements to code they depended on daily.
% FOUNDING_PROBLEM_CORROBORATION: Free software advocates themselves attest the problem persists and has worsened with DRM, cloud lock-in, and firmware restrictions. Independent software-economics researchers and antitrust regulators corroborate that vendor lock-in remains a live competitive-harm concern in specific markets (operating systems, document formats), but do not corroborate the categorical claim that all proprietary licensing constitutes the same harm — that broader claim is attested primarily from within the movement itself.
narrative_ontology:disappearance_verdict(software_control_legitimacy__freedom_imperative_reading, contested).
narrative_ontology:founding_problem_status(software_control_legitimacy__freedom_imperative_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_control_legitimacy__freedom_imperative_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(software_control_legitimacy__freedom_imperative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_control_legitimacy__freedom_imperative_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_control_legitimacy__freedom_imperative_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(software_control_legitimacy__freedom_imperative_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(software_control_legitimacy__freedom_imperative_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.71) because the reading's structure is categorical rather than case-by-case: it treats the mere existence of a proprietary license as sufficient to establish illegitimacy, sweeping in every vendor, every enterprise customer who chose proprietary tools for defensible reasons, and every developer bound by an NDA, without regard to price, quality, or voluntary uptake. Suppression is authored at a moderate 0.58: the reading does not have state coercive power, but it exerts real pressure through boycott campaigns, license-compliance enforcement (GPL litigation), platform and repository exclusion, and social pressure within developer communities, which narrows the practical space in which proprietary choices can be defended without reputational cost. Accessibility collapse is moderate (0.4) rather than high, because proprietary software remains widely available and chosen despite the reading's moral claim — the reading has not achieved anything close to collapsing the alternative. Resistance is fairly high (0.62), reflecting the substantial pushback the categorical claim receives from vendors, enterprise IT departments, and even portions of the open-source community that reject the FSF's absolutism in favor of permissive licensing or pragmatic mixed models.
 *
 * PERSPECTIVAL GAP:
 *   From the advocacy-body seat, the arrangement reads as a coordination structure defending a genuine and under-recognized user interest — the four freedoms as the correct baseline for legitimate software. From the vendor, enterprise-customer, and NDA-developer seats, the same structure reads as an imposed extraction of moral standing: their considered, often reasoned choices are re-labeled as harm inflicted upon them or by them, with no route to legitimacy that does not require abandoning their business or employment model. The engine should compute these as structurally different experiences of the same constraint, not reconcile them.
 *
 * DIRECTIONALITY LOGIC:
 *   Software_users_as_rights_holders and the advocacy bodies sit near the beneficiary end: the reading manufactures a rights claim on their behalf (users) and mobilizational capital and legitimacy for the bodies that articulate and enforce it (advocacy organizations), which is why gain_flow names the advocacy bodies rather than 'diffuse' users, who benefit rhetorically but do not capture organizational resources. Vendors, enterprise customers, and NDA-bound developers sit near the target end: the reading imposes a categorical illegitimacy verdict on them regardless of their own account of their situation, and their exit options (dropping proprietary licensing, migrating enterprise stacks, refusing NDA employment) carry real switching costs that the reading does not treat as mitigating.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — hacker-culture era lock-in that prevented users and peer developers from fixing software they depended on — was real and remains partially live in specific domains (DRM, closed firmware, platform lock-in). But the freedom_imperative_reading generalizes a domain-specific harm into a categorical claim about all proprietary licensing, which risks classifying voluntary, well-functioning commercial relationships (enterprise support contracts, paid creative tools) as ethical violations solely by license type. The tangled_rope classification reflects this: there is a genuine coordination function (organizing resistance to real lock-in harms) riding alongside a much broader extraction of moral standing from parties whose specific conduct may not resemble the founding harm at all.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    categorical_versus_case_by_case_harm,
    'Is the harm this reading identifies (denial of user control) actually present in every instance of proprietary licensing, or only in a subset characterized by lock-in, DRM, and lack of interoperability — with the remainder being voluntary, informed, low-switching-cost transactions?',
    'Empirical survey of proprietary software instances sorted by switching cost, interoperability, and user awareness at time of purchase; compare outcomes for high-lock-in versus low-lock-in proprietary products.',
    'If harm concentrates in a lock-in subset, the categorical claim over-generalizes and a narrower reading (closer to pragmatic_openness_reading with a lock-in carve-out) would more accurately track the actual extraction; if harm is genuinely uniform across all proprietary licensing, the categorical claim is better supported than currently credited.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(categorical_versus_case_by_case_harm, empirical, 'Whether the categorical illegitimacy claim over-generalizes from a lock-in-specific harm.').

omega_variable(
    consent_curing_question,
    'Does a user''s informed, voluntary acceptance of proprietary license terms cure the ethical violation the reading identifies, or does the reading correctly hold that certain freedoms cannot be waived by consent (akin to inalienable rights)?',
    'Philosophical analysis of the inalienability claim compared against established frameworks for waivable versus non-waivable rights in contract and rights theory; examine whether the free software movement''s own writings treat the freedoms as waivable.',
    'If consent cures the violation, most commercial proprietary transactions with informed consumers fall outside this reading''s proper scope, sharply reducing its actual victim set toward only non-consensual lock-in cases. If the freedoms are treated as non-waivable, the categorical reading is internally coherent as authored.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(consent_curing_question, conceptual, 'Whether informed consent to proprietary terms is compatible with, or refutes, the categorical illegitimacy claim.').

omega_variable(
    reading_selection_evidence,
    'What in the source material specifically indicated that the freedom_imperative_reading (rather than property_rights_reading or commons_reading) is the operative frame for this constraint file?',
    'The manifest explicitly assigned this reading_id and expected structural delta (all proprietary software as victim set, high epsilon, users as rights-holders beneficiaries); this omega documents that the assignment, not independent textual analysis, drove the framing choice.',
    'A different assignment (e.g. property_rights_reading) would classify the same underlying facts about proprietary licensing with an inverted beneficiary/victim structure and a much lower epsilon, since restriction would be read as legitimate rather than as denial. This is exactly the kind of framing under-determination the kernel/reading structure is designed to make explicit rather than leave implicit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_selection_evidence, conceptual, 'Documents that the reading assignment, not the raw facts alone, determines this file''s classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_control_legitimacy__freedom_imperative_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(soft_tr_t8, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 8, 0.13).
narrative_ontology:measurement(soft_tr_t16, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 16, 0.16).
narrative_ontology:measurement(soft_tr_t24, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 24, 0.18).
narrative_ontology:measurement(soft_tr_t32, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 32, 0.2).
narrative_ontology:measurement(soft_tr_t40, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 40, 0.22).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(soft_be_t8, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 8, 0.6).
narrative_ontology:measurement(soft_be_t16, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 16, 0.64).
narrative_ontology:measurement(soft_be_t24, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 24, 0.67).
narrative_ontology:measurement(soft_be_t32, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 32, 0.69).
narrative_ontology:measurement(soft_be_t40, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 40, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t0, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(soft_su_t8, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 8, 0.46).
narrative_ontology:measurement(soft_su_t16, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 16, 0.5).
narrative_ontology:measurement(soft_su_t24, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 24, 0.53).
narrative_ontology:measurement(soft_su_t32, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 32, 0.56).
narrative_ontology:measurement(soft_su_t40, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_control_legitimacy__freedom_imperative_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(software_control_legitimacy__freedom_imperative_reading, 0.08).
narrative_ontology:affects_constraint(software_control_legitimacy__freedom_imperative_reading, software_control_legitimacy__pragmatic_openness_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__freedom_imperative_reading, software_control_legitimacy__property_rights_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__freedom_imperative_reading, software_control_legitimacy__commons_reading).

% DUAL FORMULATION NOTE:
% This file is one of four sibling constraints decomposing the natural-language concept 'software control legitimacy,' per the ε-invariance principle: each reading of the kernel produces a structurally distinct constraint with its own epsilon, beneficiary/victim set, and classification. freedom_imperative_reading (this file) is the highest-epsilon, most categorical reading. property_rights_reading inverts the beneficiary/victim structure (vendors as legitimate rights-holders, restriction as protected). pragmatic_openness_reading treats the question as methodological rather than ethical, with much lower epsilon. commons_reading rejects the binary entirely in favor of negotiated governance. All four should be read together as a constraint family, not as competing measurements of one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
