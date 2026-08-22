% ============================================================================
% CONSTRAINT STORY: permissive_license_text__corporate_moat_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_permissive_license_text__corporate_moat_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: permissive_license_text__corporate_moat_reading
 *   human_readable: Permissive Open-Source License Text — Corporate Moat Reading
 *   domain: software_licensing/intellectual_property/technology_governance
 *
 * SUMMARY:
 *   This story instantiates the corporate-moat reading of the
 *   permissive_license_text kernel: the same MIT/BSD/Apache-style license
 *   grant read as an arrangement that structurally enables large
 *   institutional actors to commercialize community-produced code without
 *   reciprocal contribution. The sibling readings (commons coordination,
 *   copyleft counterfactual) evaluate the identical license text against
 *   different referents and different beneficiary/victim structures — they
 *   are separate constraint stories, not alternative measurements of this
 *   one. Under this reading, ε is moderate (0.58 at interval end): the
 *   coordination function is real and not fabricated, but a persistent
 *   extraction channel rides on top of it, concentrated on unpaid maintainers
 *   and undercapitalized foundations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(permissive_license_text__corporate_moat_reading, 0.58).
domain_priors:suppression_score(permissive_license_text__corporate_moat_reading, 0.42).
domain_priors:theater_ratio(permissive_license_text__corporate_moat_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(permissive_license_text__corporate_moat_reading, snare).
narrative_ontology:human_readable(permissive_license_text__corporate_moat_reading, "Permissive Open-Source License Text — Corporate Moat Reading").
narrative_ontology:topic_domain(permissive_license_text__corporate_moat_reading, "software_licensing/intellectual_property/technology_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(permissive_license_text__corporate_moat_reading, 'f6afa3c2-5730-40bf-acfb-d6994d32b09f').
narrative_ontology:cs_kernel_codification('f6afa3c2-5730-40bf-acfb-d6994d32b09f', fixed_text).
narrative_ontology:cs_authority_grounding('f6afa3c2-5730-40bf-acfb-d6994d32b09f', distributed).
narrative_ontology:cs_reading_relation('f6afa3c2-5730-40bf-acfb-d6994d32b09f', permissive_license_text__commons_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('f6afa3c2-5730-40bf-acfb-d6994d32b09f', permissive_license_text__copyleft_counterfactual_reading, influences).
narrative_ontology:cs_axiom('f6afa3c2-5730-40bf-acfb-d6994d32b09f', foundational, license_grant_creates_no_reciprocity_obligation).
narrative_ontology:cs_axiom_status(license_grant_creates_no_reciprocity_obligation, holdable).
narrative_ontology:cs_axiom_grounding('f6afa3c2-5730-40bf-acfb-d6994d32b09f', license_grant_creates_no_reciprocity_obligation, conventional).
narrative_ontology:cs_axiom('f6afa3c2-5730-40bf-acfb-d6994d32b09f', secondary, uncompensated_commercialization_is_licensed_outcome_not_defect).
narrative_ontology:cs_axiom_status(uncompensated_commercialization_is_licensed_outcome_not_defect, holdable).
narrative_ontology:cs_axiom_grounding('f6afa3c2-5730-40bf-acfb-d6994d32b09f', uncompensated_commercialization_is_licensed_outcome_not_defect, instrumental).
narrative_ontology:cs_reference_frame('f6afa3c2-5730-40bf-acfb-d6994d32b09f', adoption_friction_minimization_era).
narrative_ontology:cs_drift_state('f6afa3c2-5730-40bf-acfb-d6994d32b09f', cloud_hyperscaler_commercialization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f6afa3c2-5730-40bf-acfb-d6994d32b09f', '').
narrative_ontology:cs_kernel_id(permissive_license_text__corporate_moat_reading, permissive_license_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(permissive_license_text__corporate_moat_reading, cloud_hyperscalers).
narrative_ontology:constraint_beneficiary(permissive_license_text__corporate_moat_reading, enterprise_software_vendors).
narrative_ontology:constraint_victim(permissive_license_text__corporate_moat_reading, individual_maintainers).
narrative_ontology:constraint_victim(permissive_license_text__corporate_moat_reading, unfunded_project_foundations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(permissive_license_text__corporate_moat_reading, downstream_application_developers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Write and sustain the permissively licensed codebase largely as unpaid or underpaid labor, often motivated by identity as an open-source contributor. The license text they chose (MIT/BSD/Apache) lets any downstream party take their work, wrap it in a proprietary managed service or product, and sell it back to the same community without contributing money, code, or credit at scale. Forking to a reciprocal license after the fact is nearly impossible once the community and expectations have formed around permissiveness; walking away means abandoning years of reputational and technical investment.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, individual_maintainers, payer,
    powerless, biographical, identity_locked, global).

% Nonprofit or informally governed foundations that hold the permissively licensed project's trademark and governance but have no revenue mechanism tied to downstream commercial use. They must fund security audits, triage, and releases from donations while the corporations profiting most from the code contribute the least, structurally unable to compel payment because the license grants no leverage.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, unfunded_project_foundations, payer,
    moderate, biographical, trapped, global).

% Take permissively licensed software (databases, orchestration tools, ML frameworks), operate it as a fully managed cloud service, and capture the resulting revenue without any obligation to share code, pay license fees, or fund upstream maintenance. Their scale lets them out-market and out-support the original project while contributing engineering time only when it serves their own roadmap. If a project relicenses to something less permissive, they can fork the last permissive commit and continue extracting from that snapshot indefinitely.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, cloud_hyperscalers, beneficiary,
    institutional, generational, arbitrage, global).

% Embed permissively licensed components into proprietary enterprise products, capturing the coordination benefits of a mature open codebase (interoperability, hiring pool, ecosystem trust) while keeping their own value-add closed and unlicensed to the commons. They lobby for maintaining permissive terms whenever a project debates relicensing.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, enterprise_software_vendors, beneficiary,
    institutional, generational, arbitrage, global).

% Small and mid-sized developers who genuinely benefit from being able to use the permissively licensed code in commercial products without legal friction or reciprocal disclosure obligations. They are not the structural target of the extraction but their continued use of the ecosystem is cited by hyperscalers as evidence the arrangement is healthy coordination.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, downstream_application_developers, beneficiary,
    moderate, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(permissive_license_text__corporate_moat_reading, downstream_application_developers, observer).

% Maintainers and foundation staff who argue for moving to source-available or reciprocal licenses (e.g., BSL, SSPL) to recapture value from cloud providers. They are frequently outvoted or bypassed in governance because the corporate beneficiaries who fund conferences, hire maintainers, and hold board seats have disproportionate influence over relicensing decisions.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, relicensing_advocates, excluded,
    moderate, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(permissive_license_text__corporate_moat_reading, cloud_hyperscalers).
narrative_ontology:fixing_cost_class(permissive_license_text__corporate_moat_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Permissive licensing genuinely lowers legal friction for anyone who wants to use, modify, and redistribute the code, enabling wide interoperability, easy corporate adoption, and a large hiring/ecosystem pool that benefits everyone including the original authors' career prospects.
% TRANSFER_FUNCTION: Moves engineering labor and accumulated technical value from unpaid or underfunded individual maintainers and foundations to institutional actors (cloud providers, enterprise vendors) who commercialize the work without a reciprocal payment or contribution obligation attached to the license itself.
% ABSENT_VOICES: Relicensing advocates and unpaid maintainers who bear the sustainability cost are structurally underweighted in governance forums funded and staffed disproportionately by the corporate beneficiaries; their preferred remedy (reciprocal or source-available terms) rarely reaches a binding vote before the beneficiaries mobilize against it.
% DISAPPEARANCE_RATIONALE: If permissive terms were replaced overnight with a strong reciprocity requirement across the ecosystem, cloud hyperscalers would lose the ability to offer fully managed services built on uncompensated upstream code without new licensing deals or contribution commitments; some would negotiate commercial licenses, others would fork last-permissive snapshots, and maintainer funding models would shift toward negotiated support contracts.
% FOUNDING_PROBLEM: Early open-source licensing needed to solve fragmentation and legal ambiguity around reuse — permissive terms were adopted to maximize adoption, avoid the compliance burden of copyleft, and encourage corporate contribution by minimizing legal risk to downstream commercial use.
% FOUNDING_PROBLEM_CORROBORATION: Corporate beneficiaries and many original license authors attest the founding problem (adoption friction) remains the operative concern and permissive terms still solve it. Independent maintainer surveys (e.g., Tidelift and Linux Foundation sustainability reports) and testimony from foundation staff outside the beneficiary set attest the founding problem has shifted from adoption friction to uncompensated commercialization, which the license text does not address at all.
narrative_ontology:disappearance_verdict(permissive_license_text__corporate_moat_reading, world_rearranges).
narrative_ontology:founding_problem_status(permissive_license_text__corporate_moat_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(permissive_license_text__corporate_moat_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(permissive_license_text__corporate_moat_reading, 'none', 1).
narrative_ontology:epsilon_provenance(permissive_license_text__corporate_moat_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(permissive_license_text__corporate_moat_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(permissive_license_text__corporate_moat_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(permissive_license_text__corporate_moat_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rose steadily over the interval (0.32 to 0.58) as cloud providers matured from occasional contributors into full managed-service competitors of the projects they depend on — a rent-seeking layer accreting onto what began as low-friction coordination. Theater ratio also rose (0.18 to 0.40) as corporate sponsorship of foundations increasingly substitutes visible 'giving back' programs (conference sponsorships, small one-off grants) for the substantive engineering investment or revenue-sharing that would actually address maintainer sustainability. Suppression (0.42) is moderate rather than high: no one is coerced into using the permissive license, but path dependency and network effects make relicensing after adoption extremely costly, which functions as a soft suppression of the exit option.
 *
 * PERSPECTIVAL GAP:
 *   From the cloud hyperscaler seat, permissive licensing is a mundane input to a competitive cloud services market, chosen freely by the maintainers, imposing no obligation because none was written into the grant. From the maintainer seat, the same clause is the mechanism by which years of unpaid labor become someone else's product margin. The engine should compute these as structurally different experiences of the identical textual constraint, which is exactly why this reading is authored as its own constraint rather than folded into a single averaged ε.
 *
 * DIRECTIONALITY LOGIC:
 *   Cloud hyperscalers and enterprise vendors sit near the full-beneficiary end: they extract commercial value from the commons while bearing none of the reciprocal cost the coordination story would imply, and their arbitrage-grade exit (fork the last permissive commit, or simply not contribute) insulates them from any governance pressure to change. Individual maintainers and foundations sit near the full-target end: their labor is the input being extracted, their exit is identity-locked or trapped (leaving means abandoning the project's community and their own professional identity as its steward), and they have no license-based leverage to demand compensation.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling this as pure extraction (a snare with no coordination function) or as pure coordination (a rope with no victims). The permissive license genuinely solved — and still partly solves — the adoption-friction problem for downstream developers broadly; that coordination function has not vanished. What has changed is that the same textual grant that enabled broad adoption now also enables a specific, identifiable extraction pattern concentrated on the people who wrote the code. This reading holds both facts as true simultaneously rather than collapsing to either pole.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    license_text_vs_governance_capture,
    'Is the extraction attributable to the license text itself (an inherent property of permissive grants) or to governance capture of the foundations that steward permissively licensed projects (a separable, fixable institutional failure)?',
    'Compare sustainability outcomes across permissively licensed projects with strong independent governance (diverse funding, maintainer-controlled boards) versus those with corporate-dominated governance; if outcomes diverge sharply, the extraction is governance-attributable rather than license-attributable.',
    'If license-attributable, only relicensing (moving toward the copyleft_counterfactual_reading''s prescription) resolves the extraction. If governance-attributable, foundation reform could resolve it while preserving permissive terms — meaning this reading''s ε could fall without changing the license text at all.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(license_text_vs_governance_capture, empirical, 'Whether extraction traces to the license grant or to captured project governance.').

omega_variable(
    kernel_reading_disagreement_locus,
    'Where exactly do the three readings of permissive_license_text disagree — is it the same underlying fact evaluated by different normative standards, or do they disagree about what actually happens downstream?',
    'Trace each reading''s beneficiary/victim claims against empirical contribution and revenue-capture data (e.g., corporate contribution ratios relative to commercial revenue derived from the codebase); readings that diverge on values but agree on facts are normative disputes, readings that diverge on facts are empirically adjudicable.',
    'If the disagreement is purely normative (commons_coordination_reading and this reading agree on the facts but weigh adoption-friction reduction against uncompensated extraction differently), all three readings can coexist indefinitely as live positions. If factual, the empirically weaker reading should lose adherents over time regardless of normative commitments.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Whether the kernel''s contested readings diverge on values or on facts.').

omega_variable(
    corporate_beneficiary_naturalization,
    'Do corporate beneficiaries frame the extraction pattern as simply ''how open source works'' in a way that forecloses maintainers from recognizing the arrangement as contestable at all?',
    'Survey maintainer attitudes over time toward relicensing options; a rising rate of relicensing attempts (e.g., the observed wave of BSL/SSPL adoptions 2018-2023) suggests the naturalization is weakening, not holding.',
    'If naturalization is strong and holding, the moat reading persists as an unchallenged status quo (snare with low resistance). If weakening, resistance rises and the constraint may be trending toward a tangled_rope or toward eventual relicensing pressure that shifts real-world instances toward the copyleft_counterfactual_reading''s prescription.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(corporate_beneficiary_naturalization, empirical, 'Whether corporate framing of permissive licensing as neutral forecloses maintainer contestation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(permissive_license_text__corporate_moat_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perm_tr_t0, permissive_license_text__corporate_moat_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(perm_tr_t4, permissive_license_text__corporate_moat_reading, theater_ratio, 4, 0.22).
narrative_ontology:measurement(perm_tr_t8, permissive_license_text__corporate_moat_reading, theater_ratio, 8, 0.27).
narrative_ontology:measurement(perm_tr_t12, permissive_license_text__corporate_moat_reading, theater_ratio, 12, 0.31).
narrative_ontology:measurement(perm_tr_t16, permissive_license_text__corporate_moat_reading, theater_ratio, 16, 0.35).
narrative_ontology:measurement(perm_tr_t20, permissive_license_text__corporate_moat_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(perm_tr_t24, permissive_license_text__corporate_moat_reading, theater_ratio, 24, 0.4).

% Extraction over time
narrative_ontology:measurement(perm_be_t0, permissive_license_text__corporate_moat_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(perm_be_t4, permissive_license_text__corporate_moat_reading, base_extractiveness, 4, 0.38).
narrative_ontology:measurement(perm_be_t8, permissive_license_text__corporate_moat_reading, base_extractiveness, 8, 0.44).
narrative_ontology:measurement(perm_be_t12, permissive_license_text__corporate_moat_reading, base_extractiveness, 12, 0.49).
narrative_ontology:measurement(perm_be_t16, permissive_license_text__corporate_moat_reading, base_extractiveness, 16, 0.53).
narrative_ontology:measurement(perm_be_t20, permissive_license_text__corporate_moat_reading, base_extractiveness, 20, 0.56).
narrative_ontology:measurement(perm_be_t24, permissive_license_text__corporate_moat_reading, base_extractiveness, 24, 0.58).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(permissive_license_text__corporate_moat_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(permissive_license_text__corporate_moat_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(permissive_license_text__corporate_moat_reading, 0.12).
narrative_ontology:affects_constraint(permissive_license_text__corporate_moat_reading, commons_coordination_reading).
narrative_ontology:affects_constraint(permissive_license_text__corporate_moat_reading, copyleft_counterfactual_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the permissive_license_text kernel. commons_coordination_reading evaluates the same textual grant as friction-minimizing universal coordination with negligible extraction (likely rope/mountain-adjacent). copyleft_counterfactual_reading evaluates the absence of a reciprocity clause as the structural defect and treats GPL-style viral licensing as the corrective counterfactual (likely tangled_rope, with a differently constructed beneficiary/victim set centered on what reciprocity would have prevented). This story's ε, beneficiaries, and victims describe the corporate-moat framing specifically and are not merged or averaged with the siblings' values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
