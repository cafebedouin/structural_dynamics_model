% ============================================================================
% CONSTRAINT STORY: public_health_mandate_authority__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_public_health_mandate_authority__bodily_autonomy_primary, []).

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
 *   constraint_id: public_health_mandate_authority__bodily_autonomy_primary
 *   human_readable: Public Health Mandate as Categorical Violation of Bodily Sovereignty
 *   domain: public_health_law/constitutional_rights/bioethics
 *
 * SUMMARY:
 *   This story instantiates the bodily-autonomy-primary reading of the
 *   contested public_health_mandate_authority kernel: the categorical
 *   position that no aggregation of collective benefit can justify a
 *   non-consensual medical intervention on an individual body. Under this
 *   reading, the coercive mechanism itself — not any downstream medical
 *   outcome — is the harm, and it is fully realized the moment compliance is
 *   conditioned on access to employment, education, or public life. The
 *   sibling readings (public_health_primary, proportionality_reading) are NOT
 *   represented here as competing evaluations of this same constraint; they
 *   are separate constraints with separate victim/beneficiary structures,
 *   separate ε, and separate classification, linked only via
 *   network.affects_constraints and cs_structure.reading_relations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(public_health_mandate_authority__bodily_autonomy_primary, 0.81).
domain_priors:suppression_score(public_health_mandate_authority__bodily_autonomy_primary, 0.72).
domain_priors:theater_ratio(public_health_mandate_authority__bodily_autonomy_primary, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, extractiveness, 0.81).
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(public_health_mandate_authority__bodily_autonomy_primary, snare).
narrative_ontology:human_readable(public_health_mandate_authority__bodily_autonomy_primary, "Public Health Mandate as Categorical Violation of Bodily Sovereignty").
narrative_ontology:topic_domain(public_health_mandate_authority__bodily_autonomy_primary, "public_health_law/constitutional_rights/bioethics").

domain_priors:requires_active_enforcement(public_health_mandate_authority__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(public_health_mandate_authority__bodily_autonomy_primary, '32ec4956-6164-442e-80a4-293eac3c787c').
narrative_ontology:cs_kernel_codification('32ec4956-6164-442e-80a4-293eac3c787c', distributed).
narrative_ontology:cs_authority_grounding('32ec4956-6164-442e-80a4-293eac3c787c', distributed).
narrative_ontology:cs_reading_relation('32ec4956-6164-442e-80a4-293eac3c787c', public_health_mandate_authority__public_health_primary, forecloses).
narrative_ontology:cs_reading_relation('32ec4956-6164-442e-80a4-293eac3c787c', public_health_mandate_authority__proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('32ec4956-6164-442e-80a4-293eac3c787c', foundational, bodily_sovereignty_categorically_inviolable).
narrative_ontology:cs_axiom_status(bodily_sovereignty_categorically_inviolable, holdable).
narrative_ontology:cs_axiom_grounding('32ec4956-6164-442e-80a4-293eac3c787c', bodily_sovereignty_categorically_inviolable, deontological).
narrative_ontology:cs_axiom('32ec4956-6164-442e-80a4-293eac3c787c', foundational, collective_benefit_never_licenses_nonconsensual_intervention).
narrative_ontology:cs_axiom_status(collective_benefit_never_licenses_nonconsensual_intervention, holdable).
narrative_ontology:cs_axiom_grounding('32ec4956-6164-442e-80a4-293eac3c787c', collective_benefit_never_licenses_nonconsensual_intervention, deontological).
narrative_ontology:cs_reference_frame('32ec4956-6164-442e-80a4-293eac3c787c', individual_bodily_sovereignty_as_inviolable).
narrative_ontology:cs_drift_state('32ec4956-6164-442e-80a4-293eac3c787c', post_pandemic_mandate_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('32ec4956-6164-442e-80a4-293eac3c787c', '').
narrative_ontology:cs_kernel_id(public_health_mandate_authority__bodily_autonomy_primary, public_health_mandate_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__bodily_autonomy_primary, public_health_agencies).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__bodily_autonomy_primary, vaccine_manufacturers).
narrative_ontology:constraint_victim(public_health_mandate_authority__bodily_autonomy_primary, unvaccinated_individuals).
narrative_ontology:constraint_victim(public_health_mandate_authority__bodily_autonomy_primary, religious_and_conscientious_objectors).
narrative_ontology:constraint_victim(public_health_mandate_authority__bodily_autonomy_primary, workers_facing_mandate_conditioned_employment).
narrative_ontology:constraint_vindicates(public_health_mandate_authority__bodily_autonomy_primary, state_police_power_over_bodily_integrity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Face loss of employment, school access, travel, or public accommodation access unless they submit to a medical intervention they have not consented to. From this reading's premise, the mandate is a direct violation regardless of the intervention's medical merits — the harm is the coercion itself, not the vaccine's side-effect profile. Exit requires forfeiting livelihood, education, or civic participation.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, unvaccinated_individuals, payer,
    powerless, biographical, trapped, national).

% Hold sincere objections rooted in conscience or faith. Exemption processes are frequently narrow, discretionary, or effectively unavailable, making the formal exemption right largely theatrical relative to the practical coercion faced.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, religious_and_conscientious_objectors, payer,
    powerless, biographical, trapped, national).

% Employment, licensure, or continued service is conditioned on compliance. Some have professional mobility to seek unmandated employers; many do not, particularly in healthcare, government, or unionized sectors where the mandate is near-universal across employers.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, workers_facing_mandate_conditioned_employment, payer,
    moderate, biographical, constrained, national).

% Issue and enforce the mandate, framing it as a proportionate collective-action measure. Under this reading, their invocation of collective benefit is precisely the move that is rejected as a justification — no aggregation of benefit to third parties can license non-consensual bodily intervention on an individual.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, public_health_agencies, agenda_setter,
    institutional, generational, analytical, national).

% Receive guaranteed demand and liability protections tied to mandate policy. They neither administer nor bear the coercive mechanism directly but capture a durable revenue stream that depends on the mandate's persistence.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, vaccine_manufacturers, beneficiary,
    institutional, biographical, arbitrage, global).

% Under this reading, this group is EXCLUDED from the victim set: their vulnerability does not generate a duty enforceable through non-consensual bodily invasion of others. Their situation is not disputed as real, but this reading holds that no obligation to protect them can be discharged by coercing a third party's body. They remain present in the narrative as the group whose interests the public-health-primary reading would center, but this reading does not treat their protection as licensing the mandate.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, immunocompromised_and_vulnerable_populations, excluded,
    powerless, biographical, trapped, national).

% Argue the mandate discharges a collective obligation to protect vulnerable populations and health infrastructure. Under this reading, no coercion is imposed on them by the mandate — they experience the arrangement as costless or beneficial, and thus carry zero extractiveness under this reading's ledger even though they are a major party to the underlying kernel dispute.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, public_health_primary_advocates, excluded,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(public_health_mandate_authority__bodily_autonomy_primary, diffuse).
narrative_ontology:fixing_cost_class(public_health_mandate_authority__bodily_autonomy_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The mandate coordinates population-level immunity and reduces transmission-linked burden on healthcare capacity; this reading does not deny that a coordination problem exists, only that non-consensual bodily intervention is a licensed solution to it.
% TRANSFER_FUNCTION: Moves bodily control from the individual to the state/employer, and moves compliance-derived economic value (continued employment, market access, demand for medical products) toward institutions and manufacturers administering or profiting from the mandate.
% ABSENT_VOICES: Individuals whose sincere objections were denied through narrow exemption processes are structurally absent from the policymaking room; their exclusion is treated by this reading as the paradigm harm, not an incidental cost.
% DISAPPEARANCE_RATIONALE: If the mandate authority vanished, coerced individuals would regain employment/access without medical compliance, enforcement infrastructure (verification systems, exemption boards, penalty schedules) would dissolve, and manufacturers would lose a guaranteed-demand channel — a substantial rearrangement of who bears cost and who exercises bodily control.
% FOUNDING_PROBLEM: Managing communicable disease spread when voluntary uptake is judged insufficient to protect health system capacity and vulnerable populations.
% FOUNDING_PROBLEM_CORROBORATION: Public health agencies attest the problem remains live (transmission risk, healthcare capacity). Civil liberties organizations and constitutional scholars outside the administering agencies attest that even if the epidemiological problem is real, it does not corroborate the specific claim this reading rejects — that collective benefit can justify non-consensual bodily intervention; that normative claim is corroborated by no source outside the mandate's own administering and advocating parties.
narrative_ontology:disappearance_verdict(public_health_mandate_authority__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(public_health_mandate_authority__bodily_autonomy_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(public_health_mandate_authority__bodily_autonomy_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(public_health_mandate_authority__bodily_autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(public_health_mandate_authority__bodily_autonomy_primary, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(public_health_mandate_authority__bodily_autonomy_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(public_health_mandate_authority__bodily_autonomy_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(public_health_mandate_authority__bodily_autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high and rising over the interval (0.55 to 0.81) because enforcement mechanisms (verification systems, employment conditioning, access restriction) matured and hardened during the measured period, consistent with the enforcement-ratchet pattern. Theater ratio stays low-moderate (peaking ~0.24) because the enforcement machinery is doing real coercive work, not merely performing compliance theater — under this reading the harm is structural, not symbolic. Suppression requirement oscillates with policy waves (mandate tightening, court challenges, relaxation, re-tightening), which the temporal grid captures as a single shared series rather than independent per-metric grids.
 *
 * DIRECTIONALITY LOGIC:
 *   Unvaccinated individuals, religious/conscientious objectors, and mandate-conditioned workers are the victim set under this reading — trapped or constrained exit, non-consensual intervention imposed directly on their bodies. Public health agencies and vaccine manufacturers sit at the beneficiary end: agencies collect compliance and institutional legitimacy, manufacturers collect guaranteed demand. Critically, public_health_primary_advocates carry ZERO extractiveness under this reading's own ledger — the mandate imposes no coercion on them; they experience the arrangement as costless or beneficial, which is the structural delta this reading specifies relative to its siblings. Immunocompromised and vulnerable populations are explicitly excluded from the victim set here, not because their situation is denied, but because this reading holds no duty-to-protect can be discharged through non-consensual invasion of a third party's body — that exclusion is the reading's defining move, distinct from how public_health_primary would seat that same population as the constraint's primary beneficiary class.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (transmission control, health-system capacity protection) may remain epidemiologically live while the specific normative license this reading rejects — that collective benefit justifies bodily coercion — is corroborated by no party outside the mandate's own administering and advocating seats. This is why founding_problem_status is authored as 'contested' rather than 'dead': the underlying public health problem persists, but this reading holds that persistence does not corroborate the coercive mechanism's legitimacy, which is a categorically separate question from whether disease transmission is a real problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    categorical_vs_proportionality_framing_choice,
    'Is bodily sovereignty properly treated as a categorical (threshold, non-negotiable) right, or as one factor in a proportionality balancing test against collective harm reduction?',
    'Comparative constitutional analysis across jurisdictions that have adjudicated compelled medical intervention claims (vaccination, quarantine, forced treatment orders) under strict scrutiny versus balancing frameworks; convergence or divergence in outcomes would evidence which framing dominant legal traditions actually apply.',
    'If courts and legal traditions consistently apply proportionality rather than categorical rules, this reading''s premise is a minority normative position rather than settled doctrine, which would not change this story''s authored ε but would affect how much weight the reading commands relative to its siblings in cross-story analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(categorical_vs_proportionality_framing_choice, conceptual, 'Whether bodily sovereignty is a categorical or balanceable right — the axis the three sibling readings split on.').

omega_variable(
    immunocompromised_exclusion_defensibility,
    'Is it structurally coherent to exclude immunocompromised populations from the victim set of the mandate constraint while conceding their vulnerability is real, on the grounds that a duty-to-protect cannot license bodily invasion of a third party?',
    'Analysis of analogous doctrines in tort and criminal law where a duty to prevent harm to a third party is or is not held to license non-consensual bodily intervention on another person (e.g., forced blood donation is never compelled even to save a life; contrast with quarantine, which restricts movement rather than invading the body).',
    'If the analogy to forced-donation-style non-compulsion holds cleanly, the exclusion is well-grounded within existing legal reasoning; if courts have historically permitted bodily-invasive compulsion for third-party protection in analogous contexts, the exclusion is a contestable normative choice specific to this reading rather than an extension of settled doctrine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(immunocompromised_exclusion_defensibility, conceptual, 'Whether excluding the immunocompromised from the victim set follows existing non-compulsion doctrine or is a novel normative stance.').

omega_variable(
    manufacturer_beneficiary_causal_weight,
    'How much of the mandate''s persistence is causally attributable to manufacturer lobbying and liability-shield arrangements versus genuine independent public-health judgment by agencies?',
    'Discovery of lobbying records, liability-shield legislative history, and comparison of mandate persistence in jurisdictions with versus without strong manufacturer liability protections.',
    'High causal weight would strengthen the reading that agency claims of collective benefit are partly captured advocacy rather than neutral epidemiological judgment, reinforcing this story''s characterization of the mandate as extractive beyond its stated coordination function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(manufacturer_beneficiary_causal_weight, empirical, 'Whether manufacturer interests causally drive mandate persistence independent of epidemiological necessity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(public_health_mandate_authority__bodily_autonomy_primary, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(publ_tr_t0, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 0, 0.1).
narrative_ontology:measurement(publ_tr_t6, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 6, 0.12).
narrative_ontology:measurement(publ_tr_t12, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 12, 0.18).
narrative_ontology:measurement(publ_tr_t18, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 18, 0.22).
narrative_ontology:measurement(publ_tr_t24, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 24, 0.24).
narrative_ontology:measurement(publ_tr_t30, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 30, 0.19).
narrative_ontology:measurement(publ_tr_t36, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 36, 0.2).

% Extraction over time
narrative_ontology:measurement(publ_be_t0, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(publ_be_t6, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 6, 0.68).
narrative_ontology:measurement(publ_be_t12, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 12, 0.79).
narrative_ontology:measurement(publ_be_t18, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 18, 0.83).
narrative_ontology:measurement(publ_be_t24, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 24, 0.8).
narrative_ontology:measurement(publ_be_t30, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 30, 0.74).
narrative_ontology:measurement(publ_be_t36, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 36, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(publ_su_t0, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(publ_su_t6, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 6, 0.6).
narrative_ontology:measurement(publ_su_t12, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 12, 0.75).
narrative_ontology:measurement(publ_su_t18, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 18, 0.82).
narrative_ontology:measurement(publ_su_t24, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 24, 0.78).
narrative_ontology:measurement(publ_su_t30, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 30, 0.65).
narrative_ontology:measurement(publ_su_t36, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 36, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(public_health_mandate_authority__bodily_autonomy_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(public_health_mandate_authority__bodily_autonomy_primary, public_health_mandate_authority__public_health_primary).
narrative_ontology:affects_constraint(public_health_mandate_authority__bodily_autonomy_primary, public_health_mandate_authority__proportionality_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings decomposing the natural-language kernel 'public health mandate authority' per the ε-invariance principle: bodily_autonomy_primary (this story, snare — categorical rejection of coercive licensing), public_health_primary (a separate story reading the same kernel as a collective-protection obligation, likely tangled_rope or rope depending on its own authored beneficiary/victim structure), and proportionality_reading (a separate story applying a sliding-scale test, likely scaffold or tangled_rope depending on duration/sunset framing). Each carries its own ε, its own beneficiary/victim structure, and its own classification; they are linked here rather than merged because measuring the same kernel by different normative lenses produces incompatible ε values, which the framework treats as evidence of decomposition rather than observer-relativity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
