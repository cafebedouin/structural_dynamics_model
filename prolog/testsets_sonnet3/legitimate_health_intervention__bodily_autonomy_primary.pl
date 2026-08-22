% ============================================================================
% CONSTRAINT STORY: legitimate_health_intervention__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimate_health_intervention__bodily_autonomy_primary, []).

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
 *   constraint_id: legitimate_health_intervention__bodily_autonomy_primary
 *   human_readable: Bodily-Autonomy-Primary Reading of Medical Intervention Legitimacy
 *   domain: public_health/constitutional_law/medical_ethics
 *
 * SUMMARY:
 *   This story instantiates the bodily-autonomy-primary reading of the
 *   contested 'legitimate health intervention' kernel: legitimacy for a
 *   medical mandate requires informed, uncoerced consent, and state coercion
 *   attached to bodily intervention is illegitimate regardless of the
 *   magnitude of public benefit claimed. Under this reading, mandate
 *   enforcement that conditions employment, licensure, or public access on
 *   submission to a medical intervention is read as extraction — leverage
 *   exercised against a threshold right — not as proportionate coordination.
 *   This is a distinct constraint from the public_health_primary reading
 *   (which treats refusal as an externality to be corrected) and from the
 *   proportionality_reading (which weighs severity against threat level);
 *   each of those is authored as its own story with its own epsilon. The
 *   measurements show extraction and suppression rising sharply through the
 *   acute enforcement period and then plateauing once mandate architecture
 *   stabilized into settled administrative practice.
 *
 * KEY AGENTS:
 *   - state_public_health_agencies: institutional agenda-setter administering the mandate architecture
 *   - employers_and_institutions: organized intermediate enforcers transmitting state leverage into workplaces
 *   - mandate_coerced_workers: moderate-power payers facing employment loss for non-consent
 *   - religious_and_philosophical_objectors: powerless, trapped payers denied narrow discretionary exemptions
 *   - medically_vulnerable_refusers: powerless payers whose individualized medical reasons are absorbed into blanket enforcement
 *   - civil_liberties_litigators: excluded from mandate design, active only in post-hoc litigation
 *   - constitutional_courts: analytical observers adjudicating the autonomy/police-power boundary
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_health_intervention__bodily_autonomy_primary, 0.66).
domain_priors:suppression_score(legitimate_health_intervention__bodily_autonomy_primary, 0.71).
domain_priors:theater_ratio(legitimate_health_intervention__bodily_autonomy_primary, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, extractiveness, 0.66).
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_health_intervention__bodily_autonomy_primary, tangled_rope).
narrative_ontology:human_readable(legitimate_health_intervention__bodily_autonomy_primary, "Bodily-Autonomy-Primary Reading of Medical Intervention Legitimacy").
narrative_ontology:topic_domain(legitimate_health_intervention__bodily_autonomy_primary, "public_health/constitutional_law/medical_ethics").

domain_priors:requires_active_enforcement(legitimate_health_intervention__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_health_intervention__bodily_autonomy_primary, '65ef5b53-5a5b-4895-ab49-ee652e69fee5').
narrative_ontology:cs_kernel_codification('65ef5b53-5a5b-4895-ab49-ee652e69fee5', distributed).
narrative_ontology:cs_authority_grounding('65ef5b53-5a5b-4895-ab49-ee652e69fee5', distributed).
narrative_ontology:cs_reading_relation('65ef5b53-5a5b-4895-ab49-ee652e69fee5', legitimate_health_intervention__public_health_primary, forecloses).
narrative_ontology:cs_reading_relation('65ef5b53-5a5b-4895-ab49-ee652e69fee5', legitimate_health_intervention__proportionality_reading, influences).
narrative_ontology:cs_axiom('65ef5b53-5a5b-4895-ab49-ee652e69fee5', foundational, bodily_integrity_as_nonderogable_floor).
narrative_ontology:cs_axiom_status(bodily_integrity_as_nonderogable_floor, holdable).
narrative_ontology:cs_axiom_grounding('65ef5b53-5a5b-4895-ab49-ee652e69fee5', bodily_integrity_as_nonderogable_floor, deontological).
narrative_ontology:cs_axiom('65ef5b53-5a5b-4895-ab49-ee652e69fee5', foundational, population_benefit_cannot_license_nonconsensual_intervention).
narrative_ontology:cs_axiom_status(population_benefit_cannot_license_nonconsensual_intervention, holdable).
narrative_ontology:cs_axiom_grounding('65ef5b53-5a5b-4895-ab49-ee652e69fee5', population_benefit_cannot_license_nonconsensual_intervention, deontological).
narrative_ontology:cs_reference_frame('65ef5b53-5a5b-4895-ab49-ee652e69fee5', consent_based_bodily_integrity_floor).
narrative_ontology:cs_drift_state('65ef5b53-5a5b-4895-ab49-ee652e69fee5', post_pandemic_mandate_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('65ef5b53-5a5b-4895-ab49-ee652e69fee5', '').
narrative_ontology:cs_kernel_id(legitimate_health_intervention__bodily_autonomy_primary, legitimate_health_intervention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__bodily_autonomy_primary, state_public_health_agencies).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__bodily_autonomy_primary, compliant_population_cohort).
narrative_ontology:constraint_victim(legitimate_health_intervention__bodily_autonomy_primary, mandate_coerced_workers).
narrative_ontology:constraint_victim(legitimate_health_intervention__bodily_autonomy_primary, religious_and_philosophical_objectors).
narrative_ontology:constraint_victim(legitimate_health_intervention__bodily_autonomy_primary, medically_vulnerable_refusers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(legitimate_health_intervention__bodily_autonomy_primary, employers_and_institutions).
narrative_ontology:constraint_vindicates(legitimate_health_intervention__bodily_autonomy_primary, bodily_integrity_as_constitutional_floor).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and enforces vaccination/testing/treatment mandates as a condition of employment, school attendance, or access to public spaces, citing population-level benefit. From the bodily-autonomy-primary reading's lights, the agency's population-benefit justification does not license overriding an individual's non-consent; the agency nonetheless administers the leverage points (employment, licensure, access) that make refusal costly.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, state_public_health_agencies, agenda_setter,
    institutional, generational, analytical, national).

% Complies with the mandate, retains employment, access, and social standing without friction; benefits from herd-level protection without directly bearing the coercive cost the mandate machinery imposes on refusers, and can exit into any accommodation the agency offers.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, compliant_population_cohort, beneficiary,
    organized, biographical, mobile, national).

% Face termination, suspension, or exclusion from licensure and workplaces for declining the intervention. Their livelihood is leveraged against their bodily decision; the exit is trapped-adjacent because comparable employment sectors adopt parallel mandates in coordinated policy waves, closing off horizontal escape.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, mandate_coerced_workers, payer,
    moderate, biographical, constrained, national).

% Hold sincere non-medical objections to the intervention. Exemption processes, where they exist, are narrow, discretionary, and frequently denied; refusal costs them access to school, work, travel, or public services regardless of demonstrated sincerity.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, religious_and_philosophical_objectors, payer,
    powerless, biographical, trapped, national).

% Have individualized medical reasons (prior adverse reaction, contraindicated condition) to decline but face the same categorical enforcement machinery as objectors without individualized reasons; the mandate's blunt administration does not distinguish their situation from ordinary refusal absent formal, hard-to-obtain medical exemption.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, medically_vulnerable_refusers, payer,
    powerless, immediate, trapped, national).

% Administer mandate compliance as a condition of employment or enrollment under legal and reputational pressure from regulators; enforce termination and exclusion on the state's behalf, absorbing litigation risk and labor disruption as the operational cost of transmitting the state's mandate downward.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, employers_and_institutions, agenda_setter,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(legitimate_health_intervention__bodily_autonomy_primary, employers_and_institutions, payer).

% Argue in courts and legislatures that bodily integrity is a threshold right not subject to population-benefit balancing; largely excluded from the initial mandate-design process, which is dominated by public-health administrators who treat autonomy as a factor to be weighed rather than a floor.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, civil_liberties_litigators, excluded,
    organized, generational, analytical, national).

% Adjudicate challenges to mandates, weighing bodily integrity claims against state police-power justifications; their evolving jurisprudence determines how much coercive leverage the state may attach to non-consent.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimate_health_intervention__bodily_autonomy_primary, state_public_health_agencies).
narrative_ontology:fixing_cost_class(legitimate_health_intervention__bodily_autonomy_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Under this reading, the ostensible coordination function — reducing population-level disease transmission — is real as a public health matter but is explicitly denied primacy: the reading holds that no coordination benefit, however large, licenses non-consensual bodily intervention. What legitimate coordination exists is limited to voluntary uptake, incentive-based encouragement, and information provision — not conditioning survival goods (employment, access, licensure) on submission.
% TRANSFER_FUNCTION: Moves employment security, access to public and institutional spaces, and professional licensure away from non-consenting individuals and toward the state's public-health objective and toward compliant individuals who face no such leverage; coercive cost is concentrated on refusers regardless of the individualized reason for refusal.
% ABSENT_VOICES: Civil liberties litigators and bioethicists defending a hard bodily-integrity floor are structurally outside the initial mandate-design process, which is run by public-health administrators applying population-benefit calculus; their objections surface only after enforcement begins, through litigation, by which point the coercive machinery (termination, exclusion) has already been deployed against objectors.
% DISAPPEARANCE_RATIONALE: If mandate-enforced coercion disappeared overnight, terminated and excluded workers would be reinstated, exemption litigation would collapse for lack of a live controversy, and public health agencies would need to redesign uptake strategy around persuasion and incentive rather than employment/access leverage — a materially different administrative and legal landscape.
% FOUNDING_PROBLEM: Historically, mandates were built to solve free-rider and coordination failures in infectious disease control where voluntary uptake fell short of the threshold needed for population protection.
% FOUNDING_PROBLEM_CORROBORATION: Public health agencies (a benefiting party) attest the coordination problem remains live and justifies continued leverage. From outside the benefiting parties, constitutional courts in several jurisdictions and civil liberties litigators attest that, whatever the population benefit, the coercive means chosen exceed what consent-based legitimacy permits — this reading treats that outside attestation as decisive, holding the leverage-based enforcement illegitimate independent of whether the underlying public-health problem is live.
narrative_ontology:disappearance_verdict(legitimate_health_intervention__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_health_intervention__bodily_autonomy_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_health_intervention__bodily_autonomy_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(legitimate_health_intervention__bodily_autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_health_intervention__bodily_autonomy_primary, 0.66, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_health_intervention__bodily_autonomy_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimate_health_intervention__bodily_autonomy_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimate_health_intervention__bodily_autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.66 — substantial but not maximal — because the mandate machinery does produce real disease-transmission reduction as a side effect even though this reading denies that effect can license the coercion; the extraction is the leverage itself (job loss, access denial) attached to non-consent, not the underlying health goal. Suppression is authored at 0.71, higher than extraction, because the reading treats the coercive mechanism (conditioning survival goods on bodily submission) as the core violation independent of any benefit computation — this is a structural property of the enforcement architecture, not scaled by the population benefit achieved. Theater ratio is modest (0.28): most enforcement activity is functionally coercive rather than performative, though some exemption-review processes exist primarily to create the appearance of individualized consideration while functioning as near-automatic denials.
 *
 * PERSPECTIVAL GAP:
 *   From the state agency's administrative seat, the arrangement looks like a rope: broad compliance, low friction, clear population benefit. From the mandate_coerced_workers and objector seats, the identical structure computes as extraction: their bodily decision has been priced in units of employment and access. The engine should compute these divergently from the same structural facts — the claimed_type here (tangled_rope) already registers that a real coordination function (disease reduction) sits alongside asymmetric extraction (coercive leverage against non-consenting individuals), which is exactly the seat-divergence this reading exists to name.
 *
 * DIRECTIONALITY LOGIC:
 *   State public health agencies and the compliant cohort sit near the beneficiary end: agencies administer and are not personally exposed to the coercive leverage; the compliant cohort collects herd protection without bearing enforcement costs. Mandate-coerced workers, objectors, and medically vulnerable refusers sit near the full-target end: the mandate's costs (job loss, exclusion) are concentrated precisely on them, and their exit options range from constrained to trapped because parallel institutions adopt coordinated mandates, closing horizontal escape. Employers occupy a hybrid seat — administering enforcement while also bearing litigation and disruption costs, hence the dual agenda_setter/payer role.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (free-rider failure in voluntary disease control) may still be partly live as an epidemiological matter, but this reading holds that its continued liveness does not resolve the legitimacy question — a live coordination problem does not license illegitimate means. Classifying this as tangled_rope rather than snare preserves the genuine coordination function (disease reduction is real) while refusing to launder the coercive leverage as costless; classifying it as mountain or rope would erase the victim set the reading is specifically built to surface.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_ambiguity,
    'Which reading of the legitimate_health_intervention kernel should govern adjudication when population benefit is large and individual refusal is not medically grounded?',
    'Track which reading constitutional courts actually apply across jurisdictions and disease-severity contexts; a pattern of courts invoking proportionality language while functionally applying an autonomy floor (or vice versa) would reveal the operative reading independent of stated doctrine.',
    'If courts systematically apply the public_health_primary reading, this story''s classification of the standing arrangement as tangled_rope (with a real victim set) would be a minority juridical position rather than the operative legal reality, even though the reading itself remains coherent and holdable as a normative claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Which sibling reading of the kernel is judicially operative varies by jurisdiction and disease context.').

omega_variable(
    exemption_process_sincerity_screening,
    'Do religious/philosophical exemption denial rates reflect genuine insincerity screening or de facto mandate maximization by administrators?',
    'Compare exemption grant rates and stated denial rationales across agencies with different administrative incentive structures; audit a sample of denied applications for consistency with the agency''s own stated sincerity criteria.',
    'If denials are administratively motivated rather than sincerity-based, the suppression metric understates the coercive gap between the mandate''s formal legal structure (which permits exemption) and its lived operation (which forecloses it) for religious objectors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exemption_process_sincerity_screening, empirical, 'Whether exemption denial functions as genuine screening or as extraction-preserving gatekeeping.').

omega_variable(
    coordination_extraction_inseparability_under_this_reading,
    'Is the disease-reduction coordination benefit structurally separable from the coercive leverage mechanism, or does the bodily-autonomy-primary reading treat any leverage-based enforcement as inherently extractive regardless of separability?',
    'Examine voluntary-uptake public health campaigns that achieved comparable coverage without employment/access leverage; if such campaigns exist and achieve similar population outcomes, the leverage component is separable and purely extractive rather than a necessary cost of the coordination function.',
    'If separable, extractiveness may be understated at 0.66 since the entire leverage apparatus would be pure extraction with no coordination cost offset; if inseparable in practice, part of the measured extraction is a genuine (if this-reading-illegitimate) coordination cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_inseparability_under_this_reading, conceptual, 'Whether coercive leverage is a separable add-on to coordination or bound up with achieving the coordination outcome at scale.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_health_intervention__bodily_autonomy_primary, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 0, 0.1).
narrative_ontology:measurement(legi_tr_t4, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 4, 0.14).
narrative_ontology:measurement(legi_tr_t8, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 8, 0.18).
narrative_ontology:measurement(legi_tr_t12, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 12, 0.22).
narrative_ontology:measurement(legi_tr_t16, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 16, 0.25).
narrative_ontology:measurement(legi_tr_t20, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 20, 0.27).
narrative_ontology:measurement(legi_tr_t24, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(legi_be_t4, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 4, 0.42).
narrative_ontology:measurement(legi_be_t8, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 8, 0.55).
narrative_ontology:measurement(legi_be_t12, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 12, 0.63).
narrative_ontology:measurement(legi_be_t16, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 16, 0.66).
narrative_ontology:measurement(legi_be_t20, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(legi_be_t24, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 24, 0.66).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(legi_su_t4, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 4, 0.5).
narrative_ontology:measurement(legi_su_t8, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 8, 0.62).
narrative_ontology:measurement(legi_su_t12, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 12, 0.7).
narrative_ontology:measurement(legi_su_t16, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 16, 0.71).
narrative_ontology:measurement(legi_su_t20, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(legi_su_t24, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 24, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_health_intervention__bodily_autonomy_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(legitimate_health_intervention__bodily_autonomy_primary, public_health_primary).
narrative_ontology:affects_constraint(legitimate_health_intervention__bodily_autonomy_primary, proportionality_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraints decomposing the natural-language 'legitimate health intervention' kernel per the ε-invariance principle. bodily_autonomy_primary (this story) authors ε=0.66 for the standing mandate arrangement, treating coercive leverage as illegitimate extraction regardless of benefit magnitude. public_health_primary authors a materially lower ε for the identical arrangement, treating refusal as externality imposition and mandate enforcement as legitimate coordination. proportionality_reading authors an intermediate, threat-severity-conditioned ε. All three share the same underlying mandate architecture as their referent but diverge sharply in claimed_type and victim/beneficiary structure because they apply different legitimacy premises to that architecture — this is the intended kernel-reading decomposition, not measurement noise.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
