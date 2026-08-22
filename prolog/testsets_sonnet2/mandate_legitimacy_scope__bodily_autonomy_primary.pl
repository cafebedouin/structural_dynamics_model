% ============================================================================
% CONSTRAINT STORY: mandate_legitimacy_scope__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mandate_legitimacy_scope__bodily_autonomy_primary, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: mandate_legitimacy_scope__bodily_autonomy_primary
 *   human_readable: Vaccine Mandate Regime Read Through Bodily Autonomy Primacy
 *   domain: public_health_ethics/constitutional_law
 *
 * SUMMARY:
 *   This story instantiates the bodily_autonomy_primary reading of the
 *   mandate_legitimacy_scope kernel: the claim that medical intervention
 *   without individualized informed consent violates fundamental bodily
 *   integrity regardless of the collective health benefit asserted to justify
 *   it. On this reading, once compulsory mechanisms (employment termination,
 *   exclusion from public accommodation, civil penalty) attach to
 *   noncompliance, the arrangement stops being persuasion and becomes
 *   coercion, and everyone coerced into an intervention they did not consent
 *   to is a rights-bearing victim of that coercion — independent of whether
 *   the underlying public health goal was itself legitimate or achieved. The
 *   ε authored here (0.78) reflects the standing mandate arrangement AS THIS
 *   READING SEES IT: high, active extraction of bodily decision-making
 *   authority, sustained by real enforcement machinery (job loss, exclusion),
 *   not a residual or theatrical structure. This is not the same constraint
 *   as the proportionality_reading or public_health_primary siblings — those
 *   are separate constraint files with their own ε and their own
 *   victim/beneficiary sets, per the ε-invariance principle. Do not average
 *   across readings; this file holds only the bodily-autonomy reading's
 *   account.
 *
 * KEY AGENTS:
 *   - public_health_agencies: agenda_setter (institutional/analytical) — designs and enforces the mandate
 *   - unvaccinated_coerced_individuals: primary payer (powerless/trapped) — bears the bodily-integrity violation directly
 *   - medically_exempt_denied_accommodation: payer (powerless/trapped) — coerced despite legitimate contraindication
 *   - religious_objectors: payer (powerless/constrained) — objection reframed and adjudicated by a secular apparatus
 *   - employers_seeking_liability_shield: beneficiary (organized/mobile) — transfers dispute cost onto employees
 *   - vaccinated_majority: beneficiary (moderate/mobile) — receives diffuse collective benefit without bearing coercion
 *   - courts_and_civil_liberties_litigants: observer (institutional/analytical) — adjudicates the autonomy claim against asserted state interest
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mandate_legitimacy_scope__bodily_autonomy_primary, 0.78).
domain_priors:suppression_score(mandate_legitimacy_scope__bodily_autonomy_primary, 0.71).
domain_priors:theater_ratio(mandate_legitimacy_scope__bodily_autonomy_primary, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, extractiveness, 0.78).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mandate_legitimacy_scope__bodily_autonomy_primary, tangled_rope).
narrative_ontology:human_readable(mandate_legitimacy_scope__bodily_autonomy_primary, "Vaccine Mandate Regime Read Through Bodily Autonomy Primacy").
narrative_ontology:topic_domain(mandate_legitimacy_scope__bodily_autonomy_primary, "public_health_ethics/constitutional_law").

domain_priors:requires_active_enforcement(mandate_legitimacy_scope__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(mandate_legitimacy_scope__bodily_autonomy_primary, 'cbd71c63-4164-4c82-9af8-dae0a3442579').
narrative_ontology:cs_kernel_codification('cbd71c63-4164-4c82-9af8-dae0a3442579', distributed).
narrative_ontology:cs_authority_grounding('cbd71c63-4164-4c82-9af8-dae0a3442579', distributed).
narrative_ontology:cs_reading_relation('cbd71c63-4164-4c82-9af8-dae0a3442579', mandate_legitimacy_scope__public_health_primary, forecloses).
narrative_ontology:cs_reading_relation('cbd71c63-4164-4c82-9af8-dae0a3442579', mandate_legitimacy_scope__proportionality_reading, influences).
narrative_ontology:cs_axiom('cbd71c63-4164-4c82-9af8-dae0a3442579', foundational, bodily_integrity_lexically_prior_to_collective_benefit).
narrative_ontology:cs_axiom_status(bodily_integrity_lexically_prior_to_collective_benefit, holdable).
narrative_ontology:cs_axiom_grounding('cbd71c63-4164-4c82-9af8-dae0a3442579', bodily_integrity_lexically_prior_to_collective_benefit, deontological).
narrative_ontology:cs_axiom('cbd71c63-4164-4c82-9af8-dae0a3442579', secondary, consent_cannot_be_overridden_by_third_party_risk_calculus).
narrative_ontology:cs_axiom_status(consent_cannot_be_overridden_by_third_party_risk_calculus, holdable).
narrative_ontology:cs_axiom_grounding('cbd71c63-4164-4c82-9af8-dae0a3442579', consent_cannot_be_overridden_by_third_party_risk_calculus, deontological).
narrative_ontology:cs_reference_frame('cbd71c63-4164-4c82-9af8-dae0a3442579', individual_consent_as_precondition_for_lawful_medical_touching).
narrative_ontology:cs_drift_state('cbd71c63-4164-4c82-9af8-dae0a3442579', post_pandemic_mandate_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('cbd71c63-4164-4c82-9af8-dae0a3442579', '').
narrative_ontology:cs_kernel_id(mandate_legitimacy_scope__bodily_autonomy_primary, mandate_legitimacy_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__bodily_autonomy_primary, public_health_agencies).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__bodily_autonomy_primary, vaccinated_majority).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__bodily_autonomy_primary, employers_seeking_liability_shield).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__bodily_autonomy_primary, unvaccinated_coerced_individuals).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__bodily_autonomy_primary, medically_exempt_denied_accommodation).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__bodily_autonomy_primary, religious_objectors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and enforce mandate policy, framing compulsory vaccination as necessary to protect population-level health outcomes. Sets thresholds for exemption, administers penalties for noncompliance (employment loss, exclusion from public spaces, fines), and controls the evidentiary record used to justify the mandate's continuation. Bears no personal bodily cost from the intervention it compels.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, public_health_agencies, agenda_setter,
    institutional, generational, analytical, national).

% Face termination, exclusion, or civil penalty for declining a medical intervention into their own body absent individualized consent. Their stated objections — bodily integrity, personal risk assessment, distrust of expedited approval processes — are treated as noncompliance rather than as a rights claim. Exit requires either compliance against their will or accepting severe economic and social exclusion; there is no route to retain both bodily autonomy and full participation in employment or public life.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, unvaccinated_coerced_individuals, payer,
    powerless, immediate, trapped, national).

% Hold legitimate medical contraindications but find the exemption process narrow, adversarial, or unevenly administered, leaving them functionally coerced despite documented health risk from the intervention. The burden of proof sits entirely on the individual to overcome a presumption of compliance.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, medically_exempt_denied_accommodation, payer,
    powerless, immediate, trapped, national).

% Claim sincerely held objections to the intervention; exemption processes frequently require them to justify belief to a secular reviewing body, and many exemptions are denied or accepted only nominally while employment consequences proceed regardless. Their objection is a bodily-integrity claim as much as a religious one, but the two get conflated by the review apparatus.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, religious_objectors, payer,
    powerless, immediate, constrained, national).

% Adopt or comply with mandates in part to limit workplace liability and align with regulatory expectation, transferring the cost of the bodily-integrity dispute onto employees who must comply or leave. Face little direct cost themselves and can adjust policy relatively quickly compared to the individuals whose employment depends on compliance.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, employers_seeking_liability_shield, beneficiary,
    organized, biographical, mobile, national).

% Receive marginal collective health benefit from higher population coverage and are not subject to the coercive mechanism themselves, having already complied voluntarily. Their interest in herd protection is real but does not, on this reading, license overriding another person's non-consenting body.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, vaccinated_majority, beneficiary,
    moderate, biographical, mobile, national).

% Adjudicate challenges to mandate enforcement, weighing bodily autonomy claims against asserted state interest. Their rulings determine whether the coercive machinery continues, narrows, or is struck down, but they do not bear the intervention's direct cost.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, courts_and_civil_liberties_litigants, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The mandate apparatus does solve a real coordination problem — raising population immunity thresholds faster than voluntary uptake alone would achieve, which has genuine value for those actually at high risk from the disease.
% TRANSFER_FUNCTION: Moves bodily decision-making authority from the individual to the state/employer, and moves the economic and social cost of refusal (job loss, exclusion, penalty) onto individuals who withhold consent — in exchange for population-level risk reduction captured broadly and unevenly by those who never bore the coercion.
% ABSENT_VOICES: Individuals whose sincere medical, religious, or autonomy-based objections were procedurally denied are rarely heard in the policy-design process itself; exemption boards are typically staffed by the same institutions setting the mandate, not by independent bodily-autonomy advocates.
% DISAPPEARANCE_RATIONALE: If mandate enforcement disappeared overnight, employment terminations tied to vaccination status would reverse, exemption litigation would collapse for lack of a live dispute, and vaccination decisions would revert to the individualized consent model this reading holds was never legitimately suspended — a substantial population of coerced compliers and excluded workers would have their prior situation restored.
% FOUNDING_PROBLEM: Historically, mandates were built to solve population-level disease transmission during acute outbreaks where individual risk assessment was argued to be insufficient to protect third parties or the medically vulnerable.
% FOUNDING_PROBLEM_CORROBORATION: Public health agencies attest the founding problem (transmission risk to vulnerable populations) remains live and justifies continued authority. Independent civil-liberties litigation records and dissenting public-health ethicists outside the enforcing agencies attest that, on the bodily-autonomy reading, the intervention was never legitimately justified by third-party risk in the first instance — the consent violation is the injury itself, independent of whether the disease threat persists or recedes.
narrative_ontology:disappearance_verdict(mandate_legitimacy_scope__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(mandate_legitimacy_scope__bodily_autonomy_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(mandate_legitimacy_scope__bodily_autonomy_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(mandate_legitimacy_scope__bodily_autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(mandate_legitimacy_scope__bodily_autonomy_primary, 0.78, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mandate_legitimacy_scope__bodily_autonomy_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(mandate_legitimacy_scope__bodily_autonomy_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(mandate_legitimacy_scope__bodily_autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.78) because, on this reading, the mandate extracts the single most non-fungible thing a person has — control over their own body — and does so under threat of severe economic and social consequence, which this reading treats as coercion regardless of the strength of the underlying public health rationale. Suppression (0.71) reflects the active enforcement machinery: termination policies, accommodation denial, and exclusion from public life, which required real institutional buildup to operate (shown rising sharply from T0 to T8 as enforcement matured, then stabilizing). Theater ratio (0.42) captures that some exemption-review processes function more as legitimating cover for a compliance decision already made than as genuine individualized adjudication — a moderate but real proxy-goal substitution. Accessibility collapse (0.58) is moderate rather than near-total: legal challenge and exemption routes exist in principle, but this reading holds they are narrow enough that most objectors experience no live alternative. Resistance (0.74) is high, consistent with substantial organized and individual pushback (litigation, workplace disputes, public protest) that this reading treats as legitimate resistance to a rights violation rather than as mere noncompliance.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda_setter seat (public_health_agencies), the mandate is a coordination mechanism solving a genuine collective action problem — this is the public_health_primary reading's home ground. From the payer seats authored in THIS reading, the identical structure is an unconsented bodily intervention backed by coercive machinery, full stop, and the strength of the collective justification does not change that classification. The engine computes each seat's type from the structural data; this reading's authored data is built to make that divergence visible rather than resolve it in either direction.
 *
 * DIRECTIONALITY LOGIC:
 *   Unvaccinated_coerced_individuals, medically_exempt_denied_accommodation, and religious_objectors are declared victims because the mandate's coercive levers (termination, exclusion, penalty) attach specifically and only to their noncompliance — the constraint extracts bodily decision authority from them and them alone. Public_health_agencies and employers are beneficiaries: agencies collect legitimacy and population-level metrics, employers collect liability protection, neither bears the bodily cost. Vaccinated_majority sits as beneficiary rather than symmetric because, on this reading, they already complied voluntarily and now free-ride on the coercive apparatus's population effects without being subject to it themselves.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem/status mismatch is the diagnostic here: public health agencies attest the founding problem (transmission risk) remains live, while independent litigation and dissenting ethicists attest that, under this reading, the arrangement was never legitimately grounded in third-party risk to begin with — the bodily-autonomy violation is the injury, not a cost justified by an ends-based calculation. This prevents the classification from collapsing into either 'obviously coordination, ignore the objectors' or 'obviously pure extraction, ignore the outbreak-era rationale' — the tangled_rope type holds both the coordination function (raising immunity thresholds, real value to at-risk populations) and the asymmetric extraction (coerced individuals bearing the entire bodily cost) as simultaneously true facts about the same structure, exactly as this reading claims.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_divergence_autonomy_vs_publichealth,
    'Is the correct frame for evaluating vaccine mandates the individual''s unconditional bodily-integrity right, or the state''s conditional authority to act against transmissible disease risk to third parties?',
    'This is not empirically resolvable — it is a foundational disagreement about which value (individual bodily sovereignty vs. collective risk mitigation) has lexical priority when they conflict. The sibling constraint files (public_health_primary, proportionality_reading) instantiate the alternative framings; this file commits to bodily-autonomy priority.',
    'Under this reading, mandate presence alone is sufficient to place coerced individuals in the victim set and drive ε high, regardless of disease severity or vaccine efficacy. Under the public_health_primary reading, the same facts would classify as legitimate coordination with negligible or no ε. Under proportionality_reading, ε would be a function of contested epidemiological variables. The three readings are not converging estimates of one quantity; they are three different constraints sharing a kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_divergence_autonomy_vs_publichealth, conceptual, 'Location of the kernel disagreement: whether bodily autonomy or state protective authority holds lexical priority when they conflict.').

omega_variable(
    exemption_process_good_faith,
    'Are medical and religious exemption review processes genuine individualized adjudications, or largely theatrical gatekeeping designed to preserve near-universal compliance while offering a nominal due-process veneer?',
    'Audit approval/denial rates against documented medical/religious criteria, compare adjudicator independence from the mandating institution, and track whether exemption grant rates respond to individualized evidence or to administrative targets.',
    'If exemption processes are substantially theatrical, the effective accessibility_collapse and theater_ratio values authored here understate the true suppression; if processes are substantively good-faith, some fraction of the declared victim population would be more accurately characterized as voluntarily noncompliant rather than coerced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exemption_process_good_faith, empirical, 'Whether exemption review is genuine adjudication or compliance-preserving theater.').

omega_variable(
    beneficiary_free_rider_status,
    'Does the vaccinated_majority genuinely benefit as a distinct beneficiary class, or do they bear a diffuse, unmeasured cost (e.g., normalization of bodily-coercion precedent applicable to future interventions) that this reading has not captured?',
    'Track whether mandate precedent is subsequently invoked to justify unrelated compulsory interventions on the previously-compliant population; if so, today''s beneficiaries become tomorrow''s targets under the same legal reasoning.',
    'If the precedent generalizes, the vaccinated_majority''s beneficiary classification understates their long-run exposure, and the constraint''s true scope of extraction is civilizational rather than confined to the current mandate episode.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_free_rider_status, conceptual, 'Whether current beneficiaries of the mandate precedent face future exposure to the same coercive logic.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mandate_legitimacy_scope__bodily_autonomy_primary, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mand_tr_t0, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 0, 0.2).
narrative_ontology:measurement(mand_tr_t4, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 4, 0.3).
narrative_ontology:measurement(mand_tr_t8, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 8, 0.38).
narrative_ontology:measurement(mand_tr_t12, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 12, 0.44).
narrative_ontology:measurement(mand_tr_t16, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 16, 0.43).
narrative_ontology:measurement(mand_tr_t20, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 20, 0.41).
narrative_ontology:measurement(mand_tr_t24, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 24, 0.42).

% Extraction over time
narrative_ontology:measurement(mand_be_t0, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(mand_be_t4, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 4, 0.68).
narrative_ontology:measurement(mand_be_t8, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 8, 0.76).
narrative_ontology:measurement(mand_be_t12, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 12, 0.79).
narrative_ontology:measurement(mand_be_t16, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 16, 0.78).
narrative_ontology:measurement(mand_be_t20, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 20, 0.77).
narrative_ontology:measurement(mand_be_t24, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 24, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(mand_su_t0, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(mand_su_t4, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 4, 0.62).
narrative_ontology:measurement(mand_su_t8, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 8, 0.73).
narrative_ontology:measurement(mand_su_t12, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 12, 0.75).
narrative_ontology:measurement(mand_su_t16, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 16, 0.72).
narrative_ontology:measurement(mand_su_t20, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(mand_su_t24, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 24, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(mandate_legitimacy_scope__bodily_autonomy_primary, public_health_primary).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__bodily_autonomy_primary, proportionality_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the mandate_legitimacy_scope kernel. bodily_autonomy_primary (this file) authors a high, mandate-triggered ε on the theory that unconsented bodily intervention is the injury regardless of collective benefit. public_health_primary authors a low ε for the identical standing arrangement, treating the same coercive machinery as legitimate coordination given third-party transmission risk. proportionality_reading authors a variable ε as a function of contested epidemiological facts (disease severity, vaccine safety/efficacy, availability of less-restrictive alternatives). All three share the same kernel text and enforcement facts but diverge in which normative premise adjudicates legitimacy — per the ε-invariance principle, they are three constraints, not one constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
