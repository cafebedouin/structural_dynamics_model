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
 *   constraint_id: mandate_legitimacy_scope__bodily_autonomy_primary
 *   human_readable: Vaccine Mandate as Bodily Integrity Violation (Bodily-Autonomy-Primary Reading)
 *   domain: public_health_ethics/constitutional_law/medical_autonomy
 *
 * SUMMARY:
 *   This story instantiates the bodily-autonomy-primary reading of the
 *   mandate-legitimacy-scope kernel: it holds that medical intervention
 *   imposed without free, informed consent violates a fundamental right that
 *   no aggregate collective benefit can override. Under this reading, the
 *   moment a jurisdiction attaches employment, access, or economic
 *   consequences to non-vaccination, it converts what may have started as a
 *   persuasion campaign into a coercive apparatus, and everyone coerced into
 *   compliance (or penalized for non-compliance) enters the victim set
 *   regardless of whether the underlying vaccine is safe, effective, or
 *   epidemiologically necessary. The state itself is read as a rights
 *   violator once compulsion is present, because the wrong is located in the
 *   absence of consent, not in the balance of harms. Two sibling constraints
 *   read the same mandate apparatus differently: the public-health-primary
 *   reading treats state compulsion as legitimate when necessary to protect
 *   the vulnerable, and the proportionality reading makes legitimacy turn on
 *   a case-by-case balancing test. All three are separate constraints sharing
 *   one kernel, per the ε-invariance principle — this file authors only the
 *   bodily-autonomy-primary claim.
 *
 * KEY AGENTS:
 *   - state_public_health_apparatus: primary agenda-setter and enforcer, institutional/analytical exit
 *   - unvaccinated_coerced_individuals: primary victims, powerless/trapped
 *   - medically_exempt_denied_recognition: compounded victims of both the mandate and its exemption bureaucracy
 *   - conscientious_objectors: victims whose objection category the apparatus treats as lesser
 *   - compliant_majority_population: beneficiaries of coordination, whose benefit this reading holds cannot license the coercion applied to others
 *   - employers_and_institutions: secondary enforcers/beneficiaries who administer the mechanism
 *   - courts_and_civil_liberties_advocates: analytical observers who adjudicate the doctrine
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mandate_legitimacy_scope__bodily_autonomy_primary, 0.81).
domain_priors:suppression_score(mandate_legitimacy_scope__bodily_autonomy_primary, 0.78).
domain_priors:theater_ratio(mandate_legitimacy_scope__bodily_autonomy_primary, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, extractiveness, 0.81).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mandate_legitimacy_scope__bodily_autonomy_primary, snare).
narrative_ontology:human_readable(mandate_legitimacy_scope__bodily_autonomy_primary, "Vaccine Mandate as Bodily Integrity Violation (Bodily-Autonomy-Primary Reading)").
narrative_ontology:topic_domain(mandate_legitimacy_scope__bodily_autonomy_primary, "public_health_ethics/constitutional_law/medical_autonomy").

domain_priors:requires_active_enforcement(mandate_legitimacy_scope__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(mandate_legitimacy_scope__bodily_autonomy_primary, '0facdf01-11d4-424d-b085-a4caf55b72ef').
narrative_ontology:cs_kernel_codification('0facdf01-11d4-424d-b085-a4caf55b72ef', distributed).
narrative_ontology:cs_authority_grounding('0facdf01-11d4-424d-b085-a4caf55b72ef', distributed).
narrative_ontology:cs_reading_relation('0facdf01-11d4-424d-b085-a4caf55b72ef', mandate_legitimacy_scope__public_health_primary, forecloses).
narrative_ontology:cs_reading_relation('0facdf01-11d4-424d-b085-a4caf55b72ef', mandate_legitimacy_scope__proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('0facdf01-11d4-424d-b085-a4caf55b72ef', foundational, consent_violation_incommensurable_with_collective_benefit).
narrative_ontology:cs_axiom_status(consent_violation_incommensurable_with_collective_benefit, holdable).
narrative_ontology:cs_axiom_grounding('0facdf01-11d4-424d-b085-a4caf55b72ef', consent_violation_incommensurable_with_collective_benefit, deontological).
narrative_ontology:cs_axiom('0facdf01-11d4-424d-b085-a4caf55b72ef', secondary, bodily_integrity_right_triggers_at_coercion_not_outcome).
narrative_ontology:cs_axiom_status(bodily_integrity_right_triggers_at_coercion_not_outcome, holdable).
narrative_ontology:cs_axiom_grounding('0facdf01-11d4-424d-b085-a4caf55b72ef', bodily_integrity_right_triggers_at_coercion_not_outcome, deontological).
narrative_ontology:cs_reference_frame('0facdf01-11d4-424d-b085-a4caf55b72ef', individual_sovereignty_over_bodily_integrity).
narrative_ontology:cs_drift_state('0facdf01-11d4-424d-b085-a4caf55b72ef', post_pandemic_mandate_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('0facdf01-11d4-424d-b085-a4caf55b72ef', '').
narrative_ontology:cs_kernel_id(mandate_legitimacy_scope__bodily_autonomy_primary, mandate_legitimacy_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__bodily_autonomy_primary, state_public_health_apparatus).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__bodily_autonomy_primary, compliant_majority_population).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__bodily_autonomy_primary, unvaccinated_coerced_individuals).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__bodily_autonomy_primary, medically_exempt_denied_recognition).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__bodily_autonomy_primary, conscientious_objectors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__bodily_autonomy_primary, employers_and_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces the mandate through licensing conditions, employment consequences, access restrictions, and fines. Frames the intervention as necessary collective protection and controls the exemption process, deciding whose objections are legally cognizable. Bears no direct bodily cost itself.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, state_public_health_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Face loss of employment, access to public spaces, education, or travel unless they submit to a medical intervention on their bodies. Under this reading, the absence of a free, informed, uncoerced choice at the moment of consent is itself the rights violation, independent of the intervention's eventual medical benefit or the health outcomes of others. Their exit is nominally 'available' (refuse and accept the penalty) but the penalty structure is designed to make refusal practically untenable.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, unvaccinated_coerced_individuals, payer,
    powerless, biographical, trapped, national).

% Have a legitimate medical basis for non-vaccination but must navigate a bureaucratic exemption process controlled by the same authority enforcing the mandate. Denial of exemption compounds the bodily-integrity violation with a due-process failure, since the body being overridden is not even the one the mandate's own logic would exempt.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, medically_exempt_denied_recognition, payer,
    powerless, biographical, trapped, national).

% Object on grounds of bodily sovereignty, religious conviction, or informed-consent doctrine rather than medical contraindication. Under this reading, their objection is treated as a bodily-integrity claim of the same normative weight as a medical exemption, but the mandate apparatus typically treats it as a lesser or non-cognizable category, forcing compliance or steep social/economic cost.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, conscientious_objectors, payer,
    powerless, biographical, constrained, national).

% Consented (or acquiesced) to vaccination and experiences reduced disease burden and unrestricted access to institutions. Under this reading, their benefit does not legitimize the coercion applied to others — the coordination gain is real but is not treated as capable of overriding the bodily-integrity claim of the coerced minority.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, compliant_majority_population, beneficiary,
    organized, biographical, mobile, national).

% Implement the state's mandate as a condition of employment or access, shifting enforcement costs onto individuals while insulating themselves from liability by pointing to government policy. Benefit from reduced workplace transmission and reduced liability exposure but administer, rather than merely experience, the coercive mechanism.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, employers_and_institutions, agenda_setter,
    organized, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(mandate_legitimacy_scope__bodily_autonomy_primary, employers_and_institutions, beneficiary).

% Adjudicate challenges to mandates and articulate the bodily-integrity doctrine in case law and advocacy, evaluating whether the state's collective-benefit justification can ever satisfy an informed-consent floor. Their rulings determine whether this reading gains formal legal force or remains a minority position.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, courts_and_civil_liberties_advocates, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(mandate_legitimacy_scope__bodily_autonomy_primary, state_public_health_apparatus).
narrative_ontology:fixing_cost_class(mandate_legitimacy_scope__bodily_autonomy_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The mandate solves a genuine collective-action problem: individually rational non-vaccination choices can produce a population-level disease burden that harms even those who vaccinate, so coordinating uptake reduces transmission and protects the medically vulnerable.
% TRANSFER_FUNCTION: The arrangement transfers bodily decision-making authority from the individual to the state/employer, and transfers the health-and-access costs of that transfer onto the individuals who did not consent — while the epidemiological benefit flows to the broader population including those who complied without coercion.
% ABSENT_VOICES: The coerced individuals' own account of what informed consent requires is structurally absent from the legitimacy calculus once a mandate is imposed — public health rulemaking treats consent as a formality to be satisfied procedurally (notice, limited exemption categories) rather than as a substantive precondition that coercion cannot cure.
% DISAPPEARANCE_RATIONALE: If the mandate apparatus disappeared, the coerced individuals would regain full decisional authority over their bodies immediately; employers would lose their liability shield and would have to renegotiate workplace safety policy from scratch; the state would lose a compliance lever it currently uses in lieu of persuasion-based public health campaigns. The world does not merely relabel — real bodily choices and real institutional exposure change.
% FOUNDING_PROBLEM: Historically, mandates were built to solve free-rider problems in communicable disease control where voluntary uptake fell short of the threshold needed for herd effects, particularly to protect populations who cannot be vaccinated themselves (infants, immunocompromised).
% FOUNDING_PROBLEM_CORROBORATION: Public health authorities and compliant-majority advocates attest the founding problem remains live (transmission risk to vulnerable populations persists). Civil liberties litigators, bioethicists working in informed-consent doctrine, and disability-rights advocates for the medically exempt attest that whatever the epidemiological merits, the mechanism chosen collapses a substantive consent requirement into a defeatable procedural one — corroboration exists on both sides from parties outside the state apparatus itself, which is what makes this a genuinely contested kernel rather than a settled question.
narrative_ontology:disappearance_verdict(mandate_legitimacy_scope__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(mandate_legitimacy_scope__bodily_autonomy_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(mandate_legitimacy_scope__bodily_autonomy_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(mandate_legitimacy_scope__bodily_autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(mandate_legitimacy_scope__bodily_autonomy_primary, 0.81, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored high (0.81 by interval end) because, under this reading, the absence of consent is itself the extracted good — bodily decisional authority is transferred to the state/employer regardless of outcome. Suppression rises sharply over the measured interval (0.30 to 0.78) as the mandate apparatus moves from voluntary encouragement to employment/access conditioning; this reading treats that shift as decisive, since the doctrine of bodily integrity is triggered by the coercive structure, not by disease severity. Theater ratio is moderate-low (0.28) because the exemption process and informed-consent documentation retain some genuine due-process function even as they primarily legitimize a decision already made. Accessibility collapse (0.62) and resistance (0.74) are both substantial: alternatives (informed refusal without consequence) largely disappear once the mandate is in force, and the coerced population mounts real, organized resistance (litigation, exemption claims, protest) rather than passive acceptance.
 *
 * PERSPECTIVAL GAP:
 *   The state and compliant-majority seats will compute this arrangement as far less extractive than the coerced seats do, because their situation description contains no bodily override — the engine's per-seat computation should reflect that divergence directly from the declared power/exit/beneficiary-victim data, not from any adjudicated 'correct' answer between readings.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (state apparatus, compliant majority) sit near the low end of directionality: the state administers and does not bear the intervention itself; the compliant majority chose vaccination and gains health/access benefits without the informed-consent violation this reading is built to detect. Victims (unvaccinated coerced individuals, denied-exemption medically exempt, conscientious objectors) sit near the high end: they are trapped or constrained, bear the bodily and economic cost, and under this reading's premises no downstream collective benefit can move them back toward the beneficiary end — the violation is located at the moment of coercion, not at the outcome.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists collapsing into either 'pure tyranny' or 'pure public good' by keeping the coordination function (reduced transmission, protection of the medically vulnerable) explicitly named in six_questions.coordination_function while holding that the coordination benefit does not cure the informed-consent defect. This is what prevents the story from being read as simple anti-vaccine advocacy: the coordination problem is real and named, but this reading's foundational axiom is that consent violations are not commensurable with collective health gains — they are a different currency entirely, so no amount of aggregate benefit converts the coercion into a Rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_bodily_autonomy,
    'Is bodily-autonomy-primary the structurally correct reading of mandate legitimacy, or is it one of three live, mutually irreconcilable framings (alongside public_health_primary and proportionality_reading) with no neutral adjudicating standpoint?',
    'This is a foundational normative disagreement about whether consent violations are commensurable with aggregate welfare gains — not an empirical question resolvable by data about vaccine safety or disease burden. Constitutional courts across jurisdictions have reached different holdings, which is itself evidence the disagreement is a live kernel contest rather than a solvable empirical dispute.',
    'If this reading is adopted as controlling doctrine, any mandate with enforcement teeth is reclassified as a rights-violating snare regardless of epidemiological justification, and the state itself becomes a named victim-producing agent. If public_health_primary or proportionality_reading is adopted instead, the same facts support a rope or tangled_rope classification. The three readings are siblings, not competitors resolved by evidence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_bodily_autonomy, conceptual, 'Whether bodily-autonomy-primary is the operative reading of the mandate_legitimacy_scope kernel, or one of three coexisting framings.').

omega_variable(
    coercion_threshold_ambiguity,
    'At what point does a public health encouragement policy (education, access to free vaccination) cross into the coercion this reading treats as a rights violation — is it the first economic consequence, or only severe consequences (job loss, exclusion from essential services)?',
    'Compare mandate regimes with graduated consequence structures (mild inconvenience vs. termination/exclusion) and assess whether the bodily-autonomy-primary doctrine, as actually argued in litigation, treats them identically or draws its own internal line.',
    'If the doctrine treats any non-zero consequence as coercive, the victim set and ε are stable across almost all real-world mandate designs. If the doctrine internally recognizes gradations, ε should vary by consequence severity even within this single reading, which would suggest this story itself may need further decomposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_threshold_ambiguity, conceptual, 'Whether this reading''s own coercion threshold is a bright line or admits internal gradation.').

omega_variable(
    exemption_bureaucracy_good_faith,
    'Is the exemption process a genuine due-process mechanism partially mitigating the consent violation, or is it primarily a legitimating theater that rarely grants relief in practice?',
    'Empirical audit of exemption approval/denial rates and appeals outcomes across jurisdictions with comparable mandate regimes.',
    'A low approval rate for facially valid claims would support raising the theater_ratio and would strengthen the claim that the medically_exempt_denied_recognition group''s victimization is compounded rather than mitigated by the exemption apparatus.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exemption_bureaucracy_good_faith, empirical, 'Whether the exemption process functions as genuine mitigation or as procedural cover.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mandate_legitimacy_scope__bodily_autonomy_primary, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mand_tr_t0, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 0, 0.1).
narrative_ontology:measurement(mand_tr_t6, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 6, 0.14).
narrative_ontology:measurement(mand_tr_t12, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 12, 0.19).
narrative_ontology:measurement(mand_tr_t18, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 18, 0.23).
narrative_ontology:measurement(mand_tr_t24, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 24, 0.26).
narrative_ontology:measurement(mand_tr_t30, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 30, 0.27).
narrative_ontology:measurement(mand_tr_t36, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 36, 0.28).

% Extraction over time
narrative_ontology:measurement(mand_be_t0, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(mand_be_t6, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(mand_be_t12, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 12, 0.68).
narrative_ontology:measurement(mand_be_t18, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 18, 0.76).
narrative_ontology:measurement(mand_be_t24, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 24, 0.79).
narrative_ontology:measurement(mand_be_t30, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 30, 0.8).
narrative_ontology:measurement(mand_be_t36, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 36, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(mand_su_t0, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(mand_su_t6, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 6, 0.48).
narrative_ontology:measurement(mand_su_t12, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 12, 0.65).
narrative_ontology:measurement(mand_su_t18, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 18, 0.73).
narrative_ontology:measurement(mand_su_t24, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 24, 0.76).
narrative_ontology:measurement(mand_su_t30, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 30, 0.77).
narrative_ontology:measurement(mand_su_t36, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 36, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mandate_legitimacy_scope__bodily_autonomy_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__bodily_autonomy_primary, public_health_primary).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__bodily_autonomy_primary, proportionality_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the mandate_legitimacy_scope kernel. bodily_autonomy_primary (this file) authors ε=0.81, claimed_type=snare, victim set including all coerced/mandated individuals regardless of health outcome. public_health_primary authors the same standing mandate arrangement with the state as legitimate protector rather than violator, producing much lower ε and a rope/tangled_rope claim. proportionality_reading authors ε as a function of disease severity and alternative availability rather than a fixed high value, producing a variable classification depending on the specific mandate's proportionality. All three share the same underlying kernel (contested legitimacy conditions for compelled medical intervention) but are structurally distinct constraints per the ε-invariance principle — they are not to be averaged or reconciled, only linked.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
