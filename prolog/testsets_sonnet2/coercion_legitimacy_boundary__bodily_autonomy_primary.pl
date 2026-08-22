% ============================================================================
% CONSTRAINT STORY: coercion_legitimacy_boundary__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_coercion_legitimacy_boundary__bodily_autonomy_primary, []).

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
 *   constraint_id: coercion_legitimacy_boundary__bodily_autonomy_primary
 *   human_readable: Bodily Autonomy as Categorical Bar on Compelled Medical Intervention
 *   domain: Public Health Policy / Medical Ethics / Constitutional Law
 *
 * SUMMARY:
 *   This story instantiates the bodily-autonomy-primary reading of the
 *   coercion legitimacy boundary kernel: medical intervention without consent
 *   is categorically impermissible, full stop, regardless of the disease's
 *   transmission dynamics or severity. Under this reading, courts and
 *   advocacy networks treat consent as a trump right that forecloses any
 *   weighing against collective harm-prevention. The extraction here is
 *   moderate and arises specifically from non-enforcement — the categorical
 *   bar prevents compulsion even where localized outbreak severity would
 *   otherwise justify it, transferring risk onto immunocompromised people,
 *   unvaccinated infants, and the public health departments left holding the
 *   epidemiological consequences without the tool to prevent them. This is a
 *   distinct constraint from the proportionality_reading (which would scale
 *   legitimacy to disease severity and produce a different,
 *   lower-and-variable ε) and from the public_health_primary reading (which
 *   would show enforcers as beneficiaries and refusers as victims — the
 *   beneficiary/victim structure inverts). Each reading is authored as its
 *   own file per the ε-invariance principle; this file's ε (0.42) is stable
 *   and specific to the categorical-bar arrangement as this reading's own
 *   lights assess it.
 *
 * KEY AGENTS:
 *   - vaccine_refusing_individuals: primary beneficiary of the categorical shield — refusal requires no severity-specific justification
 *   - immunocompromised_individuals: primary bearer of transferred risk — no consent-transaction seat, fully exposed to community refusal rates
 *   - infants_too_young_to_vaccinate: powerless third-party bearer, structurally unrepresented in the doctrine's own terms
 *   - autonomy_rights_litigators: agenda-setters who administer and extend the doctrine through precedent
 *   - public_health_departments: institutional payer, absorbing outbreak response costs the doctrine forecloses preventing through compulsion
 *   - constitutional_scholars: analytical observers tracking the doctrine's tension with Jacobson-era precedent
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.42).
domain_priors:suppression_score(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.35).
domain_priors:theater_ratio(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, extractiveness, 0.42).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coercion_legitimacy_boundary__bodily_autonomy_primary, tangled_rope).
narrative_ontology:human_readable(coercion_legitimacy_boundary__bodily_autonomy_primary, "Bodily Autonomy as Categorical Bar on Compelled Medical Intervention").
narrative_ontology:topic_domain(coercion_legitimacy_boundary__bodily_autonomy_primary, "Public Health Policy / Medical Ethics / Constitutional Law").

domain_priors:requires_active_enforcement(coercion_legitimacy_boundary__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(coercion_legitimacy_boundary__bodily_autonomy_primary, '6e3d6d9e-7aec-4c16-8b35-6687ac1ef49d').
narrative_ontology:cs_kernel_codification('6e3d6d9e-7aec-4c16-8b35-6687ac1ef49d', distributed).
narrative_ontology:cs_authority_grounding('6e3d6d9e-7aec-4c16-8b35-6687ac1ef49d', distributed).
narrative_ontology:cs_reading_relation('6e3d6d9e-7aec-4c16-8b35-6687ac1ef49d', coercion_legitimacy_boundary__public_health_primary, forecloses).
narrative_ontology:cs_reading_relation('6e3d6d9e-7aec-4c16-8b35-6687ac1ef49d', coercion_legitimacy_boundary__proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('6e3d6d9e-7aec-4c16-8b35-6687ac1ef49d', foundational, consent_is_categorical_trump_right).
narrative_ontology:cs_axiom_status(consent_is_categorical_trump_right, holdable).
narrative_ontology:cs_axiom_grounding('6e3d6d9e-7aec-4c16-8b35-6687ac1ef49d', consent_is_categorical_trump_right, deontological).
narrative_ontology:cs_axiom('6e3d6d9e-7aec-4c16-8b35-6687ac1ef49d', foundational, collective_benefit_calculus_never_overrides_individual_bodily_integrity).
narrative_ontology:cs_axiom_status(collective_benefit_calculus_never_overrides_individual_bodily_integrity, holdable).
narrative_ontology:cs_axiom_grounding('6e3d6d9e-7aec-4c16-8b35-6687ac1ef49d', collective_benefit_calculus_never_overrides_individual_bodily_integrity, deontological).
narrative_ontology:cs_reference_frame('6e3d6d9e-7aec-4c16-8b35-6687ac1ef49d', post_nuremberg_consent_absolutism).
narrative_ontology:cs_drift_state('6e3d6d9e-7aec-4c16-8b35-6687ac1ef49d', contemporary_pandemic_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('6e3d6d9e-7aec-4c16-8b35-6687ac1ef49d', '').
narrative_ontology:cs_kernel_id(coercion_legitimacy_boundary__bodily_autonomy_primary, coercion_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__bodily_autonomy_primary, vaccine_refusing_individuals).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__bodily_autonomy_primary, autonomy_rights_litigators).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__bodily_autonomy_primary, religious_exemption_claimants).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__bodily_autonomy_primary, immunocompromised_individuals).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__bodily_autonomy_primary, infants_too_young_to_vaccinate).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__bodily_autonomy_primary, public_health_departments).
narrative_ontology:constraint_vindicates(coercion_legitimacy_boundary__bodily_autonomy_primary, bodily_integrity_as_trump_right).
narrative_ontology:constraint_vindicates(coercion_legitimacy_boundary__bodily_autonomy_primary, informed_consent_doctrine_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decline a mandated intervention on grounds that no collective benefit calculus can override their right to refuse. Under this reading their refusal is fully legitimate and categorically shielded regardless of the epidemiological consequences to others; they bear no obligation to justify the refusal against transmission risk.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, vaccine_refusing_individuals, beneficiary,
    moderate, biographical, mobile, national).

% Bring and win cases establishing that consent cannot be overridden by aggregate welfare arguments. They administer and extend the doctrine through litigation strategy, amicus networks, and precedent-building; their institutional position benefits from every case in which the categorical bar holds regardless of the specific disease's severity.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, autonomy_rights_litigators, agenda_setter,
    organized, generational, arbitrage, national).

% Invoke the categorical consent bar to secure exemptions from school-entry or workplace mandates. The categorical framing gives them a bright-line claim that does not require litigating disease-specific proportionality each time.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, religious_exemption_claimants, beneficiary,
    moderate, biographical, mobile, national).

% Cannot be vaccinated themselves or mount adequate immune response, and depend entirely on herd protection from those around them. Under a categorical consent bar their protective margin erodes with every legitimated refusal in their community; they have no voice in the individual consent transaction that determines their exposure and no exit from the shared airspace, schools, and workplaces where transmission occurs.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, immunocompromised_individuals, payer,
    powerless, biographical, trapped, local).

% Are below the age threshold for the relevant vaccine and rely entirely on adults around them being vaccinated. They have no capacity to consent or refuse on their own behalf and no representation in the categorical-bar framing, which speaks only to the rights of the consenting (or refusing) adult.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, infants_too_young_to_vaccinate, payer,
    powerless, immediate, trapped, local).

% Are responsible for outbreak containment but operate under a legal doctrine that forecloses compulsion as a tool regardless of transmission dynamics. They absorb the cost of contact tracing, quarantine alternatives, and outbreak response that a mandate could have prevented, and are blamed for outbreaks they lack the legal authority to prevent through compulsion.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, public_health_departments, payer,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(coercion_legitimacy_boundary__bodily_autonomy_primary, public_health_departments, agenda_setter).

% School administrators, employers, and local health officers who would otherwise implement compulsory measures are structurally excluded from acting on collective-benefit grounds — the categorical bar removes their tool regardless of local outbreak severity, and their operational judgment about proportional response has no doctrinal standing under this reading.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, mandate_enforcers, excluded,
    institutional, biographical, constrained, regional).

% Study how the categorical consent bar interacts with historical precedent (Jacobson v. Massachusetts and its erosion) and document the doctrinal tension between individual-rights absolutism and collective-harm-prevention frameworks.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, constitutional_scholars, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(coercion_legitimacy_boundary__bodily_autonomy_primary, diffuse).
narrative_ontology:fixing_cost_class(coercion_legitimacy_boundary__bodily_autonomy_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None in the collective sense — this reading's coordination function is intra-individual: it stabilizes a bright-line rule so that no person's body can be conscripted into a collective project without their consent, which coordinates expectations about the limits of state power over persons.
% TRANSFER_FUNCTION: Moves epidemiological risk from the refusing individual to the immunocompromised, the too-young-to-vaccinate, and the public health infrastructure that must respond to outbreaks it cannot prevent through compulsion.
% ABSENT_VOICES: Immunocompromised individuals and infants have no seat in the consent transaction the doctrine protects — the doctrine is written entirely in terms of the rights of the person being asked to consent, with no structural mechanism for the exposed third party to object.
% DISAPPEARANCE_RATIONALE: If the categorical bar disappeared overnight, public health departments could pursue proportionality-based mandates keyed to disease severity; refusal would no longer carry an absolute doctrinal shield, exemption litigation would shift from categorical claims to case-by-case severity arguments, and immunocompromised populations would gain a legal lever they currently lack.
% FOUNDING_PROBLEM: Historical abuses of medical authority — forced sterilization, non-consensual experimentation, coercive institutionalization — established that 'collective benefit' claims had been used to justify atrocities against individuals, particularly marginalized ones.
% FOUNDING_PROBLEM_CORROBORATION: Bioethicists and disability-rights historians outside the vaccine-refusal movement corroborate that the founding problem (abuse of medical authority against non-consenting individuals, especially disabled and minority populations) was real and remains a legitimate concern; however, public health scholars and immunocompromised-patient advocacy groups — also outside the beneficiary set — attest that applying the same categorical bar to routine, well-evidenced immunization mandates transposes a remedy for historical atrocity onto a structurally different problem, and that this transposition is what generates the current victim set.
narrative_ontology:disappearance_verdict(coercion_legitimacy_boundary__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(coercion_legitimacy_boundary__bodily_autonomy_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(coercion_legitimacy_boundary__bodily_autonomy_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(coercion_legitimacy_boundary__bodily_autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coercion_legitimacy_boundary__bodily_autonomy_primary_tests).
:- end_tests(coercion_legitimacy_boundary__bodily_autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) rather than high because the harm here is a foregone-prevention harm, not an active seizure of resources — the doctrine's cost is what it prevents public health departments from doing, not what it directly takes. Suppression is comparatively low (0.35) because the doctrine does not coerce anyone into compliance; its suppressive force falls instead on the state's capacity to act, which is a different vector than classic coercive suppression of dissent. Theater ratio is low (0.20) — the doctrine is not primarily performative; it has real, binding legal effect in litigation outcomes. Accessibility collapse is moderate (0.40): alternatives to the categorical framing (proportionality-based mandates) remain live in courts and legislatures, they are simply foreclosed within this reading's own framework once adopted. Resistance is comparatively high (0.60), reflecting active pushback from public health departments, immunocompromised advocacy groups, and outbreak-response professionals who contest the categorical framing case by case.
 *
 * PERSPECTIVAL GAP:
 *   From the vaccine-refusing individual's seat, this doctrine reads as rope: pure protection of a right with no coercive overhead and no identifiable victim, since the refuser experiences no direct interaction with the immunocompromised person they expose. From the immunocompromised individual's seat, the same doctrine reads as tangled_rope shading toward snare: a coordination story (bodily autonomy protection) covering what is, from their position, an enforced transfer of epidemiological risk with no recourse and no voice. The engine computes both seats from the same structural data; the divergence is the point, not an error.
 *
 * DIRECTIONALITY LOGIC:
 *   Vaccine-refusing individuals and religious exemption claimants sit near the full-beneficiary end: the categorical bar is a right they exercise costlessly relative to this doctrine, with mobile exit and no obligation to justify against transmission risk. Immunocompromised individuals and infants sit near the full-target end: trapped exit (they share community airspace and institutions regardless of consent), powerless, and structurally voiceless in the consent transaction that determines their exposure. Public health departments occupy an institutional-payer position — they bear the operational cost of outbreaks the doctrine prevents them from forestalling through compulsion, despite institutional power, because their power does not translate into legal authority to override the categorical bar.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (historical abuse of medical authority against non-consenting individuals) was genuinely live and remains partially live as a general principle. The tangled_rope classification (rather than snare) is deliberate: there IS a real coordination function — a stable bright-line rule against bodily conscription that protects against the historical abuse pattern — but it is bundled with asymmetric extraction when applied uniformly to routine, well-evidenced immunization contexts rather than to the coercive-experimentation contexts that generated it. This prevents mislabeling the doctrine as pure extraction (it is not; the autonomy protection is real and valuable in its original domain) while also not laundering the current victim-transfer as costless coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    categorical_vs_scaled_legitimacy,
    'Is a categorical, severity-independent consent bar the structurally correct locus of the coercion legitimacy boundary, or does legitimacy properly scale with disease severity and transmission dynamics (the proportionality_reading)?',
    'This is not resolvable by data alone — it depends on which normative framework (deontological rights-primacy vs. consequentialist harm-scaling) is taken as foundational. Comparative constitutional analysis of jurisdictions that have adopted severity-scaled mandate frameworks (vs. categorical-bar jurisdictions) could show whether outcomes differ in ways that inform, though not settle, the normative question.',
    'If the categorical framing is correct, this reading''s classification stands as the structurally accurate account of the coercion boundary. If proportionality is correct, this reading is itself a false-generality error — treating a severity-independent rule as if it were the right level of abstraction when the underlying moral claim is actually conditional on transmission dynamics.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(categorical_vs_scaled_legitimacy, conceptual, 'Whether the coercion boundary is properly categorical or severity-scaled — the central disagreement between this reading and its siblings.').

omega_variable(
    founding_problem_transposition,
    'Does the doctrine''s founding problem (abuse of medical authority in coercive experimentation and forced sterilization) transpose validly onto routine, well-evidenced immunization mandates, or is this a category error that imports a remedy calibrated to atrocity into a structurally different context?',
    'Historical and bioethical analysis comparing the evidentiary and power-asymmetry conditions of the founding cases (non-consensual experimentation, coercive sterilization of institutionalized/marginalized populations) against contemporary immunization mandate contexts (routine, evidence-based, applied to general population without targeting marginalized groups).',
    'If the transposition is valid, the categorical bar is correctly generalized and the extraction measured here is the necessary cost of a genuinely general principle. If invalid, the doctrine is a scope-error: a rule correctly sized to prevent atrocity is being applied at a scope where it generates a distinct victim class (immunocompromised, infants) that the founding case never contemplated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_transposition, conceptual, 'Whether the historical justification for the categorical bar validly generalizes to routine immunization contexts.').

omega_variable(
    immunocompromised_representation_gap,
    'Is the absence of immunocompromised individuals and infants from the doctrine''s own consent framework a fixable representational gap (e.g., via third-party standing or harm-to-others doctrine) or an irreducible feature of any consent-centered framework?',
    'Legal analysis of whether third-party harm doctrines (analogous to Mill''s harm principle carve-outs) can be incorporated into a consent-primary framework without collapsing it into the proportionality or public-health-primary readings.',
    'If fixable, this reading could be refined to reduce the victim-transfer without abandoning its categorical character. If irreducible, the exclusion of exposed third parties is structural to any bodily-autonomy-primary framework, not a contingent flaw.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(immunocompromised_representation_gap, conceptual, 'Whether third-party exposure can be represented within a consent-centered doctrine without changing its fundamental character.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coercion_legitimacy_boundary__bodily_autonomy_primary, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coer_tr_t0, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 0, 0.12).
narrative_ontology:measurement(coer_tr_t4, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 4, 0.14).
narrative_ontology:measurement(coer_tr_t8, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 8, 0.16).
narrative_ontology:measurement(coer_tr_t12, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 12, 0.18).
narrative_ontology:measurement(coer_tr_t16, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 16, 0.19).
narrative_ontology:measurement(coer_tr_t20, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(coer_be_t0, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(coer_be_t4, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 4, 0.31).
narrative_ontology:measurement(coer_be_t8, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 8, 0.35).
narrative_ontology:measurement(coer_be_t12, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 12, 0.38).
narrative_ontology:measurement(coer_be_t16, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 16, 0.4).
narrative_ontology:measurement(coer_be_t20, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 20, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(coer_su_t0, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(coer_su_t4, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 4, 0.31).
narrative_ontology:measurement(coer_su_t8, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 8, 0.32).
narrative_ontology:measurement(coer_su_t12, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 12, 0.33).
narrative_ontology:measurement(coer_su_t16, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 16, 0.34).
narrative_ontology:measurement(coer_su_t20, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 20, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coercion_legitimacy_boundary__bodily_autonomy_primary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.1).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__bodily_autonomy_primary, public_health_primary).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__bodily_autonomy_primary, proportionality_reading).

% DUAL FORMULATION NOTE:
% This file, public_health_primary, and proportionality_reading form a three-member constraint family reading the same coercion_legitimacy_boundary kernel. Each reading produces a distinct ε and a distinct beneficiary/victim structure from the same underlying contest over when compelled medical intervention is legitimate. bodily_autonomy_primary (this file) has moderate ε (0.42) from non-enforcement harm; public_health_primary would show high ε from active compulsion with inverted beneficiary/victim roles; proportionality_reading would show variable ε keyed to disease-severity parameters. All three should be treated as siblings, never averaged or reconciled into one story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
