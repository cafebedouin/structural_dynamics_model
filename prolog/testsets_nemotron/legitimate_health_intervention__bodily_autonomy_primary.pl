% ============================================================================
% CONSTRAINT STORY: legitimate_health_intervention__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: legitimate_health_intervention__bodily_autonomy_primary
 *   human_readable: Bodily Autonomy Primary Reading of Legitimate Health Intervention
 *   domain: medical_ethics/constitutional_law/public_health_policy
 *
 * SUMMARY:
 *   This constraint story instantiates the bodily_autonomy_primary reading of
 *   the legitimate_health_intervention kernel. The reading holds that
 *   legitimate medical intervention requires informed consent as a
 *   non-negotiable precondition; state coercion via mandates backed by
 *   employment and access leverage violates bodily integrity regardless of
 *   asserted public benefit. The constraint operates by making non-compliance
 *   materially costly (job loss, service denial, civic exclusion) such that
 *   consent is structurally coerced. The state functions as extractor through
 *   positional leverage rather than fiscal revenue — it captures the
 *   compliance of the coerced population, redirecting their labor and civic
 *   participation toward state-defined health objectives. The sibling
 *   readings (public_health_primary, proportionality_reading) are separate
 *   constraint stories with distinct victim sets and extraction pathways.
 *
 * KEY AGENTS:
 *   - mandate_coerced_individuals: Primary target (powerless/constrained) — bears extraction via employment/access leverage, consent structurally voided
 *   - conscientious_objectors_to_medical_interventions: Primary target (powerless/trapped) — bears concentrated extraction, identity-locked refusal
 *   - public_health_establishment: Primary beneficiary (institutional/arbitrage) — gains compliance capture, legitimacy for expanded authority
 *   - state_capacity_expansion_interests: Secondary beneficiary (institutional/mobile) — mandate infrastructure generalizes to other domains
 *   - medical_professionals_enforcing_mandates: Dual-positioned (organized/constrained) — institutional role requires enforcement, professional ethics may conflict
 *   - judicial_review_bodies: Observer (institutional/analytical) — adjudicate mandate challenges, define constitutional boundaries
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_health_intervention__bodily_autonomy_primary, 0.68).
domain_priors:suppression_score(legitimate_health_intervention__bodily_autonomy_primary, 0.72).
domain_priors:theater_ratio(legitimate_health_intervention__bodily_autonomy_primary, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, extractiveness, 0.68).
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_health_intervention__bodily_autonomy_primary, snare).
narrative_ontology:human_readable(legitimate_health_intervention__bodily_autonomy_primary, "Bodily Autonomy Primary Reading of Legitimate Health Intervention").
narrative_ontology:topic_domain(legitimate_health_intervention__bodily_autonomy_primary, "medical_ethics/constitutional_law/public_health_policy").

domain_priors:requires_active_enforcement(legitimate_health_intervention__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_health_intervention__bodily_autonomy_primary, '0d531d15-b276-45ba-80e0-a2c597781ff2').
narrative_ontology:cs_kernel_codification('0d531d15-b276-45ba-80e0-a2c597781ff2', fixed_text).
narrative_ontology:cs_authority_grounding('0d531d15-b276-45ba-80e0-a2c597781ff2', lineage).
narrative_ontology:cs_interpretation_layer_present('0d531d15-b276-45ba-80e0-a2c597781ff2').
narrative_ontology:cs_reading_relation('0d531d15-b276-45ba-80e0-a2c597781ff2', legitimate_health_intervention__public_health_primary, coexists_with).
narrative_ontology:cs_reading_relation('0d531d15-b276-45ba-80e0-a2c597781ff2', legitimate_health_intervention__proportionality_reading, influences).
narrative_ontology:cs_axiom('0d531d15-b276-45ba-80e0-a2c597781ff2', foundational, bodily_integrity_inalienable).
narrative_ontology:cs_axiom_status(bodily_integrity_inalienable, holdable).
narrative_ontology:cs_axiom_grounding('0d531d15-b276-45ba-80e0-a2c597781ff2', bodily_integrity_inalienable, deontological).
narrative_ontology:cs_axiom('0d531d15-b276-45ba-80e0-a2c597781ff2', foundational, informed_consent_legitimacy_precondition).
narrative_ontology:cs_axiom_status(informed_consent_legitimacy_precondition, holdable).
narrative_ontology:cs_axiom_grounding('0d531d15-b276-45ba-80e0-a2c597781ff2', informed_consent_legitimacy_precondition, deontological).
narrative_ontology:cs_axiom('0d531d15-b276-45ba-80e0-a2c597781ff2', secondary, state_mandate_cannot_override_consent).
narrative_ontology:cs_axiom_status(state_mandate_cannot_override_consent, holdable).
narrative_ontology:cs_axiom_grounding('0d531d15-b276-45ba-80e0-a2c597781ff2', state_mandate_cannot_override_consent, deontological).
narrative_ontology:cs_reference_frame('0d531d15-b276-45ba-80e0-a2c597781ff2', jacobson_smallpox_precedent).
narrative_ontology:cs_drift_state('0d531d15-b276-45ba-80e0-a2c597781ff2', contemporary_mandate_expansion, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('0d531d15-b276-45ba-80e0-a2c597781ff2', '').
narrative_ontology:cs_kernel_id(legitimate_health_intervention__bodily_autonomy_primary, legitimate_health_intervention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__bodily_autonomy_primary, public_health_establishment).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__bodily_autonomy_primary, state_capacity_expansion_interests).
narrative_ontology:constraint_victim(legitimate_health_intervention__bodily_autonomy_primary, mandate_coerced_individuals).
narrative_ontology:constraint_victim(legitimate_health_intervention__bodily_autonomy_primary, conscientious_objectors_to_medical_interventions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__bodily_autonomy_primary, medical_professionals_enforcing_mandates).
narrative_ontology:constraint_victim(legitimate_health_intervention__bodily_autonomy_primary, medical_professionals_enforcing_mandates).
narrative_ontology:constraint_vindicates(legitimate_health_intervention__bodily_autonomy_primary, bodily_integrity_as_inalienable_right).
narrative_ontology:constraint_vindicates(legitimate_health_intervention__bodily_autonomy_primary, informed_consent_as_legitimacy_precondition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Face mandate compliance as condition for employment, education access, healthcare access, and civic participation. Formally retain right to refuse but material consequences (job loss, service denial, exclusion) make refusal practically foreclosed for most. Bear the bodily intrusion and life-trajectory disruption without consent or compensation. Exit requires accepting severe material deprivation or migrating to jurisdictions without mandates.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, mandate_coerced_individuals, payer,
    powerless, biographical, constrained, national).

% Refuse mandates based on religious, philosophical, or medical conviction. Identity is fused with refusal — compliance would violate core self-concept. Face concentrated extraction: targeted enforcement, social stigmatization, complete exclusion from mainstream institutions. Exit is not practically available because the identity commitment makes compliance structurally impossible; the constraint attacks the self, not just behavior.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, conscientious_objectors_to_medical_interventions, payer,
    powerless, biographical, identity_locked, national).

% Designs, advocates for, and administers mandate regimes. Gains universal compliance capture for public health objectives, expanded institutional authority, and mandate infrastructure reusable across health domains. Justifies mandates as necessary coordination for population health. Controls the epidemiological narrative that defines threat levels and intervention necessity. Can pivot to new mandate targets as disease definitions expand.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, public_health_establishment, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(legitimate_health_intervention__bodily_autonomy_primary, public_health_establishment, agenda_setter).

% Political and administrative actors who benefit from the precedent and infrastructure of bodily mandate authority. The leverage architecture (employment/access conditioning) generalizes to climate policy, behavioral compliance, digital identity systems, and other domains. They do not directly administer health mandates but capture the expanded state capacity. Exit is mobile: they can shift to other domains if mandate authority is rolled back.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, state_capacity_expansion_interests, beneficiary,
    institutional, generational, mobile, national).

% Institutional role requires enforcing mandates (vaccination, testing, reporting) as condition of licensure and employment. Gain professional authority, liability protection, and institutional integration from mandate compliance. Bear ethical conflict (informed consent violation), professional autonomy loss, and liability risk for mandate injuries. Exit requires leaving licensed practice or migrating — constrained by credential portability and career investment.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, medical_professionals_enforcing_mandates, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(legitimate_health_intervention__bodily_autonomy_primary, medical_professionals_enforcing_mandates, beneficiary).

% Adjudicate constitutional challenges to mandates, defining the boundaries of state power versus bodily integrity. Apply frameworks (strict scrutiny, rational basis, proportionality) that determine whether mandates stand or fall. Their rulings shape the operational suppression level and the viability of the bodily_autonomy reading as a legal constraint on state power. Neither collect nor pay; they interpret the kernel.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, judicial_review_bodies, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective action problem of achieving population-level immunity or disease control when individual incentives diverge from collective benefit. The mandate architecture ensures universal participation without requiring individual consent, treating refusal as a free-rider problem.
% TRANSFER_FUNCTION: Moves bodily autonomy, labor market access, educational access, and civic participation from mandate_coerced_individuals and conscientious_objectors to the state (via public_health_establishment and state_capacity_expansion_interests) as captured compliance. The state does not collect fiscal revenue but captures the positional value of a compliant population.
% ABSENT_VOICES: Future generations who will inherit the mandate infrastructure precedent; immunocompromised individuals who cannot be vaccinated but are not separately represented in the mandate calculus; global populations subject to mandate regimes exported via international health regulations. These voices are excluded because the mandate architecture operates at national/state level with no structural mechanism for their representation.
% DISAPPEARANCE_RATIONALE: If mandate leverage architecture vanished overnight, the state would lose its primary mechanism for compelling medical compliance. Public health would revert to persuasion, incentive, and voluntary participation models. The state capacity expansion precedent would collapse. Mandate_coerced_individuals would regain bodily autonomy and access rights. Conscientious_objectors would no longer face identity-targeted exclusion. The epidemiological trajectory would depend on voluntary uptake — a genuine rearrangement of the state-individual relationship.
% FOUNDING_PROBLEM: Historical infectious disease epidemics (smallpox, polio, measles) where voluntary uptake failed to achieve herd immunity thresholds, creating persistent population-level morbidity/mortality that voluntary measures could not resolve. The founding mandate architecture was built for acute, high-mortality pathogens with sterilizing vaccines and clear transmission dynamics.
% FOUNDING_PROBLEM_CORROBORATION: Epidemiologists outside the public health establishment (e.g., Bhattacharya, Kulldorff, Gupta via Great Barrington Declaration) attest that the founding problem (acute high-mortality epidemics with sterilizing vaccines) is not the current mandate target — mandates now apply to non-sterilizing interventions for lower-mortality pathogens. Legal historians (e.g., Gostin, Jacobson scholars) attest that Jacobson v. Massachusetts (1905) was a narrow smallpox precedent, not a general mandate authority. The public health establishment attests the problem is live, citing ongoing pandemic threats and emergence risks.
narrative_ontology:disappearance_verdict(legitimate_health_intervention__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_health_intervention__bodily_autonomy_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_health_intervention__bodily_autonomy_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(legitimate_health_intervention__bodily_autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_health_intervention__bodily_autonomy_primary, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.68) is moderate-high because the constraint redirects the life trajectories of coerced individuals (employment, education, civic participation) without their consent, and the state captures the compliance value. Suppression (0.72) is high because alternatives are formally present but practically foreclosed by material consequences — the suppression is structural (leverage architecture) not merely persuasive. Theater ratio (0.28) is moderate: the informed consent framework is maintained rhetorically while its operational content is hollowed out by leverage. Accessibility collapse (0.62) reflects that once the leverage architecture is understood, meaningful exit from the constraint's reach is severely limited. Resistance (0.55) is moderate: conscientious objection exists but is individually costly and collectively fragmented.
 *
 * PERSPECTIVAL GAP:
 *   From the public_health_establishment seat, the constraint appears as coordination (rope-ish): a collective action problem solved by universal participation. From the mandate_coerced_individuals seat, it is pure extraction (snare): their bodily integrity and life access are leveraged for a collective goal they did not choose. The judicial_review_bodies seat sees a contested constitutional boundary. The engine computes these divergences from the structural data — the bodily_autonomy_primary reading claims snare; the public_health_primary reading would claim rope.
 *
 * DIRECTIONALITY LOGIC:
 *   The state (public_health_establishment + state_capacity_expansion_interests) is the structural beneficiary: it captures population compliance, expands institutional authority, and builds mandate infrastructure reusable across domains. Directionality d is near 0.0 for these seats. Mandate_coerced_individuals and conscientious_objectors are structural victims: they bear the full cost of compliance (bodily intrusion, loss of access, identity violation) with no offsetting benefit they have consented to. Directionality d is near 1.0. Medical_professionals are near symmetric (d≈0.5): they gain professional authority from the mandate system but bear ethical conflict and liability risk. Judicial_review_bodies are analytical (d=0.5 by definition).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (infectious disease control) is live but the arrangement (mandate leverage architecture) has drifted from its justification. The constraint was built for acute epidemic response with clear threat thresholds; it now operates as a standing capacity for leveraging bodily compliance across expanding health definitions. The mandate infrastructure persists because it serves state capacity expansion interests, not because the founding epidemiological problem requires it. This is mandatrophy: the mandate form has outlived its epidemiological function and persists via institutional inertia and authority capture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a distinct structural claim or merely a different weighting of the same legitimacy kernel?',
    'Compare the victim set and extraction pathways across the three readings: bodily_autonomy_primary produces mandate_coerced_individuals as a structural victim class with state extraction via employment/access leverage; public_health_primary produces no victim class (refusal is framed as externality); proportionality_reading produces a conditional victim class dependent on disease severity thresholds. If victim sets are structurally disjoint across readings, they are distinct constraints per ε-invariance.',
    'If readings share a victim set, ε differences are merely measurement variance on one constraint; if victim sets are disjoint, each reading instantiates a different constraint with its own ε, requiring separate stories linked by network.affects_constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the kernel''s three readings are structurally distinct constraints or one constraint with observable-dependent classification').

omega_variable(
    extraction_vs_externality_framing,
    'Does the state''s use of employment and access leverage to enforce mandates constitute extraction from coerced individuals, or is it a legitimate cost-internalization mechanism for externality imposition?',
    'Trace the resource flow: if the state collects no direct revenue but the constraint redirects labor market access, service access, and civic participation toward state-defined compliance, the extraction is positional (access captured by the compliant class) rather than fiscal. Compare with tax-funded public goods where the state collects and redistributes.',
    'If extraction is positional, the constraint is a snare with state as extractor via leverage; if cost-internalization, it is a tangled_rope with coordination function (public health) and asymmetric burden (non-compliant bear costs). The ε=0.68 reflects the positional extraction reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_externality_framing, conceptual, 'Whether mandate enforcement via employment/access leverage is extraction or externality internalization').

omega_variable(
    informed_consent_operationalization,
    'At what threshold does ''informed consent'' become structurally impossible under mandate pressure — when alternatives are formally available but practically foreclosed by employment/access consequences?',
    'Empirical study of mandate regimes: measure the proportion of non-compliers who experience job loss, service denial, or civic exclusion versus those who access genuine alternatives. If >80% face material consequences that eliminate meaningful choice, informed consent is structurally voided.',
    'If informed consent is structurally voided at high mandate enforcement, the constraint''s claimed legitimacy precondition is performative — theater_ratio should be higher. If meaningful alternatives persist, the bodily_autonomy reading overstates extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(informed_consent_operationalization, empirical, 'Whether informed consent retains operational meaning under high-leverage mandates').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_health_intervention__bodily_autonomy_primary, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legitimate_health_intervention__bodily_autonomy_primary_tr_t0, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 0, 0.12).
narrative_ontology:measurement(legitimate_health_intervention__bodily_autonomy_primary_tr_t3, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 3, 0.18).
narrative_ontology:measurement(legitimate_health_intervention__bodily_autonomy_primary_tr_t6, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 6, 0.22).
narrative_ontology:measurement(legitimate_health_intervention__bodily_autonomy_primary_tr_t9, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 9, 0.25).
narrative_ontology:measurement(legitimate_health_intervention__bodily_autonomy_primary_tr_t12, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 12, 0.28).

% Extraction over time
narrative_ontology:measurement(legitimate_health_intervention__bodily_autonomy_primary_be_t0, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(legitimate_health_intervention__bodily_autonomy_primary_be_t3, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(legitimate_health_intervention__bodily_autonomy_primary_be_t6, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(legitimate_health_intervention__bodily_autonomy_primary_be_t9, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 9, 0.6).
narrative_ontology:measurement(legitimate_health_intervention__bodily_autonomy_primary_be_t12, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 12, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(legitimate_health_intervention__bodily_autonomy_primary_su_t0, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(legitimate_health_intervention__bodily_autonomy_primary_su_t3, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 3, 0.52).
narrative_ontology:measurement(legitimate_health_intervention__bodily_autonomy_primary_su_t6, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 6, 0.58).
narrative_ontology:measurement(legitimate_health_intervention__bodily_autonomy_primary_su_t9, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 9, 0.65).
narrative_ontology:measurement(legitimate_health_intervention__bodily_autonomy_primary_su_t12, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 12, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_health_intervention__bodily_autonomy_primary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(legitimate_health_intervention__bodily_autonomy_primary, 0.12).
narrative_ontology:affects_constraint(legitimate_health_intervention__bodily_autonomy_primary, legitimate_health_intervention__public_health_primary).
narrative_ontology:affects_constraint(legitimate_health_intervention__bodily_autonomy_primary, legitimate_health_intervention__proportionality_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the legitimate_health_intervention kernel into three structurally distinct claims with disjoint victim sets: bodily_autonomy_primary produces mandate_coerced_individuals as victims with state extraction (snare, ε=0.68); public_health_primary produces no victim class, framing refusal as externality (rope/tangled_rope, ε≈0.15); proportionality_reading produces conditional victims dependent on severity thresholds (tangled_rope, ε≈0.40). The ε values differ by wide margins because the referent arrangements are different. They are linked via affects_constraints; the upstream epidemiological claims (public_health_primary) are often cited as evidence for the downstream mandate architecture (bodily_autonomy_primary's referent).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legitimate_health_intervention__bodily_autonomy_primary, institutional, 0.1).
constraint_indexing:directionality_override(legitimate_health_intervention__bodily_autonomy_primary, powerless, 0.92).
constraint_indexing:directionality_override(legitimate_health_intervention__bodily_autonomy_primary, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
