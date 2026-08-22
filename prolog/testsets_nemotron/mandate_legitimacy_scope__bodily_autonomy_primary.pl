% ============================================================================
% CONSTRAINT STORY: mandate_legitimacy_scope__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: mandate_legitimacy_scope__bodily_autonomy_primary
 *   human_readable: Mandate Legitimacy Scope: Bodily Autonomy Primary Reading
 *   domain: public_health_ethics/constitutional_law/medical_autonomy
 *
 * SUMMARY:
 *   This constraint story captures the 'bodily_autonomy_primary' reading of
 *   the contested kernel 'mandate_legitimacy_scope'. The reading asserts that
 *   medical intervention without informed consent violates fundamental bodily
 *   integrity regardless of collective benefit — a deontological constraint
 *   on state power. When mandates are present (vaccine mandates for school
 *   attendance, employment, travel), the unvaccinated who are coerced into
 *   compliance or punished for refusal enter the victim set; the state
 *   becomes a rights violator; and the standing arrangement exhibits high
 *   extraction (ε=0.82 at interval end). The narrative tracks the escalation
 *   from narrow mandates (school-entry) to broad population mandates
 *   (COVID-19 era), with corresponding rise in extraction, suppression, and
 *   theatrical justification.
 *
 * KEY AGENTS:
 *   - unvaccinated_coerced: Primary victims (powerless/identity_locked) — bear extraction through forced compliance or punishment
 *   - medically_exempt_denied: Victims (powerless/trapped) — legitimate medical exemptions overridden by administrative barriers
 *   - conscientious_objectors_punished: Victims (moderate/constrained) — religious/philosophical objectors face penalties
 *   - public_health_authorities: Agenda setters (institutional/arbitrage) — design and enforce mandates, claim collective benefit justification
 *   - state_legislatures: Agenda setters (institutional/arbitrage) — enact mandate statutes, define exemptions
 *   - courts: Observers/arbiters (institutional/analytical) — adjudicate constitutional challenges, define scope of bodily integrity
 *   - compliant_population: Beneficiaries (organized/constrained) — receive collective protection benefit, bear diffuse costs
 *   - vaccine_manufacturers: Beneficiaries (powerful/arbitrage) — capture guaranteed markets through mandate-driven demand
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mandate_legitimacy_scope__bodily_autonomy_primary, 0.82).
domain_priors:suppression_score(mandate_legitimacy_scope__bodily_autonomy_primary, 0.88).
domain_priors:theater_ratio(mandate_legitimacy_scope__bodily_autonomy_primary, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, extractiveness, 0.82).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mandate_legitimacy_scope__bodily_autonomy_primary, snare).
narrative_ontology:human_readable(mandate_legitimacy_scope__bodily_autonomy_primary, "Mandate Legitimacy Scope: Bodily Autonomy Primary Reading").
narrative_ontology:topic_domain(mandate_legitimacy_scope__bodily_autonomy_primary, "public_health_ethics/constitutional_law/medical_autonomy").

domain_priors:requires_active_enforcement(mandate_legitimacy_scope__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(mandate_legitimacy_scope__bodily_autonomy_primary, 'e330d0c9-4545-44ce-a14b-651cf5fa945c').
narrative_ontology:cs_kernel_codification('e330d0c9-4545-44ce-a14b-651cf5fa945c', distributed).
narrative_ontology:cs_authority_grounding('e330d0c9-4545-44ce-a14b-651cf5fa945c', extraction).
narrative_ontology:cs_interpretation_layer_present('e330d0c9-4545-44ce-a14b-651cf5fa945c').
narrative_ontology:cs_reading_relation('e330d0c9-4545-44ce-a14b-651cf5fa945c', mandate_legitimacy_scope__public_health_primary, coexists_with).
narrative_ontology:cs_reading_relation('e330d0c9-4545-44ce-a14b-651cf5fa945c', mandate_legitimacy_scope__proportionality_reading, influences).
narrative_ontology:cs_axiom('e330d0c9-4545-44ce-a14b-651cf5fa945c', foundational, bodily_integrity_inviolability).
narrative_ontology:cs_axiom_status(bodily_integrity_inviolability, holdable).
narrative_ontology:cs_axiom_grounding('e330d0c9-4545-44ce-a14b-651cf5fa945c', bodily_integrity_inviolability, deontological).
narrative_ontology:cs_axiom('e330d0c9-4545-44ce-a14b-651cf5fa945c', foundational, informed_consent_non_derogable).
narrative_ontology:cs_axiom_status(informed_consent_non_derogable, holdable).
narrative_ontology:cs_axiom_grounding('e330d0c9-4545-44ce-a14b-651cf5fa945c', informed_consent_non_derogable, deontological).
narrative_ontology:cs_axiom('e330d0c9-4545-44ce-a14b-651cf5fa945c', secondary, collective_benefit_does_not_override_consent).
narrative_ontology:cs_axiom_status(collective_benefit_does_not_override_consent, holdable).
narrative_ontology:cs_axiom_grounding('e330d0c9-4545-44ce-a14b-651cf5fa945c', collective_benefit_does_not_override_consent, deontological).
narrative_ontology:cs_reference_frame('e330d0c9-4545-44ce-a14b-651cf5fa945c', informed_consent_as_absolute_precondition).
narrative_ontology:cs_drift_state('e330d0c9-4545-44ce-a14b-651cf5fa945c', post_covid_mandate_expansion, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e330d0c9-4545-44ce-a14b-651cf5fa945c', '').
narrative_ontology:cs_kernel_id(mandate_legitimacy_scope__bodily_autonomy_primary, mandate_legitimacy_scope).

% --- Structural relationships ---
narrative_ontology:constraint_victim(mandate_legitimacy_scope__bodily_autonomy_primary, unvaccinated_coerced).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__bodily_autonomy_primary, medically_exempt_denied).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__bodily_autonomy_primary, conscientious_objectors_punished).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__bodily_autonomy_primary, compliant_population).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__bodily_autonomy_primary, vaccine_manufacturers).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__bodily_autonomy_primary, compliant_population).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals who refuse mandated vaccination face employment termination, school exclusion, travel bans, and social ostracization. Bodily integrity is identity-constitutive — compliance violates core self-concept, refusal destroys livelihood and social participation. Exit is identity_locked: leaving the constraint means violating the self.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, unvaccinated_coerced, payer,
    powerless, biographical, identity_locked, national).

% Individuals with legitimate medical contraindications (allergies, immunodeficiencies, prior adverse reactions) face administrative barriers to exemption: narrow exemption criteria, burdensome documentation, review boards that deny valid claims. They cannot safely vaccinate and cannot escape the mandate's penalties.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, medically_exempt_denied, payer,
    powerless, biographical, trapped, national).

% Individuals with sincere religious or philosophical objections to vaccination face penalties (job loss, school exclusion) with limited exemption pathways. Some jurisdictions allow religious exemptions; others eliminate them. Exit is constrained: relocation to permissive jurisdictions is possible but costly.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, conscientious_objectors_punished, payer,
    moderate, biographical, constrained, national).

% Design, justify, and enforce vaccine mandates. Frame mandates as necessary for collective protection. Control exemption criteria, enforcement mechanisms, and public messaging. Can shift policy when political winds change; not personally subject to the mandates they administer.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, public_health_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Enact mandate statutes, define exemption categories, set penalties. Respond to public health authority recommendations and political pressure. Not personally subject to mandates; can repeal or modify statutes.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, state_legislatures, agenda_setter,
    institutional, generational, arbitrage, national).

% Adjudicate constitutional challenges to mandates (bodily integrity, religious freedom, equal protection, due process). Define the legal scope of state authority vs. individual rights. Their rulings shape the constraint's enforcement but they neither enforce nor bear its costs.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, courts, observer,
    institutional, generational, analytical, national).

% Vaccinated individuals who receive collective protection benefit from high population immunity. Also bear diffuse costs: tax funding for mandate enforcement, social friction from coercion, erosion of medical autonomy norms that could affect them later. Exit is constrained — they benefit from the arrangement but cannot easily change it.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, compliant_population, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(mandate_legitimacy_scope__bodily_autonomy_primary, compliant_population, payer).

% Capture guaranteed, mandate-driven markets with liability protections (e.g., PREP Act, NVICP). Revenue scales with mandate scope. Lobby for mandate expansion. Not subject to mandates; can redirect production globally.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, vaccine_manufacturers, beneficiary,
    powerful, biographical, arbitrage, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(mandate_legitimacy_scope__bodily_autonomy_primary, diffuse).
narrative_ontology:fixing_cost_class(mandate_legitimacy_scope__bodily_autonomy_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From the bodily_autonomy_primary reading: NO genuine coordination function. The mandate arrangement presents disease control as its coordination problem, but from this reading's perspective, the coercive structure suppresses less restrictive alternatives (testing, targeted protection, voluntary vaccination) that would achieve the same end without violating bodily integrity. The coordination story is cover for extraction.
% TRANSFER_FUNCTION: Moves bodily autonomy and medical decision-making authority from non-consenting individuals to the state/collective apparatus. The extracted 'resource' is the right to refuse medical intervention; the recipient is the state's enforcement machinery and the collective protection it claims to provide. Vaccine manufacturers capture financial extraction via guaranteed mandate-driven demand.
% ABSENT_VOICES: Future generations who will inherit the precedent of state-compelled medical intervention; immunocompromised individuals who cannot vaccinate and for whom mandates create a false sense of security (they are used to justify mandates but their actual protection requires more than population immunity); global populations in low-access countries whose vaccine supply is diverted to mandate-driven demand in wealthy nations.
% DISAPPEARANCE_RATIONALE: If vaccine mandates vanished overnight: employment/school/travel penalties for non-vaccination would cease; exemption bureaucracies would dissolve; vaccine demand would shift from coerced to voluntary; public health authorities would need to rely on persuasion, access, and trust-building; the legal precedent for compelling medical intervention would be unmade; the bodily_autonomy_primary reading's core claim would be institutionally vindicated.
% FOUNDING_PROBLEM: Infectious disease control — specifically, achieving population immunity thresholds to prevent outbreaks of vaccine-preventable diseases. The mandate arrangement was built to solve the coordination problem of free-riding on herd immunity: individuals benefit from others' vaccination but may refuse themselves, risking collective vulnerability.
% FOUNDING_PROBLEM_CORROBORATION: Public health authorities and state legislatures attest the founding problem is live (ongoing outbreaks, suboptimal vaccination rates). The bodily_autonomy_primary reading and civil liberties organizations attest the founding problem is substantially solved by voluntary vaccination + targeted protection, and mandates now persist as rent extraction. Epidemiologists outside the benefiting parties are divided: some corroborate ongoing necessity; others corroborate that less restrictive alternatives were never seriously tried. No consensus outside the mandate-administering institutions.
narrative_ontology:disappearance_verdict(mandate_legitimacy_scope__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(mandate_legitimacy_scope__bodily_autonomy_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(mandate_legitimacy_scope__bodily_autonomy_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(mandate_legitimacy_scope__bodily_autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(mandate_legitimacy_scope__bodily_autonomy_primary, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   High extractiveness (0.82) because the standing mandate arrangement transfers bodily autonomy from non-consenting individuals to the state/collective — the coercion is the extraction mechanism. High suppression (0.88) because alternatives (exemptions, alternatives, opt-outs) are systematically narrowed or eliminated; enforcement is active and escalating (employment termination, school exclusion, travel bans). Theater ratio (0.25) is moderate-low: the public health justification is real (disease control) but increasingly performs as cover for the coercive core as less restrictive alternatives are rejected. Accessibility collapse (0.72) is high but not total — some exemptions exist, some jurisdictions allow opt-outs, but the practical exit is narrowing. Resistance (0.78) is high: legal challenges, civil disobedience, political mobilization against mandates are substantial and sustained.
 *
 * PERSPECTIVAL GAP:
 *   From the public_health_primary reading's seat, the same mandate arrangement appears as rope (genuine coordination for collective protection, beneficiaries = vulnerable populations). From the bodily_autonomy_primary seat, it computes as snare (pure extraction from the coerced, no genuine coordination function from their perspective). The proportionality_reading seat would see tangled_rope (coordination function present but extraction asymmetric, dependent on severity/efficacy/alternatives). The engine computes these per-seat divergences from the structural data: different exit_options (identity_locked vs constrained vs arbitrage), different power atoms, different declared roles.
 *
 * DIRECTIONALITY LOGIC:
 *   Victims (unvaccinated_coerced, medically_exempt_denied, conscientious_objectors_punished) are declared as such because they bear the costs of the constraint — forced medical intervention, penalty for refusal, denial of legitimate exemptions. They are powerless to moderate, with exit_options of trapped to identity_locked (bodily integrity is identity-constitutive; exit means violating core self-concept). The state/public_health_authorities are agenda_setters (institutional power, arbitrage exit) who design and enforce the arrangement. Vaccine_manufacturers are beneficiaries (powerful, arbitrage) who capture guaranteed revenue. The compliant_population are beneficiaries (organized, constrained) who receive collective protection but bear diffuse social costs. No beneficiary groups declared in base_properties because from THIS reading's structural analysis, the coordination function is cover — the arrangement's persistence depends on coercion and suppressing alternatives, making it a snare.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate arrangement's founding problem (infectious disease control) is live but the arrangement has accumulated extraction beyond the coordination function. The bodily_autonomy_primary reading identifies this as mandatrophy: the state's authority to compel medical intervention has outlived its legitimate coordination function (if it ever had one from this reading's perspective) and now operates as pure extraction. The high theater ratio growth and suppression escalation despite availability of less restrictive alternatives (testing, masking, ventilation) confirms the constraint persists by enforcement, not consent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint one reading of the contested kernel ''mandate_legitimacy_scope'', instantiating the ''bodily_autonomy_primary'' reading?',
    'This is a structural commitment declared by the generation prompt; the kernel_id and reading_id are fixed for this story.',
    'Establishes that this constraint''s ε, beneficiary/victim structure, and classification are reading-indexed. Sibling readings (public_health_primary, proportionality_reading) are separate constraints with their own ε values over the same referent (the standing mandate arrangement).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Kernel/reading identity declaration for the mandate_legitimacy_scope kernel').

omega_variable(
    bodily_autonomy_vs_public_health_delta,
    'Does the bodily_autonomy_primary reading structurally foreclose the public_health_primary reading within a single commitment framework, or do they coexist as competing positions?',
    'Analyze whether a single legal/constitutional framework can simultaneously hold: (a) medical intervention without consent violates fundamental bodily integrity regardless of collective benefit, AND (b) state authority to compel vaccination is legitimate when necessary to protect vulnerable populations. If both can be held by different parties in ongoing dispute without logical contradiction, they coexist; if one premise logically eliminates the other, forecloses.',
    'If forecloses, the kernel has a genuine logical split; if coexists_with, the dispute is political/institutional, not logical. Affects cs_structure.reading_relations classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bodily_autonomy_vs_public_health_delta, conceptual, 'Structural relationship between bodily_autonomy_primary and public_health_primary readings').

omega_variable(
    mandate_extraction_measurement,
    'What is the base extractiveness (ε) of the standing mandate arrangement as assessed from the bodily_autonomy_primary reading''s lights?',
    'The ε referent is the standing arrangement under contest — the actual mandate regime (vaccine mandates, school-entry requirements, employment conditions) — assessed by this reading''s own structural analysis (high extraction from coerced non-consenting individuals). Not the reading''s endorsed alternative (which would be ε ≈ 0).',
    'If ε is high (~0.8+), the mandate arrangement operates as extraction from the coerced. If ε is low, the reading''s claim of violation is inconsistent with its own structural assessment. The authored value 0.82 reflects high extraction from mandate presence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_extraction_measurement, empirical, 'ε referent fixing for kernel-reading stories: standing arrangement, not endorsed alternative').

omega_variable(
    coercion_mechanism_structural_vs_internalized,
    'Is the high suppression (0.88) primarily structural (legal penalties, employment termination, school exclusion) or does it include substantial internalized suppression (belief that refusal is morally wrong, identity fused with compliance)?',
    'Post-mandate removal tracking: if individuals who refused continue to experience self-silencing, social ostracization, or belief in their own wrongness after legal penalties are removed, internalized suppression is confirmed as a significant component.',
    'If substantially internalized, the constraint''s effective suppression persists beyond formal enforcement — the target carries the suppression after exit. This raises the true χ for identity-locked victims beyond the structural measure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_mechanism_structural_vs_internalized, empirical, 'Structural vs. internalized suppression mechanism in mandate coercion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mandate_legitimacy_scope__bodily_autonomy_primary, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mandate_legitimacy_scope__bodily_autonomy_primary_tr_t0, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 0, 0.12).
narrative_ontology:measurement(mandate_legitimacy_scope__bodily_autonomy_primary_tr_t6, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 6, 0.16).
narrative_ontology:measurement(mandate_legitimacy_scope__bodily_autonomy_primary_tr_t12, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 12, 0.19).
narrative_ontology:measurement(mandate_legitimacy_scope__bodily_autonomy_primary_tr_t18, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 18, 0.21).
narrative_ontology:measurement(mandate_legitimacy_scope__bodily_autonomy_primary_tr_t24, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 24, 0.23).
narrative_ontology:measurement(mandate_legitimacy_scope__bodily_autonomy_primary_tr_t30, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 30, 0.25).

% Extraction over time
narrative_ontology:measurement(mandate_legitimacy_scope__bodily_autonomy_primary_be_t0, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(mandate_legitimacy_scope__bodily_autonomy_primary_be_t6, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(mandate_legitimacy_scope__bodily_autonomy_primary_be_t12, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 12, 0.68).
narrative_ontology:measurement(mandate_legitimacy_scope__bodily_autonomy_primary_be_t18, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 18, 0.74).
narrative_ontology:measurement(mandate_legitimacy_scope__bodily_autonomy_primary_be_t24, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 24, 0.79).
narrative_ontology:measurement(mandate_legitimacy_scope__bodily_autonomy_primary_be_t30, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 30, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(mandate_legitimacy_scope__bodily_autonomy_primary_su_t0, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(mandate_legitimacy_scope__bodily_autonomy_primary_su_t6, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 6, 0.68).
narrative_ontology:measurement(mandate_legitimacy_scope__bodily_autonomy_primary_su_t12, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 12, 0.76).
narrative_ontology:measurement(mandate_legitimacy_scope__bodily_autonomy_primary_su_t18, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 18, 0.82).
narrative_ontology:measurement(mandate_legitimacy_scope__bodily_autonomy_primary_su_t24, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 24, 0.86).
narrative_ontology:measurement(mandate_legitimacy_scope__bodily_autonomy_primary_su_t30, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 30, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mandate_legitimacy_scope__bodily_autonomy_primary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(mandate_legitimacy_scope__bodily_autonomy_primary, 0.12).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__bodily_autonomy_primary, mandate_legitimacy_scope__public_health_primary).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__bodily_autonomy_primary, mandate_legitimacy_scope__proportionality_reading).

% DUAL FORMULATION NOTE:
% This story is one member of the mandate_legitimacy_scope constraint family. The three readings (bodily_autonomy_primary, public_health_primary, proportionality_reading) decompose the colloquial label 'vaccine mandate legitimacy' into structurally distinct constraints with different ε values, different beneficiary/victim structures, and different classifications. They are linked via affects_constraints. The ε-invariance principle requires this decomposition: the same label yields different ε depending on which reading's lights you assess by, so they must be separate stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(mandate_legitimacy_scope__bodily_autonomy_primary, institutional, 0.1).
constraint_indexing:directionality_override(mandate_legitimacy_scope__bodily_autonomy_primary, powerful, 0.15).
constraint_indexing:directionality_override(mandate_legitimacy_scope__bodily_autonomy_primary, powerless, 0.95).
constraint_indexing:directionality_override(mandate_legitimacy_scope__bodily_autonomy_primary, moderate, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
