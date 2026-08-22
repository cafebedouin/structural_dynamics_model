% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_balance__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_balance__bodily_autonomy_primary, []).

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
 *   constraint_id: vaccine_mandate_balance__bodily_autonomy_primary
 *   human_readable: Vaccine Mandate Regime â Bodily Autonomy Primary Reading
 *   domain: public_health_ethics/constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint story instantiates the bodily_autonomy_primary reading of
 *   the vaccine_mandate_balance kernel. The standing arrangement under
 *   contest is the state-imposed vaccine mandate regime. From this reading,
 *   individual consent is inviolable and state-compelled medical intervention
 *   is categorically impermissible regardless of collective benefit. The
 *   regime therefore presents as pure extraction: the state coerces bodily
 *   compliance from the unvaccinated, using public health justification as
 *   cover. Immunocompromised populations are structurally positioned as
 *   claimed beneficiaries but are explicitly denied victim status in this
 *   reading, on the ground that risk acceptance is inherent to liberty. The
 *   story is authored as a kernel reading; sibling readings
 *   (public_health_primary, proportionality_reading) will carry different
 *   victim/beneficiary structures and different Îµ values.
 *
 * KEY AGENTS:
 *   - State mandate authority (agenda_setter, institutional) â designs and enforces compulsory vaccination
 *   - Unvaccinated coerced (payer, powerless/trapped) â bear the direct extraction of compelled medical intervention
 *   - Vulnerable populations (beneficiary, powerless/constrained) â claimed beneficiaries of reduced transmission
 *   - Civil liberties advocates (observer, organized/analytical) â contest the regime on rights grounds
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_balance__bodily_autonomy_primary, 0.85).
domain_priors:suppression_score(vaccine_mandate_balance__bodily_autonomy_primary, 0.88).
domain_priors:theater_ratio(vaccine_mandate_balance__bodily_autonomy_primary, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, extractiveness, 0.85).
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_balance__bodily_autonomy_primary, snare).
narrative_ontology:human_readable(vaccine_mandate_balance__bodily_autonomy_primary, "Vaccine Mandate Regime â Bodily Autonomy Primary Reading").
narrative_ontology:topic_domain(vaccine_mandate_balance__bodily_autonomy_primary, "public_health_ethics/constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(vaccine_mandate_balance__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_balance__bodily_autonomy_primary, '7beab24f-d7b6-4feb-82e8-f30ccfbd9c6a').
narrative_ontology:cs_kernel_codification('7beab24f-d7b6-4feb-82e8-f30ccfbd9c6a', formalized).
narrative_ontology:cs_authority_grounding('7beab24f-d7b6-4feb-82e8-f30ccfbd9c6a', lineage).
narrative_ontology:cs_interpretation_layer_present('7beab24f-d7b6-4feb-82e8-f30ccfbd9c6a').
narrative_ontology:cs_reading_relation('7beab24f-d7b6-4feb-82e8-f30ccfbd9c6a', vaccine_mandate_balance__public_health_primary, forecloses).
narrative_ontology:cs_reading_relation('7beab24f-d7b6-4feb-82e8-f30ccfbd9c6a', vaccine_mandate_balance__proportionality_reading, forecloses).
narrative_ontology:cs_axiom('7beab24f-d7b6-4feb-82e8-f30ccfbd9c6a', foundational, bodily_autonomy_absolute).
narrative_ontology:cs_axiom_status(bodily_autonomy_absolute, holdable).
narrative_ontology:cs_axiom_grounding('7beab24f-d7b6-4feb-82e8-f30ccfbd9c6a', bodily_autonomy_absolute, deontological).
narrative_ontology:cs_axiom('7beab24f-d7b6-4feb-82e8-f30ccfbd9c6a', foundational, state_medical_coercion_categorically_impermissible).
narrative_ontology:cs_axiom_status(state_medical_coercion_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('7beab24f-d7b6-4feb-82e8-f30ccfbd9c6a', state_medical_coercion_categorically_impermissible, deontological).
narrative_ontology:cs_reference_frame('7beab24f-d7b6-4feb-82e8-f30ccfbd9c6a', inviolable_consent_framework).
narrative_ontology:cs_drift_state('7beab24f-d7b6-4feb-82e8-f30ccfbd9c6a', post_pandemic_mandate_expansion, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('7beab24f-d7b6-4feb-82e8-f30ccfbd9c6a', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_balance__bodily_autonomy_primary, vaccine_mandate_balance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__bodily_autonomy_primary, vulnerable_populations).
narrative_ontology:constraint_victim(vaccine_mandate_balance__bodily_autonomy_primary, unvaccinated_coerced).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces compulsory vaccination policies through public health law, administrative rules, and criminal or civil penalties. Claims legitimacy from police power and collective risk reduction. Can alter or abolish the mandate but faces political and institutional inertia.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, state_mandate_authority, agenda_setter,
    institutional, generational, constrained, national).

% Individuals compelled by law to undergo medical intervention against their will. Face exclusion from employment, education, public spaces, or direct legal penalties if they refuse. No lawful exit from the mandate except through narrow exemptions that are themselves state-gated.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, unvaccinated_coerced, payer,
    powerless, biographical, trapped, national).

% People with compromised immunity who may experience reduced exposure risk if mandate compliance achieves higher population coverage. This reading holds that any benefit they receive cannot justify the coerced medical intervention of others, and that societal risk acceptance is inherent to liberty.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, vulnerable_populations, beneficiary,
    powerless, biographical, constrained, national).

% Organizations and legal advocates that contest mandates on constitutional and human rights grounds. They litigate, publish, and lobby to frame bodily autonomy as inviolable, operating outside both the beneficiary and payer sets.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, civil_liberties_advocates, observer,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The arrangement is claimed to solve a free-riding problem in communicable disease prevention, where individual opt-out degrades population-level immunity and exposes vulnerable groups to lethal or severe outcomes.
% TRANSFER_FUNCTION: Moves bodily compliance (undergoing medical intervention) from individuals to the state, and shifts infection risk from the unvaccinated and vulnerable toward the unvaccinated alone through enforced coverage.
% ABSENT_VOICES: Individuals with prior vaccine injury, religious objectors, and dissenting clinicians who question risk-benefit profiles for low-risk demographics are routinely excluded from policy advisory roles and public discourse; their absence makes the pro-mandate consensus appear broader than it is.
% DISAPPEARANCE_RATIONALE: If the mandate regime disappeared overnight, the legal boundary between state power and bodily integrity would shift sharply toward autonomy; disease exposure patterns would alter as coverage changed, and the political economy of public health legitimacy would reorganize around voluntary rather than coercive instruments.
% FOUNDING_PROBLEM: Preventing epidemic spread when voluntary vaccination coverage leaves population-level immunity below thresholds necessary to protect vulnerable individuals who cannot be vaccinated.
% FOUNDING_PROBLEM_CORROBORATION: Public health authorities attest the problem is live; civil liberties advocates, minority rights scholars, and some epidemiologists contest both the severity framing and the necessity of coercion, attesting from outside the beneficiary set.
narrative_ontology:disappearance_verdict(vaccine_mandate_balance__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_balance__bodily_autonomy_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_balance__bodily_autonomy_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vaccine_mandate_balance__bodily_autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_balance__bodily_autonomy_primary, 0.85, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_balance__bodily_autonomy_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vaccine_mandate_balance__bodily_autonomy_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vaccine_mandate_balance__bodily_autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because the regime overrides bodily integrity and forces a medical act; suppression is higher still (0.88) because persistence depends on active state penalties and exclusion. Theater ratio is moderate (0.45): the public health narrative is performatively maintained even as the coercion generalizes to populations for whom the risk-benefit profile is contested. Accessibility collapse (0.72) reflects that legal mandates close most lawful refusal paths once the regime is understood. Resistance (0.75) captures sustained civil liberties litigation and political opposition. Temporal measurements show rising extraction and suppression as mandates expanded from childhood school requirements toward adult workplace and public-space mandates over the interval.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat experiences the constraint as legitimate exercise of police power for collective protection; the payer seat experiences it as state violation of bodily integrity. The beneficiary seat experiences a risk reduction it did not request and which this reading does not treat as legitimizing. The engine will compute these seats differently: the state and vulnerable populations will derive low directionality, while the unvaccinated derive high directionality, producing divergent per-seat classifications.
 *
 * DIRECTIONALITY LOGIC:
 *   The state_mandate_authority is the structural agenda-setter and enforcer; its directionality sits near the beneficiary end because it wields the constraint rather than suffering it. The unvaccinated_coerced are the declared victims with trapped exit options, placing them near the full-target end. Vulnerable_populations are declared beneficiaries (of reduced risk) with constrained exit from their medical status, yielding low directionality. Civil_liberties_advocates are analytical observers with no stake in the extraction flow.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling by denying that the regime possesses a coordination function. The founding problemâepidemic preventionâis acknowledged as real but contested as a justification for this arrangement. Because the reading treats the mandate as categorically impermissible, it cannot be misclassified as rope or scaffold; the classification as snare reflects the absence of a sunset, the presence of active enforcement, and the concentration of extraction on trapped payers.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_sibling_divergence,
    'This constraint is one reading of the vaccine_mandate_balance kernel. How does the classification shift under the sibling readings (public_health_primary and proportionality_reading) which acknowledge coordination functions?',
    'Comparative analysis of the compiled kernel family; divergence across readings indicates the constraint''s classification is reading-dependent rather than structurally fixed.',
    'If sibling readings compute as tangled_rope or rope, the regime is contestable coordination; if all readings compute as extractive, the regime is uncontested extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_sibling_divergence, conceptual, 'Committing omega for kernel reading family divergence').

omega_variable(
    coercion_scope_ambiguity,
    'Does the measured suppression capture only direct state penalties, or also private third-party exclusion (employment, venue access) triggered by mandate frameworks?',
    'Jurisdictional comparison separating state-imposed penalties from private enforcement cascades under mandate policies.',
    'If private exclusion dominates, the constraint''s effective extraction is mediated through organizational-level suppression, altering directionality for intermediate agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_scope_ambiguity, empirical, 'Ambiguity between direct state coercion and mediated private exclusion').

omega_variable(
    autonomy_rights_ontology,
    'Is the inviolability of bodily autonomy a deontological natural right or a historically contingent positive right constructed through legal precedent?',
    'Comparative constitutional history and anthropology of state-compelled medical intervention across jurisdictions.',
    'If contingent, the constraint''s authority grounding is lineage rather than natural law; this does not change the snare classification but shifts the false-summit risk profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autonomy_rights_ontology, conceptual, 'Ontological status of the bodily autonomy claim').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_balance__bodily_autonomy_primary, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vmb_bap_tr_t0, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 0, 0.2).
narrative_ontology:measurement(vmb_bap_tr_t8, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 8, 0.25).
narrative_ontology:measurement(vmb_bap_tr_t16, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 16, 0.32).
narrative_ontology:measurement(vmb_bap_tr_t24, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 24, 0.38).
narrative_ontology:measurement(vmb_bap_tr_t32, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 32, 0.42).
narrative_ontology:measurement(vmb_bap_tr_t40, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 40, 0.45).

% Extraction over time
narrative_ontology:measurement(vmb_bap_be_t0, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(vmb_bap_be_t8, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 8, 0.55).
narrative_ontology:measurement(vmb_bap_be_t16, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 16, 0.66).
narrative_ontology:measurement(vmb_bap_be_t24, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 24, 0.74).
narrative_ontology:measurement(vmb_bap_be_t32, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 32, 0.8).
narrative_ontology:measurement(vmb_bap_be_t40, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 40, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(vmb_bap_su_t0, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(vmb_bap_su_t8, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 8, 0.6).
narrative_ontology:measurement(vmb_bap_su_t16, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 16, 0.7).
narrative_ontology:measurement(vmb_bap_su_t24, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 24, 0.78).
narrative_ontology:measurement(vmb_bap_su_t32, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 32, 0.84).
narrative_ontology:measurement(vmb_bap_su_t40, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 40, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(vaccine_mandate_balance__bodily_autonomy_primary, vaccine_mandate_balance__public_health_primary).
narrative_ontology:affects_constraint(vaccine_mandate_balance__bodily_autonomy_primary, vaccine_mandate_balance__proportionality_reading).

% DUAL FORMULATION NOTE:
% The vaccine_mandate_balance kernel decomposes into three structurally distinct constraints because the label 'vaccine mandate' conflates: (1) an absolute bodily autonomy reading (this file), (2) a proportionality-governed mandate reading, and (3) a collective-primary reading. Each carries a different Îµ, different beneficiary/victim sets, and different classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
