% ============================================================================
% CONSTRAINT STORY: public_health_mandate_authority__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: public_health_mandate_authority__bodily_autonomy_primary
 *   human_readable: Public Health Mandate Authority â Bodily Autonomy Primary Reading
 *   domain: public_health_law_constitutional_rights_bioethics
 *
 * SUMMARY:
 *   This constraint story instantiates the bodily_autonomy_primary reading of
 *   the public_health_mandate_authority kernel. The standing arrangement
 *   under contest is the state's asserted authority to compel medical
 *   intervention (vaccination, testing, treatment) through public health
 *   mandates, justified by collective risk reduction and
 *   vulnerable-population protection. From this reading, the arrangement is a
 *   pure extraction mechanism: it coerces bodily compliance from unvaccinated
 *   individuals under the cover of coordination, categorically violating
 *   bodily sovereignty. The immunocompromised are structurally excluded from
 *   the victim set because the reading denies any duty to protect others via
 *   non-consensual bodily invasion. Public-health-primary advocates bear zero
 *   extraction; they are not targets of the coercion. The story is authored
 *   as a kernel reading under the Îµ-invariance principle: the referent is
 *   the standing mandate authority itself, assessed by this reading's own
 *   lights, not the consensual-alternative arrangement the reading would
 *   endorse.
 *
 * KEY AGENTS:
 *   - unvaccinated_individuals: Primary target (powerless/constrained) â bear direct coercion and bodily violation
 *   - public_health_authority: Agenda-setter (institutional/arbitrage) â administers and enforces mandates
 *   - immunocompromised_population: Beneficiary side (moderate/constrained) â receive risk reduction without facing coercion
 *   - public_health_advocates: Beneficiary side (organized/mobile) â policy agenda advanced without personal cost
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(public_health_mandate_authority__bodily_autonomy_primary, 0.88).
domain_priors:suppression_score(public_health_mandate_authority__bodily_autonomy_primary, 0.85).
domain_priors:theater_ratio(public_health_mandate_authority__bodily_autonomy_primary, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, extractiveness, 0.88).
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(public_health_mandate_authority__bodily_autonomy_primary, snare).
narrative_ontology:human_readable(public_health_mandate_authority__bodily_autonomy_primary, "Public Health Mandate Authority â Bodily Autonomy Primary Reading").
narrative_ontology:topic_domain(public_health_mandate_authority__bodily_autonomy_primary, "public_health_law_constitutional_rights_bioethics").

domain_priors:requires_active_enforcement(public_health_mandate_authority__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(public_health_mandate_authority__bodily_autonomy_primary, '1832206b-911a-4f6f-84bf-5fb056c0e605').
narrative_ontology:cs_kernel_codification('1832206b-911a-4f6f-84bf-5fb056c0e605', formalized).
narrative_ontology:cs_authority_grounding('1832206b-911a-4f6f-84bf-5fb056c0e605', lineage).
narrative_ontology:cs_interpretation_layer_present('1832206b-911a-4f6f-84bf-5fb056c0e605').
narrative_ontology:cs_reading_relation('1832206b-911a-4f6f-84bf-5fb056c0e605', public_health_mandate_authority__public_health_primary, forecloses).
narrative_ontology:cs_reading_relation('1832206b-911a-4f6f-84bf-5fb056c0e605', public_health_mandate_authority__proportionality_reading, forecloses).
narrative_ontology:cs_axiom('1832206b-911a-4f6f-84bf-5fb056c0e605', foundational, bodily_sovereignty_categorical).
narrative_ontology:cs_axiom_status(bodily_sovereignty_categorical, holdable).
narrative_ontology:cs_axiom_grounding('1832206b-911a-4f6f-84bf-5fb056c0e605', bodily_sovereignty_categorical, deontological).
narrative_ontology:cs_axiom('1832206b-911a-4f6f-84bf-5fb056c0e605', foundational, non_consensual_intervention_always_illegitimate).
narrative_ontology:cs_axiom_status(non_consensual_intervention_always_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('1832206b-911a-4f6f-84bf-5fb056c0e605', non_consensual_intervention_always_illegitimate, deontological).
narrative_ontology:cs_reference_frame('1832206b-911a-4f6f-84bf-5fb056c0e605', bodily_sovereignty_default).
narrative_ontology:cs_drift_state('1832206b-911a-4f6f-84bf-5fb056c0e605', contemporary_mandate_regime, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1832206b-911a-4f6f-84bf-5fb056c0e605', '').
narrative_ontology:cs_kernel_id(public_health_mandate_authority__bodily_autonomy_primary, public_health_mandate_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__bodily_autonomy_primary, public_health_authority).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__bodily_autonomy_primary, immunocompromised_population).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__bodily_autonomy_primary, public_health_advocates).
narrative_ontology:constraint_victim(public_health_mandate_authority__bodily_autonomy_primary, unvaccinated_individuals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets mandatory vaccination and testing policies through legal police powers; enforces compliance via penalties, employment restrictions, and exclusion from public spaces; collects institutional legitimacy and expanded regulatory capacity from the mandate regime.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, public_health_authority, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(public_health_mandate_authority__bodily_autonomy_primary, public_health_authority, beneficiary).

% Receive reduced exposure risk when population compliance is high; depend on the mandate for a layer of protection they cannot provide themselves through vaccination; do not themselves face forced medical intervention but rely on the coercion of others.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, immunocompromised_population, beneficiary,
    moderate, biographical, constrained, national).

% Advance policy agendas that treat collective health outcomes as paramount; their preferred interventions are enacted without bearing the direct physical or legal costs of the mandates; they operate through institutional and media channels to justify and expand mandate authority.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, public_health_advocates, beneficiary,
    organized, biographical, mobile, national).

% Face legal and social coercion to undergo medical intervention they have refused; bear the direct cost of bodily violation, privacy loss, and potential medical risk; their alternatives are limited to accepting the intervention, accepting penalties, or attempting to evade enforcement.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, unvaccinated_individuals, payer,
    powerless, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Centralized suppression of infectious disease transmission and protection of immunocompromised populations through compulsory population-wide medical compliance.
% TRANSFER_FUNCTION: Moves bodily compliance, medical risk acceptance, and autonomy forfeiture from unvaccinated individuals to the public health authority, justified under the rhetoric of collective risk reduction.
% ABSENT_VOICES: Unvaccinated individuals subject to mandate coercion are excluded from policy design tables; their objections are treated as anti-social or ignorant rather than as legitimate autonomy claims. Bodily-autonomy absolutists are structurally marginalized in public health discourse.
% DISAPPEARANCE_RATIONALE: If the mandate authority vanished, compulsory vaccination and testing regimes would collapse, public health enforcement would lose its police-power justification for medical coercion, and the legal framework would revert to consensual or incentivized health measures; the power to compel bodily intervention would be removed from the state toolkit.
% FOUNDING_PROBLEM: Epidemic infectious disease threatening population-level mortality and healthcare system collapse, with free-riding on vaccination imperiling herd immunity thresholds.
% FOUNDING_PROBLEM_CORROBORATION: Epidemiologists and public health historians attest to historical disease burdens and outbreak potential. Civil liberties scholars, constitutional lawyers, and bioethicists outside the beneficiary set attest that the founding problem did not require non-consensual bodily intervention and that less coercive alternatives were available; no corroboration from outside the benefiting parties supports the claim that bodily coercion was the only solution.
narrative_ontology:disappearance_verdict(public_health_mandate_authority__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(public_health_mandate_authority__bodily_autonomy_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(public_health_mandate_authority__bodily_autonomy_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(public_health_mandate_authority__bodily_autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(public_health_mandate_authority__bodily_autonomy_primary, 0.88, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is authored at 0.88 because the constraint operates by overriding bodily sovereignty, a high-severity extraction domain. Suppression is 0.85 because the mandate's persistence depends on active legal enforcementâpenalties, employment exclusion, and social sanctionânot on voluntary coordination. Theater ratio rises to 0.52 as the justification narratives ('herd immunity,' 'protect the vulnerable') have become increasingly performative relative to the actual coercion being exercised. Accessibility collapse is 0.75 because once mandate authority is institutionalized, alternative frameworks (pure informed consent, decentralized risk management) are pushed to the policy fringe. Resistance is 0.70 reflecting sustained legal and civil opposition. The metrics describe the constraint's operation from this reading's viewpoint without reconciling to the claimed type; the engine will compute per-seat divergence.
 *
 * PERSPECTIVAL GAP:
 *   The unvaccinated payer seat experiences the constraint as direct bodily extraction with constrained exit, yielding a high directionality and high effective extraction. The public health authority seat experiences the same arrangement as legitimate governance with arbitrage-grade exit (can alter policy), yielding low directionality and possibly negative effective extraction (subsidy of power). The immunocompromised beneficiary seat sits at low directionality, receiving protection without cost. These divergences are structurally anchored in the beneficiary/victim declarations and exit options, not in narrative framing.
 *
 * DIRECTIONALITY LOGIC:
 *   Unvaccinated individuals are declared victims (payers), which drives their directionality toward the full-target pole. Their exit options are constrainedâlegal penalties and employment loss make noncompliance costlyâamplifying effective extraction. Public health authority and immunocompromised population are declared beneficiaries, which drives directionality toward the beneficiary pole. Public health advocates are also on the beneficiary side because they do not bear coercion and their agenda is materially advanced by the constraint. No directionality overrides are needed because the structural derivation matches the reading's intended geometry.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as snare prevents mislabeling the mandate authority as tangled rope: this reading denies that the coordination function (herd immunity, vulnerable protection) is genuine or separable from the extraction of bodily compliance. If the coordination function were genuine and the extraction asymmetric but necessary, the constraint would compute as tangled rope. By authoring high theater ratio alongside high extraction and suppression, the story signals that the coordination narrative is cover rather than cost. Mandatrophy (persistence after founding problem resolution) is not the primary dynamic here; the reading holds the arrangement was never legitimate, so obsolescence is secondary to categorical wrongness.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_claim_veracity,
    'Does public health mandate authority genuinely solve a collective-action problem that purely voluntary vaccination cannot solve, or is the coordination claim entirely performative cover?',
    'Comparative cross-jurisdictional analysis of disease outcomes in regimes with mandates versus regimes with high voluntary uptake, controlling for healthcare access and baseline immunity.',
    'If the coordination function is genuine, the constraint may compute as tangled_rope rather than snare; if the claim is cover, the snare classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_claim_veracity, empirical, 'Whether the mandate''s coordination claim is structurally real or narrative cover.').

omega_variable(
    immunocompromised_beneficiary_status,
    'Are immunocompromised individuals genuine structural beneficiaries of mandate authority, or are they instrumentalized as moral cover for state coercion?',
    'Epidemiological measurement of actual infection-risk reduction accruing to immunocompromised cohorts under mandate regimes versus voluntary regimes, paired with rhetorical analysis of policy justification texts.',
    'If instrumentalized, the beneficiary set collapses toward the state alone, sharpening the extraction profile; if genuine beneficiaries, the asymmetric extraction is tempered by a real protection transfer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(immunocompromised_beneficiary_status, conceptual, 'Ambiguity over whether immunocompromised are beneficiaries or instruments.').

omega_variable(
    authority_grounding_framing,
    'Is the mandate authority better framed as lineage-based constitutional police power or as extraction of bodily compliance capacity?',
    'Historical institutional analysis tracing whether public health law evolved from limited quarantine powers toward open-ended bodily coercion, and whether legal precedent functions as genuine constraint or as retrospective justification.',
    'A lineage framing may shift computed type toward tangled_rope if constitutional limits are real; an extraction framing reinforces the snare reading by showing authority benefits from preventing kernel revision.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_grounding_framing, conceptual, 'Framing ambiguity in the commitment-system authority grounding.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(public_health_mandate_authority__bodily_autonomy_primary, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(phm_bodily_autonomy_tr_t0, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 0, 0.2).
narrative_ontology:measurement(phm_bodily_autonomy_tr_t10, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 10, 0.25).
narrative_ontology:measurement(phm_bodily_autonomy_tr_t20, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 20, 0.32).
narrative_ontology:measurement(phm_bodily_autonomy_tr_t30, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 30, 0.4).
narrative_ontology:measurement(phm_bodily_autonomy_tr_t40, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 40, 0.46).
narrative_ontology:measurement(phm_bodily_autonomy_tr_t50, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 50, 0.52).

% Extraction over time
narrative_ontology:measurement(phm_bodily_autonomy_be_t0, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(phm_bodily_autonomy_be_t10, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(phm_bodily_autonomy_be_t20, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(phm_bodily_autonomy_be_t30, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(phm_bodily_autonomy_be_t40, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 40, 0.78).
narrative_ontology:measurement(phm_bodily_autonomy_be_t50, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 50, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(phm_bodily_autonomy_su_t0, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(phm_bodily_autonomy_su_t10, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 10, 0.52).
narrative_ontology:measurement(phm_bodily_autonomy_su_t20, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(phm_bodily_autonomy_su_t30, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 30, 0.7).
narrative_ontology:measurement(phm_bodily_autonomy_su_t40, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 40, 0.78).
narrative_ontology:measurement(phm_bodily_autonomy_su_t50, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 50, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(public_health_mandate_authority__bodily_autonomy_primary, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
