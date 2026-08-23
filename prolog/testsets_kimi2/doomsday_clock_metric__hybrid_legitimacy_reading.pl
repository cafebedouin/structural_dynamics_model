% ============================================================================
% CONSTRAINT STORY: doomsday_clock_metric__hybrid_legitimacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_doomsday_clock_metric__hybrid_legitimacy_reading, []).

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
 *   constraint_id: doomsday_clock_metric__hybrid_legitimacy_reading
 *   human_readable: Doomsday Clock Hybrid Legitimacy Reading
 *   domain: science_communication/normative_epistemology/risk_governance
 *
 * SUMMARY:
 *   The Doomsday Clock is administered by the Bulletin of the Atomic
 *   Scientists as a globally recognized indicator of existential risk
 *   proximity. The hybrid_legitimacy_reading treats the clock not as a
 *   transparent empirical index nor as a mere propaganda device, but as a
 *   commitment system whose authority depends on maintaining an irreducible
 *   entanglement between scientific judgment and normative urgency. The
 *   Bulletin benefits from the institutional platform the clock provides; the
 *   public and scientific community bear the costs of methodological opacity
 *   that prevents falsification, transparent critique, or clear
 *   accountability. The reading's 'no clear beneficiary' framing is
 *   acknowledged in commentary but the schema requires structural commitment:
 *   upon analysis, legitimacy gains concentrate at the Bulletin while costs
 *   diffuse to public epistemic agency and scientific credibility.
 *
 * KEY AGENTS:
 *   - Bulletin of the Atomic Scientists: Primary agenda-setter (institutional/identity-locked) â administers the clock, benefits from authority and access
 *   - General public: Primary payer (powerless/constrained) â bears epistemic substitution and accountability void
 *   - Scientific community: Secondary payer (organized/constrained) â lends credibility, risks reputational erosion
 *   - Transparency advocates: Excluded voice (moderate/constrained) â demands methodological openness but is kept outside deliberations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(doomsday_clock_metric__hybrid_legitimacy_reading, 0.62).
domain_priors:suppression_score(doomsday_clock_metric__hybrid_legitimacy_reading, 0.45).
domain_priors:theater_ratio(doomsday_clock_metric__hybrid_legitimacy_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__hybrid_legitimacy_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__hybrid_legitimacy_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(doomsday_clock_metric__hybrid_legitimacy_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(doomsday_clock_metric__hybrid_legitimacy_reading, tangled_rope).
narrative_ontology:human_readable(doomsday_clock_metric__hybrid_legitimacy_reading, "Doomsday Clock Hybrid Legitimacy Reading").
narrative_ontology:topic_domain(doomsday_clock_metric__hybrid_legitimacy_reading, "science_communication/normative_epistemology/risk_governance").

domain_priors:requires_active_enforcement(doomsday_clock_metric__hybrid_legitimacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(doomsday_clock_metric__hybrid_legitimacy_reading, 'd866fd7e-3a2f-435d-a49e-871eec2020a4').
narrative_ontology:cs_kernel_codification('d866fd7e-3a2f-435d-a49e-871eec2020a4', fixed_text).
narrative_ontology:cs_authority_grounding('d866fd7e-3a2f-435d-a49e-871eec2020a4', lineage).
narrative_ontology:cs_interpretation_layer_present('d866fd7e-3a2f-435d-a49e-871eec2020a4').
narrative_ontology:cs_reading_relation('d866fd7e-3a2f-435d-a49e-871eec2020a4', doomsday_clock_metric__objective_index_reading, forecloses).
narrative_ontology:cs_reading_relation('d866fd7e-3a2f-435d-a49e-871eec2020a4', doomsday_clock_metric__performative_tool_reading, coexists_with).
narrative_ontology:cs_axiom('d866fd7e-3a2f-435d-a49e-871eec2020a4', foundational, scientific_normative_irreducibility).
narrative_ontology:cs_axiom_status(scientific_normative_irreducibility, holdable).
narrative_ontology:cs_axiom_grounding('d866fd7e-3a2f-435d-a49e-871eec2020a4', scientific_normative_irreducibility, empirically_contingent).
narrative_ontology:cs_axiom('d866fd7e-3a2f-435d-a49e-871eec2020a4', foundational, ambiguity_as_necessary_legitimacy_condition).
narrative_ontology:cs_axiom_status(ambiguity_as_necessary_legitimacy_condition, holdable).
narrative_ontology:cs_axiom_grounding('d866fd7e-3a2f-435d-a49e-871eec2020a4', ambiguity_as_necessary_legitimacy_condition, instrumental).
narrative_ontology:cs_reference_frame('d866fd7e-3a2f-435d-a49e-871eec2020a4', cold_war_nuclear_warning_purity).
narrative_ontology:cs_drift_state('d866fd7e-3a2f-435d-a49e-871eec2020a4', contemporary_multi_risk_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('d866fd7e-3a2f-435d-a49e-871eec2020a4', '').
narrative_ontology:cs_kernel_id(doomsday_clock_metric__hybrid_legitimacy_reading, doomsday_clock_metric).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__hybrid_legitimacy_reading, bulletin_institution).
narrative_ontology:constraint_victim(doomsday_clock_metric__hybrid_legitimacy_reading, policy_public).
narrative_ontology:constraint_victim(doomsday_clock_metric__hybrid_legitimacy_reading, scientific_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the annual Doomsday Clock setting and the surrounding media ritual. Maintains a closed Science and Security Board whose deliberative methodology and weighting criteria are not publicly specified. Derives institutional authority, donor attention, and policy access from the clock's unique cultural position as a global risk signifier.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, bulletin_institution, agenda_setter,
    institutional, generational, identity_locked, global).

% Receives the clock announcement as a hybrid scientific-political statement each year. Cannot independently verify or falsify the metric because the deliberative inputs, weights, and normative thresholds are undisclosed. Bears the cost of an accountability void in which urgent policy responses are legitimated by a metric that does not separate empirical from normative claims.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, policy_public, payer,
    powerless, biographical, constrained, global).

% Lends collective credibility to the clock through association and panel participation. Individual scientists and disciplines face difficulty separating their empirical contributions from the institution's normative laundering. Risks long-term epistemic trust erosion if the public discovers the ambiguity behind the scientific veneer.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, scientific_community, payer,
    organized, generational, constrained, global).

% Would demand open deliberation, explicit weighting schemes, and separation of empirical indicators from normative conclusions. Are structurally excluded from the closed panel process and receive no formal channel to contest the methodology before the annual announcement.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, transparency_advocates, excluded,
    moderate, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(doomsday_clock_metric__hybrid_legitimacy_reading, bulletin_institution).
narrative_ontology:fixing_cost_class(doomsday_clock_metric__hybrid_legitimacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a globally recognized, culturally legible reference point that synchronizes attention and urgency across national, disciplinary, and linguistic boundaries around existential risks that lack standardized metrics.
% TRANSFER_FUNCTION: Moves credibility and authority from the scientific community and from the symbolic weight of 'minutes to midnight' to the Bulletin institution, while transferring epistemic confusion and accountability deficits to the global public who cannot interrogate the methodology.
% ABSENT_VOICES: Methodologists demanding falsifiable criteria; global-south risk scholars who experience existential threats through colonial and economic lenses not represented on the Western-dominated panel; transparency advocates who would require open deliberation and disclosed weights.
% DISAPPEARANCE_RATIONALE: The annual global media-political ritual around existential risk would lose its central anchoring device. The Bulletin would lose its primary platform and authority source. Policy discourse would fragment across multiple risk indices without a single hybrid legitimator that merges scientific and normative registers.
% FOUNDING_PROBLEM: Early Cold War need for a visceral, universally understandable symbol to communicate the scale and immediacy of nuclear existential risk to policymakers and publics across cultures and literacy levels.
% FOUNDING_PROBLEM_CORROBORATION: Historians of science and Cold War media scholars attest to the original nuclear-communication problem from outside the Bulletin. Contemporary risk-communication researchers and critical science studies scholars outside the benefiting institution confirm the founding problem is substantially solved and the current multi-risk hybrid form persists as an institutional adaptation rather than a response to the original coordination need.
narrative_ontology:disappearance_verdict(doomsday_clock_metric__hybrid_legitimacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(doomsday_clock_metric__hybrid_legitimacy_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(doomsday_clock_metric__hybrid_legitimacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(doomsday_clock_metric__hybrid_legitimacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(doomsday_clock_metric__hybrid_legitimacy_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(doomsday_clock_metric__hybrid_legitimacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(doomsday_clock_metric__hybrid_legitimacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(doomsday_clock_metric__hybrid_legitimacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is moderately high because the clock systematically obscures normative and political weighting behind a scientific veneer, extracting legitimacy that would not accrue to a transparently political symbol. Suppression (0.45) is moderate: the constraint persists through institutional gatekeeping of the closed panel and the cultural dominance of the clock symbol, which crowds out alternative risk indices. Theater ratio (0.55) reflects that the annual announcement ritual is increasingly performative relative to the opaque methodological substance behind the hand movements. Accessibility collapse (0.4) is moderate because alternative risk indices exist but lack the cultural authority to displace the clock. Resistance (0.48) captures growing methodological criticism from science and technology studies, risk communication scholars, and some former panel members. The measurement series share one time grid so that every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The Bulletin seat experiences the constraint as a necessary and responsible exercise of judgment under radical uncertainty. The public seat experiences it as an authoritative scientific signal whose policy implications are hard to question. The analytical critical seat experiences it as legitimacy capture through deliberate ambiguity. The engine computes this divergence from the structural data rather than adjudicating it.
 *
 * DIRECTIONALITY LOGIC:
 *   The Bulletin institution sits near the beneficiary end (d low): it collects authority, media attention, and policy access through the constraint. The policy public sits near the target end (d high): they are powerless, exit-constrained, and bear the accountability void. The scientific community sits in the upper-middle range (d moderately high): their credibility is extracted and their exit is constrained by collective-action problems and reputational entanglement. Transparency advocates are excluded rather than coordinated; their exclusion is structurally necessary to preserve the ambiguity that generates legitimacy.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â simple nuclear-risk communication in the early Cold War â is dead, yet the constraint persists in an expanded multi-risk hybrid form. A piton reading would mislabel the current ambiguity as mere institutional inertia; the tangled_rope classification captures that the ambiguity is actively functional (coordinating global attention across diverse risks) while simultaneously extractive (capturing unaccountable legitimacy). This prevents misreading a live coordination-extraction hybrid as a zombie institution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ambiguity_intentionality,
    'Is the methodological ambiguity a deliberate legitimacy-preservation strategy or an unavoidable byproduct of interdisciplinary risk assessment?',
    'Archival analysis of Bulletin internal deliberations, leaked memos, or memoirs from Science and Security Board members.',
    'If deliberate, the constraint is extractive institutional strategy; if unavoidable, it may represent a coordination cost of complex risk communication that should be priced rather than eliminated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ambiguity_intentionality, empirical, 'Whether ambiguity is strategic or emergent').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (institutional gatekeeping of the closed panel) or internalized (public belief that the clock represents objective science)?',
    'Public-understanding surveys testing whether audiences distinguish empirical and normative components; comparative media-framing analysis across jurisdictions.',
    'If internalized, effective suppression exceeds the structural measure â the public carries the constraint''s legitimacy with them and would resist delegitimation even if gatekeeping ended.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, conceptual, 'Structural vs internalized suppression mechanism').

omega_variable(
    beneficiary_vacancy,
    'Does the Bulletin institution actually capture the legitimacy gains from the clock, or are the benefits truly diffuse across the global risk-governance ecosystem?',
    'Tracking resource flows (donations, media attention, policy access) to the Bulletin versus other risk-communication institutions before and after clock announcements.',
    'If concentrated at the Bulletin, the constraint exhibits classic asymmetric extraction; if diffuse, the extraction is better modeled as a coordination cost distributed across all participants.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_vacancy, empirical, 'Whether legitimacy gains concentrate or diffuse').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(doomsday_clock_metric__hybrid_legitimacy_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doomsday_hybrid_tr_t0, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(doomsday_hybrid_tr_t7, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 7, 0.35).
narrative_ontology:measurement(doomsday_hybrid_tr_t14, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 14, 0.4).
narrative_ontology:measurement(doomsday_hybrid_tr_t21, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 21, 0.48).
narrative_ontology:measurement(doomsday_hybrid_tr_t28, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 28, 0.52).
narrative_ontology:measurement(doomsday_hybrid_tr_t35, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 35, 0.55).

% Extraction over time
narrative_ontology:measurement(doomsday_hybrid_be_t0, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(doomsday_hybrid_be_t7, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 7, 0.4).
narrative_ontology:measurement(doomsday_hybrid_be_t14, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 14, 0.45).
narrative_ontology:measurement(doomsday_hybrid_be_t21, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 21, 0.52).
narrative_ontology:measurement(doomsday_hybrid_be_t28, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 28, 0.58).
narrative_ontology:measurement(doomsday_hybrid_be_t35, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 35, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(doomsday_hybrid_su_t0, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(doomsday_hybrid_su_t7, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 7, 0.3).
narrative_ontology:measurement(doomsday_hybrid_su_t14, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 14, 0.38).
narrative_ontology:measurement(doomsday_hybrid_su_t21, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 21, 0.45).
narrative_ontology:measurement(doomsday_hybrid_su_t28, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 28, 0.5).
narrative_ontology:measurement(doomsday_hybrid_su_t35, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 35, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(doomsday_clock_metric__hybrid_legitimacy_reading, doomsday_clock_metric__objective_index_reading).
narrative_ontology:affects_constraint(doomsday_clock_metric__hybrid_legitimacy_reading, doomsday_clock_metric__performative_tool_reading).

% DUAL FORMULATION NOTE:
% The natural-language label 'Doomsday Clock' conflates three structurally distinct claims: an objective index of empirical risk, a performative mobilization tool, and a hybrid legitimacy device that derives authority from irreducible entanglement. Each reading carries a distinct epsilon, stakeholder structure, and classification. They form a constraint family linked by the shared kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
