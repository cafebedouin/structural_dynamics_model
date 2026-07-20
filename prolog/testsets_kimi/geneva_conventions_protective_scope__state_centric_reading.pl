% ============================================================================
% CONSTRAINT STORY: geneva_conventions_protective_scope__state_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_protective_scope__state_centric_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: geneva_conventions_protective_scope__state_centric_reading
 *   human_readable: Geneva Conventions State-Centric Protective Scope (Article 4 Privileged Combatant Reading)
 *   domain: legal/international_humanitarian_law
 *
 * SUMMARY:
 *   This constraint instantiates the state_centric_reading of the
 *   geneva_conventions_protective_scope kernel. Under this reading, Geneva
 *   protections apply exclusively to uniformed combatants under responsible
 *   command meeting Article 4 criteria, while unprivileged belligerents fall
 *   outside treaty scope. The structural delta narrows the victim set
 *   (excluding non-state actors from POW protections), lowers effective
 *   extraction on state military operations (permitting targeting of
 *   unprivileged belligerents without combatant immunity), and concentrates
 *   beneficiary status in conventional state militaries fighting asymmetric
 *   conflicts. Sibling readings include universal_rights_reading and
 *   hybrid_proportionality_reading.
 *
 * KEY AGENTS:
 *   - states_party_to_geneva: Primary agenda_setter (institutional/constrained) â administers treaty ratification, interprets Article 4, and actively resists universal-rights expansion
 *   - conventional_state_militaries: Primary beneficiary (organized/constrained) â receives combatant immunity and POW protections while gaining targeting latitude over non-state actors
 *   - unprivileged_belligerents: Primary target/payer (powerless/trapped) â denied POW status and combatant immunity; exposed to prosecution and targeting without reciprocal protections
 *   - international_law_scholars: Analytical observer (analytical) â tracks interpretive divergence and contests the state-centric narrowing
 *   - humanitarian_organizations: Excluded voice (organized/constrained) â advocates for universal protections but is structurally absent from state-monopoly treaty fora
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_protective_scope__state_centric_reading, 0.72).
domain_priors:suppression_score(geneva_conventions_protective_scope__state_centric_reading, 0.78).
domain_priors:theater_ratio(geneva_conventions_protective_scope__state_centric_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_protective_scope__state_centric_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_protective_scope__state_centric_reading, "Geneva Conventions State-Centric Protective Scope (Article 4 Privileged Combatant Reading)").
narrative_ontology:topic_domain(geneva_conventions_protective_scope__state_centric_reading, "legal/international_humanitarian_law").

domain_priors:requires_active_enforcement(geneva_conventions_protective_scope__state_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_protective_scope__state_centric_reading, '42a6eb51-8739-4aac-8210-c9ed79272152').
narrative_ontology:cs_kernel_codification('42a6eb51-8739-4aac-8210-c9ed79272152', formalized).
narrative_ontology:cs_authority_grounding('42a6eb51-8739-4aac-8210-c9ed79272152', lineage).
narrative_ontology:cs_interpretation_layer_present('42a6eb51-8739-4aac-8210-c9ed79272152').
narrative_ontology:cs_reading_relation('42a6eb51-8739-4aac-8210-c9ed79272152', geneva_conventions_protective_scope__universal_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('42a6eb51-8739-4aac-8210-c9ed79272152', geneva_conventions_protective_scope__hybrid_proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('42a6eb51-8739-4aac-8210-c9ed79272152', foundational, combatant_status_requires_state_recognition).
narrative_ontology:cs_axiom_status(combatant_status_requires_state_recognition, holdable).
narrative_ontology:cs_axiom_grounding('42a6eb51-8739-4aac-8210-c9ed79272152', combatant_status_requires_state_recognition, conventional).
narrative_ontology:cs_axiom('42a6eb51-8739-4aac-8210-c9ed79272152', foundational, unprivileged_participation_is_unlawful).
narrative_ontology:cs_axiom_status(unprivileged_participation_is_unlawful, holdable).
narrative_ontology:cs_axiom_grounding('42a6eb51-8739-4aac-8210-c9ed79272152', unprivileged_participation_is_unlawful, conventional).
narrative_ontology:cs_reference_frame('42a6eb51-8739-4aac-8210-c9ed79272152', state_reciprocal_regularization).
narrative_ontology:cs_drift_state('42a6eb51-8739-4aac-8210-c9ed79272152', contemporary_asymmetric_conflict_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('42a6eb51-8739-4aac-8210-c9ed79272152', '').
narrative_ontology:cs_kernel_id(geneva_conventions_protective_scope__state_centric_reading, geneva_conventions_protective_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__state_centric_reading, conventional_state_militaries).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__state_centric_reading, unprivileged_belligerents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Ratify and administer the Geneva Conventions, interpret Article 4 criteria through military manuals and diplomatic practice, and actively resist universal-rights expansions that would grant non-state actors equivalent POW status.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, states_party_to_geneva, agenda_setter,
    institutional, generational, constrained, global).

% Receive combatant immunity and POW protections for uniformed members meeting Article 4 criteria; benefit from legal authority to target and prosecute non-state actors without extending reciprocal Geneva protections to them.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, conventional_state_militaries, beneficiary,
    organized, generational, constrained, global).

% Participate in hostilities without meeting Article 4 criteria; upon capture are denied POW status and combatant immunity, and may be prosecuted for acts that would be lawful for privileged combatants. No legal pathway exists to secure equivalent protections.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, unprivileged_belligerents, payer,
    powerless, immediate, trapped, global).

% Analyze treaty text, state practice, and judicial opinions to map the divergence between state-centric and universal protective scope readings; their work contests the naturalized status of the Article 4 distinction.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, international_law_scholars, observer,
    analytical, generational, analytical, global).

% Advocate for extending protections to all persons in armed conflict, including non-state actors; structurally excluded from the state-monopoly treaty interpretation and revision processes where the scope is determined.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, humanitarian_organizations, excluded,
    organized, immediate, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(geneva_conventions_protective_scope__state_centric_reading, conventional_state_militaries).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Regularizes interstate armed conflict by establishing reciprocal POW protections and combatant immunity for uniformed state military personnel, creating a legal architecture that incentivizes states to maintain disciplined armed forces clearly distinguishable from civilian populations.
% TRANSFER_FUNCTION: Moves legal immunity, targeting latitude, and procedural protections from unprivileged belligerents to conventional state militaries; the latter gain freedom to detain, prosecute, or target the former without granting reciprocal Geneva protections.
% ABSENT_VOICES: Unprivileged belligerents and humanitarian organizations are structurally excluded from treaty interpretation and revision fora; they would argue for equal protective status under international law but lack standing in the state-centric negotiation and adjudication framework.
% DISAPPEARANCE_RATIONALE: If the Article 4 state-centric distinction disappeared overnight, state militaries would lose presumptive targeting authority over non-state actors, detention and prosecution frameworks would require overhaul, and asymmetric conflict law would shift toward universal or hybrid protective standards.
% FOUNDING_PROBLEM: The need to regularize interstate warfare after 1945 by guaranteeing reciprocal treatment of captured uniformed combatants and creating incentives for states to maintain standing armies clearly distinguishable from civilians.
% FOUNDING_PROBLEM_CORROBORATION: Military historians and treaty drafters attest to the interstate reciprocal problem. Human rights law scholars, ICRC customary law studies, and transnational legal advocates from outside the state-military beneficiary set attest that the founding problem has shifted toward asymmetric and non-international conflicts while the constraint persists in its original state-centric form.
narrative_ontology:disappearance_verdict(geneva_conventions_protective_scope__state_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_protective_scope__state_centric_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_protective_scope__state_centric_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(geneva_conventions_protective_scope__state_centric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_protective_scope__state_centric_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_protective_scope__state_centric_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_protective_scope__state_centric_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_protective_scope__state_centric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the denial of POW protections and combatant immunity to unprivileged belligerents constitutes a severe legal exclusion that privileges state actors. Suppression (0.78) is higher still because the reading's persistence depends on active state enforcement: military manuals, domestic prosecution of unlawful combatants, diplomatic rejection of universalist treaty interpretations, and the structural exclusion of non-state actors from revision processes. Theater ratio (0.45) reflects a moderate performative loadâstates invoke reciprocity and civilized warfare while an increasing share of interpretive work defends operational latitude against non-state actors rather than genuine interstate restraint. Accessibility collapse (0.65) captures that universal and hybrid readings persist in scholarship and some jurisprudence but are largely inaccessible in state military practice and domestic courts. Resistance (0.60) registers pushback from human rights scholars, some international tribunals, and non-state actors who reject the framework.
 *
 * PERSPECTIVAL GAP:
 *   From the conventional_state_militaries seat, the constraint is legitimate interstate law that civilizes warfare and protects captured soldiers; from the unprivileged_belligerents seat, it operates as legalized exposure that strips them of protections based on state-centric status criteria. The engine computes this divergence from the structural asymmetry in beneficiary versus victim declarations and the radical difference in exit options (constrained versus trapped).
 *
 * DIRECTIONALITY LOGIC:
 *   Conventional_state_militaries are declared beneficiaries with constrained exit, placing structural directionality near the beneficiary end. Unprivileged_belligerents are declared victims with trapped exit, placing directionality near the full-target end. States_party_to_geneva as agenda_setter with constrained exit sits between, reflecting both administrative control and self-binding under the treaty. International_law_scholars occupy the analytical seat with no directional stake.
 *
 * MANDATROPHY ANALYSIS:
 *   The state-centric reading prevents mislabeling by preserving the genuine coordination function among states (reciprocal restraint and POW protections for uniformed combatants) while simultaneously registering the asymmetric extraction from non-state actors. Without the tangled_rope classification, one might misread the Geneva Conventions as either pure coordination (ignoring the unprivileged belligerent victim set) or pure extraction (ignoring the interstate regularization function). The classification captures that both are structurally true of the same arrangement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_scope_ambiguity,
    'Does the Geneva protective scope extend exclusively to state-party combatants meeting Article 4 criteria, or does it encompass all persons affected by armed conflict regardless of status?',
    'Comparative analysis of state military manuals, domestic court rulings on unlawful combatants, and international judicial opinions (e.g., ICRC Customary IHL study, international tribunal jurisprudence).',
    'If universal protections are deemed treaty-bound or customary, this constraint''s extractiveness collapses toward rope or mountain; if the state-centric reading holds, tangled_rope or snare classification persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_scope_ambiguity, conceptual, 'Structural ambiguity between state-centric and universal protective scope readings').

omega_variable(
    active_enforcement_necessity,
    'Would the state-centric protective scope persist without active state resistance to universal-rights expansion in treaty interpretation?',
    'Track state submissions to international courts, treaty body interpretations, and the rate of domestic prosecution of unprivileged belligerents over time.',
    'If enforcement-dependent, confirms tangled_rope classification; if self-executing treaty text, leans toward a more stable structural classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(active_enforcement_necessity, empirical, 'Whether persistence depends on active state enforcement').

omega_variable(
    marginal_coordination_genuine,
    'Does the Article 4 distinction solve a real coordination problemâreciprocal restraint among statesâthat would collapse if non-state actors received equivalent POW protections?',
    'Game-theoretic and historical analysis of state compliance with IHL before and after 1949, comparing interstate conflict regularization rates.',
    'If yes, the coordination function is genuine and tangled_rope is correct; if no, the coordination story is cover for extraction, pushing classification toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(marginal_coordination_genuine, empirical, 'Genuineness of the interstate coordination function').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_protective_scope__state_centric_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t0, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(gene_tr_t15, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 15, 0.33).
narrative_ontology:measurement(gene_tr_t30, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 30, 0.36).
narrative_ontology:measurement(gene_tr_t45, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 45, 0.4).
narrative_ontology:measurement(gene_tr_t60, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 60, 0.43).
narrative_ontology:measurement(gene_tr_t75, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 75, 0.45).

% Extraction over time
narrative_ontology:measurement(gene_be_t0, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(gene_be_t15, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(gene_be_t30, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(gene_be_t45, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 45, 0.67).
narrative_ontology:measurement(gene_be_t60, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 60, 0.7).
narrative_ontology:measurement(gene_be_t75, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 75, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t0, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(gene_su_t15, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 15, 0.52).
narrative_ontology:measurement(gene_su_t30, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 30, 0.6).
narrative_ontology:measurement(gene_su_t45, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 45, 0.68).
narrative_ontology:measurement(gene_su_t60, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 60, 0.74).
narrative_ontology:measurement(gene_su_t75, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 75, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_protective_scope__state_centric_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__state_centric_reading, universal_rights_reading).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__state_centric_reading, hybrid_proportionality_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the geneva_conventions_protective_scope kernel. The state-centric reading, universal_rights_reading, and hybrid_proportionality_reading are structurally distinct constraints with different epsilon values, beneficiary/victim structures, and directionalities, linked by their shared kernel text but not reducible to a single constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
