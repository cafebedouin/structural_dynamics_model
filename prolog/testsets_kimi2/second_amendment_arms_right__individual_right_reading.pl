% ============================================================================
% CONSTRAINT STORY: second_amendment_arms_right__individual_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_arms_right__individual_right_reading, []).

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
 *   constraint_id: second_amendment_arms_right__individual_right_reading
 *   human_readable: Second Amendment Individual Right Reading
 *   domain: constitutional/law/political-philosophy
 *
 * SUMMARY:
 *   The Second Amendment to the United States Constitution is a contested
 *   kernel yielding multiple readings. This constraint instantiates the
 *   individual-right reading: the Amendment protects a pre-political,
 *   individual liberty to possess and carry arms, enforceable against federal
 *   (and via incorporation, state) infringement. Under this reading, the
 *   constraint operates as a hard limit on legislative and regulatory
 *   authority: federal firearms regulators and state prohibition regimes are
 *   the structural targets, while individual gun owners are the
 *   beneficiaries. The constraint is maintained by active judicial
 *   enforcement (Supreme Court decisions in Heller, McDonald, and Bruen) and
 *   by an interpretive tradition that treats the right as fixed by
 *   founding-era meaning. Gun control advocates who argue for prohibition or
 *   registration are structurally excluded from the reading's framework
 *   because their preferred policies are classified as unconstitutional
 *   infringements.
 *
 * KEY AGENTS:
 *   - individual_gun_owners: Primary beneficiary (organized/constrained) â receives immunity from federal and state prohibition.
 *   - federal_firearms_regulators: Primary target (institutional/constrained) â loses authority to prohibit or register firearms; bears the extraction of regulatory capacity.
 *   - state_firearms_regulators: Secondary target (institutional/constrained) â similarly constrained by incorporation and judicial review.
 *   - federal_judiciary: Agenda setter (institutional/constrained) â administers the constraint through constitutional interpretation and precedent; could change it but is bound by interpretive methodology.
 *   - gun_control_advocates: Excluded voice (organized/constrained) â would argue for disarmament or heavy regulation but are kept out by the reading's classification of such measures as unconstitutional.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_arms_right__individual_right_reading, 0.85).
domain_priors:suppression_score(second_amendment_arms_right__individual_right_reading, 0.75).
domain_priors:theater_ratio(second_amendment_arms_right__individual_right_reading, 0.53).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, theater_ratio, 0.53).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_arms_right__individual_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_arms_right__individual_right_reading, "Second Amendment Individual Right Reading").
narrative_ontology:topic_domain(second_amendment_arms_right__individual_right_reading, "constitutional/law/political-philosophy").

domain_priors:requires_active_enforcement(second_amendment_arms_right__individual_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_arms_right__individual_right_reading, 'd56ecf2c-3de3-4a3d-b6ac-e76cbee36cc1').
narrative_ontology:cs_kernel_codification('d56ecf2c-3de3-4a3d-b6ac-e76cbee36cc1', fixed_text).
narrative_ontology:cs_authority_grounding('d56ecf2c-3de3-4a3d-b6ac-e76cbee36cc1', lineage).
narrative_ontology:cs_interpretation_layer_present('d56ecf2c-3de3-4a3d-b6ac-e76cbee36cc1').
narrative_ontology:cs_reading_relation('d56ecf2c-3de3-4a3d-b6ac-e76cbee36cc1', second_amendment_arms_right__collective_right_reading, forecloses).
narrative_ontology:cs_reading_relation('d56ecf2c-3de3-4a3d-b6ac-e76cbee36cc1', second_amendment_arms_right__civic_republican_reading, coexists_with).
narrative_ontology:cs_axiom('d56ecf2c-3de3-4a3d-b6ac-e76cbee36cc1', foundational, right_pre_exists_government).
narrative_ontology:cs_axiom_status(right_pre_exists_government, holdable).
narrative_ontology:cs_axiom_grounding('d56ecf2c-3de3-4a3d-b6ac-e76cbee36cc1', right_pre_exists_government, deontological).
narrative_ontology:cs_axiom('d56ecf2c-3de3-4a3d-b6ac-e76cbee36cc1', foundational, shall_not_infringe_absolute).
narrative_ontology:cs_axiom_status(shall_not_infringe_absolute, holdable).
narrative_ontology:cs_axiom_grounding('d56ecf2c-3de3-4a3d-b6ac-e76cbee36cc1', shall_not_infringe_absolute, conventional).
narrative_ontology:cs_reference_frame('d56ecf2c-3de3-4a3d-b6ac-e76cbee36cc1', pre_constitutional_armed_citizenry).
narrative_ontology:cs_drift_state('d56ecf2c-3de3-4a3d-b6ac-e76cbee36cc1', contemporary_post_bruen, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('d56ecf2c-3de3-4a3d-b6ac-e76cbee36cc1', '').
narrative_ontology:cs_kernel_id(second_amendment_arms_right__individual_right_reading, second_amendment_arms_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__individual_right_reading, individual_gun_owners).
narrative_ontology:constraint_victim(second_amendment_arms_right__individual_right_reading, federal_firearms_regulators).
narrative_ontology:constraint_victim(second_amendment_arms_right__individual_right_reading, state_firearms_regulators).
narrative_ontology:constraint_vindicates(second_amendment_arms_right__individual_right_reading, natural_rights_doctrine).
narrative_ontology:constraint_vindicates(second_amendment_arms_right__individual_right_reading, original_public_meaning_jurisprudence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possess and acquire firearms under a constitutional immunity from federal and state prohibition. Benefit directly from judicial decisions that invalidate restrictive laws. While they can emigrate, exiting the U.S. constitutional order is costly, and their political identity is often fused with gun ownership.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, individual_gun_owners, beneficiary,
    organized, generational, constrained, national).

% Staff agencies such as the ATF and DOJ that draft and enforce federal firearms regulations. Under this reading, their authority to prohibit weapon categories or mandate registration is stripped by judicial review. They remain in institutional roles but operate within a shrinking permissible regulatory zone.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, federal_firearms_regulators, payer,
    institutional, biographical, constrained, national).

% State and local agencies that administer firearm permitting, registration, and prohibition regimes. Following incorporation and post-Bruen history-and-tradition review, their regulatory authority is increasingly invalidated by federal courts. They bear the cost of withdrawn state police power.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, state_firearms_regulators, payer,
    institutional, biographical, constrained, regional).

% Interprets and enforces the Second Amendment through constitutional review. Has formal authority to alter or abolish the reading via new precedent, but is constrained by originalist methodology, stare decisis, and institutional legitimacy costs. Administers the constraint by striking down statutes that infringe the individual right.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, federal_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Advocate for prohibitions, registration, and licensing regimes that this reading categorizes as unconstitutional. Their preferred policy instruments are structurally excluded from the legislative option set; they can pursue constitutional amendment or court reform, both of which are institutionally blocked in the near term.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, gun_control_advocates, excluded,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_arms_right__individual_right_reading, individual_gun_owners).
narrative_ontology:fixing_cost_class(second_amendment_arms_right__individual_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the relationship between armed citizens and the state by removing the threat of government disarmament, preserving a dispersed capacity for self-defense and a structural check on tyranny.
% TRANSFER_FUNCTION: Transfers regulatory authority over personal armament from federal and state legislatures and agencies to individual citizens, immunizing a category of private conduct from prohibition.
% ABSENT_VOICES: Gun control advocates and public-health researchers who frame firearm possession as a collective risk are structurally excluded; their preferred instruments (bans, registration, licensing) are defined as unconstitutional infringements, so they are not part of the policy conversation within this reading's framework.
% DISAPPEARANCE_RATIONALE: If the individual right reading vanished, federal and state legislatures would regain plenary authority to prohibit or severely restrict private firearm possession; existing prohibitions previously struck down would be reinstated; the political economy of gun regulation would reorganize around majoritarian safety claims rather than constitutional immunity.
% FOUNDING_PROBLEM: The risk of tyranny and the need for individual self-defense against threats including state overreach and private violence, which the founding generation addressed by codifying a permanent limitation on government disarmament of the populace.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians outside the immediate gun-owner beneficiary set (e.g., Joyce Lee Malcolm) attest an individual-right founding origin; historians aligned with competing readings (e.g., Saul Cornell, Carl Bogus) dispute this from outside the beneficiary bloc, leaving the corroboration split and contested.
narrative_ontology:disappearance_verdict(second_amendment_arms_right__individual_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_arms_right__individual_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_arms_right__individual_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(second_amendment_arms_right__individual_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_arms_right__individual_right_reading, 0.85, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_arms_right__individual_right_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_arms_right__individual_right_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_arms_right__individual_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is high (0.85 at interval end) because this reading strips significant regulatory authority from federal and state governments, converting what would otherwise be majoritarian policy choices into constitutionally forbidden zones. Suppression is high (0.75) because the constraint's persistence depends on active judicial suppression of legislative and regulatory alternatives (bans, registration schemes). Theater_ratio is moderate-high (0.53) and rising because the history-and-tradition methodology increasingly functions as a performance of originalist fidelity that ratifies modern preferences. Accessibility_collapse is high (0.72): once the individual right framework is accepted, the policy alternative set for regulators collapses toward prohibition. Resistance is moderate (0.60): gun control advocates and some states mount active resistance through litigation and sanctuary doctrines, but the constitutional framework channels this resistance into narrow doctrinal channels.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (federal and state regulators) and the beneficiary seat (individual gun owners) should compute to very different types. From the gun owner's position, the constraint is a protective liberty that limits state overreach (rope-like coordination). From the regulator's position, the same structure is an enforced extraction of democratic police power (snare-like extraction). The federal judiciary, as agenda setter, occupies a third seat: it experiences the constraint as both a source of institutional power (the ability to invalidate statutes) and a bind (methodological and legitimacy constraints on how it may rule). The engine computes this divergence from the structural data rather than the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual gun owners are declared beneficiaries: they receive constitutional immunity, their directionality sits near the beneficiary end, and their effective extraction is damped or inverted into subsidy. Federal and state regulators are declared victims (payers): they bear the cost of lost regulatory authority, their directionality sits near the target end, and their effective extraction is amplified. The judiciary is not a beneficiary or victim in the receipt sense; its directionality is derived from its constrained exit and agenda-setting role, placing it between the two poles but closer to the beneficiary end because it wields the constraint's enforcement power.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint prevents mislabeling by exhibiting both a genuine coordination function (preventing tyranny, preserving self-defense, solving the commitment problem of a disarmed populace) and asymmetric extraction (government actors lose regulatory capacity that they would otherwise exercise). If the coordination function were absent, it would be a snare: pure extraction of state power for private benefit. If the extraction were absent, it would be a rope: a neutral limit on government with no concentrated beneficiaries. The presence of both, sustained by active judicial enforcement, makes tangled rope the structurally accurate classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_reading_individuation,
    'Is the individual right reading a discovery of a fixed kernel meaning, or a constructive doctrinal innovation that retroactively stabilizes a political preference?',
    'Archival and linguistic analysis of the ratification-era text against the doctrinal apparatus required to sustain the modern individual-right regime; if the reading requires substantial auxiliary hypotheses not traceable to the fixed text, it is constructive.',
    'If constructive, the constraint is a tangled rope or snare (depending on beneficiary structure) rather than a mountain of original meaning; this reading''s epsilon is sensitive to political support rather than textual necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_reading_individuation, conceptual, 'Whether the reading discovers or constructs the kernel meaning').

omega_variable(
    natural_right_vs_positive_law,
    'Does the right to keep and bear arms pre-exist government as a natural liberty, or is it entirely a positive-law construct maintained by judicial enforcement?',
    'Cross-jurisdictional comparison: if robust individual armament persists in jurisdictions without a constitutional guarantee, the right has natural-law characteristics; if not, it is enforcement-dependent.',
    'If natural, the constraint gains mountain-like persistence; if positive, it remains a tangled rope whose stability depends on continued judicial enforcement and political alignment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_right_vs_positive_law, conceptual, 'Natural law versus positive law status of the right').

omega_variable(
    history_and_tradition_test_validity,
    'Does the history-and-tradition methodology used to enforce this reading genuinely recover founding-era meaning, or does it function as a theater mechanism that ratifies modern preferences?',
    'Independent historical audit of the sources cited in major opinions (Heller, Bruen) against the conclusions drawn; measure the rate of methodological objections from historians without stake in the policy outcome.',
    'If theater dominates, the theater_ratio metric understates the performative component and the constraint''s coordination function is weaker than claimed; this would shift computed classification toward snare or piton.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(history_and_tradition_test_validity, empirical, 'Empirical validity of the interpretive methodology').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_arms_right__individual_right_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_arms_right__individual_right_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(seco_tr_t5, second_amendment_arms_right__individual_right_reading, theater_ratio, 5, 0.38).
narrative_ontology:measurement(seco_tr_t10, second_amendment_arms_right__individual_right_reading, theater_ratio, 10, 0.42).
narrative_ontology:measurement(seco_tr_t15, second_amendment_arms_right__individual_right_reading, theater_ratio, 15, 0.46).
narrative_ontology:measurement(seco_tr_t20, second_amendment_arms_right__individual_right_reading, theater_ratio, 20, 0.5).
narrative_ontology:measurement(seco_tr_t25, second_amendment_arms_right__individual_right_reading, theater_ratio, 25, 0.53).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_arms_right__individual_right_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(seco_be_t5, second_amendment_arms_right__individual_right_reading, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(seco_be_t10, second_amendment_arms_right__individual_right_reading, base_extractiveness, 10, 0.7).
narrative_ontology:measurement(seco_be_t15, second_amendment_arms_right__individual_right_reading, base_extractiveness, 15, 0.78).
narrative_ontology:measurement(seco_be_t20, second_amendment_arms_right__individual_right_reading, base_extractiveness, 20, 0.82).
narrative_ontology:measurement(seco_be_t25, second_amendment_arms_right__individual_right_reading, base_extractiveness, 25, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_arms_right__individual_right_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(seco_su_t5, second_amendment_arms_right__individual_right_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(seco_su_t10, second_amendment_arms_right__individual_right_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(seco_su_t15, second_amendment_arms_right__individual_right_reading, suppression_requirement, 15, 0.74).
narrative_ontology:measurement(seco_su_t20, second_amendment_arms_right__individual_right_reading, suppression_requirement, 20, 0.79).
narrative_ontology:measurement(seco_su_t25, second_amendment_arms_right__individual_right_reading, suppression_requirement, 25, 0.83).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_arms_right__individual_right_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_arms_right__individual_right_reading, second_amendment_arms_right__collective_right_reading).
narrative_ontology:affects_constraint(second_amendment_arms_right__individual_right_reading, second_amendment_arms_right__civic_republican_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the second_amendment_arms_right kernel. The individual right reading and its siblings (collective, civic republican) are structurally distinct constraints with different epsilon values, beneficiary/victim structures, and classifications. They are linked as a constraint family per the epsilon-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
