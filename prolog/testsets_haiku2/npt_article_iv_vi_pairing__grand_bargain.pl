% ============================================================================
% CONSTRAINT STORY: npt_article_iv_vi_pairing__grand_bargain
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_article_iv_vi_pairing__grand_bargain, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: npt_article_iv_vi_pairing__grand_bargain
 *   human_readable: NPT Article IV–VI Reciprocity: Grand Bargain Reading
 *   domain: international_law/nuclear_governance
 *
 * SUMMARY:
 *   The NPT Article IV–VI pairing is the subject of three competing kernel
 *   readings that distribute power and obligation asymmetrically across the
 *   nuclear-armed (NWS) and non-armed (NNWS) states. The grand bargain
 *   reading holds that Articles IV (peaceful nuclear technology transfer) and
 *   VI (disarmament commitment) are reciprocal obligations—NNWS restraint is
 *   conditional on NWS disarmament progress, and breach of Article VI
 *   undermines the legitimacy of Article IV enforcement. This reading emerged
 *   explicitly in the 1995 NPT Review and Extension Conference and has been
 *   sustained through successive NNWS advocacy, particularly from the
 *   Non-Aligned Movement, humanitarian organizations, and states frustrated
 *   with the stalled nuclear disarmament agenda. Under this reading, weapon
 *   states become treaty-breach actors if they fail to disarm; verification
 *   of disarmament must be reciprocal (symmetric inspection obligations); and
 *   NNWS gain standing to challenge Article IV compliance or threaten
 *   withdrawal if NWS violate Article VI. The abolitionist reading (not
 *   authored here) goes further, treating Article IV as illegitimate if it
 *   perpetuates dual-use proliferation; the nonproliferation_primary reading
 *   (not authored here) treats Article VI as aspirational and
 *   non-justiciable, grounding NNWS restraint independently in security
 *   interest, not reciprocity. This story instantiates the grand bargain
 *   reading as a single ε-invariant constraint: its ε derives from the
 *   standing arrangement under contest (NNWS restraint conditioned on NWS
 *   disarmament progress), assessed by the grand bargain reading's own
 *   lights. The sibling readings are OTHER constraints (other files) linked
 *   via network.affects_constraints; they are not part of this story.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_article_iv_vi_pairing__grand_bargain, 0.67).
domain_priors:suppression_score(npt_article_iv_vi_pairing__grand_bargain, 0.61).
domain_priors:theater_ratio(npt_article_iv_vi_pairing__grand_bargain, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, extractiveness, 0.67).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_article_iv_vi_pairing__grand_bargain, tangled_rope).
narrative_ontology:human_readable(npt_article_iv_vi_pairing__grand_bargain, "NPT Article IV–VI Reciprocity: Grand Bargain Reading").
narrative_ontology:topic_domain(npt_article_iv_vi_pairing__grand_bargain, "international_law/nuclear_governance").

domain_priors:requires_active_enforcement(npt_article_iv_vi_pairing__grand_bargain).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_article_iv_vi_pairing__grand_bargain, 'd90b3052-fce6-4b68-a0b5-88867bf66c58').
narrative_ontology:cs_kernel_codification('d90b3052-fce6-4b68-a0b5-88867bf66c58', formalized).
narrative_ontology:cs_authority_grounding('d90b3052-fce6-4b68-a0b5-88867bf66c58', lineage).
narrative_ontology:cs_interpretation_layer_present('d90b3052-fce6-4b68-a0b5-88867bf66c58').
narrative_ontology:cs_reading_relation('d90b3052-fce6-4b68-a0b5-88867bf66c58', npt_article_iv_vi_pairing__nonproliferation_primary, coexists_with).
narrative_ontology:cs_reading_relation('d90b3052-fce6-4b68-a0b5-88867bf66c58', npt_article_iv_vi_pairing__abolitionist, coexists_with).
narrative_ontology:cs_axiom('d90b3052-fce6-4b68-a0b5-88867bf66c58', foundational, article_vi_justiciable_disarmament).
narrative_ontology:cs_axiom_status(article_vi_justiciable_disarmament, holdable).
narrative_ontology:cs_axiom_grounding('d90b3052-fce6-4b68-a0b5-88867bf66c58', article_vi_justiciable_disarmament, deontological).
narrative_ontology:cs_axiom('d90b3052-fce6-4b68-a0b5-88867bf66c58', foundational, nnws_restraint_conditional_on_nws_progress).
narrative_ontology:cs_axiom_status(nnws_restraint_conditional_on_nws_progress, holdable).
narrative_ontology:cs_axiom_grounding('d90b3052-fce6-4b68-a0b5-88867bf66c58', nnws_restraint_conditional_on_nws_progress, conventional).
narrative_ontology:cs_axiom('d90b3052-fce6-4b68-a0b5-88867bf66c58', secondary, reciprocal_verification_required).
narrative_ontology:cs_axiom_status(reciprocal_verification_required, holdable).
narrative_ontology:cs_axiom_grounding('d90b3052-fce6-4b68-a0b5-88867bf66c58', reciprocal_verification_required, empirically_contingent).
narrative_ontology:cs_reference_frame('d90b3052-fce6-4b68-a0b5-88867bf66c58', conditional_reciprocal_obligations).
narrative_ontology:cs_drift_state('d90b3052-fce6-4b68-a0b5-88867bf66c58', contemporary_institutional_suppression, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d90b3052-fce6-4b68-a0b5-88867bf66c58', '2026-06-12T14:32:15Z').
narrative_ontology:cs_kernel_id(npt_article_iv_vi_pairing__grand_bargain, npt_article_iv_vi_pairing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__grand_bargain, nnws_coalition).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__grand_bargain, nws_security_establishment).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__grand_bargain, nnws_technological_aspirants).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__grand_bargain, nws_disarmament_constituencies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__grand_bargain, weapons_laboratories).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__grand_bargain, nnws_coalition).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__grand_bargain, nws_security_establishment).
narrative_ontology:constraint_vindicates(npt_article_iv_vi_pairing__grand_bargain, treaty_reciprocity_doctrine).
narrative_ontology:constraint_vindicates(npt_article_iv_vi_pairing__grand_bargain, conditional_restraint_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Non-weapon states collectively benefit from a reading that makes Article VI disarmament enforceable: it grounds their restraint on a conditional quid-pro-quo and licenses withdrawal or horizontal expansion if weapon states breach. They bear the cost of Article IV compliance (inspections, technology restrictions, non-acquisition commitments). Under this reading, their restraint becomes leverage rather than unilateral subordination. Their exit options are constrained because leaving the treaty broadcasts proliferation intent, but the threat of withdrawal or Article IV reinterpretation becomes a negotiating position.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, nnws_coalition, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(npt_article_iv_vi_pairing__grand_bargain, nnws_coalition, payer).

% Weapon states set the treaty's operational interpretation through the PrepCom and Review Conference processes. They face a structural bind under this reading: Article VI disarmament becomes justiciable and creates a standing obligation they cannot fulfill (zero weapons is incompatible with deterrence doctrine). Their restraint obligations (on horizontal proliferation, technology transfer, assistance) are symmetric with NNWS obligations. Breach of Article VI undermines Article IV legitimacy and exposes them to withdrawal threats, remedies, or horizontal expansion by NNWS. Exit from the treaty is institutionally trapped (deterrence commitment, security alliance structures).
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, nws_security_establishment, agenda_setter,
    institutional, civilizational, trapped, global).
narrative_ontology:stakeholder_secondary_role(npt_article_iv_vi_pairing__grand_bargain, nws_security_establishment, payer).

% States seeking nuclear capacity (Iran, past Brazil, past South Africa) bear the enforcement costs of Article IV most directly: inspections, technology embargoes, fuel-bank restrictions, export controls on dual-use goods. Under the grand bargain reading, they are formally justified to challenge Article IV's legitimacy if weapon states do not disarm, but the identity-lock is institutional (security doctrine, regime stability tied to non-proliferation compliance). Their exit to proliferation carries regime-delegitimization and sanctions costs that dwarf the benefit of challenging the treaty's reciprocity. The grand bargain reading offers a rhetorical path (conditional restraint) without lowering the actual enforcement cost.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, nnws_technological_aspirants, payer,
    moderate, biographical, identity_locked, national).

% Domestic constituencies (peace movements, humanitarian organizations, Global South governments) that demand weapon states honor Article VI disarmament. Under the grand bargain reading, their pressure gains formal standing: Article VI becomes justiciable and breach-able. They are payers because weapon states use treaty obligations (vague, non-enforceable) as substitutes for actual disarmament, and the burden of that substitution falls on societies that call for it. Their constrained exit is political: pushing too hard for Article VI enforcement risks being labeled naive or destabilizing within the security policy community.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, nws_disarmament_constituencies, payer,
    moderate, biographical, constrained, national).

% Implements Article IV inspections and safeguards. Under the grand bargain reading, verification becomes reciprocal: if NNWS submit to inspections, weapon states must submit to reciprocal verification of disarmament progress. This expands IAEA mandate from NNWS monitoring to NWS weapons accounting, which exceeds its institutional capacity and authority. The regime is agenda-setter (controls inspection protocols, defines compliance), but also trapped (cannot exit the treaty system without losing its institutional rationale).
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, iaea_verification_regime, agenda_setter,
    institutional, generational, trapped, global).

% Advocates for reading Article IV–VI as reciprocal obligations and demands enforcement of Article VI. They would object more forcefully if present in the treaty's formal governance, but their voice is mediated through PrepCom statements and General Assembly positions—not through binding treaty mechanisms. Their absence from the binding verification and enforcement architecture is structural, not accidental.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, non_aligned_movement, excluded,
    organized, generational, constrained, global).

% Benefit from the nonproliferation_primary reading (Article VI is aspirational and non-justiciable) which legitimates their continued operation and research. Under the grand bargain reading, their institutional mandate is exposed as incompatible with Article VI disarmament, creating pressure for remediation, repurposing, or downsizing. They are trapped because national security doctrine depends on their continuity.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, weapons_laboratories, beneficiary,
    institutional, civilizational, trapped, national).

% Academic commentators, international legal scholars, and NGO analysts who produce competing readings of the treaty's text and history. They document the structural ambiguity and trace how different readings distribute benefits and costs across the stakeholder set. They are observers in the formal governance but shape the legitimacy narratives that other seats use.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, treaty_interpretation_community, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_article_iv_vi_pairing__grand_bargain, nws_security_establishment).
narrative_ontology:fixing_cost_class(npt_article_iv_vi_pairing__grand_bargain, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves two coordination problems simultaneously: (1) NNWS restrain horizontal proliferation (reduces NWS security burden); (2) NWS commit to disarmament trajectory (legitimates NNWS restraint). The reciprocal reading frames both as enforceable conditions of each other rather than as independent commitments.
% TRANSFER_FUNCTION: Moves technological restraint and verification access from NNWS to NWS (via inspection and non-acquisition commitments); moves disarmament progress and verification reciprocity from NWS to NNWS (via Article VI). Under the grand bargain reading, both flows are conditional on the other—a swap rather than a unilateral transfer.
% ABSENT_VOICES: Armed non-state actors and non-signatories (India, Pakistan, Israel, North Korea) would object to being governed by a treaty they did not join, but they are structurally excluded from the treaty's decision-making bodies. Non-aligned movement and humanitarian organizations advocate for the grand bargain reading but lack binding standing in Review Conferences.
% DISAPPEARANCE_RATIONALE: If the reciprocal reading (and its enforcement mechanism) disappeared, the treaty would revert to the nonproliferation_primary reading: Article VI would be treated as aspirational and non-justiciable, NNWS restraint would be unconditional, and the structural justification for NNWS compliance would erode. Horizontally, NNWS would face renewed proliferation pressure (Iran, Saudi Arabia, Egypt); vertically, NWS laboratories would continue unrestricted modernization. The legitimacy foundation of the entire non-proliferation regime would shift.
% FOUNDING_PROBLEM: The NPT was negotiated (1968) as a bargain: NNWS forgo nuclear weapons in exchange for NWS commitment to disarm and share peaceful nuclear technology. By the 1970s, the disarmament obligation was reframed as aspirational; by the 1990s, the grand bargain reading—treating Article IV and VI as reciprocal and enforceable—emerged as the primary challenge to nonproliferation_primary interpretation.
% FOUNDING_PROBLEM_CORROBORATION: NWS (esp. US, Russia, UK) assert the founding problem is obsolete: disarmament remains aspirational; NNWS restraint is justified independently by security benefits. NNWS (esp. NAM, AELC, and Iran) assert the founding problem is live: the grand bargain was central to the treaty's negotiating history; NWS breach of Article VI licenses NNWS withdrawal or expansion. Independent corroboration from UNIDIR, SIPRI, and the International Court of Justice's advisory opinions recognizes the reciprocity argument as a sustained legitimate challenge to the established reading, not settled consensus.
narrative_ontology:disappearance_verdict(npt_article_iv_vi_pairing__grand_bargain, world_rearranges).
narrative_ontology:founding_problem_status(npt_article_iv_vi_pairing__grand_bargain, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_article_iv_vi_pairing__grand_bargain, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(npt_article_iv_vi_pairing__grand_bargain, 'none', 1).
narrative_ontology:epsilon_provenance(npt_article_iv_vi_pairing__grand_bargain, 0.67, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_article_iv_vi_pairing__grand_bargain_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_article_iv_vi_pairing__grand_bargain, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_article_iv_vi_pairing__grand_bargain_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness at interval end is 0.67: moderate-high. The constraint extracts NNWS restraint (technology forgoing, inspections, non-acquisition commitments) without delivering proportional NWS disarmament. However, the grand bargain reading reframes this asymmetry as conditional—if NWS breach Article VI, NNWS restraint is no longer justiciable, and they gain standing to withdraw or reinterpret Article IV. This conditionality lowers the baseline extraction relative to nonproliferation_primary (where NNWS restraint is unconditional) but does not eliminate it, because the practical ability to enforce the condition is limited (NWS control treaty governance, Review Conference agenda). Suppression at interval end is 0.61: moderate. The constraint's persistence depends on active suppression of the grand bargain reading's institutional standing—NWS control of the PrepCom agenda, exclusion of NAM from binding decision-making, and framing disarmament as 'aspirational' rather than justiciable. The measurement trajectory shows suppression increasing from 0.44 to 0.61 over the interval, reflecting the intensifying contestation and the need for stronger enforcement as the grand bargain reading gains rhetorical weight (PrepCom 2022, 2024 statements from NAM and AELC delegations). Theater at interval end is 0.52: moderate, near the theoretical threshold where performative activity begins to eclipse function. NWS conduct disarmament talks (START, New START, FMCT negotiations) that produce little practical warhead reduction; NNWS conduct ritual compliance (inspections, technology restrictions) that yields few security benefits; Review Conferences produce consensus documents that reaffirm the treaty without resolving the Article IV–VI reciprocity dispute. The rising theater trajectory (0.38 to 0.52 to projected 0.54, then slight decline as crisis pressure rises post-2030) suggests the constraint is increasingly maintained through ceremonial reaffirmation rather than structural problem-solving. The measurements share one time grid: every metric is authored at every time point (t ∈ {0, 5, 10, 15, 20, 25, 30, 35}). Early points (0–20) are observed; late points (25–35) are projected under an assumption of continued rivalry and stalled disarmament.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (NWS security establishment, IAEA) and the organized payer seats (NNWS coalition, NAM) should compute dramatically different constraint types. From the NWS seat, the grand bargain reading is an existential threat: Article VI becomes justiciable, laboratories face remediation pressure, deterrence doctrine loses treaty legitimacy. The NWS seat experiences this as a snare (extraction of security reassurance without reciprocal disarmament benefit, plus active enforcement to suppress the grand bargain interpretation). From the NNWS organized-coalition seat, the grand bargain reading is a negotiating leverage point: it grounds their restraint as conditional and licenses withdrawal or Article IV challenges if NWS breach. The NNWS seat experiences this as rope (genuine coordination problem solved—how do NNWS justify restraint without NWS reciprocity; the answer is they don't, and can demand remediation). From the NNWS technological-aspirant seat (Iran, past Brazil), the reading is identity-locked extraction: they formally could invoke reciprocity to challenge Article IV, but doing so delegitimizes their security doctrine and invites sanctions. The engine derives these divergences from power, time_horizon, exit_options, and the beneficiary/victim declarations. The authored claim (tangled_rope) reflects the structural asymmetry: there is genuine coordination (NNWS restraint, NWS disarmament progress) AND asymmetric extraction (NNWS pays in restraint, NWS pays minimally in disarmament). Active enforcement is required to hold the arrangement because the coordination function is incomplete (disarmament has not happened, creating persistent legitimacy pressure) and the extraction ratio is high (NNWS restraint is unconditional in practice, despite the grand bargain reading's conditional framing).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: (1) NNWS coalition—gains formal standing to condition restraint on NWS disarmament, shifting their position from unconditional obligation to conditional leverage. (2) NWS security establishment—benefits from the treaty structure itself (prevents horizontal proliferation) even as this particular reading threatens their interpretation monopoly. Victims: (1) NNWS technological aspirants—bear the enforcement costs of Article IV most heavily (inspections, embargoes, dual-use restrictions) and gain rhetorical but not practical escape routes. (2) NWS disarmament constituencies—domestic pressure groups demanding Article VI enforcement face institutional suppression and lack binding standing. Directionality for NWS institutional seat: d near 1.0 (target of NNWS withdrawal threats, Article IV reinterpretation pressure, reciprocal verification demands). Directionality for NNWS organized seat: d near 0.5 (symmetric: benefits from formal conditioning of restraint, but pays in continued Article IV compliance and identity-lock costs from aspiring states). Directionality for NNWS technological aspirants: d near 0.8 (target: no practical exit without proliferation costs; identity-locked by regime legitimacy; enforcement is active and asymmetric). Directionality for NWS disarmament constituencies: d near 0.9 (target: gain formal standing to demand Article VI but lack institutional power to enforce; constrained exit is political).
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy—the founding obligation outliving its function—is central to the grand bargain reading's contestation. The founding problem was the original bargain: NNWS restraint CONDITIONED on NWS disarmament. The grand bargain reading holds that this founding problem is LIVE and CONTESTED: weapon states have NOT disarmed, so NNWS restraint is no longer justiciable and the arrangement is becoming a pure snare. The nonproliferation_primary reading holds that the founding problem is DEAD or OBSOLETE: the founding problem was preventing horizontal proliferation (solved); Article VI was always aspirational. The abolitionist reading holds that the founding problem is MISFRAMED: the true founding problem was weapons abolition, which NNWS and NWS both failed, and Article IV is illegitimate as a consequence. Classification under the grand bargain reading: if the founding problem (reciprocal, conditional obligation) is LIVE, then the constraint's classification hinges on whether the conditioning is enforceable. If enforceable, it is tangled_rope (coordination + asymmetric enforcement). If unenforceable (NWS monopoly on treaty governance prevents reciprocal verification), it degradates toward snare (extraction holding because exit is costly). The measurement series suggests this degradation is underway: theater rising, suppression rising, and NNWS access to binding governance stable or declining. The (founding_problem_status=contested × disappearance_verdict=world_rearranges) mismatch confirms: the constraint is held in place by active institutional suppression of the reciprocity claim, not by structural necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reciprocal_verification_feasibility,
    'Is symmetric, reciprocal verification of NWS disarmament progress technically and institutionally feasible within the existing IAEA framework, or would it require a new international verification regime with authority over NWS?',
    'IAEA technical analysis of NWS warhead monitoring requirements; study of past verification arrangements (START, CTBT, JCPOA) to assess transferability; negotiation of binding verification protocols at Review Conference.',
    'If reciprocal verification is technically feasible but institutionally blocked, the grand bargain reading is exposed as a normative claim without practical mechanism—extraction remains asymmetric because enforcement is structurally impossible. If feasible and adopted, NNWS restraint becomes genuinely conditional and the constraint reclassifies toward rope. If infeasible, the grand bargain reading retreats to rhetorical positioning and the underlying structure reverts to snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reciprocal_verification_feasibility, empirical, 'Whether reciprocal disarmament verification is institutionally feasible.').

omega_variable(
    article_vi_justiciability,
    'Does Article VI as written (with text ''cessation of the nuclear arms race at an early date and to nuclear disarmament'') create a binding legal obligation on NWS, or is it aspirational and hortatory?',
    'International Court of Justice advisory opinion (as in the 1996 nuclear weapons opinion); binding arbitration in a treaty dispute; state practice evidence from NWS treaty interpretation and domestic legal proceedings.',
    'If Article VI is justiciable, the grand bargain reading gains structural force—NWS breach is enforceable, NNWS have standing to withdraw. If aspirational, the reading is purely rhetorical positioning and the constraint remains snare. The foundational-axiom status (justiciable vs. aspirational) is precisely the axis separating grand_bargain from nonproliferation_primary reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_vi_justiciability, conceptual, 'Whether Article VI is a binding obligation or aspirational commitment.').

omega_variable(
    identity_lock_proliferation_pressure,
    'If NNWS invoke Article VI reciprocity to challenge Article IV, what portion of their actual restraint is driven by genuine security preference versus identity-lock institutional dependence?',
    'Counterfactual: follow NNWS disarmament constituencies'' demands and NNWS civil society responses if Article VI enforcement appeared imminent. Survey evidence from diplomatic interviews. Natural experiment: observe states that have already invoked reciprocity (Iran''s statements 2022–2024) and trace whether their actual proliferation decisions follow or diverge.',
    'High identity-lock (restraint is institutional, not chosen) means the grand bargain reading offers rhetorical leverage without functional escape—NNWS cannot exit to proliferation even if they claim reciprocity justifies it. This deepens the extraction asymmetry and suggests the constraint operates as snare regardless of the formal reading. Low identity-lock means NNWS restraint is genuinely conditional and could shift on disarmament failure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_proliferation_pressure, empirical, 'Degree of identity-lock in NNWS nuclear-restraint commitment.').

omega_variable(
    nws_disarmament_constraint_vs_structural_impossibility,
    'Is Article VI disarmament a true constraint on NWS behavior (something they could do but choose not to do), or is it structurally incompatible with NWS deterrence doctrine and great-power rivalry?',
    'Historical counterfactual analysis: what NWS incentives changed between the Cold War (when disarmament seemed theoretically possible) and post-1990 (when it stalled)? Game-theoretic analysis of security-dilemma drivers for NWS arsenals. Interviews with NWS strategic planners on the tradeoffs between disarmament and deterrence.',
    'If disarmament is structurally impossible given current security architecture, Article VI is a false obligation—not an extractive constraint on NWS but a constraint on NNWS belief systems (making them think reciprocity is possible). The grand bargain reading becomes theatrical cover for a snare. If disarmament is genuinely possible (political choice, not structural necessity), then NWS breach is real, and the grand bargain reading has structural standing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(nws_disarmament_constraint_vs_structural_impossibility, conceptual, 'Whether NWS disarmament is structurally possible or incompatible with deterrence doctrine.').

omega_variable(
    kernel_committer_structure,
    'Which reading of the Article IV–VI pairing—nonproliferation_primary, grand_bargain, or abolitionist—most closely matches the treaty''s actual negotiating history and text?',
    'Textual analysis of the NPT preamble and Articles IV, VI, the 1968 negotiating record, and successive Review Conference documents (1975–2023). Genealogical evidence from drafting state testimony (especially NNWS delegations of India, Mexico, Nigeria, Sweden).',
    'If the grand bargain reading matches the original negotiating intent, it has greater structural legitimacy as the ''true'' interpretation and reclassifies toward rope (shared coordinating function). If nonproliferation_primary matches, the grand bargain reading is a post-hoc reinterpretation and remains snare (extraction holding through institutional suppression of the counter-reading). This determines whether the engine can compute the divergence as due to genuine structural difference versus reading-imposed divergence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_committer_structure, empirical, 'Whether the grand bargain reading reflects the treaty''s original negotiating intent or is a post-hoc reinterpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_article_iv_vi_pairing__grand_bargain, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t0, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 0, 0.38).
narrative_ontology:measurement_basis(npt__tr_t0, observed).
narrative_ontology:measurement(npt__tr_t5, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 5, 0.42).
narrative_ontology:measurement_basis(npt__tr_t5, observed).
narrative_ontology:measurement(npt__tr_t10, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 10, 0.46).
narrative_ontology:measurement_basis(npt__tr_t10, observed).
narrative_ontology:measurement(npt__tr_t15, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 15, 0.5).
narrative_ontology:measurement_basis(npt__tr_t15, observed).
narrative_ontology:measurement(npt__tr_t20, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 20, 0.52).
narrative_ontology:measurement_basis(npt__tr_t20, observed).
narrative_ontology:measurement(npt__tr_t25, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 25, 0.53).
narrative_ontology:measurement_basis(npt__tr_t25, projected).
narrative_ontology:measurement(npt__tr_t30, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 30, 0.54).
narrative_ontology:measurement_basis(npt__tr_t30, projected).
narrative_ontology:measurement(npt__tr_t35, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 35, 0.52).
narrative_ontology:measurement_basis(npt__tr_t35, projected).

% Extraction over time
narrative_ontology:measurement(npt__be_t0, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(npt__be_t0, observed).
narrative_ontology:measurement(npt__be_t5, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(npt__be_t5, observed).
narrative_ontology:measurement(npt__be_t10, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 10, 0.58).
narrative_ontology:measurement_basis(npt__be_t10, observed).
narrative_ontology:measurement(npt__be_t15, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 15, 0.62).
narrative_ontology:measurement_basis(npt__be_t15, observed).
narrative_ontology:measurement(npt__be_t20, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 20, 0.65).
narrative_ontology:measurement_basis(npt__be_t20, observed).
narrative_ontology:measurement(npt__be_t25, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 25, 0.67).
narrative_ontology:measurement_basis(npt__be_t25, projected).
narrative_ontology:measurement(npt__be_t30, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 30, 0.69).
narrative_ontology:measurement_basis(npt__be_t30, projected).
narrative_ontology:measurement(npt__be_t35, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 35, 0.67).
narrative_ontology:measurement_basis(npt__be_t35, projected).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t0, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 0, 0.44).
narrative_ontology:measurement_basis(npt__su_t0, observed).
narrative_ontology:measurement(npt__su_t5, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 5, 0.49).
narrative_ontology:measurement_basis(npt__su_t5, observed).
narrative_ontology:measurement(npt__su_t10, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 10, 0.54).
narrative_ontology:measurement_basis(npt__su_t10, observed).
narrative_ontology:measurement(npt__su_t15, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 15, 0.59).
narrative_ontology:measurement_basis(npt__su_t15, observed).
narrative_ontology:measurement(npt__su_t20, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 20, 0.61).
narrative_ontology:measurement_basis(npt__su_t20, observed).
narrative_ontology:measurement(npt__su_t25, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 25, 0.62).
narrative_ontology:measurement_basis(npt__su_t25, projected).
narrative_ontology:measurement(npt__su_t30, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 30, 0.63).
narrative_ontology:measurement_basis(npt__su_t30, projected).
narrative_ontology:measurement(npt__su_t35, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 35, 0.61).
narrative_ontology:measurement_basis(npt__su_t35, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_article_iv_vi_pairing__grand_bargain, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(npt_article_iv_vi_pairing__grand_bargain, 0.12).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__grand_bargain, npt_article_iv_vi_pairing__nonproliferation_primary).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__grand_bargain, npt_article_iv_vi_pairing__abolitionist).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__grand_bargain, npt_review_conference_legitimacy).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__grand_bargain, fmct_negotiation_stall).

% DUAL FORMULATION NOTE:
% The NPT Article IV–VI pairing is the kernel of three structurally distinct constraint stories: nonproliferation_primary (Article VI aspirational, NNWS restraint unconditional), grand_bargain (Article IV–VI reciprocal, NNWS restraint conditional), and abolitionist (Article IV illegitimate, Article VI mandates total disarmament). Each story has its own ε, beneficiary/victim structure, and claimed type. ε differs by orders of magnitude: nonproliferation_primary ε ≈ 0.35 (low extraction: NNWS restraint justified by security benefit); grand_bargain ε ≈ 0.67 (moderate-high: NNWS restraint largely unconditional despite formal conditioning); abolitionist ε ≈ 0.85 (high: NWS maintain arsenals while framing NNWS restraint as normative duty). The ε-invariance principle requires three separate constraint stories because measuring the constraint three different ways yields three different extractiveness values—a signal of structural difference, not observational relativity. The stories are linked via this network block to enable cross-story drift analysis and contamination propagation (if nonproliferation_primary reclassifies due to institutional change, what happens to grand_bargain's classification; how do the readings influence each other's persistence).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(npt_article_iv_vi_pairing__grand_bargain, institutional, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
