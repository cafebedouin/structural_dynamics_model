% ============================================================================
% CONSTRAINT STORY: border_normative_status__qualified_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_normative_status__qualified_sovereignty, []).

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
 *   constraint_id: border_normative_status__qualified_sovereignty
 *   human_readable: Qualified Sovereignty: Border Control Bounded by Proportionality and Rights
 *   domain: political_philosophy/international_law/migration
 *
 * SUMMARY:
 *   This story instantiates the qualified-sovereignty reading of the border
 *   normative status kernel: states retain the authority to control
 *   admission, but that authority is not unconditional — it must be exercised
 *   in pursuit of a legitimate interest, must be necessary to achieve that
 *   interest, and must be proportionate, all assessed against binding human
 *   rights obligations including non-refoulement. This is a genuinely hybrid
 *   arrangement: it coordinates a real problem (reconciling collective
 *   self-determination with the post-war human rights settlement) while
 *   producing asymmetric extraction (the adjudication burden and the costs of
 *   contestation fall overwhelmingly on excluded migrants and displaced
 *   citizens, not on the states whose discretion the framework nominally
 *   constrains). The rights floor is real — it does more than the pure
 *   sovereignty reading would permit — but it is thin enough, and
 *   self-policed enough by the very states it constrains, that its
 *   coordination function and its extractive residue coexist in the same
 *   structure.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_normative_status__qualified_sovereignty, 0.52).
domain_priors:suppression_score(border_normative_status__qualified_sovereignty, 0.58).
domain_priors:theater_ratio(border_normative_status__qualified_sovereignty, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, extractiveness, 0.52).
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_normative_status__qualified_sovereignty, tangled_rope).
narrative_ontology:human_readable(border_normative_status__qualified_sovereignty, "Qualified Sovereignty: Border Control Bounded by Proportionality and Rights").
narrative_ontology:topic_domain(border_normative_status__qualified_sovereignty, "political_philosophy/international_law/migration").

domain_priors:requires_active_enforcement(border_normative_status__qualified_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_normative_status__qualified_sovereignty, '19eec8f2-f7e0-446a-bbb4-6174e4f7ba59').
narrative_ontology:cs_kernel_codification('19eec8f2-f7e0-446a-bbb4-6174e4f7ba59', distributed).
narrative_ontology:cs_authority_grounding('19eec8f2-f7e0-446a-bbb4-6174e4f7ba59', distributed).
narrative_ontology:cs_reading_relation('19eec8f2-f7e0-446a-bbb4-6174e4f7ba59', border_normative_status__sovereignty_primary, influences).
narrative_ontology:cs_reading_relation('19eec8f2-f7e0-446a-bbb4-6174e4f7ba59', border_normative_status__freedom_primary, influences).
narrative_ontology:cs_axiom('19eec8f2-f7e0-446a-bbb4-6174e4f7ba59', foundational, sovereignty_conditioned_on_justification).
narrative_ontology:cs_axiom_status(sovereignty_conditioned_on_justification, holdable).
narrative_ontology:cs_axiom_grounding('19eec8f2-f7e0-446a-bbb4-6174e4f7ba59', sovereignty_conditioned_on_justification, conventional).
narrative_ontology:cs_axiom('19eec8f2-f7e0-446a-bbb4-6174e4f7ba59', foundational, non_refoulement_as_binding_floor).
narrative_ontology:cs_axiom_status(non_refoulement_as_binding_floor, holdable).
narrative_ontology:cs_axiom_grounding('19eec8f2-f7e0-446a-bbb4-6174e4f7ba59', non_refoulement_as_binding_floor, deontological).
narrative_ontology:cs_reference_frame('19eec8f2-f7e0-446a-bbb4-6174e4f7ba59', post_war_sovereignty_rights_settlement).
narrative_ontology:cs_drift_state('19eec8f2-f7e0-446a-bbb4-6174e4f7ba59', contemporary_migration_crisis_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('19eec8f2-f7e0-446a-bbb4-6174e4f7ba59', '').
narrative_ontology:cs_kernel_id(border_normative_status__qualified_sovereignty, border_normative_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_normative_status__qualified_sovereignty, receiving_state_governments).
narrative_ontology:constraint_beneficiary(border_normative_status__qualified_sovereignty, citizen_populations_of_receiving_states).
narrative_ontology:constraint_beneficiary(border_normative_status__qualified_sovereignty, international_human_rights_bodies).
narrative_ontology:constraint_victim(border_normative_status__qualified_sovereignty, excluded_asylum_seekers).
narrative_ontology:constraint_victim(border_normative_status__qualified_sovereignty, displaced_citizens_awaiting_reentry).
narrative_ontology:constraint_victim(border_normative_status__qualified_sovereignty, detained_border_crossers).
narrative_ontology:constraint_vindicates(border_normative_status__qualified_sovereignty, proportionality_doctrine).
narrative_ontology:constraint_vindicates(border_normative_status__qualified_sovereignty, non_refoulement_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set and administer border policy, deciding who is admitted, detained, or removed. Must justify each enforcement action as pursuing a legitimate interest (security, public order, economic capacity) and demonstrate the measure is necessary and proportionate. Retains discretion to define what counts as legitimate interest in the first instance, giving it substantial control over its own constraint.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, receiving_state_governments, agenda_setter,
    institutional, generational, arbitrage, national).

% Benefit from controlled admission that preserves labor market conditions, public services capacity, and the polity's capacity for collective self-determination, while also benefiting from the rights floor that prevents their own state from treating any minority population as excludable without justification. Can exit the polity themselves if dissatisfied; are never the ones detained or turned away.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, citizen_populations_of_receiving_states, beneficiary,
    organized, biographical, mobile, national).

% Adjudicate whether specific border measures satisfy proportionality and rights compliance; their institutional relevance and caseload depend on border control remaining a contestable, justiciable practice rather than either unconstrained sovereignty or an abolished border. They gain standing and mandate from the very existence of the adjudication burden this reading creates.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, international_human_rights_bodies, beneficiary,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(border_normative_status__qualified_sovereignty, international_human_rights_bodies, observer).

% Seek entry or protection and are turned back, detained, or left in transit while states run the proportionality and necessity test on their case. The requirement to justify exclusion is a real constraint on states but offers no guarantee of admission — it produces a right to have one's exclusion reviewed, not a right to enter, and review can take years during which the person remains outside protection.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, excluded_asylum_seekers, payer,
    powerless, immediate, trapped, global).

% Citizens stranded abroad or at the border during crises (statelessness disputes, documentation gaps, deportation-then-reentry cases) who must prove citizenship or right of return against a state applying its own discretion over what counts as adequate proof, bearing the practical costs of the state's proportionality calculus even though the underlying right to enter is nominally unconditional for them.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, displaced_citizens_awaiting_reentry, payer,
    powerless, immediate, trapped, national).

% Held in detention pending adjudication of their status. Detention itself must be justified as necessary and proportionate, but the justification standard is applied and policed largely by the same authority ordering the detention, so the constraint on the state is real but weakly self-enforcing from the detained person's vantage point.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, detained_border_crossers, payer,
    powerless, immediate, trapped, national).

% Bear the consequences of receiving states' exclusion decisions — stalled remittances, returned nationals, diplomatic friction over readmission — without a formal voice in the receiving state's proportionality determination. Occasionally negotiate bilateral arrangements but are structurally outside the rights-adjudication process itself.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, sending_state_governments, excluded,
    moderate, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_normative_status__qualified_sovereignty, receiving_state_governments).
narrative_ontology:fixing_cost_class(border_normative_status__qualified_sovereignty, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared normative vocabulary (legitimate interest, necessity, proportionality) that lets states retain border authority while giving courts, treaty bodies, and civil society a common standard against which to contest specific enforcement measures, avoiding both unconstrained state discretion and an unworkable open-border mandate.
% TRANSFER_FUNCTION: Moves the burden of justification from the excluded person (who under a pure sovereignty reading would have no claim at all) partly onto the state, while leaving the burden of proof, delay, and detention largely on the excluded and displaced individuals whose situations are adjudicated case by case.
% ABSENT_VOICES: Sending states and diaspora communities affected by exclusion and deportation decisions have no formal seat in the proportionality determination; excluded asylum seekers typically lack resources for full legal representation in the very proceedings that determine whether their exclusion was proportionate.
% DISAPPEARANCE_RATIONALE: If the proportionality/rights-compliance requirement vanished, receiving states would face no adjudicable floor on border enforcement and could exclude or detain without needing to demonstrate necessity — asylum law, non-refoulement litigation, and much of the international human rights bureaucracy built around migration would lose their operative anchor.
% FOUNDING_PROBLEM: Post-WWII and post-decolonization international law needed to reconcile two competing commitments states had already accepted: continued national sovereignty over membership and territory, and the human rights obligations (especially non-refoulement) states signed onto after witnessing the consequences of unchecked exclusion of refugees.
% FOUNDING_PROBLEM_CORROBORATION: UNHCR and regional human rights courts (outside any single receiving state's government) attest the underlying tension between sovereignty and protection obligations remains live and unresolved in ongoing litigation; several receiving-state governments simultaneously assert the balance is now settled in their favor and treat further judicial scrutiny as illegitimate second-guessing — the corroboration is genuinely split rather than unanimous.
narrative_ontology:disappearance_verdict(border_normative_status__qualified_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(border_normative_status__qualified_sovereignty, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_normative_status__qualified_sovereignty, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(border_normative_status__qualified_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(border_normative_status__qualified_sovereignty, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_normative_status__qualified_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_normative_status__qualified_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_normative_status__qualified_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.52 at interval end) rather than high because the proportionality requirement genuinely narrows what states may do compared to unconstrained exclusion — this is not a bare fig leaf. Suppression (0.58) reflects that enforcement (detention, deportation, transit-zone confinement) continues to operate on excluded individuals while their proportionality challenge is pending, so the constraint on the state does not translate into an immediate constraint on enforcement in practice. Theater ratio (0.4) captures a meaningful and growing share of state action that performs compliance with proportionality review (elaborate documentation, tribunal processes) without altering underlying admission outcomes for most claimants — this has risen over the interval as states have adapted procedural compliance without substantive reform.
 *
 * PERSPECTIVAL GAP:
 *   From the receiving state's seat this looks like a workable, self-administered balancing test it substantially controls. From the excluded asylum seeker's seat the same framework looks like an extraction mechanism wearing due-process clothing: the proportionality standard is real in principle but its application, timeline, and evidentiary burden are set by the party being constrained.
 *
 * DIRECTIONALITY LOGIC:
 *   Receiving states are near the beneficiary end: they retain the core discretion (defining legitimate interest, running the necessity/proportionality test) and bear only the cost of justifying decisions after the fact, mostly to bodies without direct enforcement power over them. Citizen populations and human rights institutions are secondary beneficiaries — the former from continued sovereignty plus a rights floor, the latter from an expanded adjudicatory mandate. Excluded asylum seekers, displaced citizens, and detained crossers sit near the full-target end: they bear the delay, detention, and evidentiary burden of the proportionality process itself, with no guaranteed favorable outcome even when a violation is eventually found.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reconciling sovereignty with post-war rights commitments) is genuinely contested rather than dead or fully resolved, which is why this reading claims tangled_rope rather than snare: a live coordination function exists (state self-determination coexisting with a binding rights floor) that pure sovereignty would not provide and pure freedom-of-movement would eliminate by removing the sovereignty side entirely. But the structure's persistence also depends on active enforcement mechanisms (detention, border policing) that fall on populations with no seat at the adjudicating table, which is the asymmetric-extraction half of the tangled rope gate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    who_sets_the_proportionality_standard,
    'Is the proportionality/necessity standard genuinely external to the state (set and enforced by an independent body with teeth) or substantially self-administered by the same state whose conduct it evaluates?',
    'Comparative study of enforcement outcomes: track the rate at which independent tribunals (regional human rights courts, UN treaty bodies) actually overturn state proportionality determinations versus the rate at which states successfully defend contested exclusions using domestically generated justifications.',
    'If independent bodies rarely overturn state determinations in practice, the qualified-sovereignty reading functions closer to sovereignty_primary despite its rights-compliant vocabulary — the coordination claim would weaken and the classification would drift toward snare. If overturn rates are substantial and consequential, the tangled_rope reading with a genuine coordination floor is well supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(who_sets_the_proportionality_standard, empirical, 'Whether proportionality review is genuinely independent or state-self-administered in practice.').

omega_variable(
    adjudication_burden_distribution,
    'Does the requirement that states justify border measures meaningfully shift costs onto states, or does it primarily shift costs onto excluded individuals by creating a lengthy adjudication process they must survive?',
    'Track average case duration, detention time pending adjudication, and success rates for excluded claimants across jurisdictions that have adopted proportionality review versus those operating under looser sovereignty-primary regimes.',
    'If adjudication primarily adds delay and detention time for claimants without materially changing exclusion rates, the framework''s coordination benefit accrues mainly to states (procedural legitimacy) while its costs land on migrants — supporting a more extractive reading of the same structural facts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adjudication_burden_distribution, empirical, 'Whether the adjudication requirement shifts real costs onto states or mainly onto those excluded.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the qualified-sovereignty framing the natural middle position between sovereignty_primary and freedom_primary, or is it better understood as sovereignty_primary wearing a thin rights-compliance overlay that rarely binds in practice?',
    'Compare this reading''s structural predictions (adjudication burden on states, dual victim set of excluded migrants and displaced citizens) against actual case outcomes; if outcomes track sovereignty_primary predictions closely, the two framings may not be structurally distinct in operation despite different vocabularies.',
    'If the two framings converge empirically, this story and the sovereignty_primary sibling may need to be re-examined for whether they are genuinely different constraints or the same arrangement described with different justificatory language — which would itself be a finding about the kernel''s contested vocabulary rather than its structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether qualified sovereignty is structurally distinct from sovereignty_primary in practice or only in stated justification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_normative_status__qualified_sovereignty, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t0, border_normative_status__qualified_sovereignty, theater_ratio, 0, 0.2).
narrative_ontology:measurement(bord_tr_t8, border_normative_status__qualified_sovereignty, theater_ratio, 8, 0.25).
narrative_ontology:measurement(bord_tr_t16, border_normative_status__qualified_sovereignty, theater_ratio, 16, 0.3).
narrative_ontology:measurement(bord_tr_t24, border_normative_status__qualified_sovereignty, theater_ratio, 24, 0.34).
narrative_ontology:measurement(bord_tr_t32, border_normative_status__qualified_sovereignty, theater_ratio, 32, 0.38).
narrative_ontology:measurement(bord_tr_t40, border_normative_status__qualified_sovereignty, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(bord_be_t0, border_normative_status__qualified_sovereignty, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(bord_be_t8, border_normative_status__qualified_sovereignty, base_extractiveness, 8, 0.4).
narrative_ontology:measurement(bord_be_t16, border_normative_status__qualified_sovereignty, base_extractiveness, 16, 0.44).
narrative_ontology:measurement(bord_be_t24, border_normative_status__qualified_sovereignty, base_extractiveness, 24, 0.47).
narrative_ontology:measurement(bord_be_t32, border_normative_status__qualified_sovereignty, base_extractiveness, 32, 0.5).
narrative_ontology:measurement(bord_be_t40, border_normative_status__qualified_sovereignty, base_extractiveness, 40, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t0, border_normative_status__qualified_sovereignty, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(bord_su_t8, border_normative_status__qualified_sovereignty, suppression_requirement, 8, 0.46).
narrative_ontology:measurement(bord_su_t16, border_normative_status__qualified_sovereignty, suppression_requirement, 16, 0.5).
narrative_ontology:measurement(bord_su_t24, border_normative_status__qualified_sovereignty, suppression_requirement, 24, 0.53).
narrative_ontology:measurement(bord_su_t32, border_normative_status__qualified_sovereignty, suppression_requirement, 32, 0.56).
narrative_ontology:measurement(bord_su_t40, border_normative_status__qualified_sovereignty, suppression_requirement, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_normative_status__qualified_sovereignty, enforcement_mechanism).
narrative_ontology:affects_constraint(border_normative_status__qualified_sovereignty, border_normative_status__sovereignty_primary).
narrative_ontology:affects_constraint(border_normative_status__qualified_sovereignty, border_normative_status__freedom_primary).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language 'border normative status' claim per the ε-invariance principle. sovereignty_primary treats territorial exclusion as foundational self-determination authority requiring no external justification (lowest extraction from the state's own framework, since exclusion is not treated as requiring justification at all). freedom_primary treats exclusion as presumptively impermissible, generating a much larger and more starkly defined victim class among excluded migrants (highest extraction under that reading's own lights). This qualified_sovereignty reading sits structurally between them: it authors a genuine coordination function (reconciling sovereignty with rights obligations) alongside asymmetric extraction (adjudication burden falling on excluded/displaced parties), which is why it alone among the three siblings is authored as tangled_rope rather than as a cleaner mountain or snare shape.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
